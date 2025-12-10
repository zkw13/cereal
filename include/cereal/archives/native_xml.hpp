/*! \file native_xml.hpp
    \brief XML input and output archives */
/*
  Copyright (c) 2014, Randolph Voorhies, Shane Grant
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:
      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.
      * Neither the name of the copyright holder nor the
        names of its contributors may be used to endorse or promote products
        derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY
  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#ifndef CEREAL_ARCHIVES_XML_HPP_
#define CEREAL_ARCHIVES_XML_HPP_
#include "cereal/cereal.hpp"
#include "cereal/details/util.hpp"

#include "cereal/external/rapidxml/rapidxml.hpp"
#include "cereal/external/rapidxml/rapidxml_print.hpp"
#include "cereal/external/base64.hpp"

#include <sstream>
#include <stack>
#include <vector>
#include <limits>
#include <string>
#include <cstring>
#include <cmath>

namespace cereal
{
  namespace xml_detail
  {
    #ifndef CEREAL_XML_STRING_VALUE
    //! The default name for the root node in a cereal xml archive.
    /*! You can define CEREAL_XML_STRING_VALUE to be different assuming you do so
        before this file is included. */
    #define CEREAL_XML_STRING_VALUE "cereal"
    #endif // CEREAL_XML_STRING_VALUE

    //! The name given to the root node in a cereal xml archive
    static const char * CEREAL_XML_STRING = CEREAL_XML_STRING_VALUE;

    /**
     * @brief Checks whether a character is ASCII whitespace.
     *
     * Considers space, horizontal tab, newline, and carriage return as whitespace.
     *
     * @param c Character to test.
     * @return true if the character is space, tab ('\t'), newline ('\n'), or carriage return ('\r'), false otherwise.
     */
    inline bool isWhitespace( char c )
    {
      return c == ' ' || c == '\t' || c == '\n' || c == '\r';
    }
  }

  // ######################################################################
  //! An output archive designed to save data to XML
  /*! This archive uses RapidXML to build an in memory XML tree of the
      data it serializes before outputting it to its stream upon destruction.
      This archive should be used in an RAII fashion, letting
      the automatic destruction of the object cause the flush to its stream.

      XML archives provides a human readable output but at decreased
      performance (both in time and space) compared to binary archives.

      XML benefits greatly from name-value pairs, which if present, will
      name the nodes in the output.  If these are not present, each level
      of the output tree will be given an automatically generated delimited name.

      The precision of the output archive controls the number of decimals output
      for floating point numbers and should be sufficiently large (i.e. at least 20)
      if there is a desire to have binary equality between the numbers output and
      those read in.  In general you should expect a loss of precision when going
      from floating point to text and back.

      XML archives can optionally print the type of everything they serialize, which
      adds an attribute to each node.

      XML archives do not output the size information for any dynamically sized structure
      and instead infer it from the number of children for a node.  This means that data
      can be hand edited for dynamic sized structures and will still be readable.  This
      is accomplished through the cereal::SizeTag object, which will also add an attribute
      to its parent field.
      \ingroup Archives */
  class NativeXMLOutputArchive : public OutputArchive<NativeXMLOutputArchive>, public traits::TextArchive
  {
    public:
      /*! @name Common Functionality
          Common use cases for directly interacting with an NativeXMLOutputArchive */
      //! @{

      //! A class containing various advanced options for the XML archive
      /*! Options can either be directly passed to the constructor, or chained using the
          modifier functions for an interface analogous to named parameters */
      class Options
      {
        public:
          /**
 * @brief Creates an Options instance initialized with the library's defaults.
 *
 * @return Options Default-initialized options (default precision, indent, outputType, and sizeAttributes).
 */
          static Options Default(){ return Options(); }

          //! Specify specific options for the NativeXMLOutputArchive
          /**
           * @brief Configuration options for NativeXMLOutputArchive controlling formatting and metadata.
           *
           * @param precision_ Number of significant digits used when serializing floating-point values.
           * @param indent_ If true, output XML will be indented with line breaks and spaces for readability.
           * @param outputType_ If true, each serialized object will include a `type` attribute with its type name.
           * @param sizeAttributes_ If true, dynamically sized containers will include a `size="dynamic"` attribute.
           */
          explicit Options( int precision_ = std::numeric_limits<double>::max_digits10,
                            bool indent_ = false,
                            bool outputType_ = false,
                            bool sizeAttributes_ = true ) :
            itsPrecision( precision_ ),
            itsIndent( indent_ ),
            itsOutputType( outputType_ ),
            itsSizeAttributes( sizeAttributes_ )
          { }

          /*! @name Option Modifiers
              An interface for setting option settings analogous to named parameters.

              @code{cpp}
              cereal::NativeXMLOutputArchive ar( myStream,
                                           cereal::NativeXMLOutputArchive::Options()
                                           .indent(true)
                                           .sizeAttributes(false) );
              @endcode
              */
          //! @{

          /**
 * @brief Set the number of digits of precision used for floating-point output.
 *
 * @param value Number of digits of precision to use for floating-point formatting.
 * @return Options& Reference to this Options instance to allow method chaining.
 */
          Options & precision( int value ){ itsPrecision = value; return * this; }
          /**
 * @brief Enable or disable indentation of output XML.
 *
 * @param enable If `true`, the archive will indent each XML line for human-readable formatting; if `false`, output will be compact without per-line indentation.
 * @return Options& Reference to this Options instance (fluent modifier).
 */
          Options & indent( bool enable ){ itsIndent = enable; return *this; }
          /**
 * @brief Enable or disable emitting a type attribute for each serialized node.
 *
 * @param enable `true` to add a `type` attribute to serialized XML nodes, `false` to omit it.
 * @return Options& Reference to this Options instance to allow method chaining.
 */
          Options & outputType( bool enable ){ itsOutputType = enable; return *this; }
          /**
 * @brief Enable or disable emitting size="dynamic" attributes for dynamically sized containers.
 *
 * @param enable `true` to add size="dynamic" attributes for dynamic containers when saving, `false` to omit them.
 * @return Options& Reference to this Options instance for method chaining.
 */
          Options & sizeAttributes( bool enable ){ itsSizeAttributes = enable; return *this; }

          //! @}

        private:
          friend class NativeXMLOutputArchive;
          int itsPrecision;
          bool itsIndent;
          bool itsOutputType;
          bool itsSizeAttributes;
      };

      //! Construct, outputting to the provided stream upon destruction
      /**
       * @brief Constructs an XML output archive that writes to the given stream using the provided options.
       *
       * Initializes an in-memory RapidXML document (including XML declaration and root node), configures
       * formatting and precision on the provided stream and the archive's internal string stream, and
       * prepares the node stack for subsequent serialization. The archive writes its serialized XML to
       * the supplied stream when the archive is destroyed.
       *
       * @param stream  Output stream to receive the serialized XML. The archive flushes its XML to this
       *                stream on destruction.
       * @param options Configuration options controlling precision, indentation, type attributes, and
       *                whether size attributes are emitted. Defaults to Options::Default().
       */
      NativeXMLOutputArchive( std::ostream & stream, Options const & options = Options::Default() ) :
        OutputArchive<NativeXMLOutputArchive>(this),
        itsStream(stream),
        itsOutputType( options.itsOutputType ),
        itsIndent( options.itsIndent ),
        itsSizeAttributes(options.itsSizeAttributes)
      {
        // rapidxml will delete all allocations when xml_document is cleared
        auto node = itsXML.allocate_node( rapidxml::node_declaration );
        node->append_attribute( itsXML.allocate_attribute( "version", "1.0" ) );
        node->append_attribute( itsXML.allocate_attribute( "encoding", "utf-8" ) );
        itsXML.append_node( node );

        // allocate root node
        auto root = itsXML.allocate_node( rapidxml::node_element, xml_detail::CEREAL_XML_STRING );
        itsXML.append_node( root );
        itsNodes.emplace( root );

        // set attributes on the streams
        itsStream << std::boolalpha;
        itsStream.precision( options.itsPrecision );
        itsOS << std::boolalpha;
        itsOS.precision( options.itsPrecision );
      }

      /**
       * @brief Flushes the in-memory XML document to the configured output stream.
       *
       * Writes the archive's XML document to the associated output stream respecting the archive's indentation setting,
       * then clears the internal XML document.
       */
      ~NativeXMLOutputArchive() CEREAL_NOEXCEPT
      {
        const int flags = itsIndent ? 0x0 : rapidxml::print_no_indenting;
        rapidxml::print( itsStream, itsXML, flags );
        itsXML.clear();
      }

      //! Saves some binary data, encoded as a base64 string, with an optional name
      /**
       * @brief Creates a child XML node containing base64-encoded binary data and finishes the node.
       *
       * The function creates a new child element of the current XML node, encodes the provided
       * binary buffer as a base64 string and stores it as the node's text content, then closes
       * the child node. If type output is enabled, the node will receive a `type="cereal binary data"` attribute.
       *
       * @param data Pointer to the binary data to encode and store.
       * @param size Number of bytes available at `data`.
       * @param name Optional name for the child node; if `nullptr`, an automatic name is used.
       */
      void saveBinaryValue( const void * data, size_t size, const char * name = nullptr )
      {
        itsNodes.top().name = name;

        startNode();

        auto base64string = base64::encode( reinterpret_cast<const unsigned char *>( data ), size );
        saveValue( base64string );

        if( itsOutputType )
          itsNodes.top().node->append_attribute( itsXML.allocate_attribute( "type", "cereal binary data" ) );

        finishNode();
      }

      //! @}
      /*! @name Internal Functionality
          Functionality designed for use by those requiring control over the inner mechanisms of
          the NativeXMLOutputArchive */
      //! @{

      //! Creates a new node that is a child of the node at the top of the stack
      /**
       * @brief Starts a new XML element and pushes it onto the internal node stack.
       *
       * Determines the child's name (consuming a name previously set via setNextName or generating
       * one from the parent's counter), allocates the name in the document, creates and appends
       * an element node with that name to the current parent, and then pushes the new node onto
       * the node stack.
       */
      void startNode()
      {
        // generate a name for this new node
        const auto nameString = itsNodes.top().getValueName();

        // allocate strings for all of the data in the XML object
        auto namePtr = itsXML.allocate_string( nameString.data(), nameString.length() + 1 );

        // insert into the XML
        auto node = itsXML.allocate_node( rapidxml::node_element, namePtr, nullptr, nameString.size() );
        itsNodes.top().node->append_node( node );
        itsNodes.emplace( node );
      }

      /**
       * @brief Mark the current XML node as complete and move focus to its parent.
       *
       * Removes the top entry from the internal node stack so subsequent writes
       * or attribute insertions apply to the parent node.
       */
      void finishNode()
      {
        itsNodes.pop();
      }

      /**
       * @brief Specify the XML name to use for the next child node created by startNode.
       *
       * The provided C-string will be used once when the next child node is created and then cleared.
       *
       * @param name Null-terminated C-string naming the upcoming child node; may be `nullptr` to unset any previously set name.
       */
      void setNextName( const char * name )
      {
        itsNodes.top().name = name;
      }

      //! Saves some data, encoded as a string, into the current top level node
      /*! The data will be be named with the most recent name if one exists,
          otherwise it will be given some default delimited value that depends upon
          the parent node */
      template <class T> /**
       * @brief Serializes a value to text and appends it as a data node to the current XML node.
       *
       * Serializes the provided value into the archive's text buffer and inserts that serialized
       * text as character data under the archive's current node. If the serialized text begins or
       * ends with whitespace, the function adds an `xml:space="preserve"` attribute to the node.
       *
       * @tparam T Type of the value to serialize.
       * @param value Value to serialize and store in the current XML node.
       */
      inline
      void saveValue( T const & value )
      {
        itsOS.clear(); itsOS.seekp( 0, std::ios::beg );
        itsOS << value << std::ends;

        auto strValue = itsOS.str();

        // itsOS.str() may contain data from previous calls after the first '\0' that was just inserted
        // and this data is counted in the length call. We make sure to remove that section so that the
        // whitespace validation is done properly
        strValue.resize(std::strlen(strValue.c_str()));

        // If the first or last character is a whitespace, add xml:space attribute
        const auto len = strValue.length();
        if ( len > 0 && ( xml_detail::isWhitespace( strValue[0] ) || xml_detail::isWhitespace( strValue[len - 1] ) ) )
        {
          itsNodes.top().node->append_attribute( itsXML.allocate_attribute( "xml:space", "preserve" ) );
        }

        // allocate strings for all of the data in the XML object
        auto dataPtr = itsXML.allocate_string(strValue.c_str(), strValue.length() + 1 );

        // insert into the XML
        itsNodes.top().node->append_node( itsXML.allocate_node( rapidxml::node_data, nullptr, dataPtr ) );
      }

      /**
       * @brief Serialize an unsigned 8-bit value as an unsigned 32-bit integer to avoid character interpretation.
       *
       * @param value Unsigned 8-bit integer to serialize; treated as its numeric value rather than a character.
       */
      void saveValue( uint8_t const & value )
      {
        saveValue( static_cast<uint32_t>( value ) );
      }

      /**
       * @brief Serialize an `int8_t` value as an integer instead of a character.
       *
       * Converts the given `int8_t` to a wider integral type and forwards it to
       * the integer `saveValue` overload so the byte is stored as its numeric value.
       *
       * @param value The 8-bit signed integer to serialize.
       */
      void saveValue( int8_t const & value )
      {
        saveValue( static_cast<int32_t>( value ) );
      }

      //! Causes the type to be appended as an attribute to the most recently made node if output type is set to true
      template <class T> /**
       * @brief Appends a `type` attribute with the demangled name of `T` to the current XML node.
       *
       * If type output is disabled for the archive, this function has no effect.
       *
       * @tparam T Type whose demangled name will be written into the `type` attribute.
       */
      inline
      void insertType()
      {
        if( !itsOutputType )
          return;

        // generate a name for this new node
        const auto nameString = util::demangledName<T>();

        // allocate strings for all of the data in the XML object
        auto namePtr = itsXML.allocate_string( nameString.data(), nameString.length() + 1 );

        itsNodes.top().node->append_attribute( itsXML.allocate_attribute( "type", namePtr ) );
      }

      /**
       * @brief Appends an attribute with the given name and value to the current top XML node.
       *
       * @param name Null-terminated C string containing the attribute name.
       * @param value Null-terminated C string containing the attribute value.
       */
      void appendAttribute( const char * name, const char * value )
      {
        auto namePtr =  itsXML.allocate_string( name );
        auto valuePtr = itsXML.allocate_string( value );
        itsNodes.top().node->append_attribute( itsXML.allocate_attribute( namePtr, valuePtr ) );
      }

      /**
 * @brief Indicates whether the archive emits size information as XML attributes.
 *
 * @return `true` if size attributes are enabled, `false` otherwise.
 */
bool hasSizeAttributes() const { return itsSizeAttributes; }

    protected:
      //! A struct that contains metadata about a node
      struct NodeInfo
      {
        /**
         * @brief Constructs a NodeInfo for tracking a RapidXML node and its next-child naming state.
         *
         * Initializes the NodeInfo with the given XML node, sets the internal child-name counter to 0,
         * and optionally provides a pre-set name to be consumed for the next generated child.
         *
         * @param n Pointer to the RapidXML node this NodeInfo represents; may be nullptr.
         * @param nm Optional C-string to use as the next child's name; may be nullptr.
         */
        NodeInfo( rapidxml::xml_node<> * n = nullptr,
                  const char * nm = nullptr ) :
          node( n ),
          counter( 0 ),
          name( nm )
        { }

        rapidxml::xml_node<> * node; //!< A pointer to this node
        size_t counter;              //!< The counter for naming child nodes
        const char * name;           //!< The name for the next child node

        //! Gets the name for the next child node created from this node
        /**
         * @brief Returns the next child name for the node, consuming a preset name if present.
         *
         * If a preset name was previously set, that name is returned once and cleared.
         * Otherwise a generated name of the form `valueN` (where N is an increasing counter) is returned.
         *
         * @return std::string The next child name (preset name if available, otherwise `valueN`).
         */
        std::string getValueName()
        {
          if( name )
          {
            auto n = name;
            name = nullptr;
            return {n};
          }
          else
            return "value" + std::to_string( counter++ ) + "\0";
        }
      }; // NodeInfo

      //! @}

    private:
      std::ostream & itsStream;        //!< The output stream
      rapidxml::xml_document<> itsXML; //!< The XML document
      std::stack<NodeInfo> itsNodes;   //!< A stack of nodes added to the document
      std::ostringstream itsOS;        //!< Used to format strings internally
      bool itsOutputType;              //!< Controls whether type information is printed
      bool itsIndent;                  //!< Controls whether indenting is used
      bool itsSizeAttributes;          //!< Controls whether lists have a size attribute
  }; // NativeXMLOutputArchive

  // ######################################################################
  //! An output archive designed to load data from XML
  /*! This archive uses RapidXML to build an in memory XML tree of the
      data in the stream it is given before loading any types serialized.

      As with the output XML archive, the preferred way to use this archive is in
      an RAII fashion, ensuring its destruction after all data has been read.

      Input XML should have been produced by the NativeXMLOutputArchive.  Data can
      only be added to dynamically sized containers - the input archive will
      determine their size by looking at the number of child nodes.  Data that
      did not originate from an NativeXMLOutputArchive is not officially supported,
      but may be possible to use if properly formatted.

      The NativeXMLInputArchive does not require that nodes are loaded in the same
      order they were saved by NativeXMLOutputArchive.  Using name value pairs (NVPs),
      it is possible to load in an out of order fashion or otherwise skip/select
      specific nodes to load.

      The default behavior of the input archive is to read sequentially starting
      with the first node and exploring its children.  When a given NVP does
      not match the read in name for a node, the archive will search for that
      node at the current level and load it if it exists.  After loading an out of
      order node, the archive will then proceed back to loading sequentially from
      its new position.

      Consider this simple example where loading of some data is skipped:

      @code{cpp}
      // imagine the input file has someData(1-9) saved in order at the top level node
      ar( someData1, someData2, someData3 );        // XML loads in the order it sees in the file
      ar( cereal::make_nvp( "hello", someData6 ) ); // NVP given does not
                                                    // match expected NVP name, so we search
                                                    // for the given NVP and load that value
      ar( someData7, someData8, someData9 );        // with no NVP given, loading resumes at its
                                                    // current location, proceeding sequentially
      @endcode

      \ingroup Archives */
  class NativeXMLInputArchive : public InputArchive<NativeXMLInputArchive>, public traits::TextArchive
  {
    public:
      /*! @name Common Functionality
          Common use cases for directly interacting with an NativeXMLInputArchive */
      //! @{

      //! Construct, reading in from the provided stream
      /**
       * @brief Constructs an input archive by reading and parsing an XML document from a stream.
       *
       * Reads the entire provided stream into memory, parses it as an XML document, and initializes
       * the archive's root node for subsequent deserialization.
       *
       * @param stream Stream to read the XML document from (e.g., std::ifstream or std::stringstream).
       *
       * @throws Exception If XML parsing fails (invalid characters or malformed XML) or if the expected
       *                   root node (CEREAL_XML_STRING) cannot be found.
       */
      NativeXMLInputArchive( std::istream & stream ) :
        InputArchive<NativeXMLInputArchive>( this ),
        itsData( std::istreambuf_iterator<char>( stream ), std::istreambuf_iterator<char>() ),
        nameNotFound(false)
      {
        try
        {
          itsData.push_back('\0'); // rapidxml will do terrible things without the data being null terminated
          itsXML.parse<rapidxml::parse_trim_whitespace | rapidxml::parse_no_data_nodes | rapidxml::parse_declaration_node>( reinterpret_cast<char *>( itsData.data() ) );
        }
        catch( rapidxml::parse_error const & )
        {
          //std::cerr << "-----Original-----" << std::endl;
          //stream.seekg(0);
          //std::cout << std::string( std::istreambuf_iterator<char>( stream ), std::istreambuf_iterator<char>() ) << std::endl;

          //std::cerr << "-----Error-----" << std::endl;
          //std::cerr << e.what() << std::endl;
          //std::cerr << e.where<char>() << std::endl;
          throw Exception("XML Parsing failed - likely due to invalid characters or invalid naming");
        }

        // Parse the root
        auto root = itsXML.first_node( xml_detail::CEREAL_XML_STRING );
        if( root == nullptr )
          throw Exception("Could not detect cereal root node - likely due to empty or invalid input");
        else
          itsNodes.emplace( root );
      }

      /**
 * @brief Destroy the NativeXMLInputArchive and release any owned parsing resources.
 */
~NativeXMLInputArchive() CEREAL_NOEXCEPT = default;

      //! Loads some binary data, encoded as a base64 string, optionally specified by some name
      /**
       * @brief Load binary data from the next XML node into a raw buffer, performing base64 decoding.
       *
       * Starts and finishes the targeted XML node for this read. If `name` is provided, the archive
       * will attempt to read a child node with that name; if not found, the function returns without
       * modifying the buffer.
       *
       * @param data Pointer to the destination buffer where decoded bytes will be written.
       * @param size Expected number of bytes to write into `data`.
       * @param name Optional XML node name to read; when nullptr the next unnamed child is used.
       *
       * @throws Exception if the decoded binary length does not equal `size`.
       */
      void loadBinaryValue( void * data, size_t size, const char * name = nullptr )
      {
        setNextName( name );
        startNode();

        if (nameNotFound)
          return;

        std::string encoded;
        loadValue( encoded );

        auto decoded = base64::decode( encoded );

        if( size != decoded.size() )
          throw Exception("Decoded binary data size does not match specified size");

        std::memcpy( data, decoded.data(), decoded.size() );

        finishNode();
      }

      //! @}
      /*! @name Internal Functionality
          Functionality designed for use by those requiring control over the inner mechanisms of
          the NativeXMLInputArchive */
      /**
 * @brief Indicates whether the last requested child name was not found during node lookup.
 *
 * @return `true` if a requested child name (set via setNextName) could not be found when starting a node, `false` otherwise.
 */

      bool isNameNotFound() const { return nameNotFound; }

      //! Prepares to start reading the next node
      /**
       * @brief Pushes the next XML node to be processed onto the internal node stack.
       *
       * Determines which child node of the current top node should become the new current node:
       * - If an expected name (from an NVP) is not provided or matches the next sequential child, that child is used.
       * - If an expected name is provided and does not match the next sequential child, siblings at the current level are searched for a matching name.
       *
       * If a matching named child is found, it is pushed onto the stack and normal processing continues.
       * If no matching child is found, the expected name is cleared, the archive's nameNotFound flag is set to true, and no node is pushed.
       */
      void startNode()
      {
        auto next = itsNodes.top().child; // By default we would move to the next child node
        auto const expectedName = itsNodes.top().name; // this is the expected name from the NVP, if provided

        // If we were given an NVP name, look for it in the current level of the document.
        //    We only need to do this if either we have exhausted the siblings of the current level or
        //    the NVP name does not match the name of the node we would normally read next
        if( expectedName && ( next == nullptr || std::strcmp( next->name(), expectedName ) != 0 ) )
        {
          next = itsNodes.top().search( expectedName );

          if( next == nullptr )
          {
            // throw Exception("XML Parsing failed - provided NVP (" + std::string(expectedName) + ") not found");
            itsNodes.top().name = nullptr;
            nameNotFound = true;
            return;
          }
        }

        itsNodes.emplace( next );
        nameNotFound = false;
      }

      /**
       * @brief Finish reading the current XML node and move the parent to its next child.
       *
       * Pops the current node from the internal node stack, advances the parent node's
       * current-child pointer, and clears any pending name set for the parent's next child.
       * If called while at the root node, the call is a no-op.
       */
      void finishNode()
      {
        // epilogue 做过判断，如果没有找到节点不会执行这个函数，但还是再做一次判断
        if (itsNodes.size() <= 1)
          return;

        // remove current
        itsNodes.pop();

        // advance parent
        itsNodes.top().advance();

        // Reset name
        itsNodes.top().name = nullptr;
      }

      //! Retrieves the current node name
      /**
       * @brief Get the name of the current top node's child.
       *
       * @return Pointer to the child's name, or `nullptr` if the node does not have a name.
       */
      const char * getNodeName() const
      {
        return itsNodes.top().getChildName();
      }

      /**
       * @brief Specify the XML name to use for the next child node created by startNode.
       *
       * The provided C-string will be used once when the next child node is created and then cleared.
       *
       * @param name Null-terminated C-string naming the upcoming child node; may be `nullptr` to unset any previously set name.
       */
      void setNextName( const char * name )
      {
        itsNodes.top().name = name;
      }

      //! Loads a bool from the current top node
      template <class T, traits::EnableIf<std::is_unsigned<T>::value,
                                          std::is_same<T, bool>::value> = traits::sfinae> /**
       * @brief Deserializes a value from the current XML node into the provided reference.
       *
       * If the requested node name was not found, the function does nothing. Otherwise it
       * parses the node's text content using stream extraction (with `boolalpha` enabled)
       * and assigns the parsed result to `value`.
       *
       * @param[out] value Destination for the parsed value.
       */
      inline
      void loadValue( T & value )
      {
        if (isNameNotFound())
          return;
        std::istringstream is( itsNodes.top().node->value() );
        is.setf( std::ios::boolalpha );
        is >> value;
      }

      //! Loads a char (signed or unsigned) from the current top node
      template <class T, traits::EnableIf<std::is_integral<T>::value,
                                          !std::is_same<T, bool>::value,
                                          sizeof(T) == sizeof(char)> = traits::sfinae> /**
       * @brief Loads a raw POD value from the current XML node into the provided reference.
       *
       * Copies the bytes stored in the current node's value directly into `value`. If a name
       * lookup previously failed (isNameNotFound()), the call is a no-op.
       *
       * @param[out] value Destination where the node's raw bytes will be copied.
       *
       * @note The node's value must contain at least sizeof(T) bytes representing a valid
       *       in-memory representation of `T`; no size or format validation is performed.
       */
      inline
      void loadValue( T & value )
      {
        if (isNameNotFound())
          return;
        value = *reinterpret_cast<T*>( itsNodes.top().node->value() );
      }

      /**
       * @brief Load an 8-bit signed integer from the current XML node into value.
       *
       * Parses the node's text as a 32-bit integer and assigns the result converted to `int8_t` to `value`.
       * If a requested name was not found for the current node, the function returns without modifying `value`.
       *
       * @param[out] value Reference where the parsed `int8_t` result is stored.
       */
      void loadValue( int8_t & value )
      {
        if (isNameNotFound())
          return;
        int32_t val; loadValue( val ); value = static_cast<int8_t>( val );
      }

      /**
       * @brief Reads an unsigned 8-bit integer from the current top XML node and stores it in `value`.
       *
       * If a previously requested name was not found at this level, the function returns without modifying `value`.
       *
       * @param[out] value Destination for the parsed uint8_t.
       */
      void loadValue( uint8_t & value )
      {
        if (isNameNotFound())
          return;
        uint32_t val; loadValue( val ); value = static_cast<uint8_t>( val );
      }

      //! Loads a type best represented as an unsigned long from the current top node
      template <class T, traits::EnableIf<std::is_unsigned<T>::value,
                                          !std::is_same<T, bool>::value,
                                          !std::is_same<T, char>::value,
                                          !std::is_same<T, unsigned char>::value,
                                          sizeof(T) < sizeof(long long)> = traits::sfinae> /**
       * @brief Parses the current XML node's text as an unsigned long and stores it in the provided output parameter converted to `T`.
       *
       * If a previous name lookup marked the node as not found, the function returns immediately without modifying `value`.
       *
       * @param value Reference receiving the parsed numeric value converted to `T`.
       * @throws std::invalid_argument If the node text does not represent a valid unsigned integer.
       * @throws std::out_of_range If the parsed unsigned long is out of range or cannot be converted to `T`.
       */
      inline
      void loadValue( T & value )
      {
        if (isNameNotFound())
          return;
        value = static_cast<T>( std::stoul( itsNodes.top().node->value() ) );
      }

      //! Loads a type best represented as an unsigned long long from the current top node
      template <class T, traits::EnableIf<std::is_unsigned<T>::value,
                                          !std::is_same<T, bool>::value,
                                          sizeof(T) >= sizeof(long long)> = /**
       * @brief Loads an unsigned integer value from the current XML node's text.
       *
       * Parses the current node's text as a decimal unsigned long long and assigns the result
       * (converted to `T`) to `value`. If a requested name was not found, the function does nothing.
       *
       * @param[out] value Destination for the parsed numeric value.
       *
       * @throws std::invalid_argument if the node text is not a valid unsigned integer.
       * @throws std::out_of_range if the parsed value does not fit in `unsigned long long`.
       */
      traits::sfinae> inline
      void loadValue( T & value )
      {
        if (isNameNotFound())
          return;
        value = static_cast<T>( std::stoull( itsNodes.top().node->value() ) );
      }

      //! Loads a type best represented as an int from the current top node
      template <class T, traits::EnableIf<std::is_signed<T>::value,
                                          !std::is_same<T, char>::value,
                                          sizeof(T) <= sizeof(int)> = traits::sfinae> /**
       * @brief Parses the current XML node's text as an integer and assigns it to value.
       *
       * If the archive's current requested name was not found, does nothing and leaves value unchanged.
       *
       * @param[out] value Destination to receive the parsed integer, converted to `T`.
       *
       * @throws std::invalid_argument if the node text does not contain a valid integer representation.
       * @throws std::out_of_range if the parsed integer is outside the range representable by `int`.
       */
      inline
      void loadValue( T & value )
      {
        if (isNameNotFound())
          return;
        value = static_cast<T>( std::stoi( itsNodes.top().node->value() ) );
      }

      //! Loads a type best represented as a long from the current top node
      template <class T, traits::EnableIf<std::is_signed<T>::value,
                                          (sizeof(T) > sizeof(int)),
                                          sizeof(T) <= sizeof(long)> = traits::sfinae> /**
       * @brief Loads an integer value from the current XML node into the provided reference.
       *
       * Parses the current node's text as a signed long using std::stol and assigns the result converted to `T` into `value`.
       *
       * @tparam T Target integral type.
       * @param[out] value Destination for the parsed numeric value.
       */
      inline
      void loadValue( T & value )
      {
        if (isNameNotFound())
          return;
        value = static_cast<T>( std::stol( itsNodes.top().node->value() ) );
      }

      //! Loads a type best represented as a long long from the current top node
      template <class T, traits::EnableIf<std::is_signed<T>::value,
                                          (sizeof(T) > sizeof(long)),
                                          sizeof(T) <= sizeof(long long)> = traits::sfinae> /**
       * @brief Parses the current node's text as a signed integer and assigns it to the provided value.
       *
       * If a name was set but not found for the current node, the function returns without modifying value.
       *
       * @param[out] value Destination to receive the parsed integer, converted to type `T`.
       * @throws std::invalid_argument If the node text does not contain a valid integer representation.
       * @throws std::out_of_range If the parsed integer is outside the range representable by `long long`.
       */
      inline
      void loadValue( T & value )
      {
        if (isNameNotFound())
          return;
        value = static_cast<T>( std::stoll( itsNodes.top().node->value() ) );
      }

      /**
       * @brief Loads a floating-point value from the current top XML node into the provided reference.
       *
       * If no matching node name was found, the function returns without modifying `value`. Otherwise the
       * node's text content is parsed and stored in `value`. Very large magnitudes that exceed `float`
       * range will propagate `std::out_of_range` unless the parsed result is a subnormal value, in which
       * case the subnormal value is accepted.
       *
       * @param[out] value Destination for the parsed `float`.
       *
       * @throws std::invalid_argument If the node text does not contain a valid floating-point representation.
       * @throws std::out_of_range If the node text represents a magnitude outside `float` range and is not a subnormal value.
       */
      void loadValue( float & value )
      {
        if (isNameNotFound())
          return;
        try
        {
          value = std::stof( itsNodes.top().node->value() );
        }
        catch( std::out_of_range const & )
        {
          // special case for denormalized values
          std::istringstream is( itsNodes.top().node->value() );
          is >> value;
          if( std::fpclassify( value ) != FP_SUBNORMAL )
            throw;
        }
      }

      /**
       * @brief Loads a floating-point value from the current top XML node into the provided reference.
       *
       * Parses the text content of the current top node as a `double` and assigns it to `value`.
       *
       * @param[out] value Destination for the parsed double.
       *
       * @throws std::invalid_argument If the node's text cannot be interpreted as a floating-point number.
       * @throws std::out_of_range If the parsed numeric value is outside the range representable by `double`
       *                           (unless the value parses as a subnormal number, in which case the
       *                           function accepts it).
       */
      void loadValue( double & value )
      {
        if (isNameNotFound())
          return;
        try
        {
          value = std::stod( itsNodes.top().node->value() );
        }
        catch( std::out_of_range const & )
        {
          // special case for denormalized values
          std::istringstream is( itsNodes.top().node->value() );
          is >> value;
          if( std::fpclassify( value ) != FP_SUBNORMAL )
            throw;
        }
      }

      /**
       * @brief Parses the current top XML node's text into a long double.
       *
       * Attempts to convert the top node's textual value to `long double` and stores the result in `value`.
       * If the expected name was not found, `value` is left unchanged.
       * For out-of-range conversions, a secondary parse is attempted to allow denormal (subnormal) values;
       * the original exception is propagated if the value is not a subnormal number.
       *
       * @param[out] value Destination for the parsed `long double`.
       */
      void loadValue( long double & value )
      {
        if (isNameNotFound())
          return;
        try
        {
          value = std::stold( itsNodes.top().node->value() );
        }
        catch( std::out_of_range const & )
        {
          // special case for denormalized values
          std::istringstream is( itsNodes.top().node->value() );
          is >> value;
          if( std::fpclassify( value ) != FP_SUBNORMAL )
            throw;
        }
      }

      //! Loads a string from the current node from the current top node
      template<class CharT, class Traits, class Alloc> /**
       * @brief Loads the current XML node's text content into the provided string.
       *
       * If the requested node name was not found, the function returns without modifying the string.
       *
       * @param str Destination string that will be assigned the node's text content.
       */
      inline
      void loadValue( std::basic_string<CharT, Traits, Alloc> & str )
      {
        if (isNameNotFound())
          return;
        std::basic_istringstream<CharT, Traits> is( itsNodes.top().node->value() );

        str.assign( std::istreambuf_iterator<CharT, Traits>( is ),
                    std::istreambuf_iterator<CharT, Traits>() );
      }

      //! Loads the size of the current top node
      template <class T> /**
       * @brief Assigns the number of child XML nodes of the current node to the provided value.
       *
       * Sets `value` to the count of child elements under the archive's current XML node.
       *
       * @param[out] value Destination that receives the number of child nodes.
       */
      inline
      void loadSize( T & value )
      {
        value = getNumChildren( itsNodes.top().node );
      }

    protected:
      /**
       * @brief Count the immediate child nodes of the given XML node.
       *
       * @param node Pointer to the parent XML node whose direct children will be counted.
       *             Behavior is undefined if `node` is `nullptr`.
       * @return size_t Number of immediate child nodes (0 if there are none).
       */
      static size_t getNumChildren( rapidxml::xml_node<> * node )
      {
        size_t size = 0;
        node = node->first_node(); // get first child

        while( node != nullptr )
        {
          ++size;
          node = node->next_sibling();
        }

        return size;
      }

      //! A struct that contains metadata about a node
      /*! Keeps track of some top level node, its number of
          remaining children, and the current active child node */
      struct NodeInfo
      {
        /**
         * @brief Constructs a NodeInfo for the given XML node and initializes traversal state.
         *
         * @param n Pointer to the RapidXML node to wrap; if `nullptr`, creates an empty NodeInfo.
         *
         * The constructed NodeInfo's `child` is set to the node's first child (or `nullptr` if none),
         * `size` is set to the number of child nodes, and `name` is initialized to `nullptr`.
         */
        NodeInfo( rapidxml::xml_node<> * n = nullptr ) :
          node( n ),
          child( n ? n->first_node() : nullptr ),
          size( NativeXMLInputArchive::getNumChildren( n ) ),
          name( nullptr )
        { }

        //! Advances to the next sibling node of the child
        /**
         * @brief Advance to the next sibling and decrement the remaining-child count.
         *
         * If there is at least one remaining child, decrements `size` and sets
         * `child` to its next sibling; when the advanced position has no sibling,
         * `child` becomes `nullptr`.
         */
        void advance()
        {
          if( size > 0 )
          {
            --size;
            child = child->next_sibling();
          }
        }

        //! Searches for a child with the given name in this node
        /**
         * @brief Searches this NodeInfo's children for a child node with the specified name.
         *
         * @param searchName Null-terminated name to search for among the immediate children.
         * @return rapidxml::xml_node<>* The matching child node, or `nullptr` if no child with that name exists.
         */
        rapidxml::xml_node<> * search( const char * searchName )
        {
          if( searchName )
          {
            size_t new_size = NativeXMLInputArchive::getNumChildren( node );
            const size_t name_size = rapidxml::internal::measure( searchName );

            for( auto new_child = node->first_node(); new_child != nullptr; new_child = new_child->next_sibling() )
            {
              if( rapidxml::internal::compare( new_child->name(), new_child->name_size(), searchName, name_size, true ) )
              {
                size = new_size;
                child = new_child;

                return new_child;
              }
              --new_size;
            }
          }

          return nullptr;
        }

        /**
         * @brief Get the name of the current child node, if present.
         *
         * @return const char* Pointer to the child's name, or `nullptr` when there is no current child.
         */
        const char * getChildName() const
        {
          return child ? child->name() : nullptr;
        }

        rapidxml::xml_node<> * node;  //!< A pointer to this node
        rapidxml::xml_node<> * child; //!< A pointer to its current child
        size_t size;                  //!< The remaining number of children for this node
        const char * name;            //!< The NVP name for next child node
      }; // NodeInfo

      //! @}

    private:
      std::vector<char> itsData;       //!< The raw data loaded
      rapidxml::xml_document<> itsXML; //!< The XML document
      std::stack<NodeInfo> itsNodes;   //!< A stack of nodes read from the document
      bool nameNotFound;               //!< A flag to indicate if the node was not found
  };

  // ######################################################################
  // XMLArchive prologue and epilogue functions
  // ######################################################################

  // ######################################################################
  //! Prologue for NVPs for XML output archives
  /*! NVPs do not start or finish nodes - they just set up the names */
  template <class T> /**
   * @brief No-op prologue for name-value pairs.
   *
   * This prologue intentionally performs no action before serializing a NameValuePair.
   *
   * @param nvp The name-value pair to be serialized (unused).
   * @tparam T Type of the wrapped value.
   */
  inline
  void prologue( NativeXMLOutputArchive &, NameValuePair<T> const & )
  { }

  //! Prologue for NVPs for XML input archives
  template <class T> /**
   * @brief No-op prologue for NameValuePair on XML input archives.
   *
   * Performs no action when beginning deserialization of a NameValuePair with
   * NativeXMLInputArchive.
   */
  inline
  void prologue( NativeXMLInputArchive &, NameValuePair<T> const & )
  { }

  // ######################################################################
  //! Epilogue for NVPs for XML output archives
  /*! NVPs do not start or finish nodes - they just set up the names */
  template <class T> /**
   * @brief No-op epilogue invoked after saving a name-value pair.
   *
   * Called after a NameValuePair has been written to the NativeXMLOutputArchive;
   * this implementation performs no action.
   */
  inline
  void epilogue( NativeXMLOutputArchive &, NameValuePair<T> const & )
  { }

  //! Epilogue for NVPs for XML input archives
  template <class T> /**
   * @brief Epilogue hook invoked after loading a name-value pair from an XML input archive.
   *
   * This implementation performs no action.
   */
  inline
  void epilogue( NativeXMLInputArchive &, NameValuePair<T> const & )
  { }

  // ######################################################################
  //! Prologue for deferred data for XML archives
  /*! Do nothing for the defer wrapper */
  template <class T> /**
   * @brief No-op prologue for saving DeferredData with NativeXMLOutputArchive.
   *
   * This function performs no action; DeferredData does not require any setup
   * before being serialized by the output archive.
   *
   * @param /*unused*/ DeferredData reference placeholder for overload resolution.
   */
  inline
  void prologue( NativeXMLOutputArchive &, DeferredData<T> const & )
  { }

  //! Prologue for deferred data for XML archives
  template <class T> /**
   * @brief No-op prologue invoked before loading deferred data from XML.
   *
   * This function intentionally performs no initialization when a DeferredData<T>
   * is encountered during input with NativeXMLInputArchive.
   *
   * @tparam T Type of the deferred data (unused).
   */
  inline
  void prologue( NativeXMLInputArchive &, DeferredData<T> const & )
  { }

  // ######################################################################
  //! Epilogue for deferred for XML archives
  /*! NVPs do not start or finish nodes - they just set up the names */
  template <class T> /**
   * @brief No-op epilogue invoked after handling a DeferredData wrapper for the XML output archive.
   *
   * @tparam T Type of the deferred value (unused).
   */
  inline
  void epilogue( NativeXMLOutputArchive &, DeferredData<T> const & )
  { }

  //! Epilogue for deferred for XML archives
  /*! Do nothing for the defer wrapper */
  template <class T> /**
   * @brief No-op epilogue for deferred data when reading from XML.
   *
   * This function intentionally performs no action for DeferredData during input
   * (it exists to match the archive epilogue interface).
   *
   * @param[in]  /*archive*/  Unused input XML archive.
   * @param[in]  deferred     DeferredData instance; ignored by the input epilogue.
   */
  inline
  void epilogue( NativeXMLInputArchive &, DeferredData<T> const & )
  { }

  // ######################################################################
  //! Prologue for SizeTags for XML output archives
  /*! SizeTags do not start or finish nodes */
  template <class T> /**
   * @brief Handle SizeTag prologue for NativeXMLOutputArchive.
   *
   * If the archive is configured to emit size attributes, appends a `size="dynamic"`
   * attribute to the current XML node to indicate a dynamically sized container.
   *
   * @param ar Archive to modify; the attribute is added to ar's current node when enabled.
   * @param[in]  /*unused*/ SizeTag marker indicating the forthcoming value represents a size.
   */
  inline
  void prologue( NativeXMLOutputArchive & ar, SizeTag<T> const & )
  {
      if (ar.hasSizeAttributes())
      {
          ar.appendAttribute("size", "dynamic");
      }
  }

  template <class T> /**
   * @brief No-op prologue for loading a SizeTag from a NativeXMLInputArchive.
   *
   * Does nothing; the input XML archive does not require any setup before
   * deserializing a SizeTag.
   */
  inline
  void prologue( NativeXMLInputArchive &, SizeTag<T> const & )
  { }

  //! Epilogue for SizeTags for XML output archives
  /*! SizeTags do not start or finish nodes */
  template <class T> /**
   * @brief Epilogue hook for writing a SizeTag to the XML output archive that performs no action.
   *
   * This function intentionally does nothing: no additional processing is required when closing
   * a SizeTag during output serialization.
   */
  inline
  void epilogue( NativeXMLOutputArchive &, SizeTag<T> const & )
  { }

  template <class T> /**
   * @brief No-op epilogue invoked after loading a SizeTag from an XML input archive.
   *
   * This function intentionally performs no actions; it exists to provide a matching
   * epilogue hook for SizeTag when deserializing with NativeXMLInputArchive.
   *
   * @tparam T Type stored in the SizeTag.
   */
  inline
  void epilogue( NativeXMLInputArchive &, SizeTag<T> const & )
  { }

  // ######################################################################
  //! Prologue for all other types for XML output archives (except minimal types)
  /*! Starts a new node, named either automatically or by some NVP,
      that may be given data by the type about to be archived

      Minimal types do not start or end nodes */
  template <class T, traits::DisableIf<traits::has_minimal_base_class_serialization<T, traits::has_minimal_output_serialization, NativeXMLOutputArchive>::value ||
                                       traits::has_minimal_output_serialization<T, NativeXMLOutputArchive>::value> = traits::sfinae> /**
   * @brief Begins an XML element for serializing a value and records its C++ type as an attribute.
   *
   * @tparam T Type of the value being serialized.
   * @param ar Archive to write the new XML node into.
   * @param /*unused*/ Unused value parameter used only to select the template overload.
   */
  inline
  void prologue( NativeXMLOutputArchive & ar, T const & )
  {
    ar.startNode();
    ar.insertType<T>();
  }

  //! Prologue for all other types for XML input archives (except minimal types)
  template <class T, traits::DisableIf<traits::has_minimal_base_class_serialization<T, traits::has_minimal_input_serialization, NativeXMLInputArchive>::value ||
                                       traits::has_minimal_input_serialization<T, NativeXMLInputArchive>::value> = traits::sfinae> /**
   * @brief Begin reading the next XML node for a value.
   *
   * Positions the input archive at the XML node that contains the next value to be loaded.
   *
   * @param ar Archive to read from.
   */
  inline
  void prologue( NativeXMLInputArchive & ar, T const & )
  {
    ar.startNode();
  }

  // ######################################################################
  //! Epilogue for all other types other for XML output archives (except minimal types)
  /*! Finishes the node created in the prologue

      Minimal types do not start or end nodes */
  template <class T, traits::DisableIf<traits::has_minimal_base_class_serialization<T, traits::has_minimal_output_serialization, NativeXMLOutputArchive>::value ||
                                       traits::has_minimal_output_serialization<T, NativeXMLOutputArchive>::value> = traits::sfinae> /**
   * @brief Closes the current XML node in the output archive.
   *
   * Completes the element corresponding to the just-serialized value by finishing the active node.
   */
  inline
  void epilogue( NativeXMLOutputArchive & ar, T const & )
  {
    ar.finishNode();
  }

  //! Epilogue for all other types other for XML output archives (except minimal types)
  template <class T, traits::DisableIf<traits::has_minimal_base_class_serialization<T, traits::has_minimal_input_serialization, NativeXMLInputArchive>::value ||
                                       traits::has_minimal_input_serialization<T, NativeXMLInputArchive>::value> = traits::sfinae> /**
   * @brief Finalizes a read node for a non-minimal type in the input XML archive.
   *
   * If the previously requested name was not found at the current level, this function does nothing;
   * otherwise it closes the current node (advances the archive's node stack).
   *
   * @param ar The input XML archive to operate on.
   * @param /*unused*/  Placeholder parameter to match the serialization prologue/epilogue signature; its type controls overload resolution.
   */
  inline
  void epilogue( NativeXMLInputArchive & ar, T const & )
  {
    if (ar.isNameNotFound())
      return;
    ar.finishNode();
  }

  // ######################################################################
  // Common XMLArchive serialization functions
  // ######################################################################

  //! Saving NVP types to XML
  template <class T> /**
   * @brief Serializes a named value into the XML output archive.
   *
   * Uses the NameValuePair's name as the upcoming XML node name and then serializes the paired value into the archive.
   *
   * @param t Name/value pair whose name will be used for the next XML node and whose value will be serialized.
   */
  inline
  void CEREAL_SAVE_FUNCTION_NAME( NativeXMLOutputArchive & ar, NameValuePair<T> const & t )
  {
    ar.setNextName( t.name );
    ar( t.value );
  }

  //! Loading NVP types from XML
  template <class T> /**
   * @brief Loads a named value from the XML archive into the provided NameValuePair.
   *
   * Uses the pair's `name` to select the corresponding XML node and deserializes
   * the node's contents into `t.value`.
   *
   * @param t NameValuePair whose `name` is used to locate the XML node and whose
   *          `value` will be populated from that node.
   */
  inline
  void CEREAL_LOAD_FUNCTION_NAME( NativeXMLInputArchive & ar, NameValuePair<T> & t )
  {
    ar.setNextName( t.name );
    ar( t.value );
  }

  // ######################################################################
  //! Saving SizeTags to XML
  template <class T> /**
   * @brief No-op save handler for SizeTag when writing XML.
   *
   * This overload intentionally performs no action; a SizeTag passed to the
   * NativeXMLOutputArchive is ignored by this save function.
   *
   * @tparam T Type wrapped by the SizeTag.
   * @param st The SizeTag instance; unused.
   */
  inline
  void CEREAL_SAVE_FUNCTION_NAME( NativeXMLOutputArchive &, SizeTag<T> const & )
  { }

  //! Loading SizeTags from XML
  template <class T> /**
   * @brief Loads a SizeTag from the current XML node.
   *
   * Populates the SizeTag's `size` member with the number of child nodes of the current XML node.
   *
   * @param st SizeTag whose `size` field will be set to the node's child count.
   */
  inline
  void CEREAL_LOAD_FUNCTION_NAME( NativeXMLInputArchive & ar, SizeTag<T> & st )
  {
    ar.loadSize( st.size );
  }

  // ######################################################################
  //! Saving for POD types to xml
  template <class T, traits::EnableIf<std::is_arithmetic<T>::value> = traits::sfinae> /**
   * @brief Serializes a POD or arithmetic value into the current XML element.
   *
   * Writes the given value into the archive's active XML node according to the archive's value serialization semantics.
   *
   * @tparam T Type of the value being serialized.
   * @param ar Archive to write into.
   * @param t Value to serialize.
   */
  inline
  void CEREAL_SAVE_FUNCTION_NAME(NativeXMLOutputArchive & ar, T const & t)
  {
    ar.saveValue( t );
  }

  //! Loading for POD types from xml
  template <class T, traits::EnableIf<std::is_arithmetic<T>::value> = traits::sfinae> /**
   * @brief Load a value from the current XML node into the provided object.
   *
   * If the archive indicates the requested name was not found, the function returns
   * without modifying the destination.
   *
   * @param ar Archive to read from.
   * @param t Destination object that will receive the loaded value.
   */
  inline
  void CEREAL_LOAD_FUNCTION_NAME(NativeXMLInputArchive & ar, T & t)
  {
    if (ar.isNameNotFound())
      return;
    ar.loadValue( t );
  }

  // ######################################################################
  //! saving string to xml
  template<class CharT, class Traits, class Alloc> /**
   * @brief Serializes a string into the XML output archive as the current node's value.
   *
   * @param ar Archive to write into.
   * @param str String to serialize.
   */
  inline
  void CEREAL_SAVE_FUNCTION_NAME(NativeXMLOutputArchive & ar, std::basic_string<CharT, Traits, Alloc> const & str)
  {
    ar.saveValue( str );
  }

  //! loading string from xml
  template<class CharT, class Traits, class Alloc> /**
   * @brief Load the current XML node's text content into the provided string unless the requested name was not found.
   *
   * If a prior NameValuePair set an expected child name and that name was not found at this level, this function is a no-op.
   *
   * @param str Destination string to receive the node's text content.
   */
  inline
  void CEREAL_LOAD_FUNCTION_NAME(NativeXMLInputArchive & ar, std::basic_string<CharT, Traits, Alloc> & str)
  {
    if (ar.isNameNotFound())
      return;
    ar.loadValue( str );
  }
} // namespace cereal

// register archives for polymorphic support
CEREAL_REGISTER_ARCHIVE(cereal::NativeXMLOutputArchive)
CEREAL_REGISTER_ARCHIVE(cereal::NativeXMLInputArchive)

// tie input and output archives together
CEREAL_SETUP_ARCHIVE_TRAITS(cereal::NativeXMLInputArchive, cereal::NativeXMLOutputArchive)

#endif // CEREAL_ARCHIVES_XML_HPP_