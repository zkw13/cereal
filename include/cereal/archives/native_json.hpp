/*! \file native_json.hpp
    \brief JSON input and output archives for C++ types conforming to JSON constraints */
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
      * Neither the name of cereal nor the
        names of its contributors may be used to endorse or promote products
        derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL RANDOLPH VOORHIES OR SHANE GRANT BE LIABLE FOR ANY
  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#ifndef CEREAL_ARCHIVES_NATIVE_JSON_HPP_
#define CEREAL_ARCHIVES_NATIVE_JSON_HPP_

#include <cereal/archives/json.hpp>
#include <cereal/types/base_class.hpp>

#include <map>
#include <type_traits>

namespace cereal
{
  // ######################################################################
  //! An output archive designed to save data to JSON
  /*! This archive uses RapidJSON to serialize a conforming C++ object to JSON.

      JSON archives provides a human readable output but at decreased
      performance (both in time and space) compared to binary archives.

      JSON benefits greatly from name-value pairs, which if present, will
      name the nodes in the output.  If these are not present, each level
      of the output will be given an automatically generated delimited name.

      The precision of the output archive controls the number of decimals output
      for floating point numbers and should be sufficiently large (i.e. at least 20)
      if there is a desire to have binary equality between the numbers output and
      those read in.  In general you should expect a loss of precision when going
      from floating point to text and back.

      JSON archives do not output the size information for any dynamically sized structure
      and instead infer it from the number of children for a node.  This means that data
      can be hand edited for dynamic sized structures and will still be readable.  This
      is accomplished through the cereal::SizeTag object, which will cause the archive
      to output the data as a JSON array (e.g. marked by [] instead of {}), which indicates
      that the container is variable sized and may be edited.

      \ingroup Archives */
  class NativeJSONOutputArchive : public OutputArchive<NativeJSONOutputArchive>, public traits::TextArchive
  {
    enum class NodeType { StartObject, InObject, InArray };

    using WriteStream = CEREAL_RAPIDJSON_NAMESPACE::OStreamWrapper;
    using JSONWriter = CEREAL_RAPIDJSON_NAMESPACE::Writer<WriteStream>;

    public:
      /*! @name Common Functionality
          Common use cases for directly interacting with an NativeJSONOutputArchive */
      //! @{

      //! A class containing various advanced options for the JSON archive
      class Options
      {
        public:
          //! Default options
          static Options Default(){ return Options(); }

          //! Specify specific options for the NativeJSONOutputArchive
          /*! @param precision The precision used for floating point numbers
           */
          explicit Options( int precision = std::numeric_limits<double>::max_digits10 )
             : itsPrecision( precision )
          { }

        private:
          friend class NativeJSONOutputArchive;
          int itsPrecision;
      };

      //! Construct, outputting to the provided stream
      /*! @param stream The stream to output to.
          @param options The JSON specific options to use.  See the Options struct
                         for the values of default parameters */
      NativeJSONOutputArchive(std::ostream & stream, Options const & options = Options::Default() ) :
        OutputArchive<NativeJSONOutputArchive>(this),
        itsWriteStream(stream),
        itsWriter(itsWriteStream),
        itsNextName(nullptr)
      {
        itsWriter.SetMaxDecimalPlaces( options.itsPrecision );
        itsNameCounter.push(0);
        itsNodeStack.push(NodeType::StartObject);
      }

      //! Destructor, flushes the JSON
      ~NativeJSONOutputArchive()
      {
        for (; !itsNodeStack.empty(); itsNodeStack.pop())
          if (itsNodeStack.top() == NodeType::InObject)
            itsWriter.EndObject();
          else if (itsNodeStack.top() == NodeType::InArray)
            itsWriter.EndArray();
      }

      //! Saves some binary data, encoded as a base64 string, with an optional name
      /*! This will create a new node, optionally named, and insert a value that consists of
          the data encoded as a base64 string */
      void saveBinaryValue( const void * data, size_t size, const char * name = nullptr )
      {
        setNextName( name );
        writeName();

        auto base64string = base64::encode( reinterpret_cast<const unsigned char *>( data ), size );
        saveValue( base64string );
      };

      //! @}
      /*! @name Internal Functionality
          Functionality designed for use by those requiring control over the inner mechanisms of
          the NativeJSONOutputArchive */
      //! @{

      //! Starts a new node in the JSON output
      /*! The node can optionally be given a name by calling setNextName prior
          to creating the node

          Nodes only need to be started for types that are themselves objects or arrays */
      void startNode()
      {
        writeName();
        itsNodeStack.push(NodeType::StartObject);
        itsNameCounter.push(0);
      }

      //! Designates the most recently added node as finished
      void finishNode()
      {
        // if we ended up serializing an empty object or array, writeName
        // will never have been called - so start and then immediately end
        // the object/array.
        //
        // We'll also end any object/arrays we happen to be in
        switch(itsNodeStack.top())
        {
          case NodeType::InArray:
            itsWriter.EndArray();
            break;
          case NodeType::StartObject:
            itsWriter.StartObject();
          case NodeType::InObject:
            itsWriter.EndObject();
            break;
        }

        itsNodeStack.pop();
        itsNameCounter.pop();
      }

      void maybeStartNodeInArray()
      {
        if (itsNodeStack.top() == NodeType::InArray)
          startNode();
      }

      void maybeFinishNodeInArray()
      {
        // Peek at the parent node.  Currently this is a bit ugly.
        // Probably better to move to use a deque here rather than a
        // pure stack (which is simply an interface constraint on deque
        // anyway) to support this without the pop/restore gymnastics.
        if (itsNodeStack.size() <= 1)
          return;
        auto top = itsNodeStack.top();
        itsNodeStack.pop();
        bool parentIsArray = itsNodeStack.top() == NodeType::InArray;
        itsNodeStack.push(top);
        if (parentIsArray)
          finishNode();
      }

      bool isInArray()
      {
        return itsNodeStack.top() == NodeType::InArray;
      }

      //! Sets the name for the next node created with startNode
      void setNextName( const char * name )
      {
        itsNextName = name;
      }

      //! Saves a bool to the current node
      void saveValue(bool b)                { itsWriter.Bool(b);                                                         }
      //! Saves an int to the current node
      void saveValue(int i)                 { itsWriter.Int(i);                                                          }
      //! Saves a uint to the current node
      void saveValue(unsigned u)            { itsWriter.Uint(u);                                                         }
      //! Saves an int64 to the current node
      void saveValue(int64_t i64)           { itsWriter.Int64(i64);                                                      }
      //! Saves a uint64 to the current node
      void saveValue(uint64_t u64)          { itsWriter.Uint64(u64);                                                     }
      //! Saves a double to the current node
      void saveValue(double d)              { itsWriter.Double(d);                                                       }
      //! Saves a string to the current node
      void saveValue(std::string const & s) { itsWriter.String(s.c_str(), static_cast<rapidjson::SizeType>( s.size() )); }
      //! Saves a const char * to the current node
      void saveValue(char const * s)        { itsWriter.String(s);                                                       }
      //! Saves a nullptr to the current node
      void saveValue(std::nullptr_t)        { itsWriter.Null();                                                          }

    private:
      // Some compilers/OS have difficulty disambiguating the above for various flavors of longs, so we provide
      // special overloads to handle these cases.

      //! 32 bit signed long saving to current node
      template <class T, traits::EnableIf<sizeof(T) == sizeof(std::int32_t),
                                          std::is_signed<T>::value> = traits::sfinae> inline
      void saveLong(T l){ saveValue( static_cast<std::int32_t>( l ) ); }

      //! non 32 bit signed long saving to current node
      template <class T, traits::EnableIf<sizeof(T) != sizeof(std::int32_t),
                                          std::is_signed<T>::value> = traits::sfinae> inline
      void saveLong(T l){ saveValue( static_cast<std::int64_t>( l ) ); }

      //! 32 bit unsigned long saving to current node
      template <class T, traits::EnableIf<sizeof(T) == sizeof(std::int32_t),
                                          std::is_unsigned<T>::value> = traits::sfinae> inline
      void saveLong(T lu){ saveValue( static_cast<std::uint32_t>( lu ) ); }

      //! non 32 bit unsigned long saving to current node
      template <class T, traits::EnableIf<sizeof(T) != sizeof(std::int32_t),
                                          std::is_unsigned<T>::value> = traits::sfinae> inline
      void saveLong(T lu){ saveValue( static_cast<std::uint64_t>( lu ) ); }

    public:
#ifdef _MSC_VER
      //! MSVC only long overload to current node
      void saveValue( unsigned long lu ){ saveLong( lu ); };
#else // _MSC_VER
      //! Serialize a long if it would not be caught otherwise
      template <class T, traits::EnableIf<std::is_same<T, long>::value,
                                          !std::is_same<T, std::int32_t>::value,
                                          !std::is_same<T, std::int64_t>::value> = traits::sfinae> inline
      void saveValue( T t ){ saveLong( t ); }

      //! Serialize an unsigned long if it would not be caught otherwise
      template <class T, traits::EnableIf<std::is_same<T, unsigned long>::value,
                                          !std::is_same<T, std::uint32_t>::value,
                                          !std::is_same<T, std::uint64_t>::value> = traits::sfinae> inline
      void saveValue( T t ){ saveLong( t ); }
#endif // _MSC_VER

      //! Save exotic arithmetic as strings to current node
      /*! Handles long long (if distinct from other types), unsigned long (if distinct), and long double */
      template <class T, traits::EnableIf<std::is_arithmetic<T>::value,
                                          !std::is_same<T, long>::value,
                                          !std::is_same<T, unsigned long>::value,
                                          !std::is_same<T, std::int64_t>::value,
                                          !std::is_same<T, std::uint64_t>::value,
                                          (sizeof(T) >= sizeof(long double) || sizeof(T) >= sizeof(long long))> = traits::sfinae> inline
      void saveValue(T const & t)
      {
        std::stringstream ss; ss.precision( std::numeric_limits<long double>::max_digits10 );
        ss << t;
        saveValue( ss.str() );
      }

      //! Write the name of the upcoming node and prepare object/array state
      /*! Since writeName is called for every value that is output, regardless of
          whether it has a name or not, it is the place where we will do a deferred
          check of our node state and decide whether we are in an array or an object.

          The general workflow of saving to the JSON archive is:

            1. (optional) Set the name for the next node to be created, usually done by an NVP
            2. Start the node
            3. (if there is data to save) Write the name of the node (this function)
            4. (if there is data to save) Save the data (with saveValue)
            5. Finish the node
          */
      void writeName()
      {
        NodeType const & nodeType = itsNodeStack.top();

        // Start up an object if necessary
        if(nodeType == NodeType::StartObject)
        {
          itsNodeStack.top() = NodeType::InObject;
          itsWriter.StartObject();
        }

        // Array types do not output names
        if(nodeType == NodeType::InArray) return;

        if(itsNextName == nullptr)
        {
          std::string name = "value" + std::to_string( itsNameCounter.top()++ ) + "\0";
          saveValue(name);
        }
        else
        {
          saveValue(itsNextName);
          itsNextName = nullptr;
        }
      }

      //! Make the current node an array type and open it
      void startArray()
      {
        itsWriter.StartArray();
        itsNodeStack.top() = NodeType::InArray;
      }

      JSONWriter& writer() { return itsWriter; }

      //! @}

    private:
      WriteStream itsWriteStream;          //!< Rapidjson write stream
      JSONWriter itsWriter;                //!< Rapidjson writer
      char const * itsNextName;            //!< The next name
      std::stack<uint32_t> itsNameCounter; //!< Counter for creating unique names for unnamed nodes
      std::stack<NodeType> itsNodeStack;
  }; // NativeJSONOutputArchive

  // ######################################################################
  //! An input archive designed to load data from JSON
  /*! This archive uses RapidJSON to read in a JSON archive to a conforming C++ object.

      Input JSON should have been produced by the NativeJSONOutputArchive.  Data can
      only be added to dynamically sized containers (marked by JSON arrays) -
      the input archive will determine their size by looking at the number of child nodes.
      Only JSON originating from a NativeJSONOutputArchive is officially supported, but data
      from other sources may work if properly formatted.

      The NativeJSONInputArchive does not require that nodes are loaded in the same
      order they were saved by NativeJSONOutputArchive.  Using name value pairs (NVPs),
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
  class NativeJSONInputArchive : public InputArchive<NativeJSONInputArchive>, public traits::TextArchive
  {
    private:
      using ReadStream = CEREAL_RAPIDJSON_NAMESPACE::IStreamWrapper;
      typedef CEREAL_RAPIDJSON_NAMESPACE::GenericValue<CEREAL_RAPIDJSON_NAMESPACE::UTF8<>> JSONValue;
      typedef JSONValue::ConstMemberIterator MemberIterator;
      typedef JSONValue::ConstValueIterator ValueIterator;
      typedef CEREAL_RAPIDJSON_NAMESPACE::Document::GenericValue GenericValue;

    public:
      /*! @name Common Functionality
          Common use cases for directly interacting with an NativeJSONInputArchive */
      //! @{

      //! Construct, reading from the provided stream
      /*! @param stream The stream to read from */
      NativeJSONInputArchive(std::istream & stream) :
        InputArchive<NativeJSONInputArchive>(this),
        itsNextName( nullptr ),
        itsReadStream(stream),
        itsRootElementStarted(false)
      {
        itsDocument.ParseStream<0>(itsReadStream);
        if(itsDocument.IsArray())
          itsIteratorStack.emplace_back(itsDocument.Begin(), itsDocument.End());
        else if(itsDocument.IsObject())
          itsIteratorStack.emplace_back(itsDocument.MemberBegin(), itsDocument.MemberEnd());
        else
          throw std::runtime_error("Not JSON Array or Object");
      }

      //! Loads some binary data, encoded as a base64 string
      /*! This will automatically start and finish a node to load the data, and can be called directly by
          users.

          Note that this follows the same ordering rules specified in the class description in regards
          to loading in/out of order */
      void loadBinaryValue( void * data, size_t size, const char * name = nullptr )
      {
        itsNextName = name;

        std::string encoded;
        loadValue( encoded );
        auto decoded = base64::decode( encoded );

        if( size != decoded.size() )
          throw Exception("Decoded binary data size does not match specified size");

        std::memcpy( data, decoded.data(), decoded.size() );
        itsNextName = nullptr;
      };

    private:
      //! @}
      /*! @name Internal Functionality
          Functionality designed for use by those requiring control over the inner mechanisms of
          the NativeJSONInputArchive */
      //! @{

      //! An internal iterator that handles both array and object types
      /*! This class is a variant and holds both types of iterators that
          rapidJSON supports - one for arrays and one for objects. */
      class Iterator
      {
        public:
          Iterator() : itsIndex( 0 ), itsType(Null_) {}

          Iterator(MemberIterator begin, MemberIterator end) :
            itsMemberItBegin(begin), itsMemberItEnd(end), itsIndex(0), itsType(Member)
          { }

          Iterator(ValueIterator begin, ValueIterator end) :
            itsValueItBegin(begin), itsValueItEnd(end), itsIndex(0), itsType(Value)
          { }

          //! Advance to the next node
          Iterator & operator++()
          {
            ++itsIndex;
            return *this;
          }

          //! For a value iterator over an array, return the number of
          //  values present in the JSON array.  Returns 0 for
          //  non-array types.
          size_type arraySize() const
          {
            if (itsType != Value)
               return 0;
            return itsValueItEnd - itsValueItBegin;
          }

          bool atBegin() const
          {
            return itsIndex == 0;
          }

          bool isValueType() const
          {
            return itsType == Value;
          }

          bool atEnd() const
          {
            switch (itsType)
            {
               case Value: return (long) itsIndex == itsValueItEnd - itsValueItBegin;
               case Member: return (long) itsIndex == itsMemberItEnd - itsMemberItBegin;
               default: throw cereal::Exception("Invalid Iterator Type!");
            }
          }

          //! Get the value of the current node
          GenericValue const & value() const
          {
            switch(itsType)
            {
              case Value : return itsValueItBegin[itsIndex];
              case Member: return itsMemberItBegin[itsIndex].value;
              default: throw cereal::Exception("Invalid Iterator Type!");
            }
          }

          //! Get the name of the current node, or nullptr if it has no name
          const char * name() const
          {
            if( itsType == Member && (itsMemberItBegin + itsIndex) != itsMemberItEnd )
              return itsMemberItBegin[itsIndex].name.GetString();
            else
              return nullptr;
          }

          //! Adjust our position such that we are at the node with the given name
          /*! @throws Exception if no such named node exists */
          bool search( const char * searchName )
          {
            if( itsType == Value )
              return true;

            int index = findName( searchName );
            if( index >= 0 )
            {
              itsIndex = index;
              return true;
            }

            return false;
          }

          int findName( const char * searchName ) const
          {
            const auto len = std::strlen( searchName );
            int index = 0;
            for( auto it = itsMemberItBegin; it != itsMemberItEnd; ++it, ++index )
            {
              const auto currentName = it->name.GetString();
              if( ( std::strncmp( searchName, currentName, len ) == 0 ) &&
                  ( std::strlen( currentName ) == len ) )
                return index;
            }

            return -1;
          }

        private:
          MemberIterator itsMemberItBegin, itsMemberItEnd; //!< The member iterator (object)
          ValueIterator itsValueItBegin, itsValueItEnd;    //!< The value iterator (array)
          size_t itsIndex;                                 //!< The current index of this iterator
          enum Type {Value, Member, Null_} itsType;    //!< Whether this holds values (array) or members (objects) or nothing
      };

    public:
      //! Searches for the expectedName node if it doesn't match the actualName
      /*! This needs to be called before every load or node start occurs.  This function will
          check to see if an NVP has been provided (with setNextName) and if so, see if that name matches the actual
          next name given.  If the names do not match, it will search in the current level of the JSON for that name.
          If the name is not found, an exception will be thrown.

          Resets the NVP name after called.

          @throws Exception if an expectedName is given and not found */
      inline bool search()
      {
        bool ok = true;

        // The name an NVP provided with setNextName()
        if( itsNextName )
        {
          // The actual name of the current node
          auto const actualName = itsIteratorStack.back().name();

          // Do a search if we don't see a name coming up, or if the names don't match
          if( !actualName || std::strcmp( itsNextName, actualName ) != 0 )
          {
            ok = itsIteratorStack.back().search( itsNextName );

            // TODO: if !found && `throw-if-not-found-option`
            // TODO:   throw Exception("JSON Parsing failed - provided NVP not found");
          }
        }

        itsNextName = nullptr;

        return ok;
      }

      /// As per search but doesn't mutate anything; can be used to check for optional/unordered fields in a loader.
      inline bool hasName( const char* name ) const
      {
        // The name an NVP provided with setNextName()
        if( name && !itsIteratorStack.empty() )
        {
          // The actual name of the current node
          auto const actualName = itsIteratorStack.back().name();

          // Do a search if we don't see a name coming up, or if the names don't match
          if( !actualName || std::strcmp( name, actualName ) != 0 )
            return itsIteratorStack.back().findName( name ) >= 0;

          return true;
        }

        return false;
      }

      //! Starts a new node, going into its proper iterator
      /*! This places an iterator for the next node to be parsed onto the iterator stack.  If the next
          node is an array, this will be a value iterator, otherwise it will be a member iterator.

          By default our strategy is to start with the document root node and then recursively iterate through
          all children in the order they show up in the document.
          We don't need to know NVPs to do this; we'll just blindly load in the order things appear in.

          If we were given an NVP, we will search for it if it does not match our the name of the next node
          that would normally be loaded.  This functionality is provided by search(). */
      bool startNode()
      {
        if (!search())
          return false;

        if(itsIteratorStack.back().value().IsArray())
          itsIteratorStack.emplace_back(itsIteratorStack.back().value().Begin(), itsIteratorStack.back().value().End());
        else if(itsIteratorStack.back().value().IsObject())
          itsIteratorStack.emplace_back(itsIteratorStack.back().value().MemberBegin(), itsIteratorStack.back().value().MemberEnd());
        else
          return false;

        return true;
      }

      //! Finishes the most recently started node
      void finishNode()
      {
        itsIteratorStack.pop_back();
        if (!itsIteratorStack.empty())
           ++itsIteratorStack.back();
      }

      void maybeStartNodeInArray()
      {
        // The appropriate type (array or object) for the root element
        // has already been opened.  This is called in the prologue.
        // For the root element, the actual 'process'ing of the
        // element will not have started.  In that case do nothing
        // here other than mark that the root element is now being
        // processed.
        // FIXME: This works for current use cases.  But there's a
        // FIXME: DisableIf selecting calls to this; need to ensure
        // FIXME: that this is marked appropriately always -- or a
        // FIXME: better solution adopted.
        if (!itsRootElementStarted)
        {
          itsRootElementStarted = true;
          return;
        }
        if (itsIteratorStack.back().arraySize())
          startNode();
      }

      void maybeFinishNodeInArray()
      {
        if (itsIteratorStack.size() > 1 && (itsIteratorStack.rbegin() + 1)->arraySize())
          finishNode();
      }

      bool atEndOfCurrentScope() const
      {
        if (itsIteratorStack.empty())
           return true;
        return itsIteratorStack.back().atEnd();
      }

      //! Retrieves the current node name
      /*! @return nullptr if no name exists */
      const char * getNodeName() const
      {
        return itsIteratorStack.back().name();
      }

      //! Sets the name for the next node created with startNode
      void setNextName( const char * name )
      {
        itsNextName = name;
      }

      //! Loads a value from the current node - small signed overload
      template <class T, traits::EnableIf<std::is_signed<T>::value,
                                          sizeof(T) < sizeof(int64_t)> = traits::sfinae> inline
      void loadValue(T & val)
      {
        if (!search())
          return;

        val = static_cast<T>( itsIteratorStack.back().value().GetInt() );
        ++itsIteratorStack.back();
      }

      //! Loads a value from the current node - small unsigned overload
      template <class T, traits::EnableIf<std::is_unsigned<T>::value,
                                          sizeof(T) < sizeof(uint64_t),
                                          !std::is_same<bool, T>::value> = traits::sfinae> inline
      void loadValue(T & val)
      {
        if (!search())
          return;

        val = static_cast<T>( itsIteratorStack.back().value().GetUint() );
        ++itsIteratorStack.back();
      }

      //! Loads a value from the current node - bool overload
      void loadValue(bool & val)        { if (!search()) return; val = itsIteratorStack.back().value().GetBool(); ++itsIteratorStack.back(); }
      //! Loads a value from the current node - int64 overload
      void loadValue(int64_t & val)     { if (!search()) return; val = itsIteratorStack.back().value().GetInt64(); ++itsIteratorStack.back(); }
      //! Loads a value from the current node - uint64 overload
      void loadValue(uint64_t & val)    { if (!search()) return; val = itsIteratorStack.back().value().GetUint64(); ++itsIteratorStack.back(); }
      //! Loads a value from the current node - float overload
      void loadValue(float & val)       { if (!search()) return; val = static_cast<float>(itsIteratorStack.back().value().GetDouble()); ++itsIteratorStack.back(); }
      //! Loads a value from the current node - double overload
      void loadValue(double & val)      { if (!search()) return; val = itsIteratorStack.back().value().GetDouble(); ++itsIteratorStack.back(); }
      //! Loads a value from the current node - string overload
      void loadValue(std::string & val) { if (!search()) return; val = itsIteratorStack.back().value().GetString(); ++itsIteratorStack.back(); }
      //! Loads a nullptr from the current node
      void loadValue(std::nullptr_t&)   { search(); CEREAL_RAPIDJSON_ASSERT(itsIteratorStack.back().value().IsNull()); ++itsIteratorStack.back(); }

      // Special cases to handle various flavors of long, which tend to conflict with
      // the int32_t or int64_t on various compiler/OS combinations.  MSVC doesn't need any of this.
      #ifndef _MSC_VER
    private:
      //! 32 bit signed long loading from current node
      template <class T> inline
      typename std::enable_if<sizeof(T) == sizeof(std::int32_t) && std::is_signed<T>::value, void>::type
      loadLong(T & l){ loadValue( reinterpret_cast<std::int32_t&>( l ) ); }

      //! non 32 bit signed long loading from current node
      template <class T> inline
      typename std::enable_if<sizeof(T) == sizeof(std::int64_t) && std::is_signed<T>::value, void>::type
      loadLong(T & l){ loadValue( reinterpret_cast<std::int64_t&>( l ) ); }

      //! 32 bit unsigned long loading from current node
      template <class T> inline
      typename std::enable_if<sizeof(T) == sizeof(std::uint32_t) && !std::is_signed<T>::value, void>::type
      loadLong(T & lu){ loadValue( reinterpret_cast<std::uint32_t&>( lu ) ); }

      //! non 32 bit unsigned long loading from current node
      template <class T> inline
      typename std::enable_if<sizeof(T) == sizeof(std::uint64_t) && !std::is_signed<T>::value, void>::type
      loadLong(T & lu){ loadValue( reinterpret_cast<std::uint64_t&>( lu ) ); }

    public:
      //! Serialize a long if it would not be caught otherwise
      template <class T> inline
      typename std::enable_if<std::is_same<T, long>::value &&
                              !std::is_same<T, std::int32_t>::value &&
                              !std::is_same<T, std::int64_t>::value, void>::type
      loadValue( T & t ){ loadLong(t); }

      //! Serialize an unsigned long if it would not be caught otherwise
      template <class T> inline
      typename std::enable_if<std::is_same<T, unsigned long>::value &&
                              !std::is_same<T, std::uint32_t>::value &&
                              !std::is_same<T, std::uint64_t>::value, void>::type
      loadValue( T & t ){ loadLong(t); }
      #endif // _MSC_VER

    private:
      //! Convert a string to a long long
      void stringToNumber( std::string const & str, long long & val ) { val = std::stoll( str ); }
      //! Convert a string to an unsigned long long
      void stringToNumber( std::string const & str, unsigned long long & val ) { val = std::stoull( str ); }
      //! Convert a string to a long double
      void stringToNumber( std::string const & str, long double & val ) { val = std::stold( str ); }

    public:
      //! Loads a value from the current node - long double and long long overloads
      template <class T, traits::EnableIf<std::is_arithmetic<T>::value,
                                          !std::is_same<T, long>::value,
                                          !std::is_same<T, unsigned long>::value,
                                          !std::is_same<T, std::int64_t>::value,
                                          !std::is_same<T, std::uint64_t>::value,
                                          (sizeof(T) >= sizeof(long double) || sizeof(T) >= sizeof(long long))> = traits::sfinae>
      inline void loadValue(T & val)
      {
        std::string encoded;
        loadValue( encoded );
        stringToNumber( encoded, val );
      }

      //! Loads the size for a SizeTag
      void loadSize(size_type & size)
      {
        auto it = itsIteratorStack.rbegin(), end = itsIteratorStack.rend();
        while (it != end
               && !it->isValueType()
               && it->atBegin())
          ++it;
        if (it != end)
          size = it->arraySize();
      }

      //! @}

    private:
      const char * itsNextName;               //!< Next name set by NVP
      ReadStream itsReadStream;               //!< Rapidjson write stream
      std::vector<Iterator> itsIteratorStack; //!< 'Stack' of rapidJSON iterators
      rapidjson::Document itsDocument;        //!< Rapidjson document
      bool itsRootElementStarted;             //!< Whether processing of the root element has started
  };

  namespace traits
  {
     template <typename A>
     struct is_native_json_archive
     {
        static constexpr bool value = std::is_same<A, NativeJSONInputArchive>::value
           || std::is_same<A, NativeJSONOutputArchive>::value;
     };

     /*! Enumeration types are handled by cereal internally, but it's
         still necessary to specialize their NVP prologue.  Hence the
         trait split here. */
     template <typename T>
     struct is_non_enum_scalar
     {
        static constexpr bool value = std::is_arithmetic<std::decay_t<T>>::value
           || std::is_same<std::decay_t<T>, std::string>::value
           || std::is_same<std::decay_t<T>, std::nullptr_t>::value
           ;
     };
     template <typename T>
     struct is_scalar
     {
        static constexpr bool value = is_non_enum_scalar<T>::value
           || std::is_enum<std::decay_t<T>>::value
           ;
     };
     template <typename T>
     struct is_scalar_or_minimal
     {
        static constexpr bool value = is_scalar<T>::value
           || has_minimal_input_serialization<std::decay_t<T>, NativeJSONInputArchive>::value
           || has_minimal_output_serialization<std::decay_t<T>, NativeJSONOutputArchive>::value
           ;

        static_assert(has_minimal_input_serialization<std::decay_t<T>, NativeJSONInputArchive>::value
           == has_minimal_output_serialization<std::decay_t<T>, NativeJSONOutputArchive>::value,
           "has_minimal_{in,out}put_serialization must agree");
     };
  };


  // ######################################################################
  // JSONArchive prologue and epilogue functions
  // ######################################################################

  template <typename T>
     struct is_nvp : std::false_type {};
  template <typename T>
     struct is_nvp<NameValuePair<T>> : std::true_type {};

  template <class T, traits::DisableIf<is_nvp<T>::value || traits::is_scalar_or_minimal<T>::value> = traits::sfinae> inline
  void prologue( NativeJSONOutputArchive &ar, T const & )
  { ar.maybeStartNodeInArray(); }

  template <class T, traits::DisableIf<is_nvp<T>::value || traits::is_scalar_or_minimal<T>::value> = traits::sfinae> inline
  void prologue( NativeJSONInputArchive & ar, T const & )
  { ar.maybeStartNodeInArray(); }

  // ######################################################################
  template <class T, traits::DisableIf<is_nvp<T>::value || traits::is_scalar_or_minimal<T>::value> = traits::sfinae> inline
  void epilogue( NativeJSONOutputArchive & ar, T const & )
  { ar.maybeFinishNodeInArray(); }

  template <class T, traits::DisableIf<is_nvp<T>::value || traits::is_scalar_or_minimal<T>::value> = traits::sfinae> inline
  void epilogue( NativeJSONInputArchive & ar, T const & )
  { ar.maybeFinishNodeInArray(); }

  // ######################################################################
  /*! Since native JSON archives serialize by name, there is no need to finish
      a base object node as its members are part of the derived object.  */
  template <class T> inline
  void epilogue( NativeJSONOutputArchive &, base_class<T> const & )
  { }

  template <class T> inline
  void epilogue( NativeJSONInputArchive &, base_class<T> const & )
  { }

  // ######################################################################
  //! Prologue for NVPs for JSON archives
  template <class T, traits::DisableIf<traits::is_scalar_or_minimal<T>::value> = traits::sfinae> inline
  void prologue( NativeJSONOutputArchive &, NameValuePair<T> const & )
  { }

  //! Prologue for NVPs for JSON archives
  template <class T, traits::DisableIf<traits::is_scalar_or_minimal<T>::value> = traits::sfinae> inline
  void prologue( NativeJSONInputArchive &, NameValuePair<T> const & )
  { }

  // ######################################################################
  //! Epilogue for NVPs for JSON archives
  template <class T, traits::DisableIf<traits::is_scalar_or_minimal<T>::value> = traits::sfinae> inline
  void epilogue( NativeJSONOutputArchive & ar, NameValuePair<T> const & )
  { ar.finishNode(); }

  //! Epilogue for NVPs for JSON archives
  template <class T, traits::DisableIf<traits::is_scalar_or_minimal<T>::value> = traits::sfinae> inline
  void epilogue( NativeJSONInputArchive & ar, NameValuePair<T> const & )
  { ar.finishNode(); }

  // ######################################################################
  //! Prologue for SizeTags for JSON archives
  template <class T> inline
  void prologue( NativeJSONOutputArchive &, SizeTag<T> const & )
  { }

  //! Prologue for SizeTags for JSON archives
  template <class T> inline
  void prologue( NativeJSONInputArchive &, SizeTag<T> const & )
  { }

  // ######################################################################
  //! Epilogue for SizeTags for JSON archives
  template <class T> inline
  void epilogue( NativeJSONOutputArchive &, SizeTag<T> const & )
  { }

  //! Epilogue for SizeTags for JSON archives
  template <class T> inline
  void epilogue( NativeJSONInputArchive &, SizeTag<T> const & )
  { }

  // ######################################################################
  //! Prologue for non-enum scalar types for JSON archives
  template <class T, traits::EnableIf<traits::is_non_enum_scalar<T>::value> = traits::sfinae> inline
  void prologue( NativeJSONOutputArchive & ar, T const & )
  {
    ar.writeName();
  }

  //! Prologue for non-enum scalar types for JSON archives
  template <class T, traits::EnableIf<traits::is_non_enum_scalar<T>::value> = traits::sfinae> inline
  void prologue( NativeJSONInputArchive &, T const & )
  { }

  // ######################################################################
  //! Epilogue for non-enum scalar types for JSON archives
  template <class T, traits::EnableIf<traits::is_non_enum_scalar<T>::value> = traits::sfinae> inline
  void epilogue( NativeJSONOutputArchive &, T const & )
  { }

  //! Epilogue for non-enum scalar types for JSON archives
  template <class T, traits::EnableIf<traits::is_non_enum_scalar<T>::value> = traits::sfinae> inline
  void epilogue( NativeJSONInputArchive &, T const & )
  { }


  // ######################################################################
  // Common JSONArchive serialization functions
  // ######################################################################
  //! Serializing NVP types to JSON
  template <class T> inline
  void CEREAL_SAVE_FUNCTION_NAME( NativeJSONOutputArchive & ar, NameValuePair<T> const & t )
  {
    ar.setNextName( t.name );
    if (!traits::is_scalar_or_minimal<T>::value)
       ar.startNode();
    ar( t.value );
  }

  template <class T> inline
  void CEREAL_LOAD_FUNCTION_NAME( NativeJSONInputArchive & ar, NameValuePair<T> & t )
  {
    ar.setNextName( t.name );
    // Only continue to load the value if the name is found and, if necessary,
    // a new node (array or object) is started.
    if (traits::is_scalar_or_minimal<T>::value)
    {
       if (!ar.search())
          return;
    }
    else if (!ar.startNode())
       return;
    ar( t.value );
  }

  //! Saving for non-enum scalars to JSON
  template <class T, traits::EnableIf<traits::is_non_enum_scalar<T>::value> = traits::sfinae> inline
  void CEREAL_SAVE_FUNCTION_NAME(NativeJSONOutputArchive & ar, T const & t)
  {
    ar.saveValue( t );
  }

  //! Loading non-enum scalars from JSON
  template <class T, traits::EnableIf<traits::is_non_enum_scalar<T>::value> = traits::sfinae> inline
  void CEREAL_LOAD_FUNCTION_NAME(NativeJSONInputArchive & ar, T & t)
  {
    ar.loadValue( t );
  }

  // ######################################################################
  //! Saving SizeTags to JSON
  template <class T> inline
  void CEREAL_SAVE_FUNCTION_NAME( NativeJSONOutputArchive & ar, SizeTag<T> const & )
  {
    ar.startArray();
  }

  //! Loading SizeTags from JSON
  template <class T> inline
  void CEREAL_LOAD_FUNCTION_NAME( NativeJSONInputArchive & ar, SizeTag<T> & st )
  {
    ar.loadSize( st.size );
  }

  // specialized container serialization

  template <typename T> inline T json_key_to( const char* key ) { return key; }
  template <typename T> inline decltype(auto) json_key_from( T&& key ) { return key; }
  template <> inline int json_key_to<int>( const char* key ) { return atoi(key); }
  inline std::string json_key_from( int key ) { return std::to_string(key); }

  //! Saving for standard-like associative types with scalar keys
  template <template <typename...> class Map, typename... Args, typename = typename Map<Args...>::mapped_type>
  inline void save( NativeJSONOutputArchive& ar, Map<Args...> const& map )
  {
    static_assert( traits::is_scalar<typename Map<Args...>::key_type>::value,
                   "Map key must be scalar for native JSON serialization" );
    for( auto const& e : map )
      ar( cereal::make_nvp( json_key_from( e.first ), e.second ) );
  }

  //! Loading for standard-like associative types with scalar keys
  template <template <typename...> class Map, typename... Args, typename = typename Map<Args...>::mapped_type>
  void load( NativeJSONInputArchive& ar, Map<Args...>& map )
  {
    static_assert( traits::is_scalar<typename Map<Args...>::key_type>::value,
                   "Map key must be scalar for native JSON serialization" );

    map.clear();

    auto hint = map.begin();
    while( true )
    {
      const auto namePtr = ar.getNodeName();

      if( !namePtr )
        break;

      auto key = json_key_to<typename Map<Args...>::key_type>( namePtr );
      typename Map<Args...>::mapped_type value; ar( value );
      hint = map.emplace_hint( hint, std::move( key ), std::move( value ) );
    }
  }

} // namespace cereal

// register archives for polymorphic support
CEREAL_REGISTER_ARCHIVE(cereal::NativeJSONInputArchive)
CEREAL_REGISTER_ARCHIVE(cereal::NativeJSONOutputArchive)

// tie input and output archives together
CEREAL_SETUP_ARCHIVE_TRAITS(cereal::NativeJSONInputArchive, cereal::NativeJSONOutputArchive)

#endif // CEREAL_ARCHIVES_NATIVE_JSON_HPP_
