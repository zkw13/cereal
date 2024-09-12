#include <cereal/types/vector.hpp>
#include <cereal/types/string.hpp>

struct Test
{
    std::vector<std::string> abc;
    std::vector<std::string> def;
    std::vector<std::string> ghi;

    template <class Archive>
    void serialize(Archive& archive)
    {
        // CEREAL_OPTIONAL_NVP(archive, abc);
        // CEREAL_OPTIONAL_NVP(archive, def);
        // CEREAL_OPTIONAL_NVP(archive, ghi);
        // CEREAL_OPTIONAL_ONVP(archive, *this, abc);
        // CEREAL_OPTIONAL_ONVP(archive, *this, def);
        // CEREAL_OPTIONAL_ONVP(archive, *this, ghi);
        auto& [l_abc, l_def, l_ghi] = *this;
        CEREAL_OPTIONAL_PNVP(archive, l_, abc);
        CEREAL_OPTIONAL_PNVP(archive, l_, def);
        CEREAL_OPTIONAL_PNVP(archive, l_, ghi);
    }
};

#include <cereal/archives/native_json.hpp>

void test(std::string s)
{
    try {
        std::cerr << s << ": " << std::flush;
        std::istringstream iss(move(s));
        cereal::NativeJSONInputArchive ar(iss);
        Test t;
        ar >> t;
        std::cerr << "OK: " << t.abc.size() << ", " << t.def.size() << ", " << t.ghi.size() << "\n";
    }
    catch (std::exception& e) {
        std::cerr << "FAIL: " << e.what() << "\n";
    }
}

int main()
{
    for (auto s : {
        R"( { "abc": ["1", "2", "3"], "def": ["4", "5"], "ghi": ["6"] } )",
        R"( { "abc": ["1", "2", "3"], "ghi": ["4", "5"], "def": ["6"] } )",
        R"( { "def": ["1", "2", "3"], "abc": ["4", "5"], "ghi": ["6"] } )",
        R"( { "def": ["1", "2", "3"], "ghi": ["4", "5"], "abc": ["6"] } )",
        R"( { "ghi": ["1", "2", "3"], "def": ["4", "5"], "abc": ["6"] } )",
        R"( { "ghi": ["1", "2", "3"], "abc": ["4", "5"], "def": ["6"] } )",
        R"( { "abc": ["1", "2", "3"], "def": ["4", "5"] } )",
        R"( { "abc": ["1", "2", "3"], "ghi": ["4", "5"] } )",
        R"( { "def": ["1", "2", "3"], "abc": ["4", "5"] } )",
        R"( { "def": ["1", "2", "3"], "ghi": ["4", "5"] } )",
        R"( { "ghi": ["1", "2", "3"], "abc": ["4", "5"] } )",
        R"( { "ghi": ["1", "2", "3"], "def": ["4", "5"] } )",
        R"( { "abc": ["1", "2", "3"] } )",
        R"( { "def": ["1", "2", "3"] } )",
        R"( { "ghi": ["1", "2", "3"] } )",
        R"( { } )",
    }) test(s);

    std::cerr << "COMPLETE.\n";
}