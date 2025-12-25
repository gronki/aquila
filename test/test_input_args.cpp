#include <memory>
#include <sstream>
#include <string>
#include <tuple>

#include "../src_cpp/lib/interpreter/bind_args.hpp"
#include "../src_cpp/lib/interpreter/operation.hpp"
#include "../src_cpp/lib/interpreter/value.hpp"

#include "testmacros.hpp"

using namespace aquila;
using namespace aquila::interpreter;

// class TestOperation : public Operation
// {
// public:
//     BIND_ARGS(&TestOperation::run);

//     std::unique_ptr<Value> run(const RealValue &a, const IntValue &b, const StrValue &s) const
//     {
//         std::stringstream ss;
//         ss << "I got an float " << a << ", int " << b << " and string " << s << ".";
//         return std::make_unique<StrValue>(ss.str());
//     }

//     std::optional<ArgListManifest> arg_manifest() const override
//     {
//         return ArgListManifest{
//             {.name = "a"},
//             {.name = "b", .default_int = 3},
//             {.name = "c", .default_str = "none"},
//         };
//     }
// };

TEST(match1)
{
    std::vector<ArgSpec> manifest{
        ArgSpec{.name = "a"},
        ArgSpec{.name = "b", .default_int = 3},
        ArgSpec{.name = "c", .default_real = 6},
    };
    auto match = match_arguments(manifest, {"", "c"});

    REQUIRE_EQ(match.size(), 3);

    REQUIRE(match[0].matched);
    REQUIRE_EQ(match[0].pos, 0);
    REQUIRE(match[0].deftgt == nullptr);

    REQUIRE(!match[1].matched);
    REQUIRE(match[1].deftgt != nullptr);

    REQUIRE(match[2].matched);
    REQUIRE_EQ(match[2].pos, 1);
    REQUIRE(match[2].deftgt == nullptr);
}

TEST(match2)
{
    std::vector<ArgSpec> manifest{
        ArgSpec{.name = "a"},
        ArgSpec{.name = "b", .default_int = 3},
        ArgSpec{.name = "c", .default_real = 6},
    };
    auto match = match_arguments(manifest, {"b", "a"});

    REQUIRE_EQ(match.size(), 3);

    REQUIRE(match[0].matched);
    REQUIRE_EQ(match[0].pos, 1);
    REQUIRE(match[0].deftgt == nullptr);

    REQUIRE(match[1].matched);
    REQUIRE_EQ(match[1].pos, 0);
    REQUIRE(match[1].deftgt == nullptr);

    REQUIRE(!match[2].matched);
    REQUIRE(match[2].deftgt != nullptr);
}

TEST(match_e1)
{
    std::vector<ArgSpec> manifest{
        ArgSpec{.name = "a"},
        ArgSpec{.name = "b", .default_int = 3},
        ArgSpec{.name = "c", .default_real = 6},
    };

    EXPECT_ERROR("required", [&]() { match_arguments(manifest, {}); });
}

TEST(match_e2)
{
    std::vector<ArgSpec> manifest{
        ArgSpec{.name = "a"},
        ArgSpec{.name = "b", .default_int = 3},
        ArgSpec{.name = "c", .default_real = 6},
    };

    EXPECT_ERROR("follow", [&]() { match_arguments(manifest, {"a", ""}); });
}
TEST(match_e3)
{
    std::vector<ArgSpec> manifest{
        ArgSpec{.name = "a"},
        ArgSpec{.name = "b", .default_int = 3},
        ArgSpec{.name = "c", .default_real = 6},
    };

    EXPECT_ERROR("twice", [&]() { match_arguments(manifest, {"", "", "a"}); });
}

int main()
{
    int failed = 0;
    RUN_ALL(failed);
    return failed != 0;
}