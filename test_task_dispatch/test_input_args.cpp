#include <memory>
#include <sstream>
#include <string>
#include <tuple>

#include "../src_cpp/interpreter/bind_args.hpp"
#include "../src_cpp/interpreter/operation.hpp"
#include "../src_cpp/interpreter/value.hpp"

#include "testmacros.hpp"

using namespace aquila;
using namespace aquila::interpreter;

void print_match(const std::vector<ArgMatch> &match)
{
    for (const auto &match_item : match)
    {
        std::cout << (match_item.matched ? "MATCHED" : "      ")
                  << "   pos=" << match_item.pos << "    "
                  << (match_item.deftgt != nullptr ? "DEFAULT" : "") << std::endl;
    }
}

TEST(match1)
{
    std::vector<ArgSpec> manifest{
        ArgSpec{.name = "a"},
        ArgSpec{.name = "b", .default_int = 3},
        ArgSpec{.name = "c", .default_real = 6},
    };
    auto match = match_arguments(manifest, {"", "c"});
    print_match(match);

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

TEST(match1a)
{
    std::vector<ArgSpec> manifest{
        ArgSpec{.name = "a"},
        ArgSpec{.name = "b", .default_int = 3},
    };
    EXPECT_ERROR("many positional", [&]() { match_arguments(manifest, {"", ""}); });
}

TEST(match2)
{
    std::vector<ArgSpec> manifest{
        ArgSpec{.name = "a"},
        ArgSpec{.name = "b", .default_int = 3},
        ArgSpec{.name = "c", .default_real = 6},
    };
    EXPECT_ERROR(
        "required but not provided", [&]() { match_arguments(manifest, {"b", "c"}); });
}

TEST(match_ellip_0)
{
    std::vector<ArgSpec> manifest{
        ArgSpec{.name = "..."},
    };
    auto match = match_arguments(manifest, {"", "", ""});
    print_match(match);

    REQUIRE_EQ(match.size(), 3);

    REQUIRE(match[0].matched);
    REQUIRE_EQ(match[0].pos, 0);
    REQUIRE(match[0].deftgt == nullptr);

    REQUIRE(match[1].matched);
    REQUIRE_EQ(match[1].pos, 1);
    REQUIRE(match[1].deftgt == nullptr);

    REQUIRE(match[2].matched);
    REQUIRE_EQ(match[2].pos, 2);
    REQUIRE(match[2].deftgt == nullptr);
}

TEST(match_ellip_1)
{
    std::vector<ArgSpec> manifest{
        ArgSpec{.name = "a"},
        ArgSpec{.name = "..."},
    };
    auto match = match_arguments(manifest, {"", "", ""});
    print_match(match);

    REQUIRE_EQ(match.size(), 3);

    REQUIRE(match[0].matched);
    REQUIRE_EQ(match[0].pos, 0);
    REQUIRE(match[0].deftgt == nullptr);

    REQUIRE(match[1].matched);
    REQUIRE_EQ(match[1].pos, 1);
    REQUIRE(match[1].deftgt == nullptr);

    REQUIRE(match[2].matched);
    REQUIRE_EQ(match[2].pos, 2);
    REQUIRE(match[2].deftgt == nullptr);
}

TEST(match_ellip_1a)
{
    std::vector<ArgSpec> manifest{
        ArgSpec{.name = "k", .default_real = 1},
        ArgSpec{.name = "..."},
    };
    auto match = match_arguments(manifest, {"", "", "k"});
    print_match(match);

    REQUIRE_EQ(match.size(), 3);

    REQUIRE(match[0].matched);
    REQUIRE_EQ(match[0].pos, 2);
    REQUIRE(match[0].deftgt == nullptr);

    REQUIRE(match[1].matched);
    REQUIRE_EQ(match[1].pos, 0);
    REQUIRE(match[1].deftgt == nullptr);

    REQUIRE(match[2].matched);
    REQUIRE_EQ(match[2].pos, 1);
    REQUIRE(match[2].deftgt == nullptr);
}

TEST(match_ellip_1b)
{
    std::vector<ArgSpec> manifest{
        ArgSpec{.name = "k", .default_real = 1},
        ArgSpec{.name = "..."},
    };
    auto match = match_arguments(manifest, {"", ""});
    print_match(match);

    REQUIRE_EQ(match.size(), 3);

    REQUIRE(!match[0].matched);
    REQUIRE(match[0].deftgt != nullptr);

    REQUIRE(match[1].matched);
    REQUIRE_EQ(match[1].pos, 0);
    REQUIRE(match[1].deftgt == nullptr);

    REQUIRE(match[2].matched);
    REQUIRE_EQ(match[2].pos, 1);
    REQUIRE(match[2].deftgt == nullptr);
}

TEST(match_ellip_3)
{
    std::vector<ArgSpec> manifest{
        ArgSpec{.name = "a"},
        ArgSpec{.name = "b", .default_int = 3},
        ArgSpec{.name = "..."},
    };
    EXPECT_ERROR("shall not be defined by key",
        [&]() { match_arguments(manifest, {"", "", "b", "a"}); });
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

    EXPECT_ERROR("follow", [&]() { match_arguments(manifest, {"b", ""}); });
}

int main()
{
    int failed = 0;
    RUN_ALL(failed);
    return failed != 0;
}
