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

TEST(match1a)
{
    std::vector<ArgSpec> manifest{
        ArgSpec{.name = "a"},
        ArgSpec{.name = "b", .default_int = 3},
    };
    auto match = match_arguments(manifest, {"", ""});

    REQUIRE_EQ(match.size(), 2);

    REQUIRE(match[0].matched);
    REQUIRE_EQ(match[0].pos, 0);
    REQUIRE(match[0].deftgt == nullptr);

    REQUIRE(match[1].matched);
    REQUIRE_EQ(match[1].pos, 1);
    REQUIRE(match[1].deftgt == nullptr);

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

TEST(match_ellip_0)
{
    std::vector<ArgSpec> manifest{
        ArgSpec{.name = "..."},
    };
    auto match = match_arguments(manifest, {"", "", ""});

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

    REQUIRE_EQ(match.size(), 2);

    REQUIRE(match[0].matched);
    REQUIRE_EQ(match[0].pos, 0);
    REQUIRE(match[0].deftgt == nullptr);

    REQUIRE(match[1].matched);
    REQUIRE_EQ(match[1].pos, 1);
    REQUIRE(match[1].deftgt == nullptr);
}

TEST(match_ellip_3)
{
    std::vector<ArgSpec> manifest{
        ArgSpec{.name = "a"},
        ArgSpec{.name = "b", .default_int = 3},
        ArgSpec{.name = "..."},
    };
    auto match = match_arguments(manifest, {"", "", "b", "a"});

    REQUIRE_EQ(match.size(), 4);

    REQUIRE(match[0].matched);
    REQUIRE_EQ(match[0].pos, 3);
    REQUIRE(match[0].deftgt == nullptr);

    REQUIRE(match[1].matched);
    REQUIRE_EQ(match[1].pos, 2);
    REQUIRE(match[1].deftgt == nullptr);

    REQUIRE(match[2].matched);
    REQUIRE_EQ(match[2].pos, 0);
    REQUIRE(match[2].deftgt == nullptr);

    REQUIRE(match[3].matched);
    REQUIRE_EQ(match[3].pos, 1);
    REQUIRE(match[3].deftgt == nullptr);
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

int main()
{
    int failed = 0;
    RUN_ALL(failed);
    return failed != 0;
}