#include "../src_cpp/lib/interpreter/value.hpp"
#include "testmacros.hpp"

using namespace aquila;
using namespace aquila::interpreter;

TEST(simple)
{
    IntValue iv1{4}, iv2{4}, iv3{5};

    REQUIRE(iv1 == iv2);
    REQUIRE(iv1 == (Int)4);
    REQUIRE((Int)4 == iv2);
    REQUIRE((Int)4 != iv3);
    REQUIRE(iv2 != iv3);

    std::cout << iv1 << std::endl;
    std::cout << iv2 << std::endl;
    std::cout << iv3 << std::endl;
}

TEST(seq)
{
    IntValue iv1{4}, iv2{4}, iv3{5};
    ValuePtrVector vv;
    vv.push_back(std::make_unique<IntValue>(1));
    vv.push_back(std::make_unique<RealValue>(1));
    vv.push_back(std::make_unique<StrValue>("a"));
    SequenceValue sv{std::move(vv)};
    SequenceValue sv2{sv};

    auto real_ptr_1 = dynamic_cast<RealValue *>(sv.items.at(1).get());
    auto real_ptr_2 = dynamic_cast<RealValue *>(sv2.items.at(1).get());
    if (real_ptr_1 && real_ptr_2)
    {
        real_ptr_1->value = 11;
        REQUIRE_NEQ(*real_ptr_1, *real_ptr_2);
    }
    else
    {
        UNREACHABLE("sth wrong went during dynamic cast");
    }

    std::cout << sv << std::endl;
    std::cout << sv2 << std::endl;
}

int main(int argc, char **argv)
{
    int failed = 0;
    RUN_ALL(failed);
    std::cout << "failed tests: " << failed << std::endl;
    return failed != 0;
}