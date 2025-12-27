#include "../src_cpp/global/types.hpp"

#include "testmacros.hpp"

using namespace aquila;

TEST(example)
{
    std::cout << "test1" << std::endl;
}

int main(int argc, char **argv)
{
    int failed = 0;
    RUN_ALL(failed);
    std::cout << "failed tests: " << failed << std::endl;
    return failed != 0;
}