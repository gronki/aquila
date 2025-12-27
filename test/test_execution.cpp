#include <iostream>

#include "../src_cpp/lib/interpreter/interpreter.hpp"

#include "test_operations.hpp"
#include "testmacros.hpp"

using namespace aquila;
using namespace aquila::interpreter;

Namespace example_ns()
{
    Namespace ns;
    ns.push("a", std::make_unique<RealValue>(1));
    ns.push("b", std::make_unique<RealValue>(2));
    return ns;
}

TEST(example)
{
    Namespace ns = example_ns();
    auto exec = build_exectree_from_str(" add(1, 2) % mul(3)", ns, global_op_db());
    auto result = exec->yield();
    std::cout << (bool(result) ? result->str() : std::string("(null)")) << std::endl;
}

int main(int argc, char **argv)
{
    int failed = 0;
    RUN_ALL(failed);
    std::cout << "failed tests: " << failed << std::endl;
    return failed != 0;
}