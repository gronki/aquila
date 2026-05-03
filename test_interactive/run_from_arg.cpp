#include "operation.hpp"
#include <interpreter.hpp>
#include <iostream>

using namespace aquila;
using namespace aquila::interpreter;

int main(int argc, char **argv)
{
    if (argc < 2)
        exit(1);

    static Namespace ns;
    auto exec = build_exectree_from_str(argv[1], ns, global_op_db());
    const Value *result = exec->yield();
    if (!result)
    {
        std::cout << "null" << std::endl;
        exit(1);
    }
    std::cout << result->str() << std::endl;
    return 0;
}
