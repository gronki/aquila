#include "operation.hpp"
#include <interpreter.hpp>
#include <iostream>

using namespace aquila;
using namespace aquila::interpreter;

int main(int argc, char **argv)
{

    static AquilaInterpreter interp;
    for (int i = 1; i < argc; i++)
    {
        try
        {
            const Value *result = interp.exec(argv[i]);
            if (!result)
            {
                std::cout << "null" << std::endl;
            }
            else
            {
                std::cout << "|" << i << "|: " << result->str() << std::endl;
            }
        }
        catch (const std::exception &e)
        {
            std::cout << "error |" << i << "|: " << e.what() << std::endl;
        }
    }
    return 0;
}
