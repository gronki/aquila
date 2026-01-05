#include <iostream>
#include <string>

#include <readline/readline.h>
#include <readline/history.h>

#include "../lib/interpreter/interpreter.hpp"
// #include "../../test/test_operations.hpp"

using namespace aquila;
using namespace aquila::interpreter;

int main()
{
    Namespace ns;  // persistent across commands

    while (true)
    {
        char* line = readline(">> ");

        // Ctrl-D
        if (!line)
            break;

        std::string input(line);
        free(line);

        if (input.empty())
            continue;

        if (input == "quit" || input == "exit")
            break;

        add_history(input.c_str());

        try
        {
            auto exec = build_exectree_from_str(input, ns, global_op_db());
            auto result = exec->yield();

            if (result)
                std::cout << result->str() << std::endl;
            else
                std::cout << "(null)" << std::endl;
        }
        catch (const std::exception& e)
        {
            std::cerr << "error: " << e.what() << std::endl;
        }
    }

    std::cout << "bye 👋" << std::endl;
    return 0;
}
