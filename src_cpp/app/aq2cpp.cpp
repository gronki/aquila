#include <iostream>
#include <string>

#include <readline/history.h>
#include <readline/readline.h>

#include "../../src/version.h"
#include "../lib/interpreter/interpreter.hpp"

using namespace aquila;
using namespace aquila::interpreter;

const std::vector<const char *> &get_opnames()
{
    static std::vector<const char *> opnames;
    if (opnames.empty())
    {

        const auto &opdb = global_op_db();
        for (const auto &kv : opdb)
        {
            opnames.push_back(kv.first.c_str());
        }
    }
    return opnames;
}

char *command_generator(const char *text, int state)
{
    static int list_index;

    /* If this is a new word to complete, initialize now.  This includes
       saving the length of TEXT for efficiency, and initializing the index
       variable to 0. */
    if (!state)
    {
        list_index = 0;
    }
    int len = strlen(text);

    /* Return the next name which partially matches from the command list. */
    auto opnames = get_opnames();
    while (list_index < opnames.size())
    {
        const char *name = opnames[list_index++];

        if (strncmp(name, text, len) == 0)
        {
            char *retname = (char *)malloc(strlen(name) + 4);
            strcpy(retname, name);
            // strcat(retname, "(");
            return retname;
        }
    }

    /* If no names matched, then return NULL. */
    return ((char *)NULL);
}

char **completion(const char *text, int start, int end)
{
    rl_completion_append_character = '(';
    // std::cout << "completion " << start << ":" << end << "    ->" << text << std::endl;
    return rl_completion_matches(text, command_generator);
}

void display_completions(char **matches, int num_matches, int max_length)
{
    char **new_matches = (char **)malloc((num_matches + 1) * sizeof(char *));
    new_matches[num_matches] = nullptr;
    int new_max_len = 0;
    const auto &db = global_op_db();
    for (size_t imatch = 0; imatch < num_matches; imatch++)
    {
        auto it = db.find(std::string(matches[imatch]));
        int len;
        if (it == db.cend())
        {
            len = strlen(matches[imatch]);
            new_matches[imatch] = (char *)malloc(sizeof(char) * (len + 1));
            strcpy(new_matches[imatch], matches[imatch]);
        }
        else
        {
            const auto &sig = it->second.signature_str;
            len = sig.size();
            new_matches[imatch] = (char *)malloc(sizeof(char) * (len + 1));
            strcpy(new_matches[imatch], sig.c_str());
        }
        new_max_len = len > new_max_len ? len : new_max_len;
    }
    rl_display_match_list(new_matches, num_matches, new_max_len);
    rl_on_new_line();
    rl_redisplay();
}

int main()
{
    Namespace ns; // persistent across commands

    rl_attempted_completion_function = completion;
    rl_completion_display_matches_hook = display_completions;

    std::cout << std::endl << "Aquila Script v. " << _AQUILA_VERSION_ << std::endl;
    std::cout << "Created by DG, inspired by FK" << std::endl << std::endl;
    std::cout << "Press [TAB] twice to print available commands." << std::endl;

    while (true)
    {
        char *line = readline(">> ");

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
        catch (const std::exception &e)
        {
            std::cerr << "error: " << e.what() << std::endl;
        }
    }

    std::cout << "bye 👋" << std::endl;
    return 0;
}
