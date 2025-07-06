#include <stdlib.h>
#include <readline/readline.h>
#include <readline/history.h>

void readline_prompt_init()
{
    using_history();
}

void readline_prompt_read(const char **input, size_t *nchar)
{
    *input = readline("> ");
    if (*input)
    {
        add_history(*input);
        *nchar = strlen(*input);
    }
}

void readline_prompt_free(char *input)
{
    if (input)
        free(input);
}