#include <cstdint>
#include <iostream>
#include <sstream>
#include <string>

#include "../src_cpp/interpreter/ast.hpp"
#include "../src_cpp/interpreter/execution.hpp"
#include "../src_cpp/interpreter/interpreter.hpp"
#include "../src_cpp/interpreter/parser.hpp"
#include "../src_cpp/interpreter/tokenizer.hpp"

#include "ftxui/component/component.hpp"
#include "ftxui/component/screen_interactive.hpp"
#include "ftxui/dom/elements.hpp"
#include "ftxui/screen/color.hpp"

using namespace aquila;
using namespace aquila::interpreter;
using namespace ftxui;

int cursor_pos;

std::string find_current_token(const std::vector<Token> &tokens)
{

    for (const Token &t : tokens)
    {
        if (t.type == TokenType::DELIM || t.type == TokenType::END)
            continue;
        if (t.loc.offset <= cursor_pos && t.loc.end >= cursor_pos)
            return t.str();
    }
    return "";
}
void update_state(const std::string &command,
    std::string &command_output,
    std::string &token_summ,
    std::string &current_token)
{
    command_output = "";
    current_token = "";
    token_summ = "";
    std::stringstream token_summ_ss;
    try
    {
        std::unique_ptr<AstNode> ast;
        LazyTokenArray tokens(Tokenizer{command});

        for (const Token &t : tokens.all_tokens())
        {
            token_summ_ss << t.str() << std::endl;
        }
        token_summ = token_summ_ss.str();

        current_token = find_current_token(tokens.all_tokens());
        parse(tokens, ast);
        if (!ast)
            return;

        std::stringstream ss;
        ss << *ast;
        command_output = ss.str();
    }
    catch (const std::exception &e)
    {
        current_token = e.what();
    }
}

int main(int argc, char **argv)
{

    std::string command;
    auto option = InputOption::Spacious();

    option.cursor_position = Ref<int>(&cursor_pos);
    auto input_command = Input(&command, "input command...", std::move(option));
    auto renderer = Renderer(input_command,
        [&]()
        {
            std::string command_output, token_summ, current_token;
            update_state(command, command_output, token_summ, current_token);
            return vbox({
                input_command->Render() | border,
                hbox({
                    paragraphAlignLeft(command_output) | flex,
                    separator(),
                    paragraphAlignCenter(token_summ) | size(WIDTH, EQUAL, 48),
                }) | border
                    | flex,
                text(current_token) | bold,
            });
        });

    auto screen = ScreenInteractive::Fullscreen();
    screen.Loop(renderer);
}
