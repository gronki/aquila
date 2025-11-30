#pragma once

#include <token.hpp>
#include <types.hpp>

namespace aquila::parser
{

class Tokenizer
{
    TokenStr buffer; // buffer to be parsed
    Int pos;         // curent cursor position
    Int start_line;  // only used to format error messages

    TokenChar get_char();
    TokenChar next_char();
    void skip_whitespace();

public:
    Tokenizer(const TokenStr &buffer, Int start_line = 1)
        : buffer(buffer), pos(0), start_line(start_line)
    {
    }

    inline bool is_end() const { return pos >= buffer.size(); }
    Token next_token();
};

} // namespace aquila::parser