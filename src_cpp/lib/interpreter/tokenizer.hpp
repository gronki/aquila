#pragma once

#include <string>

#include "../../global/types.hpp"
#include "token.hpp"

namespace aquila::parser
{

using ConsumeCondition = bool (*)(TokenChar);

class Tokenizer
{
    TokenStr buffer; // buffer to be parsed
    Int pos;         // curent cursor position
    Int start_line;  // only used to format error messages

    TokenChar get_char() const;
    TokenChar next_char();
    void skip_whitespace();
    TokenStr consume_until(ConsumeCondition consume_condition);
    void throw_error(const std::string &message);
    Token next_token_();

public:
    Tokenizer(const TokenStr &buffer, Int start_line = 1)
        : buffer(buffer), pos(0), start_line(start_line)
    {
    }

    inline bool is_end() const { return pos >= buffer.size(); }
    Token next_token();
};

} // namespace aquila::parser