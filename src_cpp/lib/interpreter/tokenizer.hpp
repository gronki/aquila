#pragma once

#include <string>
#include <vector>


#include "token.hpp"

namespace aquila::interpreter
{

using ConsumeCondition = bool (*)(TokenChar);

class Tokenizer
{
    TokenStr buffer; // buffer to be parsed
    std::int64_t pos;         // curent cursor position
    std::int64_t start_line;  // only used to format error messages

    TokenChar get_char() const;
    TokenChar next_char();
    void skip_whitespace();
    TokenStr consume_until(ConsumeCondition consume_condition);
    void throw_error(const std::string &message);
    Token next_token_();

public:
    Tokenizer(const TokenStr &buffer, std::int64_t start_line = 1)
        : buffer(buffer), pos(0), start_line(start_line)
    {
    }

    inline bool is_end() const { return pos >= buffer.size(); }
    Token next_token();
};

std::vector<Token> tokenize(const TokenStr &buffer, std::int64_t start_line = 1);

class LazyTokenArray
{
    Tokenizer &tokenizer;
    std::vector<Token> tokens;
    std::int64_t pos = 0;

public:
    LazyTokenArray(Tokenizer &tokenizer) : tokenizer(tokenizer) {}
    Token peek_token(std::int64_t offset = 0);
    Token cur_token();
    Token next_token(std::int64_t offset = 1);
};

} // namespace aquila::interpreter