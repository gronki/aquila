#include <sstream>
#include <stdexcept>

#include "characters.hpp"
#include "tokenizer.hpp"

#ifndef NDEBUG
#    include <iostream>
#endif

namespace aquila::interpreter
{

void Tokenizer::skip_whitespace()
{
    while (!is_end() && is_whitespace(get_char()))
    {
        next_char();
    }
}

TokenChar Tokenizer::get_char() const
{
    if (is_end())
        return ' ';
    return buffer[pos];
}

TokenChar Tokenizer::next_char()
{
    if (is_end())
        return ' ';
    pos += 1;
    return get_char();
}

void Tokenizer::throw_error(const std::string &message)
{
    throw std::runtime_error(message);
}

TokenStr Tokenizer::consume_until(ConsumeCondition consume_condition)
{
    std::basic_stringstream<TokenChar> ss;
    while (!is_end())
    {
        TokenChar ch = get_char();
        if (!consume_condition(ch))
            break;
        ss << ch;
        next_char();
    }
    return ss.str();
}

Token Tokenizer::next_token()
{
    Token t = next_token_();

#ifndef NDEBUG
    std::cout << t << std::endl;
#endif

    return t;
}

Token Tokenizer::next_token_()
{
    skip_whitespace();

    if (is_end())
        return Token(TokenType::END, TokenLoc(start_line, pos));

    TokenChar ch = get_char();
    TokenLoc loc{start_line, pos};

    if (is_ident_start(ch))
    {
        return Token(TokenType::IDENT, consume_until(is_ident), loc);
    }

    if (is_str_literal_start(ch))
    {
        next_char();

        if (is_end())
            throw_error("unexpected end of input after opened string literal");

        TokenStr content = consume_until(is_not_str_literal_end);

        if (is_end())
            throw_error("unexpected end of input after opened string literal");

        next_char();

        return Token(TokenType::STR_LITERAL, content, loc);
    }

    if (is_number_start(ch))
    {
        return Token(TokenType::NUM_LITERAL, consume_until(is_number_body), loc);
    }

    if (is_delim(ch))
    {
        next_char();
        return Token(TokenType::DELIM, TokenStr(1, ch), loc);
    }

    throw_error("error tokenizing expression");
    return Token(TokenType::END); // to silence warning
}

std::vector<Token> tokenize(const TokenStr &buffer, Int start_line)
{
    Tokenizer tokenizer(buffer, start_line);
    std::vector<Token> tokens;
    bool is_end = false;

    do
    {
        Token t = tokenizer.next_token();
        is_end = t.type == TokenType::END;
        tokens.push_back(std::move(t));
    } while (!is_end);

    return tokens;
}

Token LazyTokenArray::peek_token(Int offset)
{
    if (offset + pos < 0)
        throw std::runtime_error("may not rewind before the first token");

    while (tokens.size() < offset + pos + 1)
    {
        tokens.push_back(tokenizer.next_token());
    }
    return tokens[offset + pos];
}

Token LazyTokenArray::cur_token()
{
    return peek_token(0);
}

Token LazyTokenArray::next_token(Int offset)
{
    const Token &t = peek_token(offset);
    pos += offset;
    return t;
}

} // namespace aquila::interpreter