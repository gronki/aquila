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

TokenLoc Tokenizer::update_end(TokenLoc loc) const
{
    loc.end = pos;
    return loc;
}

TokenResult Tokenizer::next_token()
{
    skip_whitespace();

    if (is_end())
        return {Token(TokenType::END, TokenLoc(start_line, pos))};

    TokenChar ch = get_char();
    TokenLoc loc{start_line, pos, pos};

    if (ch == COMMENT_START)
        return {Token(TokenType::END, loc)};

    if (is_ident_start(ch))
    {
        TokenStr ident_val = consume_until(is_ident);
        return {Token(TokenType::IDENT, ident_val, update_end(loc))};
    }

    if (is_str_literal_start(ch))
    {
        next_char();

        if (is_end())
            return {Token(TokenType::STR_LITERAL, "", update_end(loc)),
                "unexpected end of input after opened string literal"};

        TokenStr content = consume_until(is_not_str_literal_end);

        if (is_end())
            return {Token(TokenType::STR_LITERAL, content, update_end(loc)),
                "unexpected end of input after opened string literal"};

        next_char();

        return {Token(TokenType::STR_LITERAL, content, update_end(loc))};
    }

    if (is_number_start(ch))
    {
        TokenStr number_body = consume_until(is_number_body);
        return {Token(TokenType::NUM_LITERAL, number_body, update_end(loc))};
    }

    if (is_delim(ch))
    {
        next_char();
        return {Token(TokenType::DELIM, TokenStr(1, ch), update_end(loc))};
    }

    return {Token(TokenType::END), "error tokenizing expression"}; // to silence warning
}

std::vector<Token> tokenize(const TokenStr &buffer, std::int64_t start_line)
{
    return LazyTokenArray(Tokenizer(buffer)).all_tokens();
}

TokenResult LazyTokenArray::get_token(std::int64_t abs_pos)
{
    if (abs_pos < 0)
        throw std::runtime_error("may not rewind before the first token");

    while (std::int64_t(tokens.size()) < abs_pos + 1)
    {
        auto result = tokenizer.next_token();
#ifndef NDEBUG
        std::cout << "at pos " << abs_pos << " token " << result.token << std::endl;
#endif
        tokens.push_back(result.token);
    }
    return tokens[abs_pos];
}

Token LazyTokenArray::peek_token(std::int64_t offset)
{
    auto result = get_token(offset + pos);
    if (result.error)
        throw std::runtime_error(result.error->message);
    return result.token;
}

std::vector<Token> LazyTokenArray::all_tokens()
{
    std::int64_t off = 0;
    std::vector<Token> only_tokens;
    bool is_end;
    do
    {
        Token t = get_token(off++).token;
        is_end = t.type == TokenType::END;
        only_tokens.push_back(std::move(t));
    } while (!is_end);

    return only_tokens;
}

Token LazyTokenArray::cur_token()
{
    return peek_token(0);
}

Token LazyTokenArray::next_token(std::int64_t offset)
{
    Token t = peek_token(offset);
    pos += offset;
    return std::move(t);
}

} // namespace aquila::interpreter
