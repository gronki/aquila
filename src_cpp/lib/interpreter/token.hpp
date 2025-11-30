#pragma once
#include <types.hpp>

namespace aquila::parser
{

using TokenChar = char;
using TokenStr = std::basic_string<TokenChar>;

enum class TokenType
{
    DELIM,
    IDENT,
    NUM_LITERAL,
    STR_LITERAL,
    END,
};

struct TokenLoc
{
    Int line = -1, offset = -1;
};

struct Token
{
    Token(TokenType type, const TokenStr &value = "", const TokenLoc &loc = {})
        : type(type), value(value), loc(loc)
    {
    }

    TokenType type;
    TokenStr value;
    TokenLoc loc;

    inline bool operator==(const Token &other) const
    {
        if (type != other.type)
            return false;
        if (type == TokenType::END)
            return true;
        return value == other.value;
    }

    inline bool operator!=(const Token &other) const
    {
        return !(*this == other);
    }
};

} // namespace aquila::parser