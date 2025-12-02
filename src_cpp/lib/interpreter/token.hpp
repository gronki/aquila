#pragma once

#include "../../global/types.hpp"
#include <map>
#include <ostream>
#include <stdexcept>

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

const std::map<TokenType, std::string> TOKENTYPE_STR{
    {TokenType::DELIM, "DELIM"},
    {TokenType::IDENT, "IDENT"},
    {TokenType::NUM_LITERAL, "NUM_LITERAL"},
    {TokenType::STR_LITERAL, "STR_LITERAL"},
    {TokenType::END, "END"}};

struct TokenLoc
{
    TokenLoc() {}
    TokenLoc(Int line, Int offset) : line(line), offset(offset) {}
    Int line = -1, offset = -1;
};

struct Token
{
    Token(TokenType type, const TokenStr &value, const TokenLoc &loc = {})
        : type(type), value(value), loc(loc)
    {
    }
    Token(TokenType type, const TokenLoc &loc = {}) : type(type), loc(loc)
    {
        if (type != TokenType::END)
            throw std::runtime_error("Only end token allows to skip value");
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

inline std::ostream &operator<<(std::ostream &os, Token t)
{
    if (t.type != TokenType::END)
    {
        os << "Token(" << TOKENTYPE_STR.at(t.type) << ", value=\"" << t.value
           << "\", pos=" << t.loc.offset << ")";
    }
    else
    {
        os << "Token(" << TOKENTYPE_STR.at(t.type) << ", pos=" << t.loc.offset
           << ")";
    }

    return os;
}

} // namespace aquila::parser