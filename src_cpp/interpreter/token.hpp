#pragma once


#include <iostream>
#include <map>
#include <ostream>
#include <sstream>
#include <stdexcept>
#include <cstdint>

namespace aquila::interpreter
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

const std::map<TokenType, std::string> TOKENTYPE_STR{{TokenType::DELIM, "DELIM"},
                                                     {TokenType::IDENT, "IDENT"},
                                                     {TokenType::NUM_LITERAL, "NUM_LITERAL"},
                                                     {TokenType::STR_LITERAL, "STR_LITERAL"},
                                                     {TokenType::END, "END"}};

struct TokenLoc
{
    TokenLoc() {}
    TokenLoc(std::int64_t line, std::int64_t offset) : line(line), offset(offset) {}
    std::int64_t line = -1, offset = -1;
};

struct Token
{
    Token(TokenType type, const TokenStr &value, const TokenLoc &loc = {})
        : type(type), value(value), loc(loc)
    {
    }
    Token(TokenType type, TokenChar value, const TokenLoc &loc = {})
        : type(type), value(TokenStr(1, value)), loc(loc)
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

    bool operator==(const Token &other) const
    {
        if (type != other.type)
            return false;
        if (type == TokenType::END)
            return true;
        return value == other.value;
    }

    bool operator!=(const Token &other) const { return !(*this == other); }

    friend std::ostream &operator<<(std::ostream &os, const Token &t)
    {
        if (t.type != TokenType::END)
        {
            os << "Token(" << TOKENTYPE_STR.at(t.type) << ", value=\"" << t.value
               << "\", pos=" << t.loc.offset << ")";
        }
        else
        {
            os << "Token(" << TOKENTYPE_STR.at(t.type) << ", pos=" << t.loc.offset << ")";
        }

        return os;
    }

    std::string str() const
    {
        std::stringstream ss;
        ss << *this;
        return ss.str();
    }
};

} // namespace aquila::interpreter