#pragma once

#include "token.hpp"

namespace aquila::interpreter
{

#ifndef _CHAIN_CALL_DELIM
static constexpr TokenChar CHAIN_CALL_DELIM = '%';
#else
static constexpr TokenChar CHAIN_CALL_DELIM = _CHAIN_CALL_DELIM;
#endif
static constexpr TokenChar KWARG_DELIM = ':';
static constexpr TokenChar EXPAND_DELIM = '*';
static constexpr TokenChar CONTRACT_DELIM = '>';

inline bool is_whitespace(TokenChar ch)
{
    return (ch == ' ') || (ch == '\t') || (ch == '\r');
}

inline bool is_delim(TokenChar ch)
{
    return (ch == '(') || (ch == ')') || (ch == ',') || (ch == '=')
        || (ch == CHAIN_CALL_DELIM) || (ch == KWARG_DELIM) || (ch == EXPAND_DELIM)
        || (ch == CONTRACT_DELIM) || (ch == '[') || (ch == ']');
}

inline bool is_str_literal_start(TokenChar ch)
{
    return ch == '"';
}

inline bool is_not_str_literal_end(TokenChar ch)
{
    return ch != '"';
}

inline bool is_ident_start(TokenChar ch)
{
    return (ch == '_') || (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z');
}

inline bool is_digit(TokenChar ch)
{
    return ch >= '0' && ch <= '9';
}

inline bool is_number_start(TokenChar ch)
{
    return ch == '+' || ch == '-' || is_digit(ch);
}

inline bool is_number_body(TokenChar ch)
{
    return ch == '.' || ch == 'e' || ch == 'E' || ch == '+' || ch == '-'
        || is_number_start(ch);
}

inline bool is_ident(TokenChar ch)
{
    return is_ident_start(ch) || is_digit(ch);
}

inline bool is_valid_char(TokenChar ch)
{
    return is_whitespace(ch) || is_delim(ch) || is_ident(ch);
}

} // namespace aquila::interpreter