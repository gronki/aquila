#pragma once

namespace aquila::parser
{

constexpr char CHAIN_CALL_DELIM = '%';
constexpr char KWARG_DELIM = ':';

inline bool is_whitespace(char ch)
{
    return (ch == ' ') || (ch == '\t');
}

inline bool is_delim(char ch)
{
    return (ch == '(') || (ch == ')') || (ch == ',') || (ch == '=') ||
           (ch == CHAIN_CALL_DELIM) || (ch == KWARG_DELIM);
}

inline bool is_str_literal_start(char ch)
{
    return ch == '"';
}

inline bool is_not_str_literal_end(char ch)
{
    return ch != '"';
}

inline bool is_ident_start(char ch)
{
    return (ch == '_') || (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z');
}

inline bool is_digit(char ch)
{
    return ch >= '0' && ch <= '9';
}

inline bool is_number_start(char ch)
{
    return ch == '+' || ch == '-' || is_digit(ch);
}

inline bool is_number_body(char ch)
{
    return ch == '.' || ch == 'e' || ch == 'E' || ch == '+' || ch == '-' ||
           is_number_start(ch);
}

inline bool is_ident(char ch)
{
    return is_ident_start(ch) || is_digit(ch);
}

inline bool is_valid_char(char ch)
{
    return is_whitespace(ch) || is_delim(ch) || is_ident(ch);
}

}