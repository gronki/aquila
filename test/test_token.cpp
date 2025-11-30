#include "testmacros.hpp"
#include <token.hpp>
#include <tokenizer.hpp>

using namespace aquila::parser;

void test_comparison()
{
    REQUIRE(Token(TokenType::IDENT, "a") == Token(TokenType::IDENT, "a"));
    REQUIRE(Token(TokenType::STR_LITERAL, "a") != Token(TokenType::IDENT, "a"));
    REQUIRE(Token(TokenType::IDENT, "b") != Token(TokenType::IDENT, "b"));
}

void test1()
{
    Tokenizer tokenizer("  y= ff(a, b) ");

    REQUIRE(tokenizer.next_token() == Token(TokenType::IDENT, "y"));
    REQUIRE(tokenizer.next_token() == Token(TokenType::DELIM, "="));
    REQUIRE(tokenizer.next_token() == Token(TokenType::IDENT, "ff"));
    REQUIRE(tokenizer.next_token() == Token(TokenType::DELIM, "("));
    REQUIRE(tokenizer.next_token() == Token(TokenType::IDENT, "a"));
    REQUIRE(tokenizer.next_token() == Token(TokenType::DELIM, ","));
    REQUIRE(tokenizer.next_token() == Token(TokenType::IDENT, "b"));
    REQUIRE(tokenizer.next_token() == Token(TokenType::DELIM, ")"));
    REQUIRE(tokenizer.next_token() == Token(TokenType::END));
}

void test2()
{
    Tokenizer tokenizer(R"( f(3.0, 4, image("file.fits")) )");

    REQUIRE(tokenizer.next_token() == Token(TokenType::IDENT, "f"));
    REQUIRE(tokenizer.next_token() == Token(TokenType::DELIM, "("));
    REQUIRE(tokenizer.next_token() == Token(TokenType::NUM_LITERAL, "3.0"));
    REQUIRE(tokenizer.next_token() == Token(TokenType::DELIM, ","));
    REQUIRE(tokenizer.next_token() == Token(TokenType::NUM_LITERAL, "4"));
    REQUIRE(tokenizer.next_token() == Token(TokenType::DELIM, ","));
    REQUIRE(tokenizer.next_token() == Token(TokenType::IDENT, "image"));
    REQUIRE(tokenizer.next_token() == Token(TokenType::DELIM, "("));
    REQUIRE(tokenizer.next_token() == Token(TokenType::STR_LITERAL, "file.fits"));
    REQUIRE(tokenizer.next_token() == Token(TokenType::DELIM, ")"));
    REQUIRE(tokenizer.next_token() == Token(TokenType::DELIM, ")"));
    REQUIRE(tokenizer.next_token() == Token(TokenType::END));
}

void test3()
{
    Tokenizer tokenizer("  ");
    REQUIRE(tokenizer.next_token() == Token(TokenType::END));
}

int main(int argc, char** argv)
{
    int failed = 0;
    RUN(test_comparison);
    RUN(test1);
    RUN(test2);
    RUN(test3);
    std::cout << "failed tests: " << failed << std::endl;
    return failed != 0;
}