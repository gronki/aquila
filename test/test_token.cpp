#include "../src_cpp/lib/interpreter/token.hpp"
#include "../src_cpp/lib/interpreter/tokenizer.hpp"
#include "testmacros.hpp"

using namespace aquila::parser;

void test_comparison()
{
    REQUIRE_EQ(Token(TokenType::IDENT, "a"), Token(TokenType::IDENT, "a"));
    REQUIRE_NEQ(Token(TokenType::STR_LITERAL, "a"), Token(TokenType::IDENT, "a"));
    REQUIRE_NEQ(Token(TokenType::IDENT, "a"), Token(TokenType::IDENT, "b"));
}

void test1()
{
    Tokenizer tokenizer("  y= ff(a, b) ");

    REQUIRE_EQ(tokenizer.next_token(), Token(TokenType::IDENT, "y"));
    REQUIRE_EQ(tokenizer.next_token(), Token(TokenType::DELIM, "="));
    REQUIRE_EQ(tokenizer.next_token(), Token(TokenType::IDENT, "ff"));
    REQUIRE_EQ(tokenizer.next_token(), Token(TokenType::DELIM, "("));
    REQUIRE_EQ(tokenizer.next_token(), Token(TokenType::IDENT, "a"));
    REQUIRE_EQ(tokenizer.next_token(), Token(TokenType::DELIM, ","));
    REQUIRE_EQ(tokenizer.next_token(), Token(TokenType::IDENT, "b"));
    REQUIRE_EQ(tokenizer.next_token(), Token(TokenType::DELIM, ")"));
    REQUIRE_EQ(tokenizer.next_token(), Token(TokenType::END));
}

void test2()
{
    Tokenizer tokenizer(R"( f(3.0, 4, image("file.fits")) )");

    REQUIRE_EQ(tokenizer.next_token(), Token(TokenType::IDENT, "f"));
    REQUIRE_EQ(tokenizer.next_token(), Token(TokenType::DELIM, "("));
    REQUIRE_EQ(tokenizer.next_token(), Token(TokenType::NUM_LITERAL, "3.0"));
    REQUIRE_EQ(tokenizer.next_token(), Token(TokenType::DELIM, ","));
    REQUIRE_EQ(tokenizer.next_token(), Token(TokenType::NUM_LITERAL, "4"));
    REQUIRE_EQ(tokenizer.next_token(), Token(TokenType::DELIM, ","));
    REQUIRE_EQ(tokenizer.next_token(), Token(TokenType::IDENT, "image"));
    REQUIRE_EQ(tokenizer.next_token(), Token(TokenType::DELIM, "("));
    REQUIRE_EQ(tokenizer.next_token(), Token(TokenType::STR_LITERAL, "file.fits"));
    REQUIRE_EQ(tokenizer.next_token(), Token(TokenType::DELIM, ")"));
    REQUIRE_EQ(tokenizer.next_token(), Token(TokenType::DELIM, ")"));
    REQUIRE_EQ(tokenizer.next_token(), Token(TokenType::END));
}

void test3()
{
    Tokenizer tokenizer("  ");
    REQUIRE_EQ(tokenizer.next_token(), Token(TokenType::END));
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