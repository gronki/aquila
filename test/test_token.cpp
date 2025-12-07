#include "../src_cpp/lib/interpreter/token.hpp"
#include "../src_cpp/lib/interpreter/tokenizer.hpp"
#include "testmacros.hpp"

using namespace aquila::interpreter;

TEST(comparison)
{
    REQUIRE_EQ(Token(TokenType::IDENT, "a"), Token(TokenType::IDENT, "a"));
    REQUIRE_NEQ(Token(TokenType::STR_LITERAL, "a"), Token(TokenType::IDENT, "a"));
    REQUIRE_NEQ(Token(TokenType::IDENT, "a"), Token(TokenType::IDENT, "b"));
}

TEST(test1)
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

TEST(test2)
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

TEST(test3)
{
    Tokenizer tokenizer("  ");
    REQUIRE_EQ(tokenizer.next_token(), Token(TokenType::END));
}

TEST(into_array)
{
    std::vector<Token> tokens = tokenize("f(x)");
    REQUIRE_EQ(tokens.size(), 5);
    REQUIRE_EQ(tokens[0], Token(TokenType::IDENT, "f"));
    REQUIRE_EQ(tokens[1], Token(TokenType::DELIM, "("));
    REQUIRE_EQ(tokens[2], Token(TokenType::IDENT, "x"));
    REQUIRE_EQ(tokens[3], Token(TokenType::DELIM, ")"));
    REQUIRE_EQ(tokens[4], Token(TokenType::END));
}

int main(int argc, char** argv)
{
    int failed = 0;
    RUN_ALL(failed);
    std::cout << "failed tests: " << failed << std::endl;
    return failed != 0;
}