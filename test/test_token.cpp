#include <assert.h>
#include <token.hpp>

using namespace aquila::parser;

void test_comparison()
{
    assert(Token(TokenType::IDENT, "a") == Token(TokenType::IDENT, "a"));
    assert(Token(TokenType::STR_LITERAL, "a") != Token(TokenType::IDENT, "a"));
    assert(Token(TokenType::IDENT, "a") != Token(TokenType::IDENT, "b"));
}

int main(int argc, char** argv)
{
    test_comparison();
}