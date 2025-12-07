#pragma once

#include <memory>
#include <stdexcept>

#include "ast.hpp"
#include "token.hpp"

namespace aquila::interpreter
{

std::unique_ptr<AstNode> parse(std::vector<Token> tokens);

}