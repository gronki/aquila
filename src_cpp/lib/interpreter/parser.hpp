#pragma once

#include <memory>
#include <stdexcept>

#include "ast.hpp"
#include "token.hpp"
#include "tokenizer.hpp"

namespace aquila::interpreter
{

bool parse(LazyTokenArray &tokens, std::unique_ptr<AstNode> &root);

}