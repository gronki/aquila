#pragma once

#include "ast.hpp"
#include "execution.hpp"
#include "operation.hpp"

namespace aquila::interpreter
{

std::unique_ptr<ExecNode> build_exec_tree(
    const std::unique_ptr<AstNode> &ast, Namespace &ns, const OpDatabase &opdb);

} // namespace aquila::interpreter