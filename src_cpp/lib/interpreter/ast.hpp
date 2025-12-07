#pragma once

#include <memory>
#include <vector>

#include "token.hpp"
#include "value.hpp"

namespace aquila::interpreter
{

struct AstNode
{
    TokenLoc loc;
    virtual ~AstNode() = default;
};

struct AstValueNode : public AstNode
{
    std::unique_ptr<AnySimpleValue> constant;
};

struct AstRefNode : public AstNode
{
    std::string refname;
};

struct AstOpNode : public AstNode
{
    struct OpArg
    {
        std::string key = "";
        std::unique_ptr<AstNode> arg_val;
    };
    std::string opname;
    std::vector<OpArg> args;
};

} // namespace aquila::interpreter
