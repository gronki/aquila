#pragma once

#include <memory>
#include <ostream>
#include <vector>

#include "token.hpp"
#include "value.hpp"

namespace aquila::interpreter
{

struct AstNode
{
    TokenLoc loc;
    virtual ~AstNode() = default;
    virtual void _print(std::ostream &, Int indent) const = 0;
    friend std::ostream &operator<<(std::ostream &os, const AstNode &node)
    {
        node._print(os, 0);
        return os;
    }
};

struct AstValueNode : public AstNode
{
    std::unique_ptr<AnySimpleValue> constant;

    virtual void _print(std::ostream &os, Int indent) const
    {
        if (constant)
        {
            os << *constant;
        }
        else
        {
            os << "(empty value node)";
        }
    }
};

struct AstRefNode : public AstNode
{
    std::string refname;

    virtual void _print(std::ostream &os, Int indent) const
    {
        if (refname != "")
        {
            os << "${" << refname << "}";
        }
        else
        {
            os << "(empty reference node)";
        }
    }
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

    virtual void _print(std::ostream &os, Int indent) const
    {
        std::string padding(std::size_t(indent), ' ');
        std::string padding_arg(std::size_t(indent + 4), ' ');
        os << "Op(" << opname << ") {";
        bool anyarg = false;
        for (const OpArg &arg : args)
        {
            if (anyarg)
                os << ",";
            os << std::endl << padding_arg;
            if (arg.key != "")
            {
                os << arg.key << ": ";
            }
            arg.arg_val->_print(os, indent + 4);
            anyarg = true;
        }
        if (anyarg)
            os << std::endl << padding;
        os << "}";
    }
};

} // namespace aquila::interpreter
