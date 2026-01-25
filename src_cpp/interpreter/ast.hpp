#pragma once

#include <memory>
#include <ostream>
#include <vector>

#include "characters.hpp"
#include "token.hpp"
#include "value.hpp"

namespace aquila::interpreter
{

struct AstNode
{
    TokenLoc loc;

    AstNode(const TokenLoc &loc) : loc(loc) {}
    AstNode() {}

    virtual ~AstNode() = default;
    virtual void _print(std::ostream &, std::int64_t indent) const = 0;
    friend std::ostream &operator<<(std::ostream &os, const AstNode &node)
    {
        node._print(os, 0);
        return os;
    }
};

struct AstValueNode : public AstNode
{
    std::unique_ptr<AnySimpleValue> constant;

    AstValueNode(std::unique_ptr<AnySimpleValue> constant, const TokenLoc &loc) :
        constant(std::move(constant)), AstNode(loc)
    {
    }

    void _print(std::ostream &os, std::int64_t indent) const override
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

struct AstExpandNode : public AstNode
{

    enum class Kind
    {
        EXPANSION,
        CONTRACTION
    };

    std::unique_ptr<AstNode> expandable;
    Kind kind;

    AstExpandNode(std::unique_ptr<AstNode> expandable, Kind kind, const TokenLoc &loc) :
        expandable(std::move(expandable)), kind(kind), AstNode(loc)
    {
    }

    void _print(std::ostream &os, std::int64_t indent) const override
    {
        os << (kind == Kind::EXPANSION ? EXPAND_DELIM : CONTRACT_DELIM);
        if (expandable)
        {
            expandable->_print(os, indent);
        }
        else
        {
            os << "none";
        }
    }
};

struct AstRefNode : public AstNode
{
    std::string refname;

    AstRefNode(const std::string &refname, const TokenLoc &loc) :
        refname(refname), AstNode(loc)
    {
    }

    void _print(std::ostream &os, std::int64_t indent) const override
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

struct AstAssignmentNode : public AstNode
{
    std::string lhs;
    std::unique_ptr<AstNode> rhs;

    AstAssignmentNode(
        const std::string &lhs, std::unique_ptr<AstNode> rhs, const TokenLoc &loc) :
        lhs(lhs), rhs(std::move(rhs)), AstNode(loc)
    {
    }

    void _print(std::ostream &os, std::int64_t indent) const override
    {
        os << lhs << " := ";
        rhs->_print(os, indent);
        os << std::endl;
    }
};

struct AstOpNode : public AstNode
{
    struct OpArg
    {
        bool has_key = false;
        std::string key = "";
        std::unique_ptr<AstNode> arg_val;
    };
    std::string opname;
    std::vector<OpArg> args;

    AstOpNode(const std::string &opname, std::vector<OpArg> args, const TokenLoc &loc) :
        opname(opname), args(std::move(args)), AstNode(loc)
    {
    }

    void _print(std::ostream &os, std::int64_t indent) const override
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
