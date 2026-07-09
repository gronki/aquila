#pragma once

#include "characters.hpp"
#include "namespace.hpp"
#include "operation.hpp"
#include "value.hpp"
#include <iostream>
#include <optional>

namespace aquila::interpreter
{

class ExecNode
{
protected:
    Namespace &ns;

public:
    ExecNode(Namespace &ns) : ns(ns) {}
    virtual Ptr<Value> yield() = 0;
    virtual std::optional<std::string> get_refname() const { return std::nullopt; }
    virtual ~ExecNode() = default;
};

class RefNode : public ExecNode
{
    std::string refname;

public:
    RefNode(const std::string &refname, Namespace &ns) : ExecNode(ns), refname(refname)
    {
    }

    Ptr<Value> yield() override { return ns.get(refname); }

    std::optional<std::string> get_refname() const override { return refname; }
};

class AssignmentNode : public ExecNode
{
    std::string lhs;
    std::unique_ptr<ExecNode> rhs;

public:
    AssignmentNode(const std::string &lhs, std::unique_ptr<ExecNode> rhs, Namespace &ns) :
        ExecNode(ns), lhs(lhs), rhs(std::move(rhs))
    {
    }

    Ptr<Value> yield() override
    {
        Ptr<Value> rhs_yield = rhs->yield();
        if (!rhs_yield)
            return {};
        auto mine = rhs_yield.own();
        mine->materialize();
        return &ns.push(lhs, std::move(mine));
    }
};

class ValueNode : public ExecNode
{
    std::unique_ptr<Value> value;

public:
    ValueNode(std::unique_ptr<Value> value, Namespace &ns) :
        ExecNode(ns), value(std::move(value))
    {
    }

    Ptr<Value> yield() override { return value.get(); }
};

class OpNode : public ExecNode
{
    std::unique_ptr<Operation> op;
    std::vector<std::unique_ptr<ExecNode>> args;
    std::vector<std::string> keys;
    std::vector<int> expansion, is_keyword;
    bool any_expansion = false;

public:
    OpNode(std::unique_ptr<Operation> op,
        std::vector<std::unique_ptr<ExecNode>> args,
        std::vector<std::string> keys,
        Namespace &ns);
    Ptr<Value> yield() override;
};

class InlineAssignmentNode : public ExecNode
{
    std::unique_ptr<ExecNode> arg;
    std::vector<std::string> idents;

public:
    InlineAssignmentNode(
        std::unique_ptr<ExecNode> arg, std::vector<std::string> idents, Namespace &ns) :
        ExecNode(ns), arg(std::move(arg)), idents(std::move(idents))
    {
    }

    Ptr<Value> yield() override;
};
} // namespace aquila::interpreter
