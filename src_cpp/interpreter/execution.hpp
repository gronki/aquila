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
    virtual Ptr<Value> yield() const = 0;
    virtual value_trace_t trace() const = 0;
    virtual std::optional<std::string> get_refname() const { return std::nullopt; }
    virtual bool trivial() const { return false; }
    virtual ~ExecNode() = default;
};

class RefNode : public ExecNode
{
    std::string refname;

public:
    RefNode(const std::string &refname, Namespace &ns) : ExecNode(ns), refname(refname)
    {
    }

    Ptr<Value> yield() const override { return ns.get(refname); }
    value_trace_t trace() const override { return ns.get(refname)->get_trace(); }
    bool trivial() const override { return true; }

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

    Ptr<Value> yield() const override;
    value_trace_t trace() const override { return rhs->trace(); }
};

class ValueNode : public ExecNode
{
    std::unique_ptr<Value> value;

public:
    ValueNode(std::unique_ptr<Value> value, Namespace &ns) :
        ExecNode(ns), value(std::move(value))
    {
    }

    Ptr<Value> yield() const override { return value.get(); }
    value_trace_t trace() const override { return value->get_trace(); }
    bool trivial() const override { return true; }
};

class OpNode : public ExecNode
{
    std::unique_ptr<Operation> op;
    std::vector<std::unique_ptr<ExecNode>> args;
    ArgManifest manifest;
    manifest_properties_t props;
    std::vector<ArgMatch> match;

public:
    OpNode(std::unique_ptr<Operation> op,
        std::vector<std::unique_ptr<ExecNode>> args,
        std::vector<std::string> keys,
        Namespace &ns);
    Ptr<Value> yield() const override;
    value_trace_t trace() const override;
    bool trivial() const override
    {
        return op->tracing_mode() == Operation::Tracing::FROM_RETVAL;
    }
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

    Ptr<Value> yield() const override;
    value_trace_t trace() const override { return arg->trace(); }
};
} // namespace aquila::interpreter
