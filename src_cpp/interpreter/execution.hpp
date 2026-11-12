#pragma once

#include "cache.hpp"
#include "namespace.hpp"
#include "operation.hpp"
#include "value.hpp"
#include <optional>
#include <set>
#include <string>

namespace aquila::interpreter
{

struct ExecCtx
{
    Namespace &ns;
    CacheSpace *cache;
};

class ExecNode
{
    virtual value_trace_t trace(ExecCtx ctx) const = 0;
    value_trace_t node_trace;

public:
    virtual Ptr<Value> yield(ExecCtx ctx) const = 0;
    virtual void trigger_sideeffects(ExecCtx ctx) const {}
    const value_trace_t &compute_trace(ExecCtx ctx)
    {
        node_trace = trace(ctx);
#ifndef NDEBUG
        std::cout << "node trace: " << node_trace << std::endl;
#endif
        return node_trace;
    }
    const value_trace_t &get_trace() const { return node_trace; }
    virtual std::optional<std::string> get_refname() const { return std::nullopt; }
    /**
     * Whether the node can be yielded during the trace pass: it is cheap and nothing
     * under it resolves to a placeholder (dummy) value in the namespace of `ctx`.
     */
    virtual bool trivial(ExecCtx ctx) const { return false; }
    virtual ~ExecNode() = default;
    void setup(ExecCtx);
};

class RefNode : public ExecNode
{
    std::string refname;
    value_trace_t trace(ExecCtx ctx) const override;

public:
    RefNode(const std::string &refname) : refname(refname) {}

    Ptr<Value> yield(ExecCtx ctx) const override { return ctx.ns.get(refname); }
    bool trivial(ExecCtx ctx) const override;

    std::optional<std::string> get_refname() const override { return refname; }
};

class AssignmentNode : public ExecNode
{
    std::string lhs;
    std::unique_ptr<ExecNode> rhs;
    value_trace_t trace(ExecCtx ctx) const override { return rhs->compute_trace(ctx); }

public:
    AssignmentNode(const std::string &lhs, std::unique_ptr<ExecNode> rhs) :
        lhs(lhs), rhs(std::move(rhs))
    {
    }

    Ptr<Value> yield(ExecCtx ctx) const override;
    void trigger_sideeffects(ExecCtx ctx) const override { yield(ctx); }
    bool trivial(ExecCtx ctx) const override { return rhs->trivial(ctx); }
};

class ValueNode : public ExecNode
{
    ValuePtr value;
    value_trace_t trace(ExecCtx ctx) const override { return value.get_trace(); }

public:
    ValueNode(ValuePtr value) : value(std::move(value)) {}

    Ptr<Value> yield(ExecCtx ctx) const override { return value; }
    bool trivial(ExecCtx ctx) const override { return true; }
};

class OpNode : public ExecNode
{
    std::unique_ptr<Operation> op;
    std::vector<std::unique_ptr<ExecNode>> args;
    const ArgManifest &manifest;
    std::vector<ArgMatch> match;
    value_trace_t trace(ExecCtx ctx) const override;

public:
    OpNode(std::unique_ptr<Operation> op,
        std::vector<std::unique_ptr<ExecNode>> args,
        std::vector<std::string> keys);
    Ptr<Value> yield(ExecCtx ctx) const override;
    bool trivial(ExecCtx ctx) const override;
    void trigger_sideeffects(ExecCtx ctx) const override;
};

class InlineAssignmentNode : public ExecNode
{
    std::unique_ptr<ExecNode> arg;
    std::vector<std::string> idents;
    value_trace_t trace(ExecCtx ctx) const override;

public:
    InlineAssignmentNode(std::unique_ptr<ExecNode> arg, std::vector<std::string> idents) :
        arg(std::move(arg)), idents(std::move(idents))
    {
    }

    Ptr<Value> yield(ExecCtx ctx) const override;
    void trigger_sideeffects(ExecCtx ctx) const override;
    bool trivial(ExecCtx ctx) const override { return arg->trivial(ctx); }
};
} // namespace aquila::interpreter
