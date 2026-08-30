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
protected:
    int depth = 0;
    bool allow_cache = true;

public:
    virtual void set_depth(int newdepth = 0) { depth = newdepth; }
    virtual Ptr<Value> yield(ExecCtx ctx) const = 0;
    virtual void trigger_sideeffects(ExecCtx ctx) const {}
    virtual value_trace_t trace(ExecCtx ctx) const = 0;
    virtual std::optional<std::string> get_refname() const { return std::nullopt; }
    virtual bool trivial() const { return false; }
    virtual ~ExecNode() = default;
    void setup();
    virtual void assignment_cache_check(std::set<std::string> &) {}
};

class RefNode : public ExecNode
{
    std::string refname;

public:
    RefNode(const std::string &refname) : refname(refname) {}

    Ptr<Value> yield(ExecCtx ctx) const override { return ctx.ns.get(refname); }
    value_trace_t trace(ExecCtx ctx) const override
    {
        // we do not trace references. example:
        // a = 1; 2 | add 1; 2 | as a | add a -- will break cache
        if (!allow_cache || !ctx.ns.contains(refname))
            return value_trace_t::corrupt();
        return ctx.ns.get(refname)->get_trace();
    }
    bool trivial() const override { return true; }

    std::optional<std::string> get_refname() const override { return refname; }
    void assignment_cache_check(std::set<std::string> &refs) override
    {
        auto it = refs.find(refname);
        allow_cache = it == refs.end();
#ifndef NDEBUG
        if (!allow_cache)
            std::cout << "untraceable reference encountered: " << refname << std::endl;
#endif
    }
};

class AssignmentNode : public ExecNode
{
    std::string lhs;
    std::unique_ptr<ExecNode> rhs;

public:
    AssignmentNode(const std::string &lhs, std::unique_ptr<ExecNode> rhs) :
        lhs(lhs), rhs(std::move(rhs))
    {
    }

    Ptr<Value> yield(ExecCtx ctx) const override;
    value_trace_t trace(ExecCtx ctx) const override { return rhs->trace(ctx); }
    void trigger_sideeffects(ExecCtx ctx) const override { yield(ctx); }
    void set_depth(int newdepth = 0) override
    {
        depth = newdepth;
        if (rhs)
            rhs->set_depth(newdepth);
    }
    void assignment_cache_check(std::set<std::string> &refs) override
    {
        if (rhs)
            rhs->assignment_cache_check(refs);
    }
};

class ValueNode : public ExecNode
{
    std::unique_ptr<Value> value;

public:
    ValueNode(std::unique_ptr<Value> value) : value(std::move(value)) {}

    Ptr<Value> yield(ExecCtx ctx) const override { return value.get(); }
    value_trace_t trace(ExecCtx ctx) const override { return value->get_trace(); }
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
        std::vector<std::string> keys);
    Ptr<Value> yield(ExecCtx ctx) const override;
    value_trace_t trace(ExecCtx ctx) const override;
    bool trivial() const override
    {
        return op->tracing_mode() == Operation::Tracing::FROM_RETVAL;
    }
    void trigger_sideeffects(ExecCtx ctx) const override
    {
        for (const auto &arg : args)
            arg->trigger_sideeffects(ctx);
    }
    void set_depth(int newdepth = 0) override
    {
        depth = newdepth;
        for (auto &arg : args)
            if (arg)
                arg->set_depth(newdepth + 1);
    }
    void assignment_cache_check(std::set<std::string> &refs) override
    {
        for (auto &arg : args)
            if (arg)
                arg->assignment_cache_check(refs);
    }
};

class InlineAssignmentNode : public ExecNode
{
    std::unique_ptr<ExecNode> arg;
    std::vector<std::string> idents;

public:
    InlineAssignmentNode(std::unique_ptr<ExecNode> arg, std::vector<std::string> idents) :
        arg(std::move(arg)), idents(std::move(idents))
    {
    }

    Ptr<Value> yield(ExecCtx ctx) const override;
    value_trace_t trace(ExecCtx ctx) const override { return arg->trace(ctx); }
    void trigger_sideeffects(ExecCtx ctx) const override { yield(ctx); }
    void set_depth(int newdepth = 0) override
    {
        depth = newdepth;
        if (arg)
            arg->set_depth(newdepth);
    }
    void assignment_cache_check(std::set<std::string> &refs) override
    {
        if (arg)
            arg->assignment_cache_check(refs);
        for (const auto &ident : idents)
        {
#ifndef NDEBUG
            std::cout << "adding refname to ignore list: " << ident << std::endl;
#endif
            refs.insert(ident);
        }
    }
};
} // namespace aquila::interpreter
