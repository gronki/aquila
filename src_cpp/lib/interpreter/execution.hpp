#pragma once

#include <iostream>

#include "namespace.hpp"
#include "operation.hpp"
#include "value.hpp"

namespace aquila::interpreter
{

class ExecNode
{
protected:
    Namespace &ns;

public:
    ExecNode(Namespace &ns) : ns(ns) {}
    enum class Modifier
    {
        NONE,
        EXPANSION,
        CONTRACTION
    };
    virtual const Value *yield() = 0;
    virtual void clean() {};
    virtual Modifier modifier() const { return Modifier::NONE; }
    virtual ~ExecNode() = default;
};

class RefNode : public ExecNode
{
    std::string refname;

public:
    RefNode(const std::string &refname, Namespace &ns) : ExecNode(ns), refname(refname)
    {
    }

    const Value *yield() override { return &ns.get(refname); }
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

    const Value *yield() override
    {
        const Value *rhs_yield = rhs->yield();
        ns.push(lhs, rhs_yield->clone());
        return rhs_yield;
    }

    void clean() override { rhs->clean(); }
};

class ValueNode : public ExecNode
{
    std::unique_ptr<Value> value;

public:
    ValueNode(std::unique_ptr<Value> value, Namespace &ns) :
        ExecNode(ns), value(std::move(value))
    {
    }

    const Value *yield() override { return value.get(); }
};

class WrapperNode : public ExecNode
{
public:
private:
    std::unique_ptr<ExecNode> wrapped;
    ExecNode::Modifier kind;

public:
    WrapperNode(std::unique_ptr<ExecNode> wrapped, ExecNode::Modifier kind, Namespace &ns) :
        ExecNode(ns), wrapped(std::move(wrapped)), kind(kind)
    {
    }

    const Value *yield() override { return wrapped->yield(); }
    void clean() override { wrapped->clean(); }
    Modifier modifier() const override { return kind; }
};

class OpNode : public ExecNode
{
    std::unique_ptr<Operation> op;
    std::unique_ptr<Value> value;
    std::vector<std::unique_ptr<ExecNode>> args;
    std::vector<ArgMatch> match;
    std::vector<SanitizerFactory> sanitizers;
    bool use_match = false;

public:
    OpNode(std::unique_ptr<Operation> op,
        std::vector<std::unique_ptr<ExecNode>> args,
        const std::vector<std::string> &keys,
        Namespace &ns) : ExecNode(ns), op(std::move(op)), args(std::move(args))
    {
        std::optional<ArgManifest> maybe_manifest = this->op->arg_manifest();
        use_match = maybe_manifest.has_value();
        std::cout << "building op " << this->op->name() << " manifest? " << use_match
                  << " args " << this->args.size() << std::endl;
        if (!use_match)
            return;
        auto &manifest = maybe_manifest.value();
        for (const auto &arg : this->args)
        {
            if (arg->modifier() == ExecNode::Modifier::EXPANSION)
                throw std::runtime_error(
                    std::string("Expansion (*) may not be used on operation ")
                    + this->op->name());
        }
        match = match_arguments(manifest, keys);
        sanitizers.reserve(manifest.size());
        for (auto &argspec : manifest)
        {
            sanitizers.push_back(argspec.convert);
        }
    }

    const Value *yield() override;

    void clean() override { value = nullptr; }
};

} // namespace aquila::interpreter