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
    virtual const Value *yield() = 0;
    virtual void clean() {};
    virtual std::optional<std::string> get_refname() const { return std::nullopt; }
    virtual ~ExecNode() = default;
};

class RefNode : public ExecNode
{
    std::string refname;
    StrValue fallback;

public:
    RefNode(const std::string &refname, Namespace &ns) :
        ExecNode(ns), refname(refname), fallback(refname)
    {
    }

    const Value *yield() override
    {
        if (ns.contains(refname))
            return &ns.get(refname);
        return &fallback;
    }

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

class OpNode : public ExecNode
{
    std::unique_ptr<Operation> op;
    std::unique_ptr<Value> value;
    std::vector<std::unique_ptr<ExecNode>> args;
    std::vector<ArgMatch> match;
    std::vector<std::string> keys;
    manifest_properties_t props;
    std::vector<int> expansion, is_keyword;

public:
    OpNode(std::unique_ptr<Operation> op,
        std::vector<std::unique_ptr<ExecNode>> args,
        std::vector<std::string> keys,
        Namespace &ns) :
        ExecNode(ns), op(std::move(op)), args(std::move(args)), keys(std::move(keys))
    {
        try
        {
            auto manifest = this->op->arg_manifest();
            props.analyze(manifest);
            match = match_arguments(manifest, props, this->keys);
            for (const auto &key : this->keys)
            {
                expansion.push_back(key == std::string(1, EXPAND_DELIM));
                is_keyword.push_back(!key.empty() && !expansion.back());
            }
        }
        catch (std::exception &e)
        {
            throw std::runtime_error(
                std::string("operation ") + this->op->name() + ": " + e.what());
        }
    }

    const Value *yield() override;

    void clean() override { value = nullptr; }
};

class InlineAssignmentNode : public ExecNode
{
    std::unique_ptr<Value> value;
    std::unique_ptr<ExecNode> arg;
    std::vector<std::string> idents;

public:
    InlineAssignmentNode(
        std::unique_ptr<ExecNode> arg, std::vector<std::string> idents, Namespace &ns) :
        ExecNode(ns), arg(std::move(arg)), idents(std::move(idents))
    {
    }

    const Value *yield() override;

    void clean() override { value = nullptr; }
};
} // namespace aquila::interpreter
