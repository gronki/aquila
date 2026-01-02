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
    virtual const Value *yield() = 0;
    virtual void clean() {};
    virtual ~ExecNode() = default;
};

class RefNode : public ExecNode
{
    String refname;

public:
    RefNode(const String &refname, Namespace &ns) : ExecNode(ns), refname(refname) {}

    const Value *yield() override { return &ns.get(refname); }
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
    bool use_match = false;

public:
    OpNode(std::unique_ptr<Operation> op,
        std::vector<std::unique_ptr<ExecNode>> args,
        const std::vector<String> &keys,
        Namespace &ns) : ExecNode(ns), op(std::move(op)), args(std::move(args))
    {
        std::optional<ArgManifest> maybe_manifest = this->op->arg_manifest();
        use_match = maybe_manifest.has_value();
        if (use_match)
            match = match_arguments(maybe_manifest.value(), keys);
    }

    const Value *yield() override;

    void clean() override { value = nullptr; }
};

} // namespace aquila::interpreter