#pragma once

#include <interpreter.hpp>

namespace aquila::interpreter::ops
{

struct SequenceOp : Operation
{
    ValuePtr call(std::vector<ValuePtr> args) const override;
    std::string name() const override { return "seq"; }
    ArgManifest arg_manifest() const override;
    bool cacheable() const override { return false; }
};

struct ItemOp : public Operation
{

    BIND_ARGS(&ItemOp::run);
    ValuePtr run(Ptr<SequenceValue> seq, Real ix) const;

    std::string name() const override { return "item"; }

    ArgManifest arg_manifest() const override;
    std::string description() const override;
};

struct ZipOp : Operation
{
    ValuePtr call(std::vector<ValuePtr> args) const override;
    std::string name() const override { return "zip"; }
    ArgManifest arg_manifest() const override;
    bool cacheable() const override { return false; }
};

} // namespace aquila::interpreter::ops
