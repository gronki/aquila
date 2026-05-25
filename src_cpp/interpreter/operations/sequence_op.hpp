#pragma once

#include "../interpreter.hpp"

namespace aquila::interpreter::ops
{

struct SequenceOp : Operation
{
    std::unique_ptr<Value> call(const std::vector<const Value *> &args) const override;
    std::string name() const override { return "seq"; }
    ArgManifest arg_manifest() const override;
};

struct ZipOp : Operation
{
    std::unique_ptr<Value> call(const std::vector<const Value *> &args) const override;
    std::string name() const override { return "zip"; }
    ArgManifest arg_manifest() const override;
};

} // namespace aquila::interpreter::ops
