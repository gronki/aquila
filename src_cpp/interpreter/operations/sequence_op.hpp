#pragma once

#include "../interpreter.hpp"

namespace aquila::interpreter::ops
{

struct SequenceOp : Operation
{
    ValuePtr call(std::vector<ValuePtr> args) const override;
    std::string name() const override { return "seq"; }
    ArgManifest arg_manifest() const override;
};

struct ZipOp : Operation
{
    ValuePtr call(std::vector<ValuePtr> args) const override;
    std::string name() const override { return "zip"; }
    ArgManifest arg_manifest() const override;
};

} // namespace aquila::interpreter::ops
