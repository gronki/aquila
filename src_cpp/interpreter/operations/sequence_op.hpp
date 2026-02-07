#pragma once

#include "../interpreter.hpp"

namespace aquila::interpreter::ops
{

struct SequenceOp : Operation
{
    std::unique_ptr<Value> call(const std::vector<const Value *> &args) const override;
    std::string name() const override { return "array"; }
    ArgManifest arg_manifest() const override;
};
// array(array(1,2,3),array(4,5,6),7)
} // namespace aquila::interpreter::ops