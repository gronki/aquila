#pragma once

#include "../interpreter.hpp"

namespace aquila::interpreter::ops
{

struct SequenceOp : Operation
{
    std::unique_ptr<Value> call(const std::vector<const Value *> &args) const override;
    std::string name() const override { return "array"; }
};

} // namespace aquila::interpreter