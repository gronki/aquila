#pragma once

#include "../operation.hpp"
#include "../value.hpp"

namespace aquila::interpreter::ops
{

struct SequenceOp : Operation
{
    std::unique_ptr<Value> call(const std::vector<const Value *> &args) const override;
    std::string name() const override { return "array"; }
};

REGISTER(SequenceOp);

} // namespace aquila::interpreter