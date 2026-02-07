#pragma once

#include "../../../interpreter/interpreter.hpp"
#include "../../values/frame.hpp"

namespace aquila::ops
{

struct StackOp : public Operation
{
    BIND_ARGS(&StackOp::run);
    ValuePtr run(const SequenceValue& data, const std::string& method) const;

    ArgManifest arg_manifest() const override;
    std::string name() const override { return "stack"; }
    std::string description() const override { return ""; }
};

} // namespace aquila::ops