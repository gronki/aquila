#pragma once

#include "../../../interpreter/interpreter.hpp"
#include "../../values/frame.hpp"

namespace aquila::ops
{

struct ExampleOp : public Operation
{
    // std::unique_ptr<Value> call(const std::vector<const Value *> &) const override;

    BIND_ARGS(&ExampleOp::run);
    ValuePtr run(const std::string &param) const;

    std::string name() const override { return "example"; }

    ArgManifest arg_manifest() const override;
    std::string description() const override;
};

} // namespace aquila::ops