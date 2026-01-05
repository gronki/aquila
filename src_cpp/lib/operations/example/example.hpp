#pragma once

#include "../../interpreter/interpreter.hpp"
#include "../../values/frame.hpp"

namespace aquila::ops
{

struct ExampleOp : public Operation
{
    // std::unique_ptr<Value> call(const std::vector<const Value *> &) const;

    BIND_ARGS(&ExampleOp::run);
    ValuePtr run(const String &param) const;

    std::optional<ArgManifest> arg_manifest() const override
    {
        return ArgManifest{ArgSpec{.name = "param"}};
    }

    std::string name() const { return "example"; }
};

} // namespace aquila::ops