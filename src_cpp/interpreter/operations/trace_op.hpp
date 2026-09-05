#pragma once

#include "../interpreter.hpp"

namespace aquila::interpreter::ops
{

struct TraceOp : Operation
{
    BIND_ARGS(&TraceOp::run);
    ValuePtr run(const Value &) const;
    std::string name() const override { return "trace"; }
    const ArgManifest &arg_manifest() const override;
};

} // namespace aquila::interpreter::ops
