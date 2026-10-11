#pragma once

#include <bind_new.hpp>
#include "../interpreter.hpp"

namespace aquila::interpreter::ops
{

struct TraceOp : Operation
{
    BIND_ARGS_NEW(&TraceOp::run);
    ValuePtr run(Ptr<Value>) const;
    std::string name() const override { return "trace"; }
    const ArgManifest &arg_manifest() const override;
};

} // namespace aquila::interpreter::ops
