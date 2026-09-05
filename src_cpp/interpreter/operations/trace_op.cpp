#include "trace_op.hpp"

namespace aquila::interpreter::ops
{

REGISTER(TraceOp);
ValuePtr TraceOp::run(const Value &v) const
{
    return Ptr<StrValue>::make(v.get_trace().content);
}

const ArgManifest &TraceOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "value", .sequence = true},
    };
    return manifest;
}

} // namespace aquila::interpreter::ops
