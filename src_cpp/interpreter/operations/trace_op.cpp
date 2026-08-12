#include "trace_op.hpp"

namespace aquila::interpreter::ops
{

REGISTER(TraceOp);
ValuePtr TraceOp::run(const Value &v) const
{
    return Ptr<StrValue>::make(v.get_trace().content);
}

ArgManifest TraceOp::arg_manifest() const
{
    return ArgManifest{
        ArgSpec{.name = "value", .sequence = true},
    };
}

} // namespace aquila::interpreter::ops
