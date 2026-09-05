#pragma once

#include <operation.hpp>
#include <value.hpp>
#include <values/frame.hpp>

namespace aquila::ops
{

struct NormalizeOp : public Operation
{
    BIND_ARGS(&NormalizeOp::run);
    ValuePtr run(Ptr<SequenceValue> buffers, const Real &margin) const;

    std::string name() const override { return "normalize"; }

    const ArgManifest &arg_manifest() const override;
    std::string description() const override;
};

} // namespace aquila::ops
