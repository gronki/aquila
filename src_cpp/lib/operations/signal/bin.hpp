#pragma once

#include <operation.hpp>
#include <value.hpp>
#include <values/frame.hpp>

namespace aquila::ops
{

struct BinOp : public Operation
{
    BIND_ARGS(&BinOp::run);
    ValuePtr run(const values::BufferValue &in, const Real &scale) const;

    std::string name() const override { return "bin"; }

    const ArgManifest &arg_manifest() const override;
    std::string description() const override;
};

} // namespace aquila::ops
