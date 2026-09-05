#pragma once

#include <operation.hpp>
#include <value.hpp>
#include <values/frame.hpp>
#include <values/transform_value.hpp>

namespace aquila::ops
{

struct ProjectOp : public Operation
{

    BIND_ARGS(&ProjectOp::run);
    ValuePtr run(const values::TransformValue &,
        const values::BufferValue &,
        const Real &resample) const;

    std::string name() const override { return "project"; }

    const ArgManifest &arg_manifest() const override;
    std::string description() const override;
};

} // namespace aquila::ops
