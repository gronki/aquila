#pragma once

#include <operation.hpp>
#include <value.hpp>
#include <values/frame.hpp>

namespace aquila::ops
{

struct NormalizeOp : public Operation
{
    BIND_ARGS(&NormalizeOp::run);
    ValuePtr run(const Real &margin, std::vector<Ptr<values::BufferValue>>) const;

    std::string name() const override { return "normalize"; }

    ArgManifest arg_manifest() const override;
    std::string description() const override;
};

} // namespace aquila::ops
