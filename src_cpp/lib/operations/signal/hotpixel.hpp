#pragma once

#include <operation.hpp>
#include <value.hpp>
#include <values/frame.hpp>

namespace aquila::ops
{

struct HotFindOp : public Operation
{

    BIND_ARGS(&HotFindOp::run);
    ValuePtr run(const values::BufferValue &buf, const Real &sigma) const;

    std::string name() const override { return "hot_find"; }

    ArgManifest arg_manifest() const override;
    std::string description() const override;
};

struct HotFixOp : public Operation
{

    BIND_ARGS(&HotFixOp::run);
    ValuePtr run(const values::BufferValue &buf, const values::BufferValue &mask) const;

    std::string name() const override { return "hot_fix"; }

    ArgManifest arg_manifest() const override;
    std::string description() const override;
};

struct HotFixLightOp : public Operation
{

    BIND_ARGS(&HotFixLightOp::run);
    ValuePtr run(const values::BufferValue &buf, const Real &sigma) const;

    std::string name() const override { return "hot_fix_light"; }

    ArgManifest arg_manifest() const override;
    std::string description() const override;
};

} // namespace aquila::ops
