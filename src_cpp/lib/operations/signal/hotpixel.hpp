#pragma once

#include <bind_new.hpp>
#include <operation.hpp>
#include <value.hpp>
#include <values/frame.hpp>

namespace aquila::ops
{

struct HotFindOp : public Operation
{

    BIND_ARGS_NEW(&HotFindOp::run);
    ValuePtr run(const values::BufferValue &buf, const Real &sigma) const;

    std::string name() const override { return "hot_find"; }

    const ArgManifest &arg_manifest() const override;
    std::string description() const override;
};

struct HotFixOp : public Operation
{

    BIND_ARGS_NEW(&HotFixOp::run);
    ValuePtr run(const values::BufferValue &buf, const values::BufferValue &mask) const;

    std::string name() const override { return "hot_fix"; }

    const ArgManifest &arg_manifest() const override;
    std::string description() const override;
};

struct HotFixLightOp : public Operation
{

    BIND_ARGS_NEW(&HotFixLightOp::run);
    ValuePtr run(const values::BufferValue &buf, const Real &sigma) const;

    std::string name() const override { return "hot_fix_light"; }

    const ArgManifest &arg_manifest() const override;
    std::string description() const override;
};

} // namespace aquila::ops
