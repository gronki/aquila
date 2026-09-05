#pragma once

#include <operation.hpp>
#include <value.hpp>
#include <values/frame.hpp>

namespace aquila::ops
{

struct WritePng : public Operation
{

    BIND_ARGS(&WritePng::run);
    ValuePtr run(const values::BufferValue &, const Str &fn, const Real &bits) const;

    std::string name() const override { return "png"; }

    const ArgManifest &arg_manifest() const override;
    std::string description() const override;
};

struct WritePngRGB : public Operation
{

    BIND_ARGS(&WritePngRGB::run);
    ValuePtr run(const values::BufferValue &,
        const values::BufferValue &,
        const values::BufferValue &,
        const Str &fn,
        const Real &bits) const;

    std::string name() const override { return "png_rgb"; }

    const ArgManifest &arg_manifest() const override;
    std::string description() const override;
};

} // namespace aquila::ops
