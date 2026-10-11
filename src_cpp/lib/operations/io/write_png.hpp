#pragma once

#include <bind_new.hpp>
#include <operation.hpp>
#include <value.hpp>
#include <values/frame.hpp>

namespace aquila::ops
{

struct WritePng : public Operation
{

    BIND_ARGS_NEW(&WritePng::run);
    ValuePtr run(Ptr<values::BufferValue>, const Str &fn, const Real &bits) const;

    std::string name() const override { return "png"; }

    const ArgManifest &arg_manifest() const override;
    std::string description() const override;
};

struct WritePngRGB : public Operation
{

    BIND_ARGS_NEW(&WritePngRGB::run);
    ValuePtr run(Ptr<values::BufferValue>,
        Ptr<values::BufferValue>,
        Ptr<values::BufferValue>,
        const Str &fn,
        const Real &bits) const;

    std::string name() const override { return "png_rgb"; }

    const ArgManifest &arg_manifest() const override;
    std::string description() const override;
};

} // namespace aquila::ops
