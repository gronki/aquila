#pragma once

#include <bind_new.hpp>
#include "../../../interpreter/interpreter.hpp"
#include "../../values/frame.hpp"

namespace aquila::ops
{

struct ConvolOp : public Operation
{

    BIND_ARGS_NEW(&ConvolOp::run);
    ValuePtr run(const values::BufferValue &buf,
        const values::BufferValue &krn,
        const std::string &edges) const;

    const ArgManifest &arg_manifest() const override;

    std::string name() const override { return "conv"; }
    std::string description() const override { return "Convolution"; }
};

struct DeconvOp : public Operation
{

    BIND_ARGS_NEW(&DeconvOp::run);
    ValuePtr run(const values::BufferValue &buf,
            const values::BufferValue& krn,
            const Real& strength,
            const Real& niter) const;

    const ArgManifest &arg_manifest() const override;

    std::string name() const override { return "deconv"; }
    std::string description() const override { return "DeConvolution"; }
};

} // namespace aquila::ops
