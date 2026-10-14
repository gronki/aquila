#pragma once

#include "../../../interpreter/interpreter.hpp"
#include "../../values/frame.hpp"
#include <bind_new.hpp>

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
    bool allow_parallel() const override { return false; }
};

struct DeconvOp : public Operation
{

    BIND_ARGS_NEW(&DeconvOp::run);
    ValuePtr run(const values::BufferValue &buf,
        const values::BufferValue &krn,
        const Real &strength,
        const Real &niter) const;

    const ArgManifest &arg_manifest() const override;

    std::string name() const override { return "deconv"; }
    std::string description() const override { return "DeConvolution"; }
    bool allow_parallel() const override { return false; }
};

} // namespace aquila::ops
