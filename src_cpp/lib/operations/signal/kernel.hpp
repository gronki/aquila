#pragma once

#include <bind_new.hpp>
#include "../../../interpreter/interpreter.hpp"
#include "../../values/frame.hpp"

namespace aquila::ops
{

struct KernelOp : public Operation
{
    BIND_ARGS_NEW(&KernelOp::run);
    ValuePtr run(const double &fwhm, const std::string &type) const;

    const ArgManifest &arg_manifest() const override;
    std::string name() const override { return "kernel"; }
    std::string description() const override
    {
        return "Builds a kernel to use for convolution. ";
    }
};

} // namespace aquila::ops