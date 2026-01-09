#pragma once

#include "../../interpreter/interpreter.hpp"
#include "../../values/frame.hpp"

namespace aquila::ops
{

struct KernelOp : public Operation
{
    BIND_ARGS(&KernelOp::run);
    ValuePtr run(const double &fwhm, const std::string &type) const;

    std::optional<ArgManifest> arg_manifest() const override
    {
        return ArgManifest{
            ArgSpec{.name = "fwhm"},
            ArgSpec{.name = "type", .default_str = "gauss", .help = "options: gauss, mexha"},
        };
    }

    std::string name() const override { return "kernel"; }
    std::string description() const override
    {
        return "Builds a kernel to use for convolution. ";
    }
};

} // namespace aquila::ops