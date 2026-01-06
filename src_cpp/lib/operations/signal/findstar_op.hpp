#pragma once

#include "../../interpreter/interpreter.hpp"
#include "../../values/frame.hpp"

namespace aquila::ops
{

struct FindstarOp : public Operation
{
    BIND_ARGS(&FindstarOp::run);
    ValuePtr run(const values::BufferValue &frame,
        const Int &limit,
        const Real &blur_radius,
        const Int &margin,
        const Real &max_rms,
        const String &rejection,
        const Int &rslice,
        const Real &thresh_sd) const;

    std::optional<ArgManifest> arg_manifest() const override
    {
        return ArgManifest{
            ArgSpec{.name = "frame"},
            ArgSpec{.name = "limit", .default_int = 256},
            ArgSpec{.name = "blur_radius", .default_real = 2.3},
            ArgSpec{.name = "margin", .default_int = 32},
            ArgSpec{.name = "max_rms", .default_real = 12},
            ArgSpec{.name = "rejection", .default_str = "abs", .help = "abs or rel"},
            ArgSpec{.name = "rslice", .default_int = 16},
            ArgSpec{.name = "thresh_sd", .default_real = 2.},
        };
    }

    std::string name() const override { return "findstar"; }
    std::string description() const override { return "Find stars in the image."; }
};

} // namespace aquila::ops