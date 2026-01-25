#pragma once

#include "../../../interpreter/interpreter.hpp"
#include "../../values/frame.hpp"

namespace aquila::ops
{

struct FindstarOp : public Operation
{
    BIND_ARGS(&FindstarOp::run);
    ValuePtr run(const values::BufferValue &frame,
        const std::int64_t &limit,
        const double &blur_radius,
        const std::int64_t &margin,
        const double &max_rms,
        const std::string &rejection,
        const std::int64_t &rslice,
        const double &thresh_sd) const;

    ArgManifest arg_manifest() const override;
    std::string name() const override { return "findstar"; }
    std::string description() const override { return "Find stars in the image."; }
};

} // namespace aquila::ops