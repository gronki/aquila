#pragma once

#include <aquila.h>
#include <bind_new.hpp>
#include <interpreter.hpp>
#include <values/frame.hpp>

namespace aquila::ops
{

struct BackgroundOp : public Operation
{
    BIND_ARGS_NEW(&BackgroundOp::run);
    ValuePtr run(const values::BufferValue &frame,
        interpreter::Struct<bkgradient_param_t> params) const;

    const ArgManifest &arg_manifest() const override;
    std::string name() const override { return "bg"; }
    std::string description() const override
    {
        return "Estimate the smooth background of the image and return it as a "
               "frame of the same size.";
    }
};

} // namespace aquila::ops
