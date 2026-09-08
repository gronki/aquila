#pragma once

#include <aquila.h>
#include <bind_new.hpp>
#include <interpreter.hpp>
#include <values/frame.hpp>

namespace aquila::ops
{

struct FindstarOp : public Operation
{
    ValuePtr call(std::vector<ValuePtr>) const override;
    ValuePtr run(const values::BufferValue &frame,
        const std::int64_t &limit,
        interpreter::Struct<findstar_params_t> params) const;

    const ArgManifest &arg_manifest() const override;
    std::string name() const override { return "findstar"; }
    std::string description() const override { return "Find stars in the image."; }
};

} // namespace aquila::ops
