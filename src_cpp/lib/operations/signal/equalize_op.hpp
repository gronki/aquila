#pragma once

#include <aquila.h>
#include <bind_new.hpp>
#include <operation.hpp>
#include <value.hpp>
#include <values/frame.hpp>

namespace aquila::ops
{

struct EqualizeOp : public Operation
{

    BIND_ARGS_NEW(&EqualizeOp::run);
    ValuePtr run(Ptr<SequenceValue> bufs,
        const Str &what,
        interpreter::Struct<bkeq_param_t> params) const;

    std::string name() const override { return "wb"; }

    const ArgManifest &arg_manifest() const override;
};

} // namespace aquila::ops
