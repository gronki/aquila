#pragma once

#include "../../interpreter/interpreter.hpp"
#include "../../values/frame.hpp"

namespace aquila::ops
{

struct ConvolOp : public Operation
{

    BIND_ARGS(&ConvolOp::run);
    ValuePtr run(const values::BufferValue &buf,
        const values::BufferValue &krn,
        const std::string &edges) const;

    std::optional<ArgManifest> arg_manifest() const override;

    std::string name() const override { return "conv"; }
    std::string description() const override { return "Convolution"; }
};

} // namespace aquila::ops