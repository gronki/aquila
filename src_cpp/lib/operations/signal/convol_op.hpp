#pragma once

#include "../../interpreter/interpreter.hpp"
#include "../../values/frame.hpp"

namespace aquila::ops
{

struct ConvolOp : public Operation
{
    // std::unique_ptr<Value> call(const std::vector<const Value *> &) const override;

    BIND_ARGS(&ConvolOp::run);
    ValuePtr run(const values::BufferValue &buf,
        const values::BufferValue &krn,
        const std::string &edges) const;

    std::optional<ArgManifest> arg_manifest() const override
    {
        return ArgManifest{
            ArgSpec{.name = "buffer"},
            ArgSpec{.name = "kernel"},
            ArgSpec{.name = "edges", .default_str = "e", .help="How to fix edges?"},
        };
    }

    std::string name() const override { return "conv"; }
    std::string description() const override { return "Convolution"; }
};

} // namespace aquila::ops