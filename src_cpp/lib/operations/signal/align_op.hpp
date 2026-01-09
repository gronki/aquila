#pragma once

#include "../../interpreter/interpreter.hpp"
#include "../../values/sourcelist.hpp"
#include "../../values/transform_value.hpp"

namespace aquila::ops
{

struct AlignOp : public Operation
{
    BIND_ARGS(&AlignOp::run);
    ValuePtr run(const values::SourceListValue &lst0,
        const values::SourceListValue &lst,
        const std::string &method) const;

    std::optional<ArgManifest> arg_manifest() const override
    {
        return ArgManifest{
            ArgSpec{.name = "list0"},
            ArgSpec{.name = "list"},
            ArgSpec{.name = "method",
                .default_str = "gravity",
                .help = "options: gravity, gravity_only, polygon"},
        };
    }

    std::string name() const override { return "align"; }
    std::string description() const override
    {
        return "Finds a transformation between two lists of sources (stars).";
    }
};

} // namespace aquila::ops