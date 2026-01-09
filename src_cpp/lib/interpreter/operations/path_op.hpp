#pragma once

#include "../../interpreter/interpreter.hpp"
#include "../../values/frame.hpp"

namespace aquila::ops
{

struct PathOp : public Operation
{
    BIND_ARGS(&PathOp::run);
    ValuePtr run(const std::string &param) const;

    std::optional<ArgManifest> arg_manifest() const override
    {
        return ArgManifest{
            ArgSpec{.name = "path", .help = "Path to expand"},
        };
    }

    std::string name() const { return "path"; }
    std::string description() const override { return "Bash-style expansion of paths. For "
        "example, file_{a,b}.fits --> file_a.fits file_b.fits"; }
};

} // namespace aquila::ops