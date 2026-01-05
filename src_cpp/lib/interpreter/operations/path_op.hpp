#pragma once

#include "../../interpreter/interpreter.hpp"
#include "../../values/frame.hpp"

namespace aquila::ops
{

struct PathOp : public Operation
{
    BIND_ARGS(&PathOp::run);
    ValuePtr run(const String &param) const;

    std::optional<ArgManifest> arg_manifest() const override
    {
        return ArgManifest{
            ArgSpec{.name = "path", .help = "Path to expand"},
        };
    }

    std::string name() const { return "path"; }
};

} // namespace aquila::ops