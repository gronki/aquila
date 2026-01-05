#pragma once

#include "../../interpreter/interpreter.hpp"
#include "../../values/frame.hpp"

namespace aquila::ops
{

struct ReadFrame : public Operation
{
    BIND_ARGS(&ReadFrame::run);
    ValuePtr run(const String &fn) const;

    std::optional<ArgManifest> arg_manifest() const override
    {
        return ArgManifest{ArgSpec{.name = "filename"}};
    }
    std::string name() const { return "file"; }
};

struct WriteFrame : public Operation
{
    BIND_ARGS(&WriteFrame::run);
    ValuePtr run(const values::BufferValue &frame, const String &fn) const;

    std::optional<ArgManifest> arg_manifest() const override
    {
        return ArgManifest{
            ArgSpec{.name = "frame"},
            ArgSpec{.name = "filename"},
        };
    }
    std::string name() const { return "save"; }
};

} // namespace aquila::ops