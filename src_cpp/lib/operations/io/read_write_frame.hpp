#pragma once

#include "../../interpreter/interpreter.hpp"
#include "../../values/frame.hpp"

namespace aquila::ops
{

struct ReadFrame : public Operation
{
    BIND_ARGS(&ReadFrame::run);
    ValuePtr run(const std::string &fn) const;

    std::optional<ArgManifest> arg_manifest() const override
    {
        return ArgManifest{ArgSpec{.name = "filename"}};
    }
    std::string name() const { return "file"; }
    std::string description() const override { return "Reads a FITS frame"; }
};

struct WriteFrame : public Operation
{
    BIND_ARGS(&WriteFrame::run);
    ValuePtr run(const values::BufferValue &frame, const std::string &fn) const;

    std::optional<ArgManifest> arg_manifest() const override
    {
        return ArgManifest{
            ArgSpec{.name = "frame"},
            ArgSpec{.name = "filename"},
        };
    }
    std::string name() const { return "save"; }
    std::string description() const override
    {
        return "Saves a FITS file, choose name with suffix if exists.";
    }
};

} // namespace aquila::ops