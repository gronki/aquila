#pragma once

#include "../../../interpreter/interpreter.hpp"
#include "../../values/frame.hpp"

namespace aquila::ops
{

struct ReadFrame : public Operation
{
    BIND_ARGS(&ReadFrame::run);
    ValuePtr run(const std::string &fn) const;

    ArgManifest arg_manifest() const override
    {
        return ArgManifest{ArgSpec{.name = "filename"}};
    }
    std::string name() const override { return "load"; }
    std::string description() const override { return "Reads a FITS frame"; }
    Operation::Tracing tracing_mode() const override
    {
        return Operation::Tracing::FROM_INPUTS;
    }
    interpreter::value_trace_t custom_trace(
        const std::vector<ValuePtr> *args, const Value *retval) const override;
};

struct WriteFrame : public Operation
{
    BIND_ARGS(&WriteFrame::run);
    ValuePtr run(const values::BufferValue &frame, const std::string &fn) const;

    ArgManifest arg_manifest() const override
    {
        return ArgManifest{
            ArgSpec{.name = "frame"},
            ArgSpec{.name = "filename"},
        };
    }
    std::string name() const override { return "save"; }
    std::string description() const override
    {
        return "Saves a FITS file, choose name with suffix if exists.";
    }
};

} // namespace aquila::ops
