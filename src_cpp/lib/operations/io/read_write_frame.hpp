#pragma once

#include "../../../interpreter/interpreter.hpp"
#include "../../values/frame.hpp"
#include <utils/expand_path.hpp>

namespace aquila::convert
{

// expands a glob/brace pattern into a sequence of matched paths;
// returns nullptr (no conversion) when there is exactly one match,
// so that op_call_with_sequencing keeps calling the op once per file
// with a single StrValue arg -- required for per-file cache tracing
// (see ReadFrame::custom_trace) to key on the individual file's mtime.
inline std::unique_ptr<Value> expandGlob(const StrValue &s)
{
    auto paths = utils::expand_path(s.value);
    if (paths.size() <= 1)
        return nullptr;

    std::vector<Ptr<Value>> items;
    items.reserve(paths.size());
    for (const auto &p : paths)
        items.push_back(Ptr<StrValue>::make(p.string()));
    return std::make_unique<SequenceValue>(std::move(items));
}

} // namespace aquila::convert

namespace aquila::ops
{

struct ReadFrame : public Operation
{
    BIND_ARGS(&ReadFrame::run);
    ValuePtr run(const std::string &fn) const;

    ArgManifest arg_manifest() const override
    {
        return ArgManifest{
            ArgSpec{.name = "filename", .convert = guard(convert::expandGlob)}};
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
