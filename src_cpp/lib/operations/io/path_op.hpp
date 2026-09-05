#pragma once

#include <interpreter.hpp>

namespace aquila::ops
{

struct PathOp : public Operation
{
    BIND_ARGS(&PathOp::run);
    ValuePtr run(std::vector<const Str *>) const;

    const ArgManifest &arg_manifest() const override
    {
        static const ArgManifest manifest{
            ArgSpec{.name = "...", .help = "Path to expand"},
        };
        return manifest;
    }

    std::string name() const override { return "find"; }
    std::string description() const override
    {
        return "Bash-style expansion of paths. For "
               "example, file_{a,b}.fits --> file_a.fits file_b.fits";
    }
    Operation::Tracing tracing_mode() const override
    {
        return Operation::Tracing::FROM_RETVAL;
    }
    interpreter::value_trace_t custom_trace(
        const std::vector<ValuePtr> *args, const Value *retval) const override
    {
        return retval->str();
    }
};

struct PwdOp : public Operation
{
    BIND_ARGS(&PwdOp::run);
    ValuePtr run() const;

    const ArgManifest &arg_manifest() const override
    {
        static const ArgManifest manifest{};
        return manifest;
    }

    std::string name() const override { return "pwd"; }
    std::string description() const override
    {
        return "Get current working directory.";
    }
    Operation::Tracing tracing_mode() const override
    {
        return Operation::Tracing::FROM_RETVAL;
    }
    interpreter::value_trace_t custom_trace(
        const std::vector<ValuePtr> *args, const Value *retval) const override
    {
        return retval->str();
    }
};

struct ChdirOp : public Operation
{
    BIND_ARGS(&ChdirOp::run);
    ValuePtr run(const std::string &) const;

    const ArgManifest &arg_manifest() const override
    {
        static const ArgManifest manifest{
            ArgSpec{.name = "path", .help = "New working directory"},
        };
        return manifest;
    }

    std::string name() const override { return "cd"; }
    std::string description() const override
    {
        return "Change the current working directory";
    }
};

} // namespace aquila::ops
