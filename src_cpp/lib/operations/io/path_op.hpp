#pragma once

#include <interpreter.hpp>

namespace aquila::ops
{

struct PathOp : public Operation
{
    BIND_ARGS(&PathOp::run);
    ValuePtr run(const std::string &param) const;

    ArgManifest arg_manifest() const override
    {
        return ArgManifest{
            ArgSpec{.name = "path", .default_str = "*", .help = "Path to expand"},
        };
    }

    std::string name() const { return "ls"; }
    std::string description() const override
    {
        return "Bash-style expansion of paths. For "
               "example, file_{a,b}.fits --> file_a.fits file_b.fits";
    }
};

struct PwdOp : public Operation
{
    BIND_ARGS(&PwdOp::run);
    ValuePtr run() const;

    ArgManifest arg_manifest() const override { return ArgManifest{}; }

    std::string name() const { return "pwd"; }
    std::string description() const override
    {
        return "Get current working directory.";
    }
};

struct ChdirOp : public Operation
{
    BIND_ARGS(&ChdirOp::run);
    ValuePtr run(const std::string &) const;

    ArgManifest arg_manifest() const override
    {
        return ArgManifest{
            ArgSpec{.name = "path", .help = "New working directory"},
        };
    }

    std::string name() const { return "cd"; }
    std::string description() const override
    {
        return "Change the current working directory";
    }
};

} // namespace aquila::ops
