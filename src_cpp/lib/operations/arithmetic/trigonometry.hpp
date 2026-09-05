#pragma once

#include "../../../interpreter/interpreter.hpp"

namespace aquila::ops
{

struct SinOp : public Operation
{
    BIND_ARGS(&SinOp::run);
    ValuePtr run(const Value &x) const;

    const ArgManifest &arg_manifest() const override
    {
        static const ArgManifest manifest{ArgSpec{.name = "x"}};
        return manifest;
    }

    std::string name() const { return "sin"; }
};

struct CosOp : public Operation
{
    BIND_ARGS(&CosOp::run);
    ValuePtr run(const Value &x) const;

    const ArgManifest &arg_manifest() const override
    {
        static const ArgManifest manifest{ArgSpec{.name = "x"}};
        return manifest;
    }

    std::string name() const { return "cos"; }
};

struct AsinhOp : public Operation
{
    BIND_ARGS(&AsinhOp::run);
    ValuePtr run(const Value &, const Real &) const;

    const ArgManifest &arg_manifest() const override
    {
        static const ArgManifest manifest{
            ArgSpec{.name = "x"},
            ArgSpec{.name = "factor", .default_real = 1, .help = "Compression factor"},
        };
        return manifest;
    }

    std::string name() const { return "asinh"; }
};

} // namespace aquila::ops
