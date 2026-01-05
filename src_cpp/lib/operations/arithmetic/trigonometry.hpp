#pragma once

#include "../../interpreter/interpreter.hpp"

namespace aquila::ops
{

struct SinOp : public Operation
{
    BIND_ARGS(&SinOp::run);
    ValuePtr run(const Value &x) const;

    std::optional<ArgManifest> arg_manifest() const override
    {
        return ArgManifest{ArgSpec{.name = "x"}};
    }

    std::string name() const { return "sin"; }
};

struct CosOp : public Operation
{
    BIND_ARGS(&CosOp::run);
    ValuePtr run(const Value &x) const;

    std::optional<ArgManifest> arg_manifest() const override
    {
        return ArgManifest{ArgSpec{.name = "x"}};
    }

    std::string name() const { return "cos"; }
};

struct AsinhOp : public Operation
{
    BIND_ARGS(&AsinhOp::run);
    ValuePtr run(const Value &x) const;

    std::optional<ArgManifest> arg_manifest() const override
    {
        return ArgManifest{ArgSpec{.name = "x"}};
    }

    std::string name() const { return "asinh"; }
};

} // namespace aquila::ops