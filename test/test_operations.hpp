#pragma once

#include <cmath>

#include "../src_cpp/lib/interpreter/operation.hpp"
#include "../src_cpp/lib/interpreter/value.hpp"

namespace aquila::interpreter::ops
{

struct AddOp : Operation
{
    BIND_ARGS(&AddOp::run);

    std::unique_ptr<Value> run(const double &a, const double &b) const
    {
        return std::make_unique<RealValue>(a + b);
    }

    std::optional<ArgManifest> arg_manifest() const override
    {
        return ArgManifest{
            {.name = "a", .help = "first number to add"},
            {.name = "b", .help = "second number to add"},
        };
    }

    std::string name() const override { return "add"; }
};

REGISTER(AddOp);

struct MulOp : Operation
{
    BIND_ARGS(&MulOp::run);

    std::unique_ptr<Value> run(const double &a, const double &b) const
    {
        return std::make_unique<RealValue>(a * b);
    }

    std::optional<ArgManifest> arg_manifest() const override
    {
        return ArgManifest{
            {.name = "a", .help = "first number to multiply"},
            {.name = "b", .help = "second number to multiply"},
        };
    }
    std::string name() const override { return "mul"; }
};

REGISTER(MulOp);

struct PowOp : Operation
{
    BIND_ARGS(&PowOp::run);

    std::unique_ptr<Value> run(const double &a, const double &p) const
    {
        return std::make_unique<RealValue>(std::pow(a, p));
    }

    std::optional<ArgManifest> arg_manifest() const override
    {
        return ArgManifest{
            {.name = "a", .help = "number"},
            {.name = "p", .help = "exponent"},
        };
    }

    std::string name() const override { return "pow"; }
};

REGISTER(PowOp);

} // namespace aquila::interpreter::ops