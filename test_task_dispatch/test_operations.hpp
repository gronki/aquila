#pragma once

#include <cmath>

#include "../src_cpp/interpreter/bind_new.hpp"
#include "../src_cpp/interpreter/operation.hpp"
#include "../src_cpp/interpreter/value.hpp"

namespace aquila::interpreter::ops
{

struct AddOp : Operation
{
    BIND_ARGS_NEW(&AddOp::run);

    ValuePtr run(const double &a, const double &b) const
    {
        return std::make_unique<RealValue>(a + b);
    }

    const ArgManifest &arg_manifest() const override
    {
        static const ArgManifest manifest{
            {.name = "a", .help = "first number to add"},
            {.name = "b", .help = "second number to add"},
        };
        return manifest;
    }

    std::string name() const override { return "add"; }
};

REGISTER(AddOp);

struct MulOp : Operation
{
    BIND_ARGS_NEW(&MulOp::run);

    ValuePtr run(const double &a, const double &b) const
    {
        return std::make_unique<RealValue>(a * b);
    }

    const ArgManifest &arg_manifest() const override
    {
        static const ArgManifest manifest{
            {.name = "a", .help = "first number to multiply"},
            {.name = "b", .help = "second number to multiply"},
        };
        return manifest;
    }
    std::string name() const override { return "mul"; }
};

REGISTER(MulOp);

struct PowOp : Operation
{
    BIND_ARGS_NEW(&PowOp::run);

    ValuePtr run(const double &a, const double &p) const
    {
        return std::make_unique<RealValue>(std::pow(a, p));
    }

    const ArgManifest &arg_manifest() const override
    {
        static const ArgManifest manifest{
            {.name = "a", .help = "number"},
            {.name = "p", .help = "exponent"},
        };
        return manifest;
    }

    std::string name() const override { return "pow"; }
};

REGISTER(PowOp);

} // namespace aquila::interpreter::ops