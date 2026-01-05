#pragma once

#include "../../interpreter/interpreter.hpp"
#include "../../values/frame.hpp"

namespace aquila::ops
{

struct AddOp : public Operation
{
    std::unique_ptr<Value> call(const std::vector<const Value *> &) const;
    std::string name() const { return "add"; }
};

struct MulOp : public Operation
{
    std::unique_ptr<Value> call(const std::vector<const Value *> &) const;
    std::string name() const { return "mul"; }
};

struct MixOp : public Operation
{
    std::unique_ptr<Value> call(const std::vector<const Value *> &) const;
    std::string name() const { return "mix"; }
};

struct LrgbOp : public Operation
{
    std::unique_ptr<Value> call(const std::vector<const Value *> &) const;
    std::string name() const { return "lrgb"; }
};

struct PowOp : public Operation
{
    BIND_ARGS(&PowOp::run);
    ValuePtr run(const Value &a, const Value &b) const;

    std::optional<ArgManifest> arg_manifest() const override
    {
        return ArgManifest{
            ArgSpec{.name = "base"},
            ArgSpec{.name = "exponent"},
        };
    }

    std::string name() const { return "pow"; }
};

struct SqrtOp : public Operation
{
    BIND_ARGS(&SqrtOp::run);
    ValuePtr run(const Value &x) const;

    std::optional<ArgManifest> arg_manifest() const override
    {
        return ArgManifest{ArgSpec{.name = "x"}};
    }

    std::string name() const { return "sqrt"; }
};

} // namespace aquila::ops