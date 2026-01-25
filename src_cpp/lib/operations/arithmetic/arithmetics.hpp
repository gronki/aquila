#pragma once

#include "../../../interpreter/interpreter.hpp"
#include "../../values/frame.hpp"

namespace aquila::ops
{

struct AddOp : public Operation
{
    std::unique_ptr<Value> call(const std::vector<const Value *> &) const;
    std::string name() const { return "add"; }
    std::string description() const override
    {
        return "Adds all given scalars/buffers.";
    }
};

struct SubOp : public Operation
{
    std::unique_ptr<Value> call(const std::vector<const Value *> &) const;

    ArgManifest arg_manifest() const override;
    std::string name() const { return "sub"; }
    std::string description() const override
    {
        return "Subtract frames. sub(A,B,C...) computes A - B - C - ...";
    }
};

struct MulOp : public Operation
{
    std::unique_ptr<Value> call(const std::vector<const Value *> &) const;
    std::string name() const { return "mul"; }
    std::string description() const override
    {
        return "Multiplies all given scalars/buffers.";
    }
};

struct MixOp : public Operation
{
    std::unique_ptr<Value> call(const std::vector<const Value *> &) const;
    std::string name() const { return "mix"; }
    std::string description() const override
    {
        return "mix(a,b,c,d,...) will compute a*b + c*d + ...";
    }
};

struct LrgbOp : public Operation
{
    std::unique_ptr<Value> call(const std::vector<const Value *> &) const;
    
    ArgManifest arg_manifest() const override;
    std::string name() const { return "lrgb"; }
    std::string description() const override
    {
        return "lrgb(A, B, C, ...) with N parameters"
               " will return N-1 items: [A * B / (B+C+...), A * C / (B+C+...), ...].";
    }
};

struct PowOp : public Operation
{
    BIND_ARGS(&PowOp::run);
    ValuePtr run(const Value &a, const Value &b) const;

    ArgManifest arg_manifest() const override;
    std::string name() const { return "pow"; }
    std::string description() const override { return "Computes base^exponent."; }
};

struct SqrtOp : public Operation
{
    BIND_ARGS(&SqrtOp::run);
    ValuePtr run(const Value &x) const;

    ArgManifest arg_manifest() const override;
    std::string name() const { return "sqrt"; }
    std::string description() const override { return "Square root of a scalar/buffer."; }
};

} // namespace aquila::ops