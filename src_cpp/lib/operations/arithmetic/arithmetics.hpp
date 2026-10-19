#pragma once

#include <bind_new.hpp>
#include <interpreter.hpp>
#include <values/frame.hpp>

namespace aquila::ops
{

struct AddOp : public Operation
{
    BIND_ARGS_NEW(&AddOp::run);
    Ptr<Value> run(Ellipsis<const Value *> args) const;
    const ArgManifest &arg_manifest() const override;
    std::string name() const override { return "add"; }
    std::string description() const override
    {
        return "Adds all given scalars/buffers.";
    }
};

struct SubOp : public Operation
{
    BIND_ARGS_NEW(&SubOp::run);
    Ptr<Value> run(Ptr<Value> first, Ellipsis<const Value *> args) const;

    const ArgManifest &arg_manifest() const override;
    std::string name() const override { return "sub"; }
    std::string description() const override
    {
        return "Subtract frames. sub(A,B,C...) computes A - B - C - ...";
    }
};

struct MulOp : public Operation
{
    BIND_ARGS_NEW(&MulOp::run);
    Ptr<Value> run(Ellipsis<const Value *> args) const;
    const ArgManifest &arg_manifest() const override;
    std::string name() const override { return "mul"; }
    std::string description() const override
    {
        return "Multiplies all given scalars/buffers.";
    }
};

struct DivOp : public Operation
{
    BIND_ARGS_NEW(&DivOp::run);
    Ptr<Value> run(Ptr<Value> first, Ellipsis<const Value *> args) const;

    const ArgManifest &arg_manifest() const override;
    std::string name() const override { return "div"; }
    std::string description() const override
    {
        return "Divide frames. div(A,B,C...) computes A / (B * C * ...)";
    }
};

struct MaxOp : public Operation
{
    BIND_ARGS_NEW(&MaxOp::run);
    Ptr<Value> run(Ptr<Value> first, Ellipsis<const Value *>) const;
    const ArgManifest &arg_manifest() const override;
    std::string name() const override { return "max"; }
    std::string description() const override { return "Maximum value pixel-wise"; }
};

struct MinOp : public Operation
{
    BIND_ARGS_NEW(&MinOp::run);
    Ptr<Value> run(Ptr<Value> first, Ellipsis<const Value *>) const;
    const ArgManifest &arg_manifest() const override;
    std::string name() const override { return "min"; }
    std::string description() const override { return "Minimum value pixel-wise"; }
};

struct SoftmaxOp : public Operation
{
    BIND_ARGS_NEW(&SoftmaxOp::run);
    Ptr<Value> run(Ptr<Value> first, const Real &alpha, Ellipsis<const Value *>) const;
    const ArgManifest &arg_manifest() const override;
    std::string name() const override { return "softmax"; }
    std::string description() const override
    {
        return "Smooth maximum pixel-wise: log(sum(exp(alpha*x)))/alpha. "
               "Approaches max() for large alpha.";
    }
};

struct MixOp : public Operation
{
    BIND_ARGS_NEW(&MixOp::run);
    Ptr<Value> run(Ellipsis<const Value *> args) const;
    const ArgManifest &arg_manifest() const override;
    std::string name() const override { return "mix"; }
    std::string description() const override
    {
        return "mix(a,b,c,d,.) will compute a*b + c*d + ...";
    }
};

struct LrgbOp : public Operation
{
    BIND_ARGS_NEW(&LrgbOp::run);
    Ptr<Value> run(Ptr<Value> lum, Ellipsis<const Value *> args) const;

    const ArgManifest &arg_manifest() const override;
    std::string name() const override { return "lrgb"; }
    std::string description() const override
    {
        return "lrgb(A, B, C, ...) with N parameters"
               " will return N-1 items: [A * B / (B+C+...), A * C / (B+C+...), ...].";
    }
};

struct PowOp : public Operation
{
    BIND_ARGS_NEW(&PowOp::run);
    ValuePtr run(const Value &a, const Value &b) const;

    const ArgManifest &arg_manifest() const override;
    std::string name() const override { return "pow"; }
    std::string description() const override { return "Computes base^exponent."; }
};

struct SqrtOp : public Operation
{
    BIND_ARGS_NEW(&SqrtOp::run);
    ValuePtr run(const Value &x) const;

    const ArgManifest &arg_manifest() const override;
    std::string name() const override { return "sqrt"; }
    std::string description() const override
    {
        return "Square root of a scalar/buffer.";
    }
};

struct ContrastOp : public Operation
{
    BIND_ARGS_NEW(&ContrastOp::run);
    ValuePtr run(const Value &x, const Real &strength) const;

    const ArgManifest &arg_manifest() const override;
    std::string name() const override { return "contrast"; }
    std::string description() const override
    {
        return "Contrast curve on [0,1]: (1-strength)*x + strength*(1-cos(pi*x))/2. "
               "Values below 0 become 0, above 1 become 1.";
    }
};

struct StretchOp : public Operation
{
    BIND_ARGS_NEW(&StretchOp::run);
    ValuePtr run(const Value &x, const Real &lo, const Real &hi) const;

    const ArgManifest &arg_manifest() const override;
    std::string name() const override { return "stretch"; }
    std::string description() const override
    {
        return "Linear levels: (x - lo) / (hi - lo), so lo maps to 0 and hi to 1. No clipping.";
    }
};

} // namespace aquila::ops
