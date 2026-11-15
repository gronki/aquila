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
    ValuePtr run(
        const Value &x, const Real &factor, const Real &pivot, const Real &mix) const;

    const ArgManifest &arg_manifest() const override;
    std::string name() const override { return "contrast"; }
    std::string description() const override
    {
        return "Contrast curve fixing 0, pivot and 1, with slope (pi/2)^factor at the pivot: "
               "two power-law branches, x^g below the pivot and 1-(1-x)^g above it, "
               "rescaled to meet there. Pivot 0 or 1 gives a gamma-like curve. "
               "Monotonic and unclipped outside [0,1]. "
               "The result is mixed with the input: mix*result + (1-mix)*x.";
    }
};

struct SaturateOp : public Operation
{
    BIND_ARGS_NEW(&SaturateOp::run);
    ValuePtr run(Ptr<SequenceValue> channels,
        const Real &factor,
        const Real &pivot,
        const Real &mix) const;

    const ArgManifest &arg_manifest() const override;
    std::string name() const override { return "saturate"; }
    std::string description() const override
    {
        return "Changes colour saturation of a sequence of channels, keeping their sum L. "
               "Each channel goes through the contrast curve, then lrgb(L, curved channels) "
               "restores the sum.";
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
