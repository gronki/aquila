#include <cmath>

#include "arithmetic_templates.hpp"
#include "arithmetics.hpp"

namespace aquila::ops
{

REGISTER(AddOp);
Ptr<Value> AddOp::run(std::vector<const Value *> args) const
{
    std::unique_ptr<Value> result = std::make_unique<RealValue>(0);
    for (const Value *arg : args)
    {
        result = apply_binary(*result, *arg, [](auto a, auto b) { return a + b; });
    }
    return result;
}

const ArgManifest &AddOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "..."},
    };
    return manifest;
}

REGISTER(SubOp);
Ptr<Value> SubOp::run(Ptr<Value> first, std::vector<const Value *> args) const
{
    auto result = first;
    for (std::size_t iarg = 0; iarg < args.size(); iarg++)
    {
        result = apply_binary(*result, *args[iarg], [](auto a, auto b) { return a - b; });
    }
    return result;
}

const ArgManifest &SubOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "x"},
        ArgSpec{.name = "..."},
    };
    return manifest;
}

REGISTER(MulOp);
Ptr<Value> MulOp::run(std::vector<const Value *> args) const
{
    std::unique_ptr<Value> result = std::make_unique<RealValue>(1);
    for (const Value *arg : args)
    {
        result = apply_binary(*result, *arg, [](auto a, auto b) { return a * b; });
    }
    return result;
}

const ArgManifest &MulOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "..."},
    };
    return manifest;
}

REGISTER(DivOp);
Ptr<Value> DivOp::run(Ptr<Value> first, std::vector<const Value *> args) const
{
    if (args.size() == 0)
        return first;
    auto result = first;
    for (std::size_t iarg = 0; iarg < args.size(); iarg++)
    {
        result = apply_binary(*result, *args[iarg], [](auto a, auto b) { return a / b; });
    }
    return result;
}

const ArgManifest &DivOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "x"},
        ArgSpec{.name = "..."},
    };
    return manifest;
}

REGISTER(MaxOp);
Ptr<Value> MaxOp::run(Ptr<Value> first, Ellipsis<Ptr<Value>> ellip) const
{
    if (ellip.data.size() == 0)
        return first;
    Ptr<Value> result = first;
    for (size_t i = 0; i < ellip.data.size(); i++)
    {
        result = apply_binary(
            *result, *ellip.data[i], [](auto a, auto b) { return std::max(a, b); });
    }
    return result;
}

const ArgManifest &MaxOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "first"},
        ArgSpec{.name = "..."},
    };
    return manifest;
}

REGISTER(MinOp);
Ptr<Value> MinOp::run(Ptr<Value> first, Ellipsis<Ptr<Value>> ellip) const
{
    if (ellip.data.size() == 0)
        return first;
    Ptr<Value> result = first;
    for (size_t i = 0; i < ellip.data.size(); i++)
    {
        result = apply_binary(
            *result, *ellip.data[i], [](auto a, auto b) { return std::min(a, b); });
    }
    return result;
}

const ArgManifest &MinOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "first"},
        ArgSpec{.name = "..."},
    };
    return manifest;
}

REGISTER(SoftmaxOp);
Ptr<Value> SoftmaxOp::run(
    Ptr<Value> first, const Real &alpha, Ellipsis<Ptr<Value>> ellip) const
{
    if (ellip.data.size() == 0)
        return first;
    if (alpha == 0)
        throw std::runtime_error("softmax: alpha must not be zero");

    // the pixel-wise maximum is subtracted before exponentiating, otherwise
    // exp() overflows the single precision buffers already for alpha * x ~ 89
    Ptr<Value> peak = first;
    for (size_t i = 0; i < ellip.data.size(); i++)
    {
        peak = apply_binary(
            *peak, *ellip.data[i], [](auto a, auto b) { return std::max(a, b); });
    }

    const real_buf_t a = alpha;
    auto weight = [a](auto x, auto m) { return std::exp(a * (x - m)); };

    Ptr<Value> sum = apply_binary(*first, *peak, weight);
    for (size_t i = 0; i < ellip.data.size(); i++)
    {
        auto term = apply_binary(*ellip.data[i], *peak, weight);
        sum = apply_binary(*sum, *term, [](auto p, auto q) { return p + q; });
    }

    return apply_binary(
        *peak, *sum, [a](auto m, auto s) { return m + std::log(s) / a; });
}

const ArgManifest &SoftmaxOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "first"},
        ArgSpec{.name = "alpha", .default_real = 1.0, .help = "sharpness of the maximum"},
        ArgSpec{.name = "..."},
    };
    return manifest;
}

REGISTER(MixOp);
Ptr<Value> MixOp::run(std::vector<const Value *> args) const
{
    if (args.size() % 2)
        throw std::runtime_error(
            std::string("Mix requires even number of arguments but got: ")
            + std::to_string(args.size()));
    std::unique_ptr<Value> result = std::make_unique<RealValue>(0);
    for (std::size_t iarg = 0; iarg < args.size(); iarg += 2)
    {
        result = apply_tertiary(*result,
            *args[iarg],
            *args[iarg + 1],
            [](auto a, auto b, auto c) { return a + b * c; });
    }
    return result;
}

const ArgManifest &MixOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "..."},
    };
    return manifest;
}

REGISTER(LrgbOp);
Ptr<Value> LrgbOp::run(Ptr<Value> lum, std::vector<const Value *> args) const
{
    if (args.size() == 0)
        return lum;
    std::unique_ptr<Value> chroma_sum = std::make_unique<RealValue>(0);
    for (std::size_t iarg = 0; iarg < args.size(); iarg++)
    {
        chroma_sum =
            apply_binary(*chroma_sum, *args[iarg], [](auto a, auto b) { return a + b; });
    }
    std::vector<ValuePtr> scaled_components;
    scaled_components.reserve(args.size() - 1);
    for (std::size_t iarg = 0; iarg < args.size(); iarg++)
    {
        scaled_components.push_back(apply_tertiary(*lum,
            *args[iarg],
            *chroma_sum,
            [](auto l, auto c, auto s) { return l * c / s; }));
    }
    return std::make_unique<interpreter::SequenceValue>(std::move(scaled_components));
}

const ArgManifest &LrgbOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "L", .help = "luminance"},
        ArgSpec{.name = "...", .help = "color channels"},
    };
    return manifest;
}

REGISTER(PowOp);
ValuePtr PowOp::run(const Value &a, const Value &b) const
{
    return apply_binary(a, b, [](auto a, auto b) { return std::pow(a, b); });
}

const ArgManifest &PowOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "base"},
        ArgSpec{.name = "exponent"},
    };
    return manifest;
}

REGISTER(SqrtOp);
ValuePtr SqrtOp::run(const Value &x) const
{
    return apply_unitary(x, [](auto a) { return std::sqrt(a); });
}

const ArgManifest &SqrtOp::arg_manifest() const
{
    static const ArgManifest manifest{ArgSpec{.name = "x"}};
    return manifest;
}

} // namespace aquila::ops
