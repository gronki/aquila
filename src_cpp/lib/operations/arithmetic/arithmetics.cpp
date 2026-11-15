#include <cmath>
#include <numbers>

#include "arithmetic_templates.hpp"
#include "arithmetics.hpp"

namespace aquila::ops
{

REGISTER(AddOp);
Ptr<Value> AddOp::run(Ellipsis<const Value *> args) const
{
    std::unique_ptr<Value> result = std::make_unique<RealValue>(0);
    for (const auto &arg : args.data)
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
Ptr<Value> SubOp::run(Ptr<Value> first, Ellipsis<const Value *> args) const
{
    auto result = first;
    for (std::size_t iarg = 0; iarg < args.data.size(); iarg++)
    {
        result = apply_binary(
            *result, *args.data[iarg], [](auto a, auto b) { return a - b; });
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
Ptr<Value> MulOp::run(Ellipsis<const Value *> args) const
{
    std::unique_ptr<Value> result = std::make_unique<RealValue>(1);
    for (const auto &arg : args.data)
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
Ptr<Value> DivOp::run(Ptr<Value> first, Ellipsis<const Value *> args) const
{
    if (args.data.size() == 0)
        return first;
    auto result = first;
    for (std::size_t iarg = 0; iarg < args.data.size(); iarg++)
    {
        result = apply_binary(
            *result, *args.data[iarg], [](auto a, auto b) { return a / b; });
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
Ptr<Value> MaxOp::run(Ptr<Value> first, Ellipsis<const Value *> ellip) const
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
Ptr<Value> MinOp::run(Ptr<Value> first, Ellipsis<const Value *> ellip) const
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
    Ptr<Value> first, const Real &alpha, Ellipsis<const Value *> ellip) const
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

    return apply_binary(*peak, *sum, [a](auto m, auto s) { return m + std::log(s) / a; });
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
Ptr<Value> MixOp::run(Ellipsis<const Value *> args) const
{
    if (args.data.size() % 2)
        throw std::runtime_error(
            std::string("Mix requires even number of arguments but got: ")
            + std::to_string(args.data.size()));
    std::unique_ptr<Value> result = std::make_unique<RealValue>(0);
    for (std::size_t iarg = 0; iarg < args.data.size(); iarg += 2)
    {
        result = apply_tertiary(*result,
            *args.data[iarg],
            *args.data[iarg + 1],
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
Ptr<Value> LrgbOp::run(Ptr<Value> lum, Ellipsis<const Value *> args) const
{
    if (args.data.size() == 0)
        return lum;
    std::unique_ptr<Value> chroma_sum = std::make_unique<RealValue>(0);
    for (std::size_t iarg = 0; iarg < args.data.size(); iarg++)
    {
        chroma_sum = apply_binary(
            *chroma_sum, *args.data[iarg], [](auto a, auto b) { return a + b; });
    }
    std::vector<ValuePtr> scaled_components;
    scaled_components.reserve(args.data.size() - 1);
    for (std::size_t iarg = 0; iarg < args.data.size(); iarg++)
    {
        scaled_components.push_back(apply_tertiary(*lum,
            *args.data[iarg],
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

// |t|^exponent with the sign of t, which keeps contrast_curve monotonic outside [0,1].
template <typename T>
static T signed_power(T t, T exponent)
{
    return std::copysign(std::pow(std::abs(t), exponent), t);
}

/**
 * Contrast curve fixing 0, pivot and 1: two power-law branches meeting at the pivot,
 * where the slope is `gain` on both sides. Pivot 0 or 1 leaves one branch, a
 * gamma-like curve. Not clipped, monotonic outside [0,1].
 *
 * Args: x (input level), gain (slope at the pivot), pivot (in [0,1]).
 * Returns: the curve value at x.
 */
template <typename T>
static T contrast_curve(T x, T gain, T pivot)
{
    const bool use_lower_branch = (x < pivot && pivot > 0) || pivot >= 1;
    if (use_lower_branch)
        return pivot * signed_power(x / pivot, gain);
    return 1 - (1 - pivot) * signed_power((1 - x) / (1 - pivot), gain);
}

REGISTER(ContrastOp);
ValuePtr ContrastOp::run(
    const Value &x, const Real &factor, const Real &pivot, const Real &mix) const
{
    const real_buf_t gain = std::pow(std::numbers::pi_v<real_buf_t> / 2, factor);
    const real_buf_t p = pivot;
    const real_buf_t m = mix;

    return apply_unitary(x,
        [gain, p, m](auto input) -> decltype(input)
        {
            const auto curved = contrast_curve<decltype(input)>(input, gain, p);
            return m * curved + (1 - m) * input;
        });
}

const ArgManifest &ContrastOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "x"},
        ArgSpec{.name = "factor",
            .default_real = 0.5,
            .help = "0 is identity; the slope at the pivot is (pi/2)^factor"},
        ArgSpec{.name = "pivot", .default_real = 0.5, .help = "in [0,1]: the level left unchanged, where the curve is steepest; 0 and 1 give gamma-like curves"},
        ArgSpec{.name = "mix", .default_real = 1.0, .help = "fraction of the curve in the result: 0 is the input, 1 is the full curve"},
    };
    return manifest;
}

REGISTER(SaturateOp);
ValuePtr SaturateOp::run(
    Ptr<SequenceValue> channels, const Real &factor, const Real &pivot, const Real &mix) const
{
    if (channels->items.empty())
        throw std::runtime_error("saturate: empty sequence");

    ValuePtr luminance = std::make_unique<RealValue>(0);
    std::vector<ValuePtr> curved_channels;
    Ellipsis<const Value *> curved_pointers;

    for (const auto &channel : channels->items)
    {
        luminance =
            apply_binary(*luminance, *channel, [](auto a, auto b) { return a + b; });
        curved_channels.push_back(ContrastOp().run(*channel, factor, pivot, mix));
        curved_pointers.data.push_back(curved_channels.back().get());
    }

    return LrgbOp().run(luminance, curved_pointers);
}

const ArgManifest &SaturateOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "channels", .sequence = true},
        ArgSpec{.name = "factor", .default_real = 0.5, .help = "contrast factor, see contrast"},
        ArgSpec{.name = "pivot", .default_real = 0.5, .help = "contrast pivot, see contrast"},
        ArgSpec{.name = "mix",
            .default_real = 1.0,
            .help = "0 leaves the colours as they are, 1 is full effect"},
    };
    return manifest;
}

REGISTER(StretchOp);
ValuePtr StretchOp::run(const Value &x, const Real &lo, const Real &hi) const
{
    const real_buf_t l = lo;
    const real_buf_t h = hi;
    return apply_unitary(x, [l, h](auto a) { return (a - l) / (h - l); });
}

const ArgManifest &StretchOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "x"},
        ArgSpec{.name = "lo", .default_real = 0.0, .help = "value mapped to 0"},
        ArgSpec{.name = "hi", .default_real = 1.0, .help = "value mapped to 1"},
    };
    return manifest;
}

} // namespace aquila::ops
