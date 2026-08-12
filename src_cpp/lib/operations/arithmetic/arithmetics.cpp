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

ArgManifest AddOp::arg_manifest() const
{
    return ArgManifest{
        ArgSpec{.name = "..."},
    };
}

REGISTER(SubOp);
Ptr<Value> SubOp::run(Ptr<Value> first, std::vector<const Value *> args) const
{
    auto result = first.own();
    for (std::size_t iarg = 0; iarg < args.size(); iarg++)
    {
        result = apply_binary(*result, *args[iarg], [](auto a, auto b) { return a - b; });
    }
    return result;
}

ArgManifest SubOp::arg_manifest() const
{
    return ArgManifest{
        ArgSpec{.name = "x"},
        ArgSpec{.name = "..."},
    };
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

ArgManifest MulOp::arg_manifest() const
{
    return ArgManifest{
        ArgSpec{.name = "..."},
    };
}

REGISTER(DivOp);
Ptr<Value> DivOp::run(Ptr<Value> first, std::vector<const Value *> args) const
{
    if (args.size() == 0)
        return first;
    auto result = first.own();
    for (std::size_t iarg = 0; iarg < args.size(); iarg++)
    {
        result = apply_binary(*result, *args[iarg], [](auto a, auto b) { return a / b; });
    }
    return result;
}

ArgManifest DivOp::arg_manifest() const
{
    return ArgManifest{
        ArgSpec{.name = "x"},
        ArgSpec{.name = "..."},
    };
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

ArgManifest MixOp::arg_manifest() const
{
    return ArgManifest{
        ArgSpec{.name = "..."},
    };
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

ArgManifest LrgbOp::arg_manifest() const
{
    return ArgManifest{
        ArgSpec{.name = "L", .help = "luminance"},
        ArgSpec{
            .name = "...", .help = "color channels"},
    };
}

REGISTER(PowOp);
ValuePtr PowOp::run(const Value &a, const Value &b) const
{
    return apply_binary(a, b, [](auto a, auto b) { return std::pow(a, b); });
}

ArgManifest PowOp::arg_manifest() const
{
    return ArgManifest{
        ArgSpec{.name = "base"},
        ArgSpec{.name = "exponent"},
    };
}

REGISTER(SqrtOp);
ValuePtr SqrtOp::run(const Value &x) const
{
    return apply_unitary(x, [](auto a) { return std::sqrt(a); });
}

ArgManifest SqrtOp::arg_manifest() const
{
    return ArgManifest{ArgSpec{.name = "x"}};
}

} // namespace aquila::ops
