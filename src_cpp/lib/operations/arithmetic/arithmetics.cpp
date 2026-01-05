#include <cmath>

#include "arithmetic_templates.hpp"
#include "arithmetics.hpp"

namespace aquila::ops
{

REGISTER(AddOp);
std::unique_ptr<Value> AddOp::call(const std::vector<const Value *> &args) const
{
    ValuePtr result = std::make_unique<RealValue>(0);
    for (const Value *arg : args)
    {
        result = apply_binary(*result, *arg, [](Real a, Real b) -> Real { return a + b; });
    }
    return result;
}

REGISTER(MulOp);
std::unique_ptr<Value> MulOp::call(const std::vector<const Value *> &args) const
{
    ValuePtr result = std::make_unique<RealValue>(1);
    for (const Value *arg : args)
    {
        result = apply_binary(*result, *arg, [](Real a, Real b) -> Real { return a * b; });
    }
    return result;
}

REGISTER(MixOp);
std::unique_ptr<Value> MixOp::call(const std::vector<const Value *> &args) const
{
    if (args.size() % 2)
        throw std::runtime_error(
            std::string("Mix requires even number of arguments but got: ")
            + std::to_string(args.size()));
    ValuePtr result = std::make_unique<RealValue>(0);
    for (Int iarg = 0; iarg < args.size(); iarg += 2)
    {
        result = apply_tertiary(*result,
            *args[iarg],
            *args[iarg + 1],
            [](Real a, Real b, Real c) -> Real { return a + b * c; });
    }
    return result;
}

REGISTER(LrgbOp);
std::unique_ptr<Value> LrgbOp::call(const std::vector<const Value *> &args) const
{
    if (args.size() == 0)
        throw std::runtime_error(
            "LRGB requires at least 1 argument (but pointless below 3) ");
    if (args.size() == 1)
        return std::make_unique<interpreter::SequenceValue>();
    ValuePtr chroma_sum = std::make_unique<RealValue>(0);
    for (Int iarg = 1; iarg < args.size(); iarg++)
    {
        chroma_sum = apply_binary(
            *chroma_sum, *args[iarg], [](Real a, Real b) -> Real { return a + b; });
    }
    std::vector<ValuePtr> scaled_components;
    scaled_components.reserve(args.size() - 1);
    for (Int iarg = 1; iarg < args.size(); iarg++)
    {
        scaled_components.push_back(apply_tertiary(*args[0],
            *args[iarg],
            *chroma_sum,
            [](Real l, Real c, Real s) -> Real { return l * c / s; }));
    }
    return std::make_unique<interpreter::SequenceValue>(std::move(scaled_components));
}

REGISTER(PowOp);
ValuePtr PowOp::run(const Value &a, const Value &b) const
{
    return apply_binary(a, b, [](Real a, Real b) -> Real { return std::pow(a, b); });
}

REGISTER(SqrtOp);
ValuePtr SqrtOp::run(const Value &x) const
{
    return apply_unitary(x, [](Real a) -> Real { return std::sqrt(a); });
}

} // namespace aquila::ops