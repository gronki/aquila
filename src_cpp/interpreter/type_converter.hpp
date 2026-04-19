#pragma once
#include "value.hpp"
#include <iostream>
#include <memory>

namespace aquila::interpreter
{

using ConvertFun = std::function<std::unique_ptr<Value>(const Value &)>;

template <typename PossibleValT>
void __maybe_convert(const Value &v,
    std::unique_ptr<Value> (*conv_fun)(const PossibleValT &),
    std::unique_ptr<Value> &dst)
{
    // if dst is allocated, means that one of the previous
    // conversions succeded -- early exit
    if (dst)
        return;
    // wet try casting to the input type.
    const auto *typed_ptr = value_cast<PossibleValT>(&v);
    if (!typed_ptr)
        return;
    // if cast successful, run the converter.
    dst = conv_fun(*typed_ptr);
}

template <typename... ConvFuncs>
ConvertFun guard(ConvFuncs... conv_funcs)
{
    return [conv_funcs...](const Value &v) -> std::unique_ptr<Value>
    {
        std::unique_ptr<Value> dst;
        (__maybe_convert(v, conv_funcs, dst), ...);
        return dst;
    };
}

} // namespace aquila::interpreter
