#pragma once
#include "value.hpp"
#include <iostream>
#include <memory>

namespace aquila::interpreter
{

template <typename BaseValT, typename... PossibleValT>
struct __converter;

template <typename BaseValT, typename PossibleValT, typename... TTT>
struct __converter<BaseValT, PossibleValT, TTT...>
{
    __converter<BaseValT, TTT...> next;
    std::unique_ptr<BaseValT> holder;
    using ConvFn = std::unique_ptr<BaseValT> (*)(const PossibleValT &);
    ConvFn conv_;

    template <typename... TFFF>
    __converter(ConvFn convF, TFFF... otherF) : next(otherF...), conv_(convF)
    {
    }

    const BaseValT *conv(const Value *in)
    {
        const PossibleValT *conv_in = dynamic_cast<const PossibleValT *>(in);
        if (!conv_in)
            return next.conv(in);
        if (holder)
            throw std::logic_error("reused converted");
        holder = conv_(*conv_in);
        if (!holder)
            return next.conv(in);
        return holder.get();
    }
};

template <typename BaseValT>
struct __converter<BaseValT>
{
    __converter() {}
    const BaseValT *conv(const Value *in)
    {
        throw std::runtime_error("conversion failed");
    }
};

struct __checker_converter_base
{
    virtual const Value *conv(const Value *in) = 0;
    virtual ~__checker_converter_base() = default;
};

template <typename BaseValT, typename... PossibleValT>
struct __checker_converter : __checker_converter_base
{
    __converter<BaseValT, PossibleValT...> converter;

    template <typename... TFuncs>
    __checker_converter(TFuncs... funcs) : converter(funcs...)
    {
    }

    const BaseValT *conv(const Value *in) override
    {
        const auto *ptr = dynamic_cast<const BaseValT *>(in);
        if (ptr)
            return ptr;

        return converter.conv(in);
    }
};

using Sanitizer = std::unique_ptr<__checker_converter_base>;
using SanitizerFactory = std::function<Sanitizer()>;

} // namespace aquila::interpreter

namespace aquila
{

template <typename BaseValT, typename... PossibleValT, typename... FuncsT>
interpreter::SanitizerFactory guard(FuncsT... funcs)
{
    return [funcs...]() -> std::unique_ptr<interpreter::__checker_converter_base>
    {
        return std::make_unique<interpreter::__checker_converter<BaseValT, PossibleValT...>>(
            funcs...);
    };
}

} // namespace aquila