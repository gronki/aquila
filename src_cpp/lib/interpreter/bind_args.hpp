#pragma once

#include <memory>
#include <vector>

#include "value.hpp"

namespace aquila::interpreter
{

template <typename... ArgsT>
struct __args_binder;

template <>
struct __args_binder<>
{
    template <typename OpT, typename... ArgsT, typename... CallArgsT>
    static void run(OpT *obj,
                    std::unique_ptr<Value> (OpT::*exec_fun)(const ArgsT &...),
                    const std::vector<const Value *> &args,
                    std::unique_ptr<Value> &result,
                    std::size_t idx,
                    const CallArgsT &...callargs)
    {
        result = (obj->*exec_fun)(callargs...);
    }
};

template <typename T>
const T *cast_value(const Value *v)
{
    return dynamic_cast<const T *>(v);
}

template <typename T>
const T *cast_simple_value(const Value *v)
{
    const SimpleValue<T> *sv = dynamic_cast<const SimpleValue<T> *>(v);
    if (!sv)
        return nullptr;
    return &sv->value;
}

template <>
const Real *cast_value<Real>(const Value *v)
{
    return cast_simple_value<Real>(v);
}

template <>
const Int *cast_value<Int>(const Value *v)
{
    return cast_simple_value<Int>(v);
}

template <>
const std::string *cast_value<std::string>(const Value *v)
{
    return cast_simple_value<std::string>(v);
}

template <typename T, typename... TT>
struct __args_binder<T, TT...>
{
    using next = __args_binder<TT...>;
    template <typename OpT, typename... ArgsT, typename... CallArgsT>
    static void run(OpT *obj,
                    std::unique_ptr<Value> (OpT::*exec_fun)(const ArgsT &...),
                    const std::vector<const Value *> &args,
                    std::unique_ptr<Value> &result,
                    std::size_t idx,
                    const CallArgsT &...callargs)
    {
        const T *tptr = cast_value<T>(args[idx]);
        if (!tptr)
            throw std::runtime_error(std::string("Error trying to interpret "
                                                 "argument ") +
                                     std::to_string(idx + 1));
        next::run(obj, exec_fun, args, result, idx + 1, callargs..., *tptr);
    }
};

template <typename OpT, typename... ArgsT>
std::unique_ptr<Value> bind_args(OpT *obj,
                                 std::unique_ptr<Value> (OpT::*exec_fun)(const ArgsT &...),
                                 const std::vector<const Value *> &args)
{
    if (sizeof...(ArgsT) != args.size())
    {
        throw std::runtime_error(std::string("Argument list length incorrect: "
                                             "expected ") +
                                 std::to_string(sizeof...(ArgsT)) +
                                 " arguments but got " + std::to_string(args.size()));
    }

    __args_binder<ArgsT...> binder;
    std::unique_ptr<Value> result = nullptr;
    binder.run(obj, exec_fun, args, result, 0);
    return result;
}

} // namespace aquila::interpreter