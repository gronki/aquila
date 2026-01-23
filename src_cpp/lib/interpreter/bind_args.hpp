#pragma once

#include <cstdint>
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
    static void run(const OpT *obj,
        std::unique_ptr<Value> (OpT::*exec_fun)(const ArgsT &...) const,
        const std::vector<const Value *> &args,
        std::unique_ptr<Value> &result,
        std::size_t idx,
        const CallArgsT &...callargs)
    {
        result = (obj->*exec_fun)(callargs...);
    }
};

template <typename T>
inline const T *cast_value(const Value *v)
{
    return dynamic_cast<const T *>(v);
}

template <typename T>
inline const T *cast_simple_value(const Value *v)
{
    const SimpleValue<T> *sv = dynamic_cast<const SimpleValue<T> *>(v);
    if (!sv)
        return nullptr;
    return &sv->value;
}

template <>
inline const double *cast_value<double>(const Value *v)
{
    return cast_simple_value<double>(v);
}

template <>
inline const std::int64_t *cast_value<std::int64_t>(const Value *v)
{
    return cast_simple_value<std::int64_t>(v);
}

template <>
inline const std::string *cast_value<std::string>(const Value *v)
{
    return cast_simple_value<std::string>(v);
}

template <typename T, typename... TT>
struct __args_binder<T, TT...>
{
    using next = __args_binder<TT...>;
    template <typename OpT, typename... ArgsT, typename... CallArgsT>
    static void run(const OpT *obj,
        std::unique_ptr<Value> (OpT::*exec_fun)(const ArgsT &...) const,
        const std::vector<const Value *> &args,
        std::unique_ptr<Value> &result,
        std::size_t idx,
        const CallArgsT &...callargs)
    {
        if (!args[idx])
        {
            throw std::runtime_error(
                std::string("Nul input argument ") + std::to_string(idx + 1));
        }
        const T *tptr = cast_value<T>(args[idx]);
        // cast worked
        if (tptr)
        {
            next::run(obj, exec_fun, args, result, idx + 1, callargs..., *tptr);
            return;
        }
        // could be due to string being given for readable -- try reading
        if constexpr (std::is_base_of<IFromFile<T>, T>::value)
        {
            const auto *perhaps_fn = dynamic_cast<const StrValue *>(args[idx]);
            if (perhaps_fn)
            {
                // input is string -- attempt to read it and pass as argumenet
                T input = T::value_from_file(perhaps_fn->value);
                next::run(obj, exec_fun, args, result, idx + 1, callargs..., input);
                return;
            }
        }
        // wrong cast
        throw std::runtime_error(std::string("Error trying to interpret "
                                             "argument ")
            + std::to_string(idx + 1));
    }
};

template <typename OpT, typename... ArgsT>
inline std::unique_ptr<Value> bind_args(const OpT *obj,
    std::unique_ptr<Value> (OpT::*exec_fun)(const ArgsT &...) const,
    const std::vector<const Value *> &args)
{
    if (sizeof...(ArgsT) != args.size())
    {
        throw std::runtime_error(
            std::string("Argument list length incorrect: expected ")
            + std::to_string(sizeof...(ArgsT)) + " arguments but got "
            + std::to_string(args.size()));
    }

    __args_binder<ArgsT...> binder;
    std::unique_ptr<Value> result = nullptr;
    binder.run(obj, exec_fun, args, result, 0);
    return result;
}

} // namespace aquila::interpreter