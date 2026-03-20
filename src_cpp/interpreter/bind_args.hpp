#pragma once

#include <cstdint>
#include <memory>
#include <vector>

#include "value.hpp"

namespace aquila::interpreter
{

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

template <typename T>
const T &__cast_one(const std::vector<const Value *> &args, std::size_t idx)
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
        return *tptr;
    }
    // wrong cast
    throw std::runtime_error(std::string("Error trying to interpret "
                                         "argument ")
        + std::to_string(idx + 1));
}

template <typename OpT, typename... ArgsT, std::size_t... iarg>
inline std::unique_ptr<Value> __bind_args(const OpT *obj,
    std::unique_ptr<Value> (OpT::*exec_fun)(const ArgsT &...) const,
    const std::vector<const Value *> &args,
    std::index_sequence<iarg...>)
{
    return (obj->*exec_fun)(__cast_one<ArgsT>(args, iarg)...);
}

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

    return __bind_args(obj, exec_fun, args, std::index_sequence_for<ArgsT...>{});
}

} // namespace aquila::interpreter