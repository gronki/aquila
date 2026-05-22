#pragma once

#include <cstdint>
#include <memory>
#include <variant>
#include <vector>

#include "value.hpp"

namespace aquila::interpreter
{

template <typename T>
struct __handler
{
    static const T *cast(const Value *v) { return value_cast<T>(v); }
    static const value_type &type_name() { return T::type_name; }
};
template <>
struct __handler<Value>
{
    static constexpr value_type any_value{"any"};
    static const Value *cast(const Value *v) { return v; }
    static const value_type &type_name() { return any_value; }
};
template <typename T>
struct __sv_handler
{
    static const T *cast(const Value *v)
    {
        auto sv = value_cast<SimpleValue<T>>(v);
        return sv ? &sv->value : nullptr;
    }
    static const value_type &type_name() { return SimpleValue<T>::type_name; }
};
template <>
struct __handler<double> : __sv_handler<double>
{
};
template <>
struct __handler<std::int64_t> : __sv_handler<std::int64_t>
{
};
template <>
struct __handler<std::string> : __sv_handler<std::string>
{
};

template <typename T>
struct vector_or
{
};
template <typename T>
struct vector_or<std::vector<const T *>>
{
    std::vector<const T *> vec;
};

struct __vector_holder_base
{
};

template <typename T>
struct __caster
{
    static constexpr std::size_t count_arg = 1;
    const T &cast(const std::vector<const Value *> &args, std::size_t idx)
    {
        const Value *arg = args[idx];
        if (!arg)
        {
            throw std::runtime_error(
                std::string("Nul input argument ") + std::to_string(idx + 1));
        }
        const T *tptr = __handler<T>::cast(arg);
        // cast worked
        if (tptr)
        {
            return *tptr;
        }
        // wrong cast
        throw std::runtime_error(std::string("Error trying to interpret "
                                             "argument ")
            + std::to_string(idx + 1) + ": expected " + __handler<T>::type_name().str()
            + " but got " + arg->get_type().str() + " " + arg->str());
    }
};

template <typename T>
struct __caster<std::vector<const T *>>
{
    std::vector<const T *> ptrs;
    static constexpr std::size_t count_arg = 0;
    const std::vector<const T *> &cast(
        const std::vector<const Value *> &args, std::size_t idx)
    {
        ptrs.reserve(idx <= args.size() ? args.size() - idx : 0);
        for (auto iarg = idx; iarg < args.size(); iarg++)
        {
            const Value *arg = args[iarg];
            if (!arg)
            {
                throw std::runtime_error(
                    std::string("Nul input argument ") + std::to_string(iarg + 1));
            }
            const T *tptr = __handler<T>::cast(arg);
            if (!tptr)
                // wrong cast
                throw std::runtime_error(std::string("Error trying to interpret "
                                                     "argument ")
                    + std::to_string(iarg + 1) + ": expected "
                    + __handler<T>::type_name().str() + " but got "
                    + arg->get_type().str() + " " + arg->str());
            ptrs.push_back(tptr);
        }
        return ptrs;
    }
};

template <typename OpT, typename... ArgsT, std::size_t... iarg>
inline std::unique_ptr<Value> __bind_args(const OpT *obj,
    std::unique_ptr<Value> (OpT::*exec_fun)(const ArgsT &...) const,
    const std::vector<const Value *> &args,
    std::index_sequence<iarg...>)
{
    std::tuple<__caster<ArgsT>...> casters;
    auto min_args = ((std::get<iarg>(casters).count_arg) + ... + std::size_t(0));
    bool has_vector = sizeof...(ArgsT) > min_args;
    if (args.size() < min_args || (!has_vector && args.size() != sizeof...(ArgsT)))
        throw std::runtime_error(
            std::string("Argument list length incorrect: expected minimum ")
            + std::to_string(min_args) + " arguments but got "
            + std::to_string(args.size()));
    return (obj->*exec_fun)(std::get<iarg>(casters).cast(args, iarg)...);
}

template <typename OpT, typename... ArgsT>
inline std::unique_ptr<Value> bind_args(const OpT *obj,
    std::unique_ptr<Value> (OpT::*exec_fun)(const ArgsT &...) const,
    const std::vector<const Value *> &args)
{
    return __bind_args(obj, exec_fun, args, std::index_sequence_for<ArgsT...>{});
}

} // namespace aquila::interpreter
