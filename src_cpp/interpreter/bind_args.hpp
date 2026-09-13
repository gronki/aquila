#pragma once

#include <concepts>
#include <memory>
#include <vector>

#include "value.hpp"

namespace aquila::interpreter
{

template <typename T>
struct __cast_helper;

template <typename T>
struct __cast_helper
{
    static T cast(ValuePtr &arg)
        requires std::same_as<T, Str> || std::same_as<T, Int> || std::same_as<T, Real>
    {
        if (!arg)
            return {};
        const auto *sv = value_cast<SimpleValue<T>>(arg.get());
        if (!sv)
            // wrong cast
            throw std::runtime_error("type error: expected " + value_type_info<T>().str()
                + " but got " + arg->get_type().str() + " " + arg->str());
        return sv->value;
    }
};
template <typename T>
struct __cast_helper<const T &>
{
    static const T &cast(ValuePtr &arg)
        requires std::same_as<T, Str> || std::same_as<T, Int> || std::same_as<T, Real>
    {
        if (!arg)
            throw std::runtime_error("empty argument");
        const auto *sv = value_cast<SimpleValue<T>>(arg.get());
        if (!sv)
            // wrong cast
            throw std::runtime_error("type error: expected " + value_type_info<T>().str()
                + " but got " + arg->get_type().str() + " " + arg->str());
        return sv->value;
    }
    static const T &cast(ValuePtr &arg)
        requires std::derived_from<T, Value>
    {
        if (!arg)
            throw std::runtime_error("empty argument");
        const auto *v = value_cast<T>(arg.get());
        if (!v)
            // wrong cast
            throw std::runtime_error("type error: expected " + value_type_info<T>().str()
                + " but got " + arg->get_type().str() + " " + arg->str());
        return *v;
    }
};

template <typename T>
struct __cast_helper<const T *>
{
    static const T *cast(ValuePtr &arg)
        requires std::same_as<T, Str> || std::same_as<T, Int> || std::same_as<T, Real>
    {
        if (!arg)
            return {};
        const auto *sv = value_cast<SimpleValue<T>>(arg.get());
        if (!sv)
            // wrong cast
            throw std::runtime_error("type error: expected " + value_type_info<T>().str()
                + " but got " + arg->get_type().str() + " " + arg->str());
        return &sv->value;
    }
    static const T *cast(ValuePtr &arg)
        requires std::derived_from<T, Value>
    {
        if (!arg)
            return {};
        const auto *v = value_cast<T>(arg.get());
        if (!v)
            // wrong cast
            throw std::runtime_error("type error: expected " + value_type_info<T>().str()
                + " but got " + arg->get_type().str() + " " + arg->str());
        return v;
    }
};

template <typename T>
struct __cast_helper<Ptr<T>>
{
    static Ptr<T> cast(ValuePtr &arg)
    {
        if (!arg)
            return {};
        auto casted = value_cast<T>(arg);
        if (casted)
            return casted;
        // wrong cast
        throw std::runtime_error("type error: expected " + value_type_info<T>().str()
            + " but got " + arg->get_type().str() + " " + arg->str());
    }
};

template <typename T>
struct __caster;

template <typename T>
struct __caster
{
    static constexpr std::size_t count_arg = 1;
    T cast(std::vector<ValuePtr> &args, std::size_t idx)
    {
        try
        {
            return __cast_helper<T>::cast(args[idx]);
        }
        catch (const std::exception &e)
        {
            // wrong cast
            throw std::runtime_error(std::string("Error trying to interpret argument ")
                + std::to_string(idx + 1) + ": " + e.what());
        }
    }
};

template <typename T>
struct __caster<std::vector<T>>
{
    static constexpr std::size_t count_arg = 0;
    std::vector<T> cast(std::vector<ValuePtr> &args, std::size_t idx)
    {
        std::vector<T> collected;
        collected.reserve(idx <= args.size() ? args.size() - idx : 0);
        for (auto iarg = idx; iarg < args.size(); iarg++)
        {
            try
            {
                collected.push_back(__cast_helper<T>::cast(args[iarg]));
            }
            catch (const std::exception &e)
            {
                // wrong cast
                throw std::runtime_error(
                    std::string("Error trying to interpret argument ")
                    + std::to_string(idx + 1) + ": " + e.what());
            }
        }
        return collected;
    }
};

template <typename OpT, typename... ArgsT, std::size_t... iarg>
inline ValuePtr __bind_args(const OpT *obj,
    ValuePtr (OpT::*exec_fun)(ArgsT...) const,
    std::vector<ValuePtr> &args,
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
inline ValuePtr bind_args(
    const OpT *obj, ValuePtr (OpT::*exec_fun)(ArgsT...) const, std::vector<ValuePtr> &args)
{
    return __bind_args(obj, exec_fun, args, std::index_sequence_for<ArgsT...>{});
}

template <typename OpT, typename... ArgsT, std::size_t... iarg>
inline std::unique_ptr<Value> __bind_args_u(const OpT *obj,
    std::unique_ptr<Value> (OpT::*exec_fun)(ArgsT...) const,
    std::vector<ValuePtr> &args,
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
inline ValuePtr bind_args(const OpT *obj,
    std::unique_ptr<Value> (OpT::*exec_fun)(ArgsT...) const,
    std::vector<ValuePtr> &args)
{
    return __bind_args_u(obj, exec_fun, args, std::index_sequence_for<ArgsT...>{});
}

} // namespace aquila::interpreter
