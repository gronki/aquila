#pragma once

#include <cstdint>
#include <memory>
#include <vector>

#include "value.hpp"

namespace aquila::interpreter
{

template <typename T>
struct __handler {
    static const T* cast(const Value* v) { return value_cast<T>(v); }
    static const value_type& type_name() { return T::type_name; }
};
template <>
struct __handler<Value> {
    static constexpr value_type any_value{"any"};
    static const Value* cast(const Value* v) { return v; }
    static const value_type& type_name() { return any_value; }
};
template <typename T>
struct __sv_handler {
    static const T* cast(const Value* v) { 
            auto sv = value_cast<SimpleValue<T>>(v); 
            return sv ? &sv->value : nullptr; }
    static const value_type& type_name() { return SimpleValue<T>::type_name; }
};
template<> struct __handler<double> : __sv_handler<double> {};
template<> struct __handler<std::int64_t> : __sv_handler<std::int64_t> {};
template<> struct __handler<std::string> : __sv_handler<std::string> {};

template <typename T>
const T &__cast_one(const std::vector<const Value *> &args, std::size_t idx)
{
    const Value* arg = args[idx];
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
