#pragma once

#include <iostream>
#include <memory>
#include <operation.hpp>
#include <stdexcept>
#include <struct_bind.hpp>
#include <value.hpp>
#include <vector>

namespace aquila::interpreter
{

template <typename T>
std::runtime_error __type_error(const Value &got)
{
    return std::runtime_error("type error: expected " + value_type_info<T>().str()
        + " but got " + got.get_type().str() + " " + got.str());
}

template <typename T>
struct __castng_helper
{
};
template <typename T>
struct __castng_helper<Ptr<T>>
{
    static Ptr<T> cast(ValuePtr &ptr)
    {
        if (!ptr)
            return {};
        auto casted = value_cast<T>(ptr);
        if (!casted)
            throw __type_error<T>(*ptr);
        return casted;
    }
};
template <typename T>
struct __castng_helper<const T &>
{
    static const T &cast(ValuePtr &ptr)
    {
        if (!ptr)
            throw std::runtime_error("empty argument");
        auto casted = value_cast<T>(ptr.get());
        if (!casted)
            throw __type_error<T>(*ptr);
        return *casted;
    }
};
template <typename T>
struct __castng_helper<const T *>
{
    static const T *cast(ValuePtr &ptr)
    {
        if (!ptr)
            return nullptr;
        auto casted = value_cast<T>(ptr.get());
        if (!casted)
            throw __type_error<T>(*ptr);
        return casted;
    }
};

/* names the offending argument, the way the error reaches the user:
   "Error trying to interpret argument 2: type error: expected str but got real 3" */
template <typename T>
T __cast_arg(std::vector<ValuePtr> &args, size_t iarg)
{
    try
    {
        return __castng_helper<T>::cast(args[iarg]);
    }
    catch (const std::exception &e)
    {
        throw std::runtime_error(std::string("Error trying to interpret argument ")
            + std::to_string(iarg + 1) + ": " + e.what());
    }
}

template <typename T>
struct __arg_helper
{
    static T process(const ArgManifest &manifest, std::vector<ValuePtr> &args, size_t iarg)
    {
        if (iarg < manifest.num_keyword + manifest.num_positionals && iarg < args.size())
            return __cast_arg<T>(args, iarg);
        throw std::logic_error("argument list longer than argspec");
    }
};

template <typename T>
struct Ellipsis
{
    std::vector<T> data;
};

template <typename T>
struct __arg_helper<Ellipsis<T>>
{
    static Ellipsis<T> process(
        const ArgManifest &manifest, std::vector<ValuePtr> &args, size_t iarg)
    {
        if (!manifest.has_ellipsis)
            throw std::logic_error("argspec does not contain an ellipsis but the "
                                   "argument list requests it");
        if (iarg < manifest.num_positionals)
            throw std::logic_error("Ellipsis may not appear in the argument list until "
                                   "all positional args are exhausted");
        Ellipsis<T> el;
        auto num_args = manifest.num_keyword + manifest.num_positionals;
        for (auto iarg = num_args; iarg < args.size(); iarg++)
            el.data.push_back(__cast_arg<T>(args, iarg));
        return el;
    }
};

template <typename T, int struct_nr = 0>
struct Struct
{
    Struct(const T &t) : data(t) {}
    T data;
};

template <typename TS>
TS __collect_struct(
    const std::vector<Ptr<Value>> &vals, const ArgManifest &manifest, int struct_nr = 0)
{
    TS result;
    if (vals.size() < manifest.args.size())
        throw std::logic_error("consistency error: value list shorter than manifest");
    bool seeded = false;
    for (auto i = 0ul; i < manifest.args.size(); i++)
    {
        const auto &spec = manifest.args[i];
        if (!spec.field)
            continue;
        if (spec.field->struct_nr != struct_nr)
            continue;
        const auto *ts_ptr = static_cast<const StructFieldB<TS> *>(spec.field.get());
        if (!seeded)
        {
            result = ts_ptr->struct_default;
            seeded = true;
        }
        if (vals[i])
            ts_ptr->read(vals[i].get(), result);
    }
    return result;
}

template <typename T, int struct_nr>
struct __arg_helper<Struct<T, struct_nr>>
{
    static Struct<T, struct_nr> process(
        const ArgManifest &manifest, std::vector<ValuePtr> &args, size_t iarg)
    {
        if (iarg < manifest.num_positionals)
            throw std::logic_error(
                "Struct collection may not appear in the argument list until "
                "all positional args are exhausted");
        return {__collect_struct<T>(args, manifest, struct_nr)};
    }
};

template <typename OpT, typename... ArgsT, std::size_t... iarg>
inline ValuePtr __bind_args_new(const OpT *obj,
    ValuePtr (OpT::*exec_fun)(ArgsT...) const,
    const ArgManifest &manifest,
    std::vector<ValuePtr> &args,
    std::index_sequence<iarg...>)
{
    return (obj->*exec_fun)(__arg_helper<ArgsT>::process(manifest, args, iarg)...);
}

template <typename OpT, typename... ArgsT>
inline ValuePtr bind_args_new(const OpT *obj,
    ValuePtr (OpT::*exec_fun)(ArgsT...) const,
    const ArgManifest &manifest,
    std::vector<ValuePtr> &args)
{
    return __bind_args_new(
        obj, exec_fun, manifest, args, std::index_sequence_for<ArgsT...>{});
}

#define BIND_ARGS_NEW(proc)                                                            \
    ValuePtr call(std::vector<ValuePtr> args) const override                           \
    {                                                                                  \
        return bind_args_new(this, (proc), arg_manifest(), args);                      \
    }

} // namespace aquila::interpreter

namespace aquila
{
using interpreter::bind_args_new;
using interpreter::Ellipsis;
using interpreter::Struct;
} // namespace aquila
