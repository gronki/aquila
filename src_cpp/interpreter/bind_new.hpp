#pragma once

#include <iostream>
#include <memory>
#include <operation.hpp>
#include <stdexcept>
#include <string>
#include <struct_bind.hpp>
#include <value.hpp>
#include <vector>

namespace aquila::interpreter
{

template <typename T>
struct __castng_helper
{
};
template <typename T>
struct __castng_helper<Ptr<T>>
{
    static Ptr<T> cast(ValuePtr &ptr)
    {
        auto casted = value_cast<T>(ptr);
        if (!casted)
            throw std::runtime_error("Cast error");
        return casted;
    }
};
template <typename T>
struct __castng_helper<const T &>
{
    static const T &cast(ValuePtr &ptr)
    {
        if (!ptr)
            throw std::runtime_error("Empty value not allowed");
        auto casted = value_cast<T>(ptr.get());
        if (!casted)
            throw std::runtime_error("Cast error");
        return *casted;
    }
};
template <typename T>
struct __castng_helper<const T *>
{
    static const T &cast(ValuePtr &ptr)
    {
        if (!ptr)
            return nullptr;
        auto casted = value_cast<T>(ptr.get());
        if (!casted)
            throw std::runtime_error("Cast error");
        return casted;
    }
};

template <typename T>
struct __arg_helper
{
    static T process(const ArgManifest &manifest,
        const manifest_properties_t &props,
        std::vector<ValuePtr> &args,
        size_t iarg)
    {
        if (iarg < props.num_keyword + props.num_positionals)
            return __castng_helper<T>::cast(args[iarg]);
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
    static Ellipsis<T> process(const ArgManifest &manifest,
        const manifest_properties_t &props,
        std::vector<ValuePtr> &args,
        size_t iarg)
    {
        if (!props.has_ellipsis)
            throw std::logic_error("argspec does not contain an ellipsis but the "
                                   "argument list requests it");
        if (iarg < props.num_positionals)
            throw std::logic_error("Ellipsis may not appear in the argument list until "
                                   "all positional args are exhausted");
        Ellipsis<T> el;
        auto num_args = props.num_keyword + props.num_positionals;
        for (auto iarg = num_args; iarg < args.size(); iarg++)
            el.data.push_back(__castng_helper<T>::cast(args[iarg]));
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
    if (vals.size() < manifest.size())
        throw std::logic_error("consistency error: value list shorter than manifest");
    bool seeded = false;
    for (auto i = 0ul; i < manifest.size(); i++)
    {
        const auto &spec = manifest[i];
        if (!spec.field)
            continue;
        if (spec.field->struct_nr != struct_nr)
            continue;
        const auto *ts_ptr = static_cast<const StructFieldB<TS> *>(spec.field.get());
        if (!seeded)
        {
            result = ts_ptr->struct_default();
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
    static Struct<T, struct_nr> process(const ArgManifest &manifest,
        const manifest_properties_t &props,
        std::vector<ValuePtr> &args,
        size_t iarg)
    {
        if (iarg < props.num_positionals)
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
    const manifest_properties_t &props,
    std::vector<ValuePtr> &args,
    std::index_sequence<iarg...>)
{
    return (obj->*exec_fun)(__arg_helper<ArgsT>::process(manifest, props, args, iarg)...);
}

template <typename OpT, typename... ArgsT>
inline ValuePtr bind_args_new(const OpT *obj,
    ValuePtr (OpT::*exec_fun)(ArgsT...) const,
    const ArgManifest &manifest,
    const manifest_properties_t &props,
    std::vector<ValuePtr> &args)
{
    return __bind_args_new(
        obj, exec_fun, manifest, props, args, std::index_sequence_for<ArgsT...>{});
}

} // namespace aquila::interpreter

namespace aquila
{
using interpreter::bind_args_new;
}
