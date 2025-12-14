#pragma once

#include <memory>
#include <vector>

#include "value.hpp"

namespace aquila::interpreter
{

template <typename... Args>
struct arg_caster;

template <>
struct arg_caster<>
{
    arg_caster(const std::vector<Value *> &args, int idx = 0)
    {
        if (idx != args.size())
            throw std::runtime_error("list length mismatch");
    }
};

template <typename ArgF, typename... ArgsT>
struct arg_caster<ArgF, ArgsT...>
{
    arg_caster(const std::vector<Value *> &args, int idx = 0) : next(args, idx + 1)
    {
        if (idx > args.size())
            throw std::runtime_error("out of bounds");
        val = dynamic_cast<ArgF *>(args[idx]);
        if (!val)
            throw std::runtime_error("bad cast");
    }
    ArgF *val;
    arg_caster<ArgsT...> next;
};

template <typename TT, typename... CasterArgs, typename... CallArgsT, typename... Args>
void collector(const arg_caster<CasterArgs...> &caster,
               TT *obj,
               std::unique_ptr<Value> (TT::*f)(const CallArgsT &...),
               std::unique_ptr<Value> &result,
               const Args &...args)
{
    if constexpr (sizeof...(CasterArgs) == 0)
    {
        result = (obj->*f)(args...);
    }
    else
    {
        collector(caster.next, obj, f, result, args..., *caster.val);
    }
}

template <typename TT, typename... ArgsT>
std::unique_ptr<Value> bind_args(TT *obj,
                                 std::unique_ptr<Value> (TT::*f)(const ArgsT &...),
                                 const std::vector<Value *> &args)
{
    arg_caster<ArgsT...> caster(args);
    std::unique_ptr<Value> result;
    collector(caster, obj, f, result);
    return result;
}

} // namespace aquila::interpreter