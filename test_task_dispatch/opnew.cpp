#include "../src_cpp/interpreter/value.hpp"
#include <iostream>
#include <memory>
#include <tuple>
#include <vector>

using namespace aquila;
using namespace aquila::interpreter;
using std::make_unique;

// template <typename T>
// class ValueHolder
// {
//     std::unique_ptr<T> t_own;
//     T *t_ref;

//     T &get_t() { return t_own ? *t_own : t_ref; }
//     std::unique_ptr<T> pop_t()
//     {
//         return t_own ? std::move(t_own) : std::make_unique<T>(*t_ref);
//     }
// };
//

template <value_type tid>
struct mytype
{
    static constexpr value_type my_type{tid};
};

template <typename T>
const T *__cast_safe(const Value *arg)
{
    return arg && typeid(*arg) == typeid(T) ? static_cast<const T *>(arg) : nullptr;
}

template <typename T>
const T *__cast_or_throw(const Value *arg)
{
    auto ptr = __cast_safe<T>(arg);
    if (!ptr)
    {
        throw std::runtime_error(
            std::string("Error trying to interpret argument, expected: ")
            + typeid(T).name() + " but got: " + typeid(*arg).name());
    }
    return ptr;
}

template <typename T>
void __cast_single_args(const std::vector<const Value *> &args,
    std::vector<const SequenceValue *> &seqs,
    std::size_t idx,
    const T **tptr)
{
    const SequenceValue *maybe_sequence = __cast_safe<SequenceValue>(args[idx]);
    seqs[idx] = maybe_sequence;
    if (!maybe_sequence)
    {
        *tptr = __cast_or_throw<T>(args[idx]);
    }
}

template <>
void __cast_single_args<SequenceValue>(const std::vector<const Value *> &args,
    std::vector<const SequenceValue *> &seqs,
    std::size_t idx,
    const SequenceValue **tptr)
{
    const SequenceValue *maybe_sequence = __cast_safe<SequenceValue>(args[idx]);
    seqs[idx] = nullptr;
#ifndef NDEBUG
    std::cout << "seq at: " << idx << std::endl;
#endif
    if (!maybe_sequence)
    {
        throw std::runtime_error(std::string("Sequence expected at position ")
            + std::to_string(idx) + " but not provided");
    }
    *tptr = maybe_sequence;
}

template <typename T>
void __cast_seq_args(const std::vector<const SequenceValue *> &seqs,
    std::size_t iseq,
    std::size_t idx,
    const T **tptr)
{
    if (!seqs[idx])
        return;
    std::cout << "converting: " << iseq << std::endl;
    *tptr = __cast_or_throw<T>(seqs[idx]->items[iseq].get());
}

void check_sequences(
    const std::vector<const SequenceValue *> &seqs, size_t &seq_len, bool &any)
{
    size_t n_args = seqs.size();
    any = false;
    for (size_t i = 0; i < n_args; i++)
    {
        auto seq_ptr = seqs[i];
        if (!seq_ptr)
            continue;
        if (!any)
        {
            any = true;
            seq_len = seq_ptr->size();
        }
        else if (seq_len != seq_ptr->size())
        {
            throw std::runtime_error("seq len mismatch");
        }
    }
}

template <typename Op>
class OpBase
{
    template <typename... ArgsT, std::size_t... iarg>
    std::unique_ptr<Value> __bind_args(
        std::unique_ptr<Value> (Op::*exec_fun)(const ArgsT &...) const,
        const std::vector<const Value *> &args,
        std::index_sequence<iarg...>)
    {
        Op *obj = static_cast<Op *>(this);
        std::vector<const SequenceValue *> sequences(sizeof...(ArgsT));
        std::tuple<const ArgsT *...> ptrs;
        (__cast_single_args<ArgsT>(args, sequences, iarg, &std::get<iarg>(ptrs)), ...);
        size_t seq_len;
        bool any_seq;
        check_sequences(sequences, seq_len, any_seq);
        if (!any_seq)
            return (obj->*exec_fun)(*std::get<iarg>(ptrs)...);
        std::vector<std::unique_ptr<Value>> results(seq_len);
        for (size_t iseq = 0; iseq < seq_len; iseq++)
        {
            std::tuple<const ArgsT *...> seq_ptrs(ptrs);
            (__cast_seq_args<ArgsT>(sequences, iseq, iarg, &std::get<iarg>(seq_ptrs)), ...);
            results[iseq] = (obj->*exec_fun)(*std::get<iarg>(seq_ptrs)...);
        }
        return make_unique<SequenceValue>(std::move(results));
    }

    template <typename... ArgsT>
    inline std::unique_ptr<Value> bind_args(
        std::unique_ptr<Value> (Op::*exec_fun)(const ArgsT &...) const,
        const std::vector<const Value *> &args)
    {
        if (sizeof...(ArgsT) != args.size())
        {
            throw std::runtime_error(
                std::string("Argument list length incorrect: expected ")
                + std::to_string(sizeof...(ArgsT)) + " arguments but got "
                + std::to_string(args.size()));
        }

        return __bind_args(exec_fun, args, std::index_sequence_for<ArgsT...>{});
    }

public:
    ValuePtr _run(const std::vector<const Value *> &vals)
    {
        return bind_args(&Op::run, vals);
    }
};

class Op : public OpBase<Op>
{
public:
    ValuePtr run(const RealValue &a, const RealValue &b) const
    {
        return std::make_unique<RealValue>(a.value * b.value);
    }
};

int main()
{
    Op op;
    RealValue v1{2.0}, v2{3.0};
    ValuePtrVector vv;
    vv.push_back(make_unique<RealValue>(1.0));
    vv.push_back(make_unique<RealValue>(2.0));
    SequenceValue sv(std::move(vv));

    std::cout << *op._run({&sv, &v2}) << std::endl;
    std::cout << *op._run({&v1, &v2}) << std::endl;

    value_type tid("tabonamasawa");
    for (size_t i = 0; i < 24; i++)
    {
        std::cout << tid.tname[i] << " " << (int)tid.tname[i] << std::endl;
    }
    std::cout << tid << std::endl;
    mytype<value_type("test")> t;
    std::cout << t.my_type << std::endl;
    return 0;
}
