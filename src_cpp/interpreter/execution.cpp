#include <algorithm>
#include <cmath>
#include <exception>
#include <thread>

#include "execution.hpp"
#include "operation.hpp"
#include "type_converter.hpp"
#include "value.hpp"

namespace aquila::interpreter
{
OpNode::OpNode(std::unique_ptr<Operation> op,
    std::vector<std::unique_ptr<ExecNode>> args,
    std::vector<std::string> keys,
    Namespace &ns) :
    ExecNode(ns), op(std::move(op)), args(std::move(args)), keys(std::move(keys))
{
    try
    {
        for (const auto &key : this->keys)
        {
            bool is_expansion = key == std::string(1, EXPAND_DELIM);
            expansion.push_back(is_expansion);
            is_keyword.push_back(!key.empty() && !is_expansion);
            any_expansion = any_expansion || is_expansion;
        }
    }
    catch (std::exception &e)
    {
        throw std::runtime_error(
            std::string("operation ") + this->op->name() + ": " + e.what());
    }
}

static std::vector<Ptr<Value>> make_ith_argument(const std::vector<Ptr<Value>> &args,
    std::vector<Ptr<SequenceValue>> &sequences,
    std::int64_t iseq)
{
    std::vector<Ptr<Value>> argvec(args.size());

    for (size_t iarg = 0; iarg < args.size(); iarg++)
    {
        auto &seq = sequences[iarg];
        if (seq)
        {
            if (seq.is_owned())
            {
                argvec[iarg] = seq.own_item(iseq);
            }
            else
            {
                argvec[iarg] = seq->items[iseq].get();
            }
        }
        else
        {
            argvec[iarg] = args[iarg].get();
        }
    }

    return argvec;
}

static ValuePtr op_call_with_debug(const Operation &op, std::vector<Ptr<Value>> args)
{
#ifndef NDEBUG
    std::cout << "running " << op.name() << "(";
    for (size_t iarg = 0; iarg < args.size(); iarg++)
    {
        if (iarg > 0)
            std::cout << ", ";
        std::cout << (args[iarg] ? args[iarg]->str() : "(null)");
    }
    std::cout << ")" << std::endl << "          ---> ";
#endif

    try
    {
        auto result = op.call(std::move(args));
#ifndef NDEBUG
        std::cout << (result ? result->str() : "(null)") << std::endl;
#endif
        return result;
    }
    catch (const std::exception &e)
    {
#ifndef NDEBUG
        std::cout << "(error) " << e.what() << std::endl;
#endif
        throw;
    }
}

static Ptr<Value> run_sanitizer(Ptr<Value> arg, const ConvertFun &conv)
{
    if (!arg)
        return {nullptr};
    auto converted = conv(*arg);
    if (converted)
        return converted;
    return arg;
}

static constexpr int64_t SEQUENCE_NOT_FOUND = -1;

static std::vector<Ptr<SequenceValue>> pick_sequences(
    std::vector<ValuePtr> &args, const std::vector<ArgMatch> &match, int64_t &sequence_len)
{

    std::vector<Ptr<SequenceValue>> sequences(args.size());

    sequence_len = SEQUENCE_NOT_FOUND;

    for (size_t iarg = 0; iarg < args.size(); iarg++)
    {
        auto &arg = args[iarg];

        if (!arg)
            continue;

        if (match[iarg].sequence || !arg->is_sequence())
            continue;

        sequences[iarg] = std::move(arg);
        auto &seq_arg = sequences[iarg];

        if (sequence_len == SEQUENCE_NOT_FOUND)
        {
            sequence_len = seq_arg->size();
        }
        else if (sequence_len != std::int64_t(seq_arg->size()))
        {
            throw std::runtime_error(
                std::string("sequence length must be the same but got: ")
                + std::to_string(seq_arg->size()) + " != " + std::to_string(sequence_len));
        }
    }
    return sequences;
}

static ValuePtr op_call_with_sequencing(
    const Operation &op, std::vector<ValuePtr> args, const std::vector<ArgMatch> &match)
{

    if (args.size() == 0)
        return op_call_with_debug(op, {});

    int64_t sequence_len;
    auto sequences = pick_sequences(args, match, sequence_len);

    // sanitize non-sequence args
    for (size_t iarg = 0; iarg < args.size(); iarg++)
    {
        if (!args[iarg] || !match[iarg].convert)
            continue;
        args[iarg] = run_sanitizer(std::move(args[iarg]), match[iarg].convert);
    }

    if (sequence_len == SEQUENCE_NOT_FOUND)
    {
        return op_call_with_debug(op, std::move(args));
    }

    if (sequence_len == 0)
        return Ptr<SequenceValue>::make(std::vector<ValuePtr>{});

    std::vector<ValuePtr> result(sequence_len);

    auto process_seq_item = [&op, &args, &sequences, &match](int64_t iseq) -> ValuePtr
    {
        std::vector<ValuePtr> sanitized_seq(args.size());
        auto ith_vector = make_ith_argument(args, sequences, iseq);
        for (size_t iarg = 0; iarg < args.size(); iarg++)
        {
            if (!sequences[iarg] || !match[iarg].convert)
                continue;
            ith_vector[iarg] =
                run_sanitizer(std::move(ith_vector[iarg]), match[iarg].convert);
        }

        return op_call_with_debug(op, std::move(ith_vector));
    };

#ifdef AQUILA_PARALLEL

    const auto ncpu = std::max(1l, (std::int64_t)std::thread::hardware_concurrency());
    const auto num_threads = std::min(ncpu, sequence_len);
    const auto block_size = (sequence_len + num_threads - 1) / num_threads;
#    ifndef NDEBUG
    std::cout << op.name() << " threads = " << num_threads << " bs = " << block_size
              << std::endl;
#    endif

    std::vector<std::thread> threads;
    threads.reserve(num_threads);
    std::vector<std::exception_ptr> exceptions(num_threads);

    for (std::int64_t ithread = 0; ithread < num_threads; ithread++)
    {
        threads.emplace_back(
            [&, ithread]()
            {
                try
                {
                    for (std::int64_t iseq = ithread * block_size;
                        iseq < std::min((ithread + 1) * block_size, sequence_len);
                        iseq++)
                    {
#    ifndef NDEBUG
                        std::cout << " --- thread = " << ithread << " item = " << iseq
                                  << std::endl;
#    endif
                        result[iseq] = process_seq_item(iseq);
                    }
                }
                catch (...)
                {
                    exceptions[ithread] = std::current_exception();
                }
            });
    }

    for (auto &thread : threads)
    {
        thread.join();
    }

    for (auto &except_ptr : exceptions)
    {
        if (except_ptr)
            std::rethrow_exception(except_ptr);
    }

#else
    for (std::int64_t iseq = 0; iseq < sequence_len; iseq++)
    {
        result[iseq] = process_seq_item(iseq);
    }
#endif

    return Ptr<SequenceValue>::make(std::move(result));
}

Ptr<Value> OpNode::yield()
{

    std::vector<Ptr<Value>> arg_results;
    std::vector<Str> key_results;

    for (size_t iarg = 0; iarg < args.size(); iarg++)
    {
        auto result = args[iarg]->yield();
        if (!expansion[iarg])
        {
            arg_results.push_back(std::move(result));
            key_results.push_back(keys[iarg]);
            continue;
        }
        auto seq_result = value_cast<SequenceValue>(result);
        if (!seq_result)
        {
            std::cout << "Expansion but not a sequence..." << std::endl;
            arg_results.push_back(std::move(result));
            key_results.push_back({});
            continue;
        }

        if (seq_result.is_owned())
        {
            auto seq_owned = seq_result.own();
            for (size_t iexp = 0; iexp < seq_owned->size(); iexp++)
            {
                arg_results.emplace_back(std::move(seq_owned->items[iexp]));
                key_results.push_back({});
            }
        }
        else
        {
            for (size_t iexp = 0; iexp < seq_result->size(); iexp++)
            {
                arg_results.emplace_back(seq_result->items[iexp].get());
                key_results.push_back({});
            }
        }
    }
    for (size_t iarg = 0; iarg < arg_results.size(); iarg++)
    {
        std::cout << "ARG " << iarg + 1 << ": key \"" << key_results[iarg]
                  << "\", value = " << *arg_results[iarg] << std::endl;
    }
    auto manifest = op->arg_manifest();
    manifest_properties_t props;
    props.analyze(manifest);
    auto match = match_arguments(manifest, props, key_results);
    try
    {
        return op_call_with_sequencing(
            *op, build_ptrs_from_match(arg_results, match), match);
    }
    catch (const std::runtime_error &e)
    {
        throw std::runtime_error(
            std::string("Error in operation ") + op->name() + ": " + e.what());
    }
}

Ptr<Value> InlineAssignmentNode::yield()
{

    Ptr<Value> in = arg->yield();

    if (!in)
        throw std::runtime_error("empty value may not be assigned");

    if (idents.size() == 1)
    {
        return &ns.push(idents[0], in.own());
    }
    else
    {
        auto owned = in.own();
        SequenceValue &sqv = value_cast<SequenceValue>(*owned);
        if (sqv.size() != idents.size())
            throw std::runtime_error("expected sequence of length "
                + std::to_string(idents.size()) + ", got: " + std::to_string(sqv.size()));
        for (std::size_t iarg = 0; iarg < idents.size(); iarg++)
        {
            ns.push(idents[iarg], sqv.items[iarg].own());
        }

        return in;
    }

    return nullptr;
}

} // namespace aquila::interpreter
