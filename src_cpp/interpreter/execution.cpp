#include <algorithm>
#include <cmath>
#include <exception>
#include <thread>

#include "execution.hpp"
#include "operation.hpp"
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
        auto manifest = this->op->arg_manifest();
        props.analyze(manifest);
        if (props.has_ellipsis)
            ellipsis_entry = {.name = ARG_ELLIPSIS,
                .sequence = manifest.back().sequence,
                .convert = manifest.back().convert};
        match = match_arguments(manifest, props, this->keys);
        for (const auto &key : this->keys)
        {
            expansion.push_back(key == std::string(1, EXPAND_DELIM));
            is_keyword.push_back(!key.empty() && !expansion.back());
        }
    }
    catch (std::exception &e)
    {
        throw std::runtime_error(
            std::string("operation ") + this->op->name() + ": " + e.what());
    }
}

static std::vector<const Value *> make_ith_argument(const std::vector<const Value *> &args,
    const std::vector<int> &is_sequence,
    std::int64_t iseq)
{
    std::vector<const Value *> argvec(args.size());

    for (size_t iarg = 0; iarg < args.size(); iarg++)
    {
        if (is_sequence[iarg])
        {
            const SequenceValue *seq_ptr = static_cast<const SequenceValue *>(args[iarg]);
            const Value *ptr = seq_ptr->items[iseq].get();
            argvec[iarg] = ptr;
        }
        else
        {
            argvec[iarg] = args[iarg];
        }
    }

    return argvec;
}

static ValuePtr op_call_with_debug(
    const Operation &op, const std::vector<const Value *> &args)
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
        auto result = op.call(args);
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

static std::unique_ptr<Value> op_call_with_sequencing(const Operation &op,
    std::vector<const Value *> args,
    const std::vector<ArgMatch> &match)
{

    if (args.size() == 0)
        return op_call_with_debug(op, args);

    std::vector<int> is_sequence(args.size(), 0);

    constexpr std::int64_t SEQUENCE_NOT_FOUND = -1;
    std::int64_t sequence_len = SEQUENCE_NOT_FOUND;

    for (size_t iarg = 0; iarg < args.size(); iarg++)
    {
        const Value *arg = args[iarg];

        // confusing, but if an argument is expected to be a sequence,
        // we consider it as a single value instead of expanding it
        auto seq_arg = match[iarg].sequence ? nullptr : value_cast<SequenceValue>(arg);

        is_sequence[iarg] = seq_arg != nullptr;

        if (!seq_arg)
            continue;

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

    auto get_sanitized = [&is_sequence, &match](std::vector<ValuePtr> &owner,
                             std::vector<const Value *> args,
                             int mask_seq)
    {
        for (size_t iarg = 0; iarg < args.size(); iarg++)
        {
            if (is_sequence[iarg] != mask_seq || !match[iarg].convert)
                continue;
            owner[iarg] = match[iarg].convert(*args[iarg]);
            if (owner[iarg])
                args[iarg] = owner[iarg].get();
        }
        return args;
    };
    std::vector<std::unique_ptr<Value>> sanitized(args.size());
    // sanitize non-sequence args
    args = get_sanitized(sanitized, std::move(args), 0);

    if (sequence_len == SEQUENCE_NOT_FOUND)
    {
        return op_call_with_debug(op, args);
    }

    if (sequence_len == 0)
        return std::make_unique<SequenceValue>(std::vector<ValuePtr>{});

    std::vector<std::unique_ptr<Value>> result(sequence_len);

    auto process_seq_item = [&op, &args, &is_sequence, &get_sanitized](int64_t iseq) -> ValuePtr
    {
        std::vector<std::unique_ptr<Value>> sanitized_seq(args.size());
        return op_call_with_debug(op,
            get_sanitized(sanitized_seq, make_ith_argument(args, is_sequence, iseq), 1));
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

    return std::make_unique<SequenceValue>(std::move(result));
}

const Value *OpNode::yield()
{
    if (value)
        return value.get();

    std::vector<const Value *> arg_results(args.size());
    std::vector<const SequenceValue *> seq_results(args.size());
    std::vector<int> expanded_counts(args.size());
    arg_results.reserve(args.size());
    size_t num_expanded = 0;

    for (size_t iarg = 0; iarg < args.size(); iarg++)
    {
        auto result = args[iarg]->yield();
        arg_results[iarg] = result;
        seq_results[iarg] = expansion[iarg] ? value_cast<SequenceValue>(result) : nullptr;
        if (!is_keyword[iarg])
            num_expanded += seq_results[iarg] ? seq_results[iarg]->size() : 1;
    }
    num_expanded += props.num_keyword;
    auto orig_ptrs = build_ptrs_from_match(arg_results, match);
    const auto num_match = props.num_positionals + props.num_keyword;
#ifndef NDEBUG
    std::cout << "num expanded= " << num_expanded << std::endl;
#endif
    if (!props.has_ellipsis && num_expanded != num_match)
        throw std::runtime_error(
            std::string("Mismatch in total argument count for function ") + op->name()
            + std::string("; expected ") + std::to_string(num_match)
            + std::string(" but got ") + std::to_string(num_expanded));
    std::vector<const Value *> expanded_ptrs(num_expanded);
    for (size_t imatch = props.num_positionals; imatch < num_match; imatch++)
        expanded_ptrs[imatch] = orig_ptrs[imatch];

    auto idest = [this](size_t ipos_cursor)
    {
        return ipos_cursor < this->props.num_positionals
            ? ipos_cursor
            : ipos_cursor + this->props.num_keyword;
    };

    size_t ipos_cursor = 0;
    for (size_t ipos = 0; ipos < args.size(); ipos++)
    {
        if (!keys[ipos].empty() && !expansion[ipos])
            continue;
#ifndef NDEBUG
        std::cout << "ipos = " << ipos << "   cursor = " << ipos_cursor << std::endl;
#endif
        if (!expansion[ipos] || !seq_results[ipos])
        {
#ifndef NDEBUG
            std::cout << "arg @" << ipos << " " << *arg_results[ipos] << " -> expanded "
                      << idest(ipos_cursor) << std::endl;
#endif
            expanded_ptrs[idest(ipos_cursor)] = arg_results[ipos];
            ipos_cursor += 1;
            continue;
        }
        for (auto &ptr : seq_results[ipos]->items)
        {

#ifndef NDEBUG
            std::cout << "seq @" << ipos << " " << *seq_results[ipos] << " -> expanded "
                      << idest(ipos_cursor) << std::endl;
#endif
            expanded_ptrs[idest(ipos_cursor)] = ptr.get();
            ipos_cursor += 1;
        }
    }
    while (match.size() < num_expanded)
        match.push_back({.pos = match.size(),
            .convert = ellipsis_entry.convert,
            .sequence = ellipsis_entry.sequence});
    try
    {
        value = op_call_with_sequencing(*op, expanded_ptrs, match);
    }
    catch (const std::runtime_error &e)
    {
        throw std::runtime_error(
            std::string("Error in operation ") + op->name() + ": " + e.what());
    }

    for (auto &arg : args)
    {
        arg->clean();
    }

    return value.get();
}

const Value *InlineAssignmentNode::yield()
{
    if (value)
        return value.get();

    const Value *in = arg->yield();

    if (idents.size() == 1)
    {
        return ns.push(idents[0], in->clone());
    }
    else
    {
        const SequenceValue &sqv = value_cast<SequenceValue>(*in);
        if (sqv.size() != idents.size())
            throw std::runtime_error("expected sequence of length "
                + std::to_string(idents.size()) + ", got: " + std::to_string(sqv.size()));
        for (std::size_t iarg = 0; iarg < idents.size(); iarg++)
        {
            ns.push(idents[iarg], sqv.items[iarg]->clone());
        }

        return in;
    }

    return nullptr;
}

} // namespace aquila::interpreter
