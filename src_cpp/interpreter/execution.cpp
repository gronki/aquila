#include <algorithm>

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

static std::vector<const Value *> make_ith_argument(
    const std::vector<const SequenceValue *> &sequence_args,
    const std::vector<const Value *> &args,
    std::int64_t iseq)
{
    std::vector<const Value *> argvec(sequence_args.size());

    for (size_t iarg = 0; iarg < sequence_args.size(); iarg++)
    {
        if (sequence_args[iarg])
        {
            const Value *ptr = sequence_args[iarg]->items[iseq].get();
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
        std::cout << "(error)" << std::endl;
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

    std::vector<const SequenceValue *> sequence_args(args.size(), nullptr);

    constexpr std::int64_t SEQUENCE_NOT_FOUND = -1;
    std::int64_t sequence_len = SEQUENCE_NOT_FOUND;

    for (size_t iarg = 0; iarg < args.size(); iarg++)
    {
        const Value *arg = args[iarg];

        // confusing, but if an argument is expected to be a sequence,
        // we consider it as a single value instead of expanding it
        auto seq_arg = match[iarg].sequence ? nullptr : value_cast<SequenceValue>(arg);

        sequence_args[iarg] = seq_arg;

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

    std::vector<std::unique_ptr<Value>> sanitized(args.size());
    // sanitize non-sequence args
    for (size_t iarg = 0; iarg < args.size(); iarg++)
    {
        if (sequence_args[iarg] || !match[iarg].convert)
            continue;
        sanitized[iarg] = match[iarg].convert(*args[iarg]);
        if (sanitized[iarg])
            args[iarg] = sanitized[iarg].get();
    }

    if (sequence_len == SEQUENCE_NOT_FOUND)
    {
        return op_call_with_debug(op, args);
    }

    std::vector<std::unique_ptr<Value>> result(sequence_len);

    for (std::int64_t iseq = 0; iseq < sequence_len; iseq++)
    {
        std::vector<std::unique_ptr<Value>> sanitized_seq(args.size());
        auto ith_args = make_ith_argument(sequence_args, args, iseq);
        for (size_t iarg = 0; iarg < args.size(); iarg++)
        {
            if (!sequence_args[iarg] || !match[iarg].convert)
                continue;
            sanitized_seq[iarg] = match[iarg].convert(*ith_args[iarg]);
            if (sanitized_seq[iarg])
                ith_args[iarg] = sanitized_seq[iarg].get();
        }
        result[iseq] = op_call_with_debug(op, ith_args);
    }

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
            const auto idest = ipos_cursor < props.num_positionals
                ? ipos_cursor
                : ipos_cursor + props.num_keyword;
#ifndef NDEBUG
            std::cout << "arg @" << ipos << " " << *arg_results[ipos] << " -> expanded "
                      << idest << std::endl;
#endif
            expanded_ptrs[idest] = arg_results[ipos];
            ipos_cursor += 1;
            continue;
        }
        for (auto &ptr : seq_results[ipos]->items)
        {
            const auto idest = ipos_cursor < props.num_positionals
                ? ipos_cursor
                : ipos_cursor + props.num_keyword;

#ifndef NDEBUG
            std::cout << "seq @" << ipos << " " << *seq_results[ipos] << " -> expanded "
                      << idest << std::endl;
#endif
            expanded_ptrs[idest] = ptr.get();
            ipos_cursor += 1;
        }
    }
    while (match.size() < num_expanded)
        match.push_back({.pos = 0,
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
