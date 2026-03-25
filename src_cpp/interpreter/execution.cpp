#include <algorithm>

#include "execution.hpp"

namespace aquila::interpreter
{

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
    std::cout << "running " << op.name() << "(";
    for (size_t iarg = 0; iarg < args.size(); iarg++)
    {
        if (iarg > 0)
            std::cout << ", ";
        std::cout << (args[iarg] ? args[iarg]->str() : "(null)");
    }
    std::cout << ")" << std::endl << "          ---> ";

    try
    {
        auto result = op.call(args);
        std::cout << (result ? result->str() : "(null)") << std::endl;
        return result;
    }
    catch (const std::exception &e)
    {
        std::cout << "(error)" << std::endl;
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
        auto seq_arg = match[iarg].sequence ? nullptr
                                            : dynamic_cast<const SequenceValue *>(arg);

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

template <typename T>
static std::vector<T> build_from_match(
    const std::vector<T> &given, const std::vector<ArgMatch> &match, const T &defval)
{
    const size_t n_args = match.size();

    std::vector<T> result(n_args);

    for (size_t ispec = 0; ispec < n_args; ispec++)
    {
        if (match[ispec].matched)
        {
            auto iarg = match[ispec].pos;
            result[ispec] = given[iarg];
            continue;
        }

        result[ispec] = defval;
    }

    return result;
}

const Value *OpNode::yield()
{
    if (value)
        return value.get();

    std::vector<const Value *> arg_results;
    arg_results.reserve(args.size());

    for (auto &arg : args)
    {
        auto result = arg->yield();
        arg_results.push_back(result);
    }

    try
    {
        value = op_call_with_sequencing(
            *op, build_ptrs_from_match(arg_results, match), match);
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

} // namespace aquila::interpreter