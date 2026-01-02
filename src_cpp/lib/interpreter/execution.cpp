#include <algorithm>

#include "execution.hpp"

namespace aquila::interpreter
{

static std::vector<const Value *> make_ith_argument(
    const std::vector<const SequenceValue *> &sequences,
    const std::vector<const Value *> &args,
    Int iseq)
{
    std::vector<const Value *> argvec(sequences.size());

    for (size_t iarg = 0; iarg < sequences.size(); iarg++)
    {
        if (sequences[iarg])
        {
            argvec[iarg] = sequences[iarg]->items[iseq].get();
        }
        else
        {
            argvec[iarg] = args[iarg];
        }
    }

    return argvec;
}

static std::unique_ptr<Value> op_call_with_sequencing(
    const Operation &op, const std::vector<const Value *> &args)
{

    if (args.size() == 0)
        return op.call(args);

    std::vector<const SequenceValue *> sequences;

    sequences.reserve(args.size());
    constexpr Int SEQUENCE_NOT_FOUND = -1;
    Int sequence_len = SEQUENCE_NOT_FOUND;

    for (const Value *arg : args)
    {
        auto seq_arg = dynamic_cast<const SequenceValue *>(arg);
        sequences.push_back(seq_arg);
        if (!seq_arg)
            continue;
        if (sequence_len == SEQUENCE_NOT_FOUND)
        {
            sequence_len = seq_arg->size();
        }
        else if (sequence_len != seq_arg->size())
        {
            throw std::runtime_error(
                std::string("sequence length must be the same but got: ")
                + std::to_string(seq_arg->size()) + " != " + std::to_string(sequence_len));
        }
    }

    if (sequence_len == SEQUENCE_NOT_FOUND)
        return op.call(args);

    std::vector<std::unique_ptr<Value>> result(sequence_len);

    for (Int iseq = 0; iseq < sequence_len; iseq++)
    {
        result[iseq] = op.call(make_ith_argument(sequences, args, iseq));
    }

    return std::make_unique<SequenceValue>(std::move(result));
}

const Value *OpNode::yield()
{
    if (value)
        return value.get();

    std::vector<const Value *> arg_results;
    arg_results.reserve(args.size());

    for (auto &arg : args)
    {
        arg_results.push_back(arg->yield());
    }

    try
    {
        if (use_match)
        {
            value =
                op_call_with_sequencing(*op, build_ptrs_from_match(arg_results, match));
        }
        else
        {
            value = op_call_with_sequencing(*op, arg_results);
        }
    }
    catch (const std::runtime_error &e)
    {
        throw std::runtime_error(
            std::string("Error procession operation ") + op->name() + ": " + e.what());
    }

    for (auto &arg : args)
    {
        arg->clean();
    }

    return value.get();
}

} // namespace aquila::interpreter