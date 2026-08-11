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
    ExecNode(ns), op(std::move(op)), args(std::move(args)),
    manifest(this->op->arg_manifest()), props(manifest),
    match(match_arguments(manifest, props, keys))
{
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
            if (auto seq_mut = seq.get_mut())
            {
                argvec[iarg] = seq_mut->items[iseq].own();
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

static value_trace_t default_op_trace(
    const Operation &op, const std::vector<value_trace_t> &expanded)
{
    std::stringstream ss;
    ss << op.name() << "{";
    for (size_t iarg = 0; iarg < expanded.size(); iarg++)
    {
        ss << expanded[iarg];
        if (iarg + 1 < expanded.size())
            ss << "; ";
    }
    ss << "}";
    return ss.str();
}

static constexpr int64_t SEQUENCE_NOT_FOUND = -1;

static void pick_sequences(std::vector<ValuePtr> &args,
    std::vector<Ptr<SequenceValue>> &sequences,
    const std::vector<ArgMatch> &match,
    int64_t &sequence_len)
{

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
}

struct dummy_value_t : public ValueBase<dummy_value_t>
{
    TYPE_NAME("dummy");
    dummy_value_t(const value_trace_t &trace) { this->trace = trace; }
    dummy_value_t(const dummy_value_t &other) { this->trace = other.trace; }
    void write(std::ostream &os) const override {}
};

static std::vector<value_trace_t> collect_traces(const std::vector<ValuePtr> &args)
{
    std::vector<value_trace_t> traces;
    traces.reserve(args.size());
    for (const auto &arg : args)
    {
        if (!arg)
        {
            traces.push_back({});
            continue;
        }
        traces.push_back(arg->get_trace());
    }
    return traces;
}
static std::vector<value_trace_t> collect_traces(
    const std::vector<std::unique_ptr<ExecNode>> &args)
{
    std::vector<value_trace_t> traces;
    traces.reserve(args.size());
    for (const auto &arg : args)
    {
        if (!arg)
        {
            traces.push_back({});
            continue;
        }
        traces.push_back(arg->trace());
    }
    return traces;
}

static ValuePtr run_op_with_trace(
    const Operation &op, std::vector<ValuePtr> &args, bool trace_only)
{
    value_trace_t trace{value_trace_t::corrupt()};

    if (op.tracing_mode() == Operation::Tracing::FROM_INPUTS)
    {
        trace = op.custom_trace(&args, nullptr);
    }
    else if (op.tracing_mode() == Operation::Tracing::DEFAULT)
    {
        trace = default_op_trace(op, collect_traces(args));
    }
    if (trace_only)
        return Ptr<dummy_value_t>::make(trace);
    auto result = op_call_with_debug(op, std::move(args));
    if (op.tracing_mode() == Operation::Tracing::FROM_RETVAL)
    {
        trace = op.custom_trace(nullptr, result.get());
    }
    if (auto mut = result.get_mut())
    {
        mut->trace = trace;
    }
    return result;
}

static ValuePtr op_call_with_sequencing(const Operation &op,
    std::vector<ValuePtr> args,
    const std::vector<ArgMatch> &match,
    bool trace_only,
    bool parallel)
{

    if (args.size() == 0)
        return run_op_with_trace(op, args, trace_only);

    int64_t sequence_len = SEQUENCE_NOT_FOUND;
    std::vector<Ptr<SequenceValue>> sequences(args.size());

    pick_sequences(args, sequences, match, sequence_len);

    // sanitize non-sequence args
    for (size_t iarg = 0; iarg < args.size(); iarg++)
    {
        if (!args[iarg] || !match[iarg].convert)
            continue;
        args[iarg] = run_sanitizer(std::move(args[iarg]), match[iarg].convert);
    }

    // 2nd pass for pick sequences (post-sanitizer). this is for example
    // for operations which might expand filenames in preprocessing
    pick_sequences(args, sequences, match, sequence_len);

    if (sequence_len == SEQUENCE_NOT_FOUND)
    {
        return run_op_with_trace(op, args, trace_only);
    }

    if (sequence_len == 0)
        return Ptr<SequenceValue>::make(std::vector<ValuePtr>{});

    std::vector<ValuePtr> result(sequence_len);
    std::vector<value_trace_t> arg_traces(args.size());
    for (size_t iarg = 0; iarg < args.size(); iarg++)
    {
        if (sequences[iarg])
        {
            arg_traces[iarg] = sequences[iarg]->get_trace();
        }
        else if (args[iarg])
        {
            arg_traces[iarg] = args[iarg]->get_trace();
        }
    }

    auto process_seq_item = [&op, &args, &sequences, &match, trace_only](int64_t iseq) -> ValuePtr
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

        return op_call_with_sequencing(op, std::move(ith_vector), match, trace_only, false);
    };

#ifdef AQUILA_PARALLEL

    if (parallel)
    {

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
                            std::cout << " --- thread = " << ithread
                                      << " item = " << iseq << std::endl;
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
    }
    else
    {
#endif
        for (std::int64_t iseq = 0; iseq < sequence_len; iseq++)
        {
            result[iseq] = process_seq_item(iseq);
        }
#ifdef AQUILA_PARALLEL
    }
#endif

    if (op.tracing_mode() == Operation::Tracing::DEFAULT)
    {
        return Ptr<SequenceValue>::make(
            std::move(result), default_op_trace(op, arg_traces));
    }

    return Ptr<SequenceValue>::make(std::move(result));
}
static std::uint64_t fnv1a(std::string s)
{
    std::uint64_t h = 1469598103934665603ull;
    for (unsigned char c : s)
    {
        h ^= c;
        h *= 1099511628211ull;
    }
    return h;
}
Ptr<Value> OpNode::yield() const
{
    if (op->cacheable())
    {
        auto lookup_trace = trace();
        if (!lookup_trace.is_corrupt)
        {
            std::string ns_name =
                "__cache_" + std::to_string(fnv1a(lookup_trace.flatten()));
            if (ns.contains(ns_name))
            {
#ifndef NDEBUG
                std::cout << "retrieved from cache: " << ns_name
                          << " := " << lookup_trace << std::endl;
#endif
                return ns.get(ns_name);
            }
#ifndef NDEBUG
            std::cout << "cache miss: " << lookup_trace << std::endl;
#endif
        }
    }

    std::vector<Ptr<Value>> arg_results;
    for (size_t iarg = 0; iarg < args.size(); iarg++)
    {
        arg_results.push_back(args[iarg]->yield());
    }
    try
    {
        auto result = op_call_with_sequencing(
            *op, build_ptrs_from_match(arg_results, match), match, false, true);
        if (op->cacheable() && result->sequence_len() < 10)
        {
            std::string ns_name =
                "__cache_" + std::to_string(fnv1a(result->get_trace().flatten()));
#ifndef NDEBUG
            std::cout << "writing to cache: " << ns_name
                      << " := " << result->get_trace() << std::endl;
#endif
            ns.push(ns_name, result->clone());
        }
        return result;
    }
    catch (const std::runtime_error &e)
    {
        throw std::runtime_error(
            std::string("Error in operation ") + op->name() + ": " + e.what());
    }
}

value_trace_t OpNode::trace() const
{

    if (op->tracing_mode() == Operation::Tracing::UNTRACEABLE)
        return value_trace_t::corrupt();

    if (op->tracing_mode() == Operation::Tracing::DEFAULT)
    {
        auto traces = collect_traces(args);
        for (const auto &trace : traces)
        {
            if (trace.is_corrupt)
            {
                std::cout << "op: " << op->name() << " : " << "child trace is corrupt"
                          << std::endl;
                return value_trace_t::corrupt();
            }
        }
        return default_op_trace(*op, build_traces_from_match(traces, match));
    }

    for (const auto &arg : args)
    {
        if (!arg->trivial())
        {

            std::cout << "op: " << op->name() << " : " << "child arg is non-trivial"
                      << std::endl;
            return value_trace_t::corrupt();
        }
    }

    std::vector<Ptr<Value>> arg_results;
    for (size_t iarg = 0; iarg < args.size(); iarg++)
    {
        arg_results.push_back(args[iarg]->yield());
    }

    auto result = op_call_with_sequencing(*op,
        build_ptrs_from_match(arg_results, match),
        match,
        op->tracing_mode() == Operation::Tracing::FROM_INPUTS,
        false);

    if (!result)
        throw std::runtime_error("Error in evaluating trace for operation " + op->name());

    return result->get_trace();
}

Ptr<Value> AssignmentNode::yield() const
{
    Ptr<Value> rhs_yield = rhs->yield();
    if (!rhs_yield)
        return {};
    return ns.push(lhs, rhs_yield.own());
}

Ptr<Value> InlineAssignmentNode::yield() const
{

    Ptr<Value> in = arg->yield();

    if (!in)
        throw std::runtime_error("empty value may not be assigned");

    if (idents.size() == 1)
    {
        return ns.push(idents[0], in.own());
    }

    auto seq_trace = in->get_trace();
    auto owned = in.own();
    SequenceValue &sqv = value_cast<SequenceValue>(*owned);
    if (sqv.size() != idents.size())
        throw std::runtime_error("expected sequence of length "
            + std::to_string(idents.size()) + ", got: " + std::to_string(sqv.size()));
    auto sq_ret = std::make_unique<SequenceValue>();
    sq_ret->trace = seq_trace;
    for (std::size_t iarg = 0; iarg < idents.size(); iarg++)
    {
        auto peeled = sqv.items[iarg].own();
        peeled->trace = "item{" + seq_trace.content + "; " + std::to_string(iarg + 1) + "}";
        sq_ret->items.push_back(ns.push(idents[iarg], std::move(peeled)));
    }

    return sq_ret;
}

} // namespace aquila::interpreter
