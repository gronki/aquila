#include <algorithm>
#include <exception>
#include <thread>

#include "execution.hpp"
#include "operation.hpp"
#include "type_converter.hpp"
#include "value.hpp"

namespace aquila::interpreter
{

struct dummy_value_t : public ValueBase<dummy_value_t>
{
    /** Dummy value type, mostly used to hold only value trace */
    TYPE_NAME("dummy");
    dummy_value_t() {}
    void write(std::ostream &os) const override {}
};

void ExecNode::setup(ExecCtx ctx)
{
    Namespace dummy_ns(&ctx.ns);
    compute_trace({dummy_ns, nullptr});
}

value_trace_t RefNode::trace(ExecCtx ctx) const
{
    if (!ctx.ns.contains(refname))
        throw std::runtime_error("variable name not found: " + refname);
    return ctx.ns.get(refname).get_trace();
}

bool RefNode::trivial(ExecCtx ctx) const
{
    return ctx.ns.contains(refname)
        && !value_cast<const dummy_value_t>(ctx.ns.get(refname).get());
}

OpNode::OpNode(std::unique_ptr<Operation> op,
    std::vector<std::unique_ptr<ExecNode>> args,
    std::vector<std::string> keys) :
    op(std::move(op)), args(std::move(args)), manifest(this->op->arg_manifest()),
    match(match_arguments(manifest, keys))
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
            argvec[iarg] = seq->items[iseq];
        }
        else
        {
            argvec[iarg] = args[iarg];
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

        sequences[iarg] = arg;
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
        traces.push_back(arg.get_trace());
    }
    return traces;
}

static std::vector<value_trace_t> compute_traces(
    const std::vector<std::unique_ptr<ExecNode>> &args, ExecCtx ctx)
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
        traces.push_back(arg->compute_trace(ctx));
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
    if (trace_only && op.tracing_mode() != Operation::Tracing::FROM_RETVAL)
        return Ptr<dummy_value_t>::make().with_trace(trace);
    auto result = op_call_with_debug(op, std::move(args));
    if (op.tracing_mode() == Operation::Tracing::FROM_RETVAL)
    {
        trace = op.custom_trace(nullptr, result.get());
    }
    return result.with_trace(trace);
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
        args[iarg] = run_sanitizer(args[iarg], match[iarg].convert);
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
            arg_traces[iarg] = sequences[iarg].get_trace();
        }
        else if (args[iarg])
        {
            arg_traces[iarg] = args[iarg].get_trace();
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

    if (parallel && op.allow_parallel())
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
        return Ptr<SequenceValue>::make(std::move(result))
            .with_trace(default_op_trace(op, arg_traces));
    }

    return Ptr<SequenceValue>::make(std::move(result));
}

Ptr<Value> OpNode::yield(ExecCtx ctx) const
{
    if (ctx.cache && op->cacheable() && !get_trace().is_corrupt())
    {
#ifndef NDEBUG
        std::cout << "lookup trace: " << get_trace() << std::endl;
#endif
        auto maybe_cached = ctx.cache->get_and_score(get_trace(), 1);
        if (maybe_cached)
        {
#ifndef NDEBUG
            std::cout << " --> retrieved from cache! " << std::endl;
#else
            std::cout << "used cached result: " << op->name() << std::endl;
#endif
            trigger_sideeffects(ctx);
            return maybe_cached;
        }
#ifndef NDEBUG
        std::cout << " --> cache miss" << std::endl;
#endif
    }

    std::vector<Ptr<Value>> arg_results;
    for (size_t iarg = 0; iarg < args.size(); iarg++)
    {
        arg_results.push_back(args[iarg]->yield(ctx));
    }
    try
    {
        std::cout << "Executing: " << op->name() << std::endl;
        auto result = op_call_with_sequencing(
            *op, build_ptrs_from_match(arg_results, match), match, false, true);
        if (ctx.cache && result && op->cacheable())
        {
            auto result_trace = result.get_trace();
            if (!result_trace.is_corrupt())
            {
#ifndef NDEBUG
                std::cout << "writing to cache: " << result_trace << std::endl;
#endif
                ctx.cache->push(result_trace, result);
            }
        }
        return result;
    }
    catch (const std::runtime_error &e)
    {
        throw std::runtime_error(
            std::string("Error in operation ") + op->name() + ": " + e.what());
    }
}

value_trace_t OpNode::trace(ExecCtx ctx) const
{

    auto traces = compute_traces(args, ctx);

    if (op->tracing_mode() == Operation::Tracing::UNTRACEABLE)
        return value_trace_t::corrupt();

    if (op->tracing_mode() == Operation::Tracing::DEFAULT)
    {
        for (const auto &trace : traces)
        {
            if (trace.is_corrupt())
            {
#ifndef NDEBUG
                std::cout << "op: " << op->name() << " : " << "child trace is corrupt"
                          << std::endl;
#endif
                return value_trace_t::corrupt();
            }
        }
        return default_op_trace(*op, build_traces_from_match(traces, match));
    }

    for (const auto &arg : args)
    {
        if (!arg->trivial(ctx))
        {
#ifndef NDEBUG
            std::cout << "op: " << op->name() << " : " << "child arg is non-trivial"
                      << std::endl;
#endif
            return value_trace_t::corrupt();
        }
    }

    std::vector<Ptr<Value>> arg_results;
    for (size_t iarg = 0; iarg < args.size(); iarg++)
    {
        arg_results.push_back(args[iarg]->yield(ctx));
    }

    auto result = op_call_with_sequencing(*op,
        build_ptrs_from_match(arg_results, match),
        match,
        op->tracing_mode() == Operation::Tracing::FROM_INPUTS,
        false);

    if (!result)
        throw std::runtime_error("Error in evaluating trace for operation " + op->name());

    return result.get_trace();
}

bool OpNode::trivial(ExecCtx ctx) const
{
    if (op->tracing_mode() != Operation::Tracing::FROM_RETVAL)
        return false;
    for (const auto &arg : args)
        if (!arg->trivial(ctx))
            return false;
    return true;
}

void OpNode::trigger_sideeffects(ExecCtx ctx) const
{
    for (const auto &arg : args)
        arg->trigger_sideeffects(ctx);
}

Ptr<Value> AssignmentNode::yield(ExecCtx ctx) const
{
    Ptr<Value> rhs_yield = rhs->yield(ctx);
    if (!rhs_yield)
        return {};
    return ctx.ns.push(lhs, rhs_yield);
}

value_trace_t InlineAssignmentNode::trace(ExecCtx ctx) const
{
    /**
        Inline assignments are tricky to track, since they change the traces of a symbol
        mid-execution. This is why we need to inject traces of values assigned inline
        into a dummy namespace in trace evaluation pass so that they resolve correctly.

        Example how to break the system:

            a = 1
            2 | add 1
            2 | as a | add a

        (right result = 4, wrong result = 3)
    */

    auto arg_trace = arg->compute_trace(ctx);
    if (trivial(ctx))
    {
        yield(ctx);
        return arg_trace;
    }
    if (idents.size() == 1)
    {
        ctx.ns.push(idents[0], Ptr<dummy_value_t>::make().with_trace(arg_trace));
#ifndef NDEBUG
        std::cout << " as " << idents[0] << " trace ::: " << arg_trace << std::endl;
#endif
    }
    else
    {
        int idx = 0;
        for (const auto &ident : idents)
        {
            auto item_trace =
                "item{" + arg_trace.flatten() + "; " + std::to_string(idx + 1) + "}";
#ifndef NDEBUG
            std::cout << " as " << ident << " trace ::: " << item_trace << std::endl;
#endif
            ctx.ns.push(ident, Ptr<dummy_value_t>::make().with_trace(item_trace));
            idx++;
        }
    }
    return arg_trace;
}

void InlineAssignmentNode::trigger_sideeffects(ExecCtx ctx) const
{
    yield(ctx);
}

Ptr<Value> InlineAssignmentNode::yield(ExecCtx ctx) const
{

    Ptr<Value> in = arg->yield(ctx);

    if (!in)
        throw std::runtime_error("empty value may not be assigned");

    if (idents.size() == 1)
    {
        std::cout << "Pushing: " << idents[0] << std::endl;
        return ctx.ns.push(idents[0], in);
    }

    auto seq_trace = in.get_trace();
    const SequenceValue &sqv = value_cast<const SequenceValue>(*in);
    if (sqv.size() != idents.size())
        throw std::runtime_error("expected sequence of length "
            + std::to_string(idents.size()) + ", got: " + std::to_string(sqv.size()));
    auto sq_ret = std::make_unique<SequenceValue>();
    for (std::size_t iarg = 0; iarg < idents.size(); iarg++)
    {
        auto peeled = sqv.items[iarg];
        auto item_trace =
            "item{" + seq_trace.flatten() + "; " + std::to_string(iarg + 1) + "}";
        sq_ret->items.push_back(ctx.ns.push(idents[iarg], peeled.with_trace(item_trace)));
    }

    return Ptr<SequenceValue>(std::move(sq_ret)).with_trace(seq_trace);
}

} // namespace aquila::interpreter
