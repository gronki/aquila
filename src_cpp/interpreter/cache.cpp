#include "cache.hpp"

namespace aquila::interpreter
{

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

ValuePtr CacheSpace::push(const ValuePtr &v, int64_t initial_score)
{
    const auto &trace = v.get_trace();
    if (trace.is_corrupt() || trace.empty() || !v)
        return {};
    auto [it, replaced] =
        vault.insert_or_assign(trace.trace(), CacheEntry{v, initial_score});
    return it->second.val;
}

ValuePtr CacheSpace::push(const value_trace_t &trace, ValuePtr v, int64_t initial_score)
{
    return push(v.with_trace(trace), initial_score);
}

ValuePtr CacheSpace::get(const value_trace_t &trace) const
{
    if (trace.is_corrupt() || trace.empty())
        return {};
    auto it = vault.find(trace.trace());
    if (it != vault.end())
        return it->second.val;
    return {};
}

ValuePtr CacheSpace::get_and_score(const value_trace_t &trace, int64_t score)
{
    if (trace.is_corrupt() || trace.empty())
        return {};
    auto it = vault.find(trace.trace());
    if (it != vault.end())
    {
        it->second.score += score;
        return it->second.val;
    }
    return {};
}
} // namespace aquila::interpreter
