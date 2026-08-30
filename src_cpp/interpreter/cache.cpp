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

ValuePtr CacheSpace::push(
    const value_trace_t &trace, std::unique_ptr<Value> v, int64_t initial_score)
{
    if (trace.is_corrupt || trace.content.empty() || !v)
        return {};
    v->materialize();
    auto [it, replaced] =
        vault.insert_or_assign(trace.flatten(), CacheEntry{std::move(v), initial_score});
    return it->second.val->shallow();
}

ValuePtr CacheSpace::get(const value_trace_t &trace) const
{
    if (trace.is_corrupt || trace.content.empty())
        return {};
    auto it = vault.find(trace.flatten());
    if (it != vault.end())
        return it->second.val->shallow();
    return {};
}

ValuePtr CacheSpace::get_and_score(const value_trace_t &trace, int64_t score)
{
    if (trace.is_corrupt || trace.content.empty())
        return {};
    auto it = vault.find(trace.flatten());
    if (it != vault.end())
    {
        it->second.score += score;
        return it->second.val->shallow();
    }
    return {};
}
} // namespace aquila::interpreter
