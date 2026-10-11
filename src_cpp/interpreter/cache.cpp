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

ValuePtr CacheSpace::push(const ValuePtr &v, double initial_score)
{
    const auto &trace = v.get_trace();
    if (trace.is_corrupt() || trace.empty() || !v)
        return {};
    auto [it, replaced] = vault.insert_or_assign(
        trace.trace(), CacheEntry{v, initial_score, v->mem_size()});
    return it->second.val;
}

double CacheSpace::total_mem_size() const
{
    double result = 0;
    for (const auto &it : vault)
    {
        result += it.second.mem_size;
    }
    return result;
}

/**
 * Entries with the lowest priority are evicted first. Every use of an entry
 * earns it a point of score, its size costs it a point per gigabyte, and each
 * pruning charges it CACHE_AGE_PENALTY -- so an entry that stops being used
 * sinks below the entries pushed after it, which start from zero.
 */
static double entry_priority(const CacheEntry &entry)
{
    return entry.score - entry.mem_size * CACHE_SIZE_PENALTY;
}

void CacheSpace::prune()
{
    for (auto &it : vault)
    {
        it.second.score -= CACHE_AGE_PENALTY;
    }

    double total = total_mem_size();

    while (total > CACHE_MEM_LIMIT && !vault.empty())
    {
        auto victim = vault.begin();
        for (auto it = vault.begin(); it != vault.end(); it++)
        {
            if (entry_priority(it->second) < entry_priority(victim->second))
                victim = it;
        }
#ifndef NDEBUG
        std::cout << "cache: dropping " << victim->first << std::endl;
#endif
        total -= victim->second.mem_size;
        vault.erase(victim);
    }
}

ValuePtr CacheSpace::push(const value_trace_t &trace, ValuePtr v, double initial_score)
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

ValuePtr CacheSpace::get_and_score(const value_trace_t &trace, double score)
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
