#pragma once

#include <map>

#include "value.hpp"

namespace aquila::interpreter
{

struct CacheEntry
{
    ValuePtr val;
    double score = 0;
    int64_t mem_size = 0;
};

// how much memory the cache is allowed to hold, in bytes
static constexpr double CACHE_MEM_LIMIT = 8e9;
// score charged to every entry on each pruning, and per byte it occupies
static constexpr double CACHE_AGE_PENALTY = 0.1;
static constexpr double CACHE_SIZE_PENALTY = 1 / 1e9;

class CacheSpace
{
    std::map<std::string, CacheEntry> vault;

public:
    ValuePtr push(const value_trace_t &, ValuePtr, double initial_score = 0);
    ValuePtr push(const ValuePtr &, double initial_score = 0);
    ValuePtr get(const value_trace_t &) const;
    double total_mem_size() const;
    void prune();
    ValuePtr get_and_score(const value_trace_t &, double score);
};

} // namespace aquila::interpreter
