#pragma once

#include <map>

#include "value.hpp"

namespace aquila::interpreter
{

struct CacheEntry
{
    ValuePtr val;
    int64_t score = 0;
    int64_t mem_size = 0;
};

// how much memory the cache is allowed to hold, in bytes
static constexpr double CACHE_MEM_LIMIT = 8e9;

class CacheSpace
{
    std::map<std::string, CacheEntry> vault;

public:
    ValuePtr push(const value_trace_t &, ValuePtr, int64_t initial_score = 0);
    ValuePtr push(const ValuePtr &, int64_t initial_score = 0);
    ValuePtr get(const value_trace_t &) const;
    double total_mem_size() const;
    void prune();
    ValuePtr get_and_score(const value_trace_t &, int64_t score);
};

} // namespace aquila::interpreter
