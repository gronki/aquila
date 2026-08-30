#pragma once

#include <map>

#include "value.hpp"

namespace aquila::interpreter
{

struct CacheEntry
{
    std::unique_ptr<Value> val;
    int64_t score = 0;
};

class CacheSpace
{
    std::map<std::string, CacheEntry> vault;

public:
    ValuePtr push(
        const value_trace_t &, std::unique_ptr<Value> v, int64_t initial_score = 0);
    ValuePtr get(const value_trace_t &) const;
    ValuePtr get_and_score(const value_trace_t &, int64_t score);
};

} // namespace aquila::interpreter
