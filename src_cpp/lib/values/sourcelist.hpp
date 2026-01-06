#pragma once

#include "../../../src/c_binding/aquila.hpp"
#include "../buffer/buffer.hpp"
#include "../interpreter/value.hpp"

namespace aquila::values
{

struct SourceListValue : public interpreter::CompoundValue
{
    std::vector<source_t> sources;
    SourceListValue(std::vector<source_t> sources) : sources(std::move(sources)) {}
    std::unique_ptr<Value> clone() const
    {
        return std::make_unique<SourceListValue>(sources);
    }
    void write(std::ostream &os) const
    {
        os << "(source list " << sources.size() << " items)";
    }
};

}; // namespace aquila::values
