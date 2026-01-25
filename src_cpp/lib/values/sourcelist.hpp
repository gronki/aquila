#pragma once

#include "../../../src/c_binding/aquila.h"
#include "../buffer/buffer.hpp"
#include "../../interpreter/value.hpp"

namespace aquila::values
{

struct SourceListValue : public interpreter::CompoundValue
{
    std::vector<source_t> sources;
    std::int64_t nx, ny;
    SourceListValue(std::vector<source_t> sources, std::int64_t nx, std::int64_t ny) :
        sources(std::move(sources)), nx(nx), ny(ny)
    {
    }
    std::unique_ptr<Value> clone() const
    {
        return std::make_unique<SourceListValue>(sources, nx, ny);
    }
    void write(std::ostream &os) const
    {
        os << "(source list " << sources.size() << " items on image " << nx << "x" << ny
           << ")";
    }
};

}; // namespace aquila::values
