#pragma once

#include "../../../src/c_binding/aquila.h"
#include "../../interpreter/value.hpp"
#include "../buffer/buffer.hpp"

namespace aquila::values
{

struct SourceListValue : public ValueBase<SourceListValue>
{
    TYPE_NAME("starlist");

    std::vector<source_t> sources;
    std::int64_t nx, ny;
    SourceListValue(std::vector<source_t> sources, std::int64_t nx, std::int64_t ny) :
        sources(std::move(sources)), nx(nx), ny(ny)
    {
    }
    SourceListValue(const SourceListValue &other) :
        sources(other.sources), nx(other.nx), ny(other.ny)
    {
    }
    void write(std::ostream &os) const
    {
        os << "(source list " << sources.size() << " items on image " << nx << "x" << ny
           << ")";
    }
};

}; // namespace aquila::values
