#pragma once

#include "../buffer/buffer.hpp"
#include "../interpreter/value.hpp"

namespace aquila::values
{

struct BufferValue : public interpreter::CompoundValue
{
    Buffer<Real> buffer;
    BufferValue(Buffer<Real> v) : buffer(std::move(v)) {}
    std::unique_ptr<Value> clone() const
    {
        return std::make_unique<BufferValue>(buffer);
    }
    void write(std::ostream &os) const
    {
        os << "(frame " << buffer.cols() << "x" << buffer.rows() << ")";
    }
};

}; // namespace aquila::values
