#pragma once

#include "../../interpreter/value.hpp"
#include "../buffer/buffer.hpp"
#include "../io/fits.hpp"

namespace aquila::values
{

struct BufferValue : public ValueBase<BufferValue>
{
    TYPE_NAME("frame");

    Buffer<real_buf_t> buffer;
    BufferValue(Buffer<real_buf_t> v) : buffer(std::move(v)) {}
    BufferValue(const BufferValue &other) : buffer(other.buffer) {}
    void write(std::ostream &os) const
    {
        os << "(frame " << buffer.cols() << "x" << buffer.rows() << ")";
    }
};

}; // namespace aquila::values

namespace aquila::convert
{

inline std::unique_ptr<Value> loadFrame(const StrValue &s)
{
    return std::make_unique<values::BufferValue>(read_fits(s.value));
}

} // namespace aquila::convert

namespace aquila
{

inline buffer_descriptor_t c_buf(Buffer<real_buf_t> &buf)
{
    return {buf.data(), buf.rows(), buf.cols()};
}

inline const_buffer_descriptor_t c_const_buf(const Buffer<real_buf_t> &buf)
{
    return {buf.data(), buf.rows(), buf.cols()};
}

}; // namespace aquila
