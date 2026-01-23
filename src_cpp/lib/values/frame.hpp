#pragma once

#include "../buffer/buffer.hpp"
#include "../interpreter/value.hpp"
#include "../io/fits.hpp"

namespace aquila::values
{

struct BufferValue : public interpreter::CompoundValue,
                     public interpreter::IFromFile<BufferValue>
{
    Buffer<real_buf_t> buffer;
    BufferValue(Buffer<real_buf_t> v) : buffer(std::move(v)) {}
    std::unique_ptr<Value> clone() const
    {
        return std::make_unique<BufferValue>(buffer);
    }
    void write(std::ostream &os) const
    {
        os << "(frame " << buffer.cols() << "x" << buffer.rows() << ")";
    }
    static BufferValue from_file(const std::string &fn) { return {read_fits(fn)}; }
};

}; // namespace aquila::values

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