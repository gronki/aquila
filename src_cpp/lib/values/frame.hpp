#pragma once

#include "../../interpreter/value.hpp"
#include "../buffer/buffer.hpp"
#include "../io/fits.hpp"

namespace aquila::values
{

struct FrameInfo
{
    std::string fn_origin;
};

struct BufferValue : public ValueBase<BufferValue>
{
    TYPE_NAME("frame");

    Buffer<real_buf_t> buffer;
    FrameInfo info;

    BufferValue(Buffer<real_buf_t> v) : buffer(std::move(v)) {}
    BufferValue(Buffer<real_buf_t> v, FrameInfo info) :
        buffer(std::move(v)), info(std::move(info))
    {
    }
    BufferValue(const BufferValue &other) : buffer(other.buffer), info(other.info)
    {
        trace = other.trace;
    }
    void write(std::ostream &os) const
    {
        os << "(frame ";
        if (info.fn_origin != "")
            os << info.fn_origin << " ";
        os << buffer.cols() << "x" << buffer.rows() << ")";
    }
    int64_t mem_size() const override { return sizeof(real_buf_t) * buffer.size(); }
};

} // namespace aquila::values
