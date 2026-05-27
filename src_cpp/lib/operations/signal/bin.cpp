#include "bin.hpp"
#include "values/frame.hpp"

namespace aquila::ops
{

template <int64_t scale, typename T>
void bin_row(const T *__restrict in, T *__restrict dst, int64_t dst_len)
{
    for (int64_t isrc = 0, idst = 0; idst < dst_len; isrc += scale, idst++)
    {
        for (int64_t isub = 0; isub < scale; isub++)
        {
            dst[idst] += in[isrc + isub];
        }
    }
}

template <int64_t scale>
Buffer<real_buf_t> bin(const Buffer<real_buf_t> &in)
{
    Buffer<real_buf_t> newbuf(in.cols() / scale, in.rows() / scale, 0);
    const int64_t nvec = newbuf.nvec();
    for (int64_t ivec = 0, idst = 0; idst < newbuf.vecs(); ivec += scale, idst++)
    {
        for (int64_t isub = 0; isub < scale; isub++)
        {
            bin_row<scale>(in.vec(ivec + isub), newbuf.vec(idst), nvec);
        }
    }
    return std::move(newbuf);
}

REGISTER(BinOp);
ValuePtr BinOp::run(const values::BufferValue &in, const Real &scale) const
{
    int scale_i = (int)scale;
    switch (scale_i)
    {
    case 1:
        return std::make_unique<values::BufferValue>(in.buffer);
    case 2:
        return std::make_unique<values::BufferValue>(bin<2>(in.buffer));
    case 3:
        return std::make_unique<values::BufferValue>(bin<3>(in.buffer));
    case 4:
        return std::make_unique<values::BufferValue>(bin<4>(in.buffer));
    case 5:
        return std::make_unique<values::BufferValue>(bin<5>(in.buffer));
    case 6:
        return std::make_unique<values::BufferValue>(bin<6>(in.buffer));
    case 7:
        return std::make_unique<values::BufferValue>(bin<7>(in.buffer));
    case 8:
        return std::make_unique<values::BufferValue>(bin<8>(in.buffer));
    default:
        throw std::runtime_error("unsupported bin scale!");
    }
    return nullptr;
}

ArgManifest BinOp::arg_manifest() const
{
    return ArgManifest{
        ArgSpec{.name = "frame"},
        ArgSpec{.name = "scale"},
    };
}

std::string BinOp::description() const
{
    return "Bin down image with absolute scale from 2 to 8.";
}

} // namespace aquila::ops
