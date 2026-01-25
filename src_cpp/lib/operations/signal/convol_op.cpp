#include "convol_op.hpp"

namespace aquila::ops
{

REGISTER(ConvolOp);
ValuePtr ConvolOp::run(const values::BufferValue &buf,
    const values::BufferValue &krn,
    const std::string &edges) const
{
    int err;
    Buffer<real_buf_t> result(buf.buffer.cols(), buf.buffer.rows());
    conv2d_smallkernel(c_const_buf(buf.buffer),
        c_const_buf(krn.buffer),
        edges.c_str(),
        c_buf(result),
        true,
        &err);
    if (err)
        throw std::runtime_error("Convolution failed.");
    return std::make_unique<values::BufferValue>(std::move(result));
}

std::optional<ArgManifest> ConvolOp::arg_manifest() const
{
    return ArgManifest{
        ArgSpec{.name = "buffer",
            .convert = guard<values::BufferValue, StrValue>(convert::loadFrame)},
        ArgSpec{.name = "kernel",
            .convert = guard<values::BufferValue, StrValue>(convert::loadFrame)},
        ArgSpec{.name = "edges", .default_str = "e", .help = "How to fix edges?"},
    };
}

} // namespace aquila::ops