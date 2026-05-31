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
        false,
        &err);
    if (err)
        throw std::runtime_error("Convolution failed.");
    return std::make_unique<values::BufferValue>(std::move(result), buf.info);
}

ArgManifest ConvolOp::arg_manifest() const
{
    return ArgManifest{
        ArgSpec{.name = "buffer", .convert = guard(convert::loadFrame)},
        ArgSpec{.name = "kernel", .convert = guard(convert::loadFrame)},
        ArgSpec{.name = "edges", .default_str = "e", .help = "How to fix edges?"},
    };
}

REGISTER(DeconvOp);

ValuePtr DeconvOp::run(const values::BufferValue &buf,
    const values::BufferValue &krn,
    const Real &strength,
    const Real &niter) const
{
    Buffer<real_buf_t> result(buf.buffer.cols(), buf.buffer.rows());
    deconvol_lr(c_const_buf(buf.buffer),
        c_const_buf(krn.buffer),
        strength,
        (int)niter,
        c_buf(result),
        false);
    return std::make_unique<values::BufferValue>(std::move(result), buf.info);
}

ArgManifest DeconvOp::arg_manifest() const
{
    return ArgManifest{
        ArgSpec{.name = "buffer", .convert = guard(convert::loadFrame)},
        ArgSpec{.name = "kernel", .convert = guard(convert::loadFrame)},
        ArgSpec{.name = "strength",
            .default_real = 0.5,
            .help = "How much of deconvolution apply each step?"},
        ArgSpec{.name = "niter", .default_real = 32, .help = "Number of iterations"},
    };
}

} // namespace aquila::ops
