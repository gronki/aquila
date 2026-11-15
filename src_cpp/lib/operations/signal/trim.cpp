#include "trim.hpp"
#include "values/frame.hpp"

namespace aquila::ops
{

REGISTER(TrimOp);
ValuePtr TrimOp::run(const values::BufferValue &in, const Real &pixels) const
{
    const Int margin = static_cast<Int>(pixels);
    if (margin != pixels || margin < 0)
        throw std::runtime_error("trim: pixels must be a non-negative whole number");

    const Int cols = in.buffer.cols();
    const Int rows = in.buffer.rows();
    if (2 * margin >= cols || 2 * margin >= rows)
        throw std::runtime_error("trim: cannot remove " + std::to_string(margin)
            + " pixels from each edge of a " + std::to_string(cols) + "x"
            + std::to_string(rows) + " image");

    return std::make_unique<values::BufferValue>(
        Buffer<real_buf_t>(in.buffer.view(margin, cols - margin, margin, rows - margin)),
        in.info);
}

const ArgManifest &TrimOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "frame"},
        ArgSpec{.name = "pixels", .help = "number of pixels removed from each edge"},
    };
    return manifest;
}

std::string TrimOp::description() const
{
    return "Cuts the given number of pixels from every edge of the image, "
           "so a WxH image becomes (W-2n)x(H-2n).";
}

} // namespace aquila::ops
