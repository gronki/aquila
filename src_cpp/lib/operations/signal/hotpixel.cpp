#include "hotpixel.hpp"
#include <aquila.h>
#include <memory>

namespace aquila::ops
{

REGISTER(HotFindOp);
ValuePtr HotFindOp::run(const values::BufferValue &im, const Real &sigma) const
{
    Buffer<real_buf_t> result(im.buffer.cols(), im.buffer.rows());
    find_hot(c_const_buf(im.buffer), sigma, c_buf(result));
    return std::make_unique<values::BufferValue>(std::move(result), im.info);
}

const ArgManifest &HotFindOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "im"},
        ArgSpec{.name = "sigma", .default_real = 3.},
    };
    return manifest;
}

std::string HotFindOp::description() const
{
    return "Find hotpixels in the image and return the mask showing their positions.";
}
REGISTER(HotFixOp);
ValuePtr HotFixOp::run(const values::BufferValue &im, const values::BufferValue &mask) const
{
    Buffer<real_buf_t> result(im.buffer);
    fix_hot(c_buf(result), c_const_buf(mask.buffer));
    return std::make_unique<values::BufferValue>(std::move(result), im.info);
}

const ArgManifest &HotFixOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "im"},
        ArgSpec{.name = "mask"},
    };
    return manifest;
}

std::string HotFixOp::description() const
{
    return "Fix hotpixels found by hot_find().";
}

REGISTER(HotFixLightOp);
ValuePtr HotFixLightOp::run(const values::BufferValue &im, const Real &sigma) const
{
    Buffer<real_buf_t> result(im.buffer.cols(), im.buffer.rows());
    fix_hot_light(c_const_buf(im.buffer), sigma, c_buf(result));
    return std::make_unique<values::BufferValue>(std::move(result), im.info);
}

const ArgManifest &HotFixLightOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "im"},
        ArgSpec{.name = "sigma", .default_real = 3.},
    };
    return manifest;
}

std::string HotFixLightOp::description() const
{
    return "Find hotpixels in the image when dark was not available.";
}

} // namespace aquila::ops
