#include "kernel.hpp"
#include "../../../../src/c_binding/aquila.h"
#include "../../buffer/buffer.hpp"
#include "../../values/frame.hpp"

#include <cmath>

namespace aquila::ops
{

REGISTER(KernelOp);
ValuePtr KernelOp::run(const double &fwhm, const std::string &type) const
{
    std::int64_t pixels = get_kernel_size(fwhm);
    Buffer<real_buf_t> krn(pixels, pixels);

    if (type == "gauss")
    {
        gausskrn(fwhm, c_buf(krn));
    }
    else if (type == "mexha")
    {
        mexhakrn(fwhm, c_buf(krn));
    }
    else
    {
        throw std::runtime_error(std::string("unknown kernel type: ") + type);
    }

    return std::make_unique<values::BufferValue>(std::move(krn));
}

std::optional<ArgManifest> KernelOp::arg_manifest() const
{
    return ArgManifest{
        ArgSpec{.name = "fwhm"},
        ArgSpec{.name = "type", .default_str = "gauss", .help = "options: gauss, mexha"},
    };
}

} // namespace aquila::ops