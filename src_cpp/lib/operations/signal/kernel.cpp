#include "kernel.hpp"
#include "../../../../src/c_binding/aquila.hpp"
#include "../../buffer/buffer.hpp"
#include "../../values/frame.hpp"

#include <cmath>

namespace aquila::ops
{

REGISTER(KernelOp);
ValuePtr KernelOp::run(const Real &fwhm, const String &type) const
{
    Int pixels = get_kernel_size(fwhm);
    Buffer<Real> krn(pixels, pixels);

    if (type == "gauss")
    {
        gausskrn(fwhm, krn);
    }
    else if (type == "mexha")
    {
        mexhakrn(fwhm, krn);
    }
    else
    {
        throw std::runtime_error(std::string("unknown kernel type: ") + type);
    }

    return std::make_unique<values::BufferValue>(std::move(krn));
}

} // namespace aquila::ops