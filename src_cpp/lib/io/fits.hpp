#pragma once

#include "../../global/types.hpp"
#include "../../lib/buffer/buffer.hpp"

namespace aquila
{

Buffer<Real> read_fits(const String &filename);
void write_fits(const String &filename, const View<Real> &img);

}; // namespace aquila
