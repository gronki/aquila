#pragma once

#include <types.hpp>
#include <buffer.hpp>

namespace aquila {

Buffer<Real> read_fits(const String& filename);
void write_fits(const String& filename, const View<Real>& img);

};
