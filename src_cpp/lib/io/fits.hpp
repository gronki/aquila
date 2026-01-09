#pragma once

#include "../../lib/buffer/buffer.hpp"

namespace aquila
{

Buffer<real_buf_t> read_fits(const std::string &filename);
void write_fits(const std::string &filename, const View<real_buf_t> &img);

}; // namespace aquila
