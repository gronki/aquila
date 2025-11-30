#pragma once

#include <cstdint>
#include <string>

namespace aquila
{

using Int = std::int64_t;
#ifdef SINGLE
using Real = float;
#else
using Real = double;
#endif
using String = std::string;

} // namespace aquila