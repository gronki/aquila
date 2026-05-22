#include <cmath>

#include "aquila.h"
#include "arithmetic_templates.hpp"
#include "trigonometry.hpp"

namespace aquila::ops
{

REGISTER(SinOp);
ValuePtr SinOp::run(const Value &x) const
{
    return apply_unitary(x, [](auto xi) -> auto { return std::sin(xi); });
}

REGISTER(CosOp);
ValuePtr CosOp::run(const Value &x) const
{
    return apply_unitary(x, [](auto xi) -> auto { return std::cos(xi); });
}

REGISTER(AsinhOp);
ValuePtr AsinhOp::run(const Value &x, const Real &factor) const
{
    real_buf_t denom = std::asinh(factor);
    real_buf_t factor_f = factor;
    return apply_unitary(x,
        [denom, factor_f](auto xi) -> auto { return std::asinh(xi * factor_f) / denom; });
}

} // namespace aquila::ops
