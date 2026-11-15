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
ValuePtr AsinhOp::run(
    const Value &x, const Real &factor, const Real &mix, const Real &black) const
{
    real_buf_t denom = std::asinh(factor);
    real_buf_t factor_f = factor;
    real_buf_t mix_f = mix;
    real_buf_t black_f = black;
    return apply_unitary(x,
        [denom, factor_f, mix_f, black_f](auto xi) -> auto
        {
            auto rescaled = (xi - black_f) / (1 - black_f);
            return mix_f * std::asinh(rescaled * factor_f) / denom + (1 - mix_f) * xi;
        });
}

} // namespace aquila::ops
