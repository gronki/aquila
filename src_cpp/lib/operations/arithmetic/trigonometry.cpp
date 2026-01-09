#include <cmath>

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
ValuePtr AsinhOp::run(const Value &x) const
{
    return apply_unitary(x, [](auto xi) -> auto { return std::asinh(xi); });
}

} // namespace aquila::ops