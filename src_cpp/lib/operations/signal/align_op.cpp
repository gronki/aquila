#include "align_op.hpp"

namespace aquila::ops
{

REGISTER(AlignOp);
ValuePtr AlignOp::run(const values::SourceListValue &lst0,
    const values::SourceListValue &lst,
    const std::string &method,
    const std::string &prealign) const
{

    transform_t trans;
    int err;
    if (prealign != "yes" && prealign != "no")
        throw std::runtime_error("prealign must be: yes or no");
    classic_align(lst0.sources.data(),
        lst0.sources.size(),
        lst.sources.data(),
        lst.sources.size(),
        method.c_str(),
        {
            .scale = (double(lst0.nx) + double(lst0.ny)) / 3,
            .prealign_polygon = (prealign == "yes"),
        },
        trans,
        err);
    if (err)
        throw std::runtime_error("error finding transformation");
    return std::make_unique<values::TransformValue>(trans);
}

} // namespace aquila::ops