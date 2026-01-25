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

    align_params_t params = default_align_params();
    params.scale = (double(lst0.nx) + double(lst0.ny)) / 3;
    params.prealign_polygon = (prealign == "yes");

    classic_align(lst0.sources.data(),
        lst0.sources.size(),
        lst.sources.data(),
        lst.sources.size(),
        method.c_str(),
        &params,
        &trans,
        &err);
    if (err)
        throw std::runtime_error("error finding transformation");
    return std::make_unique<values::TransformValue>(trans);
}

ArgManifest AlignOp::arg_manifest() const
{
    return ArgManifest{
        ArgSpec{.name = "list0"},
        ArgSpec{.name = "list"},
        ArgSpec{.name = "method",
            .default_str = "affine",
            .help = "options: polygon, xyr, affine"},
        ArgSpec{.name = "prealign",
            .default_str = "no",
            .help = "Prealign using polygons? Useful "
                    "for rotated photos. yes/no"},
    };
}

} // namespace aquila::ops