#include "align_op.hpp"

namespace aquila::ops
{

REGISTER(AlignOp);
ValuePtr AlignOp::run(const values::SourceListValue &lst,
    const values::SourceListValue &lst_ref,
    const std::string &method,
    const std::string &prealign) const
{

    transform_t trans;
    if (prealign != "yes" && prealign != "no")
        throw std::runtime_error("prealign must be: yes or no");

    align_params_t params = default_align_params();
    params.scale = (double(lst_ref.nx) + double(lst_ref.ny)) / 3;
    params.prealign_polygon = (prealign == "yes");
    error_status_t err;

    classic_align(lst_ref.sources.data(),
        lst_ref.sources.size(),
        lst.sources.data(),
        lst.sources.size(),
        method.c_str(),
        &params,
        &trans,
        &err);
    if (err.status)
        throw std::runtime_error(
            std::string("error finding transformation: ") + err.message);
    return std::make_unique<values::TransformValue>(trans);
}

const ArgManifest &AlignOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "stars"},
        ArgSpec{.name = "ref_stars", .help = "star list to align to"},
        ArgSpec{.name = "method",
            .default_str = "affine",
            .help = "options: polygon, xyr, affine"},
        ArgSpec{.name = "prealign",
            .default_str = "no",
            .help = "Prealign using polygons? Useful "
                    "for rotated photos. yes/no"},
    };
    return manifest;
}

} // namespace aquila::ops
