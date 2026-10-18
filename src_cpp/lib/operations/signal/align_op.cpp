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

    align_params_t params = default_align_params();
    params.scale = (double(lst_ref.nx) + double(lst_ref.ny)) / 3;
    if (prealign == "polygon")
    {
        params.prealign = AQ_PREALIGN_POLYGON;
    }
    else if (prealign == "flip")
    {
        params.prealign = AQ_PREALIGN_FLIP;
    }
    else if (prealign == "none")
    {
        params.prealign = AQ_PREALIGN_NONE;
    }
    else
    {
        throw std::runtime_error("Prealign must be: none, flip, polygon");
    }
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
            .default_str = "flip",
            .help = "Prealign using polygons? Useful "
                    "for rotated photos. yes/no"},
    };
    return manifest;
}

} // namespace aquila::ops
