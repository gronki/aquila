#include "findstar_op.hpp"
#include "../../values/sourcelist.hpp"
#include "aquila.hpp"

namespace aquila::ops
{

REGISTER(FindstarOp);
ValuePtr FindstarOp::run(const values::BufferValue &frame,
    const Int &limit,
    const Real &blur_radius,
    const Int &margin,
    const Real &max_rms,
    const String &rejection,
    const Int &rslice,
    const Real &thresh_sd) const
{
    findstar_param_t params;
    params.blur_radius = blur_radius;
    params.margin = margin;
    params.max_rms = max_rms;
    params.rejection = rejection == "rel" ? FINDSTAR_REJECTION_RELATIVE
                                          : FINDSTAR_REJECTION_ABSOLUTE;
    params.rslice = rslice;
    params.thresh_sd = thresh_sd;
    std::vector<source_t> sources(limit);
    Int nstar;
    register_stars(frame.buffer, sources.data(), limit, params, nstar);
    sources.resize(nstar);
    return std::make_unique<values::SourceListValue>(std::move(sources));
}

} // namespace aquila::ops