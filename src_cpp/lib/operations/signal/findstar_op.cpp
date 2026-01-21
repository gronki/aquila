#include "findstar_op.hpp"
#include "../../values/sourcelist.hpp"
#include "aquila.h"

namespace aquila::ops
{

REGISTER(FindstarOp);
ValuePtr FindstarOp::run(const values::BufferValue &frame,
    const std::int64_t &limit,
    const double &blur_radius,
    const std::int64_t &margin,
    const double &max_rms,
    const std::string &rejection,
    const std::int64_t &rslice,
    const double &thresh_sd) const
{
    findstar_param_t params = default_findstar_param();
    params.blur_radius = blur_radius;
    params.margin = margin;
    params.max_rms = max_rms;
    params.rejection = rejection == "rel" ? FINDSTAR_REJECTION_RELATIVE
                                          : FINDSTAR_REJECTION_ABSOLUTE;
    params.rslice = rslice;
    params.thresh_sd = thresh_sd;
    std::vector<source_t> sources(limit);
    std::int64_t nstar;
    register_stars(c_const_buf(frame.buffer), sources.data(), limit, &params, &nstar);
    sources.resize(nstar);
    return std::make_unique<values::SourceListValue>(
        std::move(sources), frame.buffer.cols(), frame.buffer.rows());
}

} // namespace aquila::ops