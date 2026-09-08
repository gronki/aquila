#include "findstar_op.hpp"
#include "../../values/sourcelist.hpp"
#include "aquila.h"

STRUCT_DEFAULT(findstar_params_t, default_findstar_params);

namespace aquila::ops
{

REGISTER(FindstarOp);
ValuePtr FindstarOp::call(std::vector<ValuePtr> args) const
{
    return bind_args_new(this, &FindstarOp::run, arg_manifest(), props, args);
}

ValuePtr FindstarOp::run(const values::BufferValue &frame,
    const std::int64_t &limit,
    interpreter::Struct<findstar_params_t> params) const
{
    std::vector<source_t> sources(limit);
    std::int64_t nstar;
    register_stars(c_const_buf(frame.buffer), sources.data(), limit, &params.data, &nstar);
    sources.resize(nstar);
    return std::make_unique<values::SourceListValue>(
        std::move(sources), frame.buffer.cols(), frame.buffer.rows());
}

const ArgManifest &FindstarOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "frame"},
        ArgSpec{.name = "limit", .default_int = 256},
        ArgSpec{.name = "blur_radius", .field = field(&findstar_params_t::blur_radius)},
        ArgSpec{.name = "margin", .field = field(&findstar_params_t::margin)},
        ArgSpec{.name = "max_rms", .field = field(&findstar_params_t::max_rms)},
        ArgSpec{.name = "reject_relative",
            .field = field(&findstar_params_t::reject_relative)},
        ArgSpec{.name = "rslice", .field = field(&findstar_params_t::rslice)},
        ArgSpec{.name = "thresh_sd", .field = field(&findstar_params_t::thresh_sd)},
    };
    return manifest;
}

} // namespace aquila::ops
