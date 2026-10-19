#include "bkgradient_op.hpp"

STRUCT_DEFAULT(bkgradient_param_t, default_bkgradient_params);

namespace aquila::ops
{

REGISTER(BackgroundOp);
ValuePtr BackgroundOp::run(const values::BufferValue &frame,
    interpreter::Struct<bkgradient_param_t> params) const
{
    Buffer<real_buf_t> result(frame.buffer.cols(), frame.buffer.rows());

    error_status_t err = {};
    estimate_gradient(c_buf(frame.buffer), c_buf(result), &params.data, &err);

    if (err.status != 0)
        throw std::runtime_error(std::string("bg: ") + err.message);

    return std::make_unique<values::BufferValue>(std::move(result), frame.info);
}

const ArgManifest &BackgroundOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "frame"},
        ArgSpec{.name = "sigma",
            .field = field(&bkgradient_param_t::sd_sigma),
            .help = "sigma for rejection of background points"},
        ArgSpec{.name = "offset",
            .field = field(&bkgradient_param_t::offset),
            .help = "fraction of constant signal to mix in"},
        ArgSpec{.name = "gradient",
            .field = field(&bkgradient_param_t::gradient),
            .help = "fraction of gradient to mix in. =0 will only approximate uniform "
                    "background"},
        ArgSpec{.name = "shift",
            .field = field(&bkgradient_param_t::sigma_shift),
            .help =
                "compensate local background value with that many standard deviations. "
                "Tweak if you are unhappy with the background subtraction"},
        ArgSpec{.name = "points", .field = field(&bkgradient_param_t::n_points)},
        ArgSpec{.name = "box", .field = field(&bkgradient_param_t::window_size)},
        ArgSpec{.name = "margin", .field = field(&bkgradient_param_t::margin)},
        ArgSpec{.name = "seed", .field = field(&bkgradient_param_t::seed)},
        ArgSpec{.name = "draw", .field = field(&bkgradient_param_t::visualize)},
    };
    return manifest;
}

} // namespace aquila::ops
