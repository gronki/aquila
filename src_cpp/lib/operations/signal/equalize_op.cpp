#include "equalize_op.hpp"
#include <aquila.h>
#include <values/frame.hpp>

STRUCT_DEFAULT(bkeq_param_t, default_bkeq_params);

namespace aquila::ops
{

REGISTER(EqualizeOp);

ValuePtr EqualizeOp::run(Ptr<SequenceValue> channels,
    const Str &what,
    interpreter::Struct<bkeq_param_t> params) const
{
    std::vector<std::unique_ptr<values::BufferValue>> out_bufs;
    std::vector<buffer_descriptor_r32_t> c_bufs;
    Int nx = -1, ny = -1;

    for (const auto &item : channels->items)
    {
        const auto *casted = value_cast<values::BufferValue>(item.get());
        if (!casted)
            throw std::runtime_error("wb: expected buffer in sequence");
        if (nx < 0 && ny < 0)
        {
            nx = casted->buffer.rows();
            ny = casted->buffer.cols();
        }
        else if (nx != casted->buffer.rows() || ny != casted->buffer.cols())
        {
            throw std::runtime_error("normalized buffer must be equal dimension");
        }
        out_bufs.push_back(std::make_unique<values::BufferValue>(
            Buffer<real_buf_t>(casted->buffer), casted->info));
        c_bufs.push_back(c_buf(out_bufs.back()->buffer));
    }

    params.data.background = what == "bg" || what == "both";
    params.data.stars = what == "stars" || what == "both";

    error_status_t err = {};
    equalize_background(c_bufs.data(), c_bufs.size(), &params.data, &err);

    if (err.status != 0)
    {
        throw std::runtime_error(std::string("equalize_background failed: ") + err.message);
    }

    std::vector<Ptr<Value>> return_bufs;
    for (auto &buf : out_bufs)
    {
        return_bufs.push_back(std::move(buf));
    }
    return Ptr<SequenceValue>::make(std::move(return_bufs));
}

const ArgManifest &EqualizeOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "channels", .sequence = true},
        ArgSpec{.name = "what", .default_str = "both", .help = "bg, stars or both"},
        ArgSpec{.name = "apar", .field = field(&bkeq_param_t::apar)},
        ArgSpec{.name = "bpar", .field = field(&bkeq_param_t::bpar)},
        ArgSpec{.name = "sigma", .field = field(&bkeq_param_t::sigma)},
        ArgSpec{.name = "sigma_star", .field = field(&bkeq_param_t::sigma_star)},
        ArgSpec{.name = "niter", .field = field(&bkeq_param_t::niter)},
        ArgSpec{.name = "margin", .field = field(&bkeq_param_t::margin)},
    };
    return manifest;
}

} // namespace aquila::ops
