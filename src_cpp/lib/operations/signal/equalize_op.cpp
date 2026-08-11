#include "equalize_op.hpp"
#include <aquila.h>
#include <values/frame.hpp>

namespace aquila::ops
{

REGISTER(EqualizeOp);

ValuePtr EqualizeOp::run(Ptr<SequenceValue> channels,
    const Str &what,
    const Real &apar,
    const Real &bpar,
    const Real &sigma,
    const Real &sigma_star,
    const Int &niter,
    const Int &margin) const
{
    std::vector<Ptr<values::BufferValue>> bufs;
    for (const auto &item : channels->items)
    {
        const auto *casted = value_cast<values::BufferValue>(item.get());
        if (!casted)
            throw std::runtime_error("wb: expected buffer in sequence");
        bufs.push_back(casted);
    }

    bkeq_param_t params;
    params.background = what == "bg" || what == "both";
    params.stars = what == "stars" || what == "both";
    params.apar = apar;
    params.bpar = bpar;
    params.sigma = sigma;
    params.sigma_star = sigma_star;
    params.niter = static_cast<int32_t>(niter);
    params.margin = static_cast<int32_t>(margin);

    std::vector<std::unique_ptr<values::BufferValue>> out_bufs;
    std::vector<buffer_descriptor_t> c_bufs;

    for (auto &buf : bufs)
    {
        out_bufs.push_back(buf.own());
        c_bufs.push_back(c_buf(out_bufs.back()->buffer));
    }

    error_status_t err = {};
    equalize_background(c_bufs.data(), c_bufs.size(), &params, &err);

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

ArgManifest EqualizeOp::arg_manifest() const
{
    return ArgManifest{
        ArgSpec{.name = "channels", .sequence = true},
        ArgSpec{.name = "what", .default_str = "both", .help = "bg, stars or both"},
        ArgSpec{.name = "apar", .default_real = 2.0},
        ArgSpec{.name = "bpar", .default_real = 0.5},
        ArgSpec{.name = "sigma", .default_real = 3.0},
        ArgSpec{.name = "sigma_star", .default_real = 4.0},
        ArgSpec{.name = "niter", .default_int = 32},
        ArgSpec{.name = "margin", .default_int = 32},
    };
}

} // namespace aquila::ops
