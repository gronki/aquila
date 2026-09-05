#pragma once

#include <operation.hpp>
#include <value.hpp>
#include <values/frame.hpp>

namespace aquila::ops
{

struct EqualizeOp : public Operation
{

    BIND_ARGS(&EqualizeOp::run);
    ValuePtr run(Ptr<SequenceValue> bufs,
        const Str &what,
        const Real &apar,
        const Real &bpar,
        const Real &sigma,
        const Real &sigma_star,
        const Int &niter,
        const Int &margin) const;

    std::string name() const override { return "wb"; }

    const ArgManifest &arg_manifest() const override;
};

} // namespace aquila::ops
