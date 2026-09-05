#include "project.hpp"
#include <aquila.h>
#include <buffer/buffer.hpp>

namespace aquila::ops
{

REGISTER(ProjectOp);
ValuePtr ProjectOp::run(const values::TransformValue &trans,
    const values::BufferValue &bufval,
    const Real &resample) const
{

    const auto &buf = bufval.buffer;
    std::int64_t cols_out = buf.cols() * resample;
    std::int64_t rows_out = buf.rows() * resample;
    if (cols_out < 3 || rows_out < 3)
        throw std::runtime_error("invalid output size");
    Buffer<real_buf_t> buf_out(cols_out, rows_out, 0.);

    error_status_t err;
    project_bilinear(&trans.transform, c_const_buf(buf), c_buf(buf_out), resample, &err);
    if (err.status)
        throw std::runtime_error(std::string(err.message));
    return std::make_unique<values::BufferValue>(buf_out, bufval.info);
}

const ArgManifest &ProjectOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "transform"},
        ArgSpec{.name = "buffer"},
        ArgSpec{.name = "resample", .default_real = 1.0},
    };
    return manifest;
}

std::string ProjectOp::description() const
{
    return "Projects images based on alignment determined by register()";
}

} // namespace aquila::ops
