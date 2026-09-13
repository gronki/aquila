#include "stack_op.hpp"
#include <operation.hpp>
#include <values/frame.hpp>

namespace aquila::ops
{

REGISTER(StackOp);
ValuePtr StackOp::run(Ptr<SequenceValue> buffers, const std::string &method) const
{
    if (buffers->size() == 0)
        return nullptr;

    std::vector<const_buffer_descriptor_r32_t> inputs;
    std::int64_t out_cols = 0, out_rows = 0;

    for (const auto &item : buffers->items)
    {
        const auto *buf_value = value_cast<values::BufferValue>(item.get());
        if (!buf_value)
            throw std::runtime_error("stack: expected buffer in sequence");
        out_cols = buf_value->buffer.cols();
        out_rows = buf_value->buffer.rows();
        inputs.push_back(c_const_buf(buf_value->buffer));
    }

    Buffer<real_buf_t> frame_out(out_cols, out_rows);
    error_status_t err;
    stack_frames(inputs.data(), inputs.size(), method.c_str(), c_buf(frame_out), &err);

    if (err.status == AQ_STATUS_OK)
        return std::make_unique<values::BufferValue>(std::move(frame_out));

    throw std::runtime_error(std::string(err.message));
}

const ArgManifest &StackOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "buffers", .sequence = true, .help = "buffers to stack"},
        ArgSpec{.name = "method", .default_str = "average"},
    };
    return manifest;
}

} // namespace aquila::ops
