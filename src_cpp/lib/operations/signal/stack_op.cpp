#include "stack_op.hpp"
#include "../../values/frame.hpp"

namespace aquila::ops
{

REGISTER(StackOp);
ValuePtr StackOp::run(const SequenceValue &data, const std::string &method) const
{

    if (data.size() == 0)
        return nullptr;

    auto buf_vals = data.items_as<values::BufferValue>();

    std::vector<const_buffer_descriptor_t> inputs;
    std::int64_t out_cols = 0, out_rows = 0;
    
    for (const auto *buf_value : buf_vals)
    {
        out_cols = buf_value->buffer.cols();
        out_rows = buf_value->buffer.rows();
        inputs.push_back(c_const_buf(buf_value->buffer));
    }

    Buffer<real_buf_t> frame_out(out_cols, out_rows);
    stack_frames(inputs.data(), inputs.size(), method.c_str(), c_buf(frame_out));
    return std::make_unique<values::BufferValue>(std::move(frame_out));
}

ArgManifest StackOp::arg_manifest() const
{
    return ArgManifest{
        ArgSpec{.name = "data", .sequence = true},
        ArgSpec{.name = "method", .default_str = "average"},
    };
}

} // namespace aquila::ops