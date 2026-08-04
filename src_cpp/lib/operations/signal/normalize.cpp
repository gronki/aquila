#include "normalize.hpp"
#include "aquila.h"
#include "value.hpp"

namespace aquila::ops
{

REGISTER(NormalizeOp);
ValuePtr NormalizeOp::run(Ptr<SequenceValue> buffers, const Real &margin) const
{
    std::vector<ValuePtr> items;
    std::vector<const_buffer_descriptor_t> buf_inputs;
    std::vector<buffer_descriptor_t> buf_outputs;

    for (const auto &item : buffers->items)
    {
        const auto *ptr = value_cast<values::BufferValue>(item.get());
        if (!ptr)
            throw std::runtime_error("normalize: expected buffer in sequence");
        auto buf = std::make_unique<values::BufferValue>(
            Buffer<real_buf_t>(ptr->buffer.cols(), ptr->buffer.rows()), ptr->info);
        buf_inputs.push_back(c_const_buf(ptr->buffer));
        buf_outputs.push_back(c_buf(buf->buffer));
        items.push_back(std::move(buf));
    }
    error_status_t err;
    normalize_offset_gain(
        buf_inputs.data(), buf_outputs.data(), buf_inputs.size(), margin, &err);
    if (err.status)
        throw std::string(err.message);
    return Ptr<SequenceValue>::make(std::move(items));
}

ArgManifest NormalizeOp::arg_manifest() const
{
    return ArgManifest{
        ArgSpec{.name = "buffers", .sequence = true},
        ArgSpec{.name = "margin", .default_real = 80},
    };
}

std::string NormalizeOp::description() const
{
    return "Normalize frames intensity -- important before sigma stacking.";
}

} // namespace aquila::ops
