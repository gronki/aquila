#include "sequence_op.hpp"

namespace aquila::interpreter::ops
{

std::unique_ptr<Value> SequenceOp::call(const std::vector<const Value *> &args) const
{
    std::vector<std::unique_ptr<Value>> items;
    items.resize(args.size());
    for (size_t i = 0; i < args.size(); i++)
    {
        items[i] = args[i]->clone();
    }
    return std::make_unique<SequenceValue>(std::move(items));
}

} // namespace aquila::interpreter::ops