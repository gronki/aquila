#include "sequence_op.hpp"

namespace aquila::interpreter::ops
{

REGISTER(SequenceOp);
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

ArgManifest SequenceOp::arg_manifest() const
{
    return ArgManifest{
        ArgSpec{.name = "...", .sequence = true},
    };
}

REGISTER(ZipOp);
std::unique_ptr<Value> ZipOp::call(const std::vector<const Value *> &args) const
{
    std::vector<std::unique_ptr<Value>> items;
    items.resize(args.size());
    for (size_t i = 0; i < args.size(); i++)
    {
        items[i] = args[i]->clone();
    }
    return std::make_unique<SequenceValue>(std::move(items));
}

ArgManifest ZipOp::arg_manifest() const
{
    return ArgManifest{
        ArgSpec{.name = "...", .sequence = false},
    };
}

} // namespace aquila::interpreter::ops
