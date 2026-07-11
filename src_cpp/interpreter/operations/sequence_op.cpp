#include "sequence_op.hpp"

namespace aquila::interpreter::ops
{

REGISTER(SequenceOp);
ValuePtr SequenceOp::call(std::vector<ValuePtr> args) const
{
    std::vector<ValuePtr> items;
    items.resize(args.size());
    for (size_t i = 0; i < args.size(); i++)
    {
        items[i] = std::move(args[i]);
    }
    return Ptr<SequenceValue>::make(std::move(items));
}

ArgManifest SequenceOp::arg_manifest() const
{
    return ArgManifest{
        ArgSpec{.name = "...", .sequence = true},
    };
}

REGISTER(ZipOp);
ValuePtr ZipOp::call(std::vector<ValuePtr> args) const
{
    std::vector<ValuePtr> items;
    items.resize(args.size());
    for (size_t i = 0; i < args.size(); i++)
    {
        items[i] = std::move(args[i]);
    }
    return Ptr<SequenceValue>::make(std::move(items));
}

ArgManifest ZipOp::arg_manifest() const
{
    return ArgManifest{
        ArgSpec{.name = "...", .sequence = false},
    };
}

} // namespace aquila::interpreter::ops
