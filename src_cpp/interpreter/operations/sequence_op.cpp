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

REGISTER(ItemOp);
ValuePtr ItemOp::run(Ptr<SequenceValue> seq, Real ix) const
{
    int64_t idx = int64_t(ix) - 1;
    if (idx < 0 || size_t(idx) >= seq->items.size())
        throw std::runtime_error("Index out of bounds");
    if (!seq->items[idx])
        return {nullptr};
    if (auto ptr = seq.get_mut())
    {
        return std::move(ptr->items[idx]);
    }
    return seq->items[idx].get();
}

ArgManifest ItemOp::arg_manifest() const
{
    return ArgManifest{
        ArgSpec{.name = "seq", .sequence = true},
        ArgSpec{.name = "ix"},
    };
}

std::string ItemOp::description() const
{
    return "Extracts item from a sequence";
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
