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

const ArgManifest &SequenceOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "...", .sequence = true},
    };
    return manifest;
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

const ArgManifest &ItemOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "seq", .sequence = true},
        ArgSpec{.name = "ix"},
    };
    return manifest;
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

const ArgManifest &ZipOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "...", .sequence = false},
    };
    return manifest;
}

} // namespace aquila::interpreter::ops
