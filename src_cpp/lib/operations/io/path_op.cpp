#include <vector>

#include "path_op.hpp"
#include <expand_path.hpp>

namespace aquila::ops
{

REGISTER(PathOp);
ValuePtr PathOp::run(const std::string &path) const
{
    auto wildcarded = utils::expand_path(path);
    if (wildcarded.size() == 1)
    {
        return std::make_unique<StrValue>(wildcarded[0]);
    }
    std::vector<std::unique_ptr<Value>> items;
    items.reserve(wildcarded.size());
    for (const auto &p : wildcarded)
    {
        items.push_back(std::make_unique<StrValue>(p));
    }
    return std::make_unique<interpreter::SequenceValue>(std::move(items));
}

REGISTER(ChdirOp);
ValuePtr ChdirOp::run(const std::string &path) const
{
    std::filesystem::current_path(path);
    return std::make_unique<StrValue>(std::filesystem::current_path());
}

REGISTER(PwdOp);
ValuePtr PwdOp::run() const
{
    return std::make_unique<StrValue>(std::filesystem::current_path());
}

} // namespace aquila::ops
