#include <vector>

#include "path_op.hpp"
#include <utils/expand_path.hpp>

namespace aquila::ops
{

REGISTER(PathOp);
ValuePtr PathOp::run(Ellipsis<const Str *> paths) const
{
    std::vector<Ptr<Value>> items;
    for (const auto &path : paths.data)
    {
        auto wildcarded = utils::expand_path(*path);
        for (const auto &p : wildcarded)
        {
            items.push_back(Ptr<StrValue>::make(p));
        }
    }
    if (items.size() == 1)
        return std::move(items[0]);

    return Ptr<interpreter::SequenceValue>::make(std::move(items));
}

REGISTER(ChdirOp);
ValuePtr ChdirOp::run(const std::string &path) const
{
    std::filesystem::current_path(path);
    return Ptr<StrValue>::make(std::filesystem::current_path());
}

REGISTER(PwdOp);
ValuePtr PwdOp::run() const
{
    return Ptr<StrValue>::make(std::filesystem::current_path());
}

} // namespace aquila::ops
