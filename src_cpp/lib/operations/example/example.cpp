#include "example.hpp"

namespace aquila::ops
{

// REGISTER(ExampleOp);
ValuePtr ExampleOp::run(const std::string &param) const
{
    /**
     * operation implementation
     */
    throw std::logic_error(std::string("not implemented: ") + name());
    return nullptr;
}

std::optional<ArgManifest> ExampleOp::arg_manifest() const
{
    return ArgManifest{ArgSpec{.name = "param"}};
}

} // namespace aquila::ops