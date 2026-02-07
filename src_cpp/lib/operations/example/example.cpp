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

ArgManifest ExampleOp::arg_manifest() const
{
    return ArgManifest{
        ArgSpec{.name = "param"},
    };
}

std::string ExampleOp::description() const
{
    return "";
}

} // namespace aquila::ops