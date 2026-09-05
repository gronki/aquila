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

const ArgManifest &ExampleOp::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "param"},
    };
    return manifest;
}

std::string ExampleOp::description() const
{
    return "";
}

} // namespace aquila::ops