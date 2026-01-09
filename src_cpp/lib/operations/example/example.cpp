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

} // namespace aquila::ops