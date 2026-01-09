#pragma once

#include "../../../src/c_binding/aquila.h"
#include "../interpreter/value.hpp"

namespace aquila::values
{

struct TransformValue : public interpreter::CompoundValue
{
    transform_t transform;
    TransformValue(const transform_t &transform) : transform(transform) {}
    std::unique_ptr<Value> clone() const
    {
        return std::make_unique<TransformValue>(transform);
    }
    void write(std::ostream &os) const
    {
        os << "transform[" << std::string(transform.type, 7) << "; r= " << transform.scale << "; v= ";
        for (size_t i = 0; i < std::max(transform.npar, TRANSFORM_MAX_PAR); i++)
        {
            if (i > 0)
                os << ", ";
            os << transform.vec[i];
        }
        os << "]";
    }
};

}; // namespace aquila::values
