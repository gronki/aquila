#pragma once

#include "../../buffer/buffer.hpp"
#include "../../interpreter/value.hpp"
#include "../../values/frame.hpp"

namespace aquila::ops
{

template <typename F, typename... Args>
ValuePtr apply_unitary(const Value &a, F op, Args &&...args)
{
    auto scalar_ptr = dynamic_cast<const RealValue *>(&a);

    if (scalar_ptr)
        return std::make_unique<RealValue>(op(scalar_ptr->value), args...);

    auto buffer_ptr = dynamic_cast<const values::BufferValue *>(&a);

    if (buffer_ptr)
        return std::make_unique<values::BufferValue>(
            apply(buffer_ptr->buffer.view(), op), args...);

    throw std::runtime_error(
        std::string("Value not suitable for arithmetic operation: ") + a.str());
}

template <typename F, typename... Args>
ValuePtr apply_binary(const Value &a, const Value &b, F op, Args &&...args)
{
    auto scalar_ptr_a = dynamic_cast<const RealValue *>(&a);
    auto buffer_ptr_a = dynamic_cast<const values::BufferValue *>(&a);
    auto scalar_ptr_b = dynamic_cast<const RealValue *>(&b);
    auto buffer_ptr_b = dynamic_cast<const values::BufferValue *>(&b);

    if (scalar_ptr_a && scalar_ptr_b)
        return std::make_unique<RealValue>(
            op(scalar_ptr_a->value, scalar_ptr_b->value), args...);

    if (buffer_ptr_a && buffer_ptr_b)
    {
        if (!buffer_ptr_a->buffer.matches_shape(buffer_ptr_b->buffer))
        {
            throw std::runtime_error(std::string("buffers do not match in dimensions: ")
                + a.str() + " and " + b.str());
        }
        return std::make_unique<values::BufferValue>(
            apply(buffer_ptr_a->buffer.view(), buffer_ptr_b->buffer.view(), op), args...);
    }

    if (scalar_ptr_a && buffer_ptr_b)
    {
        auto scalar = scalar_ptr_a->value;
        return std::make_unique<values::BufferValue>(
            apply(buffer_ptr_b->buffer.view(),
                [scalar, op](Real x) -> Real { return op(scalar, x); }),
            args...);
    }

    if (buffer_ptr_a && scalar_ptr_b)
    {
        auto scalar = scalar_ptr_b->value;
        return std::make_unique<values::BufferValue>(
            apply(buffer_ptr_a->buffer.view(),
                [scalar, op](Real x) -> Real { return op(x, scalar); }),
            args...);
    }

    throw std::runtime_error(
        std::string("Values not suitable for arithmetic operation: ") + a.str() + ", "
        + b.str());
}

/**
 * This monstrosity only exists to support mix() operation.
 */
template <typename F, typename... Args>
ValuePtr apply_tertiary(const Value &a, const Value &b, const Value &c, F op, Args &&...args)
{
    auto scalar_ptr_a = dynamic_cast<const RealValue *>(&a);
    auto buffer_ptr_a = dynamic_cast<const values::BufferValue *>(&a);
    auto scalar_ptr_b = dynamic_cast<const RealValue *>(&b);
    auto buffer_ptr_b = dynamic_cast<const values::BufferValue *>(&b);
    auto scalar_ptr_c = dynamic_cast<const RealValue *>(&c);
    auto buffer_ptr_c = dynamic_cast<const values::BufferValue *>(&c);

    // 3x scalar

    if (scalar_ptr_a && scalar_ptr_b && scalar_ptr_c)
        return std::make_unique<RealValue>(
            op(scalar_ptr_a->value, scalar_ptr_b->value, scalar_ptr_c->value), args...);

    // 3x buffer

    if (buffer_ptr_a && buffer_ptr_b && buffer_ptr_c)
    {
        if ((!buffer_ptr_a->buffer.matches_shape(buffer_ptr_b->buffer))
            || (!buffer_ptr_a->buffer.matches_shape(buffer_ptr_c->buffer)))
        {
            throw std::runtime_error(std::string("buffers do not match in dimensions: ")
                + a.str() + " and " + b.str() + " and " + c.str());
        }
        return std::make_unique<values::BufferValue>(apply(buffer_ptr_a->buffer.view(),
                                                         buffer_ptr_b->buffer.view(),
                                                         buffer_ptr_c->buffer.view(),
                                                         op),
            args...);
    }

    // 1x scalar, 2x buffer

    if (scalar_ptr_a && buffer_ptr_b && buffer_ptr_c)
    {
        if (!buffer_ptr_b->buffer.matches_shape(buffer_ptr_c->buffer))
        {
            throw std::runtime_error(std::string("buffers do not match in dimensions: ")
                + b.str() + " and " + c.str());
        }
        auto xa = scalar_ptr_a->value;
        return std::make_unique<values::BufferValue>(
            apply(buffer_ptr_b->buffer.view(),
                buffer_ptr_c->buffer.view(),
                [xa, op](Real xb, Real xc) -> Real { return op(xa, xb, xc); }),
            args...);
    }

    if (buffer_ptr_a && scalar_ptr_b && buffer_ptr_c)
    {
        if (!buffer_ptr_a->buffer.matches_shape(buffer_ptr_c->buffer))
        {
            throw std::runtime_error(std::string("buffers do not match in dimensions: ")
                + a.str() + " and " + c.str());
        }
        auto xb = scalar_ptr_b->value;
        return std::make_unique<values::BufferValue>(
            apply(buffer_ptr_a->buffer.view(),
                buffer_ptr_c->buffer.view(),
                [xb, op](Real xa, Real xc) -> Real { return op(xa, xb, xc); }),
            args...);
    }

    if (buffer_ptr_a && buffer_ptr_b && scalar_ptr_c)
    {
        if (!buffer_ptr_a->buffer.matches_shape(buffer_ptr_b->buffer))
        {
            throw std::runtime_error(std::string("buffers do not match in dimensions: ")
                + a.str() + " and " + b.str());
        }
        auto xc = scalar_ptr_c->value;
        return std::make_unique<values::BufferValue>(
            apply(buffer_ptr_a->buffer.view(),
                buffer_ptr_b->buffer.view(),
                [xc, op](Real xa, Real xb) -> Real { return op(xa, xb, xc); }),
            args...);
    }

    // 2x scalar, 1x buffer

    if (buffer_ptr_a && scalar_ptr_b && scalar_ptr_c)
    {
        auto xb = scalar_ptr_b->value;
        auto xc = scalar_ptr_c->value;
        return std::make_unique<values::BufferValue>(
            apply(buffer_ptr_a->buffer.view(),
                [xb, xc, op](Real xa) -> Real { return op(xa, xb, xc); }),
            args...);
    }

    if (scalar_ptr_a && buffer_ptr_b && scalar_ptr_c)
    {
        auto xa = scalar_ptr_a->value;
        auto xc = scalar_ptr_c->value;
        return std::make_unique<values::BufferValue>(
            apply(buffer_ptr_b->buffer.view(),
                [xa, xc, op](Real xb) -> Real { return op(xa, xb, xc); }),
            args...);
    }

    if (scalar_ptr_a && scalar_ptr_b && buffer_ptr_c)
    {
        auto xa = scalar_ptr_a->value;
        auto xb = scalar_ptr_b->value;
        return std::make_unique<values::BufferValue>(
            apply(buffer_ptr_c->buffer.view(),
                [xa, xb, op](Real xc) -> Real { return op(xa, xb, xc); }),
            args...);
    }

    throw std::runtime_error(
        std::string("Values not suitable for arithmetic operation: ") + a.str() + ", "
        + b.str() + ", " + c.str());
}

}; // namespace aquila::ops