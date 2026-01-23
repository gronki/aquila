#pragma once

#include <cstdint>
#include <iostream>
#include <memory>
#include <ostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace aquila::interpreter
{

struct Value
{
    Value() {}
    Value(const Value &) = delete;
    Value &operator=(const Value &) = delete;
    virtual std::unique_ptr<Value> clone() const = 0;
    virtual void write(std::ostream &os) const = 0;
    virtual ~Value() = default;
    std::string str() const
    {
        std::stringstream ss;
        write(ss);
        return ss.str();
    }
};

inline std::ostream &operator<<(std::ostream &os, const Value &v)
{
    v.write(os);
    return os;
}

template <typename T>
std::unique_ptr<T> as_ptr(T &&t)
{
    return std::make_unique<T>(std::move(t));
}

template <typename T>
std::unique_ptr<T> as_ptr(const T &t)
{
    return std::make_unique<T>(t);
}

struct AnySimpleValue : public Value
{
public:
    virtual bool dyn_compare(const AnySimpleValue &other) const = 0;

    friend bool operator==(const AnySimpleValue &a, const AnySimpleValue &b)
    {
        return a.dyn_compare(b);
    }

    friend bool operator!=(const AnySimpleValue &a, const AnySimpleValue &b)
    {
        return !a.dyn_compare(b);
    }
};

template <typename T>
struct SimpleValue : public AnySimpleValue
{
    T value;

    SimpleValue(const T &value) : value(value) {}
    SimpleValue(const SimpleValue<T> &value) : value(value.value) {}

    void write(std::ostream &os) const override { os << value; }

    std::unique_ptr<Value> clone() const override { return as_ptr(*this); }

    friend bool operator==(const SimpleValue<T> &a, const SimpleValue<T> &b)
    {
        return a.value == b.value;
    }

    friend bool operator!=(const SimpleValue<T> &a, const SimpleValue<T> &b)
    {
        return a.value != b.value;
    }

    virtual bool dyn_compare(const AnySimpleValue &other) const override
    {
        std::cout << "Dynamic compare called between " << *this << " and " << other
                  << std::endl;
        try
        {
            const SimpleValue<T> &other_sametype =
                dynamic_cast<const SimpleValue<T> &>(other);
            return other_sametype.value == value;
        }
        catch (const std::bad_cast &ex)
        {
            return false;
        }
    }
};

template <>
inline void SimpleValue<std::string>::write(std::ostream &os) const
{
    os << "\"" << value << "\"";
}

using IntValue = SimpleValue<std::int64_t>;
using RealValue = SimpleValue<double>;
using StrValue = SimpleValue<std::string>;

struct CompoundValue : public Value
{
};

using ValuePtr = std::unique_ptr<Value>;
using ValuePtrVector = std::vector<ValuePtr>;

template <typename DerivedValue>
struct IFromFile
{
    static auto value_from_file(const std::string& fn) {
        return DerivedValue::from_file(fn);
    }
};

struct SequenceValue : public Value
{
    ValuePtrVector items;

    SequenceValue(const SequenceValue &other)
    {
        items.reserve(other.items.size());
        for (const auto &item : other.items)
        {
            items.push_back(item->clone());
        }
    }
    SequenceValue(ValuePtrVector items) : items(std::move(items)) {}
    SequenceValue() : items(0) {}

    size_t size() const { return items.size(); }

    void write(std::ostream &os) const override
    {
        os << "[";
        bool first = true;
        for (const auto &item : items)
        {
            if (first)
            {
                first = false;
            }
            else
            {
                os << ", ";
            }
            if (item)
            {
                item->write(os);
            }
            else
            {
                os << "(null)";
            }
        }
        os << "]";
    }

    std::unique_ptr<Value> clone() const override { return as_ptr(*this); }
};

} // namespace aquila::interpreter

namespace aquila
{

// export frequently used names
using interpreter::RealValue;
using interpreter::SequenceValue;
using interpreter::StrValue;
using interpreter::Value;
using interpreter::ValuePtr;

} // namespace aquila