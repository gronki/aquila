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

static const size_t TYPE_N_LEN = 24;
struct value_type
{
    char tname[TYPE_N_LEN];
    std::uint64_t hsh = 1469598103934665603ull;
    constexpr value_type(const char *name)
    {
        for (std::size_t i = 0; i < TYPE_N_LEN; i++)
        {
            tname[i] = ' ';
        }
        for (std::size_t i = 0; i < TYPE_N_LEN; i++)
        {
            if (name[i])
            {
                tname[i] = name[i];
                hsh ^= name[i];
                hsh *= 1099511628211ull;
            }
            else
            {
                break;
            }
        }
    }
    std::string str() const {
        std::size_t len_trim;
        for (len_trim = TYPE_N_LEN; len_trim > 0; len_trim--)
        {
            if (tname[len_trim - 1] != ' ')
                break;
        }
        std::string trimmed(tname, tname + len_trim);
        return trimmed;
    }
    friend std::ostream &operator<<(std::ostream &os, const value_type &tid)
    {
        os << tid.str() << " (" << std::hex <<  tid.hsh << std::dec << ")";
        return os;
    }
    constexpr bool operator==(const value_type &other) const
    {
        return hsh == other.hsh;
    }
    constexpr bool operator!=(const value_type &other) const
    {
        return hsh != other.hsh;
    }
};

struct Value
{
    Value() {}
    Value(const Value &) = delete;
    Value &operator=(const Value &) = delete;
    virtual std::unique_ptr<Value> clone() const = 0;
    virtual void write(std::ostream &os) const = 0;
    virtual ~Value() = default;
    virtual const value_type &get_type() const = 0;
    std::string str() const
    {
        std::stringstream ss;
        write(ss);
        return ss.str();
    }
};

#define TYPE_NAME(x)                                                                   \
    constexpr static aquila::interpreter::value_type type_name                         \
    {                                                                                  \
        x                                                                              \
    }
template <typename T>
struct ValueBase : public Value
{

    std::unique_ptr<Value> clone() const override
    {
        const T *tptr = static_cast<const T *>(this);
        return std::make_unique<T>(*tptr);
    }

    const value_type &get_type() const override { return T::type_name; }
};

inline std::ostream &operator<<(std::ostream &os, const Value &v)
{
    v.write(os);
    return os;
}

template <typename T>
inline T *value_cast(Value *other)
{
    if (!other)
        return nullptr;
    if (other->get_type() != T::type_name)
        return nullptr;
    return static_cast<T *>(other);
}

template <>
inline Value *value_cast<Value>(Value *other)
{
    return other;
}

template <typename T>
inline const T *value_cast(const Value *other)
{
    if (!other)
        return nullptr;
    if (other->get_type() != T::type_name)
        return nullptr;
    return static_cast<const T *>(other);
}

template <>
inline const Value *value_cast<Value>(const Value *other)
{
    return other;
}

template <typename T>
inline T &value_cast(Value &other)
{
    if (other.get_type() != T::type_name)
        throw std::bad_cast{};
    return static_cast<T &>(other);
}

template <>
inline Value &value_cast<Value>(Value &other)
{
    return other;
}

template <typename T>
inline const T &value_cast(const Value &other)
{
    if (other.get_type() != T::type_name)
        throw std::bad_cast{};
    return static_cast<const T &>(other);
}

template <>
inline const Value &value_cast<Value>(const Value &other)
{
    return other;
}

template <typename T>
struct __simpleval_typenames;
template <>
struct __simpleval_typenames<std::string>
{
    TYPE_NAME("str");
};
template <>
struct __simpleval_typenames<std::int64_t>
{
    TYPE_NAME("int");
};
template <>
struct __simpleval_typenames<double>
{
    TYPE_NAME("float");
};

template <typename T>
struct SimpleValue : public ValueBase<SimpleValue<T>>
{
    constexpr static value_type type_name{__simpleval_typenames<T>::type_name};

    T value;

    SimpleValue(const T &value) : value(value) {}
    SimpleValue(const SimpleValue<T> &value) : value(value.value) {}

    void write(std::ostream &os) const override { os << value; }

    friend bool operator==(const SimpleValue<T> &a, const SimpleValue<T> &b)
    {
        return a.value == b.value;
    }

    friend bool operator!=(const SimpleValue<T> &a, const SimpleValue<T> &b)
    {
        return a.value != b.value;
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

using ValuePtr = std::unique_ptr<Value>;
using ValuePtrVector = std::vector<ValuePtr>;

struct SequenceValue : public ValueBase<SequenceValue>
{
    TYPE_NAME("sequence");

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

    template <typename U>
    std::vector<const U *> items_as() const
    {
        std::vector<const U *> casted(items.size());
        for (size_t iarg = 0; iarg < items.size(); iarg++)
        {
            auto &casted_item = casted[iarg];
            if (!items[iarg])
            {
                casted_item = nullptr;
                continue;
            }
            casted_item = value_cast<U>(items[iarg].get());
            if (!casted_item)
                throw std::runtime_error(std::string("Cast failed for item ")
                    + std::to_string(iarg + 1) + " of the list.");
        }
        return casted;
    }
};

} // namespace aquila::interpreter

namespace aquila
{

// export frequently used names
using interpreter::RealValue;
using interpreter::SequenceValue;
using interpreter::StrValue;
using interpreter::Value;
using interpreter::value_cast;
using interpreter::value_type;
using interpreter::ValueBase;
using interpreter::ValuePtr;

} // namespace aquila
