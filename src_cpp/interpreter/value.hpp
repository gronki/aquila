#pragma once

#include <concepts>
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
    constexpr value_type(const value_type &other)
    {
        for (std::size_t i = 0; i < TYPE_N_LEN; i++)
        {
            tname[i] = other.tname[i];
        }
        hsh = other.hsh;
    }
    std::string str() const
    {
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
        os << tid.str() << " (" << std::hex << tid.hsh << std::dec << ")";
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

struct Value;

template <typename T>
concept ValueConcept = std::derived_from<T, Value>;

template <ValueConcept T>
class Ptr
{
    std::unique_ptr<T> owned;
    const T *ref;

public:
    template <ValueConcept U>
    friend class Ptr;

    Ptr() : owned(nullptr), ref(nullptr) {};
    Ptr(std::nullptr_t) : owned(nullptr), ref(nullptr) {};

    template <ValueConcept U>
    Ptr(const U *ref) : owned(nullptr), ref(static_cast<const T *>(ref))
    {
    }

    template <ValueConcept U>
    Ptr(std::unique_ptr<U> owned) :
        owned(owned ? static_cast<T *>(owned.release()) : nullptr), ref(nullptr)
    {
    }

    template <ValueConcept U>
    Ptr(Ptr<U> &&other) :
        owned(other.owned ? std::unique_ptr<T>(static_cast<T *>(other.owned.release()))
                          : nullptr),
        ref(static_cast<const T *>(other.ref))
    {
    }

    template <ValueConcept U>
    Ptr &operator=(Ptr<U> &&other)
    {
        owned = other.owned ? std::unique_ptr<T>(static_cast<T *>(other.owned.release()))
                            : nullptr;
        ref = static_cast<const T *>(other.ref);
        return *this;
    }

    Ptr &operator=(std::nullptr_t)
    {
        owned = nullptr;
        ref = nullptr;
        return *this;
    }

    std::unique_ptr<T> own()
    {
        if (owned)
        {
            ref = owned.get();
            return std::move(owned);
        }
        if (!ref)
            throw std::runtime_error("trying to dereferenc empty pointer!");
        std::unique_ptr<Value> cloned = ref->clone();
        return std::unique_ptr<T>(static_cast<T *>(cloned.release()));
    }

    bool is_owned() const noexcept { return (bool)owned; }

    const T &operator*() const
    {
        if (owned)
            return *owned;
        if (!ref)
            throw std::runtime_error("trying to dereferenc empty pointer!");
        return *ref;
    }

    template <typename... Args>
    static Ptr make(Args &&...args)
    {
        return std::make_unique<T>(std::forward<Args>(args)...);
    }

    const T *operator->() const noexcept { return owned ? owned.get() : ref; }
    const T *get() const noexcept { return owned ? owned.get() : ref; }
    T *get_mut() const noexcept { return owned ? owned.get() : nullptr; }
    explicit operator bool() const noexcept { return owned || ref; }
};

using ValuePtr = Ptr<Value>;

struct value_trace_t
{
    std::string content;
    bool is_corrupt;
    value_trace_t() : is_corrupt(true) {}
    value_trace_t(const std::string &content) : content(content), is_corrupt(false) {};
    std::string flatten() const { return content; }
    static value_trace_t corrupt()
    {
        value_trace_t t;
        t.is_corrupt = true;
        return t;
    }
};

inline std::ostream &operator<<(std::ostream &os, const value_trace_t &trace)
{
    if (trace.is_corrupt)
    {
        os << "##NOTRACE##";
    }
    else
    {
        os << trace.flatten();
    }
    return os;
}

struct Value
{
    Value() {}
    Value(const Value &) = delete;
    Value &operator=(const Value &) = delete;
    virtual std::unique_ptr<Value> clone() const = 0;
    virtual void write(std::ostream &os) const = 0;
    virtual ~Value() = default;
    virtual const value_type &get_type() const = 0;
    virtual void materialize() {}
    virtual bool is_sequence() const { return false; }
    virtual int64_t sequence_len() const { return -1; }
    virtual ValuePtr shallow() const { return {this}; }
    virtual int64_t mem_size() const { return 0; }
    value_trace_t trace;
    virtual value_trace_t get_trace() const
    {
        if (!trace.content.empty())
            return trace;
        return str();
    }
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
inline bool __is_compatible(const value_type &t)
{
    return t == T::type_name;
}

template <>
inline bool __is_compatible<Value>(const value_type &t)
{
    return true;
}

template <typename T>
inline T *value_cast(Value *other)
{
    if (!other)
        return nullptr;
    if (!__is_compatible<T>(other->get_type()))
        return nullptr;
    return static_cast<T *>(other);
}

template <typename T>
inline const T *value_cast(const Value *other)
{
    if (!other)
        return nullptr;
    if (!__is_compatible<T>(other->get_type()))
        return nullptr;
    return static_cast<const T *>(other);
}

template <typename T>
inline T &value_cast(Value &other)
{
    if (!__is_compatible<T>(other.get_type()))
        throw std::bad_cast{};
    return static_cast<T &>(other);
}

template <typename T>
inline const T &value_cast(const Value &other)
{
    if (!__is_compatible<T>(other.get_type()))
        throw std::bad_cast{};
    return static_cast<const T &>(other);
}

template <ValueConcept T, ValueConcept U>
inline Ptr<T> value_cast(Ptr<U> &other)
{
    if (!other)
        return {};
    if (!__is_compatible<T>(other->get_type()))
        return {};
    return std::move(other);
}

using Real = double;
using Str = std::string;
using Int = std::int64_t;

template <typename T>
struct __simpleval_typenames
{
    TYPE_NAME(T::type_name);
};
template <>
struct __simpleval_typenames<Value>
{
    TYPE_NAME("any");
};
template <>
struct __simpleval_typenames<Str>
{
    TYPE_NAME("str");
};
template <>
struct __simpleval_typenames<Int>
{
    TYPE_NAME("int");
};
template <>
struct __simpleval_typenames<Real>
{
    TYPE_NAME("real");
};

template <typename T>
const value_type &value_type_info()
{
    return __simpleval_typenames<T>::type_name;
}

template <typename T>
struct SimpleValue : public ValueBase<SimpleValue<T>>
{
    constexpr static value_type type_name{__simpleval_typenames<T>::type_name};

    T value;

    SimpleValue(const T &value, const value_trace_t &trace = {}) : value(value)
    {
        this->trace = trace;
    }
    SimpleValue(const SimpleValue<T> &other) : value(other.value)
    {
        this->trace = other.trace;
    }

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
inline void SimpleValue<Str>::write(std::ostream &os) const
{
    os << "\"" << value << "\"";
}

using StrValue = SimpleValue<Str>;

template <>
inline Str *value_cast<Str>(Value *other)
{
    auto sv = value_cast<SimpleValue<Str>>(other);
    if (!sv)
        return nullptr;
    return &sv->value;
}
template <>
inline const Str *value_cast<Str>(const Value *other)
{
    auto sv = value_cast<SimpleValue<Str>>(other);
    if (!sv)
        return nullptr;
    return &sv->value;
}
template <>
inline Str &value_cast<Str>(Value &other)
{
    return value_cast<SimpleValue<Str>>(other).value;
}
template <>
inline const Str &value_cast<Str>(const Value &other)
{
    return value_cast<SimpleValue<Str>>(other).value;
}

using IntValue = SimpleValue<Int>;

template <>
inline Int *value_cast<Int>(Value *other)
{
    auto sv = value_cast<SimpleValue<Int>>(other);
    if (!sv)
        return nullptr;
    return &sv->value;
}
template <>
inline const Int *value_cast<Int>(const Value *other)
{
    auto sv = value_cast<SimpleValue<Int>>(other);
    if (!sv)
        return nullptr;
    return &sv->value;
}
template <>
inline Int &value_cast<Int>(Value &other)
{
    return value_cast<SimpleValue<Int>>(other).value;
}
template <>
inline const Int &value_cast<Int>(const Value &other)
{
    return value_cast<SimpleValue<Int>>(other).value;
}
using RealValue = SimpleValue<Real>;

template <>
inline Real *value_cast<Real>(Value *other)
{
    auto sv = value_cast<SimpleValue<Real>>(other);
    if (!sv)
        return nullptr;
    return &sv->value;
}
template <>
inline const Real *value_cast<Real>(const Value *other)
{
    auto sv = value_cast<SimpleValue<Real>>(other);
    if (!sv)
        return nullptr;
    return &sv->value;
}
template <>
inline Real &value_cast<Real>(Value &other)
{
    return value_cast<SimpleValue<Real>>(other).value;
}
template <>
inline const Real &value_cast<Real>(const Value &other)
{
    return value_cast<SimpleValue<Real>>(other).value;
}

using ValuePtrVector = std::vector<ValuePtr>;

struct SequenceValue : public ValueBase<SequenceValue>
{
    TYPE_NAME("sequence");

    std::vector<Ptr<Value>> items;

    SequenceValue(const SequenceValue &other)
    {
        items.reserve(other.items.size());
        for (const auto &item : other.items)
        {
            items.push_back(item->clone());
        }
        trace = other.trace;
    }
    SequenceValue(std::vector<Ptr<Value>> items, const value_trace_t &trace = {}) :
        items(std::move(items))
    {
        this->trace = trace;
    }
    SequenceValue() : items(0) {}

    size_t size() const { return items.size(); }
    virtual bool is_sequence() const override { return true; }
    int64_t sequence_len() const override { return items.size(); }

    void materialize() override
    {
        for (auto &item : items)
        {
            auto owned = item.own();
            owned->materialize();
            item = std::move(owned);
        }
    }

    value_trace_t get_trace() const override
    {
        if (!trace.content.empty())
        {
            return trace;
        }
        std::stringstream ss;
        ss << "[";
        bool first = true;
        for (const auto &item : items)
        {
            if (first)
            {
                first = false;
            }
            else
            {
                ss << ", ";
            }
            if (item)
            {
                ss << item->get_trace();
            }
            else
            {
                ss << "(null)";
            }
        }
        ss << "]";
        return ss.str();
    }

    virtual ValuePtr shallow() const override
    {
        ValuePtrVector shallow_items;
        for (const auto &item : items)
        {
            if (!item)
            {
                shallow_items.emplace_back(nullptr);
            }
            else
            {
                shallow_items.emplace_back(item->shallow());
            }
        }
        return Ptr<SequenceValue>::make(std::move(shallow_items), trace);
    }

    int64_t mem_size() const override
    {
        int64_t size = 0;
        for (const auto &item : items)
            if (item)
                size += item->mem_size();
        return size;
    }

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
};

} // namespace aquila::interpreter

namespace aquila
{

// export frequently used names

using interpreter::Int;
using interpreter::IntValue;
using interpreter::Real;
using interpreter::RealValue;
using interpreter::SequenceValue;
using interpreter::Str;
using interpreter::StrValue;
using interpreter::Value;

using interpreter::Ptr;
using interpreter::value_cast;
using interpreter::value_type;
using interpreter::ValueBase;
using interpreter::ValuePtr;

} // namespace aquila
