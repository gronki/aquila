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

// A trace records *how a value was obtained*: "stack{normalize{...}}". It is
// the key under which results are memoised (see cache.hpp), so two values
// reached by different routes must not share one. That is why a trace belongs
// to the reference (ValueRef below) and not to the value itself: the very same
// frame may be reachable both as "x" and as "item{seq{...}; 1}".
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

struct Value;

template <typename T>
concept ValueConcept = std::derived_from<T, Value>;

// A reference to a value, plus the trace of how this particular reference came
// to be. Values are immutable once they are referenced, so references may be
// copied freely and share the pointee; an operation that needs something it can
// write into asks for clone() and says so.
template <ValueConcept T>
class ValueRef
{
    std::shared_ptr<const T> ptr;
    // the trace is held by pointer, not by value: copying a reference is a
    // common operation and a trace can be a long nested string, so all the
    // references that share a trace share one copy of it
    std::shared_ptr<const value_trace_t> trace;

    // wraps a trace for sharing. An empty trace carries no information (see
    // get_trace below, which falls back to the value itself), so it is stored
    // as a null pointer rather than paying for an allocation.
    static std::shared_ptr<const value_trace_t> share_trace(value_trace_t t)
    {
        if (t.content.empty())
            return nullptr;
        return std::make_shared<const value_trace_t>(std::move(t));
    }

public:
    template <ValueConcept U>
    friend class ValueRef;

    ValueRef() {}
    ValueRef(std::nullptr_t) {}

    // a freshly built value: nobody else can see it yet, so sealing it as const
    // costs nothing
    template <ValueConcept U>
    ValueRef(std::unique_ptr<U> owned, value_trace_t trace = {}) :
        trace(share_trace(std::move(trace)))
    {
        if (owned)
            // hand ownership over as a unique_ptr, so that a throwing
            // control-block allocation does not lose the object
            ptr = std::unique_ptr<const T>(static_cast<const T *>(owned.release()));
    }

    // Converts a reference to one value type into a reference to another:
    // the same object, seen as a different type, with the two references
    // sharing one refcount so the object lives as long as either of them.
    //
    // static_pointer_cast is the shared_ptr equivalent of static_cast: it
    // changes the type at compile time and performs NO runtime check.
    // Widening (a BufferValue seen as a Value) is always correct. Narrowing
    // (a Value seen as a BufferValue) is only correct if the value really is
    // one, and nothing here verifies that -- go through value_cast below,
    // which checks the type first and hands back an empty reference when it
    // does not match.
    template <ValueConcept U>
    ValueRef(const ValueRef<U> &other) :
        ptr(std::static_pointer_cast<const T>(other.ptr)), trace(other.trace)
    {
    }

    const T &operator*() const
    {
        if (!ptr)
            throw std::runtime_error("trying to dereference empty value reference!");
        return *ptr;
    }

    const T *operator->() const noexcept { return ptr.get(); }
    const T *get() const noexcept { return ptr.get(); }
    explicit operator bool() const noexcept { return (bool)ptr; }
    bool operator==(std::nullptr_t) const noexcept { return !ptr; }

    // an independent, writable copy. The only way to a mutable value, and
    // deliberately explicit: it deep-copies, which for a frame means megabytes.
    std::unique_ptr<T> clone() const
    {
        if (!ptr)
            throw std::runtime_error("trying to clone empty value reference!");
        std::unique_ptr<Value> cloned = ptr->clone();
        return std::unique_ptr<T>(static_cast<T *>(cloned.release()));
    }

    // how this reference was obtained; falls back to what the value can say
    // about itself (a literal traces as itself, a sequence as its items)
    value_trace_t get_trace() const;

    void set_trace(value_trace_t new_trace) { trace = share_trace(std::move(new_trace)); }

    ValueRef with_trace(value_trace_t new_trace) const
    {
        ValueRef copy(*this);
        copy.trace = share_trace(std::move(new_trace));
        return copy;
    }

    template <typename... Args>
    static ValueRef make(Args &&...args)
    {
        ValueRef out;
        out.ptr = std::make_shared<const T>(std::forward<Args>(args)...);
        return out;
    }
};

using ValuePtr = ValueRef<Value>;

struct Value
{
    Value() {}
    Value(const Value &) = delete;
    Value &operator=(const Value &) = delete;
    virtual std::unique_ptr<Value> clone() const = 0;
    virtual void write(std::ostream &os) const = 0;
    virtual ~Value() = default;
    virtual const value_type &get_type() const = 0;
    virtual bool is_sequence() const { return false; }
    virtual int64_t sequence_len() const { return -1; }
    virtual int64_t mem_size() const { return 0; }
    virtual int64_t sequence_depth() const { return 0; }
    // the trace a value implies when the reference to it carries none of its own
    virtual value_trace_t implicit_trace() const { return str(); }
    std::string str() const
    {
        std::stringstream ss;
        write(ss);
        return ss.str();
    }
};

template <ValueConcept T>
value_trace_t ValueRef<T>::get_trace() const
{
    if (!ptr)
        return {};
    if (trace)
        return *trace;
    return ptr->implicit_trace();
}

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
inline ValueRef<T> value_cast(const ValueRef<U> &other)
{
    if (!other)
        return {};
    if (!__is_compatible<T>(other->get_type()))
        return {};
    return ValueRef<T>(other);
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
template <>
struct __simpleval_typenames<bool>
{
    TYPE_NAME("bool");
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

    SimpleValue(const T &value) : value(value) {}
    SimpleValue(const SimpleValue<T> &other) : value(other.value) {}

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

using StrValue = SimpleValue<Str>;

template <>
inline void SimpleValue<Str>::write(std::ostream &os) const
{
    os << "\"" << value << "\"";
}

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
using BoolValue = SimpleValue<bool>;

template <>
inline void SimpleValue<bool>::write(std::ostream &os) const
{
    os << (value ? "true" : "false");
}

template <>
inline bool *value_cast<bool>(Value *other)
{
    auto sv = value_cast<SimpleValue<bool>>(other);
    if (!sv)
        return nullptr;
    return &sv->value;
}
template <>
inline const bool *value_cast<bool>(const Value *other)
{
    auto sv = value_cast<SimpleValue<bool>>(other);
    if (!sv)
        return nullptr;
    return &sv->value;
}
template <>
inline bool &value_cast<bool>(Value &other)
{
    return value_cast<SimpleValue<bool>>(other).value;
}
template <>
inline const bool &value_cast<bool>(const Value &other)
{
    return value_cast<SimpleValue<bool>>(other).value;
}

using ValuePtrVector = std::vector<ValuePtr>;

struct SequenceValue : public ValueBase<SequenceValue>
{
    TYPE_NAME("sequence");

    std::vector<ValuePtr> items;

    // the items are immutable and reference-counted, so copying the sequence
    // copies references, not frames
    SequenceValue(const SequenceValue &other) : items(other.items) {}
    SequenceValue(std::vector<ValuePtr> items) : items(std::move(items)) {}
    SequenceValue() {}

    size_t size() const { return items.size(); }
    virtual bool is_sequence() const override { return true; }
    int64_t sequence_len() const override { return items.size(); }
    int64_t sequence_depth() const override
    {
        int64_t children_depth = -1;
        for (const auto &item : items)
        {
            if (!item)
                continue;
            auto child_depth = item->sequence_depth();
            children_depth = (children_depth < 0 || child_depth < children_depth)
                ? child_depth
                : children_depth;
        }
        return children_depth + 1;
    }

    value_trace_t implicit_trace() const override
    {
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
                ss << item.get_trace();
            }
            else
            {
                ss << "(null)";
            }
        }
        ss << "]";
        return ss.str();
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

using interpreter::value_cast;
using interpreter::value_type;
using interpreter::ValueBase;
using interpreter::ValuePtr;
using interpreter::ValueRef;

} // namespace aquila
