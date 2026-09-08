#pragma once

#include <iostream>
#include <memory>
#include <value.hpp>

namespace aquila
{
template <typename TS>
struct __struct_default_registry
{
    using FacT = TS (*)();
    static FacT get_factory()
    {
        std::cerr << "Warning: no default factory for " << typeid(TS).name() << std::endl;
        return nullptr;
    }
};
} // namespace aquila

namespace aquila::interpreter
{

struct StructFieldBase
{
    int struct_nr = 0;
    StructFieldBase(int struct_nr) : struct_nr(struct_nr) {}
    virtual std::unique_ptr<Value> build_default() const = 0;
    virtual ~StructFieldBase() = default;
};

#define STRUCT_DEFAULT(TS, FAC)                                                        \
    template <>                                                                        \
    struct aquila::__struct_default_registry<TS>                                       \
    {                                                                                  \
        using FacT = TS (*)();                                                         \
        static FacT get_factory() { return FAC; }                                      \
    }

template <typename TS>
struct StructFieldB : public StructFieldBase
{
    using FacT = TS (*)();
    FacT def_factory;
    StructFieldB(int struct_nr) :
        StructFieldBase(struct_nr),
        def_factory(aquila::__struct_default_registry<TS>::get_factory())
    {
    }
    TS struct_default() const
    {
        if (!def_factory)
            return {};
        return def_factory();
    }
    virtual void read(const Value *, TS &) const = 0;
};

template <typename TF>
struct field_cast
{
};

template <>
struct field_cast<const char *>
{
    static const char *cast(const Value &val)
    {
        return value_cast<SimpleValue<Str>>(val).value.c_str();
    }
    static std::unique_ptr<Value> inv(const char *c)
    {
        return std::make_unique<StrValue>(c);
    }
};

template <>
struct field_cast<int>
{
    static int cast(const Value &val)
    {
        return value_cast<SimpleValue<Real>>(val).value;
    }
    static std::unique_ptr<Value> inv(int i) { return std::make_unique<RealValue>(i); }
};

template <>
struct field_cast<int64_t>
{
    static int64_t cast(const Value &val)
    {
        return value_cast<SimpleValue<Real>>(val).value;
    }
    static std::unique_ptr<Value> inv(int64_t l)
    {
        return std::make_unique<RealValue>(l);
    }
};

template <>
struct field_cast<float>
{
    static float cast(const Value &val)
    {
        return value_cast<SimpleValue<Real>>(val).value;
    }
    static std::unique_ptr<Value> inv(float f)
    {
        return std::make_unique<RealValue>(f);
    }
};

template <>
struct field_cast<double>
{
    static double cast(const Value &val)
    {
        return value_cast<SimpleValue<Real>>(val).value;
    }
    static std::unique_ptr<Value> inv(double d)
    {
        return std::make_unique<RealValue>(d);
    }
};

template <>
struct field_cast<bool>
{
    static bool cast(const Value &val)
    {
        if (auto casted = value_cast<SimpleValue<Real>>(&val))
            return casted->value > 0;

        return value_cast<SimpleValue<bool>>(val).value;
    }
    static std::unique_ptr<Value> inv(bool b) { return std::make_unique<BoolValue>(b); }
};

template <typename TF, typename TS>
struct StructField : public StructFieldB<TS>
{
    TF TS::*field;
    TF default_val;
    StructField(int struct_nr, TF TS::*field) :
        StructFieldB<TS>(struct_nr), field(field),
        default_val(StructFieldB<TS>::struct_default().*(this->field))
    {
    }
    void read(const Value *val, TS &dest) const override
    {
        if (!val)
            return;
        dest.*field = field_cast<TF>::cast(*val);
    }
    std::unique_ptr<Value> build_default() const override
    {
        return field_cast<TF>::inv(default_val);
    }
};

template <typename TF, typename TS>
std::shared_ptr<StructFieldBase> field(int struct_nr, TF TS::*field)
{
    return std::make_shared<StructField<TF, TS>>(struct_nr, field);
}

template <typename TF, typename TS>
std::shared_ptr<StructFieldBase> field(TF TS::*field)
{
    return std::make_shared<StructField<TF, TS>>(0, field);
}

} // namespace aquila::interpreter

namespace aquila
{
using aquila::interpreter::field;
}
