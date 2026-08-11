#pragma once

#include <cstdint>
#include <functional>
#include <map>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>

#include "bind_args.hpp"
#include "type_converter.hpp"
#include "value.hpp"

namespace aquila::interpreter
{

struct ArgSpec
{
    std::string name;

    // this might not be the prettiest way, but makes it easy
    // to build manifests with initializer lists and ensures
    // that user does not give anything crazy as defaults.
    std::optional<std::int64_t> default_int = std::nullopt;
    std::optional<double> default_real = std::nullopt;
    std::optional<std::string> default_str = std::nullopt;

    bool has_default() const;
    std::unique_ptr<Value> build_default() const;

    bool sequence = false;

    std::string help = "";
    ConvertFun convert = nullptr;
};

using ArgManifest = std::vector<ArgSpec>;
static const std::string ARG_ELLIPSIS = "...";
struct manifest_properties_t
{
    manifest_properties_t(const ArgManifest &m);
    size_t num_positionals, num_keyword;
    bool has_ellipsis;
};

struct Operation
{
    enum class Tracing
    {
        DEFAULT,
        FROM_INPUTS,
        FROM_RETVAL,
        UNTRACEABLE
    };
    virtual ArgManifest arg_manifest() const
    {
        return ArgManifest{ArgSpec{.name = ARG_ELLIPSIS}};
    }
    virtual ValuePtr call(std::vector<ValuePtr>) const = 0;
    virtual std::string name() const = 0;
    virtual std::string description() const { return ""; }
    virtual Tracing tracing_mode() const { return Tracing::DEFAULT; }
    virtual bool cacheable() const
    {
        return tracing_mode() != Operation::Tracing::FROM_RETVAL
            && tracing_mode() != Operation::Tracing::UNTRACEABLE;
    }
    virtual value_trace_t custom_trace(const std::vector<ValuePtr> *, const Value *) const
    {
        return {};
    }
    std::string signature_str() const;
    virtual ~Operation() = default;
};

#define BIND_ARGS(proc)                                                                \
    ValuePtr call(std::vector<ValuePtr> args) const override                           \
    {                                                                                  \
        return bind_args(this, (proc), args);                                          \
    }

struct ArgMatch
{
    bool matched = false;
    size_t pos;
    std::unique_ptr<Value> deftgt = nullptr;
    ConvertFun convert = nullptr;
    bool sequence = false;
};

std::vector<ArgMatch> match_arguments(const std::vector<ArgSpec> &manifest,
    const manifest_properties_t &,
    const std::vector<std::string> &given_keys);

std::vector<ValuePtr> build_ptrs_from_match(
    std::vector<ValuePtr> &given_args, const std::vector<ArgMatch> &match);
std::vector<value_trace_t> build_traces_from_match(
    const std::vector<value_trace_t> &traces, const std::vector<ArgMatch> &match);

using OpFactory = std::unique_ptr<Operation> (*)();

struct OpDbEntry
{
    OpFactory factory;
    std::string signature_str, description;
};
using OpDatabase = std::map<std::string, OpDbEntry>;

OpDatabase &global_op_db();

template <typename OpClass>
struct register_op_global
{
    register_op_global()
    {
        OpClass op;
        auto name = op.name();

        OpDbEntry entry;
        entry.factory = []() -> std::unique_ptr<aquila::interpreter::Operation>
        { return std::make_unique<OpClass>(); };
        entry.signature_str = op.signature_str();
        entry.description = op.description();

        auto [it, inserted] = aquila::interpreter::global_op_db().insert({name, entry});

        if (!inserted)
        {
            std::cerr << "Duplicate operation definition: " << name << ", skipping..."
                      << std::endl;
            return;
        }
    }
};

#define REGISTER(opclass)                                                              \
    aquila::interpreter::register_op_global<opclass> __register__operation__##opclass;

} // namespace aquila::interpreter

namespace aquila
{

// export frequently used names
using interpreter::ArgManifest;
using interpreter::ArgSpec;
using interpreter::Operation;

} // namespace aquila
