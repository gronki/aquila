#pragma once

#include <functional>
#include <map>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>

#include "bind_args.hpp"
#include "value.hpp"

namespace aquila::interpreter
{

template <typename T>
inline bool is_type(const Value &val)
{
    return dynamic_cast<T *>(val) != nullptr;
}

inline bool default_check(const Value &val)
{
    return true;
}

using CheckFunction = bool (*)(const Value &);

struct ArgSpec
{
    std::string name;
    
    // this might not be the prettiest way, but makes it easy
    // to build manifests with initializer lists and ensures
    // that user does not give anything crazy as defaults.
    std::optional<Int> default_int = std::nullopt;
    std::optional<Real> default_real = std::nullopt;
    std::optional<String> default_str = std::nullopt;
    
    bool has_default() const;
    std::unique_ptr<Value> build_default() const;
    // CheckFunction check = default_check;

    std::string help = "";
};

using ArgManifest = std::vector<ArgSpec>;

struct Operation
{
    virtual std::optional<ArgManifest> arg_manifest() const { return std::nullopt; }
    virtual std::unique_ptr<Value> call(const std::vector<const Value *> &) const = 0;
    virtual std::string name() const = 0;
    virtual std::string description() const { return ""; }
    std::string signature_str() const;
    virtual ~Operation() = default;
};

#define BIND_ARGS(proc)                                                                \
    std::unique_ptr<Value> call(const std::vector<const Value *> &args) const override \
    {                                                                                  \
        return bind_args(this, (proc), args);                                          \
    }

struct ArgMatch
{
    bool matched = false;
    size_t pos;
    std::unique_ptr<Value> deftgt = nullptr;
};

std::vector<ArgMatch> match_arguments(
    const std::vector<ArgSpec> &manifest, const std::vector<std::string> &given_keys);

std::vector<const Value *> build_ptrs_from_match(
    const std::vector<const Value *> &given_args, const std::vector<ArgMatch> &match);

using OpFactory = std::unique_ptr<Operation> (*)();

struct OpDbEntry
{
    OpFactory factory;
    std::string signature_str, description;
};
using OpDatabase = std::map<String, OpDbEntry>;

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

        if (inserted)
        {
            std::cout << "* " << entry.signature_str;
            if (!entry.description.empty())
                std::cout << " -- " << entry.description;
            std::cout << std::endl;
        }
        else
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
