#include <map>
#include <set>

#include "characters.hpp"
#include "operation.hpp"

namespace aquila::interpreter
{

using std::size_t;

OpDatabase &global_op_db()
{
    static OpDatabase db;
    return db;
}

bool ArgSpec::has_default() const
{
    return default_int.has_value() || default_real.has_value() || default_str.has_value();
}

std::unique_ptr<Value> ArgSpec::build_default() const
{
    if (int(default_int.has_value()) + int(default_real.has_value())
            + int(default_str.has_value())
        > 1)
    {
        throw std::invalid_argument(
            std::string("only one default may be given for argument ") + name);
    }

    if (default_int)
        return std::make_unique<IntValue>(default_int.value());
    if (default_real)
        return std::make_unique<RealValue>(default_real.value());
    if (default_str)
        return std::make_unique<StrValue>(default_str.value());

    return nullptr;
}
std::vector<const Value *> build_ptrs(const std::vector<std::unique_ptr<Value>> &given_args)
{
    std::vector<const Value *> args;
    args.reserve(given_args.size());
    for (const auto &arg : given_args)
    {
        args.push_back(arg.get());
    }
    return args;
}

static bool valid_argspec_key(const std::string &name)
{
    if (name.empty())
        return false;
    for (size_t ipos = 0; ipos < name.size(); ipos++)
    {
        auto ch = name[ipos];
        if (ipos == 0 && !is_ident_start(ch))
            return false;
        if (!is_ident(ch))
            return false;
    }
    return true;
}

static void check_argspec_integrity(const std::vector<ArgSpec> &manifest)
{
    bool first_keyword = false;
    int iarg = 1;
    for (const auto &argspec : manifest)
    {
        if (argspec.name.empty())
        {
            throw std::invalid_argument(
                std::string("Argument name must not be empty in argument"
                            "manifest at position ")
                + std::to_string(iarg));
        }

        if (!valid_argspec_key(argspec.name))
        {
            throw std::invalid_argument(std::string("Argument name in position ")
                + std::to_string(iarg) + " is invalid: " + argspec.name);
        }

        first_keyword = first_keyword || argspec.has_default();

        if (first_keyword && !argspec.has_default())
        {
            throw std::invalid_argument(
                std::string("In the manifest, optional arguments must go after "
                            "required arguments: ")
                + argspec.name);
        }

        iarg++;
    }
}

std::vector<ArgMatch> match_arguments(
    const std::vector<ArgSpec> &manifest, const std::vector<std::string> &given_keys)
{
    check_argspec_integrity(manifest);

    bool first_keyword = false;

    const size_t n_args = manifest.size();

    std::vector<ArgMatch> match(n_args);

    std::map<std::string, size_t> key_positions;

    for (size_t ispec = 0; ispec < manifest.size(); ispec++)
    {
        key_positions.insert_or_assign(manifest[ispec].name, ispec);
    }

    for (size_t iarg = 0; iarg < given_keys.size(); iarg++)
    {
        if (iarg >= manifest.size())
        {
            throw std::runtime_error("argument list too long.");
        }

        const std::string &key = given_keys[iarg];

        first_keyword = first_keyword || !key.empty();
        if (first_keyword && key.empty())
        {
            throw std::runtime_error("keyword arguments must follow positional"
                                     " arguments in the list");
        }

        if (!first_keyword)
        {
            // positional arguments
            match[iarg].matched = true;
            match[iarg].pos = iarg;
            continue;
        }

        // keyword arguments

        auto position_it = key_positions.find(key);
        if (position_it == key_positions.end())
        {
            throw std::runtime_error(std::string("key ") + key + " not allowed");
        }
        size_t ispec = position_it->second;
        if (match[ispec].matched)
        {
            throw std::runtime_error(std::string("key ") + key
                + " declared twice at position " + std::to_string(iarg + 1));
        }
        // mark key as visited
        match[ispec].matched = true;
        match[ispec].pos = iarg;
    }

    // last pass -- we allocate defaults

    for (size_t ispec = 0; ispec < manifest.size(); ispec++)
    {
        if (match[ispec].matched)
            continue;

        // unmatched argument. allocate a default
        if (manifest[ispec].has_default())
        {
            match[ispec].deftgt = manifest[ispec].build_default();
            continue;
        }

        throw std::runtime_error(std::string("Argument ") + manifest[ispec].name
            + " required but not provided");
    }

    return match;
}

std::vector<const Value *> build_ptrs_from_match(
    const std::vector<const Value *> &given_args, const std::vector<ArgMatch> &match)
{
    const size_t n_args = match.size();

    std::vector<const Value *> args(n_args);

    for (size_t ispec = 0; ispec < n_args; ispec++)
    {
        if (match[ispec].matched)
        {
            auto iarg = match[ispec].pos;
            args[ispec] = given_args[iarg];
            continue;
        }

        args[ispec] = match[ispec].deftgt.get();
    }

    return args;
}

} // namespace aquila::interpreter
