#include <map>
#include <set>
#include <sstream>

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

static void check_argspec_integrity(
    const std::vector<ArgSpec> &manifest, size_t &num_positionals, bool &has_ellipsis)
{
    bool first_keyword = false;
    has_ellipsis = false;
    int iarg = 1;

    num_positionals = 0;

    for (const auto &argspec : manifest)
    {
        if (argspec.name.empty())
        {
            throw std::invalid_argument(
                std::string("Argument name must not be empty in argument"
                            "manifest at position ")
                + std::to_string(iarg));
        }

        const bool is_keyword = argspec.has_default();
        first_keyword = first_keyword || is_keyword;

        const bool is_ellipsis = argspec.name == ARG_ELLIPSIS;
        has_ellipsis = has_ellipsis || is_ellipsis;

        const bool is_positional = !is_keyword && !is_ellipsis;

        if (is_positional)
            num_positionals += 1;

        if (is_ellipsis)
        {
            if (is_keyword)
                throw std::invalid_argument(
                    "Ellipsis ... may not have default argument.");
        }
        else if (has_ellipsis)
        {
            throw std::invalid_argument(
                "Ellipsis ... must be at the end of the ArgSpec.");
        }
        else if (!valid_argspec_key(argspec.name))
        {
            throw std::invalid_argument(std::string("Argument name in position ")
                + std::to_string(iarg) + " is invalid: " + argspec.name);
        }

        if (first_keyword && is_positional)
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
    size_t num_positionals = 0;
    bool expect_ellipsis = false;
    check_argspec_integrity(manifest, num_positionals, expect_ellipsis);

    std::cout << "num_positionals =" << num_positionals << std::endl;

    const size_t n_given = given_keys.size();
    const size_t n_spec = expect_ellipsis ? manifest.size() - 1 : manifest.size();
    std::vector<ArgMatch> match(n_spec);

    std::map<std::string, size_t> argspec_key_positions;

    for (size_t ispec = 0; ispec < manifest.size(); ispec++)
    {
        argspec_key_positions.insert_or_assign(manifest[ispec].name, ispec);
    }

    for (size_t ikey = 0; ikey < given_keys.size(); ikey++)
    {
        const std::string &key = given_keys[ikey];

        if (key.empty())
            continue;

        auto position_it = argspec_key_positions.find(key);
        if (position_it == argspec_key_positions.end())
            throw std::runtime_error(std::string("key: ") + key
                + " not allowed at position: " + std::to_string(ikey + 1));

        int match_pos = position_it->second;

        if (match[match_pos].matched)
        {
            throw std::runtime_error(std::string("key ") + key
                + " declared twice at position " + std::to_string(ikey + 1));
        }

        // mark key as visited
        match[match_pos].matched = true;
        match[match_pos].pos = ikey;
    }

    bool first_keyword = false;
    size_t iarg_skip = 0;

    for (size_t iarg = 0; iarg < given_keys.size(); iarg++)
    {
        if (!expect_ellipsis && iarg >= n_spec)
        {
            throw std::runtime_error("argument list too long.");
        }

        const bool is_keyword = !given_keys[iarg].empty();
        first_keyword = first_keyword || is_keyword;

        if (first_keyword)
        {
            if (!is_keyword)
                throw std::runtime_error("keyword arguments must follow positional"
                                         " arguments in the list");
            continue;
        }

        // positional arguments

        while (iarg + iarg_skip < n_spec && match[iarg + iarg_skip].matched)
            iarg_skip++;

        const size_t iarg_dest = iarg + iarg_skip;

        if (iarg_dest < n_spec)
        {
            // named
            match[iarg_dest].matched = true;
            match[iarg_dest].pos = iarg;
        }
        else if (expect_ellipsis)
        {
            // ellipsis
            match.push_back(ArgMatch{.matched = true, .pos = iarg});
        }
        else
        {
            throw std::runtime_error(
                std::string("Too many positional arguments, maximum allowed: ")
                + std::to_string(num_positionals));
        }
    }

#ifndef NDEBUG
    std::cout << "size(match)" << match.size() << std::endl;
    std::cout << "size(manifest)" << manifest.size() << std::endl;
#endif

    // last pass -- we allocate defaults and assign sanitizers

    for (size_t imatch = 0; imatch < match.size(); imatch++)
    {
        match[imatch].sanitizer_factory =
            manifest[std::min(imatch, manifest.size() - 1)].convert;
        match[imatch].sequence =
            manifest[std::min(imatch, manifest.size() - 1)].sequence;

        if (match[imatch].matched)
            continue;

        // unmatched argument. allocate a default
        if (manifest[imatch].has_default())
        {
            match[imatch].deftgt = manifest[imatch].build_default();
            continue;
        }

        throw std::runtime_error(std::string("Argument ") + manifest[imatch].name
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

std::string Operation::signature_str() const
{
    std::stringstream ss;

    ss << name() << "(";
    auto manifest = arg_manifest();
    bool first = true;
    for (const auto &argspec : manifest)
    {
        if (first)
        {
            first = false;
        }
        else
        {
            ss << ", ";
        }
        if (argspec.sequence) {
            ss << "[" << argspec.name << ", ...]";
        } else {
            ss << argspec.name;
        }
        if (argspec.has_default())
        {
            auto def = argspec.build_default();
            ss << ": " << def->str();
        }
    }
    ss << ")";
    return ss.str();
}

} // namespace aquila::interpreter
