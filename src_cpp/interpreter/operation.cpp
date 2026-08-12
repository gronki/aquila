#include <map>
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

manifest_properties_t::manifest_properties_t(const std::vector<ArgSpec> &manifest)
{
    bool first_keyword = false;
    has_ellipsis = false;
    int iarg = 1;

    num_positionals = 0;
    num_keyword = 0;

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

        if (is_keyword)
            num_keyword += 1;

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

std::vector<ArgMatch> match_arguments(const std::vector<ArgSpec> &manifest,
    const manifest_properties_t &props,
    const std::vector<std::string> &given_keys)
{
    const size_t n_spec = props.has_ellipsis ? manifest.size() - 1 : manifest.size();
    std::vector<ArgMatch> match(n_spec);

    std::map<std::string, size_t> argspec_key_positions;

    for (size_t ispec = 0; ispec < manifest.size(); ispec++)
    {
        argspec_key_positions.insert_or_assign(manifest[ispec].name, ispec);
    }

    bool first_keyword = false;
    // bool any_expansion = false;
    // std::ptrdiff_t pos_shift = 0;

    for (size_t iarg = 0; iarg < given_keys.size(); iarg++)
    {
        if (!props.has_ellipsis && iarg >= n_spec)
        {
            throw std::runtime_error("argument list too long.");
        }

        const std::string &key = given_keys[iarg];
        /* const bool is_expansion = !key.empty() && key == std::string(1, EXPAND_DELIM);
         if (is_expansion)
             pos_shift -= 1;
         any_expansion = any_expansion || is_expansion; */
        const bool is_keyword = !key.empty() /* &&  !is_expansion */;
        first_keyword = first_keyword || is_keyword;

        if (first_keyword && !is_keyword)
            throw std::runtime_error("keyword arguments must follow positional"
                                     " arguments in the list");

        // keyword arguments

        if (is_keyword)
        {

            auto position_it = argspec_key_positions.find(key);
            if (position_it == argspec_key_positions.end())
                throw std::runtime_error(std::string("key: ") + key
                    + " not allowed at position: " + std::to_string(iarg + 1));

            int match_pos = position_it->second;
            if (!manifest[match_pos].has_default())
                throw std::runtime_error(std::string("argument ") + key
                    + " is positional; shall not be defined by key at position "
                    + std::to_string(iarg + 1));
            if (match[match_pos].matched)
            {
                throw std::runtime_error(std::string("key ") + key
                    + " declared twice at position " + std::to_string(iarg + 1));
            }

            // mark key as visited
            match[match_pos].matched = true;
            match[match_pos].pos = iarg;

            // positional arguments
        }
        else // if (!is_expansion)
        {
            auto iarg_corrected = iarg /* + pos_shift */;
            if (iarg_corrected < props.num_positionals)
            {
                // named
                match[iarg_corrected].matched = true;
                match[iarg_corrected].pos = iarg;
            }
            else if (props.has_ellipsis)
            {
                // ellipsis
                match.push_back(ArgMatch{.matched = true, .pos = iarg_corrected});
            }
            else
            {
                throw std::runtime_error(
                    std::string("Too many positional arguments, maximum allowed: ")
                    + std::to_string(props.num_positionals));
            }
        }
    }

#ifndef NDEBUG
    std::cout << "size(match)" << match.size() << std::endl;
    std::cout << "size(manifest)" << manifest.size() << std::endl;
#endif

    // last pass -- we allocate defaults and assign sanitizers

    for (size_t imatch = 0; imatch < match.size(); imatch++)
    {
        match[imatch].convert = manifest[std::min(imatch, manifest.size() - 1)].convert;
        match[imatch].sequence = manifest[std::min(imatch, manifest.size() - 1)].sequence;

        if (match[imatch].matched)
            continue;

        // unmatched argument. allocate a default
        if (manifest[imatch].has_default())
        {
            match[imatch].deftgt = manifest[imatch].build_default();
            continue;
        }

        // if (!any_expansion)
        throw std::runtime_error(std::string("Argument ") + manifest[imatch].name
            + " required but not provided");
    }

    return match;
}

std::vector<ValuePtr> build_ptrs_from_match(
    std::vector<ValuePtr> &given_args, const std::vector<ArgMatch> &match)
{
    const size_t n_args = match.size();

    std::vector<ValuePtr> args(n_args);

    for (size_t ispec = 0; ispec < n_args; ispec++)
    {
        if (match[ispec].matched)
        {
            auto iarg = match[ispec].pos;
            args[ispec] = std::move(given_args[iarg]);
            continue;
        }
        if (match[ispec].deftgt)
        {
            args[ispec] = match[ispec].deftgt.get();
            continue;
        }
        std::cout << "Warning! empty argument " << ispec << std::endl;
    }

    return args;
}
std::vector<value_trace_t> build_traces_from_match(
    const std::vector<value_trace_t> &traces, const std::vector<ArgMatch> &match)
{
    const size_t n_args = match.size();

    std::vector<value_trace_t> out_traces(n_args);

    for (size_t ispec = 0; ispec < n_args; ispec++)
    {
        if (match[ispec].matched)
        {
            auto iarg = match[ispec].pos;
            out_traces[ispec] = traces[iarg];
            continue;
        }
        if (match[ispec].deftgt)
        {
            out_traces[ispec] = match[ispec].deftgt->get_trace();
            continue;
        }
        std::cout << "Warning! empty argument " << ispec << std::endl;
    }

    return out_traces;
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
        if (argspec.sequence)
        {
            ss << "[" << argspec.name << "]";
        }
        else
        {
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
