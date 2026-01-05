#include <algorithm>
#include <optional>
#include <vector>

#include "path_op.hpp"

namespace aquila::ops
{
#include <functional>
#include <iostream>
#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <vector>

using String = std::string;

struct PathElement
{
    std::string snippet;
    std::vector<std::unique_ptr<PathElement>> options;
    std::unique_ptr<PathElement> remainder;

    std::string str() const
    {
        std::stringstream ss;
        ss << snippet << "{";
        for (const auto &opt : options)
        {
            ss << opt->str() << ",";
        }
        ss << "}(" << (remainder ? remainder->str() : "END") << ")";
        return ss.str();
    }
};

using StrIter = String::const_iterator;
using EndCondition = std::function<bool(const StrIter &)>;

static std::unique_ptr<PathElement> parse_seq(
    StrIter &cur, const StrIter &end, EndCondition end_condition);

static std::vector<std::unique_ptr<PathElement>> parse_option(
    StrIter &cur, const StrIter &end)
{
    std::vector<std::unique_ptr<PathElement>> opts;

    while (true)
    {
        if (++cur == end)
            throw std::runtime_error("pattern expansion: unclosed option (1)");
        opts.push_back(parse_seq(cur,
            end,
            [end](const StrIter &it) { return it != end && (*it == ',' || *it == '}'); }));
        if (cur == end)
            throw std::runtime_error("pattern expansion: unclosed option (2)");
        if (*cur == '}')
        {
            break;
        }
    }

    return opts;
}

static std::unique_ptr<PathElement> parse_seq(
    StrIter &cur, const StrIter &end, EndCondition end_condition)
{
    auto elem = std::make_unique<PathElement>();

    StrIter start = cur;
    while (true)
    {
        if (end_condition(cur))
        {
            elem->snippet = String(start, cur);
            // std::cout << "snippet: <" << elem->snippet << ">" << std::endl;
            return elem;
        }
        if (cur == end)
            throw std::runtime_error("pattern expansion: unexpected end");
        if (*cur == '{')
        {
            elem->snippet = String(start, cur);
            // std::cout << "snippet: <" << elem->snippet << ">" << std::endl;
            elem->options = parse_option(cur, end);
            cur++;
            break;
        }
        cur++;
    }
    if (!end_condition(cur))
        elem->remainder = parse_seq(cur, end, end_condition);
    return elem;
}

static void expand_options(
    const PathElement &elem, String base, std::vector<String> &expanded)
{
    String base_snippet = base + elem.snippet;
    if (elem.options.size() == 0 && !elem.remainder)
    {
        expanded.push_back(base_snippet);
        return;
    }

    std::vector<String> expanded_options;

    for (const auto &option : elem.options)
    {
        expand_options(*option, base_snippet, expanded_options);
    }

    if (elem.remainder)
    {
        for (const auto &expanded_base : expanded_options)
        {
            expand_options(*elem.remainder, expanded_base, expanded);
        }
    }
    else
    {
        expanded.insert(expanded.end(), expanded_options.begin(), expanded_options.end());
    }
}

std::vector<String> expand_expression(const String &str)
{
    auto cur = str.begin();
    auto end = str.end();
    auto opts = parse_seq(cur, end, [end](const StrIter &it) { return it == end; });
#ifndef NDEBUG
    std::cout << opts->str() << std::endl;
#endif
    std::vector<String> expanded;
    expand_options(*opts, "", expanded);
    return expanded;
}

REGISTER(PathOp);
ValuePtr PathOp::run(const String &path) const
{
    auto expanded = expand_expression(path);
    std::vector<std::unique_ptr<Value>> items;
    items.reserve(expanded.size());
    for (const auto &s : expanded)
    {
        items.push_back(std::make_unique<StrValue>(s));
    }
    return std::make_unique<interpreter::SequenceValue>(std::move(items));
}

} // namespace aquila::ops