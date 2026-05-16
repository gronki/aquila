#pragma once

#include <filesystem>
#include <string>
#include <tuple>
#include <vector>

namespace aquila::utils
{

struct split_fn_t
{
    split_fn_t(const std::string &fn);
    std::string base;
    std::string ext;
};

std::string free_filename(const std::string &);

struct simple_pattern_t
{
    std::string prefix;
    std::string suffix;
    bool exact;

    simple_pattern_t(const std::string &p);
    bool matches(const std::string &s) const;
};

std::vector<std::filesystem::path> expand_wildcard(std::filesystem::path in_path);

} // namespace aquila::utils
