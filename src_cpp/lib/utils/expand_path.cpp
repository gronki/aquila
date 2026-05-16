
#include <file_utils.hpp>
#include <filesystem>
#include <pattern_expansion.hpp>

namespace aquila::utils
{
std::vector<std::filesystem::path> expand_path(const std::string &path)
{
    auto expanded = utils::expand_expression(path);
    std::vector<std::filesystem::path> wildcarded;
    for (const auto &s : expanded)
    {
        auto one_wildcarded = utils::expand_wildcard(s);
        wildcarded.insert(wildcarded.end(), one_wildcarded.begin(), one_wildcarded.end());
    }
    return wildcarded;
}
} // namespace aquila::utils
