#include <algorithm>
#include <cstdio>
#include <filesystem>
#include <iostream>
#include <sys/stat.h>
#include <vector>

#include "file_utils.hpp"

namespace aquila::utils
{

split_fn_t::split_fn_t(const std::string &fn)
{
    auto it_dot = std::find(fn.crbegin(), fn.crend(), '.');
    if (it_dot == fn.crend())
    {
        base = fn;
        ext = "";
        return;
    }
    it_dot++;
    base = std::string(fn.begin(), it_dot.base());
    ext = std::string(it_dot.base(), fn.end());
}

class fn_incrementer_t
{
    split_fn_t split;
    int counter = -1;

public:
    fn_incrementer_t(const std::string &fn) : split(fn) {}
    std::string next()
    {
        if (++counter >= 1)
            return split.base + "_" + std::to_string(counter) + split.ext;
        return split.base + split.ext;
    }
};

static bool file_exists(const std::string &fn)
{
    struct stat buffer;
    return stat(fn.c_str(), &buffer) == 0;
}

static time_t file_creation(const std::string &fn)
{
    struct stat buffer;
    if (stat(fn.c_str(), &buffer))
        return 0;
    return buffer.st_ctim.tv_sec;
}

std::string free_filename(const std::string &filename)
{
    fn_incrementer_t gen(filename);
    std::string free_fn;
    do
    {
        free_fn = gen.next();
#ifndef NDEBUG
        std::cout << "trying: " << free_fn << std::endl;
#endif
    } while (file_exists(free_fn));
    return free_fn;
}

simple_pattern_t::simple_pattern_t(const std::string &p)
{
    auto star = p.find('*');
    if (star == std::string::npos)
    {
        exact = true;
        prefix = p;
#ifndef NDEBUG
        std::cout << " built pattern  =" << prefix << std::endl;
#endif
        return;
    }

    exact = false;
    prefix = std::string(p, 0, star);
    suffix = std::string(p, star + 1);
#ifndef NDEBUG
    std::cout << " built pattern =" << prefix << "..." << suffix << std::endl;
#endif
}

bool simple_pattern_t::matches(const std::string &s) const
{
    if (exact)
    {
        return s == prefix;
    }
    if (s.size() < prefix.size() || s.substr(0, prefix.size()) != prefix)
        return false;
    if (s.size() < suffix.size() || s.substr(s.size() - suffix.size()) != suffix)
        return false;

    return true;
}

std::vector<std::filesystem::path> expand_wildcard_(std::filesystem::path parent,
    std::filesystem::path::iterator path_it,
    const std::filesystem::path::iterator &end)
{

    /*
     * Written during friend trip to Beatka's house on 16.05.2026 with Marcin and Fatima
     */

    if (path_it == end)
        return {parent};

#ifndef NDEBUG
    std::cout << " expanding from dir: " << parent << " pattern: " << *path_it << std::endl;
#endif

    simple_pattern_t matcher(*path_it);

    // if no wildcards were found in this level, we simply go one level further
    if (matcher.exact)
    {
        auto next_parent = parent / *path_it;
#ifndef NDEBUG
        std::cout << " simple: " << next_parent << std::endl;
#endif
        return expand_wildcard_(next_parent, ++path_it, end);
    }

    if (!std::filesystem::is_directory(parent))
        return {};

    std::vector<std::filesystem::path> matches;
    auto next_it = ++path_it;
    for (const auto &p : std::filesystem::directory_iterator(parent))
    {
        const auto &ppath = p.path();
#ifndef NDEBUG
        std::cout << " testing: " << ppath << std::endl;
#endif
        if (matcher.matches(ppath.filename()))
        {
#ifndef NDEBUG
            std::cout << "  *** match! " << ppath << std::endl;
#endif
            auto child_results = expand_wildcard_(ppath, next_it, end);
            matches.insert(matches.end(), child_results.begin(), child_results.end());
        }
    }

    return matches;
}

std::vector<std::filesystem::path> expand_wildcard(std::filesystem::path in_path)
{
    return expand_wildcard_(in_path.has_root_path() ? in_path.root_path()
            : in_path.has_parent_path()             ? std::filesystem::path{}
                                                    : std::filesystem::path{"."},
        in_path.begin(),
        in_path.end());
}

} // namespace aquila::utils
