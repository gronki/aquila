#include <algorithm>
#include <cstdio>
#include <iostream>
#include <sys/stat.h>

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

} // namespace aquila::utils