#pragma once

#include <string>
#include <tuple>

namespace aquila::utils
{

struct split_fn_t
{
    split_fn_t(const std::string &fn);
    std::string base;
    std::string ext;
};

std::string free_filename(const std::string &);

} // namespace aquila::utils