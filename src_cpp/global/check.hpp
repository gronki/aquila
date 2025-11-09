#pragma once

#include <iostream>

#ifndef NDEBUG
#define check(cond) run_check(cond, (#cond), __FILE__, __LINE__)
#else
#define check                                                                          \
    {                                                                                  \
    }
#endif

namespace aquila
{

inline void run_check(bool condition,
                      const std::string& condition_str,
                      const std::string& sourcefile,
                      long int line)
{
    if (condition)
        return;
    std::cerr << sourcefile << ":" << line << std::endl
              << "Runtime check failed: " << condition_str << std::endl
              << std::flush;
    exit(1);
}

}