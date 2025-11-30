#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>

inline std::string _build_err_msg(const std::string &cond,
                                  const std::string &file,
                                  int line)
{
    std::stringstream ss;
    ss << "Check failed (" << file << ":" << line << ")" << std::endl
       << "\t" << cond;
    return ss.str();
}

#define REQUIRE(cond)                                                            \
    do                                                                           \
    {                                                                            \
        if (!(cond))                                                             \
            throw std::runtime_error(_build_err_msg(#cond, __FILE__, __LINE__)); \
    } while (false)

#define RUN(test)                                                              \
    do                                                                         \
    {                                                                          \
        try                                                                    \
        {                                                                      \
            (test)();                                                          \
        }                                                                      \
        catch (const std::runtime_error &e)                                    \
        {                                                                      \
            std::cerr << "FAIL (" << #test << "): " << e.what() << std::endl;  \
            failed += 1;                                                       \
        }                                                                      \
    } while (false)