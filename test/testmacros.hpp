#include <iostream>
#include <sstream>
#include <stdexcept>

#define REQUIRE(cond)                                                          \
    do                                                                         \
    {                                                                          \
        if (!(cond))                                                           \
        {                                                                      \
            std::stringstream ss;                                              \
            ss << "Check failed (" << file << ":" << line << ")" << std::endl  \
               << "     | " << cond;                                           \
            throw std::runtime_error(ss.str());                                \
        }                                                                      \
    } while (false)

#define REQUIRE_EQ(a, b)                                                       \
    do                                                                         \
    {                                                                          \
        if (!((a) == (b)))                                                     \
        {                                                                      \
            std::stringstream ss;                                              \
            ss << "Check failed (" << __FILE__ << ":" << __LINE__ << ")"       \
               << std::endl                                                    \
               << "     | " << #a << " == " << #b << std::endl                 \
               << " but | " << (a) << " != " << (b) << std::endl;              \
            throw std::runtime_error(ss.str());                                \
        }                                                                      \
    } while (false)

#define REQUIRE_NEQ(a, b)                                                      \
    do                                                                         \
    {                                                                          \
        if ((a) == (b))                                                        \
        {                                                                      \
            std::stringstream ss;                                              \
            ss << "Check failed (" << __FILE__ << ":" << __LINE__ << ")"       \
               << std::endl                                                    \
               << "     | " << #a << " != " << #b << std::endl                 \
               << " but | " << (a) << " == " << (a) << std::endl;              \
            throw std::runtime_error(ss.str());                                \
        }                                                                      \
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