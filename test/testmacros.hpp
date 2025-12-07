#include <iostream>
#include <map>
#include <sstream>
#include <stdexcept>
#include <string>

class __TestBase
{
public:
    virtual ~__TestBase() = default;
    virtual void run() const = 0;
};

static std::map<std::string, __TestBase *> __tests;

#define UNREACHABLE(messg)                                                        \
    do                                                                            \
    {                                                                             \
        std::stringstream ss;                                                     \
        ss << "Check failed (" << __FILE__ << ":" << __LINE__ << ")" << std::endl \
           << "     | " << (messg);                                               \
        throw std::runtime_error(ss.str());                                       \
    } while (false)

#define REQUIRE(cond)                                                          \
    do                                                                         \
    {                                                                          \
        if (!(cond))                                                           \
        {                                                                      \
            std::stringstream ss;                                              \
            ss << "Check failed (" << __FILE__ << ":" << __LINE__ << ")"       \
               << std::endl                                                    \
               << "     | " << (cond);                                         \
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

#define TEST(name)                                                             \
    class name##_test_cls : public __TestBase                                  \
    {                                                                          \
    public:                                                                    \
        name##_test_cls() { __tests[#name] = this; }                           \
        void run() const override;                                             \
    };                                                                         \
    static const name##_test_cls name##_test_obj;                              \
    inline void name##_test_cls::run() const

#define RUN_ALL(failed)                                                            \
    do                                                                             \
    {                                                                              \
        for (const auto &kv : __tests)                                             \
        {                                                                          \
            try                                                                    \
            {                                                                      \
                kv.second->run();                                                  \
                std::cout << "PASS " << kv.first << std::endl;                     \
            }                                                                      \
            catch (const std::runtime_error &e)                                    \
            {                                                                      \
                std::cerr << "FAIL " << kv.first << ": " << e.what() << std::endl; \
                (failed) += 1;                                                     \
            }                                                                      \
        }                                                                          \
    } while (false)