#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <tuple>
#include <vector>

class __TestBase
{
    std::string testname;

public:
    __TestBase(const std::string &testname) : testname(testname) {}
    const std::string &getname() const { return testname; }
    virtual ~__TestBase() = default;
    virtual void run() const = 0;
};

static std::vector<__TestBase *> __tests;

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

#define REQUIRE_NNUL(a)                                                        \
    do                                                                         \
    {                                                                          \
        if (!(a))                                                              \
        {                                                                      \
            std::stringstream ss;                                              \
            ss << "Check failed (" << __FILE__ << ":" << __LINE__ << ")"       \
               << std::endl                                                    \
               << "     | " << #a << " is NULL " << std::endl;                 \
            throw std::runtime_error(ss.str());                                \
        }                                                                      \
    } while (false)

#define TEST(name)                                                             \
    class name##__test_cls__ : public __TestBase                               \
    {                                                                          \
    public:                                                                    \
        name##__test_cls__() : __TestBase(#name) { __tests.push_back(this); }  \
        void run() const override;                                             \
    };                                                                         \
    const name##__test_cls__ name##__test_obj__;                               \
    void name##__test_cls__::run() const

#define RUN_ALL(num_failed)                                                    \
    do                                                                         \
    {                                                                          \
        for (const __TestBase *test : __tests)                                 \
        {                                                                      \
            try                                                                \
            {                                                                  \
                std::cout << "running test: " << test->getname() << std::endl; \
                test->run();                                                   \
                std::cout << "  *** PASS " << test->getname() << " ***"        \
                          << std::endl;                                        \
            }                                                                  \
            catch (const std::runtime_error &e)                                \
            {                                                                  \
                std::cerr << "  *** FAIL " << test->getname() << " *** "       \
                          << std::endl                                         \
                          << e.what() << std::endl;                            \
                (num_failed) += 1;                                             \
            }                                                                  \
        }                                                                      \
    } while (false)