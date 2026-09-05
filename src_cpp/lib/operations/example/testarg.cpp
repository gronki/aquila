#ifndef NDEBUG

#    include <iostream>
#    include <memory>
#    include <operation.hpp>
#    include <value.hpp>

using namespace aquila;
class TestOperation : public Operation
{
public:
    std::string name() const override { return "testing"; }
    const interpreter::ArgManifest &arg_manifest() const override
    {
        static const interpreter::ArgManifest manifest{
            ArgSpec{.name = "pos1"},
            ArgSpec{.name = "pos2"},
            ArgSpec{.name = "key1", .default_str = "key1"},
            ArgSpec{.name = "key2", .default_str = "key2"},
            ArgSpec{.name = "..."},
        };
        return manifest;
    }
    interpreter::ValuePtr run(const Str &pos1,
        const Str &pos2,
        const Str &key1,
        const Str &key2,
        std::vector<const Str *> ellipsis) const
    {
        std::cout << "pos1 = " << pos1 << std::endl
                  << "pos2 = " << pos2 << std::endl
                  << "key1 = " << key1 << std::endl
                  << "key2 = " << key2 << std::endl;
        for (auto strptr : ellipsis)
        {
            std::cout << " ... " << *strptr << std::endl;
        }
        return std::make_unique<StrValue>("hello");
    }
    BIND_ARGS(&TestOperation::run);
};
REGISTER(TestOperation);

#endif
