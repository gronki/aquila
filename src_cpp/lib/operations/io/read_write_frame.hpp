#include "../../interpreter/interpreter.hpp"
#include "../../values/frame.hpp"

namespace aquila::ops
{

struct ReadFrame : public Operation
{
    BIND_ARGS(&ReadFrame::run);
    std::unique_ptr<Value> run(const String &fn) const;

    std::optional<ArgManifest> arg_manifest() const
    {
        return ArgManifest{ArgSpec{.name = "filename"}};
    }
    std::string name() const { return "file"; }
};

REGISTER(ReadFrame);

struct WriteFrame : public Operation
{
    BIND_ARGS(&WriteFrame::run);
    std::unique_ptr<Value> run(const values::BufferValue &frame, const String &fn) const;

    std::optional<ArgManifest> arg_manifest() const
    {
        return ArgManifest{
            ArgSpec{.name = "frame"},
            ArgSpec{.name = "filename"},
        };
    }
    std::string name() const { return "save"; }
};

REGISTER(WriteFrame);

} // namespace aquila::ops