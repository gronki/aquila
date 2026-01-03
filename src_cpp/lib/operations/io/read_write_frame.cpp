#include "read_write_frame.hpp"
#include "../../io/fits.hpp"

namespace aquila::ops
{

std::unique_ptr<Value> ReadFrame::run(const String &fn) const
{
    return std::make_unique<values::BufferValue>(read_fits(fn));
}

std::unique_ptr<Value> WriteFrame::run(const values::BufferValue &frame, const String &fn) const
{
    write_fits(fn, frame.buffer);
    return nullptr;
}

} // namespace aquila::ops