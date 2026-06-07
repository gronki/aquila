#include "read_write_frame.hpp"
#include "../../io/fits.hpp"
#include <filesystem>
#include <utils/file_utils.hpp>

namespace aquila::ops
{

REGISTER(ReadFrame);
ValuePtr ReadFrame::run(const std::string &fn) const
{
    return std::make_unique<values::BufferValue>(
        read_fits(fn), values::FrameInfo{.fn_origin = fn});
}

REGISTER(WriteFrame);
ValuePtr WriteFrame::run(const values::BufferValue &frame, const std::string &fn) const
{
    if (std::filesystem::exists(fn))
        std::filesystem::remove(fn);
    write_fits(fn, frame.buffer);
    return nullptr;
}

} // namespace aquila::ops
