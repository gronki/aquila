#include "read_write_frame.hpp"
#include "../../io/fits.hpp"
#include "../../utils/file_utils.hpp"

namespace aquila::ops
{

REGISTER(ReadFrame);
ValuePtr ReadFrame::run(const std::string &fn) const
{
    return std::make_unique<values::BufferValue>(read_fits(fn));
}

REGISTER(WriteFrame);
ValuePtr WriteFrame::run(const values::BufferValue &frame, const std::string &fn) const
{
    write_fits(utils::free_filename(fn), frame.buffer);
    return nullptr;
}

} // namespace aquila::ops