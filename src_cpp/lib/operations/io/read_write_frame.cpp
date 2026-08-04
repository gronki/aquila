#include "read_write_frame.hpp"
#include "../../io/fits.hpp"
#include "value.hpp"
#include <filesystem>
#include <utils/file_utils.hpp>

namespace aquila::ops
{

REGISTER(ReadFrame);
ValuePtr ReadFrame::run(const std::string &fn) const
{
    return std::make_unique<values::BufferValue>(read_fits(fn),
        values::FrameInfo{.fn_origin = std::filesystem::weakly_canonical(fn)});
}

interpreter::value_trace_t ReadFrame::custom_trace(
    const std::vector<ValuePtr> *args, const Value *retval) const
{
    if (args->size() < 1)
        return interpreter::value_trace_t::corrupt();
    if (auto str_val = value_cast<StrValue>((*args)[0].get()))
    {
        const auto &filepath = str_val->value;
        auto abs_path = std::filesystem::weakly_canonical(filepath);
        if (!std::filesystem::exists(abs_path))
            return "{" + std::string(abs_path) + "@###}";
        auto mod_time = std::filesystem::last_write_time(abs_path);
        return "{" + std::string(abs_path) + "@"
            + std::to_string(mod_time.time_since_epoch().count()) + "}";
    }
    return interpreter::value_trace_t::corrupt();
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
