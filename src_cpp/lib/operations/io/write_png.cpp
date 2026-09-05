#include "write_png.hpp"
#include "values/frame.hpp"
#include <buffer/apply.hpp>
#include <io/png.hpp>

namespace aquila::ops
{

template <typename T>
void write_png_mono(const Str &fn, const values::BufferValue &data)
{

    utils::PngWriteWrapper writer(fn);

    auto converted = apply(data.buffer.view(),
        [](float f) -> T
        {
            float clipped = f < 0. ? 0. : f > 1. ? 1. : f;
            return (T)(clipped * (T)(-1));
        });

    writer.write(converted.view());
}
template <typename T>
void write_png_rgb(const Str &fn,
    const values::BufferValue &data_r,
    const values::BufferValue &data_g,
    const values::BufferValue &data_b)
{

    utils::PngWriteWrapper writer(fn);
    auto to_bits = [](float f) -> T
    {
        float clipped = f < 0. ? 0. : f > 1. ? 1. : f;
        return (T)(clipped * (T)(-1));
    };
    auto converted_r = apply(data_r.buffer.view(), to_bits);
    auto converted_g = apply(data_g.buffer.view(), to_bits);
    auto converted_b = apply(data_b.buffer.view(), to_bits);

    writer.write_rgb(converted_r.view(), converted_g.view(), converted_b.view());
}

REGISTER(WritePng);
ValuePtr WritePng::run(
    const values::BufferValue &in_mono, const Str &fn, const Real &bits_) const
{
    int bits = (int)bits_;
    if (bits == 8)
    {
        write_png_mono<utils::png8pix_t>(fn, in_mono);
    }
    else if (bits == 16)
    {
        write_png_mono<utils::png16pix_t>(fn, in_mono);
    }
    else
        throw std::runtime_error("BITS In png must be 8 or 16!");
    return in_mono.clone();
}

const ArgManifest &WritePng::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "data"},
        ArgSpec{.name = "filename"},
        ArgSpec{.name = "bits", .default_real = 8},
    };
    return manifest;
}

std::string WritePng::description() const
{
    return "";
}

REGISTER(WritePngRGB);
ValuePtr WritePngRGB::run(const values::BufferValue &in_r,
    const values::BufferValue &in_g,
    const values::BufferValue &in_b,
    const Str &fn,
    const Real &bits_) const
{
    int bits = (int)bits_;
    if (bits == 8)
    {
        write_png_rgb<utils::png8pix_t>(fn, in_r, in_g, in_b);
    }
    else if (bits == 16)
    {
        write_png_rgb<utils::png16pix_t>(fn, in_r, in_g, in_b);
    }
    else
        throw std::runtime_error("BITS In png must be 8 or 16!");
    std::vector<ValuePtr> rgb(3);
    rgb[0] = in_r.clone();
    rgb[1] = in_g.clone();
    rgb[2] = in_b.clone();
    return std::make_unique<SequenceValue>(std::move(rgb));
}

const ArgManifest &WritePngRGB::arg_manifest() const
{
    static const ArgManifest manifest{
        ArgSpec{.name = "r"},
        ArgSpec{.name = "g"},
        ArgSpec{.name = "b"},
        ArgSpec{.name = "filename"},
        ArgSpec{.name = "bits", .default_real = 8},
    };
    return manifest;
}

std::string WritePngRGB::description() const
{
    return "";
}
} // namespace aquila::ops
