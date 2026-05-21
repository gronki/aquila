#pragma once

#include <buffer/buffer.hpp>
#include <concepts>
#include <png.h>
#include <string>

namespace aquila::utils
{

using png16pix_t = png_uint_16;
using png8pix_t = png_byte;

template <typename T>
concept PngPixT = std::same_as<T, png16pix_t> || std::same_as<T, png8pix_t>;

class PngWriteWrapper
{
    FILE *file{nullptr};
    png_structp png_ptr;
    png_infop info_ptr;

public:
    PngWriteWrapper(std::string fn);
    ~PngWriteWrapper();

    template <PngPixT T>
    void write(View<T>);
    template <PngPixT T>
    void write_rgb(View<T>, View<T>, View<T>);
};

extern template void PngWriteWrapper::write<png16pix_t>(View<png16pix_t>);
extern template void PngWriteWrapper::write<png8pix_t>(View<png8pix_t>);
extern template void PngWriteWrapper::write_rgb<png16pix_t>(
    View<png16pix_t>, View<png16pix_t>, View<png16pix_t>);
extern template void PngWriteWrapper::write_rgb<png8pix_t>(
    View<png8pix_t>, View<png8pix_t>, View<png8pix_t>);

} // namespace aquila::utils
