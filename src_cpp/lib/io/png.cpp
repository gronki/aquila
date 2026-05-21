#include "png.hpp"
#include <cstdio>
#include <png.h>
#include <stdexcept>

namespace aquila::utils
{

PngWriteWrapper::PngWriteWrapper(std::string fn)
{
    file = fopen(fn.c_str(), "wb");
    if (!file)
        throw std::runtime_error(std::string("Cannot open file to write: ") + fn);
    png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, nullptr, nullptr, nullptr);
    if (!png_ptr)
        throw std::runtime_error("creating png struct failed");
    info_ptr = png_create_info_struct(png_ptr);
    if (!info_ptr)
        throw std::runtime_error("png_info_ptr init failed");
    png_init_io(png_ptr, file);
}

PngWriteWrapper::~PngWriteWrapper()
{
    if (info_ptr && png_ptr)
    {
        png_free_data(png_ptr, info_ptr, PNG_FREE_ALL, -1);
        png_destroy_write_struct(&png_ptr, &info_ptr);
    }
    if (file)
        fclose(file);
}

template <typename T>
void collect_row(
    const View<T> &in, std::int64_t iy, std::int64_t offset, std::int64_t stride, T *row)
{
    for (std::int64_t ix = 0; ix < in.cols(); ix++)
    {
        row[offset + ix * stride] = in(ix, iy);
    }
}

template <typename T>
int bitdepth_of_type()
{

    if constexpr (std::is_same_v<T, png16pix_t>)
    {
        return 16;
    }
    else if constexpr (std::is_same_v<T, png8pix_t>)
    {
        return 8;
    }
    else
    {
        throw std::logic_error("Impossible");
    }
}

template <PngPixT T>
void PngWriteWrapper::write(View<T> data)
{
    std::int64_t nx = data.cols();
    std::int64_t ny = data.rows();

    png_set_IHDR(png_ptr,
        info_ptr,
        nx,
        ny,
        bitdepth_of_type<T>(),
        PNG_COLOR_TYPE_GRAY,
        PNG_INTERLACE_NONE,
        PNG_COMPRESSION_TYPE_DEFAULT,
        PNG_FILTER_TYPE_DEFAULT);

    png_write_info(png_ptr, info_ptr);

    std::vector<T> row(nx);
    for (std::int64_t iy = 0; iy < ny; iy++)
    {
        collect_row(data, iy, 0, 1, row.data());
        png_write_row(png_ptr, (png_bytep)row.data());
    }

    png_write_end(png_ptr, info_ptr);
}

template void PngWriteWrapper::write<png16pix_t>(View<png16pix_t>);
template void PngWriteWrapper::write<png8pix_t>(View<png8pix_t>);

template <PngPixT T>
void PngWriteWrapper::write_rgb(View<T> data_r, View<T> data_g, View<T> data_b)
{
    if (data_r.rows() != data_g.rows() || data_g.rows() != data_b.rows())
        throw std::runtime_error("Channel dimensions do not match!");
    if (data_r.cols() != data_g.cols() || data_g.cols() != data_b.cols())
        throw std::runtime_error("Channel dimensions do not match!");

    std::int64_t nx = data_r.cols();
    std::int64_t ny = data_r.rows();

    png_set_IHDR(png_ptr,
        info_ptr,
        nx,
        ny,
        bitdepth_of_type<T>(),
        PNG_COLOR_TYPE_RGB,
        PNG_INTERLACE_NONE,
        PNG_COMPRESSION_TYPE_DEFAULT,
        PNG_FILTER_TYPE_DEFAULT);

    png_write_info(png_ptr, info_ptr);

    std::vector<T> row(3 * nx);
    for (std::int64_t iy = 0; iy < ny; iy++)
    {
        collect_row(data_r, iy, 0, 3, row.data());
        collect_row(data_g, iy, 1, 3, row.data());
        collect_row(data_b, iy, 2, 3, row.data());
        png_write_row(png_ptr, (png_bytep)row.data());
    }

    png_write_end(png_ptr, info_ptr);
}

template void PngWriteWrapper::write_rgb<png16pix_t>(
    View<png16pix_t>, View<png16pix_t>, View<png16pix_t>);
template void PngWriteWrapper::write_rgb<png8pix_t>(
    View<png8pix_t>, View<png8pix_t>, View<png8pix_t>);

} // namespace aquila::utils
