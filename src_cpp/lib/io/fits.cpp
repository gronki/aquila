#include "fits.hpp"
#include <fitsio.h>
#include <sstream>
#include <stdexcept>
#include <vector>
#include <cstdint>

using namespace aquila;

static void __throw_for_error(int status, const char *file, int line)
{
    if (status)
    {
        char status_str[FLEN_STATUS];
        fits_get_errstatus(status, status_str);

        std::stringstream errtext;
        errtext << "FITSIO error (" << file << ":" << line << "): " << status_str;
        throw std::runtime_error(errtext.str());
    }
}

#define throw_for_error(status) __throw_for_error((status), __FILE__, __LINE__)

Buffer<real_buf_t> aquila::read_fits(const std::string &filename)
{
    fitsfile *fptr = nullptr;
    int status = 0, anynul;

    fits_open_file(&fptr, filename.c_str(), READONLY, &status);
    throw_for_error(status);

    long naxes[2];
    fits_get_img_size(fptr, 2, naxes, &status);
    throw_for_error(status);

    const std::int64_t nx = naxes[0];
    const std::int64_t ny = naxes[1];

    std::vector<real_buf_t> data(nx * ny, 0.L);
    fits_read_img_flt(fptr, 1, 1, nx * ny, 0.L, data.data(), &anynul, &status);
    throw_for_error(status);

    Buffer<real_buf_t> buf(nx, ny);
    MutableView<real_buf_t> img{buf};
    for (std::int64_t ix = 0; ix < nx; ix++)
    {
        for (std::int64_t iy = 0; iy < ny; iy++)
        {
            img(ix, iy) = data[ix + nx * iy];
        }
    }

    fits_close_file(fptr, &status);
    throw_for_error(status);

    return buf;
}

void aquila::write_fits(const std::string &filename, const View<real_buf_t> &img)
{

    fitsfile *fptr = nullptr;
    int status = 0;

    std::int64_t nx = img.cols();
    std::int64_t ny = img.rows();

    std::vector<real_buf_t> data(img.size(), 0.L);

    for (std::int64_t ix = 0; ix < nx; ix++)
    {
        for (std::int64_t iy = 0; iy < ny; iy++)
        {
            data[ix + nx * iy] = img(ix, iy);
        }
    }

    fits_create_diskfile(&fptr, filename.c_str(), &status);
    throw_for_error(status);

    long naxes[2] = {nx, ny};
    fits_write_imghdr(fptr, -32, 2, naxes, &status);
    throw_for_error(status);

    fits_write_img_flt(fptr, 1, 1, nx * ny, data.data(), &status);
    throw_for_error(status);

    fits_close_file(fptr, &status);
    throw_for_error(status);
}
