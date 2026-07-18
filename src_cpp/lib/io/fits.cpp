#include "fits.hpp"
#include <cstdint>
#include <fitsio.h>
#include <sstream>
#include <stdexcept>
#include <vector>

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

    Buffer<real_buf_t> buf(nx, ny);
    fits_read_img_flt(fptr, 1, 1, nx * ny, 0.L, buf.data(), &anynul, &status);
    throw_for_error(status);
    fits_close_file(fptr, &status);
    throw_for_error(status);

    return buf;
}

void aquila::write_fits(const std::string &filename, const Buffer<real_buf_t> &img)
{

    fitsfile *fptr = nullptr;
    int status = 0;

    std::int64_t nx = img.cols();
    std::int64_t ny = img.rows();

    fits_create_diskfile(&fptr, filename.c_str(), &status);
    throw_for_error(status);

    long naxes[2] = {nx, ny};
    fits_write_imghdr(fptr, -32, 2, naxes, &status);
    throw_for_error(status);

    fits_write_img_flt(fptr, 1, 1, nx * ny, const_cast<real_buf_t *>(img.data()), &status);
    throw_for_error(status);

    fits_close_file(fptr, &status);
    throw_for_error(status);
}
