#include "fits.hpp"
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

Buffer<Real> aquila::read_fits(const String &filename)
{
    fitsfile *fptr = nullptr;
    int status = 0, anynul;

    fits_open_file(&fptr, filename.c_str(), READONLY, &status);
    throw_for_error(status);

    long naxes[2];
    fits_get_img_size(fptr, 2, naxes, &status);
    throw_for_error(status);

    const Int nx = naxes[0];
    const Int ny = naxes[1];

    std::vector<Real> data(nx * ny, 0.L);
    fits_read_img_dbl(fptr, 1, 1, nx * ny, 0.L, data.data(), &anynul, &status);
    throw_for_error(status);

    Buffer<Real> buf(nx, ny);
    MutableView<Real> img{buf};
    for (Int ix = 0; ix < nx; ix++)
    {
        for (Int iy = 0; iy < ny; iy++)
        {
            img(ix, iy) = data[ix + nx * iy];
        }
    }

    fits_close_file(fptr, &status);
    throw_for_error(status);

    return buf;
}

void aquila::write_fits(const String &filename, const View<Real> &img)
{

    fitsfile *fptr = nullptr;
    int status = 0;

    Int nx = img.cols();
    Int ny = img.rows();

    std::vector<Real> data(img.size(), 0.L);

    for (Int ix = 0; ix < nx; ix++)
    {
        for (Int iy = 0; iy < ny; iy++)
        {
            data[ix + nx * iy] = img(ix, iy);
        }
    }

    fits_create_diskfile(&fptr, filename.c_str(), &status);
    throw_for_error(status);

    long naxes[2] = {nx, ny};
    fits_write_imghdr(fptr, -64, 2, naxes, &status);
    throw_for_error(status);

    fits_write_img_dbl(fptr, 1, 1, nx * ny, data.data(), &status);
    throw_for_error(status);

    fits_close_file(fptr, &status);
    throw_for_error(status);
}
