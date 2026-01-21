
#include <aquila.h>
#include <buffer.hpp>
#include <cmath>
#include <fits.hpp>
#include <iomanip>
#include <iostream>

using namespace aquila;
using std::cout, std::cerr, std::endl;


static buffer_descriptor_t c_buf(Buffer<real_buf_t> &buf)
{
    return {buf.data(), buf.rows(), buf.cols()};
}

static const_buffer_descriptor_t c_const_buf(const Buffer<real_buf_t> &buf)
{
    return {buf.data(), buf.rows(), buf.cols()};
}

int main(int argc, char **argv)
{

    if (argc != 2)
    {
        cout << "usage: aqfindstar <file_in>" << endl;
        exit(1);
    }

    try
    {
        auto buf = read_fits(argv[1]);

        std::int64_t nstar;
        const std::int64_t max_stars = 256;
        source_t src[max_stars];
        findstar_param_t param;
        register_stars(c_const_buf(buf), src, max_stars, &param, &nstar);
        cout << nstar << endl;
        for (std::int64_t i = 0; i < nstar; i++)
        {
            const auto &star = src[i];
            std::cout << std::fixed << std::setprecision(2) << std::setw(12)
                      << star.ix << std::setw(12) << star.iy << std::setw(12)
                      << star.flux << std::setw(12) << star.rms << std::setw(12)
                      << star.asymmetry << std::endl;
        }
    }
    catch (std::runtime_error e)
    {
        cerr << "error: " << e.what() << endl;
        exit(1);
    }
}