
#include <aquila.hpp>
#include <iostream>
#include <iomanip>
#include <buffer.hpp>
#include <fits.hpp>
#include <cmath>

using namespace aquila;
using std::cout, std::cerr, std::endl;

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

        Int nstar;
        const Int max_stars = 256;
        source_t src[max_stars];
        findstar_param_t param;
        register_stars_f(buf.data(), buf.rows(), buf.cols(), src, max_stars, &param, &nstar);
        cout << nstar << endl;
        for (Int i = 0; i < nstar; i++)
        {
            const auto &star = src[i];
            std::cout
                << std::fixed << std::setprecision(2)
                << std::setw(12)
                << star.ix
                << std::setw(12)
                << star.iy
                << std::setw(12)
                << star.flux
                << std::setw(12)
                << star.rms
                << std::setw(12)
                << star.asymmetry
                << std::endl;
        }
    }
    catch (std::runtime_error e)
    {
        cerr << "error: " << e.what() << endl;
        exit(1);
    }
}