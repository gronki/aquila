
#include <aquila.hpp>
#include <iostream>
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
        source_t src[100];
        register_stars_f(buf.data(), buf.rows(), buf.cols(), src, 100,
                         33, 33, 2.0, &nstar);
        cout << nstar << endl;
        for (Int i = 0; i < nstar; i++)
        {
            const auto &star = src[i];
            std::cout << star.x << ", " << star.y << std::endl;
        }
    }
    catch (std::runtime_error e)
    {
        cerr << "error: " << e.what() << endl;
        exit(1);
    }

}