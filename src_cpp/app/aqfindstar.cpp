
#include <aquila.hpp>
#include <iostream>
#include <buffer.hpp>

using namespace aquila;

int main(int argc, char** argv) {
    source_t src[100];

    Buffer<Real> buf(300, 300);

    buf.view(10, 20, 10, 20) = 1;

    Int nstar;

    register_stars_f(buf.data(), buf.rows(), buf.cols(), src, 100,
        33, 33, 2.0, &nstar);

    std::cout << nstar << std::endl;

}