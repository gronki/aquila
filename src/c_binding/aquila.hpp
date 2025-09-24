#pragma once
#include "types.hpp"

namespace aquila
{

    constexpr Int N_SOURCE_MOMENTS = 3;

    struct source_t
    {
        Real x, y;
        Real flux = 0;
        Real rms = 0;
        Real moments[N_SOURCE_MOMENTS]{0};
    };

    extern "C"
    {
        void register_stars_f(Real *image, Int ni, Int nj, source_t *list,
                              Int limit, Int rslice, Int margin, Real kernel, Int *nstar);
    };

};
