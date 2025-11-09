#pragma once
#include "types.hpp"

namespace aquila
{

constexpr Int N_SOURCE_MOMENTS = 3;

struct source_t
{
    Real x, y;
    Real ix, iy;
    Real flux = 0;
    Real rms = 0;
    Real asymmetry_xy, asymmetry_uv, asymmetry;
    Real kurtosis;
};

static const int FINDSTAR_REJECTION_ABSOLUTE = 1, FINDSTAR_REJECTION_RELATIVE = 2;

struct findstar_param_t
{
    Int rslice = 16;
    Int margin = 32;
    Int min_star_pixels = 8;
    Real blur_radius = 2.3;
    Real thresh_sd = 2.;
    int rejection = FINDSTAR_REJECTION_ABSOLUTE;
    Real max_rms = 12.;
};

extern "C"
{
    void register_stars_f(Real* image,
                          Int ni,
                          Int nj,
                          source_t* list,
                          Int limit,
                          const findstar_param_t* param,
                          Int* nstar);
};

};
