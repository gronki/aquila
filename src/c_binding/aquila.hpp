#pragma once

#include "../../src_cpp/global/types.hpp"
#include "../../src_cpp/lib/buffer/buffer.hpp"

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

struct buffer_descriptor_t
{
    buffer_descriptor_t(Buffer<Real> &buf) :
        data(buf.data()), rows(buf.rows()), cols(buf.cols())
    {
    }
    Real *data;
    Int rows;
    Int cols;
};

struct const_buffer_descriptor_t
{
    const_buffer_descriptor_t(const Buffer<Real> &buf) :
        data(buf.data()), rows(buf.rows()), cols(buf.cols())
    {
    }
    const Real *data;
    Int rows;
    Int cols;
};

extern "C"
{
    void register_stars(const const_buffer_descriptor_t&,
        source_t *list,
        Int limit,
        const findstar_param_t &param,
        Int &nstar);

    void mexhakrn(Real fwhm, const buffer_descriptor_t &);
    void gausskrn(Real fwhm, const buffer_descriptor_t &);
    
    Int get_kernel_size(Real fwhm);
};

}; // namespace aquila
