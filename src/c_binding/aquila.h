#pragma once

#ifdef __cplusplus
#    include <cstdlib>
extern "C"
{
#else
#    include <stdlib.h>
#endif

    #define AQUILA_VERSION "260109"

    typedef float real_buf_t;

    const size_t N_SOURCE_MOMENTS = 3;

    struct source_t
    {
        double x, y;
        double ix, iy;
        double flux = 0;
        double rms = 0;
        double asymmetry_xy, asymmetry_uv, asymmetry;
        double kurtosis;
    };

    enum
    {
        FINDSTAR_REJECTION_ABSOLUTE = 1,
        FINDSTAR_REJECTION_RELATIVE = 2
    };

    struct findstar_param_t
    {
        int64_t rslice = 16;
        int64_t margin = 32;
        int64_t min_star_pixels = 8;
        double blur_radius = 2.3;
        double thresh_sd = 2.;
        int rejection = FINDSTAR_REJECTION_ABSOLUTE;
        double max_rms = 12.;
    };

    constexpr int TRANSFORM_MAX_PAR = 16;

    struct transform_t
    {
        char type[16];
        double scale = 1;
        int npar = 0;
        double vec[TRANSFORM_MAX_PAR]{0};
    };

    struct buffer_descriptor_t
    {
        real_buf_t *data;
        int64_t rows;
        int64_t cols;
    };

    struct const_buffer_descriptor_t
    {
        const real_buf_t *data;
        int64_t rows;
        int64_t cols;
    };

    void register_stars(const const_buffer_descriptor_t &,
        source_t *list,
        int64_t limit,
        const findstar_param_t &param,
        int64_t &nstar);

    void mexhakrn(double fwhm, const buffer_descriptor_t &);
    void gausskrn(double fwhm, const buffer_descriptor_t &);

    int64_t get_kernel_size(double fwhm);

    void classic_align(const source_t *lst0,
        size_t n0,
        const source_t *lst,
        size_t n,
        const char *align_method,
        transform_t &,
        int &);

#ifdef __cplusplus
} /* extern "C" */
#endif
