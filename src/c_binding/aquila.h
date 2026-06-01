#pragma once

#ifdef __cplusplus
#    include <cstdlib>
extern "C"
{
#else
#    include <stdbool.h>
#    include <stdlib.h>
#endif

#define AQUILA_VERSION "260525"

#define AQ_STATUS_OK 0
#define AQ_STATUS_ERROR 1
#define AQ_STATUS_ERR_SHAPE_MISMATCH 100
#define AQ_STATUS_ERR_EMPTY_BUFFER 100

    typedef struct
    {
        int status;
        char message[128];
    } error_status_t;

    typedef float real_buf_t;

    typedef struct
    {
        double x, y;
        double ix, iy;
        double flux;
        double rms;
        double asymmetry_xy, asymmetry_uv, asymmetry;
        double kurtosis;
    } source_t;

    enum
    {
        FINDSTAR_REJECTION_ABSOLUTE = 1,
        FINDSTAR_REJECTION_RELATIVE = 2
    };

    typedef struct
    {
        int64_t rslice;
        int64_t margin;
        int64_t min_star_pixels;
        double blur_radius;
        double thresh_sd;
        int rejection;
        double max_rms;
    } findstar_params_t;

    findstar_params_t default_findstar_params();

#define TRANSFORM_MAX_PAR 16

    typedef struct
    {
        char type[16];
        double scale;
        int npar;
        double vec[TRANSFORM_MAX_PAR];
    } transform_t;

    typedef struct
    {
        double scale;
        double gravity_precision;
        double gravity_precision_pre;
        int poly_stars;
        int poly_matches;
        bool prealign_polygon;
    } align_params_t;

    align_params_t default_align_params();

    typedef struct
    {
        real_buf_t *data;
        int64_t rows;
        int64_t cols;
    } buffer_descriptor_t;

    typedef struct
    {
        const real_buf_t *data;
        int64_t rows;
        int64_t cols;
    } const_buffer_descriptor_t;

    void register_stars(const_buffer_descriptor_t,
        source_t *list,
        int64_t limit,
        const findstar_params_t *param,
        int64_t *nstar);

    void mexhakrn(double fwhm, buffer_descriptor_t);
    void gausskrn(double fwhm, buffer_descriptor_t);

    int64_t get_kernel_size(double fwhm);

    void classic_align(const source_t *lst0,
        size_t n0,
        const source_t *lst,
        size_t n,
        const char *align_method,
        const align_params_t *,
        transform_t *,
        error_status_t *);

    void conv2d_smallkernel(const_buffer_descriptor_t x,
        const_buffer_descriptor_t k,
        const char *method,
        buffer_descriptor_t y,
        bool parallel,
        int *err);

    void stack_frames(const const_buffer_descriptor_t *frames,
        int n_frames,
        const char *method,
        buffer_descriptor_t frame_out,
        error_status_t *err);

    void project_bilinear(const transform_t *vc,
        const_buffer_descriptor_t im0,
        buffer_descriptor_t im,
        double resample,
        error_status_t *err);

    typedef struct
    {
        bool background;
        bool stars;
        real_buf_t apar;
        real_buf_t bpar;
        real_buf_t sigma;
        real_buf_t sigma_star;
        int32_t niter;
        int32_t margin;
    } bkeq_param_t;

    bkeq_param_t default_bkeq_params();

    void equalize_background(const buffer_descriptor_t *buffers,
        int32_t nbuf,
        bkeq_param_t *param,
        error_status_t *err);

    void deconvol_lr(const_buffer_descriptor_t im,
        const_buffer_descriptor_t krn,
        real_buf_t strength,
        int maxiter,
        buffer_descriptor_t im2,
        bool parallel);

    void normalize_offset_gain(const const_buffer_descriptor_t *buffers,
        const buffer_descriptor_t *buffers_out,
        int n_buf,
        int margin,
        error_status_t *err);

    void find_hot(
        const_buffer_descriptor_t buf, real_buf_t sigma, buffer_descriptor_t mask_out);
    void fix_hot(buffer_descriptor_t im, const_buffer_descriptor_t mask);
    void fix_hot_light(
        const_buffer_descriptor_t in, real_buf_t sigma, buffer_descriptor_t out);

#ifdef __cplusplus
} /* extern "C" */
#endif
