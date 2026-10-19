#include <stddef.h>
#ifdef __cplusplus
#include <complex>
#define __GFORTRAN_FLOAT_COMPLEX std::complex<float>
#define __GFORTRAN_DOUBLE_COMPLEX std::complex<double>
#define __GFORTRAN_LONG_DOUBLE_COMPLEX std::complex<long double>
extern "C" {
#else
#define __GFORTRAN_FLOAT_COMPLEX float _Complex
#define __GFORTRAN_DOUBLE_COMPLEX double _Complex
#define __GFORTRAN_LONG_DOUBLE_COMPLEX long double _Complex
#endif

#include <ISO_Fortran_binding.h>

typedef struct source_t {
    double x /* WARNING: Converting 'REAL(8)' to interoperable type */;
    double y /* WARNING: Converting 'REAL(8)' to interoperable type */;
    double ix /* WARNING: Converting 'REAL(8)' to interoperable type */;
    double iy /* WARNING: Converting 'REAL(8)' to interoperable type */;
    double flux /* WARNING: Converting 'REAL(8)' to interoperable type */;
    double rms /* WARNING: Converting 'REAL(8)' to interoperable type */;
    double asymmetry_xy /* WARNING: Converting 'REAL(8)' to interoperable type */;
    double asymmetry_uv /* WARNING: Converting 'REAL(8)' to interoperable type */;
    double asymmetry /* WARNING: Converting 'REAL(8)' to interoperable type */;
    double kurtosis /* WARNING: Converting 'REAL(8)' to interoperable type */;
} source_t;

typedef struct error_status_t {
    int status /* WARNING: Converting 'INTEGER(4)' to interoperable type */;
    char message[128];
} error_status_t;

typedef struct buffer_descriptor_r32_t {
    void *data;
    long rows /* WARNING: Converting 'INTEGER(8)' to interoperable type */;
    long cols /* WARNING: Converting 'INTEGER(8)' to interoperable type */;
} buffer_descriptor_r32_t;

typedef struct bkgradient_model_t {
    float a /* WARNING: Converting 'REAL(4)' to interoperable type */;
    float b /* WARNING: Converting 'REAL(4)' to interoperable type */;
    float c /* WARNING: Converting 'REAL(4)' to interoperable type */;
} bkgradient_model_t;

typedef struct box_sample_param_t {
    long seed /* WARNING: Converting 'INTEGER(8)' to interoperable type */;
    long n_points /* WARNING: Converting 'INTEGER(8)' to interoperable type */;
    long window_size /* WARNING: Converting 'INTEGER(8)' to interoperable type */;
    long margin /* WARNING: Converting 'INTEGER(8)' to interoperable type */;
} box_sample_param_t;

typedef struct box_px_t {
    long x /* WARNING: Converting 'INTEGER(8)' to interoperable type */;
    long y /* WARNING: Converting 'INTEGER(8)' to interoperable type */;
    long w /* WARNING: Converting 'INTEGER(8)' to interoperable type */;
    long h /* WARNING: Converting 'INTEGER(8)' to interoperable type */;
} box_px_t;

typedef struct box_stats_t {
    double x /* WARNING: Converting 'REAL(8)' to interoperable type */;
    double y /* WARNING: Converting 'REAL(8)' to interoperable type */;
    float av /* WARNING: Converting 'REAL(4)' to interoperable type */;
    float sd /* WARNING: Converting 'REAL(4)' to interoperable type */;
} box_stats_t;

typedef struct align_params_t {
    double scale /* WARNING: Converting 'REAL(8)' to interoperable type */;
    double gravity_precision /* WARNING: Converting 'REAL(8)' to interoperable type */;
    double gravity_precision_pre /* WARNING: Converting 'REAL(8)' to interoperable type */;
    int poly_stars /* WARNING: Converting 'INTEGER(4)' to interoperable type */;
    int poly_matches /* WARNING: Converting 'INTEGER(4)' to interoperable type */;
    int prealign /* WARNING: Converting 'INTEGER(4)' to interoperable type */;
} align_params_t;

typedef struct bkeq_param_t {
    int_fast8_t background /* WARNING: Converting 'LOGICAL(1)' to interoperable type */;
    int_fast8_t stars /* WARNING: Converting 'LOGICAL(1)' to interoperable type */;
    float apar /* WARNING: Converting 'REAL(4)' to interoperable type */;
    float bpar /* WARNING: Converting 'REAL(4)' to interoperable type */;
    float sigma /* WARNING: Converting 'REAL(4)' to interoperable type */;
    float sigma_star /* WARNING: Converting 'REAL(4)' to interoperable type */;
    int niter /* WARNING: Converting 'INTEGER(4)' to interoperable type */;
    int margin /* WARNING: Converting 'INTEGER(4)' to interoperable type */;
} bkeq_param_t;

typedef struct bkgradient_param_t {
    double sd_sigma /* WARNING: Converting 'REAL(8)' to interoperable type */;
    double offset /* WARNING: Converting 'REAL(8)' to interoperable type */;
    double gradient /* WARNING: Converting 'REAL(8)' to interoperable type */;
    double sigma_shift /* WARNING: Converting 'REAL(8)' to interoperable type */;
    long seed /* WARNING: Converting 'INTEGER(8)' to interoperable type */;
    long n_points /* WARNING: Converting 'INTEGER(8)' to interoperable type */;
    long window_size /* WARNING: Converting 'INTEGER(8)' to interoperable type */;
    long margin /* WARNING: Converting 'INTEGER(8)' to interoperable type */;
    int_fast8_t visualize /* WARNING: Converting 'LOGICAL(1)' to interoperable type */;
} bkgradient_param_t;

typedef struct findstar_params_t {
    long rslice /* WARNING: Converting 'INTEGER(8)' to interoperable type */;
    long margin /* WARNING: Converting 'INTEGER(8)' to interoperable type */;
    long min_star_pixels /* WARNING: Converting 'INTEGER(8)' to interoperable type */;
    double blur_radius /* WARNING: Converting 'REAL(8)' to interoperable type */;
    double thresh_sd /* WARNING: Converting 'REAL(8)' to interoperable type */;
    int_fast8_t reject_relative /* WARNING: Converting 'LOGICAL(1)' to interoperable type */;
    double max_rms /* WARNING: Converting 'REAL(8)' to interoperable type */;
} findstar_params_t;

typedef struct transform_c_t {
    char type[16];
    double scale /* WARNING: Converting 'REAL(8)' to interoperable type */;
    int npar /* WARNING: Converting 'INTEGER(4)' to interoperable type */;
    double vec[16] /* WARNING: Converting 'REAL(8)' to interoperable type */;
} transform_c_t;

void aqfindstar (const float *im /* WARNING: non-interoperable KIND */ , CFI_cdesc_t *master_mask, long ni /* WARNING: non-interoperable KIND */ , long nj /* WARNING: non-interoperable KIND */ , source_t *list, long limit /* WARNING: non-interoperable KIND */ , findstar_params_t *param, long *nstar /* WARNING: non-interoperable KIND */ );
void avsd_2d (buffer_descriptor_r32_t x, float *av /* WARNING: non-interoperable KIND */ , float *sd /* WARNING: non-interoperable KIND */ );
void classic_align (const source_t *lst0, long n0 /* WARNING: non-interoperable KIND */ , const source_t *lst, long n /* WARNING: non-interoperable KIND */ , const char *align_method, const align_params_t *params, transform_c_t *txc, error_status_t *err);
void conv2d_smallkernel (buffer_descriptor_r32_t x, buffer_descriptor_r32_t k, const char *method, buffer_descriptor_r32_t y, int_fast8_t parallel /* WARNING: non-interoperable KIND */ , int *err_code /* WARNING: non-interoperable KIND */ );
void deconvol_lr (buffer_descriptor_r32_t im1, buffer_descriptor_r32_t psf, float strength /* WARNING: non-interoperable KIND */ , int maxiter /* WARNING: non-interoperable KIND */ , buffer_descriptor_r32_t im2, int_fast8_t parallel /* WARNING: non-interoperable KIND */ );
align_params_t default_align_params ();
bkeq_param_t default_bkeq_params ();
bkgradient_param_t default_bkgradient_params ();
findstar_params_t default_findstar_params ();
void equalize_background (buffer_descriptor_r32_t *buffers, int nbuf /* WARNING: non-interoperable KIND */ , bkeq_param_t *param, error_status_t *err);
void estimate_gradient (buffer_descriptor_r32_t buf_in, buffer_descriptor_r32_t buf_out, const bkgradient_param_t *param, error_status_t *err_out);
void find_hot (buffer_descriptor_r32_t im, float sigma /* WARNING: non-interoperable KIND */ , buffer_descriptor_r32_t hot_mask);
void fix_hot (buffer_descriptor_r32_t im, buffer_descriptor_r32_t hot_mask);
void fix_hot_light (buffer_descriptor_r32_t im, float sigma /* WARNING: non-interoperable KIND */ , buffer_descriptor_r32_t im_out);
void gausskrn (double fwhm /* WARNING: non-interoperable KIND */ , buffer_descriptor_r32_t kd);
long get_kernel_size /* WARNING: Converting 'INTEGER(8)' to interoperable type */ (double fwhm /* WARNING: non-interoperable KIND */ );
void mask_margins (CFI_cdesc_t *mask, long *ni /* WARNING: non-interoperable KIND */ , long *nj /* WARNING: non-interoperable KIND */ , long *margin /* WARNING: non-interoperable KIND */ );
void mexhakrn (double fwhm /* WARNING: non-interoperable KIND */ , buffer_descriptor_r32_t kd);
void normalize_offset_gain (buffer_descriptor_r32_t *buffers, buffer_descriptor_r32_t *buffers_out, int n_buf /* WARNING: non-interoperable KIND */ , int margin /* WARNING: non-interoperable KIND */ , error_status_t *err);
void project_bilinear (const transform_c_t *vc, buffer_descriptor_r32_t im0, buffer_descriptor_r32_t im, double resample /* WARNING: non-interoperable KIND */ , error_status_t *err);
void register_stars (buffer_descriptor_r32_t imd, source_t *list, long limit /* WARNING: non-interoperable KIND */ , findstar_params_t *param, long *nstar /* WARNING: non-interoperable KIND */ );
void register_stars_core (const float *im /* WARNING: non-interoperable KIND */ , long ni /* WARNING: non-interoperable KIND */ , long nj /* WARNING: non-interoperable KIND */ , source_t *list, long limit /* WARNING: non-interoperable KIND */ , findstar_params_t *param, long *nstar /* WARNING: non-interoperable KIND */ );
void stack_frames (const buffer_descriptor_r32_t *frames, int n_frames /* WARNING: non-interoperable KIND */ , const char *method_, buffer_descriptor_r32_t frame_out, error_status_t *err);

#ifdef __cplusplus
}
#endif
