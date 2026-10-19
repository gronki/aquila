#pragma once

/*
 * Anything that can be derived from the Fortran BIND(C) interfaces lives in
 * aquila_gen.h, which gfortran generates from src/aquila.F90 (see the
 * add_custom_command in CMakeLists.txt). Only what gfortran cannot emit is
 * kept here by hand.
 */

/* -fc-prototypes emits int_fast8_t and friends without providing stdint.h */
#include <stdint.h>

#include "aquila_gen.h"

#define AQUILA_VERSION "261019"

#define AQ_STATUS_OK 0
#define AQ_STATUS_ERROR 1
#define AQ_STATUS_ERR_SHAPE_MISMATCH 100
#define AQ_STATUS_ERR_EMPTY_BUFFER 100

#define AQ_PREALIGN_NONE 0
#define AQ_PREALIGN_FLIP 1
#define AQ_PREALIGN_POLYGON 2

/* C-side names with no Fortran counterpart to generate them from */
typedef float real_buf_t;
#define TRANSFORM_MAX_PAR 16
typedef transform_c_t transform_t;

#ifdef __cplusplus
/* -fc-prototypes spells Fortran kinds as plain C types, losing the exact width
   the Fortran side declared: INTEGER(8) comes out as long, LOGICAL(1) as
   int_fast8_t, and so on. On a platform where one of those widths differs
   (long is 4 bytes on Windows LLP64) every field after it would shift
   unnoticed, so pin all the spellings the generated header actually uses. */
static_assert(sizeof(int_fast8_t) == 1, "LOGICAL(1) is emitted as int_fast8_t");
static_assert(sizeof(long) == 8, "INTEGER(8) is emitted as long");
static_assert(sizeof(int) == 4, "INTEGER(4) is emitted as int");
static_assert(sizeof(float) == 4, "REAL(4) is emitted as float");
static_assert(sizeof(double) == 8, "REAL(8) is emitted as double");
static_assert(sizeof(char) == 1, "CHARACTER(1) is emitted as char");
#endif
