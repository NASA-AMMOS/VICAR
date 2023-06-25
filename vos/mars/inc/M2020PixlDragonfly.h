/*****************************************************************************
 *   Copyright 2016, by the California Institute of Technology.
 *   ALL RIGHTS RESERVED. United States Government Sponsorship
 *   acknowledged. Any commercial use must be negotiated with the Office
 *   of Technology Transfer at the California Institute of Technology.
 *
 *   Information included herein is controlled under the International
 *   Traffic in Arms Regulations ("ITAR") by the U.S. Department of State.
 *   Export or transfer of this information to a Foreign Person or foreign
 *   entity requires an export license issued by the U.S. State Department
 *   or an ITAR exemption prior to the export or transfer.
 ******************************************************************************/

#ifndef DRAGONFLY_H
#define DRAGONFLY_H

#ifdef __GNUG__
extern "C" {
#endif // __GNUG__

#define DFLY_META_ARGS  6

/* 
 * Possible Status values from Dragonfly 
 *
 */

#define DFLY_STATUS_OK                           0
#define DFLY_STATUS_WARN_condition          0x0010
#define DFLY_STATUS_FAIL_INVALID_K_BITS     0x0022
#define DFLY_STATUS_FAIL_INVALID_SAML_PCT   0x0023

#define DFLY_MAX_OUTPUT 4100 // Theoretcial max size of out the dragon fly output is 16388 bytes, or 4097 words

/* The Descriptor Field is 32-bits long it consist of the following pieces
 *
 *    Bits     Content
 *   31 - 20   0xFFF00000 -- Net Sample Count (Note: 0x000 = max value = 4096)
 *   19 - 13   0x000FE000 -- Leading zero data (max value: 127)
 *   12 -  8   0x00001F00 -- Fundamental width of large encoded data (max value 31)
 *    7 -  4   0x000000F0 -- Fundamental width of small encoded data (max value 15)
 *    3 -  0   0x0000000F -- k-split bits (max value 15)
 */

#define DFLY_SAMPLE_COUNT_MASK  0xFFF00000
#define DFLY_LEADING_ZERO_MASK  0x000FE000
#define DFLY_LARGE_DATA_MASK    0x00001F00
#define DFLY_SMALL_DATA_MASK    0x000000F0
#define DFLY_K_SPLIT_MASK       0x0000000F

#define DFLY_SAMPLE_COUNT_SHIFT         20
#define DFLY_LEADING_ZERO_SHIFT         13
#define DFLY_LARGE_DATA_SHIFT            8
#define DFLY_SMALL_DATA_SHIFT            4

#define DFLY_MAX_LEADING_ZERO          127
#define DFLY_SAMPLES                  4096

struct dfly_output_t{
    DEF_U32   Descriptor;
    DEF_U32   data[ DFLY_MAX_OUTPUT + 20 ];  // extra elements to appease Coverity
};

/*  Dragonfly Parameters 
 *
 *  The elements in the following structure control the behavior of
 *  the Dragonfly compressor as follows:
 *
 *  control is a bit mask:
 *  0x00000001 -- Force out reporting of the results in the Meta data
 *  0x00000010 -- Disable the Dual-size compression (that is only use Fundamental Sequence)
 *  0x00000020 -- Disable the Fundamental Sequence (only use Dual-size compression)
 *
 */
struct dfly_params_t {
    DEF_I32 control;
    DEF_I32 small_percent;
    DEF_I32 k_low_limit;
    DEF_I32 k_high_limit;
};

#define DFLY_CONTROL_FORCE_REPORT      0x00000001
#define DFLY_CONTROL_DISABLE_DUALSIZE  0x00000010
#define DFLY_CONTROL_DISABLE_FS        0x00000020
#define DFLY_CONTROL_VERBOSE_1         0x00000100     // Signpost
#define DFLY_CONTROL_VERBOSE_2         0x00000200     // K-Bits debug stuff
#define DFLY_CONTROL_VERBOSE_4         0x00000400     // Fundamental Sequence Debug stuff
#define DFLY_CONTROL_VERBOSE_8         0x00000800     // Dual-Size Encoding debug stuff

struct dfly_meta_t {
    DEF_I32 status;
    DEF_I32 arg[ DFLY_META_ARGS ];
};

DEF_I32 dfly_compress
(
    const DEF_I32* input,
    struct dfly_output_t* outdata,
    const struct dfly_params_t* params,
    struct dfly_meta_t* Meta
);

void dfly_establish_huge_data
(
    DEF_I32 * arg_k_bits,
    DEF_I32 * arg_enc_bits,
    DEF_U32 * arg_ds_enc,
    DEF_U32 * arg_fs_enc
);

#ifdef __GNUG__
}
#endif // __GNUG__

#endif  /* DRAGONFLY_H */
