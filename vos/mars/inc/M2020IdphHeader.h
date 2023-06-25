/*
       Copyright 2008-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/

/**
 * @author Alice Stanboli {Alice.Stanboli@jpl.nasa.gov}
 */
#ifndef M2020IDPHHEADER_H_
#define M2020IDPHHEADER_H_

#include "M2020MainInclude.h"
#include "M2020Env.h"
#include "M2020InstrumentHeader.h"
#include "M2020DPOParser.h"
#include "M2020Exception.h"

/*
typedef struct
{
  IDPH_ENUM compress;
  IDPH_F32  bpp;
  IDPH_U8   min_loss;
  IDPH_ENUM wfilter;
  IDPH_U8   n_decomps;
  IDPH_U8   n_segs;
} ImgDpoStructComp;

typedef struct
{
  IDPH_U32 imgid;
  IDPH_ENUM camera;
  IDPH_ENUM rsm_frame;
  IDPH_I32  rsm_findex;
  IDPH_ENUM rsm_ctype;
  IDPH_F64  rsm_coord[3];
  IDPH_ENUM acquire;
  IDPH_ENUM shutter;
  IDPH_U16  shutter_thresh;
  IDPH_U8   flush;
  IDPH_ENUM exposure;
  IDPH_F32  exp_time_scale;
  IDPH_U16  exp_time;
  IDPH_ENUM exp_table;
  IDPH_BOOL exp_update;
  IDPH_U16  exp_auto_dn;
  IDPH_F32  exp_auto_frac;
  IDPH_U8   exp_auto_iter;
  IDPH_U8   exp_auto_percent;
  IDPH_U32  power_timeout;
  IDPH_BOOL bad;
  IDPH_BOOL flat;
  IDPH_BOOL scale_early;
  IDPH_BOOL img_early;
  IDPH_ENUM subframe;
  IDPH_U16  sub_row0;
  IDPH_U16  sub_col0;
  IDPH_U16  sub_rows;
  IDPH_U16  sub_cols;
  IDPH_BOOL histogram;
  IDPH_U8   hist_prio;
  IDPH_BOOL rowsums;
  IDPH_U8   row_prio;
  IDPH_BOOL colsums;
  IDPH_U8   col_prio;
  IDPH_BOOL ref;
  IDPH_U8   ref_prio;
  ImgDpoStructComp ref_comp;
  IDPH_BOOL thumbnail;
  IDPH_U8   thumb_prio;
  IDPH_U16  thumb_rows;
  IDPH_U16  thumb_cols;
  IDPH_ENUM thumb_scale;
  ImgDpoStructComp thumb_comp;
  IDPH_BOOL image;
  IDPH_U8   img_prio;
  IDPH_ENUM resolution;
  IDPH_U16  res_rows;
  IDPH_U16  res_cols;
  IDPH_ENUM scale;
  ImgDpoStructComp comp;
} ImgDpoStructParam;

typedef struct
{
  ImgDpoStructParam params;
  IDPH_U32  sclk_seconds;
  IDPH_U32  sclk_subseconds;
  IDPH_U16  rmc_site;
  IDPH_U16  rmc_drive;
  IDPH_U16  rmc_pose;
  IDPH_U16  rmc_arm;
  IDPH_U16  rmc_chimra;
  IDPH_U16  rmc_drill;
  IDPH_U16  rmc_rsm;
  IDPH_U16  rmc_hga;
  IDPH_F32  rvr_p[3];
  IDPH_F32  rvr_q[4];
  IDPH_ENUM rvr_quality;
  IDPH_I32  rvr_saved_i;
  IDPH_F32  rvr_saved_p[3];
  IDPH_F32  rvr_saved_q[4];
  IDPH_F32  rsm_initial_azimuth;
  IDPH_F32  rsm_initial_elevation;
  IDPH_F32  rsm_target_azimuth;
  IDPH_F32  rsm_target_elevation;
  IDPH_F32  rsm_final_azimuth;
  IDPH_F32  rsm_final_elevation;
  IDPH_F32  rsm_res_azimuth;
  IDPH_F32  rsm_res_elevation;
  IDPH_BOOL rsm_deployed;
  IDPH_I32  rsm_findex;
  IDPH_F32  rsm_p[3];
  IDPH_F32  rsm_q[4];
  IDPH_F32  arm_qenc[5];
  IDPH_F32  arm_qres[5];
  IDPH_F32  arm_pos[3];
  IDPH_F32  arm_quat[4];
  IDPH_F32  arm_temp[5];
  IDPH_F32  arm_tilt[3];
  IDPH_U8   arm_contact;
  IDPH_U8   arm_instrument;
  IDPH_U8   arm_mode;
  IDPH_F32  temp[24];
  //IDPH_F32  temp[20];
  IDPH_F32  resolver[6];
  IDPH_F32  steer_fl;
  IDPH_F32  steer_fr;
  IDPH_F32  steer_rl;
  IDPH_F32  steer_rr;
  IDPH_F32  hga_azimuth;
  IDPH_F32  hga_elevation;
  IDPH_U8   serial_no;
  IDPH_BOOL stereo;
  IDPH_BOOL shutter;
  IDPH_U16  exp_time;
  IDPH_U16  exp_count;
  IDPH_U16  stripes;
  IDPH_U16  overlap;
  IDPH_U16  voff;
  IDPH_BOOL hw_binning;
  IDPH_U16  hw_minrow;
  IDPH_U16  hw_numrows;
  IDPH_BOOL hw_scale;
  IDPH_ENUM rotation;
  IDPH_U16  bad;
  IDPH_F32  flat_params[5];
  IDPH_U16  row0;
  IDPH_U16  col0;
  IDPH_U16  rows;
  IDPH_U16  cols;
  IDPH_U16  res_rows;
  IDPH_U16  res_cols;
  IDPH_ENUM scale;
  IDPH_U16  cmod_model_id;
  IDPH_U8   cmod_serial_no;
  IDPH_ENUM cmod_camera_id;
  IDPH_ENUM cmod_mclass;
  IDPH_U8   cmod_mtype;
  IDPH_F64  cmod_mparm;
  IDPH_F64  cmod_c[3];
  IDPH_F64  cmod_a[3];
  IDPH_F64  cmod_h[3];
  IDPH_F64  cmod_v[3];
  IDPH_F64  cmod_o[3];
  IDPH_F64  cmod_r[3];
  IDPH_F64  cmod_e[3];
} ImgDpoStructIdph;
//#pragma pack(0)
*/

class M2020IdphHeader:public M2020InstrumentHeader {
      protected:
   M2020Env * env_;
   M2020DPOParser *parser_;

      public:
    explicit M2020IdphHeader(M2020DPOParser *) throw(exception);
    virtual ~ M2020IdphHeader() {
   }
   //ImgDpoStructIdph idph;
   //static void dump(const ImgDpoStructIdph &idph);
   void dump() const throw(exception);
   IDPH_U8 getU8(const char *fieldName) const throw(exception);
   IDPH_I8 getI8(const char *fieldName) const throw(exception);
   IDPH_U16 getU16(const char *fieldName) const throw(exception);
   IDPH_I16 getI16(const char *fieldName) const throw(exception);
   IDPH_U32 getU32(const char *fieldName) const throw(exception);
   IDPH_I32 getI32(const char *fieldName) const throw(exception);
   IDPH_U64 getU64(const char *fieldName) const throw(exception);
   IDPH_I64 getI64(const char *fieldName) const throw(exception);
   IDPH_F32 getF32(const char *fieldName) const throw(exception);
   IDPH_F64 getF64(const char *fieldName) const throw(exception);

/**********************************************************************************************/
   template < typename T > T getValue_(const string & fieldName) const throw(exception) {
      T val;

      //if (this->parser_==NULL) return (T)0;

      try {
         this->parser_->get(fieldName.c_str(), val);
      }
      catch(const M2020Exception & e) {
         string s = "Error obtaining value for filed '";
         s.append(fieldName).append("'");
         THROW_EXCEPTION_E(M2020Exception::LABEL_ERROR, e, s.c_str());
      }
      return val;
   }
};

#endif            /*IDPHHEADER_H_ */
