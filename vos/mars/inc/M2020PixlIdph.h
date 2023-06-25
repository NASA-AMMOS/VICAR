/*
  Copyright 2017-Present, California Institute of Technology. 
  ALL RIGHTS RESERVED. U.S. Government Sponsorship acknowledge.

  @author Hyun Lee (Hyun.H.Lee@jpl.nasa.gov)
*/
#ifndef M2020PIXLIDPH_H_
#define M2020PIXLIDPH_H_

//#include "M2020PixlEdr.h"
//#include <vector>
//#include <list>
//using namespace std;

//#define IDPH_SET4_SIZE 92

typedef struct 
{
   IDPH_U32 sclk_seconds;
   IDPH_U32 sclk_subseconds;
   IDPH_U16 site;
   IDPH_U16 drive;
   IDPH_U16 pose;
   IDPH_U16 arm;
   IDPH_U16 sha;
   IDPH_U16 drill;
   IDPH_U16 rsm;
   IDPH_U16 hga;
   IDPH_U16 bitcar;
   IDPH_U16 seal;
   IDPH_I32 sappQual;
   IDPH_F32 sapp_p[3];
   IDPH_F32 sapp_q[4];
} IdphSystem; // 60 bytes

typedef struct 
{
   IDPH_F32 steer_lf;
   IDPH_F32 steer_rf;
   IDPH_F32 steer_lr;
   IDPH_F32 steer_rr;
   IDPH_F32 left_bogie;
   IDPH_F32 right_bogie;
   IDPH_F32 left_differential;
   IDPH_F32 right_differential;
} IdphMob;  // 32 bytes

typedef struct 
{
   IDPH_F32 drive_lf;
   IDPH_F32 drive_rf;
   IDPH_F32 drive_lm;
   IDPH_F32 drive_rm;
   IDPH_F32 drive_lr;
   IDPH_F32 drive_rr;
   IDPH_F32 sapp_raw_gravity[3];
   IDPH_F32 sapp_combined_counter;
   IDPH_F32 hga_azimuth;
   IDPH_F32 hga_elevation;
   IDPH_F32 sun_p[3];
} IdphRvr;  // 60 bytes

typedef struct 
{
   IDPH_F32 q_hall[5];
   IDPH_F32 q_res[5];
   IDPH_F32 tool_pos[3];
   IDPH_F32 tool_quat[4];
   IDPH_F32 temp_actuator[5];
   IDPH_U8 contact;
   IDPH_U8 tool;
   IDPH_F32 turret_pos[3];
   IDPH_F32 turret_quat[4];
   IDPH_F32 temp_deflection[2];
   IDPH_F32 att_ref[4];
   IDPH_I16 preload;
   IDPH_U8 preload_tool;
   IDPH_F32 scs_docking_clock_angle;
} IdphArm; // 149 bytes

typedef struct 
{
   IDPH_F32 initial_resolver_azimuth;
   IDPH_F32 initial_resolver_elevation;
   IDPH_F32 initial_encoder_azimuth;
   IDPH_F32 initial_encoder_elevation;
   IDPH_F32 target_azimuth;
   IDPH_F32 target_elevation;
   IDPH_F32 encoder_azimuth;
   IDPH_F32 encoder_elevation;
   IDPH_F32 resolver_azimuth;
   IDPH_F32 resolver_elevation;
   IDPH_U8 deployed;
   IDPH_F32 p[3];
   IDPH_F32 q[4];
   IDPH_I32 frame;
   IDPH_I32 findex;
   IDPH_I32 ctype;
   IDPH_F32 coord[3];
   IDPH_F32 camera_plate_temp2;
   IDPH_F32 camera_plate_temp1;
} IdphRsm; // 101 bytes
 
typedef struct 
{
   IdphSystem sys;
   IdphMob mob;
   IdphRvr rvr;
   IdphArm arm;
   IdphRsm rsm;
   int vid;
   string sourceFname;
} IdphSet4;  // 402 bytes

#endif    /* M2020PIXLIDPH_H_ */
