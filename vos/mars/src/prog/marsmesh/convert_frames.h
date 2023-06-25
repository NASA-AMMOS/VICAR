//convert_frames.h 
//Support functions for marsmesh program
//
#include "PigVector.h"
#include "PigCAHV.h"

////////////////////////////////////////////////////////////////////////


PigPoint ZconvertWorldToObjFrame(PigPoint* world_pt, PigPoint* pt_offset,
                                double rot[3][3]);
bool ZconvertWorldToObjFrame(double* world_pt, double* pt_offset,
                            double rot[3][3], double* cm_pt);
bool ZconvertObjToWorldFrame(double* cm_pt, double* pt_offset, double rot[3][3],
                            double* world_pt);
void Zcreate_camera_centric_frame(PigCAHV* camera_model, double rot[3][3]);
