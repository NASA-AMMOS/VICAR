//convert_frames.cc
//Support functions for marsmesh program
//
#include "convert_frames.h"
#include <mat3.h>

////////////////////////////////////////////////////////////////////////


PigPoint ZconvertWorldToObjFrame(PigPoint* world_pt, PigPoint* pt_offset,
                                double rot[3][3]) {
         //translate first
         PigPoint *xyz = world_pt;
         *xyz -= *pt_offset;
         double xyz_in[3];
         xyz->getXYZ(xyz_in);
         //rotate now
         double xyz_out[3];
         mult133(xyz_in, rot, xyz_out);
         //points behind camera frame are bogus
         if (xyz_out[0] < 0.0)
             return PigPoint();
         return PigPoint(xyz_out);
}
bool ZconvertWorldToObjFrame(double* world_pt, double* pt_offset,
                            double rot[3][3], double* cm_pt) {
    //translate first
    double tmp[3];
    sub3(world_pt, pt_offset, tmp);
    //rotate now
    mult133(tmp, rot, cm_pt);
    //points behind camera frame are bogus
    if (cm_pt[0] < 0.0)
        return false;

    return true;

}
bool ZconvertObjToWorldFrame(double* cm_pt, double* pt_offset, double rot[3][3],
                            double* world_pt) {
  // char msg[256];
    double rot_t[3][3];
    trans33(rot, rot_t);
    double tmp[3];
    //rotate first
    mult133(cm_pt, rot_t, tmp);
    //translate now
    add3(tmp, pt_offset, world_pt);

    return true;

}
void Zcreate_camera_centric_frame(PigCAHV* camera_model, double rot[3][3]) {
    PigPoint point_C;
    PigVector vector_A;
    PigVector vector_H;
    PigVector vector_V;
    PigVector vector_H_proj;
    PigVector vector_Temp;
    PigVector vector_Z;
    // char msg[256];

    camera_model->getCurrentCAHV(point_C, vector_A, vector_H, vector_V);
    // normalize A, make sure it's not zero -> new X (forward)
    vector_A.normalize();
    // project H onto image plane -> new Y (left)
    // dot product of normalized A and H
    double ah_dot_product = vector_A%vector_H;
    vector_Temp = vector_A*ah_dot_product;
    vector_H_proj = vector_Temp - vector_H;
    vector_H_proj.normalize();

    vector_Z = vector_A*vector_H_proj;

    // now construct transform matrix
    // double r_t[3][3];
    rot[0][0] = vector_A.getX();
    rot[1][0] = vector_A.getY();
    rot[2][0] = vector_A.getZ();

    rot[0][1] = vector_H_proj.getX();
    rot[1][1] = vector_H_proj.getY();
    rot[2][1] = vector_H_proj.getZ();

    rot[0][2] = vector_Z.getX();
    rot[1][2] = vector_Z.getY();
    rot[2][2] = vector_Z.getZ();


    //transform matrix
    //double roll = atan2(r[2][1], r[2][2]);
    //double pitch = atan2(-1.0*r[2][0], sqrt(r[0][0]*r[0][0]+r[1][0]*r[1][0]));
    //double yaw = atan2(r[1][0], r[0][0]);

    //Use transform matrix to get Euler Angles
    double roll = atan2(rot[1][2], rot[2][2]);
    double pitch = atan2(-1.0*rot[0][2], sqrt(rot[0][0]*rot[0][0]+rot[0][1]*rot[0][1]));
    double yaw = atan2(rot[0][1], rot[0][0]);


    //snprintf(msg, 256, "roll=%lf, pitch=%lf, yaw=%lf\n",
    //        roll*180/PI, pitch*180/PI, yaw*180/PI);
    //zvmessage(msg, "");

    PigQuaternion quat(1.0, 0.0, 0.0, 0.0);
    quat.setEulerAngles(roll, pitch, yaw);
    double v[4];
    quat.getComponents(v);

    PigPoint final_point(0.0, 0.0, 0.0);
    PigQuaternion initial_orientation(1.0, 0.0, 0.0, 0.0);
    camera_model->moveCamera(point_C, initial_orientation,
                             final_point, quat, camera_model->getCoordSystem());
    PigPoint ccc;
    PigVector aaa, hhh, vvv;
    camera_model->getCurrentCAHV(ccc, aaa, hhh, vvv);

    return;
}
