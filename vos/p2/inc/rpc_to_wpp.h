#ifndef __RPC_TO_WPP_H
#define __RPC_TO_WPP_H

#ifdef __cplusplus
extern "C" {
#endif

int rpc_to_wpp(int n_plh, double *PLH, int rpc_version, double *r,
               double *m_adr, double *q_adr, double *tau_adr, 
               double *theta_adr, double *phi_adr, double *psi_adr, 
               double *bu_adr, double *bv_adr);

int create_ecef_correspondences(int n_plh, double *PLH, int rpc_version, 
               double *r, int *n_grid_adr, double **grid_xyz_adr, 
               double **grid_uv_adr);

int wpp_attitude_quaternion(double theta, double phi, double psi, double *quat);

int wpp_position_quaternion(double alt, double u0, double v0, 
    double m, double q, double tau, double theta, double phi, double psi, 
    double bu, double bv, double *quat);

#ifdef __cplusplus
}
#endif

#endif


