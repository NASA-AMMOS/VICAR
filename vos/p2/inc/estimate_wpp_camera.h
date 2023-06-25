#ifndef __ESTIMATE_WPP_CAMERA_H
#define __ESTIMATE_WPP_CAMERA_H



#ifdef __cplusplus
extern "C" {
#endif

int recover_wpp_camera(double *P, double *m_adr, double *q_adr, double *tau_adr, 
			double *theta_adr,  double *phi_adr, double *psi_adr, double *bu_adr, double *bv_adr);
   
int estimate_wpp_camera(double phi0, double psi0, double *corners, int imwidth, int imheight,
                        double *m_adr, double *q_adr, double *tau_adr, double *theta_adr, 
		        double *phi_adr, double *psi_adr, double *bu_adr, double *bv_adr);

int wpp_projection_matrix(double theta, double phi, double psi, 
			  double m, double q, double tau, double bu,
                          double bv, double *M);

#ifdef __cplusplus
}
#endif

#endif


