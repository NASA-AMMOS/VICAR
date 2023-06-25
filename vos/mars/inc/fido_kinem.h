#ifndef FIDO_KINEM_H
#define FIDO_KINEM_H

/*****************************************************************************
    Function Name:  fido_kinem_ee
    Description:    Computes end effector pose
    Parameters:
    theta_rad[4]  =    array of doubles representing 4 joint mast arm angles 
                    expressed in radians.

    Returns:
    r_ee[3][3]
    t_ee[3]
*****************************************************************************/
void fido_kinem_ee(double theta_rad[4], double r_ee[3][3], double t_ee[3]);

/*****************************************************************************
    Function Name:  fido_kinem_cam
    Description:    Computes camera pose
    Parameters:
    theta_rad[4]  =    array of doubles representing 4 joint mast arm angles 
                    expressed in radians.

    Returns:
    r_cam[3][3]
    t_cam[3]
*****************************************************************************/
void fido_kinem_cam(double theta_rad[4], double r_cam[3][3], double t_cam[3]);

#endif  /* FIDO_KINEM_H */

