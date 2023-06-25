/*
	fido_kinem.c

	rover reference frame:
		x --- forward
		y --- to the right
		z --- downward


			x_rover
			^
			|
		--------|--------
		|	|	|
		|	|	|
		|	------	|
		|       y_rover |
		|		|
		|	     _ _|
		|     y_ee  |   O
		|======<====|-+-|---- y_cam
		|	    | |	O
		-----------------
			      |
			      v
			z_ee, x_cam

		mast in initial folded position:
			theta1,2,3,4 = 90, 180, -180, 90

		x_ee = -0.533, y_ee = 0.327, z_ee = -0.583

			|  0   0  -1 |
		r_ee =  |  0  -1   0 |
			| -1   0   0 |

			| -1   0   0 |
		r_cam =  |  0   1   0 |
			|  0   0  -1 |



                         x_rover 
                         ^
                         |
                 --------|--------
                 |       |       |
                 |       |       |
                 |       ------ |
                 |       y_rover |
                 |               |
                 |     y_cam ^   |
                 |        _o_|_o_|
                 |       |   |   |
                 |       |   x --|---- z_ee, x_cam
                 -----------------
                             | 
                             v
                            y_ee

		mast in unfolded straight up default position:
			theta1,2,3,4 = 0, 90, 0, 0

		x_ee = -0.601, y_ee = 0.330, z_ee = -1.832

			|  0  -1   0 |
		r_ee =  |  0   0   1 |
			| -1   0   0 |

			|  0   1   0 |
		r_cam = |  1   0   0 |
			|  0   0  -1 |

*/


#include <stdio.h>

/* FIDO mast kinematics */

#define	MAST_a2		 0.660400		/* 26.000 inches */
#define	MAST_a3		 0.657225		/* 25.875 inches */
#define	MAST_l4		 0.068275		/* 2.688 inches */
#define	MAST_a4		 0.083337		/* 3.281 inches */

#define MAST_x0_ROVER	-0.533044		/* -20.986 inches */
#define MAST_y0_ROVER	 0.330098		/* 12.996 inches */
#define MAST_z0_ROVER	-0.482295		/* -18.988 inches */



#if 0

#define PI 3.14159265358979323846
#define TO_RAD  (PI/180.)

main()
{
	double theta1, theta2, theta3, theta4;
	double theta_rad[4];
	double r_ee[3][3];
	double t_ee[3];
	double r_cam[3][3];
	double t_cam[3];


	printf("theta1 theta2 theta3 theta4 (in deg) =  ");
	scanf("%lf %lf %lf %lf", &theta1, &theta2, &theta3, &theta4);

	theta_rad[0] = theta1 * TO_RAD;
	theta_rad[1] = theta2 * TO_RAD;
	theta_rad[2] = theta3 * TO_RAD;
	theta_rad[3] = theta4 * TO_RAD;

	fido_kinem_ee(theta_rad, r_ee, t_ee);

	printf("r_ee: %f %f %f\n      %f %f %f\n      %f %f %f\n",
		r_ee[0][0], r_ee[0][1], r_ee[0][2],
		r_ee[1][0], r_ee[1][1], r_ee[1][2],
		r_ee[2][0], r_ee[2][1], r_ee[2][2]);

	printf("t_ee = %f %f %f\n",
		t_ee[0], t_ee[1], t_ee[2]);

	fido_kinem_cam(theta_rad, r_cam, t_cam);

	printf("r_cam: %f %f %f\n      %f %f %f\n      %f %f %f\n",
		r_cam[0][0], r_cam[0][1], r_cam[0][2],
		r_cam[1][0], r_cam[1][1], r_cam[1][2],
		r_cam[2][0], r_cam[2][1], r_cam[2][2]);

	printf("t_cam = %f %f %f\n",
		t_cam[0], t_cam[1], t_cam[2]);

}
#endif

/*
	end effector pose
*/
fido_kinem_ee(theta_rad, r_ee, t_ee)
double theta_rad[4];
double r_ee[3][3];
double t_ee[3];
{
	double theta23, theta234;
	double s1, c1, s2, c2, s23, c23, s234, c234;
	double sin(), cos();
	double theta1, theta2, theta3, theta4;
	double L1, L2;


	theta1 = theta_rad[0];
	theta2 = theta_rad[1];
	theta3 = theta_rad[2];
	theta4 = theta_rad[3];

	s1 = sin (theta1);
	c1 = cos (theta1);

	s2 = sin (theta2);
	c2 = cos (theta2);

	theta23 = theta2 + theta3;
	theta234 = theta23 + theta4;

	s23 = sin (theta23);
	c23 = cos (theta23);

	s234 = sin (theta234);
	c234 = cos (theta234);


	r_ee[0][0] = c1*c234;  r_ee[0][1] = -c1*s234;  r_ee[0][2] = -s1;
	r_ee[1][0] = s1*c234;  r_ee[1][1] = -s1*s234;  r_ee[1][2] =  c1;
	r_ee[2][0] =   -s234;  r_ee[2][1] =    -c234;  r_ee[2][2] =  0.;


	L1 = c2*MAST_a2 + c23*MAST_a3 - s23*MAST_l4 + c234*MAST_a4;
	L2 = s2*MAST_a2 + s23*MAST_a3 + c23*MAST_l4 + s234*MAST_a4;

	t_ee[0] = MAST_x0_ROVER + c1*L1;
	t_ee[1] = MAST_y0_ROVER + s1*L1;
	t_ee[2] = MAST_z0_ROVER - L2;
}

fido_kinem_cam(theta_rad, r_cam, t_cam)
double theta_rad[4];
double r_cam[3][3];
double t_cam[3];
{
	double r_ee[3][3];
	double t_ee[3];

	fido_kinem_ee(theta_rad, r_ee, t_ee);

	r_cam[0][0]=r_ee[0][2]; r_cam[0][1]=-r_ee[0][1]; 
r_cam[0][2]=r_ee[0][0];
	r_cam[1][0]=r_ee[1][2]; r_cam[1][1]=-r_ee[1][1]; 
r_cam[1][2]=r_ee[1][0];
	r_cam[2][0]=r_ee[2][2]; r_cam[2][1]=-r_ee[2][1]; 
r_cam[2][2]=r_ee[2][0];

	t_cam[0] = t_ee[0];
	t_cam[1] = t_ee[1];
	t_cam[2] = t_ee[2];
}
