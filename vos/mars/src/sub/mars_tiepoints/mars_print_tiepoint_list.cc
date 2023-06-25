////////////////////////////////////////////////////////////////////////
// mars_print_tiepoint_list.cc
//
// Print out a tiepoint list
////////////////////////////////////////////////////////////////////////

#include "mars_tiepoints.h"
#include "zvproto.h"

#include <stdio.h>

void mars_print_tiepoint_list(TiePoint *tiepoints, int n)
{
    int i;

    zvmessage("   n  L   R  L_samp L_line R_samp R_line  New_s  New_l Qual Del_s Del_l man Res", "");
    for (i=0; i < n; i++) {
	mars_print_one_tiepoint(&tiepoints[i], i);
    }
}

void mars_print_one_tiepoint(TiePoint *tiepoint, int which)
{
    char msg[200];
      switch (tiepoint->type) {

	case TIEPOINT_TRADITIONAL:
        case TIEPOINT_MISS_DISTANCE:
	case TIEPOINT_1D:
	case TIEPOINT_INFINITY:
	case TIEPOINT_Z_SURFACE:
	case TIEPOINT_DYNAMIC_XYZ:
	  sprintf(msg,
	  "%4d %3d %3d %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f %4.2f %5.1f %5.1f %1s %5.1f",
		which,
		tiepoint->left_image+1, tiepoint->right_image+1,
		tiepoint->left_sample, tiepoint->left_line,
		tiepoint->right_sample, tiepoint->right_line,
		tiepoint->corrected_sample, tiepoint->corrected_line,
		tiepoint->quality,
		tiepoint->right_sample - tiepoint->corrected_sample,
		tiepoint->right_line - tiepoint->corrected_line,
		tiepoint->interactive ? "y" : "n",
		sqrt(tiepoint->residual));
	  zvmessage(msg, "");
	  if (tiepoint->type != TIEPOINT_Z_SURFACE &&
	      tiepoint->type != TIEPOINT_DYNAMIC_XYZ) // fall through: TOTAL HACK!!!
	    break;

	case TIEPOINT_FIDUCIAL:
	case TIEPOINT_ORBITAL_XYZ:
	  sprintf(msg, "%4d %3d     %6.1f %6.1f (%10.4f %9.4f %9.4f) %1s %5.1f",
		which,
		tiepoint->left_image,
		tiepoint->left_sample, tiepoint->left_line,
		tiepoint->xyz.getX(), tiepoint->xyz.getY(),
		tiepoint->xyz.getZ(),
		tiepoint->interactive ? "y" : "n",
		sqrt(tiepoint->residual));
	  zvmessage(msg, "");
	  break;

	default:
	  sprintf(msg, "Printing of tiepoint type %d not implemented", tiepoint->type);
	  zvmessage(msg, "");
	  break;

    }
}

