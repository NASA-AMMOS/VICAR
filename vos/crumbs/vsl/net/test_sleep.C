/***************** ACTORS Software System *******************************
 *
 *      Copyright (C) 1994, California Institute of Technology
 *      All rights reserved.
 *
 ************************************************************************
 *      Developed by the Visualization & Earth Science Applications Group,
 *      Image Processing Applications and Development Section,
 *      Jet Propulsion Laboratory,
 *      California Institute of Technology
 ************************************************************************
 * Module: test_sleep.C
 *
 * Purpose: Slave test task
 *
 * Limitations:
 *
 * Original Author: John Wright
 *
 * Current cognizant programmer: John Wright
 *
 * Created: 08/03/94
 *
 * Last Modified: 24 Nov 1994  1.0
 *
 * Modification History:
 *
 ************************************************************************
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include "net/net_lib.h"


main(int argc, char **argv)
{
  int	time=5;

  if(argc > 1)sscanf(argv[1],"%d",&time);
  sleep(time);

  exit(0);
}


