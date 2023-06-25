/***************** ACTORS Software **************************************
 *
 *	Copyright (C) 1994, California Institute of Technology
 *	All rights reserved.
 *
 ************************************************************************
 *	Developed by the Visualization & Earth Science Applications Group,
 *	Image Processing Applications and Development Section,
 *	Jet Propulsion Laboratory,
 *	California Institute of Technology
 ************************************************************************
 * Module: pmake.C
 *
 * Purpose: Parallel make driver program
 *
 * Limitations:
 *
 * Original Author: John Wright
 *
 * Current cognizant programmer: John Wright
 *
 * Created: October 1994
 *
 * Last Modified: 09/03/97  1.2
 *
 ************************************************************************
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include "net/net_lib.h"


main(int argc, char **argv)
{

  if (argc < 2)
    {
      printf("Usage: %s [module1 module2 ...]\n\n",argv[0]);
      exit(0);
    }

// printf("Trying parallel maker.\n");


Pool_Of_Commands	*pool;
Network			*my_net;
char			*args[20];
char			curr_dir[1024];
char			command[2048];
int			num_modules, i, okay;

	num_modules = argc - 1;

  okay = FALSE;
  if( num_modules > 1)  {
	my_net = new Network();
  	okay = (my_net->get_net_status() == NET_READY );
  }
  if(!okay || num_modules <= 1) {
	// fprintf(stderr,"Running locally doing %d modules.\n", num_modules);
	for (i = 0; i < num_modules; i++) {
		sprintf(command, "/usr/bin/make %s", argv[i+1]);
		system(command);
	}
  } else {
	pool = new Pool_Of_Commands(num_modules);
	sprintf(command, "cd %s ; /usr/bin/make", getwd(curr_dir));
	for (i = 0; i < num_modules; i++) {
		args[0] = argv[i+1];
		args[1] = NULL;
		pool->get_task(i)->set_task_string(command);
		pool->get_task(i)->set_args_string(args);

	// fprintf(stderr,"Call format for command %d is:\n",i);
	// for(j=0; args[j]; j++) fprintf(stderr," %s %s ", command, args[j]);
	// fprintf(stderr,"\n");
	}
	
	my_net->run_pool_blocked(pool);
	while(pool->get_pool_status() != POOL_COMPLETE) sleep(1);
  }
}

