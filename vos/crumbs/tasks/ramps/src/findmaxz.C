/*
** findmaxz
**    Runs through one or two octrees and finds the 5 highest (max z)
** points for each tree in OBJECT space.
*/

#include <float.h>  //for MINFLOAT
#include "grape/octree.h"
#include <math.h>

double max_points[5][3];  // Max of 5 points, saving x[0], y[1], z[2]

void traverse_octree(Octree_Data *oct, ZMatrix transmat)
{
    NodeSpec	*ns;
    ldparam	*ctr;
    double model_center[4];
    double obj_center[4];
    int	ref;
    int j;

	// if NULL pointer then return
	if(!oct)return;

	if(!(oct->any_children())) {

		if ((ns = oct->get_node_data()) == NULL)
		{
		    fprintf(stderr, "findmaxz:  get_node_data() failed for child.\n");
		    return;
		}

		do
		{
			ctr = ns->get_global_center();
			if (ctr == NULL)
			{
				fprintf(stderr,
					"findmaxz:  get_global_center() failed for node.\n");
				return;
			}

			model_center[0] = ctr->get_value(0);
			model_center[1] = ctr->get_value(1);
			model_center[2] = ctr->get_value(2);
                        model_center[3] = 0.0;

			// Multiply ctr by transmatrix to get value in object space
			MultPoints(model_center, transmat, obj_center);

			// Top 5 max Z points are saved, max_point[0] being
			// the highest

			if (obj_center[2] > max_points[4][2])
                        {
			   if (obj_center[2] > max_points[3][2])
                           {
			      if (obj_center[2] > max_points[2][2])
			      {
			         if (obj_center[2] > max_points[1][2])
 				 {
			            if (obj_center[2] > max_points[0][2])
				    {
					// Shift 4, 3, 2, 1, 0
					for (j=0;j<3;j++)
					   max_points[4][j] = max_points[3][j];
					for (j=0;j<3;j++)
					   max_points[3][j] = max_points[2][j];
					for (j=0;j<3;j++)
					   max_points[2][j] = max_points[1][j];
					for (j=0;j<3;j++)
					   max_points[1][j] = max_points[0][j];
					for (j=0;j<3;j++)
					   max_points[0][j] = obj_center[j];
				    }
				    else  // Shift 4, 3, 2, 1
				    {
					for (j=0;j<3;j++)
					   max_points[4][j] = max_points[3][j];
					for (j=0;j<3;j++)
					   max_points[3][j] = max_points[2][j];
					for (j=0;j<3;j++)
					   max_points[2][j] = max_points[1][j];
					for (j=0;j<3;j++)
					   max_points[1][j] = obj_center[j];
				    }
				}
				else  // Shift 4, 3, 2
				{
				   for (j=0;j<3;j++)
				      max_points[4][j] = max_points[3][j];
				   for (j=0;j<3;j++)
				      max_points[3][j] = max_points[2][j];
				   for (j=0;j<3;j++)
				      max_points[2][j] = obj_center[j];
				}
			    }
			    else  // Shift 4, 3
			    {
				for (j=0;j<3;j++)
				   max_points[4][j] = max_points[3][j];
				for (j=0;j<3;j++)
				   max_points[3][j] = obj_center[j];
			    }
			}
			else  // Shift 4
			{
			   for (j=0;j<3;j++)
			      max_points[4][j] = obj_center[j];
			}
		    }
		} while (ns=ns->next);

	} else {
		// recursively process each valid child of node
		for(ref=UPPER_LEFT_BACK; ref<NUMBER_OF_SPACE_PARTITIONS; ref++) {
			traverse_octree(oct->get_child((Octree_Child_Ref)ref), transmat);
		}
	}
}

main (int argc, char **argv)
{
FILE_Dataport  *fp1; 
FILE_Dataport  *fp2;
Octree * oct1;
Octree * oct2;
Octree_Data *od1;
Octree_Data *od2;
ObjNode * objn1;
ObjNode * objn2;
int i,j;
ZMatrix transmat1, transmat2;

   // Check arguments
   if (argc < 2)
   {
	printf("Usage: %s octree1 [octree2]\nRuns through one or two octrees and finds the 5 highest (max z)\n\
points for each tree in OBJECT space.\n", argv[0]);
	exit(1);
   }

   fp1 = new FILE_Dataport();
   fp2 = new FILE_Dataport();

   if (!fp1->ropen( argv[1]))
   {
      fprintf(stderr, "findmaxz(): unable to open %s for reading\n", argv[1]);
      exit(1);
   }

   char token[4096];
   get_next_token(fp1, token);
   oct1 = new Octree();
   oct1->parse_in(fp1);
   od1 = oct1->get_data();
   objn1 = new ObjNode();
   objn1->set_object( oct1);
   oct1->GetModelToObjectTransform( transmat1 );

   fprintf(stdout, "%s trans matrix is:\n", argv[1]);
   for ( i=0; i < DIM_ZMAT; i++)
   {
      for ( j=0; j < DIM_ZMAT; j++)
      {
         fprintf(stdout, "tran[%d][%d]= %f ", i, j, transmat1[i][j]);
      }
      fprintf(stdout, "\n");
   }

   if (argv[2])
   {
      if (fp2->ropen( argv[2]))
      {
         char token2[4096];
         get_next_token(fp2, token2);
         oct2 = new Octree();
         oct2->parse_in(fp2);
         od2 = oct2->get_data();
         objn2 = new ObjNode();
         objn2->set_object( oct2);
         oct2->GetModelToObjectTransform( transmat2 );
      }
   }

    //Initialize max_points
    for (i=0;i<5;i++)
       for (j=0;j<3;j++)
	  max_points[i][j] = -FLT_MAX;

    if (!od1)
    {
	fprintf(stderr,"Warning: %s's octree data pointer is null\n", argv[1]);
	exit(1);
    }
    else
        traverse_octree(od1, transmat1);

    fprintf(stdout, "The top 5 points based on max Z for %s are: \n", argv[1]);
    for (i=0;i<5;i++)
    {
          fprintf(stdout, "%f %f %f\n", max_points[i][0], max_points[i][1], 
               max_points[i][2]);
    }

   // read in octree 2, if one has been specified
   if (argv[2])
   {
      //Re-initialize max_points
      for (i=0;i<5;i++)
         for (j=0;j<3;j++)
            max_points[i][j] = -FLT_MAX;

      traverse_octree(od2, transmat2);

      fprintf(stdout, "\nThe top 5 points based on max Z for %s are: \n", argv[2]);
      for (i=0;i<5;i++)
      {
         fprintf(stdout, "%f %f %f\n", max_points[i][0], max_points[i][1], 
               max_points[i][2]);
      }
   }
}
