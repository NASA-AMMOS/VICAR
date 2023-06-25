// vtkcubeoctree.C 1.8 01/04/06 13:13:51
/** \file
* Render octree or forest as a bunch of cubes, or normal-tangent squares
*/
#include "forest.h"
#include "vtk.h"
#include <math.h>

long	voxel_count;
long	non_red_count;
int 	normals=FALSE;	// draw squares tangent to cell normal vectors?
ZMatrix normalxf;	// model-to-object transform matrix for normals
vtkRenderer *renderer;

/// Recursive scan
void parse_octree(Octree_Data *oct, ZMatrix xform=NULL, 
	double xedge=1.0, double yedge=1.0, double zedge=1.0)
{
    vtkActor *volActor;
    vtkProperty *actorColorProp;
    NodeSpec	*ns;
    double	center[3], tcenter[3];
    float	vtk_ctr[3];
    int	ref;
    int		r,g,b;
    float	colors[3]; // red, green, blue

	// if NULL pointers then return
	if(!oct)return;

	// if non-leaf node, render all my children
	if((oct->any_children())) {
		// recursively process each valid child of node
		for(ref=UPPER_LEFT_BACK; ref<NUMBER_OF_SPACE_PARTITIONS; ref++) {
			parse_octree(oct->get_child((Octree_Child_Ref)ref), 
				xform, xedge, yedge, zedge);
		}
	}

	// add this cell's voxels to renderer
	if ((ns = oct->get_node_data()) == NULL)
	    return;

	while (ns) {
		voxel_count++;
		ns->get_color(&r, &g, &b);
		if(r == 0 || g != 0 || b != 0) non_red_count++;

		colors[0] = (float) r/255.0;
		colors[1] = (float) g/255.0;
		colors[2] = (float) b/255.0;

    		ldparam *ctr = ns->get_global_center();
		if (ctr == NULL)
		{
			fprintf(stderr,
				"vtkcubeoctree:  get_global_center() failed for node.\n");
			return;
		}

		center[0] = ctr->get_value(0);
		center[1] = ctr->get_value(1);
		center[2] = ctr->get_value(2);
		if(xform) {		// transform to object coordinates
			MultPoints(center, xform, tcenter);
			vtk_ctr[0] = (float)(tcenter[0]);
			vtk_ctr[1] = (float)(tcenter[1]);
			vtk_ctr[2] = (float)(tcenter[2]);
		} else {
			vtk_ctr[0] = (float)(center[0]);
			vtk_ctr[1] = (float)(center[1]);
			vtk_ctr[2] = (float)(center[2]);
		}

    		vtkPolyDataMapper *volMapper = vtkPolyDataMapper::New();

		ldparam *norm = ns->get_normal();
		if (normals && norm && norm->get_num()) {
			// want normal plane, and normal is defined
			vtkPlaneSource *plane = new vtkPlaneSource;
			float edge = ns->edge_length * xedge / 2.0;
			plane->SetOrigin(-edge, -edge, 0.0);
			plane->SetPoint1(edge, -edge, 0.0);
			plane->SetPoint2(-edge, edge, 0.0);
			// rotate plane tangent to normal
			if (xform) {	// transform normal
				double nmod[3], nobj[3];
				nmod[0] = norm->get_value(0);
				nmod[1] = norm->get_value(1);
				nmod[2] = norm->get_value(2);
				MultPoints(nmod, normalxf, nobj);
				plane->SetNormal(nobj[0], nobj[1], nobj[2]);
			} else {
				plane->SetNormal(norm->get_value(0),
					norm->get_value(1), 
					norm->get_value(2));
			}
			plane->SetCenter(vtk_ctr);
			volMapper->SetInput(plane->GetOutput());
		} else {
			// show cube voxel
    			vtkCubeSource *cube = new vtkCubeSource;
			cube->SetXLength(ns->edge_length * xedge);
			cube->SetYLength(ns->edge_length * yedge);
			cube->SetZLength(ns->edge_length * zedge);
			cube->SetCenter(vtk_ctr);
			volMapper->SetInput(cube->GetOutput());
		}

		volActor = vtkActor::New();
		volActor->SetMapper(volMapper);
		volActor->SetMapper(volMapper);
		actorColorProp = volActor->GetProperty();
		actorColorProp->SetColor(colors);
		renderer->AddActor(volActor);

		ns=ns->next;
	} 

}

main (int argc, char **argv)
{

	char	usage[] = "Usage: vtkcubeoctree [-/-h/-help] [-t] [-n] [-i input_file]\n\
vtkcubeoctree displays an octree or forest using vtk.\n\
Octrees are shown in model space, or transformed to object space if\n\
the -t option is used. Forests are shown in world space.\n\
Option -n uses surface normals to show flat planes instead of cubes.\n\
If the input file is not specified with the -i option, stdin is used.\n\n";

	char	*fname=NULL;
	int	i, t_flag=FALSE;
	ZMatrix	xform;

	for(i=1; i<argc; i++) {
		if(!strcmp(argv[i],"-") || !strcmp(argv[i],"-h") || !strcmp(argv[i],"-help")) {
			fprintf(stderr,"%s",usage);
			exit(0);
		} else if(!strcmp(argv[i],"-t")) {
			t_flag = TRUE;
		} else if(!strcmp(argv[i],"-n")) {
			normals = TRUE;
		} else if(!strcmp(argv[i],"-i")) {
			fname = argv[++i];
		} else {
			fprintf(stderr,"Unrecognized option %s\n", argv[i]);
			fprintf(stderr,"%s",usage);
			exit(10);
		}
	}

	voxel_count = 0;
	non_red_count = 0;

    renderer = vtkRenderer::New();
    if (renderer == NULL) {
	fprintf(stderr, "can't create renderer\n");
	exit(1);
    }
    vtkRenderWindow *renWin = vtkRenderWindow::New();
	renWin->AddRenderer(renderer);
    vtkRenderWindowInteractor *iren = vtkRenderWindowInteractor::New();
	iren->SetRenderWindow(renWin);

    // read input model
        FILE_Dataport  *fp;
        fp = new FILE_Dataport();
	if(fname) {
		fp->open(fname);
	} else {
		fp->open(stdin);
	}
	char    token[4096];
	get_next_token(fp, token);      // get model type

	Octree *octree;

	if (!strcmp(token, "SFC_MODEL_V1"))
		get_next_token(fp, token);
	if (!strcmp(token, "OCTREE_V1")) {
       		// make an octree shape
		octree = new Octree(10);       // allow 10 levels
		octree->parse_in(fp);

		if (t_flag) {
			fprintf(stderr,
				"Transforming to Object Space for display.\n");
			octree->GetModelToObjectTransform(xform);
			// normal transform is transpose of inverse of xform
			MatInvert(xform, normalxf);
			MatTranspose(normalxf, normalxf);
			parse_octree(octree->get_data(), xform, 
				octree->xscale.get_value(),
				octree->yscale.get_value(), 
				octree->zscale.get_value());
		} else {
			parse_octree(octree->get_data());
		}

	} else if (!strcmp(token, "GRP_V1")) {
		Forest forest;
		forest.parse_in(fp);
		for (i=0; i<forest.get_num_children(); i++) {
			ObjNode *node = forest.get_child(i);
			octree = (Octree *)node->get_object();
			// model to world transform
			node->GetTransformationMatrix(xform);
			// normal transform is transpose of inverse of xform
			MatInvert(xform, normalxf);
			MatTranspose(normalxf, normalxf);
			parse_octree(octree->get_data(), xform, 
				octree->xscale.get_value(),
				octree->yscale.get_value(), 
				octree->zscale.get_value());
		}
	} else {
		fprintf(stderr, "Unknown input type %s\n", token);
		exit(1);
	}
	fp->close();

	fprintf(stderr,"Found %d voxels\n", voxel_count);
	fprintf(stderr,"Found %d non-red voxels\n", non_red_count);

    renderer->SetBackground(0.7,0.5,0.5);
    renWin->SetSize(450,450);

    // interact with data
    renWin->Render();
    iren->Start();


    // Clean up
    renderer->Delete();
    renWin->Delete();
    iren->Delete();
}
