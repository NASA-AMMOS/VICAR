// perf_renderer.C

#ifdef _USE_PERFORMER_

#include "grape/perf_renderer.h"


int PerfRenderer::set_scene(Scene *scn, ImgInfo *img_nfo)
{
    ObjNode	*tmpObjNode=NULL;
	ImgInfo	*tmpImgInfo=NULL;
	pfGeoState *globalGSt=NULL;

	int	output_method;	// method for disposal of images
	int xres=0,yres=0;	// output size in pixels

    last_rendered = CHANGE_COUNTER;

	scene = scn;	// give the renderer the scene pointer

    if(img_nfo != NULL) {
		tmpImgInfo = img_nfo;
	}
	else {		// use image info associated with the scene camera
		if((tmpObjNode = scn->get_camera()) == NULL) {
			fprintf(stderr," Whoops, Grape scene doesn't have a camera - aborting\n");
			return(FALSE); // user should supply a camera

/*			tmpObjNode = new ObjNode(); // create camera
			tmpObjNode->set_object(new Camera());
			scene->set_camera(tmpObjNode);
*/
		}
		// associate an ImgInfo w/ the scene camera if there isn't one already
		if((tmpImgInfo = ((Camera *)(tmpObjNode->get_object()))->get_img_info()) == NULL) {
			fprintf(stderr," Scene camera has no ImgInfo - aborting\n");
			return(FALSE); // user should supply an ImgIfo from somewhere

/*			tmpImgInfo = new ImgInfo(); // make an ImgInfo
			((Camera *)(tmpObjNode->get_object()))->set_img_info(tmpImgInfo);
*/
		}
    }

//	tmpImgInfo->prepare();		// should be done before calling render

	if(perfScn != NULL)
		pfFree(perfScn);
	perfScn = new pfScene;

	globalGSt = perfScn->getGState();
	if(globalGSt == NULL)
	    globalGSt = new pfGeoState();

	// set global pfGeoState stuff here to be inherited by all objects

	perfScn->setGState(globalGSt);

	tmpImgInfo->get_res(&xres, &yres);	// get the window size

/*********************************************************************
 * determine the output method
 */

	output_method = tmpImgInfo->get_output_method();

/*	if(output_method == OUT_FILE) { // not yet implemented
	}
	else if(output_method == OUT_XDRW) { // Use a given X Drawable pointer
	    perfPipe = pfGetPipe(0);
		perfPW = new pfPipeWindow(perfPipe);

		// set stuff
		pfInitGfx(); // instead of pfPipeWindow->open()
	}
*/
	if(output_method == OUT_XWIN) // Use a given X Window pointer
	{
		perfPipe = pfGetPipe(0);
		perfPW = new pfPipeWindow(perfPipe);
	    if(tmpImgInfo->get_xdisplay() && tmpImgInfo->get_xwindow())
		{	// xwindow and xdisplay are defined
			perfPW->setWinType(PFPWIN_TYPE_X);
			perfPW->setWSWindow(tmpImgInfo->get_xdisplay(),
								tmpImgInfo->get_xwindow());
			perfPW->setName("Performer Renderer Out - Xwin"); // doesn't seem to work
		}
		else {
		    
		    if(tmpImgInfo->get_xdisplay() == NULL)
				fprintf(stderr," Whoops - could not find xdisplay to draw to\n");
			if(tmpImgInfo->get_xwindow() == NULL)
				fprintf(stderr," Whoops - could not find xwindow to draw to\n");
			fprintf(stderr,"..using Performer default\n");
			perfPW->setName("Performer Renderer Out - default");
		}
		perfPW->setOriginSize(0,0,xres,yres);
		perfPW->open();
	}
	else {
	    perfPipe = pfGetPipe(0);
		perfPW = new pfPipeWindow(perfPipe);
		perfPW->setName("Performer Renderer Out - default");
		perfPW->setOriginSize(0,0,xres,yres);
		perfPW->open();
	    fprintf(stderr," Whoops - not a known output method: %d, using Performer default\n", output_method);
	}

/*********************************************************************
 * having opened a window, set the scene and the view port to draw to the
 * channel associated with that window
 */

	perfChan = new pfChannel(perfPipe);
	perfChan->setScene(perfScn);	// objects are yet to be added to perfScn

/*	pfLightModel *lm = pfGetCurLModel();
	if(lm->getTwoSide() != PF_ON)
	    lm->setTwoSide(PF_ON);
//	if(pfGetCullFace() != PFCF_OFF)	// set culling in each pfGeoState with
//	    pfCullFace(PFCF_OFF);		// setMode(PFSTATE_CULLFACE,PFCF_OFF);

	set_backface_cull(FALSE); // show both front and back sides of polys
*/
	set_two_sided_normals(TRUE); // proper lighting on both sides of polys

/*********************************************************************
 * construct Grape object heirarchy in Performer
 */
    int		k;
	int		num_objects = scene->get_num_objects();

	for(k = 0; k<num_objects; k++)
	{
	    tmpObjNode = scene->get_obj(k); // get ObjNode pointer

		if(tmpObjNode->get_object()->get_type() == PERF_V1)
		{
		  if(!add_PerfObj(tmpObjNode)) {
			fprintf(stderr," Whoops - error adding PerfObj %d to scene\n",k);
			return(FALSE);
		  }
		}
		else if(tmpObjNode->get_object()->get_type() == POLY_V1
				||
				tmpObjNode->get_object()->get_type() == POLY_TERRAIN_V1)
		{
		  if(!add_poly_obj(tmpObjNode)) {
			fprintf(stderr," Whoops - error adding poly_object %d to scene\n",k);
			return(FALSE);
		  }
		}
		else if(tmpObjNode->get_object()->get_type() == DIRECTIONAL_LIGHT_V1)
		{
		  if(!add_light_obj(tmpObjNode)) {
			fprintf(stderr," Whoops - error adding light_object %d to scene\n",k);
			return(FALSE);
		  }
		}
	}

	// camera objects are different in Performer so process here
	// only supports one camera for now, not an array of cameras
	if(!set_view(scene->get_camera())) {
		fprintf(stderr," Whoops - error setting camera in scene\n");
		return(FALSE);
	}

	return(TRUE);	// success!

}	// set_scene

int PerfRenderer::render(ObjNode *CamON, ImgInfo *img_nfo)
{
	// Currently the img_nfo arg means nothing here, only in set_scene.
	// Eventually render will care about output methods, but for now
	// it just does what set_scene set for the output method.

	ObjNode *tmpObjNode;

	if(perfScn->getNumChildren() < 0) {
	    fprintf(stderr," Whoops - Internal Performer scene heirarchy is empty - did you call set_scene before calling render?\n");
	    return(FALSE);
	}

//	if(img_nfo)img_nfo->get_res(&xres, &yres);	// get the window size
//	perfPW->setSize(xres,yres);	// set window size (in case of a window resize

/*********************************************************************
 * traverse Grape heirarchy looking for changed items to update in Performer
 * (right now, it always updates only transf. matrices
 */

    int		k;
	int		num_objects = scene->get_num_objects();

	for(k = 0; k<num_objects; k++)
	{
	    tmpObjNode = scene->get_obj(k); // get ObjNode pointer

		if(tmpObjNode->get_object()->get_type() == PERF_V1)
		{
		  if(!process_PerfObj(tmpObjNode)) {
			fprintf(stderr," Whoops - error processing PerfObj %d\n",k);
			return(FALSE);
		  }
		}
		else if(tmpObjNode->get_object()->get_type() == POLY_V1
				||
				tmpObjNode->get_object()->get_type() == POLY_TERRAIN_V1)
		{
		  if(!process_poly_obj(tmpObjNode)) {
			fprintf(stderr," Whoops - error processing poly_object %d\n",k);
			return(FALSE);
		  }
		}
		else if(tmpObjNode->get_object()->get_type() == DIRECTIONAL_LIGHT_V1)
		{
		  if(!process_light_obj(tmpObjNode)) {
			fprintf(stderr," Whoops - error processing light_object %d\n",k);
			return(FALSE);
		  }
		}
	}

	// camera objects are different in Performer so process here
	// only supports one camera for now, not an array of cameras
	if(CamON == NULL) {
	  if(!set_view(scene->get_camera())) {
		fprintf(stderr," Whoops - error setting camera in scene\n");
		return(FALSE);
	  }
	} else {
	  if(!set_view(CamON)) {
		fprintf(stderr," Whoops - error setting camera in scene\n");
		return(FALSE);
	  }
	}

/*********************************************************************
 * Draw the frame
 */

    pfFrame();

//	img_nfo->dispose(CHANGE_COUNTER -1);	// should be done outside renderer

	last_rendered = CHANGE_COUNTER;

	return(TRUE);	// success!

}	// render()

int PerfRenderer::add_PerfObj(ObjNode *PerfON)
{
    // attach the actual pfDCS ptr to the actual pfScene
	PerfObj *tmpPerfObj = (PerfObj *)PerfON->get_object();

	// apply filename to initialize object
	if(!tmpPerfObj->set_file()) {
		fprintf(stderr," Whoops - couldn't set filename in PerfObj %d\n",
				PerfON->get_object()->get_id());
		return(FALSE);
	}
	if(!perfScn->addChild(tmpPerfObj->get_pfDCS_ptr())) {
		fprintf(stderr," Whoops - couldn't add PerfObj %d to perfScn.\n",
				PerfON->get_object()->get_id());
		return(FALSE);
	}

	return(TRUE);
}

int PerfRenderer::process_PerfObj(ObjNode *PerfON)
{
	if(!((PerfObj *)(PerfON->get_object()))->apply_transforms(PerfON)) {
		fprintf(stderr," Whoops - error applying transforms to PerfObj %d.\n",
				PerfON->get_object()->get_id());
		return(FALSE);
	}

	return(TRUE);
}

int PerfRenderer::add_poly_obj(ObjNode *PolyON, int k)
{
	if(k == -1)
	    k = PolyON->get_object()->get_id();	// used to 'name' pfDCS for later retrieval

	int num_colors;	// number of colors in object
	int num_edges;	// used to optimize wireframing
	int num_polys;	// number of polygons
	int num_points;	// total number of vertices
	int num_points_in_poly;
	int point_index;	// index of vertex in grape poly_object
	int nrml_index;		// index of normal in grape poly_object
	int color_index;
	double  vert[3], texel[3], norm[3];
	float  fvert[3], ftexel[3], fnorm[3];
	double  red,green,blue;
	pfVec4 cv;
	cv.set(-1.0, -1.0, -1.0, 1.0);		// temp face color vect
//	pfVec4 *face_colors;	// list of colors for all prims (per prim)

/*	pfTexture	*tmpTexture=NULL;
	pfTexEnv	*tenv=NULL;
	uint *timage=NULL;	// image for the pfTexture
	int nchan=0, ntxlsx=0, ntxlsy=0, ntxlsz=0;	// num channels and num texels
*/
//	int num_tri_strips=0;
//	int curr_tri_strip=0;
	int *striplengths=NULL;	// list of lengths of each strip of tris
	pfGeoSet *tristrips=NULL;
	pfVec3 *ts_coords=NULL, *ts_norms=NULL;	// lists of verts and vertex normal vects
	pfVec4 *ts_colors=NULL;					// list of colors (per prim)
	pfVec2 *ts_tcoords=NULL;				// list of texture coords
	ushort *ts_nindex=NULL, *ts_vindex=NULL, *ts_cindex=NULL, *ts_tindex=NULL;	// lookup lists

	int num_tris=0;
	int curr_tri=0;
	pfGeoSet *tris=NULL;
	pfVec3 *tri_coords=NULL, *tri_norms=NULL;	// lists of verts and vertex normal vects
	pfVec4 *tri_colors = NULL;					// list of colors (per prim)
	pfVec2 *tri_tcoords = NULL;					// list of texture coords
//	ushort *tri_tindex=NULL;					// lookup lists

	int num_quads=0;
	int curr_quad=0;
	pfGeoSet *quads=NULL;
	pfVec3 *quad_coords=NULL, *quad_norms=NULL;	// lists of verts and vertex normal vects
	pfVec4 *quad_colors=NULL;					// list of colors (per prim)
	pfVec2 *quad_tcoords=NULL;					// list of texture coords
//	ushort *quad_tindex=NULL;					// lookup lists

	int num_gen_polys=0;
	int curr_gen_poly=0;
	int ttl_gen_poly_verts=0;
	int *gen_poly_lengths=NULL;		// list of number of verts in each general poly
	pfGeoSet *gen_polys=NULL;
	pfVec3 *gen_poly_coords=NULL, *gen_poly_norms=NULL;	// lists of verts and vertex normal vects
	pfVec4 *gen_poly_colors=NULL;				// list of colors (per prim)
	pfVec2 *gen_poly_tcoords=NULL;				// list of texture coords
//	ushort *gen_poly_tindex=NULL;				// lookup lists

    pfGeoState *gstate = new pfGeoState();	// attach to each pfGeoSet
	pfGeode *PolyGeode = new pfGeode(); // attach pfGeoSets to this
	pfDCS *PolyDCS = new pfDCS();
	pfHighlight *hlit=NULL;

	int dummy;
	int force_normal_calcs = FALSE;
	int flatshading = FALSE;
	gstate->setMode(PFSTATE_ENLIGHTING,PF_OFF);	// disable lighting/normals
//	int bDoNormals = FALSE;	// true if normals be calculated

	PolyObject1 *tmpPolyObj = (PolyObject1 *)PolyON->get_object();

	num_colors = tmpPolyObj->get_num_colors();
	num_edges  = tmpPolyObj->get_num_edges();
	num_polys  = tmpPolyObj->get_num_faces();	// number of polys
	num_points = tmpPolyObj->get_num_points();	// total number of vertices

	// see if we need to generate normals
	if (tmpPolyObj->get_obj_model_type() == SmoothShaded ||
		force_normal_calcs) {
		gstate->setMode(PFSTATE_ENLIGHTING,PF_ON);	// we need to use normals
//		bDoNormals = TRUE;		// we need to use normals

								// generate normals only if there are not
								// any point normals already defined; for now
								// assume that we will never want to re-generate
								// normals: in other words, calc normals one time
								// per object
		fprintf(stderr,"Building t_mesh normal structure...");
		tmpPolyObj->build_normal_structure(); // make space
		fprintf(stderr,"done.\nGenerating normals...");
		tmpPolyObj->generate_normals();		  // calc normals
		fprintf(stderr,"done.\n");

	}
	else if (tmpPolyObj->get_obj_model_type() == TextureMapped) {
		gstate->setMode(PFSTATE_ENLIGHTING,PF_OFF);	// do not use normals
//		bDoNormals = 0;			// do not use normals
		fprintf(stderr,"Texture mapping...\n");
		pfEnable(PFEN_TEXTURE);
	}
	else if (tmpPolyObj->get_obj_model_type() == FlatShaded) {
		gstate->setMode(PFSTATE_ENLIGHTING,PF_OFF);	// do not use normals
//		bDoNormals = 0;         // do not use normals
		flatshading = TRUE;
	}

//	tmpPolyObj->dump();		// show the object

/*
	// handle textures, if any
	// 1st, load named texture, if named
	if (tmpPolyObj->get_texture_name() != NULL) {
		if (!tmpPolyObj->load_texture_image()) {
			fprintf( stderr, "Unable to load texture image file %s\n",
					tmpPolyObj->get_texture_name());
		}
	}

	// if Image exists, prepare it
	if (tmpPolyObj->surface.get_tmap_color() != NULL
		&&
		tmpPolyObj->surface.get_tmap_color()->get_texture() != NULL) {

		timage = convert_image_to_uint(
					tmpPolyObj->surface.get_tmap_color()->get_texture(),
					&ntxlsx, &ntxlsy, &nchan);
		if(nchan != 4) {
			fprintf(stderr,"Only textures w/ 4 bands are supported right now.\n");
			return(FALSE);
		}
		// set image into pfTexture - num z texels always 1 for now -
		// we're only doing 2D textures
		ntxlsz=1;
		pfEnable(PFEN_TEXTURE);
		tmpTexture = new pfTexture();
		tmpTexture->setImage(timage,nchan,ntxlsx,ntxlsy,ntxlsz);
//		int tmp = tmpTexture->getFormat(PFTEX_EXTERNAL_FORMAT);
//		uint *i=NULL; int nc=0,sx=0,sy=0,sz=0;
//		tmpTexture->getImage(&i,&nc,&sx,&sy,&sz);
//		tmpTexture->setLoadImage(timage);
//		tmpTexture->setLoadSize(ntxlsx,ntxlsy);
		tmpTexture->setName("terrain");
		tmpTexture->apply();
//		int tmp = tmpTexture->isFormatted();
//		if(tmpTexture->getFormat(PFTEX_INTERNAL_FORMAT)==PFTEX_RGBA_4)
//		  fprintf(stderr,"PFTEX_RGBA_4\n");
//		if(tmpTexture->getFormat(PFTEX_EXTERNAL_FORMAT)==PFTEX_PACK_8)
//		  fprintf(stderr,"PFTEX_PACK_8\n");
//		pfTexture *tmptx = pfGetCurTex();
//		fprintf(stderr,"");	// so you can stop here in dbx
	}
*/
    if (tmpPolyObj->get_type() == POLY_TERRAIN_V1) {
	    int i, j;
		int r, c;	// row and col index of current poly in mesh
		int xf, yf;	// horiz and vertcl meshing factors (num of rows and cols)
		int nTrianglesPerStrip;
		int actual_num_points;	// total number of vertices in mesh
								// (some are shared between strips)
		int apparent_num_points;// number of vertices if tri_strips were independant
		int curr_vert_count = 0;	// goes from 0 to apparent_num_points
		int last_vert_added = 0;	// goes from 0 to actual_num_points

		PolyTerrainObj *pTerr = (PolyTerrainObj *)tmpPolyObj;

		// do the math
		pTerr->get_mesh_factor( xf, yf);
		nTrianglesPerStrip = xf * 2;
		actual_num_points = (xf+1)*(yf+1);	// number of pfVec's to malloc
		apparent_num_points = 2*(xf+1)*yf;	// number of points performer will
											// index over all strips
		striplengths = (int *)pfMalloc(yf * sizeof(int));
		tristrips = new pfGeoSet();
//		striplengths = new int[yf];
/*start here*/
		ts_coords = (pfVec3 *)pfMalloc(actual_num_points * sizeof(pfVec3));
		ts_vindex = (ushort *)pfMalloc(apparent_num_points * sizeof(ushort));
//		if (bDoNormals) {
		if (gstate->getMode(PFSTATE_ENLIGHTING) == PF_ON) {
		    ts_norms = (pfVec3 *)pfMalloc(actual_num_points * sizeof(pfVec3));
			ts_nindex = (ushort *)pfMalloc(apparent_num_points * sizeof(ushort));
		}
		ts_colors = (pfVec4 *)pfMalloc(num_points * sizeof(pfVec4));
		ts_cindex = (ushort *)pfMalloc(num_points * sizeof(ushort));
/*		if (tmpTexture != NULL) {
			ts_tcoords = (pfVec2 *)pfMalloc(num_points * sizeof(pfVec2));
			ts_tindex = (ushort *)pfMalloc(num_points * sizeof(ushort));
		}
*/
		fprintf(stderr,"Loading t_mesh into Performer...\n");
		for (r = 0; r < yf; r++) { // for each row of triangles

			for(c=0; c< nTrianglesPerStrip; c++) {
				
				i = r * nTrianglesPerStrip + c;	// index of triangle in poly list
				
				// loop for number of triangles in row
				
				num_points_in_poly = tmpPolyObj->get_num_verts(i);	// this better be 3
				if(num_points_in_poly != 3) {
				    fprintf(stderr," Whoops - incorrect number of points in terrain polygon\n");
				    return(FALSE);
				}
				// set the color
				color_index = tmpPolyObj->get_pcolor(i);
				// leave this out for now - have to do weird indexing for t-strips
/*				if (tmpPolyObj->get_color(color_index,fred,fgreen,fblue) != FALSE) {
//					glColor3f(fred,fgreen,fblue);
					
					if (fred != cv[0] ||	// material color has changed, update it
						fgreen != cv[1] ||
						fblue != cv[2]) {
						
						cv[0] = fred; cv[1] = fgreen; cv[2] = fblue; cv[3] = 1.0;
//						glMaterialfv( GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, (const GLfloat *)cv);
					}
				}
*/				
				// for each vertex in the polygon
				for(j=0; j<num_points_in_poly; j++) {
					
					if (c && j != num_points_in_poly - 1) // 3rd point only
						// unless first triangle in strip
						continue;	// go back to top of loop

					point_index = tmpPolyObj->get_vert(i,j); // get pt's index
					
					// deal with normals, if requested
//					if (bDoNormals) {
					if (gstate->getMode(PFSTATE_ENLIGHTING) == PF_ON) {
						
						// get index of normal that corresponds to the
						// point_index-th vertex
						nrml_index = tmpPolyObj->get_pt_normal( point_index);
						
						// get the normal data
						if (tmpPolyObj->get_normal( nrml_index, norm[0], norm[1], norm[2])
							== TRUE)	{ 
							
							fnorm[0] = norm[0];
							fnorm[1] = norm[1];
							fnorm[2] = -norm[2];	// reverse z-component
						
							// got valid normal data
							// only add new points (not repeated data)

							if(r == 0) { // first row only
							    ts_norms[last_vert_added].set(fnorm[0],fnorm[1],fnorm[2]);
								ts_nindex[curr_vert_count] = curr_vert_count;
							}
							else {
							    if(curr_vert_count % 2 != 0) { // odd index
							    ts_norms[last_vert_added].set(fnorm[0],fnorm[1],fnorm[2]);
								ts_nindex[curr_vert_count] = last_vert_added;
								}
								else // even perf vert index
								    ts_nindex[curr_vert_count] = ts_vindex[curr_vert_count - (nTrianglesPerStrip+1)];
							}
						}
					}

					// get pt's coords
					tmpPolyObj->get_point(point_index,dummy,fvert[0],fvert[1],fvert[2]);
					
					// specify texture coords, if applicable
					// add to vertex list if new vertex
					if(r == 0) { // first row only
/*						if (tmpTexture != NULL) {
							tmpPolyObj->surface.get_tmap_color()->get_texture_coord( 
										  vert, texel);
							ts_tcoords[last_vert_added].set(texel[0],texel[1]);
							ts_tindex[curr_vert_count] = curr_vert_count;
						}
*/
					    ts_coords[last_vert_added].set(fvert[0],fvert[1],fvert[2]);
						ts_vindex[curr_vert_count] = curr_vert_count;
						last_vert_added++;
					}
					else {
					    if(curr_vert_count % 2 != 0) {	// odd index
/*							if (tmpTexture != NULL) {
								tmpPolyObj->surface.get_tmap_color()->get_texture_coord( 
										  vert, texel);
								ts_tcoords[last_vert_added].set(texel[0],texel[1]);
								ts_tindex[curr_vert_count] = last_vert_added;
							}
*/
							ts_coords[last_vert_added].set(fvert[0],fvert[1],fvert[2]);
							ts_vindex[curr_vert_count] = last_vert_added;
							last_vert_added++;
						}
						else { // even perf vert index
						    ts_vindex[curr_vert_count] = ts_vindex[curr_vert_count -
														 (nTrianglesPerStrip+1)];
							ts_tindex[curr_vert_count] = ts_tindex[curr_vert_count -
														 (nTrianglesPerStrip+1)];
						}
					}
					curr_vert_count++;

				}	// for j
				
			}	// for c
			
			striplengths[r] = nTrianglesPerStrip + 2;	// end of the current strip

		}	// for r

//		print_indeces(ts_vindex, xf, yf);
//		print_indeces(ts_nindex, xf, yf);

		if(flatshading)
		    tristrips->setPrimType(PFGS_FLAT_TRISTRIPS);
		else
			tristrips->setPrimType(PFGS_TRISTRIPS);

		tristrips->setNumPrims(yf);	// set number of strips
		tristrips->setPrimLengths(striplengths);	// set each strip length

	}			// end POLY_TERRAIN_V1

	else if (tmpPolyObj->get_type() == POLY_V1) {

	    int i,j;

	    // determine number of verts in each prim, and tally them up
		for(i=0; i<num_polys; i++) {
			num_points_in_poly = tmpPolyObj->get_num_verts(i);
			if(num_points_in_poly == 3)
			    num_tris++;
			else if(num_points_in_poly == 4)
			    num_quads++;
			else if(num_points_in_poly > 4) {	// polygon with an arbitrary vertex count
				ttl_gen_poly_verts += num_points_in_poly;
			    num_gen_polys++;
			}
		}

		// allocate memory for each kind of primative and it's elements
		if(num_tris > 0) {
		    tris = new pfGeoSet();
			tris->setPrimType(PFGS_TRIS);
			tris->setNumPrims(num_tris);

			tri_coords = (pfVec3 *)pfMalloc(3 * num_tris * sizeof(pfVec3));
//			if (bDoNormals)
			if (gstate->getMode(PFSTATE_ENLIGHTING) == PF_ON)
			    tri_norms = (pfVec3 *)pfMalloc(3 * num_tris * sizeof(pfVec3));
			tri_colors = (pfVec4 *)pfMalloc(num_tris * sizeof(pfVec4));
/*			if(tmpTexture != NULL)
				tri_tcoords = (pfVec2 *)pfMalloc(3 * num_tris * sizeof(pfVec2));
*/
		}
		if(num_quads > 0) {
		    quads = new pfGeoSet();
			quads->setPrimType(PFGS_QUADS);
			quads->setNumPrims(num_quads);

			quad_coords = (pfVec3 *)pfMalloc(4 * num_quads * sizeof(pfVec3));
//			if (bDoNormals)
			if (gstate->getMode(PFSTATE_ENLIGHTING) == PF_ON)
			    quad_norms = (pfVec3 *)pfMalloc(4 * num_quads * sizeof(pfVec3));
			quad_colors = (pfVec4 *)pfMalloc(num_quads * sizeof(pfVec4));
/*			if(tmpTexture != NULL)
				quad_tcoords = (pfVec2 *)pfMalloc(4 * num_quads * sizeof(pfVec2));
*/
		}
		if(num_gen_polys > 0) {
		    gen_polys = new pfGeoSet();
			gen_polys->setPrimType(PFGS_POLYS);
			gen_polys->setNumPrims(num_gen_polys);
			// need to set lengths of each prim later

			gen_poly_lengths = (int *)pfMalloc(num_gen_polys * sizeof(int));
			gen_poly_coords = (pfVec3 *)pfMalloc(ttl_gen_poly_verts * sizeof(pfVec3));
//			if (bDoNormals)
			if (gstate->getMode(PFSTATE_ENLIGHTING) == PF_ON)
			    quad_norms = (pfVec3 *)pfMalloc(ttl_gen_poly_verts * sizeof(pfVec3));
			gen_poly_colors = (pfVec4 *)pfMalloc(num_gen_polys * sizeof(pfVec4));
/*			if(tmpTexture != NULL)
				gen_poly_tcoords = (pfVec2 *)pfMalloc(ttl_gen_poly_verts * sizeof(pfVec2));
*/
		}

//		face_colors = (pfVec4 *)pfMalloc(num_colors * sizeof(pfVec4));

		for(i=0; i<num_polys; i++) { // loop for number of faces in object
			num_points_in_poly = tmpPolyObj->get_num_verts(i);

			if(num_points_in_poly == 3) {	// process triangles

			    // set the color
			    color_index = tmpPolyObj->get_pcolor(i);

				if (tmpPolyObj->get_color(color_index,red,green,blue) != FALSE) {
					tri_colors[curr_tri].set(red,green,blue,1.0);	// set RGBA for face
				}

				// for each vertex in the polygon
				for(j=0; j<3; j++) {			// process triangles
					point_index = tmpPolyObj->get_vert(i,j); // get vert's index

					// deal with normals, if requested
//					if (bDoNormals) {
					if (gstate->getMode(PFSTATE_ENLIGHTING) == PF_ON) {
						// get index of corresponding normal
						nrml_index = tmpPolyObj->get_pt_normal( point_index);

						// get the normal data
						if (tmpPolyObj->get_normal(nrml_index, norm[0],
												   norm[1], norm[2]) == TRUE)
							// got valid normal data
						    // specify normal of following pts
							tri_norms[3*curr_tri + j].set(norm[0],norm[1],norm[2]);
					}

					// get pt's coords
					tmpPolyObj->get_point(point_index,dummy,vert[0],vert[1],vert[2]);

/*					// specify texture coords, if applicable
					if (pTextures[nObjectNdx].pTexture != NULL) {
						int TexelNdx;
						tmpPolyObj->surface.get_tmap_color()->get_texture_coord(vert,texel);
//						glTexCoord3dv( texel);
					}
*/
					// set the vertex
					tri_coords[3*curr_tri + j].set(vert[0],vert[1],vert[2]);

				}	// for j
				curr_tri++;

			}	// end tri

			else if(num_points_in_poly == 4) {	// process quads

			    // set the color
			    color_index = tmpPolyObj->get_pcolor(i);

				if (tmpPolyObj->get_color(color_index,red,green,blue) != FALSE) {
					quad_colors[curr_quad].set(red,green,blue,1.0);	// set RGBA for face
				}

				// for each vertex in the polygon
				for(j=0; j<4; j++) {
					point_index = tmpPolyObj->get_vert(i,j); // get pt's index

					// deal with normals, if requested
//					if (bDoNormals) {
					if (gstate->getMode(PFSTATE_ENLIGHTING) == PF_ON) {
						// get index of corresponding normal
						nrml_index = tmpPolyObj->get_pt_normal( point_index);

						// get the normal data
						if (tmpPolyObj->get_normal(nrml_index, norm[0],
												   norm[1], norm[2]) == TRUE)
							// got valid normal data
						    // specify normal of following pts
							quad_norms[4*curr_quad + j].set(norm[0],norm[1],norm[2]);
					}

					// get pt's coords
					tmpPolyObj->get_point(point_index,dummy,vert[0],vert[1],vert[2]);
/*
					// specify texture coords, if applicable
					if (pTextures[nObjectNdx].pTexture != NULL) {
						int TexelNdx;
						tmpPolyObj->surface.get_tmap_color()->get_texture_coord( 
										  vert, texel);
//						glTexCoord3dv( texel);
					}
*/
					// set the vertex
					quad_coords[4*curr_quad + j].set(vert[0],vert[1],vert[2]);

				}	// for j
				curr_quad++;

			}	// end quad
			  
			else if(num_points_in_poly > 4) {	// process general polygons

			    // set the color
			    color_index = tmpPolyObj->get_pcolor(i);

				if (tmpPolyObj->get_color(color_index,red,green,blue) != FALSE) {
					// set RGBA for face
					gen_poly_colors[curr_gen_poly].set(red,green,blue,1.0);
				}

				// for each vertex in the polygon
				for(j=0; j<num_points_in_poly; j++) { // go backwards
					point_index = tmpPolyObj->get_vert(i,j); // get pt's index

					// deal with normals, if requested
//					if (bDoNormals) {
					if (gstate->getMode(PFSTATE_ENLIGHTING) == PF_ON) {
						// get index of corresponding normal
						nrml_index = tmpPolyObj->get_pt_normal( point_index);

						// get the normal data
						if (tmpPolyObj->get_normal(nrml_index, norm[0],
												   norm[1], norm[2]) == TRUE)
							// got valid normal data
						    // specify normal of following pts
							gen_poly_norms[num_points_in_poly*curr_gen_poly
										  + j].set(norm[0],norm[1],norm[2]);
					}

					// get pt's coords
					tmpPolyObj->get_point(point_index,dummy,vert[0],vert[1],vert[2]);

					// specify texture coords, if applicable
/*					if (tmpTexture != NULL) {
						int TexelNdx;
						tmpPolyObj->surface.get_tmap_color()->get_texture_coord( 
										  vert, texel);
//						glTexCoord3dv( texel);
					}
*/
					// set the vertex
					gen_poly_coords[num_points_in_poly*curr_gen_poly
								   + j].set(vert[0],vert[1],vert[2]);

				}	// for j
				// store num of verts in this poly
				gen_poly_lengths[curr_gen_poly] = num_points_in_poly;
				curr_gen_poly++;

			}	// end gen_poly

			else {
			    fprintf(stderr,"Whoops, I don't know how to handle this polygon:\n");
				fprintf(stderr,"number of verts: %d, Grape poly_index: %d",num_points_in_poly,i);
			}
		}			// for i - finished traversing poly list
		if(gen_polys != NULL)
		    gen_polys->setPrimLengths(gen_poly_lengths);

	}			// end POLY_V1
	else
		fprintf(stderr,"Not a recognized kind of PolyObject or derivative type: %s\n",
				object_file_type_list[tmpPolyObj->get_type()]);


	// set GeoState to control things like lighting, surface material and texture mapping

	// do texture mapping, if specified
/*	if (tmpTexture != NULL)	{ // current object is texturemapped
		gstate->setMode(PFSTATE_ENTEXTURE,PF_ON);
		gstate->setAttr(PFSTATE_TEXTURE, tmpTexture);

//		tenv =  new pfTexEnv;
//		tenv->setMode(PFTE_DECAL); // default is PFTE_MODULATE
//		gstate->setAttr(PFSTATE_TEXENV, tenv);

		if(nchan == 4) {	// has an alpha channel
			gstate->setMode(PFSTATE_TRANSPARENCY, PFTR_FAST);
			// set alpha function to block pixels of 0 alpha for 
			// transparent textures
			gstate->setMode(PFSTATE_ALPHAFUNC, PFAF_NOTEQUAL);
			gstate->setVal(PFSTATE_ALPHAREF, 0.0f);
		}
	}
*/

//	if(bDoNormals) {
	if (gstate->getMode(PFSTATE_ENLIGHTING) == PF_ON) {
		// show normals for debugging purposes
		hlit = new pfHighlight();
		hlit->setMode(PFHL_NORMALS);
		gstate->setAttr(PFSTATE_HIGHLIGHT, hlit);
		gstate->setMode(PFSTATE_ENHIGHLIGHTING, PF_ON);

		gstate->setMode(PFSTATE_ENLIGHTING, PF_ON);
	}

/*	// material for testing purposes
	pfMaterial *redMtl = new pfMaterial;
	redMtl->setColor(PFMTL_DIFFUSE, 1.0f, 0.0f, 0.0f);

	// Disable color mode so the PFMTL_DIFFUSE color is not ignored
	redMtl->setColorMode(PFMTL_BOTH, PFMTL_CMODE_OFF);
//	redMtl->setColor(PFMTL_SPECULAR, 1.0f, 1.0f, 1.0f);
	redMtl->setColor(4, 1.0f, 1.0f, 1.0f); // KLUGE - use value, not token
	redMtl->setShininess(16.0f);

	gstate->setAttr(PFSTATE_BACKMTL, redMtl);
	gstate->setAttr(PFSTATE_FRONTMTL, redMtl);
*/

	gstate->setMode(PFSTATE_CULLFACE,PFCF_OFF);	// show all poly sides

	if(tristrips != NULL)	// construct GeoSet from elements created above
	{
	    tristrips->setAttr(PFGS_COORD3, PFGS_PER_VERTEX, ts_coords, ts_vindex);
//		if(bDoNormals)
		if (gstate->getMode(PFSTATE_ENLIGHTING) == PF_ON)
		    tristrips->setAttr(PFGS_NORMAL3, PFGS_PER_VERTEX, ts_norms, ts_nindex);
//		tristrips->setAttr(PFGS_COLOR4, PFGS_PER_PRIM, ts_colors, NULL);
/*		if(tmpTexture != NULL)
			tristrips->setAttr(PFGS_TEXCOORD2, PFGS_PER_VERTEX, ts_tcoords, ts_tindex);
*/
		tristrips->setGState(gstate);
		PolyGeode->addGSet(tristrips);
	}

	if(tris != NULL)	// construct GeoSet from elements created above
	{
	    tris->setAttr(PFGS_COORD3, PFGS_PER_VERTEX, tri_coords, NULL);
//		if(bDoNormals)
		if (gstate->getMode(PFSTATE_ENLIGHTING) == PF_ON)
		    tris->setAttr(PFGS_NORMAL3, PFGS_PER_VERTEX, tri_norms, NULL);
		tris->setAttr(PFGS_COLOR4, PFGS_PER_PRIM, tri_colors, NULL);
/*		if(tmpTexture != NULL)
			tris->setAttr(PFGS_TEXCOORD2, PFGS_PER_VERTEX, tri_tcoords, NULL);
*/
		tris->setGState(gstate);
		PolyGeode->addGSet(tris);
	}

	if(quads != NULL)	// construct GeoSet from elements created above
	{
	    quads->setAttr(PFGS_COORD3, PFGS_PER_VERTEX, quad_coords, NULL);
//		if(bDoNormals)
		if (gstate->getMode(PFSTATE_ENLIGHTING) == PF_ON)
		    quads->setAttr(PFGS_NORMAL3, PFGS_PER_VERTEX, quad_norms, NULL);
		quads->setAttr(PFGS_COLOR4, PFGS_PER_PRIM, quad_colors, NULL);
/*		if(tmpTexture != NULL)
			quads->setAttr(PFGS_TEXCOORD2, PFGS_PER_VERTEX, quad_tcoords, NULL);
*/
		quads->setGState(gstate);
		PolyGeode->addGSet(quads);
	}

	if(gen_polys != NULL)	// construct GeoSet from elements created above
	{
	    gen_polys->setAttr(PFGS_COORD3, PFGS_PER_VERTEX, gen_poly_coords, NULL);
//		if(bDoNormals)
		if (gstate->getMode(PFSTATE_ENLIGHTING) == PF_ON)
		    gen_polys->setAttr(PFGS_NORMAL3, PFGS_PER_VERTEX, gen_poly_norms, NULL);
		gen_polys->setAttr(PFGS_COLOR4, PFGS_PER_PRIM, gen_poly_colors, NULL);
/*		if(tmpTexture != NULL)
			gen_polys->setAttr(PFGS_TEXCOORD2, PFGS_PER_VERTEX, gen_poly_tcoords, NULL);
*/
		gen_polys->setGState(gstate);
		PolyGeode->addGSet(gen_polys);
	}

	if(PolyGeode->getNumGSets() == 0) {	// if the Geode is empty for some reason, complain
	    fprintf(stderr,"Couldn't copy any poly objects into Performer,\n");
		fprintf(stderr,"adding PolyObj %d to Performer pfScene anyway...\n",k);
	}

	// set the name so process_poly can retrieve object
	char tmpName[16];
	sprintf(tmpName,"%d",k);
	PolyDCS->setName(tmpName);

	PolyDCS->addChild(PolyGeode);
	perfScn->addChild(PolyDCS);

	return(TRUE);
}

int PerfRenderer::process_poly_obj(ObjNode *PolyON, int k)
{
	if(k == -1)
	    k = PolyON->get_object()->get_id();
    ZMatrix zmat;
	pfMatrix perfPolyMat;
	pfDCS *tmpPolyDCS;
	char tmpName[16];

	// search Performer Scene for object named w/ value in k
	sprintf(tmpName,"%d",k);
	tmpPolyDCS = (pfDCS *)perfScn->find(tmpName,pfDCS::getClassType());

	if(tmpPolyDCS == NULL) {
		fprintf(stderr," Whoops - DCS with id:%d was not found in Performer scene.\n",k);
		return(FALSE);
	}

	PolyON->GetTransformationMatrix(zmat);
	convert_ZMat_to_pfMat(zmat, &perfPolyMat);
	tmpPolyDCS->setMat(perfPolyMat);

	return(TRUE);
}

int PerfRenderer::add_light_obj(ObjNode *LightON, int k)
{
	if(k == -1)
	    k = LightON->get_object()->get_id(); // used to name and retreive objects

	pfLightSource *ls = new pfLightSource();
	pfDCS		*lsDCS = new pfDCS();
	char tmpName[16];

	if(nLights < PF_MAX_LIGHTS)	// PF_MAX_LIGHTS is usually around 8 or 9
	{
		// give the pfDCS a unique name for later retrieval
		sprintf(tmpName,"%d",k);
		ls->setName(strdup(tmpName));
		lsDCS->addChild(ls);	// add lightsource to pfDCS
		perfScn->addChild(lsDCS);	// add DCS to pfScene

		nLights++;
	}
	else {
		fprintf(stderr," Whoops - tried to add too many lights (%d) to Performer scene\n",k);
		return(FALSE);
	}

	return(TRUE);
}

int PerfRenderer::process_light_obj(ObjNode *LightON, int k)
{
	if(k == -1)
	    k = LightON->get_object()->get_id();

    DirectionalLightObj	*DL = (DirectionalLightObj *)LightON->get_object();

	ZMatrix zmat;
	pfMatrix	perfLightMat;
	pfVec4		PosVec;
	pfLightSource *ls = NULL;

	int PointLight = FALSE;	// FALSE means light position at infinity
	// right now, must be RGB + alpha to avoid segmentation errors
	float a[4],d[4],s[4];
	float x,y,z,w;
	char tmpName[16];

	sprintf(tmpName,"%d",k);
	ls = (pfLightSource *)perfScn->find(tmpName,pfLightSource::getClassType());
	if(ls == NULL) {
		fprintf(stderr," Whoops - light with id '%d' not found in Performer scene.\n",k);
		return(FALSE);
	}

	// account for any enveloped values
	if(DL->get_num_bands() != 3)
	    fprintf(stderr," Don't understand how to do more or less than 3 color bands\n");

	// get Grape values...
	DL->get_amb(a, DL->get_intensity());
	DL->get_diff(d, DL->get_intensity());
	DL->get_spec(s, DL->get_intensity());

	// ...and store into Performerer light object
	ls->setColor(AMBIENT,a[0],a[1],a[2]);	// KLUGE - use IrisGL tokens
	ls->setColor(DIFFUSE,d[0],d[1],d[2]);	// explicitly.
	ls->setColor(SPECULAR,s[0],s[1],s[2]);	// bug in Performer set_color?

/*	ls->setColor(PFLT_AMBIENT,a[0],a[1],a[2]);	// tokens PFLT_... not getting recognized
	ls->setColor(PFLT_DIFFUSE,d[0],d[1],d[2]);	// b/c set to OpenGL values
	ls->setColor(PFLT_SPECULAR,s[0],s[1],s[2]);	// need to have IrisGL values
	// if having problems w/ this be sure you have '-DIRISGL' in all Makefiles
*/
	// apply transforms to light source
	LightON->GetTransformationMatrix(zmat);
	convert_ZMat_to_pfMat(zmat, &perfLightMat);

	perfLightMat.getRow(PF_W, &x, &y, &z, &w);	// get translation vector
	PosVec.set(x, y, z, w);
	// if this is a point lightsource, modify its position
	// (otherwise, just modify direction)
	if(PointLight) {
	    w = PosVec.length();	// get distance of light source from origin
		PosVec[3] = w;
	} else
	    PosVec[3] = w = 0;	// 0 means infinite light source
	ls->setPos(x, y, z, w);	// 4th coord is for distance, 1st 3 for direction

	return(TRUE);

}	// process_light_object

int PerfRenderer::set_view(ObjNode *CamON) // pass in grape camera coords
{
	float fov = 45.0f;

    if(perfScn->getNumChildren() < 0) {
	    fprintf(stderr," Whoops - no objects in pfScene to be rendered - aborting..\n");
		return(FALSE);
	}
	if(CamON == NULL) {
		fprintf(stderr," Whoops - NULL CamON pointer given to set_view - aborting\n");
		return(FALSE);
	}

    Camera *CamObj = new Camera; // derived from a GRAPE Obj, not ObjNode

	ZMatrix	zmat;
	pfMatrix perfViewMat;

	// get camera Obj pointer from ObjNode
    CamObj = (Camera *)(CamON->get_object());

    fov = (float)CamObj->get_fov();

	// load transforms from Grape Camera into Performer viewport
	CamON->GetTransformationMatrix(zmat);
	convert_ZMat_to_pfMat(zmat, &perfViewMat);

	// correct for differences between Performer and Grape coord system
	perfViewMat.preRot(-90.0f, 0.0f, 0.0f, 1.0f, perfViewMat);
	
	perfChan->setViewMat(perfViewMat);

	// find a better way of determining far clip plane?
//	pfSphere bsphere;
//	perfScn->getBound(&bsphere);
//	perfChan->setNearFar(1.0f, 100.0f * bsphere.radius);

	perfChan->setFOV(fov, -1.0f); // -1 means automatically match vertical FOV to viewport aspect ratio

	return(TRUE);

}	// set_view

uint *PerfRenderer::convert_image_to_uint(Image *pImg, int *w, int *h, int *nbands)
{
	// Theory behind alg: 4 uchars are equivalent in size to 1 uint
	// therefore, 1 uint contains an RGBA vector, and an image is made
	// of a w*h length array of uints or a 4*w*h length array of uchars

	int i, j, origWidth, origHeight, nBands;
    uchar	*tptr=NULL;
	uint	*outimg=NULL, *pixptr=NULL;
	uint	tmp;

	pImg->get_res( &origWidth, &origHeight, &nBands);
	*w = origWidth; *h = origHeight;
	*nbands = 4;	// KLUGE - this alg must always return 4 bands
	// *nbands = nBands;

	pixptr = (uint*)pfMalloc(origWidth*origHeight*sizeof(uint));
	if(pixptr == NULL) {
	    fprintf( stderr, "Error: could not pfMalloc texture image data\n");
		return(NULL);
	}

	int counter = 0;
	// copy 4 uchar vals into one uint val
	outimg = pixptr;	// save place at the beginning of the array
	for(j=0;j<origHeight;j++) {
	  for(i=0;i<origWidth;i++) {
//		tptr = (uchar*)pixptr;
		tptr = (uchar*)&tmp;
		pImg->get_color(i,j,tptr,tptr+1,tptr+2,tptr+3);
		*pixptr = tmp;
		counter++;
		pixptr++;
	  }
/*	  fprintf(stderr,"%d,%d,%d,%u ",j,i,j*origWidth+i-1,outimg[j*origWidth+i-1]);
	  if(j==(origHeight-10))
		  fprintf(stderr,"here\n");	// dummy placeholder
*/
	}

	return(outimg);	// return pointer to beginning of array
}

#endif	// _USE_PERFORMER_
