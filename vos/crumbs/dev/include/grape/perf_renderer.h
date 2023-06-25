// perf_renderer.h - Performer Renderer Encapsulation for the Grape Renderer
//
// The PerfRenderer class is an encapsulation of the Performer renderer
// in a GRAPE wrapper. You give it a GRAPE scene, and it currently knows
// how to recognize a
// PerfObj
// LightObject (specifically DirectionalLightObj)
// Camera
// PolyObject1
// and PolyTerrainObj
// (consider the last 2 in developmental stages - texture mapping is
// the main problem)
//
// see also PerfRenderer man page

#ifndef	_PERFRENDERER_H_
#define _PERFRENDERER_H_

#ifdef _USE_PERFORMER_	// must include a -D_USE_PERFORMER_ in your Makefile

#include "grape/perf_object.h"
#include "grape/renderer.h"
#include "grape/poly_object_1.h"

#include <Performer/pf/pfChannel.h>
#include <Performer/pf/pfLightSource.h>
#include <Performer/pr/pfLight.h>
#include <Performer/pf/pfGeode.h>
#include <Performer/pr/pfTexture.h>
#include <Performer/pr/pfMaterial.h>
#include <Performer/pr/pfHighlight.h>
#include <Performer/pr/pfGeoSet.h>

#include <Performer/pfutil.h>
#include <Performer/pfdu.h>

// This is the derived class for the Performer rederer:

class PerfRenderer : public Renderer {

  protected:

    pfCoord			perfView;
	pfScene			*perfScn;
	pfPipe			*perfPipe;
	pfPipeWindow	*perfPW;
	pfChannel		*perfChan;

	int	nLights;		// number of lights currently being used
	int	last_rendered;	// not implemented: keep track of change count

	// method to get image data Performer recognizes out of an Image object
	// returns a pfMalloc'd uint array for the pfTexture's setImage method
	uint *convert_image_to_uint(Image *pImg, int *w, int *h, int *nbands);

	void init() {
		perfScn = NULL;
		perfPipe =NULL;
		perfPW = NULL;
		perfChan = NULL;
		nLights = 0;
		last_rendered = 0;
	}
	void cleanup() {
		pfDelete(perfScn);
		pfDelete(perfPipe);
		pfDelete(perfPW);
		pfDelete(perfChan);
		nLights = 0;
		last_rendered = 0;
	}

  public:

	// Pass the renderer scene data
	int	set_scene(Scene *scn, ImgInfo *nfo=NULL);

	// add_* methods should be called in set_scene
	// process_* methods should be called in render

	// add a Performer object to the Performer pfScene (k is the object index in the Grape scene)
	int add_PerfObj(int k) {
	    return(add_PerfObj(scene->get_obj(k))); }
	int add_PerfObj(ObjNode *PerfON);

	// update matrices of an object in the Performer pfScene
	// (k is the object index in the Grape scene)
	int process_PerfObj(int k) {
	    return(process_PerfObj(scene->get_obj(k))); }
	int process_PerfObj(ObjNode *PerfON);

	// construct and add polygon object to the Performer pfScene
	int add_poly_obj(int k) {
	    return(add_poly_obj(scene->get_obj(k), k)); }
	int add_poly_obj(ObjNode *PolyON, int k=-1);

	// update Performer matrices (k = Grape scene index)
	int process_poly_obj(int k) {
	    return(process_poly_obj(scene->get_obj(k), k)); }
	int process_poly_obj(ObjNode *PolyON, int k=-1);

	// add light to Performer pfScene,
	// k is the object index in the Grape scene
	int add_light_obj(int k) { return(add_light_obj(scene->get_obj(k), k)); }
	int add_light_obj(ObjNode *LightON, int k=-1);

	// update lighting info in Performer pfScene, k is the object index in the Grape scene
	int process_light_obj(int k) { return(process_light_obj(scene->get_obj(k), k)); }
	int process_light_obj(ObjNode *LightON, int k=-1);

	// Pass the renderer camera data
	int set_view(ObjNode *CamON);

	// method to turn on/off Performers backface culling routine for all objs
	// when FALSE, backward facing polys will be drawn - the Grape default
	void set_backface_cull(int on=FALSE) {
		if(on)pfCullFace(PFCF_BACK);	// remove back facing polys
		else  pfCullFace(PFCF_OFF); }	// make polys visible from all sides

	// method to turn on normals on back side of polys
	// (this allows for proper lighting on the "inside" of poly objects
	// when TRUE, normals on both sides of a poly will be used for lighting
	// note: set_backface_cull(FALSE) must be called to see this, though
	void set_two_sided_normals(int on=TRUE) {
		pfLightModel *lm = pfGetCurLModel();
		if(on)lm->setTwoSide(PF_ON);
		else  lm->setTwoSide(PF_OFF); }

	// Render a frame, return success status
	int	render(ObjNode *CamON, ImgInfo *img_nfo);

	int	render(void) { return(render(NULL, NULL)); }

/*	int	render(void) {
		ObjNode *CamON = scene->get_camera();
		return(render(CamON,((Camera *)(cam->get_object()))->get_img_info()));
	}
*/
	// Start asynchronous rendering, return success status
/*	int	render_start(void) { return(render()); }
*/
	int	render_start(ObjNode *CamON=NULL, ImgInfo *img_nfo=NULL) {
		return(render(CamON, img_nfo)); }

	// Returns true if done with asynchronous rendering
	int	render_done() { return(TRUE); }

	// Returns a unique id number for the renderer - *not yet implemented*
	int	get_id() { return 0; }

	// Sets a function to be called when asynchronous rendering is done
	void	set_done_callback(RenderCB cb=NULL, void *dat=NULL) {
				done_callback=cb; done_data=dat; }

	// Mechanism for sending or requesting renderer information
	// Messages consist of a message type id, and optional data buffer
	// Returns true if message type is supported
	int	send_message(int, void * =NULL) { return 0; }

	// Function for doing inverse render queries, returns success status
	int	inverse_render(InvRender *) { return 0; }

	// Returns mask of supported inverse render queries
	int	inv_render_supported() { return 0; }

	// returns count of how many objects can be handled in a scene
	// 0 means unlimited
	int object_limit(void) { return(0); }

	PerfRenderer()	// Constructor
	{
	    init();

	    // initialize fundamental datastructures in Performer
	    pfInit();
//		pfMultiprocess(PFMP_APPCULLDRAW);
		pfMultiprocess( PFMP_DEFAULT );
		pfConfig();
		pfFilePath(".:/usr/share/Performer/data");

	}

	~PerfRenderer()	// Destructor
	{
	    cleanup();

	    pfExit();
	}
};

// this was just for debugging
void	print_indeces(ushort *list, int xfact, int yfact) {
    int i,j;
	char ndx[1024], vertndx[1024]; // ndx_lst[1024], vertndx_lst[1024];
	strcpy(ndx,"");
	strcpy(vertndx,"");
	int length = 2*(xfact+1);
	fprintf(stderr,"w: %d, h: %d, length: %d\n", xfact, yfact, length);
	for(j=0;j<yfact;j++) {
	    for(i=0;i<length;i++) {
		    sprintf(ndx, "%s%d	", ndx, j*length+i);
		//	strcat(ndx_lst, ndx);
		    sprintf(vertndx, "%s %u	", vertndx, list[j*length+i]);
		//	strcat(vertndx_lst, vertndx);
		}
		fprintf(stderr, "%s\n", ndx);
		fprintf(stderr, "%s\n\n", vertndx);
		strcpy(ndx,"");
		strcpy(vertndx,"");
	}
}

#endif	// _USE_PERFORMER_

#endif	// _PERFRENDERER_H_
