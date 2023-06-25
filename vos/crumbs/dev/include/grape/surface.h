#include "grape/texture.h"

								/* this class holds all surface-mapping info */
								/* for texture mapping any of the contained */
								/* properties (color, displacement, */
								/* reflectivity, etc) */

class GrapeSurface {
 protected:
	TextureMap *pColor;			/* texture map for color */
	TextureMap *pDisp;			/* texture map for displacement (elevation) */
	TextureMap *pRefl;			/* texture map for reflectivity */

	char    *reference;			/* file from which this obj came */

	int changed;

	void init(void) {
		pColor = NULL;
		pDisp = NULL;
		pRefl = NULL;
		reference = NULL;
		changed=CHANGE_COUNTER;
	}
	void destroy( void) {
		if (pColor != NULL) delete pColor;
		if (pDisp != NULL) delete pDisp;
		if (pRefl != NULL) delete pRefl;
		if (reference != NULL) free ( reference);
	}

	void set_changed(void) { changed = CHANGE_COUNTER;}

 public:
	int get_changed();

	void	set_reference(char *nm) { if(reference)free(reference); reference= strdup(nm); }
	char	*get_reference(void) { return(reference); }

	void set_tmap_color( TextureMap *pTM) { 
		if (pColor != NULL) delete pColor;
		pColor = pTM;
		set_changed();
	}
	void set_tmap_disp( TextureMap *pTM) { 
		if (pDisp != NULL) delete pDisp;
		pDisp = pTM;
		set_changed();
	}
	void set_tmap_refl( TextureMap *pTM) { 
		if (pRefl != NULL) delete pRefl;
		pRefl = pTM;
		set_changed();
	}
	
	TextureMap *get_tmap_color( void) { return pColor;}
	TextureMap *get_tmap_disp( void) { return pDisp;}
	TextureMap *get_tmap_refl( void) { return pRefl;}

	int	parse_in(Dataport *fp);
	int	parse_out(Dataport *fp, int expand=FALSE);

	GrapeSurface() { init();}
	~GrapeSurface() { destroy(); }
};

