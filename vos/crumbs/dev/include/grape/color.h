#ifndef _COLOR_H
#define _COLOR_H
// color.h 1.6 02/07/10 14:44:02
/** \file
 ** Classes for specifying object color properties
 **/
#include "dataport.h"
#include "grape/scene.h"
#include "grape/parameter.h"
#include "grape/animate.h"

class ColorSpec {

 private:

	int	nBands;		///< num bands of color
	dparam	alpha;		///< alpha value, if any : ENVELOPED
	ldparam	ColorVals;	///< actual color values for each band
	int bGetAlpha;		///< if true, then using alpha, too

 public:

	void dump( void);

	int get_num_bands(void){ return ColorVals.get_num();}
	void set_color( int ndx, double dVal) {
		ColorVals.set_value( ndx, dVal);
	}
	double get_color( int ndx) {
		return ColorVals.get_value( ndx);
	}
	void set_color( double *pColorVector);
	void set_color( float *pColorVector);
	void get_color( double *pColorVector, double dIntensity=1.0);
	void get_color( float *pColorVector, double dIntensity=1.0);
	void set_alpha( double a) { alpha = a;}
	double get_alpha(void){ return (double)alpha;}
	void use_alpha( int bFlag) { bGetAlpha = bFlag; }
	int using_alpha( void) { return bGetAlpha; }
	void get_color( double **pColorVector, double dIntensity=1.0) {
		int size = nBands;
		if(bGetAlpha)size++;
		*pColorVector = (double *)malloc(size * sizeof(double));
		get_color(*pColorVector, dIntensity);
	}
	void get_color( float **pColorVector, double dIntensity=1.0) {
		int size = nBands;
		if(bGetAlpha)size++;
		*pColorVector = (float *)malloc(size * sizeof(float));
		get_color(*pColorVector, dIntensity);
	}
	int color_vector_size(void) {
		int size = nBands;
		if(bGetAlpha)size++;
		return(size);
	}

	void cleanup(void) { ColorVals.free_list(); }
	void init(void) { 
		nBands = 0;
		alpha = 1.0;
	}

	virtual int	parse_in(Dataport *fp);
	virtual int	parse_out(Dataport *fp, int expand=FALSE);

	ColorSpec(int nColorBands)	{
		int i;

		nBands = nColorBands;
		bGetAlpha = FALSE;
		ColorVals.set_num( nColorBands);
		for (i = 0; i < nColorBands; i++)
			ColorVals.set_value( i, 0.0);
	}
			
	~ColorSpec() { ; }
};

class RgbColorSpec : public ColorSpec {
 private:
#define RGB_NDX_RED 0
#define RGB_NDX_GREEN 1
#define RGB_NDX_BLUE 2
 public:
	void set_color( double r, double g, double b) {
		ColorSpec::set_color( RGB_NDX_RED, r);
		ColorSpec::set_color( RGB_NDX_GREEN, g);
		ColorSpec::set_color( RGB_NDX_BLUE, b);
	}
	void get_color( double &r, double &g, double &b) {
		r = ColorSpec::get_color( RGB_NDX_RED);
		g = ColorSpec::get_color( RGB_NDX_GREEN);
		b = ColorSpec::get_color( RGB_NDX_BLUE);
	}
	RgbColorSpec() : ColorSpec(3) { ; }
	~RgbColorSpec();
};

class ColorList {
 private:

	int nColors;			///< num elements in array of colors
	ColorSpec **ppColorList;	///< ptr to ptr to color objects
	int nBands;			///< num bands in each color
	
	void init(void) {
		ppColorList = NULL;
		nColors = 0;
	}
	void cleanup( void) {
		int i;
		ColorSpec **ppCur;

		if (ppColorList != NULL) {
			ppCur = ppColorList;
			for (i = 0; i < nColors; i++, ppCur++) {
				if (*ppCur != NULL)
					delete *ppCur;
			}
			free( ppColorList);
		}
		init();
	}

 protected:

 public:
	
	void dump( void);

	int get_num_colors( void) { return nColors; }
	int set_num_bands( int n);
	int get_num_bands( void) { return nBands;}
	int using_alpha( void);

	ColorSpec *get_color_by_ndx( int ndx);

	int set_color_by_ndx( int ndx, ColorSpec *pNewColorSpec);

	virtual int	parse_in(Dataport *fp);
	virtual int	parse_out(Dataport *fp, int expand=FALSE);

	ColorList(int n) {
		int ii;
		ColorSpec **ppCur;

		cleanup();

		ppColorList = (ColorSpec **)malloc( n * sizeof( ColorSpec *));
		
		ppCur = ppColorList;
		for (ii = 0; ii < n; ii++, ppCur++) {
			*ppCur = NULL;
		}

		this->nColors = n;
		this->nBands = -1;		/* undefined value */
	}

	ColorList(int n, int nbands) {
		int ii;
		ColorSpec **ppCur;

#if 0
		cleanup();
#endif

		ppColorList = (ColorSpec **)malloc( n * sizeof( ColorSpec *));
		
		ppCur = ppColorList;
		for (ii = 0; ii < n; ii++, ppCur++) {
			*ppCur = new ColorSpec( nbands); /* default alpha usage */
		}

		this->nBands = nbands;
		this->nColors = n;
			
	}

	ColorList(int n, int nbands, int bAlphaFlag) {
		int ii;
		ColorSpec **ppCur;

		ppColorList = (ColorSpec **)malloc( n * sizeof( ColorSpec *));
		
		ppCur = ppColorList;
		for (ii = 0; ii < n; ii++, ppCur++) {
			*ppCur = new ColorSpec( nbands);
			(*ppCur)->use_alpha( bAlphaFlag); /* enable/disable alpha usage */
		}

		this->nBands = nbands;
		this->nColors = n;
			
	}

	~ColorList() {
		cleanup();
	}
};

/// convenience function prototype
ColorSpec *get_new_colorspec(Dataport *);

#endif
