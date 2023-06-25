
#ifndef _TEXTURE_H_
#define _TEXTURE_H_

								/* this is the beginning of a Texture class */
								/* and related classes */
#include "math.h"

#include "image/image.h"
#include "image/geoimage.h"

#include "grape/parameter.h"

#define FATAL_ERROR(txt) { \
		   fprintf( stderr, "Fatal error: %s:%s:%d\n", \
				   txt, __FILE__, __LINE__); \
				   exit( 0);  }

/**************************************************************************/

/**************************************************************************/


								/* NOTE: this class does not yet have support */
								/* for envelopes; I will add it in the future */

								/* since it is based on the dparam class, I */
								/* may need to rewrite this class altogether */
								/* without using the dparam class at all; my */
								/* hopes are that I can add enveloping simply */
								/* by extending this present version*/
//typedef dparam lld_datatype;
typedef double lld_datatype;

class lldparam : public param {
 protected:
	int *size;
	int num;
	lld_datatype ***pppDparam;		/* array of arrays of ptrs to lld_datatypes */
	int index1,index2;

	void init(void) { 
		size = NULL;
		pppDparam = NULL;
		num = 0;
	}
	void destroy(void) {
		int i;
		if (pppDparam != NULL) {
			for (i = 0; i < num; i++) {
				free_list( i);
			}
		}
		if (size != NULL) free( size);
		num = 0;
	}
 public:

	int parse_in( Dataport *fp);
	int parse_out( Dataport *fp, int expand=FALSE);

	void set_value( int i1, int i2, double v) {
		if (i1 >= num) {
			FATAL_ERROR( "i1 exceeds num");
		}
		if (i2 >= size[i1]) {
			FATAL_ERROR( "i2 exceeds size[i1]");
		}
		if (pppDparam[i1][i2] == NULL) {
			FATAL_ERROR( "pppDparam[i1][i2] is NULL");
		}
		*pppDparam[i1][i2] = v;
	}

	double get_value( int i1, int i2) {
		if (i1 >= num) {
			FATAL_ERROR( "i1 exceeds num");
		}
		if (i2 >= size[i1]) {
			FATAL_ERROR( "i2 exceeds size[i1]");
		}
		if (pppDparam[i1][i2] == NULL) {
			FATAL_ERROR( "pppDparam[i1][i2] is NULL");
		}
		return *pppDparam[i1][i2];
	}
	void set_num( int n) {
		int i;
		if (n < num) {
			for (i = n; i < num; i++)
				free_list( i);
		}
		if ((size=(int *)realloc(size,n*sizeof(int))) == NULL) {
			FATAL_ERROR( "realloc of size");
		}
		if ((pppDparam = (lld_datatype ***)realloc( pppDparam, n * sizeof( lld_datatype **)))
			== NULL) {
			FATAL_ERROR( "realloc of pppDparam");
		}
		if (n > num) {
			for (i = num; i < n; i++) {
				pppDparam[i] = NULL;
				size[i] = 0;
			}
		}
		num = n;
		changed=CHANGE_COUNTER;
	}
	
	int get_num( void) {
		return num;
	}
	
	void set_size( int i, int s) {
		int j;
		if (i >= num) {
			FATAL_ERROR( "i exceeds num");
		}
		if (s < size[i]) {
			for (j = s; j < size[i]; j++) {
				if (pppDparam[i][j] == NULL) {
					FATAL_ERROR( "pppDparam[i][j] == NULL");
				}
				delete pppDparam[i][j];
			}
		}
		if ((pppDparam[i] = (lld_datatype **)realloc( pppDparam[i],
											   s * sizeof(lld_datatype *)))
		 == NULL) {
			FATAL_ERROR( "realloc of pppDparam[i]");
		}
		if (s > size[i]) {
			for (j = size[i]; j < s; j++) {
				if ((pppDparam[i][j] = new lld_datatype) == NULL) {
					FATAL_ERROR( "new lld_datatype fails");
				}
			}
		}
		
		size[i] = s;
		changed=CHANGE_COUNTER;
	}

	int get_size(int i) {
        if (i >= num) {
			FATAL_ERROR( "i exceeds num");
		}
		return size[i];
	}

	void add_list( int s=0) {
		set_num(num+1);
		if(s)
			set_size(num-1,s);
	}

	void add_value( int i, double v) {
        if (i >= num) {
			FATAL_ERROR( "i exceeds num");
		}
		set_size(i,size[i]+1);
		if ((pppDparam[i][size[i]-1] = new lld_datatype) == NULL) {
			FATAL_ERROR( "new lld_datatype is NULL");
		}
		*pppDparam[i][size[i]-1] = v;
	}
	void free_list( int i) {
		int j;
		if (i >= num) {
			FATAL_ERROR( "bad index i");
		}
		if (pppDparam[i] == NULL) {
			FATAL_ERROR( "pppDparam[i] is null");
		}
		for (j = 0; j < size[i]; j++) {
			if (pppDparam[i][j] == NULL) {
				FATAL_ERROR( "pppDparam[i][j] is null");
			}
			delete pppDparam[i][j];
		}					
		free( pppDparam[i]);
		pppDparam[i] = NULL;
		size[i] = 0;
	}

	void    free_all(void) {
		destroy();
	}

	

	lldparam &operator()(int i, int j) { index1=i; index2=j; return *this; }

	double operator=(int v) { set_value( index1,index2,v); return v; }

	operator double() { return get_value(index1,index2); }

	lldparam() { init();}

	~lldparam() { destroy(); }
};

/**************************************************************************/

								/* the DomRangeVec class sets up vectors that */
								/* map from the domain space to the range */
								/* space; the i-th vector in the domain is */
								/* (d[i],d1[i]); the i-th vector in the range */
								/* is (r[i],r1[i]) */
								/* */
								/* */
class DomRangeVec {
 protected:
	int dimD;					/* dimensions of domain */
	int dimR;					/* dimensions of range */
	lldparam lld0, lld1, llr0, llr1;
	void init(void);
	void destroy(void);
 public:


								/* set i-th 2-point mapping from domain to */
								/* range */
	void set_vector( int ndx,
					double *pVecDom, /* point 0 in domain, w/ dimD elements */
					double *pVecRange, /* point 0 in range, w/ dimR elements */
					double *pVecDom1, /* point 1 in domain, w/ dimD elements */
					double *pVecRange1); /* point 1 in range, w/ dimR elements */

								/* get i-th 2-point mapping from domain to */
								/* range */
	void get_vector( int ndx, 
					double *pVecDom,
					double *pVecRange,
					double *pVecDom1,
					double *pVecRange1);

	int parse_in( Dataport *fp);
	int parse_out( Dataport *fp, int expand=FALSE);

	void dump(void);
	DomRangeVec( int nDimDom, int nDimRange);
	~DomRangeVec() {
		destroy();
	}
};

/**************************************************************************/

enum MappingType {
	ExplicitMap,				/* explicit texture map types */

    ImpMapLinear,				/* implicit texture map types */
    ImpMapPlanar,
    ImpMapCylindrical,
    ImpMapSpherical,
    ImpMapCubic,
    ImpMapFrontProjection,
    GeoMap
};

class Mapping {
 private:
	void init(void) {
		nDimDomain = nDimRange = 0;
	}
	void cleanup(void) {
	}
 protected:

	int nDimDomain;				/* num dim of domain (i.e. object) "D" */
	int nDimRange;				/* num dim of texture "R" */

								/* these vectors map to (0,1) in each */
								/* range dimension; there are always D of */
								/* these; */
								/* D < R : some vectors will be (0,0) */
								/* D > R : some values will be 0 */
								/* D = R : 1-to-1 */
								/* */
								/* the i-th element of this contains 2 */
								/* vectors, one in the domain space and one */
								/* ine the range space; in other words, the */
								/* i-th element contains 4 points, 2 in the */
								/* domain and 2 in the range */
								/* */

	char *reference;

 public:

	void virtual dump( void) = 0; /* dump all info in the mapping */

	MappingType virtual get_type(void) = 0;

	virtual void set_dims( int d, int r) { nDimDomain = d; nDimRange = r;}
	virtual void get_dims( int &d, int &r) {
		d = nDimDomain;
		r = nDimRange;
	}

	virtual int eval( double *pDomVector, double *pRetRangeVector) = 0;
	virtual int eval( float *pDomVector, float *pRetRangeVector) = 0;

	virtual int parse_in(Dataport *) = 0;
	virtual int parse_out(Dataport *, int=FALSE) = 0;

	void set_reference( char *nm) {
		reference = strdup(nm);
	}
	char *get_reference(void) { return reference;}

	Mapping( int dimDom, int dimRange);
	Mapping() { init();}
	~Mapping() {
		cleanup();
	}
};

class ExplicitMapping: public Mapping {
 private:

	void init( void) {
		plldDomain = plldRange = NULL;
	}
	lldparam *plldDomain;
	lldparam *plldRange;
 protected:
 public:

	MappingType virtual get_type(void) { return ExplicitMap;}

	virtual int parse_in(Dataport *) = 0;
	virtual int parse_out(Dataport *, int=FALSE) = 0;

	ExplicitMapping(int DimDom, int DimRange) : 
		Mapping(DimDom, DimRange) { 
		init();
	}
	~ExplicitMapping() {;}
};

class ImplicitMapping: public Mapping {

								/* this is an abstract class */
 private:

	void init( void) {
		;
	}

 protected:
 public:

	virtual void set_dims( int d, int r) { nDimDomain = d; nDimRange = r;}

	MappingType virtual get_type(void)=0;

	virtual int parse_in(Dataport *) = 0;
	virtual int parse_out(Dataport *, int=FALSE) = 0;

	ImplicitMapping( int dimDom, int domRange) :
		Mapping( dimDom, domRange) {
			init();
		}
	ImplicitMapping() {;}
	~ImplicitMapping() {;}
};

// this is the class for georeferenced mappings
class GeoMapping : public Mapping {
private:
	GeoData *disp_geoData;
	GeoData *color_geoData;
	int color_xres, color_yres;	// used to scale into normal coordinates
	int disp_xres, disp_yres;	// these are part of the kludge in eval
							    // and should be removed later 
public:
	virtual MappingType get_type(void) { return GeoMap;}

	virtual int eval( float *pDomVector, float *pRetRangeVector);
	virtual int eval( double *pDomVector, double *pRetRangeVector);
		
// do nothing for now, just to satify pure virtuals declared aabove
	virtual int parse_in(Dataport *) { return 0; }
	virtual int parse_out(Dataport *, int=FALSE) { return 0; }
	void virtual dump( void) {}

	virtual void set_disp_geo_data(GeoData *gd) { disp_geoData = gd; }
	virtual void set_color_geo_data(GeoData *gd) { color_geoData = gd; }
	virtual void set_color_xres(int xres) { color_xres = xres; }
	virtual void set_color_yres(int yres) { color_yres = yres; }
	virtual void set_disp_xres(int xres) { disp_xres = xres; }
	virtual void set_disp_yres(int yres) { disp_yres = yres; }

	GeoMapping() : disp_geoData(0), color_geoData(0), color_xres(-1),
			 color_yres(-1), disp_xres(-1), disp_yres(-1) {}
    	GeoMapping(GeoImage *color_gi, GeoImage *disp_gi);
    	GeoMapping(GeoData *color_gd, GeoData *disp_gd, 
			int cxres, int cyres, int dxres, int dyres);

	~GeoMapping() { disp_geoData = color_geoData = 0; }
};

class ImpMapPlanarProj : public ImplicitMapping {
 private:

 protected:
	DomRangeVec *pdv;

	void init(void) {
		pdv = NULL;
	}
	void destroy( void) {
		if (pdv != NULL)
			delete pdv;
	}

 public:

	MappingType virtual get_type(void) { return ImpMapPlanar; }
	
	void virtual dump( void) {	/* dump all info in the mapping */
		pdv->dump();
	}

	int eval( double *pDomVector, double *pRetRangeVector);
	int eval( float *pDomVector, float *pRetRangeVector);
	void set_dom_to_range_vector( int ndxVec,
								 double *pD,
								 double *pR,
								 double *pD1,
								 double *pR1
								 );
	
	void get_dom_to_range_vector( int ndxVec,
								 double *pD,
								 double *pR,
								 double *pD1,
								 double *pR1
								 );

	void clear_all_map_vectors( void);

	virtual void set_dims( int d, int r) { 
		nDimDomain = d; nDimRange = r;
		if (pdv != NULL) delete pdv;
		pdv = new DomRangeVec( d, r);
	}
		
	virtual int parse_in(Dataport *);
	virtual int parse_out(Dataport *, int=FALSE);

	ImpMapPlanarProj( int d, int r);
	ImpMapPlanarProj() {
		init();
	}
	~ImpMapPlanarProj() {
		destroy();
	}

};

class ImpMapSphereProj : public ImplicitMapping {
 private:

 protected:
	ldparam center;			/* center of sphere in domain space */
	double theta;				/* right-hand angle to meridian: [0,360) */
	double phi;					/* angle from north pole that defines axis */
								/* of rotation of sphere */
 public:

	MappingType virtual get_type(void) { return ImpMapSpherical; }
	
	void virtual dump( void) {;}	/* dump all mapping info */

	void set_center( double *pVec) {
		int i;
		center.set_num( nDimDomain);
		for (i=0; i<nDimDomain; i++) {
			center.set_value( i, pVec[i]);
		}
	}
	void get_center( double *pVec) {
		int i;
		for (i=0; i<nDimDomain; i++) {
			pVec[i] = center.get_value( i);
		}
	}

	virtual void set_dims( int d, int r) { 
		if (d != 3 || r != 2) {
			FATAL_ERROR( "Spherical mapping supported only for 3-to-2 dimensions!");
		}
		else {
			nDimDomain = d; nDimRange = r;
			center.set_num( nDimDomain);
		}
	}

	int eval( double *pDomVector, double *pRetRangeVector) {
		double v0[3], dxy, td;
								/* assumes 3 dimensions, for now */
		if (nDimDomain != 3 || nDimRange != 2) {
			FATAL_ERROR( "only 3 dimensions supported, so far");
		}
		
								/* vector from center to passed point */
		v0[0] = pDomVector[0] - center.get_value( 0);
		v0[1] = pDomVector[1] - center.get_value( 1);
		v0[2] = pDomVector[2] - center.get_value( 2);
		
		dxy = sqrt( v0[0]*v0[0] + v0[1]*v0[1]);

								/* 0 rads maps to 0.0 */
		td = atan2( v0[1], v0[0]) / (2.0 * M_PI); 
		if (td < 0.0) td += 1.0;
		pRetRangeVector[0] = td;

								/* 0 rads maps to 0.5 */
								/* pi rads maps to 1.0 */
								/* -pi rads maps to 0.0 */
		td = atan2( v0[2], dxy) / M_PI + 0.5;
		if (td < 0.0) td += 1.0;
		pRetRangeVector[1] = td;

		return 1;
	}

	int eval( float *pDomVector, float *pRetRangeVector) {
		double v0[3], dxy, td;
								/* assumes 3 dimensions, for now */
		if (nDimDomain != 3 || nDimRange != 2) {
			FATAL_ERROR( "only 3 dimensions supported, so far");
		}
		
								/* vector from center to passed point */
		v0[0] = pDomVector[0] - center.get_value( 0);
		v0[1] = pDomVector[1] - center.get_value( 1);
		v0[2] = pDomVector[2] - center.get_value( 2);
		
		dxy = sqrt( v0[0]*v0[0] + v0[1]*v0[1]);

								/* 0 rads maps to 0.0 */
		td = atan2( v0[1], v0[0]) / (2.0 * M_PI); 
		if (td < 0.0) td += 1.0;
		pRetRangeVector[0] = (float)td;

								/* 0 rads maps to 0.5 */
								/* pi rads maps to 1.0 */
								/* -pi rads maps to 0.0 */
		td = atan2( v0[2], dxy) / M_PI + 0.5;
		if (td < 0.0) td += 1.0;
		pRetRangeVector[1] = (float)td;

		return 1;
	}

    virtual int parse_in(Dataport *);
	virtual int parse_out(Dataport *, int=FALSE);
	
	ImpMapSphereProj() {
		set_dims( 3, 2);
		center.set_value( 0, 0.0);
		center.set_value( 1, 0.0);
		center.set_value( 2, 0.0);
	}
	~ImpMapSphereProj() { ;}

};

Mapping *mapping_creator( Dataport *fp);

/**************************************************************************/

class TextureMap {
 private:
 protected:

	int nDimDomain, nDimRange;	/* domain is obj space, range is texture space */
	imgparam texture;
	Mapping *pMap;				/* ptr to abstract class */
	char *reference;			/* file from which this obj was parsed in */

	int changed;

	void init(void) {
		nDimDomain = nDimRange = 0;
		pMap = NULL;
		reference = NULL;
		changed = CHANGE_COUNTER;
	}
	void destroy(void) {
		if (pMap != NULL) delete pMap;
		if (reference != NULL) free( reference);
	}
	void set_changed(void) { changed = CHANGE_COUNTER;}

 public:

	int get_changed(void) { return changed;}

	virtual void dump(void) {
		if (pMap != NULL) {
			pMap->dump();
		}
	}

	void set_texture( Image *pImg) { texture.set_value( pImg); set_changed(); }
	Image *get_texture( void) { return (Image *)texture;}

	void set_mapping( Mapping *pM) { pMap = pM; set_changed();}
	Mapping *get_mapping( void) { return pMap;}

    void    set_reference(char *nm) {
		if(reference)free(reference);
		reference= strdup(nm);
	}
	char    *get_reference(void) { return(reference); }

	int get_texture_coord( double *pObjCoordVec,
						  double *pTexCoordVec) {
		
		if (pMap == NULL) {
			FATAL_ERROR( "no mapping exists");
			return 0;			/* failure */
		}
		else {
			return pMap->eval( pObjCoordVec, pTexCoordVec);
		}
	}
	
	int get_texture_coord( float *pObjCoordVec,
						  float *pTexCoordVec) {
		
		if (pMap == NULL) {
			FATAL_ERROR( "no mapping exists");
			return 0;			/* failure */
		}
		else {
			return pMap->eval( pObjCoordVec, pTexCoordVec);
		}
	}
	
    int parse_in(Dataport *);
	int parse_out(Dataport *, int=FALSE);
	
	void set_dims( int d, int r) {
		nDimDomain = d;
		nDimRange = r;
	}
	void get_dims( int &d, int &r) {
		if (pMap != NULL)
			pMap->get_dims( d, r);
		else {
			FATAL_ERROR( "no mapping exists");
		}
	}

	TextureMap( int d, int r) { 
		init();
		nDimDomain = d;
		nDimRange = r;
	}
	TextureMap() {
		init();
	}
	~TextureMap() {
		destroy();
	}
};

#endif
