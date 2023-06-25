/*

   This module provides the implementation for light objects.



*/

#ifndef _LIGHT_OBJECT_H_
#define _LIGHT_OBJECT_H_

#include "grape/object.h"
#include "grape/color.h"


								/* these must be added to object_types.h */

#define LIGHT_OBJ_END         "LIGHT_END"
#define BANDS_TOKEN				"BANDS"	/* number of color bands */
#define COLORS_TOKEN			"COLORS" /* number of colors */
#define COLOR_SELECTOR			"COLOR"	/* color index selector */
#define ALPHA_TOKEN				"ALPHA"	/* alpha channel specification */

#define AMBIENT_TOKEN			"AMBIENT" /* ambient color index follows */
#define DIFFUSE_TOKEN			"DIFFUSE" /* diffuse color index follows */
#define SPECULAR_TOKEN			"SPECULAR" /* specular color index follows */
#define INTENSITY_TOKEN			"INTENSITY"	/* intensity of light */

#define MIN_INTENSITY			0.0
#define MAX_INTENSITY			1.0

enum LightObjType {

	BadLight = -1,
	DefaultLight,
	DirectionalLight=DefaultLight,
	PointLight,
	SpotLight,
	AreaLight
};

class LightObject : public Obj
{

 private:

	char	*version;
	
	int nBands;
	int bGetAlpha;
	dparam intensity;			/* ENVELOPED */

 protected:

	LightObjType light_type;	/* directional, spot, etc */
#if 0
	ColorSpec *p_amb_color;			/* ambient color */
	ColorSpec *p_diff_color;		/* diffuse color */
	ColorSpec *p_spec_color;		/* specular color */
#endif
	int amb_ndx;
	int diff_ndx;
	int spec_ndx;
	dparam	att_k0, att_k1, att_k2;	/* attenuation at dist d: */
								/*  d := 1/(k0 + k1*d + k2*d^2)  */

	int bDoIntensityFallOff;

	void set_light_type( LightObjType t) { light_type = t;} /* set light type */

	ColorList *pColors;			/* list of ColorSpec objects */

 public:

	iparam light_id;			/* simple identifier: */
								/*  may have special meaning for certain */
								/*  implementations*/

	virtual int	get_type(void) = 0;

	int get_num_bands( void) { return nBands;}
	void set_num_bands( int n) { nBands = n; }
	int using_alpha( void);

#if 0
	void set_amb( double *pColorVector){ /* set ambient light */
		p_amb_color->set_color( pColorVector);
	}
	void set_diff( double *pColorVector){ /* set ambient light */
		p_diff_color->set_color( pColorVector);
	}
	void set_spec( double *pColorVector){ /* set ambient light */
		p_spec_color->set_color( pColorVector);
	}
#endif

								/* light color set_*() functions */

	void set_amb( int ndx) { 
		if (ndx < pColors->get_num_colors())
			amb_ndx = ndx;
	}
	void set_diff( int ndx) { 
		if (ndx < pColors->get_num_colors())
			diff_ndx = ndx;
	}
	void set_spec( int ndx) { 
		if (ndx < pColors->get_num_colors())
			spec_ndx = ndx;
	}
	void set_color( int ndx) {
		set_diff( ndx);
	}

								/* 'double' vector versions get_*() */

	void get_amb( double *pVect) {
		ColorSpec *p;

		if ((p = pColors->get_color_by_ndx( amb_ndx)) != NULL)
			p->get_color( pVect);
	}
	void get_diff( double *pVect) {
		ColorSpec *p;

		if ((p = pColors->get_color_by_ndx( diff_ndx)) != NULL)
			p->get_color( pVect);
	}
	void get_spec( double *pVect) {
		ColorSpec *p;

		if ((p = pColors->get_color_by_ndx( spec_ndx)) != NULL)
			p->get_color( pVect);
	}

	void get_amb( double *pVect, double dIntensity) {
		ColorSpec *p;

		if ((p = pColors->get_color_by_ndx( amb_ndx)) != NULL)
			p->get_color( pVect, dIntensity);
	}
	void get_diff( double *pVect, double dIntensity) {
		ColorSpec *p;

		if ((p = pColors->get_color_by_ndx( diff_ndx)) != NULL)
			p->get_color( pVect, dIntensity);
	}
	void get_spec( double *pVect, double dIntensity) {
		ColorSpec *p;

		if ((p = pColors->get_color_by_ndx( spec_ndx)) != NULL)
			p->get_color( pVect, dIntensity);
	}

								/* 'float' versions of same functions */

	void get_amb( float *pVect) {
		ColorSpec *p;

		if ((p = pColors->get_color_by_ndx( amb_ndx)) != NULL)
			p->get_color( pVect);
	}
	void get_diff( float *pVect) {
		ColorSpec *p;

		if ((p = pColors->get_color_by_ndx( diff_ndx)) != NULL)
			p->get_color( pVect);
	}
	void get_spec( float *pVect) {
		ColorSpec *p;

		if ((p = pColors->get_color_by_ndx( spec_ndx)) != NULL)
			p->get_color( pVect);
	}
	void get_amb( float *pVect, double dIntensity) {
		ColorSpec *p;

		if ((p = pColors->get_color_by_ndx( amb_ndx)) != NULL)
			p->get_color( pVect, dIntensity);
	}
	void get_diff( float *pVect, double dIntensity) {
		ColorSpec *p;

		if ((p = pColors->get_color_by_ndx( diff_ndx)) != NULL)
			p->get_color( pVect, dIntensity);
	}
	void get_spec( float *pVect, double dIntensity) {
		ColorSpec *p;

		if ((p = pColors->get_color_by_ndx( spec_ndx)) != NULL)
			p->get_color( pVect, dIntensity);
	}

#if 0
								/* for the single-color light */
	void set_color( double *pColorVector) {
		double *pWhite;

								/* only diffuse component is set */
		p_diff_color->set_color( pColorVector);

								/* set ambient and specular to white */
		pWhite = (double *)new char( p_amb_color->get_num_bands() *
									sizeof(double *));

		p_amb_color->set_color( pWhite);
		p_spec_color->set_color( pWhite);

		delete pWhite;
	}
#endif

#if 0
								/* set colors _by index_ */

	void set_amb( int ndx, double color) {
		p_amb_color->set_color( ndx, color);
	}
	void set_diff( int ndx, double color) {
		p_diff_color->set_color( ndx, color);
	}
	void set_spec( int ndx, double color) {
		p_spec_color->set_color( ndx, color);
	}

								/* the single-component color model */
	void set_color( int ndx, double color) {
		p_diff_color->set_color( ndx, color);

								/* ambient and specular set to white */
		p_amb_color->set_color( ndx, 1.0);
		p_spec_color->set_color( ndx, 1.0);
	}
#endif

	LightObjType get_light_type(void) { return light_type; } /* get light type */
	
								/* set attenuation at distance d */
								/*  d := 1/(k0 + k1*d + k2*d^2)  */
	void set_attenuation( double k0, double k1, double k2) {
		att_k0.set_value( k0);
		att_k1.set_value( k1);
		att_k2.set_value( k2);
	}
	
	void dump(void) {
		int i;
		ColorSpec *pAmb, *pDiff, *pSpec, bad(nBands);

		for (i = 0; i < nBands; i++)
			bad.set_color( i, -1.0);

		if ((pAmb = pColors->get_color_by_ndx( amb_ndx)) == NULL)
			pAmb = &bad;
		if ((pDiff = pColors->get_color_by_ndx( diff_ndx)) == NULL)
			pDiff = &bad;
		if ((pSpec = pColors->get_color_by_ndx( spec_ndx)) == NULL)
			pSpec = &bad;

		fprintf( stderr, "ambient   diffuse   specular, nBands=%d\n",
				nBands );
		for (i = 0; i < nBands; i++) {
			fprintf( stderr,
					"%5f     %5f     %5f\n",
					pAmb->get_color(i),
					pDiff->get_color(i),
					pSpec->get_color(i) );
		}
	}

	void set_intensity_fall_off( int bZeroOffElseOn) {
		if (!bZeroOffElseOn)
			bDoIntensityFallOff = 0;
		else
			bDoIntensityFallOff = 1;
	}
	int get_intensity_fall_off( void) {
		return (bDoIntensityFallOff) ? 1 : 0;
	}

	void set_intensity( double val) {
		intensity.set_value( val);
	}
	double get_intensity( void) {
		return intensity.get_value();
	}

	void set_colorlist( ColorList *p) {
		pColors = p;
	}
	ColorList *get_colorlist( void) {
		return pColors;
	}
    virtual int parse_in(Dataport *fp);
	virtual int parse_out(Dataport *fp, int expand=FALSE);
	

	LightObject(int nColorBands) {
#if 0
		double *pWhite;

		p_amb_color = new ColorSpec(nColorBands);
		p_diff_color = new ColorSpec(nColorBands);
		p_spec_color = new ColorSpec(nColorBands);

								/* set default color to white */
		pWhite = (double *)new char( nColorBands * sizeof( double *));
		p_diff_color->set_color( pWhite);
		p_amb_color->set_color( pWhite);
		p_spec_color->set_color( pWhite);

		delete pWhite;
#endif
		set_attenuation( 1.0, 0.0, 0.0);
		set_intensity_fall_off( 0);	/* off */

		version = NULL;
		pColors = NULL;
		this->nBands = nColorBands;
		amb_ndx = diff_ndx = spec_ndx = -1;
		bGetAlpha = FALSE;
		set_intensity( 1.0);
		light_id.set_value( 0);
	}

	~LightObject(void) {
#if 0
		delete p_amb_color;
		delete p_diff_color;
		delete p_spec_color;
#endif
		if (version != NULL)
			free (version);
		if (pColors != NULL)
			delete pColors;
	}

};								/* end LightObject def */

class RgbLightObject : public LightObject
{

 private:

 protected:

 public:

	virtual int	get_type(void) = 0;

	void set_amb( double r, double g, double b){ /* set ambient light */
        ColorSpec *pAmb;

		pAmb = pColors->get_color_by_ndx( amb_ndx);
		if (pAmb != NULL) {
			pAmb->set_color( RGB_NDX_RED, r);
			pAmb->set_color( RGB_NDX_GREEN, g);
			pAmb->set_color( RGB_NDX_BLUE, b);
		}
	}
	void set_diff( double r, double g, double b){ /* set diffuse light */
        ColorSpec *pDiff;

		pDiff = pColors->get_color_by_ndx( diff_ndx);
		if (pDiff != NULL) {
			pDiff->set_color( RGB_NDX_RED, r);
			pDiff->set_color( RGB_NDX_GREEN, g);
			pDiff->set_color( RGB_NDX_BLUE, b);
		}
	}
	void set_spec( double r, double g, double b){ /* set specular light */
        ColorSpec *pSpec;

		pSpec = pColors->get_color_by_ndx( spec_ndx);
		if (pSpec != NULL) {
			pSpec->set_color( RGB_NDX_RED, r);
			pSpec->set_color( RGB_NDX_GREEN, g);
			pSpec->set_color( RGB_NDX_BLUE, b);
		}
	}


								/* for the single-color light */
	void set_color( double r, double g, double b) {
		set_diff( r, g, b);
		set_spec( 1.0, 1.0, 1.0);
		set_amb( 1.0, 1.0, 1.0);
	}

	LightObjType get_light_type(void) { return light_type; } /* get light type */
	
	RgbLightObject(void): LightObject(3) {
	}

	~RgbLightObject(void) { ; }

};								/* end RgbLightObject def */

class DirectionalLightObj : public LightObject {

 private:

 protected:

 public:

	virtual int get_type(void) { return DIRECTIONAL_LIGHT_V1; }

	DirectionalLightObj(void): LightObject(3) {
		set_light_type( DirectionalLight);
	}
	DirectionalLightObj( int nbands) : LightObject(nbands) {
		set_light_type( DirectionalLight);
	}

	~DirectionalLightObj(void) { ; }
};								/* end DirectionalLightObj def */

class RgbDirectionalLightObj : public RgbLightObject {

 private:

 protected:

 public:

	virtual int get_type(void) { return DIRECTIONAL_LIGHT_V1; }

	RgbDirectionalLightObj(void) {
		set_light_type( DirectionalLight);
	}

	~RgbDirectionalLightObj(void) { ; }
};								/* end RgbDirectionalLightObj def */

class SpotLightObj : public LightObject {

 private:
	
	dparam		cone_angle;		/* angle in degrees from axis of cone to */
								/* edge of cone */
	dparam		soft_shadow_angle; /* penumbra??? */

 protected:

 public:

	virtual int get_type(void) { return SPOT_LIGHT_V1; }

	double get_cone_angle( void) { return cone_angle.get_value(); }
	double get_soft_shadow_angle( void) { return soft_shadow_angle.get_value();}

	SpotLightObj(void):LightObject(3) {
		set_light_type( SpotLight);
	}
	SpotLightObj(int nbands):LightObject(nbands) {
		set_light_type( SpotLight);
	}
	

	~SpotLightObj(void) { ; }
};								/* end SpotLightObj def */

class RgbSpotLightObj : public RgbLightObject {

 private:
	
	dparam		cone_angle;		/* angle in degrees from axis of cone to */
								/* edge of cone */
	dparam		soft_shadow_angle; /* penumbra??? */

 protected:

 public:

	virtual int get_type(void) { return SPOT_LIGHT_V1; }

	double get_cone_angle( void) { return cone_angle.get_value(); }
	double get_soft_shadow_angle( void) { return soft_shadow_angle.get_value();}

	RgbSpotLightObj(void) {
		set_light_type( SpotLight);
	}

	~RgbSpotLightObj(void) { ; }
};								/* end RgbSpotLightObj def */


class PointLightObj : public LightObject {

 private:

 protected:

 public:

	virtual int get_type(void) { return POINT_LIGHT_V1; }

	PointLightObj(void): LightObject(3) {
		set_light_type( PointLight);
	}
	PointLightObj( int nbands) : LightObject(nbands) {
		set_light_type( PointLight);
	}

	~PointLightObj(void) { ; }
};								/* end PointLightObj def */

class RgbPointLightObj : public RgbLightObject {

 private:

 protected:

 public:

	virtual int get_type(void) { return POINT_LIGHT_V1; }

	RgbPointLightObj(void) {
		set_light_type( PointLight);
	}

	~RgbPointLightObj(void) { ; }
};								/* end RgbPointLightObj def */


class AreaLightObj : public LightObject {

 private:

 protected:
	
	Obj AreaLightSource;		/* light emitter: polygon, surface, etc */

 public:

	virtual int get_type(void) { return AREA_LIGHT_V1; }

	AreaLightObj(void): LightObject(3) {
		set_light_type( AreaLight);
	}
	AreaLightObj( int nbands) : LightObject(nbands) {
		set_light_type( AreaLight);
	}

	~AreaLightObj(void) { ; }
};								/* end AreaLightObj def */

class RgbAreaLightObj : public RgbLightObject {

 private:

 protected:

 public:

	virtual int get_type(void) { return AREA_LIGHT_V1; }

	RgbAreaLightObj(void) {
		set_light_type( AreaLight);
	}

	~RgbAreaLightObj(void) { ; }
};								/* end RgbAreaLightObj def */



#endif
