#include <stdlib.h>

								// this is the beginning of a texture class
								// and related classes

#include "grape/texture.h"


/*************************** DomRangeVec *********************************/

DomRangeVec::DomRangeVec( int nDimDom, int nDimRange)
{
	init();
	dimD = nDimDom;
	dimR = nDimRange;

#if 0
	int i;

	lld0.set_num( nDimDom);
	lld1.set_num( nDimDom);
	llr0.set_num( nDimDom);
	llr1.set_num( nDimDom);

	for (i = 0; i < nDimDom; i++) {
		lld0.set_size( i, nDimDom);
		lld1.set_size( i, nDimDom);
	}
	for (i = 0; i < nDimDom; i++) {
		llr0.set_size( i, nDimRange);
		llr1.set_size( i, nDimRange);
	}
#endif

}

void DomRangeVec::init(void) {
	dimD = dimR = 0;
}

void DomRangeVec::destroy(void) {
#if 0
	int i, j, k;

	if (d != NULL) {
		for (i = 0; i < dimD; i++) {
			if (d[i] != NULL) {
				for (j = 0; j < dimD; j++) {
					if (d[i][j] != NULL) {
						delete d[i][j];
					}
				}
			}
		}
	}
	if (r != NULL) {
		for (i = 0; i < dimD; i++) {
			if (r[i] != NULL) {
				for (j = 0; j < dimR; j++) {
					if (r[i][j] != NULL) {
						delete r[i][j];
					}
				}
			}
		}
	}
	if (d1 != NULL) {
		for (i = 0; i < dimD; i++) {
			if (d1[i] != NULL) {
				for (j = 0; j < dimD; j++) {
					if (d1[i][j] != NULL) {
						delete d1[i][j];
					}
				}
			}
		}
	}
	if (r1 != NULL) {
		for (i = 0; i < dimD; i++) {
			if (r1[i] != NULL) {
				for (j = 0; j < dimR; j++) {
					if (r1[i][j] != NULL) {
						delete r1[i][j];
					}
				}
			}
		}
	}
#else
	;
#endif
}

void DomRangeVec::set_vector( int ndx, double *pVecDom, double *pVecRange,
							 double *pVecDom1, double *pVecRange1)
{
	int i;
	
	if (lld0.get_num() < ndx + 1)
		lld0.set_num( ndx + 1);
	if (lld1.get_num() < ndx + 1)
		lld1.set_num( ndx + 1);
	if (llr0.get_num() < ndx + 1)
		llr0.set_num( ndx + 1);
	if (llr1.get_num() < ndx + 1)
		llr1.set_num( ndx + 1);
	lld0.set_size( ndx, dimD);
	lld1.set_size( ndx, dimD);
	llr0.set_size( ndx, dimR);
	llr1.set_size( ndx, dimR);

	for (i = 0; i < dimD; i++)
		lld0.set_value( ndx, i, pVecDom[i]);
	for (i = 0; i < dimR; i++)
		llr0.set_value( ndx, i, pVecRange[i]);
	for (i = 0; i < dimD; i++)
		lld1.set_value( ndx, i, pVecDom1[i]);
	for (i = 0; i < dimR; i++)
		llr1.set_value( ndx, i, pVecRange1[i]);
}
void DomRangeVec::get_vector( int ndx, double *pVecDom, double *pVecRange,
							 double *pVecDom1, double *pVecRange1)
{
	int i;
	for (i = 0; i < dimD; i++)
		pVecDom[i] = lld0.get_value( ndx, i);
	for (i = 0; i < dimR; i++)
		pVecRange[i] = llr0.get_value( ndx, i);
	for (i = 0; i < dimD; i++)
		pVecDom1[i] = lld1.get_value( ndx, i);
	for (i = 0; i < dimR; i++)
		pVecRange1[i] = llr1.get_value( ndx, i);
}

void DomRangeVec::dump(void)
{
	int i, k;
	double *d0, *d1, *r0, *r1;
	char szMask[1024], tstr[1024];

	d0 = (double *)malloc( dimD * sizeof(double));
	d1 = (double *)malloc( dimD * sizeof(double));
	r0 = (double *)malloc( dimR * sizeof(double));
	r1 = (double *)malloc( dimR * sizeof(double));

	printf ("\nDump of DomRangeVec object\n");
	for (i = 0; i < dimD; i++) {
		printf ("  i=%d\n", i);
		get_vector( i, d0, r0, d1, r1);

		memset( szMask, 0, 1024);
		strcat( szMask, "      (");
		for (k = 0; k < dimD; k++)
			strcat( szMask, "%f ");
		strcat( szMask, ")");
		strcat( szMask, " ----> (");
		for (k = 0; k < dimD; k++)
			strcat( szMask, "%f ");
		strcat( szMask, ")\n");

		if (dimD == 3 && dimR == 2)
			sprintf( tstr, szMask, d0[0], d0[1], d0[2], r0[0], r0[1]);
		else if (dimD == 2 && dimR == 2)
			sprintf( tstr, szMask, d0[0], d0[1], r0[0], r0[1]);
		else
			sprintf( tstr, "not yet supported\n");

		printf ("    0th point:\n");
		printf( tstr);
				
		if (dimD == 3 && dimR == 2)
			sprintf( tstr, szMask, d1[0], d1[1], d1[2], r1[0], r1[1]);
		else if (dimD == 2 && dimR == 2)
			sprintf( tstr, szMask, d1[0], d1[1], r1[0], r1[1]);
		else
			sprintf( tstr, "not yet supported\n");

		printf ("    1st point:\n");
		printf( tstr);
		
	}

	free( d0);
	free( d1);
	free( r0);
	free( r1);
}

int DomRangeVec::parse_in( Dataport *fp)
{
	char	token[4096];


	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in DomRangeVec\n");
		return(FALSE);
	}

	if (strcmp( token, "LLD")) {
		fprintf(stderr," Whoops - missing tag 'LLD' in DomRangeVec\n");
		return(FALSE);
	}
		
	lld0.parse_in( fp);
	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in DomRangeVec\n");
		return(FALSE);
	}

	if (strcmp( token, "LLD")) {
		fprintf(stderr," Whoops - missing tag 'LLD' in DomRangeVec\n");
		return(FALSE);
	}

	lld1.parse_in( fp);
	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in DomRangeVec\n");
		return(FALSE);
	}

	if (strcmp( token, "LLD")) {
		fprintf(stderr," Whoops - missing tag 'LLD' in DomRangeVec\n");
		return(FALSE);
	}


	llr0.parse_in( fp);
	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in DomRangeVec\n");
		return(FALSE);
	}

	if (strcmp( token, "LLD")) {
		fprintf(stderr," Whoops - missing tag 'LLD' in DomRangeVec\n");
		return(FALSE);
	}

	llr1.parse_in( fp);


	return TRUE;
}

int DomRangeVec::parse_out( Dataport *fp, int expand)
{
	lld0.parse_out( fp, expand);
	lld1.parse_out( fp, expand);

	llr0.parse_out( fp, expand);
	llr1.parse_out( fp, expand);

	return TRUE;
}

/*************************** Mapping *********************************/

Mapping::Mapping( int dimDom, int dimRange)
{
	init();
	if (nDimRange > 2) {
		FATAL_ERROR( "Sorry, textures are images, and the current max dimension is 2");
	}
	set_dims( dimDom, dimRange);
}

/*************************** ImpMapPlanarProj ************************/

ImpMapPlanarProj::ImpMapPlanarProj( int d, int r) : ImplicitMapping( d, r) {

	/* orthogonal mapping */
	pdv = new DomRangeVec( d, r);
}

int ImpMapPlanarProj::eval( double *pDomVector, double *pRetRangeVector)
{
	int i;
	double *d0, *d1, *r0, *r1;

	d0 = (double *)malloc( nDimDomain * sizeof(double));
	d1 = (double *)malloc( nDimDomain * sizeof(double));
	r0 = (double *)malloc( nDimRange * sizeof(double));
	r1 = (double *)malloc( nDimRange * sizeof(double));

	for (i = 0; i < nDimDomain; i++) {
		pdv->get_vector( i, d0, r0, d1, r1);


		if (d1[i] - d0[i] != 0.0) {
			pRetRangeVector[i] = (((pDomVector[i] - d0[i]) / 
			(d1[i] - d0[i])) * (r1[i] - r0[i])) + r0[i];
		}
		else {
			pRetRangeVector[i] = 0.0;
		}
		
	}

	free( d0);
	free( d1);
	free( r0);
	free( r1);
	return 1;
		
}

int ImpMapPlanarProj::eval( float *pDomVector, float *pRetRangeVector)
{
	int i;
	double *d0, *d1, *r0, *r1;

	d0 = (double *)malloc( nDimDomain * sizeof(double));
	d1 = (double *)malloc( nDimDomain * sizeof(double));
	r0 = (double *)malloc( nDimRange * sizeof(double));
	r1 = (double *)malloc( nDimRange * sizeof(double));

	for (i = 0; i < nDimDomain; i++) {
		pdv->get_vector( i, d0, r0, d1, r1);


		if (d1[i] - d0[i] != 0.0) {
			pRetRangeVector[i] = (float)((((pDomVector[i] - d0[i]) / (d1[i] - d0[i])) * 
				(r1[i] - r0[i])) + r0[i]);
		}
		else {
			pRetRangeVector[i] = 0.0;
		}
		
	}

	free( d0);
	free( d1);
	free( r0);
	free( r1);
	return 1;
		
}

void ImpMapPlanarProj::set_dom_to_range_vector( int ndxVec, 
											   double *pD,
											   double *pR,
											   double *pD1,
											   double *pR1)
{
	pdv->set_vector( ndxVec, pD, pR, pD1, pR1);
}

void ImpMapPlanarProj::get_dom_to_range_vector(int ndxVec, 
											   double *pD,
											   double *pR,
											   double *pD1,
											   double *pR1)
{
	pdv->get_vector( ndxVec, pD, pR, pD1, pR1);
}

void ImpMapPlanarProj::clear_all_map_vectors( void)
{
	double *d0, *r0;
	int i;

	if ((d0 = (double *)calloc( nDimDomain, sizeof(double))) == NULL) {
		FATAL_ERROR( "d0 is NULL");
	}
	if ((r0 = (double *)calloc( nDimRange, sizeof(double))) == NULL) {
		FATAL_ERROR( "r0 is NULL");
	}

	for (i = 0; i < nDimDomain; i++) {
		pdv->set_vector( i, d0, r0, d0, r0);
	}

	free( d0);
	free( r0);

}

int ImpMapPlanarProj::parse_in(Dataport *fp)
{
	char	token[4096];
	int  nDimDom, nDimRan;
	

	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in planar proj mapping file\n");
		return(FALSE);
	}
	nDimDom = atoi( token);
	
	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in planar proj mapping file\n");
		return(FALSE);
	}
	nDimRan = atoi( token);
	
	set_dims( nDimDom, nDimRan);


	if (!pdv->parse_in( fp)) {
		FATAL_ERROR( "pdv->parse_in failed");
	}
	
	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in planar proj mapping file\n");
		return(FALSE);
	}

	if (strcmp( token, "END_MAPPING")) {
		FATAL_ERROR( "Missing tag 'END_MAPPING'");
	}
	return(TRUE);
	
}


int ImpMapPlanarProj::parse_out(Dataport *fp, int expand)
{
	char token[1024];

	put_token( fp, "\nMAPPING");

	put_token( fp, "IMP_PLANAR_MAPPING\n");

	sprintf( token, "%d %d\n", nDimDomain, nDimRange);
	put_token( fp, token);

	pdv->parse_out( fp, expand);

	put_token( fp, "\nEND_MAPPING");

	return TRUE;
}

int ImpMapSphereProj::parse_in(Dataport *fp)
{
	char	token[4096];
	int  nDimDom, nDimRan;
	int i;

	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in spherical proj mapping file\n");
		return(FALSE);
	}
	nDimDom = atoi( token);
	
	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in spherical proj mapping file\n");
		return(FALSE);
	}
	nDimRan = atoi( token);
	
	set_dims( nDimDom, nDimRan);

	for (i = 0; i < nDimDom; i++) {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in spherical proj mapping file\n");
			return(FALSE);
		}
		center.set_value( i, atof( token));
	}

	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in spherical proj mapping file\n");
		return(FALSE);
	}
	if (strcmp( token, "END_MAPPING")) {
		FATAL_ERROR( "Missing tag 'END_MAPPING'");
	}
	return(TRUE);
	
}
int ImpMapSphereProj::parse_out(Dataport *fp, int expand)
{
	char token[1024];
	int i;

	put_token( fp, "\nMAPPING");

	put_token( fp, "IMP_SPHERE_MAPPING\n");

	sprintf( token, "%d %d\n", nDimDomain, nDimRange);
	put_token( fp, token);

	for (i = 0; i < nDimDomain; i++) {
		if (!i)
			sprintf( token, "\n%f ", center.get_value( i));
		else
			sprintf( token, "%f ", center.get_value( i));
		put_token( fp, token);
	}

	put_token( fp, "\nEND_MAPPING");

	return TRUE;
}

GeoMapping::GeoMapping(GeoImage *color_gi, GeoImage *disp_gi) 
{
     if (!(disp_geoData = disp_gi->get_geo_data()))
	  fprintf(stdout,"Invalid Displacement GeoImage\n");
     else disp_gi->get_res(&disp_xres, &disp_yres);

     if (!(color_geoData = color_gi->get_geo_data()))
	  fprintf(stdout,"Invalid Color GeoImage\n");
     else color_gi->get_res(&color_xres, &color_yres);
}

GeoMapping::GeoMapping(GeoData *color_gd, GeoData *disp_gd, 
				int cxres, int cyres, int dxres, int dyres)
{
	color_geoData = color_gd; disp_geoData = disp_gd; 
	color_xres = cxres; color_yres = cyres;
	disp_xres = dxres; disp_yres = dyres;
}

int GeoMapping::eval(float *pDomVector, float *pRetRangeVector)
{
	double disp_x = pDomVector[0];	
	double disp_y = pDomVector[1];
	double disp_lat, disp_lon;
	double  color_x, color_y;

	disp_geoData->xytoll(disp_x, disp_y, disp_lat, disp_lon);
	color_geoData->lltoxy(disp_lat, disp_lon, color_x, color_y);

	pRetRangeVector[0] = color_x / color_xres;
	pRetRangeVector[1] = color_y / color_yres;

	return 1;
}

int GeoMapping::eval(double *pDomVector, double *pRetRangeVector)
{
	double disp_x = pDomVector[0];	
	double disp_y = pDomVector[1];	
	double disp_lat, disp_lon;
	double  color_x, color_y;

	disp_geoData->xytoll(disp_x, disp_y, disp_lat, disp_lon);
	color_geoData->lltoxy(disp_lat, disp_lon, color_x, color_y);

	pRetRangeVector[0] = color_x / color_xres;
	pRetRangeVector[1] = color_y / color_yres;

	return 1;
}

#if 0
/************************ ImageTexture *************************************/
int ImageTexture::get_val( int *pCoordVector, /* coordinate vector  */
						int nBandNdx, /* index of band */
						uchar *pReturnData, /* returned value */
						double t = 0.0 /* time */
						)
{
	Image *pI;
	ImageData *pID;

	pI = pImgList->get_image_by_time( t);
	if (pI == NULL)
		return 0;				// fail
	pID = pI->get_data();
	if (pID == NULL)
		return 0;				// fail

	pI->set_default_band( nBandNdx);
	if (nDimensions == 2)
		*pReturnData = 
			pID->get_uchar( pCoordVector[0], pCoordVector[1], nBandNdx);
	else
		*pReturnData =
			pID->get_uchar( pCoordVector[0], 0, nBandNdx);

	return 1;					// success
}

								/* get value of all bands */
int ImageTexture::get_val( int *pCoordVector, /* coordinate vector  */
						uchar *pReturnData, /* returned vector of bands */
						double t = 0.0 /* time */
						)
{
	
	Image *pI;
	ImageData *pID;
	int i;

	pI = pImgList->get_image_by_time( t);
	if (pI == NULL)
		return 0;				// fail
	pID = pI->get_data();
	if (pID == NULL)
		return 0;				// fail

	if (nDimensions == 2) {
		for (i = 0; i < 2; i++)
			pReturnData[i] = 
				pID->get_uchar( pCoordVector[0], pCoordVector[1], i);
	}
	else
		for (i = 0; i < 2; i++)
			pReturnData[i] = 
				pID->get_uchar( pCoordVector[0], 0, i);

	return 1;					// success
}



void *ImageTexture::get_texture( double t)
{
								// for now, only get the image associated with
								// time 0.0

	t = 0.0;
	return (void *)images.get_image_by_time( t);
	
}

void ImageTexture::add_image( Image *pImg, double t)
{
	

								// for now, only set texture of 1st image

	images.add_image( pImg, t);
}

#endif

/******************************* lldparam ************************************/

int lldparam::parse_in(Dataport *fp)
{
	char	token[4096];

#if 0
	if(!strcmp(token, "DATA")) {

		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
			return(FALSE);
		}
		if(!strcmp(token, "{")) {
			env = envelope_create(fp);
			if(!env) {
				fprintf(stderr,"Whoops - Unrecognized envelope type in scene file %s\n",token);
				return(FALSE);
			}
			if(!env->parse_in(fp)) {
				fprintf(stderr,"Whoops - Problems parsing referenced envelope %s\n",token);
				return(FALSE);
			}
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
				return(FALSE);
			}
			if(strcmp(token, "}")) {
				fprintf(stderr," Whoops - Missing } after inline envelope \n");
				return(FALSE);
			}
		} else {
			tfp = dataport_create(fp->get_type());
			if(tfp && tfp->ropen(token)) {
				env = envelope_create(tfp);
				if(!env) {
					fprintf(stderr,"Whoops - Unrecognized envelope type in external object file %s\n",token);
					return(FALSE);
				}
				if(!env->parse_in(tfp)) {
					fprintf(stderr,"Whoops - Problems parsing referenced envelope %s\n",token);
					return(FALSE);
				}
				env->set_reference(token);
			} else {
				fprintf(stderr,"Whoops - Unable to open referenced envelope file %s\n", token);
				return(FALSE);
			}
		}
		set_data(env);
	} else {
	}
#else
	
	while ( 1) {
		
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in parsing lld parameter\n");
			return(FALSE);
		}

		if ( !strcmp( token, "LLD_END"))
			break;				// end of lldparam

		else if ( !strcmp( token, "LD")) {
			add_list();
		}
		else if (!get_num()) {	// go no LD tags yet! no lists yet created!

			fprintf(stderr," Whoops - missing beg token 'LD' parsing lld parameter\n");
			return FALSE;
		}

		else {					// add double to end of current last list
			add_value( get_num() - 1, atof( token));
		}
	}
#endif

	return(TRUE);
}

int lldparam::parse_out(Dataport *fp, int expand)
{
	int i, j;
	char    token[4096];

	if (dat) {
								// fill in later
	}
	else {

		put_token( fp, "\nLLD");
		for (i = 0; i < num ; i++) {
			put_token( fp, "\nLD");
			for (j = 0; j < size[i]; j++) {
				sprintf(token,"%f", get_value( i, j));
				put_token(fp, token);
			}
		}
		put_token( fp, "\nLLD_END");
	}

	return TRUE;
}

/****************************** TextureMap ********************************/

int TextureMap::parse_in( Dataport *fp)
{
	char token[4096];
	Dataport    *tfp;

    if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in parsing texture map\n");
		return(FALSE);
	}

	while ( strcmp( token, "END_TEXTURE_MAPPING")) {

		if ( !strcmp( token, "TEXTURE")) {
			if (!texture.parse_in( fp)) {
				fprintf(stderr," Failed parsing in imgparam object while parsing TextureMap object\n");
				return FALSE;
			}

		}
		
		else if (! strcmp( token, "MAPPING")) {

			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in parsing texture map\n");
				return(FALSE);
			}
			
			if ( !strcmp( token, "{")) { // inline mapping
				FATAL_ERROR( "NYI");
			}
			else {				// token is name of mapping file
				if ((tfp = dataport_create(fp->get_type())) == NULL) {
					fprintf( stderr, "Unable to create dataport while parsing TextureMap\n");
					return FALSE;
				}

								// open map file
				if (tfp->ropen( token)) {

					Mapping *pTMap;

								// read in mapping type & create object for it
					if ((pTMap = mapping_creator( tfp)) == NULL) {
						fprintf( stderr, "Unable to create mapping\n");
						return FALSE;
					}
			
					set_mapping( pTMap);

					if (!pTMap->parse_in( tfp)) {
						fprintf( stderr, "Unable to parse_in() mapping\n");
						return FALSE;
					}
					tfp->close();

					pTMap->set_reference( token);
				}
				else {
                    fprintf( stderr, "Unable to open mapping file '%s'\n",
							token);
					return FALSE;
				}

			}

			
		}
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in parsing texture map\n");
			return(FALSE);
		}

	}
	
	return TRUE;
}


int TextureMap::parse_out( Dataport *fp, int expand)
{

	if (expand || !get_reference() ) {
		FATAL_ERROR( "NYI");
	}
	else {
		put_token(fp, get_reference());
	}

	put_token( fp, "\nTEXTURE");
	texture.parse_out( fp, expand);

	if (pMap != NULL) {
		put_token( fp, "\nMAPPING");

		if (expand || !pMap->get_reference()) {
			put_token(fp, "{");
			pMap->parse_out( fp, expand);
			put_token(fp, "}");
		}
		else {
			put_token( fp, pMap->get_reference());
		}
	}

	put_token( fp, "\nEND_TEXTURE_MAP\n");

	return TRUE;
}

/******************************** other routines ***************************/

Mapping *mapping_creator( Dataport *fp)
{
	char    token[128];
	Mapping *pRet = NULL;

	get_next_token(fp, token);
	if (!strcmp( token, "IMP_PLANAR_MAPPING"))
		pRet = new ImpMapPlanarProj();
	else if (!strcmp( token, "IMP_SPHERE_MAPPING"))
		pRet = new ImpMapSphereProj(); // currently 3-to-2 dims only!
	else {
		FATAL_ERROR( "Unknown mapping type");
	}

	return pRet;
}
