// kap_geodata.h
//
// Written by John Wright 04/01/97

#include "image/types/kap_geodata.h"

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

// This data is used to convert to/from character strings in the
// kap file header to our internal datum and projection indexes.
// These MUST correspond to the indexes found in ../geodatatypes.h


	static	char	*kap_datum_strings[] = {
			(char *)"UNKNOWN",
			(char *)"NAD02",
			(char *)"NAD27",
			(char *)"NAD83",
			(char *)"WGS84",
			(char *)"Astronomic Datum",
			(char *)"Astronomic Datum (general)",
			(char *)"Astronomic Datums (general)",
			(char *)"Astronomic Datum 1928 (field)",
			(char *)"Astronomic Datum 1930 (field)",
			(char *)"Astronomic Datum 1931 (field)",
			(char *)"Astro Datum 1939",
			(char *)"Local Astronomic Datum",
			(char *)"Local Datum (undetermined)",
			(char *)"Local Datum",
			(char *)"Gardners Pinnacles 1929",
			(char *)"Neckers 1928",
			(char *)"Nihoa 1928",
			(char *)"Old Hawaiian",
			(char *)"Togcha Datum",
			(char *)"Johnston Isl 1961 Astro Datum",
			(char *)"Junk Place Holder Insert above Here" };
			

	static	char	*kap_projection_strings[] = {
			(char *)"UNKNOWN",
			(char *)"MERCATOR",
			(char *)"UNIVERSAL TRANSVERSE MERCATOR",
			(char *)"POLYCONIC",
			(char *)"LAMBERT CONFORMAL CONIC",
			(char *)"GEOGRAPHIC LAT LON",
			(char *)"Junk Place Holder Insert above Here" };

/*
c**************************************************************
C
C1    GENERAL LEAST SQUARES SOLUTION OF NE EQUATIONS WITH NU UNKNOWNS,
C     C(I,1)*X1(1)+C(I,2)*X1(2)+...+C(I,NU)=CL(I) OF EQUAL WEIGHTS,WITH
C     I RANGING FROM 1 TO NE.
C
C2    THE INFORMATION FROM THE MAIN PROGRAM IS:
C          C(I,J) = COEFFICIENT MATRIX
C          CL(I) = ARRAY OF FREE TERMS
C          NE = NUMBER OF EQUATIONS
C          NU=NUMBER OF UNKNOWNS
c          wts=weight for each input point
C
C3    THE INFORMATION RETURNED TO THE MAIN PROGRAM IS:
C          X1(J) = COMPUTED VALUES OF THE UNKNOWNS
C
C5    ALL THE STATEMENTS BELOW ARE VALID FOR ANY NU LARGER THAN 1 AND
C     ANY NE LARGER THAN NU.
C
**************************************
NOTE NOTE NOTE 
In this translation from fortran to C the same subscripts were used for simplicity
Therefore use entries 1 -> N in each array rather that 0 -> N-1
**************************************
*/
//      SUBROUTINE LSQP(ind,NE,NU,C,CL,wts,X1)
	int lsqp(int num_eqs, int num_unks, double coeffs[100][100], double values[100], double weights[100], double results[100], double residuals[100], double *mean_err_unit_wt, double mean_err_unk[100])
{
//      REAL*8  A(4,4),AL(4),R(4,4),RL(4),Q(4,4),X(4),SL,SQ,P,SUM
//      REAL*8 C(20,4),CL(20),X1(4),V(20),EX(4),wts(20)
	double a[100][100], al[100], r[100][100], rl[100], q[100][100], x[100], sum;
	double *cl, *wts;
	int	i, j, k, l;
	int	num, nup, ix, ixi;

	if(num_eqs < num_unks) return(FALSE);
	cl = values;
	wts = weights;

//      ind=0
//      DO 57 J = 1,NU
//      DO 57 I=1,NU
//      A(I,J)=0.
//      R(I,J)=0.
//57    Q(I,J)=0.
	for(i=1; i<=num_unks; i++) {
		for(j=1; j<=num_unks; j++) {
			a[i][j] = 0.0;
			r[i][j] = 0.0;
			q[i][j] = 0.0;
		}
	}

//      DO 100 I=1,NU
//      DO 100 J=1,NU
//      DO 100 K=1,NE
//100   A(I,J)=A(I,J)+C(K,I)*C(K,J)*wts(k)
	for(i=1; i<=num_unks; i++) {
		for(j=1; j<=num_unks; j++) {
			for(k=1; k<=num_eqs; k++) {
				a[i][j] += coeffs[k][i]*coeffs[k][j]*wts[k];
			}
		}
	}

//      DO 102 I=1,NU
//      AL(I)=0.
//      DO 102 K=1,NE
//102   AL(I)=AL(I)+C(K,I)*CL(K)*wts(k)
	for(i=1; i<=num_unks; i++) {
		al[i] = 0.0;
		for(k=1; k<=num_eqs; k++) {
			al[i] += coeffs[k][i]*cl[k]*wts[k];
		}
	}

//      NUM=NU-1
//      NUP=NU+1
	num = num_unks - 1;
	nup = num_unks + 1;

//      DO 110 I=1,NUM
//      K=I+1
//      DO 110 J=K,NU
//      if(a(i,i).eq.0.d0)goto 999
//      R(I,J)=A(I,J)/A(I,I)
//      DO 110 L=1,I
//110   A(K,J)=A(K,J)-R(L,K)*A(L,J)
	for(i=1; i<=num; i++) {
		k = i + 1;
		for(j=k; j<=num_unks; j++) {
			if(a[i][i] == 0.0) return(FALSE);
			r[i][j] = a[i][j]/a[i][i];
			for(l=1; l<=i; l++) {
				a[k][j] -= r[l][k]*a[l][j];
			}
		}
	}

//      if(a(1,1).eq.0.d0)goto 999
//      RL(1)=AL(1)/A(1,1)
	if(a[1][1] == 0.0) return(FALSE);
	rl[1] = al[1]/a[1][1];

//      DO 125 I=2,NU
//      DO 122 J=1,I
//122   AL(I)=AL(I)-R(J,I)*AL(J)
//      if(a(i,i).eq.0.d0)goto 999
//125   RL(I)=AL(I)/A(I,I)
	for(i=2; i<=num_unks; i++) {
		for(j=1; j<=i; j++) {
			al[i] -= r[j][i]*al[j];
		}
		rl[i] = al[i]/a[i][i];
	}

//       X(NU)=RL(NU)
	x[num_unks] = rl[num_unks];

//      DO 131 I=1,NUM
//      IX=NU-I
//      IXI=IX+1
//      SUM=0.
//      DO 130 J=IXI,NU
//130   SUM=SUM-R(IX,J)* X(J)
//131    X(IX)=RL(IX)+SUM
	for(i=1; i<=num; i++) {
		ix = num_unks - i;
		ixi = ix + 1;
		sum = 0.0;
		for(j=ixi; j<=num_unks; j++) {
			sum -= r[ix][j] * x[j];
		}
		x[ix] = rl[ix] + sum;
	}

//      DO 200 J=1,NU
//200   X1(J)=X(J)
	for(j=1; j<=num_unks; j++) {
		results[j] = x[j];
	}

	int np, nm, jp, npm;
	double p, sq;

//      Q(NU,NU)=1./A(NU,NU)
	q[num_unks][num_unks] = 1.0/a[num_unks][num_unks];

//      DO 150 I=1,NUM
//      NP=NUP-1
	for(i=1; i<=num; i++) {
		np = nup - 1;
//      DO 135 J=I,NUM
//      NM=NU-J
//      JP=NM+1
//      P=0.0D0
//      DO 135 K=JP,NU
//      P=P-R(NM,K)*Q(NP,K)
//      Q(NP,NM)=P
//135   Q(NM,NP)=P
		for(j=i; j<=num; j++) {
			nm = num_unks - j;
			jp = nm + 1;
			p = 0.0;
			for(k=jp; k<=num_unks; k++) {
				q[np][nm] = p;
				q[nm][np] = p;
			}
		}

//      NPM=NP-1
//      SQ=0.0D0
//      DO 145 L=NP,NU
//145   SQ=SQ-R(NPM,L)*Q(L,NPM)
		npm = np - 1;
		sq = 0.0;
		for(l=np; l<=num_unks; l++) {
			sq -= r[npm][l] * q[l][npm];
		}

//150   Q(NPM,NPM)=1./A(NPM,NPM)+SQ
		q[npm][npm] = 1.0 / a[npm][npm] + sq;
	}

//      DO 151 I=1,NE
//      V(I)=0.
//      DO 151 J=1,NU
//151   V(I)=V(I)+C(I,J)* X(J)
	for(i=1; i<=num_eqs; i++) {
		residuals[i] = 0.0;
		for(j=1; j<=num_unks; j++) {
			residuals[i] += coeffs[i][j] * x[j];
		}
	}

//      SL=0.0D0
//      DO 153 I=1,NE
//      V(I)=CL(I)-V(I)
//153   SL=SL+V(I)*V(I)
	double sl = 0.0;
	for(i=1; i<=num_eqs; i++) {
		residuals[i] = cl[i] - residuals[i];
		sl += residuals[i] * residuals[i];
	}

//      FNE=NE
//      FNU=NU
//      E=DSQRT(SL/(FNE-FNU))
	*mean_err_unit_wt = sqrt(sl/(num_eqs-num_unks));

//      DO 160 I=1,NU
//        IF ( Q(I,I) .GE. 0.D0 ) THEN
//          EX(I)=E*DSQRT(Q(I,I))
//        ELSE
//          EX(I)= 0.0                ! HANDLE NEGATIVES DUE TO ROUNDOFF.
//        END IF
//160   CONTINUE   
	for(i=1; i<=num_unks; i++) {
		if(q[i][i] >= 0.0) {
			mean_err_unk[i] = *mean_err_unit_wt * sqrt(q[i][i]);
		} else {
			mean_err_unk[i] = 0.0;
		}
	}

	return(TRUE);

//      RETURN
//999   ind=1
//      return
//      END
}

void	KAPGeoData::init(void)
{
	datum_strings = kap_datum_strings;
	projection_strings = kap_projection_strings;

	kapname = NULL;
	kapnumber = 0;
	draw_units = 0;

	scale = 0;
	datum = UNKNOWN;
	projection = UNKNOWN;
	projparm = 0.0;
	projint = 0.0;
	num_planes = 0;
	state_plane_system = NULL;
	state_plane_intervals = NULL;
	skew_angle = 0.0;
	text_angle = 0.0;
	units = NULL;
	sound_datum = NULL;
	x_meters = 0.0;
	y_meters = 0.0;

	mag_north = 0.0;
	year_estab = 0;
	variance = 0.0;

	edition = 0;
	raster_ed = 0;
	ed_date	= NULL;

	major = 0;
	minor = 0;

	image_lines = 1;

	ref_count = 0;
	ref_x = NULL;
	ref_y = NULL;
	ref_lat = NULL;
	ref_long = NULL;

	shift_value = 0.0;

	wpx_term_count = 0;
	wpx_terms = NULL;

	pwx_term_count = 0;
	pwx_terms = NULL;

	wpy_term_count = 0;
	wpy_terms = NULL;

	pwy_term_count = 0;
	pwy_terms = NULL;

	pixel_x_err = NULL;
	pixel_y_err = NULL;
	latitude_err = NULL;
	longitude_err = NULL;

	vertices = 0;
	v_latitude = NULL;
	v_longitude = NULL;
	clear_min_max();

	lat_shift = 0.0;
	long_shift = 0.0;

}

KAPGeoData::~KAPGeoData(void)
{
	set_kapname(NULL);
	clear_state_planes();
	set_units(NULL);
	set_sound_datum(NULL);
	set_ed_date(NULL);
	clear_reference_points();
	clear_wpx_terms();
	clear_pwx_terms();
	clear_wpy_terms();
	clear_pwy_terms();
	clear_poly_verts();
}

	// coordinate space converters (instantiation of virtuals)
double KAPGeoData::lltox(double lat, double lng)
{
	double	temp = 0.0;

//	if(!in_image(lat, lng)) return(-1.0);
	lng += get_shift_value();
	while(lng >= 180.0) lng -= 360.0;
	if(get_num_wpx_terms() < 3) return(temp);
	temp += wpx_terms[0] + wpx_terms[1]*lng + wpx_terms[2]*lat;
	if(get_num_wpx_terms() < 6) return(temp);
	temp += wpx_terms[3]*lng*lng + wpx_terms[4]*lng*lat + wpx_terms[5]*lat*lat;
	if(get_num_wpx_terms() < 10) return(temp);
	temp += wpx_terms[6]*lng*lng*lng + wpx_terms[7]*lng*lng*lat + wpx_terms[8]*lng*lat*lat + wpx_terms[9]*lat*lat*lat;
	if(get_num_wpx_terms() < 11) return(temp);
	temp += wpx_terms[10]*lat*lat*lat*lat;
	if(get_num_wpx_terms() < 12) return(temp);
	temp += wpx_terms[11]*lat*lat*lat*lat*lat;
	
	return(temp);
}
double KAPGeoData::lltoy(double lat, double lng)
{
	double	temp = 0.0;

//	if(!in_image(lat, lng)) return(-1.0);
	lng += get_shift_value();
	while(lng >= 180.0) lng -= 360.0;
	if(get_num_wpy_terms() < 3) return(temp);
	temp += wpy_terms[0] + wpy_terms[1]*lng + wpy_terms[2]*lat;
	if(get_num_wpy_terms() < 6) return(temp);
	temp += wpy_terms[3]*lng*lng + wpy_terms[4]*lng*lat + wpy_terms[5]*lat*lat;
	if(get_num_wpy_terms() < 10) return(temp);
	temp += wpy_terms[6]*lng*lng*lng + wpy_terms[7]*lng*lng*lat + wpy_terms[8]*lng*lat*lat + wpy_terms[9]*lat*lat*lat;
	if(get_num_wpy_terms() < 11) return(temp);
	temp += wpy_terms[10]*lat*lat*lat*lat;
	if(get_num_wpy_terms() < 12) return(temp);
	temp += wpy_terms[11]*lat*lat*lat*lat*lat;
	
	return(temp);
}

double KAPGeoData::utmtox(double /* easting */, double /* northing */)
{
	return(0.0);
}
double KAPGeoData::utmtoy(double /* easting */, double /* northing */)
{
	return(0.0);
}

double KAPGeoData::utmtolat(double /* easting */, double /* northing */)
{
	return(0.0);
}
double KAPGeoData::utmtolong(double /* easting */, double /* northing */)
{
	return(0.0);
}

double KAPGeoData::lltoe(double /* latitude */, double /* longitude */)
{
	return(0.0);
}
double KAPGeoData::llton(double /* latitude */, double /* longitude */)
{
	return(0.0);
}

double KAPGeoData::xytolat(double x, double y)
{
	double	temp = 0.0;

	if(get_num_pwy_terms() < 3) return(temp);
	temp = pwy_terms[0] + pwy_terms[1]*x + pwy_terms[2]*y;
	if(get_num_pwy_terms() < 6) return(temp);
	temp += pwy_terms[3]*x*x + pwy_terms[4]*x*y + pwy_terms[5]*y*y;
	if(get_num_pwy_terms() < 10) return(temp);
	temp += pwy_terms[6]*x*x*x + pwy_terms[7]*x*x*y + pwy_terms[8]*x*y*y + pwy_terms[9]*y*y*y;
	if(get_num_pwy_terms() < 11) return(temp);
	temp += pwy_terms[10]*y*y*y*y;
	if(get_num_pwy_terms() < 12) return(temp);
	temp += pwy_terms[11]*y*y*y*y*y;
	
	return(temp);
}
double KAPGeoData::xytolong(double x, double y)
{
	double	temp = 0.0;

	if(get_num_pwx_terms() < 3) return(temp);
	temp = pwx_terms[0] + pwx_terms[1]*x + pwx_terms[2]*y;
	if(get_num_pwx_terms() >= 6) 
	temp += pwx_terms[3]*x*x + pwx_terms[4]*x*y + pwx_terms[5]*y*y;
	if(get_num_pwx_terms() >= 10) 
	temp += pwx_terms[6]*x*x*x + pwx_terms[7]*x*x*y + pwx_terms[8]*x*y*y + pwx_terms[9]*y*y*y;
	if(get_num_pwx_terms() >= 11) 
	temp += pwx_terms[10]*y*y*y*y;
	if(get_num_pwx_terms() >= 12) 
	temp += pwx_terms[11]*y*y*y*y*y;

	temp -= get_shift_value();
	if(temp < -180.00000000) temp += 360.0;
	if(temp >  180.00000000) temp -= 360.0;
	
	return(temp);
}

double KAPGeoData::xytoe(double /* x */, double /* y */)
{
	return(0.0);
}
double KAPGeoData::xyton(double /* x */, double /* y */)
{
	return(0.0);
}


int	KAPGeoData::num_terms_from_code(int code) {
		int	poly_terms_count[] = { 1, 3, 6, 10, 11, 12 };

		return(poly_terms_count[code]);
	}
int	KAPGeoData::num_terms_to_code(int num) {
		int	poly_terms_count[] = { 0, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 5 };

		return(poly_terms_count[num]);
	}

int 	KAPGeoData::gen_poly_verts(void)
{
	double	lat, lng;
	int	i;
	int	dummyx, dummyy;

	if(!get_num_ref_points()) return(0);

	if(!get_num_poly_vertices()) { // if none gen poly verts
		// generate 4 poly vertices from reference points
		for(i=0; i<get_num_ref_points(); i++) {
			get_reference_point(i, &dummyx, &dummyy, &lat, &lng);
			min_max_lat_lng(lat, lng);
		}
		add_poly_vert(min_lat, min_lng);
		add_poly_vert(max_lat, min_lng);
		add_poly_vert(max_lat, max_lng);
		add_poly_vert(min_lat, max_lng);
		return(4);
	} else {
		return(0);
	}
}

int     KAPGeoData::force_shift_value(void)
{
	double	eastmost_long, lat, lng;
	int	i;

	if(!get_num_poly_vertices()) { // gen poly verts
		if(!gen_poly_verts()) {
			return(FALSE);
		}
	}

	eastmost_long = -180.0;
	for(i=0; i<get_num_poly_vertices(); i++) {
		get_poly_vert(i, &lat, &lng);
		if(lng > eastmost_long) {
			eastmost_long = lng;
		}
	}
	set_shift_value(fabs(eastmost_long));
	return(TRUE);
}

int     KAPGeoData::gen_shift_value(void)
{
	double	eastmost_long, lat, lng;
	int	flag, i;

	if(!get_num_poly_vertices()) { // gen poly verts
		if(!gen_poly_verts()) {
			return(FALSE);
		}
	}

	eastmost_long = -180.0;
	flag = 0;
	for(i=0; i<get_num_poly_vertices(); i++) {
		get_poly_vert(i, &lat, &lng);
		if(lng > eastmost_long && lng < 0.0) {
			eastmost_long = lng;
		}
		if(lng > 90.0) flag |= 1;
		if(lng < -90.0) flag |= 2;
	}
	if(flag == 3) {
		set_shift_value(fabs(eastmost_long));
	} else {
		set_shift_value(0.0);
	}
	return(TRUE);
}

int	KAPGeoData::gen_pwx_terms(int poly_order)
{
	double	*lats, *longs, *xs, *ys;
	double	*lt, *ln, *lx, *ly;
	int	i, j, num_unks, num_eqs;
	double	coeffs[100][100], *weights, *terms, *results;
	double	*residuals, mean_err_unit_wt, *mean_err_unk, last_mean_err_unit_wt;
	int	tx, ty;

	if(!get_num_ref_points()) return(FALSE);

	lats = (double *)malloc((get_num_ref_points() + 1) * sizeof(double));
	longs = (double *)malloc((get_num_ref_points() + 1) * sizeof(double));
	xs = (double *)malloc((get_num_ref_points() + 1) * sizeof(double));
	ys = (double *)malloc((get_num_ref_points() + 1) * sizeof(double));
	lt = lats; ln = longs; lx = xs; ly = ys;
	for(i=0; i<get_num_ref_points(); i++) {
		get_reference_point(i, &tx, &ty, ++lt, ++ln);
		*(++lx) = (double)tx;
		*(++ly) = (double)ty;
	}

	last_mean_err_unit_wt = 1.0e55;

	for(i=1; i<poly_order; i++) {					// *****
		num_unks = num_terms_from_code(i);
		num_eqs = get_num_ref_points();
		weights = (double *)malloc((num_eqs+1)*sizeof(double));
		terms = (double *)malloc((num_eqs+1)*sizeof(double));
		results = (double *)malloc((num_unks+1)*sizeof(double));
		residuals = (double *)malloc((num_eqs+1)*sizeof(double));
		mean_err_unk = (double *)malloc((num_unks+1)*sizeof(double));

		// fill arrays starting at 1 for lsqp
		lx = longs;					// *****
		ln = xs;					// *****
		lt = ys;					// *****
		for(j=1; j<=num_eqs; j++) {
			weights[j] = 1.0;
			terms[j] = lx[j];
			if(num_unks > 0) {
				coeffs[j][0] = 1.0;
			}
			if(num_unks > 1) {
				coeffs[j][1] = ln[j];
			}
			if(num_unks > 2) {
				coeffs[j][2] = lt[j];
			}
			if(num_unks > 3) {
				coeffs[j][3] = ln[j]*ln[j];
			}
			if(num_unks > 4) {
				coeffs[j][4] = ln[j]*lt[j];
			}
			if(num_unks > 5) {
				coeffs[j][5] = lt[j]*lt[j];
			}
			if(num_unks > 6) {
				coeffs[j][6] = ln[j]*ln[j]*ln[j];
			}
			if(num_unks > 7) {
				coeffs[j][7] = ln[j]*ln[j]*lt[j];
			}
			if(num_unks > 8) {
				coeffs[j][8] = ln[j]*lt[j]*lt[j];
			}
			if(num_unks > 9) {
				coeffs[j][9] = lt[j]*lt[j]*lt[j];
			}
			if(num_unks > 10) {
				coeffs[j][10] = lt[j]*lt[j]*lt[j]*lt[j];
			}
			if(num_unks > 11) {
				coeffs[j][11] = lt[j]*lt[j]*lt[j]*lt[j]*lt[j];
			}
			coeffs[j][num_unks] = coeffs[j][0];
		}
		// least squares fit
		if(lsqp(num_eqs, num_unks, coeffs, terms, weights, results,
			residuals, &mean_err_unit_wt, mean_err_unk)) {
//fprintf(stderr,"In gen_pwx with order = %d and mean error = %f\n",i,mean_err_unit_wt);
			// if better than best so far then copy to poly term holder
			if(mean_err_unit_wt < last_mean_err_unit_wt * 0.7) {
				clear_pwx_terms();					// *****
				results[0] = results[num_unks];
				for(j=0; j<num_unks; j++) {
					add_pwx_term(results[j]);		// *****
//fprintf(stderr,"    Term %d = %f  with residual = %f\n",j,results[j], residuals[j]);
				}
				last_mean_err_unit_wt = mean_err_unit_wt;
			} else {
				break;
			}
		}
		// free arrays
		free(weights);
		free(terms);
		free(results);
		free(residuals);
		free(mean_err_unk);
	}

	free(lats);
	free(longs);
	free(xs);
	free(ys);
	return(TRUE);
}
int	KAPGeoData::gen_pwy_terms(int poly_order)
{
	double	*lats, *longs, *xs, *ys;
	double	*lt, *ln, *lx, *ly;
	int	i, j, num_unks, num_eqs;
	double	coeffs[100][100], *weights, *terms, *results;
	double	*residuals, mean_err_unit_wt, *mean_err_unk, last_mean_err_unit_wt;
	int	tx, ty;

	if(!get_num_ref_points()) return(FALSE);

	lats = (double *)malloc((get_num_ref_points() + 1) * sizeof(double));
	longs = (double *)malloc((get_num_ref_points() + 1) * sizeof(double));
	xs = (double *)malloc((get_num_ref_points() + 1) * sizeof(double));
	ys = (double *)malloc((get_num_ref_points() + 1) * sizeof(double));
	lt = lats; ln = longs; lx = xs; ly = ys;
	for(i=0; i<get_num_ref_points(); i++) {
		get_reference_point(i, &tx, &ty, ++lt, ++ln);
		*(++lx) = (double)tx;
		*(++ly) = (double)ty;
	}

	last_mean_err_unit_wt = 1.0e55;

	for(i=1; i<poly_order; i++) {					// *****
		num_unks = num_terms_from_code(i);
		num_eqs = get_num_ref_points();
		weights = (double *)malloc((num_eqs+1)*sizeof(double));
		terms = (double *)malloc((num_eqs+1)*sizeof(double));
		results = (double *)malloc((num_unks+1)*sizeof(double));
		residuals = (double *)malloc((num_eqs+1)*sizeof(double));
		mean_err_unk = (double *)malloc((num_unks+1)*sizeof(double));

		// fill arrays starting at 1 for lsqp
		lx = lats;					// *****
		ln = xs;					// *****
		lt = ys;					// *****
		for(j=1; j<=num_eqs; j++) {
			weights[j] = 1.0;
			terms[j] = lx[j];
			if(num_unks > 0) {
				coeffs[j][0] = 1.0;
			}
			if(num_unks > 1) {
				coeffs[j][1] = ln[j];
			}
			if(num_unks > 2) {
				coeffs[j][2] = lt[j];
			}
			if(num_unks > 3) {
				coeffs[j][3] = ln[j]*ln[j];
			}
			if(num_unks > 4) {
				coeffs[j][4] = ln[j]*lt[j];
			}
			if(num_unks > 5) {
				coeffs[j][5] = lt[j]*lt[j];
			}
			if(num_unks > 6) {
				coeffs[j][6] = ln[j]*ln[j]*ln[j];
			}
			if(num_unks > 7) {
				coeffs[j][7] = ln[j]*ln[j]*lt[j];
			}
			if(num_unks > 8) {
				coeffs[j][8] = ln[j]*lt[j]*lt[j];
			}
			if(num_unks > 9) {
				coeffs[j][9] = lt[j]*lt[j]*lt[j];
			}
			if(num_unks > 10) {
				coeffs[j][10] = lt[j]*lt[j]*lt[j]*lt[j];
			}
			if(num_unks > 11) {
				coeffs[j][11] = lt[j]*lt[j]*lt[j]*lt[j]*lt[j];
			}
			coeffs[j][num_unks] = coeffs[j][0];
		}
		// least squares fit
		if(lsqp(num_eqs, num_unks, coeffs, terms, weights, results,
			residuals, &mean_err_unit_wt, mean_err_unk)) {
			// if better than best so far then copy to poly term holder
			if(mean_err_unit_wt < last_mean_err_unit_wt * 0.7) {
				clear_pwy_terms();					// *****
				results[0] = results[num_unks];
				for(j=0; j<num_unks; j++) {
					add_pwy_term(results[j]);		// *****
				}
				last_mean_err_unit_wt = mean_err_unit_wt;
			} else {
				break;
			}
		}
		// free arrays
		free(weights);
		free(terms);
		free(results);
		free(residuals);
		free(mean_err_unk);
	}

	free(lats);
	free(longs);
	free(xs);
	free(ys);
	return(TRUE);
}
int	KAPGeoData::gen_wpx_terms(int poly_order)
{
	double	*lats, *longs, *xs, *ys;
	double	*lt, *ln, *lx, *ly;
	int	i, j, num_unks, num_eqs;
	double	coeffs[100][100], *weights, *terms, *results;
	double	*residuals, mean_err_unit_wt, *mean_err_unk, last_mean_err_unit_wt;
	int	tx, ty;

	if(!get_num_ref_points()) return(FALSE);

	lats = (double *)malloc((get_num_ref_points() + 1) * sizeof(double));
	longs = (double *)malloc((get_num_ref_points() + 1) * sizeof(double));
	xs = (double *)malloc((get_num_ref_points() + 1) * sizeof(double));
	ys = (double *)malloc((get_num_ref_points() + 1) * sizeof(double));
	lt = lats; ln = longs; lx = xs; ly = ys;
	for(i=0; i<get_num_ref_points(); i++) {
		get_reference_point(i, &tx, &ty, ++lt, ++ln);
		*(++lx) = (double)tx;
		*(++ly) = (double)ty;
	}

	last_mean_err_unit_wt = 1.0e55;

	for(i=1; i<poly_order; i++) {					// *****
		num_unks = num_terms_from_code(i);
		num_eqs = get_num_ref_points();
		weights = (double *)malloc((num_eqs+1)*sizeof(double));
		terms = (double *)malloc((num_eqs+1)*sizeof(double));
		results = (double *)malloc((num_unks+1)*sizeof(double));
		residuals = (double *)malloc((num_eqs+1)*sizeof(double));
		mean_err_unk = (double *)malloc((num_unks+1)*sizeof(double));

		// fill arrays starting at 1 for lsqp
		lx = xs;					// *****
		ln = longs;					// *****
		lt = lats;					// *****
		for(j=1; j<=num_eqs; j++) {
			weights[j] = 1.0;
			terms[j] = lx[j];
			if(num_unks > 0) {
				coeffs[j][0] = 1.0;
			}
			if(num_unks > 1) {
				coeffs[j][1] = ln[j];
			}
			if(num_unks > 2) {
				coeffs[j][2] = lt[j];
			}
			if(num_unks > 3) {
				coeffs[j][3] = ln[j]*ln[j];
			}
			if(num_unks > 4) {
				coeffs[j][4] = ln[j]*lt[j];
			}
			if(num_unks > 5) {
				coeffs[j][5] = lt[j]*lt[j];
			}
			if(num_unks > 6) {
				coeffs[j][6] = ln[j]*ln[j]*ln[j];
			}
			if(num_unks > 7) {
				coeffs[j][7] = ln[j]*ln[j]*lt[j];
			}
			if(num_unks > 8) {
				coeffs[j][8] = ln[j]*lt[j]*lt[j];
			}
			if(num_unks > 9) {
				coeffs[j][9] = lt[j]*lt[j]*lt[j];
			}
			if(num_unks > 10) {
				coeffs[j][10] = lt[j]*lt[j]*lt[j]*lt[j];
			}
			if(num_unks > 11) {
				coeffs[j][11] = lt[j]*lt[j]*lt[j]*lt[j]*lt[j];
			}
			coeffs[j][num_unks] = coeffs[j][0];
		}
		// least squares fit
		if(lsqp(num_eqs, num_unks, coeffs, terms, weights, results,
			residuals, &mean_err_unit_wt, mean_err_unk)) {
			// if better than best so far then copy to poly term holder
			if(mean_err_unit_wt < last_mean_err_unit_wt * 0.7) {
				clear_wpx_terms();					// *****
				results[0] = results[num_unks];
				for(j=0; j<num_unks; j++) {
					add_wpx_term(results[j]);		// *****
				}
				last_mean_err_unit_wt = mean_err_unit_wt;
			} else {
				break;
			}
		}
		// free arrays
		free(weights);
		free(terms);
		free(results);
		free(residuals);
		free(mean_err_unk);
	}

	free(lats);
	free(longs);
	free(xs);
	free(ys);
	return(TRUE);
}
int	KAPGeoData::gen_wpy_terms(int poly_order)
{
	double	*lats, *longs, *xs, *ys;
	double	*lt, *ln, *lx, *ly;
	int	i, j, num_unks, num_eqs;
	double	coeffs[100][100], *weights, *terms, *results;
	double	*residuals, mean_err_unit_wt, *mean_err_unk, last_mean_err_unit_wt;
	int	tx, ty;

	if(!get_num_ref_points()) return(FALSE);

	lats = (double *)malloc((get_num_ref_points() + 1) * sizeof(double));
	longs = (double *)malloc((get_num_ref_points() + 1) * sizeof(double));
	xs = (double *)malloc((get_num_ref_points() + 1) * sizeof(double));
	ys = (double *)malloc((get_num_ref_points() + 1) * sizeof(double));
	lt = lats; ln = longs; lx = xs; ly = ys;
	for(i=0; i<get_num_ref_points(); i++) {
		get_reference_point(i, &tx, &ty, ++lt, ++ln);
		*(++lx) = (double)tx;
		*(++ly) = (double)ty;
	}

	last_mean_err_unit_wt = 1.0e55;

	for(i=1; i<poly_order; i++) {					// *****
		num_unks = num_terms_from_code(i);
		num_eqs = get_num_ref_points();
		weights = (double *)malloc((num_eqs+1)*sizeof(double));
		terms = (double *)malloc((num_eqs+1)*sizeof(double));
		results = (double *)malloc((num_unks+1)*sizeof(double));
		residuals = (double *)malloc((num_eqs+1)*sizeof(double));
		mean_err_unk = (double *)malloc((num_unks+1)*sizeof(double));

		// fill arrays starting at 1 for lsqp
		lx = ys;					// *****
		ln = longs;					// *****
		lt = lats;					// *****
		for(j=1; j<=num_eqs; j++) {
			weights[j] = 1.0;
			terms[j] = lx[j];
			if(num_unks > 0) {
				coeffs[j][0] = 1.0;
			}
			if(num_unks > 1) {
				coeffs[j][1] = ln[j];
			}
			if(num_unks > 2) {
				coeffs[j][2] = lt[j];
			}
			if(num_unks > 3) {
				coeffs[j][3] = ln[j]*ln[j];
			}
			if(num_unks > 4) {
				coeffs[j][4] = ln[j]*lt[j];
			}
			if(num_unks > 5) {
				coeffs[j][5] = lt[j]*lt[j];
			}
			if(num_unks > 6) {
				coeffs[j][6] = ln[j]*ln[j]*ln[j];
			}
			if(num_unks > 7) {
				coeffs[j][7] = ln[j]*ln[j]*lt[j];
			}
			if(num_unks > 8) {
				coeffs[j][8] = ln[j]*lt[j]*lt[j];
			}
			if(num_unks > 9) {
				coeffs[j][9] = lt[j]*lt[j]*lt[j];
			}
			if(num_unks > 10) {
				coeffs[j][10] = lt[j]*lt[j]*lt[j]*lt[j];
			}
			if(num_unks > 11) {
				coeffs[j][11] = lt[j]*lt[j]*lt[j]*lt[j]*lt[j];
			}
			coeffs[j][num_unks] = coeffs[j][0];
		}
		// least squares fit
		if(lsqp(num_eqs, num_unks, coeffs, terms, weights, results,
			residuals, &mean_err_unit_wt, mean_err_unk)) {
			// if better than best so far then copy to poly term holder
			if(mean_err_unit_wt < last_mean_err_unit_wt * 0.7) {
				clear_wpy_terms();					// *****
				results[0] = results[num_unks];
				for(j=0; j<num_unks; j++) {
					add_wpy_term(results[j]);		// *****
				}
				last_mean_err_unit_wt = mean_err_unit_wt;
			} else {
				break;
			}
		}
		// free arrays
		free(weights);
		free(terms);
		free(results);
		free(residuals);
		free(mean_err_unk);
	}

	free(lats);
	free(longs);
	free(xs);
	free(ys);
	return(TRUE);
}

int	KAPGeoData::gen_err_terms(void)
{
	int	i, ixpix, iypix;
	double	lat, lng, xpix, ypix;
	double	lat_err, lng_err, xpix_err, ypix_err;

	if(!get_num_ref_points()) return(FALSE);

	for(i=0; i<get_num_ref_points(); i++) {
		get_reference_point(i, &ixpix, &iypix, &lat, &lng);
		xpix = (double)ixpix;
		ypix = (double)iypix;
		lat_err = fabs(xytolat(xpix, ypix) - lat);
		lng_err = fabs(xytolong(xpix, ypix) - lng);
		set_latlong_error(i, lat_err, lng_err);
		xpix_err = fabs(lltox(lat, lng) - xpix);
		ypix_err = fabs(lltoy(lat, lng) - ypix);
		set_pixel_error(i, xpix_err, ypix_err);
	}
	return(TRUE);
}

char	*KAPGeoData::to_string(void)
{
	char	buff[218192];	// this should change to handle any size through realloc but I can't think of a good way
	char	lbuf[1024];
	int	i;
	int	temp_int, temp_int2;
	double	temp_double, temp_double2;
	KAPGeoData	*geodat;

	geodat = this;

		buff[0] = '\0';
		// output magic string to define data type
		strcat(buff,"KAPGeoData\n");
		// output VER record
		sprintf(lbuf,"VER/%d.%d\n", geodat->get_major(), geodat->get_minor());
		strcat(buff,lbuf);
		// output BSB record
		sprintf(lbuf,"BSB/");
		strcat(buff,lbuf);
		sprintf(lbuf,"NA=");
		strcat(buff,lbuf);
		sprintf(lbuf,"%s",geodat->get_kapname());
		strcat(buff,lbuf);
		sprintf(lbuf,",NU=%d",geodat->get_kapnumber());
		strcat(buff,lbuf);
		sprintf(lbuf,",RA=%d,%d",0,0);
		strcat(buff,lbuf);
		sprintf(lbuf,",DU=%d",geodat->get_draw_units());
		strcat(buff,lbuf);
		sprintf(lbuf,"\n");
		strcat(buff,lbuf);
		// output KNP record
		sprintf(lbuf,"KNP/");
		strcat(buff,lbuf);
		sprintf(lbuf,"SC=%d", geodat->get_scale());
		strcat(buff,lbuf);
		sprintf(lbuf,",GD=%s", geodat->datum_type_to_string(geodat->get_datum()));
		strcat(buff,lbuf);
		sprintf(lbuf,",PR=%s", geodat->projection_type_to_string(geodat->get_projection()));
		strcat(buff,lbuf);
		sprintf(lbuf,",PP=%f", geodat->get_projparm());
		strcat(buff,lbuf);
		sprintf(lbuf,",PI=%f", geodat->get_projint());
		strcat(buff,lbuf);
		sprintf(lbuf,",SP=");
		strcat(buff,lbuf);
		for(i=0; i<geodat->get_num_state_planes(); i++) {
			sprintf(lbuf,"%s,%f",geodat->get_state_plane(i),geodat->get_state_plane_interval(i));
		strcat(buff,lbuf);
			if((i+1) < geodat->get_num_state_planes()) sprintf(lbuf,",");
		strcat(buff,lbuf);
		}
		sprintf(lbuf,",SK=%f", geodat->get_skew_angle());
		strcat(buff,lbuf);
		sprintf(lbuf,",TA=%f", geodat->get_text_angle());
		strcat(buff,lbuf);
		sprintf(lbuf,",UN=%s", geodat->get_units());
		strcat(buff,lbuf);
		sprintf(lbuf,",SD=%s", geodat->get_sound_datum());
		strcat(buff,lbuf);
		sprintf(lbuf,",DX=%f", geodat->get_x_meters());
		strcat(buff,lbuf);
		sprintf(lbuf,",DY=%f", geodat->get_y_meters());
		strcat(buff,lbuf);
		sprintf(lbuf,"\n");
		strcat(buff,lbuf);
		// output CED record
		sprintf(lbuf,"CED/");
		strcat(buff,lbuf);
		sprintf(lbuf,"SE=%d", geodat->get_edition());
		strcat(buff,lbuf);
		sprintf(lbuf,",RE=%d", geodat->get_raster_ed());
		strcat(buff,lbuf);
		sprintf(lbuf,",ED=%s", geodat->get_ed_date());
		strcat(buff,lbuf);
		sprintf(lbuf,"\n");
		strcat(buff,lbuf);
		// output OST record
		sprintf(lbuf,"OST/");
		strcat(buff,lbuf);
		sprintf(lbuf,"%d", geodat->get_image_lines());
		strcat(buff,lbuf);
		sprintf(lbuf,"\n");
		strcat(buff,lbuf);
		// output IFM record
		sprintf(lbuf,"IFM/%d\n", 0);
		strcat(buff,lbuf);
		// output REF records
		for(i=0; i<geodat->get_num_ref_points(); i++) {
			geodat->get_reference_point(i, &temp_int, &temp_int2, &temp_double, &temp_double2);
			sprintf(lbuf,"REF/%d,%d,%d,%.10f,%.10f\n",i+1, temp_int, temp_int2, temp_double, temp_double2);
		strcat(buff,lbuf);
		}
		// output PLY records
		for(i=0; i<geodat->get_num_poly_vertices(); i++) {
			geodat->get_poly_vert(i, &temp_double, &temp_double2);
			sprintf(lbuf,"PLY/%d,%.10f,%.10f\n",i+1, temp_double, temp_double2);
		strcat(buff,lbuf);
		}
		// output DTM record
		sprintf(lbuf,"DTM/%.10f,%.10f\n", geodat->get_lat_shift(), geodat->get_long_shift());
		strcat(buff,lbuf);
		// output CPH record
		sprintf(lbuf,"CPH/%.10f\n", geodat->get_shift_value());
		strcat(buff,lbuf);
		// output Polynomial records
		sprintf(lbuf,"WPX/%d",geodat->num_terms_to_code(geodat->get_num_wpx_terms()));
		strcat(buff,lbuf);
		for(i=0; i<geodat->get_num_wpx_terms(); i++) {
			if(fabs(geodat->get_wpx_term(i)) < 0.001) {
				sprintf(lbuf,",%.12e", geodat->get_wpx_term(i));
		strcat(buff,lbuf);
			} else {
				sprintf(lbuf,",%.12f", geodat->get_wpx_term(i));
		strcat(buff,lbuf);
			}
		}
		sprintf(lbuf,"\n");
		strcat(buff,lbuf);
		sprintf(lbuf,"PWX/%d",geodat->num_terms_to_code(geodat->get_num_pwx_terms()));
		strcat(buff,lbuf);
		for(i=0; i<geodat->get_num_pwx_terms(); i++) {
			if(fabs(geodat->get_pwx_term(i)) < 0.001) {
				sprintf(lbuf,",%.12e", geodat->get_pwx_term(i));
		strcat(buff,lbuf);
			} else {
				sprintf(lbuf,",%.12f", geodat->get_pwx_term(i));
		strcat(buff,lbuf);
			}
		}
		sprintf(lbuf,"\n");
		strcat(buff,lbuf);
		sprintf(lbuf,"WPY/%d",geodat->num_terms_to_code(geodat->get_num_wpy_terms()));
		strcat(buff,lbuf);
		for(i=0; i<geodat->get_num_wpy_terms(); i++) {
			if(fabs(geodat->get_wpy_term(i)) < 0.001) {
				sprintf(lbuf,",%.12e", geodat->get_wpy_term(i));
		strcat(buff,lbuf);
			} else {
				sprintf(lbuf,",%.12f", geodat->get_wpy_term(i));
		strcat(buff,lbuf);
			}
		}
		sprintf(lbuf,"\n");
		strcat(buff,lbuf);
		sprintf(lbuf,"PWY/%d",geodat->num_terms_to_code(geodat->get_num_pwy_terms()));
		strcat(buff,lbuf);
		for(i=0; i<geodat->get_num_pwy_terms(); i++) {
			if(fabs(geodat->get_pwy_term(i)) < 0.001) {
				sprintf(lbuf,",%.12e", geodat->get_pwy_term(i));
		strcat(buff,lbuf);
			} else {
				sprintf(lbuf,",%.12f", geodat->get_pwy_term(i));
		strcat(buff,lbuf);
			}
		}
		sprintf(lbuf,"\n");
		strcat(buff,lbuf);
		// output ERR records
		for(i=0; i<geodat->get_num_ref_points(); i++) {
			geodat->get_pixel_error(i, &temp_double, &temp_double2);
			sprintf(lbuf,"ERR/%d,%.10f,%.10f",i+1, temp_double, temp_double2);
		strcat(buff,lbuf);
			geodat->get_latlong_error(i, &temp_double, &temp_double2);
			sprintf(lbuf,",%.10e,%.10e\n", temp_double, temp_double2);
		strcat(buff,lbuf);
		}

		sprintf(lbuf,"El_Scale = %g\nEl_Base = %g\n",geodat->get_elev_scale(), geodat->get_elev_offset());
		strcat(buff,lbuf);

		if(strlen(buff) >= 218192) {
			fprintf(stderr,"Whoops - Length of buff %d exceeds size %d\n", strlen(buff), 218192);
			fprintf(stderr,"  in converting KAPGeoData to string.\n");
			return(NULL);
		}

	return(strdup(buff));
}

int	KAPGeoData::from_string(char *buf)
{
	KAPGeoData	*geodat;
	char *temp_str = NULL;
	int	i;
	int	temp_int, temp_int2, temp_int3;
	double	temp_double, temp_double2;
	char	temp_string[1024];

	geodat = this;

		// check magic string in buffer
		if(strncmp(buf, "KAPGeoData", strlen("KAPGeoData"))) {
			return(FALSE);
		}
		
		// get georef data from header and put in GeoData object
		// get name, number, and units from BSB record
		if(temp_str = strstr(buf,"BSB/")) {
			if(temp_str = strstr(temp_str,"NA=")) {
				sscanf(temp_str,"NA=%[^,\n\r]",temp_string);
				geodat->set_kapname(temp_string);
			}
		} else {
			return(FALSE);
		}
		if(temp_str = strstr(buf,"BSB/")) {
			if(temp_str = strstr(temp_str,"NU=")) {
				sscanf(temp_str,"NU=%d",&temp_int);
				geodat->set_kapnumber(temp_int);
			}
		}
		if(temp_str = strstr(buf,"BSB/")) {
			if(temp_str = strstr(temp_str,"DU=")) {
				sscanf(temp_str,"DU=%d",&temp_int);
				geodat->set_draw_units(temp_int);
			}
		}
		// get stuff from KNP record
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"SC=")) {
				sscanf(temp_str,"SC=%d",&temp_int);
				geodat->set_scale((long)temp_int);
			}
		} else {
			return(FALSE);
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"GD=")) {
				sscanf(temp_str,"GD=%[^,\n\r]",temp_string);
				geodat->set_datum(geodat->datum_type_from_string(temp_string));
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"PR=")) {
				sscanf(temp_str,"PR=%[^,\n\r]",temp_string);
				geodat->set_projection(geodat->projection_type_from_string(temp_string));
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"PP=")) {
				sscanf(temp_str,"PP=%lf",&temp_double);
				geodat->set_projparm(temp_double);
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"PI=")) {
				sscanf(temp_str,"PI=%lf",&temp_double);
				geodat->set_projint(temp_double);
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"SP=")) {
				geodat->clear_state_planes();
				temp_string[0] = '\0';
				sscanf(temp_str,"SP=%[^,\n\r]",temp_string);
				while(strlen(temp_string) > 0 && !strstr(temp_string,"=")) {
					temp_str = strstr(temp_str,",") + 1;
					sscanf(temp_str,"%lf", &temp_double);
					geodat->add_state_plane(temp_string, temp_double);
					temp_str = strstr(temp_str,",") + 1;
					sscanf(temp_str,"%[^,\n\r]",temp_string);
				}
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"SK=")) {
				sscanf(temp_str,"SK=%lf",&temp_double);
				geodat->set_skew_angle(temp_double);
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"TA=")) {
				sscanf(temp_str,"TA=%lf",&temp_double);
				geodat->set_text_angle(temp_double);
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"UN=")) {
				sscanf(temp_str,"UN=%[^,\n\r]",temp_string);
				geodat->set_units(temp_string);
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"SD=")) {
				sscanf(temp_str,"SD=%[^,\n\r]",temp_string);
				geodat->set_sound_datum(temp_string);
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"DX=")) {
				sscanf(temp_str,"DX=%lf",&temp_double);
				geodat->set_x_meters(temp_double);
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"DY=")) {
				sscanf(temp_str,"DY=%lf",&temp_double);
				geodat->set_y_meters(temp_double);
			}
		}

		// get stuff from CED record
		if(temp_str = strstr(buf,"CED/")) {
			if(temp_str = strstr(temp_str,"SE=")) {
				sscanf(temp_str,"SE=%d",&temp_int);
				geodat->set_edition(temp_int);
			}
		} else {
			return(FALSE);
		}
		if(temp_str = strstr(buf,"CED/")) {
			if(temp_str = strstr(temp_str,"RE=")) {
				sscanf(temp_str,"RE=%d",&temp_int);
				geodat->set_raster_ed(temp_int);
			}
		}
		if(temp_str = strstr(buf,"CED/")) {
			if(temp_str = strstr(temp_str,"ED=")) {
				// added space to sep string as append_token usage
				sscanf(temp_str,"ED=%[^,\n\r ]",temp_string);
				geodat->set_ed_date(temp_string);
			}
		}

		// get stuff from VER record
		if(temp_str = strstr(buf,"VER/")) {
			sscanf(temp_str,"VER/%d.%d",&temp_int, &temp_int2);
			geodat->set_major(temp_int);
			geodat->set_minor(temp_int2);
		} else {
			return(FALSE);
		}
		
		// get stuff from OST record
		if(temp_str = strstr(buf,"OST/")) {
			sscanf(temp_str,"OST/%d",&temp_int);
			geodat->set_image_lines(temp_int);
		} else {
			return(FALSE);
		}
		
		// get stuff from IFM record
		// Unnecessary because already took care of it
		
		// get stuff from RGB record
		// Unnecessary because already took care of it
		
		// get stuff from REF records
		temp_str = strstr(buf,"REF/");
		geodat->clear_reference_points();
		while(temp_str) {
			sscanf(temp_str,"REF/%d,%d,%d,%lf,%lf",
				&temp_int, &temp_int2, &temp_int3, &temp_double, &temp_double2);
			while(temp_int > geodat->get_num_ref_points()) {
				geodat->add_reference_point(temp_int2, temp_int3, temp_double, temp_double2);
			}
			if(temp_int < geodat->get_num_ref_points()) {
				geodat->set_reference_point(temp_int, temp_int2, temp_int3, temp_double, temp_double2);
			}
			temp_str = strstr(temp_str+4,"REF/");
		}
		
		// get stuff from PLY records
		temp_str = strstr(buf,"PLY/");
		geodat->clear_poly_verts();
		while(temp_str) {
			sscanf(temp_str,"PLY/%d,%lf,%lf",
				&temp_int, &temp_double, &temp_double2);
			geodat->add_poly_vert(temp_double, temp_double2);
			temp_str = strstr(temp_str+4,"PLY/");
		}
		
		// get stuff from DTM record
		if(temp_str = strstr(buf,"DTM/")) {
			sscanf(temp_str,"DTM/%lf,%lf",&temp_double, &temp_double2);
			geodat->set_lat_shift(temp_double);
			geodat->set_long_shift(temp_double2);
		} else {
			return(FALSE);
		}
		
		// get stuff from CPH record
		if(temp_str = strstr(buf,"CPH/")) {
			sscanf(temp_str,"CPH/%lf",&temp_double);
			geodat->set_shift_value(temp_double);
		} else {
			return(FALSE);
		}
		

		// get stuff from WPX record
		if(temp_str = strstr(buf,"WPX/")) {
			geodat->clear_wpx_terms();
			sscanf(temp_str,"WPX/%d%*[ ,\t\n\r]%n",&temp_int, &temp_int2);
			for(i=0; i<geodat->num_terms_from_code(temp_int); i++) {
				temp_str += temp_int2;
				sscanf(temp_str,"%lf%*[ ,\t\n\r]%n", &temp_double, &temp_int2);
				geodat->add_wpx_term(temp_double);
			}
		} else {
			return(FALSE);
		}
		
		// get stuff from PWX record
		if(temp_str = strstr(buf,"PWX/")) {
			geodat->clear_pwx_terms();
			sscanf(temp_str,"PWX/%d%*[ ,\t\n\r]%n",&temp_int, &temp_int2);
			for(i=0; i<geodat->num_terms_from_code(temp_int); i++) {
				temp_str += temp_int2;
				sscanf(temp_str,"%lf%*[ ,\t\n\r]%n", &temp_double, &temp_int2);
				geodat->add_pwx_term(temp_double);
			}
		} else {
			return(FALSE);
		}
		
		// get stuff from WPY record
		if(temp_str = strstr(buf,"WPY/")) {
			geodat->clear_wpy_terms();
			sscanf(temp_str,"WPY/%d%*[ ,\t\n\r]%n",&temp_int, &temp_int2);
			for(i=0; i<geodat->num_terms_from_code(temp_int); i++) {
				temp_str += temp_int2;
				sscanf(temp_str,"%lf%*[ ,\t\n\r]%n", &temp_double, &temp_int2);
				geodat->add_wpy_term(temp_double);
			}
		} else {
			return(FALSE);
		}
		
		// get stuff from PWY record
		if(temp_str = strstr(buf,"PWY/")) {
			geodat->clear_pwy_terms();
			sscanf(temp_str,"PWY/%d%*[ ,\t\n\r]%n",&temp_int, &temp_int2);
			for(i=0; i<geodat->num_terms_from_code(temp_int); i++) {
				temp_str += temp_int2;
				sscanf(temp_str,"%lf%*[ ,\t\n\r]%n", &temp_double, &temp_int2);
				geodat->add_pwy_term(temp_double);
			}
		} else {
			return(FALSE);
		}
		
		// get stuff from ERR records
		temp_str = strstr(buf,"ERR/");
                if(!temp_str && geodat->get_num_ref_points()) {
                        geodat->gen_err_terms();
                }
		while(temp_str) {
			sscanf(temp_str,"ERR/%d,%lf,%lf,%n", &temp_int, &temp_double, &temp_double2, &temp_int2);
			geodat->set_pixel_error(temp_int-1, temp_double, temp_double2);
			temp_str += temp_int2;
			sscanf(temp_str,"%lf,%lf", &temp_double, &temp_double2);
			geodat->set_latlong_error(temp_int-1, temp_double, temp_double2);
			temp_str = strstr(temp_str,"ERR/");
		}

		if(temp_str = strstr(buf,"El_Scale")) {
			sscanf(temp_str,"El_Scale = %lf", &temp_double);
			geodat->set_elev_scale(temp_double);
		} else {
			return(FALSE);
		}

		if(temp_str = strstr(buf,"El_Base")) {
			sscanf(temp_str,"El_Base = %lf", &temp_double);
			geodat->set_elev_offset(temp_double);
		} else {
			return(FALSE);
		}

		// check for missing polynomials (no pwy, wpy, pwx, wpx, or err terms)
		// if missing generate them
		if(!geodat->get_num_wpx_terms()) {
			geodat->gen_wpx_terms();
		}
		if(!geodat->get_num_pwx_terms()) {
			geodat->gen_pwx_terms();
		}
		if(!geodat->get_num_wpy_terms()) {
			geodat->gen_wpy_terms();
		}
		if(!geodat->get_num_pwy_terms()) {
			geodat->gen_pwy_terms();
		}

	return(TRUE);
}

