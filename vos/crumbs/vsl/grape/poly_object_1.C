#include <iostream>
#include <fstream>
#include <math.h>
#include "dataport.h"
#include "grape/object_types.h"
#include "grape/poly_object_1.h"

using namespace std;
#define HI_SHPERE	1	

static void xyz_to_spherical(double x, double y, double z, double *lat, 
					double *lon, double *radius);
#define TOLERANCE 1E-6
static const double delta = TOLERANCE;

char	*poly_object_type_master_list[POLY_NUM_OBJ_TYPES] =
	{
		 "wireframe" ,			// this must match the sequence for the
								// PolyObjectType!
		 "smooth_shaded" ,
		 "texture_mapped" ,
		 "flat_shaded"
	};

PolyObjectType PolyObject1::poly_object_from_string(char *type)
{
	int	i;

	for(i=0; i<POLY_NUM_OBJ_TYPES; i++) {
		if(!strcmp(poly_object_type_master_list[i], type)) {
			return( (PolyObjectType)i);
		}
	}
	return( BadPolyObj);
}


int PolyObject1::get_changed()
{
register int	c;

if((c=Obj::get_changed()) > changed) changed=c;
if(changed==CHANGE_COUNTER) return changed;

if((c=r.get_changed()) > changed) changed=c;
if((c=g.get_changed()) > changed) changed=c;
if((c=b.get_changed()) > changed) changed=c;
if((c=x_vert.get_changed()) > changed) changed=c;
if((c=y_vert.get_changed()) > changed) changed=c;
if((c=z_vert.get_changed()) > changed) changed=c;
if((c=x_norm.get_changed()) > changed) changed=c;
if((c=y_norm.get_changed()) > changed) changed=c;
if((c=z_norm.get_changed()) > changed) changed=c;
if((c=point_colors.get_changed()) > changed) changed=c;
if((c=point_normals.get_changed()) > changed) changed=c;
if((c=vrtx_poly_users.get_changed()) > changed) changed = c;
if((c=edge_start.get_changed()) > changed) changed=c;
if((c=edge_end.get_changed()) > changed) changed=c;
if((c=edge_colors.get_changed()) > changed) changed=c;
if((c=poly_verts.get_changed()) > changed) changed=c;
if((c=poly_colors.get_changed()) > changed) changed=c;
if((c=poly_normals.get_changed()) > changed) changed=c;

return changed;
}

void PolyObject1::init(void)
{
	if(version)free(version);
	if(file_type)free(file_type);
	if(group_name)free(group_name);
	if(pTextureName)free(pTextureName);
}


int PolyObject1::parse_in(Dataport *fp)
{
	char	token[4096];
	int	i, j, temp, curr_color;

	if(!Obj::parse_in(fp)) {
		return(FALSE);
	}

	do {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
			return(FALSE);
		}
		if(!strcmp(token, "VERSION")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
				return(FALSE);
			}
			if(strcmp(token, "1.0")) {
				fprintf(stderr," Whoops - Unexpected version number encountered in parsing polygon object\n");
				fprintf(stderr,"   Token=>%s<  Expecting >1.0<\n", token);
				return(FALSE);
			}
			version = strdup(token);
		} else if(!strcmp(token, "FILE_TYPE")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
				return(FALSE);
			}
			if(file_type)free(file_type);
			file_type = strdup(token);
	
		} else if(!strcmp(token, "OBJECT_TYPE")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
				return(FALSE);
			}
			object_type = poly_object_from_string(token);
			if(object_type == BadPolyObj) {
				fprintf(stderr," Whoops - Unknown object type %s\n", token);
				return(FALSE);
			}
								// this next stuff is a hack which must go away!
#if 0
			else if (object_type == TextureMapped) {

								// get the texture file name
				if(!get_next_token(fp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
					return(FALSE);
				}
				set_texture_name( token);
			}
#endif
		} else if(!strcmp(token, "GROUP_NAME")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
				return(FALSE);
			}
			if(group_name)free(group_name);
			group_name = strdup(token);
		} else if(!strcmp(token, "COLORS")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
				return(FALSE);
			}
			temp = atoi(token);	// number of colors in object
			r.set_num(temp);
			g.set_num(temp);
			b.set_num(temp);
			for(i=0; i<temp; i++) {	// for each color, get the rgb values
				if(!get_next_token(fp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
					return(FALSE);
				}
				r[i] = atof(token);
				if(!get_next_token(fp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
					return(FALSE);
				}
				g[i] = atof(token);
				if(!get_next_token(fp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
					return(FALSE);
				}
				b[i] = atof(token);
			}
				
		} else if(!strcmp(token, "POINTS")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
				return(FALSE);
			}
			temp = atoi(token); // there are this many points
			x_vert.set_num(temp);
			y_vert.set_num(temp);
			z_vert.set_num(temp);
			point_colors.set_num(temp);	// same num in list of color indeces
#if 0
			vrtx_poly_users.set_num(temp); // same num of shared vertices
			point_normals.set_num(temp); // same num of normals
#endif
			curr_color = 0;
			i = 0;
			while(i<temp) { // for each point
				if(!get_next_token(fp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
					return(FALSE);
				}
				if(!strcmp(token, "COLOR")) {
					if(!get_next_token(fp, token)) {
						fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
						return(FALSE);
					}
					curr_color = atoi(token);
				} else {

								// get x,y,z triplet

					x_vert[i] = atof(token);
					if(!get_next_token(fp, token)) {
						fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
						return(FALSE);
					}
					y_vert[i] = atof(token);
					if(!get_next_token(fp, token)) {
						fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
						return(FALSE);
					}
					z_vert[i] = atof(token);
					point_colors[i] = curr_color; // the color index
#if 0
					point_normals[i] = -1;		  // mark as undefined
#endif
					i++;
				}
			}
				
		} else if(!strcmp(token, "EDGES")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
				return(FALSE);
			}
			temp = atoi(token);	// number of edges
			edge_start.set_num(temp);
			edge_end.set_num(temp);
			edge_colors.set_num(temp);
			curr_color = 0;
			i = 0;
			while(i<temp) {
				if(!get_next_token(fp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
					return(FALSE);
				}
				if(!strcmp(token, "COLOR")) {
					if(!get_next_token(fp, token)) {
						fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
						return(FALSE);
					}
					curr_color = atoi(token); // index into color list
				} else {
					edge_start[i] = atoi(token); // index into vertex list
					if(!get_next_token(fp, token)) {
						fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
						return(FALSE);
					}
					edge_end[i] = atoi(token); // index into vertex list
					edge_colors[i] = curr_color;
					i++;
				}
			}
				
		} else if(!strcmp(token, "FACES")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
				return(FALSE);
			}
			temp = atoi(token);	// number of faces (i.e. polygons)
			poly_colors.set_num(temp); // same as num faces
			poly_verts.set_num(temp); // same as num faces

#if 0			
					  // moved to generate() phase
			poly_normals.set_num(temp);	// same as num faces
#endif

			curr_color = 0;
			i = 0;
			while(i<temp) {		// for each face
				if(!get_next_token(fp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
					return(FALSE);
				}
				if(!strcmp(token, "COLOR")) {
					if(!get_next_token(fp, token)) {
						fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
						return(FALSE);
					}
					curr_color = atoi(token); // index into color list
				} else {
								// for the i-th polygon

					poly_verts.set_size(i, atoi(token)); // num vertices in poly

					for(j=0; j<poly_verts.get_size(i); j++) {
						if(!get_next_token(fp, token)) {
							fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
							return(FALSE);
						}

						int vtx_ndx = atoi(token);

								// set index of j-th vertex in i-th polygon
						poly_verts.set_value(i, j, vtx_ndx);

#if 0
								// add this poly to this vertex's list
						if ( vtx_ndx >= vrtx_poly_users.get_num()) {
								// make sure that there's room for an entry
								// for the vtx_ndx-th vertex
							vrtx_poly_users.set_num( vtx_ndx + 1);
						}
								// add poly index i to vtx_ndx's list
						vrtx_poly_users.add_value( vtx_ndx, i);
						k = vrtx_poly_users.get_size( vtx_ndx);
#endif
						
					}
					poly_colors[i] = curr_color; // index into color list
#if 0					
			  // moved to generate() phase
					poly_normals[i] = -1;		 // mark as undefined
#endif
					i++;
				}
			}
		} else if(strcmp(token, "THAT'S_ALL_FOLKS")) {
			fprintf(stderr," Whoops - Unexpected token encountered in parsing polygon object\n");
			fprintf(stderr,"   Token=>%s<  Expecting one of COLORS POINTS EDGES FACES THAT'S_ALL_FOLKS\n", token);
			return(FALSE);
		}
	} while(strcmp(token, "THAT'S_ALL_FOLKS"));

	return(TRUE);
	
}

int PolyObject1::parse_out(Dataport *fp, int )
{
	char	token[4096];
	int	i, j, curr_color;

	put_token(fp, "POLY_V1");

	Obj::parse_out(fp);

	put_token(fp, "\nVERSION");
	put_token(fp, "1.0");
	if(file_type) {
		put_token(fp, "\nFILE_TYPE");
		put_token(fp, file_type);
	}
	put_token(fp, "\nOBJECT_TYPE");
	put_token(fp, poly_object_type_master_list[object_type]);
	if(group_name) {
		put_token(fp, "\nGROUP_NAME");
		put_token(fp, group_name);
	}

	put_token(fp, "\n\nCOLORS");
	sprintf(token, "%d", r.get_num());
	put_token(fp, token);
	for(i=0; i<r.get_num(); i++) {
		sprintf(token, "\n%f",(double)r[i]);
		put_token(fp, token);
		sprintf(token, "%f", (double)g[i]);
		put_token(fp, token);
		sprintf(token, "%f", (double)b[i]);
		put_token(fp, token);
	}

	put_token(fp, "\n\nPOINTS");
	sprintf(token, "%d", x_vert.get_num());
	put_token(fp, token);
	curr_color = -1;
	for(i=0; i<x_vert.get_num(); i++) {
		if((int)point_colors[i] != curr_color) {
			put_token(fp, "\nCOLOR");
			sprintf(token, "%d", (int)point_colors[i]);
			put_token(fp, token);
			curr_color = point_colors[i];
		}
		sprintf(token, "\n%f",(double)x_vert[i]);
		put_token(fp, token);
		sprintf(token, "%f", (double)y_vert[i]);
		put_token(fp, token);
		sprintf(token, "%f", (double)z_vert[i]);
		put_token(fp, token);
	}

	put_token(fp, "\n\nEDGES");
	sprintf(token, "%d", edge_start.get_num());
	put_token(fp, token);
	curr_color = -1;
	for(i=0; i<edge_start.get_num(); i++) {
		if((int)edge_colors[i] != curr_color) {
			put_token(fp, "\nCOLOR");
			sprintf(token, "%d", (int)edge_colors[i]);
			put_token(fp, token);
			curr_color = edge_colors[i];
		}
		sprintf(token, "\n%d",(int)edge_start[i]);
		put_token(fp, token);
		sprintf(token, "%d", (int)edge_end[i]);
		put_token(fp, token);
	}

	put_token(fp, "\n\nFACES");
	sprintf(token, "%d", poly_colors.get_num());
	put_token(fp, token);
	for(i=0; i<poly_colors.get_num(); i++) {
		if((int)poly_colors[i] != curr_color) {
			put_token(fp, "\nCOLOR");
			sprintf(token, "%d", (int)poly_colors[i]);
			put_token(fp, token);
			curr_color = poly_colors[i];
		}
		sprintf(token,"\n%d", poly_verts.get_size(i));
		put_token(fp, token);
		for(j=0; j<poly_verts.get_size(i); j++) {
			sprintf(token," %d", (int)poly_verts.get_value(i,j));
			put_token(fp, token);
		}
	}

	put_token(fp, "\nTHAT'S_ALL_FOLKS\n");

	return(TRUE);
}

void 	PolyObject1::dump( void)
{
	FILE_Dataport *odpt;
	int i, j, size;
	char token[4096];
	double ni, nj, nk;

    odpt = new FILE_Dataport();
	odpt->open(stderr);
	odpt->put_string( " ***** begin object *******\n");

	parse_out( odpt, 0);


	odpt->put_string( "*** begin raw normal data ***\n");
	for (i = 0; i < x_norm.get_num(); i++) {
		sprintf(token, "%d: %9.4f %9.4f %9.4f\n",
				i,
				x_norm.get_value( i),
				y_norm.get_value( i),
				z_norm.get_value( i));
		odpt->put_string( token);
	}
	odpt->put_string( "*** end raw normal data ***\n");

	odpt->put_string( "*** begin poly normals ***\n");
	for (i = 0; i < get_num_faces(); i++) {
		j = get_poly_normal( i); // index of normal
		if (j == -1)
			continue;
		sprintf(token, "%d: %9.4f %9.4f %9.4f\n",
				i,
				x_norm.get_value( j),
				y_norm.get_value( j),
				z_norm.get_value( j));
		odpt->put_string( token);
	}
	odpt->put_string( "*** end poly normals ***\n");

	odpt->put_string( "*** begin shared vertex list ***\n");
//	for (i = 0; i < get_num_points(); i++) {
	for (i = 0; i < vrtx_poly_users.get_num(); i++) {
		sprintf( token, "size=%d: ", vrtx_poly_users.get_size( i));
		odpt->put_string( token);
		sprintf( token, "%d: [", i);
		odpt->put_string( token);
		strcpy( token, "");
		size = vrtx_poly_users.get_size( i);
		for (j = 0; j < size; j++) {

			sprintf( token, " %d", (int)vrtx_poly_users( i, j));
			odpt->put_string( token);
		}
		sprintf( token, "]\n");
		odpt->put_string( token);
	}
	odpt->put_string( "*** end shared vertex list ***\n");

	odpt->put_string( "*** begin vertex normal list ***\n");
//	for (i = 0; i < get_num_points(); i++) {
	for (i = 0; i < point_normals.get_num(); i++) {
		int ndx;

		ndx = get_pt_normal(i);
		if (ndx == -1)
			continue;
		get_normal( ndx, ni, nj, nk);
		sprintf( token, "%d(%d): %f %f %f (%f %f %f)\n", 
				i, ndx, 
				(double)x_norm[ndx],
				(double)y_norm[ndx],
				(double)z_norm[ndx],
				ni, nj, nk);
				
		odpt->put_string( token);
	}
	odpt->put_string( "*** end vertex normal list ***\n");


	odpt->put_string( " ***** end object *******\n");

}

// Data access member functions
int		PolyObject1::get_num_points(void)
{
	return(x_vert.get_num());
}
int		PolyObject1::get_point(int i, int &c, double &x, double &y, double &z)
{
	if(i >= 0 && i < get_num_points()) {
		c = point_colors.get_value(i);
		x = x_vert.get_value(i);
		y = y_vert.get_value(i);
		z = z_vert.get_value(i);
		return(TRUE);
	} else {
		return(FALSE);
	}
}
int		PolyObject1::get_point(int i, int &c, float &x, float &y, float &z)
{
	if(i >= 0 && i < get_num_points()) {
		c = point_colors.get_value(i);
		x = (float)x_vert.get_value(i);
		y = (float)y_vert.get_value(i);
		z = (float)z_vert.get_value(i);
		return(TRUE);
	} else {
		return(FALSE);
	}
}
void PolyObject1::set_point(int i, int c, double x, double y, double z)
{
	if(i >= 0 && i < get_num_points()) {
		point_colors.set_value(i, c);
		x_vert.set_value(i, x);
		y_vert.set_value(i, y);
		z_vert.set_value(i, z);
	}
}

int		PolyObject1::get_num_edges(void)
{
	return(edge_start.get_num());
}
int		PolyObject1::get_edge(int i, int &c, int &s, int &e)
{
	if(i >= 0 && i < get_num_edges()) {
		c = edge_colors.get_value(i);
		s = edge_start.get_value(i);
		e = edge_end.get_value(i);
		return(TRUE);
	} else {
		return(FALSE);
	}
}
int		PolyObject1::get_num_colors(void)
{
	return(r.get_num());
}
int		PolyObject1::get_color(int i, double &rp, double &gp, double &bp)
{
	if(i >= 0 && i < get_num_colors()) {
		rp = r.get_value(i);
		gp = g.get_value(i);
		bp = b.get_value(i);
		return(TRUE);
	} else {
		return(FALSE);
	}
}
int		PolyObject1::get_color(int i, float &rp, float &gp, float &bp)
{
	if(i >= 0 && i < get_num_colors()) {
		rp = (float)r.get_value(i);
		gp = (float)g.get_value(i);
		bp = (float)b.get_value(i);
		return(TRUE);
	} else {
		return(FALSE);
	}
}

int		PolyObject1::get_num_faces(void)
{
	return(poly_verts.get_num());
}
int		PolyObject1::get_pcolor(int i)
{
	if(i >= 0 && i < get_num_faces()) {
		return(poly_colors.get_value(i));
	} else {
		return(-1);
	}
}
int		PolyObject1::get_num_normal_vects(void)
{
								// number of distinct normal vectors
	return x_norm.get_num();

}
int		PolyObject1::get_normal( int i, double &ni, double &nj, double &nk)
{
								// get the i-th normal vector in the list of
								// normal vectors

	if(i >= 0 && i < get_num_normal_vects()) {
		ni = x_norm.get_value(i);
		nj = y_norm.get_value(i);
		nk = z_norm.get_value(i);
#if 0
		(double)x_norm[i],		// optional method
		(double)y_norm[i],
		(double)z_norm[i],
#endif
		return(TRUE);
	} else {
		return(FALSE);
	}
}
int		PolyObject1::get_num_verts(int i)
{
	if(i >= 0 && i < get_num_faces()) {
		return(poly_verts.get_size(i));
	} else {
		return(0);
	}
}
int		PolyObject1::get_vert(int poly_ndx, int vert_ndx)
{
	if(poly_ndx >= 0 && poly_ndx < get_num_faces()) {

								// valid polygon index

		if(vert_ndx >= 0 && vert_ndx < get_num_verts(poly_ndx)) {

								// valid vertex index for this poly

								// return index into list of vertex coords

			return(poly_verts.get_value(poly_ndx, vert_ndx));
		}
	}
	return(-1);
}

int		PolyObject1::get_pt_normal(int pt_ndx)
{
//	if(pt_ndx >= 0 && pt_ndx < x_norm.get_num()) {
	if(pt_ndx >= 0 && pt_ndx < point_normals.get_num()) {

								// get index of the normal corresponding to the
								// pt_ndx-th vertex

		return point_normals.get_value( pt_ndx);
	} else {
		return( -1);			// error
	}
}

int		PolyObject1::get_poly_normal(int poly_ndx)
{
	if(poly_ndx >= 0 && poly_ndx < poly_normals.get_num()) {

								// valid polygon index

			return(poly_normals.get_value(poly_ndx));

	}
	return(-1);
}

int eq_tol( double &d0, double &d1, const double &tol)
{
	if ( fabs( d1-d0) < fabs(tol))
		return 1;
	else
		return 0;
}

int z_tol( double &d0, const double &tol)
{
	double z = 0.0;

	return eq_tol( d0, z, tol);
}


double dot3( 	double i1,
			 	double j1,
			 	double k1,
			 	double i2,
			 	double j2,
			 	double k2
			)
{
	return ( i1*i2 + j1*j2 + k1*k2);
}

void cross3(    double i1,
			  double j1,
			  double k1,
			  double i2,
			  double j2,
			  double k2,
			  double &i,
			  double &j,
			  double &k
			  )
{

	i = j1*k2 - j2*k1;
	j = -(i1*k2 - i2*k1);
	k = i1*j2 - i2*j1;
}

double norm_vect( double dx, double dy, double dz,
				 double &i,
				 double &j,
				 double &k
				 )
{
	double d;

	d = sqrt( dx*dx + dy*dy + dz*dz);
	if (!z_tol( d, delta)) {
		i = dx/d;
		j = dy/d;
		k = dz/d;
	}
	else {
		i = j = k = 0.0;		// error!
	}

	return d;
}

double get_uvect(
				double x1,
			 	double y1,
			 	double z1,
			 	double x2,
			 	double y2,
			 	double z2,
				double &i,
				double &j,
				double &k
			)

			//
			// get unit vector from (x1,y1,z1) to (x2,y2,z2) and pass back
			// in (i,j,k)
			//
			// returns pre-normalized length: if zero then (i,j,k) invalid
			//
{
	double dx, dy, dz;

	dx = x2-x1;
	dy = y2-y1;
	dz = z2-z1;
	return norm_vect( dx, dy, dz, i, j, k);
}

int colinear( 	double i1,
			 	double j1,
			 	double k1,
			 	double i2,
			 	double j2,
			 	double k2
			 )
{
	double cosa, one = 1.0;

	cosa = dot3( i1, j1, k1, i2, j2, k2);
	if (eq_tol(cosa, one, delta))
		return 1;
	else
		return 0;
}

int coplanar(	double a,		// coefficients of plane
			 	double b,
			 	double c,
			 	double d,
			 	double x,		// point in question: is it in plane?
			 	double y,
			 	double z)
{
	double dist;

								// plane: ax + by + cz + d = 0
								// dist: d = -(ax + by + cz)
	dist = -(a*x + b*y + c*z);
	if (eq_tol(dist,d,delta)) {
		return 1;				// (x,y,x) is in plane defined by a,b,c,d
	}
	else {
		return 0;				// not in plane!
	}
		
}

int get_plane( double i, double j, double k, // normal to plane
			  double x, double y, double z,	// point on plane
			  double &a, double &b, double &c, double &d) // plane's params
{

	a = i; b = j; c = k;
	d = -(a*x + b*y + c*z);

	return 1;					// meaningless for now...
}

int plane_equal( double a0, double b0, double c0, double d0,
				double a1, double b1, double c1, double d1)
{
	if (eq_tol(a0,a1,delta) ||
		eq_tol(b0,b1,delta) ||
		eq_tol(c0,c1,delta) ||
		eq_tol(d0,d1,delta))
		return 0;				// different planes
	else
		return 1;				// same planes
}
#ifdef FUNCT_HDR

/* ************************************************************************* 

Function Name:           build_normal_structure
                         
Purpose:                 Allocate space for the list of polygon normal
                         indeces, the list of point normal indeces,
                         and the list of lists of polygons that use
                         each vertex.
                         
                         This should generally be called 1x, before
                         actually calculating the normals of an
                         object.
                         
Sample Call:             obj->build_normal_structure();
                         
Input Parameters:        none

Output Parameters:       none

Return Value:            currently always 0 (and meaningless)
                         
Externals:               none

Input Data:              none

Output Data:             none

Calling Functions:       ???
                         
Functions Called:        get_num_faces(),
                         liparam::set_num()
                         get_num_points()
                         lliparam::set_num()
                         liparam::get_size()
                         liparam::get_value()
                         lliparam::add_value()
                         
Description:             Call this before calling generate_normals() !!!!!

****************************************************************************/ 
#endif


int PolyObject1::build_normal_structure(void)
{
	int nFaces, nVerts, vrtx, poly, vtx_ndx;

								// get rid of any existing normal data
	x_norm.free_list();
	y_norm.free_list();
	z_norm.free_list();
	vrtx_poly_users.free_all();
	point_normals.free_list();
	poly_normals.free_list();
	
	nFaces = get_num_faces();	// total num faces (polygons) in object

	poly_normals.set_num(nFaces);	// same as num faces

    nVerts = get_num_points();  // total number of vertices in object
	
	vrtx_poly_users.set_num( nVerts); // same num of shared vertices

	point_normals.set_num( nVerts); // same num of normals

								// init the point normal list of indeces
	for (vtx_ndx = 0; vtx_ndx < nVerts; vtx_ndx++)
		point_normals[ vtx_ndx ] = -1;        // init each point normal index

								// for each polygon
	for (poly = 0; poly < nFaces; poly++) {

		poly_normals[ poly ] = -1;        // init each poly normal index

								// for each vertex in each polygon
		for (vrtx = 0; vrtx < poly_verts.get_size( poly); vrtx++) {

								// get index into raw data
			vtx_ndx = poly_verts.get_value( poly, vrtx);

								// vertex 'vtx_ndx' is used by 'poly'
			vrtx_poly_users.add_value( vtx_ndx, poly);
			
		}
	}

	return 0;
}

#ifdef FUNCT_HDR

/* ************************************************************************* 

Function Name:           generate_poly_normals
                         
Purpose:                 generate the normal vectors to all polygon
                         faces
                         
Sample Call:             obj->generate_poly_normals();
                         
Input Parameters:        none

Output Parameters:       none

Return Value:            0 if failure, 1 if success
                         
Externals:               none

Input Data:              none

Output Data:             poly_normals - the list of indeces of polygon
                         normals
                         x_norm, y_norm, z_norm - the lists of normal
                         vectors
                         
Calling Functions:       optional, according to needs of PolyObject1
                         users
                         
Functions Called:        get_num_faces()
                         get_num_verts()
                         get_vert()
                         get_point()
                         get_uvect
                         colinear()
                         cross3()
                         norm_vect()
                         get_num_normal_vects()
                         get_plane()
                         plane_equal()
                         
Description:             This checks to ensure that all points are
                         coplanar
                         
                         It checks that the normal is created with
                         non-parallel vectors (i.e. that colinear
                         points are not used, which would create an
                         unpredictable normal)
                         
****************************************************************************/ 
#endif

int PolyObject1::generate_poly_normals( void)
{
	int		nFaces,
			nVert,
			poly, vrtx,
			dummy, bGotPlane,
			nNormals = 0,			// num normals obtained
			v0, v1;
	double  i0, j0, k0, i1, j1, k1,
			x0, y0, z0, x1, y1, z1,
			ni, nj, nk,
			pa0, pb0, pc0, pd0;		// 1st plane's params
		//
		// make the normals for each face
		//
	
	nFaces = get_num_faces();	// total num faces (polygons) in object

	for (poly = 0; poly < nFaces; poly++) {

								// for each polygon

		nVert = get_num_verts( poly);
		if (nVert < 3) {
			fprintf( stderr,
			  "Found polygon %d with %d vertices; normals won't be right\n",
					poly, nVert);
			continue;
		}

		v0 = get_vert( poly, 0); // first vertex index
		get_point( v0, dummy, x0, y0, z0); // get coords

		bGotPlane = 0;		// reset for each poly

		for (vrtx = 1; vrtx < nVert; vrtx++) {

								// for each vertex in polygon

			v1 = get_vert( poly, vrtx);	// 2nd vertex
			get_point( v1, dummy, x1, y1, z1);
			
			if (get_uvect( x0, y0, z0, x1, y1, z1, i1, j1, k1) == 0.0) {
				fprintf( stderr,
						"zero length unit vector: poly %d, vrtx %d to %d\n",
						poly, vrtx-1, vrtx);
				continue;
			}
			
			if (vrtx > 1) {		// got 2 vectors so far

				if (!colinear( i0, j0, k0, i1, j1, k1)) {
					int nVectors;
					double pa1, pb1, pc1, pd1; // this plane
					double unit_ni, unit_nj, unit_nk;


					cross3( i0, j0, k0, i1, j1, k1, ni, nj, nk); // normal
					norm_vect( ni, nj, nk,						 // normalize
							  unit_ni, unit_nj, unit_nk);

								// add vector to list of vectors
					nVectors = get_num_normal_vects();
					poly_normals[ poly ] = nVectors;
					x_norm.add_value( unit_ni);
					y_norm.add_value( unit_nj);
					z_norm.add_value( unit_nk);
					
					nNormals++; // this poly's normal was obtained ok

								// get plane that contains both vector
								// (vrtx-2, vrtx-1) as well as vector
								// (vrtx-1, vrtx)
					get_plane( unit_ni, unit_nj, unit_nk,
							  x1, y1, z1,
							  pa1, pb1, pc1, pd1);

					if (!bGotPlane) {
								// store as the 1st plane obtainable from poly
						pa0 = pa1; pb0 = pb1; pc0 = pc1; pd0 = pd1;
						bGotPlane = 1; // get 1x only
					}

								// compare 1st plane obatined with that created
								// by the current and previous vectors: we
								// want all points in the poly to be coplanar
					else if (!plane_equal( pa0, pb0, pc0, pd0,
									   pa1, pb1, pc1, pd1
									   )) { // watch for non-unique planes
						fprintf( stderr,
								"poly %d does not have unique plane\n",
								poly);
					}

				}
				else {			// are colinear points
				fprintf( stderr,
						"dubious colinear points: poly %d, vrtx %d,%d,%d\n",
						poly, vrtx-2, vrtx-1, vrtx);
				}

			}					// got 2 vectors so far
			
								// shift 2nd vertex info into 1st's position
			v0 = v1; x0 = x1; y0 = y1; z0 = z1; i0 = i1; j0 = j1; k0 = k1;
				
		}						// for (vrtx...)

	}							// for (poly...)

	if (nNormals == nFaces)		// got one normal for each polygon
		return 1;				// success
	else
		return 0;				// 1 or more normals missing: failure
}

#ifdef FUNCT_HDR

/* ************************************************************************* 

Function Name:           generate_vertex_normals
                         
Purpose:                 create normal vectors for each vertex in the
                         polygonal object.  It will do this by
                         averaging the normal vectors of all faces
                         that share a given vertex.
                         
Sample Call:             obj->generate_vertex_normals()
                         
Input Parameters:        none

Output Parameters:       none

Return Value:            0 if failure, 1 if success
                         
Externals:               none

Input Data:              none

Output Data:             none

Calling Functions:       optional, according to needs of PolyObject1
                         users
                         
Functions Called:        none

Description:             none

****************************************************************************/ 
#endif

int PolyObject1::generate_vertex_normals( void)
{
	int vtx,					// current vertex index
		nVerts,					// num vertices in object
		nUsers,					// num users of a particular vertex
		poly_ndx,				// index of polygon
		norm_ndx,				// index of normal vector data
	    i; 
	double dSumx, dSumy, dSumz, d;

	nVerts = get_num_points();	// total number of vertices in object

								// for each vertex in the object
	for (vtx = 0; vtx < nVerts; vtx++) {

		dSumx = dSumy = dSumz = 0.0; // clear for each vertex

		nUsers = vrtx_poly_users.get_size( vtx); // how many polys use vertex?
		if (!nUsers)
			continue;			// no users of this vertex: skip calculation

								// for each poly that uses this vertex
		for (i = 0; i < nUsers; i++) {
			poly_ndx = (int)vrtx_poly_users( vtx, i);

								// get its normal vector
			norm_ndx = (int)poly_normals[ poly_ndx ];

								// and add to cumulative vector
								// NOTE: the casting is _required_ so that the
								// overloaded cast does the right thing!!!!
			dSumx += (double)x_norm[ norm_ndx ];
			dSumy += (double)y_norm[ norm_ndx ];
			dSumz += (double)z_norm[ norm_ndx ];
			
		}
		d = sqrt( dSumx*dSumx + dSumy*dSumy + dSumz*dSumz);	// vector length

		dSumx /= d;				// get the average unit vector
		dSumy /= d;
		dSumz /= d;

		point_normals[ vtx ] = x_norm.get_num(); // assign index into raw
												 // vector data assuming 


		x_norm.add_value( dSumx); // add to list of raw normal vectors
		y_norm.add_value( dSumy);
		z_norm.add_value( dSumz);

	}

	return 1;					// meaningless
}


int PolyObject1::generate_normals( void)
{
	int nRetval;

	if ((nRetval = generate_poly_normals()) == 1) // for each polygon
		nRetval = generate_vertex_normals();	  // average the normals of
												  // polys which share each
												  // vertex
	return nRetval;
}

int PolyObject1::load_texture_image(void)
{
	int nRetval = FALSE; // assume failure
	char *pName;
    	Image *pImg;

	if ((pName = get_texture_name()) == NULL) // no image file named!
		return FALSE;

	if ((pImg = new Image()) == NULL)
		return FALSE;

	if (!(pImg->read(pName))) {
		nRetval = TRUE;			// success
		set_texture_image( pImg); // save image ptr only if succesfully loaded
	}
	else
		delete pImg;			// no need for it if we can't load image file

	return nRetval;
}

int PolyObject1::get_texel(int i, double &x, double &y, double &z)
{
//	if(i >= 0 && i < get_num_points()) {
	if(i >= 0 && i < x_text.get_num()) {
		x = x_text.get_value(i);
		y = y_text.get_value(i);
		z = z_text.get_value(i);
		return(TRUE);
	} else {
		return(FALSE);
	}
}

int PolyObject1::get_texel_ndx( int poly_ndx, int vert_ndx)
{
	if(poly_ndx >= 0 && poly_ndx < get_num_faces()) {

								// valid polygon index

		if(vert_ndx >= 0 && vert_ndx < get_num_verts(poly_ndx)) {

								// valid vertex index for this poly

								// return index into list of vertex coords

			return(text_verts.get_value(poly_ndx, vert_ndx));
		}
	}
	return(-1);
}

/********************************* misc routines **************************/

int PolyTerrainObj::gen_tmesh( void)
								// build a tmesh from the surface's
								// displacement texture map
{
	int i, j, nX, nY, xres, yres;
	double dXStep, dYStep, dCurX, dCurY, dElevY, dCurZ;;
	Image *pI;
	ImageData *pID;
	int texture_flag = 0;
//	uchar r, g, b;

	
								// 
								// error checking
								// 
	if (surface.get_tmap_disp() == NULL) {
		FATAL_ERROR( "Can not build tmesh without a displacement map!");
		return 0;
	}

	if (surface.get_tmap_color()) texture_flag = 1;


								// 
								// compute step size 
								// 
	pI = surface.get_tmap_disp()->get_texture();
	pI->get_res( &xres, &yres);
	pID = pI->get_data();

	dXStep = (double)xres / (double)xfactor;
	dYStep = (double)yres / (double)yfactor;
	
	nX = xfactor;
	nY = yfactor;
	
	
								// 
								// build vertices by starting at largest
								// elevation y and decreasing it
	                            // towards 0 (i.e. flip it about 
								// the x axis) - this gets it in a right-handed
								// system (we do the same thing when we load
								// textures)
								// NOTE: we still build the traingles with
								// increasing y
	x_vert.free_list();
	y_vert.free_list();
	z_vert.free_list();

	if (texture_flag) {
		x_text.free_list();
		y_text.free_list();
		z_text.free_list();
	}

	dCurY = 0.0;
	dElevY = nY * dYStep - 1;
	for (j = 0; j <= nY; j++, dCurY += dYStep, dElevY -= dYStep) {
		dCurX = 0.0;
		for (i = 0; i <= nX; i++, dCurX += dXStep) {

			dCurZ = pID->iget_double( dCurX, dElevY) / zscale;
			x_vert.add_value( dCurX);
			y_vert.add_value( dCurY);
			z_vert.add_value( dCurZ);
			if (texture_flag) {
				double ftexel[3];
				double fvert[3];
				fvert[0] = dCurX; fvert[1] = dElevY; fvert[2] = dCurZ;
            	surface.get_tmap_color()->get_texture_coord(fvert, ftexel);
				x_text.add_value(ftexel[0]);
				y_text.add_value(ftexel[1]);
				z_text.add_value(0.0);
			}

			point_colors.add_value( 0);
		}
	}

								// 
								// build polygons in CW orientation
								// 
	int toffset, trow, nPolys, col;
	
	poly_verts.free_all();
	poly_colors.free_list();
	r.free_list();
	g.free_list();
	b.free_list();

	r.add_value( 1); 
	g.add_value( 0);
	b.add_value( 0);

	r.add_value( 0); 
	g.add_value( 1);
	b.add_value( 0);

	r.add_value( 0); 
	g.add_value( 0);
	b.add_value( 1);

	r.add_value( 0); 
	g.add_value( 1);
	b.add_value( 1);

	r.add_value( 1); 
	g.add_value( 1);
	b.add_value( 0);

	nPolys = 2 * xfactor * yfactor;
	for (i = 0; i < nPolys; i++) { // for each polygon
		poly_verts.add_list();
		poly_colors.add_value( i % 5); // all are white
		toffset = i % (2 * xfactor);	// triangle offset in this row of triangles
		trow = i / (2 * xfactor);	// row of triangles
		col = toffset / 2;
		if (toffset & 1) {		// toffset is odd
			poly_verts.add_value( poly_verts.get_num() - 1,
									 (trow * (xfactor+1)) + col + 1);
			poly_verts.add_value( poly_verts.get_num() - 1,
									 ((trow+1) * (xfactor+1)) + col);
			poly_verts.add_value( poly_verts.get_num() - 1,
									 ((trow+1) * (xfactor+1)) + col + 1);
		}
		else {					// toffset is even
			poly_verts.add_value( poly_verts.get_num() - 1, 
									 (trow * (xfactor+1)) + col);
			poly_verts.add_value( poly_verts.get_num() - 1,
									 ((trow+1) * (xfactor+1)) + col);
			poly_verts.add_value( poly_verts.get_num() - 1,
									 (trow * (xfactor+1)) + col + 1);
		}
	}
	
	return 1;
	
}

int PolyTerrainObj::make_flat()
{
        double lon, lat, radius;
	double xx, yy, zz;
	double newx, newy, newz;
        int dummy = 0;
        int npts = get_num_points();

	if (sphere_radius < 0) {
	   // cout << "Object already flat" << endl;
	   return 0;
	}

        GeoImage *eimg = (GeoImage*)(surface.get_tmap_disp()->get_texture());
        if (!eimg) {
                cout << "No Elevation Image is Set" << endl;
                return -1;
        } else if ((eimg->image_class_type() != GEOIMAGE) || 
        	 				!eimg->get_geo_data()) {
                cout << "Image is not Georeferenced" << endl;
                return -1;
        }

	sphere_radius = z_exaggeration = -1;

	int xres, yres;
	eimg->get_res(&xres, &yres);
	y = yres;		// set  y centroid back to zero

        GeoData *gd = eimg->get_geo_data();
	ImageData *dat = eimg->get_data();
        for (int i = 0; i < npts; i++) {
 	    get_point(i, dummy, xx, yy, zz);
	    xyz_to_spherical(xx, yy, zz, &lat, &lon, &radius);
            gd->lltoxy(lat, lon, newx, newy);
	    newz = dat->iget_double(newx, newy);
	    newy = yres - newy - 1;
            set_point(i, dummy, newx, newy, newz);
        }
	return 1;
}

void xyz_to_spherical(double x, double y, double z, double *lat, double *lon,
                                                        double *radius)
{
	double PII = 3.141592653589793238462643;

        *lon = atan2(y, x);
        *lon *= (180.0/PII);
        double xp = sqrt(x*x + y*y);
        *lat = atan2(z, xp);
        *lat *= (180.0/PII);
        *radius = sqrt(x*x + y*y + z*z);
}

int PolyTerrainObj::make_spherical(double radius, double z_exag)
{
        double zvert, lon, lat;
	double xx, yy;
	double xtex, ytex, ztex;
        int dummy = 0;

	if (radius > 0) {   // already spherical mode
	   if (sphere_radius == radius && z_exag == z_exaggeration) {
	      cout << 
	"Sphere of specified radius and exaggeration already created" << endl;
	      return 0;
	   } else {
	     make_flat();
	   }
 	} else if (radius <= 0) {
	   cout << "Radius must be greater than zero" << endl;
	   return -1;
	}
	sphere_radius = radius;
	z_exaggeration = z_exag;
		
        int npts = get_num_points();

	double PII = 3.141592653589793238462643;

        GeoImage *img = (GeoImage*)(surface.get_tmap_color()->get_texture());
        if (!img) {
                cout << "No Texture Image is Set" << endl;
                return -1;
        } else if (img->image_class_type() != GEOIMAGE || 
		    				!img->get_geo_data()) {
                cout << "Image is not Georeferenced" << endl;
                return -1;
        }
	y = 0.0;		// set  y centroid back to zero
	int xres, yres;
	img->get_res(&xres, &yres);
// 	ImageData *dat = img->get_data();

// first warp the texture
        GeoData *gd = img->get_geo_data();
        for (int i = 0; i < npts; i++) {
	    get_texel(i, xtex, ytex, ztex);
	    get_point(i, dummy, xx, yy, zvert);
	    xtex *= xres;
	    ytex *= yres;
	    int ix = (int)(xtex + 0.5);
	    int iy = (int)(ytex + 0.5);
	    if (ix < 0) ix = 0;
	    else if (ix >= xres) ix = xres-1;
	    if (iy < 0) iy = 0;
	    else if (iy >= yres) iy = yres-1;
            gd->xytoll((double)ix, (double)iy, lat, lon);
            lat *= (PII/180.0);
            lon *= (PII/180.0);
            double rp = radius+(zvert*z_exag);
            double rpp = rp * cos(lat);
            double newx = rpp * cos(lon);
            double newy = rpp * sin(lon);
            double newz = rp * sin(lat);
            set_point(i, dummy, newx, newy, newz);
        }
	return 1;
}

int PolyTerrainObj::parse_in(Dataport *fp)
{

	char	token[4096];

	if(!Obj::parse_in(fp)) {
		return(FALSE);
	}

	do {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in polyterrain file\n");
			return(FALSE);
		}
		if(!strcmp(token, "VERSION")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polyterrain file\n");
				return(FALSE);
			}
			if(strcmp(token, "1.0")) {
				fprintf(stderr," Whoops - Unexpected version number encountered in parsing polyterrain object\n");
				fprintf(stderr,"   Token=>%s<  Expecting >1.0<\n", token);
				return(FALSE);
			}
			version = strdup(token);
		} else if(!strcmp(token, "FILE_TYPE")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polyterrain file\n");
				return(FALSE);
			}
			if(file_type)free(file_type);
			file_type = strdup(token);
	
		} else if(!strcmp(token, "OBJECT_TYPE")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polyterrain file\n");
				return(FALSE);
			}
			object_type = poly_object_from_string(token);
			if(object_type == BadPolyObj) {
				fprintf(stderr," Whoops - Unknown object type %s\n", token);
				return(FALSE);
			}
								// this next stuff is a hack which must go away!
		} else if(!strcmp(token, "GROUP_NAME")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polyterrain file\n");
				return(FALSE);
			}
			if(group_name)free(group_name);
			group_name = strdup(token);
		} else if(!strcmp(token, "ZSCALE")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polyterrain file\n");
				return(FALSE);
			}
			set_zscale( atoi( token));
		} else if(!strcmp(token, "MESHING_FACTOR")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polyterrain file\n");
				return(FALSE);
			}
			set_mesh_factor_x( atoi(token));

			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polyterrain file\n");
				return(FALSE);
			}
			set_mesh_factor_y( atoi(token));

				
		} else if(strcmp(token, "THAT'S_ALL_FOLKS")) {
			fprintf(stderr," Whoops - Unexpected token encountered in parsing polyterrain object\n");
			fprintf(stderr,"   Token=>%s<  Expecting one of COLORS POINTS EDGES FACES THAT'S_ALL_FOLKS\n", token);
			return(FALSE);
		}
	} while(strcmp(token, "THAT'S_ALL_FOLKS"));

	if (xfactor < 1|| yfactor < 1) {
		fprintf(stderr," bad meshing factors specified in polyterrain object\n");
		return FALSE;
	}
	gen_tmesh();

	return(TRUE);
	
}

int PolyTerrainObj::parse_out(Dataport *fp, int expand)
{
	put_token(fp, "POLY_TERRAIN_V1");

	Obj::parse_out(fp);

	put_token(fp, "\nVERSION");
	put_token(fp, "1.0");
	if(file_type) {
		put_token(fp, "\nFILE_TYPE");
		put_token(fp, file_type);
	}
	put_token(fp, "\nOBJECT_TYPE");
	put_token(fp, poly_object_type_master_list[object_type]);
	if(group_name) {
		put_token(fp, "\nGROUP_NAME");
		put_token(fp, group_name);
	}

	put_token(fp, "\nTHAT'S_ALL_FOLKS\n");

	return(TRUE);

}

PolyTerrainObj *CreatePolyTerrainObj(Image *terrain, Image *elevation,
	int xfactor, int yfactor, double xscale, double yscale, double zscale)
{
	PolyTerrainObj *tObj = new PolyTerrainObj;

	int geo_flag = 0;

	if (terrain->image_class_type() == GEOIMAGE &&
	    elevation->image_class_type() == GEOIMAGE) {
		if (((GeoImage*)elevation)->get_geo_data() && 
			((GeoImage*)terrain)->get_geo_data()) geo_flag = 1;
	}

	int terr_xres, terr_yres, elev_xres, elev_yres;
	terrain->get_res(&terr_xres, &terr_yres);
	elevation->get_res(&elev_xres, &elev_yres);

	// centroid

	tObj->x = tObj->z = tObj->xrot = tObj->yrot = tObj->zrot = 0.0;
	tObj->y = elev_yres;

	tObj->xscale = xscale;
	tObj->yscale = yscale;
	((Obj*)tObj)->zscale = zscale;

	tObj->set_zscale(zscale);		// terrain object

	TextureMap *color = new TextureMap;
	color->set_dims(3, 2);
	TextureMap *disp = new TextureMap;
	color->set_texture(terrain);
	disp->set_texture(elevation);

	tObj->surface.set_tmap_color(color);
	tObj->surface.set_tmap_disp(disp);
	tObj->set_mesh_factor(xfactor, yfactor);

// now create the default Mapping, must be done before gen_tmesh

	Mapping *map;

	if (!geo_flag) {
		map = new ImpMapPlanarProj;
		map->set_dims(3,2);
		color->set_mapping(map);

		double pvD0[3], pvD1[3], pvR0[2], pvR1[2];

		pvD0[0] = pvD0[1] = pvD0[2] = 0.0;
		pvR0[0] = pvR0[1] = 0.0;
		for (int i = 0; i < 3; i++) {
		    switch(i) {
		   	case 0:
			   pvD1[0] = elev_xres; pvD1[1] = pvD1[2] = 0.0;
			   pvR1[0] = 1.0; pvR1[1] = 0.0;
			   break;
		        case 1:
			   pvD1[1] = elev_yres; pvD1[0] = pvD1[2] = 0.0;
			   pvR1[0] = 0.0; pvR1[1] = 1.0;
			   break;
		        case 2:
			   pvD1[0] = pvD1[1] = pvD1[2] = 0.0;
			   pvR1[0] = pvR1[1] = 0.0;
			   break;
		    }
		    ((ImpMapPlanarProj*)map)->set_dom_to_range_vector(i, pvD0, 
							pvR0, pvD1, pvR1);
	     	}
	} else {
		map = new GeoMapping((GeoImage*)terrain, (GeoImage*)elevation);	
		map->set_dims(3,2);
		color->set_mapping(map);
	}

    tObj->gen_tmesh();

	return tObj;
}

int PolySphereObj::gen_sphere()
{
	int	i,j;

	if (radius <= 0) return -1;
#if HI_SPHERE
	ofstream out("sphere.out");
#endif

	const double PII = 3.141592653589793238462643;

	poly_colors.free_list();
	r.free_list();
	g.free_list();
	b.free_list();

	r.add_value( 1); 
	g.add_value( 1);
	b.add_value( 1);

	r.add_value( 1); 
	g.add_value( 0);
	b.add_value( 0);

	x_vert.free_list();
	y_vert.free_list();
	z_vert.free_list();

// add north pole
	x_vert.add_value(0.0);
	y_vert.add_value(0.0);
	z_vert.add_value(radius);

// add south pole
	x_vert.add_value(0.0);
	y_vert.add_value(0.0);
	z_vert.add_value(-radius);

#if HI_SPHERE
	out << "0: " << "0   0   " << radius << endl;
	out << "1: " << "0   0   " << -radius << endl;
	int ct = 2;
#endif

	int start_lat = 90 - lat_res;


	for (j = 0; j <= 360-lon_res; j += lon_res) {
	    	double lon = j * PII / 180.0;  
	    	double cos_lon = cos(lon);
	    	double sin_lon = sin(lon);
		for (i = start_lat; i >= -start_lat; i -= lat_res) {
			double lat = i * PII / 180.0;  
	    		double cos_lat = cos(lat);
	    		double sin_lat = sin(lat);

			double x = radius * cos_lat * cos_lon;
			double y = radius * cos_lat * sin_lon;
			double z = radius * sin_lat;
			
			x_vert.add_value(x);
			y_vert.add_value(y);
			z_vert.add_value(z);

			point_colors.add_value(0);
			if (i == 0) {
				edge_colors.add_value(1);
				point_colors.add_value(1);
			} else {
				point_colors.add_value(0);
				edge_colors.add_value(0);
			}

#if HI_SPHERE
			out << ct << " LAT " << i << " LON " << j << "  " << 
				x << "  " << y << "  " << z << endl;
			ct++;
#endif
		}
	}
	// 
	// build polygons in CW orientation
	// 
	
	poly_verts.free_all();
#if HI_SPHERE
	ct = 0;
	out << endl << endl << "poly_verts" << endl << endl;
#endif

	int ncols = 360 / lon_res;
	int nrows = 180 / lat_res;

	// first create top triangles CW
	int second_pt = nrows+1;
	int third_pt = 2;;
	for (i = 0; i < ncols; i++) {
		poly_verts.add_list();
		poly_verts.add_value( poly_verts.get_num() - 1, 0);
		poly_verts.add_value( poly_verts.get_num() - 1, second_pt);
		poly_verts.add_value( poly_verts.get_num() - 1, third_pt);
#if HI_SPHERE
		out << ct << "  0  " << " " << second_pt << " " << third_pt << 
					endl;
		ct++;
#endif
		if (i < ncols-2) second_pt += (nrows-1);
	    	else second_pt = 2;
		third_pt += (nrows-1);
	}
		
	// now process middle polys
	int first_pt;
	for (j = 0; j < ncols; j++) { // for each longitude 
	    if (j < ncols-1) first_pt = (nrows+1) + (j*(nrows-1));
	    else first_pt = 2;
	    int last_pt = 2 + (j*(nrows-1));
	    for (i = 0; i < nrows-2; i++) { // for each polygon
		poly_verts.add_list();
		poly_verts.add_value( poly_verts.get_num() - 1, first_pt);
		poly_verts.add_value( poly_verts.get_num() - 1, first_pt+1); 
		poly_verts.add_value( poly_verts.get_num() - 1, last_pt+1);
		poly_verts.add_value( poly_verts.get_num() - 1, last_pt);
#if HI_SPHERE
		out << ct << "  " << first_pt << " " << first_pt+1 << " " << 
				last_pt+1 << " " << last_pt << endl;
		ct++;
#endif
		first_pt++;
		last_pt++;
	    }
	}

	// now create bottom triangles CW
	second_pt = nrows;
	third_pt = second_pt + nrows - 1;
	for (i = 0; i < ncols; i++) {
		poly_verts.add_list();
		poly_verts.add_value( poly_verts.get_num() - 1, 1);
		poly_verts.add_value( poly_verts.get_num() - 1, second_pt); 
		poly_verts.add_value( poly_verts.get_num() - 1, third_pt);
#if HI_SPHERE
		out << ct << "  1  " << " " << second_pt << " " << third_pt << 
				endl;
		ct++;
#endif
		second_pt += (nrows-1);
		if (i < ncols-2) third_pt += (nrows-1);
		else third_pt = nrows; 
	}
	return 1;
}
