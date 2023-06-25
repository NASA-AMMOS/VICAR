// object.C 1.38 02/07/10 15:02:52
/** \file
 ** Obj, ObjNode, and GroupObj class methods.
 **/
#include <math.h>
#include "grape/object.h"

int Obj::get_changed()
{
register int	c;

if(changed==CHANGE_COUNTER) return changed;

if((c=x.get_changed()) > changed) changed=c;
if((c=y.get_changed()) > changed) changed=c;
if((c=z.get_changed()) > changed) changed=c;
if((c=xrot.get_changed()) > changed) changed=c;
if((c=yrot.get_changed()) > changed) changed=c;
if((c=zrot.get_changed()) > changed) changed=c;
if((c=xscale.get_changed()) > changed) changed=c;
if((c=yscale.get_changed()) > changed) changed=c;
if((c=zscale.get_changed()) > changed) changed=c;
if((c=surface.get_changed()) > changed) changed = c;
return changed;
}

/// Load object from file
int Obj::parse_in(Dataport *fp)
{
	char	token[4096];

	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
		return(FALSE);
	}
	if(!strcmp(token, "OBJECT_NAME")) {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
			return(FALSE);
		}
		if(name)free(name);
		name = strdup(token);

		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
			return(FALSE);
		}
	}
	if (!strcmp( token, "GRAPE_SURFACE")) {
		if (!surface.parse_in( fp)) {
			fprintf(stderr," Whoops - Unable to parse in object's surface\n");
			return FALSE;
		}
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in object file\n");
			return(FALSE);
		}
	}
	if(strcmp(token, "CENTROID")) {
		fprintf(stderr," Whoops - Unexpected token encountered in parsing object\n");
		fprintf(stderr,"   Token=>%s<  Expecting >CENTROID<\n", token);
		return(FALSE);
	}

	// Parse in centroid values
	x.parse_in(fp);
	y.parse_in(fp);
	z.parse_in(fp);
	xrot.parse_in(fp);
	yrot.parse_in(fp);
	zrot.parse_in(fp);
	xscale.parse_in(fp);
	yscale.parse_in(fp);
	zscale.parse_in(fp);

	return(TRUE);
}

/// Save object to file
/** Optionally expand paths */
int Obj::parse_out(Dataport *fp, int expand)
{
	if(name) {
		put_token(fp, "\nOBJECT_NAME");
		put_token(fp, name);
	}
	put_token(fp, "\nCENTROID");
	x.parse_out(fp, expand);
	y.parse_out(fp, expand);
	z.parse_out(fp, expand);
	xrot.parse_out(fp, expand);
	yrot.parse_out(fp, expand);
	zrot.parse_out(fp, expand);
	xscale.parse_out(fp, expand);
	yscale.parse_out(fp, expand);
	zscale.parse_out(fp, expand);

	return(TRUE);
}

void Obj::get_look_vector(double &a, double &b, double &c)
{
	double	look[3], newlook[3];
	ZMatrix rotMatrix;

	look[0] = 1.0;
	look[1] = 0.0;
	look[2] = 0.0;

	MakeRotationMatrix(rotMatrix,xrot,yrot,zrot);
	MultPoints(look,rotMatrix,newlook);
	a = newlook[0];
	b = newlook[1];
	c = newlook[2];
}

void Obj::get_up_vector(double &a, double &b, double &c)
{
	double	up[3], newup[3];
	ZMatrix rotMatrix;

	
	up[0] = 0.0;
	up[1] = 0.0;
	up[2] = 1.0;

	MakeRotationMatrix(rotMatrix,xrot,yrot,zrot);
	MultPoints(up,rotMatrix,newup);
	a = newup[0];
	b = newup[1];
	c = newup[2];
}

/// Build the matrix to transform from model space to object space
/**
// Rm . Sm . Tm
// Rm = rotate about centroid to orient
//   model space in object space
// Sm = scale from model space to object spc
// Tm = model origin in model space
*/
void Obj::GetModelToObjectTransform( ZMatrix InOut)
{
	ZMatrix CxMat, CScMat;

	// build the model (centroid) matrices
	MakeRotationMatrix(InOut, xrot, yrot, zrot);
	MakeScaleMatrix(CScMat, xscale, yscale, zscale);
	MakeTranslationMatrix( CxMat, -x, -y, -z);

	// build the final matrix
	MatMult(CScMat, CxMat);		// Sm . Tm
	MatMult(InOut, CScMat); 	// Rm . (Sm . Tm)
}

/// Compute and cache object-to-model transforms. 
/** 
// Caller promises not to change transform values (translate/scale/rotate)
*/
void Obj::freeze_xform()
{
	if (obj2mdl == NULL) {		// allocate cached matrices
		obj2mdl = new ZMatrix;
		norm2mdl = new ZMatrix;
	}

	if (obj2mdl == NULL || norm2mdl == NULL) {
		fprintf(stderr, "Obj::freeze_xform: can't allocate "
				"cached transform matrices\n");
		return;
	}

	ZMatrix mdl2obj;
	GetModelToObjectTransform(mdl2obj);	// get forward transform
	MatInvert(mdl2obj, obj2mdl);		// save reverse transform
	MatTranspose(mdl2obj, norm2mdl);	// save normal vector transform
}

/// Release cached matrices, allowing transform changes
void Obj::unfreeze_xform()
{
	delete obj2mdl;			// release cached matrices
	delete norm2mdl;
	obj2mdl = norm2mdl = NULL;	// indicate not valid
}

/// Build the matrix to transform to model space from object space
/**
// This is the inverse of Model-to-Object matrix.
// Use cached version if valid.
*/
void Obj::GetObjectToModelTransform(ZMatrix InOut)
{
	if (obj2mdl) {		// have valid cached version
		MatCopy(obj2mdl, InOut);
		return;
	}

	// need to recompute it
	ZMatrix mdl2obj;
	GetModelToObjectTransform(mdl2obj);
	MatInvert(mdl2obj, InOut);
}

/// Build matrix to transform a normal vector to model space from object space
/**
// This is the transpose of the Model-to-Object matrix
// (the transpose of the inverse of the Object-to-Model matrix).
// Use cached version if valid.
*/
void Obj::GetNormalToModelTransform(ZMatrix InOut)
{
	if (norm2mdl) {			// have valid cached version
		MatCopy(norm2mdl, InOut);
		return;
	}

	// need to recompute it
	ZMatrix mdl2obj;
	GetModelToObjectTransform(mdl2obj);
	MatTranspose(mdl2obj, InOut);
}

/// Assignment operator, also used by copy constructor.
// Handles self-assignment and allocated data members.
Obj& Obj::operator=(const Obj &other)
{
	if (this == &other)	// handle self-assignment
		return *this;

	id = other.id;		// easy stuff
	x = other.x;
	y = other.y;
	z = other.z;
	xrot = other.xrot;
	yrot = other.yrot;
	zrot = other.zrot;
	xscale = other.xscale;
	yscale = other.yscale;
	zscale = other.zscale;
	surface = other.surface;

	unfreeze_xform();		// release cached matrices
	set_name(other.name);		// copy allocated string
	set_changed();

	return *this;
}

// ************************** ObjNode functions *********

/// Get the matrix to transform from object space to world space
/**
// To . So . Ro
// To = translate centroid in world space
// So = scale from object space to world spc
// Ro = rotate about centroid in object space
*/
void ObjNode::GetObjToWorldTransform( ZMatrix InOut)
{
	ZMatrix rotMatrix, ScMat;

	// build the object matrices
	MakeScaleMatrix( ScMat, xscale, yscale, zscale);
	// MatDump(stdout, "ScMat", ScMat);

	MakeTranslationMatrix( InOut, x, y, z);
	// MatDump(stdout, "STMat", scMat);

	MakeRotationMatrix(rotMatrix, xrot, yrot, zrot);
	// MatDump(stdout, "RotMat", rotMatrix);

	// build the final matrix
	MatMult( ScMat, rotMatrix);		 // So . Ro
	// MatDump(stdout, "SoRoMat", ScMat);

	MatMult( InOut, ScMat);			 // To . (So . Ro)
	// MatDump(stdout, "ToSoRo", InOut);
}

// Get the matrix that transforms from model space into world space
/**
                         
Input Parameters:        ZMatrix InOut - the matrix into which the
                         model-to-world matrix will be stored.
                         
Output Parameters:       ZMatrix InOut - contains the model-to-world
                         matrix upon exit
                         
Return Value:            none

Functions Called:        various matrix routines
                         
Description:             This does NOT support hierarchically defined
                         object nodes (at least, not yet).
**/                         

void ObjNode::GetTransformationMatrix( ZMatrix InOut)
{
	ZMatrix Mc;
	Obj *o = get_object();			// object centroid info

	GetObjToWorldTransform(InOut);
	// MatDump(stdout, "Mo", InOut);

	o->GetModelToObjectTransform( Mc);
	// MatDump(stdout, "Mc", Mc);

	MatMult( InOut, Mc);
	// MatDump(stdout, "MoMc", InOut);
}

/// Get the position of the object in world space
void ObjNode::get_object_pos(double &a, double &b, double &c)
{
	double	look[3], newlook[3];
	ZMatrix Cum;

	look[0] = 0.0;
	look[1] = 0.0;
	look[2] = 0.0;

	GetTransformationMatrix( Cum);

	MultPoints(look,Cum,newlook);
	a = newlook[0];
	b = newlook[1];
	c = newlook[2];
}

/**
// Get the position of the end of a unit vector that starts at
// the object itself and ends 1 unit away in the object's x
// direction; returns world coords
*/
void ObjNode::get_look_vector(double &a, double &b, double &c)
{
	double whereat[3], lookat[3];

	get_object_pos( whereat[0], whereat[1], whereat[2]);
	get_lookat_ref_point( lookat[0], lookat[1], lookat[2]);
	
	a = lookat[0] - whereat[0];
	b = lookat[1] - whereat[1];
	c = lookat[2] - whereat[2];
}

/**
// Get the position of the end of a unit vector that starts at
// the object itself and ends 1 unit away in the object's x
// direction; returns world coords
*/
void ObjNode::get_lookat_ref_point(double &a, double &b, double &c)
{
	double	look[3], newlook[3];
	ZMatrix Cum;

	look[0] = 1.0;
	look[1] = 0.0;
	look[2] = 0.0;

	GetTransformationMatrix( Cum);

	MultPoints(look,Cum,newlook);
	a = newlook[0];
	b = newlook[1];
	c = newlook[2];

}

/**
// Get the position of the end of a unit vector that starts at
// the object itself and ends 1 unit away in the object's z
// direction; returns world coords
*/
void ObjNode::get_up_vector(double &a, double &b, double &c)
{
	double whereat[3], up_point[3];

	get_object_pos( whereat[0], whereat[1], whereat[2]);
	get_up_vector_ref_point( up_point[0], up_point[1], up_point[2]);
	
	a = up_point[0] - whereat[0];
	b = up_point[1] - whereat[1];
	c = up_point[2] - whereat[2];
}

/**
// Get the position of the end of a unit vector that starts at
// the object itself and ends 1 unit away in the object's z
// direction; returns world coords
*/
void ObjNode::get_up_vector_ref_point(double &a, double &b, double &c)
{
	double	look[3], newlook[3];
	ZMatrix Cum;

	look[0] = 0.0;
	look[1] = 0.0;
	look[2] = 1.0;

	GetTransformationMatrix( Cum);

	MultPoints(look,Cum,newlook);
	a = newlook[0];
	b = newlook[1];
	c = newlook[2];
}

static char	*append_token(char *in_string, char *add_string)
{
	if(in_string) {
		in_string = (char *)realloc(in_string, strlen(in_string) + strlen(add_string) + 2);
		strcat(in_string," ");
		strcat(in_string,add_string);
	} else {
		in_string = strdup(add_string);
	}

	return(in_string);
}

/// Load referenced object data (during ObjNode parse_in(), or delayed)
int ObjNode::parse_reference(Dataport *fp)
{
	if (!reference) {
		fprintf(stderr, "Whoops, no reference obj for ObjNode %s\n",
			name);
		return FALSE;
	}
	if(!fp || !fp->ropen(reference)) {
		fprintf(stderr,"Whoops - Unable to open referenced object file %s\n", reference);
		return(FALSE);
	}

	// create appropriate object
	Obj *tobj = object_creator(fp);
	if(!tobj) {
		fprintf(stderr,"Whoops - Unrecognized object type in external object file %s\n", reference);
		fp->close();
		return(FALSE);
	}

	// now parse in referenced object
	if(!tobj->parse_in(fp)) {
		fprintf(stderr,"Whoops - Problems parsing referenced object %s\n",reference);
		fp->close();
		return(FALSE);
	}

	fp->close();
	set_object(tobj);
	return TRUE;
}

/// Load object node from file, and optionally the referenced object data
int ObjNode::parse_in(Dataport *fp, int expand)
{
	char    token[4096];
	char    *token_string = NULL;
	int	geoflag = FALSE;

	if (name)		// don't leak these if allocated
		free(name);
	if (reference)
		free(reference);
	init();

	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
		return(FALSE);
	}
	if(!strcmp(token, "GEOOBJECT")) {
		geoflag = TRUE;
	} else if(strcmp(token, "OBJECT")) {
		fprintf(stderr," Whoops - Unexpected token encountered in parsing scene object\n");
		fprintf(stderr,"   Token=>%s<  Expecting >OBJECT< or >GEOOBJECT<\n", token);
		return(FALSE);
	}

	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
		return(FALSE);
	}
	set_name(token);

	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
		return(FALSE);
	}
	if(!strcmp(token, "{")) {	// inline object reference
		// call object creator with current fp
		Obj *tobj = object_creator(fp);
		if(!tobj) {
			fprintf(stderr,"Whoops - Unrecognized object type in internal object %s\n",
				name);
			return(FALSE);
		}
		if(!tobj->parse_in(fp)) {
			fprintf(stderr,"Whoops - Problems parsing inline object %s\n",
				name);
			return(FALSE);
		}
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
			return(FALSE);
		}
		if(strcmp(token, "}")) {
			fprintf(stderr," Whoops - Missing } after inline object %s\n",
				name);
			return(FALSE);
		}
		set_object(tobj);
	} else {	// must be external reference
		set_reference(token);
		if (expand) {
			Dataport *tfp = dataport_create(fp->get_type());
			if (!parse_reference(tfp))
				return(FALSE);
			delete tfp;
		}
	}

	// get transform
	x.parse_in(fp);
	y.parse_in(fp);
	z.parse_in(fp);
	xrot.parse_in(fp);
	yrot.parse_in(fp);
	zrot.parse_in(fp);
	xscale.parse_in(fp);
	yscale.parse_in(fp);
	zscale.parse_in(fp);
	
	if(geoflag) {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
			return(FALSE);
		}
		if(strcmp(token, "GEODATA")) {
			fprintf(stderr," Whoops - Missing GEODATA for inline geoobject %s\n", 
				get_name());
			return(FALSE);
		}
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
			return(FALSE);
		}
		while(strcmp(token, "ENDGEODATA")) {
			token_string = append_token(token_string, token);
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
				return(FALSE);
			}
		}
		geo_data = parse_object_geodata(token_string);
	}
	return(TRUE);
}
		
/// Save object node to file
int ObjNode::parse_out(Dataport *fp, int expand)
{
	GeoData	*gd;

	if(gd = get_geo_data()) {
		put_token(fp, "\nGEOOBJECT");
	} else {
		put_token(fp, "\nOBJECT");
	}
	if(get_name()) {
		put_token(fp, get_name());
	}
	if(expand || !get_reference() ) {
		put_token(fp, " {");
		get_object()->parse_out(fp, expand);
		put_token(fp, "\n}");
	} else {
		put_token(fp, get_reference());
	}
	x.parse_out(fp, expand);
	y.parse_out(fp, expand);
	z.parse_out(fp, expand);
	xrot.parse_out(fp, expand);
	yrot.parse_out(fp, expand);
	zrot.parse_out(fp, expand);
	xscale.parse_out(fp, expand);
	yscale.parse_out(fp, expand);
	zscale.parse_out(fp, expand);

	if(gd = get_geo_data()) {
		put_token(fp, "\nGEODATA\n");
		put_token(fp, gd->to_string());
		put_token(fp, "\nENDGEODATA\n");
	}

	return(TRUE);
}

/// Assignment operator, also used by copy constructor.
// Handles self-assignment and allocated data members.
ObjNode& ObjNode::operator=(const ObjNode &other)
{
	if (this == &other)	// handle self-assignment
		return *this;
	
	x = other.x;		// easy stuff
	y = other.y;
	z = other.z;
	xrot = other.xrot;
	yrot = other.yrot;
	zrot = other.zrot;
	xscale = other.xscale;
	yscale = other.yscale;
	zscale = other.zscale;

	set_object(other.obj);	// copy pointers not freed in destructor
	set_geo_data(other.geo_data);

	set_name(other.name);	// new copy of allocated strings
	set_reference(other.reference);

	set_changed();

	return *this;
}

// *************************** GroupObj functions *********

/// Add child object node to group
/**
// Increase allocated array of children pointers and stuff in
// new object node. pos = -1 means add to end.
// Returns TRUE if successful.
// Not very efficient when adding lots of kids.
*/
int GroupObj::add_child(ObjNode *on, int pos)
{
	if (pos < 0 || pos > num_children)
		pos = num_children;	// set to valid new index

	ObjNode **new_list = (ObjNode **)malloc((num_children+1) *
					sizeof(ObjNode**));
	if (new_list == NULL)
		return FALSE;		// allocation failed

	if (num_children) {		// copy in old data
		if (pos > 0)
			memcpy(new_list, children, 
				pos * sizeof(ObjNode *));
		if (pos < num_children)
			memcpy(new_list+pos+1, children+pos,
				(num_children-pos) * sizeof(ObjNode *));
		free(children);		// release old list
	}

	new_list[pos] = on;		// set new node into list
	num_children++;
	children = new_list;
	return TRUE;
}

/// Remove a child from list, by index; optionally delete it
/**
// Not bothering to reallocate
// the array (nor keep track of unused free space).
// Returns TRUE if index was valid.
*/
int GroupObj::remove_child(int n, int del)
{
	if (n<0 || n >= num_children)
		return FALSE;
	if (del) {			// want it freed?
		delete children[n]->get_object();
		delete children[n];
	}
	num_children--;			// update count
	if (n < num_children)		// shift pointers
		memcpy(children+n, children+n+1, 
			(num_children-n) * sizeof(ObjNode *));
	return TRUE;
}

/// Remove a child from list, by pointer; optionally delete it
/**
// Returns TRUE if pointer indicated a child in the group
*/
int GroupObj::remove_child(ObjNode *on, int del)
{
	for (int i=0; i<num_children; i++) {
		if (children[i] == on)
			return remove_child(i, del);
	}
	return FALSE;
}

/// Does object node pointer indicate a child in this group?
int GroupObj::is_child(ObjNode *on)
{
	for (int i=0; i<num_children; i++) {
		if (children[i] == on)
			return TRUE;
	}
	return FALSE;
}

/// Save group to file
int GroupObj::parse_out(Dataport *fp, int expand)
{
        char    token[4096];

        put_token(fp, "\nGRP_V1");

        Obj::parse_out(fp, expand);

        sprintf(token, "\nCHILDREN %d", num_children);
        put_token(fp, token);

	for (int i=0; i<num_children; i++)
		children[i]->parse_out(fp, expand);

	return(TRUE);
}

/// common initial part of group file parsing
int GroupObj::parse_start(Dataport *fp)
{
	char token[1024];

	if (num_children) {		// reset group
		free(children);
		init();
	}

	// initial GRP_V1 token assumed to already be read in
	// (e.g. to determine that this *is* a group)

	// get top-level transform
	if (!Obj::parse_in(fp))
		return FALSE;

	// get number of children, allocate array
	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in group file\n");
		return(FALSE);
	}
	if (strcmp(token, "CHILDREN")) {
		fprintf(stderr," Whoops - Expected CHILDREN in group file\n");
		return(FALSE);
	}
	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in group file\n");
		return(FALSE);
	}
	
	num_children = atoi(token);
	children = (ObjNode **)malloc(num_children * sizeof(ObjNode*));
	if (children == NULL) {
		fprintf(stderr," Whoops - group allocation failed\n");
		return FALSE;
	}

	return TRUE;
}

/// Load ObjNode group from file, optionally load referenced data
int GroupObj::parse_in(Dataport *fp, int expand)
{
	if (!parse_start(fp))
		return FALSE;

	// allocate and parse children
	for (int i=0; i<num_children; i++) {
		children[i] = new ObjNode;
		if (!children[i]->parse_in(fp, expand)) {
			num_children = i;
			return FALSE;
		}
	}

	return(TRUE);
}
