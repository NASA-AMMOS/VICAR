// perf_object.C - Performer Object Encapsulation for Grape Renderer

#ifdef _USE_PERFORMER_

#include "grape/perf_object.h"


int PerfObj::set_file(char *filename)
{
	pfNode   *node=NULL;			// file named in 'fname' loaded into here

	if(filename != NULL) {	// use argument if not NULL
		if(fname)free(fname);
		fname = strdup(filename);
	}

    if(dcs == NULL) {
	    if(fname == NULL) {
		    fprintf(stderr," Whoops - need to have a filename to initialize object\n");
			return(FALSE);
		}
		pfdInitConverter(fname);
		if((node = pfdLoadFile(fname)) == NULL) {
			fprintf(stderr," Whoops - error loading file %s\n",fname);
			return(FALSE);
		}
		dcs = new pfDCS;
		dcs->addChild(node);
	}
	// else file already applied, dcs initialized
	return(TRUE);
}

	// load Grape ZMatrix into Performer pfMatrix
int PerfObj::apply_transforms(ObjNode *on)
{
	ZMatrix  mat;			// grape matrix to which transformations
							// are applied before loading into DCSs

	pfMatrix pfmat;			// performer matrix

    if(on == NULL) {
	    fprintf(stderr," Whoops - NULL pointer for ObjNode %s\n",fname);
		fprintf(stderr,"Could not copy transform matrix - using identity.\n");
		idmat(mat);
	}
	else
	    on->GetTransformationMatrix(mat);

	// copy grape ZMatrix into performer pfMatrix
	convert_ZMat_to_pfMat(mat, &pfmat); // no error checking here

	if(dcs == NULL) {
		fprintf(stderr," Whoops - NULL dcs pointer in apply_transforms\n");
		return(FALSE);
	}
	else
		dcs->setMat(pfmat);	// put pfmat into pfDCS

	return(TRUE);
}

pfDCS *PerfObj::get_pfDCS_ptr()
{
/*    if(dcs == NULL && fname != NULL && !is_light())
	    set_file();
		// returns NULL if dcs and the filename are NULL or it is a NULL light
*/
	return(dcs);
}

int PerfObj::parse_in(Dataport *fp)
{
	char	token[4096];

	if(!Obj::parse_in(fp)) { // parse in first part of the file
	    return(FALSE);
	}

	// now do the PerfObj-specific stuff
	do {
	    if(!get_next_token(fp, token)) {
		    fprintf(stderr," Whoops - Unexpected EOF in PERF_V1 file\n");
			return(FALSE);
		}
		if(!strcmp(token, "VERSION")) {
		    if(!get_next_token(fp, token)) {
			    fprintf(stderr," Whoops - Unexpected EOF in PERF_V1 file\n");
				return(FALSE);
			}
			if(strcmp(token, "1.0")) {
			        fprintf(stderr," Whoops - Unexpected version number encountered parsing in Performer object\n");
				fprintf(stderr,"   Token=>%s<  Expecting >1.0<\n", token);
				return(FALSE);
			}
			version = strdup(token);
		} else if(!strcmp(token, "FILE_TYPE")) {
		    if(!get_next_token(fp, token)) {
			    fprintf(stderr," Whoops - Unexpected EOF in PERF_V1 file\n");
				return(FALSE);
			}
			if(file_type)free(file_type);
			file_type = strdup(token);
		} else if(!strcmp(token, "FILENAME")) { // this is the only one that matters
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in PERF_V1 file\n");
				return(FALSE);
			}
			if(fname)free(fname);
			fname = strdup(token);
		} else if(!strcmp(token, "GROUP_NAME")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in PERF_V1 file\n");
				return(FALSE);
			}
			if(group_name)free(group_name);
			group_name = strdup(token);
		}
	} while(strcmp(token, "THAT'S_ALL_FOLKS"));

	return(TRUE);

}

int PerfObj::parse_out(Dataport *fp, int expand)
{
	put_token(fp, "PERF_V1");

	Obj::parse_out(fp);

	put_token(fp, "\nVERSION");
	put_token(fp, "1.0");
	if(file_type) {
	    put_token(fp, "\nFILE_TYPE");
		put_token(fp, file_type);
	}
	put_token(fp, "\nFILENAME");
	put_token(fp, fname);
	if(group_name) {
	    put_token(fp, "\nGROUP_NAME");
		put_token(fp, group_name);
	}

	put_token(fp, "\nTHAT'S_ALL_FOLKS\n");

	return(TRUE);
}


void convert_pfMat_to_ZMat(pfMatrix *pfmat, ZMatrix mat) {

    int		i;
	float x,y,z,w;	// have to put into temp place, type cast, then get values

	if(pfmat == NULL)
	    fprintf(stderr," Whoops - NULL pointer exception reading pfMatrix! - need a pfMatrix to read from.\n");

	for(i = 0; i<DIM_ZMAT; i++)
	{
	    pfmat->getCol(i, &x, &y, &z, &w);
		mat[i][0] = (double)x;
		mat[i][1] = (double)y;
		mat[i][2] = (double)z;
		mat[i][3] = (double)w;
	}
}

void convert_ZMat_to_pfMat(ZMatrix mat, pfMatrix *pfmat) {

    int		i;

	if(pfmat == NULL)
	    fprintf(stderr," Whoops - NULL pointer exception reading pfMatrix! - need a pfMatrix to write to.\n");

	for(i = 0; i<DIM_ZMAT; i++) {
	    pfmat->setCol(i,(float)(mat[i][0]),
						(float)(mat[i][1]),
						(float)(mat[i][2]),
						(float)(mat[i][3]));
	}

}

#endif	// _USE_PERFORMER_
