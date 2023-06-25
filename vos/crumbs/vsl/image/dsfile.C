// dsfile.C
//
// Written by Dave Kagels 10/31/95

#ifndef _NO_DATASERVER_

#include "image/types/dsfile.h"
#include "image/image.h"
#include "image/datads.h"
#include "ds/client.h"


int DSFile::read_image(char *fname)
{
int	s,r,b,x,y,tsize,type,lev,tilesize;
dataDS	*dat;

reset_error();
if(!img) return error=ImageFileErr_NoImage;             // check for an Image

if(fname) set_filename(fname);
free_comments(); free_flags();

s=dsOpen(filename);
if(s<0) return error=ImageFileErr_BadFile;

r=requestHeader(s,&b,&y,&x,&tsize,&type,&lev,&tilesize);
if(r<0) return error=ImageFileErr_BadFile;

switch(type) {
	case UCHAR_DATA:	dat=new ucharDS;	break;
//	case SHORT_DATA:	dat=new shortDS;	break;
//	case LONG_DATA:		dat=new longDS;		break;
	default:		dat=NULL;
	}
if(!dat) return error=ImageFileErr_BadFile;

if(img->get_data()) img->free_data();
img->set_data(dat);

dat->set_socket(s);
dat->allocate(x,y,b);
dat->set_levels(lev);

return error;
}

#endif // _NO_DATASERVER_
