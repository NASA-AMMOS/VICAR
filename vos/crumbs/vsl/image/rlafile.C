// rlafile.C
//
// Written by Dave Kagels 10/09/95

#include "image/types/rlafile.h"
#include "image/image.h"
#include "image/datatypes.h"

#include <string.h>
#include <strings.h>
#include <stdlib.h>

#include <time.h>

// Global variablefor loading auxiliary channel of RLA files:
// Defaults to loading both main and auxiliary channels.
int	RLA_Load=RLA_LOAD_AUX;


#ifdef SUN4
#define	fread(a,b,c,d)	fread((char *)(a),b,c,d)
#define	fwrite(a,b,c,d)	fwrite((char *)(a),b,c,d)
#define	free(a)		free((char *)(a))

int gethostname(char *s, int n) {
	char *str=getenv("HOST");
	if(str) { strncpy(s,str,n); return 0; }
	return -1;
	}
#endif


#ifdef SOLARIS
extern "C" {
extern void bzero(void *s, size_t n);
}

int gethostname(char *s, int n) {
	char *str=getenv("HOST");
	if(str) { strncpy(s,str,n); return 0; }
	return -1;
	}
#endif


/* data types for windows and image related stuff
 */
typedef struct {
    int         left, right, bottom, top;
} WINDOW_I;

typedef struct {
    short       left, right, bottom, top;
} WINDOW_S;

typedef struct {
    float       left, right, bottom, top;
} WINDOW_F;

typedef unsigned char U_CHAR;
typedef char*		POINTER;
typedef int             (*FUNC_PTR)();
typedef int             (*FUNC_PTR_I)();
typedef long            (*FUNC_PTR_L)();
typedef short           (*FUNC_PTR_S)();
typedef float           (*FUNC_PTR_F)();
typedef char            *(*FUNC_PTR_P)();


#define RLA_REVISION    0xfffe
#define RLA_TIME_OFFSET 614
#define IMF_C_byte      0
#define RLA_C_INTEGER   IMF_C_byte
#define RLA_C_FLOAT     4

typedef struct {
    WINDOW_S            window;
    WINDOW_S            active_window;
    short               frame;
    short               storage_type;
    short               num_chan;
    short               num_matte;
    short               num_aux;
    short               revision;              /* used to be aux_mask */
    char                gamma[16];
    char                red_pri[24];
    char                green_pri[24];
    char                blue_pri[24];
    char                white_pt[24];
    long                job_num;
    char                name[128];
    char                desc[128];
    char                program[64];
    char                machine[32];
    char                user[32];
    char                date[20];
    char                aspect[24];
    char                aspect_ratio[8];
    char                chan[32];
                                               /* new fields */
    short               field;
    char                time[12];
    char                filter[32];
    short               chan_bits;
    short               matte_type;
    short               matte_bits;
    short               aux_type;
    short               aux_bits;
    char                aux[32];
    char                space[36];             /* expansion space */
    long                next;
} RLA_HEADER;


/*	decode - decodes run length encoded pixels
 */
void decode(U_CHAR *c_in, U_CHAR *c_out, int len)
{
    int     ct;

    while ( len > 0) {
	ct = *c_in++;
	len--;
	if ( ct < 128) {
	    /* repeat pixel value ct+1 times */
	    while ( ct-- >= 0) 
		*c_out++ = *c_in;
	    c_in++;
	    len--;
	} else {
	    /* copy ct unencoded values */
	    for ( ct = 256-ct; ct-- > 0; len--)
		*c_out++ = *c_in++;
	}
    }
}


/*	encode - run length encodes pixel data for one channel
 *                 returns size of encoded data
 */
int encode(U_CHAR *c_in, U_CHAR *c_out, int width)
{
    int     len;
    int     ct;

 len = 0;

    while ( width > 0) {
	/* don't run-length encode unless we have > 2 equal values in a row */
	if ((width > 2) && (c_in[0] == c_in[1]) && (c_in[1] == c_in[2])) {
	    for ( ct=2; ct < width; ct++) {
		if ( c_in[ct] != c_in[ct-1])
		    break;
		if ( ct >= 127)
		    break;
	    }

	    /* write out co unt */
	    *c_out++ = ct-1;
	    len++;

	    /* write out value */
	    *c_out++ = *c_in;
	    len++;
	    width -= ct;
	    c_in += ct;
	} else {
	    for ( ct=1; ct < width; ct++) {
		/* don't run-length encode unless we have > 2 equal values in a row */
		if (( width-ct > 2) && (c_in[ct] == c_in[ct+1]) && (c_in[ct+1] == c_in[ct+2]))
		    break;
		if ( ct >= 127)
		    break;
	    }
	    /* write out count */
	    *c_out++ = 256 - ct;
	    len++;

	    /* copy string of pixels */
	    for ( ; ct-- > 0; len++, width--)
		*c_out++ = *c_in++;
	}
    }
    return len;
}


int RLAFile::read_data(FILE *f)
{
int		xres,yres,x1,i,bytes;
ImageData	*idat,*adat;
float		*aux=NULL;
U_CHAR		*auxi=NULL,*ptr;
ulong		uldat;
          
    RLA_HEADER      rla_head;
    int             *offset;
    int             act_x_res, act_y_res;
    int             width;
    int             x,y;
    int             left;
    int             scan;
    U_CHAR          *red=NULL, *green, *blue, *matte;
    U_CHAR          *buf;
    short           len;

if(!img) return error=ImageFileErr_NoImage;		// check for an Image

if(f) attach(f);					// set file pointer

/* read rla file header
 * this can be read in one chunk on 68000 aligned machines
 */
if ( ::fread( &rla_head, 740, 1, fp) != 1) {
	return error=ImageFileErr_BadFile;
	}

xres=rla_head.window.right - rla_head.window.left + 1;
yres=rla_head.window.top - rla_head.window.bottom + 1;

act_x_res = rla_head.active_window.right -
		rla_head.active_window.left + 1;
act_y_res = rla_head.active_window.top -
		rla_head.active_window.bottom + 1;

if(RLA_Load==RLA_AUX_ONLY) {
	if(rla_head.num_aux>0) { aux_img=img; clean=False; }
	else	return error=ImageFileErr_BadFile;
	}
else {
	// Get an image data pointer:
	idat=*img;
	if(!idat) {
		idat=new ucharData();
		*img=idat;
		}
	if(!(idat->access() & DATA_WRITE))
		return error=ImageFileErr_BadImageType;

	// Allocate memory:
	if(!idat->allocate(xres,yres,4))
		return error=ImageFileErr_AllocFailed;

	// Set up image:
	idat->set_rgb_bands(0,1,2);
	idat->set_alpha_band(3);
	idat->free_map();
	}

if(rla_head.num_aux>0 && RLA_Load>RLA_NO_AUX) {		// aux channels
	if(!aux_img) { aux_img=new Image; clean=True; }
	adat=*aux_img;
	if(rla_head.aux_bits<=8) bytes=1;
	else if(rla_head.aux_bits<=16) bytes=2;
	else bytes=4;
	if(!adat) {
		if(rla_head.aux_type==4) adat=new floatData();
		else {	if(bytes==1) adat=new ucharData();
			else if(bytes==2) adat=new ushortData();
			else adat=new ulongData();
			}
		*aux_img=adat;
		}
	if(!(adat->access() & DATA_WRITE))
		return error=ImageFileErr_BadImageType;

	// Allocate memory:
	if(!adat->allocate(xres,yres,rla_head.num_aux))
		return error=ImageFileErr_AllocFailed;

	// Set up image:
	if(rla_head.num_aux==1) adat->set_rgb_bands(0,0,0);
	else if(rla_head.num_aux==2) adat->set_rgb_bands(0,1,0);
	else	adat->set_rgb_bands(0,1,2);
	adat->set_alpha_band(-1);
	}

/* read scanline offset table */

if ( !(offset = (int *)malloc( sizeof(int) * act_y_res))) {
	return error=ImageFileErr_AllocFailed;
	}
if ( ::fread( offset, sizeof(int), act_y_res, fp) != act_y_res) {
	return error=ImageFileErr_BadFile;
	}

width = rla_head.window.right - rla_head.window.left + 1;
if ( !(buf = (U_CHAR *)malloc( sizeof(U_CHAR) * width * 5))) {
	return error=ImageFileErr_AllocFailed;
	}

if(RLA_Load!=RLA_AUX_ONLY) {
    if ( !(red = (U_CHAR *)malloc( sizeof(U_CHAR) * width * 4))) {
	return error=ImageFileErr_AllocFailed;
	}
 
    green = &red[width];
    blue = &green[width];
    matte = &blue[width];
    }

if(rla_head.num_aux>0) {
	// allocate space for 32 bit float aux channel data
	if(rla_head.aux_type==4) {
		if(!(aux=(float *)malloc(4*width))) return error=ImageFileErr_AllocFailed;
		}
	else if(RLA_Load>RLA_NO_AUX) {
		if(!(auxi=(U_CHAR *)malloc(bytes*width))) return error=ImageFileErr_AllocFailed;
		}
	}

if(rla_head.desc[0]) set_comments(rla_head.desc);
else free_comments();

    /* loop through window of image,
     * write blank lines outside active window
     */
    left = rla_head.active_window.left;
    y=yres-1;
    for ( scan=rla_head.window.bottom;
	   	scan <= rla_head.window.top; scan++) {

	/* check for black regions outside active window */
	if ((scan < rla_head.active_window.bottom) ||
	    (scan > rla_head.active_window.top)) {

	   /* set a blank scanline */
	   if(RLA_Load!=RLA_AUX_ONLY)
		for(x=0;x<xres;x++) idat->set_color(x,y,0,0,0,255);
	   if(RLA_Load>RLA_NO_AUX) {
		for(i=0;i<rla_head.num_aux;i++)
			for(x=0;x<xres;x++) adat->set_uchar(0,x,y,i);
		}
	   }
	else {
	   /* read red scanline */
	   ::fread( &len, sizeof(short), 1, fp);
	   ::fread( buf, sizeof(U_CHAR), (int)len, fp);
	   if(RLA_Load!=RLA_AUX_ONLY) decode( buf, red, (int)len);

	   /* read green scanline */
	   ::fread( &len, sizeof(short), 1, fp);
	   ::fread( buf, sizeof(U_CHAR), (int)len, fp);
	   if(RLA_Load!=RLA_AUX_ONLY) decode( buf, green, (int)len);

	   /* read blue scanline */
	   ::fread( &len, sizeof(short), 1, fp);
	   ::fread( buf, sizeof(U_CHAR), (int)len, fp);
	   if(RLA_Load!=RLA_AUX_ONLY) decode( buf, blue, (int)len);

	   /* read matte scanline */
	   ::fread( &len, sizeof(short), 1, fp);
	   ::fread( buf, sizeof(U_CHAR), (int)len, fp);
	   if(RLA_Load!=RLA_AUX_ONLY) decode( buf, matte, (int)len);

	   /* write out RGBM for each pixel */
	   if(RLA_Load!=RLA_AUX_ONLY) {
	     x1=0;
	     for (x=rla_head.window.left; 
	   	x <= rla_head.window.right; x++) {

		if (( x < rla_head.active_window.left) ||
		    ( x > rla_head.active_window.right) ) {
		    idat->set_color(x1,y,0,0,0,255);
		    }
		else {
		    idat->set_color(x1,y,red[x-left],green[x-left],
					blue[x-left],matte[x-left]);
		    }
		x1++;
	    	}
	      }

	   // read aux channel
	   if(rla_head.aux_type==4) {
		for(i=0;i<rla_head.num_aux;i++) {
			::fread( &len, sizeof(short), 1, fp);
			::fread(aux, 1, (int)len, fp);
			if(RLA_Load>RLA_NO_AUX) {
				x1=0;
				for (x=rla_head.window.left; 
				  x <= rla_head.window.right; x++) {
					if (( x < rla_head.active_window.left) ||
					    ( x > rla_head.active_window.right) )
					    adat->set_float(0.0,x1,y,i);
					else
					    adat->set_float(aux[x-left],x1,y,i);
					x1++;
	    				}
				}
			}
		}
	   else {
		for(i=0;i<rla_head.num_aux;i++) {
			::fread( &len, sizeof(short), 1, fp);
	   		::fread( buf, 1, (int)len, fp);
			if(RLA_Load>RLA_NO_AUX) {
				decode( buf, auxi, (int)len);
				x1=0;
				for (x=rla_head.window.left; 
				  x <= rla_head.window.right; x++) {
					if (( x < rla_head.active_window.left) ||
					    ( x > rla_head.active_window.right) )
						adat->set_uchar(0,x1,y,i);
					else {
					    if(bytes==1)
						adat->set_uchar(auxi[x-left],x1,y,i);
					    else if(bytes==2) {
						adat->set_ushort(((ushort)auxi[x-left]<<8)|(ushort)auxi[x-left+act_x_res],x1,y,i);
						}
					    else {
						ptr=&auxi[x-left];
						uldat=(ulong)*ptr; ptr+=act_x_res;
						uldat=(uldat<<8)|(ulong)*ptr; ptr+=act_x_res;
						uldat=(uldat<<8)|(ulong)*ptr; ptr+=act_x_res;
						uldat=(uldat<<8)|(ulong)*ptr;
						adat->set_ulong(uldat,x1,y,i);
						}
					    }
					x1++;
	    				}

				}
			}
		}
	   }
	y--;
	}

free(offset); free(buf);
if(red) free(red);
if(aux) free(aux);
if(auxi) free(auxi);

return error;
}


int RLAFile::write_data(FILE *f)
{
int		xres,yres,x,y,bytes,off,i;
ImageData	*idat,*adat;
float		ftemp;
ushort		ustemp;
ulong		ultemp;

    RLA_HEADER      rla_head;
    int             *offset;
    int             scan;
    U_CHAR          *red, *green, *blue, *matte;
    U_CHAR          *buf;
    short           len;
    char            *d_str;
    long	    second;

if(!img) return error=ImageFileErr_NoImage;		// check for an Image

if(f) attach(f);					// set file pointer

// Get an image data pointer:
idat=*img;
if(!idat)
	return error=ImageFileErr_BadImageType;
if(!(idat->access() & DATA_READ))
	return error=ImageFileErr_BadImageType;

// Get resolution:
idat->get_res(&xres,&yres);
if(xres<1 || yres<1)
	return error=ImageFileErr_BadImageData;

	/* fill in rla header structure */

	bzero( &rla_head, (size_t)740);

	rla_head.window.right = xres - 1;
	rla_head.window.left = 0;
	rla_head.window.top = yres - 1;
	rla_head.window.bottom = 0;
	rla_head.active_window.right = xres - 1;
	rla_head.active_window.left = 0;
	rla_head.active_window.top = yres - 1;
	rla_head.active_window.bottom = 0;

	rla_head.frame = 1;
	rla_head.storage_type = 0;
	rla_head.num_chan = 3;
	rla_head.num_matte = 1;
	rla_head.num_aux = 0;
	rla_head.revision = 0;

	strcpy( rla_head.gamma, "2.2");
	strcpy( rla_head.red_pri, "0.670 0.330");
	strcpy( rla_head.green_pri, "0.210 0.710");
	strcpy( rla_head.blue_pri, "0.140 0.080");
	strcpy( rla_head.white_pt, "0.310 0.316");
	rla_head.job_num = 0;
	strncpy( rla_head.name, filename, 127);
	if(comments) strncpy( rla_head.desc, comments, 127);
	else	strcpy( rla_head.desc, "New image created by VSL");
	strcpy( rla_head.program, "VSL");
	gethostname( rla_head.machine, 32);
	d_str=getenv("USER");
	if(d_str) strncpy( rla_head.user, d_str, 31);
	else	strcpy( rla_head.user, "unknown");

    /* fill in file date - varies with OS */
    second = time((long *)NULL);
    d_str = ctime( &second);
    strncpy( rla_head.date, &d_str[4], 12);
    strncpy( &rla_head.date[12], &d_str[19], 5);
    rla_head.date[17] = '\0';

    strcpy( rla_head.aspect, "user defined");
    strcpy( rla_head.aspect_ratio, "1.0");
    strcpy( rla_head.chan, "rgb");

	rla_head.chan_bits=8;
	rla_head.matte_type=0;
	rla_head.matte_bits=8;

    if(aux_img && aux_img!=img) {
	adat=*aux_img;
	if(!adat)
		return error=ImageFileErr_BadImageType;
	if(!(adat->access() & DATA_READ))
		return error=ImageFileErr_BadImageType;

	rla_head.num_aux=adat->get_bands();
	strcpy( rla_head.aux, "depth");
	switch(adat->preferred_data_type()) {
	    case BIT_DATA:
	    case CHAR_DATA:
	    case UCHAR_DATA:
		rla_head.aux_type=0;
		rla_head.aux_bits=8;
		bytes=1;
		break;
	    case SHORT_DATA:
	    case USHORT_DATA:
		rla_head.aux_type=0;
		rla_head.aux_bits=16;
		bytes=2;
		break;
	    case LONG_DATA:
	    case ULONG_DATA:
		rla_head.aux_type=0;
		rla_head.aux_bits=32;
		bytes=4;
		break;
	    default:
		rla_head.aux_type=4;
		rla_head.aux_bits=32;
		bytes=4;
	    }
	}

    /* write rla file header */
    ::fwrite( &rla_head, 740, 1, fp);

    /* allocate scanline offset table */
    if ( !(offset = (int *)malloc( sizeof(int) * yres)))
	return error=ImageFileErr_AllocFailed;
    bzero(offset,(size_t)(sizeof(int) * yres));

    /* write offset table as place holder */
    ::fwrite( offset, sizeof(int), yres, fp);

    if ( !(buf = (U_CHAR *)malloc( sizeof(U_CHAR) * xres * 5)))
	return error=ImageFileErr_AllocFailed;

    if ( !(red = (U_CHAR *)malloc( sizeof(U_CHAR) * xres * 4)))
	return error=ImageFileErr_AllocFailed;

  green = &red[xres];
  blue = &green[xres];
  matte = &blue[xres];

off=740+sizeof(int)*yres;

/* loop through window of image,
 * encode RGBM values into rla format
 */
for ( scan=0; scan < yres; scan++) {
	y=yres-scan-1;

	/* read in RGBM for each pixel */
	for ( x=0; x < xres; x++)
		idat->get_color(x,y,&red[x],&green[x],&blue[x],&matte[x]);

	/* record location in file */
	offset[scan] = off;

	/* write red scanline */
	len = encode( red, buf, xres);
	::fwrite( &len, sizeof(short), 1, fp);
	::fwrite( buf, sizeof(U_CHAR), (int)len, fp);
	off+=len+2;

	/* write green scanline */
	len = encode( green, buf, xres);
	::fwrite( &len, sizeof(short), 1, fp);
	::fwrite( buf, sizeof(U_CHAR), (int)len, fp);
	off+=len+2;

	/* write blue scanline */
	len = encode( blue, buf, xres);
	::fwrite( &len, sizeof(short), 1, fp);
	::fwrite( buf, sizeof(U_CHAR), (int)len, fp);
	off+=len+2;

	/* write matte scanline */
	len = encode( matte, buf, xres);
	::fwrite( &len, sizeof(short), 1, fp);
	::fwrite( buf, sizeof(U_CHAR), (int)len, fp);
	off+=len+2;

	// write aux channels
	if(rla_head.aux_type==4) {
		len=xres*sizeof(float);
		for(i=0;i<rla_head.num_aux;i++) {
			::fwrite( &len, sizeof(short), 1, fp);
			for(x=0; x < xres; x++) {
				ftemp=adat->get_float(x,y,i);
				::fwrite(&ftemp,sizeof(float),1,fp);
				}
			off+=len+2;
			}
		}
	else {	for(i=0;i<rla_head.num_aux;i++) {
			if(bytes==1) {
				for ( x=0; x < xres; x++)
					red[x]=adat->get_uchar(x,y,i);
				len = encode( red, buf, xres);
				::fwrite( &len, sizeof(short), 1, fp);
				::fwrite( buf, sizeof(U_CHAR), (int)len, fp);
				}
			else if(bytes==2) {
				for ( x=0; x < xres; x++) {
					ustemp=adat->get_ushort(x,y,i);
					red[x]=*((uchar *)&ustemp);
					green[x]=*(((uchar *)&ustemp)+1);
					}
				len=encode( red, buf, xres);
				len+=encode( green, buf+len, xres);
				::fwrite( &len, sizeof(short), 1, fp);
				::fwrite( buf, sizeof(U_CHAR), (int)len, fp);
				}
			else {	for ( x=0; x < xres; x++) {
					ultemp=adat->get_ulong(x,y,i);
					red[x]=*((uchar *)&ultemp);
					green[x]=*(((uchar *)&ultemp)+1);
					blue[x]=*(((uchar *)&ultemp)+2);
					matte[x]=*(((uchar *)&ultemp)+3);
					}
				len=encode( red, buf, xres);
				len+=encode( green, buf+len, xres);
				len+=encode( blue, buf+len, xres);
				len+=encode( matte, buf+len, xres);
				::fwrite( &len, sizeof(short), 1, fp);
				::fwrite( buf, sizeof(U_CHAR), (int)len, fp);
				}
			off+=len+2;
			}
		}
	}

	/* seek back to offset table, and write values */
	if(!::fseek( fp, (long)740, 0))
		::fwrite( offset, sizeof(int), yres, fp);

// free buffers
free(offset); free(buf);
free(red);

return error;
}
