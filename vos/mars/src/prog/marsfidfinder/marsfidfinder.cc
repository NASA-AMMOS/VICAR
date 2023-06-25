// vim:set tabstop=4: 
// vim:set expandtab: 
// vim:set shiftwidth=4: 
/////////////////////////////////////////////////////////////////////////
// marsfidfinder - multimission fiduciary finder program              
////////////////////////////////////////////////////////////////////////

#include "vicmain_c"
#include "zvprintf.h"

#include "mars_support.h"
#include "mars_tiepoints.h"


#include "PigFileModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"
#include "PigVector.h"
#include "RadiometryModel.h"
#include "PigCoordSystem.h"
#include "PigMission.h"
#include "PigUtilities.h"
#include "PigXerces.h"
#include "PigCameraMapper.h"
#include "PigCameraMapEntry.h"

#include <string.h>
#include<sys/param.h>
#include <stdlib.h>

/* buffer sizes in main program */
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS

#define MAXLINELEN  80
#define MAXFIDS 6
#define MAX_INPUTS 2500


void find_fids(
	       PigCameraModel *camera_in[],
	       PigFileModel *file_models[], int band,
	       PigCoordSystem *cs,
	       int search_area_ns,int search_area_nl,double searchpcnt,
	       int imno,struct TiePoint *tiepoints,int *n_tiepoints,
	       double quality,double maskvalue);
int read_area(double  *area, int line, int samp,
	      PigFileModel *file_model, int band, int height, int width);

int maskedmatch(double *left, int nlw, int nsw, 
		double *right, int nlw2, int nsw2, 
		double *correl,
		double *line_offset, double *samp_offset,
		double line_coef[4], double samp_coef[4],
		double *quality, 
		double maskvalue,double searchpcnt);



////////////////////////////////////////////////////////////////////////
// MarsFIDFINDER program
////////////////////////////////////////////////////////////////////////

void main44()
{

  int count;

  // Inputs

  int nids;
  PigFileModel *file_models[MAXFIDS*MAX_INPUTS];
  PigCameraModel *camera_in[MAXFIDS*MAX_INPUTS];
  PigPointingModel *pointing_in[MAXFIDS*MAX_INPUTS];
  int homogeneous_inputs = TRUE;
  PigCoordSystem *cs;

  PigSurfaceModel *surface_model;
  // User Parameters

  char outfilename[MAXPATHLEN];
  int band;
  int search_area_ns;
  int search_area_nl;
  char mission[64], instrument[64];


  struct TiePoint tiepoints[MAX_INPUTS*MAXFIDS];
  int n_tiepoints;
  double minquality;
  int def;


  zvmessage("MARSFIDFINDER version 2020-03-20", "");

  zvp("SEARCHNS", &search_area_ns, &count);
  zvp("SEARCHNL", &search_area_nl, &count);
  zvp("OUT_TPT", outfilename, &count);
  // get parameter overrides if any
  zvp("BAND", &band, &count);
  zvparmd("QUALITY", &minquality, &count,&def,1,0);
  double  searchpcnt;
  zvparmd("SEARCHPCNT",&searchpcnt,&count,&def,1,0);
  double maskvalue;
  zvparmd("MASK_VALUE",&maskvalue,&count,&def,1,0);





  mars_setup(nids, file_models, camera_in, pointing_in,surface_model,
	     NULL, cs,
	     mission, instrument, homogeneous_inputs,
	     MAX_NL, MAX_NS, MAX_INPUTS);



  // Print out input status from labels
  mars_print_inputs(nids, pointing_in, camera_in, file_models,
		    homogeneous_inputs, mission, instrument);

  // go get em
  n_tiepoints=0;
  for (int searchno=0;searchno<nids;++searchno) {
    find_fids( camera_in,
	       file_models, band,
	       cs,search_area_ns,
	       search_area_nl,searchpcnt,searchno,&(tiepoints[0]),
	       &n_tiepoints,minquality,maskvalue);
  }
  int start_key;
  zvp("START_KEY", &start_key, &count);
  int status = mars_save_tiepoints(outfilename, tiepoints, n_tiepoints,
				   file_models, nids, start_key, cs);
  if (status != 0)
    zvnprintf(256, "Error saving tiepoints!! code=%d", status);

  return;

}

////////////////////////////////////////////////////////////////////////////
// determine the following
// mission,short_mission_name,host_id,serial_number,filterno
////////////////////////////////////////////////////////////////////////////

static void  get_mission_info( PigFileModel *file_models[],int imno,
			       char mission[256], char short_mission_name[256],
			       char host_id[256],char **serial_number,char **filterno) {

  int unit = -1, status = -1;
  const char *instrument=NULL;
  status= PigGetMission(file_models[imno]->getFilename(), &unit,
			mission, host_id);
  if(status != 0)
    zvnabend(PIG_MAX_FILENAME_SIZE+100,"PigGetMission failed  for %s\n",
	     file_models[imno]->getFilename());
  strcpy(short_mission_name, mission);
  char *p;
  if ((p = strchr(short_mission_name, ':')) != NULL)
    *p = '\0';                      // strip off the host_id
  instrument=file_models[imno]->getInstrumentId();
  PigXerces::initialize();
  PigCameraMapper *map = new PigCameraMapper(mission, host_id);
  *serial_number = NULL;
  *filterno=NULL;
  int has_filt = FALSE;
  if (instrument) {
    PigCameraMapEntry *entry = map->findFromID(instrument);
    if(!entry)
      zvnabend(PIG_MAX_FILENAME_SIZE+100,"couldnt open xml file for %s\n",instrument);
    *serial_number = strdup(entry->getSerialNumber());
    has_filt = entry->getFilters();
  }
  if (has_filt)
    *filterno=strdup(file_models[imno]->getFilterNumber());
  delete map;
  PigXerces::close();
}

///////////////////////////////////////////////////////////////////
//parse the fidinfo file. For each template retrieve xyz point and image name
//////////////////////////////////////////////////////////////////
static void get_fid_info(char * short_mission_name, char *host_id,
			 float *xs,float *ys,float *zs, int *numtemplates,
			 char images[MAXFIDS][MAXPATHLEN]) {
  char fidinfofilename[MAXPATHLEN];
  char path[PIG_MAX_FILENAME_SIZE];
  char fidline[MAXLINELEN+1];
  int fidno;

  snprintf(fidinfofilename,MAXPATHLEN,"param_files/%s_%s_fiducial_info.parms",
	  short_mission_name, host_id);
  FILE *fp= PigModelBase::openConfigFile(fidinfofilename, path);
  if(!fp)
    zvnabend(PIG_MAX_FILENAME_SIZE+100,"couldnt open %s\n",fidinfofilename);
  fidno=0;
  while(1) {
    fgets(fidline,MAXLINELEN,fp);
    if(feof(fp)) break;
    ++*numtemplates;
    int res=sscanf(fidline,"%f %f %f   %s",&xs[fidno],&ys[fidno],
		   &zs[fidno],images[fidno]);
    if (res != 4)
      zvnabend(PIG_MAX_FILENAME_SIZE+100,"error reading line %d of  %s\n",fidno,fidinfofilename);
    ++fidno;
    if (fidno == MAXFIDS) break;
  }
  fclose(fp);
}


/*************************************************************************/
/* Loop through a set of templates (specified in a text file) and search */
/* the image (deternined from the index value imno) for the location     */
/* with the highest correlation value.                                   */
/*************************************************************************/
void find_fids(
	       PigCameraModel *camera_in[],
	       PigFileModel *file_models[], int band,
	       PigCoordSystem *cs,
	       int search_area_ns,int search_area_nl,double searchpcnt,
	       int imno,struct TiePoint *tiepoints,int *n_tiepoints,
	       double minquality,double maskvalue)
{

  int i ;

  // double percent;
  // int limits;
  // int mode;
  // int kl = -1000;
  // int kr = -1000;
  int ind = 0;

  // variables for storing info about fiducials obtained from text file
  float xs[MAXFIDS];
  float ys[MAXFIDS];
  float zs[MAXFIDS];
  char images[MAXFIDS][MAXPATHLEN];

  char path[PIG_MAX_FILENAME_SIZE];

  double line2,samp2,line_offset=0, samp_offset=0;
  double line_coef[4],samp_coef[4];

  double *fid_template;
  double *search_image;
  double *correl;
  float           sep_angle;
  double          sep_cos;
  int border;
  int count;
  char verbosityparam[1024];
  int debug=0;

  int unit = -1, status = -1;
  // percent = 100.;
  // limits = 10000;
  double quality=0;
  // mode = 0;


  zvp("VERBOSITY", verbosityparam, &count);
  if(strcasecmp(verbosityparam,"DEBUG") == 0) debug=1;
  zvnprintf(PIG_MAX_FILENAME_SIZE+100,"Searching image %s for fiducials",
	    file_models[imno]->getFilename());

  // get info from image
  char short_mission_name[256];
  char mission[256],host_id[256];
  char *serial_number;
  char *filterno;
  get_mission_info(file_models,imno,mission,short_mission_name,host_id,
		   &serial_number,&filterno);


  if (file_models[imno]->getDownsampleXFactor(1.0) != 1.0 ||
      file_models[imno]->getDownsampleYFactor(1.0) != 1.0) {
    zvnprintf(PIG_MAX_FILENAME_SIZE+100,"Scaled images not supported\n");
    return;
  }
  // open  the image to be searched 
  status=zvopen(file_models[imno]->getUnit(), "OP", "READ",
		"U_FORMAT", "DOUB", "OPEN_ACT", "SA", NULL);
  if (status != 1)
    zvnabend(PIG_MAX_FILENAME_SIZE+100,"couldnt open search image\n");
  file_models[imno]->setFileOpen(TRUE);
  static int inst=1; //unit numbers

  //read info about approximate fid locations from flight or engineering file
  int numtemplates=0;
  get_fid_info(short_mission_name, host_id,
	       xs,ys,zs,&numtemplates,images);

  //template loop
  zvp("SEP_ANGLE", &sep_angle, &count);
  sep_cos = cos(PigDeg2Rad(sep_angle));
  zvp("BORDER", &border, &count); 
  for (i=0; i < numtemplates; i++) {

    // open template
    // construct filename
    char templatename[PIG_MAX_FILENAME_SIZE];
    snprintf(templatename,PIG_MAX_FILENAME_SIZE,"fiducials/%s_%s_SN_%s_F_%s.IMG",
	    short_mission_name, images[i],serial_number,filterno);
    if(debug)
      zvnprintf(PIG_MAX_FILENAME_SIZE+100,"Searching with  template %s",templatename);

    // quick sanity test
    PigPoint fiducial_pt(xs[i],ys[i],zs[i]);
    PigVector fiducial_vec = fiducial_pt - 
      camera_in[imno]->getCameraPosition(); 
    fiducial_vec.normalize(); 
    double dot = fiducial_vec % camera_in[imno]->getCameraOrientation();
    if (dot < sep_cos) {
      if (debug)
	zvnprintf(PIG_MAX_FILENAME_SIZE+100,"Ignoring ... too much separation");
      continue;            // skip this image 
    }

    // project template xyz to search image
    PigPoint surf_pt;
    surf_pt.setXYZ(xs[i],ys[i],zs[i]);

    int hits=1; //FIXME
    int nl,ns;
    double search_line=0,search_sample=0;
    camera_in[imno]->XYZtoLS(surf_pt, (hits<=0), &search_line, 
			     &search_sample, cs);
    search_line -= file_models[imno]->getYOffset();
    search_sample -=  file_models[imno]->getXOffset();
    search_line += 1; // convert to 1 based indexing
    search_sample += 1;
    if (debug)
      zvnprintf(PIG_MAX_FILENAME_SIZE+100,"Searching area around line=%f samp=%f",
		search_line,search_sample);

    if( search_sample < -border || 
	search_sample> file_models[imno]->getNS() + 
	border || search_line < -border || 
	search_line> file_models[imno]->getNL() + border) {
      if (debug)
	zvnprintf(PIG_MAX_FILENAME_SIZE+100,"Ignoring  template %s: too far from border\n",
		  templatename);
      continue;
    }

    FILE *f = PigModelBase::openConfigFile(templatename, path);
    if (f != NULL) {
      fclose(f);
      // re-open the flat field image using VICAR
      //zvunit(&unit, "rad_cal", inst++, "U_NAME", path, NULL);
      zvunit(&unit, "rad_cal", inst++, "U_NAME", path, NULL);
      status = zvopen(unit, "U_FORMAT", "DOUB",
		      "OPEN_ACT", "", "IO_ACT", "S", NULL);
    } else {
      zvnprintf(PIG_MAX_FILENAME_SIZE+100,"couldnt open template image %s. Skipping\n",templatename);
      continue;
    }
    zvget(unit, "NS", &ns, "NL", &nl, NULL);

    // determine the total search area from the template size and
    // desired search parameters

    search_area_nl = 2*search_area_nl + nl  ;
    search_area_ns = 2*search_area_ns + ns  ;
    // read  source image at approximate template location
    search_image= (double *)malloc(search_area_nl*search_area_ns*
				   sizeof(double));
    if (!search_image)
      zvnabend(PIG_MAX_FILENAME_SIZE+100,"couldnt  allocate memory for search region:"
	       " template %d\n",i);

    for(int j=0;j<search_area_nl*search_area_ns;++j)
      search_image[j] = maskvalue; //mask_value
    ind = read_area(search_image,
		    (int)(search_line - search_area_nl/2),
		    (int)(search_sample -search_area_ns/2),
		    file_models[imno], band, search_area_nl, search_area_ns);
    if (ind == 1)
      zvnabend(PIG_MAX_FILENAME_SIZE+100,"Invalid search area");

    //read in template
    fid_template= (double  *) malloc(nl*ns*sizeof(double));
    if (!fid_template)
      zvnabend(PIG_MAX_FILENAME_SIZE+100,"couldnt  allocate memory for template %d",i);

    for(int j=0;j<nl;++j) {
      status = zvread(unit,fid_template+j*ns, NULL);
      if (status != 1)
	zvnabend(PIG_MAX_FILENAME_SIZE+100,"Invalid read of %s: error code %d\n",
		 templatename,status);
    }

    float centroid_line_offset;
    status=zlget(unit, "property", "LINE", &centroid_line_offset, "format", 
		 "real", "property", "CENTROID_OFFSET", NULL);
    if (status != 1) {
      zvnprintf(PIG_MAX_FILENAME_SIZE+100,"%s missing centroid correction. Skipping \n",
		templatename);
      free(fid_template);
      free(search_image);
      zvclose(unit,NULL);
      continue;
    }
    float centroid_sample_offset;
    status=zlget(unit, "property", "SAMPLE", &centroid_sample_offset, 
		 "format", "real", "property", "CENTROID_OFFSET", NULL);
    if (status != 1) {
      zvnprintf(PIG_MAX_FILENAME_SIZE+100,"%s missing centroid correction. Skipping \n",
		templatename);
      free(fid_template);
      free(search_image);
      zvclose(unit,NULL);
      continue;
    }
    zvclose(unit,NULL);
    correl = (double *)malloc(search_area_nl*search_area_ns*
			      sizeof(double));
    if (!correl)
      zvnabend(PIG_MAX_FILENAME_SIZE+100,"couldnt  allocate memory for correlation matrix");

    line_coef[0]=0.;
    line_coef[1]=1.;
    line_coef[2]=0.;
    line_coef[3]=0.;
    samp_coef[0]=1.;
    samp_coef[1]=0.;
    samp_coef[2]=0.;
    samp_coef[3]=0.;
    ind = maskedmatch(fid_template, nl, ns,
		      search_image, search_area_nl, search_area_ns, 
		      correl, &line_offset, &samp_offset,
		      line_coef, samp_coef,
		      &quality, 
		      maskvalue,searchpcnt);

    free(search_image);
    free(fid_template);
    free(correl);
    if (debug)
      zvnprintf(PIG_MAX_FILENAME_SIZE+100,"maskedmask returned with quality %f",quality);

    if (ind == 0) {

      if (quality < minquality) continue; //ignore low quality matches
      zvnprintf(PIG_MAX_FILENAME_SIZE+100,"Found template %s in image",templatename);

      // compute template  image coordinates from offsets
      // adjust for the centroid not being in the center of the template
      line2 = search_line + line_offset +centroid_line_offset;
      samp2 = search_sample + samp_offset +centroid_sample_offset;
      tiepoints[*n_tiepoints].type=TIEPOINT_FIDUCIAL;
      tiepoints[*n_tiepoints].left_image=imno;
      tiepoints[*n_tiepoints].left_sample=samp2;
      tiepoints[*n_tiepoints].left_line=line2; 
      tiepoints[*n_tiepoints].corrected_sample=samp2;
      tiepoints[*n_tiepoints].corrected_line=line2;
      tiepoints[*n_tiepoints].xyz=PigPoint(xs[i],ys[i],zs[i]);
      tiepoints[*n_tiepoints].quality=quality;
      tiepoints[*n_tiepoints].interactive=FALSE;
      tiepoints[*n_tiepoints].active=TRUE;
      tiepoints[*n_tiepoints].cs=cs;

      if(debug) {
	zvnprintf(PIG_MAX_FILENAME_SIZE+100,"old line = %f sample = %f",search_line,search_sample);
	zvnprintf(PIG_MAX_FILENAME_SIZE+100,"new line = %f sample = %f",line2, samp2 );
      }

      ++ *n_tiepoints;

    } else
      zvnprintf(PIG_MAX_FILENAME_SIZE+100,"maskedmatch found a location on the border. ignoring");
  }
  // close  the search image 
  zvclose(file_models[imno]->getUnit(),NULL);
  file_models[imno]->setFileOpen(FALSE);



}

////////////////////////////////////////////////////////////////////////
// Reads an area from the file into a double-precision buffer.  Assumes
// that the file is open with U_FORMAT, DOUB.
// width/height are the sizes of the area to be read.
//
// line starting line of search image
// samp starting samp of search image
//height - height of search area
//width - width of search area
// Returns 0 for success, 1 for error.
////////////////////////////////////////////////////////////////////////

int read_area(double  *area, int line, int samp,
	      PigFileModel *file_model, int band, int height,
	      int width)
{

  // Convert line/samp to physical (0-based) coords, and the upper-left edge

  int line_offset=0;
  int samp_offset=0;
  int start_line = (int)((line - file_model->getYOffset()) );
  int start_samp = (int)((samp - file_model->getXOffset()) );

  if (start_line < 0) {
    line_offset=-start_line;
    height += start_line;
    start_line=0;
  }
  if (start_samp < 0) {
    samp_offset=-start_samp;
    start_samp=0;
  }
  if (start_line+height-1 >= file_model->getNL()) 
    height= file_model->getNL() - start_line-1;


  int readamount=width-samp_offset;
  if (start_samp+readamount >= file_model->getNS()) 
    readamount = file_model->getNS() - start_samp -1;
  if(height < 1 ||  readamount < 1) return 1;


  for (int i=0; i < height; i++) {
    zvread(file_model->getUnit(), 
	   area+((i+line_offset)*width + samp_offset),
	   //            "LINE", start_line + i + 1,
	   //           "SAMP", start_samp + 1,
	   "LINE", start_line + i ,
	   "SAMP", start_samp ,
	   "BAND", band, "NSAMPS", readamount, NULL);
  }
  return 0;
}

#include <math.h>


/* Function prototypes for this module */

// static double cost(double p[], int ndim, void *func_args);
static void maskedsearch_area(double *left_p,int nlw,int nsw,
			      double * right_p,int nlw2,int nsw2,double *correl,
			      double answer[6], double *quality,
			      double maskvalue,double searchpcnt,int *ind);

/************************************************************************/
/* A simple wrapper around maskedsearch_area                            */
/************************************************************************/


int maskedmatch(double *left_p, int nlw, int nsw, 
		double *right_p, int nlw2, int nsw2, 
		double *correl_p,
		double *line_offset, double *samp_offset,
		double line_coef[4], double samp_coef[4],
		double *quality,
		double maskvalue,double searchpcnt)
{

  double answer[8] = {0., 0., 0., 0., 0., 0., 0., 0.};



  /* Use conventional linear correlation scheme only */

  int ind;
  maskedsearch_area( left_p,nlw, nsw,right_p,nlw2,nsw2, correl_p,
		     answer,quality, maskvalue,searchpcnt,&ind);

  /* restore & return new polynomial coefficients */
  line_coef[0]=answer[0];
  line_coef[1]=answer[1];
  line_coef[2]=answer[2];
  samp_coef[0]=answer[3];
  samp_coef[1]=answer[4];
  samp_coef[2]=answer[5];
  line_coef[3]=answer[6];
  samp_coef[3]=answer[7];

  /* return offset from initial estimate, Note other polynomial terms are
     zero because multiply by x=y=0 at template origin */
  *line_offset=line_coef[2];
  *samp_offset=samp_coef[2];

  return ind;
}



/****************************************************************************/
/* Routine to perform conventional correlation only.			            */
/* Correl is filled with the matrix of correlation coefficients . */
/* Then the peak value is interpolated to sub pixel.			            */
/* Answer is returned with the equivalent polynomial mapping from	        */
/* left to right areas centered on the central pixel.			            */
/* This is modeled after a similar routine foound in gruen but it has been  */
/* optimized to remove some of the inner loop multiplications.              */
/************************************************************************/

static void maskedsearch_area(double *left_p,int nlw,int nsw,
			      double * right_p,int nlw2,int nsw2,double *correl,
			      double answer[6], double *quality,
			      double maskvalue,double searchpcnt,int *ind)
{
  double rn,sumy,sumxy,sumy2;
  double sumx2;
  double right_dn,left_dn;
  double r2,a,b,c;
  double numer,denom,sample,line;
  int i=0,j=0,m,n;
  double sumx;
  double left_sum;
  double *left_scan;
  double *right_scan;


  //rn=nlw * nsw;


  sumx=0.0;
  sumx2=0.0;

  //precompute left image values
  left_scan=left_p;
  for (j=0; j < nlw; j++) {
    for (i=0; i < nsw; i++) {
      left_dn=*left_scan++;
      sumx2 += left_dn*left_dn;
      sumx += left_dn;
    }
  }
  /* fill correlation matrix */
  double savesumx,savesumx2;
  for (n=0; n < nlw2-nlw+1; n++) {
    for (m=0; m < nsw2-nsw+1; m++) {
      // do a quick test to see if there is sufficient data to
      // perform the correlation
      rn=0;
      for (j=0; j < nlw; j++) { 
	right_scan=right_p + (j+n)*nsw2+m;
	for (i=0; i < nsw; i++) {
	  right_dn=*right_scan++;
	  if(right_dn !=maskvalue ) ++rn;
	}
      }
      if((100.0*rn)/(nlw* nsw) < searchpcnt) { 
	continue;
      }
      rn=0;
      sumy=0.0;
      sumy2=0.0;
      sumxy=0.0;
      savesumx=sumx;
      savesumx2=sumx2;
      left_scan=left_p;
      for (j=0; j < nlw; j++) {
	right_scan=right_p + (j+n)*nsw2+m;
	for (i=0; i < nsw; i++) {
	  right_dn=*right_scan++;
	  left_dn=*left_scan++;
	  // the next line is an addition to the regular gruen
	  // and may effect performance. This is the ONLY addition
	  // added to handle targets on the edge of the image
	  // and masked templates
	  if(right_dn !=maskvalue && 
	     left_dn !=maskvalue) {
	    sumy += right_dn;
	    sumy2 += right_dn*right_dn;
	    sumxy += right_dn*left_dn;
	    ++rn;
	  } 
	  else { //masked case
	    savesumx -= left_dn;
	    savesumx2  -= left_dn*left_dn;
	  }
	}
      }
      if (rn == 0) {
	correl[n*nsw2+m] = -1.0;
	continue; 
      } 
      left_sum=savesumx2-savesumx*savesumx/rn;
      if( left_sum == 0) {
	correl[n*nsw2+m] = -1.0;
	continue; 
      }
      numer = sumxy-savesumx*sumy/rn;
      //correl[n*nsw2+m] = numer*numer / 
      //  (left_sum*(sumy2-sumy*sumy/rn));
      if(sumy2-sumy*sumy/rn == 0) {
	correl[n*nsw2+m] = -1.0;
	continue; 
      }
      correl[n*nsw2+m] = numer / 
	(sqrt(left_sum)*sqrt((sumy2-sumy*sumy/rn)));
    }
  }
  /* locate highest value */
  r2= -1.0;
  for (n=0; n < nlw2-nlw+1; n++) {
    for (m=0; m < nsw2-nsw+1; m++) {
      double cor=correl[n*nsw2+m];
      if (r2 < cor) {
	r2=cor;
	j=n;
	i=m;
      }
    }
  }
  /* locate best area fit */

  /* reject point if on border */
  if ((j == 0) || (j == nlw2-nlw+1) ||
      (i == 0) || (i == nsw2-nsw+1)) {
    printf("i = %d j = %d %f\n",i,j,r2);
    *ind = 1;
    return;
  }
  *quality=correl[j*nsw2+i];

  /* compute sub-pixel location of best correlation.
     See Numerical Recipes, eqn: 10.2.1, note b+1/2 should read b-1/2   */
  a=correl[j*nsw2+i-1];
  b=2.0*correl[j*nsw2+i-1];
  c=correl[j*nsw2+ i+1];
  denom=2.0*(b-c-a);
  if (denom != 0.0) {
    sample=(c-a)/denom+i+(nsw-1)/2;}
  else{
    sample=i+(nsw-1)/2;
  }
  a=correl[(j-1)*nsw2+i];
  c=correl[(j+1)*nsw2+i];
  denom=2.0*(b-c-a);
  if (denom != 0.0) {
    line=(c-a)/denom+j+(nlw-1)/2;}
  else{
    line=j+(nlw-1)/2;
  }

  answer[0]=0.0;
  answer[1]=1.0;
  answer[2]=line-(nlw2-1)/2 ;
  answer[3]=1.0;
  answer[4]=0.0;
  answer[5]=sample-(nsw2-1)/2 ;
  //if( nlw2 % 2 == 0) answer[2] -= 1/2;
  //if( nsw2 % 2 == 0) answer[5] -= 1/2;

  *ind = 0;
}
