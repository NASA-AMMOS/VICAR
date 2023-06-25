#include "JPLStereo.h"
#include <string.h>    // for memset


// To avoid thrashing the heap, we allocate some memory in static bufffers.
// This macro allows a buffer to be reused in place when possible, or grown
// to the needed size.  When debugging memory, output a diagnostic each
// time the buffer grows.  Note that if no memory is allocated, the existing
// memory will be zeroed out.

#ifdef MEM_DEBUG
#define PREPARE_STATIC_BUFFER(type,buf,sizevar,newsize) \
	if ((buf) == NULL || (sizevar) < (newsize)) { \
	  if (buf) DELETEV (mm, buf); \
	  sizevar = (newsize); \
	  printf ( ">>> Allocating %s: %ld bytes\n", # buf, newsize * sizeof(type));\
	  buf = NEW(mm, #buf "(" #type ")") type[sizevar]; \
	} else { memset ((char *) buf, 0, newsize); }
#else
#define PREPARE_STATIC_BUFFER(type,buf,sizevar,newsize) \
	if ((buf) == NULL || (sizevar) < (newsize)) { \
	  if (buf) DELETEV (mm, buf); \
	  sizevar = (newsize); \
	  buf = NEW(mm, #buf "(" #type ")") type[sizevar]; \
	} else { memset ((char *) buf, 0, newsize); }
#endif





// Static buffers used in the stereo computation, used to
// avoid trashing the heap.  On VxWorks, you're better off
// doing this than risking heap fragmentation.
//
// Intermediate image buffers.  We could probably use even
// fewer of these if necessary.
//
// These pic* buffers are only allocated and sized once.  If
// you need to resize them, you must call ResizeImageBuffers
// explicitly.

static JPLPic *pic1 = NULL, *pic2 = NULL, *pic3 = NULL, *pic4 = NULL;
static JPLPic *pic5 = NULL, *pic6 = NULL, *pic7 = NULL, *pic8 = NULL;
// pic7 leftRawRectPic, pic8 rigt
static JPLPic *pic9 = NULL, *pic10 = NULL; // pic9 is subDispPic
// pic10 rangePic

// These buffers are resized each time they're used.

static long *same = NULL;
static int *region = NULL;
static float *fwd_col_sum = NULL;
static float *fwd_col_sum_sq = NULL;
static long same_size = 0L, region_size = 0L;
static long fwd_col_sum_size = 0L;
static long fwd_col_sum_sq_size = 0L;


// columns: For each useful pixel in a scanline, one slot for
// each disparity.  pixels within the row are adjacent, the
// disparities are the major axis.
static short *columns = NULL;
static long columns_size = 0L;

static short *disparity_space = NULL;
static long disparity_space_size = 0L;

static RejFlagT *rejected_flag = NULL;
static long rejected_flag_size = 0L;

static MinRecordPtr rightRecord = NULL;
static MinFullRecordPtr leftRecord = NULL;
static long rightRecord_size = 0L, leftRecord_size = 0L;

// uc_buffer: span of useful pixels horizontally, vertical
// window size, number of integer disparities searched.
// Disparities are the major axis, row within the window is
// next, and scanline-adjacent pixels are next to each other
// in memory.
static unsigned char *uc_buffer = NULL;
static long uc_buffer_size = 0L;


MinRecordPtr JPLStereo::GetMinRecordBuffer (int size)
{
  PREPARE_STATIC_BUFFER (MinRecord, rightRecord, rightRecord_size, size);

  return rightRecord;
}

MinFullRecordPtr JPLStereo::GetMinFullRecordBuffer (int size)
{
  PREPARE_STATIC_BUFFER (MinFullRecord, leftRecord, leftRecord_size, size);

  return leftRecord;
}

unsigned char *JPLStereo::GetUCBuffer (int size)
{
  PREPARE_STATIC_BUFFER (unsigned char, uc_buffer, uc_buffer_size, size);

  return uc_buffer;
}

short *JPLStereo::GetColumnsBuffer (int size)
{
  PREPARE_STATIC_BUFFER (short, columns, columns_size, size);

  return columns;
}


short *JPLStereo::GetDisparitySpaceBuffer (int size)
{
  PREPARE_STATIC_BUFFER (short, disparity_space, disparity_space_size, size);

  return disparity_space;
}



RejFlagT *JPLStereo::GetRejFlagBuffer (int size)
{
  PREPARE_STATIC_BUFFER (RejFlagT, rejected_flag, rejected_flag_size, size);

  return rejected_flag;
}


long *JPLStereo::GetSameBuffer (int size)
{
  PREPARE_STATIC_BUFFER (long, same, same_size, size);

  return same;
}



int *JPLStereo::GetRegionBuffer (int size)
{
  PREPARE_STATIC_BUFFER (int, region, region_size, size);

  return region;
}



float *JPLStereo::GetFwdColSumBuffer (int size)
{
  PREPARE_STATIC_BUFFER (float, fwd_col_sum, fwd_col_sum_size, size);

  return fwd_col_sum;
}



float *JPLStereo::GetFwdColSumSqBuffer (int size)
{
  PREPARE_STATIC_BUFFER (float, fwd_col_sum_sq, fwd_col_sum_sq_size, size);

  return fwd_col_sum_sq;
}



JPLPic *JPLStereo::GetPic1 (void)
{
  if (pic1 == NULL)
    pic1 = NEW(mm, "pic1") JPLPic(mm);
  return pic1;
}

JPLPic *JPLStereo::GetPic2 (void)
{
  if (pic2 == NULL)
    pic2 = NEW(mm, "pic2") JPLPic(mm);
  return pic2;
}


JPLPic *JPLStereo::GetPic3 (void)
{
  if (pic3 == NULL)
    pic3 = NEW(mm, "pic3") JPLPic(mm);
  return pic3;
}


JPLPic *JPLStereo::GetPic4 (void)
{
  if (pic4 == NULL)
    pic4 = NEW(mm, "pic4") JPLPic(mm);
  return pic4;
}


JPLPic *JPLStereo::GetPic5 (void)
{
  if (pic5 == NULL)
    pic5 = NEW(mm, "pic5") JPLPic(mm);
  return pic5;
}


JPLPic *JPLStereo::GetPic6 (void)
{
  if (pic6 == NULL)
    pic6 = NEW(mm, "pic6") JPLPic(mm);
  return pic6;
}


JPLPic *JPLStereo::GetPic7 (void)
{
  if (pic7 == NULL)
    pic7 = NEW(mm, "pic7") JPLPic(mm);
  return pic7;
}


JPLPic *JPLStereo::GetPic8 (void)
{
  if (pic8 == NULL)
    pic8 = NEW(mm, "pic8") JPLPic(mm);
  return pic8;
}


JPLPic *JPLStereo::GetPic9 (void)
{
  if (pic9 == NULL)
    pic9 = NEW(mm, "pic9") JPLPic(mm);
  return pic9;
}


JPLPic *JPLStereo::GetPic10 (void)
{
  if (pic10 == NULL)
    pic10 = NEW(mm, "pic10") JPLPic(mm);
  return pic10;
}



int JPLStereo::ResizeImageBuffers (long new_rows, long new_cols,
					  int pyrlevel)
{
  if (pic1 == NULL)
    pic1 = NEW(mm, "pic1") JPLPic(mm);
  if (pic1 == NULL ||
      pic1->Init (new_rows, new_cols, UC8_PIXEL) != NO_ERR) {
    char buf[100];
    snprintf (buf, 100, "Cannot resize pic1 temp space (%ldx%ld)\n",
	     new_rows, new_cols);
    FatalErr (buf);
    return MEM_ERR;
  }
  
  if (pic2 == NULL)
    pic2 = NEW(mm, "pic2") JPLPic(mm);
  if (pic2 == NULL ||
      pic2->Init (new_rows, new_cols, UC8_PIXEL) != NO_ERR) {
    char buf[100];
    snprintf (buf, 100, "Cannot resize pic2 temp space (%ldx%ld)\n",
	     new_rows, new_cols);
    FatalErr (buf);
    return MEM_ERR;
  }
  
  if (pic3 == NULL)
    pic3 = NEW(mm, "pic3") JPLPic(mm);
  if (pic3 == NULL ||
      pic3->Init (new_rows, new_cols, UC8_PIXEL) != NO_ERR) {
    char buf[100];
    snprintf (buf, 100, "Cannot resize pic3 temp space (%ldx%ld)\n",
	     new_rows, new_cols);
    FatalErr (buf);
    return MEM_ERR;
  }
  
  if (pic4 == NULL)
    pic4 = NEW(mm, "pic4") JPLPic(mm);
  if (pic4 == NULL ||
      pic4->Init (new_rows, new_cols, UC8_PIXEL) != NO_ERR) {
    char buf[100];
    snprintf (buf, 100, "Cannot resize pic4 temp space (%ldx%ld)\n",
	     new_rows, new_cols);
    FatalErr (buf);
    return MEM_ERR;
  }
  
  if (pic5 == NULL)
    pic5 = NEW(mm, "pic5") JPLPic(mm);
  if (pic5 == NULL ||
      pic5->Init (new_rows, new_cols, UC8_PIXEL) != NO_ERR) {
    char buf[100];
    snprintf (buf, 100, "Cannot resize pic5 temp space (%ldx%ld)\n",
	     new_rows, new_cols);
    FatalErr (buf);
    return MEM_ERR;
  }
  
  if (pic6 == NULL)
    pic6 = NEW(mm, "pic6") JPLPic(mm);
  if (pic6 == NULL ||
      pic6->Init (new_rows, new_cols, UC8_PIXEL) != NO_ERR) {
    char buf[100];
    snprintf (buf, 100, "Cannot resize pic6 temp space (%ldx%ld)\n",
	     new_rows, new_cols);
    FatalErr (buf);
    return MEM_ERR;
  }
  
  // pic7 is the leftRawRectPic
  if (pic7 == NULL)
    pic7 = NEW(mm, "pic7") JPLPic(mm);
  if (pic7 == NULL ||
      pic7->Init (new_rows, new_cols, UC8_PIXEL) != NO_ERR) {
    char buf[100];
    snprintf (buf, 100, "Cannot resize pic7 temp space (%ldx%ld)\n",
	     new_rows, new_cols);
    FatalErr (buf);
    return MEM_ERR;
  }
  
  // pic8 is the rightRawRectPic
  if (pic8 == NULL)
    pic8 = NEW(mm, "pic8") JPLPic(mm);
  if (pic8 == NULL ||
      pic8->Init (new_rows, new_cols, UC8_PIXEL) != NO_ERR) {
    char buf[100];
    snprintf (buf, 100, "Cannot resize pic8 temp space (%ldx%ld)\n",
	     new_rows, new_cols);
    FatalErr (buf);
    return MEM_ERR;
  }

  // pic9 is the subDispPic.
  if (pic9 == NULL)
    pic9 = NEW(mm, "pic9") JPLPic(mm);
  if (pic9 == NULL ||
      pic9->Init (new_rows, new_cols, INT16_PIXEL) != NO_ERR) {
    char buf[100];
    snprintf (buf, 100, "Cannot resize pic9 temp space (%ldx%ld)\n",
	     new_rows, new_cols);
    return MEM_ERR;
  }

  // pic10 is the static space for the rangePic.  Don't explicitly allocate
  // it here, only resize if it already exists.
  
  if (pic10) {
    if (pic10->Init ((new_rows+1) >> pyrlevel, (new_cols+1) >> pyrlevel,
		     XYZ_FLOAT_PIXEL) != NO_ERR) {
      char buf[100];
      snprintf (buf, 100, "Cannot resize pic10 temp space (%ldx%ld)\n",
	       new_rows, new_cols);
      return MEM_ERR;
    }
  }

  if (leftRectPic) {
    if (leftRectPic->Init (new_rows, new_cols, UC8_PIXEL) !=
	NO_ERR) {
      char buf[100];
      snprintf (buf, 100, "Cannot resize leftRectPic space (%ldx%ld)\n",
	       new_rows, new_cols);
      return MEM_ERR;
    }
  }


  if (rightRectPic) {
    if (rightRectPic->Init (new_rows, new_cols, UC8_PIXEL) !=
	NO_ERR) {
      char buf[100];
      snprintf (buf, 100, "Cannot resize rightRectPic space (%ldx%ld)\n",
	       new_rows, new_cols);
      FatalErr (buf);
      return MEM_ERR;
    }
  }


  if (leftRawRectPic) {
    if (leftRawRectPic->Init (new_rows, new_cols, UC8_PIXEL) !=
	NO_ERR) {
      char buf[100];
      snprintf (buf, 100, "Cannot resize leftRawRectPic space (%ldx%ld)\n",
	       new_rows, new_cols);
      FatalErr (buf);
      return MEM_ERR;
    }
  }


  if (rightRawRectPic) {
    if (rightRawRectPic->Init (new_rows, new_cols, UC8_PIXEL) !=
	NO_ERR) {
      char buf[100];
      snprintf (buf, 100, "Cannot resize rightRawRectPic space (%ldx%ld)\n",
	       new_rows, new_cols);
      FatalErr (buf);
      return MEM_ERR;
    }
  }

  return NO_ERR;
}



void JPLStereo::FreeStaticBuffers (void)
{
  FreeJPLStereoStaticBuffers (mm);
}



void FreeJPLStereoStaticBuffers (JMemoryManager *mm)
{
  if (pic1) DELETE(mm, pic1);
  if (pic2) DELETE(mm, pic2);
  if (pic3) DELETE(mm, pic3);
  if (pic4) DELETE(mm, pic4);
  if (pic5) DELETE(mm, pic5);
  if (pic6) DELETE(mm, pic6);
  if (pic7) DELETE(mm, pic7);
  if (pic8) DELETE(mm, pic8);
  if (pic9) DELETE(mm, pic9);
  if (pic10) DELETE(mm, pic10);
  // All the RectPic's point back to one of the pic# buffers, so don't free
  // them or you'll cause a memPartFree error
  //  if (leftRectPic) DELETE(mm, leftRectPic);
  //  if (rightRectPic) DELETE(mm, rightRectPic);
  //  if (leftRawRectPic) DELETE(mm, leftRawRectPic);
  //  if (rightRawRectPic) DELETE(mm, rightRawRectPic);
  // The *RectCam models should be deleted by the calling routine
  //  if (leftRectCam) DELETE (mm, leftRectCam);
  //  if (rightRectCam) DELETE (mm, rightRectCam);
  if (rightRecord) DELETEV(mm, rightRecord);
  if (leftRecord) DELETEV(mm, leftRecord);
  if (uc_buffer) DELETEV(mm, uc_buffer);
  if (disparity_space) DELETEV(mm, disparity_space);
  if (rejected_flag) DELETEV(mm, rejected_flag);
  if (fwd_col_sum) DELETEV(mm, fwd_col_sum);
  if (fwd_col_sum_sq) DELETEV(mm, fwd_col_sum_sq);
  if (columns) DELETEV(mm, columns);
  if (same) DELETEV(mm, same);
  if (region) DELETEV(mm, region);
}




