#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "zvproto.h"
#include "zifmessage.h"
#include "vicmain_c.h"
#include "zmabend.h"
#include "zvprintf.h"

static void add_image(char * name_buf, float * out_buf, int * pix_count, int nl, int ns, unsigned char * line_buf);
static void add_to_image(int iunit, float * out_buf, int * pix_count, int nl, int ns, unsigned char * line_buf);

void main44(void)
{
  char list_filename[100];
  int parmct, parmdf;
  FILE * list_file = NULL;
  int iunit, ounit;
  float * out_buf = NULL;
  int * pix_count = NULL;
  unsigned char * line_buf = NULL;
  int nl, ns, nb;
  char format[33];
  char inp_name[132];

  zifmessage("AVGPIX version 2020-05-14");

  // get list filename
  zvparm ("list", list_filename, &parmct, &parmdf, 1, 99);

  zvnprintf(200, "Reading list file %s", list_filename);

  // open inp
  if (zvunit(&iunit, "INP", 1, NULL) != 1)
    zmabend("Error zvuniting input");
  if (zvopen(iunit, "OPEN_ACT", "SA", "IO_ACT", "SA", NULL) != 1)
    zmabend("Error opening input");
    
  // get format, nl, ns, nb, name from inp
  if (zvget(iunit, "FORMAT", format, "NL", &nl, "NS", &ns, "NB", &nb, "NAME", inp_name, NULL) != 1)
    zmabend("Error reading inp image format and size");

  zvnprintf(200, "Using input image: %s for output labeling", inp_name);

  if (strcmp(format, "BYTE"))
    zmabend("Non-BYTE images not supported");

  if (nb != 1)
    zmabend("Multi-band images not supported");

  // open out
  if (zvunit(&ounit, "OUT", 1, NULL) != 1)
    zmabend("Error zvuniting output");
  if (zvopen(ounit, "OP", "WRITE", "U_FORMAT","REAL","O_FORMAT", "BYTE",
	     "OPEN_ACT", "SA", "IO_ACT", "SA", NULL) != 1)
    zmabend("Error opening output");
    
  // allocate line_buf for reading inp
  if (! (line_buf = new unsigned char[nl]))
    zmabend("Error allocating line_buf");
  memset(line_buf, 0, nl * sizeof(unsigned char));

  // allocate out_buf for out image
  if (! (out_buf = new float[nl * ns]))
    zmabend("Error allocating out_buf");
  memset(out_buf, 0, nl * ns * sizeof(float));

  // allocate valid pix count image
  if (! (pix_count = new int[nl * ns]))
    zmabend("Error allocating out_buf");
  memset(pix_count, 0, nl * ns * sizeof(int));

  zvclose(iunit, NULL);

  /* open list file */
  if (! (list_file = fopen (list_filename, "rb")))
    zvnabend(200, "Error opening %s", list_filename);

  const int name_buf_size = 200;
  char name_buf[name_buf_size];
  while (!feof(list_file)) {
    if (fgets(name_buf, name_buf_size, list_file) != NULL &&
	name_buf[0] != '#') {	// skip commented names
      if (name_buf[strlen(name_buf) - 1] == '\n')
	name_buf[strlen(name_buf) - 1] = '\0';
      add_image(name_buf, out_buf, pix_count, nl, ns, line_buf);
    }
  }

  // divide output by count
  for (int line = 0; line < nl; ++line) {
    for (int samp = 0; samp < ns; ++samp) {
      int count = pix_count[line * ns + samp];
      if (count > 0)
	out_buf[line * ns + samp] /= count;
    }
  }
      
  // write output
  for (int line = 0; line < nl; ++line)
    zvwrit(ounit, (void*) (&out_buf[line * ns]), "LINE", line + 1, "SAMP", 1, "NSAMPS", ns, NULL);

  zvclose(ounit, NULL);
}

void add_image(char * name_buf, float * out_buf, int * pix_count, int nl, int ns, unsigned char * line_buf) {
  int _nl, _ns, _nb;
  int iunit;
  static int instance = 1;
  char format[33];

  zvnprintf(200, "Processing input image: %s", name_buf);

  if (zvunit(&iunit, "IGNORE", instance, "U_NAME", name_buf, NULL) != 1)
    zmabend("Error zvuniting secondary input");
  ++ instance;
  if (zvopen(iunit, "OPEN_ACT", "SA", "IO_ACT", "SA", NULL) != 1)
    zmabend("Error opening secondary input");
    
  // get format, nl, ns, nb from inp
  if (zvget(iunit, "FORMAT", format, "NL", &_nl, "NS", &_ns, "NB", &_nb, NULL) != 1)
    zmabend("Error reading inp image format and size");

  if (strcmp(format, "BYTE"))
    zmabend("Non-BYTE images not supported");

  if (_nb != 1)
    zmabend("Multi-band images not supported");

  if (_nl != nl || _ns != ns)
    zmabend("Input images must have same shape");

  // add valid input pixels to output buf
  add_to_image(iunit, out_buf, pix_count, nl, ns, line_buf);

  zvclose(iunit, NULL);
}

void add_to_image(int iunit, float * out_buf, int * pix_count, int nl, int ns, unsigned char * line_buf) {
  for (int line = 0; line < nl; ++line) {
    zvread(iunit, line_buf, "LINE", line + 1, "SAMP", 1, "NSAMPS", ns, NULL);
    for (int samp = 0; samp < ns; ++samp) {
      unsigned char in_pix = line_buf[samp];
      if (in_pix != 0) {
	out_buf[line * ns + samp] += in_pix;
	++ pix_count[line * ns + samp];
      }
    }
  }
}
