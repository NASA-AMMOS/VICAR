// support functions for terrain merge master and standalone slave.
#include "cahvor.h"

void get_pds_cmod(PDSFile *f, CAHVOR *cmod);
void cmod_rov_to_site(PDSFile *f, CAHVOR *cmod);
int get_pds_site(PDSFile *f);
void get_site_vector(const char *svf_file, int sitenum, double site[3]);
