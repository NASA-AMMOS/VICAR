int csvd_sizes(int xr, int xc, int *ur_adr, int *uc_adr, int *sr_adr, int *sc_adr, int *vr_adr, int *vc_adr);
int csvd_alloc(int n, int p, double **U_adr, double **S_adr, double **V_adr);
int csvd(double *x, int n, int p, double *u, double *s, double *v);

