#ifdef __cplusplus
extern "C" {
#endif
extern int dsOpen(char *);
extern int dsClose(int);
extern int requestHeader(int, int *, int *, int *, int *, int *, int *, int *);
extern int requestValue(int, int, int, long, long, void **);
extern int requestPixel(int, int, long, long, void **);

extern int writen(int, char *, int);
extern int readn(int, char *, int);
extern int err_sys(char *);
#ifdef __cplusplus
}
#endif


