// parameter.h

#ifndef	_PARAMETER_H_
#define _PARAMETER_H_

#include "data.h"

// The base class for parameters:


class param {

    protected:

	int	changed;

	Data	*dat;

	void	init() { changed=CHANGE_COUNTER; dat=NULL; }

    public:

	int	get_changed() { if(dat) return dat->get_changed();
				else return changed; }

	void	set_data(Data *d) { dat=d; }
	Data	*get_data() { return dat; }

	virtual	int	parse_in(Dataport *) { return(FALSE); }
	virtual	int	parse_out(Dataport *, int=FALSE) { return(FALSE); }

	virtual	Data	*envelope_create(Dataport *) { return NULL; }

	param() { init(); }

	};


// From param specific types of parameters can be derived:


class dparam : public param {

    protected:

	double	val;

	void	init() { val=0.0; }

    public:

	void	set_value(double v) {	if(dat) ((dData *)dat)->set_value(v);
					else {	val=v; changed=CHANGE_COUNTER; }
					}
	double	get_value() {	if(dat) return ((dData *)dat)->get_value();
				else	return val; }

	double	operator=(double v) { set_value(v); return v; }

	operator double() { return get_value(); }

	int	parse_in(Dataport *);
	int	parse_out(Dataport *, int expand=FALSE);

	Data	*envelope_create(Dataport *fp);

	dparam() { init(); }

	};


class iparam : public param {

    protected:

	int	val;

	void	init() { val=0; }

    public:

	void	set_value(int v) {	if(dat) ((iData *)dat)->set_value(v);
					else {	val=v; changed=CHANGE_COUNTER; }
					}
	int	get_value() {	if(dat) return ((iData *)dat)->get_value();
				else	return val; }

	int	operator=(int v) { set_value(v); return v; }

	operator int() { return get_value(); }

	int	parse_in(Dataport *);
	int	parse_out(Dataport *, int expand=FALSE);

	iparam() { init(); }

	};


class ldparam : public param {

    protected:

	double	*val;
	int	num;

	int	index;

	void	init() { val=NULL; num=0; }

	void	cleanup() { if(val) free(val); val=NULL; num=0; changed=CHANGE_COUNTER; }

    public:

	void	set_value(int i, double v) {
					if(dat) ((ldData *)dat)->set_value(i,v);
					else {	val[i]=v; changed=CHANGE_COUNTER; }
					}
	double	get_value(int i) {	if(dat) return ((ldData *)dat)->get_value(i);
					else	return val[i]; }

	void	set_num(int n) {	if(dat) ((ldData *)dat)->set_num(n);
					else {	num=n; val=(double *)realloc(val,num*sizeof(double));
						changed=CHANGE_COUNTER; }
					}
	int	get_num() {	if(dat) return ((ldData *)dat)->get_num();
				else	return num; }

	void	add_value(double v) {	if(dat) ((ldData *)dat)->add_value(v);
					else {	set_num(num+1); val[num-1]=v; } }

	void	free_list() {	if(dat) ((ldData *)dat)->free_list();
				else	cleanup(); }

	ldparam	&operator[](int i) { index=i; return *this; }

	double	operator=(double v) { set_value(index,v); return v; }

	operator double() { return get_value(index); }

	int	parse_in(Dataport *);
	int	parse_out(Dataport *, int expand=FALSE);

	ldparam() { init(); }
	~ldparam() { cleanup(); }
	};


class liparam : public param {

    protected:

	int	*val;
	int	num;

	int	index;

	void	init() { val=NULL; num=0; }

	void	cleanup() { if(val) free(val); val=NULL; num=0; changed=CHANGE_COUNTER; }

    public:

	void	set_value(int i, int v) {
					if(dat) ((liData *)dat)->set_value(i,v);
					else {	val[i]=v; changed=CHANGE_COUNTER; }
					}
	int	get_value(int i) {	if(dat) return ((liData *)dat)->get_value(i);
					else	return val[i]; }

	void	set_num(int n) {	if(dat) ((liData *)dat)->set_num(n);
					else {	num=n; val=(int *)realloc(val,num*sizeof(int));
						changed=CHANGE_COUNTER; }
					}
	int	get_num() {	if(dat) return ((liData *)dat)->get_num();
				else	return num; }

	void	add_value(int v) {	if(dat) ((liData *)dat)->add_value(v);
					else {	set_num(num+1); val[num-1]=v; } }

	void	free_list() {	if(dat) ((liData *)dat)->free_list();
				else	cleanup(); }

	liparam	&operator[](int i) { index=i; return *this; }

	int	operator=(int v) { set_value(index,v); return v; }

	operator int() { return get_value(index); }

	int	parse_in(Dataport *);
	int	parse_out(Dataport *, int expand=FALSE);

	liparam() { init(); }
	~liparam() { cleanup(); }
	};


class lliparam : public param {

    protected:

	int	**val;
	int	*size;
	int	num;

	int	index1,index2;

	void	init() { val=NULL; size=NULL; num=0; }

	void	cleanup() {	int i;
				if(val) {
				    for(i=0;i<num;i++) if(val[i]) free(val[i]);
				    free(val); val=NULL; }
				if(size) free(size); size=NULL; num=0;
				changed=CHANGE_COUNTER; }

    public:

	void	set_value(int i1, int i2, int v) {
				if(dat) ((lliData *)dat)->set_value(i1,i2,v);
				else {	val[i1][i2]=v; changed=CHANGE_COUNTER; }
				}
	int	get_value(int i1, int i2) {
				if(dat) return ((lliData *)dat)->get_value(i1,i2);
				else	return val[i1][i2]; }

	void	set_num(int n) { int i;
				if(dat) ((lliData *)dat)->set_num(n);
				else {	if(n<num) {
						for(i=n;i<num;i++) if(val[i]) free(val[i]); }
					val=(int **)realloc(val,n*sizeof(int *));
					size=(int *)realloc(size,n*sizeof(int));
					if(n>num) for(i=num;i<n;i++) {
						val[i]=NULL; size[i]=0; }
					num=n; changed=CHANGE_COUNTER; }
					}
	int	get_num() {	if(dat) return ((lliData *)dat)->get_num();
				else	return num; }

	void	set_size(int i, int s) {
				if(dat) ((lliData *)dat)->set_size(i,s);
				else {	size[i]=s; val[i]=(int *)realloc(val[i],size[i]*sizeof(int));
					changed=CHANGE_COUNTER; }
				}

	int	get_size(int i) {	if(dat) return ((lliData *)dat)->get_size(i);
					else return size[i]; }

	void	add_list(int s=0) {	if(dat) ((lliData *)dat)->add_list(s);
					else {	set_num(num+1);
						if(s) set_size(num-1,s); }
					}

	void	add_value(int i, int v) {
					if(dat) ((lliData *)dat)->add_value(i,v);
					else {	set_size(i,size[i]+1); val[i][size[i]-1]=v; } }

	void	free_list(int i) {	if(dat) ((lliData *)dat)->free_list(i);
					if(val[i]) free(val[i]); size[i]=0;
					changed=CHANGE_COUNTER; }

	void	free_all() {	if(dat) ((lliData *)dat)->free_all();
				else	cleanup(); }

	lliparam &operator()(int i, int j) { index1=i; index2=j; return *this; }

	int	operator=(int v) { set_value(index1,index2,v); return v; }

	operator int() { return get_value(index1,index2); }

	lliparam() { init(); }
	~lliparam() { cleanup(); }
	};


class Image;

class imgparam : public param {

    protected:

	Image	*img;

	void	init() { img=NULL; }

    public:

	void	set_value(Image *i) {	if(dat) ((imgData *)dat)->set_value(i);
					else {	img=i; changed=CHANGE_COUNTER; }
					}
	Image	*get_value() {	if(dat) return ((imgData *)dat)->get_value();
				else	return img; }

	Image	*operator=(Image *i) { set_value(i); return i; }

	operator Image*() { return get_value(); }

	int	parse_in(Dataport *);
	int	parse_out(Dataport *, int expand=FALSE);

	Data	*envelope_create(Dataport *fp);

	imgparam() { init(); }
	};


#endif
