// d_envelope.h

#ifndef	_D_ENVELOPE_H_
#define _D_ENVELOPE_H_

#include "dataport.h"
#include "grape/envelope.h"
#include "path/wvspline.h"
#include "path/survspline.h"

class dEdigital : public dEnvelope {

    protected:

	double	*data;
	int	num;

	int	alloc_flag;

	double	start,delta;

	int	interp;

	int	changed;

	void	init() { changed=CHANGE_COUNTER; data=NULL; num=0;
			 start=0.0; delta=1.0; alloc_flag=1; interp=1; }

	void	cleanup() { if(data && alloc_flag) free(data);
			    data=NULL; num=0; alloc_flag=1; }

    public:

	void	set_num(int n) { num=n; data=(double *)realloc(data,num*sizeof(double));
				 changed=CHANGE_COUNTER; }

	int	get_num() { return num; }

	void	set_data(double *d, int n) { data=d; num=n;
					     alloc_flag=0; changed=CHANGE_COUNTER; }

	double	*get_data() { return data; }

	void	free_data() { cleanup(); changed=CHANGE_COUNTER; }

	void	set_value_at_index(int i, double v) { data[i]=v; changed=CHANGE_COUNTER; }

	double	get_value_at_index(int i) { return data[i]; }

	void	set_start(double s) { start=s; changed=CHANGE_COUNTER; }

	double	get_start() { return start; }

	void	set_delta(double d) { delta=d; changed=CHANGE_COUNTER; }

	double	get_delta() { return delta; }

	void	set_interpolation(int i) { interp=i; changed=CHANGE_COUNTER; }

	int	get_interpolation() { return interp; }

	int     parse_in(Dataport *fp);
	int     parse_out(Dataport *fp, int expand=FALSE);


	// from dEnvelope:

	void	set_value_at_time(double t, double val);

	double	get_value_at_time(double t);

	// from dData:

	void	set_value(double v) { set_value_at_time(*clk,v); }

	double	get_value() { return get_value_at_time(*clk); }

	// from Data:

	int	get_changed() { int c=clk->get_changed(); return c>changed?c:changed; }

	dEdigital() { init(); }

	~dEdigital() { cleanup(); }
	};



class dElinear : public dEnvelope {

    protected:

	double	*data;
	double	*time;
	int	num;

	int	alloc_flag;

	int	interp;

	int	changed;

	void	init() { changed=CHANGE_COUNTER; data=NULL; time=NULL; num=0;
			 alloc_flag=1; interp=1; }

	void	cleanup() { if(data && alloc_flag) free(data);
			    if(time && alloc_flag) free(time);
			    data=NULL; time=NULL; num=0; alloc_flag=1; }

    public:

	int	get_num() { return num; }

	void	set_data(double *d, double *t, int n) { data=d; time=t; num=n;
				alloc_flag=0; changed=CHANGE_COUNTER; }

	void	get_data(double **d, double **t) { *d=data; *t=time; }

	void	free_data() { cleanup(); changed=CHANGE_COUNTER; }

	void	set_value_at_index(int i, double v) { data[i]=v; changed=CHANGE_COUNTER; }

	double	get_value_at_index(int i) { return data[i]; }

	void	set_time_at_index(int i, double t) { time[i]=t; changed=CHANGE_COUNTER; }

	double	get_time_at_index(int i) { return time[i]; }

	void	set_interpolation(int i) { interp=i; changed=CHANGE_COUNTER; }

	int	get_interpolation() { return interp; }

	void	delete_index(int i);

	int     parse_in(Dataport *fp);
	int     parse_out(Dataport *fp, int expand=FALSE);

	// from dEnvelope:

	void	set_value_at_time(double t, double val);

	double	get_value_at_time(double t);

	// from dData:

	void	set_value(double v) { set_value_at_time(*clk,v); }

	double	get_value() { return get_value_at_time(*clk); }

	// from Data:

	int	get_changed() { int c=clk->get_changed(); return c>changed?c:changed; }

	dElinear() { init(); }

	~dElinear() { cleanup(); }
	};



class dEspline : public dEnvelope {

    protected:

	wvspline wvs;

	int	changed;

	void	init() { changed=CHANGE_COUNTER; }

	void	cleanup() { ; }

    public:

	int	get_num() { return wvs.get_numpts(); }

	void	set_value_at_index(int i, double v) {
			wvs.set_value(i,v); changed=CHANGE_COUNTER; }

	double	get_value_at_index(int i) { return wvs.get_value_at_index(i); }

	void	set_time_at_index(int i, double t) {
			wvs.set_time(i,t); changed=CHANGE_COUNTER; }

	double	get_time_at_index(int i) { return wvs.get_time_at_index(i); }

	void	set_tension_at_index(int i, double v) {
			wvs.set_tension(i,v); changed=CHANGE_COUNTER; }

	double	get_tension_at_index(int i) { return wvs.get_tension(i); }

	void	set_weight_at_index(int i, double v) {
			wvs.set_weight(i,v); changed=CHANGE_COUNTER; }

	double	get_weight_at_index(int i) { return wvs.get_weight(i); }

	void	insert_knot(double t, double v, double tn=0.0, double w=1.0) {
			wvs.insert_knot(t,v,tn,w); changed=CHANGE_COUNTER; }

	void	delete_knot(int i) { wvs.delete_knot(i); changed=CHANGE_COUNTER; }

	int     parse_in(Dataport *fp) { return(wvs.parse_in(fp)); }
	int     parse_out(Dataport *fp, int expand=FALSE) { return(wvs.parse_out(fp, expand)); }

	// from dEnvelope:

	void	set_value_at_time(double t, double val) {
			wvs.insert_knot(t,val); changed=CHANGE_COUNTER; }

	double	get_value_at_time(double t) { return wvs.get_value(t); }

	// from dData:

	void	set_value(double v) { set_value_at_time(*clk,v); }

	double	get_value() { return get_value_at_time(*clk); }

	// from Data:

	int	get_changed() { int c=clk->get_changed(); return c>changed?c:changed; }

	dEspline() { init(); }

	~dEspline() { cleanup(); }
	};


class dEsurvspline : public dEnvelope {

    protected:

	survspline SurvSpline;

	int	changed;

	void	init() { changed=CHANGE_COUNTER; }

	void	cleanup() { ; }

    public:

	int	get_num() { return SurvSpline.get_numpts(); }

	void	set_value_at_index(int i, double v) {
			SurvSpline.set_value(i,v); changed=CHANGE_COUNTER; }

	double	get_value_at_index(int i) { return SurvSpline.get_value_at_index(i); }

	void	set_time_at_index(int i, double t) {
			SurvSpline.set_time(i,t); changed=CHANGE_COUNTER; }

	double	get_time_at_index(int i) { return SurvSpline.get_time_at_index(i); }

	void	insert_knot(double t, double v) {
			SurvSpline.insert_knot(t,v); changed=CHANGE_COUNTER; }

	void	delete_knot(int i) { SurvSpline.delete_knot(i); changed=CHANGE_COUNTER; }

	int     parse_in(Dataport *fp) { return(SurvSpline.parse_in(fp)); }
	int     parse_out(Dataport *fp, int expand=FALSE) { return(SurvSpline.parse_out(fp, expand)); }

	// from dEnvelope:

	void	set_value_at_time(double t, double val) {
			SurvSpline.insert_knot(t,val); changed=CHANGE_COUNTER; }

	double	get_value_at_time(double t) { return SurvSpline.get_value(t); }

	// from dData:

	void	set_value(double v) { set_value_at_time(*clk,v); }

	double	get_value() { return get_value_at_time(*clk); }

	// from Data:

	int	get_changed() { int c=clk->get_changed(); return c>changed?c:changed; }

	dEsurvspline() { init(); }

	~dEsurvspline() { cleanup(); }
	};



#endif
