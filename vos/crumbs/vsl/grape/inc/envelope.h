// envelope.h

#ifndef	_ENVELOPE_H_
#define _ENVELOPE_H_

#include <string.h>
#include "grape/data.h"
#include "grape/clock.h"

// Base envelope class for double data:

class dEnvelope : public dData {

    protected:

	int	id;
	char	*name;
	char	*version;

	Clock	*clk;

	void	init() { id=0; name=NULL; clk=&global_clock; version = NULL; }

    public:

	void	set_id(int n) { id=n; }		// should this be automatic?
	int	get_id() { return id; }

	void	set_name(char *n) { if(name) free(name);
				    name=strdup(n); }
	char	*get_name() { return name; }

	virtual	double	get_value_at_time(double t)=0;
	virtual	void	set_value_at_time(double t, double val)=0;

	void	set_clock(Clock *c) { clk=c; }
	Clock	*get_clock() { return clk; }

	dEnvelope() { init(); }

	};


// Base envelope class for int data:

class iEnvelope : public iData {

    protected:

	int	id;
	char	*name;
	char	*version;

	Clock	*clk;

	void	init() { id=0; name=NULL; clk=&global_clock; version = NULL; }

    public:

	void	set_id(int n) { id=n; }		// should this be automatic?
	int	get_id() { return id; }

	void	set_name(char *n) { if(name) free(name);
				    name=strdup(n); }
	char	*get_name() { return name; }

	virtual	int	get_value_at_time(double t)=0;
	virtual	void	set_value_at_time(double t, int val)=0;

	void	set_clock(Clock *c) { clk=c; }
	Clock	*get_clock() { return clk; }

	iEnvelope() { init(); }

	};


#endif
