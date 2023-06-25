#ifndef	_DATA_H_
#define _DATA_H_
// data.h 1.7 02/07/10 12:48:10
/** \file
 ** Data access mechanisms
 **/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dataport.h"

extern	int	CHANGE_COUNTER;

/// The Data base class is for all data access mechanisms:

class Data {

    protected:

	char	*reference;

    public:

	virtual	int	get_changed()=0;

	virtual	int	parse_in(Dataport *) { return(FALSE); }
	virtual	int	parse_out(Dataport *, int) { return(FALSE); }

	void	set_reference(char *str) { 
		if(reference)free(reference);
		reference = strdup(str);
	}
	char	*get_reference(void) { return(reference); }

	Data() { reference = NULL; }
	~Data() { if(reference)free(reference); }

	};


/// from Data specific types of data can be derived:
class dData : public Data {

    protected:


    public:

	virtual void	set_value(double)=0;

	virtual	double	get_value()=0;

	dData() { ; }

	};


class iData : public Data {

    protected:


    public:

	virtual void	set_value(int)=0;

	virtual	int	get_value()=0;

	iData() { ; }

	};


class ldData : public Data {

    protected:


    public:

	virtual void	set_value(int, double)=0;

	virtual	double	get_value(int)=0;

	virtual	void	set_num(int)=0;

	virtual	int	get_num()=0;

	virtual	void	add_value(double)=0;

	virtual void	free_list()=0;

	ldData() { ; }

	};


class liData : public Data {

    protected:


    public:

	virtual void	set_value(int, int)=0;

	virtual	int	get_value(int)=0;

	virtual	void	set_num(int)=0;

	virtual	int	get_num()=0;

	virtual	void	add_value(int)=0;

	virtual void	free_list()=0;

	liData() { ; }

	};


class lliData : public Data {

    protected:


    public:

	virtual void	set_value(int, int, int)=0;

	virtual	int	get_value(int, int)=0;

	virtual	void	set_num(int)=0;

	virtual	int	get_num()=0;

	virtual void	set_size(int, int)=0;

	virtual	int	get_size(int)=0;

	virtual	void	add_list(int)=0;

	virtual	void	add_value(int, int)=0;

	virtual void	free_list(int)=0;

	virtual void	free_all()=0;

	lliData() { ; }

	};


class Image;

class imgData : public Data {

    protected:


    public:

	virtual void	set_value(Image *)=0;

	virtual	Image	*get_value()=0;

	imgData() { ; }

	};


#endif
