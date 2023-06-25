// dataport.h 1.1 01/10/11 13:28:36
// DAM datport
// This is included from "../dataport.h" if DAM_DATAPORT is defined

#ifndef _DATAPORT_H_
#define DAM_DATAPORT
#include "../dataport.h"
#endif

#ifndef	_DAM_DATAPORT_H_
#define _DAM_DATAPORT_H_

/* to be completed later

class DAM_Dataport : public Dataport {

    private:

    protected:

	void	init();

    public:

	virtual	int	open(FILE *)=0;
	virtual	int	open(const char *)=0;
	virtual	int	ropen(const char *)=0;
	virtual	int	wopen(const char *)=0;
	virtual	int	rwopen(const char *)=0;
	virtual	int	close(void)=0;
	virtual	int	flush(int=0)=0;
	virtual int	get_next_string(char *)=0;
	virtual int	put_string(const char *)=0;
	virtual	int	getc(void)=0;
	virtual	int	get_type(void) { return(DAM_PORT); }

	DAM_Dataport() { 
		init(); 
	}
};
*/

#endif
