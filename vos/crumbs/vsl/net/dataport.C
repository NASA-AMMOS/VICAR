// dataport.C 1.3 01/10/11 14:06:54
#include <string.h>
#define PVM_DATAPORT
#include "dataport.h"

int PVM_Dataport::get_next_string(char *buff)
{
	int	status;
	char	*tbuff;

	if(pvm_task) {
		*buff = '\0';
		status = pvm_task->get_from_buffer(&tbuff);
		if(!status) {
//			strcpy(buff, tbuff);
			sscanf(tbuff, "%s", buff);
			free(tbuff);
		}

		return(TRUE);
	}

	return(FALSE);
}

int PVM_Dataport::put_string(const char *buff)
{
	if(pvm_task) 
		return(pvm_task->put_in_buffer((char *)buff));
	return(FALSE);
}
