// glrend.C

#include "grape/renderer.h"
#include <math.h>

// The renderer class.
//


#define TORADIANS  0.017453293 /* PI/180 to convert to radians */
#define TODEGREES  57.29578    /* 180/PI to convert to degrees */


/****************************************************************************
*
* Function name: flatten
*
* Function purpose: To flatten all hierarchical object constructs in scene
*
* Function History: 9/13/95  Created.
*
* Parameters: ObjNode ***m_list : Input to function. Place to put flattened list pointer
*
* Return value: int which is total number of objects in flattened hierarchy
*
* Global variables/defines used: None.
*
* Module variables/defines used: None.
*
* Function method (description):  
*
* Created by: John Wright
*
* Bugs, possible improvements: 
*
* This function IS NOT machine specific.
*
*****************************************************************************/
int Renderer::flatten(ObjNode ***m_list)
{
	int	i, count=0;

	if(!m_list || !scene) {
		fprintf(stderr,"Whoops - Bad pointer in hierarchy flattener.\n");
		return(0);
	}
	for(i=0; i<scene->get_num_objects(); i++) {
		// if child object then flatten
		// else just copy to master list
			*m_list = (ObjNode **)realloc((void *)(*m_list), ++count * sizeof(ObjNode *));
			(*m_list)[count-1] = scene->get_obj(i);
	}
	return(count);
}
