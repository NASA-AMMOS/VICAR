// wave_object_1.h
// intended to be the first wavefront object to be
// compatible with grape

#ifndef	_WAVE_OBJECT_1_H_
#define _WAVE_OBJECT_1_H_

#include <X11/Intrinsic.h>

#include "grape/poly_object_1.h"

//#include <gl/gl.h>
//#include <gl/device.h>


// The class for a wavefront polygonal object
// 
class WaveObject1 : public PolyObject1
   {
   private:

   protected:

	void	skip_line(Dataport *fp);

	void	init(void);

   public:

	virtual int     get_type(void)  { return(POLY_V1); } // same as poly

	void dump_texture_map( void);

	virtual int	parse_in(Dataport *fp);

	WaveObject1(void) {
		init();
	}

}; /* Class */

#endif
