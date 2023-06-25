#ifndef _SUMMITT_RANGE_H_
#define _SUMMITT_RANGE_H_
// range.h 1.7 02/07/26 15:24:13
/** \file
 ** Rename GRAPE Range class as Summitt_range for backward compatibility
 **/

#include "grape/range.h"

#define SUMMITT_OBJECT_SPACE	RANGE_OBJECT_SPACE
#define SUMMITT_MODEL_SPACE	RANGE_MODEL_SPACE

class Summitt_range : public Range {
};

#endif
