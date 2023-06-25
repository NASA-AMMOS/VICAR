// newdata.C
//
// Written by Dave Kagels 9/95

#include "image/datatypes.h"
#include "image/datamod.h"
#include "image/datammap.h"

DE_FUNC	Data_Extension_Func;


ImageData *new_data_by_type(int type)
{
	switch(type) {

	case    1:      return new bitData;
	case    2:      return new charData;
	case    3:      return new ucharData;
	case    4:      return new shortData;
	case    5:      return new ushortData;
	case    6:      return new longData;
	case    7:      return new ulongData;
	case    8:      return new floatData;
	case    9:      return new doubleData;
	case	10:	return new complexData;

#ifndef _NO_MMAP_
	case	13:	return new ucharMMap;
	case    14:     return new shortMMap;
	case    15:     return new ushortMMap;
	case    18:     return new floatMMap;
	case	23:	return new ucharMMapFV;
	case    24:     return new shortMMapFV;
	case    25:     return new ushortMMapFV;
	case    28:     return new floatMMapFV;
#endif

	case	50:	return new RLEData;

	case	100:	return new blackData;
	case	101:	return new transData;
	case	102:	return new compData;
	case	103:	return new blackRect;
	case	104:	return new doubleMap;
	case	105:	return new rgbData;
	case	106:	return new rgbRect;
	case	107:	return new multiBand;
	case	108:	return new XData;
	case	109:	return new pyrData;
	case	110:	return new fadeBorder;
	case	111:	return new cropData;
	case	112:	return new pyrpixData;

#ifndef _NO_IMAGEDISP_
	case 1000:
					return new ximageData;
#endif // _NO_IMAGEDISP_

	}
	if(Data_Extension_Func) return (*Data_Extension_Func)(type);
	return NULL;
}
