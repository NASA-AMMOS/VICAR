////////////////////////////////////////////////////////////////////////
// PigColorConverter
//
// Base class of color converter, which basically does nothing other 
// than defining functions that need to be overwritten in sub-classes. 
////////////////////////////////////////////////////////////////////////

#include "PigModelBase.h"
#include "PigColorConverter.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigColorConverter::PigColorConverter(const char *src_color_space, 
                                     const char *des_color_space)
{
    _src_color_space = _des_color_space = NULL;
    _is_valid = TRUE;
    
    if (src_color_space) 
        _src_color_space = strdup(src_color_space);
    else
        _is_valid = FALSE;
    if (des_color_space)
        _des_color_space = strdup(des_color_space);
    else
       _is_valid = FALSE;
}

PigColorConverter::PigColorConverter()
{
    _src_color_space = _des_color_space = NULL;
    _is_valid = FALSE;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigColorConverter::~PigColorConverter()
{
    if (_src_color_space)
        delete _src_color_space;
    if (_des_color_space)
        delete _des_color_space;
}

int PigColorConverter::isConvertible(const char *src_color_space, 
                                     const char *des_color_space)
{
    if (_src_color_space == NULL || _des_color_space == NULL)
        return FALSE;

    if (strcmp(_src_color_space, src_color_space) == 0 &&
        strcmp(_des_color_space, des_color_space) == 0) {
        return TRUE;
    } else {
        return FALSE;
    }
}
