///////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//         This example code is from the book:
//
//           Object-Oriented Programming with C++ and OSF/Motif
//         by
//           Douglas Young
//           Prentice Hall, 1992
//           ISBN 0-13-630252-1	
//
//         Copyright 1991 by Prentice Hall
//         All Rights Reserved
//
//  Permission to use, copy, modify, and distribute this software for 
//  any purpose except publication and without fee is hereby granted, provided 
//  that the above copyright notice appear in all copies of the software.
///////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////
// InfoDialogManager.h
//////////////////////////////////////////////////////////
#ifndef INFODIALOGMANAGER_H
#define INFODIALOGMANAGER_H
#include "DialogManager.h"

class InfoDialogManager : public DialogManager {
    
  protected:
    
    Widget createDialog ( Widget );
    
  public:
    
    InfoDialogManager ( const char * );
    
};
extern DialogManager *theInfoDialogManager;
#endif
