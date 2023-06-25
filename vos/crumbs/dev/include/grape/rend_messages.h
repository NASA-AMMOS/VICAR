// rend_messages.h

#ifndef	_REND_MESS_H_
#define _REND_MESS_H_


//**********	Inverse Render Queries

// Inverse render mask bits:

#define	INV_DISTANCE	1
#define	INV_SOURCE	2
#define	INV_COLOR	4
#define	INV_ALPHA	8

// Inverse render message structure:

struct InvRender {

	int	x,y;		// should be set before call
	int	requst_mask;

	double	distance;	// will be set by call (depending on mask)
	double	sourcex,
		sourcey,
		sourcez;
	double	red,
		green,
		blue;
	double	alpha;
	};


//**********	Renderer Message Passing

// Negative render message numbers are for renderer specific messages.
// Positive render message numbers are for messages that may be supported by
//  many or all renderers.

// #defines go here ...


#endif
