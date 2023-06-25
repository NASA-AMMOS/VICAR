//  clock.h

#ifndef	_CLOCK_H_
#define _CLOCK_H_

extern	int	CHANGE_COUNTER;

// Everything based on time has a pointer to a clock object:
// Normally there would be only one clock.


class Clock {

    private:

	double	time;

	int	changed;

    public:

	void	set_time(double t) { time=t; changed=++CHANGE_COUNTER; }

	double	get_time() { return time; }

	int	get_changed() { return changed; }

	double	operator=(double t) { set_time(t); return t; }

	operator double() { return get_time(); }

	Clock(double t=0.0) { set_time(t); }
	};


extern	Clock	global_clock;

inline	void	set_time(double t) { global_clock.set_time(t); }
inline	double	get_time() { return global_clock.get_time(); }

#endif
