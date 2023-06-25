// hazeinfo.h

#ifndef	_HAZEINFO_H_
#define _HAZEINFO_H_

#include <stdlib.h>

#include "dataport.h"
#include "grape/parameter.h"

#define	NUM_HAZE_MODELS	3

#define	HAZE_NONE	0
#define	ZAREH_HAZE	1
#define	JOHN_HAZE	2

extern	char	*haze_model_master_list[NUM_HAZE_MODELS];

int	haze_model_from_type(char);

// Class for storing common parameters for various haze models.

class HazeInfo {

    private:

    protected:

	char	*version;
	char	*reference;

	int	local;

	dparam	haze_red, haze_green, haze_blue;
	dparam	sky_red, sky_green, sky_blue;

	void	init() { version=NULL; reference = NULL; local = FALSE; }

    public:

	void	get_haze_color(double *r, double *g, double *b) {
		*r = (double)haze_red.get_value();
		*g = (double)haze_green.get_value();
		*b = (double)haze_blue.get_value();
	}
	void	set_haze_color(double r, double g, double b) {
		haze_red.set_value(r);
		haze_green.set_value(g);
		haze_blue.set_value(b);
	}

	void	get_sky_color(double *r, double *g, double *b) {
		*r = (double)sky_red.get_value();
		*g = (double)sky_green.get_value();
		*b = (double)sky_blue.get_value();
	}
	void	set_sky_color(double r, double g, double b) {
		sky_red.set_value(r);
		sky_green.set_value(g);
		sky_blue.set_value(b);
	}

	void	set_local(int f) { local = f; }
	int	get_local(void) { return(local); }

	void	set_reference(char *ref) {
		if(reference)free(reference);
		reference = strdup(ref);
	}

	char	*get_reference(void) { return(reference); }

	char	*get_version(void) { return(version); }

	virtual	int	get_model(void) { return(HAZE_NONE); }

        virtual	int	parse_in(Dataport *) {return(FALSE);}
	virtual	int	parse_out(Dataport *, int ) { return(FALSE); }

	HazeInfo() { init(); }
	};

// Class for the Haze model implemented by Zareh for Render/Surveyor

class ZarehHazeInfo : public HazeInfo {

   private:

   protected:

      int    HazeMode;
      dparam HazeDistExp;
      int    HazeSymmetric;
      dparam HazeSkyPitch;
      dparam HazeGroundPitch;
      dparam HazeStartDist;
      dparam HazeEndDist;
      dparam HazeStartPerc;
      dparam HazeEndPerc;

      void init() { ; }

    public:

	virtual	int	get_model(void) { return(ZAREH_HAZE); }

	virtual	int	parse_in(Dataport *fp);
	virtual	int	parse_out(Dataport *fp, int expand=FALSE);

        int get_haze_mode(void) { return(HazeMode); }
        void set_haze_mode(int m) { HazeMode = m; }

        double get_haze_dist_exp(void) { return((double)HazeDistExp.get_value()); }
        void set_haze_dist_exp(double hde) { HazeDistExp.set_value(hde); }

        int get_haze_symmetric(void) { return(HazeSymmetric); }
        void set_haze_symmetric(int hsym) { HazeSymmetric = hsym; }

        double get_haze_sky_pitch(void) { return((double)HazeSkyPitch.get_value()); }
        void set_haze_sky_pitch(double hsp) { HazeSkyPitch.set_value(hsp); }

        double get_haze_ground_pitch(void) { return((double)HazeGroundPitch.get_value()); }
        void set_haze_ground_pitch(double hgp) { HazeGroundPitch.set_value(hgp); }

        double get_haze_start_dist(void) { return((double)HazeStartDist.get_value()); }
        void set_haze_start_dist(double hsd) { HazeStartDist.set_value(hsd); }

        double get_haze_end_dist(void) { return((double)HazeEndDist.get_value()); }
        void set_haze_end_dist(double hed) { HazeEndDist.set_value(hed); }

        double get_haze_start_perc(void) { return((double)HazeStartPerc.get_value()); }
        void set_haze_start_perc(double hsp) { HazeStartPerc.set_value(hsp); }

        double get_haze_end_perc(void) { return((double)HazeEndPerc.get_value()); }
        void set_haze_end_perc(double hep) { HazeEndPerc.set_value(hep); }

	ZarehHazeInfo() { init(); }
	};

// Class for the Haze model implemented by John

class JohnHazeInfo : public HazeInfo{

    private:

    protected:

	dparam	Haze_A, Haze_B;

	void	init() { ; }

    public:

        void set_haze_A(double hed) { Haze_A.set_value(hed); }
        double get_haze_A(void) { return((double)Haze_A.get_value()); }

        void set_haze_B(double hed) { Haze_B.set_value(hed); }
        double get_haze_B(void) { return((double)Haze_B.get_value()); }

	virtual	int	get_model(void) { return(JOHN_HAZE); }

	virtual	int	parse_in(Dataport *fp);
	virtual	int	parse_out(Dataport *fp, int expand=FALSE);

	JohnHazeInfo() { init(); }
	};

HazeInfo *create_haze_info(Dataport *fp);

#endif
