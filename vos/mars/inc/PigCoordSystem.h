////////////////////////////////////////////////////////////////////////
// PigCoordSystem
//
// Defines a coordinate system.  CS's are identified by four values:
// Mission, FrameName, IndexSet, and SolutionID.  Mission is more or less
// implicit.  FrameName is the name of the coordinate system (e.g. Site,
// Rover, RSM).  IndexSet is the set of identifiers (on rover missons, based
// on the RMC) that uniqely identifies this instance of the CS.  Solution ID
// says which of potentially several versions of this CS it is (solution ID
// is rarely used and generally NULL).
//
// CS's are identified with a string of the form:
//    FRAME:(index1,index2,...);solution
// where ;solution is optional and may be omitted.  If there are no index
// values, FRAME:() is legal.  This name is constructed internally (to ensure
// consistency) and is used to search for matching CS's.
//
// Each frame refers to a "reference" frame, which is the CS in which this
// frame is defined.  That chain continues all the way to some frame which
// is designated as the "fixed" frame... the root of the tree from which
// all references descend.  The fixed frame is generally the lowest-numbered
// Site frame, and varies from run to run of PIG (depending on the inputs).
// In other words, it's the tree root we know of at run time, not the full
// tree root of all frames that could possibly exist.
//
// Reference frames are lazily evaluated... pointers are not created until
// they are needed.  This allows for partially-populated trees while reading
// in CS entries, for example.
//
// The CS object maintains an offset and quaternion to the reference CS.
// The offset is defined as the origin of this as measured in the reference.
// The quaternion is defined as a quat that will rotate avector expressed
// in this frame to the reference frame.
//
// In principle, coordinate transforms work by transforming to or from the
// reference frame using the offset/quat, with the reference frame transforming
// w.r.t. its reference, and so on.  In order to avoid a bunch of unnecessary
// work, the final composite result from this to the fixed frame is saved,
// and used for transforms.  The cache is invalidated whenever something
// changes in the chain of references.
//
// As implied above, the offset/quat for a given CS can be changed.  CS
// implement the PigAdjustable interface, which allows items to be changed
// in a matter compatible with bundle adjustment in e.g. marsnav.
//
// Thus, transformation between frames consists of transforming the value
// (point or vector) to the fixed frame, and then back to the destination
// frame.
//
// CS objects are shared as much as possible.  The create functions search
// a static global table of CS objects (which is actually managed in the
// PigMission subclass for each mission), and return an existing object if
// the mission, frame, indexes, and solution id match.  This ensures that
// adjusting a CS affects everything that should be affected.  If you want
// a disconnected CS object (that doesn't move when it "should" via the
// PigAdjustable interface), use a different solution ID.

// CS object support conversions of three quantities:  Point, Vector, and
// az/el.  Vectors are simply rotated as necessary; there is no origin (they
// are just an orientation, typically they are unit vectors but this is not
// required).  Points are rotated as well, but are translated too based on
// the offsets between the CS's (they are a vector from the CS origin to
// the given physical point, or equivalently, coordinates of a point in the
// CS).  Az/el angles are simply a convenience to avoid having to go to/from
// a vector all the time.
//
// Note that EVERY time az/el is converted to/from an orientation vector,
// the corresponding CS routines MUST BE USED!  The reason is that some CS's
// define positive elevation as toward the +Z axis (e.g. M98 Fixed, LL,
// MVACS) while most others define it as toward the -Z axis (all MER, MSL,
// 2020, etc).  The CS objects take this into account.  Plus, it is possible
// for some CS's to use clockwise azimuth while others use CCW.
//
// Note that az/el are always measured in radians.
////////////////////////////////////////////////////////////////////////

#ifndef PIGCOORDSYSTEM_H
#define PIGCOORDSYSTEM_H

#include "PigModelBase.h"
#include "PigAdjustable.h"
#include "PigVector.h"
#include "PigQuaternion.h"
#include "PigCSDefinition.h"
#include "PigMission.h"

#include "lbl_coordinate.h"
#include "lbl_derived_geometry.h"

class PigFileModel;

#include <string.h>

class PigCoordSystem : public PigModelBase, public PigAdjustable {

    friend PigMission;			// creation and cache management
    friend PigLabelModel;		// direct offset/quat access
    friend class PigPointMERiddCamera;	// for direct quat access... really
					// should be rewritten to not do this
  protected:

    PigMission *_mission;

    PigCSDefinition *_csdef;

    PigCoordSystem *_ref_cs;	// NULL if not yet evaluated

    // Update cached values, if any, when needed (lazy evaluation)

    int _cacheValid;
    int _updatingCache;		// Flag to detect loops in the reference chain

    // Cached values:
    PigVector _fixed_to_this_offset;	// this' origin measured in fixed
    PigQuaternion _this_to_fixed_quat;	// rotate vector expr. in this to fixed

    // Get the actual transform to the Fixed frame

    virtual PigVector getFixedOffset()
	{ if (!_cacheValid)
	    updateCache();
	  return _fixed_to_this_offset;
	}

    virtual PigQuaternion getFixedQuaternion()
	{ if (!_cacheValid)
	    updateCache();
	  return _this_to_fixed_quat;
	}


    // Should only be called by the create functions in PigMission

    PigCoordSystem(PigMission *m, PigCSDefinition *csdef);

    // Explicitly invalidate the cache.  Users generally won't need to do this.

    virtual void invalidateCache();

    // Update the cache.  Triggers setting the Fixed CS, if not already done

    virtual void updateCache();

  public:

    virtual ~PigCoordSystem();

    ////////////////////////////////////////////////////////////////////
    // Access functions to get at or set the values
    ////////////////////////////////////////////////////////////////////

    const PigMission *getMission() { return _mission; }
    const char *getFrameName() { return _csdef->getIdentity()->getFrameName(); }
    const char *getFrameShortName()
			{ return _csdef->getIdentity()->getShortName(); }
    const char *getFullName() { return _csdef->getIdentity()->getFullName(); }
    int getNumIndices() { return _csdef->getIdentity()->getNumIndices(); }
    const int *getIndices() { return _csdef->getIdentity()->getIndices(); }
    int getIndex(int i) { return _csdef->getIdentity()->getIndex(i); }
    const char *getSolutionId()
			{ return _csdef->getIdentity()->getSolutionId(); }
    PigCSReference *getIdentity() { return _csdef->getIdentity(); }
    PigCSReference *getReference() { return _csdef->getReference(); }

    //!!!! Should this instantiate if null?  Prob not for PigLabel usage...
    PigCoordSystem *getReferenceCS() { return _ref_cs; }

    // Base class impl asks PigMission for RMC names; can override if needed
    virtual const char *getIndexName(int i)
	{ return _mission->getRmcIndexName(i); }

    virtual void setOffset(PigVector offset);
    virtual void setQuaternion(PigQuaternion quat);
    virtual void setQuaternion(double heading, double pitch, double roll);

    // DO NOT USE THESE unless you know EXACTLY what you are doing!!!!
    // The quat/offset really should be hidden (and protected) too much
    // existing code needed it...

    PigQuaternion getQuaternion()
	{ return _csdef->getQuaternion(); }
    PigVector getOffset()
	{ return _csdef->getOffset(); }

    ////////////////////////////////////////////////////////////////////
    // Conversion functions... the whole purpose behind this class.
    ////////////////////////////////////////////////////////////////////

    // Convert point, vector, or az/el from another CS to this one

    virtual PigPoint convertPoint(PigPoint old_point, PigCoordSystem *old_cs);
    virtual PigVector convertVector(PigVector old_vect, PigCoordSystem *old_cs);
    virtual void convertAzEl(double old_az, double old_el,
		double &new_az, double &new_el, PigCoordSystem *old_cs);
    virtual void convertAzElDegrees(double old_az, double old_el,
		double &new_az, double &new_el, PigCoordSystem *old_cs);
    virtual PigQuaternion convertQuat(PigQuaternion old_quat,
		PigCoordSystem *old_cs);

    // Convert point, vector, or az/el to/from the Fixed frame related
    // to this mission

    virtual PigPoint convertPointToFixed(PigPoint point)
	{ return (getFixedQuaternion() * point) + getFixedOffset(); }

    virtual PigPoint convertPointFromFixed(PigPoint fixed_point)
	{ return ( ~ getFixedQuaternion()) *
			(fixed_point - getFixedOffset()); }

    virtual PigVector convertVectorToFixed(PigVector vector)
	{ return getFixedQuaternion() * vector; }

    virtual PigVector convertVectorFromFixed(PigVector fixed_vector)
	{ return ( ~ getFixedQuaternion()) * fixed_vector; }

    virtual void convertAzElToFixed(double az, double el,
					double &fixed_az, double &fixed_el)
	{ PigVector v = constructVector(az, el);
	  PigVector fv = convertVectorToFixed(v);
	  fixed_az = _mission->getFixedCS()->getAz(fv);
	  fixed_el = _mission->getFixedCS()->getEl(fv);
	}
    virtual void convertAzElFromFixed(double fixed_az, double fixed_el,
				double &az, double &el)
	{ PigVector fv = _mission->getFixedCS()->constructVector(fixed_az,
								fixed_el);
	  PigVector v = convertVectorFromFixed(fv);
	  az = getAz(v);
	  el = getEl(v);
	}

    // Base class assumes elevation matches -z direction
    // +Z UP MISSIONS MUST OVERRIDE IN A SUBCLAS
    // getElevationDirection returns 1 if elevation is towards +z, or
    // -1 if it is towards -z.  getAzimuthDirection returns 1 if azimuth
    // is CW as viewed from +elevation or -1 if it is CCW (alternatively,
    // 1 means if you sit on the origin with your head in +elevation, az
    // will increase left to right).  For CS's following the right-hand-rule
    // for azimuth, an el direction of +1 matches an az direction of -1,
    // and vice-versa, so the base class defines it based on the el direction.

    virtual int getElevationDirection() { return -1; }
    virtual int getAzimuthDirection() { return 1; }

    // Convert az/el to/from a Unit Vector

    virtual PigVector constructVector(double az, double el)
	{ PigVector v;
	  v.setSpherical(getAzimuthDirection() * az,
			 getElevationDirection() * el,
			 1.0);
	  return v;
	}
    virtual double getAz(PigVector v)
	{ return v.getAz() * getAzimuthDirection(); }
    virtual double getEl(PigVector v)
	{ return v.getEl() * getElevationDirection(); }

    ////////////////////////////////////////////////////////////////////
    // Print the CS in a readable form
    ////////////////////////////////////////////////////////////////////

    virtual void print();

    ////////////////////////////////////////////////////////////////////
    // The below functions come from PigAdjustable.
    ////////////////////////////////////////////////////////////////////

    virtual int getPointingParamCount() { return 6; }
    virtual void getPointingParameters(double params[], const int max_count);
    virtual void setPointingParameters(const double params[], const int count);
    virtual void getPointingErrorEstimate(double errors[], const int max_count);
    virtual const char *const getPointingParamName(int i);	// 0-based

    ////////////////////////////////////////////////////////////////////

    virtual const char *const getModelName() { return "PigCoordSystem"; }

};

#endif

