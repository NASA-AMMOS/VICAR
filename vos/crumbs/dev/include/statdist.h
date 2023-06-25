#ifndef _STATDIST_H_
#define _STATDIST_H_
/** \file
 ** Statistical distribution accumulator class
 **/

/// Statistical distribution accumulator
/**
// StatDist objects are used to accumulate and compute
// basic statistics on a distribution of floating point
// (double) values.
//
// To use the class, declare a StatDist object, and call
// the sample() method once for each data value.
// Then, use the other methods to retrieve statistical results.
// To reuse a StatDist object on a new dataset, call reset().
*/

class StatDist {

public:
	/// reset distribution to empty dataset
	void reset() {
		nsamp = 0;
		sum = sum2 = 0.0;
	}

	/// add one sample value to the distribution
	void sample(double x) {
		sum += x;
		sum2 += x*x;
		if (nsamp == 0)
			smin = smax = x;
		else if (x < smin)
			smin = x;
		else if (x > smax)
			smax = x;
		nsamp++;
	}

	/// retreive the current number of samples stored
	int samples() {
		return nsamp;
	}

	/// retrieve the arithmetic mean (average) sample value
	double mean() {
		return nsamp ? (sum/nsamp) : 0.0;
	}

	/// retrieve the minimum sample value
	double min() {
		return nsamp ? smin : 0.0;
	}

	/// retrieve the maximum sample value
	double max() {
		return nsamp ? smax : 0.0;
	}

	/// retrieve the distribution's variance
	double variance() {
		if (nsamp<2) return 0.0;
		// careful about integer overflow 
		return (sum2 - sum*sum/nsamp) / (nsamp-1);
	}

	/// retrieve the distribution's standard deviation
	double sdev() {
		double sqrt(double x);
		return sqrt(variance());
	}

	/// constructor
	StatDist() { reset(); }
	
private:
	int nsamp;		///< number of samples collected
	double sum;		///< sum of samples
	double sum2;		///< sum of squared samples
	double smin, smax;	///< minimum/maximum sample
};

#endif
