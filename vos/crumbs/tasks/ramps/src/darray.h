#ifndef _DARRAY_H_
#define _DARRAY_H_
// darray.h 1.3 02/07/10 15:38:08
/** \file
 ** (not-very) dynamic array object.
 ** Used where a contiguous list is needed.
 ** Uses Performer pfMalloc() if pf.h was previously included.
 **/

class DArray {

public:
	/// store an object at next slot, return 0 if okay
	int store(const void *obj) {		
		if (next >= len)
			return 1;
		memcpy(ptr + next*size, obj, size);
		next++;
		return 0;
	}

	void *array() { 		///< get array pointer
		return (void *)ptr; 
	}

	void *item(unsigned i) {	///< get item pointer
		return (void *)(ptr + i * size);
	}

	unsigned curlen() {		///< get current length
		return next;
	}

	unsigned alloc_len() {		///< allocated size
		return len;
	}

	/// constructor
	DArray(unsigned element_size, unsigned length) {
		size = element_size;
		len  = length;
#ifdef PFASD_NIL_ID
		ptr  = (char *)pfMalloc(len*size, pfGetSharedArena());
#else
		ptr  = (char *)malloc(len*size);
#endif
		next = 0;
	}

private:
	unsigned size;		///< size of each element
	unsigned len;		///< current allocation size
	unsigned next;		///< next unused slot
	char *ptr;		///< allocated storage

	/// hidden copy constructor and assignment op to avoid trouble
	DArray(const DArray &copy) { }	
	void operator=(const DArray &src) { }
};

#endif
