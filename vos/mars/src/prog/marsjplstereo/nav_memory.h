#ifndef NAV_MEMORY_H
#define NAV_MEMORY_H

#include <stdio.h>
#include "ErrHandle.h"
#if !defined(RTI_VXWORKS) && !defined (__VXWORKS__) && !defined (VXWORKS)
#include <stdlib.h>
#else
#include <stdlib.h>
#endif
#include "real_helpers.h"	// for believev()

//////////////////////////////////////////////////////////////////////////////
//
// nav_memory.h
//
// This header file defines the   JMemoryManager   class interface.
//
//////////////////////////////////////////////////////////////////////////////
//
// This provides a single framework that supports both a
// limited-memory access model that accepts any number of well-defined
// fixed-memory buffers and allocates memory from them as needed, and
// also dynamically allocated memory accesses using the system's
// malloc() and free() (unfortunately the default new and delete must be
// overridden and are unavailable).
//
// It is expected that memory accesses will be limited, typically
// allocating buffers once at system startup and reusing them.
//
// You can choose between the fixed-memory model and a malloc/free
// model simply by calling Init with different arguments.  As an extra
// feature, if an allocation is requested before Init is called,
// malloc/free are used by default until the fixed memory buffer is
// created by Init.  You can determine if this has happened after the
// fact by scanning through the JMemoryManager linked list stored via
// the "next" fields, to see if a JMemoryManager with NULL
// map.actual_mem_start exists.
//
// Dynamic memory allocation and release is DISCOURAGED, but possible.
//
//////////////////////////////////////////////////////////////////////////////
//
//				MemoryMap Class
//
// This code implements a simplistic memory manager that keeps track
// of free memory in word-size chunks (by default 4 bytes == 1 word,
// changable in MEMORY_MAP_GRAIN_SIZE_BITS at compiletime), and
// performs a linear search from the start of free space until it
// finds enough memory to satisfy a request.  It is *not* smart about
// memory organization, other than performing this linear search for
// new free space.
// 
// Memory layout of each allocated chunk:
//
// |PAD0|LLLL|3210|PAD1|****......****|PAD2|
//					   \_  PADDING2 after_all
//			        \____________  usable memory
//		       \_____________________  PADDING1 between_length_and_mem
//		  \__________________________  pad lengths, freed bit
//	     \_______________________________  memory length, including padding
//					       in NETWORK BYTE ORDER (htonl)
//	\____________________________________  PADDING0 before_all
//
// This free space table requires storage IN ADDITION to the
// per-allocation words defined above.  If 4-byte words are used, then
// just under 4% = (1/32) of the memory is dedicated to this free
// space table.  If 1-byte words were used, then 1/8 of the buffer
// space would be required.
//
// Any number of bytes may be requested in an allocation.  However,
// when using fixed-size buffers rather than malloc/free, this number
// is always rounded up to the nearest word boundary.  Two additional
// words are also allocated; these include the length of the total
// memory allocated, a bit indicating whether this chunk has already
// been freed, and three unsigned char values that indicate how much
// optional padding has been included with the allocation.
//
// Statistics are maintained regarding memory usage, and you can print
// out the current map memory allocations.  You may also set
// parameters to indicate how much buffer memory padding to include
// around each allocated portion.  This allows you to look for memory
// overflow errors by checking the memory padding to see if it's been
// overwritten.  Such checking is currently NOT done automatically.



// The memory map keeps track of allocations in 2^(grain size) sized
// pieces.  E.g., grain size bits == 0 means every byte, 2 means every
// 4 bytes, 3 means 8 bytes.  MEMORY_MAP_GRAIN_SIZE_BITS defines the
// size of this word.

#define MEMORY_MAP_GRAIN_SIZE_BITS  2

// If the padding values are enabled, MEMORY_MAP_FILLER_VALUE defines
// the byte used to fill that padding

#define MEMORY_MAP_FILLER_VALUE 0xbd

#define DEFAULT_PADDING_WORDS_BEFORE_ALL 1
#define DEFAULT_PADDING_WORDS_BETWEEN_LENGTH_AND_MEM 1
#define DEFAULT_PADDING_WORDS_AFTER_ALL 1

// Verbosity defines how much diagnostic information is generated.  This
// is a value from 0 to 10.

#define DEFAULT_MM_VERBOSITY 0

// We use 4 bytes of memory to store the amount of padding around
// allocated memory, in case the user decided to change the defaults
// between allocations, and also a bit indicating whether this chunk
// has already been "free"d.  These are the byte offsets within that
// word.

#define PAD_BEFORE_OFFSET 0
#define PAD_BETWEEN_OFFSET 1
#define PAD_AFTER_OFFSET 2
#define PAD_FREED_OFFSET 3

// Macro that invokes the allocator with useful context.  Although things
// will work with unmodified "new" and "delete" invocations, it is much
// better to use these macros since they provide valuable additional
// context when writing diagnostic messages.
//
//	UNMODIFIED C++ CODE	   RECOMMENDED C++ CODE
//-------------------------------------------------------------------------
//	variable = new Type	   variable = NEW(mm,"blah") type
//	variable = new Type[num]   variable = NEW(mm,"blah") type[num]
//	delete variable;	   DELETE(mm,variable);
//	delete[] variable;	   DELETEV(mm,variable);
//
//	****NOTE****  Many objects now take an (mm) as a parameter 
//	to their constructors.  So when you "NEW" such objects, you'll
//	have to remember to add (mm) to the "type" spec AS WELL AS to
//	the NEW macro.
//-------------------------------------------------------------------------

//      The unfortunate lack of symmetry here is because C++ does not
//	provide a means of passing arguments to delete.  "Placement new"
//	does this for "new", but not for "delete", hence the asymmetry.


#ifndef USE_NAV_MEMORY_OVERLOADS

#define NEW(mm,desc) new
#define DELETE(mm,addr) delete (addr)
#define DELETEV(mm,addr) delete[] (addr)

class JMemoryManager {
 public:
  JMemoryManager () { }
  JMemoryManager (unsigned char new_flags) { }
  ~JMemoryManager () { }
};


#else

#define NEW(mm,desc) new(mm,__FILE__,__LINE__,desc)
#define DELETE(mm,addr) do {if (mm) (mm)->SetFileLine (__FILE__,__LINE__); delete (addr);} while (0)
#define DELETEV(mm,addr) do {if (mm) (mm)->SetFileLine (__FILE__,__LINE__); delete[] (addr);} while (0)



//	Provide C-style malloc and free functionality too

#define MM_malloc(x) \
	( CreateMMRootIfNeeded (), \
	  MMRoot->Alloc ((x), NULL, __FILE__, __LINE__) )
#define MM_free(x) \
	( CreateMMRootIfNeeded (), \
	  MMRoot->Free ((unsigned char *) (x), __FILE__, __LINE__))

// MEMORYMAP class
//
// This implements only the fixed-size buffer memory model.

class MemoryMap {
  unsigned char *actual_mem_start, *actual_mem_end; // Input buffer area
  unsigned char *usable_mem_start, *usable_mem_end; // Word-aligned buffer area
  unsigned char *map_start, *map_end;	// subset area used for free cell map
  unsigned char *first_empty_map_cell;	// cached location inside cell map
  unsigned char *peak_first_empty_map_cell; // max location of lowest empty cell
  unsigned char *last_alloc_map_cell;	// cached location inside cell map
  unsigned char *peak_last_alloc_map_cell; // max location of highest used cell + 1

  int padding_words_before_all;		// padding to use in future allocations
  int padding_words_between_length_and_mem;
  int padding_words_after_all;

  unsigned long requested_bytes_allocated, actual_bytes_allocated;  // stats
  unsigned long requested_bytes_freed, actual_bytes_freed;
  unsigned long actual_map_bits_redefined;
  unsigned long padding_bytes_used;
  unsigned long num_successful_allocs, num_failed_allocs;
  unsigned long num_successful_frees, num_failed_frees;

 public:

  int verbosity;

  MemoryMap();
  ~MemoryMap();

  // PRIMARY EXTERNAL INTERFACE FUNCTIONS

  long CreateMemoryMapFramework (unsigned char *mem_start, long num_bytes);
  unsigned char *AllocateAndTagMemory (long num_bytes, char *description,
				       char *file, int line);
  unsigned long Free (unsigned char *address); /* Returns # bytes freed */
  unsigned long FreeSpaceRemaining ();

  // INTERNAL FUNTIONS

  void InitVars();
  unsigned char *FindFirstChunkWithSize (long num_bytes);
  long Address2MapCell (unsigned char *address,
			unsigned char **map_cell, int *map_bit);
  unsigned char *MapCell2Address (unsigned char *map_cell, int map_bit);
  int PossibleStartBitIndex (unsigned char map_char, long map_bits);
  int NBitsFreeAtTheStart (unsigned char map_char, long map_bits);
  long SetMapUsingAddressRange (unsigned char *address, int new_bit,
				long num_bytes);
  unsigned char *FindLengthAddress (unsigned char *address);

  // DISPLAY FUNCTIONS

  void ShowStats ()
    { ShowStats (stdout, "", 0); }
  void ShowStats (FILE *fp, const char *prefix, int gory_details);
  void ShowMap ()
    { ShowMap (stdout, ""); }
  void ShowMap (FILE *fp, const char *prefix);
  void ShowRow (unsigned char *map_ptr, FILE *fp, const char *prefix);
  void ShowHeadings (FILE *fp, const char *prefix);

  // ACCESSOR FUNCTIONS

  void SetPaddingWordsBeforeAll (int new_padding_words_before_all)
    { padding_words_before_all = new_padding_words_before_all; }

  int GetPaddingWordsBeforeAll ()
    { return padding_words_before_all; }

  void SetPaddingWordsBetweenLengthAndMem (int new_padding)
    { padding_words_between_length_and_mem = new_padding; }

  int GetPaddingWordsBetweenLengthAndMem ()
    { return padding_words_between_length_and_mem; }

  void SetPaddingWordsAfterAll (int new_padding_words_after_all)
    { padding_words_after_all = new_padding_words_after_all; }

  int GetPaddingWordsAfterAll ()
    { return padding_words_after_all; }

  unsigned char *GetActualMemStart ()
    { return actual_mem_start; }

  unsigned char *GetActualMemEnd ()
    { return actual_mem_end; }

  unsigned char *GetUsableMemStart ()
    { return usable_mem_start; }

  unsigned char *GetUsableMemEnd ()
    { return usable_mem_end; }

  unsigned char *GetFirstEmptyMapCell ()
    { return first_empty_map_cell; }

  unsigned char *GetPeakFirstEmptyMapCell ()
    { return peak_first_empty_map_cell; }

  unsigned char *GetLastAllocMapCell ()
    { return last_alloc_map_cell; }

  unsigned char *GetPeakLastAllocMapCell ()
    { return peak_last_alloc_map_cell; }

  unsigned long GetRequestedBytesAllocated ()
    { return requested_bytes_allocated; }

  unsigned long GetRequestedBytesFreed ()
    { return requested_bytes_freed; }

  unsigned long GetActualBytesAllocated ()
    { return actual_bytes_allocated; }

  unsigned long GetActualBytesFreed ()
    { return actual_bytes_freed; }

  unsigned long GetActualMapBitsRedefined ()
    { return actual_map_bits_redefined; }

  unsigned long GetPaddingBytesUsed ()
    { return padding_bytes_used; }

  unsigned long GetNumSuccessfulAllocs ()
    { return num_successful_allocs; }

  unsigned long GetNumFailedAllocs ()
    { return num_failed_allocs; }

  unsigned long GetNumSuccessfulFrees ()
    { return num_successful_frees; }

  unsigned long GetNumFailedFrees ()
    { return num_failed_frees; }

  /* Returns number of bytes required at peak usage, including fragmentation */
  long GetPeakUsage ()
    { unsigned char *peak = GetPeakLastAllocMapCell ();
    return (peak == NULL) ? -1 : ((peak - map_start) <<
				  (3+MEMORY_MAP_GRAIN_SIZE_BITS)); }

  /* Returns number of bytes required NOW, including fragmentation */
  long GetCurrentUsage ()
    { unsigned char *last = GetLastAllocMapCell ();
    return (last == NULL) ? -1 : ((last - map_start) <<
				   (3+MEMORY_MAP_GRAIN_SIZE_BITS)); }
};

//////////////////////////////////////////////////////////////////////////////
//			JMemoryManager Class
//
// This implements the generic memory model, which supports both
// malloc/free and the fixed-buffer scheme implemeneted in MemoryMap.


// Bitmasks used to define flags 

#define MM_FLAG_ALLOW_SYS_FALLBACK 1
#define MM_FLAG_ERASE_AFTER_FREE 2
#define MM_FLAG_ALWAYS_ASSERT_ALLOC 4

// Default flags used for a new memory manager object

#define MM_FLAG_DEFAULT 0

// Descriptions on "delete" operators unfortunately require a two
// step-process; one step saves the filename in a queue, so that even
// nested delete calls will have the description available at the end
// of the delete cycle.  STACK_LEN defines how many nested delete
// calls can be supported.  This is small, because it's only used for
// diagnostics, it doesn't contribute to memory map integrity.

// FIXME Right now it's 25K; you might want to shrink this back down to 5K.

#define MM_FILENAME_BUFFER_LEN 512
#define MM_FILENAME_STACK_LEN 50

class JMemoryManager {
 public:
  JMemoryManager ();
  JMemoryManager (unsigned char new_flags);
  ~JMemoryManager ();


  // PRIMARY EXTERNAL INTERFACE FUNTIONS

  void CallThisIfYouJustCallocTheJMemoryManager ();

  long Init ()
    { return Init (NULL, 0); }
  long Init (unsigned char *address, long num_bytes);

  unsigned char *Alloc (long bytes, char *description)
    { return Alloc (bytes, description, NULL, -1); }
  unsigned char *Alloc (long bytes, char *description, char *file, int line);

  long Free (unsigned char *address, char *file, int line);

  // DISPLAY FUNCTIONS

  void ShowStats ()
    { ShowStats (stdout, "", 0); }
  void ShowStats (FILE *fp, const char *prefix, int gory_details);
  unsigned long FreeSpaceRemaining ();
  char *PartOf (JMemoryManager *mgr, char *file, int line);

  // INTERNAL FUNCTIONS

  void InitVars ();
  long Copy (JMemoryManager *dest);
  JMemoryManager *Copy () {
    JMemoryManager *dest = (JMemoryManager *) calloc (1, sizeof(JMemoryManager));

    if (Copy (dest) == NO_ERR) return dest; else return NULL;
  }
  void InsertIntoRoot (JMemoryManager *mgr);
  void DeleteFromRoot (JMemoryManager *mgr);
  void SetFileLine (char *file, int line);

  // ACCESSOR FUNCTIONS

  void SetFlags (unsigned char new_flags)
    { flags |= new_flags; }

  void ClearFlags (unsigned char new_flags)
    { flags &= ~new_flags; }

  unsigned char GetFlags ()
    { return flags; }

  unsigned long GetSysBytesAllocated ()
    { return sys_bytes_allocated; }

  unsigned long GetSysBlocksAllocated ()
    { return sys_blocks_allocated; }

  unsigned long GetSysBlocksFreed ()
    { return sys_blocks_freed; }

  unsigned long GetSysBogusFrees ()
    { return sys_bogus_frees; }

  MemoryMap *GetMemoryMap ()
    { return &map; }

  JMemoryManager *GetNext ()
    { return next; }

  int GetVerbosity ()
    { return verbosity; }

  void SetVerbosity (int verb)
    { verbosity = verb; map.verbosity = verb; if (verbosity > 1) printf ("VERBOSITY set to %d\n", verb); }

  int GetPeakUsage ()
    { return map.GetPeakUsage (); }

  int GetCurrentUsage ()
    { return map.GetCurrentUsage (); }

  int verbosity;

  char filename_buffer[MM_FILENAME_STACK_LEN][MM_FILENAME_BUFFER_LEN ];
  int line_number[MM_FILENAME_STACK_LEN];
  int filename_index;

 private:
  MemoryMap map;
  unsigned long sys_bytes_allocated;	// bytes malloc()ed
  unsigned long sys_blocks_allocated;	// number of malloc() calls
  unsigned long sys_blocks_freed;	// number of free() calls
  unsigned long sys_bogus_frees;	// number of failed free() calls
  JMemoryManager *next;			// point to another memory manager
  unsigned char flags;			// flags
};

#endif /* USE_NAV_MEMORY_OVERLOADS */

//////////////////////////////////////////////////////////////////////////////
//		OVERRIDING THE NEW AND DELETE OPERATORS
//
// I wish there were some way to completely encapsulate this memory
// manager so that it only affected allocations that explicitly
// invoked it.  However, it appears we cannot do that.
//
// Using the keyword "new" causes the compiler to do two things.
// First it calls a memory allocator to establish object storage.
// Second it invokes the object constructor on that memory.
// Similarly, "delete" first invokes the destructor and then frees up
// the memory formerly assoicated with the object.
//
// The problem is that there is no way to invoke the appropriate
// constructor/destructor without using new/delete.  Even a macro like
// #define DELETE(ptr,type,mm) (ptr->~type(),mm->Free(ptr))
// requires an explicit type.  We could get around this with
// templates, but unfortunately that's not an option.
//
// So while "placement new" provides a means of overriding the new()
// operator in a clean way (by including an additional argument that
// could name an explicit JMemoryManager), there is no equivalent for
// delete.  Since I'm forced to override the system's delete operator,
// I must correspondingly override the new operator as well.



// This is the global variable that holds pointers to all allocated
// heaps.  Use of new without a specific heap will cause the root
// JMemoryManager to be used.

extern JMemoryManager *MMRoot;

#ifdef USE_NAV_MEMORY_OVERLOADS

inline void CreateMMRootIfNeeded ()
{
  if (MMRoot == NULL) {
    MMRoot = (JMemoryManager *) malloc (sizeof(JMemoryManager));
    believev (MMRoot);
    MMRoot->InitVars();
  }
}

inline void *operator new (size_t sz)
{
  CreateMMRootIfNeeded ();
  return MMRoot->Alloc (sz, NULL);
}

inline void *operator new[] (size_t sz)
{
  CreateMMRootIfNeeded ();
  return MMRoot->Alloc (sz, NULL);
}

inline void *operator new (size_t sz, JMemoryManager *mm,
			   char *file, int line)
{
  believez (mm);
  return mm->Alloc (sz, NULL, file, line);
}

inline void *operator new[] (size_t sz, JMemoryManager *mm,
			     char *file, int line)
{
  believez (mm);
  return mm->Alloc (sz, NULL, file, line);
}

inline void *operator new (size_t sz, JMemoryManager *mm,
			   char *file, int line, char *desc)
{
  believez (mm);
  return mm->Alloc (sz, desc, file, line);
}

inline void *operator new[] (size_t sz, JMemoryManager *mm,
			     char *file, int line, char *desc)
{
  believez (mm);
  return mm->Alloc (sz, desc, file, line);
}

inline void operator delete[] (void *ptr)
{
  CreateMMRootIfNeeded ();
  MMRoot->Free ((unsigned char *) ptr,
		MMRoot->filename_buffer[MMRoot->filename_index],
		MMRoot->line_number[MMRoot->filename_index]);
  MMRoot->SetFileLine ("", 0);
}

inline void operator delete (void *ptr)
{
  CreateMMRootIfNeeded ();
  MMRoot->Free ((unsigned char *) ptr,
		MMRoot->filename_buffer[MMRoot->filename_index],
		MMRoot->line_number[MMRoot->filename_index]);
  MMRoot->SetFileLine ("", 0);
}

#endif
#endif
