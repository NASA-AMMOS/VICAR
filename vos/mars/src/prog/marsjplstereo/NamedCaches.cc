#include <stdio.h>
#include <string.h>	// memcmp, memcpy, memset
#include <stdint.h>
#include "nav_memory.h"
#include "NamedCaches.h"

typedef union {
  int *i;
  unsigned char *s;
} UnionT;


//////////////////////////////////////////////////////////////////////////////
///   INITIALIZERS / DESTRUCTORS
//////////////////////////////////////////////////////////////////////////////

NamedCaches::NamedCaches (JMemoryManager *mgr,
			  unsigned char *new_mem_start, int new_mem_len)

{
  Init (mm, new_mem_start, new_mem_len);
} // NamedCaches::NamedCaches



NamedCaches::~NamedCaches ()
{
} // NamedCaches::~NamedCaches



static int default_name_eq (unsigned char *n1, int l1,
			    unsigned char *n2, int l2);


void NamedCaches::Init (JMemoryManager *mgr,
			unsigned char *new_mem_start, int new_mem_len)
{
  mm = mgr;
  mem_start = new_mem_start;
  mem_len = new_mem_len;
  strategy = NC_DEFAULT_STRATEGY;
  verbosity = NC_DEFAULT_VERBOSITY;
  alignment_word_size = NC_DEFAULT_ALIGNMENT_WORD_SIZE;
  name2s = NULL;
  data2s = NULL;
  nameEq = default_name_eq;

  // Initialize the memory so the NC_MAGIC test in
  // Request() has some valid memory to use for comparison

  memset (mem_start, 0, new_mem_len);

} // NamedCaches::Init


//////////////////////////////////////////////////////////////////////////////
///   HELPING FUNCTIONS -- NOT EXPORTED
//////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////
///  default_name_eq -- Default function used to compare names.  This function
///  does a bitwise comparison between the two pieces of memory.

static int default_name_eq (unsigned char *n1, int l1,
			    unsigned char *n2, int l2)
{
  return memcmp (n1, n2, MIN (l1, l2)) == 0;
} // default_name_eq



//////////////////////////////////////////////////////////////////////////////
///  bytes_to_skip -- Computes the actual number of bytes that will be
///  used in memory by data with the given length, taking the current
///  alignment_word_size into account.

int NamedCaches::bytes_to_skip (int len)
{
  return alignment_word_size * ((len + (alignment_word_size - 1)) /
				alignment_word_size);
} // NamedCaches::bytes_to_skip



//////////////////////////////////////////////////////////////////////////////
///  cache_memory_length -- Returns the total size in bytes of a
///  word-aligned entry with given name and data field lengths,
///  according to the memory layout strategy defined for this class.

int NamedCaches::cache_memory_length (int name_len, int data_len)
{
  return bytes_to_skip (NC_MAGIC_LEN) +
    sizeof(int) + 
    bytes_to_skip (name_len) +
    sizeof(int) +
    bytes_to_skip (data_len);
} // NamedCaches::cache_memory_length



//////////////////////////////////////////////////////////////////////////////
///  cache_name_len_ptr -- Returns the address of the NAME LENGTH
///  component of the given cache entry, IF the given address does
///  indeed point to a valid entry (as determined by the NC_MAGIC
///  check).  If the parameter checks fail, NULL is returned.

unsigned char *NamedCaches::cache_name_len_ptr (unsigned char *start)
{
  if (start == NULL)
    return NULL;

  if (memcmp (NC_MAGIC, start, NC_MAGIC_LEN) != 0)
    return NULL;

  // Skip opening magic string
  start += bytes_to_skip (NC_MAGIC_LEN);

  return start;
} // NamedCaches::cache_name_len_ptr



//////////////////////////////////////////////////////////////////////////////
///  cache_data_len_ptr -- Returns the address of the DATA LENGTH
///  component of the given cache entry, IF the given address does
///  indeed point to a valid entry (as determined by the NC_MAGIC
///  check in cache_name_len_ptr).  If the parameter checks fail, NULL
///  is returned.

unsigned char *NamedCaches::cache_data_len_ptr (unsigned char *start)
{
  start = cache_name_len_ptr (start);

  if (start) {
    UnionT u;

    u.s = start;
    start = u.s + sizeof(int) + bytes_to_skip (*u.i);
  }

  return start;
} // NamedCaches::cache_data_len_ptr



//////////////////////////////////////////////////////////////////////////////
///  next_cache -- Returns the address of the next cache entry stored
///  in the input buffer memory.  Returns NULL if no next entry
///  exists.  As a special case,   start   value of NULL will return
///  the first entry, if one exists.

unsigned char *NamedCaches::next_cache (unsigned char *start,
					unsigned char *mstart, int mlen)
{
  UnionT u;

  if (start == NULL && cache_name_len_ptr (mstart))
    return mstart;

  if (start < mstart || start >= mstart + mlen)
    return NULL;

  // Verify that this memory has already been allocated and Skip ahead
  // to the name of this cache
  u.s = cache_name_len_ptr (start);

  // If this memory hasn't been allocated, use it as the next cache location
  if (u.s == NULL)
    return NULL;

  // Skip over the name
  u.s += sizeof(int) + bytes_to_skip (*u.i);

  // Skip cache data
  u.s += sizeof(int) + bytes_to_skip (*u.i);

  if (!cache_name_len_ptr (u.s))
    return NULL;

  return u.s;
} // NamedCaches::next_cache



//////////////////////////////////////////////////////////////////////////////
///  last_cache -- Returns the address of the final cache entry stored
///  in the input buffer memory.  Returns NULL if there are no named
///  caches in the buffer.

unsigned char *NamedCaches::last_cache (unsigned char *mstart, int mlen)
{
  unsigned char *cache = mstart;
  unsigned char *next = cache;

  if (cache_name_len_ptr (mstart) == NULL)
    cache = NULL;
  else {
    while ((next = next_cache (cache, mstart, mlen)))
      cache = next;
  }

  return cache;
} // NamedCaches::last_cache


//////////////////////////////////////////////////////////////////////////////
///  end_of_caches -- Returns the address following the last entry
///  stored in the input buffer memory.  If the buffer is empty, the
///  start of the buffer is returned; if the buffer is full, the
///  address just after the final entry (i.e., an address OUTSIDE the
///  buffer) is returned.

unsigned char *NamedCaches::end_of_caches (unsigned char *mstart, int mlen)
{
  unsigned char *end = last_cache (mstart, mlen);

  if (end == NULL)
    end = mstart;
  else {
    UnionT u;
    u.s = cache_data_len_ptr (end);
    if (u.s == NULL)
      end = mstart;
    else {
      u.s += sizeof(int) + bytes_to_skip (*u.i);  
      end = u.s;
    }
  }

  return end;
} // NamedCaches::end_of_caches




static char *safe_bcopy (char *dest, char *src, long count)
{
    if (src == NULL || dest == NULL) return NULL;

    if (src + count >= dest && src < dest)
	for (; count > 0; count--)
	    dest[count-1] = src[count-1];
    else if (src != dest)
	for (; count > 0; count--)
	    *dest++ = *src++;

    return dest;
} /* safe_bcopy */



//////////////////////////////////////////////////////////////////////////////
///  DeleteCache -- Internal function -- Verify that the given address
///  is a valid named cache stored in the buffer associated with this
///  object.  If it is, delete it by copying subsequent named caches
///  down in memory to overwrite it.
///  THIS FUNCTION INVALIDATES ANY PRIOR RETURNED ADDRESSES!!!!
///  But that's okay since that's an advertised aspect of this class.

void NamedCaches::DeleteCache (unsigned char *cache)
{
  UnionT u;
  unsigned char *end;

  if (cache < mem_start || cache >= mem_start + mem_len)
    return;
  u.s = cache_data_len_ptr (cache);

  if (u.s == NULL)
    return;

  u.s += sizeof(int) + bytes_to_skip (*u.i);

  // Now u.s points to the start of the next cache after the one to be
  // deleted, if any.  Copy everything after this one down in the buffer.

  end = end_of_caches (mem_start, mem_len);
  safe_bcopy ((char *) cache, (char *) u.s, end - u.s);

  // Clear the memory at the end of the list so we don't think it's
  // still a valid cache
  memset (end - (u.s - cache), 0, u.s - cache);
} // NamedCaches::DeleteCache



//////////////////////////////////////////////////////////////////////////////
///  CreateCache -- Internal Function -- Use the input name and
///  lengths to create new space to accomodate a newly named cache.
///  This function does NOT look to see if an identical name already
///  exists, it assumes such a check has been done before it is
///  called.
///  If there is insufficient free space available, existing caches
///  will be SUMMARILY DELETED until enough space has been freed up;
///  the order of deletion is given by the   strategy   variable.
///  THIS FUNCTION INVALIDATES ANY PRIOR RETURNED ADDRESSES!!!!
///  But that's okay since that's an advertised aspect of this class.

unsigned char *NamedCaches::CreateCache (unsigned char *name, int name_len,
					 int data_len)
{
  unsigned char *end = end_of_caches (mem_start, mem_len);
  unsigned char *ptr, *next;
  unsigned char *result = NULL;
  int need = cache_memory_length (name_len, data_len);
  UnionT u;

  if (mem_len < need) {
    printf ("CreateCache:  Buffer memory (%d bytes) too small "
	    "for (%d+%d+eps)\n", mem_len, name_len, data_len);
    return NULL;
  }

  while (mem_len - (end - mem_start) < need) {

    // There isn't enough unused memory to create this cache.  We need
    // to delete existing caches until enough space is available.

    switch (strategy) {
    case NC_FIFO:
      DeleteCache (mem_start);
      break;
    case NC_LIFO:
      DeleteCache (last_cache (mem_start, mem_len));
      break;
    default:
      printf ("CreateCache: Unknown strategy %d!\n", (int) strategy);
      return NULL;
    } // switch 
    end = end_of_caches (mem_start, mem_len);
  } // while

  // Now we know there's enough space at the end of the buffer
  // memory to hold the soon-to-be-created named cache, starting at
  // address   end.   Set up the internal data structures.

  result = end;

  // WRITE the MAGIC STRING

  memcpy (end, NC_MAGIC, NC_MAGIC_LEN);
  next = end + bytes_to_skip (NC_MAGIC_LEN);

  for (ptr = end + NC_MAGIC_LEN; ptr < next; ptr++)
    *ptr = '\0';

  end = next;

  // Now WRITE the NAME

  u.s = end;
  *u.i = name_len;
  end += sizeof(int);
  memcpy (end, name, name_len);
  next = end + bytes_to_skip (name_len);

  for (ptr = end + name_len; ptr < next; ptr++)
    *ptr = '\0';

  end = next;
  u.s = end;
  *u.i = data_len;
  end += sizeof(int);

  // Zero out the data area to avoid accidentally reusing earlier
  // models
  next = end + data_len;

  for (ptr = end; ptr < next; ptr++)
    *ptr = '\0';

  return result;
} // NamedCaches::CreateCache



//////////////////////////////////////////////////////////////////////////////
///   EXTERNALLY DEFINED FUNCTIONS
//////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////
///  SetName2s -- Establish a function that will convert the NAME
///  associated with each named cache into a printable string.  This
///  function must provide static storage for the resulting string
///  buffer, so that the resulting string will persist even after the
///  call to the function has popped off the runtime stack.  Each line
///  of the generated string must start with the input prefix.
///  This need not make all the information explicit, a summary would
///  be fine. If present, this function will be used by the Show
///  method to display the entire contents of the NamedCaches object.
void NamedCaches::SetName2s (char *(*new_name2s) (char *prefix,
						  unsigned char *start,
						  int len))
{
  name2s = new_name2s;
} // NamesCaches::SetName2s



//////////////////////////////////////////////////////////////////////////////
///  SetData2s -- Establish a function that will convert the DATA
///  associated with each named cache into a printable string.  This
///  function must provide static storage for the resulting string
///  buffer, so that the resulting string will persist even after the
///  call to the function has popped off the runtime stack.  Each line
///  of the generated string must start with the input prefix.
///  This need not make all the information explicit, a summary would
///  be fine. If present, this function will be used by the Show
///  method to display the entire contents of the NamedCaches object.

void NamedCaches::SetData2s (char *(*new_data2s) (char *prefix,
						  unsigned char *start,
						  int len))
{
  data2s = new_data2s;
} // Namedcaches::SetData2s



//////////////////////////////////////////////////////////////////////////////
///  SetNameEq -- Establish a function that will be used to compare
///  NAMEs associated with the individual entries in this NamedCaches
///  object.  This should return nonzero if you consider two names to
///  be equivalent, and 0 if the two names should be considered
///  different.

void NamedCaches::SetNameEq (int (*new_nameEq) (unsigned char *n1, int l1,
						unsigned char *n2, int l2))
{
  nameEq = new_nameEq;
} // NamedCaches::SetNameEq



//////////////////////////////////////////////////////////////////////////////
///  SetQueueStrategy -- Set the strategy to eliminate
///  named caches, used  when the buffer memory is full.

void NamedCaches::SetQueueStrategy (QueueStrategyT s)
{
  strategy = s;
} // NamedCaches::SetQueueStrategy



//////////////////////////////////////////////////////////////////////////////
///  SetAlignmentWordSize -- Set the size (in bytes) of the alignment
///  to be used internally to the buffer memory.  Requests for name
///  and data lengths that are not integer multiples of this number
///  will be rounded up when memory is allocated.

void NamedCaches::SetAlignmentWordSize (int bytes)
{
  if (bytes < 1) {
    printf ("SetAlignmentWordSize: size %d < 1 bytes, using 1\n", bytes);
    bytes = 1;
  }

  alignment_word_size = bytes;
} // NamedCaches::SetAlignmentWordSize



//////////////////////////////////////////////////////////////////////////////
///  SetVerbosity -- Set the verbosity associated with with internal
///  diagnostic messages

void NamedCaches::SetVerbosity (int new_verbosity)
{
  if (new_verbosity < 0 || new_verbosity > 10) {
    printf ("NamedCaches: verbosity %d out of range", new_verbosity);
    new_verbosity = MIN (10, MAX (0, new_verbosity));
    printf (" using %d\n", new_verbosity);
  }
  verbosity = new_verbosity;
} // NamedCaches::SetVerbosity



//////////////////////////////////////////////////////////////////////////////
///  Request -- main external function -- This function is called
///  whenever a client of this class wants to obtain cache memory
///  associated with the given name.  
///  If a cache with the given name and at least the given size
///  already exists, it will be returned.  Otherwise, space will be
///  found so that this request can be accomodated; note that finding
///  space may involve shifting and deleting existing caches.  So do
///  *NOT* assume you can maintain any addresses returned by earlier
///  calls.
///  THIS FUNCTION INVALIDATES ANY PRIOR RETURNED ADDRESSES!!!!
///  But that's okay since that's an advertised aspect of this class.

void *NamedCaches::Request (unsigned char *name, int name_len, int data_len)
{
  unsigned char *cache = mem_start;

  // See if we already have a cache with this name

  while (cache && cache < mem_start + mem_len) {
    UnionT u;
    int this_len;

    u.s = cache_name_len_ptr (cache);
    if (u.s == NULL) {
      cache = NULL;
      break;
    }
    this_len = *u.i;
    u.s += sizeof(int);

    if ((*nameEq) (u.s, this_len, name, name_len))
      break;

    cache = next_cache (cache, mem_start, mem_len);
  } // while

  if (cache && cache < mem_start + mem_len) {  

    // We found a cache with this name.  See if its data is big enough
    // to accomodate the requested size

    unsigned char *cache_end = next_cache (cache, mem_start, mem_len);
    unsigned char *name_start = cache_name_len_ptr (cache);

    if (cache_end == NULL)
      cache_end = cache + mem_len - (cache - mem_start);

    // Adjust the given name and data lengths for alignment

    if (name_start == NULL ||
	cache_end - cache < cache_memory_length (name_len, data_len)) {

      // Not enough room!  Delete the current entry

      DeleteCache (cache);
      cache = NULL;
    }
  }

  // Now either we've found an existing cache with enough memory to
  // support this request, we've deleted a similarly-named cache with
  // too little memory, or none was found at all.  So we either return
  // the found cache or we grab enough space to return a new one.

  if (cache == NULL) {

    // Grab the space needed to accomodate this request

    cache = CreateCache (name, name_len, data_len);

  }

  if (cache == NULL)
    return NULL;

  // We have a perfectly valid cache space.  Return the data pointer.

  cache = cache_data_len_ptr (cache);

  if (cache == NULL)
    return NULL;

  cache += sizeof(int);

  return (void *) cache;
} // NamedCaches::Request



//////////////////////////////////////////////////////////////////////////////
///  Realloc -- change the buffer memory associated with this class.
///  This can be used either to grow or shrink memory available for
///  named caches.  As many existing named caches as possible will be
///  copied into the new space.
///  THE CALLING FUNCTION MUST FREE UP THE PREVIOUSLY USED SPACE!!  No
///  such recovery will be performed by this class.
///  THIS FUNCTION INVALIDATES ANY PRIOR RETURNED ADDRESSES!!!!
///  But that's okay since that's an advertised aspect of this class.

long NamedCaches::Realloc (unsigned char *new_mem_start, int new_mem_len)
{
  if (new_mem_start == NULL)
    return PARAM_ERR;

  if (new_mem_start != mem_start) {
    unsigned char *from, *next;

    // Copy over as much data as we can using LIFO, regardless of strategy

    from = mem_start;

    while (from < mem_start + mem_len &&
	   from < mem_start + new_mem_len) {
      next = next_cache (from, mem_start, mem_len);
      if (next == NULL || next >= mem_start + new_mem_len)
	break;
      from = next;
    }

    memcpy (new_mem_start, mem_start, from - mem_start);
  }

  // Do *NOT* free the old memory; leave that to the calling routine

  mem_start = new_mem_start;
  mem_len = new_mem_len;

  return NO_ERR;
} // NamedCaches::Realloc



//////////////////////////////////////////////////////////////////////////////
///    DISPLAY FUNCTIONS
//////////////////////////////////////////////////////////////////////////////



//////////////////////////////////////////////////////////////////////////////
///  Show -- Write information to stdout about this instance object

void NamedCaches::Show ()
{
  Show (stdout, "");
} // NamedCaches::Show



//////////////////////////////////////////////////////////////////////////////
///  Show -- Write information to stdout using the given
///  prefix on each line.

void NamedCaches::Show (FILE *fp, char *prefix)
{
  unsigned char *ptr;
  int i;

  fprintf (fp, "%sNamedCache at 0x%lx:\n", prefix, (uintptr_t) this);
  fprintf (fp, "%s  Total Memory: %d bytes (0x%x) at 0x%lx\n",
	   prefix, mem_len, mem_len, (uintptr_t) mem_start);
  fprintf (fp, "%s  Queuing strategy is %s\n", prefix,
	   strategy2s (strategy, NULL));

  for (i = 0, ptr = NULL;
       ((ptr = next_cache (ptr, mem_start, mem_len))) &&
	 ptr < mem_start + mem_len;
       i++) {
    UnionT u, v;
    char *str;
    int len;

    u.s = cache_name_len_ptr (ptr);
    v.s = cache_data_len_ptr (ptr);
    fprintf (fp, "%s  %d.  0x%lx  name %d bytes, data %d bytes\n",
	     prefix, i, (uintptr_t) ptr, *u.i, *v.i);

    if (name2s) {
      char new_pref[100];
      if (u.s != NULL) {
	len = *u.i;
	u.s += sizeof(int);
	snprintf (new_pref, 100, "%s      ", prefix);
	str = (*name2s) (new_pref, u.s, len);
	if (str)
	  fprintf (fp, "%s\n", str);
      }
    }
    if (data2s) {
      char new_pref[100];
      if (v.s != NULL) {
	len = *v.i;
	v.s += sizeof(int);
	snprintf (new_pref, 100, "%s      ", prefix);
	str = (*data2s) (new_pref, v.s, len);
	if (str)
	  fprintf (fp, "%s\n", str);
      }
    }
  }
} // NamedCaches::Show


void NamedCaches::ShowHex ()
{
  ShowHex (stdout, "");
} // NamedCaches::ShowHex



void NamedCaches::ShowHex (FILE *fp, char *prefix)
{
  unsigned char *end = end_of_caches (mem_start, mem_len);
  unsigned char *ptr;

  for (ptr = mem_start; ptr < end; ptr += 16) {
    int i;

    fprintf (fp, "%s0x%08lx   ", prefix, (uintptr_t) ptr);
    for (i = 0; i < 16 && ptr + i < end; i++)
      if ((ptr[i] >= 'a' && ptr[i] <= 'z') ||
	  (ptr[i] >= 'A' && ptr[i] <= 'Z'))
	fprintf (fp, " %c ", ptr[i]);
      else
	fprintf (fp, "%02x ", ptr[i]);

    fprintf (fp, "\n");
  }
} // NamedCaches::ShowHex


//////////////////////////////////////////////////////////////////////////////
///  strategy2s -- Generate a human-readable representation of the
///  input QueueStrategyT value.  Uses the input buffer (assumed to be
///  long enough) if given, or a static buffer if the input is NULL.

char *NamedCaches::strategy2s (QueueStrategyT s, char *result)
{
  static char buf[100];

  if (result == NULL)
    result = buf;

  switch (s) {
  case NC_FIFO:
    strcpy (result, "FIFO");
    break;
  case NC_LIFO:
    strcpy (result, "LIFO");
    break;
  default:
    strcpy (result, "*Unknown*");
    break;
  } // switch

  return result;  
} // NamedCaches::strategy2s

