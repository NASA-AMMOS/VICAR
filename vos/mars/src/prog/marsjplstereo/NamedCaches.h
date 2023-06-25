#ifndef __NAMED_CACHES_H__
#define __NAMED_CACHES_H__

#include <stdio.h>
#include "ErrHandle.h"
#include "nav_memory.h"

//////////////////////////////////////////////////////////////////////////////
//  NamedCaches class
//
//  NamedCaches provides a back-end tool for managing semi-persistent
//  cache storage given limited resources.  Caches are given names
//  based on arbitrary-length keys in memory.  If a cache with the
//  given name exists, its contents will be made available.  If no
//  such name exists (either because it has never been entered or has
//  been overwritten), space for the cache will be allocated and
//  returned.  Unused space will be used when possible, but if no
//  additional space is available an existing cache will be wiped out
//  and made available for reuse.
//
//  CACHES ONLY PERSIST BETWEEN CALLS TO NamedCaches::Request !!
//  There is no guarantee that previously returned cache memory will
//  still exist if another cache is requested.  The intent here is to
//  keep some useful precomputed products around, for use at a SINGLE
//  location within a software architecture.  If you need multiple
//  named caches, create multiple instances of this class.
//
//  If memory were infinite, these functions would simply maintain an
//  infinite list of cache memories and associated names.  But with
//  limited memory, the various values are likely to be reused often.
//
//  The reuse strategy can be either FIFO or LIFO, and may be changed
//  at any time with the SetQueueStrategy method.
//
//  The internal memory strategy for each cache is as follows.  "bts"
//  refers to the static function bytes_to_skip() which computes the
//  length based on   alignment_word_size.   Here "cachemem" indicates
//  the starting address of an individual named cache within the
//  defined buffer.
//
//	cachemem:		NC_MAGIC string
//	^ + bts(NC_MAGIC_LEN):	int length of name
//	^ + sizeof(int):	name
//	^ + bts(name_len):	int length of data
//	^ + sizeof(int):	DATA -- CONTENTS MAINTAINED EXTERNALLY
//	^ + bts(data_len):	start of next cachemem
//
//////////////////////////////////////////////////////////////////////////////




//  Byte string written at the beginning of each named cache entry,
//  used to identify memory that's already been assigned

#define NC_MAGIC "navcache"
#define NC_MAGIC_LEN strlen(NC_MAGIC)

//  Default parameter values

#define NC_DEFAULT_VERBOSITY 0
#define NC_DEFAULT_STRATEGY NC_LIFO
#define NC_DEFAULT_ALIGNMENT_WORD_SIZE 4

//  Queuing strategy enumerated type

typedef enum { NC_FIFO, NC_LIFO } QueueStrategyT;

class NamedCaches
{
 public:
  NamedCaches (JMemoryManager *mgr, unsigned char *mem_start, int mem_len);
  ~NamedCaches ();

  void Init (JMemoryManager *mgr,
	     unsigned char *new_mem_start, int new_mem_len);
  void SetQueueStrategy (QueueStrategyT s);
  void SetVerbosity (int new_verbosity);
  void SetAlignmentWordSize (int bytes);
  void SetName2s (char *(*name2s) (char *prefix,
				   unsigned char *start, int len));
  void SetData2s (char *(*data2s) (char *prefix,
				   unsigned char *start, int len));
  void SetNameEq (int (*nameEq) (unsigned char *n1, int l1,
				 unsigned char *n2, int l2));
  unsigned char *GetMemoryStart ()
    { return mem_start; }

  void *Request (unsigned char *name, int name_len, int data_len);
  long Realloc (unsigned char *new_mem_start, int new_mem_len);

  void Show();
  void Show(FILE *fp, char *prefix);
  void ShowHex();
  void ShowHex(FILE *fp, char *prefix);

  char *strategy2s (QueueStrategyT s, char *result);

 protected:
  int bytes_to_skip (int len);
  int cache_memory_length (int name_len, int data_len);
  unsigned char *cache_name_len_ptr (unsigned char *start);
  unsigned char *cache_data_len_ptr (unsigned char *start);
  unsigned char *next_cache (unsigned char *start,
			     unsigned char *mstart, int mlen);
  unsigned char *last_cache (unsigned char *mstart, int mlen);
  unsigned char *end_of_caches (unsigned char *mstart, int mlen);
  void DeleteCache (unsigned char *cache);
  unsigned char *CreateCache (unsigned char *name, int name_len,
			      int data_len);

  QueueStrategyT strategy;
  unsigned char *mem_start;
  int mem_len;
  int verbosity;
  int alignment_word_size;
  int (*nameEq) (unsigned char *n1, int l1, unsigned char *n2, int l2);
  char *(*name2s) (char *prefix, unsigned char *start, int len);
  char *(*data2s) (char *prefix, unsigned char *start, int len);
  JMemoryManager *mm;
};

#endif /* __NAMED_CACHES_H__ */
