#ifndef _ZVPRINTF_H_
#define _ZVPRINTF_H_

/*
  zvprintf

  Functions for safely sending formatted output to zvmessage without
  having do allocate buffers, call snprintf, etc.:

  void zvnprintf(size_t bufsize, const char* format, ...);
  void zvnprintf_key(size_t bufsize, const char* format, char* key, ...);
  void zvnabend(size_t bufsize, const char* format, ...);

  The difference between the first two is that the former hard-wires a
  key value of " " to zvmessage. The third function is like zvnprintf,
  but passes the formatted string to zmabend instead of zvmessage.

  These are "safe" because they use snprintf, rather than sprintf, to
  control the length of data written to the buffer. The two functions
  provided here require the user to estimate the maximum necessary
  buffer size, including the trailing null. If the size provided is
  less than that needed, then the output will be safely truncated.

  So if a double is formatted as %lf (not using exponential notation)
  and the value is 1E300, and the buffer size is set to 100, then
  about 200 bytes of the printed value will be truncated, which is
  better than corrupting 200 bytes of the stack.

  For example, in the test routine tzvprint.c, this works as desired:

  double bigFloat = 1E300;
  zvnprintf(350, "big float: %lf", bigFloat);

  While this would corrupt the stack:

  char buf[100];
  sprintf(buf, "big float: %lf", bigFloat);

  And this would be safe, but not have the desired effect, because not
  all of bigFloat would be printed by zvmessage:

  zvnprintf(100, "big float: %lf", bigFloat);

  History:
  2019-07-17 WLB - Initial version.

 */

#include <stddef.h> /* for size_t */

#ifdef __cplusplus
extern "C" {
#endif

void zvnprintf(size_t bufsize, const char* format, ...);
void zvnprintf_key(size_t bufsize, const char* format, char* key, ...);
void zvnabend(size_t bufsize, const char* format, ...);

#ifdef __cplusplus
}
#endif

#endif
