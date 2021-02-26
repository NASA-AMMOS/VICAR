/* Test program for zvprintf routines */

#include <stdio.h>
#include "zvprintf.h"

int main(int argc, char* argv[]) {
  double bigFloat = 1E300;

  zvnprintf(50, "int %d float %f string %s", 42, 3.14159, "foo");
  zvnprintf_key(50, "int %d float %f string %s", "some key", 42, 3.14159, "foo");

  /* this prints all of bigFloat */
  zvnprintf(350, "big float: %lf", bigFloat);

  /* this truncates about 200 bytes of bigFloat */
  zvnprintf(100, "big float truncated: %lf", bigFloat);

  /* die */
  printf("Going to ABEND now ...\n");
  zvnabend(50, "int %d float %f string %s", 42, 3.14159, "foo");

  return 0;
}
