/* testing callbacks from c code */

#include <stdio.h>

char easy_write(int i) {
  fprintf(stderr, "%d\n", i);
  return i + 1;
}

char return_your_return(char (*c)()) { return c(); }

/* gcc test-callback.c -o test-callback.so -shared -fPIC */
