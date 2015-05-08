/* testing callbacks from c code */

#include <stdio.h>

int i = 0;

char easy_write(int j) {
  i += j;
  return i;
}

char return_your_return(char (*c)()) { return c(); }

/* gcc test-callback.c -o test-callback.so -shared -fPIC */
