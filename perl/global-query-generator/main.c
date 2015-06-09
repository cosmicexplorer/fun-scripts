#include <stdio.h>

#include "test_header.h"

int main() {
  printf("hello world!\n");
#ifdef REPLACE_FOR_GEN
  ___gen_print_selection(0);
  ___gen_print_selection(1);
  for (size_t sss = 0; sss < ___TOTAL_GEN; ++sss) {
    ___gen_print_selection(sss);
  }
#endif /* REPLACE_FOR_GEN */
}
