//gcc test_ffi.c -o test_ffi.so -shared -fPIC

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#define IN_LENGTH 5
#define OUT_LENGTH 20

#define NUM 32416187567
// intentially slow
void fact (size_t num){
  for (size_t i = 2; i < num; ++i){
    if (num % i == 0){
      return;
    }
  }
  return;
}

size_t print_text(char* in, char* out){
  size_t iterations = 0;
  for (size_t i = 0; i < 20; ++i, ++iterations){
    out[i] = in[i%5];
  }
  fact(NUM);
  iterations = 27;
  return iterations;
}

int main(){
  char* in = "hiya";
  char* out = (char*) malloc(OUT_LENGTH*sizeof(char));
  printf("%zu\n",print_text(in,out));
  printf("%s\n",in);
  free(out);
}
