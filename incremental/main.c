#include <stdio.h>
#include <inttypes.h>

typedef uint64_t SNAKEVAL;

const uint64_t BOOL_TAG   = 0x0000000000000001;

const SNAKEVAL BOOL_TRUE  = 0xFFFFFFFFFFFFFFFF; // These must be the same values
const SNAKEVAL BOOL_FALSE = 0x7FFFFFFFFFFFFFFF; // as chosen in compile.ml

SNAKEVAL print(SNAKEVAL val) {
  if ((val & BOOL_TAG) == 0) { // val is even ==> number
    printf("%" PRIu64 "\n", ((int64_t)(val)) / 2); // shift bits right to remove tag
  } else if (val == BOOL_TRUE) {
    printf("true\n");
  } else if (val == BOOL_FALSE) {
    printf("false\n");
  } else {
    printf("Unknown value: %" PRIx64 "\n", val); // print unknown val in hex
  }
  return val;
}

extern int our_code_starts_here() asm("our_code_starts_here");

int main(int argc, char** argv) {
  int result = our_code_starts_here();
  print(result);
  return 0;
}

