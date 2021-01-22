#!/bin/bash
set -e

# Helper script to compile .mank files with a mank_main() to a binary
# Usage: ./mank.sh my_prog.mank
# Creates ./my_prog binary

if [[ -z "${MANK_HOME}" ]]; then
  MANK_HOME=$(pwd)
else
  MANK_HOME="${MANK_HOME}"
fi

build_dir=$(mktemp --directory)
output_dir=$(pwd)
cd $build_dir

cat << EOF > ./main.c
#include <gc/gc.h>
#include <unistd.h>
#include <string.h>

extern void __mank__main(void);

int stderr_putchar(char c) {
  if (write(STDERR_FILENO, &c, 1) == -1) {
    return -1;
  }
  return c;
}

void* __mank_alloc__any(size_t bytes) {
  return GC_MALLOC(bytes);
}

void* __mank_alloc__atomic(size_t bytes) {
  return GC_MALLOC_ATOMIC(bytes);
}

char* __mank_builtin__str_concat(size_t l1, char* s1, size_t l2, char* s2, size_t* l_out) {
  *l_out = l1 + l2;
  char* new_str = __mank_alloc__atomic(*l_out);
  memcpy(new_str, s1, l1);
  memcpy(new_str + l1, s2, l2);
  return new_str;
}

char* __mank_builtin__str_cast(char c) {
  char* new_str = __mank_alloc__atomic(1);
  *new_str = c;
  return new_str;
}

int main() {
  __mank__main();
}
EOF

gcc ./main.c -c -o main.o
$MANK_HOME/mankc $output_dir/$1 --codegen > mank_dump.ll.packed
csplit --quiet -b "%d.ll" ./mank_dump.ll.packed "/;--fin/"

for llvm_ir in ./*.ll
do
  llc $2 -relocation-model=pic "$llvm_ir"
done

bin_name=$(basename $1 .mank)
gcc ./main.o ./*.s -o ./$bin_name -lgc -lm

mv ./$bin_name $output_dir/$bin_name

