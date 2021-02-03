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

extern void __mank__main(size_t, size_t, size_t, void*);

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

// Pretty much 32 bytes
struct __mank_vec {
  size_t
    type_size,
    capacity,
    length;
  void* data;
};

struct __mank_str {
  size_t length;
  char* data;
};

void __mank_builtin__init_vec(struct __mank_vec* vec) {
  vec->data = __mank_alloc__any(vec->type_size * vec->capacity);
  vec->length = 0;
}

void __mank_builtin__push_back(struct __mank_vec* vec, void* new_element) {
  vec->length += 1;
  if (vec->length > vec->capacity) {
    vec->capacity *= 2;
    vec->data = GC_REALLOC(vec->data, vec->capacity * vec->type_size);
  }
  memcpy(vec->data + (vec->length - 1) * vec->type_size, new_element, vec->type_size);
}

int main(int argc, char* argv[]) {
    struct __mank_vec mank_args;
    mank_args.type_size = sizeof(struct __mank_str);
    mank_args.capacity = 10;
    __mank_builtin__init_vec(&mank_args);
    for (size_t i = 0; i < argc; i++) {
      char* raw_str = argv[i];
      struct __mank_str str = { .length = strlen(raw_str), .data = raw_str };
      __mank_builtin__push_back(&mank_args, &str);
    }
  // LLVM codegens passing struct by value as passing as a bunch of params
  __mank__main(
    mank_args.type_size,
    mank_args.capacity,
    mank_args.length,
    mank_args.data);
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

