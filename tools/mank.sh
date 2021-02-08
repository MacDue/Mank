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

source_dir_base=$output_dir

if [[ "${ABS_SOURCE_PATH}" == "1" ]]; then
  source_dir_base=""
fi

cd $build_dir

cat << EOF > ./main.c
#include <gc/gc.h>
#include <unistd.h>
#include <string.h>
#include <stdbool.h>

extern void __mank__main(void*);

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

// Pretty much 24 bytes
struct __mank_vec_header {
  size_t
    type_size,
    capacity,
    length;
};

struct __mank_vec {
  void* data; //-3 <> -2<> -1<> 0 1 2
};

struct __mank_str {
  size_t length;
  char* data;
};

#define MANK_VEC_INIT_CAPACITY 10

inline static struct __mank_vec_header* get_vec_header(struct __mank_vec vec, bool offset) {
  if (offset) {
    vec.data -= sizeof(struct __mank_vec_header);
  }
  struct __mank_vec_header* header = (struct __mank_vec_header*)(vec.data);
}

void __mank_builtin__init_vec(struct __mank_vec* vec, size_t type_size) {
  vec->data = __mank_alloc__any(sizeof(struct __mank_vec_header) + type_size * MANK_VEC_INIT_CAPACITY);
  struct __mank_vec_header* header = get_vec_header(*vec, false);
  header->type_size = type_size;
  header->capacity = MANK_VEC_INIT_CAPACITY;
  header->length = 0;
  vec->data += sizeof(struct __mank_vec_header);
}

static inline struct __mank_vec_header* resize_vector(struct __mank_vec* vec) {
  struct __mank_vec_header* header = get_vec_header(*vec, true);
  void* data = vec->data - sizeof(struct __mank_vec_header);
  data = GC_REALLOC(data, sizeof(struct __mank_vec_header) + header->capacity * header->type_size);
  vec->data = data + sizeof(struct __mank_vec_header);
  return get_vec_header(*vec, true);
}

void __mank_builtin__push_back(struct __mank_vec* vec, void* new_element) {
  struct __mank_vec_header* header = get_vec_header(*vec, true);
  header->length += 1;
  if (header->length > header->capacity) {
    header->capacity *= 2;
    header = resize_vector(vec);
  }
  memcpy(vec->data + (header->length - 1) * header->type_size, new_element, header->type_size);
}

void __mank_builtin__pop_back(struct __mank_vec* vec) {
  struct __mank_vec_header* header = get_vec_header(*vec, true);
  if (header->length <= 0) {
    return;
  }
  header->length -= 1; // good enough
}

void __mank_builtin__vector_fill(struct __mank_vec* vec, void* fill, size_t count) {
  struct __mank_vec_header* header = get_vec_header(*vec, true);
  if (count > header->capacity) {
    header->capacity = count;
    header = resize_vector(vec);
  }
  header->length = count;
  void* el = vec->data;
  for (size_t i = 0; i < count; i++) {
    memcpy(el, fill, header->type_size);
    el += header->type_size;
  }
}

int main(int argc, char* argv[]) {
  struct __mank_vec mank_args;
  __mank_builtin__init_vec(&mank_args, sizeof(struct __mank_str));
  for (size_t i = 0; i < argc; i++) {
    char* raw_str = argv[i];
    struct __mank_str str = { .length = strlen(raw_str), .data = raw_str };
    __mank_builtin__push_back(&mank_args, &str);
  }
  // LLVM codegens passing struct by value as passing as a bunch of params
  __mank__main(mank_args.data);
}
EOF

bin_name=$(basename $1 .mank)
if [[ "$2" == "--tests" ]]; then
  LLC_ARGS=""
  MANK_ARGS="--tests"
  bin_name+="_tests"
else
  LLC_ARGS="$2"
  MANK_ARGS=""
fi

gcc ./main.c -c -o main.o
$MANK_HOME/mankc $source_dir_base/$1 --codegen $MANK_ARGS > mank_dump.ll.packed
csplit --quiet -b "%d.ll" ./mank_dump.ll.packed "/;--fin/"

for llvm_ir in ./*.ll
do
  llc $LLC_ARGS -relocation-model=pic "$llvm_ir"
done

gcc ./main.o ./*.s -o ./$bin_name -lgc -lm

mv ./$bin_name $output_dir/$bin_name

