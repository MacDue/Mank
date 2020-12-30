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
extern void mank_main(void);

int main() {
  mank_main();
}
EOF

gcc ./main.c -c -o main.o
$MANK_HOME/mankc $output_dir/$1 --codegen > mank_dump.ll.packed
csplit --quiet -b "%d.ll" ./mank_dump.ll.packed "/;--fin/"

for llvm_ir in ./*.ll
do
  llc -relocation-model=pic "$llvm_ir"
done

bin_name=$(basename $1 .mank)
gcc ./main.o ./*.s -o ./$bin_name

mv ./$bin_name $output_dir/$bin_name

