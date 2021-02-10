#!/bin/bash
set -e

cd $(dirname $BASH_SOURCE)

PWD=$(pwd)
TESTS_DIR="${PWD}/../tests"
MANKC="${PWD}/mank.sh"
MANK_HOME="${PWD}/../build"
export MANK_HOME

for tests in ${TESTS_DIR}/self_tests/*.mank
do
  test_name=$(basename $tests .mank)
  echo "Compiling tests ${test_name}.mank"
  build_dir=$(mktemp --directory)
  cd $build_dir
  ABS_SOURCE_PATH=1 $MANKC $tests --tests
  echo "Running test ${test_name}.mank:"
  ./${test_name}_tests
done
