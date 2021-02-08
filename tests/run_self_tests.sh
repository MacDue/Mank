#!/bin/bash
set -e

cd $(dirname $BASH_SOURCE)

TESTS_DIR=$(pwd)
MANKC="${TESTS_DIR}/../tools/mank.sh"
MANK_HOME="${TESTS_DIR}/../build"
export MANK_HOME

for tests in ./self_tests/*.mank
do
  test_name=$(basename $tests .mank)
  echo "Compiling tests ${test_name}.mank"
  build_dir=$(mktemp --directory)
  cd $build_dir
  ABS_SOURCE_PATH=1 $MANKC "$TESTS_DIR/$tests" --tests
  echo "Running test ${test_name}.mank:"
  ./${test_name}_tests
done
