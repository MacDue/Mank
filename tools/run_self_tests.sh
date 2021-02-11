#!/bin/bash
set -e

cd $(dirname $BASH_SOURCE)

PWD=$(pwd)
TESTS_DIR="${PWD}/../tests"
MANKC="${PWD}/mank.sh"
TEST_PP="${PWD}/tests_pp.py"
MANK_HOME="${PWD}/../build"
export MANK_HOME

NC='\033[0m'
RED='\033[1;31m'
GREEN='\033[0;32m'

for tests in ${TESTS_DIR}/self_tests/*.mank
do
  test_name=$(basename $tests .mank)
  echo "Compiling tests ${test_name}.mank"
  build_dir=$(mktemp --directory)
  cd $build_dir
  {
    $TEST_PP $tests > ./${test_name}.mank \
    && $MANKC ./${test_name}.mank --tests \
    && echo "Running test ${test_name}.mank:" \
    && ./${test_name}_tests && echo -e "${GREEN}Test ${test_name} PASS!${NC}"
  } || {
    echo -e "${RED}Test ${test_name} FAILED!${NC}"
  }
  cd ..
  rm -rf $build_dir
done
