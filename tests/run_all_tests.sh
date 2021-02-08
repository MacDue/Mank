#!/bin/bash
set -e

cd $(dirname $BASH_SOURCE)

# C++ unit tests

echo "Running C++ unit tests"
../build/tests/UnitTests

# Mank self tests

echo "Running Mank self tests"
./run_self_tests.sh
