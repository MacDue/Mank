#!/bin/bash

mydir="${0%/*}"
curl https://raw.githubusercontent.com/catchorg/Catch2/master/single_include/catch2/catch.hpp > "$mydir"/catch/catch.hpp
