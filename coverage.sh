#!/bin/bash
set -e
./rebuild.sh
rm -f ./CoverageTests.tix
./CoverageTests
hpc report CoverageTests
hpc markup CoverageTests
