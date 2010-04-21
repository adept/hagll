#!/bin/bash
./rebuild.sh
rm -f ./CoverageTests.tix
./CoverageTests
hpc report CoverageTests
hpc markup CoverageTests
