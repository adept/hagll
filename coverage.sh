#!/bin/bash
ghc --make -fhpc -main-is SAB SAB.hs
rm -f SAB.tix
./SAB
hpc report SAB
hpc markup SAB
