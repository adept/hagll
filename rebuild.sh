#!/bin/sh
set -e

runhaskell ./Setup.hs configure --user --bindir=`pwd` --libdir=`pwd`/lib --docdir=`pwd`
runhaskell ./Setup.hs build
runhaskell ./Setup.hs copy
