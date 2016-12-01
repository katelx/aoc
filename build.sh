#!/bin/bash
for solution in day*/solution.hs
do
    ghc -O2 -rtsopts $solution ../runner.hs
done
