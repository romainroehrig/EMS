#!/bin/sh

#set -ex

solib='lfa'
routines='caracteres_lfa cllang lfa'

FFLAGS='-fconvert=big-endian -fbacktrace -fPIC -O2 -m64'

for rr in $routines
do
  gfortran -cpp -I. $FFLAGS -static -c $rr.f90 -o $rr.o
done

ar r liblfa.a *.o *.mod

gfortran $FFLAGS -c famusc.f90 -o famusc.o

gfortran $FFLAGS -shared famusc.o liblfa.a -o $solib.so

rm -f *.o *.mod
