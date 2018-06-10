#!/bin/bash

DIR0=`pwd`

for ff in cllang caracteres_lfa lfa
do
  gfortran -I$DIR0 -fconvert=big-endian -cpp  -c -fPIC $ff.f90
done

/usr/bin/ar r liblfa.a *.o

gfortran -L$DIR0 -llfa -I$DIR0 -fconvert=big-endian -cpp  -c -fPIC -o famusc.o famusc.f90

/opt/cdat/bin/f2py -I/opt/cdat/include -lgfortran -c famusc.pyf famusc.o liblfa.a

rm -f *.o *.mod *.a

mv lfa.so ../

# Test

#python tmp.py
