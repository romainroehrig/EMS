#!/bin/bash

source activate myuvcdat

DIR0=`pwd`

for ff in cllang caracteres_lfa lfa
do
  gfortran -I$DIR0 -fconvert=big-endian -cpp  -c -fPIC $ff.f90
done

/usr/bin/ar r liblfa.a *.o

gfortran -L$DIR0 -llfa -I$DIR0 -fconvert=big-endian -cpp  -c -fPIC -o famusc.o famusc.f90

f2py -lgfortran -c famusc.pyf famusc.o liblfa.a

rm -f *.o *.mod *.a

mv lfa.so ../

# Test

#python tmp.py

source deactivate myuvcdat
