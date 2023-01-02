
FFLAGS=-fconvert=big-endian -fbacktrace -fPIC -O2 -m64
FC=gfortran
GCC=gcc

all: lfa4py.so

lfa.o: lfa.f90 lfatail.h lfayom.h cllang.o caracteres_lfa.o
	$(FC) -cpp -I. $(FFLAGS) -static -c lfa.f90 -o lfa.o

%.o: %.c
	$(GCC) -m64 -fPIC -g -O2 -c $< -o $@

%.o: %.F90
	$(FC) -I. $(FFLAGS) -static -c $< -o $@

%.o: %.f90
	$(FC) -cpp -I. $(FFLAGS) -static -c $< -o $@

liblfa.a: cllang.o caracteres_lfa.o lfa.o init_gfortran.o
	ar r liblfa.a cllang.o caracteres_lfa.o lfa.o init_gfortran.o

lfa4py.so: lfa4py.o liblfa.a
	gfortran $(FFLAGS) -shared lfa4py.o -I. -L. -l lfa -o lfa4py.so
	rm -f *.o *.a *.mod

clean: 
	rm -rf *.o *.a *.mod *.so
