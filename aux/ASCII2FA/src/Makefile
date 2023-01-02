# acadfa makefile

FC       = gfortran
F90FLAGS = -fopenmp -fconvert=swap -fno-second-underscore -fbacktrace -m64 \
           -gdwarf-3 -ffree-line-length-none -fno-sign-zero -fpic -fno-range-check -ffree-form \
           -DLINUX -DLITTLE_ENDIAN -DLITTLE -g -O2 -ftree-vectorize  -DADDRESS64 -fdefault-real-8

EXEC     = acadfa_exe

OBJS     = acadfa1D_main.o \
           acadfa_sueframe.o \
           real2spec.o

XRD           = /Users/romain/rootpack/arp632_climat.02.MPIGNU1220.x/lib/libxrd.local.a
XLA           = /Users/romain/rootpack/arp632_climat.02.MPIGNU1220.x/lib/libxla.local.a
GRIBEX        = /Users/romain/libraries/gcc12/auxlibs.3.5/lib/libgribex.a
MPIDUMMY      = /Users/romain/libraries/gcc12/auxlibs.3.5/lib/libmpidummy.a
ASCII2FA_LIBS ?= $(XRD) $(XLA) $(GRIBEX) $(MPIDUMMY)

%.o:%.F90
	$(FC) $(F90FLAGS) -c $<

all:	$(OBJS)
	$(FC) $(F90FLAGS) -o $(EXEC) $(OBJS) $(ASCII2FA_LIBS)
	/bin/cp -f $(EXEC) ../bin/ascii2fa

clean:
	/bin/rm -f $(EXEC) $(OBJS)

