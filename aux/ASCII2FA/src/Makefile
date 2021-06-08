# acadfa makefile

FC	 =	gfortran
F90FLAGS =      -fopenmp -fconvert=swap -fno-second-underscore -fbacktrace -m64 -fopenmp -gdwarf-3 -ffree-line-length-none -fno-sign-zero -fpic -fno-range-check -ffree-form  -DLINUX -DLITTLE_ENDIAN -DLITTLE -g -O2 -ftree-vectorize  -DADDRESS64
EXEC	 =	acadfa_exe

OBJS	 =	acadfa1D_main.o \
		acadfa_sueframe.o \
		real2spec.o \
		empty.o 

XRD	 =	/home/common/pack/arp603_export.01.GFORTRAN610.cx/lib/libxrd.local.a
XLA	 =      /home/common/pack/arp603_export.01.GFORTRAN610.cx/lib/libxla.local.a
GRIBEX   =      /home/common/sync/gfortran/auxlibs/lib/libgribex.a 
MPIDUMMY =      /home/common/sync/gfortran/auxlibs/lib/libmpidummy.a
LIBS	 =	$(XRD) $(XLA) $(GRIBEX) $(MPIDUMMY)

%.o:%.F90
	$(FC) $(F90FLAGS) -c $<

all:	$(OBJS)
	$(FC) $(F90FLAGS) -o $(EXEC) $(OBJS) $(LIBS)
	/bin/cp -f $(EXEC) ../bin/ascii2fa

clean:
	/bin/rm -f $(EXEC) $(OBJS)

