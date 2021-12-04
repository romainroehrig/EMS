# acadfa makefile

FC	 =	gfortran
FFLAGS	 =      -ff2c -fconvert=big-endian -frecord-marker=4 $(M32) -ffixed-form -fdefault-real-8 -DADDRESS64 -DLITTLE_ENDIAN -DHIGHRES
F90FLAGS =      -fconvert=swap -fno-second-underscore -fbacktrace -fopenmp -fno-sign-zero -fpic -ffree-line-length-none -m64 -ffree-form  -DLINUX -DLITTLE_ENDIAN -DLITTLE -DDARWIN -DADDRESS64 -fdefault-real-8 -fdefault-double-8 -g -O2
EXEC	 =	acadfa_exe

OBJS	 =	acadfa1D_main.o \
		acadfa_sueframe.o \
		real2spec.o

XRDLOC	 =	/Users/romain/rootpack/arp603_export.01.MPIGNU1120.x/lib/libxrd.local.a 
#XRDMAIN	 =	/Users/romain/rootpack/arp603_export.01.MPIGNU1120.x/lib/libxrd.local.a 
XLA	 =	/Users/romain/rootpack/arp603_export.01.MPIGNU1120.x/lib/libxla.local.a 
GRIBEX   =      /Users/romain/libraries/auxlibs.3.5/lib/libgribex.a 
MPIDUMMY =      /Users/romain/libraries/auxlibs.3.5/lib/libmpidummy.a
#LIBS	 =	$(XRDLOC) $(XRDMAIN) $(XLA) $(GRIBEX) $(MPIDUMMY) 
LIBS	 =	$(XRDLOC) $(XLA) $(GRIBEX) $(MPIDUMMY) 

%.o:%.F90
	$(FC) $(F90FLAGS) -c $<

all:	$(OBJS)
	$(FC) $(F90FLAGS) -o $(EXEC) $(OBJS) $(LIBS)
	/bin/cp -f $(EXEC) ../bin/ascii2fa

clean:
	/bin/rm -f $(EXEC) $(OBJS)

