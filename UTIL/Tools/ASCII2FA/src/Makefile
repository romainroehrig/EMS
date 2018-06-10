# acadfa makefile

FC	 =	gfortran
FFLAGS	 =      -ff2c -fconvert=big-endian -frecord-marker=4 $(M32) -ffixed-form -fdefault-real-8 -DADDRESS64 -DLITTLE_ENDIAN -DHIGHRES
F90FLAGS =      -fconvert=big-endian -frecord-marker=4 $(M32) -ffree-form -fdefault-real-8 -DADDRESS64 -DHIGHRES
EXEC	 =	acadfa_exe

OBJS	 =	acadfa1D_main.o \
		acadfa_sueframe.o \
		real2spec.o \
		empty.o 

XRD	 =	libxrd.a 
GRIBEX   =      libgribex.a 
MPIDUMMY =      libmpidummy.a
LIBS	 =	$(XRD) $(GRIBEX) $(MPIDUMMY)

%.o:%.F90
	$(FC) $(F90FLAGS) -c $<

all:	$(OBJS)
	$(FC) $(F90FLAGS) -o $(EXEC) $(OBJS) $(LIBS)
	/bin/cp $(EXEC) ../bin/ascii2fa

clean:
	/bin/rm -f $(EXEC) $(OBJS)

