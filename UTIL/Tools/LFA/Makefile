#
#-----------------------------------------------
# Makefile area.
#-----------------------------------------------
#
EXEf=lfac lfafreq lfacre lfalaf lfaminm lfareu lfatest lfa_demo
EXEF=lfadiffart lfadiffrel lfadiff lfa2lfa lfamoy lfa_autodocumentation
DIRBIN=bin

all: liblfa.a lfa2lfp lfp2lfa $(EXEf) $(EXEF)

exe: lfa2lfp lfp2lfa cllangu $(EXEf) $(EXEF)

lfa.o: lfa.f90 lfatail.h lfayom.h
	$(FC) -c lfa.f90

liblfa.a: lfa.o
	$(BIBC) liblfa.a lfa.o

lfa2lfp: lfa2lfp.f90 liblfa.a lfp.f90 lfpyom.h 
	$(FC) -o $@ lfa2lfp.f90 lfp.f90 liblfa.a
	mv  $@ $(DIRBIN)
	rm -f $@.o

lfp2lfa: lfp2lfa.f90 liblfa.a lfp.f90 lfpyom.h 
	$(FC) -o $@ lfp2lfa.f90 lfp.f90 liblfa.a
	mv  $@ $(DIRBIN)
	rm -f $@.o

cllangu: cllangu.f90 cllang.f90
	$(FC) -o $@ cllangu.f90
	mv  $@ $(DIRBIN)
	rm -f $@.o

$(EXEf): $(%.f90) liblfa.a
	$(FC) -o $@ $@.f90 liblfa.a
	mv  $@ $(DIRBIN)
	rm -f $@.o

$(EXEF): $(%.f90) liblfa.a
	$(FC) -o $@ $@.f90 liblfa.a
	mv  $@ $(DIRBIN)
	rm -f $@.o

clean: 
	rm -rf *.o *.a *.dSYM lfac lfafreq lfacre lfalaf lfaminm lfareu lfatest lfa_demo lfadiffart lfadiffrel lfadiff lfa2lfp lfp2lfa lfa2lfa cllangu lfamoy lfa_autodocumentation
