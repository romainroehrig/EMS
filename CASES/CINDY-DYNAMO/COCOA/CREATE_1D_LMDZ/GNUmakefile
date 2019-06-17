# This is a makefile for GNU make.

# 1. Source files and libraries

makefile_dir = .
mod_dir = ${HOME}/Ocean_skin
VPATH = ${makefile_dir}:${mod_dir}

sources = create_1D_LMDZ.f mod_sst_ostia.f mod_time.f mod_alert.f

lib_list_dep = jumble datetime_module netcdf
lib_list_link = jumble datetime netcdff

# 2. Objects and executable file

objects := $(sources:.f=.o)
execut = create_1D_LMDZ

# 3. Compiler-dependent part

mode = debug
include ${gen_comp_opt_dir}/${FC}_${mode}.mk

# 4. Rules

.PHONY: all clean clobber depend
all: ${execut} 
include ${gen_comp_opt_dir}/settings.mk
${execut}: ${objects}

depend ${makefile_dir}/depend.mk:
	makedepf90 -free -Wmissing -Wconfused $(addprefix -I, ${VPATH}) -nosrc $(addprefix -u , ${lib_list_dep}) ${sources} >${makefile_dir}/depend.mk

clean:
	rm -f ${execut} ${objects}

clobber: clean
	rm -f *.mod ${makefile_dir}/depend.mk

ifneq ($(MAKECMDGOALS), clobber)
include ${makefile_dir}/depend.mk
endif
