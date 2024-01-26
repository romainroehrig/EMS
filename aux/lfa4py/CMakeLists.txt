if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
#    set(dialect "-std=legacy -ffree-form -fimplicit-none")
    set(dialect "-cpp -fconvert=big-endian -fPIC -m64")
    set(bounds "-fbounds-check")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(dialect "-convert big_endian -cpp")
    set(bounds "-check bounds")
endif()
set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")

set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${bounds}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${dialect}")


set(HEADER_FILES lfatail.h lfayom.h)
add_library(lfa_extra STATIC lfa.f90 ${HEADER_FILES} cllang.f90 caracteres_lfa.f90 init_gfortran.c)
add_library(lfa4py SHARED lfa4py.F90)
target_link_libraries(lfa4py lfa_extra)

install (TARGETS lfa4py RUNTIME DESTINATION "lib")
install (TARGETS lfa_extra RUNTIME DESTINATION "lib")

