cmake_minimum_required(VERSION 3.16)
  
project(EMS LANGUAGES Fortran C)

find_package (Python3 COMPONENTS Interpreter)

enable_testing()

add_subdirectory (apptools)
add_subdirectory (aux/lfatools)
add_subdirectory (aux/lfa4py)
add_subdirectory (ems)
add_subdirectory (data)
add_subdirectory (examples)
