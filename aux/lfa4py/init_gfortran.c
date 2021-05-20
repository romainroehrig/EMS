/* Philippe Marguinaud idea */

void init_gfortran_big_endian_(){
  _gfortran_set_convert (2);
}
void init_gfortran_native_endian_(){
  _gfortran_set_convert (0);
}
