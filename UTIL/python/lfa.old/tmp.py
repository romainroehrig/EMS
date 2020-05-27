import lfa
import netCDF4

a = lfa.readr('Out.008.5833.lfa','PT',80)

f = netCDF4.Dataset('test.nc','w',format='NETCDF3_CLASSIC')
level = f.createDimension('level',80)

temp = f.createVariable('temp','f4',('level'))
temp[:] = a[:]

f.close()
