import cdms2
import netCDF4 as nc
import time as TT

ntest = 100
fin = '/Users/romainroehrig/MUSC/simulations//arp631.diag.pcmt.num/CMIP6.tmp2/L91_300s/ARMCU//REF/Output/netcdf/Out_klevel.nc'
var2read = ['zf','theta','qv','u','v','ql','qlc']

TT0 = TT.time()

for i in range(0,ntest):
    data = {}
    f = cdms2.open(fin)
    for var in var2read:
        data[var] = f(var)
    f.close()

TT1 = TT.time()
print 'Test 1:', TT1-TT0
TT0 = TT.time()


for i in range(0,ntest):
    data = {}
    f = nc.Dataset(fin,'r')
    for var in var2read:
        data[var] = f[var][:]
    f.close()

TT1 = TT.time()
print 'Test 2:', TT1-TT0
TT0 = TT.time()



