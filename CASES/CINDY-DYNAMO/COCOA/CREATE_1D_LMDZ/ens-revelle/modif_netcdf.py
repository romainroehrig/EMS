import netCDF4
import numpy as np

dset = netCDF4.Dataset('/homel/otorres/FORCAGE_CINDY_HUGO/modif_FILES/final.nc')

ts_m      = dset['ts'][:]
ps_m      = dset['psurf'][:]
lh_m      = dset['flat'][:]
sh_m      = dset['sens'][:]
qh_m      = dset['hq'][:,:]
qv_m      = dset['vq'][:,:]
th_m      = dset['hT'][:,:]
tv_m      = dset['vT'][:,:]
w_m       = dset['w'][:,:]
omega_m_s = dset['omega_srf'][:]
u_m       = dset['u'][:,:]
u_m_s     = dset['u_srf'][:]
v_m       = dset['v'][:,:]
v_m_s     = dset['v_srf'][:]
q_m       = dset['rv'][:,:]
q_m_s     = dset['q_srf'][:]
t_m       = dset['temperature'][:,:]
t_m_s     = dset['T_srf'][:]
advq_m    = dset['advq'][:,:]
advt_m    = dset['advT'][:,:]
lev_m     = dset['lev'][:]
time_m    = dset['time'][:]
dset.close()

dset = netCDF4.Dataset('cas_ini_f.nc','r+')

time_o = dset['time'][8:736]
dset.variables['lev'][:] = lev_m
dset.variables['ts'][8:736,:,:] = ts_m
dset.variables['flat'][8:736,:,:] = lh_m
dset.variables['sens'][8:736,:,:] = sh_m
dset.variables['w'][8:736,:,:,:] = w_m
dset.variables['w'][8:736,0,:,:] = omega_m_s
dset.variables['u'][8:736,:,:,:] = u_m
dset.variables['u'][8:736,0,:,:] = u_m_s
dset.variables['v'][8:736,:,:,:] = v_m
dset.variables['v'][8:736,0,:,:] = v_m_s

dset.variables['rv'][8:736,:,:,:] = q_m
dset.variables['rv'][8:736,0,:,:] = q_m_s
dset.variables['temp'][8:736,:,:,:] = t_m
dset.variables['temp'][8:736,0,:,:] = t_m_s

dset.variables['pp'][8:736,0,:,:] = ps_m
for i in range(8,736) :
    print(i)
    dset.variables['pp'][i,1:39,0,0] = lev_m[1:39]

#on met zz a 1 pour voir si il est utilise
dset.variables['zz'][:,:,:,:] = 1

dset.variables['hq'][8:736,:,:,:] = qh_m
dset.variables['vq'][8:736,:,:,:] = qv_m
dset.variables['hT'][8:736,:,:,:] = th_m
dset.variables['vT'][8:736,:,:,:] = tv_m

dset.variables['advq'][8:736,:,:,:] = advq_m
dset.variables['advT'][8:736,:,:,:] = advt_m


dset.close()

#dset = netCDF4.Dataset('cas_ini.nc','r+')

#pp = dset['pp'][7:735,:,0,0]
#zz = dset['zz'][7:735,:,0,0]
#lev = dset.variables['lev'][:]
#dset.close()


#dset = netCDF4.Dataset('/homel/otorres/FORCAGE_CINDY_HUGO/rev180varanaecmwfanaradar50kmC1.c1.20111001.000000.cdf','r+')
#levini = dset.variables['lev'][:]
#dset.close()

