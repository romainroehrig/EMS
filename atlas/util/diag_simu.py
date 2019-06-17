# -*- coding:UTF-8 -*-

import cdtime
import cdms2, MV2
import os, sys

value = 0
cdms2.setNetcdfShuffleFlag(value) ## where value is either 0 or 1
cdms2.setNetcdfDeflateFlag(value) ## where value is either 0 or 1
cdms2.setNetcdfDeflateLevelFlag(value)

missing = 1.e20

rg = 9.80665

def f_zcb(zf,zneb):

    time = zneb.getTime()
    lev = zneb.getLevel()
    nt,nlev = zneb.shape

    zcb = MV2.zeros(nt,typecode=MV2.float32) + missing

    if zf[0,0] > zf[0,1]:
        lpos = False
    else:
        lpos = True

    for it in range(0,nt):
      lfound = False
      ilev = nlev-1
      if lpos: ilev = 0
      while not(lfound) and not(ilev == -1) and not(ilev == nlev):
        if zneb[it,ilev] >= 0.001:
            lfound = True
            zcb[it] = zf[it,ilev]
        else:
            if lpos: # zf is in increasing order
                ilev = ilev + 1
            else:    # zf is in decreasing order
                ilev = ilev - 1

    zcb = MV2.masked_values(zcb,missing)
    zcb.missing_value = missing
    zcb.id = 'zcb'
    zcb.long_name = 'Cloud base height'
    zcb.units = 'm'
    zcb.setAxis(0,time)

    return zcb

def f_zct(zf,zneb):

    time = zneb.getTime()
    lev = zneb.getLevel()
    nt,nlev = zneb.shape

    zct = MV2.zeros(nt,typecode=MV2.float32) + missing

    if zf[0,0] > zf[0,1]:
        lpos = False
    else:
        lpos = True

    for it in range(0,nt):
      lfound = False
      ilev = 0
      if lpos: ilev = nlev-1
      while not(lfound) and not(ilev == -1) and not(ilev == nlev):
        if zneb[it,ilev] >= 0.001:
            lfound = True
            zct[it] = zf[it,ilev]
        else:
            if lpos: # zf is in increasing order
                ilev = ilev - 1
            else:    # zf is in decreasing order
                ilev = ilev + 1

    zct = MV2.masked_values(zct,missing)
    zct.missing_value = missing
    zct.id = 'zct'
    zct.long_name = 'Cloud top height'
    zct.units = 'm'
    zct.setAxis(0,time)

    return zct

def f_int(ph,var2int):

    nt,nlev = var2int.shape
    time = var2int.getTime()
    zout = MV2.zeros(nt,typecode=MV2.float)
    zout.setAxis(0,time)

    if ph[0,0] < ph[0,1]:
        lpos = 1
    else:
        lpos = -1
    for ilev in range(0,nlev):
        dp = (ph[:,ilev+1]-ph[:,ilev])*lpos
        zout[:] = zout[:] + var2int[:,ilev]*dp/rg

    return zout

def f_avg(zf,var2avg,zmin,zmax):

    nt,nlev = var2avg.shape
    time = var2avg.getTime()

    zout = MV2.zeros(nt,typecode=MV2.float)
    ztot = MV2.zeros(nt,typecode=MV2.float)

    if zf[0,0] < zf[0,1]:

      zup = MV2.zeros((nt,nlev),typecode=MV2.float)
      zdn = MV2.zeros((nt,nlev),typecode=MV2.float)

      zup[:,nlev-1] = 1.e20
      zdn[:,0] = 0

      zup[:,0:nlev-1] = (zf[:,0:nlev-1]+zf[:,1:nlev])/2.
      zdn[:,1:nlev] = (zf[:,0:nlev-1]+zf[:,1:nlev])/2.

      dz = MV2.zeros((nt,nlev),typecode=MV2.float)
      dz = MV2.where((zdn <= zmin) & (zup > zmin), zup-zmin, dz)
      dz = MV2.where((zdn < zmax)  & (zup >= zmax), zmax - zdn, dz)
      dz = MV2.where((zdn >= zmin) & (zup <= zmax), zup - zdn, dz)

      zout = MV2.sum(var2avg*dz,axis=1)
      ztot = MV2.sum(dz,axis=1)

      if False: # to be cleaned
       for it in range(0,nt):

        zup = (zf[it,0]+zf[it,1])/2.
        zdn = 0
        if zdn <= zmin and zup > zmin:
          dz = zup-zmin
        elif zdn < zmax and zup >= zmax:
          dz = zmax-zdn
        elif zdn >= zmin and zup <= zmax: 
          dz = zup-zdn
        elif zup <= zmin or zdn >= zmax:
          dz = 0.
        else:
          print 'This case should not occur...'
          sys.exit()

        zout[it] = zout[it] + var2avg[it,0]*dz
        ztot[it] = ztot[it] + dz

        zup= 1.e20
        zdn = (zf[it,nlev-2]+zf[it,nlev-1])/2.
        if zdn <= zmin and zup > zmin:
          dz = zup-zmin
        elif zdn < zmax and zup >= zmax:
          dz = zmax-zdn
        elif zdn >= zmin and zup <= zmax: 
          dz = zup-zdn
        elif zup < zmin or zdn > zmax:
          dz = 0.
        else:
          print 'This case should not occur...'
          sys.exit()

        zout[it] = zout[it] + var2avg[it,nlev-1]*dz
        ztot[it] = ztot[it] + dz

        
        for ilev in range(1,nlev-1):
            zup = (zf[it,ilev]+zf[it,ilev+1])/2.
            zdn = (zf[it,ilev-1]+zf[it,ilev])/2.
            if zdn <= zmin and zup > zmin:
              dz = zup-zmin
            elif zdn < zmax and zup >= zmax:
              dz = zmax-zdn
            elif zdn >= zmin and zup <= zmax: 
              dz = zup-zdn
            elif zup <= zmin or zdn >= zmax:
              dz = 0.
            else:
              print 'This case should not occur...'
              sys.exit()
            zout[it] = zout[it] + var2avg[it,ilev]*dz
            ztot[it] = ztot[it] + dz

    else:

      zup = MV2.zeros((nt,nlev),typecode=MV2.float)
      zdn = MV2.zeros((nt,nlev),typecode=MV2.float)

      zup[:,0] = 1.e20
      zdn[:,nlev-1] = 0

      zup[:,1:nlev] = (zf[:,0:nlev-1]+zf[:,1:nlev])/2.
      zdn[:,0:nlev-1] = (zf[:,0:nlev-1]+zf[:,1:nlev])/2.

      dz = MV2.zeros((nt,nlev),typecode=MV2.float)
      dz = MV2.where((zdn <= zmin) & (zup > zmin), zup-zmin, dz)
      dz = MV2.where((zdn < zmax)  & (zup >= zmax), zmax - zdn, dz)
      dz = MV2.where((zdn >= zmin) & (zup <= zmax), zup - zdn, dz)

      zout = MV2.sum(var2avg*dz,axis=1)
      ztot = MV2.sum(dz,axis=1)

      if False: # to be cleaned
       for it in range(0,nt):

        zup = (zf[it,nlev-1]+zf[it,nlev-2])/2.
        zdn = 0
        if zdn <= zmin and zup > zmin:
          dz = zup-zmin
        elif zdn < zmax and zup >= zmax:
          dz = zmax-zdn
        elif zdn >= zmin and zup <= zmax: 
          dz = zup-zdn
        elif zup <= zmin or zdn >= zmax:
          dz = 0.
        else:
          print 'This case should not occur...'
          sys.exit()

        zout[it] = zout[it] + var2avg[it,nlev-1]*dz
        ztot[it] = ztot[it] + dz

        zup= 1.e20
        zdn = (zf[it,0]+zf[it,1])/2.
        if zdn <= zmin and zup > zmin:
          dz = zup-zmin
        elif zdn < zmax and zup >= zmax:
          dz = zmax-zdn
        elif zdn >= zmin and zup <= zmax: 
          dz = zup-zdn
        elif zup < zmin or zdn > zmax:
          dz = 0.
        else:
          print 'This case should not occur...'
          sys.exit()

        zout[it] = zout[it] + var2avg[it,0]*dz
        ztot[it] = ztot[it] + dz
        
        for ilev in range(nlev-2,0,-1):
            zdn = (zf[it,ilev]+zf[it,ilev+1])/2.
            zup = (zf[it,ilev-1]+zf[it,ilev])/2.
            if zdn <= zmin and zup > zmin:
              dz = zup-zmin
            elif zdn < zmax and zup >= zmax:
              dz = zmax-zdn
            elif zdn >= zmin and zup <= zmax: 
              dz = zup-zdn
            elif zup <= zmin or zdn >= zmax:
              dz = 0.
            else:
              print 'This case should not occur...'
              sys.exit()
            zout[it] = zout[it] + var2avg[it,ilev]*dz
            ztot[it] = ztot[it] + dz


    zout[:] = zout[:]/ztot[:]
    zout.setAxis(0,time)    

    return zout


def prepare(filein,fileout,var2compute=[]):

    os.system('cp -f {0} {1}'.format(filein,fileout))

    if len(var2compute) > 0:
        f = cdms2.open(fileout,'a')
        for var in var2compute:
            print 'Compute {0}'.format(var)
            if var == 'zcb':
                zf = f('zf')
                zneb = f('rneb')
                zcb = f_zcb(zf,zneb)
                f.write(zcb)
            elif var == 'zct':
                zf = f('zf')
                zneb = f('rneb')
                zct = f_zct(zf,zneb)
                f.write(zct)
            elif var == 'ql':
                zql = f('ql') + f('qlc')
                zql.id = 'ql'
                zql.long_name = 'Liquid water content'
                zql.units = 'kg kg-1'
                f.write(zql)
            elif var == 'qr':
                zqr = f('qr') + f('qrc')
                zqr.id = 'qr'
                zqr.long_name = 'Rain water content'
                zqr.units = 'kg kg-1'
                f.write(zqr)
            elif var == 'lwp':
                zql = f('ql') + f('qlc')
                zh = f('zh')
                zlwp = f_int(zh,zql)
                zlwp.id = 'lwp'
                zlwp.long_name = 'Liquid Water Path'
                zlwp.units = 'kg m-2'
                f.write(zlwp)
            elif var == 'rwp':
                zqr = f('qr') + f('qrc')
                zh = f('zh')
                zrwp = f_int(zh,zqr)
                zrwp.id = 'rwp'
                zrwp.long_name = 'Rain Water Path'
                zrwp.units = 'kg m-2'
                f.write(zrwp)   
            elif var == 'theta_0_500':
                ztheta = f('theta')
                zf = f('zf')
                ztheta_avg = f_avg(zf,ztheta,0,500)
                ztheta_avg.id = var
                ztheta_avg.long_name = 'Potential temperature averaged over 0-500m'
                ztheta_avg.units = 'K'
                f.write(ztheta_avg) 
            elif var == 'qv_0_500':
                zqv = f('qv')
                zf = f('zf')
                zqv_avg = f_avg(zf,zqv,0,500)
                zqv_avg.id = var
                zqv_avg.long_name = 'Specific humidity averaged over 0-500m'
                zqv_avg.units = 'kg kg-1'
                f.write(zqv_avg) 
            elif var == 'theta_2000_5000':
                ztheta = f('theta')
                zf = f('zf')
                ztheta_avg = f_avg(zf,ztheta,2000,5000)
                ztheta_avg.id = var
                ztheta_avg.long_name = 'Potential temperature averaged over 2000-5000m'
                ztheta_avg.units = 'K'
                f.write(ztheta_avg) 
            elif var == 'qv_2000_5000':
                zqv = f('qv')
                zf = f('zf')
                zqv_avg = f_avg(zf,zqv,2000,5000)
                zqv_avg.id = var
                zqv_avg.long_name = 'Specific humidity averaged over 2000-5000m'
                zqv_avg.units = 'kg kg-1'
                f.write(zqv_avg)    
            elif var == 'max_cf':
                zcf = f('rneb')
                zmax_cf = MV2.max(zcf,axis=1)
                zmax_cf.id = var
                zmax_cf.long_name = 'Maximum cloud fraction'
                zmax_cf.units = '-'
                f.write(zmax_cf)
            elif var == 'Qr_int':
                zqr = f('rsus')-f('rsds')+f('rlus')-f('rlds')+f('rsdt')-f('rsut')-f('rlut')
                zqr.id = var
                zqr.long_name = 'Integrated radiative heating'
                zqr.units = 'W m-2'
                f.write(zqr)
            elif var == 'TOA_cre_sw':
                zcre = f('rsutcs')-f('rsut')
                zcre.id = var
                zcre.long_name = 'TOA SW CRE'
                zcre.units = 'W m-2'
                f.write(zcre)    
            elif var == 'TOA_cre_lw':
                zcre = f('rlutcs')-f('rlut')
                zcre.id = var
                zcre.long_name = 'TOA LW CRE'
                zcre.units = 'W m-2'
                f.write(zcre)
            elif var == 'Qr_int_cre':
                zqr = f('rsus')-f('rsds')+f('rlus')-f('rlds')+f('rsdt')-f('rsut')-f('rlut')
                #print zqr
                #print 'rsuscs', f('rsuscs')
                #print 'rsdscs', f('rsdscs')
                #print 'rldscs', f('rldscs')
                #print 'rsutcs', f('rsutcs')
                #print 'rlutcs', f('rlutcs')
                zqrcs = f('rsuscs')-f('rsdscs')+f('rlus')-f('rldscs')+f('rsdt')-f('rsutcs')-f('rlutcs')
                #print zqrcs
                zqrcre = zqr-zqrcs
                zqrcre.id = var
                zqrcre.long_name = 'Atmospheric CRE'
                zqrcre.units = 'W m-2'
                #print zqrcre
                f.write(zqrcre)
            else:
                print 'variable to be computed is unknown:', var
                sys.exit()

        f.close()
