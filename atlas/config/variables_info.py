names = {}
units = {}
coefs = {}

########################
# 2D Variables
########################

# Zonal wind
vv = 'u'
names[vv] = 'Zonal wind'
units[vv] = 'm s$^{-1}$'
coefs[vv] = 1.

# Meridional wind
vv = 'v'
names[vv] = 'Meridional wind'
units[vv] = 'm s$^{-1}$'
coefs[vv] = 1.

# Potential temperature
vv = 'theta'
names[vv] = 'Potential temperature'
units[vv] = 'K'
coefs[vv] = 1.

# Specific humidity
vv = 'qv'
names[vv] = 'Specific humidity'
units[vv] = 'g kg$^{-1}$'
coefs[vv] = 1000.

# Cloud Fraction
vv = 'rneb'
names[vv] = 'Cloud fraction'
units[vv] = '%'
coefs[vv] = 100.

# Liquid water
vv = 'ql'
names[vv] = 'Liquid water content'
units[vv] = 'mg kg$^{-1}$'
coefs[vv] = 1.e6

# Ice water
vv = 'qi'
names[vv] = 'Ice water content'
units[vv] = 'mg kg$^{-1}$'
coefs[vv] = 1.e6

# Rain content
vv = 'qr'
names[vv] = 'Rain content'
units[vv] = 'mg kg$^{-1}$'
coefs[vv] = 1.e6

# Rain content
vv = 'qsn'
names[vv] = 'Snow content'
units[vv] = 'mg kg$^{-1}$'
coefs[vv] = 1.e6

# Rain content
vv = 'hur'
names[vv] = 'Relative humidity'
units[vv] = '%'
coefs[vv] = 100.

# Turbulent Kinetic Energy
vv = 'tke'
names[vv] = 'TKE'
units[vv] = 'm$^{2}$ s$^{-2}$'
coefs[vv] = 1.

# Updraft Vertical Velocity
vv = 'w_up'
names[vv] = 'Updraft vertical velocity'
units[vv] = 'm s$^{-1}$'
coefs[vv] = 1.

# Updraft Area Fraction
vv = 'alpha_up'
names[vv] = 'Updraft area fraction'
units[vv] = '%'
coefs[vv] = 100.

# Updraft Mass Flux
vv = 'Mf'
names[vv] = 'Updraft mass flux'
units[vv] = 'kg m$^{-2}$ s$^{-1}$'
coefs[vv] = 1.

# Updraft dTv
vv = 'dTv_up'
names[vv] = 'Updraft dTv'
units[vv] = 'K'
coefs[vv] = 1.

# Updraft Buoyancy
vv = 'B_up'
names[vv] = 'Updraft Buoyancy'
units[vv] = 'm s$^{-2}$'
coefs[vv] = 1.

# Updraft Entrainment
vv = 'eps_u'
names[vv] = 'Updraft Entrainment'
units[vv] = 'km$^{-1}$'
coefs[vv] = 1000.

# Updraft Entrainment
vv = 'det_u'
names[vv] = 'Updraft Detrainment'
units[vv] = 'km$^{-1}$'
coefs[vv] = 1000.

# Theta_l tendency
vv = 'tnthl'
names[vv] = r'Physical tendency of $\theta_l$'
units[vv] = 'K h$^{-1}$'
coefs[vv] = 3600.

# q_t tendency
vv = 'tnqt'
names[vv] = 'Physical tendency of $q_t$'
units[vv] = 'g kg$^{-1}$ h$^{-1}$'
coefs[vv] = 3600.*1000.

# Q1
vv = 'Q1'
names[vv] = 'Apparent heat source'
units[vv] = 'K day$^{-1}$'
coefs[vv] = 86400.

# Q2
vv = 'Q2'
names[vv] = 'Apparent moisture sink'
units[vv] = 'K day$^{-1}$'
coefs[vv] = 86400.

########################
# 1D Variables
########################

# Sensible heat flux
vv = 'shf'
names[vv] = 'Sensible heat flux'
units[vv] = 'W m$^{-2}$'
coefs[vv] = 1.

# Latent heat flux
vv = 'lhf'
names[vv] = 'Latent heat flux'
units[vv] = 'W m$^{-2}$'
coefs[vv] = 1.

# ustar
vv = 'ustar'
names[vv] = 'Surface friction velocity'
units[vv] = 'm s$^{-1}$'
coefs[vv] = 1.

# Surface temperature
vv = 'tsurf'
names[vv] = 'Surface temperature'
units[vv] = 'K'
coefs[vv] = 1.

# Precipitation
vv = 'rain'
names[vv] = 'Precipitation'
units[vv] = 'mm day$^{-1}$'
coefs[vv] = 86400.

# Total Cloud Fraction
vv = 'cc'
names[vv] = 'Total Cloud Fraction'
units[vv] = '%'
coefs[vv] = 100.

# Low Cloud Fraction
vv = 'cltl'
names[vv] = 'Low Cloud Fraction'
units[vv] = '%'
coefs[vv] = 100.

# High Cloud Fraction
vv = 'clth'
names[vv] = 'High Cloud Fraction'
units[vv] = '%'
coefs[vv] = 100.

# Liquid Water Path
vv = 'lwp'
names[vv] = 'Liquid Water Path'
units[vv] = 'g m$^{{-2}}$'
coefs[vv] = 1000.

# Ice Water Path
vv = 'iwp'
names[vv] = 'Ice Water Path'
units[vv] = 'g m$^{{-2}}$'
coefs[vv] = 1000.

# Cloud Base Height
vv = 'zcb'
names[vv] = 'Cloud Base Height'
units[vv] = 'm'
coefs[vv] = 1.

# Cloud Top Height
vv = 'zct'
names[vv] = 'Cloud Top Height'
units[vv] = 'm'
coefs[vv] = 1.

# Potential temperature averaged over 0-500m
vv = 'theta_0_500'
names[vv] = 'Potential temperature averaged over 0-500m'
units[vv] = 'K'
coefs[vv] = 1.

# Potential temperature averaged over 2000-5000m
vv = 'theta_2000_5000'
names[vv] = 'Potential temperature averaged over 2000-5000m'
units[vv] = 'K'
coefs[vv] = 1.

# Specific humidity averaged over 0-500m
vv = 'qv_0_500'
names[vv] = 'Specific humidity averaged over 0-500m'
units[vv] = 'K'
coefs[vv] = 1000.

# Specific humidity averaged over 2000-5000m
vv = 'qv_2000_5000'
names[vv] = 'Specific humidity averaged over 2000-5000m'
units[vv] = 'K'
coefs[vv] = 1000.

# TOA Outgoing Shortwave Radiation
vv = 'rsut'
names[vv] = 'TOA Outgoing Shortwave Radiation'
units[vv] = 'W m$^{-2}$'
coefs[vv] = 1.

# TOA Incoming Shortwave Radiation
vv = 'rsdt'
names[vv] = 'TOA Incoming Shortwave Radiation'
units[vv] = 'W m$^{-2}$'
coefs[vv] = 1.

# TOA Outgoing Longwave Radiation
vv = 'rlut'
names[vv] = 'TOA Outgoing Longwave Radiation'
units[vv] = 'W m$^{-2}$'
coefs[vv] = 1.

# Surface Upwelling Shortwave Radiation
vv = 'rsus'
names[vv] = 'Surface Upwelling Shortwave Radiation'
units[vv] = 'W m$^{-2}$'
coefs[vv] = 1.

# Surface Downwelling Shortwave Radiation
vv = 'rsds'
names[vv] = 'Surface Downwelling Shortwave Radiation'
units[vv] = 'W m$^{-2}$'
coefs[vv] = 1.

# Surface Upwelling Longwave Radiation
vv = 'rlus'
names[vv] = 'Surface Upwelling Longwave Radiation'
units[vv] = 'W m$^{-2}$'
coefs[vv] = 1.

# Surface Downwelling Longwave Radiation
vv = 'rlds'
names[vv] = 'Surface Downwelling Longwave Radiation'
units[vv] = 'W m$^{-2}$'
coefs[vv] = 1.

# Qr_int
vv = 'Qr_int'
names[vv] = 'Atmospheric radiation flux divergence'
units[vv] = 'W m$^{-2}$'
coefs[vv] = 1.

# Qr_int_cre
vv = 'Qr_int_cre'
names[vv] = 'Atmospheric radiative CRE'
units[vv] = 'W m$^{-2}$'
coefs[vv] = 1.

# TOA SW CRE
vv = 'TOA_cre_sw'
names[vv] = 'TOA SW Cloud Radiative Effect'
units[vv] = 'W m$^{-2}$'
coefs[vv] = 1.

# TOA LW CRE
vv = 'TOA_cre_lw'
names[vv] = 'TOA LW Cloud Radiative Effect'
units[vv] = 'W m$^{-2}$'
coefs[vv] = 1.

