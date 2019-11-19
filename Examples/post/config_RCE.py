#saveall = True
saveall = False

var2save = ['pf','zf','ph','zh','theta','temp','qv','u','v','wap','ql','qi','qr','qsn','rneb','cc','prw','rain','lhf','lhfn','shf','tsurf','prw','hur','Q1','Q2','qlc','qic','qrc','qsnc','tke','omega_up']

var2save = var2save + ['tntrlw','tntrsw','tntrlwcs','tntrswcs','tntpbl','tntlscp','tntc','tntd','tnupbl','tnuc','tnud','tnvpbl','tnvc','tnvd','tntadv','tntnudg','tnugeo','tnvgeo','tendu','tendv','tendq','tendh']

var2save = var2save + ['ZTAU_ECUME','t2m','huss','uas','vas']
#,'O3','rO3','rCO2','rCH4','rN2O','rsdt','rsut','rlut','rsds','rsus','rsdscs','rsuscs','rlds','rlus','rldscs','rluscs','rsutcs','rlutcs']

#var2save = var2save + ['sprw','fmse0','intfmse0','tnfmseadvw','inttnfmseadvw','lhl0','lhi0','thetae','hur','qlrad','qirad','lwp','iwp','cwp','evap','Mf','prc']

convert2p = False
convert2z = False

convertkday = False
convertpday = False
convertzday = False

convertk1h = False
convertp1h = False
convertz1h = False

convertk3h = False # not coded for True
convertp3h = False
convertz3h = False # not coded for True

# Pour convert2p, niveau en hPa
levout = []
for i in range(1,41):
  levout.append(i*25.)
levout.reverse()

# Pour convert2z, niveau en m
levoutz = []
for i in range(0,50):
  levoutz.append(10*i)
for i in range(0,11):
  levoutz.append(500+100*i)

# Niveau de print (0, 1 ou 2)
verbose = 2
