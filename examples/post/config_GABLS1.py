#saveall = True
saveall = False


#var2save = ['pf','zf','theta','temp','qv','u','v','wap','ql','qi','qr','qsn','rneb','cc','prw','rain','lhf','shf','tsurf','prw','hur','Q1','Q2','qlc','qic','qrc','qsnc','tke']

#var2save = var2save + ['tntrlw','tntrsw','tntpbl','tntlscp','tntc','tntd','tnupbl','tnuc','tnud','tnvpbl','tnvc','tnvd','tntadv','tntnudg','tnugeo','tnvgeo','tendu','tendv','tendq','tendh','alpha_up','w_up','omega_up','ZS15','ZS16','ZTAU','ZWMAX','ZZMAX','ZKMAX','ZINTEGMAX','ZMMAX','eps_u','eps_u_org','eps_u_tur','Mf','ZINTEGA','dTv_up']

var2save = ['pf','zf','theta','temp','u','v','shf','tke','tsurf','ustar','tntpbl','tnupbl','tnvpbl','ECT0','ECT1','TNECT_DYN','TNECT_BUO','TNECT_DIF','TNECT_DIS','KU']

convert2p = False
convert2z = True #False

convertkday = False
convertpday = False
convertzday = False

convertk1h = False
convertp1h = False
convertz1h = True #False

convertk3h = False # not coded for True
convertp3h = False
convertz3h = False # not coded for True

# Pour convert2p, niveau en hPa
levout = []
for i in range(1,41):
  levout.append(i*25.)
levout.reverse()

# Pour convert2z, niveau en m
levoutz = range(0,402,2)

# Niveau de print (0, 1 ou 2)
verbose = 2
