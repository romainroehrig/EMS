#saveall = True
saveall = False

var2save = ['pf','zf','theta','thetav','temp','qv','u','v','wap','ql','qi','qr','qsn','rneb','cc','prw','rain','lhf','shf','tsurf','hur','Q1','Q2','qlc','qic','qrc','qsnc','tke','w_up','Mf','alpha_up','ustar','z0','omega']

convert2p = False
convert2z = False

convertkday = False
convertpday = False
convertzday = False

convertk1h = False
convertp1h = False
convertz1h = False

convertk3h = False
convertp3h = False
convertz3h = False

# Pour convert2p, niveau en hPa
levout = []
for i in range(1,41):
  levout.append(i*25.)
levout.reverse()

# Pour convert2z, niveau en m
levoutz = []
for i in range(0,401):
  levoutz.append(i*50.)
levoutz.reverse()

# Niveau de print (0, 1 ou 2)
verbose = 2
