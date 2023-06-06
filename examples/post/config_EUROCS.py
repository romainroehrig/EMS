#saveall = True
saveall = False

var2save = ['pf','zf','theta','temp','qv','u','v','wap','ql','qr','qi','qsn','qlc','qic','qrc','qsnc','rneb','cc','prw','rain','lhf','shf','tsurf','w_up','alpha_up','Mf','dTv_up','B_up','eps_u','det_u','ustar','tke']

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
for i in range(0,401):
  levoutz.append(i*50.)
levoutz.reverse()

# Niveau de print (0, 1 ou 2)
verbose = 2
