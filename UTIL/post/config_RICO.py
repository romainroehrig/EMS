#saveall = True
saveall = False

#var2save = ['ta','hus','u','v','pres']
#var2save = ['th','thl','ta','hus','hur','ua','va','pres','presH','wap',\
#	    'ql','qi','qr','qsn','qlc','qic','qrc','qsnc',\
#	    'cl','pr','hfls','hfss','prw',\
#	    'tnthrsw','tnthrlw','tnthpbl','tnthlscp','tnthc','tnthd',\
#	    'tnthlrsw','tnthlrlw','tnthlpbl','tnthllscp','tnthlc','tnthld',\
#	    'tnqtpbl','tnqtlscp','tnqtc','tnqtd',\
#	    'wpqtp_pbl','wpthlp_pbl','wpqtp_conv','wpthlp_conv',\
#	    'Q1','Q2','zg','zgH']

var2save = ['pf','zf','theta','temp','qv','u','v','wap','ql','qi','qr','qsn','rneb','cc','prw','rain','lhf','shf','tsurf','hur','Q1','Q2','qlc','qic','qrc','qsnc','tke','w_up','Mf','alpha_up','ustar','z0']

convert2p = False
convert2z = True

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
