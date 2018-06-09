#saveall = True
saveall = False


var2save = ['pf','zf','theta','temp','qv','u','v','wap','ql','qi','qr','qsn','rneb','cc','prw','rain','lhf','lhfn','shf','tsurf','prw','hur','Q1','Q2','qlc','qic','qrc','qsnc','tke']

var2save = var2save + ['tntrlw','tntrsw','tntpbl','tntlscp','tntc','tntd','tnupbl','tnuc','tnud','tnvpbl','tnvc','tnvd','tntadv','tntnudg','tnugeo','tnvgeo','tendu','tendv','tendq','tendh']

var2save = var2save + ['alpha_up']
var2save = var2save + ['delta_t','delta_q','dt_wake','dq_wake','dt_wake2','dq_wake2','omega_up','alpha_up','wake_fip','Cstar','hw','ZMWAKE','ZCWAKE','ZS12','ZCTH','ZMTH','ZALPTH','ZWUP','ZALPHAUP','sigmaw']

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
