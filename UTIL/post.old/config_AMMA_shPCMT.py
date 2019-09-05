#saveall = True
saveall = False

#var2save = ['ta','hus','u','v','pres']
var2save = ['ta','hus','ua','va','pres','wap','ql','qi','cl','pr','hfls','hfss','prw','qflux','tnqadv','ppr','tnqvpbl','tnqvc','tnqvshc','tnqvlscp','tnqvd','tntadv','tntrlw','tntrsw','tntlscp','tntc','tntshc','tntpbl','tntd','ts','hur','Q1','Q2','qr','qsn','qlc','qic','qrc','qsnc','qlshc','qishc','qrshc','qsnshc']

lfalaf = '/home/roehrig/LFA/Romain/bin/lfalaf'

tunits = 'seconds since 2010-07-10 0:0:0.0'

# Pour convert2p, niveau en hPa
#levout = [1000.,950.,900.,850.,800.,750.,700,650.,600,550.,500.,450.,400.,350.,300.,250.,200.,150.,100.,50.]
levout = []
for i in range(1,41):
  levout.append(i*25.)
levout.reverse()

# Niveau de print (0, 1 ou 2)
verbose = 2
