#saveall = True
saveall = False

#var2save = ['ta','hus','u','v','pres']
#var2save = ['ta','hus','u','v','pres','wap','ql','qi','cl','prw','pr','hfls','hfss','prw','qflux','tnqadv','ppr','tnqvpbl','tnqvc','tnqvlscp','tnqvd','tntrlw','tntrsw','tntlscp','tntc','tntpbl','tntd','ts','FORCQINT','PCVGQ_CPPHINP','PCVGQ_CPPHINP0','OMU','OMD','OMEF','hur','Q1','Q2','QRad']
var2save = ['ta','hus','ua','va','pres','wap','ql','qi','cl','prw','pr','hfls','hfss','prw','qflux','tnqadv','ppr','tnqvpbl','tnqvc','tnqvlscp','tnqvd','tntadv','tntrlw','tntrsw','tntlscp','tntc','tntpbl','tntd','ts','hur','Q1','Q2','QRad','mueff','I0','SWd','rsdt','rsut','rsus','rsds','rsutcs','rsuscs','rsdscs','rlut','rlutcs','rlds','rldscs','rlus','rluscs','alb_ss','qflux','qfluxPr','qfluxEv','dwater','iQadv','iQnud','iQw','Cd','Ch','Ce','Cdn','Chn','Cen']

lfalaf = '/home/roehrig/LFA/Romain/bin/lfalaf'

tunits = 'seconds since 2011-10-01 0:0:0.0'

# Pour convert2p, niveau en hPa
#levout = [1000.,950.,900.,850.,800.,750.,700,650.,600,550.,500.,450.,400.,350.,300.,250.,200.,150.,100.,50.]
levout = []
for i in range(1,41):
  levout.append(i*25.)
levout.reverse()

# Niveau de print (0, 1 ou 2)
verbose = 2
