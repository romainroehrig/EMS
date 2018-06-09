import numpy

#saveall = True
saveall = False

#var2save = ['ta','hus','u','v','pres']
var2save = ['ta','hus','ua','va','zg','zgH','pres','wap','ql','qi','cl','pr','prls','hfls','hfss','prw','qflux','tnqadv','ppr','tnqvpbl','tnqvc','tnqvlscp','tnqvd','tntadv','tntrlw','tntrsw','tntlscp','tntc','tntpbl','tntd','ts','hur','Q1','Q2','rsdt','rsut','rlut','rlds','rsds','rlus','rsus','qlc','qic','qrc','qsnc','alpha_up','w_up','omega_up','alpha_dn','w_dn','omega_dn','cape','T_up','qv_up','omega_ref']

lfalaf = '/home/roehrig/LFA/Romain/bin/lfalaf'

tunits = 'seconds since 2002-09-08 16:0:0.0'

# Pour convert2p, niveau en hPa
levout = [1000.,950.,900.,850.,800.,750.,700,650.,600,550.,500.,450.,400.,350.,300.,250.,200.,150.,100.,50.]
#levout = []
#for i in range(1,41):
#  levout.append(i*25.)
#levout.reverse()

levoutz =[0.07213, 0.15048, 0.23783, 0.33677, 0.44973, 0.57889, 0.72629, 0.89375, 1.0829, 1.2952, 1.5319, 1.794, 2.0824, 2.3977, 2.7406, 3.1111, 3.5096, 3.9357, 4.3893, 4.8698, 5.3766, 5.9089, 6.4655, 7.0452, 7.6465, 8.2695, 8.9145, 9.5825, 10.275, 10.993, 11.736, 12.507, 13.305, 14.132, 14.99, 15.878, 16.798, 17.752, 18.741, 19.765]
levoutz = numpy.array(levoutz,dtype=numpy.float32)*1000.

# Niveau de print (0, 1 ou 2)
verbose = 2
