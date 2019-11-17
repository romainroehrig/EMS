#saveall = True
saveall = False

#var2save = ['ta','hus','u','v','pres']
var2save = ['cldtot','cldlow','tglwp','precw','tsair','precc','precl','prect','lh','sh','pblh','fsntc','fsnt','flntc','flnt','fsnsc','fsns','flnsc','flns','p','T','qv','ql','cloud','mu','tdt_turb','tdt_cond','tdt_deep','tdt_lw','tdt_sw','tdt_ls','qdt_turb','qdt_cond','qdt_deep','qdt_ls','presH']

lfalaf = '/home/roehrig/LFA/Romain/bin/lfalaf'

tunits = 'seconds since 1999-07-15 0:0:0.0'

# Pour convert2p, niveau en hPa
levout = [1000.,950.,900.,850.,800.,750.,700,650.,600,550.,500.,450.,400.,350.,300.,250.,200.,150.,100.,50.]

# Pour convert2z, niveau en m 
levoutz = [100,200,300,400,500,600,700,800,900,1000,1200,1400,1600,1800,2000,2500,3000,3500,4000,4500,5000,6000,7000,8000,9000,10000,11000,12000,13000,140000,15000,16000,17000,18000,19000,20000]

# Niveau de print (0, 1 ou 2)
verbose = 2
