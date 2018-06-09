#saveall = True
saveall = False

#var2save = ['ta','hus','u','v','pres']
var2save = ['th','thl','ta','hus','hur','ua','va','pres','presH','wap',\
	    'ql','qi','qr','qsn','qlc','qic','qrc','qsnc',\
	    'cl','pr','prls','prc','hfls','hfss','prw',\
	    'tnthrsw','tnthrlw','tnthpbl','tnthlscp','tnthc','tnthd',\
	    'tnthlrsw','tnthlrlw','tnthlpbl','tnthllscp','tnthlc','tnthld',\
	    'tnqtpbl','tnqtlscp','tnqtc','tnqtd',\
	    'wpqtp_pbl','wpthlp_pbl','wpqtp_conv','wpthlp_conv',\
	    'Q1','Q2','zg','zgH','delta_t','delta_q','d_delta_t_gw',\
	    'omgb_dth','dp_omgb','dt_KE','dq_KE','dt_PBL','dq_PBL',\
	    'omg_w','dp_delt_omg','spread_w','delta_th',\
	    'dt_wake','dq_wake','t_undi','q_undi',\
	    'd_delta_t','d_delta_q','hw','sigmaw',\
	    'wake_pe','wake_fip','wake_gfl','Cstar','wdens',\
	    'dt_dn','dt_up','dq_dn','dq_up','Mf_up','Mf_dn','sigd','omgb',\
	    't_undi_pcmt','q_undi_pcmt','t_wake_pcmt','q_wake_pcmt',\
	    'qw_undi','qw_wake','Tw_wake','Tw_undi','qsat_undi','qsat_wake',\
	    'alpha_up','w_up','omega_up','alpha_dn','w_dn','omega_dn','cape','T_up','qv_up']

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
