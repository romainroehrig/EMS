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

var2save = ['th','ta','hus','ua','va','pres','wap','ql','qi','qr','qsn','cl','prw','pr','hfls','hfss','prw','qflux','tnqadv','ppr','tnqvpbl','tnqvc','tnqvlscp','tnqvd','tntadv','tntrlw','tntrsw','tntlscp','tntc','tntpbl','tntd','ts','hur','Q1','Q2','QRad','mueff','I0','SWd','rsdt','rsut','rsus','rsds','rsutcs','rsuscs','rsdscs','rlut','rlutcs','rlds','rldscs','rlus','rluscs','alb_ss','qflux','qfluxPr','qfluxEv','dwater','iQadv','iQnud','iQw','Cd','Ch','Ce','Cdn','Chn','Cen','cltl','cltm','clth','clt','Q11','igs','igs2','sigs','sigs2','omega_up','omega_dn','alpha_up','alpha_dn','rho','mlen','sigs2turb','sigs2conv','igs2turb','igs2conv','Q11min','Q11max','acoef','qlc','qic','qrc','qsnc','tke','lwp','iwp','sigc0','sigc1','cltcalipso','cllcalipso','clmcalipso','clhcalipso','clcalipso','cllcalipsoice','clmcalipsoice','clhcalipsoice','cltcalipsoice','cllcalipsoliq','clmcalipsoliq','clhcalipsoliq','cltcalipsoliq','cllcalipsoun','clmcalipsoun','clhcalipsoun','cltcalipsoun','clcalipso','lidarBetaMol532','clcalipsoice','clcalipsoliq','clcalipsoun','clcalipsotmp','clcalipsotmpice','clcalipsotmpliq','clcalipsotmpun','parasolRefl','cltlidarradar','clcalipso2','cltisccp','pctisccp','tauisccp','albisccp','meantbisccp','meantbclrisccp','boxtauisccp','boxptopisccp','cltmodis','clwmodis','climodis','clhmodis','clmmodis','cllmodis','tautmodis','tauwmodis','tauimodis','tautlogmodis','tauwlogmodis','tauilogmodis','reffclwmodis','reffclimodis','pctmodis','lwpmodis','iwpmodis','toffset','fracout','atb532','cfadLidarsr532','dbze94','cfadDbze94','clisccp','clmodis','clMISR']#,'ZLAT_COSP','ZLON_COSP','PLSM_COSP','PMU0_COSP','PEMIS_COSP','PTS_COSP','ZAP_COSP','ZAPH_COSP','ZAPHI_COSP','ZAPHIF_COSP','ZU_COSP','ZV_COSP','ZT_COSP','ZQ_COSP','ZRH_COSP','ZOZN_COSP','ZCLFR_COSP','ZCLFRCC_COSP','ZMRLSLIQ_COSP','ZMRLSICE_COSP','ZMRCCLIQ_COSP','ZMRCCICE_COSP','ZFLLSRAIN_COSP','ZFLLSSNOW_COSP','ZFLCCRAIN_COSP','ZFLCCSNOW_COSP','ZRADLP_COSP','ZRADIP_COSP','ZTAUSW_COSP','ZEMILW_COSP']

var2save = var2save + ['delta_t','delta_q','d_delta_t_gw',\
	    'omgb_dth','dp_omgb','dt_KE','dq_KE','dt_PBL','dq_PBL',\
	    'omg_w','dp_delt_omg','spread_w','delta_th',\
	    'dt_wake','dq_wake','t_undi','q_undi',\
	    'd_delta_t','d_delta_q','hw','sigmaw',\
	    'wake_pe','wake_fip','wake_gfl','Cstar','wdens',\
	    'dt_dn','dt_up','dq_dn','dq_up','Mf_up','Mf_dn','sigd','omgb',\
	    't_undi_pcmt','q_undi_pcmt','t_wake_pcmt','q_wake_pcmt',\
	    'qw_undi','qw_wake','Tw_wake','Tw_undi','qsat_undi','qsat_wake',\
	    'alpha_up','w_up','omega_up','alpha_dn','w_dn','omega_dn','cape','T_up','qv_up',\
            'w_up_bud','dw_buoy','dw_fric','dw_Kd','dw_entr','dw_transp','buoy',\
            'Mf','eps_u','eps_u_org','eps_u_tur','entr_u','detr_u','dTv_up',\
            'tnqadv','tnqvpbl','tnqvc','tnqvlscp','tnqvd',\
            'tntadv','tntrlw','tntrsw','tntlscp','tntc','tntpbl','tntd']

lfalaf = '/home/roehrig/Logiciels/LFA/Romain/bin/lfalaf'

tunits = 'seconds since 2006-07-10 0:0:0.0'

# Pour convert2p, niveau en hPa
#levout = [1000.,950.,900.,850.,800.,750.,700,650.,600,550.,500.,450.,400.,350.,300.,250.,200.,150.,100.,50.]
levout = []
for i in range(1,41):
  levout.append(i*25.)
levout.reverse()

levoutz = []
for i in range(0,401):
  levoutz.append(i*50.)
levoutz.reverse()

# Niveau de print (0, 1 ou 2)
verbose = 2