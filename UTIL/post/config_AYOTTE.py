#saveall = True
saveall = False


var2save = ['th','ta','hus','ua','va','pres','wap','ql','qi','qr','qsn','cl','prw','pr','hfls','hfss','prw','tnqadv','tnqvpbl','tnqvc','tnqvlscp','tnqvd','tntadv','tntrlw','tntrsw','tntlscp','tntc','tntpbl','tntd','ts','hur','Q1','Q2','qlc','qic','qrc','qsnc','tke','tnupbl','tnuc','tnud','tnvpbl','tnvc','tnvd']

var2save = var2save + ['alpha_up','w_up','omega_up','alpha_dn','w_dn','omega_dn','cape','T_up','qv_up','omega_ref','w_up_bud','dw_buoy','dw_fric','dw_Kd','dw_entr','dw_transp','buoy','Mf','eps_u','eps_u_org','eps_u_tur','entr_u','detr_u','dTv_up','wpqp_pbl','wpthp_pbl','wpqp_conv','wpthp_conv','aipcmt','knnd','knlab','wpup_conv','wpup_pbl','wpvp_conv','wpvp_pbl']

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
