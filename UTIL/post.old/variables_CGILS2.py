#pres et presH joue un role particulier donc doivent etre definies
varnames = {}
names = {}
units = {}

#---------------------------------------------------------------------------------------------------
#                        Variable in ARPEGE/MUSC
#---------------------------------------------------------------------------------------------------

# 1. Variables dynamiques et thermodynamiques
#      pres, presH, zg, zgH, rho, T, qv, hur, ql, qi, qr, qsn, tke, th, thv, thl, thlv, qsat,
#      ua, va, wa, wap
#      qlc, qic, qrc, qsnc, qlshc, qishc, qrshc, qsnshc
# 2. Variables nuageuses
#      cl, cls, cldtot, cltc, cldlow, cltm, clth
# 3. Variables pluies
#      precls, snowls, precc, snowc, prect, precl, precc, ppr, pprls, pprc    
# 4. Variables rayonnement
#      rsdt, rsdtcs, rldt, rldtcs, rsut, rsutcs, flnt, flntc, fsnt, fsntc, rlt, rltcs
#      rsds, rsdscs, rsus, rsuscs, rlds, rldscs, rlus, rluscs, fsns, fsnsc, flns, flnsc
#      SWd, SWu, SWdcs, SWucs, SWnet, LWd, LWu, LWdcs, LWucs, LWnet
#      mu1, mueff, I0, alb_ss
# 5. Variables flux de surface
#      sh, lh, hflsn
#      evap, evapi, evapn
#      Cd, Ch, Ce, Cdn, Chn, Cen
# 6. Variables integrees sur la colonne
#      precw, tglwp, iwp, cwp
# 7. Variables en surface
#      tsair, huss, hurs, uas, vas, pblh, ts
# 8.1 Tendances de la physique
#      Q1, Q2, Qrad
#      tnthl, tnqt
#      tdt_sw, tdt_lw, tntrswcs, tntrlwcs, tdt_turb, tdt_cond, tdt_deep, tntshc, tntd
#      tnthrswn tnthrlw, tnthrswcs, tnthrlwcs, tnthpbl, tnthlscp, tnthc, tnthshc, tnthd
#      qdt_turb, qdt_cond, qdt_deep, tnqvshc, tnqvd
#      tnupbl, tnuc, tnushc, tnud, tnvpbl, tnvc, tnvshc, tnvd
#      tnthlrswn tnthlrlw, tnthlrswcs, tnthlrlwcs, tnthlpbl, tnthllscp, tnthlc, tnthlshc, tnthld
#      tnqtpbl, tnqtlscp, tnqtc, tnqtshc, tnqtd
#      tnql, tnqi, tnqr, tnqsn
#      tnqlc, tnqic, tnqrc, tnqsnc
#      tnqlshc, tnqishc, tnqrshc, tnqsnc
# 8.2 Flux de la physique
#      wpqtp_pbl, wpthlp_pbl, wpqtp_conv, wpthlp_conv
# 9. Tendances liees au forcages
#      tdt_ls, tntnudg, qdt_ls, tnqnudg
# 10. Divers
#      Cp, Lv
# 11. Bilan eau et energie
#      qflux, qfluxPr, qfluxEv, dwater, efluxTOA, efluxSfc, denergy
#      RMSE9, RMSE0, RMSE1, RDMSE, msefluxSfc
#      iQadv, iQnud, iTadv, iTnud, iUadv, iUnud, iVadv, iVnud, iEadv, iEnud
#      efluxSfcRad, efluxSfcTurb, efluxSfcConv, efluxSfcAdj, efluxSfcPrSen, efluxSfcPrLat
#      iMSEadv, iMSEnud, iQw, iEw
# 12. Especes Gazeuses
#      rCO2, rCH4, rN2O, rNO2, rCFC11, rCFC12, rCFC22, rCCL4, rO3
# 13. Variables du schema de convection Bougeault 1985
#      alpha, Mf, Tu, Thu, qvu, qcu
# 14. Variables du schema PCMT
#      alpha_up, w_up, omega_up, alpha_dn, w_dn, omega_dn, cape, T_up, qv_up
# 15. Variables pour le wake
#      delta_t, delta_q, d_delta_t_gw, omgb_dth, omgb, dt_KE, dq_KE, dt_PBL, dq_PBL
#      omg_w, dp_delt_omg, spread_w, delta_th, dt_wake, dq_wake, t_undi, q_undi, 
#      d_delta_t, d_delta_q,
#      hw, sigmaw, wake_pe, wake_fip, wake_gfl, Cstar, wdens
#      dt_dn, dt_up, dq_dn, dq_up, Mf_dn, Mf_up, sigd, omgb
#      t_undi_pcmt, q_undi_pcmt, t_wake_pcmt, q_wake_pcmt
#      qw_undi, qw_wake, Tw_wake, Tw_undi, qsat_undi, qsat_wake

##########################################
# 1. Variables dynamiques et thermodynamiques

varnames['pres']  = 'PAPRSF'
varnames['p']  = 'PAPRSF'
varnames['presH'] = 'PAPRS'

varnames['zg']    = 'PAPHIF'
varnames['zgH']   = 'PAPHI'

varnames['rho']   = 'RHO'
varnames['T']    = 'PT'
varnames['qv']   = 'PQ'
varnames['hur']   = 'PRH'
varnames['ql']    = 'PQLI'
varnames['qi']    = 'PQICE'
varnames['qr']    = 'PQR'
varnames['qsn']   = 'PQSN'

varnames['qlc']   = 'PQLCONV'
varnames['qic']   = 'PQICONV'
varnames['qrc']   = 'PQRCONV'
varnames['qsnc']  = 'PQSCONV'

varnames['qlshc'] = 'PQLSHCONV'
varnames['qishc'] = 'PQISHCONV'
varnames['qrshc'] = 'PQRSHCONV'
varnames['qsnshc']= 'PQSSHCONV'

varnames['tke']   = 'PECT'

varnames['th']    = 'THETA'
varnames['thv']   = 'THETAV'
varnames['thl']   = 'THETAL'
varnames['thlv']  = 'THETAVL'

varnames['qsat']  = 'PQSAT'

varnames['ua']    = 'PU'
varnames['va']    = 'PV'
varnames['wa']    = 'ZW'
varnames['wap']   = 'ZOMEGA'

##########################################
# 2. Variables nuageuses

varnames['cloud']    = 'PNEB'
varnames['cls']   = 'ZNEBS'
varnames['cldtot']   = 'PCLCT'
varnames['cltc']  = 'PCLCC'
varnames['cldlow']  = 'PCLCL'
varnames['cltm']  = 'PCLCM'
varnames['clth']  = 'PCLCH'

##########################################
# 3. Variables pluies

#varnames['precls']= 'PFPLSL'
#varnames['snowls']= 'PFPLSN'
#varnames['precc'] = 'PFPLCL'
#varnames['snowc'] = 'PFPLCN'

varnames['prect']    = 'PRECS_TOT'
varnames['precl']  = 'PRECS_LS'
varnames['precc']   = 'PRECS_C'
varnames['ppr']   = 'PREC_TOT'
varnames['pprls'] = 'PREC_LS'
varnames['pprc']  = 'PREC_C'

##########################################
# 4. Variables rayonnement

varnames['rsdt']  = 'SW_TOA_dn'
varnames['rsdtcs']= 'SW_TOA_cs_dn'
varnames['rldt']  = 'LW_TOA_dn'
varnames['rldtcs']= 'LW_TOA_cs_dn'
varnames['rsut']  = 'SW_TOA_up'
varnames['rsutcs']= 'SW_TOA_cs_up'
varnames['flnt']  = 'LW_TOA_up'
varnames['flntc']= 'LW_TOA_cs_up'
varnames['fsnt']   = 'SW_TOA'
varnames['fsntc'] = 'SW_TOA_cs'
varnames['rlt']   = 'LW_TOA'
varnames['rltcs'] = 'LW_TOA_cs'

varnames['rsds']  = 'SW_Surf_dn'
varnames['rsdscs']= 'SW_Surf_cs_dn'
varnames['rsus']  = 'SW_Surf_up'
varnames['rsuscs']= 'SW_Surf_cs_up'
varnames['rlds']  = 'LW_Surf_dn'
varnames['rldscs']= 'LW_Surf_cs_dn'
varnames['rlus']  = 'LW_Surf_up'
varnames['rluscs']= 'LW_Surf_cs_up'
varnames['fsns']   = 'SW_Surf'
varnames['fsnsc'] = 'SW_Surf_cs'
varnames['flns']   = 'LW_Surf'
varnames['flnsc'] = 'LW_Surf_cs'

varnames['SWd'] = 'SWd'
varnames['SWu'] = 'SWu'
varnames['SWdcs'] = 'SWdcs'
varnames['SWucs'] = 'SWucs'
varnames['SWnet'] = 'PFRSO'

varnames['LWd'] = 'LWd'
varnames['LWu'] = 'LWu'
varnames['LWdcs'] = 'LWdcs'
varnames['LWucs'] = 'LWucs'
varnames['LWnet'] = 'PFRTH'

varnames['mu1'] = 'ZMU0'
varnames['mueff'] = 'ZMU0EFF'
varnames['I0'] = 'ZI0'
varnames['daydur'] = 'Day duration'

varnames['alb_ss'] = 'SW_ALB'

##########################################
# 5. Variables flux de surface

varnames['lh']  = 'PFCLL'
varnames['sh']  = 'PFCS'
varnames['hflsn'] = 'PFCLN'

varnames['evap'] = 'PFEVL'
varnames['evapi'] = 'PFEVI'
varnames['evapn'] = 'PFEVN'

varnames['Cd'] = 'PCD'
varnames['Ch'] = 'PCH'
varnames['Ce'] = 'PCE'

varnames['Cdn'] = 'PCDN'
varnames['Chn'] = 'ZCHN'
varnames['Cen'] = 'ZCEN'

##########################################
# 6. Variables integrees sur la colonne

varnames['precw']   = 'WVP'
varnames['tglwp']   = 'LWP'
varnames['iwp']   = 'IWP'
varnames['cwp']   = 'CWP'

##########################################
# 7. Variables en surface

varnames['tsair']   = 'PTCLS'
varnames['huss']  = 'PQCLS'
varnames['hurs']  = 'PRHCLS'
varnames['uas']   = 'PUCLS'
varnames['vas']   = 'PVCLS'

varnames['pblh']  = 'HCLA'

varnames['ts']    = 'PTS'

##########################################
# 8.1 Tendances de la physique

varnames['Q1']    = 'Q1'
varnames['Q2']    = 'Q2'
varnames['QRad']  = 'QRad'

varnames['tnth'] = 'TENDTHETA'
varnames['tnthl'] = 'TENDTHETAL'
varnames['tnqt']  = 'TENDQT'

varnames['tdt_sw']= 'TENDSWT'
varnames['tdt_lw']= 'TENDLWT'
varnames['tntrswcs']= 'TENDSWTCS'
varnames['tntrlwcs']= 'TENDLWTCS'
varnames['tdt_turb']= 'TENDTT'
varnames['tdt_cond'] = 'TENDST'
varnames['tdt_deep']  = 'TENDCT'
varnames['tntshc']  = 'TENDSHCT'
varnames['tntd']  = 'TENDMT'

varnames['tnthrsw']= 'TENDSWTH'
varnames['tnthrlw']= 'TENDLWTH'
varnames['tnthrswcs']= 'TENDSWTHCS'
varnames['tnthrlwcs']= 'TENDLWTHCS'
varnames['tnthpbl']= 'TENDTTH'
varnames['tnthlscp'] = 'TENDSTH'
varnames['tnthc']  = 'TENDCTH'
varnames['tnthshc']  = 'TENDSHCTH'
varnames['tnthd']  = 'TENDMTH'

varnames['qdt_turb'] = 'TENDTQ'
varnames['qdt_cond'] = 'TENDSQ'
varnames['qdt_deep'] = 'TENDCQ'
varnames['tnqvshc'] = 'TENDSHCQ'
varnames['tnqvd'] = 'TENDMQ'

varnames['tnupbl']= 'TENDTU'
varnames['tnuc']  = 'TENDCU'
varnames['tnushc']  = 'TENDSHCU'
varnames['tnud']  = 'TENDMU'

varnames['tnvpbl']= 'TENDTV'
varnames['tnvc']  = 'TENDCV'
varnames['tnvshc']  = 'TENDSHCV'
varnames['tnvd']  = 'TENDMV'

varnames['tnthlrsw']= 'TENDSWTHL'
varnames['tnthlrlw']= 'TENDLWTHL'
varnames['tnthlrswcs']= 'TENDSWTHLCS'
varnames['tnthlrlwcs']= 'TENDLWTHLCS'
varnames['tnthlpbl'] = 'TENDTTHL'
varnames['tnthllscp'] = 'TENDSTHL'
varnames['tnthlc']= 'TENDCTHL'
varnames['tnthlshc']= 'TENDSHCTHL'
varnames['tnthld']= 'TENDMTHL'

varnames['tnqtpbl'] = 'TENDTQT'
varnames['tnqtlscp'] = 'TENDSQT'
varnames['tnqtc'] = 'TENDCQT'
varnames['tnqtshc'] = 'TENDSHCQT'
varnames['tnqtd'] = 'TENDMQT'

varnames['tnql']  = 'TENDQL'
varnames['tnqi']  = 'TENDQI'
varnames['tnqr']  = 'TENDQR'
varnames['tnqsn']  = 'TENDQS'

varnames['tnqlc'] = 'TENDQLCONV'
varnames['tnqic'] = 'TENDQICONV'
varnames['tnqrc'] = 'TENDQRCONV'
varnames['tnqsnc'] = 'TENDQSCONV'

varnames['tnqlshc'] = 'TENDQLSHCONV'
varnames['tnqishc'] = 'TENDQISHCONV'
varnames['tnqrshc'] = 'TENDQRSHCONV'
varnames['tnqsnshc'] = 'TENDQSSHCONV'

##########################################
# 8.2 Flux de la physique

varnames['wpqtp_pbl'] = 'ZWPQT_tur'
varnames['wpthlp_pbl'] = 'WTHL_tur'
varnames['wpqtp_conv'] = 'ZWPQT_dee'
varnames['wpthlp_conv'] = 'WTHL_dee'

##########################################
# 9. Tendances liees au forcages

varnames['tdt_ls'] = 'ZFT_ADV'
varnames['tntnudg'] = 'ZFT_NUDG'
varnames['qdt_ls'] = 'ZFQ_ADV'
varnames['tnqnudg'] = 'ZFQ_NUDG'

##########################################
# 10. Divers

varnames['Cp'] = 'PCP'
varnames['Lv'] = 'PLH'

##########################################
# 11. Bilan eau et energie

varnames['qflux'] = 'WATERFLUX'
varnames['qfluxEv'] = 'WATERFLUXE'
varnames['qfluxPr'] = 'WATERFLUXP'
varnames['dwater'] = 'RDWATER'
varnames['efluxTOA'] = 'EFLUXTOA'
varnames['efluxSfc'] = 'EFLUXSFC'
varnames['denergy'] = 'RDENERGY'
varnames['RMSE9'] = 'RMSE9'
varnames['RMSE0'] = 'RMSE0'
varnames['RMSE1'] = 'RMSE1'
varnames['RDMSE'] = 'RDMSE'
varnames['msefluxSfc'] = 'MSEFLUXSFC'
varnames['iQadv'] = 'TOTFORCQADV'
varnames['iQnud'] = 'TOTFORCQNUD'
varnames['iTadv'] = 'TOTFORCTADV'
varnames['iTnud'] = 'TOTFORCTNUD'
varnames['iUadv'] = 'TOTFORCUADV'
varnames['iUnud'] = 'TOTFORCUNUD'
varnames['iVadv'] = 'TOTFORCVADV'
varnames['iVnud'] = 'TOTFORCVNUD'
varnames['iEadv'] = 'TOTFORCEADV'
varnames['iEnud'] = 'TOTFORCENUD'
varnames['efluxSfcRad'] = 'EFLUXSFCRAD'
varnames['efluxSfcTurb'] = 'EFLUXSFCTS'
varnames['efluxSfcConv'] = 'EFLUXSFCCS'
varnames['efluxSfcAdj'] = 'EFLUXSFCCAS'
varnames['efluxSfcPrSen'] = 'EFLUXSFC1'
varnames['efluxSfcPrLat'] = 'EFLUXSFC2'
varnames['iMSEadv'] = 'MSEADV'
varnames['iMSEnud'] = 'MSENUD'
varnames['iMSEw'] = 'MSEW'
varnames['iQw'] = 'WATERW'
varnames['iEw'] = 'ENERGYW'

##########################################
# 12. Especes Gazeuses

varnames['rCO2'] = 'ZCO2'
varnames['rCH4'] = 'ZCH4'
varnames['rN2O'] = 'ZN2O'
varnames['rNO2'] = 'ZNO2'
varnames['rCFC11'] = 'ZC11'
varnames['rCFC12'] = 'ZC12'
varnames['rCFC22'] = 'ZC22'
varnames['rCCL4'] = 'ZCL4'
varnames['rO3'] = 'ZOZN'

##########################################
# 13. Variables du schema de convection Bougeault 1985

varnames['alpha'] = 'ZALF'
varnames['mu'] = 'ZFORM2'
varnames['Tu'] = 'ZTN'
varnames['Thu'] = 'ZTHN'
varnames['qvu'] = 'ZQN'
varnames['qcu'] = 'ZLN'

##########################################
# 14. Variables du schema PCMT

varnames['alpha_up'] = 'PUDAL'
varnames['w_up'] = 'ZUDW' #'ZUDW_ACPCMT' #'PUDW'
varnames['omega_up'] = 'PUDOM'
varnames['alpha_dn'] = 'PDDAL'
varnames['w_dn'] = 'ZDDW' #'ZDDW_ACPCMT' #'PDDW'
varnames['omega_dn'] = 'PDDOM'
varnames['cape'] = 'PCAPE'
varnames['T_up'] = 'ZTU_PCMT'
varnames['qv_up'] = 'ZQU_PCMT'

##########################################
# 15. Variables pour le wake

varnames['delta_t'] = 'PWAKEDELTAT'
varnames['delta_q'] = 'PWAKEDELTAQ'
varnames['d_delta_t_gw'] = 'PWAKEDDELTATGW'
varnames['omgb_dth'] = 'PWAKEOMGBDTH'
varnames['dp_omgb'] = 'PWAKEDPOMGB'
varnames['dt_KE'] = 'PWAKEDTKE'
varnames['dq_KE'] = 'PWAKEDQKE'
varnames['dt_PBL'] = 'PWAKEDTPBL'
varnames['dq_PBL'] = 'PWAKEDQPBL'
varnames['omg_w'] = 'PWAKEOMG'
varnames['dp_delt_omg'] = 'PWAKEDPDELTOMG'
varnames['spread_w'] = 'PWAKESPREAD'
varnames['delta_th'] = 'PWAKEDTH'
varnames['dt_wake'] = 'PDTWAKE'
varnames['dq_wake'] = 'PDQWAKE'
varnames['t_undi'] = 'PTUNDI'
varnames['q_undi'] = 'PQUNDI'
varnames['d_delta_t'] = 'PWAKEDDELTAT'
varnames['d_delta_q'] = 'PWAKEDDELTAQ'
varnames['hw'] = 'PWAKEH'
varnames['sigmaw'] = 'PWAKES'
varnames['wake_pe'] = 'PWAKEPE'
varnames['wake_fip'] = 'PWAKEFIP'
varnames['wake_gfl'] = 'PWAKEGFL'
varnames['Cstar'] = 'PWAKECSTAR'
varnames['wdens'] = 'PWAKEDENS'
varnames['dt_dn'] = 'ZDTDWN'
varnames['dt_up'] = 'ZDTA'
varnames['dq_dn'] = 'ZDQDWN'
varnames['dq_up'] = 'ZDQA'
varnames['Mf_dn'] = 'ZMDWN'
varnames['Mf_up'] = 'ZMUP'
varnames['sigd'] = 'ZSIGD'
varnames['omgb'] = 'POMGB'
varnames['t_undi_pcmt'] = 'ZTUNDI'
varnames['q_undi_pcmt'] = 'ZQUNDI'
varnames['t_wake_pcmt'] = 'ZTWAKE'
varnames['q_wake_pcmt'] = 'ZQWAKE'
varnames['qw_undi'] = 'ZQWUNDI'
varnames['qw_wake'] = 'ZQWWAKE'
varnames['Tw_wake'] = 'ZTWWAKE'
varnames['Tw_undi'] = 'ZTWUNDI'
varnames['qsat_undi'] = 'ZQSATUNDI'
varnames['qsat_wake'] = 'ZQSATWAKE'

#---------------------------------------------------------------------------------------------------
#                        Long name of variables
#---------------------------------------------------------------------------------------------------

##########################################
# 1. Variables dynamiques et thermodynamiques

names['pres']  = 'Pressure on Full Levels'
names['p']  = 'Pressure on Full Levels'
names['presH'] = 'Pressure on Half Levels'

names['zg']    = 'Geopotential on Full Levels'
names['zgH']   = 'Geopotential on Half Levels'

names['rho']   = 'Air Volumic Mass'
names['T']    = 'Air Temperature'
names['qv']   = 'Specific Humidity'
names['hur']   = 'Relative Humidity'
names['ql']    = 'Specific Mass of Cloud Liquid Water'
names['qi']    = 'Specific Mass of Cloud Ice Water'
names['qr']    = 'Specific Mass of Rain'
names['qsn']   = 'Specific Mass of Snow'

names['qlc']   = 'Specific Mass of Convective Liquid Water'
names['qic']   = 'Specific Mass of Convective Ice Water'
names['qrc']   = 'Specific Mass of Convective Rain'
names['qsnc']  = 'Specific Mass of Convective Snow'

names['qlshc'] = 'Specific Mass of Shallow Convection Liquid Water'
names['qishc'] = 'Specific Mass of Shallow Convection Ice Water'
names['qrshc'] = 'Specific Mass of Shallow Convection Rain'
names['qsnshc']= 'Specific Mass of Shallow Convection Snow'

names['tke']   = 'Turbulent Kinetic Energy'

names['th']    = 'Potential Temperature'
names['thv']    = 'Virtual Potential Temperature'
names['thl']    = 'Liquid Potential Temperature'
names['thlv']    = 'Virtual Potential Temperature'

names['qsat']  = 'Specific Humidity at Saturation'

names['ua']    = 'Zonal Wind'
names['va']    = 'Meridional Wind'
names['wa']    = 'Vertical Velocity'
names['wap']   = 'Vertical Pressure Velocity'

##########################################
# 2. Variables nuageuses

names['cloud']    = 'Cloud Fraction'
names['cls']   = 'Stratiform Cloud Fraction'
names['cldtot']   = 'Total Cloud Fraction'
names['cltc']  = 'Total Convective Cloud Fraction'
names['cldlow']  = 'Total Low Cloud Fraction'
names['cltm']  = 'Total Mid Cloud Fraction'
names['clth']  = 'Total High Cloud Fraction'

##########################################
# 3. Variables pluies

#names['precls']= 'Large-Scale Precipitation Flux'
#names['snowls']= 'Large-Scale Snow Flux'
#names['precc'] = 'Convective Precipitation Flux'
#names['snowc'] = 'Convective Snow Flux'

names['prect']    = 'Surface Precipitation'
names['precl']  = 'Surface Large-Scale Precipitation'
names['precc']   = 'Surface Convective Precipitation'
names['ppr']   = 'Total Precipitation Profile'
names['pprls'] = 'Large-Scale Precipitation Profile'
names['pprc']  = 'Convective Precipitation Profile'

##########################################
# 4. Variables rayonnement

names['rsdt']  = 'Downward SW Radiation at TOA'
names['rsdtcs']= 'Clear-sky Downward SW Radiation at TOA'
names['rldt']  = 'Downward LW Radiation at TOA'
names['rldtcs']= 'Clear-sky Downward LW Radiation at TOA'
names['rsut']  = 'Upward SW Radiation at TOA'
names['rsutcs']= 'Clear-sky Upward SW Radiation at TOA'
names['flnt']  = 'Upward LW Radiation at TOA'
names['flntc']= 'Clear-sky Upward LW Radiation at TOA'
names['fsnt']   = 'SW Radiation at TOA'
names['fsntc'] = 'Clear-sky SW Radiation at TOA'
names['rlt']   = 'LW Radiation at TOA'
names['rltcs'] = 'Clear-sky LW Radiation at TOA'

names['rsds']  = 'Downward SW Radiation at Surface'
names['rsdscs']= 'Clear-sky Downward SW Radiation at Surface'
names['rsus']  = 'Upward SW Radiation at Surface'
names['rsuscs']= 'Clear-sky Upward SW Radiation at Surface'
names['rlds']  = 'Downward LW Radiation at Surface'
names['rldscs']= 'Clear-sky Downward LW Radiation at Surface'
names['rlus']  = 'Upward LW Radiation at Surface'
names['rluscs']= 'Clear-sky Upward LW Radiation at Surface'
names['fsns']   = 'SW Radiation at Surface'
names['fsnsc'] = 'Clear-sky SW Radiation at Surface'
names['flns']   = 'LW Radiation at Surface'
names['flnsc'] = 'Clear-sky LW Radiation at Surface'

names['SWd'] = 'Downward SW Flux'
names['SWu'] = 'Upward SW Flux'
names['SWdcs'] = 'CS Downward SW Flux'
names['SWucs'] = 'CS Upward SW Flux'
names['SWnet'] = 'Net SW Flux'

names['LWd'] = 'Downward LW Flux'
names['LWu'] = 'Upward LW Flux'
names['LWdcs'] = 'CS Downward LW Flux'
names['LWucs'] = 'CS Upward LW Flux'
names['LWnet'] = 'Net LW Flux'

names['mu1'] = 'Sinus of zenith angle'
names['mueff'] = 'Sinus of effective zenith angle'
names['I0'] = 'Solar irradiance'
names['daydur'] = 'Day duration'

names['alb_ss'] = 'SW Surface Albedo'

##########################################
# 5. Variables flux de surface

names['lh']  = 'Surface Latent Heat Flux'
names['sh']  = 'Surface Sensible Heat Flux'
names['hflsn'] = 'Surface Latent Heat Flux over Snow or Ice'

names['evap']  = 'Evaporation over liquid water (or wet soil)'
names['evapi'] = 'Evaporation over frozen soil'
names['evapn'] = 'Evaporation over snow (or ice) and frozen soil'

names['Cd'] = 'Surface Exchange Coefficient for Wind'
names['Ch'] = 'Surface Exchange Coefficient for Heat'
names['Ce'] = 'Surface Exchange Coefficient for Water Vapor'

names['Cdn'] = 'Surface Exchange Neutral Coefficient for Wind'
names['Chn'] = 'Surface Exchange Neutral Coefficient for Heat'
names['Cen'] = 'Surface Exchange Neutral Coefficient for Water Vapor'

##########################################
# 6. Variables integrees sur la colonne

names['precw']   = 'Precipitable Water'
names['tglwp']   = 'Liquid Water Path'
names['iwp']   = 'Ice Water Path'
names['cwp']   = 'Cloud Water Path'

##########################################
# 7. Variables en surface

names['tsair']   = '2-meter Air Temperature'
names['huss']  = '2-meter Specific Humidity'
names['hurs']  = '2-meter Relative Humidity'
names['uas']   = '10-meter Zonal Wind'
names['vas']   = '10-meter Meridional Wind'

names['pblh']  = 'Planetary Boundary Layer Height'

names['ts']    = 'Surface Temperature'

##########################################
# 8.1 Tendances de la physique

names['Q1']    = 'Apparent Heat Source'
names['Q2']    = 'Apparent Moisture Sink'
names['QRad']  = 'Radiative Heating Rate'

names['tnthl'] = 'Liquid Potential Temperature Tendency due to Physics'
names['tnqt'] = 'Total Water Tendency due to Physics'

names['tdt_sw'] = 'Temperature Tendency due to SW Radiation'
names['tdt_lw'] = 'Temperature Tendency due to LW Radiation'
names['tntrswcs'] = 'Temperature Tendency due to Clear-sky SW Radiation'
names['tntrlwcs'] = 'Temperature Tendency due to Clear-sky LW Radiation'
names['tdt_turb'] = 'Temperature Tendency due to Turbulence'
names['tdt_cond'] = 'Temperature Tendency due to Large-scale Condensation and Precipitation'
names['tdt_deep'] = 'Temperature Tendency due to Convection'
names['tntshc'] = 'Temperature Tendency due to Shallow Convection'
names['tntd'] = 'Temperature Tendency due to Other Processes'

names['tnthrsw'] = 'Potential Temperature Tendency due to SW Radiation'
names['tnthrlw'] = 'Potential Temperature Tendency due to LW Radiation'
names['tnthrswcs'] = 'Potential Temperature Tendency due to Clear-sky SW Radiation'
names['tnthrlwcs'] = 'Potential Temperature Tendency due to Clear-sky LW Radiation'
names['tnthpbl'] = 'Potential Temperature Tendency due to Turbulence'
names['tnthlscp'] = 'Potential Temperature Tendency due to Large-scale Condensation and Precipitation'
names['tnthc'] = 'Potential Temperature Tendency due to Convection'
names['tnthshc'] = 'Potential Temperature Tendency due to Shallow Convection'
names['tnthd'] = 'Potential Temperature Tendency due to Other Processes'

names['qdt_turb'] = 'Specific Humidity Tendency due to Turbulence'
names['qdt_cond'] = 'Specific Humidity Tendency due to Large-scale Condensation and Precipitation'
names['qdt_deep'] = 'Specific Humidity Tendency due to Convection'
names['tnqvshc'] = 'Specific Humidity Tendency due to Shallow Convection'
names['tnqvd'] = 'Specific Humidity Tendency due to Other Processes'

names['tnupbl'] = 'Zonal Wind Tendency due to Turbulence'
names['tnuc'] = 'Zonal Wind Tendency due to Convection'
names['tnushc'] = 'Zonal Wind Tendency due to Shallow Convection'
names['tnud'] = 'Zonal Wind Tendency due to Other Processes'

names['tnvpbl'] = 'Meridional Wind Tendency due to Turbulence'
names['tnvc'] = 'Meridional Wind Tendency due to Convection'
names['tnvshc'] = 'Meridional Wind Tendency due to Shallow Convection'
names['tnvd'] = 'Meridional Wind Tendency due to Other Processes'

names['tnthlrsw'] = 'Liquid Potential Temperature Tendency due to SW Radiation'
names['tnthlrlw'] = 'Liquid Potential Temperature Tendency due to LW Radiation'
names['tnthlrswcs'] = 'Liquid Potential Temperature Tendency due to Clear-sky SW Radiation'
names['tnthlrlwcs'] = 'Liquid Potential Temperature Tendency due to Clear-sky LW Radiation'
names['tnthlpbl'] = 'Liquid Potential Temperature Tendency due to Turbulence'
names['tnthllscp'] = 'Liquid Potential Temperature Tendency due to Large-scale Condensation and Precipitation'
names['tnthlc'] = 'Liquid Potential Temperature Tendency due to Convection'
names['tnthlshc'] = 'Liquid Potential Temperature Tendency due to Shallow Convection'
names['tnthld'] = 'Liquid Potential Temperature Tendency due to Other Processes'

names['tnqtpbl'] = 'Total Water Tendency due to Turbulence'
names['tnqtlscp'] = 'Total Water Tendency due to Large-scale Condensation and Precipitation'
names['tnqtc'] = 'Total Water Tendency due to Convection'
names['tnqtshc'] = 'Total Water Tendency due to Shallow Convection'
names['tnqtd'] = 'Total Water Tendency due to Other Processes'

names['tnql'] = 'Liquid Water Tendency due to Physics'
names['tnqi'] = 'Ice Water Tendency due to Physics'
names['tnqr'] = 'Rain Tendency due to Physics'
names['tnqsn'] = 'Snow Tendency due to Physics'

names['tnqlc'] = 'Convective Liquid Water Tendency due to Physics'
names['tnqic'] = 'Convective Ice Water Tendency due to Physics'
names['tnqrc'] = 'Convective Rain Tendency due to Physics'
names['tnqsnc'] = 'Convective Snow Tendency due to Physics'

names['tnqlshc'] = 'Shallow Convection Liquid Water Tendency due to Physics'
names['tnqishc'] = 'Shallow Convection Ice Water Tendency due to Physics'
names['tnqrshc'] = 'Shallow Convection Rain Tendency due to Physics'
names['tnqsnshc'] = 'Shallow Convection Snow Tendency due to Physics'

##########################################
# 8.2 Flux de la physique

names['wpqtp_pbl'] = 'Total Water Flux due to Turbulence'
names['wpthlp_pbl'] = 'Liquid Potential Temperature Flux due to Turbulence'
names['wpqtp_conv'] = 'Total Water flux due to Convection'
names['wpthlp_conv'] = 'Liquid Potential Temperature Flux due to Convection'

##########################################
# 9. Tendances liees au forcages

names['tdt_ls'] = 'Temperature Tendency due to Horizontal Advection'
names['tntnudg'] = 'Temperature Tendency due to Nudging'
names['qdt_ls'] = 'Specific Humidity Tendency due to Horizontal Advection'
names['tnqnudg'] = 'Specific Humidity Tendency due to Nudging'

##########################################
# 10. Divers

names['Cp'] = 'Heat Capacity of Air'
names['Lv'] = 'Latent Heat of Vaporization'

##########################################
# 11. Bilan eau et energie

names['qflux'] = 'Net Flux of water in the atmosphere'
names['qfluxPr'] = 'Net Flux of water at Surface due to Precipitation'
names['qfluxEv'] = 'Net Flux of water at Surface due to Evaporation'
names['dwater'] = 'RDWATER'
names['efluxTOA'] = 'Net Enthalpy Flux at TOA'
names['efluxSfc'] = 'Net Enthalpy Flux at Surface'
names['denergy'] = 'RDENERGY'

names['RMSE9'] = 'RMSE9'
names['RMSE0'] = 'RMSE0'
names['RMSE1'] = 'RMSE1'
names['RDMSE'] = 'RDMSE'
names['msefluxSfc'] = 'Net Moist Static Energy Flux at Surface'
names['iQadv'] = 'Horizontal Advection of moisture integrated over the atmospheric column'
names['iQnud'] = 'Moisture Tendency due to nudging integrated over the atmospheric column'
names['iTadv'] = 'Horizontal Advection of temperature integrated over the atmospheric column'
names['iTnud'] = 'Temperature Tendency due to nudging integrated over the atmospheric column'
names['iUadv'] = 'Horizontal Advection of zonal wind integrated over the atmospheric column'
names['iUnud'] = 'Zonal Wind Tendency due to nudging integrated over the atmospheric column'
names['iVadv'] = 'Horizontal Advection of meridional wind integrated over the atmospheric column'
names['iVnud'] = 'Meridional Wind Tendency due to nudging integrated over the atmospheric column'
names['iEadv'] = 'Horizontal Advection of enthalpy integrated over the atmospheric column'
names['iEnud'] = 'Enthalpy Tendency due to nudging integrated over the atmospheric column'

names['efluxSfcRad'] = 'Surface Enthalpy Flux due to radiation'
names['efluxSfcTurb'] = 'Surface Enthalpy Flux due to turbulence'
names['efluxSfcConv'] = 'Surface Enthalpy Flux due to convective eddies'
names['efluxSfcAdj'] = 'Surface Enthalpy Flux due to dry adjustment'
names['efluxSfcPrSen'] = 'Surface Enthalpy Flux due to sensible heat of precipitation'
names['efluxSfcPrLat'] = 'Surface Enthalpy Flux due to latent heat of precipitation'
names['iMSEadv'] = 'Horizontal Advection of moist static energy integrated over the atmospheric column'
names['iMSEnud'] = 'Moist Static Energy Tendency due to nudging integrated over the atmospheric column'
names['iMSEw'] = 'Vertical Advection of moist static energy integrated over the atmospheric column'
names['iQw'] = 'Vertical Advection of specific humidity integrated over the atmospheric column'
names['iEw'] = 'Vertical Advection of enthalpy integrated over the atmospheric column'

##########################################
# 12. Especes Gazeuses

names['rCO2'] = 'CO2 Mixing Ratio'
names['rCH4'] = 'CH4 Mixing Ratio'
names['rN2O'] = 'N2O Mixing Ratio'
names['rNO2'] = 'NO2 Mixing Ratio'
names['rCFC11'] = 'CFC11 Mixing Ratio'
names['rCFC12'] = 'CFC12 Mixing Ratio'
names['rCFC22'] = 'CFC22 Mixing Ratio'
names['rCCL4'] = 'CCL4 Mixing Ratio'
names['rO3'] = 'O3 Mixing Ratio'

##########################################
# 13. Variables du schema de convection Bougeault 1985

names['alpha'] = 'Closure coefficient'
names['mu'] = 'Mass Flux'
names['Tu'] = 'Updraft Temperature'
names['Thu'] = 'Updraft Potential Temperature'
names['qvu'] = 'Updraft Specific Humidity'
names['qcu'] = 'Updraft Condensed Water'

##########################################
# 14. Variables du schema PCMT

names['alpha_up'] = 'Convective Updraft Fraction'
names['w_up'] = 'Convective Updraft Velocity'
names['omega_up'] = 'Convective Updraft Pressure Velocity'
names['alpha_dn'] = 'Convective Downdraft Fraction'
names['w_dn'] = 'Convective Downdraft Velocity'
names['omega_dn'] = 'Convective Downdraft Pressure Velocity'
names['cape'] = 'CAPE'
names['T_up'] = 'Convective Updraft Temperature'
names['qv_up'] = 'Convective Updraft Specific Humidity'

##########################################
# 15. Variables pour le wake

names['delta_t'] = 'delta T'
names['delta_q'] = 'delta q'
names['d_delta_t_gw'] = ' delta T tendency due to GW'
names['omgb_dth'] = 'flux of delta_theta transported by LS omega'
names['dp_omgb'] = 'vertical gradient of large scale omega'
names['dt_KE'] = 'differential heating (wake-unperturbed) CONV'
names['dq_KE'] = 'differential moistening (wake-unperturbed) CONV'
names['dt_PBL'] = 'differential heating (wake-unperturbed) PBL'
names['dq_PBL'] = 'differential moistening (wake-unperturbed) PBL'
names['omg_w'] = 'Wake verticale velocity'
names['dp_delt_omg'] = 'vertical gradient of wake_omg'
names['spread_w'] = 'Spreading term in wake_delt'
names['delta_th'] = 'Potential Temperature Difference'
names['dt_wake'] = 'T tendency due to wake'
names['dq_wake'] = 'q tendency due to wake'
names['t_undi'] = 'Temperature in unperturbed area'
names['q_undi'] = 'Specific humidity in unperturbed area'
names['d_delta_t'] = 'delta T tendency'
names['d_delta_q'] = 'delta q tendency'
names['hw'] = 'Wake depth'
names['sigmaw'] = 'wake fractional area'
names['wake_pe'] = 'Wake Potential Energy (WAPE)'
names['wake_fip'] = 'wake ALP'
names['wake_gfl'] = 'Wake Gust Front Length'
names['Cstar'] = 'Wake spreading velocity'
names['wdens'] = 'Wake density'
names['dt_dn'] = 'T tendendy due to downdrafts'
names['dt_up'] = 'T tendency due to updrafts'
names['dq_dn'] = 'qv tendendy due to downdrafts'
names['dq_up'] = 'qv tendency due to updrafts'
names['Mf_dn'] = 'Downdraft Mass flux'
names['Mf_up'] = 'Updraft Mass flux'
names['sigd'] = 'Downdraft area fraction'
names['omgb'] = 'LS Vertical Velocity'
names['t_undi_pcmt'] = 'Temperature in undisturbed area (PCMT)'
names['q_undi_pcmt'] = 'Specific Humidity in undisturbed area (PCMT)'
names['t_wake_pcmt'] = 'Temperature in wake (PCMT)'
names['q_wake_pcmt'] = 'Specific Humidity in wake (PCMT)'
names['qw_undi'] = 'Wet-bulb Specific Humidity in undisturbed area'
names['qw_wake'] = 'Wet-bulb Specific Humidity in wake'
names['Tw_wake'] = 'Wet-bulb Temperature in wake'
names['Tw_undi'] = 'Wet-bulb Temperature in undisturbed area'
names['qsat_undi'] = 'Saturation Specific Humidity in undisturbed area'
names['qsat_wake'] = 'Saturation Specific Humidity in wake'


#---------------------------------------------------------------------------------------------------
#                        Units
#---------------------------------------------------------------------------------------------------

##########################################
# 1. Variables dynamiques et thermodynamiques

units['pres']  = 'Pa'
units['p']  = 'Pa'
units['presH'] = 'Pa'

units['zg']    = 'm2/s2'
units['zgH']   = 'm2/s2'

units['rho']   = 'kg/m3'
units['T']    = 'K'
units['qv']   = 'kg/kg'
units['hur']   = '-'
units['ql']    = 'kg/kg'
units['qi']    = 'kg/kg'
units['qr']    = 'kg/kg'
units['qsn']   = 'kg/kg'

units['qlc']   = 'kg/kg'
units['qic']   = 'kg/kg'
units['qrc']   = 'kg/kg'
units['qsnc']  = 'kg/kg'

units['qlshc'] = 'kg/kg'
units['qishc'] = 'kg/kg'
units['qrshc'] = 'kg/kg'
units['qsnshc']= 'kg/kg'


units['tke']   = 'm2/s2'

units['th']    = 'K'
units['thv']    = 'K'
units['thl']    = 'K'
units['thlv']    = 'K'

units['qsat']  = 'kg/kg'

units['ua']    = 'm/s'
units['va']    = 'm/s'
units['wa']    = 'm/s'
units['wap']   = 'Pa/s'

##########################################
# 2. Variables nuageuses

units['cloud']    = '-'
units['cls']   = '-'
units['cldtot']   = '-'
units['cltc']  = '-'
units['cldlow']  = '-'
units['cltm']  = '-'
units['clth']  = '-'

##########################################
# 3. Variables pluies

#units['precls']= 'kg/m2/s'
#units['snowls']= 'kg/m2/s'
#units['precc'] = 'kg/m2/s'
#units['snowc'] = 'kg/m2/s'

units['prect']  = 'kg/m2/s'
units['precl']  = 'kg/m2/s'
units['precc']  = 'kg/m2/s'
units['ppr'] = 'kg/m2/s'
units['pprls'] = 'kg/m2/s'
units['pprc'] = 'kg/m2/s'

##########################################
# 4. Variables rayonnement

units['rsdt']  = 'W/m2'
units['rsdtcs']= 'W/m2'
units['rldt']  = 'W/m2'
units['rldtcs']= 'W/m2'
units['rsut']  = 'W/m2'
units['rsutcs']= 'W/m2'
units['flnt']  = 'W/m2'
units['flntc']= 'W/m2'
units['fsnt']   = 'W/m2'
units['fsntc'] = 'W/m2'
units['rlt']   = 'W/m2'
units['rltcs'] = 'W/m2'

units['rsds']  = 'W/m2'
units['rsdscs']= 'W/m2'
units['rsus']  = 'W/m2'
units['rsuscs']= 'W/m2'
units['rlds']  = 'W/m2'
units['rldscs']= 'W/m2'
units['rlus']  = 'W/m2'
units['rluscs']= 'W/m2'
units['fsns']   = 'W/m2'
units['fsnsc'] = 'W/m2'
units['flns']   = 'W/m2'
units['flnsc'] = 'W/m2'

units['SWd'] = 'W/m2'
units['SWu'] = 'W/m2'
units['SWdcs'] = 'W/m2'
units['SWucs'] = 'W/m2'
units['SWnet'] = 'W/m2'

units['LWd'] = 'W/m2'
units['LWu'] = 'W/m2'
units['LWdcs'] = 'W/m2'
units['LWucs'] = 'W/m2'
units['LWnet'] = 'W/m2'

units['mu1'] = '-'
units['mueff'] = '-'
units['I0'] = 'W/m2'
units['daydur'] = 's'

units['alb_ss'] = '-'

##########################################
# 5. Variables flux de surface

units['lh']  = 'W/m2'
units['sh']  = 'W/m2'
units['hflsn'] = 'W/m2'

units['evap'] = 'kg/m2/s'
units['evapi'] = 'kg/m2/s'
units['evapn'] = 'kg/m2/s'

units['Cd'] = '-'
units['Ch'] = '-'
units['Ce'] = '-'

units['Cdn'] = '-'
units['Chn'] = '-'
units['Cen'] = '-'

##########################################
# 6. Variables integrees sur la colonne

units['precw']   = 'kg/m2'
units['tglwp']   = 'kg/m2'
units['iwp']   = 'kg/m2'
units['cwp']   = 'kg/m2'

##########################################
# 7. Variables en surface

units['tsair']   = 'K'
units['huss']  = 'kg/kg'
units['hurs']  = '-'
units['uas']   = 'm/s'
units['vas']   = 'm/s'

units['pblh']  = 'm'

units['ts']    = 'K'

##########################################
# 8.1 Tendances de la physique

units['Q1']    = 'K/s'
units['Q2']    = 'K/s'
units['QRad']  = 'K/s'

units['tnthl'] = 'K/s'
units['tnqt'] = 'kg/kg/s'

units['tdt_sw'] = 'K/s'
units['tdt_lw'] = 'K/s'
units['tntrswcs'] = 'K/s'
units['tntrlwcs'] = 'K/s'
units['tdt_turb'] = 'K/s'
units['tdt_cond'] = 'K/s'
units['tdt_deep'] = 'K/s'
units['tntshc'] = 'K/s'
units['tntd'] = 'K/s'

units['tnthrsw'] = 'K/s'
units['tnthrlw'] = 'K/s'
units['tnthrswcs'] = 'K/s'
units['tnthrlwcs'] = 'K/s'
units['tnthpbl'] = 'K/s'
units['tnthlscp'] = 'K/s'
units['tnthc'] = 'K/s'
units['tnthshc'] = 'K/s'
units['tnthd'] = 'K/s'

units['qdt_turb'] = 'kg/kg/s'
units['qdt_cond'] = 'kg/kg/s'
units['qdt_deep'] = 'kg/kg/s'
units['tnqvshc'] = 'kg/kg/s'
units['tnqvd'] = 'kg/kg/s'

units['tnupbl'] = 'm/s2'
units['tnuc'] = 'm/s2'
units['tnushc'] = 'm/s2'
units['tnud'] = 'm/s2'

units['tnvpbl'] = 'm/s2'
units['tnvc'] = 'm/s2'
units['tnvshc'] = 'm/s2'
units['tnvd'] = 'm/s2'

units['tnthlrsw'] = 'K/s'
units['tnthlrlw'] = 'K/s'
units['tnthlrswcs'] = 'K/s'
units['tnthlrlwcs'] = 'K/s'
units['tnthlpbl'] = 'K/s'
units['tnthllscp'] = 'K/s'
units['tnthlc'] = 'K/s'
units['tnthlshc'] = 'K/s'
units['tnthld'] = 'K/s'

units['tnqtpbl'] = 'kg/kg/s'
units['tnqtlscp'] = 'kg/kg/s'
units['tnqtc'] = 'kg/kg/s'
units['tnqtshc'] = 'kg/kg/s'
units['tnqtd'] = 'kg/kg/s'

units['tnql'] = 'kg/kg/s'
units['tnqi'] = 'kg/kg/s'
units['tnqr'] = 'kg/kg/s'
units['tnqsn'] = 'kg/kg/s'

units['tnqlc'] = 'kg/kg/s'
units['tnqic'] = 'kg/kg/s'
units['tnqrc'] = 'kg/kg/s'
units['tnqsnc'] = 'kg/kg/s'

units['tnqlshc'] = 'kg/kg/s'
units['tnqishc'] = 'kg/kg/s'
units['tnqrshc'] = 'kg/kg/s'
units['tnqsnshc'] = 'kg/kg/s'

##########################################
# 8.2 Flux de la physique

units['wpqtp_pbl'] = 'kg kg-1 m s-1'
units['wpthlp_pbl'] = 'K m s-1'
units['wpqtp_conv'] = 'kg kg-1 m s-1'
units['wpthlp_conv'] = 'K m s-1'

##########################################
# 9. Tendances liees au forcages

units['tdt_ls'] = 'K/s'
units['tntnudg'] = 'K/s'
units['qdt_ls'] = 'kg/kg/s'
units['tnqnudg'] = 'kg/kg/s'

##########################################
# 10. Divers

units['Cp'] = 'J/K/kg'
units['Lv'] = 'J/kg'

##########################################
# 11. Bilan eau et energie

units['qflux'] = 'kg/m2/s'
units['qfluxPr'] = 'kg/m2/s'
units['qfluxEv'] = 'kg/m2/s'
units['dwater'] = 'kg/m2'
units['efluxTOA'] = 'W/m2'
units['efluxSfc'] = 'W/m2'
units['denergy'] = 'J/kg'

units['RMSE9'] = '-'
units['RMSE0'] = '-'
units['RMSE1'] = '-'
units['RDMSE'] = '-'
units['msefluxSfc'] = 'W/m2'
units['iQadv'] = 'kg/m2/s'
units['iQnud'] = 'kg/m2/s'
units['iTadv'] = 'K kg/m2/s'
units['iTnud'] = 'K kg/m2/s'
units['iUadv'] = 'm2/s2'
units['iUnud'] = 'm2/s2'
units['iVadv'] = 'm2/s2'
units['iVnud'] = 'm2/s2'
units['iEadv'] = 'W/m2'
units['iEnud'] = 'W/m2'

units['efluxSfcRad'] = 'W/m2'
units['efluxSfcTurb'] = 'W/m2'
units['efluxSfcConv'] = 'W/m2'
units['efluxSfcAdj'] = 'W/m2'
units['efluxSfcPrSen'] = 'W/m2'
units['efluxSfcPrLat'] = 'W/m2'

units['iMSEadv'] = 'W/m2'
units['iMSEnud'] = 'W/m2'
units['iMSEw'] = 'W/m2'
units['iQw'] = 'kg/m2/s'
units['iEw'] = 'W/m2'

##########################################
# 12. Especes Gazeuses

units['rCO2'] = 'kg/kg'
units['rCH4'] = 'kg/kg'
units['rN2O'] = 'kg/kg'
units['rNO2'] = 'kg/kg'
units['rCFC11'] = 'kg/kg'
units['rCFC12'] = 'kg/kg'
units['rCFC22'] = 'kg/kg'
units['rCCL4'] = 'kg/kg'
units['rO3'] = 'kg/kg'

##########################################
# 13. Variables du schema de convection Bougeault 1985

units['alpha'] = '-'
units['mu'] = 'kg/m2/s'
units['Tu'] = 'K'
units['Thu'] = 'K'
units['qvu'] = 'kg/kg'
units['qcu'] = 'kg/kg'

##########################################
# 14. Variables du schema PCMT

units['alpha_up'] = '-'
units['w_up'] = 'm/s'
units['omega_up'] = 'Pa/s'
units['alpha_dn'] = '-'
units['w_dn'] = 'm/s'
units['omega_dn'] = 'Pa/s'
units['cape'] = 'J/kg'
units['T_up'] = 'K'
units['qv_up'] = 'kg/kg'

##########################################
# 15. Variables pour le wake

units['delta_t'] = 'K'
units['delta_q'] = 'kg/kg'
units['d_delta_t_gw'] = 'K/s'
units['omgb_dth'] = 'K/s'
units['dp_omgb'] = '/s'
units['dt_KE'] = 'K/s'
units['dq_KE'] = 'kg/kg/s'
units['dt_PBL'] = 'K/s'
units['dq_PBL'] = 'kg/kg/S'
units['omg_w'] = 'Pa/s'
units['dp_delt_omg'] = '/s'
units['spread_w'] = 'K/s'
units['delta_th'] = 'K'
units['dt_wake'] = 'K/s'
units['dq_wake'] = 'K_s'
units['t_undi'] = 'K'
units['q_undi'] = 'kg/kg'
units['d_delta_t'] = 'K/s'
units['d_delta_q'] = 'kg/kg/s'
units['hw'] = 'm'
units['sigmaw'] = '-'
units['wake_pe'] = 'J/kg'
units['wake_fip'] = 'W/m2'
units['wake_gfl'] = 'm'
units['Cstar'] = 'm/s'
units['wdens'] = '/m2'
units['dt_dn'] = 'K/s'
units['dt_up'] = 'K/s'
units['dq_dn'] = 'kg/kg/s'
units['dq_up'] = 'kg/kg/s'
units['Mf_dn'] = 'kg/m2/s'
units['Mf_up'] = 'kg/m2/s'
units['sigd'] = '-'
units['omgb'] = 'Pa/s'
units['t_undi_pcmt'] = 'K'
units['q_undi_pcmt'] = 'kg/kg'
units['t_wake_pcmt'] = 'K'
units['q_wake_pcmt'] = 'kg/kg'
units['qw_undi'] = 'kg/kg'
units['qw_wake'] = 'kg/kg'
units['Tw_wake'] = 'K'
units['Tw_undi'] = 'K'
units['qsat_undi'] = 'kg/kg'
units['qsat_wake'] = 'kg/kg'


