import variables as VV

variables = VV.varnames.keys()

for var in variables:
    if var not in VV.units:
        VV.units[var] = '??'
    if var not in VV.coefs:
        VV.coefs[var] = 1.
    if var not in VV.names:
        VV.names[var] = VV.varnames[var]


def write(f, var):
    tmp = "    {0:20}: {{'varname':{1:>20}, 'units':{2:>20}, 'coef':{3:>10}, 'name':{4}}}, \n".format("'"+var+"'", "'"+VV.varnames[var]+"'", "'"+VV.units[var]+"'", VV.coefs[var], "'"+VV.names[var]+"'")
    f.write(tmp)

def write_vargroup(f, vargroup):
    tmp = [var.rstrip().lstrip() for var in vargroup.split(',')]
    for var in sorted(tmp):
        write(f, var)


f = open('variables_new.py','w')

tmp = """#!/usr/bin/env python3
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

"""
f.write(tmp)

f.write("# 1. Dynamical and thermodynamical variables\n")
f.write("# 2. Cloud variables\n")
f.write("# 3. Precipitation variables\n")
f.write("# 4. Radiation variables\n")
f.write("# 5. Surface turbulent fluxes\n")
f.write("# 6. Vertically-integrated variables\n")
f.write("# 7. Near-surface variables\n")
f.write("# 8. Physical tendencies\n")
f.write("# 9. Physics fluxes\n")
f.write("# 10. Forcing tendencies\n")
f.write("# 11. Water and energy budget\n")
f.write("# 12. Gaseous variables\n")
f.write("# 13. Convective variables\n")
f.write("# 14. Wake variables\n")
f.write("# 15. Turbulence variables\n")
f.write("# 16. COSP variables\n")
f.write("# 16.1 COSP input variables\n")
f.write("# 17. Miscellanenous\n")
f.write("\n")

f.write('variables = {\n')

f.write("# 1. Dynamical and thermodynamical variables\n")
tmp = "pf, ph, zf, zh, rho, temp, qv, hur, ql, qi, qr, qsn, tke, theta, thv, thl, thlv, qsat, u, v, wa, wap, qlc, qic, qrc, qsnc, qlshc, qishc, qrshc, qsnshc"
write_vargroup(f, tmp)

f.write("# 2. Cloud variables\n")
tmp = "rneb, cls, cc, cltc, cltl, cltm, clth"
write_vargroup(f, tmp)

f.write("# 3. Precipitation variables\n")
tmp = "precls, snowls, precc, snowc, rain, prls, prc, ppr, pprls, pprc"
write_vargroup(f, tmp)

f.write("# 4. Radiation variables\n")
tmp = "rsdt, rsdtcs, rldt, rldtcs, rsut, rsutcs, rlut, rlutcs, rst, rstcs, rlt, rltcs, rsds, rsdscs, rsus, rsuscs, rlds, rldscs, rlus, rluscs, rss, rsscs, rls, rlscs, SWd, SWu, SWdcs, SWucs, SWnet, LWd, LWu, LWdcs, LWucs, LWnet, mu1, mueff, I0, alb_ss"
write_vargroup(f, tmp)

f.write("# 5. Surface turbulent fluxes\n")
tmp = "shf, lhf, lhfn,      evap, evapi, evapn,     tauu, tauv, ustar, Cd, Ch, Ce, Cdn, Chn, Cen, Ugr, z0, z0h, zref, tsurf, qsurf"
write_vargroup(f, tmp)

f.write("# 6. Vertically-integrated variables\n")
tmp = "prw, lwp, iwp, cwp"
write_vargroup(f, tmp)

f.write("# 7. Near-surface variables\n")
tmp = "t2m, huss, hurs, uas, vas, pblh, ts"
write_vargroup(f, tmp)

f.write("# 8. Physical tendencies\n")
tmp = "Q1, Q2, QRad,      tnthl, tnqt,      tntrsw, tntrlw, tntrswcs, tntrlwcs, tntpbl, tntlscp, tntc, tntshc, tntd,      tnthrsw, tnthrlw, tnthrswcs, tnthrlwcs, tnthpbl, tnthlscp, tnthc, tnthshc, tnthd,      tnqvpbl, tnqvlscp, tnqvc, tnqvshc, tnqvd,      tnupbl, tnuc, tnushc, tnud, tnvpbl, tnvc, tnvshc, tnvd,      tnthlrsw, tnthlrlw, tnthlrswcs, tnthlrlwcs, tnthlpbl, tnthllscp, tnthlc, tnthlshc, tnthld,      tnqtpbl, tnqtlscp, tnqtc, tnqtshc, tnqtd,      tnql, tnqi, tnqr, tnqsn,      tnqlc, tnqic, tnqrc, tnqsnc,      tnqlshc, tnqishc, tnqrshc, tnqsnc,      tntcas, tntcs, tntfplcl, tntfplcn, tntfccql, tntfecl, tntfccqn, tntfecn, tntfhimcc,      tntfplsl, tntfplsn, tntfcsql, tntfesl, tntfcsqn, tntfesn,      tendu, tendv, tendh, tendq"
write_vargroup(f, tmp)

f.write("# 9. Physics fluxes\n")
tmp = "wpqp_pbl, wpthp_pbl, wpqp_conv, wpthp_conv, wpqtp_pbl, wpthlp_pbl, wpqtp_conv, wpthlp_conv,      wpup_pbl, wpvp_pbl, wpup_conv, wpvp_conv"
write_vargroup(f, tmp)

f.write("# 10. Forcing tendencies\n")
tmp = "tntadv, tntnudg, tnqvadv, tnqvnudg, tnunudg, tnvnudg, tnugeo, tnvgeo"
write_vargroup(f, tmp)

f.write("# 11. Water and energy budget\n")
tmp = "qflux, qfluxPr, qfluxEv, dwater, efluxTOA, efluxSfc, denergy,      RMSE9, RMSE0, RMSE1, RDMSE, msefluxSfc,      iQadv, iQnud, iTadv, iTnud, iUadv, iUnud, iVadv, iVnud, iEadv, iEnud,      efluxSfcRad, efluxSfcTurb, efluxSfcConv, efluxSfcAdj, efluxSfcPrSen, efluxSfcPrLat,      iMSEadv, iMSEnud, iQw, iEw"
write_vargroup(f, tmp)

f.write("# 12. Gaseous variables\n")
tmp = "rCO2, rCH4, rN2O, rNO2, rCFC11, rCFC12, rCFC22, rCCL4, rO3"
write_vargroup(f, tmp)

f.write("# 13. Convective variables\n")
tmp = "alpha, Tu, Thu, qvu, qcu, alpha_up, w_up, omega_up, alpha_dn, w_dn, omega_dn, cape, T_up, qv_up, omega_ref,      w_up_bud, dw_buoy, dw_fric, dw_Kd, dw_entr, dw_transp, buoy, Mf, eps_u, eps_u_org, eps_u_tur, entr_u, detr_u, dTv_up, B_up,      aipcmt, knnd, knlab, ZS15, ZS16, ZTAU,ZWMAX,ZZMAX,ZKMAX,ZINTEGMAX,ZMMAX"
write_vargroup(f, tmp)

f.write("# 14. Wake variables\n")
tmp = "delta_t, delta_q, d_delta_t_gw, omgb_dth, omgb, dt_KE, dq_KE, dt_PBL, dq_PBL,      omg_w, dp_delt_omg, spread_w, delta_th, dt_wake, dq_wake, t_undi, q_undi,       d_delta_t, d_delta_q,      hw, sigmaw, wake_pe, wake_fip, wake_gfl, Cstar, wdens,      dt_dn, dt_up, dq_dn, dq_up, Mf_dn, Mf_up, sigd, omgb,      t_undi_pcmt, q_undi_pcmt, t_wake_pcmt, q_wake_pcmt,      qw_undi, qw_wake, Tw_wake, Tw_undi, qsat_undi, qsat_wake,      dt_wake2, dq_wake2,      ZMWAKE, ZCWAKE, ZS12"
write_vargroup(f, tmp)

f.write("# 15. Turbulence variables\n")
tmp = "Q11, igs, igs2, igs2turb, igs2conv, sigs, sigs2, sigs2turb, sigs2conv, mlen,      Q11min, Q11max, acoef, sigc0, sigc1,"
tmp += ",".join(['LMECT','TKEDIF','TKEDISS','TKEPRDY','TKEPRTH','TKETEND','PECTCLS','PKCLS','PKUROV','PUSLE','PPRODTH'])
tmp += ',' +','.join(['ECT0','ECT1','TNECT_DYN','TNECT_BUO','TNECT_DIS','TNECT_DIF','KU'])
write_vargroup(f, tmp)

f.write("# 16. COSP variables\n")
tmp = "cltcalipso, cllcalipso, clmcalipso, clhcalipso, clcalipso,      cllcalipsoice, clmcalipsoice, clhcalipsoice, cltcalipsoice,      cllcalipsoliq, clmcalipsoliq, clhcalipsoliq, cltcalipsoliq,      cllcalipsoun, clmcalipsoun, clhcalipsoun, cltcalipsoun,      clcalipso, lidarBetaMol532,      clcalipsoice, clcalipsoliq, clcalipsoun,       clcalipsotmp, clcalipsotmpice, clcalipsotmpliq, clcalipsotmpun,      parasolRefl,      atb532, cfadLidarsr532,      dbze94, cfadDbze94,      cltlidarradar, clcalipso2,      cltisccp, pctisccp, tauisccp, albisccp, meantbisccp, meantbclrisccp,      boxtauisccp, boxptopisccp,      clisccp,      cltmodis, clwmodis, climodis, clhmodis, clmmodis, cllmodis,      tautmodis, tauwmodis, tauimodis, tautlogmodis, tauwlogmodis, tauilogmodis,      reffclwmodis, reffclimodis,      pctmodis, lwpmodis, iwpmodis,      clmodis,      clMISR"
write_vargroup(f, tmp)

f.write("# 16.1 COSP input variables\n")
tmp = ",".join(['ZLAT_COSP','ZLON_COSP','PLSM_COSP','PMU0_COSP','PEMIS_COSP','PTS_COSP', 'ZAP_COSP','ZAPH_COSP','ZAPHI_COSP','ZAPHIF_COSP',     'ZU_COSP','ZV_COSP',      'ZT_COSP','ZQ_COSP','ZRH_COSP','ZOZN_COSP','ZCLFR_COSP','ZCLFRCC_COSP',      'ZMRLSLIQ_COSP','ZMRLSICE_COSP','ZMRCCLIQ_COSP','ZMRCCICE_COSP',      'ZFLLSRAIN_COSP','ZFLLSSNOW_COSP','ZFLCCRAIN_COSP','ZFLCCSNOW_COSP',      'ZRADLP_COSP','ZRADIP_COSP',      'ZTAUSW_COSP','ZEMILW_COSP'])
write_vargroup(f, tmp)

f.write("# 17. Miscellanenous\n")
tmp = "Cp, Lv"
write_vargroup(f, tmp)


f.write('}')


f.close()
