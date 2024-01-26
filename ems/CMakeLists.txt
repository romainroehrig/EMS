function (install_ems)
  foreach (file ${ARGV})
    install (FILES ${file} DESTINATION ems)
  endforeach ()
endfunction()

function (install_dephycf)
  foreach (file ${ARGV})
    install (FILES ${file} DESTINATION ems/dephycf)
  endforeach ()
endfunction()

function (install_scripts)
  foreach (file ${ARGV})
    install (FILES ${file} DESTINATION ems/scripts)
  endforeach ()
endfunction()

install_ems (
__init__.py
install_MUSC.py
lfa2nc_dephycf.py
lfa2nc.py
cases.py
lfa.py
namelist.py
prep_init_forc_atm_ARPCLIMAT.py
prep_init_forc_atm_GMAP.py
prep_init_forc_atm.py
prep_nam_atm_AROME.py
prep_nam_atm_ARPCLIMAT.py
prep_nam_atm_ARPPNT.py
prep_nam_atm.py
prep_nam_sfx.py)

install_dephycf (
dephycf/attributes.py
dephycf/Axis.py
dephycf/Case.py
dephycf/constants.py
dephycf/__init__.py
dephycf/plotbasics.py
dephycf/thermo.py
dephycf/Variable.py
dephycf/variables_attributes.py)

install_scripts ( scripts/run.sh )
