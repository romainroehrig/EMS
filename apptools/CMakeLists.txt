function (install_scripts)
  foreach (file ${ARGV})
    get_filename_component (name_without_extension ${file} NAME_WE)
    install (FILES ${file}
             PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ
             DESTINATION bin)
#             RENAME ${name_without_extension})
  endforeach ()
endfunction()

install_scripts (ems_convert2p.py ems_convert2z.py ems_convertLFA2nc.py ems_lfa2nc.py ems_list_cases.py ems_prep_init_forc_atm.py ems_prep_nam_atm.py ems_prep_nam_sfx.py MUSC.py)
