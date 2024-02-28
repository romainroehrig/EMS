files='
MUSC.py                   ems_convertLFA2nc.py      ems_prep_init_forc_atm.py
ems_convert2p.py          ems_lfa2nc.py             ems_prep_nam_atm.py
ems_convert2z.py          ems_list_cases.py         ems_prep_nam_sfx.py
'

for f in $files
do
    gvim -d $f /Users/romain/Tools/EMS/V2.4.2.mac/apptools/$f
done
