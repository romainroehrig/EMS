# ems\_list\_cases.py

## DESCRIPTION
Provide information about cases/subcases available in EMS


## USAGE
```shell
usage: ems_list_cases.py [-h] [-c CASE]
```

## OPTIONS

| Option               | Description                    |
|----------------------|--------------------------------|
| -h, --help           |Show this help message and exit |
| -c CASE, --case CASE | Case name for which you want to list available subcases

## Examples

```shell
$ ems_list_cases.py
2024-02-29 09:09:11,628 -                      ems.cases - INFO - ------------------------------ Available cases
2024-02-29 09:09:11,628 -                      ems.cases - INFO - GABLS1 REF /home/ewhelan/metapp/ems/ems/../share/CASES/GABLS1_REF_SCM_driver.nc
2024-02-29 09:09:11,628 -                      ems.cases - INFO - GABLS1 MESONH /home/ewhelan/metapp/ems/ems/../share/CASES/GABLS1_MESONH_SCM_driver.nc
2024-02-29 09:09:11,628 -                      ems.cases - INFO - GABLS4 STAGE3 /home/ewhelan/metapp/ems/ems/../share/CASES/GABLS4_STAGE3_SCM_driver.nc
2024-02-29 09:09:11,628 -                      ems.cases - INFO - GABLS4 STAGE3-SHORT /home/ewhelan/metapp/ems/ems/../share/CASES/GABLS4_STAGE3-SHORT_SCM_driver.nc
2024-02-29 09:09:11,628 -                      ems.cases - INFO - AYOTTE 00SC /home/ewhelan/metapp/ems/ems/../share/CASES/AYOTTE_00SC_SCM_driver.nc
2024-02-29 09:09:11,628 -                      ems.cases - INFO - AYOTTE 00WC /home/ewhelan/metapp/ems/ems/../share/CASES/AYOTTE_00WC_SCM_driver.nc
2024-02-29 09:09:11,628 -                      ems.cases - INFO - AYOTTE 03SC /home/ewhelan/metapp/ems/ems/../share/CASES/AYOTTE_03SC_SCM_driver.nc
2024-02-29 09:09:11,628 -                      ems.cases - INFO - AYOTTE 05SC /home/ewhelan/metapp/ems/ems/../share/CASES/AYOTTE_05SC_SCM_driver.nc
2024-02-29 09:09:11,628 -                      ems.cases - INFO - AYOTTE 05WC /home/ewhelan/metapp/ems/ems/../share/CASES/AYOTTE_05WC_SCM_driver.nc
2024-02-29 09:09:11,628 -                      ems.cases - INFO - AYOTTE 24SC /home/ewhelan/metapp/ems/ems/../share/CASES/AYOTTE_24SC_SCM_driver.nc
2024-02-29 09:09:11,628 -                      ems.cases - INFO - IHOP REF /home/ewhelan/metapp/ems/ems/../share/CASES/IHOP_REF_SCM_driver.nc
2024-02-29 09:09:11,628 -                      ems.cases - INFO - BLLAST REF /home/ewhelan/metapp/ems/ems/../share/CASES/BLLAST_REF_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - BLLAST NOADV /home/ewhelan/metapp/ems/ems/../share/CASES/BLLAST_NOADV_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - BLLAST MESONH /home/ewhelan/metapp/ems/ems/../share/CASES/BLLAST_MESONH_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - RICO SHORT /home/ewhelan/metapp/ems/ems/../share/CASES/RICO_SHORT_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - RICO MESONH /home/ewhelan/metapp/ems/ems/../share/CASES/RICO_MESONH_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - ARMCU REF /home/ewhelan/metapp/ems/ems/../share/CASES/ARMCU_REF_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - ARMCU MESONH /home/ewhelan/metapp/ems/ems/../share/CASES/ARMCU_MESONH_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - ARMCU E3SM /home/ewhelan/metapp/ems/ems/../share/CASES/ARMCU_E3SM_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - BOMEX REF /home/ewhelan/metapp/ems/ems/../share/CASES/BOMEX_REF_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - SCMS REF /home/ewhelan/metapp/ems/ems/../share/CASES/SCMS_REF_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - MPACE REF /home/ewhelan/metapp/ems/ems/../share/CASES/MPACE_REF_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - MPACE E3SM /home/ewhelan/metapp/ems/ems/../share/CASES/MPACE_E3SM_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - ISDAC REF /home/ewhelan/metapp/ems/ems/../share/CASES/ISDAC_REF_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - FIRE REF /home/ewhelan/metapp/ems/ems/../share/CASES/FIRE_REF_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - SANDU REF /home/ewhelan/metapp/ems/ems/../share/CASES/SANDU_REF_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - SANDU FAST /home/ewhelan/metapp/ems/ems/../share/CASES/SANDU_FAST_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - SANDU SLOW /home/ewhelan/metapp/ems/ems/../share/CASES/SANDU_SLOW_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - KB2006 REF /home/ewhelan/metapp/ems/ems/../share/CASES/KB2006_REF_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - KB2006 MESONH /home/ewhelan/metapp/ems/ems/../share/CASES/KB2006_MESONH_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - EUROCS REF /home/ewhelan/metapp/ems/ems/../share/CASES/EUROCS_REF_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - LBA REF /home/ewhelan/metapp/ems/ems/../share/CASES/LBA_REF_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - AMMA REF /home/ewhelan/metapp/ems/ems/../share/CASES/AMMA_REF_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - DYNAMO NSA3A /home/ewhelan/metapp/ems/ems/../share/CASES/DYNAMO_NSA3A_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - DYNAMO NSA3A_D1 /home/ewhelan/metapp/ems/ems/../share/CASES/DYNAMO_NSA3A_D1_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - DYNAMO NSA3A_MJO1 /home/ewhelan/metapp/ems/ems/../share/CASES/DYNAMO_NSA3A_MJO1_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - MAGIC LEG04A /home/ewhelan/metapp/ems/ems/../share/CASES/MAGIC_LEG04A_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - MAGIC LEG05A /home/ewhelan/metapp/ems/ems/../share/CASES/MAGIC_LEG05A_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - MAGIC LEG06A /home/ewhelan/metapp/ems/ems/../share/CASES/MAGIC_LEG06A_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - MAGIC LEG07A /home/ewhelan/metapp/ems/ems/../share/CASES/MAGIC_LEG07A_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - MAGIC LEG11A /home/ewhelan/metapp/ems/ems/../share/CASES/MAGIC_LEG11A_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - MAGIC LEG12A /home/ewhelan/metapp/ems/ems/../share/CASES/MAGIC_LEG12A_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - MAGIC LEG13A /home/ewhelan/metapp/ems/ems/../share/CASES/MAGIC_LEG13A_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - MAGIC LEG14A /home/ewhelan/metapp/ems/ems/../share/CASES/MAGIC_LEG14A_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - MAGIC LEG15A /home/ewhelan/metapp/ems/ems/../share/CASES/MAGIC_LEG15A_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - MAGIC LEG16A /home/ewhelan/metapp/ems/ems/../share/CASES/MAGIC_LEG16A_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - MAGIC LEG17A /home/ewhelan/metapp/ems/ems/../share/CASES/MAGIC_LEG17A_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - MAGIC LEG18A /home/ewhelan/metapp/ems/ems/../share/CASES/MAGIC_LEG18A_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - ARPEGE SODANKYLA_2018031512 /home/ewhelan/metapp/ems/ems/../share/CASES/ARPEGE_SODANKYLA_2018031512_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - ARPEGE SODANKYLA_2018031512_NOFORC_NORAD /home/ewhelan/metapp/ems/ems/../share/CASES/ARPEGE_SODANKYLA_2018031512_NOFORC_NORAD_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - ARPEGE SODANKYLA_2018031512_NOFORC /home/ewhelan/metapp/ems/ems/../share/CASES/ARPEGE_SODANKYLA_2018031512_NOFORC_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - ARPEGE SODANKYLA_2018031512_NONUD /home/ewhelan/metapp/ems/ems/../share/CASES/ARPEGE_SODANKYLA_2018031512_NONUD_SCM_driver.nc
2024-02-29 09:09:11,629 -                      ems.cases - INFO - ------------------------------------------------------------
```
