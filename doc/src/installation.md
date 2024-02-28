# EMS Installation

A copy of the contents of the README

## Prerequisites

* Python 3.\*
* Python packages numpy, scipy, netCDF4 and matplotlib
* GMAP tools in `/home/common/sync`

## Quick installation
To install EMS on a CNRM computer, with access to the CNRM Lustre system:
1. Get the installation script: 

   `wget https://raw.githubusercontent.com/romainroehrig/EMS/master/install.sh`

2. Modify `install.sh` (or execute `install.sh -h` to get its usage):

   * Set `EMS_VERSION`, e.g., `EMS_VERSION=2.4.4`
   * Set where you want to install EMS: default is `REP_EMS=$HOME/Tools/EMS/V${EMS_VERSION}`
   * Set where you want to run MUSC: default is `REP_MUSC=$HOME/MUSC/V${EMS_VERSION}`

3. Execute `install.sh`. A test is done at the end with ARPEGE-Climat 6.3.2 for the ARMCU/REF case.

## Using EMS
1. Go in the `REP_MUSC` directory
2. Source setenv to have the right PATH and PYTHONPATH environment variables

   `source setenv`

3. You can manage your own namelists (`namelist` directory), vertical grid (`grid` directory) and MUSC configuration files (`config` directory), and postprocessing (`post` directory).

4. Run MUSC:

   `MUSC.py -config config/YOUR_CONFIG_FILE -case CASE -subcase SUBCASE`

## Other available tools

* `ems_list_cases.py`: Provide information about cases/subcases available in EMS
* `ems_prep_init_forc_atm.py`: prepare the nam1D namelist for ARPIFS restart
* `ems_prep_nam_atm.py`: Modify an ARPIFS namelist to simulate a case described by a netcdf file using the DEPHY common format
* `ems_prep_nam_sfx.py`: Modify a SURFEX namelist to simulate a case described by a netcdf file using the DEPHY common format
* `ems_convert2p.py`
* `ems_convert2z.py`
* `ems_convertLFA2nc.py`
* `ems_lfa2nc.py`
