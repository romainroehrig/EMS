
-----------

SYNOPSIS:


The  ascii2scm  script converts data received from Françoise Guichard to SCM LFA
files.

The  directory data_received_from_francoise is used in input only. The directory
Profiles  is  used in output only. The tmp directory is used for temporary files
only. It can be removed after the execution of the ascii2scm script. The sources
to convert data from input to output directories are in the src directory.

The final LFA files will contain information about

1.  The  profiles  of  T,  qv, and the forcings. This information is read in the
input directory data_received_from_francoise/KIT_IDEAL.

2.  Cloudiness,  Q1,  Q2,  precipitation  fluxes,  predicted  by  the  CRM. This
information is read from ../data_crm_predictions/b2.*.MESONH ASCII files.

3.   qv   budget   predicted   by   the  CRM.  This  information  is  read  from
../data_crm_predictions/qv_budget/br.*.bilrv_mesonh


-----------

MORE DETAILS:

The ascii2scm script performs the following actions:

- convert ASCII files into LFA files (the only change here is a file format one,
no change in the data themselves).

- interpolates variables in the vertical to the current SCM vertical grid. The A
and B vertical coordinates are read in the A_and_B directory.

-  interpolates  in  time  variables from a 3h to 30mn time sampling (to get the
same as fluxes time sampling).

- combines variables and fluxes into a single file.

-  converts  to  SI  units,  computes  variables required by the SCM, writes out
articles  whose names are those expected by the SCM. A file is produced for each
30mn.

