#!/bin/sh

#------------------------------------------------------------
# 			INTIALISATION
#------------------------------------------------------------
set -ex

export OMP_NUM_THREADS=1

export DR_HOOK_IGNORE_SIGNALS=-1
export DR_HOOK=0

if [ $model = "AROME" ] || [ $model = "ARPPNT" ] ; then
  export LIBRARY_PATH=$LIBRARY_PATH:/usr/lib/x86_64-linux-gnu
  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/common/sync/gcc/mpfr-3.1.3/lib:/home/common/sync/gcc/jasper-1.900.1/lib:/home/common/sync/gcc/torque:/opt/google/earth/pro
  export C_INCLUDE_PATH=$C_INCLUDE_PATH:/usr/include/x86_64-linux-gnu
  export CPLUS_INCLUDE_PATH=$CPLUS_INCLUDE_PATH:/usr/include/x86_64-linux-gnu
fi


. ./param

os=`uname`

EXP=ARPE

#       *************************************
#       * Directories Initialisation        *
#       *************************************

DIR=`pwd`
OUTPUTDIR=$DIR/Output/LFA/
OUTPUTDIR0=$DIR/Output/
LISTINGDIR=$DIR/listings

if [ -z "$TMPDIR" ] ; then
  TMPDIR=$HOME/tmp/EXEMUSC.$$
else
  TMPDIR=$TMPDIR/EXEMUSC.$$
fi

[ ! -d $LISTINGDIR ] && mkdir -p $LISTINGDIR
[ ! -d $OUTPUTDIR ] && mkdir -p $OUTPUTDIR
[ -d $TMPDIR ] && rm -rf $TMPDIR; mkdir -p $TMPDIR

cd $TMPDIR

ladate=`date`
set +x
echo '------------------------------------------------------------'
echo 'run execute le ' $ladate
echo 'cycle =' $cycle 'config = ' $CONFIG
echo 'Atmospheric namelist =' $NAMARP
echo 'Surfex namelist =' $NAMSFX
echo 'Executable =' $MASTER
echo 'Time step =' $TSTEP 'seconds; Run stops at ' $NSTOP
echo 'Number of levels =' $levels
echo 'Conditions initiales et forcages =' $INITFILE
echo 'Conditions pour la surface =' $PREP
echo 'Dossier des sorties =' $OUTPUTDIR
echo '------------------------------------------------------------'
set -x
#       **********************************
#       *        Get namelists           *
#       **********************************

set +x
echo ''
echo ' Get the atmospheric namelist'
echo ''
set -x

ln -s $DIR/$NAMARP fort.4
cat < fort.4

if [ -n "$NAMSFX" ]; then
  set +x
  echo ''
  echo ' Get the namelist SURFEX'
  echo ''
  set -x

  ln -s $DIR/$NAMSFX EXSEG1.nam
  cat < EXSEG1.nam
fi

#       **********************************
#       * Get initial and forcing files  *
#       **********************************

set +x
echo ''
echo ' Get Initial conditions for atmosphere and surface '
echo ' Get Forcing files '
echo ''
set -x


ln -s $INITFILE ICMSH${EXP}INIT
[ -n "$FORCING_FILES" ] && ln -s $FORCING_FILES files

if [ $model = "ARPCLIMAT" ]; then
    [ -n "$PREP" ] && ln -s  $PREP TEST.lfi
    [ -n "$PGD" ] && ln -s  $PGD PGD.lfi
else
    [ -n "$PREP" ] && ln -s  $PREP ICMSH${EXP}INIT.sfx
    [ -n "$PGD" ] && ln -s  $PGD Const.Clim.sfx	
fi


#       **********************************
#       *            For RRTM            *
#       **********************************

if [ -n "$RRTM" ]; then
    ln -s $RRTM rrtm.tgz
    tar zxf rrtm.tgz
fi

#       **********************************
#       *            For SURFEX          *
#       **********************************


ln -s $ecoclimap/ecoclimapII_eu_covers_param.bin ecoclimapII_eu_covers_param.bin
ln -s $ecoclimap/ecoclimapI_covers_param.bin ecoclimapI_covers_param.bin

#       **********************************
#       *            Execution           *
#       **********************************

set +x
echo ''
echo ' Get the executable file '
echo ''
set -x

ln -s $MASTER MASTER
[ ! -x MASTER ] && chmod 755 MASTER

set +x
echo ''
echo ' ALADIN job running '
echo ''
set -x

[ ! $os = "Darwin" ] && ulimit -s unlimited

unset LD_LIBRARY_PATH

date
if [ $model = "ARPCLIMAT" ]; then
    ./MASTER -c001 -vmeteo -maladin -e${EXP} -t$TSTEP -f$NSTOP -asli  >lola 2>&1
else
    ./MASTER >lola 2>&1
fi
date
set +e
ls -l
set -e

#       **********************************
#       *     Save model results         *
#       **********************************

set +x
echo ''
echo ' Historic files saving '
echo ''
set -x

find $OUTPUTDIR/ -name '*' -exec rm -f {} \;
find ./ -name 'Out*' -exec mv {} $OUTPUTDIR \;
find ./ -name 'NODE*' -exec mv {} $LISTINGDIR \;
find ./ -name 'lola' -exec mv {} $LISTINGDIR \;

set +x
echo ''
echo ' Present files on the workdir $TMPDIR '
echo ''
set -x

ls -l

#       **********************************
#       *        Nettoyage final         *
#       *        Final Cleaning          *
#       **********************************

set +x
echo ''
echo ' Final cleaning of the workdir $TMPDIR '
echo ''
set -x

set +x
rm -rf $TMPDIR
set -x
#       ********************************************
#       * Copie eventuelle des routines convert2nc *
#       * Possible copy of convert2nc routines     *
#       ********************************************

if [ $installpost = True ]
then
  cd $OUTPUTDIR0
  rm -f *.py *.pyc 
  set +x
  echo ''
  echo ' Install post-processing '
  echo ''
  set -x

  files2install='convertLFA2nc.py lfa2nc.py convert2p.py convert2z.py'

  for file in $files2install
  do
    ln -s $dirpost/$file
  done
  ln -s $dirpost/$configpost config.py
  ln -s $dirpost/$variablesDict variables.py
fi

#       ********************************************
#       * Conversion eventuelle en netcdf          *
#       * Possible conversion in netcdf            *
#       ********************************************

if [ $runpost = True ]
then
  cd $OUTPUTDIR0
  set +x
  echo ''
  echo ' Postprocessing '
  echo ''
  set -x

  # seems necessary in some circumstances (deep shells?)
  unset PYTHONHOME
  ./convertLFA2nc.py

fi

date

cd $DIR
