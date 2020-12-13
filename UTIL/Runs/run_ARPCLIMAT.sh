#!/bin/sh

#------------------------------------------------------------
# 			INTIALISATION
#------------------------------------------------------------
set -x

export OMP_NUM_THREADS=1

export DR_HOOK_IGNORE_SIGNALS=-1
export DR_HOOK=0

. ./param

EXP=ARPE
ADVEC=sli

#       *************************************
#       * Directories Initialisation        *
#       *************************************

DIR=`pwd`

OUTPUTDIR=$DIR/Output/LFA/
OUTPUTDIR0=$DIR/Output/

if [ ! -d $OUTPUTDIR ] ; then
  mkdir -p $OUTPUTDIR
fi

TMPDIR=$HOME/tmp/EXEMUSC

if [ ! -d $TMPDIR ] ; then
  mkdir -p $TMPDIR
else
  find $TMPDIR/ -name '*' -exec rm -rf {} \; || :
fi

cd $TMPDIR

ladate=`date`
set +x
echo '------------------------------------------------------------'
echo 'run execute le ' $ladate
echo 'cycle =' $cycle 'config = ' $CONFIG
echo 'Namelist Arpege =' $NAMARP
echo 'Namelist Surfex =' $NAMSFX
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
echo ' Get the namelist ARPEGE'
echo ''
set -x

ln -s $DIR/$NAMARP fort.4
cat < fort.4

set +x
echo ''
echo ' Get the namelist SURFEX'
echo ''
set -x

ln -s $DIR/$NAMSFX EXSEG1.nam
cat < EXSEG1.nam


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
ln -s $FORCING_FILES files

ln -s  $PREP TEST.lfi
ln -s  $PGD PGD.lfi


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
chmod 755 MASTER

set +x
echo ''
echo ' ALADIN job running '
echo ''
set -x

ulimit -s unlimited

date
./MASTER -c001 -vmeteo -maladin -e${EXP} -t$TSTEP -f$NSTOP -a$ADVEC  >lola 2>&1
date
ls -l

set +x
echo ''
echo ' Listing for the not parallelised part: file lola'
echo ''
set -x

cat lola

if [ -a NODE.001_01 ]
then
  for file in NODE*
  do
    set +x
    echo ''
    echo ' Listing for the parallelised part: file' $file
    echo ''
    set -x
    cat $file
  done
fi

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
#find ./ -name 'out*.txt' -exec mv {} $OUTPUTDIR \;
find ./ -name 'NODE*' -exec mv {} $OUTPUTDIR \;
find ./ -name 'lola' -exec mv {} $OUTPUTDIR \;

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
#rm -rf $TMPDIR/*
find $TMPDIR/ -name '*' -exec rm -rf {} \;
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
  ./convertLFA2nc.py -format $lfaformat

fi

date

cd $DIR
