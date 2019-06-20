#!/bin/sh

source activate myuvcdat
alias cdat='python'

cat << EOF > tmp.py
import os

g = open('tmp.sh','w')
print >> g, '#!/bin/sh'
print >> g, 'export OMP_NUM_THREADS=1'
print >> g, "export ASCII2FA=" + os.getenv('REP_EMS') + "/UTIL/Tools/ASCII2FA/bin/ascii2fa"
g.close()
EOF


cdat tmp.py
chmod u+x tmp.sh
. ./tmp.sh

rm -f tmp.py tmp.sh

cat << EOF > config.py
import os

#zorog = 0.

nlev = $1

dt = $2

lforc = True
lnam1D = True
if not(os.getenv('LDEPHY') is None):
  lDEPHY = True
else:
  lDEPHY = False

variablesAux = {}
variablesAux['SURFAEROS.SEA']=6.2E-3
variablesAux['SURFAEROS.LAND']=2.2E-2
variablesAux['SURFAEROS.SOOT']=1.53E-3
variablesAux['SURFAEROS.DESERT']=2.6E-2
variablesAux['SURFA.OF.OZONE']=7.1E-2
variablesAux['SURFB.OF.OZONE']=3166.
variablesAux['SURFC.OF.OZONE']=3.
variablesAux['SURFTEMPERATURE']=292.
variablesAux['SURFRESERV.NEIGE']=0.     
variablesAux['SURFRESERV.EAU']=10.      
variablesAux['SURFZ0.FOIS.G']=0.1
variablesAux['SURFALBEDO']=0.07      
variablesAux['SURFEMISSIVITE']=0.98          
variablesAux['SURFET.GEOPOTENT']=0.0
variablesAux['SURFVAR.GEOP.ANI']=0.0
variablesAux['SURFVAR.GEOP.DIR']=0.0
variablesAux['SURFIND.TERREMER']=0.0
variablesAux['PROFTEMPERATURE']=292.
variablesAux['PROFRESERV.EAU']=8000.0   
variablesAux['PROFRESERV.GLACE']=0.0
variablesAux['SURFRESERV.INTER']=0.0
variablesAux['SURFRESERV.GLACE']=0.0 
variablesAux['SURFIND.VEG.DOMI']=1.0  
variablesAux['SURFRESI.STO.MIN']=50.
variablesAux['SURFPROP.ARGILE']=3.944         
variablesAux['SURFPROP.SABLE']=85.09        
variablesAux['SURFEPAIS.SOL']=8.0
variablesAux['SURFIND.FOLIAIRE']=1.0
variablesAux['SURFRES.EVAPOTRA']=1.
variablesAux['SURFGZ0.THERM']=0.1
variablesAux['SURFPROP.VEGETAT']=0.
variablesAux['SURFALBEDO NEIGE']=0.65
variablesAux['SURFDENSIT.NEIGE']=0.30
variablesAux['SURFALBEDO.SOLNU']=0.75
variablesAux['SURFALBEDO.VEG']=0.1
variablesAux['SURFALBEDO.COMPL']=0.1
variablesAux['SURFZ0REL.FOIS.G']=0.1

EOF

rm -rf files_L${1}_${2}s
mkdir files_L$1_${2}s

cdat prepare_nam1D.py

cp nam1D_L$1 nam1D

$ASCII2FA > ascii2fa_$1.log 2>&1

rm -f nam1D

mv 1D.file initfile_L$1


cdat prepare_forcing.py

