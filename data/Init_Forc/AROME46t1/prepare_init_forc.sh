#!/bin/sh

cat << EOF > tmp.py
import os

g = open('tmp.sh','w')
print >> g, '#!/bin/sh'
print >> g, 'export OMP_NUM_THREADS=1'
print >> g, "export ASCII2FA=" + os.getenv('REP_EMS') + "/UTIL/Tools/ASCII2FA_GMAP/bin/ascii2fa"
g.close()
EOF


python tmp.py
chmod u+x tmp.sh
. ./tmp.sh

rm -f tmp.py tmp.sh

cat << EOF > config.py
import os

case = '$1'
subcase = '$2'

#zorog = 0.

vert_grid = '$3'

dt = $4

lforc = True
lnam1D = True

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

python prepare_init_forc.py

cp nam1D_$3 nam1D

$ASCII2FA > ascii2fa_$3.log 2>&1

rm -f nam1D

mv 1D.file initfile_$3
