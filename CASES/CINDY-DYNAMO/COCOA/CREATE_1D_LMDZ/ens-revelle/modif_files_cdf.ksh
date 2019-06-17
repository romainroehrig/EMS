rm forcage_cindy.nc

### Pour le 50km
cp ../rev180varanaecmwfanaradar50kmC1.c1.20111001.000000.cdf forcage_cindy.nc

### Pour le 150km
#cp ../rev180varanaecmwfanaradarC1.c1.20111001.000000.cdf forcage_cindy.nc

ncrename -v LH,flat -v T_skin,ts -v SH,sens -v omega,w -v q,rv -v q_adv_h,hq -v q_adv_v,vq -v T_adv_h,hT -v T_adv_v,vT -v p_srf_aver,psurf -v T,temperature forcage_cindy.nc

cp forcage_cindy.nc in.nc

#ensuite on modifie les valeurs
ncap2 -s 'T_srf += 273.15' in.nc out.nc
rm in.nc
ncap2 -s "omega_srf = omega_srf/3600*100" out.nc in.nc
rm out.nc

ncap2 -s 'ts += 273.15' in.nc out.nc
rm in.nc
ncap2 -s "w = w/3600*100" out.nc in.nc
rm out.nc

ncap2 -s 'q1 = q1/24' in.nc out.nc
rm in.nc
ncap2 -s "q2 = q2/24" out.nc in.nc
rm out.nc

ncap2 -s 'hq = hq/-1000/3600' in.nc out.nc
rm in.nc
ncap2 -s "vq = vq/-1000/3600" out.nc in.nc
rm out.nc

ncap2 -s 'hT = hT/-3600' in.nc out.nc
rm in.nc
ncap2 -s "vT = vT/-3600" out.nc in.nc
rm out.nc

ncap2 -s 'lev = lev*100' in.nc out.nc
rm in.nc

ncap2 -s 'psurf = psurf*100' out.nc in.nc
rm out.nc

#ensuite on cree les variables advections

cdo expr,'advq=hq+vq' in.nc t1.nc
cdo merge in.nc t1.nc toto.nc
rm t1.nc
cdo expr,'advT=hT+vT' in.nc t2.nc
cdo merge toto.nc t2.nc out.nc
rm t2.nc 
rm toto.nc
rm in.nc

#on s occupe maintenant de modifier les attribus

ncatted -O -a units,lev,o,c,"hPa" out.nc
ncatted -O -a units,ts,o,c,"K" out.nc
ncatted -O -a units,q1,o,c,"K/day" out.nc
ncatted -O -a units,q2,o,c,"K/day" out.nc
ncatted -O -a units,hT,o,c,"K/day" out.nc
ncatted -O -a units,vT,o,c,"K/day" out.nc
ncatted -O -a units,vq,o,c,"kg/kg/day" out.nc
ncatted -O -a units,hq,o,c,"kg/kg/day" out.nc
ncatted -O -a units,w,o,c,"Pa/s" out.nc
ncatted -O -a units,omega_srf,o,c,"Pa/s" out.nc
ncatted -O -a units,T_srf,o,c,"K" out.nc
ncatted -O -a units,advT,o,c,"K/day" out.nc
ncatted -O -a units,advq,o,c,"kg/kg/day" out.nc
ncatted -O -a units,psurf,o,c,"hPa" out.nc

#on fixe le temps
cdo  setreftime,2011-10-01,0,1s  -settaxis,2011-10-01,3:00:00,1day  -setcalendar,360_day out.nc in.nc

#on prend que 40 niveau
cdo sellevel,4000,6500,9000,11500,14000,16500,19000,21500,24000,26500,29000,31500,34000,36500,39000,41500,44000,46500,49000,51500,54000,56500,59000,61500,64000,66500,69000,71500,74000,76500,79000,81500,84000,86500,89000,91500,94000,96500,99000,101500 in.nc toto.nc

#on extrait aussi la temperature et SH et LH
ncks -v sens,flat,ts,psurf,u_srf,v_srf,T_srf,q_srf,omega_srf in.nc titi.nc

#On merge les fichiers ce qui ressemble au fichier final mais sans dimensions lon et lat
cdo merge titi.nc toto.nc final.nc

#on nettoie nos affaires
rm toto.nc titi.nc out.nc in.nc
 


#############################################
#Maintenant on s attaque au fichier initiale 

#on selectionne les pas de temps pour coller au nouveau forcage

#ncks -d time,8,735 cas_ini.nc cas_ini_f.nc
cp cas_ini.nc cas_ini_f.nc

#on envoie tout ca dans python car ancien forcage definition pourrie donc on gere en python

python modif_netcdf.py

#on recupere le fichier final et on l'envoie au bon endroit pour effectuer un test par la suite !
mv cas_ini_f.nc cas.nc
#ncatted -a units,time,o,c,"seconds since 2011-10-01 00:00:00" cas.nc
chmod +x cas.nc
#cp cas.nc /homel/otorres/LMDZ/LMDz_2017_test_CINDY/LMDZ20170316.trunk/1D/CAS/cindynamo/setup/.
cp cas.nc /homel/otorres/LMDz_2018_BULK_JLR/LMDZtrunk/1D/CAS/cindynamo/setup/.
#rm final.nc

