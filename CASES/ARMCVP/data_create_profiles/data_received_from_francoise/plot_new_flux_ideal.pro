;  purpose: plot time series fields of the new surface heat fluxes

set_plot,'ps' & device,filename="new_flux_ideal.ps",/landscape,/color,/helvetica,/bold
!p.thick=1.
!p.charsize=1
!p.charthick=1
!p.gridstyle=1
!x.style=1
!y.style=1
!p.font=0
tek_color
a=findgen(16)*(!pi*2/16.) & usersym,cos(A),sin(a),/fill ; disk symbol

nt=97
leg_nf=""     ; number of fields
leg_nt=""     ; number of time index
leg_cd=""  & ca=fltarr(nt) ; calendar day
leg_yy=""  & yy=fltarr(nt) ; year
leg_mo=""  & mo=fltarr(nt) ; month
leg_dd=""  & dd=fltarr(nt) ; day of month
leg_hh=""  & hh=fltarr(nt) ; hour of day
leg_mi=""  & mi=fltarr(nt) ; minute
leg_lh=""  & lh=fltarr(nt) ; latent   heat flux
leg_sh=""  & sh=fltarr(nt) ; sensible heat flux

;------------- read data file ------------------

OPENR, unit, 'new_flux_ideal.dat', /Get_Lun
READF,unit,leg_nf & READF,unit, nf,Format='(6x,i2)'
READF,unit,leg_nt & READF,unit,rnt,Format='(5x,f7.3)'       & nt=fix(rnt)
READF,unit,leg_cd & READF,unit,ca,Format='(5(1x,e14.7))'
READF,unit,leg_yy & READF,unit,yy,Format='(5(1x,e14.7))'
READF,unit,leg_mo & READF,unit,mo,Format='(5(1x,e14.7))'
READF,unit,leg_dd & READF,unit,dd,Format='(5(1x,e14.7))'
READF,unit,leg_hh & READF,unit,hh,Format='(5(1x,e14.7))'
READF,unit,leg_mi & READF,unit,mi,Format='(5(1x,e14.7))'
READF,unit,leg_lh & READF,unit,lh,Format='(5(1x,e14.7))'
READF,unit,leg_sh & READF,unit,sh,Format='(5(1x,e14.7))'
FREE_LUN, unit
dt_tim=var_to_dt(yy,mo,dd,hh,mi)

;-------------- plot fluxes ---------------

!p.multi=[0,1,2]
n1=0 & n2=nt-1
 plot,dt_tim(n1:n2),lh(n1:n2),psym=-8,yrange=[-50,600],ticklen=0.5,symsize=.1,/Box,ytitle="(W/m2)"
oplot,dt_tim(n1:n2),lh(n1:n2)          ,psym=-4,color= 4,symsize=.8
oplot,dt_tim(n1:n2),sh(n1:n2)          ,psym=-5,color= 2,symsize=.8
oplot,dt_tim(n1:n2),sh(n1:n2)+lh(n1:n2),psym=-8,color=15,symsize=.8

xyouts,2.1,550.,"idealized values of:"           ,charsize=1.,color=0
xyouts,2.1,500.,"   sensible+latent heat flux"   ,charsize=1.,color=15
xyouts,2.1,460.,"   latent   heat flux"          ,charsize=1.,color=4
xyouts,2.1,420.,"   sensible heat flux"          ,charsize=1.,color=2

;-------------- plot bowen ratio ----------

bo=fltarr(nt)  ; bowen ratio
for n=0,nt-1 do begin
if lh(n) ne 0. then bo(n)=sh(n)/lh(n)
if lh(n) eq 0. then bo(n)=-1.
endfor

 plot,dt_tim(n1:n2),bo(n1:n2),psym=-8,yrange=[-.1,.5],ticklen=0.5,symsize=.5,/Box,ytitle="(unitless)"
oplot,dt_tim(n1:n2),bo(n1:n2),psym=-8,color=2,symsize=.5
xyouts,2.0,.42,"Bowen ratio (Bo=sensible/lat)",charsize=1.2,color=2

END
