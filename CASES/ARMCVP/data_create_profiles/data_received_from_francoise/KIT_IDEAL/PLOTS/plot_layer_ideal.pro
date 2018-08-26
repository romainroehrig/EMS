;======================
;   CHOOSE CONFIG
;======================

; config='idea_2days_advtot_with_prescribed_rad'
  config='idea_2days_advtot'
; config='idea_2days_advver_with_prescribed_rad'
; config='idea_2days_advver'

;======================

set_plot,'ps'
device,filename="layer_"+config+".ps",/landscape,/color,/helvetica,/Bold

;  ===================
;    READ INPUT DATA
;  ===================

openr,1,'../DATA/layer_'+config+'.dat'

nz=0 & nt=0 & anz='' & ant='' & readf,1,ant,nt,anz,nz,format="(A70,/,F13.3,/,A70,/,F13.3)"
atabz='' & tabz=fltarr(nz)    & readf,1,atabz,tabz,format="(A60,/,3(5(E15.7),/),3(E15.7))"

nf=6 & atabw='' & tabw=fltarr(nt) & legtab=strarr(nf) & tabsrf=fltarr(nf,nt)
for ii=0,5 do begin
  readf,1,atabw,tabw,format="(A60,/,3(5(E15.7),/),3(E15.7))"
  legtab(ii)=atabw
  tabsrf(ii,0:nt-1)=tabw(0:nt-1)
endfor

anff='' & nff=0 & readf,1,anff,nff,format="(A60,/,I8)"

atabwf='' & tabwf=fltarr(nt,nz) & legtabff=strarr(nff) & tabff=fltarr(nt,nz,nff)

sf1=",/,3(5(E15.7),/),2(E15.7)"

sf6=sf1+sf1+sf1+sf1+sf1+sf1 & sf18=sf6+sf6+sf6

for ii=0,nff-1 do begin
readf,1,atabwf,tabwf,format="(A50"+sf18+")"
legtabff(ii)=atabwf
tabff(0:nt-1,0:nz-1,ii)=tabwf(0:nt-1,0:nz-1)
endfor

close,1
;------

;    ===============
;         PLOTS
;    ===============

n1=0
n2=nt-1

xx=fltarr(nt) & for i =0,nt-1 do xx(i)=tabsrf(3,i)+tabsrf(4,i)/24.+tabsrf(5,i)/24./60.
yy=fltarr(nz) & for j =0,nz-1 do yy(j)=-tabz(j)

!p.multi=[0,1,2]
!p.thick=1.
!p.charsize=.7
!p.charthick=1
!p.gridstyle=0
!x.style=1
!y.style=1
tek_color
!p.font=0

nl=100 & nl1=nl-1 & levtot=fltarr(nff,nl) & lev=fltarr(nl)

for n=0,nl1 do begin
  levtot( 0,n)=205+float(n)*2.
  levtot( 1,n)=float(n)/2.
  levtot( 2,n)=float(n-25)
  levtot( 3,n)=float(n-25)
  levtot( 4,n)=float(n-20)
  levtot( 5,n)=float(n-25)*2.E-6
  levtot( 6,n)=float(n-25)*0.025
  levtot( 7,n)=float(n-25)*0.05
  levtot( 8,n)=float(n-50)*0.01
  levtot( 9,n)=float(n-25)*0.01
  levtot(10,n)=298.+float(n)
  levtot(11,n)=float(n-50)*0.01
  levtot(12,n)=float(n-50)*0.05
  levtot(13,n)=float(n-50)*0.05
  levtot(14,n)=float(n-50)*0.05
  levtot(15,n)=float(n-50)*0.025
  levtot(16,n)=float(n-25)*0.05
  levtot(17,n)=float(n-25)*0.05
  if (config eq 'idea_2days_advtot_with_prescribed_rad' or        $
      config eq 'idea_2days_advver_with_prescribed_rad'    ) then $
  levtot(18,n)=float(n-50)*.5
endfor

print,'xx=',xx

for n=n1,n2 do $
print,'day=',tabsrf(3,n),' h=',+tabsrf(4,n),'min=',tabsrf(5,n)

for ii=0,nff-1 do begin
 lev(0:nl1)=levtot(ii,0:nl1)
 isolin_eurocs,tabff(n1:n2,0:nz-1,ii), xx(n1:n2),yy(0:nz-1),$
legtabff(ii),'Day','P (mb)',lev,$
yrange=[-1000.,0.]
endfor

;!p.multi=[0,2,2]
;
;ii=nff-1
;for mm=0,3 do begin
;plot, avg(tabff(8*mm:8*mm+7,0:nz-1,ii),0),yy(0:nz-1),xrange=[-5,5]
;for nn=0,7 do oplot,tabff(nn+8*mm,0:nz-1,ii),yy(0:nz-1),color=nn+2
;endfor

device,/close_file
set_plot,'x'

end

;============================================================
; purpose draw isolignes and assume some default if not given
; exemple call:
; ISOLIN,THEER,DD,FONZ,'THEER','DD','ZZ',levs,$
; xrg=[2,9],yrg=[0,165000],labn=[0]
;============================================================

pro isolin_eurocs,tab,x,y,stab,sx,sy,lev,$
   xrange=xrange,yrange=yrange,ttit=ttitp,xtit=xtitp,ytit=ytitp,labn=zero,$
   xgrad=xgradp

nx=n_elements(x)   & nx1=nx-1
ny=n_elements(y)   & ny1=ny-1
labn=intarr(1)
xgrad=intarr(1) & xgradp=intarr(1)

mint=strcompress(min(tab))
maxt=strcompress(max(tab))
minx=strcompress(min(x))
maxx=strcompress(max(x))
miny=strcompress(-min(y))
maxy=strcompress(-max(y))

nl=n_elements(lev)  & nl1=nl-1
snl=strcompress(nl)

isomin=strcompress(min(lev))
isomax=strcompress(max(lev))
isoint=strcompress(lev(1)-lev(0))

lab=intarr(nl)   
icnt=0
for i=0,nl1 do begin
if(icnt eq 1) then begin
lab(i)=1
icnt=0
endif else begin
lab(i)=0
icnt=1
endelse
endfor  
; if keyword_set(labn)  then print,'labs key' & lab(0:nl1)=labn(0)

if n_elements(xrange)    eq 0 then xrange = [min(x), max(x)]
if n_elements(yrange)    eq 0 then yrange = [min(y), max(y)]

if (not keyword_set(ttit)) then ttit=" "
if (not keyword_set(xtit)) then xtit=" "
if (not keyword_set(ytit)) then ytit=" "

if (not keyword_set(xgrad)) then xgrad(0)=6  else  xgrad(0)=xgradp(0)

contour,tab,x,y,$
xrange=xrange,yrange=yrange,xstyle=1,ystyle=1,$
level=lev,c_linestyle=(lev lt 0)*1.0,c_label=lab,$
title=stab+ttit+ $
" (delta isol="+isoint+"  isomin="+isomin+"  isomax="+isomax+")"+$
" [min="+mint+", max="+maxt+"]",$
xtitle=sx+xtit,ytitle=sy+ytit+" (min:"+miny+",max:"+maxy+")"

end
