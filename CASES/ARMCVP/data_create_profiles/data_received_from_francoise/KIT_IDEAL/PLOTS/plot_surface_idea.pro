;======================
;   CHOOSE CONFIG
;======================

  config='idea_2days'

;======================

set_plot,'ps'
device,filename="surface_"+config+".ps",/landscape,/color,/helvetica,/bold

;  ===================
;    READ INPUT DATA
;  ===================

anf='' & ant='' & nf=0 & nt=0

openr,1,'../DATA/surface_'+config+'.dat'

readf,1,anf,nf,ant,nt,$
format="(A70,/,6X,I2,/,A70,/,5X,F8.3)"

tabw  =fltarr(nt)    & atabw=''           ; temporary array and legend
tabsrf=fltarr(nf,nt) & legtab=strarr(nf)  ; array and legend

ii=0
while(not eof(1)) do begin
  readf,1,atabw,tabw,format="(A70,/,6(5(E15.7),/),3(E15.7))"
  print,'ii=',ii,' atabw=', atabw
  legtab(ii)=atabw
  tabsrf(ii,0:nt-1)=tabw(0:nt-1)
  if ii le nf-2 then ii=ii+1
endwhile

;    ===============
;         PLOTS
;    ===============

!p.multi=[0,2,2]
!p.thick=1.
!p.charsize=1
!p.charthick=1
!p.gridstyle=1
!x.style=1
!y.style=1
tek_color
!p.font=0

a=findgen(16)*(!pi*2/16.) & usersym,cos(a),sin(a),/fill

yrgmi=[   0,   0, -20, 970,975,20, 23, 50,    1, -1,0,-50,200,$
          0,   0,   0,   0,  0, 0,  0,  0,    0,  0,          $
      -0.05 ,-0.05,-300,-300 ,-300, 0                         ]

yrgma=[0.15, 300, 100, 980,985,32, 27,100,    4,  1,3,450,300,$
       1000,1100,   5,  30, 80 ,100,1.5, 10,0.025,0.6,        $
       0.35, 0.4, 300, 300,300 ,100                           ]

n1=0 & n2=nt-1
xmin=fix(tabsrf(3,n1)+tabsrf(4,n1)/24.+tabsrf(5,n1)/24./60.)
xmax=fix(tabsrf(3,n2)+tabsrf(4,n2)/24.+tabsrf(5,n2)/24./60.)+1

for ii=6,nf-1 do begin  ; ---------- loop on the fields ---------               
  plot,tabsrf(3,n1:n2)+tabsrf(4,n1:n2)/24.+tabsrf(5,n1:n2)/24./60.,$
  tabsrf(ii,n1:n2),title=strcompress(legtab(ii)),psym=-8,$
  yrange=[yrgmi(ii-6),yrgma(ii-6)],xrange=[xmin,xmax],$
  ticklen=0.5,xtitle="June 1997"
endfor

;    ==========================================

device,/close_file
set_plot,'x'

END
