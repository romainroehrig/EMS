!/opt/cdat/bin/f2py -c -I/opt/cdat/include interpvertp.F90 -m interpvertp
SUBROUTINE interpvertp(nt,nlevin,nlevout,levin,levout,datain,dataout,missing)

IMPLICIT NONE

INTEGER, INTENT(IN) :: nt, nlevin, nlevout
REAL, INTENT(IN) :: levin(nt,nlevin)
REAL, INTENT(IN) :: levout(nt,nlevout)
REAL, INTENT(IN) :: datain(nt,nlevin)
REAL, INTENT(OUT) :: dataout(nt,nlevout)
REAL, INTENT(OUT) :: missing(nt,nlevout)

!f2py intent(in) nt,nlevin,nlevout,levin,levout,datain
!f2py intent(out) dataout, missing

integer ilevin, ilevout, ii, it
real slope
logical ltop, lbottom

if (levin(1,1).gt.levin(1,2)) then

do it=1,nt
  do ilevout=1,nlevout
    missing(it,ilevout) = 1.
    do ilevin=1,nlevin-1
      if ((levin(it,ilevin).ge.levout(it,ilevout)) &
         & .and.(levin(it,ilevin+1).lt.levout(it,ilevout))) then
        ii = ilevin
        missing(it,ilevout) = 0.
        ltop = .false.
        lbottom = .false.
      else
        if (levout(it,ilevout).lt.levin(it,nlevin)) then
          ii = nlevin
          missing(it,ilevout) = 0.
          ltop = .true.
          lbottom = .false.
        endif            
        if (levout(it,ilevout).gt.levin(it,1)) then
          ii = 1
          missing(it,ilevout) = 0.
          lbottom = .true.
          ltop = .false.
        endif   
      endif
    enddo
    if (missing(it,ilevout) == 0.) then
      if (ltop.or.lbottom) then
        dataout(it,ilevout) = datain(it,ii)
      else            
        slope = (levout(it,ilevout)-levin(it,ii))/(levin(it,ii+1)-levin(it,ii))

        dataout(it,ilevout) = datain(it,ii)+slope*(datain(it,ii+1)-datain(it,ii))
      endif
    endif
  enddo
enddo

else

do it=1,nt
  do ilevout=1,nlevout
    missing(it,ilevout) = 1.
    do ilevin=1,nlevin-1
      if ((levin(it,ilevin).lt.levout(it,ilevout)) &
         & .and.(levin(it,ilevin+1).ge.levout(it,ilevout))) then
        ii = ilevin
        missing(it,ilevout) = 0.
        ltop = .false.
        lbottom = .false.
      else
        if (levout(it,ilevout).lt.levin(it,1)) then
          ii = 1
          missing(it,ilevout) = 0.
          ltop = .true.
          lbottom = .false.
        endif
        if (levout(it,ilevout).gt.levin(it,nlevin)) then
          ii = nlevin
          missing(it,ilevout) = 0.
          ltop = .false.
          lbottom = .true.
        endif
      endif
    enddo
    if (missing(it,ilevout) == 0.) then
      if (ltop.or.lbottom) then
        dataout(it,ilevout) = datain(it,ii)
      else            
        slope = (levout(it,ilevout)-levin(it,ii))/(levin(it,ii+1)-levin(it,ii))

        dataout(it,ilevout) = datain(it,ii)+slope*(datain(it,ii+1)-datain(it,ii))
      endif
    endif
  enddo
enddo
        

endif

return

END      
