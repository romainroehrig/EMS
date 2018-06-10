!/opt/cdat/bin/f2py -c -I/opt/cdat/include convert2z.F90 -m convert2z
SUBROUTINE convert2z(nt,nlevin,nlevout,levin,levout,datain,dataout,missing)

IMPLICIT NONE

INTEGER, INTENT(IN) :: nt, nlevin, nlevout
REAL, INTENT(IN) :: levin(nt,nlevin)
REAL, INTENT(IN) :: levout(nlevout)
REAL, INTENT(IN) :: datain(nt,nlevin)
REAL, INTENT(OUT) :: dataout(nt,nlevout)
REAL, INTENT(OUT) :: missing(nt,nlevout)

!f2py intent(in) nt,nlevin,nlevout,levin,levout,datain
!f2py intent(out) dataout,missing

integer ilevin, ilevout, ii, it
real slope

do it=1,nt
  do ilevout=1,nlevout
    missing(it,ilevout) = 1.
    do ilevin=1,nlevin-1
!      if (it == 1) then
!        print*, levin(it,ilevin), levin(it,ilevin+1), levout(ilevout)
!      endif
      if ((levin(it,ilevin).gt.levout(ilevout)) &
         & .and.(levin(it,ilevin+1).le.levout(ilevout))) then
        ii = ilevin
        missing(it,ilevout) = 0.
      endif
    enddo
    if (missing(it,ilevout) == 0.) then
      slope = (levout(ilevout)-levin(it,ii))/(levin(it,ii+1)-levin(it,ii))

      dataout(it,ilevout) = datain(it,ii)+slope*(datain(it,ii+1)-datain(it,ii))
    endif
  enddo
enddo

return

END      
