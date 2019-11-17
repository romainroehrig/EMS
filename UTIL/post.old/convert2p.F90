!f2py -c convert2p.F90 -m convert2p
SUBROUTINE convert2p(nt,nlevin,nlevout,levin,levout,datain,dataout,missing)

IMPLICIT NONE

INTEGER, INTENT(IN) :: nt, nlevin, nlevout
REAL, INTENT(IN) :: levin(nt,nlevin)
REAL, INTENT(IN) :: levout(nlevout)
REAL, INTENT(IN) :: datain(nt,nlevin)
REAL, INTENT(OUT) :: dataout(nt,nlevout)
REAL, INTENT(OUT) :: missing(nt,nlevout)

!f2py intent(in) nt,nlevin,nlevout,levin,levout,datain
!f2py intent(out) dataout, missing

integer ilevin, ilevout, ii, it
real slope

do it=1,nt
  do ilevout=1,nlevout
    missing(it,ilevout) = 1.
    do ilevin=1,nlevin-1
      if ((levin(it,ilevin).lt.levout(ilevout)) &
         & .and.(levin(it,ilevin+1).ge.levout(ilevout))) then
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
