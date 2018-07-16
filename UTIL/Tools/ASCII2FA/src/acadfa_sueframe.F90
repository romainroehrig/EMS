      SUBROUTINE ACADFA_SUEFRAME(CDMCA,PEBETA,LDMAP, &
     &                    KMSMAX,KSMAX,KDGL,KDLON, &
     &                    LDGARD,KROTEQ,PELONR,PELATR,PELON1,PELAT1, &
     &                    PELON2,PELAT2,PELON0,PELAT0,PERPK,KSOTRP, &
     &                    KGIV0,PELX,PELY,PEDELX,PEDELY,PEXWN,PEYWN, &
     &                    KDLUN,KDLUX,KDGUN,KDGUX,KBZONL,KBZONG,KFLEV, &
     &                    PVP00,PVALH,PVBH,KDOM,KS)
      IMPLICIT LOGICAL(L)
      CHARACTER*16 CDMCA
      REAL ZGEOM(18)
      INTEGER IDOM(8),ISUBTR(1)
      REAL ZVALH(2),ZVBH(2), PVALH(0:KFLEV), PVBH(0:KFLEV),PVP00
!*       1.   DEFINE THE FRAME
      ZSLAPO = PEBETA
      ZCLOPO = 0.
      ZSLOPO = 0.
      IF (LDMAP) THEN
        ZCODIL = 0.
      ELSE
        ZCODIL = -1.
      ENDIF
      ITYPTR = -KMSMAX
      ITRONC = KSMAX
      INLATI = KDGL
      INXLON = KDLON
      ISUBTR(1)=1 
      ZGEOM (1)  = REAL(KROTEQ)
      ZGEOM (2)  = PELONR
      ZGEOM (3)  = PELATR
      ZGEOM (4)  = PELON1
      ZGEOM (5)  = PELAT1
      ZGEOM (6)  = PELON2
      ZGEOM (7)  = PELAT2
      ZGEOM (8)  = PELON0
      ZGEOM (9)  = PELAT0
      ZGEOM (10) = PERPK
      ZGEOM (11) = REAL(KSOTRP)
      ZGEOM (12) = REAL(KGIV0)
      ZGEOM (13) = PELX
      ZGEOM (14) = PELY
      ZGEOM (15) = PEDELX
      ZGEOM (16) = PEDELY
      ZGEOM (17) = PEXWN
      ZGEOM (18) = PEYWN
      IDOM  (1)  = KS
      IDOM  (2)  = KDOM
      IDOM  (3)  = KDLUN
      IDOM  (4)  = KDLUX
      IDOM  (5)  = KDGUN
      IDOM  (6)  = KDGUX
      IDOM  (7)  = KBZONL
      IDOM  (8)  = KBZONG
      IF (KFLEV.GT.1) THEN
      write(6,*)'avant facade'  
      write(6,*)'CDMCA=',CDMCA
      write(6,*)'ITYPTR=',ITYPTR
      write(6,*)'ZSLAPO=',ZSLAPO
      write(6,*)'ZCLOPO=',ZCLOPO
      write(6,*)'ZSLOPO=',ZSLOPO
      write(6,*)'ZCODIL=',ZCODIL
      write(6,*)'ITRONC=',ITRONC
      write(6,*)'INLATI=',INLATI
      write(6,*)'INXLON=',INXLON
      write(6,*)'IDOM=',IDOM
      write(6,*)'ISUBTR=',ISUBTR
      write(6,*)'ZGEOM=',ZGEOM
      write(6,*)'KFLEV=',KFLEV
      write(6,*)'PVP00=',PVP00
      write(6,*)'PVALH=',PVALH
      write(6,*)'PVBH=',PVBH
      write(6,*)'LDGARD=',LDGARD
      write(6,*)'ROEHRIG 1' 
      CALL FACADE(CDMCA,ITYPTR,ZSLAPO,ZCLOPO,ZSLOPO,ZCODIL,ITRONC, &
     &   INLATI,INXLON,IDOM,ISUBTR,ZGEOM,KFLEV,PVP00,PVALH,PVBH,LDGARD)
      write(6,*)'ROEHRIG 2'
      ELSE
        ZVALH(1) = 0.
        ZVALH(2) = 0.
        ZVBH(1) = 0.
        ZVBH(2) = 1.
        CALL FACADE(CDMCA,ITYPTR,ZSLAPO,ZCLOPO,ZSLOPO,ZCODIL,ITRONC, &
     &   INLATI,INXLON,IDOM,ISUBTR,ZGEOM,KFLEV,PVP00,ZVALH,ZVBH,LDGARD)
      ENDIF
! -------------------------------------------------------------------
      RETURN
      END SUBROUTINE ACADFA_SUEFRAME
