      SUBROUTINE SWO_DIB(SPGRP,DBH,CR,DIB)
      IMPLICIT NONE
      INTEGER*4 SPGRP
      REAL*4 DBH,CR,DIB,BTPAR(18,3),E1,E2,E3
C
C  BARK THICKNESS (3 parameters - all species)
C
      DATA BTPAR/
     1   0.92443655,  0.92162494,  0.80860026,  0.85897904,  0.87875535, !  DF,GW,PP,SP,IC,
     1   0.933707  ,  0.9497    ,  0.97      ,  0.96317   ,  0.94448   , !  WH,RC,PY,MD,GC,
     1   0.859151  ,  0.910499  ,  0.97059   ,  0.878457  ,  0.889703  , !  TA,CL,BL,WO,BO,
     1   0.947     ,  0.94448   ,  0.94448   ,                           !  RA,PD,WI
C
     2   0.98886654,  1.0       ,  1.01742589,  1.0       ,  1.0       , !  DF,GW,PP,SP,IC,
     2   1.0       ,  1.0       ,  1.0       ,  1.0       ,  0.9875170 , !  WH,RC,PY,MD,GC,
     2   1.0178109 ,  1.01475   ,  0.993585  ,  1.02393   ,  1.0104062 , !  TA,CL,BL,WO,BO,
     2   1.0       ,  0.9875170 ,  0.9875170 ,                           !  RA,PD,WI
C
     3  -0.03414550, -0.03415396,  0.0       , 0.0        , -0.07696055, !  DF,GW,PP,SP,IC,
     3   0.0       ,  0.0       ,  0.0       , 0.0        ,  0.0       , !  WH,RC,PY,MD,GC,
     3   0.0       ,  0.0       ,  0.0       , 0.0        ,  0.0       , !  TA,CL,BL,WO,BO,
     3   0.0       ,  0.0       ,  0.0       /                           !  RA,PD,WI
      E1=BTPAR(SPGRP,1)
      E2=BTPAR(SPGRP,2)
      E3=BTPAR(SPGRP,3)
      DIB=E1*(DBH**E2)*EXP(E3*(1.0-CR)**0.5)
      RETURN
      END
C**********************************************************************
      SUBROUTINE NWO_DIB(SPGRP,DBH,CR,DIB)
      IMPLICIT NONE
      INTEGER*4 SPGRP
      REAL*4 DBH,CR,DIB,BTPAR(11,3),E1,E2,E3
C
C  BARK THICKNESS (3 parameters - all species)
C
      DATA BTPAR/
     1            0.971330, 0.92162494 , 0.933707, 0.9497  , 0.97    ,    !  DF,GF,WH,RC,PY
     1            0.96317 , 0.97059    , 0.878457, 0.947   , 0.94448 ,    !  MD,BL,WO,RA,PD
     1            0.94448 ,                                               !  WI
C
     2            0.966365, 1.0        , 1.0     , 1.0     , 1.0     ,    !  DF,GF,WH,RC,PY
     2            1.0     , 0.993585   , 1.02393 , 1.0     , 0.987517,    !  MD,BL,WO,RA,PD
     2            0.987517,                                               !  WI
C
     3            0.0     , -0.03415396, 0.0     , 0.0     , 0.0     ,    !  DF,GF,WH,RC,PY
     3            0.0     ,  0.0       , 0.0     , 0.0     , 0.0     ,    !  MD,BL,WO,RA,PD
     3            0.0     /                                               !  WI
      IF(SPGRP .EQ. 1) THEN
         E1=0.903563
         E2=0.989388
         E3=0.0
      ELSE
         E1=BTPAR(SPGRP,1)
         E2=BTPAR(SPGRP,2)
         E3=BTPAR(SPGRP,3)
      ENDIF
      DIB=E1*DBH**E2*EXP(E3*(1.0-CR)**0.5)
      RETURN
      END
C**********************************************************************
      SUBROUTINE RAP_DIB(SPGRP,DBH,DIB)
      IMPLICIT NONE
      INTEGER*4 SPGRP
      REAL*4 DBH,DIB,BTPAR(7,2),E1,E2
C
C  BARK THICKNESS (2 parameters - all species)
C
      DATA BTPAR/
     1            0.947   , 0.971330, 0.933707, 0.9497  , 0.97059 ,    !  RA,DF,WH,RC,BL
     1            0.94448 , 0.94448 ,                                  !  PD,WI

     2            1.0     , 0.966365, 1.0     , 1.0     , 0.993585,    !  RA,DF,WH,RC,BL
     2            0.987517, 0.987517/                                  !  PD,WI
C
      E1=BTPAR(SPGRP,1)
      E2=BTPAR(SPGRP,2)
      DIB=E1*DBH**E2
      RETURN
      END
C**********************************************************************
      SUBROUTINE SWO_DIB1FT(SPGRP,DBH,CR,DIB1FT)
      IMPLICIT NONE
      INTEGER*4 SPGRP
      REAL*4 DBH,CR,DIB1FT,SDPAR(18,5),G0,G1,G2,G3,G4
C
C  STUMP DIAMETER (4 parameters - all species)
C
      DATA SDPAR/
     1  0.149809111 , 0.393048214, 0.0      , 0.0       , 0.451569966 ,  !  DF,GW,PP,SP,IC,
     1  0.0         , 0.476734   , 0.476734 , 0.0       , 0.0         ,  !  WH,RC,PY,MD,GC,
     1  0.0         , 0.0        , 0.0      , 0.0       , 0.0         ,  !  TA,CL,BL,WO,BO,
     1  0.0         , 0.0        , 0.0      ,                            !  RA,PD,WI
C
     2  0.900790279 , 0.729932627, 1.0      , 1.04030514, 0.831752493 ,  !  DF,GW,PP,SP,IC,
     2  0.989819    , 0.819613   , 0.819613 , 1.0       , 1.0         ,  !  WH,RC,PY,MD,GC,
     2  1.0         , 1.0        , 1.0      , 1.0       , 1.0         ,  !  TA,CL,BL,WO,BO,
     2  1.0         , 1.0        , 1.0      ,                            !  RA,PD,WI
C
     3  0.133648456 , 0.120814754, 0.0      , 0.0       , 0.216216295 ,  !  DF,GW,PP,SP,IC,
     3  0.0         , 0.0        , 0.0      , 0.0       , 0.0         ,  !  WH,RC,PY,MD,GC,
     3  0.0         , 0.0        , 0.0      , 0.0       , 0.0         ,  !  TA,CL,BL,WO,BO,
     3  0.0         , 0.0        , 0.0      ,                            !  RA,PD,WI
C
     4  3.67532829  , 1.0        , 1.0      , 1.0       , 7.00446878  ,  !  DF,GW,PP,SP,IC,
     4  1.0         , 1.0        , 1.0      , 1.0       , 1.0         ,  !  WH,RC,PY,MD,GC,
     4  1.0         , 1.0        , 1.0      , 1.0       , 1.0         ,  !  TA,CL,BL,WO,BO,
     4  1.0         , 1.0        , 1.0      ,                            !  RA,PD,WI

C
     5  1.0213663112, 1.097851010, 1.0      , 1.0       , 1.0560026859,  !  DF,GW,PP,SP,IC,
     5  1.0         , 1.067437   , 1.067437 , 1.0       , 1.0         ,  !  WH,RC,PY,MD,GC,
     5  1.0         , 1.0        , 1.0      , 1.0       , 1.0         ,  !  TA,CL,BL,WO,BO,
     5  1.0         , 1.0        , 1.0      /                            !  RA,PD,WI
C
      G0=SDPAR(SPGRP,1)
      G1=SDPAR(SPGRP,2)
      G2=SDPAR(SPGRP,3)
      G3=SDPAR(SPGRP,4)
      G4=SDPAR(SPGRP,5)
      DIB1FT=G0+G1*EXP(G2*CR**G3)*DBH**G4
      RETURN
      END
C**********************************************************************
      SUBROUTINE NWO_DIB1FT(SPGRP,DBH,CR,DIB1FT)
      IMPLICIT NONE
      INTEGER*4 SPGRP
      REAL*4 DBH,CR,DIB1FT,SDPAR(11,5),G0,G1,G2,G3,G4
C
C  STUMP DIAMETER (5 parameters - all species)
C
      DATA SDPAR/
     1  0.149809111 , 0.393048214, 0.0     , 0.451569966 , 0.451569966,   ! DF,GF,WH,RC,PY
     1  0.0         , 0.0        , 0.0     , 0.0         , 0.0        ,   ! MD,BL,WO,RA,PD
     1  0.0         ,                                                     ! WI
C
     2  0.900790279 , 0.729932627, 0.989819, 0.831752493 , 0.831752493,   ! DF,GF,WH,RC,PY
     2  1.0         , 1.0        , 1.0     , 1.0         , 1.0        ,   ! MD,BL,WO,RA,PD
     2  1.0         ,                                                     ! WI
C
     3  0.133648456 , 0.120814754, 0.0     , 0.216216295 , 0.216216295,   ! DF,GF,WH,RC,PY
     3  0.0         , 0.0        , 0.0     , 0.0         , 0.0        ,   ! MD,BL,WO,RA,PD
     3  0.0     ,                                                         ! WI
C
     4  3.67532829  , 1.0        , 1.0     , 7.00446878  , 7.00446878 ,   ! DF,GF,WH,RC,PY
     4  1.0         , 1.0        , 1.0     , 1.0         , 1.0        ,   ! MD,BL,WO,RA,PD
     4  1.0         ,                                                     ! WI
C
     5  1.0213663112, 1.097851010, 1.0     , 1.0560026859, 1.0560026859,  ! DF,GF,WH,RC,PY
     5  1.0         , 1.0        , 1.0     , 1.0         , 1.0         ,  ! MD,BL,WO,RA,PD
     5  1.0         /                                                     ! WI
C
      G0=SDPAR(SPGRP,1)
      G1=SDPAR(SPGRP,2)
      G2=SDPAR(SPGRP,3)
      G3=SDPAR(SPGRP,4)
      G4=SDPAR(SPGRP,5)
      DIB1FT=G0+G1*EXP(G2*CR**G3)*DBH**G4
      RETURN
      END

C**********************************************************************
      SUBROUTINE RAP_DIB1FT(SPGRP,DBH,CR,DIB1FT)
      IMPLICIT NONE
      INTEGER*4 SPGRP
      REAL*4 DBH,CR,DIB1FT,SDPAR(7,5),G0,G1,G2,G3,G4
C
C  STUMP DIAMETER (5 parameters - all species)
C
      DATA SDPAR/
     1  0.0     , 0.149809111 , 0.0         , 0.451569966 , 0.0     ,  !  RA,DF,WH,RC,BL
     1  0.0     , 0.0         ,                                        !  PD,WI
C
     2  1.0     , 0.900790279 , 0.989819    , 0.831752493 , 1.0     ,  !  RA,DF,WH,RC,BL
     2  1.0     , 1.0         ,                                        !  PD,WI
C
     3  0.0     , 0.133648456 , 0.0         , 0.216216295 , 0.0     ,  !  RA,DF,WH,RC,BL
     3  0.0     , 0.0         ,                                        !  PD,WI
C
     4  1.0     , 3.67532829  , 1.0         , 7.00446878  , 1.0     ,  !  RA,DF,WH,RC,BL
     4  1.0     , 1.0         ,                                        !  PD,WI
C
     5  1.0     , 1.0213663112, 1.0         , 1.0560026859, 1.0     ,  !  RA,DF,WH,RC,BL
     5  1.0     , 1.0         /                                        !  PD,WI
C
      G0=SDPAR(SPGRP,1)
      G1=SDPAR(SPGRP,2)
      G2=SDPAR(SPGRP,3)
      G3=SDPAR(SPGRP,4)
      G4=SDPAR(SPGRP,5)
      DIB1FT=G0+G1*EXP(G2*CR**G3)*DBH**G4
      RETURN
      END
C**********************************************************************
      SUBROUTINE SWO_TAPER(SPGRP,AA1,AA2,A3,A4,ALP)
      IMPLICIT NONE
      INTEGER*4 SPGRP
      REAL*4 AA1,AA2,A3,A4,ALP,TPAR(8,5)
C
C  TAPER (5 parameters - conifer only)
C
      DATA TPAR/
     1 -0.55029801,-0.342017552, -0.595823501 ,-0.6       ,-0.596278066, !  DF,GW,PP,SP,IC
     1 -0.55029801,-0.596278066, -0.596278066 ,                          !  WH,RC,PY
C
     2 -0.69479837,-0.777574201, -1.25803662  ,-0.48435806,-0.83987883 , !  DF,GW,PP,SP,IC
     2 -0.69479837,-0.83987883 , -0.83987883  ,                          !  WH,RC,PY
C
     3 -6.13100423,-4.33569876 ,-13.867406    ,-3.3249206 ,-6.85768402 , !  DF,GW,PP,SP,IC
     3 -6.13100423,-6.85768402 , -6.85768402  ,                          !  WH,RC,PY
C
     4  0.35697451, 0.672963393,  0.0998711245, 0.10862035, 0.134178717, !  DF,GW,PP,SP,IC
     4  0.35697451, 0.134178717,  0.134178717 ,                          !  WH,RC,PY
C
     5  0.5       , 0.33       ,  0.6         , 0.74      , 0.71       , !  DF,GW,PP,SP,IC
     5  0.5       , 0.71       ,  0.71/                                  !  WH,RC,PY
C
      AA1=TPAR(SPGRP,1)
      AA2=TPAR(SPGRP,2)
      A3=TPAR(SPGRP,3)/100.0
      A4=TPAR(SPGRP,4)
      ALP=TPAR(SPGRP,5)
      RETURN
      END
C**********************************************************************
      SUBROUTINE NWO_TAPER(SPGRP,AA1,AA2,A3,A4,ALP)
      IMPLICIT NONE
      INTEGER*4 SPGRP
      REAL*4 AA1,AA2,A3,A4,ALP,TPAR(5,5)
C
C  TAPER (5 parameters - conifer only)
C
      DATA TPAR/
     1 -0.55029801,-0.342017552,-0.55029801,-0.596278066,-0.596278066,! DF,GF,WH,RC,PY
C
     2 -0.69479837,-0.777574201,-0.69479837,-0.83987883 ,-0.83987883 ,! DF,GF,WH,RC,PY
C
     3 -6.13100423,-4.33569876 ,-6.13100423,-6.85768402 ,-6.85768402 ,! DF,GF,WH,RC,PY
C
     4  0.35697451, 0.672963393, 0.35697451, 0.134178717, 0.134178717,! DF,GF,WH,RC,PY
C
     5  0.5       ,  0.33      ,  0.5      ,  0.71      ,  0.71      /! DF,GF,WH,RC,PY
C
      AA1=TPAR(SPGRP,1)
      AA2=TPAR(SPGRP,2)
      A3=TPAR(SPGRP,3)
      A4=TPAR(SPGRP,4)
      ALP=TPAR(SPGRP,5)
      RETURN
      END
C**********************************************************************
      SUBROUTINE RAP_TAPER(SPGRP,AA1,AA2,A3,A4,ALP)
      IMPLICIT NONE
      INTEGER*4 SPGRP,ISPGRP
      REAL*4 AA1,AA2,A3,A4,ALP,TPAR(3,5)
C
C  TAPER (5 parameters - conifer only)
C
      DATA TPAR/
     1 -0.55029801, -0.55029801, -0.596278066,! DF,WH,RC
C
     2 -0.69479837, -0.69479837, -0.83987883 ,! DF,WH,RC
C
     3 -6.13100423, -6.13100423, -6.85768402 ,! DF,WH,RC
C
     4  0.35697451,  0.35697451,  0.134178717,! DF,WH,RC
C
     5  0.5       ,  0.5       ,  0.71       /! DF,WH,RC
C
      ISPGRP=SPGRP-1
      AA1=TPAR(ISPGRP,1)
      AA2=TPAR(ISPGRP,2)
      A3=TPAR(ISPGRP,3)
      A4=TPAR(ISPGRP,4)
      ALP=TPAR(ISPGRP,5)
      RETURN
      END
