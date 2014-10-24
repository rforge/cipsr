C     CIPSANON GROWTH AND YIELD MODEL
C     SUBROUTINES INCLUDED:
C         HTGRO1
C         HTGRO2
C         HS_HG
C         B_HG
C         F_HG
C         HG_SWO
C         HG_SMC
C         LIMIT
C         HD_SMC
C         HG_FERT
C         HG_THIN
C         RAGEA
C         RAH40
C         SWO_DF_HINC
C         SMC_DF_HINC
********************************************************************************
      SUBROUTINE HTGRO1(K,M,ON,VERSION,CYCLG,IB,TDATAI,TDATAR,WHC20,
     1                  PPTDD41,SI_1,SI_2,SI_3,DFSQ,SBALL,SBAL,BA,CCH,
     2                  CALIB,PN,YF,BABT,BART,YT,OLD,GROWTH)
      IMPLICIT NONE
C     CALCULATE 5-YEAR HEIGHT GROWTH
C
C     K = TREE NUMBER TO GROW HEIGHT
C
C     M = METHOD OF TRIPLING
C       0 = NO TRIPLING
C       1 = TRIPLE EVERY TREE
C       2 = TRIPLE EVERY OTHER TREE
C       3 = RANDOM ERROR
C
C     ON = EVERY OTHER TREE TRIPLING DESIGNATOR
C        1 = TRIPLE THIS TREE
C        0 = DON'T TRIPLE THIS TREE

      INTEGER*4 K,M,ON,VERSION,CYCLG,IB,TDATAI(2000,3),ISPGRP,I,ISISP,
     1          DFSQ
      REAL*4 TDATAR(2000,8),WHC20,PPTDD41,SI_1,SI_2,SI_3,SBALL(51),
     1       SBAL(500),CCH(41),BA,CALIB(6,18),PN(25),YF(25),BABT,
     2       BART(25),YT(25),OLD,GROWTH(2000,4),CR,DBH,H,XI,XXI,TCCH,
     3       SITE,GEAGE,GP,PHTGRO,IDXAGE,HG,FERTADJ,THINADJ,BAL
      INTEGER*2 GEA,IERRORS(3),IGP,IGEAGE
C      INTEGER*4 IANS,IYN
C
C     CALCULATE HEIGHT GROWTH
C
      CR=TDATAR(K,3)
      DBH=TDATAR(K,1)
      H=TDATAR(K,2)
      ISPGRP=TDATAI(K,2)
C
C     FOR MAJOR SPECIES
C
      IF(ISPGRP .LE. IB)THEN
           XI=40.0*(H/CCH(41))
           I=INT(XI)+2
           XXI=FLOAT(I)-1.0
           IF(H .GE. CCH(41)) THEN
              TCCH=0.0
           ELSE IF(I.EQ.41) THEN
              TCCH=CCH(40)*(40.0-XI)
           ELSE
              TCCH=CCH(I)+(CCH(I-1)-CCH(I))*(XXI-XI)
           ENDIF
           CALL GET_BAL(DBH,SBALL,SBAL,BAL)
C
C     COMPUTE HEIGHT GROWTH OF UNTREATED TREES
C
           GP=1.0
           IGP=INT(GP)
           SELECT CASE(VERSION)
              CASE(1)
                 IDXAGE=500.0
C
C                POTENTIAL HEIGHT GROWTH FROM HANN AND SCRIVANI'S (1987) DOMINANT
C                HEIGHT GROWTH EQUATION
C
                 IF(TDATAI(K,1) .EQ. 122)THEN
                    SITE=SI_2
                    ISISP=2
                 ELSE
                    SITE=SI_1
                    IF(TDATAI(K,1) .EQ. 81) SITE=(SI_1+4.5)*0.66-4.5
                    ISISP=1
                 ENDIF
                 IF(TDATAI(K,1) .EQ. 202 .AND. DFSQ .EQ. 1)THEN
                    CALL HS_HG(ISISP,SITE,H,GP,GEAGE,PHTGRO)
                    CALL SWO_DF_HINC(DBH,CR,WHC20,PPTDD41,BAL,BA,HG)
                 ELSE
                    CALL HS_HG(ISISP,SITE,H,GP,GEAGE,PHTGRO)
                    CALL HG_SWO(ISPGRP,PHTGRO,CR,TCCH,HG)
                 ENDIF
              CASE(2)
                 IDXAGE=100.0
                 IF(TDATAI(K,1) .EQ. 202) THEN
                    CALL FCGMT_GEA(SI_1,H,IGEAGE,IERRORS)
                    GEAGE=FLOAT(IGEAGE)
                    IF(IERRORS(1) .GT. 0 .OR. IERRORS(2) .GT. 0 .OR.
     1                 IERRORS(3).GT. 0) THEN
                       CALL B_HG(SI_3,H,GP,GEAGE,PHTGRO)
                    ENDIF
                    CALL FCGMT_HG(SI_1,IGP,IGEAGE,PHTGRO,IERRORS)
                    IF(IERRORS(1) .GT. 0 .OR. IERRORS(2) .GT. 0 .OR.
     1                 IERRORS(3) .GT. 0) THEN
                       CALL B_HG(SI_3,H,GP,GEAGE,PHTGRO)
                    ENDIF
                    IF(DFSQ .EQ. 1)THEN
                       CALL SMC_DF_HINC(DBH,CR,WHC20,PPTDD41,BAL,BA,HG)
                    ELSE
                       CALL HG_SMC(ISPGRP,PHTGRO,CR,TCCH,HG)
                    ENDIF
                 ELSEIF(TDATAI(K,2) .EQ. 3)THEN
C
C                   POTENTIAL HEIGHT GROWTH FROM FLEWELLING'S WESTERN HEMLOCK
C                   DOMINANT HEIGHT GROWTH
C
                    SITE = SI_2+4.5
                    CALL F_WH_HG(SITE,H,GP,GEAGE,PHTGRO)
                    CALL HG_SMC(ISPGRP,PHTGRO,CR,TCCH,HG)
                 ELSE
C
C                   POTENTIAL HEIGHT GROWTH FROM BRUCE'S (1981) DOMINANT HEIGHT
C                   GROWTH FOR GRAND FIR
C
                    SITE=SI_1+4.5
                    CALL B_HG(SITE,H,GP,GEAGE,PHTGRO)
                    CALL HG_SMC(ISPGRP,PHTGRO,CR,TCCH,HG)

                 ENDIF
           ENDSELECT
           IF(ISPGRP .LE. IB .AND. GEAGE .GT. IDXAGE)THEN
                OLD=OLD+1.
                IF(M .EQ. 1 .OR. (M .EQ. 2 .AND. ON .EQ. 1))OLD=OLD+2.
           ENDIF
           CALL HG_FERT(CYCLG,VERSION,ISPGRP,SI_3,PN,YF,FERTADJ)
           CALL HG_THIN(CYCLG,VERSION,ISPGRP,BABT,BART,YT,THINADJ)
           GROWTH(K,1)=HG*THINADJ*FERTADJ
           CALL LIMIT(VERSION,TDATAI(K,1),DBH,H,GROWTH(K,2),GROWTH(K,1))
      ENDIF
      RETURN
      END
**********************************************************************
      SUBROUTINE HTGRO2(K,VERSION,IB,TDATAI,TDATAR,RASI,CALIB,
     1                  GROWTH)
      IMPLICIT NONE
C     CALCULATE 5-YEAR HEIGHT GROWTH
C
C     K = TREE NUMBER TO GROW HEIGHT
C

      INTEGER*4 K,VERSION,IB,TDATAI(2000,3),ISPGRP
C      INTEGER*4 IANS,IYN
      REAL*4 TDATAR(2000,8),RASI,CALIB(6,18),GROWTH(2000,4),CR,DBH,
     1       PDBH,PRDHT1,PRDHT2,PRDHT,GEARA,RAH1,RAH2,RAHG
C
C     CALCULATE HEIGHT GROWTH FOR MINOR SPECIES
C
      CR=TDATAR(K,3)
      DBH=TDATAR(K,1)
      ISPGRP=TDATAI(K,2)
      IF(TDATAI(K,2) .GT. IB)THEN
           PDBH=DBH-GROWTH(K,2)
           SELECT CASE(VERSION)
              CASE(1)
                  CALL HD_SWO(ISPGRP,DBH,PRDHT2)
                  CALL HD_SWO(ISPGRP,PDBH,PRDHT1)
              CASE(2)
                  CALL HD_SMC(ISPGRP,DBH,PRDHT2)
                  CALL HD_SMC(ISPGRP,PDBH,PRDHT1)
           ENDSELECT
           PRDHT1=4.5+CALIB(1,ISPGRP)*(PRDHT1-4.5)
           PRDHT2=4.5+CALIB(1,ISPGRP)*(PRDHT2-4.5)
           PRDHT=(PRDHT2/PRDHT1)*TDATAR(K,2)
C
C          RED ALDER HEIGHT GROWTH - ADDED 12/13/02
C
           IF(TDATAI(K,1) .EQ. 351 .AND. VERSION .LE. 3) THEN
             CALL RAGEA(TDATAR(K,2),RASI,GEARA)
             IF(GEARA .LE. 0.0) THEN
                GROWTH(K,1)=0.0
             ELSE
               CALL RAH40(GEARA,RASI,RAH1)
               CALL RAH40(GEARA+1.0,RASI,RAH2)
               RAHG=RAH2-RAH1
               GROWTH(K,1)=RAHG
             ENDIF
           ELSE
              GROWTH(K,1)=PRDHT-TDATAR(K,2)
           ENDIF
           GROWTH(K,3)=GROWTH(K,3)+GROWTH(K,1)
      ENDIF
      RETURN
      END
**********************************************************************
      SUBROUTINE HS_HG(ISP,SI,HT,GP,GEAGE,PHTGRO)
      IMPLICIT NONE
      INTEGER*4 ISP
      REAL*4 SI,GEAGE,HT,GP,PHTGRO,B0,B1,B2,BBC,X50,A1A,XAI,XAI5
C
C     Hann and Scrivani (1987) FRL Research Bulletin 59
C
      IF(ISP .EQ. 1)THEN
         B0=-6.21693
         B1=0.281176
         B2=1.14354
      ELSE
         B0=-6.54707
         B1=0.288169
         B2=1.21297
      ENDIF
      BBC=B0+B1*LOG(SI)
      X50=1.0-EXP((-1.0)*EXP(BBC+B2*3.912023))
      A1A=1.0-(HT-4.5)*(X50/SI)
      IF(A1A .LE. 0.0)THEN
         GEAGE=500.0
         PHTGRO=0.0
      ELSE
         GEAGE=((-1.0*LOG(A1A))/(EXP(B0)*SI**B1))**(1.0/B2)
         XAI=1.0-EXP(-1.0*EXP(BBC+B2*LOG(GEAGE)))
         XAI5=1.0-EXP((-1.)*EXP(BBC+B2*LOG(GEAGE+GP)))
         PHTGRO=(4.5+(HT-4.5)*(XAI5/XAI))-HT
      ENDIF
      RETURN
      END
**********************************************************************
      SUBROUTINE B_HG(SI,HT,GP,GEAGE,PHTGRO)
C
C     Bruce (1981) Forest Science 27: 711-725
C
      IMPLICIT NONE
      REAL*4 SI,HT,X1,X2,B1,B2,GP,GEAGE,PHT,XX1,PHTGRO
      X1=13.25-SI/20.0
      X2=63.25-SI/20.0
      B2=-0.447762-0.894427*SI/100.0+0.793548*(SI/100.0)**2
     1   -0.171666*(SI/100.0)**3
      B1=ALOG(4.5/SI)/(X1**B2-X2**B2)
      XX1=ALOG(HT/SI)/B1+X2**B2
      IF(XX1 .GT. 0.0) THEN
         GEAGE=XX1**(1.0/B2)-X1
      ELSE
         GEAGE=500.0
      ENDIF
      PHT=SI*EXP(B1*((GEAGE+GP+X1)**B2-X2**B2))
      PHTGRO=PHT-HT
      RETURN
      END
**********************************************************************
      SUBROUTINE F_WH_HG(SI,HT,GP,GEAGE,PHTGRO)
C     For Western Hemlock compute Growth Effective Age and 5-year potential
C     or 1-year height growth using the western hemlock top height curves of
C     Flewelling.  These subroutines are required:
C       SITECV_F   computes top height from site and age
C       SITEF_C    computes model parameters
C       SITEF_SI   calculates an approximate psi for a given site
C       Note: Flewelling's curves are metric.
C             Site Index is not adjusted for stump height.
C
      IMPLICIT NONE
      INTEGER*4 I
      REAL*4 SI,GEAGE,HT,GP,PHTGRO,SIM,HTM,AGE,HTOP,PHT,XHTOP1,XHTOP2
      SIM = SI * 0.3048
      HTM = HT * 0.3048
C
C     Compute growth effective age
C
      AGE = 1.0
      DO I=1,4
    5     AGE = AGE + 100./10.**I
          IF(AGE .GT. 500.0) THEN
             GEAGE = 500.0
             CALL SITECV_F(SIM,GEAGE,XHTOP1)
             CALL SITECV_F(SIM,GEAGE+GP,XHTOP2)
             PHTGRO = 3.2808*(XHTOP2-XHTOP1)
             RETURN
          ENDIF
          CALL SITECV_F(SIM,AGE,HTOP)
          IF (HTOP .LT. HTM) GO TO 5
          AGE = AGE - 100./10.**I
      ENDDO
      GEAGE = AGE
C
C     Compute top height and potential height growth
C
      CALL SITECV_F(SIM,GEAGE+GP,HTOP)
      PHT = HTOP*3.2808
      PHTGRO = PHT - HT
      RETURN
      END
C**********************************************************************
      SUBROUTINE FCGMT_GEA(FCGMT_SI,HT_GIVEN,GEA,IERRORS)
      IMPLICIT NONE
      REAL*4 FCGMT_SI,HT40
      INTEGER*2 I,N,JAGE1,JAGE2,IAGE,IER1,IER2,IER3,IERRORS(3),GEA
      REAL*4 HT_PLANT,HT_GIVEN,HBASE_R4,HADJ_R4,PSI,SI
      CHARACTER*1 A1
      INTEGER*2 NVEC(10),IER,IHTYPE,LASTDENA
      REAL*4 SI_R4,FVEC(10),HV(100),HAV(100)
      REAL*4 HTM,HTOP1,HTOP2,X1,X2
C
      DO I=1,10
         NVEC(I)=0
         FVEC(I)=0.0
      ENDDO
      IER1=0
      IER2=0
      IER3=0
      NVEC(1)=100
      A1='E'
      IF(A1 .EQ. 'M' .OR. A1 .EQ. 'm') THEN
         NVEC(2)=2
      ELSE
         NVEC(2)=1
      ENDIF
      FVEC(1)=0.0
      CALL DFSITE1( FVEC, NVEC, IER)
C
C     Height at planting
C
      HT_PLANT = 0.0
      IF(HT_PLANT .LT. 0.0) RETURN
      CALL DFSITE2(HT_PLANT)
C
      DO I=1,100
         HV(I)=0.0
         HAV(I)=0.0
      ENDDO
C
C     Do not specify a density regime
C
      LASTDENA=0
      IHTYPE=1
      IER1=0
      IER2=0
      IER3=0
      IAGE=30
      CALL DFSITE3H(FCGMT_SI,IHTYPE,IAGE,PSI,SI_R4,IER3)
C
      IERRORS(1)=IER1
      IERRORS(2)=IER2
      IERRORS(3)=IER3
      GEA=0
      IF(IER1 .EQ. 0 .AND. IER2 .EQ. 0 .AND. IER3 .EQ. 0) THEN
         HTM = HT_GIVEN
         HTOP1 = 0.0
         HTOP2 = 0.0
         DO I=1,99
            JAGE1 = I
            JAGE2 = I+1
            CALL DFSITE4(JAGE1,HBASE_R4,HADJ_R4)
            HTOP1 = HBASE_R4
            CALL DFSITE4(JAGE2,HBASE_R4,HADJ_R4)
            HTOP2 = HBASE_R4
            IF (HTOP2 .GE. HTM) THEN
               X1=ABS(HTM-HTOP1)
               X2=ABS(HTOP2-HTM)
               IF(X1 .LT. X2) THEN
                  GEA = JAGE1
               ELSE
                  GEA = JAGE2
               ENDIF
               EXIT
            ENDIF
         ENDDO
      ENDIF
      RETURN
      END
C**********************************************************************
      SUBROUTINE FCGMT_HG(FSI,GP,GEA1,POTHG,IERRORS)
      IMPLICIT NONE
      REAL*4 FSI,POTHG
      REAL*4 HT401,HT402
      INTEGER*2 GP,GEA1,IERRORS(3)
      INTEGER*2 GEA2,IERRORS1(3),IERRORS2(3),I
      GEA2=GEA1+GP
      CALL FCGMT_H40(FSI,GEA1,HT401,IERRORS1)
      CALL FCGMT_H40(FSI,GEA2,HT402,IERRORS2)
      DO I=1,3
         IERRORS(I)=IERRORS1(I)+IERRORS2(I)
      ENDDO
      POTHG=HT402-HT401
      RETURN
      END
C**********************************************************************
      SUBROUTINE FCGMT_H40(FCGMT_SI,JAGE,HT40,IERRORS)
      IMPLICIT NONE
      REAL*4 FCGMT_SI,HT40
      INTEGER*2 I,N,JAGE,IAGE,IER1,IER2,IER3,IERRORS(3)
      REAL*4 HT_PLANT,HT_GIVEN,HBASE_R4,HADJ_R4,PSI,SI
      CHARACTER A1*1
      INTEGER*2 NVEC(10),IER,IHTYPE,LASTDENA
      REAL*4 SI_R4,FVEC(10),HV(100),HAV(100)
      DO I=1,10
         NVEC(I)=0
         FVEC(I)=0.0
      ENDDO
      IER1=0
      IER2=0
      IER3=0
C     *** Specify units
      A1 = 'E'
C     *** Use default height at age 2 (0 = default, stop if <0)
      HT_PLANT = 0.0
C     *** Don't specify densities
      HT_GIVEN=FCGMT_SI
      IAGE=30
C
      NVEC(1)=100
      IF(A1 .EQ. 'M' .OR. A1 .EQ. 'm') THEN
         NVEC(2)=2
      ELSE
         NVEC(2)=1
      ENDIF
      FVEC(1)=0.0
      CALL DFSITE1( FVEC, NVEC, IER)
C
C     Height at planting
C
      IF(HT_PLANT .LT. 0.0) RETURN
      CALL DFSITE2(HT_PLANT)
C
      DO I=1,100
         HV(I)=0.0
         HAV(I)=0.0
      ENDDO
C
C     Do not specify a density regime
C
      LASTDENA=0
      IHTYPE=1
      IER1=0
      IER2=0
      IER3=0
      CALL DFSITE3H(HT_GIVEN,IHTYPE,IAGE,PSI,SI_R4,IER3)
      IERRORS(1)=IER1
      IERRORS(2)=IER2
      IERRORS(3)=IER3
C      WRITE(20,123) IAGE, HT_GIVEN, PSI, SI_R4
C  123 FORMAT(' AGE = ',I2,' HT = ', F6.2, ' PSI = ',F9.4,' SI = ',
C     1       F11.4)
      IF(IER1 .EQ. 0 .AND. IER2 .EQ. 0 .AND. IER3 .EQ. 0) THEN
        CALL DFSITE4(JAGE,HBASE_R4,HADJ_R4)
        HT40=HBASE_R4
      ENDIF
      RETURN
      END
C**********************************************************************
      SUBROUTINE DF_DENC
      IMPLICIT NONE
c          Set up density coefficients

C      IMPLICIT REAL*8 (A-H, O-Z)
      INTEGER*2 MAXH_YR
      PARAMETER (MAXH_YR = 100)

      REAL*8  FTPALIM1(MAXH_YR), FTPALIM2(MAXH_YR)
      REAL*8  F1(MAXH_YR), F2(MAXH_YR)
      REAL*8  LTPAREF(MAXH_YR)
      REAL*8  FTROL(10)
      REAL*8  HBASE
      REAL*8  P0,P1,P2,P3,P4,P5,P6,P7,AGE,XPOS,TPALOW,TPAHIGH
      INTEGER*2 NTROL(10),IAGEBASE
      INTEGER*2 I

      COMMON /DF_CONTR/ FTROL, NTROL
      COMMON /DF_PLANT/ HBASE, IAGEBASE

      COMMON /DF_DEN1/ FTPALIM1, FTPALIM2, F1, F2, LTPAREF

c
c                            Fill in reference values for ln(TPA)
c
       DO I=1, NTROL(1)
          LTPAREF(I) = DLOG(300.0D0)
       ENDDO

c                          Limits of application young-age density effect
c                          Equivalent log(tpa) values stored.
       DO I=1, NTROL(1)

         IF(I .LE. 10) THEN
                 TPALOW = 80.0D0
                 TPAHIGH=1250.0D0
         ELSE
                 TPALOW = 40.0D0
                 TPAHIGH = 1900.0D0
         ENDIF
         FTPALIM1(I)= DLOG(  TPALOW )
         FTPALIM2(I)= DLOG(  TPAHIGH)
       ENDDO
c
c             Calculate F1 and F2 constants (by age) for offsets
c             Also the limits of ln(TPA) for application
c             FROM  smc\an4\deng1    Mar 26 2000
       P0 = 7.00D0
       P1 = -.2320D0
       P2 = .1636D0
       P3 = -.030128D0
       P5 = 34.636D0
       P6 = -13.18D0
       P7 = 1.393D0

       DO I=IAGEBASE, NTROL(1)
         AGE=DBLE(I)
         F2(I) = P1 + P2*DLOG(AGE+P0) + P3*(DLOG(AGE+P0))**2.0
c                  XPOS is value of log(TPA) where multiplier function is at maximum
         XPOS = P5 + P6*DLOG(AGE+P0) + P7*(DLOG(AGE+P0))** 2.0
         F1(I) = -2.0 * F2(I) * XPOS
       ENDDO

      RETURN
      END
C**********************************************************************
      SUBROUTINE DF_EVAL( IAGE, PSI_TEST, HT_EVAL )
      IMPLICIT NONE
c
c        Evaluate a trial value of psi
c
c     IAGE      integer  input    age
c     PSI_TEST  real*8   input    value of psi (feet)
c     HT_EVAL   real*8   output   height at age IAGE
c                                   (base or adjusted depending on ITYPE)
c
c
c        IAGE    integer   input      AGE >= IAGEBASE,  Hence IAGE > 2.
c
c      Note:  adjustment factors must have been precomputed thru age IAGE-1

C      IMPLICIT REAL*8 (A-H, O-Z)
      REAL*8 PSI_TEST,HT_EVAL
      INTEGER*2 IAGE,IAGEBASE,ITYPE
      REAL*8 HBASEV(100),HADJV(100)
      REAL*8 HBASE
      COMMON /DF_PLANT/   HBASE, IAGEBASE
      COMMON /DF_HTS/     HBASEV, HADJV
      COMMON /DF_SOLVT /  ITYPE

      CALL DF_PSET(PSI_TEST)

      IF(ITYPE .EQ. 1) THEN
         CALL DF_HN(IAGE, HT_EVAL)
      ELSE
         CALL DF_HADJ(IAGEBASE, IAGE)
         HT_EVAL = HADJV(IAGE)
      ENDIF

      RETURN
      END
C**********************************************************************
      SUBROUTINE DF_HADJ( IAGE1, IAGE2 )
      IMPLICIT NONE
c
c        Compute and store all base heights and adjusted heights
c               from IAGE1 to IAGE2.  (IAGE1 >= IAGEBASE, IAGE2>=IAGE1)
c        All earlier ages must have already been processed.
c
c
c      Note:  adjustment factors must have been precomputed thru age IAGE2-1

C      IMPLICIT REAL*8 (A-H, O-Z)
      REAL*8 FMULT(100),HBASEV(100),HADJV(100)
      REAL*8 HBASE
      INTEGER*2 IAGE1,IAGE2,IAGEBASE
      INTEGER*2 I

      COMMON /DF_DEN2/ FMULT
      COMMON /DF_HTS  / HBASEV, HADJV
      COMMON /DF_PLANT/ HBASE, IAGEBASE

      DO I = IAGE1, IAGE2
        CALL DF_HN( I, HBASEV(I))
        IF(I .EQ. IAGEBASE) THEN
           HADJV(I)= HBASEV(I)
        ELSE
           HADJV(I) = HADJV(I-1) + FMULT(I-1) * ( HBASEV(I)-HBASEV(I-1))
        ENDIF
      ENDDO
      RETURN
      END
C**********************************************************************
      SUBROUTINE DF_HN(IAGE,H)
      IMPLICIT NONE
c
c         Compute normative base height (before any density adjustment)
c         IAGE   Input   I*4 Starting age   [must be >= IAGEBASE ]
c         H      Output  R*8 Site ht (unadjusted)
c
C      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 LAMDA1,LAMDA2,X,H,Z
      REAL*8 HBASE
      REAL*8 PSI, B1, XK1, YK1, XK2, YK2, YPK, C, ALPHA, BETA
      INTEGER*2 IAGE,IAGEBASE
      COMMON /DF_PLANT/ HBASE, IAGEBASE

      COMMON /DF_CSPEC/  PSI, B1, XK1, YK1, XK2, YK2, YPK, C,
     1                   LAMDA1, LAMDA2, ALPHA, BETA

      IF(IAGE .GT. IAGEBASE) THEN
         X = DBLE(IAGE - IAGEBASE)
         IF(X .LT. XK1) THEN
             H = HBASE + PSI * ( X + (1.0D0-B1)*XK1/(C+1.0D0)*
     1         (( 1.0D0 -X/XK1)**(C+1.0D0) -1.0D0 ))
C            WRITE(20,1000) IAGE,IAGEBASE,X,H
C 1000       FORMAT(' #1A ',I2,1X,I2,1X,F11.4,1X,F11.4)
         ELSEIF(X .LE. XK2) THEN
             H = YK1 + (X - XK1)*PSI
C            WRITE(20,1100) IAGE,IAGEBASE,X,H
C 1100       FORMAT(' #1B ',I2,1X,I2,1X,F11.4,1X,F11.4)
         ELSE
            Z = X - XK2
C            WRITE(20,1300) ALPHA,BETA,LAMDA1,LAMDA2,Z,YK2
C 1300       FORMAT(' ALPHA = ',F12.6,' BETA = ',F12.6,' LAMBDA1 = ',
C     1             F12.6,' LAMDA2 = ',F12.6,' Z = ',F12.6,' YK2 = ',
C     2             F12.6)
            H = ALPHA *LAMDA1**Z + BETA*LAMDA2**Z + YK2 - ALPHA - BETA
C            WRITE(20,1200) IAGE,IAGEBASE,X,H
C 1200       FORMAT(' #1C ',I2,1X,I2,1X,F11.4,1X,F11.4)
         ENDIF
      ELSE
         H = HBASE
      ENDIF
      RETURN
      END
C**********************************************************************
      SUBROUTINE DF_PSET(AREF)
      IMPLICIT NONE
c                         establish specific curve coefficients   COEFSP
c
c     Aref     input   REAL*8     maximum growth rate (ft/yr)
c
C      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 LAMDA1 , LAMDA2, L, LAMDAMAX
      REAL*8 PSI, B1, XK1, YK1, XK2, YK2, YPK, C, ALPHA, BETA
      REAL*8 HBASE
      REAL*8 AREF,FP(13),TERM,RATE0,TEMP,SHAPE,X_STRAIGHT,TOP
      INTEGER*2 IAGEBASE,I

      COMMON /DF_PLANT/ HBASE, IAGEBASE

      COMMON /DF_CSPEC/  PSI, B1, XK1, YK1, XK2, YK2, YPK, C,
     1                   LAMDA1, LAMDA2, ALPHA, BETA

c                model coefficients from MOD12_04.coe
      DATA (FP(I), I=1,13) /     0.64640000D+04 ,  -0.16910000D+04,
     &                          -0.29230000D+02 ,   0.75100000D+01,
     &                           0.90750000D+00 ,   0.17880000D+00,
     &                          -0.10240000D+03 ,   0.31930000D+02,
     &                           0.39670000D+02 ,   0.66580000D+02,
     &                          -0.42790000D+02 ,   0.16570000D+02,
     &                           0.35050000D+00 /

c                         True Origin
      IF(IAGEBASE .NE. 2) THEN
          RETURN
      ENDIF
c                          Maximum growth rate (at inflection) == PSI
      PSI = AREF

c                          Growth rate at origin (x=0, age=2) is b1 * PSI
      TERM = FP(3) + FP(4)* AREF
      IF(TERM .LT. -8.0D0) TERM = -8.0D0
      IF(TERM .GT.  8.0D0) TERM =  8.0D0
      RATE0 = DEXP(TERM) / (1.0D0 + DEXP(TERM) ) * AREF

      B1 = RATE0/PSI
c                        Inflection is at x= xk
      TEMP = FP(1)+FP(2)*PSI
      IF(TEMP .GT. 0.0) TEMP=TEMP**FP(13)
      XK1 = MAX( 1.0D0 , TEMP)
c     xk1 = max( 2.0d0, fp(1) + fp(2)*psi)

c                        Shape of the below-inflection curve
      C = FP(5)+FP(6)*PSI

c                        YK is value of y at inflection
       YK1 = HBASE + XK1*PSI * (1.0D0 - (1.0D0-B1)/(C+1.0D0) )

c                                  insert the straight segment here
       X_STRAIGHT = MAX(0.0D0 , FP(7) + FP(8)*PSI )

       XK2 = XK1 + X_STRAIGHT
       YK2 = YK1 + X_STRAIGHT * PSI

c
c                        SLOPE  dY/dZ at Inflection  YPK
c                           if dZ/dX =1  then YPK = PSI
c                           else PSI = dY/dZ * dZ/dX
c                               (or YPK = PSI / (dZ/dX) )
       YPK = PSI
c
c                        TOP is the asymptote (ht at an infinite age).
       TOP = FP(9)*PSI + FP(10)
c                        For iteration purposes only, put in a limit
       TOP = MAX (TOP, YK2 +15.0)
c
c

c
       LAMDAMAX = DEXP( 2.0D0*YPK/(YK2-TOP))
       SHAPE =  FP(11) + FP(12)*PSI
       IF(SHAPE .GT. 8.0D0) SHAPE=8.0D0
       IF(SHAPE .LT. -8.0D0) SHAPE=-8.0D0
       LAMDA1 = 0.05D0 + ( LAMDAMAX-0.05D0 ) *
     1                   DEXP(SHAPE)/(1.0D0+DEXP(SHAPE))
       L = DLOG(LAMDA1)
       ALPHA = -YPK*YPK/( L*L*(YK2-TOP) -2.0*L*YPK)
       BETA = YK2 - TOP - ALPHA
       LAMDA2 = DEXP( (YPK-ALPHA*DLOG(LAMDA1))/BETA)

c
      RETURN
      END

C**********************************************************************
      SUBROUTINE DF_SOLV1( IAGE, HTARGET, PSIGUESS )
      IMPLICIT NONE
c
c     GUESS an approximate value of PSI that produces HTARGET at age IAGE
c                HTARGET is a base ht if ITYPE=1, or an adjusted ht if ITYPE=2
c     IAGE     integer   input   age; must be > IAGEBASE(which is typically 2)
c     HTARGET  Real*8    input   desired height; must be > HBASE
c     PSIGUESS Real*8    output  A very rough estimate of PSI

C      IMPLICIT REAL*8 (A-H, O-Z)
      REAL*8 A0(4),A1(4),A2(4),A3(4)
      INTEGER*2 IAGE,NTROL(10),IAGEBASE,I,IAGEGRP
      REAL*8 FTROL(10)
      REAL*8 HBASE
      REAL*8 PSIGUESS,HTARGET,RHS,X
      COMMON /DF_CONTR/ FTROL, NTROL
      COMMON /DF_PLANT/ HBASE, IAGEBASE

      DATA (A0(I),I=1,4)/ 55.28D0 , 16.37D0 , 36.47D0 , 51.14D0 /
      DATA (A1(I),I=1,4)/ 3.087D0 , 1.905D0 , 1.422D0 , 1.162D0 /
      DATA (A2(I),I=1,4)/-.00734D0,-.0438D0 ,-.0317D0 , -.028D0 /
      DATA (A3(I),I=1,4)/ 1.756D0 , 2.007D0 , 1.768D0 , 1.630D0 /

      PSIGUESS=2.5
      IF(IAGE .LE. IAGEBASE) RETURN
      IF(HTARGET .LE. HBASE) RETURN
c                                         find age group (IAGEGRP)
      IF(IAGE .LE. 6) THEN
            IAGEGRP=1
      ELSEIF(IAGE .LE. 20) THEN
            IAGEGRP=2
      ELSEIF(iage .LE. 40) THEN
            IAGEGRP=3
      ELSE
            IAGEGRP=4
      ENDIF


c     regressions had been fit earlier by IAGEGRP (1 to 4) (\an4\regpsi2.sas)
c     Using a model of the form:

c      H = HBASE +  A0*PSI**A1  * ( 1 - exp(a2*(iage-iagebase)))**A3
c
c      The solution for PSI is:
c
c       A0*PSI**A1 = (H - HBASE) /  ( 1 - exp(a2*(iage-iagebase)))**A3
c
      X=DBLE(IAGE-IAGEBASE)
      RHS = (HTARGET - HBASE) /
     &        ( 1.0D0 - DEXP( A2(IAGEGRP)*X) )**A3(IAGEGRP)

      PSIGUESS = (RHS/A0(IAGEGRP))**(1.0D0/A1(IAGEGRP))

      RETURN
      END
C**********************************************************************
      SUBROUTINE DF_SOLV( IAGE, HTARGET, GUESS, JER )
c
c     FIND a value of PSI that produces HTARGET at age IAGE
c                HTARGET is a base ht if ITYPE=1, or an adjusted ht if ITYPE=2
c                (ITYPE is not explicity used here).
c     IAGE     integer   input   age; must be > IAGEBASE(which is typically 2)
c     HTARGET  Real*8    input   desired height; must be > HBASE
c     GUESS    Real*8    input   a starting value for psi (or zero)
c     JER      integer   output  0 for success,  1 for failure , 2 illegal input
c
c     Root-finding methodology is Newton's with derivatives from the DELTA X method.
C
c     A better choice (at least for the higher site indices) would have been the
C     false position method, followed by polishing with Newton's method.
C     The reason for this is that the derivative of HT vs PSI has some discontinuities.
c

c     CHANGES  ******************************************************
c     2012.05.20   JWF    MAJOR CHANGES. A CODING ERROR CORRECTED AND ENHANCEMENTS MADE.

      IMPLICIT NONE

C      INTEGER*2 IAGE,NTROL(10),IAGEBASE,JER,MAXEVAL,NEVAL
C      REAL*8 HTARGET,GUESS,FTROL(10),HBASE
C      REAL*8 TOO_HIGH,TOO_LOW,FUZZ,PSIGUESS,PSI1,HT1,PSI2,PSINEXT,SLOPE,
C     1       HT2
C      COMMON /DF_CONTR/ FTROL, NTROL
C      COMMON /DF_PLANT/ HBASE, IAGEBASE


C      INTEGER*4 IAGE, JER                ! Total age where height HTARGET is specified,  + error code
C      INTEGER*4 IAGEBASE                 ! Age where height is independent of site index (usually 2).
C      INTEGER*4 MAXEVAL, NEVAL           ! Interations - maximum allowed, and counter.
C      INTEGER*4 NTROL(10)                ! Control values (integer)

      INTEGER*2 IAGE, JER                ! Total age where height HTARGET is specified,  + error code
      INTEGER*2 IAGEBASE                 ! Age where height is independent of site index (usually 2).
      INTEGER*2 MAXEVAL, NEVAL           ! Interations - maximum allowed, and counter.
      INTEGER*2 NTROL(10)                ! Control values (integer)

      REAL*8    DELAmin                  ! Absolute value of minimum step for slope calculation
      REAL*8    DELpsi                   ! Change in psi to be used for slope evaluation.
      REAL*8    FTROL(10)                ! control values (floating point)
      REAL*8    FUZZ                     ! Maximum allowed error in achieving HTARGET
      REAL*8    GUESS                    ! INPUT: zero or well-informed guess for psi
      REAL*8    HBASE                    ! [common]  height at IAGEBASE
      REAL*8    HT1, HT2                 ! evaluated height at PSI1 and PSI2.
      REAL*8    HTARGET                  ! INPUT: top height wanted at specified IAGE.
      REAL*8    PSI1, PSI2               ! Psi values used at which height is evaluated.
      REAL*8    PSIguess                 ! Trial value of psi (changes each iteration).
      REAL*8    PSInext                  ! PSI to be evaluated in next iteration.
      REAL*8    SLOPE                    ! Approximate derivative of HT with respect to PSI.
      REAL*8    STEPmin, STEPmax         ! allowable changes in PSI
      REAL*8    STEPpsi                  ! proposed change in psi for improvement in height.
      REAL*8    TOO_LOW, TOO_HIGH        ! limits on psi (arbitrary to start, then narrowed).

      COMMON /DF_CONTR/ FTROL, NTROL           ! Real*8(10), I*4(10)
      COMMON /DF_PLANT/ HBASE, IAGEBASE        ! REL*8, I*4

      JER = 0

      IF((HTARGET .LE. HBASE) .OR. (IAGE .LE. IAGEBASE)) THEN
         JER=2
         RETURN
      ENDIF

c    Establish known benchmarks
      TOO_HIGH = 11.0D0
      TOO_LOW  = 0.01D0
      DELAmin  = 0.003D0

c                                 Convergence criteria:  abs(H - HTARGET) < FUZZ
      FUZZ = 0.0D0
      FUZZ=MAX(FUZZ, FTROL(1))
      IF(FTROL(2) .GT. 0.0)  FUZZ = MIN( FUZZ, FTROL(2)*HTARGET)

c                                                Prepare for first evaluation
      MAXEVAL = 100
      IF(GUESS .gt. 0.0D0) THEN
         PSIGUESS = GUESS
      ELSE
         CALL DF_SOLV1( IAGE, HTARGET, PSIGUESS)
      ENDIF

      PSI1 = PSIGUESS
      NEVAL = 0
      DELpsi = DELAmin
c                                               Iterative evaluation loop
50    NEVAL = NEVAL+1
      CALL DF_EVAL(IAGE, PSI1, HT1)
c                                              Test for solution or error
      IF(ABS(HT1 - HTARGET) .LT. FUZZ ) THEN
         RETURN
      ENDIF
      IF(NEVAL .GE. MAXEVAL) THEN
         JER = 1
         RETURN
      ENDIF

c              Estimate the slope of HT versus psi function and prepare
c              to implement Newton's method to get next trial psi,
      PSI2 = PSI1 + DELpsi
      CALL DF_EVAL( IAGE, PSI2, HT2)
      SLOPE = (HT2 - HT1)/(PSI2-PSI1)
      STEPpsi = (HTARGET - HT1)/SLOPE
c                 usually will want  [ PSInext = PSI1 + STEPpsi ]
c
c          Under some circumstances, modify the step size.
c             Start this process by limiting to 50% increas or 40% decline.

      IF(STEPpsi/PSI1 .GT. 0.5D0 .AND. PSI1 .GT. 0.50D0 ) THEN
         STEPpsi = 0.50D0 * PSI1
      ELSEIF(STEPpsi/PSI1 .lt. -0.40D0) then
         STEPpsi =-0.4D0 *PSI1
      ENDIF

c              calculate a tentative next trial psi
      PSInext = PSI1 + STEPpsi

c            Compare the tentative step (STEPpsi) against known limits.
c            Do not move more than 90% of the distance to a known limit.
c
c             Also, if step size is limited by 90% rule,
c             allow the value of DELpsi to be up to 5% of the range.
c             Always set DELpsi so that it is in the direction from which we came.
C             (For positive STEPpsi, want negative DELpsi), and

      IF(STEPpsi .GT. 0.0D0) THEN
          DELpsi = -DELAmin
      ELSE
          DELpsi = DELAmin
      ENDIF

      STEPmax = 0.90D0 * (TOO_HIGH - PSI1)
      STEPmin = 0.90D0 * ( TOO_LOW - PSI1)
      IF(STEPpsi .GT. STEPmax) THEN
         STEPpsi = STEPmax
         DELpsi = MIN(DELpsi, 0.05D0* (PSI1 - TOO_HIGH) )
      ELSEIF(STEPpsi .LT. STEPmin) THEN
         STEPpsi = STEPmin
         DELpsi = MAX( DELAmin, 0.05D0* (TOO_LOW - PSI1) )
      ENDIF
c
c            Reset TOO_HIGH and TOO_LOW
      IF(HT1 .GT. HTARGET) THEN
         TOO_HIGH=MIN( TOO_HIGH, PSI1)
      ELSE
         TOO_LOW = MAX( TOO_LOW, PSI1)
      ENDIF
c
c             Calculate the trial psi that we will use next.
      PSInext = PSI1 + STEPpsi
c
      PSI1 = PSINEXT
      GO TO 50
      RETURN
      END
C**********************************************************************
      SUBROUTINE DFCALC(A1,DENSITY,N_DEN,IX,Y,HT_PLANT,HT_GIVEN,IAGE,
     1                  PSI_FT,SI_R4,U_LENGTH,IER1,IER2,IER3)
C
      IMPLICIT NONE
      CHARACTER*1 A1, DENSITY
      CHARACTER U_LENGTH*2
      INTEGER*2 N_DEN,IAGE,IER1,IER2,IER3,IX(100),NVEC(10),IER,I,IHTYPE,
     1          LASTDENA,N,J1,J2,I1,I2,K1
      REAL*4 HT_GIVEN,PSI_FT,SI_R4,FVEC(10),DENV(100),HV(100),HAV(100),
     1       Y(100),HT_PLANT,Y1,Y2,FL
C
      NVEC(1)=100
      IF(A1 .EQ. 'M' .OR. A1 .EQ. 'm') THEN
         NVEC(2)=2
         U_LENGTH='m.'
      ELSE
         NVEC(2)=1
         U_LENGTH='ft'
      ENDIF
      FVEC(1)=0.0
      CALL DFSITE1( FVEC, NVEC, IER)
C
C     Height at planting
C
      IF(HT_PLANT .LT. 0.0) RETURN
      CALL DFSITE2(HT_PLANT)
C
      DO I=1,100
         HV(I)=0.0
         HAV(I)=0.0
      ENDDO
C
C     Do not specify a density regime
C
      LASTDENA=0
      IHTYPE=1
      IF (DENSITY .EQ. 'Y') THEN
         IHTYPE=2
         N = N_DEN
C
         DENV(1)=0.0
         DO J1 = 1, N-1
            J2 = J1+1
            I1 = IX(J1)
            I2 = IX(J2)
            Y1 = Y(J1)
            Y2 = Y(J2)
            DO I=I1,I2
               FL = LOG(Y1)
               IF (I. GT. I1) THEN
                  FL = FL + FLOAT(I-I1)/FLOAT(I2-I1)*(LOG(Y2)-LOG(Y1))
               ENDIF
               DENV(I) = EXP(FL)
            ENDDO
         ENDDO
         LASTDENA = IX(N)
         IER1=0
         K1=2
         CALL DFSITE3D( K1, LASTDENA, DENV, IER1)
         IF(IER1 .GT. 0) RETURN
         IER2=0
         IF (IAGE .GT. LASTDENA+1) THEN
            IER2=1
            RETURN
         ENDIF
      ENDIF
      IER3=0
      CALL DFSITE3H(HT_GIVEN,IHTYPE,IAGE,PSI_FT,SI_R4,IER3)
      RETURN
      END
C**********************************************************************
      SUBROUTINE DFSITE1( FVEC, NVEC, IER )
c
c        Set up parameters that affect operation of software
c         FVEC(10)     REAL*4   INPUT    floating point control parameters
C         NVEC(10)   integer  INPUT    integer control parameters
c         IER          integer  OUTPUT   error code (0 for no error)
c                                        <0 for warning.  >0 for fatal error.
c
c     Interpretation of the variables
c            NVEC(1) = maximum age to ever use.  Required: 30-100  (0 for default)
c            NVEC(2) =  1 for english units (feet, TPA), 2 for metric (m., TPH)
c
c            FVEC(1) = maximum allowable error in ht (ft) while finding psi.
c            FVEC(2) = maximum allowable error in fractional height.
c                    For above 2, values of zero will be ignored.
c                    BUT at LEAST ONE of them SHOULD be positive.
c                    If both are 0 or negative, default values are used.
c                    If both are positive, then the maximum error is:
c                      MIN [ Fvec(1),  FVEC(2)* (target height) ]
c
c    CHANGES *************************************************************
C     2012.05.20  JWF   Default allowable error vector FVEC changed from being
C                        (1.0D-7,  1.0D-8) to (0.0003D0,  1.0D-5)

      REAL*4 FVEC(10)
      INTEGER*2 NVEC(10),NTROL(10),IER,IAGEBASE,I
      REAL*8 FTROL(10),HMULT,TMULT
      REAL*8 HBASE

      COMMON /DF_CONTR/ FTROL, NTROL
      COMMON /DF_CONV / HMULT, TMULT
      COMMON /DF_PLANT/ HBASE, IAGEBASE

      DO I=1,10
         NTROL(I) = NVEC(I)
      ENDDO
      IF(NTROL(2) .EQ. 2) then
        HMULT = 0.3048D0
        TMULT = 0.40468564D0
      ELSE
        HMULT = 1.0D0
        TMULT = 1.0D0
      ENDIF

      DO I= 1,10
         FTROL(I) = DBLE( FVEC(I))
      ENDDO
      IF(FTROL(1) .LE. 0.0D0 .AND. FTROL(2) .LE. 0.0D0) then
         FTROL(1) = 0.0003D0
         FTROL(2) = 1.0D-5
      ENDIF


c               checks on the control parameters
      IER=0
      IF(NTROL(1) .EQ. 0) THEN
         NTROL(1) = 100
      ENDIF
      IF(NTROL(1) .LT. 30 .OR. NTROL(1) .GT. 100) THEN
         IER = 1
         NTROL(1)=30
      ENDIF

c                             initializations
      IAGEBASE = 2
      IF(IER .LE. 0) THEN
         CALL DF_DENC
      ENDIF
      RETURN
      END
C**********************************************************************
      SUBROUTINE DFSITE2( HPLANT )
      IMPLICIT NONE
c
c        Specify height at planting. Reinitializes site curve and density history.
c      HPLANT  Real*4   input   height at planting. (or 0 for default)

C      IMPLICIT REAL*8 (A-H, O-Z)
      INTEGER*2 NTROL(10),IAGEBASE,IBASEMAX,IADJMAX,LHIST,I
      REAL*4 HPLANT
      REAL*8 HBASE
      REAL*8 LAMDA1, LAMDA2
      REAL*8 FTROL(10),HMULT,TMULT
      REAL*8 PSI, B1, XK1, YK1, XK2, YK2, YPK, C, ALPHA, BETA
      REAL*8 HBASEV(100), HADJV(100), TPAV(100)

      COMMON /DF_CONTR/ FTROL, NTROL
      COMMON /DF_CONV / HMULT, TMULT
      COMMON /DF_CSPEC/  PSI, B1, XK1, YK1, XK2, YK2, YPK, C,
     1                   LAMDA1, LAMDA2, ALPHA, BETA
      COMMON /DF_HTS/ HBASEV, HADJV
      COMMON /DF_PLANT/ HBASE, IAGEBASE
      COMMON /DF_RPT/  IBASEMAX, IADJMAX
      COMMON /DF_THIST/  TPAV, LHIST

      IF(HPLANT .NE. 0.0) THEN
         HBASE = DBLE(HPLANT) / HMULT
      ELSE
         HBASE=1.4D0
      ENDIF

      DO I=1, NTROL(1)
         HBASEV(I)=0.0D0
         HADJV(I) =0.0D0
      ENDDO

      LHIST = 0
      PSI = 0.0D0
c                     consider that the output arrays have been filled to:
      IBASEMAX =IAGEBASE-1
      IADJMAX  =IAGEBASE-1

      RETURN
      END
C**********************************************************************
      SUBROUTINE DFSITE3D( IAGE1, IAGE2, DENV, IER )
      IMPLICIT NONE
c
c        Specify a complete or partial density history.
c        Compute density effect multipliers for these ages.
c        IAGE1    integer     input    1st age with new specification ( >= 1)
c        IAGE2    integer     input    last age with new spec. (IAGE2 >= IAGE1)
c        DENV     REAL*4(100) input    densities (cells IAGE1 to IAGE2)
c        IER      integer     output   Error code (0=OK, 1= WRONG age range)

c      HPLANT  Real*4   input   height at planting.

C      IMPLICIT REAL*8 (A-H, O-Z)
      INTEGER*2 IAGE1,IAGE2,IER,IAGEBASE,LHIST,I
      REAL*4 DENV(100)
      REAL*8 LTPAREF(100)
      REAL*8 HMULT,TMULT
      REAL*8 FTPALIM1(100),FTPALIM2(100),F1(100),F2(100),FMULT(100)
      REAL*8 HBASE
      REAL*8 TPAV(100),DLOGTPA

      COMMON /DF_CONV / HMULT, TMULT
      COMMON /DF_DEN1/  FTPALIM1,FTPALIM2,F1,F2,LTPAREF
      COMMON /DF_DEN2/  FMULT
      COMMON /DF_PLANT/ HBASE,IAGEBASE
      COMMON /DF_THIST/  TPAV,LHIST


      IF( (IAGE1 .LE. IAGEBASE  .OR. IAGE1 .LE. LHIST+1)
     &    .AND.  ( IAGE2 .GE. IAGE1) .AND. IAGE2 .LE. 100) THEN

        DO I=IAGE1, IAGE2
           IF(TMULT .NE. 1.0D0) THEN
              TPAV(I) = DBLE(DENV(I))* TMULT
           ELSE
              TPAV(I) = DBLE(DENV(I))
           ENDIF
c                        calculate the density effect
           DLOGTPA = DLOG( TPAV( I) )
           DLOGTPA = MAX( DLOGTPA, FTPALIM1(I))
           DLOGTPA = MIN( DLOGTPA, FTPALIM2(I))

           FMULT( I) = 1.0d0 +  F1(I) * ( DLOGTPA - LTPAREF(I))
     &              + F2(I) *( DLOGTPA*DLOGTPA - LTPAREF(I)*LTPAREF(I) )
        ENDDO
        LHIST = IAGE2
        IER=0
      ELSE
        IER=1
      ENDIF
      RETURN
      END
C**********************************************************************
      SUBROUTINE DFSITE3H( HT_R4, IHTYPE, IAGE, PSI_FEET, SI_R4, IER )
      IMPLICIT NONE
c
c        Specify a height (base or adjusted). Calculate psi and SI.
c
c     HT_R4    real*4   input   height (feet or meters)
c     IHTYPE   integer  input   1 if HT_R4 is a BASE height, 2 if density-adjusted
c     IAGE     integer  input   age from seed  [ > 2, <= NTROL(1) ]
c     PSI_FEET real*4   output  psi (feet)
c     SI_R4    real*4   output  site index (from base curve), feet or meters
c     IER      integer  output  0 if successful, >0 if a problem
c                                 1:  IAGE is not in bounds
c                                 2:  DFSITE3D has not provided densities to IAGE-1
C      IMPLICIT REAL*8 (A-H, O-Z)
      REAL*4 PSI_FEET, HT_R4, SI_R4
      REAL*8 LAMDA1,LAMDA2
      INTEGER*2 IHTYPE,IAGE,IER,IBAGE

      INTEGER*2 NTROL(10),IAGEBASE,IBASEMAX,IADJMAX,LHIST,ITYPE,I,JER
      REAL*4 HPLANT
      REAL*8 FTROL(10),HMULT,TMULT
      REAL*8 PSI, B1, XK1, YK1, XK2, YK2, YPK, C, ALPHA, BETA
      REAL*8 HBASEV(100), HADJV(100), TPAV(100)
      REAL*8 FMULT(100)
      REAL*8 HT_WANT1,DENMULT,PSIGUESS,HT_WANTB,SI
      REAL*8 HBASE

      COMMON /DF_CONTR/ FTROL, NTROL
      COMMON /DF_CONV / HMULT, TMULT
      COMMON /DF_CSPEC/  PSI, B1, XK1, YK1, XK2, YK2, YPK, C,
     1                   LAMDA1, LAMDA2, ALPHA, BETA
      COMMON /DF_DEN2/  FMULT
      COMMON /DF_HTS/  HBASEV, HADJV
      COMMON /DF_PLANT/ HBASE, IAGEBASE
      COMMON /DF_SOLVT / ITYPE
      COMMON /DF_THIST/  TPAV, LHIST

      IER = 0

      HT_WANT1 = DBLE(HT_R4) / HMULT
c                                               Is age in bounds?
      IF (IAGE .LE. IAGEBASE  .OR. IAGE .GT. NTROL(1)) then
            IER=1
            GO TO 900
      ENDIF
c                        If density dependent, have densities been provided?
      IF(IHTYPE .EQ. 2 .AND. IAGE .GT. LHIST+1) then
           IER=2
           GO TO 900
      ENDIF
c                                            Is height acceptable ?
      IF(HT_WANT1 .LE. HBASE) then
           IER=3
           GO TO 900
      ENDIF

      IF(IHTYPE .EQ. 1) THEN
          PSIGUESS = 0.0D0
          ITYPE = IHTYPE
          CALL DF_SOLV(IAGE, HT_WANT1, PSIGUESS, JER)
          IF(JER .GT. 0) THEN
              IER=4
              GO TO 900
          ENDIF
      ELSE

c         estimate (roughly) the density adjustment to height
          DENMULT = 0.0D0
          DO I = IAGEBASE, IAGE-1
             DENMULT = DENMULT + FMULT(I)
          ENDDO
          DENMULT = DENMULT / DBLE(IAGE - IAGEBASE)
          HT_WANTB = HBASE + (HT_WANT1 - HBASE)/DENMULT

c                  using that estimated base height, calculate psi
          ITYPE = 1
          PSIGUESS = 0.0D0
          CALL DF_SOLV(IAGE, HT_WANTB, PSIGUESS ,JER)
          IF(JER .GT. 0) THEN
              IER=4
              GO TO 900
          ENDIF
c               obtain an improved estimate of the density adjustment
          CALL DF_HADJ(IAGEBASE, IAGE)
          DENMULT = (HADJV(IAGE) - HBASE)/( HBASEV(IAGE) - HBASE)
c               using the result estimated base height, calculate psi
          PSIGUESS = PSI
          HT_WANTB = HBASE + (HT_WANT1 - HBASE)/DENMULT
          CALL DF_SOLV( IAGE, HT_WANTB, PSIGUESS, JER)
          IF(JER .GT. 0) THEN
              IER=4
              GO TO 900
          ENDIF
c             now that I have an almost exact answer, get the perfect psi
          ITYPE = 2
          PSIGUESS=PSI
          CALL DF_SOLV( IAGE, HT_WANT1, PSIGUESS, JER)
          IF(JER .GT. 0) THEN
              IER=5
              GO TO 900
           ENDIF
       ENDIF

      PSI_FEET = SNGL(PSI)
c                                                   Find site index
      IBAGE=30
      CALL DF_HN( IBAGE, SI)
      SI_R4 = SNGL( SI * HMULT)

900   RETURN
      END
C**********************************************************************
      SUBROUTINE DFSITE4(IAGE, HBASE_R4, HADJ_R4)
      IMPLICIT NONE
c
c      IAGE      integer  input    total age
c      HBASE_R4  real*4   output   base (unadjusted) height (ft or m)
c      HADJ_R4   real*4   output   Reserved for future use. (is set to zero now)

C      IMPLICIT REAL*8 (A-H, O-Z)
      INTEGER*2 IAGE
      REAL*4 HBASE_R4, HADJ_R4
      REAL*8 HT
      REAL*8 LAMDA1, LAMDA2
      REAL*8 PSI, B1, XK1, YK1, XK2, YK2, YPK, C, ALPHA, BETA
      REAL*8 HMULT,TMULT

      COMMON /DF_CONV / HMULT, TMULT
      COMMON /DF_CSPEC/  PSI, B1, XK1, YK1, XK2, YK2, YPK, C,
     1                   LAMDA1, LAMDA2, ALPHA, BETA

      HADJ_R4 = 0.0
      CALL DF_HN(IAGE, HT)
C      WRITE(20,1000) IAGE,HT
C 1000 FORMAT(' #2 ',I2,1X,F11.4)
      HBASE_R4 = SNGL(HT) * HMULT

      RETURN
      END
C**********************************************************************
      SUBROUTINE HG_SWO(ISPGRP,PHTGRO,CR,TCCH,HG)
      IMPLICIT NONE
      REAL*4 PHTGRO,CR,TCCH,HG,HGPAR(5,8),P1,P2,P3,P4,P5,P6,P7,P8,FCR,
     1       B0,B1,MODIFER,CRADJ
      INTEGER*4 ISPGRP
C
C  HEIGHT GROWTH PARAMETERS (8 parameters - big 5 conifers only)
C
C     DF Coefficients Fit Using Data From the SWO Douglas-fir Plots
C     GW Coefficients from Hann and Hanus (2002) FRL Research Contribution 41
C     PP Coefficients from Hann and Hanus (2002) FRL Research Contribution 41
C     SP Coefficients from Hann and Hanus (2002) FRL Research Contribution 41
C     IC Coefficients from Hann and Hanus (2002) FRL Research Contribution 41
C
      DATA HGPAR/
     1  1.0       ,  1.0       ,  1.0       ,  1.0       ,  1.0       ,! DF,GW,PP,SP,IC
C
     2 -0.02478678, -0.01453250, -0.14889850, -0.14889850, -0.01453250,! DF,GW,PP,SP,IC
C
     3 -0.00368965, -0.00407303, -0.00322752, -0.00678955, -0.00637434,! DF,GW,PP,SP,IC
C
     4  1.0       ,  1.0       ,  1.0       ,  1.0       ,  1.0       ,! DF,GW,PP,SP,IC
C
     5  2.23890423,  7.69023575,  0.92071847,  0.92071847,  1.27228638,! DF,GW,PP,SP,IC
C
     6  2.0       ,  2.0       ,  2.0       ,  2.0       ,  2.0       ,! DF,GW,PP,SP,IC
C
     7  0.0       ,  0.0       ,  0.0       ,  0.0       ,  0.0       ,! DF,GW,PP,SP,IC
C
     8  1.0       ,  1.0       ,  1.0       ,  1.0       ,  1.0/       ! DF,GW,PP,SP,IC
C
      P1=HGPAR(ISPGRP,1)
      P2=HGPAR(ISPGRP,2)
      P3=HGPAR(ISPGRP,3)
      P4=HGPAR(ISPGRP,4)
      P5=HGPAR(ISPGRP,5)
      P6=HGPAR(ISPGRP,6)
      P7=HGPAR(ISPGRP,7)
      P8=HGPAR(ISPGRP,8)
      FCR=(-P5*(1.0-CR)**P6)*EXP(P7*TCCH**0.5)
      B0=P1*EXP(P2*TCCH)
      B1=EXP(P3*TCCH**P4)
      MODIFER=P8*(B0+(B1-B0)*EXP(FCR))
      CRADJ=(1.0-EXP(-(25.0*CR)**2))
      HG=PHTGRO*MODIFER*CRADJ
      RETURN
      END
**********************************************************************
      SUBROUTINE HG_SMC(ISPGRP,PHTGRO,CR,TCCH,HG)
      IMPLICIT NONE
      REAL*4 PHTGRO,CR,TCCH,HG,HGPAR(3,8),P1,P2,P3,P4,P5,P6,P7,P8,FCR,
     1       B0,B1,MODIFER,CRADJ
      INTEGER*4 ISPGRP
C
C  HEIGHT GROWTH PARAMETERS (8 parameters - big 3 conifers only)
C
C     DF Coefficients Fit Using Data From the SMC Type I Control Plots
C     GF Coefficients from Ritchie and Hann (1990) FRL Research Paper 54
C     WH Coefficients from Johnson (2002) Willamette Industries Report
C
      DATA HGPAR/
     1           0.816519984 ,  1.0         ,  1.0         ,           !  DF,GF,WH
C
     2          -0.0091258738, -0.0328142   , -0.0384415   ,           !  DF,GF,WH
C
     3          -0.0454491642, -0.0127851   , -0.0144139   ,           !  DF,GF,WH
C
     4           0.5         ,  1.0         ,  0.5         ,           !  DF,GF,WH
C
     5           1.04147381  ,  6.19784     ,  1.04409     ,           !  DF,GF,WH
C
     6           2.0         ,  2.0         ,  2.0         ,           !  DF,GF,WH
C
     7           0.496019682 ,  0.0         ,  0.0         ,           !  DF,GF,WH
C
     8           1.00443936  ,  1.01        ,  1.03        /           !  DF,GF,WH
C
      P1=HGPAR(ISPGRP,1)
      P2=HGPAR(ISPGRP,2)
      P3=HGPAR(ISPGRP,3)
      P4=HGPAR(ISPGRP,4)
      P5=HGPAR(ISPGRP,5)
      P6=HGPAR(ISPGRP,6)
      P7=HGPAR(ISPGRP,7)
      P8=HGPAR(ISPGRP,8)
      FCR=(-P5*(1.0-CR)**P6)*EXP(P7*TCCH**0.5)
      B0=P1*EXP(P2*TCCH)
      B1=EXP(P3*TCCH**P4)
      MODIFER=P8*(B0+(B1-B0)*EXP(FCR))
      CRADJ=(1.0-EXP(-(25.0*CR)**2))
      HG=PHTGRO*MODIFER*CRADJ
      RETURN
      END
**********************************************************************
      SUBROUTINE LIMIT(VERSION,ISP,DBH,HT,DG,HG)
      IMPLICIT NONE
      REAL*4 DBH,HT,DG,HG,A0,A1,A2,HT1,HT2,HT3,DBH1,DBH2,DBH3,PHT1,PHT2,
     1       PHT3,PHGR1,PHGR2
      INTEGER*4 VERSION,ISP,JSP
      JSP=ISP
      IF(ISP .EQ. 263 .AND. VERSION .EQ. 1) JSP=2631
      SELECT CASE(JSP)
        CASE(202,263)
           A0=19.04942539
           A1=-0.04484724
           A2=1.0
        CASE(17,15)
           A0=16.26279948
           A1=-0.04484724
           A2=1.0
        CASE(122)
           A0=17.11482201
           A1=-0.04484724
           A2=1.0
        CASE(117)
           A0=14.29011403
           A1=-0.04484724
           A2=1.0
        CASE(351)
           A0=60.619859
           A1=-1.59138564
           A2=0.496705997
        CASE DEFAULT
           A0=15.80319194
           A1=-0.04484724
           A2=1.0
      ENDSELECT
      HT1=HT-4.5
      HT2=HT1+HG
      HT3=HT2+HG
      DBH1=DBH
      DBH2=DBH1+DG
      DBH3=DBH2+DG
      PHT1=A0*DBH1/(1.0-A1*DBH1**A2)
      PHT2=A0*DBH2/(1.0-A1*DBH2**A2)
      PHT3=A0*DBH3/(1.0-A1*DBH3**A2)
      PHGR1=(PHT2-PHT1+HG)/2.0
      PHGR2=PHT2-HT1
      IF(HT2.GT.PHT2) THEN
         IF(PHGR1.LT.PHGR2) THEN
            HG=PHGR1
         ELSE
            HG=PHGR2
         ENDIF
      ELSE IF(HT3.GT.PHT3) THEN
         HG=PHGR1
      ENDIF
      IF(HG.LT.0.0) THEN
         HG=0.0
      ENDIF
      RETURN
      END
**********************************************************************
      SUBROUTINE HD_SWO(ISPGRP,DBH,PRDHT)
      IMPLICIT NONE
      REAL*4 DBH,PRDHT,HDPAR(18,3),B0,B1,B2
      INTEGER*4 ISPGRP
C
C  NEW HEIGHT/DIAMETER PARAMETERS FOR UNDAMAGED TREES. EXCEPT RC, WO, AND RA (3 parameters - all species)
C
C     DF Coefficients from SWO Douglas-fir Plots
C     GW Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     PP Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     SP Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     IC Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     WH Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
C     PY Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     MD Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     GC Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     TA Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     CL Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     BL Coefficients Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
C     BO Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     RA Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #1
C     PD Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     WI Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C
      DATA HDPAR/
     1             6.35647231 ,  6.75286569,  6.27233557,  5.81876360, !  DF,GW,PP,SP
     1            10.04621768 ,  6.58804   ,  6.14817441,  5.10707208, !  IC,WH,RC,PY
     1             6.53558288 ,  9.2251518 ,  8.49655416,  9.01612971, !  MD,GC,TA,CL
     1             5.20018445 ,  4.69753118,  5.04832439,  5.59759126, !  BL,WO,BO,RA
     1             7.49095931 ,  3.26840527,                           !  PD,WI
C
     2            -4.79520392,  -5.52614439, -5.57306985, -5.31082668, !  DF,GW,PP,SP
     2            -8.72915115,  -5.25312496, -5.40092761, -3.28638769, !  IC,WH,RC,PY
     2            -4.69059053,  -7.65310387, -6.68904033, -7.34813829, !  MD,GC,TA,CL
     2            -2.86671078,  -3.51586969, -3.32715915, -3.19942952, !  BL,WO,BO,RA
     2            -5.40872209,  -0.95270859,                           !  PD,WI
C
     3            -0.35976041, -0.33012156, -0.40384171, -0.47349388, !  DF,GW,PP,SP
     3            -0.14040106,  -0.31895401, -0.38922036, -0.24016101, !  IC,WH,RC,PY
     3            -0.24934807,  -0.15480725, -0.16105112, -0.134025626,!  MD,GC,TA,CL
     3            -0.42255220,  -0.57665068, -0.43456034, -0.38783403, !  BL,WO,BO,RA
     3            -0.16874962,  -0.98015696/                           !  PD,WI
C
      B0=HDPAR(ISPGRP,1)
      B1=HDPAR(ISPGRP,2)
      B2=HDPAR(ISPGRP,3)
      PRDHT=4.5+EXP(B0+B1*DBH**B2)
      RETURN
      END
**********************************************************************
      SUBROUTINE HD_SMC(ISPGRP,DBH,PRDHT)
      IMPLICIT NONE
      REAL*4 DBH,PRDHT,HDPAR(11,3),B0,B1,B2
      INTEGER*4 ISPGRP
C
C  HEIGHT/DIAMETER PARAMETERS (3 parameters - all species)
C
C     DF Coefficients Fit Using Data From the SMC Type I Control Plots
C     GF Coefficients from Wang and Hann (1988) FRL Research Paper 51
C     WH Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
C     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
C     PY Coefficients from Wang and Hann (1988) FRL Research Paper 51
C     MD Coefficients from Wang and Hann (1988) FRL Research Paper 51
C     BL Coefficients from Wang and Hann (1988) FRL Research Paper 51
C     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
C     RA Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #1
C     PD Coefficients from Wang and Hann (1988) FRL Research Paper 51
C     WI Coefficients from Wang and Hann (1988) FRL Research Paper 51
C
      DATA HDPAR/
     1            12.0355642 ,  7.42808    ,  6.555344622,  6.14817441,!  DF,GF,WH,RC
     1            9.30172    ,  5.84487    ,  5.21462    ,  4.69753118,!  PY,MD,BL,WO
     1            5.59759126 ,  4.49727    ,  4.88361    ,             !  RA,PD,WI
C
     2           -10.6297694 , -5.80832    , -5.137174162, -5.40092761,!  DF,GF,WH,RC
     2           -7.50951    , -3.84795    , -2.70252    , -3.51586969,!  PY,MD,BL,WO
     2           -3.19942952 , -2.07667    , -2.47605    ,             !  RA,PD,WI
C
     3           -0.126993368, -0.240317   , -0.364550800, -0.38922036,!  DF,GF,WH,RC
     3           -0.100000   ,  -0.289213  , -0.354756   , -0.57665068,!  PY,MD,BL,WO
     3           -0.38783403 ,  -0.388650  , -0.309050  /              !  RA,PD,WI
C
      B0=HDPAR(ISPGRP,1)
      B1=HDPAR(ISPGRP,2)
      B2=HDPAR(ISPGRP,3)
      PRDHT=4.5+EXP(B0+B1*DBH**B2)
      RETURN
      END
**********************************************************************
      SUBROUTINE HG_FERT(CYCLG,VERSION,ISPGRP,SITE,PN,YF,FERTADJ)
      IMPLICIT NONE
      REAL*4 SITE,PN(25),YF(25),FERTADJ,PF1,PF2,PF3,PF4,PF5,FALDWN,
     1       XTIME,FERTX1,FERTX2,ANNUAL
      INTEGER*4 CYCLG,VERSION,ISPGRP,I
      IF(VERSION .LE. 3) THEN
         IF(ISPGRP .EQ. 1)THEN
            PF1=1.0
            PF2=0.333333333
            PF3=-1.107409443
            PF4=-2.133334346
            PF5=1.5
            ANNUAL=3.1836
         ELSE
            PF1=0.0
            PF2=1.0
            PF3=0.0
            PF4=0.0
            PF5=1.0
            ANNUAL=0.0
         ENDIF
      ELSE
         PF1=0.0
         PF2=1.0
         PF3=0.0
         PF4=0.0
         PF5=1.0
         ANNUAL=0.0
      ENDIF
      FALDWN=1.0
      XTIME=FLOAT(CYCLG)
      FERTX1=0.0
      DO I=2,5
         FERTX1=FERTX1+(PN(I)/800.0)*EXP((PF3/PF2)*(YF(1)-YF(I)))
      ENDDO
      FERTX2=EXP(PF3*(XTIME-YF(1))+PF4*(SITE/100.0)**PF5)
      FERTADJ=1.0+(ANNUAL*PF1*((PN(1)/800.0)+FERTX1)**PF2*FERTX2)*FALDWN
      RETURN
      END

**********************************************************************
      SUBROUTINE HG_THIN(CYCLG,VERSION,ISPGRP,BABT,BART,YT,THINADJ)
      IMPLICIT NONE
      REAL*4 BABT,BART(25),YT(25),THINADJ,PT1,PT2,PT3,XTIME,THINX1,
     1       THINX2,THINX3,PREM,ANNUAL
      INTEGER*4 CYCLG,VERSION,ISPGRP,I
      IF(VERSION .LE. 3) THEN
         IF(ISPGRP .EQ. 1)THEN
            PT1=-0.3197415492
            PT2=0.7528887377
            PT3=-0.2268800162
            ANNUAL=1.4963
         ELSE
            PT1=0.0
            PT2=1.0
            PT3=0.0
            ANNUAL=0.0
         ENDIF
      ELSE
         IF(ISPGRP .EQ. 1)THEN
            PT1=-0.613313694
            PT2=1.0
            PT3=-0.443824038
            ANNUAL=1.0
         ELSEIF(ISPGRP .EQ. 2)THEN
            PT1=-0.3197415492
            PT2=0.7528887377
            PT3=-0.2268800162
            ANNUAL=1.4963
         ELSE
            PT1=0.0
            PT2=1.0
            PT3=0.0
            ANNUAL=0.0
         ENDIF
      ENDIF
      XTIME=FLOAT(CYCLG)
      THINX1=0.0
      DO I=2,25
         THINX1=THINX1+BART(I)*EXP((PT3/PT2)*(YT(1)-YT(I)))
      ENDDO
      THINX2=THINX1+BART(1)
      THINX3=THINX1+BABT
      IF(THINX3 .LE. 0.0) THEN
         PREM=0.0
      ELSE
         PREM=THINX2/THINX3
      ENDIF
      IF(PREM .GT. 0.75) PREM=0.75
      THINADJ=1.0+ANNUAL*PT1*PREM**PT2*EXP(PT3*(XTIME-YT(1)))
      RETURN
      END
C***********************************************************************
      SUBROUTINE RAGEA(H,SI,GEA)
C
C     RED ALDER GROWTH EFFECTIVE AGE EQUATION BASED ON H40 EQUATION FROM
C         WORTHINGTON, JOHNSON, STAEBLER AND LLOYD (1960) PNW RESEARCH
C         PAPER 36
C
      IMPLICIT NONE
      REAL*4 H,SI,GEA
      GEA=19.538*H/(SI-0.60924*H)
      RETURN
      END
C***********************************************************************
      SUBROUTINE RAH40(A,SI,H)
C
C     RED ALDER H40 EQUATION FROM FROM WORTHINGTON, JOHNSON, STAEBLER
C         AND LLOYD (1960) PNW RESEARCH PAPER 36
C
      IMPLICIT NONE
      REAL*4 A,SI,H
      H=SI/(0.60924+19.538/A)
      RETURN
      END
C**********************************************************************
      SUBROUTINE SWO_DF_HINC(DBH,CR,WHC20,PPTDD41,BAL,BA,HINC)
      IMPLICIT NONE
      REAL*4 DBH,CR,WHC20,PPTDD41,BAL,BA,HINC
      REAL*4 X1,X2,X3,X4,X5,X6,X7
      REAL*4 B0,B1,B2,B3,B4,B5,B6
      B0=-0.233701804
      B1=3.88956899
      B2=-4.51487556
      B3=0.981582755
      B4=1.45452035
      B5=1.103427
      B6=-4.55192092
      X1=LOG(DBH)/10.0
      X2=DBH/100.0
      X3=LOG((CR+0.2)/1.2)
      X4=WHC20/10.0
      X5=PPTDD41/100.0
      X6=BAL/(1000.0*LOG(DBH+2.72))
      X7=SQRT(BA)/100.0
      HINC=EXP(B0+B1*X1+B2*X2+B3*X3+B4*X4+B5*X5+B6*X6)
      RETURN
      END
C**********************************************************************
      SUBROUTINE SMC_DF_HINC(DBH,CR,WHC20,PPTDD41,BAL,BA,HINC)
      IMPLICIT NONE
      REAL*4 DBH,CR,WHC20,PPTDD41,BAL,BA,HINC
      REAL*4 X1,X2,X3,X4,X5,X6,X7
      REAL*4 B0,B1,B2,B3,B4,B5,B6
      B0=-0.233701804
      B1=3.88956899
      B2=-4.51487556
      B3=0.981582755
      B4=1.45452035
      B5=1.103427
      B6=-4.55192092
      X1=LOG(DBH)/10.0
      X2=DBH/100.0
      X3=LOG((CR+0.2)/1.2)
      X4=WHC20/10.0
      X5=PPTDD41/100.0
      X6=BAL/(1000.0*LOG(DBH+2.72))
      X7=SQRT(BA)/100.0
      HINC=EXP(B0+B1*X1+B2*X2+B3*X3+B4*X4+B5*X5+B6*X6)
      RETURN
      END
