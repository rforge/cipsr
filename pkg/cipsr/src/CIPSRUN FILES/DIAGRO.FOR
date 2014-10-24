C     CIPSANON GROWTH AND YIELD MODEL
C     SUBROUTINES INCLUDED:
C         DIAMGRO
C         DG_SWO
C         DG_SMC
C         DG_THIN
C         DG_FERT
C         GET_BAL
C         SWO_DF_DINC
C         SMC_DF_DINC
********************************************************************************
      SUBROUTINE DIAMGRO(VERSION,K,CYCLG,TDATAI,TDATAR,WHC20,PPTDD41,
     1           SI_1,SI_2,SI_3,DFSQ,SBA1,BALL1,BAL1,CALIB,PN,YF,BABT,
     2           BART,YT,GROWTH)
      IMPLICIT NONE
C
C     CALCULATES ONE-YEAR DIAMETER GROWTH RATE OF THE K-TH TREE
C
      INTEGER*4 VERSION,K,CYCLG,TDATAI(2000,3),I,ISP,ISPGRP,DFSQ
      REAL*4 TDATAR(2000,8),WHC20,PPTDD41,SI_1,SI_2,SI_3,SBA1,BALL1(51),
     1       BAL1(500),CALIB(6,18),PN(25),YF(25),BABT,BART(25),YT(25),
     2       GROWTH(2000,4),DGRO,DBH,CR,DG,SBAL1,FERTADJ,THINADJ,SITE
C      INTEGER*4 IANS,IYN
C
C     CALCULATE BASAL AREA IN LARGER TREES
C
      DBH=TDATAR(K,1)
      CR=TDATAR(K,3)
      ISP=TDATAI(K,1)
      ISPGRP=TDATAI(K,2)
      CALL GET_BAL(DBH,BALL1,BAL1,SBAL1)
      SELECT CASE(VERSION)
         CASE(1)
            SITE=SI_1
         CASE(2)
            IF(ISP .EQ. 202) THEN
               SITE=SI_1
            ELSEIF(ISP .EQ. 263) THEN
               SITE=SI_2
            ELSE
               SITE=SI_3
            ENDIF
      END SELECT
C
C     CALCULATE DIAMETER GROWTH RATE FOR UNTREATED TREES
C
      SELECT CASE(VERSION)
         CASE(1)
            IF(ISPGRP .LE. 1 .AND. DFSQ .EQ. 1) THEN
               CALL SWO_DF_DINC(DBH,CR,WHC20,PPTDD41,SBAL1,SBA1,DG)
            ELSE
               CALL DG_SWO(ISPGRP,DBH,CR,SITE,SBAL1,SBA1,DG)
            ENDIF
         CASE(2)
            IF(ISPGRP .LE. 1 .AND. DFSQ .EQ. 1) THEN
               CALL SMC_DF_DINC(DBH,CR,WHC20,PPTDD41,SBAL1,SBA1,DG)
            ELSE
               CALL DG_SMC(ISPGRP,DBH,CR,SITE,SBAL1,SBA1,DG)
            ENDIF
      END SELECT
C
C     CALCULATE FERTILIZER ADJUSTMENT
C
      CALL DG_FERT(ISP,VERSION,CYCLG,SI_3,PN,YF,FERTADJ)
C
C     CALCULATE THINNING ADJUSTMENT
C
      CALL DG_THIN(ISP,VERSION,CYCLG,BABT,BART,YT,THINADJ)
C
C     CALCULATE DIAMETER GROWTH RATE FOR UNTREATED OR TREATED TREES
C
      DGRO=DG*CALIB(3,ISPGRP)*FERTADJ*THINADJ
      GROWTH(K,2)=DGRO
      RETURN
      END
*******************************************************************************
      SUBROUTINE DG_SWO(ISPGRP,DBH,CR,SITE,SBAL1,SBA1,DG)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 B0,B1,B2,B3,B4,B5,B6,K1,K2,K3,K4,DBH,CR,SBAL1,SBA1,LNDG,
     1       SITE,DG,DGPAR(18,11),CRADJ,ADJ,AGP
C
C  DIAMETER GROWTH PARAMETERS FOR SOUTHWEST OREGON (11 parameters - all species)
C
C     DF Coefficients Fit Using Data From the SWO Young Douglas-fir Plots
C     GW Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     PP Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     SP Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     IC Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     WH Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
C     PY Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     MD Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     GC Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     TA Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     CL Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     BL Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
C     BO Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     RA Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #1
C     PD Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     WI Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C
      DATA DGPAR/
     1             -6.5793441 ,     -5.84904111,     -4.51958940,      !  DF,GW,PP
     1             -4.12342552,     -2.08551255,     -5.70052255,      !  SP,IC,WH
     1            -11.45456097,     -9.15835863,     -8.84531757,      !  RC,PY,MD
     1             -7.78451344,     -3.36821750,     -3.59333060,      !  GC,TA,CL
     1             -3.41449922,     -7.81267986,     -4.43438109,      !  BL,WO,BO
     1             -4.39082007,     -8.08352683,     -8.08352683,      !  RA,PD,WI
C
     2              0.640812093,     1.668196109,     0.813998712,     !  DF,GW,PP
     2              0.734988422,     0.596043703,     0.865087036,     !  SP,IC,WH
     2              0.784133664,     1.0        ,     1.5        ,     !  RC,PY,MD
     2              1.2        ,     1.2        ,     1.2        ,     !  GC,TA,CL
     2              1.0        ,     1.405616529,     0.930930363,     !  BL,WO,BO
     2              1.0        ,     1.0        ,     1.0        ,     !  RA,PD,WI
C
     3             -0.0370315153,   -0.0853271265,   -0.0493858858,    !  DF,GW,PP
     3             -0.0425469735,   -0.0215223077,   -0.0432543518,    !  SP,IC,WH
     3             -0.0261377888,   -0.00000035  ,   -0.0006      ,    !  RC,PY,MD
     3             -0.07        ,   -0.07        ,   -0.07        ,    !  GC,TA,CL
     3             -0.05        ,   -0.0603105850,   -0.0465947242,    !  BL,WO,BO
     3             -0.0945057147,   -0.00000035  ,   -0.00000035  ,    !  RA,PD,WI
C
     4              0.987893185 ,    1.21222176  ,    1.10249641  ,    !  DF,GW,PP
     4              1.05942163  ,    1.02734556  ,    1.10859727  ,    !  SP,IC,WH
     4              0.70174783  ,    1.16688474  ,    0.51225596  ,    !  RC,PY,MD
     4              0.0         ,    0.0         ,    0.51637418  ,    !  GC,TA,CL
     4              0.0         ,    0.64286007  ,    0.0         ,    !  BL,WO,BO
     4              1.06867026  ,    0.31176647  ,    0.31176647  ,    !  RA,PD,WI
C
     5              1.05379203  ,    0.679346647 ,    0.879440023 ,    !  DF,GW,PP
     5              0.808656390 ,    0.383450822 ,    0.977332597 ,    !  SP,IC,WH
     5              2.057236260 ,    0.0         ,    0.418129153 ,    !  RC,PY,MD
     5              1.01436101  ,    0.0         ,    0.0         ,    !  GC,TA,CL
     5              0.324349277 ,    1.037687142 ,    0.510717175 ,    !  BL,WO,BO
     5              0.685908029 ,    0.0         ,    0.0         ,    !  RA,PD,WI
C
     6             -0.0103442484,  -0.00809965733,  -0.0108521667,    !  DF,GW,PP
     6             -0.0107837565 ,  -0.00489046624,   0.0         ,    !  SP,IC,WH
     6             -0.00415440257,   0.0          ,  -0.00355254593,   !  RC,PY,MD
     6             -0.00834323811,   0.0          ,   0.0          ,   !  GC,TA,CL
     6              0.0          ,   0.0          ,   0.0          ,   !  BL,WO,BO
     6             -0.00586331028,   0.0          ,   0.0          ,   !  RA,PD,WI
C
     7             -0.0128574067,    0.0         ,   -0.0333706948,    !  DF,GW,PP
     7              0.0         ,   -0.0609024782,   -0.0526263229,    !  SP,IC,WH
     7              0.0         ,   -0.02        ,   -0.0321315389,    !  RC,PY,MD
     7              0.0         ,   -0.0339813575,   -0.02        ,    !  GC,TA,CL
     7             -0.0989519477,   -0.0787012218,   -0.0688832423,    !  BL,WO,BO
     7              0.0         ,   -0.0730788052,   -0.0730788052,    !  RA,PD,WI
C
     8              0.9         ,    5.0         ,    5.0         ,    !  DF,GW,PP
     8              5.0         ,    5.0         ,    5.0         ,    !  SP,IC,WH
     8              5.0         , 4000.0         ,  110.0         ,    !  RC,PY,MD
     8             10.0         ,   10.0         ,   10.0         ,    !  GC,TA,CL
     8             10.0         ,    5.0         ,    5.0         ,    !  BL,WO,BO
     8              5.0         , 4000.0         , 4000.0         ,    !  RA,PD,WI
C
     9              1.0         ,    1.0         ,    1.0         ,    !  DF,GW,PP
     9              1.0         ,    1.0         ,    1.0         ,    !  SP,IC,WH
     9              1.0         ,    4.0         ,    2.0         ,    !  RC,PY,MD
     9              1.0         ,    1.0         ,    1.0         ,    !  GC,TA,CL
     9              1.0         ,    1.0         ,    1.0         ,    !  BL,WO,BO
     9              1.0         ,    4.0         ,    4.0         ,    !  RA,PD,WI
C
     X              1.0         ,    1.0         ,    1.0         ,    !  DF,GW,PP
     X              1.0         ,    1.0         ,    1.0         ,    !  SP,IC,WH
     X              1.0         ,    1.0         ,    1.0         ,    !  RC,PY,MD
     X              1.0         ,    1.0         ,    1.0         ,    !  GC,TA,CL
     X              1.0         ,    1.0         ,    1.0         ,    !  BL,WO,BO
     X              1.0         ,    1.0         ,    1.0         ,    !  RA,PD,WI
C
     A              2.72        ,    2.7         ,    2.7         ,    !  DF,GW,PP
     A              2.7         ,    2.7         ,    2.7         ,    !  SP,IC,WH
     A              2.7         ,    2.7         ,    2.7         ,    !  RC,PY,MD
     A              2.7         ,    2.7         ,    2.7         ,    !  GC,TA,CL
     A              2.7         ,    2.7         ,    2.7         ,    !  BL,WO,BO
     A              2.7         ,    2.7         ,    2.7/             !  RA,PD,WI
C
      IF(ISPGRP .LE. 1) THEN
         AGP=0.0
      ELSE
         AGP=LOG(0.2)
      ENDIF
      B0=DGPAR(ISPGRP,1)
      B1=DGPAR(ISPGRP,2)
      B2=DGPAR(ISPGRP,3)
      B3=DGPAR(ISPGRP,4)
      B4=DGPAR(ISPGRP,5)
      B5=DGPAR(ISPGRP,6)
      B6=DGPAR(ISPGRP,7)
      K1=DGPAR(ISPGRP,8)
      K2=DGPAR(ISPGRP,9)
      K3=DGPAR(ISPGRP,10)
      K4=DGPAR(ISPGRP,11)
      LNDG=AGP+B0
     1    +B1*LOG(DBH+K1)
     2    +B2*DBH**K2
     3    +B3*LOG((CR+0.2)/1.2)
     4    +B4*LOG(SITE)
     5    +B5*((SBAL1**K3)/LOG(DBH+K4))
     6    +B6*SQRT(SBA1)
C
C     CROWN RATIO ADJUSTMENT
C
      CRADJ=1.0-EXP(-(25.0*CR)**2)
C
C      FULL NEW ADJUSTMENTS
C
      IF(ISPGRP .EQ. 1) THEN
         ADJ=0.8938
      ELSEIF(ISPGRP .EQ. 2) THEN
         ADJ=0.8722
      ELSEIF(ISPGRP .EQ. 4) THEN
         ADJ=0.7903
      ELSEIF(ISPGRP .EQ. 9) THEN
         ADJ=0.7928
      ELSEIF(ISPGRP .EQ. 10) THEN
         ADJ=0.7259
      ELSEIF(ISPGRP .EQ. 14) THEN
         ADJ=1.0
      ELSEIF(ISPGRP .EQ. 15) THEN
         ADJ=0.7667
      ELSE
         ADJ=0.8
      ENDIF
      DG=EXP(LNDG)*CRADJ*ADJ
      RETURN
      END
*******************************************************************************
      SUBROUTINE DG_SMC(ISPGRP,DBH,CR,SITE,SBAL1,SBA1,DG)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 B0,B1,B2,B3,B4,B5,B6,K1,K2,K3,K4,DBH,CR,SBAL1,SBA1,LNDG,
     1       SITE,DG,DGPAR(11,11),CRADJ,ADJ,AGP
C
C  DIAMETER GROWTH PARAMETERS (11 parameters - all species)
C
C     DF Coefficients Fit Using Data From the SMC Type I Control Plots
C     GF Coefficients from Zumrawi and Hann (1993) FRL Research Contribution 4
C     WH Coefficients from Unpublished Equation on File at OSU Dept. Forest Resources
C     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
C     PY Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     MD Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     BL Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
C     RA Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #1
C     PD Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C     WI Coefficients from Hann and Hanus (2002) FRL Research Contribution 39
C
      DATA DGPAR/
     1          -5.08054564   ,     -2.34619      ,     -4.87447412   ,! DF,GF,WH
     1         -11.45456097   ,     -9.15835863   ,     -8.84531757   ,! RC,PY,MD
     1          -3.41449922   ,     -7.81267986   ,     -4.39082007   ,! BL,WO,RA
     1          -8.08352683   ,     -8.08352683   ,                    ! PD,WI
C
     2           0.505519518  ,      0.594640     ,      0.4150723209 ,! DF,GF,WH
     2           0.784133664  ,      1.0          ,      1.5          ,! RC,PY,MD
     2           1.0          ,      1.405616529  ,      1.0          ,! BL,WO,RA
     2           1.0          ,      1.0          ,                    ! PD,WI
C
     3          -0.0129701775 ,     -0.000976092  ,     -0.023744997  ,! DF,GF,WH
     3          -0.0261377888 ,     -0.00000035   ,     -0.0006       ,! RC,PY,MD
     3          -0.05         ,     -0.0603105850 ,     -0.0945057147 ,! BL,WO,RA
     3          -0.00000035   ,     -0.00000035   ,                    ! PD,WI
C
     4           1.49898858   ,      1.12712      ,      0.907837299  ,! DF,GF,WH
     4           0.70174783   ,      1.16688474   ,      0.51225596   ,! RC,PY,MD
     4           0.0          ,      0.64286007   ,      1.06867026   ,! BL,WO,RA
     4           0.31176647   ,      0.31176647   ,                    ! PD,WI
C
     5           0.994610663  ,      0.555333     ,      1.1346766989 ,! DF,GF,WH
     5           2.057236260  ,      0.0          ,      0.418129153  ,! RC,PY,MD
     5           0.324349277  ,      1.037687142  ,      0.685908029  ,! BL,WO,RA
     5           0.0          ,      0.0          ,                    ! PD,WI
C
     6          -0.0105805406 ,     -0.0000290672 ,     -0.015333503  ,! DF,GF,WH
     6          -0.00415440257,      0.0          ,     -0.00355254593,! RC,PY,MD
     6           0.0          ,      0.0          ,     -0.00586331028,! BL,WO,RA
     6           0.0          ,      0.0          ,                    ! PD,WI
C
     7          -0.067026185  ,     -0.0470848    ,     -0.03309787   ,! DF,GF,WH
     7           0.0          ,     -0.02         ,     -0.0321315389 ,! RC,PY,MD
     7          -0.0989519477 ,     -0.0787012218 ,      0.0          ,! BL,WO,RA
     7          -0.0730788052 ,     -0.0730788052 ,                    ! PD,WI
C
     8           1.0          ,      1.0          ,      5.0          ,! DF,GF,WH
     8           5.0          ,   4000.0          ,    110.0          ,! RC,PY,MD
     8          10.0          ,      5.0          ,      5.0          ,! BL,WO,RA
     8        4000.0          ,   4000.0          ,                    ! PD,WI
C
     9           1.0          ,      2.0          ,      1.0          ,! DF,GF,WH
     9           1.0          ,      4.0          ,      2.0          ,! RC,PY,MD
     9           1.0          ,      1.0          ,      1.0          ,! BL,WO,RA
     9           4.0          ,      4.0          ,                    ! PD,WI
C
     X           1.0          ,      2.0          ,      1.0          ,! DF,GF,WH
     X           1.0          ,      1.0          ,      1.0          ,! RC,PY,MD
     X           1.0          ,      1.0          ,      1.0          ,! BL,WO,RA
     X           1.0          ,      1.0          ,                    ! PD,WI
C
     A           2.72         ,      5.0          ,      2.7          ,! DF,GF,WH
     A           2.7          ,      2.7          ,      2.7          ,! RC,PY,MD
     A           2.7          ,      2.7          ,      2.7          ,! BL,WO,RA
     A           2.7          ,      2.7/                              ! PD,WI
C
      IF(ISPGRP .LE. 1) THEN
         AGP=0.0
      ELSE
         AGP=LOG(0.2)
      ENDIF
      B0=DGPAR(ISPGRP,1)
      B1=DGPAR(ISPGRP,2)
      B2=DGPAR(ISPGRP,3)
      B3=DGPAR(ISPGRP,4)
      B4=DGPAR(ISPGRP,5)
      B5=DGPAR(ISPGRP,6)
      B6=DGPAR(ISPGRP,7)
      K1=DGPAR(ISPGRP,8)
      K2=DGPAR(ISPGRP,9)
      K3=DGPAR(ISPGRP,10)
      K4=DGPAR(ISPGRP,11)
      LNDG=AGP+B0
     1    +B1*LOG(DBH+K1)
     2    +B2*DBH**K2
     3    +B3*LOG((CR+0.2)/1.2)
     4    +B4*LOG(SITE)
     5    +B5*((SBAL1**K3)/LOG(DBH+K4))
     6    +B6*SQRT(SBA1)
C
C     CROWN RATIO ADJUSTMENT
C
      CRADJ=1.0-EXP(-(25.0*CR)**2)
      IF(ISPGRP .EQ. 1) THEN
         ADJ=1.0
      ELSEIF(ISPGRP .EQ. 2) THEN
         ADJ=0.8722
      ELSEIF(ISPGRP .EQ. 3) THEN
         ADJ=1.0
      ELSEIF(ISPGRP .EQ. 6) THEN
         ADJ=0.7928
C      ELSEIF(ISPGRP .EQ. 8) THEN
C         ADJ=0.7608
      ELSEIF(ISPGRP .EQ. 8) THEN
         ADJ=1.0
      ELSE
         ADJ=0.8
      ENDIF
      DG=EXP(LNDG)*CRADJ*ADJ
      RETURN
      END
*******************************************************************************
      SUBROUTINE DG_THIN(ISP,VERSION,CYCLG,BABT,BART,YT,THINADJ)
C
C     CALCULATE THINNING ADJUSTMENT FOR DIAMETER GROWTH RATE FROM
C     HANN ET AL.(2003) FRL RESEARCH CONTRIBUTION 40
C
      IMPLICIT NONE
      REAL*4 BABT,BART(25),YT(25),THINADJ,PT1,PT2,PT3,XTIME,THINX1,
     1       THINX2,THINX3,PREM,ANNUAL
      INTEGER*4 ISP,VERSION,CYCLG,I
C      INTEGER*4 IANS,IYN,K
C
C     SET PARAMETERS FOR ADJUSTMENT
C
      IF(ISP .EQ. 263)THEN
         PT1=0.723095045
         PT2=1.0
         PT3=-0.2644085320
         ANNUAL=1.2655
      ELSEIF(ISP .EQ. 202)THEN
         PT1=0.6203827985
         PT2=1.0
         PT3=-0.2644085320
         ANNUAL=1.2655
      ELSEIF(VERSION .EQ. 4 .AND. ISP .EQ. 351)THEN
         PT1=0.0
         PT2=1.0
         PT3=0.0
         ANNUAL=0.0
      ELSE
         PT1=0.6203827985
         PT2=1.0
         PT3=-0.2644085320
         ANNUAL=1.2655
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
*******************************************************************************
      SUBROUTINE DG_FERT(ISP,VERSION,CYCLG,SI_1,PN,YF,FERTADJ)
C
C     CALCULATE FERTILIZER ADJUSTMENT FOR DIAMETER GROWTH RATE
C     FROM HANN ET AL.(2003) FRL RESEARCH CONTRIBUTION 40
C
      IMPLICIT NONE
      REAL*4 SI_1,PN(25),YF(25),FERTADJ,PF1,PF2,PF3,PF4,PF5,FALDWN,
     1       XTIME,FERTX1,FERTX2,ANNUAL
      INTEGER*4 ISP,VERSION,CYCLG,I
C
C     SET PARAMETERS FOR ADJUSTMENT
C
      IF(VERSION .LE. 3) THEN
         IF(ISP .EQ. 263)THEN
            PF1=0.0
            PF2=1.0
            PF3=0.0
            PF4=0.0
            PF5=1.0
            ANNUAL=0.0
        ELSEIF(ISP .EQ. 202)THEN
            PF1=1.368661121
            PF2=0.741476964
            PF3=-0.214741684
            PF4=-0.851736558
            PF5=2.0
            ANNUAL=1.4619
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
      DO I=2,25
         FERTX1=FERTX1+(PN(I)/800.0)*EXP((PF3/PF2)*(YF(1)-YF(I)))
      ENDDO
      FERTX2=EXP(PF3*(XTIME-YF(1))+PF4*(SI_1/100.0)**PF5)
      FERTADJ=1.0+(ANNUAL*PF1*((PN(1)/800.0)+FERTX1)**PF2*FERTX2)*FALDWN
      RETURN
      END
*******************************************************************************
      SUBROUTINE GET_BAL(DBH,BALL1,BAL1,BAL)
      IMPLICIT NONE
      INTEGER*4 K
      REAL*4 DBH,BALL1(51),BAL1(500),BAL
      IF(DBH .GT. 100.0) THEN
         BAL=0.0
      ELSEIF(DBH .GT. 50.0)THEN
         K=DBH-49.0
         BAL=BALL1(K)
      ELSE
         K=DBH*10.0+0.5
         BAL=BAL1(K)
      ENDIF
      RETURN
      END
C**********************************************************************
      SUBROUTINE SWO_DF_DINC(DBH,CR,WHC20,PPTDD41,BAL,BA,DINC)
      IMPLICIT NONE
      REAL*4 DBH,CR,WHC20,PPTDD41,BAL,BA,DINC
      REAL*4 X1,X2,X3,X4,X5,X6,X7
      REAL*4 B0,B1,B2,B3,B4,B5,B6,B7
      B0=-2.44336034
      B1=5.74581154
      B2=-3.22285717
      B3=1.1102687
      B4=1.19902765
      B5=1.04310573
      B6=-10.2380938
      B7=-0.849057285
      X1=LOG(DBH+1.6)/10.0
      X2=DBH/100.0
      X3=LOG((CR+0.2)/1.2)
      X4=WHC20/10.0
      X5=PPTDD41/100.0
      X6=BAL/(1000.0*LOG(DBH+2.72))
      X7=SQRT(BA)/100.0
      DINC=EXP(B0+B1*X1+B2*X2+B3*X3+B4*X4+B5*X5+B6*X6+B7*X7)
      RETURN
      END
C**********************************************************************
      SUBROUTINE SMC_DF_DINC(DBH,CR,WHC20,PPTDD41,BAL,BA,DINC)
      IMPLICIT NONE
      REAL*4 DBH,CR,WHC20,PPTDD41,BAL,BA,DINC
      REAL*4 X1,X2,X3,X4,X5,X6,X7
      REAL*4 B0,B1,B2,B3,B4,B5,B6,B7
      B0=-2.44336034
      B1=5.74581154
      B2=-3.22285717
      B3=1.1102687
      B4=1.19902765
      B5=1.04310573
      B6=-10.2380938
      B7=-0.849057285
      X1=LOG(DBH+1.6)/10.0
      X2=DBH/100.0
      X3=LOG((CR+0.2)/1.2)
      X4=WHC20/10.0
      X5=PPTDD41/100.0
      X6=BAL/(1000.0*LOG(DBH+2.72))
      X7=SQRT(BA)/100.0
      DINC=EXP(B0+B1*X1+B2*X2+B3*X3+B4*X4+B5*X5+B6*X6+B7*X7)
      RETURN
      END
