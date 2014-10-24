C     CIPSANON GROWTH AND YIELD MODEL
C     SUBROUTINES INCLUDED:
C         DG_SWO
C         DG_SMC
C         PM_SWO
C         PM_SMC
C         HD_NWO
C         HD_SMC
C         HCB_SMC
C         MCW_SWO
C         MCW_SMC
C         SWO_DF_DINC
C         SMC_DF_DINC
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
C**********************************************************************
      SUBROUTINE PM_SWO(ISPGRP,DBH,CR,SI_1,BAL,OG,POW,PM)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 DBH,CR,SI_1,BAL,OG,POW,PM,B0,B1,B2,B3,B4,B5,B6,B7,
     1       MPAR(18,9)
C
C  NEW SWO MORTALITY WITH REVISED CLO PARAMETERS (8 parameters - all species)
C
C     DF Coefficients Fit Using Data From the SWO Young Douglas-fir Plots
C     GW Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     PP Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     SP Coefficients from Unpublished Equation on File at OSU Dept. Forest Resources
C     IC Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WH Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     RC Coefficients from WH of Hann, Marshall, Hanus (2003) FRL Research Contribution 40
C     PY Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     MD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     GC Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     TA Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     CL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
C     BO Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     RA Coefficients from Best Guess
C     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WI Coefficients from Best Guess
C
      DATA MPAR/
     1         -5.268279491, -2.215777201, -1.050000682, -1.531051304, !  DF,GW,PP,SP
     1         -1.922689902, -1.166211991, -0.761609   , -4.072781265, !  IC,WH,RC,PY
     1         -6.089598985, -4.317549852, -2.410756914, -2.990451960, !  MD,GC,TA,CL
     1         -2.976822456, -6.00031085 , -3.108619921, -2.0        , !  BL,WO,BO,RA
     1         -3.020345211, -1.386294361,                             !  PD,WI
C
     2         -.2338213369, -0.162895666, -0.194363402,  0.0        , !  DF,GW,PP,SP
     2         -0.136081990,  0.0        , -0.529366   , -0.176433475, !  IC,WH,RC,PY
     2         -0.245615070, -0.057696253,  0.0        ,  0.0        , !  MD,GC,TA,CL
     2          0.0        , -0.10490823 , -0.570366764, -0.5        , !  BL,WO,BO,RA
     2          0.0        ,  0.0        ,                             !  PD,WI
C
     3          0.0        ,  0.003317290,  0.003803100,  0.0        , !  DF,GW,PP,SP
     3          0.002479863,  0.0        ,  0.0        ,  0.0        , !  IC,WH,RC,PY
     3          0.0        ,  0.0        ,  0.0        ,  0.0        , !  MD,GC,TA,CL
     3          0.0        ,  0.0        ,  0.018205398,  0.015      , !  BL,WO,BO,RA
     3          0.0        ,  0.0        ,                             !  PD,WI
C
     4         -4.843342580, -3.561438261, -3.557300286,  0.0        , !  DF,GW,PP,SP
     4         -3.178123293, -4.602668157, -4.74019    , -1.729453975, !  IC,WH,RC,PY
     4         -3.208265570,  0.0        , -1.049353753,  0.0        , !  MD,GC,TA,CL
     4         -6.223250962, -0.99541909 , -4.584655216, -3.0        , !  BL,WO,BO,RA
     4         -8.467882343,  0.0        ,                             !  PD,WI
C
     5          0.029376827,  0.014644689,  0.003971638,  0.0        , !  DF,GW,PP,SP
     5          0.0        ,  0.0        ,  0.0119587  ,  0.0        , !  IC,WH,RC,PY
     5          0.033348079,  0.004861355,  0.008845583,  0.0        , !  MD,GC,TA,CL
     5          0.0        ,  0.00912739 ,  0.014926170,  0.015      , !  BL,WO,BO,RA
     5          0.013966388,  0.0        ,                             !  PD,WI
C
     6          0.007211610,  0.0        ,  0.005573601,  0.0        , !  DF,GW,PP,SP
     6          0.004684133,  0.0        ,  0.00756365 ,  0.012525642, !  IC,WH,RC,PY
     6          0.013571319,  0.00998129 ,  0.0        ,  0.002884840, !  MD,GC,TA,CL
     6          0.0        ,  0.87115652 ,  0.012419026,  0.01       , !  BL,WO,BO,RA
     6          0.009461545,  0.0        ,                             !  PD,WI

C
     7          0.0        ,  0.0        ,  0.0        ,  0.0        , !  DF,GW,PP,SP
     7          0.0        ,  0.0        ,  0.0        ,  0.0        , !  IC,WH,RC,PY
     7          0.0        ,  0.0        ,  0.0        ,  0.0        , !  MD,GC,TA,CL
     7          0.0        ,  0.0        ,  0.0        ,  0.0        , !  BL,WO,BO,RA
     7          0.0        ,  0.0        ,                             !  PD,WI
C
     8          0.0        ,  0.0        ,  0.0        ,  0.0        , !  DF,GW,PP,SP
     8          0.0        ,  0.0        ,  0.0        ,  0.0        , !  IC,WH,RC,PY
     8          0.0        ,  0.0        ,  0.0        ,  0.0        , !  MD,GC,TA,CL
     8          0.0        ,  0.0        ,  0.0        ,  0.0        , !  BL,WO,BO,RA
     8          0.0        ,  0.0        ,                             !  PD,WI
C
     9          1.0        ,  0.2        ,  0.2        ,  0.2        , !  DF,GW,PP,SP
     9          0.2        ,  0.2        ,  0.2        ,  0.2        , !  IC,WH,RC,PY
     9          0.2        ,  0.2        ,  0.2        ,  0.2        , !  MD,GC,TA,CL
     9          0.2        ,  0.2        ,  0.2        ,  0.2        , !  BL,WO,BO,RA
     9          0.2        ,  0.2/                                     !  PD,WI
C
      B0=MPAR(ISPGRP,1)
      B1=MPAR(ISPGRP,2)
      B2=MPAR(ISPGRP,3)
      B3=MPAR(ISPGRP,4)
      B4=MPAR(ISPGRP,5)
      B5=MPAR(ISPGRP,6)
      B6=MPAR(ISPGRP,7)
      B7=MPAR(ISPGRP,8)
      POW=MPAR(ISPGRP,9)
      IF(ISPGRP .EQ. 14) THEN   ! Oregon White Oak
         PM=B0+B1*DBH+B2*DBH**2+B3*CR+B4*(SI_1+4.5)+B5*ALOG(BAL+5.0)
      ELSE
         PM=B0+B1*DBH+B2*DBH**2+B3*CR+B4*(SI_1+4.5)+B5*BAL
     1     +B6*BAL*EXP(B7*OG)
      ENDIF
      RETURN
      END
C**********************************************************************
      SUBROUTINE PM_SMC(ISPGRP,DBH,CR,SI_1,SI_2,SI_3,BAL,BA,POW,PM)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 DBH,CR,SI_1,SI_2,SI_3,BAL,BA,POW,PM,B0,B1,B2,B3,B4,B5,B6,
     1       MPAR(11,8)
C
C  SMC MORTALITY (6 parameters - all species)
C
C     DF Coefficients Fit Using Data From the SMC Type I Control Plots
C     GF Coefficients from Unpublished Equation on File at OSU Dept. Forest Resources
C     WH Coefficients from Hann, Marshall, Hanus (2003) FRL Research Contribution 40
C     RC Coefficients from WH of Hann, Marshall, Hanus (2003) FRL Research Contribution 40
C     PY Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     MD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
C     RA Coefficients from Best Guess
C     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
C     WI Coefficients from Best Guess
C
      DATA MPAR/
     1         -5.149445353, -7.60159    , -0.761609   , -0.761609   , !  DF,GF,WH,RC
     1         -4.072781265, -6.089598985, -2.976822456, -6.00031085 , !  PY,MD,BL,WO
     1         -2.0        , -3.020345211, -1.386294361,               !  RA,PD,WI
C
     2         -0.717704874, -0.200523   , -0.529366   , -0.529366   , !  DF,GF,WH,RC
     2         -0.176433475, -0.245615070,  0.0        , -0.10490823 , !  PY,MD,BL,WO
     2         -0.5        ,  0.0        ,  0.0        ,               !  RA,PD,WI
C
     3          0.0        ,  0.0        ,  0.0        ,  0.0        , !  DF,GF,WH,RC
     3          0.0        ,  0.0        ,  0.0        ,  0.0        , !  PY,MD,BL,WO
     3          0.015      ,  0.0        ,  0.0        ,               !  RA,PD,WI
C
     4          0.0        ,  0.0        , -4.74019    , -4.74019    , !  DF,GF,WH,RC
     4         -1.729453975, -3.208265570, -6.223250962, -0.99541909 , !  PY,MD,BL,WO
     4         -3.0        , -8.467882343,  0.0        ,               !  RA,PD,WI
C
     5          0.005062927,  0.0441333  ,  0.0119587  ,  0.0119587  , !  DF,GF,WH,RC
     5          0.0        ,  0.033348079,  0.0        ,  0.00912739 , !  PY,MD,BL,WO
     5          0.015      ,  0.013966388,  0.0        ,               !  RA,PD,WI
C
     6          0.011332066,  0.00063849 ,  0.00756365 ,  0.00756365 , !  DF,GF,WH,RC
     6          0.012525642,  0.013571319,  0.0        ,  0.87115652 , !  PY,MD,BL,WO
     6          0.01       ,  0.009461545,  0.0        ,               !  RA,PD,WI
C
     7          0.022929897,  0.0        ,  0.0        ,  0.0        , !  DF,GF,WH,RC
     7          0.0        ,  0.0        ,  0.0        ,  0.0        , !  PY,MD,BL,WO
     7          0.0        ,  0.0        ,  0.0        ,               !  RA,PD,WI
C
     8          1.0        ,  0.2        ,  0.2        ,  0.2        , !  DF,GF,WH,RC
     8          0.2        ,  0.2        ,  0.2        ,  0.2        , !  PY,MD,BL,WO
     8          0.2        ,  0.2        ,  0.2/                       !  RA,PD,WI
C
      B0=MPAR(ISPGRP,1)
      B1=MPAR(ISPGRP,2)
      B2=MPAR(ISPGRP,3)
      B3=MPAR(ISPGRP,4)
      B4=MPAR(ISPGRP,5)
      B5=MPAR(ISPGRP,6)
      B6=MPAR(ISPGRP,7)
      POW=MPAR(ISPGRP,8)
      IF(ISPGRP .EQ. 1) THEN                          ! Douglas-fir
         PM=B0+B1*DBH+B2*DBH**2+B3*CR+B4*(SI_1+4.5)+B5*BAL+B6*BA
      ELSEIF(ISPGRP .EQ. 2) THEN                          ! Grand Fir
         PM=B0+B1*DBH+B4*(SI_3+4.5)+B5*(BAL/DBH)
      ELSEIF(ISPGRP .EQ. 3 .OR. ISPGRP .EQ. 4) THEN   ! Western Hemlock and Western Red Cedar
         PM=B0+B1*DBH+B2*DBH**2+B3*CR+B4*(SI_2+4.5)+B5*BAL
      ELSEIF(ISPGRP .EQ. 8) THEN   ! Oregon White Oak
         PM=B0+B1*DBH+B2*DBH**2+B3*CR+B4*(SI_3+4.5)+B5*ALOG(BAL+5.0)
      ELSE
         PM=B0+B1*DBH+B2*DBH**2+B3*CR+B4*(SI_3+4.5)+B5*BAL+B6*BA
      ENDIF
      RETURN
      END
C**********************************************************************
      SUBROUTINE MCW_SWO(ISPGRP,D,H,MCW)
C     DETERMINE MCW FOR EACH TREE
C**********************************************************************
C
C     MCW = MCW VALUE
C
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4    D,H,MCW,MCWPAR(18,4),B0,B1,B2,DBH,HT,PKDBH
C
C  MAXIMUM CROWN WIDTH (4 parameters - all species)
C
C     DF Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     GW Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     PP Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     SP Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     IC Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     WH Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     RC Coefficients from Smith (1966) Proc. 6th World Forestry Conference
C     PY Coefficients from WH
C     MD Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     GC Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     TA Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     CL Coefficients from TA
C     BL Coefficients from Ek (1974) School of Natural Res., U. Wisc., Forestry Res. Notes.
C     WO Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     BO Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     RA Coefficients from Smith (1966) Proc. 6th World Forestry Conference
C     PD Coefficients from GC
C     WI Coefficients from GC
C
      DATA MCWPAR/
     1            4.6366    ,   6.1880    ,   3.4835    ,   4.6600546 ,! DF,GW,PP,SP
     1            3.2837    ,   4.5652    ,   4.0       ,   4.5652    ,! IC,WH,RC,PY
     1            3.4298629 ,   2.9793895 ,   4.4443    ,   4.4443    ,! MD,GC,TA,CL
     1            4.0953    ,   3.0785639 ,   3.3625    ,   8.0       ,! BL,WO,BO,RA
     1            2.9793895 ,   2.9793895 ,                            !  PD,WI
C
     2            1.6078    ,   1.0069    ,   1.343     ,   1.0701859 ,! DF,GW,PP,SP
     2            1.2031    ,   1.4147    ,   1.65      ,   1.4147    ,! IC,WH,RC,PY
     2            1.3532302 ,   1.5512443 ,   1.7040    ,   1.7040    ,! MD,GC,TA,CL
     2            2.3849    ,   1.9242211 ,   2.0303    ,   1.53      ,! BL,WO,BO,RA
     2            1.5512443 ,   1.5512443 ,                            !  PD,WI
C
     3           -0.009625  ,   0.0       ,  -0.0082544 ,   0.0       ,! DF,GW,PP,SP
     3           -0.0071858 ,   0.0       ,   0.0       ,   0.0       ,! IC,WH,RC,PY
     3            0.0       ,  -0.01416129,  0.0        ,   0.0       ,! MD,GC,TA,CL
     3           -0.011630  ,   0.0       ,  -0.0073307 ,   0.0       ,! BL,WO,BO,RA
     3           -0.01416129,  -0.01416129,                            !  PD,WI
C
     4           88.52      , 999.99      ,  81.35      , 999.99      ,! DF,GW,PP,SP
     4           83.71      , 999.99      , 999.99      , 999.99      ,! IC,WH,RC,PY
     4          999.99      ,  54.77      , 999.99      , 999.99      ,! MD,GC,TA,CL
     4          102.53      , 999.99      , 138.93      , 999.99      ,! BL,WO,BO,RA
     4           54.77      ,  54.77/                                  !  PD,WI
C
      DBH=D
      HT=H
      B0=MCWPAR(ISPGRP,1)
      B1=MCWPAR(ISPGRP,2)
      B2=MCWPAR(ISPGRP,3)
      PKDBH=MCWPAR(ISPGRP,4)
      IF(DBH .GT. PKDBH) THEN
         DBH=PKDBH
      ENDIF
      IF(HT .LT. 4.501)THEN
           MCW=HT/4.5*B0
      ELSE
           MCW=B0+B1*DBH+B2*DBH**2
      ENDIF
      RETURN
      END
C
C**********************************************************************
      SUBROUTINE MCW_SMC(ISPGRP,D,H,MCW)
C     DETERMINE MCW FOR EACH TREE
C**********************************************************************
C
C     MCW = MCW VALUE
C
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4    D,H,MCW,MCWPAR(11,4),B0,B1,B2,DBH,HT,PKDBH
C
C  MAXIMUM CROWN WIDTH (4 parameters - all species)
C
C     DF Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     GF Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     WH Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     RC Coefficients from Smith (1966) Proc. 6th World Forestry Conference
C     PY Coefficients from WH of Paine and Hann (1982) FRL Research Paper 46
C     MD Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     BL Coefficients from Ek (1974) School of Natural Res., U. Wisc., Forestry Res. Notes.
C     WO Coefficients from Paine and Hann (1982) FRL Research Paper 46
C     RA Coefficients from Smith (1966) Proc. 6th World Forestry Conference
C     PD Coefficients from GC of Paine and Hann (1982) FRL Research Paper 46
C     WI Coefficients from GC of Paine and Hann (1982) FRL Research Paper 46
C
      DATA MCWPAR/
     1            4.6198    ,   6.1880    ,   4.5652    ,   4.0       ,! DF,GF,WH,RC
     1            4.5652    ,   3.4298629 ,   4.0953    ,   3.0785639 ,! PY,MD,BL,WO
     1            8.0       ,   2.9793895 ,   2.9793895 ,              ! RA,PD,WI
C
     2            1.8426    ,   1.0069    ,   1.4147    ,   1.65      ,! DF,GF,WH,RC
     2            1.4147    ,   1.3532302 ,   2.3849    ,   1.9242211 ,! PY,MD,BL,WO
     2            1.53      ,   1.5512443 ,   1.5512443 ,              ! RA,PD,WI
C
     3           -0.011311  ,   0.0       ,   0.0       ,   0.0       ,! DF,GF,WH,RC
     3            0.0       ,   0.0       ,  -0.011630  ,   0.0       ,! PY,MD,BL,WO
     3            0.0       ,  -0.01416129,  -0.01416129,              ! RA,PD,WI
C
     4           81.45      , 999.99      , 999.99      , 999.99      ,! DF,GF,WH,RC
     4          999.99      , 999.99      , 102.53      , 999.99      ,! PY,MD,BL,WO
     4          999.99      ,  54.77      ,  54.77/                    ! RA,PD,WI
C
      DBH=D
      HT=H
      B0=MCWPAR(ISPGRP,1)
      B1=MCWPAR(ISPGRP,2)
      B2=MCWPAR(ISPGRP,3)
      PKDBH=MCWPAR(ISPGRP,4)
      IF(DBH .GT. PKDBH) THEN
         DBH=PKDBH
      ENDIF
      IF(HT .LT. 4.501)THEN
           MCW=HT/4.5*B0
      ELSE
           MCW=B0+B1*DBH+B2*DBH**2
      ENDIF
      RETURN
      END
C
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
