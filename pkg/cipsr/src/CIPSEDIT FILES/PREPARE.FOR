C     CIPSANON GROWTH AND YIELD MODEL
C     SUBROUTINES INCLUDED:
C        PREPARE
C        EDIT
C        SPGROUP
C        CKSP
C        CKAGE
C        HSHG
C        BHG
C        FHG
C        GET_ORGEDIT_EDITION
C
**********************************************************************
      SUBROUTINE PREPARE(VERSION,NPTS,NTREES,STAGE,BHAGE,SPECIES,
     1                   USER,IEVEN,DFSQ,DBH,HT,CR,EXPAN,RADGRO,RVARS,
     2                   SERROR,TERROR,SWARNING,TWARNING,IERROR,IRAD,
     3                   GROWTH,ACALIB)
      IMPLICIT NONE
      INTEGER*4 VERSION,NPTS,NTREES,STAGE,BHAGE,SPECIES(2000),
     1          USER(2000),SERROR(13),TERROR(2000,6),SWARNING(8),
     2          TWARNING(2000)
      INTEGER*4 IB,NSPN,TDATAI(2000,3),SPGRP(2000),RADIN(18),
     1          ENTDBH(18),ENTHT(18),ENTCR(18),ENT
      INTEGER*4 IEVEN,DFSQ,IERROR,IRAD
      REAL*4 DBH(2000),HT(2000),CR(2000),EXPAN(2000),RADGRO(2000),
     1       RVARS(30)
      REAL*4 SITE_1,SITE_2,SITE_3,MSDI_1,MSDI_2,MSDI_3,WHC20,PPTDD41
      REAL*4 TDATAR(2000,4),SBA,BALL(51),BAL(500),GROWTH(2000),
     1       PDG(2000),PTRHT(2000),PCR(2000),CCFLL(51),
     2       CCFL(500),STDATAR(2000,4),OG,ACALIB(3,18),HT40,D40,PDF,
     3       PTF,PPP,PWH,PRA,X1,X2
      LOGICAL*2 EVEN,ERROR,MISSHT,MISSCR,RAD
      INTEGER*4 I,II,J,K,KK,IDXAGE,IDXCYC
C      INTEGER*4 IANS,IYN
C      DLL_EXPORT PREPARE
      EVEN =   .FALSE.
      IF(IEVEN .EQ. 1) THEN
         EVEN = .TRUE.
      ENDIF
      SITE_1=RVARS(1)
      SITE_2=RVARS(2)
      MSDI_1=RVARS(3)
      MSDI_2=RVARS(4)
      MSDI_3=RVARS(5)
      WHC20=RVARS(6)
      PPTDD41=RVARS(7)
      IF(DFSQ .LE. 0) THEN
         IF(SITE_1 .LE. 0.0) THEN
            SERROR(4)=1
            IERROR=1
            RETURN
         ENDIF
      ELSEIF(DFSQ .EQ.1) THEN
         IF(WHC20 .LE. 0.0 .OR. PPTDD41 .LE. 0.0) THEN
            SERROR(4)=1
            IERROR=1
            RETURN
         ENDIF
      ENDIF
      IF(SITE_1 .LE. 0.0 .AND. SITE_2 .LE. 0.0) THEN
         X1=(WHC20**2.0)/100.0
         X2=LOG(PPTDD41)/10.0
         SITE_1=EXP(3.79454279+1.22768331*X1+1.98505821*X2);
      ENDIF
      SITE_3=4.5
      IF(VERSION .EQ. 1)THEN
        IF(SITE_1 .LE. 0.0 .AND. SITE_2 .GT. 0.0)THEN
          SITE_1=1.062934*SITE_2
        ELSEIF(SITE_2 .LE. 0.0)THEN
          SITE_2=0.940792*SITE_1
        ENDIF
      ELSE
C       Site index conversion equation from Nigh (1995, Forest Science 41:84-98)
        IF(SITE_1 .LE. 0.0 .AND. SITE_2 .GT. 0.0)THEN
          SITE_3=0.480 +( 1.110 * SITE_2)
          SITE_1=0.212539*SITE_3**1.201868
        ELSEIF(SITE_2 .LE. 0.0)THEN
          SITE_3=3.627414*SITE_1**0.832038
          SITE_2=-0.432 +( 0.899 * SITE_3)
        ENDIF
      ENDIF

      CALL EDIT(VERSION,NPTS,NTREES,STAGE,BHAGE,SPECIES,EVEN,DBH,HT,CR,
     1          EXPAN,RADGRO,WHC20,PPTDD41,SITE_1,SITE_2,SITE_3,MSDI_1,
     2          MSDI_2,MSDI_3,IB,NSPN,SPGRP,RADIN,SERROR,TERROR,
     3          SWARNING,TWARNING,ERROR,MISSHT,MISSCR,RAD)
      IERROR =   0
      IF(ERROR) THEN
         IERROR = 1
         RETURN
      ENDIF
      IF(VERSION .EQ. 1) THEN
         IDXAGE=500
         IDXCYC=100
      ELSE
         IDXAGE=120
         IDXCYC=24
      ENDIF
      DO I=1,2000
        TDATAI(I,1)=0.0
        TDATAI(I,2)=0.0
        TDATAI(I,3)=0.0
        TDATAR(I,1)=0.0
        TDATAR(I,2)=0.0
        TDATAR(I,3)=0.0
        TDATAR(I,4)=0.0
        STDATAR(I,1)=0.0
        STDATAR(I,2)=0.0
        STDATAR(I,3)=0.0
        STDATAR(I,4)=0.0
        PCR(I)=0.0
        PTRHT(I)=0.0
        GROWTH(I)=0.0
      ENDDO
      DO I=1,NTREES
        TDATAI(I,1)=SPECIES(I)
        TDATAI(I,2)=SPGRP(I)
        TDATAI(I,3)=USER(I)
        TDATAR(I,1)=DBH(I)
        TDATAR(I,2)=HT(I)
        TDATAR(I,3)=CR(I)
        TDATAR(I,4)=EXPAN(I)
      ENDDO
      DO I=1,18
         ACALIB(1,I)=0.0
         ACALIB(2,I)=0.0
         ACALIB(3,I)=0.0
         ENTDBH(I)=0
         ENTHT(I)=0
         ENTCR(I)=0
      ENDDO
      CALL HDCALIB(VERSION,IB,NSPN,NTREES,NPTS,STAGE,BHAGE,TDATAI,EVEN,
     1     SITE_1,SITE_2,SITE_3,TDATAR,D40,HT40,PDF,PTF,PPP,PWH,PRA,
     2     ENTHT,ENTDBH,PTRHT,ACALIB)
C      CALL HDCALIB(VERSION,IB,NSPN,NTREES,NPTS,STAGE,BHAGE,TDATAI,EVEN,
C     1     WHC20,PPTDD41,SITE_1,SITE_2,DFSQ,TDATAR,D40,HT40,PDF,PTF,PPP,
C     2     PWH,PRA,ENTHT,ENTDBH,PTRHT,ACALIB)
      IF(MISSHT) THEN
         CALL PRDHT(VERSION,NTREES,TDATAI,ENT,TDATAR,D40,HT40,PDF,PTF,
     1        PPP,PWH,PRA,ACALIB,PTRHT)
      ENDIF
      CALL CRCALIB(VERSION,IB,NSPN,NTREES,NPTS,TDATAI,ENTDBH,WHC20,
     1     PPTDD41,SITE_1,SITE_2,SITE_3,DFSQ,TDATAR,STDATAR,BAL,BALL,
     2     CCFL,CCFLL,SBA,OG,ENTCR,PCR,ACALIB)
      IF(MISSCR) THEN
         CALL PRDCR(VERSION,IB,NSPN,NTREES,NPTS,TDATAI,WHC20,PPTDD41,
     1              SITE_1,SITE_2,SITE_3,DFSQ,TDATAR,CCFL,CCFLL,SBA,OG,
     2              ENT,PCR,ACALIB)
      ENDIF
      IF(RAD) THEN
         CALL DGCALIB(VERSION,IB,NSPN,NPTS,NTREES,RADIN,TDATAI,RAD,BAL,
     1                BALL,OG,TDATAR,GROWTH,WHC20,PPTDD41,SITE_1,SITE_2,
     2                SITE_3,DFSQ,RADGRO,SBA,STDATAR,PDG,ACALIB)
      ELSE
         DO I=1,18
            ACALIB(3,I)=1.0
         ENDDO
      ENDIF
      IRAD=0
      IF(RAD) THEN
         IRAD=1
      ENDIF
      DO I=1,NTREES
        SPECIES(I)=TDATAI(I,1)
        USER(I)=TDATAI(I,3)
        DBH(I)=TDATAR(I,1)
        HT(I)=TDATAR(I,2)
        CR(I)=TDATAR(I,3)
        EXPAN(I)=TDATAR(I,4)
      ENDDO
      RETURN
      END
C***********************************************************************
      SUBROUTINE EDIT(VERSION,NPTS,NTREES,STAGE,BHAGE,SPECIES,EVEN,DBH,
     1           HT,CR,EXPAN,RADGRO,WHC20,PPTDD41,SI_1,SI_2,SI_3,MSDI_1,
     2           MSDI_2,MSDI_3,IB,NSPN,SPGRP,RADIN,SERROR,TERROR,
     3           SWARNING,TWARNING,ERROR,MISSHT,MISSCR,RAD)

      IMPLICIT NONE
      INTEGER*4 VERSION,NPTS,NTREES,STAGE,BHAGE,SPECIES(2000),IB,
     1          NSPN,IIB
      INTEGER*4 I,J,SPGRP(2000),RADIN(18),BIG6,BNXT,SERROR(13),
     1          TERROR(2000,6),SWARNING(8),TWARNING(2000),TEMPERR
      REAL*4 DBH(2000),HT(2000),CR(2000),EXPAN(2000),RADGRO(2000),
     1       WHC20,PPTDD41,SI_1,SI_2,SI_3,MSDI_1,MSDI_2,MSDI_3,MAXDF,
     2       MAXGF,MAXWH,MAXPP,MAXIC,MAXRA,MAXHT,ICSI,PRA
      REAL*4 BA,SBA,B6SBA,HWSBA,B0,B1,PHT,OLD,Z50,X,XSI50,X1,X2
      LOGICAL*2 EVEN,ERROR,MISSHT,MISSCR,RAD
      ERROR = .FALSE.
      RAD = .FALSE.
      MISSHT = .FALSE.
      MISSCR = .FALSE.
      BIG6=0
      BNXT=0
      MAXGF = 0.0
      MAXDF = 0.0
      MAXWH = 0.0
      MAXPP = 0.0
      MAXIC = 0.0
      MAXRA = 0.0
      DO I=1,18
         RADIN(I)=0
      ENDDO
      DO I=1,2000
         DO J=1,6
            TERROR(I,J)=0
         ENDDO
         TWARNING(I)=0
      ENDDO
      DO I=1,13
         SERROR(I)=0
      ENDDO
      DO I=1,8
         SWARNING(I)=0
      ENDDO
      IF(NTREES .LT. 1 .OR. NTREES .GT. 2000) THEN
         SERROR(1) = 1
      ENDIF
      IF(VERSION .LT. 1 .OR. VERSION .GT. 2) THEN
         SERROR(2) = 1
      ENDIF
      IF(NPTS .LE. 0) THEN
         SERROR(3) = 1
      ENDIF
C      IF(WHC20 .LE. 0.0 .OR. PPTDD41 .LE. 0.0) THEN
C          SERROR(4) = 1
C      ENDIF
      IF(EVEN .AND. BHAGE .LE. 0) THEN
         SERROR(6) = 1
      ENDIF
      IF(.NOT. EVEN .AND. BHAGE .GT. 0) THEN
         SERROR(7) = 1
      ENDIF
      IF(EVEN .AND. (STAGE-BHAGE) .LE. 1) THEN
         SERROR(8) = 1
      ENDIF
      IF(MSDI_1 .GT. 1000.0 .OR. MSDI_2 .GT. 1000.0 .OR. MSDI_3 .GT.
     1   1000.0) THEN
         SERROR(9) = 1
      ENDIF
      DO I=1,13
         IF(SERROR(I) .EQ. 1) THEN
            ERROR = .TRUE.
            RETURN
         ENDIF
      ENDDO
      SELECT CASE(VERSION)
        CASE(1)
          IB=5
          NSPN=18
        CASE(2)
          IB=3
          NSPN=11
      ENDSELECT
C
C     EDIT TREE RECORDS FOR ERRORS
C
      DO I=1,NTREES
         CALL CKSP(VERSION,I,SPECIES,TERROR)
         IF(DBH(I) .LE. 0.09)THEN
            TERROR(I,2) = 1
         ENDIF
         IF(HT(I) .LE. 0.0)THEN
            MISSHT = .TRUE.
         ELSEIF(HT(I) .LE. 4.5)THEN
            TERROR(I,3) = 1
         ENDIF
         IF(CR(I) .LE. 0.0)THEN
            MISSCR = .TRUE.
         ELSEIF(CR(I) .GT. 1.0)THEN
            TERROR(I,4) = 1
         ENDIF
         IF(EXPAN(I) .LT. 0.0)THEN
            TERROR(I,5) = 1
         ENDIF
      ENDDO
      DO I=1,NTREES
         DO J=1,6
            IF(TERROR(I,J) .EQ. 1) THEN
               ERROR = .TRUE.
               RETURN
            ENDIF
         ENDDO
      ENDDO
      DO I=1,NTREES
         SELECT CASE(VERSION)
           CASE(1)           !  SWO  BIG SIX
             IF(SPECIES(I) .EQ. 122 .AND. HT(I) .GT. MAXPP)THEN
               MAXPP=HT(I)
             ELSEIF(SPECIES(I) .EQ. 81 .AND. HT(I) .GT. MAXIC)THEN
               MAXIC=HT(I)
             ELSEIF(SPECIES(I) .EQ. 202 .AND. HT(I) .GT. MAXDF)THEN
               MAXDF=HT(I)
             ELSEIF(SPECIES(I) .EQ. 15 .AND. HT(I) .GT. MAXDF)THEN
               MAXDF=HT(I)
             ELSEIF(SPECIES(I) .EQ. 17 .AND. HT(I) .GT. MAXDF)THEN
               MAXDF=HT(I)
             ELSEIF(SPECIES(I) .EQ. 117 .AND. HT(I) .GT. MAXDF)THEN
               MAXDF=HT(I)
             ENDIF
           CASE(2)
             IF(SPECIES(I) .EQ. 17 .AND. HT(I) .GT. MAXGF)THEN
               MAXGF=HT(I)
             ELSEIF(SPECIES(I).EQ.202 .AND. HT(I) .GT. MAXDF)THEN
               MAXDF=HT(I)
             ELSEIF(SPECIES(I) .EQ. 263 .AND. HT(I) .GT. MAXWH)THEN
               MAXWH=HT(I)
             ENDIF
         ENDSELECT
         CALL SPGROUP(VERSION,I,SPECIES,SPGRP)
         IIB=IB
         IF(SPGRP(I).LE.IIB) THEN
            BIG6=BIG6+1
            IF(EXPAN(I) .LE. 0.0) THEN
               BNXT=BNXT+1
            ENDIF
         ENDIF
      ENDDO
C
C     DETERMINE IF SPECIES MIX CORRECT FOR STAND AGE
C
      SBA=0.0
      B6SBA=0.0
      HWSBA=0.0
      DO I=1,NTREES
        IF(EXPAN(I) .LE. 0.0) CYCLE
        BA=DBH(I)**2*EXPAN(I)
        SBA=SBA+BA
        IF(SPGRP(I) .LE. IIB) B6SBA=B6SBA+BA
        IF(VERSION .EQ. 1) THEN
           IF(SPECIES(I) .EQ. 361 .OR. SPECIES(I) .EQ. 431 .OR.
     1        SPECIES(I) .EQ. 818) THEN
              HWSBA=HWSBA+BA
           ENDIF
        ENDIF
      ENDDO
      SBA=SBA*.005454154/FLOAT(NPTS)
      B6SBA=B6SBA*.005454154/FLOAT(NPTS)
      IF(B6SBA .LE. 0) THEN
         SERROR(5) = 1
         ERROR = .TRUE.
         RETURN
      ENDIF
C      IF(SI_1 .LE. 0.0 .AND. SI_2 .LE. 0.0) THEN
C         X1=(WHC20**2.0)/100.0
C         X2=LOG(PPTDD41)/10.0
C         SI_1=EXP(3.79454279+1.22768331*X1+1.98505821*X2);
C      ENDIF
C      IF(VERSION .EQ. 1)THEN
C        IF(SI_1 .LE. 0.0 .AND. SI_2 .GT. 0.0)THEN
C          SI_1=1.062934*SI_2
C        ELSEIF(SI_2 .LE. 0.0)THEN
C          SI_2=0.940792*SI_1
C        ENDIF
C      ELSEIF(VERSION .EQ. 2)THEN
CC  Site index conversion equation from Nigh (1995, Forest Science 41:84-98)
C        IF(SI_1 .LE. 0.0 .AND. SI_2 .GT. 0.0)THEN
C          SI_1=0.480 +( 1.110 * SI_2)
C        ELSEIF(SI_2 .LE. 0.0)THEN
C          SI_2=-0.432 +( 0.899 * SI_1)
C        ENDIF
C      ENDIF
C
C     DETERMINE WARNINGS (IF ANY)
C
      SELECT CASE (VERSION)
         CASE(1)
            IF((SI_1.GT.0.0).AND.(SI_1.LT.40.0 .OR. SI_1.GT. 150.0))THEN
               SWARNING(1) = 1
            ENDIF
            IF((SI_2.GT.0.0).AND.(SI_2.LT.50.0 .OR. SI_2.GT. 140.0))THEN
               SWARNING(2) = 1
            ENDIF
         CASE(2)
            IF((SI_1.GT.0.0).AND.(SI_1.LT.35.0 .OR. SI_1.GT. 105.0))THEN
               SWARNING(1) = 1
            ENDIF
            IF((SI_2.GT.0.0).AND.(SI_2.LT.90.0 .OR. SI_2.GT. 142.0))THEN
               SWARNING(2) = 1
            ENDIF
      ENDSELECT
      SELECT CASE(VERSION)
        CASE(1)
          IF(MAXPP .GT. 0.0)THEN
             MAXHT=(SI_2-4.5)*(1.0/(1.-EXP(-0.164985*(SI_2-4.5)
     1              **0.288169)))+4.5
             IF(MAXPP .GT. MAXHT) SWARNING(3) = 1
          ENDIF
          IF(MAXIC .GT. 0.0)THEN
             ICSI=(0.66*SI_1)-4.5
             MAXHT=ICSI*(1.0/(1.-EXP(-0.174929*ICSI**0.281176)))+4.5
             IF(MAXIC .GT. MAXHT) SWARNING(3) = 1
          ENDIF
          IF(MAXDF .GT. 0.0) THEN
             MAXHT=(SI_1-4.5)*(1.0/(1.0 -
     1             EXP(-0.174929*(SI_1-4.5)**0.281176)))+4.5
             IF(MAXDF .GT. MAXHT) SWARNING(3) = 1
          ENDIF
        CASE(2)
          IF(MAXDF .GT. 0.0)THEN
             Z50=2500.0/(SI_3-4.5)
             MAXHT=4.5+1.0/(-0.000733819+0.000197693*Z50)
             IF(MAXDF .GT. MAXHT) SWARNING(3) = 1
          ENDIF
          IF(MAXGF .GT. 0.0)THEN
             Z50=2500.0/(SI_3-4.5)
             MAXHT=4.5+1.0/(-0.000733819+0.000197693*Z50)
             IF(MAXGF .GT. MAXHT) SWARNING(3) = 1
          ENDIF
          IF(MAXWH .GT. 0.0)THEN
             Z50=2500.0/(SI_2-4.5)
             MAXHT=4.5+1.0/(0.00192+0.00007*Z50)
             IF(MAXWH .GT. MAXHT) SWARNING(3) = 1
          ENDIF
      ENDSELECT
      IF(EVEN .AND. VERSION .LE. 2 .AND. BHAGE .LT. 10) THEN
         SWARNING(4) = 1
      ENDIF
      IF((VERSION .EQ. 1 .AND. (B6SBA + HWSBA) .LT. SBA*.2).OR.
     1   (VERSION .EQ. 2 .AND. (B6SBA + HWSBA) .LT. SBA*.5))THEN
         SWARNING(5) = 1
      ENDIF
      IF(NTREES .LT. 50)THEN
         SWARNING(6) = 1
      ENDIF
      CALL CKAGE(VERSION,NTREES,IB,SPGRP,SI_1,SI_2,SI_3,HT,OLD)
      X=100.0*(OLD/(BIG6-BNXT))
      IF(X .GE. 50.0) SWARNING(7) = 1
      IF(VERSION .EQ. 1) THEN
         IF(EVEN .AND. BHAGE .GT. 500.) SWARNING(8) = 1
      ELSE
         IF(EVEN .AND. BHAGE .GT. 100.) SWARNING(8) = 1
      ENDIF
      B1=-0.04484724
      DO I=1,NTREES
         SELECT CASE(SPECIES(I))
           CASE(202)
              B0=19.04942539
           CASE(263)
              IF(VERSION .EQ. 2)B0=19.04942539
           CASE(17,15)
              B0=16.26279948
           CASE(122)
              B0=17.11482201
           CASE(117)
              B0=14.29011403
           CASE DEFAULT
              B0=15.80319194
         ENDSELECT
         IF(HT(I) .GT. 4.5) THEN
            PHT=4.5+B0*DBH(I)/(1.0-B1*DBH(I))
            IF(HT(I) .GT. PHT) THEN
               TWARNING(I) = 1
            ENDIF
         ENDIF
         IF(RADGRO(I) .GT. 0.0) THEN
            RAD = .TRUE.
            RADIN(SPGRP(I))=RADIN(SPGRP(I))+1
         ENDIF
      ENDDO
      RETURN
      END
C***********************************************************************
      SUBROUTINE SPGROUP(VERSION,I,SPECIES,SPGRP)
C     DETERMINE SPECIES GROUP FOR EACH TREE IN TREE LIST
C
C     I = TREE INDICATOR
C
C
      IMPLICIT NONE
      INTEGER*4 VERSION,I,SPECIES(2000),SPGRP(2000),ISX,J
C
      INTEGER*4  SCODE1(19)/
     1           202,15,17,122,117,81,263,242,231,361,431,631,805,312,
     2           815,818,351,492,920/
C
      INTEGER*4   SCODE2(11)/
     1           202,17,263,242,231,361,312,815,351,492,920/
C
      ISX = -9999
      SELECT CASE (VERSION)
        CASE(1)
           DO J = 1, 19
             IF(SPECIES(I).EQ. SCODE1(J))THEN
                ISX = J
                IF(ISX .GT.2) ISX=ISX-1
                EXIT
             ENDIF
           ENDDO
        CASE(2)
           DO J = 1, 11
              IF(SPECIES(I).EQ. SCODE2(J))THEN
                 ISX = J
                 EXIT
             ENDIF
           ENDDO
      ENDSELECT
      SPGRP(I)=ISX
      RETURN
      END
C***********************************************************************
      SUBROUTINE CKSP(VERSION,I,SPECIES,TERROR)
C     DETERMINE SPECIES GROUP FOR EACH TREE IN TREE LIST
C
C     I = TREE INDICATOR
C
C
      IMPLICIT NONE
      INTEGER*4 VERSION,I,SPECIES(2000),SPGRP(2000),J,
     1          TERROR(2000,6)
      LOGICAL*2 BAD
C
      INTEGER*4  SCODE1(19)/
     1           202,15,17,122,117,81,263,242,231,361,431,631,805,312,
     2           815,818,351,492,920/
C
      INTEGER*4   SCODE2(11)/
     1           202,17,263,242,231,361,312,815,351,492,920/
      BAD=.TRUE.
      SELECT CASE (VERSION)
        CASE(1)
           DO J = 1, 19
             IF(SPECIES(I).EQ. SCODE1(J))THEN
                BAD =.FALSE.
                EXIT
             ENDIF
           ENDDO
        CASE(2)
           DO J = 1, 11
              IF(SPECIES(I).EQ. SCODE2(J))THEN
                 BAD =.FALSE.
             ENDIF
           ENDDO
      ENDSELECT
      IF(BAD) THEN
         TERROR(I,1) = 1
      ENDIF
      RETURN
      END
C***********************************************************************
      SUBROUTINE CKAGE(VERSION,NTREES,IB,SPGRP,SI_1,SI_2,SI_3,HT,OLD)
      IMPLICIT NONE
      INTEGER*4 VERSION,NTREES,IB,SPGRP(2000),K,ISISP
      INTEGER*2 GEA,IERRORS(3)
      REAL*4 SITE,SI_1,SI_2,SI_3,HT(2000),OLD,GEAGE,PHTGRO,IDXAGE
      OLD=0.0
      DO K=1,NTREES
         IF(HT(K) .LE. 4.5) CYCLE
         IF(SPGRP(K).GT.IB) CYCLE
         SELECT CASE(VERSION)
         CASE(1)
C
C        GROWTH EFFECTIVE AGE FROM HANN AND SCRIVANI'S (1987) DOMINANT
C        HEIGHT GROWTH EQUATION
C
            IF(SPGRP(K) .EQ. 3)THEN
               SITE=SI_2-4.5
               ISISP=2
            ELSE
               SITE=SI_1-4.5
               IF(SPGRP(K) .EQ. 5) SITE=SI_1*0.66-4.5
               ISISP=1
            ENDIF
            CALL HS_HG(ISISP,SITE,HT(K),GEAGE,PHTGRO)
            IDXAGE=500.0
         CASE(2)
C
C           GROWTH EFFECTIVE AGE FROM FLEWELLING ET AL. (2001)
C           DOUGLAS-FIR DOMINANT HEIGHT GROWTH EQUATION
C
            IF(SPGRP(K) .EQ. 1) THEN
               CALL FCGMT_GEA(SI_1,HT(K),GEA,IERRORS)
               GEAGE=FLOAT(GEA)
               IF(IERRORS(1) .GT. 0 .OR. IERRORS(2) .GT. 0 .OR.
     1            IERRORS(3).GT. 0) THEN
                     CALL B_HG(SI_3,HT(K),GEAGE,PHTGRO)
               ENDIF


            ELSEIF(SPGRP(K) .EQ. 3)THEN
C
C           GROWTH EFFECTIVE AGE FROM FLEWELLING'S WESTERN HEMLOCK
C           DOMINANT HEIGHT GROWTH EQUATION
C
                CALL F_WH_HG(SI_2,HT(K),GEAGE,PHTGRO)
            ELSE
C
C           GROWTH EFFECTIVE AGE FROM BRUCE'S (1981) DOMINANT HEIGHT
C           GROWTH EQUATION FOR GRAND FIR
C
               CALL B_HG(SI_3,HT(K),GEAGE,PHTGRO)
             ENDIF
             IDXAGE=100.0
         ENDSELECT
         IF(GEAGE.GT.IDXAGE)THEN
            OLD=OLD+1.0
         ENDIF
      ENDDO
      RETURN
      END
**********************************************************************
      SUBROUTINE HS_HG(ISP,SI,HT,GEAGE,PHTGRO)
      IMPLICIT NONE
      INTEGER*4 ISP
      REAL*4 SI,GEAGE,HT,PHTGRO,B0,B1,B2,BBC,X50,A1A,XAI,XAI5
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
      X50=1.0-EXP((-1.)*EXP(BBC+B2*3.912023))
      A1A=1.0-(HT-4.5)*(X50/SI)
      IF(A1A .LE. 0.)THEN
         GEAGE=500.
         PHTGRO=0.
      ELSE
         GEAGE=((-1.0*LOG(A1A))/(EXP(B0)*SI**B1))**(1.0/B2)
         XAI=1.0-EXP(-1.0*EXP(BBC+B2*LOG(GEAGE)))
         XAI5=1.0-EXP((-1.)*EXP(BBC+B2*LOG(GEAGE+1.0)))
         PHTGRO=(4.5+(HT-4.5)*(XAI5/XAI))-HT
      ENDIF
      RETURN
      END
**********************************************************************
      SUBROUTINE B_HG(SI,HT,GEAGE,PHTGRO)
      IMPLICIT NONE
      REAL*4 SI,HT,X1,X2,B1,B2,GEAGE,PHT,XX1,PHTGRO
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
      PHT=SI*EXP(B1*((GEAGE+1.0+X1)**B2-X2**B2))
      PHTGRO=PHT-HT
      RETURN
      END
**********************************************************************
      SUBROUTINE F_WH_HG(SI,HT,GEAGE,PHTGRO)
      IMPLICIT NONE
C     For Western Hemlock compute Growth Effective Age and 5-year potential
C     height growth using the western hemlock top height curves of
C     Flewelling.  These subroutines are required:
C       SITECV_F   computes top height from site and age
C       SITEF_C    computes model parameters
C       SITEF_SI   calculates an approximate psi for a given site
C       Note: Flewelling's curves are metric.
C             Site Index is not adjusted for stump height.
C
      REAL*4 SI,GEAGE,HT,PHTGRO,SIM,HTM,AGE,HTOP,PHT,XHTOP1,XHTOP2
      INTEGER*4 I
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
             CALL SITECV_F(SIM,GEAGE+1.0,XHTOP2)
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
      CALL SITECV_F(SIM,GEAGE+1.0,HTOP)
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
      SUBROUTINE FCGMT_H40(FCGMT_SI,JAGE,HT40,IERRORS)
      IMPLICIT NONE
      REAL*4 FCGMT_SI,HT40
      INTEGER*2 I,N,JAGE,IAGE,IER1,IER2,IER3,IERRORS(3)
      REAL*4 HT_PLANT,HT_GIVEN,HBASE_R4,HADJ_R4,PSI,SI
      CHARACTER A1*1
      INTEGER*2 NVEC(10),IER,IHTYPE,LASTDENA
      REAL*4 SI_R4,FVEC(10),HV(100),HAV(100)
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
      INTEGER*2 IHTYPE,IAGE,IER,BAGE

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
      BAGE=30
      CALL DF_HN( BAGE, SI)
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
C***********************************************************************
      SUBROUTINE GET_CIPSEDIT_EDITION(EDITION)
      IMPLICIT NONE
      REAL*4 EDITION
C      DLL_EXPORT GET_CIPSEDIT_EDITION
      EDITION=1.0
      RETURN
      END
