      SUBROUTINE MTNV(ARRAY,NORDER,NSIZE,DET)
C &KCLFT Invert matrix <860314.1909>

C   Last EDIT  <860314.1909>
C **********************************************************************

C+ MTNV  &KCLFT S  Matrix inversion

C  FUNCTIONAL DESCRIPTION:  Inverts a matrix and finds its determinant
C    following Program B-2 in Bevington, DATA REDUCTION AND ERROR 
C    ANALYSIS, McGraw-Hill, 1969.

C  CALLING SEQUENCE:  CALL MTNV( ARRAY,NORDER,NSIZE,DET ) 
C     ARG   I/O/M  TYPE    DESCRIPTION
C    ------  ---   ----    ---------------------------------------------
C    ARRAY : mod   REAL*4  matrix to be inverted
C    NORDER: in    INT     order of the matrix
C    NSIZE : in    INT     size of array in which matrix is stored
C    DET   : out   REAL*4  determinant

C  SUBROUTINES CALLED:

C  REVISION HISTORY:
C    DATE      PROGRAMMER      DESCRIPTION OF CHANGES 
C    --------  --------------  ---------------------------------------- 
C    14Mar86   T.B.H.Kuiper    HEADed

C  OPERATING ENVIRONMENT: 
C    Language           : HP 1000 FORTRAN 4X
C    System Dependencies: 
C    Hardware Required  :

C  KEYWORDS (MTNV  ): JPL-RAG,MATH,DATA REDUCTION,
C **********************************************************************

C  VARIABLE DICTIONARY
C  -------- ----------
C    AMAX  : maximam element in the matrix
C    I     : loop index 
C    IK    : array with column order
C    J     : loop index 
C    JK    : array with row order 
C    K     : loop index 
C    L     : loop index 
C    SAVE  : temporary storage while re-ordering
C **********************************************************************

      real*8 ARRAY(NSIZE,NSIZE), AMAX, SAVE, DET
      PARAMETER (MXPARM=20) 
      DIMENSION IK(MXPARM), JK(MXPARM)
Cf2py intent(inout) ARRAY
Cf2py intent(in) NORDER
Cf2py intent(in) NSIZE
Cf2py intent(out) DET

      DET = 1.
      DO 100 K=1,NORDER

        AMAX=0.
   21   DO 30 I=K,NORDER
          DO 30 J=K,NORDER
            IF(DABS(AMAX)-DABS(ARRAY(I,J))) 24,24,30
   24         AMAX=ARRAY(I,J)
              IK(K)=I
              JK(K)=J
   30       CONTINUE

        IF(AMAX) 41,32,41 
   32     DET=0.
          GO TO 140

   41   I=IK(K)
        IF(I-K) 21,51,43
   43     DO 50 J=1,NORDER
            SAVE=ARRAY(K,J)
            ARRAY(K,J)=ARRAY(I,J)
   50       ARRAY(I,J)=-SAVE
   51   J=JK(K)
        IF(J-K) 21,61,53
   53     DO 60 I=1,NORDER
            SAVE=ARRAY(I,K)
            ARRAY(I,K)=ARRAY(I,J)
   60       ARRAY(I,J)=-SAVE
   
   61   DO 70 I=1,NORDER
          IF(I-K) 63,70,63
   63       ARRAY(I,K)=-ARRAY(I,K)/AMAX
   70     CONTINUE
        DO 80 I=1,NORDER
          DO 80 J=1,NORDER
            IF(I-K) 74,80,74
   74         IF(J-K) 75,80,75
   75           ARRAY(I,J)=ARRAY(I,J) + ARRAY(I,K)*ARRAY(K,J) 
   80       CONTINUE
        DO 90 J=1,NORDER
          IF(J-K) 83,90,83
   83       ARRAY(K,J)=ARRAY(K,J)/AMAX
   90     CONTINUE
          ARRAY(K,K)=1./AMAX
  100   DET=DET*AMAX
  
      DO 130 L=1,NORDER
        K=NORDER - L +1
        J=IK(K)
        IF(J-K) 111,111,105
  105     DO 110 I=1,NORDER
            SAVE=ARRAY(I,K)
            ARRAY(I,K)=-ARRAY(I,J)
  110       ARRAY(I,J)=SAVE
  111   I=JK(K)
        IF(I-K) 130,130,113
  113     DO 120 J=1,NORDER
            SAVE=ARRAY(K,J) 
            ARRAY(K,J)=-ARRAY(I,J)
  120       ARRAY(I,J)=SAVE
  130   CONTINUE
  140 return
      end
