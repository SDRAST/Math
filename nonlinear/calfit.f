      SUBROUTINE CLFT(X,Y,WEIGHT,NPTS,A,SIGMAA,FLAMDA,DISPN2,IFNC,
     +                NTERMS,MINUS)
C TBHK &KCLFT <860314.1909>
C **********************************************************************

C+ CLFT  &KCLFT S  Fits arbitrary functions to data

C  FUNCTIONAL DESCRIPTION:  Fits arbitrary, non-linear functions to data
C    of the form Y(X), with WEIGHT assigned to the Y's. The function
C    fitted is specified by IFNC: 
C    1- Gaussians   A(1),A(4),... amplitudes
C                   A(2),A(5),... offsets from X=0
C                   A(3),A(6),... halfwidths
C       Multiple Gaussians are added: Guas1 + Gaus2 + Gaus3 +...
C    2- Polynomials A(1) + A(2)*X + A(3)*X*X + .......
C    3- Cosines     A(1)          amplitude 
C                   A(2),A(4),... phases
C                   A(3),A(5),... wavelengths 
C       Multiple cosines are multiplied: Cos1 * Cos2 * Cos3 *...

C  This is Bevington's CURFIT: Bevington, Data Reduction and Error
C  Analysis for the Physical Sciences (pages 237-239).  This is adapted
C  from: "An Algorithm for Least-Squares Estimation of Parameters",
C  J. Soc. Ind. Appl. Math., no. 2, pp. 431-441, June, 1963.

C  CALLING SEQUENCE:  CALL CLFT( X,Y,WEIGHT,NPTS,A,SIGMAA,FLAMDA,DISPN2,
C                                IFNC,NTERMS,MINUS) 
C     ARG   I/O/M  TYPE    DESCRIPTION
C    ------  ---   ----    ---------------------------------------------
C    X     : IN    F       the independent variable 
C    Y     : IN    F       the dependent variable 
C    WEIGHT: IN    F       weight assigned to the values of Y. Examples:
C                          For no weighting, use
C                                w(i) = 1.,
C                          For instrumental (Gaussian) weighting, use
C                                w(i) = 1./s(i)^2
C                          For statistical (Poisson) weighting, use
C                                w(i) = 1./y(i)
C    NPTS  : IN    I       number of samples in the data set
C    A     : MOD   F       array of parameters for fitting function 
C    SIGMAA: MOD   F       uncertainties is derived best-fit parameters 
C    FLAMDA: IN    F       controls step size in searching hyperspace:
C                          Acceptance criterion for acceptable fit.
C                          This number is equal to the relative change in
C                          the ChiSquare statistic from one iteration to
C                          the next.  1.0E-5 is a typical value.
C    DISPN2: OUT   F       square of dispersion of data about best fit
C    IFNC  : IN    I       identifies the functional form 
C    NTERMS: IN    I       number of parameters in fitting function 
C    MINUS : OUT   I       error return indicator

C  SUBROUTINES CALLED:
C    FDRV  : &KFDRV    Derivatives of function w.r.t. parameters
C    FNCTN : &KFCTN    Fitting functions
C    MTNV  : &KMTNV    Matrix inversion 
C    SQRT  : HP RELS   Square root of real X

C  REVISION HISTORY:
C    DATE      PROGRAMMER      DESCRIPTION OF CHANGES 
C    --------  --------------  ---------------------------------------- 
C    14Mar86   T.B.H.Kuiper    use PARAMETER to set max # params to 20
C    20Mar83   Tim Thompson    Increase capacity to 21 parameters 
C    19Mar82   T.B.H.Kuiper    added named COMMON 
C    unknown   T.B.H.Kuiper    created

C  OPERATING ENVIRONMENT: 
C    Language           : HP 1000 FORTRAN 4 
C    System Dependencies: none
C    Hardware Required  : none
C    1. At present, the routine handles up to 20 parameters. If this is 
C       increased, arrays in MATINV will need to be   increased also. 
C    2. This program follows Bevington's CURFIT.

C  KEYWORDS (CLFT  ): MATH,STATISTICS,DATA REDUCTION, 
C **********************************************************************
C  VARIABLE DICTIONARY
C  -------- ----------
C    ALPHA : sum of derivatives of function w.r.t. parameters 
C    AVWT  : average of the WEIGHTs 
C    B     : new values for parameters
C    CHISQ1: initial values of chi squared or proportional quantity 
C    CHISQR: new valu of chi squared or proportional quantity 
C    DERIV : derivatives of function with respect to parameters 
C    DET   : determinant of the matrix
C    EX    : specific value of X for subroutine calls 
C    I     : loop index 
C    J     : loop index 
C    K     : loop index 
C    KCHK  : checks whether weights were assigned to the data 
C    NSIZE : size of parameter array
C    SUMWT : sum of the WEIGHTs 
C **********************************************************************

      real*8 X(NPTS),Y(NPTS), WEIGHT(NPTS), A(NTERMS), SIGMAA(NTERMS)
      real*8 EX
      COMMON/LUS/ LU
      
      PARAMETER (MXPARM=20)
      REAL*8    ARRAY(MXPARM,MXPARM), DET, ALPHA(MXPARM,MXPARM)
      INTEGER   TERMS
      real*8    BETA(MXPARM), B(MXPARM), DERIV(MXPARM)
Cf2py intent(in)    X
Cf2py intent(in)    Y
Cf2py intent(in)    WEIGHT
Cf2py intent(in)    NPTS
Cf2py intent(inout) A
Cf2py intent(inout) SIGMAA
Cf2py intent(in)    FLAMDA
Cf2py intent(out)   DISPN2
Cf2py intent(in)    IFNC
Cf2py intent(in)    NTERMS
Cf2py intent(out)   MINUS

      MINUS=0
      
      DO 110 I=1,MXPARM
  110   B(I)=A(I)

C     This ensures that the minimum number of terms are available.
      IF (NTERMS.LT.TERMS(IFNC)) NTERMS=TERMS(IFNC)
      
      IF(NTERMS.GT.MXPARM) THEN
        WRITE(LU,120) MXPARM  
  120   FORMAT(' clft: Only',I2,' parameters allowed in CLFT')
        MINUS=-2
        RETURN
      END IF
      IF(NPTS.LE.NTERMS) THEN 
        WRITE(LU,130) 
  130   FORMAT(' clft: There are not enough data points to fit the',
     +  1X,'desired function.')
        MINUS=-1
        RETURN
      END IF
      
C     Set KCHK if WEIGHTs are being used, and compute average weight
C     The default is not to use weights.  All weights = 1 means not
C     to use weights.
      KCHK = 0
      SUMWT = 0.
      DO 150 I=1,NPTS
        IF (WEIGHT(I).NE.1.) KCHK=1
        SUMWT=SUMWT+WEIGHT(I)
  150 CONTINUE
      write(LU,151) KCHK
  151 format(' clft: using weights is ',I1)
      AVWT=SUMWT/NPTS
      
      DO 160 J=1,NTERMS
        BETA(J)=0.
        DO 160 K=1,J
  160     ALPHA(J,K)=0.
  
      DO 180 I=1,NPTS
        EX=X(I) 
        CALL FDRV(EX,A,DERIV,IFNC,NTERMS)
        DO 170 J=1,NTERMS
          BETA(J)=BETA(J) + WEIGHT(I) * ( Y(I)-FNCTN(EX,A,IFNC,NTERMS))
     +                                * DERIV(J)
          DO 170 K=1,J
  170       ALPHA(J,K)=ALPHA(J,K) + WEIGHT(I)*DERIV(J)*DERIV(K)   
  180   CONTINUE
      DO 190 J=1,NTERMS 
        DO 190 K=1,J
  190     ALPHA(K,J)=ALPHA(J,K)

      CHISQ1=0.
      DO 200 I=1,NPTS
  200   CHISQ1=CHISQ1 + WEIGHT(I)*( Y(I)-FNCTN(X(I),A,IFNC,NTERMS) )**2
      CHISQ1=CHISQ1/(NPTS-NTERMS)
      write(LU,201) CHISQ1
  201 format(' calfit: Initial chi^2 = ',E10.2)

  210 continue
C      write(LU,211) FLAMDA
C  211 format(' calfit: trying with step size ',E10.2)
      DO 230 J=1,NTERMS
        DO 220 K=1,NTERMS 
  220     ARRAY(J,K)=ALPHA(J,K)
  230   ARRAY(J,J)=(1.+FLAMDA)*ALPHA(J,J)
      CALL MTNV(ARRAY,NTERMS,MXPARM,DET)
      DO 240 J=1,NTERMS
        B(J)=A(J)
        DO 240 K=1,NTERMS
  240     B(J)=B(J)+BETA(K)*ARRAY(J,K)

      CHISQR=0.
      DO 250 I=1,NPTS
  250   CHISQR=CHISQR + WEIGHT(I)*( Y(I)-FNCTN(X(I),B,IFNC,NTERMS) )**2
      CHISQR=CHISQR/(NPTS-NTERMS)
C      write(LU,251) CHISQR
C  251 format(' calfit: chi^2 is now ',E10.2)
      IF(CHISQ1-CHISQR) 260,270,270
      
  260 FLAMDA=10.*FLAMDA
      GO TO 210
      
  270 DO 300 J=1,NTERMS
        A(J)=B(J) 
        IF (ARRAY(J,J).EQ.0.D0) THEN
          WRITE(LU,280)J,J
  280     FORMAT('clft: Array(',I2,',',I2, 
     +           ') is zero; change your guess.')
          MINUS=1
          RETURN
        END IF 
        SIGMAA(J)=DSQRT(ARRAY(J,J))
  300   IF(KCHK.LT.1) SIGMAA(J)=SIGMAA(J)*SQRT(CHISQR)
      FLAMDA=FLAMDA/10. 
      DISPN2=CHISQR/AVWT
      return
      end
