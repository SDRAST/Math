      SUBROUTINE FDRV(X,A,DERIV,IFNC,NTERMS)
C &KFDRV TBHK <860206.1552>
C   Last EDIT  <860206.1552>
C **********************************************************************
C+ FDRV  &KFDRV S  Derivatives of functions for fitting by CLFT     
C  FUNCTIONAL DESCRIPTION:  Provides the derivatives of functions 
C    specified in FNCTN with respect to the parameters in those 
C    functions. These derivatives are used in the fitting routine 
C  CALLING SEQUENCE:  CALL FDRV( X,A,DERIV,IFNC,NTERMS )
C     ARG   I/O/M  TYPE    DESCRIPTION
C    ------  ---   ----    ---------------------------------------------
C   X     :  IN    F       value of independent variable at which the 
C                            derivatives are to be evaluated
C   A     :  IN    F       array of parameters of function
C   DERIV :  OUT   F       array of derivatives w.r.t. parameters 
C   IFNC  :  IN    I       function specifier 
C   NTERMS:  IN    I       number of terms in the function
C  SUBROUTINES CALLED:
C  REVISION HISTORY:
C    DATE      PROGRAMMER      DESCRIPTION OF CHANGES 
C    --------  --------------  ---------------------------------------- 
C    20Jan84   T.B.H.Kuiper    added propyne, deut. ammonia 
C    20Mar83   Tim Thompson    increased from 11 to 21 parameters 
C     9Sep81   T.B.H.Kuiper    last pre-HEAD revision 
C  OPERATING ENVIRONMENT: 
C    Language           : HP 1000 FORTRAN 4X
C    System Dependencies: none
C    Hardware Required  : none
C  NOTES: This follows the corresponding subroutine given by Bevington, 
C  KEYWORDS (FDRV  ): STATISTICS,DATA REDUCTION,
C **********************************************************************
C  VARIABLE DICTIONARY
C  -------- ----------
C    DF    : frequency offset from principal hyperfine component
C    E     : energies of rotational levels
C    EF    : one of the factors in the symmetric rotor calculation
C    EIGLN2: 8 * ln(2)
C    ENL   : column density 
C    FACTOR: a common factor in the terms of the cosine derivative series 
C    FACTR1: a common factor in hyperfine derivative calculations 
C    FACTR2:  " 
C    FACTR3:  " 
C    FACTR4:  " 
C    FORLN2: 4 * ln(2)
C    FREQ  : frequency of the transition in MHz 
C    G     : inclusive stat.weight, line str. factor for sym. rot.
C    I     : loop index 
C    II    : secondary index derived from I 
C    ITAU  : indicator for optical depth case in h.f. calculation 
C    J     : secondary index derived from I 
C    JJ    : secondary index derived from I 
C    MOLDAT: index for molecular data 
C    NCOMPS: number of hyperfine components 
C    NCOS  : number of cosine terms 
C    NGAUSS: number of gaussians
C    PI    : number of radians in a semi-circle 
C    NCOMPS: number of hyperfine components 
C    SQRLN2: twice the square root of logarithm of 2
C    SUM0  : temporary sum used in computing derivative 
C    SUM1  :  "
C    SUM2  :  "
C    SUM3  :  "
C    SUM4  :  "
C    MOLDAT: index for molecular data
C    T     :
C    TAU   : optical depth
C    V0    : offset from main component in multiplet
C    W     : line width for components
C **********************************************************************
      COMMON/LUS/ LU
      real*8 X, A(21), DERIV(21)
      DIMENSION DF(7,6),S(7,6),E(7,6),G(7,6),FREQ(6)
Cf2py intent(in) X
Cf2py intent(in) A
Cf2py intent(out) DERIV
Cf2py intent(in) IFNC
Cf2py intent(in) NTERMS
      DATA DF/ 0.000, 0.047,-0.567,  0.614,  1.465, -1.487,  0.000, 
     +         0.000,-1.960,-7.840,-17.637,-31.348,-48.967,  0.000, 
     +         0.000,-1.634,-6.534,-14.700,-26.127,  0.000,  0.000, 
     +         0.000,-0.175, 0.315, -0.951,  1.266, -1.655,  1.795, 
     +         0.000,-0.599, 0.597, -1.539,  1.539,  0.000,  0.000, 
     +         0.000,-1.315, 1.315, -2.046,  2.046,  0.000,  0.000/ 
      DATA S/0.416, 0.083, 0.139, 0.139, 0.111, 0.111, 0.000, 
     +       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 
     +       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,   
     +       0.415, 0.150, 0.231, 0.052, 0.052, 0.050, 0.050, 
     +       0.500, 0.140, 0.140, 0.110, 0.110, 0.000, 0.000, 
     +       0.796, 0.052, 0.052, 0.050, 0.050, 0.000, 0.000/ 
      DATA E/0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000,
     &       8.5520, 13.403, 27.957, 52.209, 86.153, 129.78, 0.0000,
     &        5.701, 10.553, 25.106, 49.358, 83.303, 0.0000, 0.0000,
     &        0.000,  0.000,  0.000,  0.000,  0.000, 0.0000, 0.0000,
     &        0.000,  0.000,  0.000,  0.000,  0.000, 0.0000, 0.0000,
     &        0.000,  0.000,  0.000,  0.000,  0.000, 0.0000, 0.0000/
      DATA G/0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 
     &       3.000, 2.920, 2.670, 4.500, 1.670, 0.916, 0.000, 
     &       2.500, 2.400, 2.100, 3.200, 0.900, 0.000, 0.000, 
     &       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 
     &       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 
     &       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000/ 
      DATA FREQ/85926.270, 102499.11, 85431.22, 74155.485,  
     &          23694.496,  23722.633/
      DATA PI/3.1415926/,SQRLN2/1.665109/,FORLN2/2.773/,EIGLN2/5.546/

      real*8 VV, VV0, WW
      real*8      V0,  W
C     Statement functions for symmetric rotor calculation 
      F(TT)     = 1.26E-13*(1.-EXP(-FREQ(MOLDAT)*4.783E-5/TT))/SQRT(TT)
      FF(II,TT)        = G(II,MOLDAT)*EXP(-1.44*E(II,MOLDAT)/TT) 
      EX(II,VV,VV0,WW) = EXP(-FORLN2*((VV-VV0-DF(II,MOLDAT))/WW)**2)

C            1 2 3 4 5 6 7 8 9 10 11 12 13 14
      GO TO (3,2,4,5,5,6,6,5,5, 5, 5, 5, 5, 7,1000),IFNC

C     Polynomial derivatives (IFNC=2)
    2 DERIV(1)=1
      DO 200 I=2,NTERMS 
  200   DERIV(I)=X**(I-1)
      return

C     Gaussian derivatives (IFNC=1)
    3 IF (IFNC.LT.1) GO TO 1000 
      NGAUSS=NTERMS/3
      DO 300 I=1,NGAUSS
        II = (I-1)*3
        DERIV(II+1) = EXP(-((X-A(II+2))*SQRLN2/A(II+3))**2) 
        DERIV(II+2) = DERIV(II+1) * 2.*A(II+1) * SQRLN2**2
     &                                * (X-A(II+2))/A(II+3)**2
  300   DERIV(II+3) = DERIV(II+2) * (X-A(II+2))/A(II+3)
      return

C     Cosine derivatives (IFNC=3)
    4 FACTOR=A(1) 
      NCOS=(NTERMS-1)/2 
      DO 400 I=1,NCOS 
  400   FACTOR=FACTOR*COS( (X-A(2*I))*2*PI/A(2*I+1) ) 
      DERIV(1)=FACTOR/A(1)
      DO 401 I=1,NCOS 
        J=2*I 
        JJ=2*I+1
        DERIV(J)=FACTOR*2*PI*SIN( (X-A(J))*2*PI/A(JJ) ) 
     +                   /(COS( (X-A(J))*2*PI/A(JJ) )*A(JJ))
  401   DERIV(JJ)=DERIV(J)*(X-A(J))/A(JJ)
      return

C     Hyperfine structure (IFNC=4,5,8,9,10,11,12,13)
    5 T   = A(1)
      V0  = A(2)
      W   = A(3)
      TAU = A(4)
      IF (IFNC.EQ.4) THEN 
C       Deuterated ammonia [85.9 GHz]; low optical depth case 
        ITAU=0
        NCOMPS=6
        MOLDAT=1
      ELSE IF (IFNC.EQ.5) THEN
C       Deuterated ammonia [85.9 GHz]; fitted optical depth 
        ITAU=1
        NCOMPS=6
        MOLDAT=1
      ELSE IF (IFNC.EQ.8) THEN  
C       Deuterated ammonia [74.2 GHz]; low optical depth case 
        ITAU=0
        NCOMPS=7
        MOLDAT=4
      ELSE IF (IFNC.EQ.9) THEN
C       Deuterated ammonia [74.2 GHz]; fitted optical depth 
        ITAU=1
        NCOMPS=7
        MOLDAT=4
      ELSE IF (IFNC.EQ.10) THEN 
C       Ammonia [23.69 GHz]; low optical depth  
        ITAU=0
        NCOMPS=5
        MOLDAT=5
      ELSE IF (IFNC.EQ.11) THEN 
C       Ammonia [23.69 GHz]; fitted optical depth 
        ITAU=1
        NCOMPS=5
        MOLDAT=5
      ELSE IF (IFNC.EQ.12) THEN 
C       Ammonia [23.72 GHz]; low optical depth  
        ITAU=0
        NCOMPS=5
        MOLDAT=6
      ELSE IF (IFNC.EQ.13) THEN 
C       Ammonia [23.72 GHz]; fitted optical depth 
        ITAU=1
        NCOMPS=5
        MOLDAT=6
      else
        GO TO 1000
      END IF
      
      SUM1=0. 
      SUM2=0. 
      SUM3=0. 
      DO 162 I=1,NCOMPS
        TERM=S(I,MOLDAT)*EX(I,X,V0,W) 
        SUM1=SUM1+TERM
        SUM2=SUM2+TERM*(X - V0 - DF(I,MOLDAT))  
  162   SUM3=SUM3+TERM*(X - V0 - DF(I,MOLDAT))**2
      SUM1=SUM1/S(1,MOLDAT) 
      SUM2=SUM2/S(1,MOLDAT) 
      SUM3=SUM3/S(1,MOLDAT) 
      DERIV(1)=SUM1 
      DERIV(2)=EIGLN2 * T * SUM2/(W**2) 
      DERIV(3)=EIGLN2 * T * SUM3/(W**3) 
      DERIV(4)=0.
      IF (ITAU.EQ.1) THEN 
        EMTAU=EXP(-TAU*SUM1)
        DERIV(1) = 1.-EMTAU 
        DERIV(2) = TAU*EMTAU*DERIV(2) 
        DERIV(3) = TAU*EMTAU*DERIV(3)
        DERIV(4) = T*EMTAU*SUM1
      END IF
      return

C     Symmetric rotor (IFNC=6,7)
    6 ENL = A(1)
      T   = A(2)
      V0  = A(3) 
      W   = A(4)  
      IF (IFNC.EQ.6) THEN 
        MOLDAT=2
        NCOMPS=6
      ELSE
        MOLDAT=3
        NCOMPS=5
      END IF
      SUM0=0. 
      SUM1=0. 
      SUM2=0. 
      SUM3=0. 
      DO 10 I=1,NCOMPS
        FACTOR=FF(I,T)*EX(I,X,V0,W) 
        SUM0=SUM0+FACTOR
        SUM1=SUM1+E(I,MOLDAT)*FACTOR  
        SUM2=SUM2+FACTOR*(X-V0-DF(I,MOLDAT))**2 
        SUM3=SUM3+FACTOR*(X-V0-DF(I,MOLDAT))
   10   CONTINUE
      EF=F(T) 
      DERIV(1)=SUM0*EF/ABS(W) 
      DERIV(2)=-(FREQ(MOLDAT)*6.027E-18*EXP(-FREQ(MOLDAT)*4.783E-5/T)/
     &               (EF*T**2.5)+0.5/T)*ENL*EF*SUM0/W 
     +          +1.44*ENL*EF*SUM1/(ABS(W)*T**2) 
      DERIV(3)=-ENL*EF*SUM0/W**2
     +          +EIGLN2*ENL*EF*SUM2/W**4  
      DERIV(4)=EIGLN2*ENL*EF*SUM3/ABS(W**3)
      return

C     pulse arrival time (function ID 14)
    7 DERIV(1) = 1
      DERIV(2) = 4150./X**2
      return
      
C     Invalid function code 
 1000 WRITE(LU,1001) IFNC 
 1001 FORMAT('FDRV error: Function code',I6,' is invalid.') 
      return
      end