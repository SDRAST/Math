      FUNCTION FNCTN(X,A,IFNC,NTERMS)
C TBHK &KFCTN <860206.1554>

C **********************************************************************

C+ FNCTN &KFCTN F  Functions for fitting data with CLFT

C  FUNCTIONAL DESCRIPTION:  Provides funstional forms for fitting
C    to data. The functions currently coded are:
C    1- Gaussians   A(1),A(4),... amplitudes
C                   A(2),A(5),... offsets from X=0
C                   A(3),A(6),... halfwidths
C       Multiple Gaussians are added: Guas1 + Gaus2 + Gaus3 +...
C    2- Polynomials A(1) + A(2)*X + A(3)*X*X + .......
C    3- Cosines     A(1)          amplitude 
C                   A(2),A(4),... phases
C                   A(3),A(5),... wavelengths 
C       Multiple cosines are multiplied: Cos1 * Cos2 * Cos3 *...
C    4- Hyperfine structure: deuterated ammonia 1(1,1,+)-1(0,1,-);
C       low optical depth case. The parameters A(1) - A(3) are defined
C       as for a single Gaussian. 
C    5- Hyperfine structure: deuterated ammonia 1(1,1,+)-1(0,1,-);
C       optical depth treated as a parameter. A(1) - A(3) are the 
C       Gaussian profile parameters. A(4) is the optical depth. 
C    6- Symmetric rotor multiplet: methyl acetylene J=6-5 
C          A(1) - column density
C          A(2) - peak antenna temperature
C          A(3) - LSR velocity
C          A(4) - line width
C    7- Symmetric rotor multiplet: methyl acetylene J=5-4 
C          Parameters as for (6) above. 
C    8- Hyperfine structure: deuterated ammonia 2(1,2,-)-2(0,2,+);
C       low optical depth case. The parameters A(1) - A(3) are defined
C       as for a single Gaussian. 
C    9- Hyperfine structure: deuterated ammonia 2(1,2,-)-2(0,2,+);
C       optical depth treated as a parameter. A(1) - A(3) are the 
C       Gaussian profile parameters. A(4) is the optical depth. 
C   10- Hyperfine structure: ammonia 1(1);
C       low optical depth case. The parameters A(1) - A(3) are defined
C       as for a single Gaussian. 
C   11- Hyperfine structure: ammonia 1(1);
C       optical depth treated as a parameter. A(1) - A(3) are the 
C       Gaussian profile parameters. A(4) is the optical depth. 
C   12- Hyperfine structure: ammonia 2(2);
C       low optical depth case. The parameters A(1) - A(3) are defined
C       as for a single Gaussian. 
C   13- Hyperfine structure: ammonia 2(2);
C       optical depth treated as a parameter. A(1) - A(3) are the 
C       Gaussian profile parameters. A(4) is the optical depth.
C   14- Pulse arrival time (seconds vs MHz).  A(1) is arrival time of
C       undispersed pulse. A(2) is the dispersion measure.

C  CALLING SEQUENCE:  RETURN = FNCTN( X,A,IFNC,NTERMS ) 
C     ARG   I/O/M  TYPE    DESCRIPTION
C    ------  ---   ----    ---------------------------------------------
C    RETURN: OUT   F       value of the dependent variable
C    X     : IN    F       value of independent variable
C    A     : IN    F       array of parameters for function 
C    IFNC  : IN    I       functional form indicator
C    NTERMS: IN    I       number of parameters in the function

C  SUBROUTINES CALLED:

C  REVISION HISTORY:
C    DATE      PROGRAMMER      DESCRIPTION OF CHANGES 
C    --------  --------------  ---------------------------------------- 
C    3Oct84    T.B.H.Kuiper    added ammonia
C    20Jan84   T.B.H.Kuiper    added propyne, deut. ammonia 
C    20Mar83   Tim Thompson    increased from 11 to 21 parameters 
C    24Aug80   T.B.H.Kuiper    last pre-HEAD revision

C  OPERATING ENVIRONMENT: 
C    Language           : HP 1000 FORTRAN 4X
C    System Dependencies: none
C    Hardware Required  : none

C  NOTES: The spectroscopic parameters for multiplets are contained 
C    in the doubly dimensioned arrays DF, E, G,  and S. The second
C    index MOLDAT, specifies the molecule and transition: 
C       1  : deuterated ammonia 1(1,1,+)-1(0,1,-) [85.9 GHz]
C       2  : methyl acetylene J=6-5 [102.5 GHz] 
C       3  : methyl acetylene J=5-4 [85.4 GHz]
C       4  : deuterated ammonia 2(1,2,-)-2(0,2,+) [74.2 GHz]
C       5  : ammonia 1(1) [23.69 GHz] 
C       6  : ammonia 2(2) [23.72 GHz]

C  KEYWORDS (FNCTN ): JPL-RAG,MATH,DATA REDUCTION,

C **********************************************************************
C  VARIABLE DICTIONARY
C  -------- ----------
C    COMPNT: one component of a hyperfine multiplet 
C    DF    : frequency offset from principal hyperfine component
C    E     : rotational energy levels 
C    ENL   : column density 
C    FREQ  : frequency of the transition in MHz 
C    G     : inclusive stat.weight, line str. factor for sym. rot.
C    I     : loop index 
C    II    : secondary index derived from I 
C    ITAU  : specifier for optical depth case 
C    MOLDAT: index for molecular data 
C    NCOMPS: number of hyperfine components 
C    NCOS  : number of cosines in cosine function 
C    NGAUSS: number of gaussians in fitting function
C    PI    : radians in a semicircle
C    S     : line strengths of the hyperfine components 
C    SQRLN2: twice the square root of logarithm of 2
C    T     : peak antenna temperature 
C    V0    : offset from main component 
C    W     : line width 
C **********************************************************************

      COMMON/LUS/ LU
      real*8 A, X
      DIMENSION A(21),DF(7,6),S(7,6),E(7,6),G(7,6),FREQ(6)
Cf2py intent(in) X
Cf2py intent(in) A
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
      DATA FREQ/85926.270, 102499.110, 85431.220, 74155.485,  
     &          23694.496,  23722.633/
      DATA PI/3.1415926/, SQRLN2/1.665109/
      
C     Statement functions for symmetric rotor calculation
      real*8 VV, VV0, WW
      real*8      V0,  W
      F(TT)     = 1.26E-13*(1.-EXP(-FREQ(MOLDAT)*4.783E-5/TT))/SQRT(TT)
      FF(II,TT)        = G(II,MOLDAT)*EXP(-1.44*E(II,MOLDAT)/TT) 
      EX(II,VV,VV0,WW) = EXP(-2.773*((VV-VV0-DF(II,MOLDAT))/WW)**2)

      GO TO (120,100,140,160,160,170,170,160,160,160,
     &       160,160,160,180,1000),IFNC

C     Polynomial (IFNC=2)
  100 FNCTN=A(1)
      DO 110 I=2,NTERMS
  110   FNCTN = FNCTN + A(I)*X**(I-1)
      return

C     Gaussian (IFNC=1) 
  120 IF (IFNC.LT.1) GO TO 1000 
      FNCTN = 0.
      NGAUSS=NTERMS/3 
      DO 130 I=1,NGAUSS 
        II=(I-1)*3  
  130   FNCTN = FNCTN
     &          + A(II+1)*EXP( -( (X-A(II+2))*SQRLN2/A(II+3) )**2 )
      return

C     Cosine (IFNC=3) 
  140 FNCTN=A(1)
      NCOS=(NTERMS-1)/2 
      DO 150 I=1,NCOS 
  150   FNCTN = FNCTN * COS((X-A(2*I))*2*PI/A(2*I+1))
      return

C     Hyperfine structure (IFNC=4,5,8,9)
  160 FNCTN=0.
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
      ELSE
        GO TO 1000
      END IF

      FNCTN=0.
      T   = A(1)
      V0  = A(2)
      W   = A(3)
      TAU = A(4)
      DO 162 I=1,NCOMPS
  162   FNCTN = FNCTN + S(I,MOLDAT)*EX(I,X,V0,W)
      FNCTN = FNCTN/S(1,MOLDAT) 
      IF (ITAU.EQ.0) THEN 
        FNCTN = T*FNCTN
      ELSE
        FNCTN = T*(1.-EXP(-TAU*FNCTN))
      END IF
      return
      
C     Symmetric Rotors (IFNC=6,7) 
  170 ENL = A(1)
      T   = A(2)
      V0  = A(3)
      W   = A(4)
      IF (IFNC.EQ.6) THEN 
        MOLDAT=2
        NCOMPS=6
      else
        MOLDAT=3
        NCOMPS=5
      END IF

      FNCTN=0.
      DO 172 I=1,NCOMPS 
  172   FNCTN = FNCTN + FF(I,T)*EX(I,X,V0,W)
      FNCTN = FNCTN*ENL*F(T)/ABS(W)
      return

      # Time delay from plasma dispersion
  180 FNCTN = A(1) + 4150.*A(2)/X**2
      return

C     Invalid function code 
 1000 WRITE(LU,1001) IFNC 
 1001 FORMAT('FNCTN error: Function code',I6,' is invalid.')
      RETURN
      END
