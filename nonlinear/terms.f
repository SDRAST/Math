      INTEGER FUNCTION TERMS(IFNC)
C &KFCTN TBHK <860206.1554>

C   Last EDIT  <860206.1554>
C **********************************************************************

C+ TERMS &KFCTN F  Minimum number of parameters for specified function

C  FUNCTIONAL DESCRIPTION:  Returns the minimum number of parameters
C    which is reasonable for the function specified.

C  CALLING SEQUENCE:  IRETRN = TERMS( IFNC )
C     ARG   I/O/M  TYPE    DESCRIPTION
C    ------  ---   ----    ---------------------------------------------
C    IRETRN: OUT   I       minimum number of parameters 
C    IFNC  : IN    I       function specifier

C  SUBROUTINES CALLED:

C  REVISION HISTORY:
C    DATE      PROGRAMMER      DESCRIPTION OF CHANGES 
C    --------  --------------  ---------------------------------------- 
C    20Jan84   T.B.H.Kuiper    added propyne, deut. ammonia 
C    20Dec83   T.B.H.Kuiper    created

C  OPERATING ENVIRONMENT: 
C    Language           : HP 1000 FORTRAN 4X
C    System Dependencies: 
C    Hardware Required  :

C  KEYWORDS (TERMS ): JPL-RAG,MATH,DATA REDUCTION,

C **********************************************************************
C  VARIABLE DICTIONARY
C  -------- ----------

C **********************************************************************

      COMMON/LUS/ LU
      
C     func=   1   2   3   4   5   6   7   8   9 
      GO TO (110,100,120,130,140,150,150,130,140,
C            10  11  12  13  14  no good
     +       130,140,130,140,160,1000), IFNC
     
C     Polynomial
  100 TERMS=2
      return
      
C     Gaussian
  110 TERMS=3
      return

C     Cosine
  120 TERMS=3
      return
      
C     Ammonia Hyperfine, Low Optical Depth
  130 TERMS=3
      return
      
C     Ammonia Hyperfine, Fit Optical Depth
  140 TERMS=4
      return
      
C     Methyl Acetylene
  150 TERMS=4
      return

  160 TERMS=2
      return
      
C     Invalid function code 
 1000 TERMS=0
      WRITE(LU,1001) IFNC 
 1001 FORMAT('TERMS error: Function code',I6,' is invalid.')
      return
      end
