!************************************************************************
!Additional function that TTUTIL version 4.21 do not have
!Edit by TaoLi, 2 Jan 2011
!************************************************************************
	REAL FUNCTION LIMIT (MIN,MAX,X)
      IMPLICIT NONE
      REAL MIN,MAX,X
      CHARACTER MESSAG*52
      SAVE
      IF (MAX.LT.MIN) THEN
        MESSAG = 'argument error, MIN > MAX'
        CALL FatalERR ('LIMIT',MESSAG)
      END IF
      IF (X.LT.MIN) THEN
      LIMIT = MIN
      ELSE IF (X.LE.MAX) THEN
      LIMIT = X
      ELSE
      LIMIT = MAX
      END IF
      RETURN
      END
	     
		 
	REAL FUNCTION LINT2 (TABNAM,TABLE,ILTAB,X)
      IMPLICIT NONE
      CHARACTER*(*) TABNAM
      INTEGER ILTAB
      REAL TABLE(ILTAB), X
      INTEGER I1, IUP, IL, ILEN
      REAL SLOPE, TINY
      PARAMETER (TINY=1.E-7)
      LOGICAL ERR
      SAVE
      ERR  = .FALSE.
      IF (MOD(ILTAB,2).NE.0 .OR. ILTAB.LE.2) THEN
      IL = MAX (1, LEN_TRIM (TABNAM))
      WRITE (*,'(2A,/,A)')
     &' Number of elements in interpolation table: ',TABNAM(1:IL),
     &' not correct !'
      ERR = .TRUE.
      ELSE
      IUP = 0
      DO 10 I1=3,ILTAB,2
      IF (TABLE(I1).LE.TABLE(I1-2)) THEN
      IL = MAX (1, LEN_TRIM (TABNAM))
      WRITE (*,'(2A,/,A,I4)')
     &' X-coordinates in interpolation table: ',TABNAM(1:IL),
     &' not in ascending order at point',I1
      ERR = .TRUE.
      END IF
      IF (IUP.EQ.0 .AND. TABLE(I1).GE.X) IUP = I1
10    CONTINUE
      END IF
      IF (.NOT.ERR .AND. X.LT.TABLE(1)) THEN
      IUP = 3
      IF ((TABLE(1)-X) .GT. ABS(X)*TINY) THEN
      IL = MAX (1, LEN_TRIM (TABNAM))
      WRITE (*,'(A,G13.5,/,2A)')
     &' WARNING in LINT2: X-value below defined region at X=',X,
     &' in interpolation table: ',TABNAM(1:IL)
      END IF
      ELSE IF (.NOT.ERR .AND. X.GT.TABLE(ILTAB-1)) THEN
      IUP = ILTAB-1
      IF ((X-TABLE(ILTAB-1)) .GT. ABS(X)*TINY) THEN
      IL = MAX (1, LEN_TRIM (TABNAM))
      WRITE (*,'(A,G13.5,/,2A)')
     &' WARNING in LINT2: X-value above defined region at X=',X,
     &' in interpolation table: ',TABNAM(1:IL)
      END IF
      END IF
      IF (ERR) CALL FatalERR ('LINT2',' ')
      SLOPE = (TABLE(IUP+1)-TABLE(IUP-1))/(TABLE(IUP)-TABLE(IUP-2))
      LINT2 = TABLE(IUP-1)+(X-TABLE(IUP-2))*SLOPE
      RETURN
      END

      INTEGER FUNCTION ILEN (STRING)
      IMPLICIT NONE
      CHARACTER*(*) STRING
      INTEGER I, L
      SAVE
      L = LEN (STRING)
      DO 10 I=L,1,-1
      IF (STRING(I:I).NE.' ') THEN
      ILEN = I
      RETURN
      END IF
10    CONTINUE
      ILEN = 0
      RETURN
      END

      SUBROUTINE LOWERC (STRING)
      IMPLICIT NONE
      CHARACTER*(*) STRING
      INTEGER I,IC,L
      SAVE
      L = LEN (STRING)
      DO 10 I=1,L
      IC = ICHAR(STRING(I:I))
      IF (IC.GE.65.AND.IC.LE.90) STRING(I:I) = CHAR(IC+32)
10    CONTINUE
      RETURN
      END

	      REAL FUNCTION INSW (X1,X2,X3)
      IMPLICIT NONE
      REAL X1,X2,X3
      SAVE
      IF (X1.LT.0.) THEN
      INSW = X2
      ELSE
      INSW = X3
      END IF
      RETURN
      END
      REAL FUNCTION INTGRL (STATE, RATE, DELT)
      IMPLICIT NONE
      REAL STATE, RATE, DELT
      SAVE
      INTGRL = STATE + RATE * DELT
      RETURN
      END

   
   !-----------------------------------------------------------------------------------------------------------------------------
!This subroutine updates TIME and related variables each time it is called with ITASK=2.
!-----------------------------------------------------------------------------------------------------------------------------
!VARIABLE   MEANING                                                                         TYPE        I/O
!ITASK      Task that the routine should carry out:
!           =1, Initialize,store variable for processing with ITASK=2
!           =2, Increase TIME by DELT, update other time variables                          I4          I
!STTIME     Start day of simulation (1 <= STTIME <= 365, 366 in leap years,
!           leap years are not flagged when IYEAR < 1500)                                   R4          I
!DELT       Time step of simulation (multiple of 1 or 1/DELT = integer e.g.
!               0.25, 1/3, 0.5, 1, 2, 3)                                                    R4          I
!PRDEL      Time between successive outputs (must be zero, equal to DELT or
!            multiple of DELT)                                                              R4          I
!FINTIM     Finish time of simulation (counted from start of simulation !)                  R4          I
!IYEAR      Start year with ITASK=1 and current year with ITASK=2, not
!               updated when IYEAR < 1500                                                   I4          I/O
!TIME       Time from start of simulation (starts at value of STTIME)                       R4          O
!DOY        Day number within YEAR (REAL version)                                           R4          O
!IDOY       Day number within YEAR (INTEGER version)                                        I4          O
!TERMNL     Flag that indicates if FINTIM has been reached                                  L4          O
!OUTPUT     Flag that indicates if past TIME is a multiple of PRDEL                         L4          O
!---------------------------------------------------------------------------------------------------------------------------

      SUBROUTINE TIMER2 (ITASK, STTIME, DELT, PRDEL , FINTIM, IYEAR, 
     &      TIME, DOY, IDOY, TERMNL, OUTPUT)
            
      INTEGER ITASK, IDOY,IYEAR
      REAL STTIME, DELT, PRDEL , FINTIM, TIME, DOY
      LOGICAL  TERMNL, OUTPUT
    
    !---LOCAL VARIABLE
      REAL YEAR
      LOGICAL isLEAP
      REAL DAYS, TOTALtime
    
    
      IF(ITASK.EQ.1) THEN
        IDOY = INT(STTIME)
        DOY = REAL(IDOY)
        YEAR = REAL(IYEAR) 
        IF(AMOD(YEAR,4.0).EQ.0.0) THEN
            DAYS = 366.0
        ELSE
            DAYS=365.0
        ENDIF
        TIME =0.0
        TOTALTIME = REAL(IDOY)
        TERMNL = .FALSE.
        OUTPUT = .TRUE.
      ELSEIF(ITASK.EQ.2) THEN
        IDOY = INT(STTIME)
        DOY = DOY + DELT
        TIME = TIME + DELT
        totalTIME = totalTIME + DELT
        IF (totalTIME.GE.FINTIM) THEN
            TERMNL = .TRUE.
        ELSE
            TERMNL = .FALSE.
        ENDIF
        IF(DOY.GE.DAYS) THEN
            YEAR=YEAR+1
            DOY = DOY - DAYS;TOTALTIME=TOTALTIME-DAYS
            IF(AMOD(YEAR,4.0).EQ.0.0) THEN
                DAYS = 366.0
            ELSE
                DAYS = 365.0
            ENDIF
            IF(AMOD(TIME, PRDEL).EQ.0.0) THEN
                OUTPUT = .TRUE.
            ELSE
                OUTPUT = .FALSE.
            END IF           
        ELSE
            IF(AMOD(TIME, PRDEL).EQ.0.0) THEN
                OUTPUT = .TRUE.
            ELSE
                OUTPUT = .FALSE.
            END IF             
        ENDIF
      ENDIF 
    
    
      END SUBROUTINE TIMER2
