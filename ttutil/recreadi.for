*     input file is closed automatically when end of file is reached
*     call recread_term is not strictly necessary then (closes only when the
*     file is still open)
      SUBROUTINE RECREAD_INIT (UNIT,INPUT_FILE)

      IMPLICIT NONE

*     Formal parameters
      CHARACTER*(*) INPUT_FILE
      INTEGER UNIT

*     Local variables

*     common block
      INCLUDE 'recread.inc'

*     miscellaneous
      INTEGER FILE_LEN
      SAVE

*     open first with FOPENS for error checks
      L_UNIT = UNIT
      CALL FOPENS (L_UNIT,INPUT_FILE,'RDO',' ')
      CLOSE (L_UNIT)

      FILE_LEN = LEN_TRIM (INPUT_FILE)

*     Mac with LS-Fortran
c      OPEN (UNIT=L_UNIT,FILE=INPUT_FILE(1:FILE_LEN),STATUS='OLD',
c     &      RECORDTYPE='STREAM')

*     IBM compatible PC with Microsoft Fortran 5.1 or
*     Microsoft Powerstation 1.0
      OPEN (UNIT=L_UNIT,FILE=INPUT_FILE(1:FILE_LEN),STATUS='OLD',
     &      ACCESS='SEQUENTIAL',FORM='BINARY')

      F_BUF       = ' '
      F_BUF_P     = F_BUF_DEC_LEN
      F_BUF_LEN   = F_BUF_DEC_LEN
      LAST_BUF    = .FALSE.
      L_EOF       = .FALSE.
      CR          = CHAR (13)
      LF          = CHAR (10)
      FILE_CLOSED = .FALSE.
      INIT        = .TRUE.

      RETURN
      END

      BLOCK DATA RECREAD_DATA
      IMPLICIT NONE
      INCLUDE 'recread.inc'
      SAVE
      DATA INIT /.FALSE./
      END

