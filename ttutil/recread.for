      SUBROUTINE RECREAD (STBUF,RECLEN,STBLEN,EOF,IWAR)

*     This routine can be used to access an input file using fast
*     buffered block I/O. Works only on Mac's and IBM's

*     STBUF -  String supplied by the user to which the input record is
*              copied
*     RECLEN - Declared length of STBUF
*     STBLEN - Significant length of string STBUF
*     EOF    - End_of_file flag, on EOF, this routines closes the file
*     IWAR   - Set to 1 if input record overflows STBUF, otherwise IWAR is
*              zero

      IMPLICIT NONE

*     Formal parameters
      CHARACTER*(*) STBUF
      LOGICAL EOF
      INTEGER RECLEN,STBLEN,IWAR

*     Local variables
      INCLUDE 'recread.inc'

      LOGICAL FND_CR,SEC_PART
      INTEGER CR_POS,IOS,SLR,LR,TMPI1

      SAVE

      IF (.NOT.INIT) CALL FATALERR ('RECREAD','system not initialized')

      FND_CR   = .FALSE.
      EOF      = .FALSE.
      STBLEN   = 0
      IWAR     = 0
      SEC_PART = .FALSE.

      SLR = 0
      LR = 0

10    IF (.NOT.FND_CR.AND..NOT.L_EOF) THEN
         F_BUF_P = F_BUF_P+1

         IF (F_BUF_P.LE.F_BUF_LEN) THEN
            IF (F_BUF(F_BUF_P:F_BUF_P).EQ.LF)
     &          F_BUF_P = F_BUF_P+1
         END IF

         IF (F_BUF_P.LE.F_BUF_LEN) THEN

            CR_POS = INDEX (F_BUF(F_BUF_P:F_BUF_LEN),CR)

            IF (CR_POS.GT.1) THEN
               LR = LR+CR_POS-1
               TMPI1 = LEN_TRIM (F_BUF(F_BUF_P:F_BUF_P+CR_POS-2))
               IF (TMPI1.GT.0) SLR = LR-(CR_POS-TMPI1)+1
            ELSE IF (CR_POS.EQ.0) THEN
               LR = LR+F_BUF_LEN-F_BUF_P+1
               TMPI1 = LEN_TRIM (F_BUF(F_BUF_P:F_BUF_LEN))
               IF (TMPI1.GT.0) SLR = LR-(F_BUF_LEN-F_BUF_P-TMPI1)-1
            END IF

            IF (CR_POS.GT.0.AND..NOT.SEC_PART) THEN

               FND_CR = .TRUE.
               CR_POS = CR_POS-1
               IF (CR_POS.LE.RECLEN) THEN
                  STBLEN = CR_POS
               ELSE
                  STBLEN = RECLEN
               END IF
               IF (STBLEN.GT.0) THEN
                  STBUF   = F_BUF(F_BUF_P:F_BUF_P+STBLEN-1)
               ELSE
                  STBUF = ' '
               END IF
               F_BUF_P = F_BUF_P+CR_POS

            ELSE IF (CR_POS.EQ.0.AND..NOT.SEC_PART) THEN

               FND_CR    = LAST_BUF
               SEC_PART  = .TRUE.
               IF (F_BUF_LEN-F_BUF_P+1.LE.RECLEN) THEN
                  STBLEN = F_BUF_LEN-F_BUF_P+1
               ELSE
                  STBLEN = RECLEN
               END IF
               STBUF     = F_BUF(F_BUF_P:F_BUF_P+STBLEN-1)
               F_BUF_P   = F_BUF_LEN

            ELSE IF (SEC_PART) THEN

               IF (CR_POS.GT.0) THEN
                  CR_POS   = CR_POS-1
                  FND_CR   = .TRUE.
                  SEC_PART = .FALSE.
               ELSE
                  CR_POS   = F_BUF_LEN
                  FND_CR   = LAST_BUF
                  SEC_PART = .NOT.LAST_BUF
               END IF

               IF (CR_POS+STBLEN.LE.RECLEN) THEN
                  IF (CR_POS.GT.0) STBUF(STBLEN+1:STBLEN+CR_POS) =
     &                             F_BUF(1:CR_POS)
                  STBLEN = STBLEN+CR_POS
               ELSE IF (STBLEN.LT.RECLEN) THEN
                     STBUF(STBLEN+1:RECLEN) = F_BUF(1:RECLEN-STBLEN)
                     STBLEN = RECLEN
               ELSE IF (STBLEN.GT.RECLEN) THEN
                  CALL FATALERR ('RECREAD','internal error')
               END IF

               F_BUF_P = CR_POS+1

            END IF
         ELSE
            IF (.NOT.LAST_BUF) THEN
               IOS   = 0
               F_BUF = ' '
               READ (L_UNIT,IOSTAT=IOS) F_BUF
               F_BUF_P = 0
               IF (IOS.NE.0) THEN
                  LAST_BUF  = .TRUE.
                  F_BUF_LEN = LEN_TRIM (F_BUF)
                  IF (F_BUF_LEN.EQ.0.AND.STBLEN.GT.0) FND_CR = .TRUE.
               END IF
            ELSE
               CLOSE (L_UNIT)
               FILE_CLOSED = .TRUE.
               L_EOF       = .TRUE.
               EOF         = .TRUE.
               STBUF       = ' '
               INIT        = .FALSE.
            END IF
         END IF

      GOTO 10
      END IF

C      write (*,*) lr,slr
      IF (SLR.GT.RECLEN) THEN
         IWAR = 1
         STBLEN = RECLEN
      ELSE
         STBLEN = SLR
      END IF

      RETURN
      END
