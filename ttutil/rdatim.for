      SUBROUTINE RDATIM (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IFND
      DOUBLE PRECISION X
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)

**    local variables ; dummy set
      DOUBLE PRECISION      DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM

*     other
      INTEGER IS
      SAVE

*     the number of values is set by RDDATA
      CALL RDDATA (6,'RDATIM',0,0,' ',IS,XNAME,'T',
     $             X,R,I,C,L,ILDEC,1,1,1,1,IFND,
     $             DM,RM,IM,CM,LM)

      RETURN
      END
