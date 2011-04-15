      INTEGER FUNCTION ILEN (STRING)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) STRING
 
**    local variables
      SAVE
 
      ILEN = LEN_TRIM (STRING)
 
      RETURN
      END
