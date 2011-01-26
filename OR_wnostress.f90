!----------------------------------------------------------------------!
!  SUBROUTINE WNOSTRESS                                                !
!  Used in ORYZA2000 model version 1.0                                 !
!  Date  : December 2001                                               !
!  Author: B.A.M. Bouman                                               !
!                                                                      !
!  Purpose: Invoked when production environment is POTENTIAL.          !
!           Sets actual transpiration of a crop at zero, and sets      !
!           effects of water stress on growth and development of rice  !
!           at unity.                                                  !
!                                                                      !
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      !
! name   type meaning (unit)                                     class !
! ----   ---- ---------------                                    ----- !
! NL      I4  Number of soil layers (-)                             I  !
! TRW     R4  Actual transpiration rate (mm d-1)                    O  !
! TRWL    R4  Array of actual transpiration rate/layer (mm d-1)     O  !
! LRSTRS  R4  Stress factor for rolling of leaves (-)               O  !
! LDSTRS  R4  Stress factor for accelerating leaf death (-)         O  !
! LESTRS  R4  Stress factor for reducing expansion of leaves (-)    O  !
! PCEW    R4  Stress factor for CO2 assimilation (-)                O  !
! ZRT     R4  Root depth (m)                                           !
!----------------------------------------------------------------------!
      SUBROUTINE WNOSTRESS (NL, TRW, TRWL, ZRT, TKL, LRSTRS, LDSTRS, LESTRS, PCEW, CPEW)

		USE public_module
      IMPLICIT NONE
!-----Formal parameters
      INTEGER NL
      REAL    TRW, LRSTRS, LDSTRS, LESTRS, PCEW, CPEW
      REAL    TRWL(NL),theTRW, ZRLT, ZZL,TKL(NL),ZRT
!-----Local variables
      INTEGER I
      !&SAVE

	  theTRW = TRW   !TAOLI 17 AUG, 2010
      TRW    = 0.
      LRSTRS = 1.
      LDSTRS = 1.
      LESTRS = 1.
      CPEW   = 1.
      PCEW   = 1.
	  ZZL=0.
	  IF(ZRT.EQ.0.0) THEN
		TRWL=0.0;TRWL(1)=THETRW
	  ELSE
		DO I=1,NL			
!        TRWL(I) = 0.
		!EQUAVLENT SPLIT TRC INTO ROOT ZONE, TAOLI, 17 AUG 2010
			ZRLT=MAX(0.0,MIN(ZRT-ZZL,TKL(I)))
			TRWL(I)=THETRW/ZRT*ZRLT
			PV%PTRWL(I)=TRWL(I)
			ZZL=ZZL+ZRLT 		 
		END DO
	  ENDIF
      RETURN
      END

