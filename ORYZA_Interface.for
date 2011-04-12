!=======================================================================
!  ORYZA_Interface, Subroutine
!  DSSAT interface for ORYZA2000 rice growth routine.
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  01/26/2011 TL/CHP Written.
!=======================================================================
      SUBROUTINE ORYZA_Interface (CONTROL, ISWITCH,      !Input
     &     EOP, YREND, NH4, NO3, SNOW, SOILPROP,           !Input
     &     SRFTEMP, ST, SW, TRWUP, WEATHER, YRPLT, HARVFRAC,!Input
     &     CANHT, HARVRES, KCAN, KEP, MDATE, NSTRES,PORMIN,!Output
     &     RLV, RWUMX, SENESCE, STGDOY, UNH4, UNO3, XLAI)  !Output

      USE ModuleDefs
      USE ModuleData
      IMPLICIT NONE
      SAVE

      CHARACTER*1   IDETG, IDETL, IDETO, IDETS, ISWWAT, ISWNIT, RNMODE
      CHARACTER*2   CROP
      CHARACTER*30  FILEIO
      CHARACTER*120 FILEIOCS

      INTEGER DYNAMIC, RUN, TN, RUNI, RN, ON
      INTEGER REP, STEP, CN, YRHAR, YREND, YRDOY
      INTEGER MDATE, L, NLAYR
      INTEGER MULTI, FROP, SN, YEAR, DOY
      INTEGER STGDOY(20), YEARPLT, YRPLT

      REAL WUPT, EOP, EP, ET, TRWUP, SRAD, TMAX, TMIN, CO2
      REAL SNOW, KCAN, KEP, DEPMAX
      REAL NSTRES, XLAI, LAI, NFP, SLPF
      REAL DAYL, TWILEN, PORMIN, RAIN, RWUMX, SRFTEMP
      REAL CANHT, EO, TOTIR, WINDSP

      REAL, DIMENSION(NL) :: BD, DLAYR, DS, DUL, LL
      REAL, DIMENSION(NL) :: NH4, NO3, RLV, SAT, SHF
      REAL, DIMENSION(NL) :: ST, SW, UNO3, UNH4, UH2O
      REAL, DIMENSION(0:NL) :: SENC, SENN, SENLIG
      REAL, DIMENSION(0:NL) :: CRESC, CRESN, CRESLIG
      REAL, DIMENSION(0:NL) :: SOILTEMP
      REAL, DIMENSION(2)  :: HARVFRAC

!     ORYZA2000 variables:
      INTEGER ITASK, IUNITD, IUNITL, IDOY, CROPSTA, DAE
      CHARACTER*80 FILEI1, FILEI2, FILEIT, ESTAB
      REAL TIME, DELT, LAT, RDD, TMMN, TMMX
      REAL NFLF, NSLLV, NRT, RNSTRS, TKLT, ZRTMS
      REAL LRSTRS, LDSTRS, LESTRS, PCEW, CPEW, TRC, SLA
      REAL NFLV, LAIROL, ZRT, DVS, LLV, DLDR, WLVG, WST
      REAL WSO, GSO, GGR, GST, GLV, PLTR, WCL, WL0

      LOGICAL TERMNL

      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP
      TYPE (SwitchType)  ISWITCH
      Type (ResidueType) HARVRES
      Type (ResidueType) SENESCE
      TYPE (WeatherType) WEATHER

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      CROP    = CONTROL % CROP
      FROP    = CONTROL % FROP
      MULTI   = CONTROL % MULTI
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

      ISWWAT  = ISWITCH % ISWWAT
      ISWNIT  = ISWITCH % ISWNIT
      IDETG   = ISWITCH % IDETG
      IDETL   = ISWITCH % IDETL
      IDETO   = ISWITCH % IDETO
      IDETS   = ISWITCH % IDETS

      BD     = SOILPROP % BD     
      DLAYR  = SOILPROP % DLAYR  
      DS     = SOILPROP % DS    
      DUL    = SOILPROP % DUL    
      LL     = SOILPROP % LL     
      NLAYR  = SOILPROP % NLAYR  
      SAT    = SOILPROP % SAT    
      SHF    = SOILPROP % WR
      SLPF   = SOILPROP % SLPF

      YRHAR = YREND
      WUPT  = TRWUP

      DEPMAX = DS(NLAYR)

      FILEIOCS(1:30) = FILEIO

      CALL YR_DOY(YRDOY, YEAR, DOY)

      IF (DYNAMIC .EQ. RUNINIT .OR. DYNAMIC .EQ. SEASINIT) THEN
        TN = 0
        RN = 0
        SN = 0
        ON = 0
        CN = 0  !Crop component
        REP = 1
        STEP = 1
        RUNI = 1
        TOTIR = 0.0
      ELSEIF (DYNAMIC == RATE) THEN
        CALL GET('SPAM','EO',  EO)
        CALL GET('SPAM','EP',  EP)
        CALL GET('SPAM','UH2O',UH2O)
      ELSEIF (DYNAMIC == INTEGR) THEN
        CALL GET('SPAM','ET',  ET)
        CALL Get('MGMT','TOTIR', TOTIR)
      ENDIF

      SOILTEMP(0) = SRFTEMP
      DO L = 1, NLAYR
        SOILTEMP(L) = ST(L)
      ENDDO

      CO2   = WEATHER % CO2
      DAYL  = WEATHER % DAYL 
      RAIN  = WEATHER % RAIN
      SRAD  = WEATHER % SRAD
      TMAX  = WEATHER % TMAX
      TMIN  = WEATHER % TMIN
      TWILEN= WEATHER % TWILEN
      WINDSP= WEATHER % WINDSP

!-----------------------------------------------------------------------
      CALL ORYZA1(ITASK,  IUNITD, IUNITL, FILEI1, FILEI2,FILEIT, 
     &                   OUTPUT, TERMNL, IDOY  , DOY, 
     &                   TIME,   DELT,   LAT,    RDD,    TMMN,   TMMX, 
     &                   NFLV,   NSLLV,  NRT,	RNSTRS,                 
     &                   ESTAB,  TKLT,   ZRTMS,  CROPSTA, 
     &                   LRSTRS, LDSTRS, LESTRS, PCEW,  CPEW, TRC, 
     &                   DAE,    SLA, LAI,    LAIROL, ZRT,    DVS, 
     &                   LLV,    DLDR, WLVG, WST, WSO, GSO, GGR, GST, 
     &                   GLV, PLTR, WCL, WL0)
!      KCAN   = KPAR
!      KEP    = KSRAD
      XLAI   = LAI
      NSTRES = NFP

      IF (STGDOY(11).EQ.YRDOY) THEN
        MDATE = YRDOY
        YREND = YRDOY
      ENDIF 

      if (dynamic .eq. integr) then
        DO L=0, NLAYR
          SENESCE % ResWt(L)  = (SENC(L) + CRESC(L)) / 0.40
          SENESCE % ResLig(L) = SENLIG(L) + CRESLIG(L)
          SENESCE % ResE(L,1) = SENN(L) + CRESN(L)
        ENDDO
      endif
                      
      IF (YREND.EQ.YRDOY .AND. DYNAMIC.EQ.INTEGR) THEN 
        !Transfer harvest residue from senescence variable to 
        !harvest residue variable on day of harvest.
        HARVRES = SENESCE
        SENESCE % ResWt  = 0.0
        SENESCE % ResLig = 0.0
        SENESCE % ResE   = 0.0
      ELSE
        MDATE = -99
      ENDIF

      RETURN
      END SUBROUTINE ORYZA_Interface
