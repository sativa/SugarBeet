!=======================================================================
!  ORYZA_Interface, Subroutine
!  DSSAT interface for ORYZA2000 rice growth routine.
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  01/26/2011 TL/CHP Written.
!=======================================================================
      SUBROUTINE ORYZA_Interface (CONTROL, ISWITCH,         & !Input
          EOP, YREND, NH4, NO3, SNOW, SOILPROP,             & !Input
          SRFTEMP, ST, SW, TRWUP, WEATHER, YRPLT, HARVFRAC, & !Input
          CANHT, HARVRES, KCAN, KEP, MDATE, NSTRES,PORMIN,  & !Output
          RLV, RWUMX, SENESCE, STGDOY, UNH4, UNO3, XLAI)      !Output

      USE ModuleDefs
      USE ModuleData
      USE Public_Module		!VARIABLES
	  use RootGrowth

      IMPLICIT NONE
      SAVE

      CHARACTER*1   IDETG, IDETL, IDETO, IDETS, RNMODE
      CHARACTER*2   CROP
      CHARACTER*30  FILEIO
      CHARACTER*120 FILEIOCS

      INTEGER DOY1, DYNAMIC, RUN, TN, RUNI, RN, ON
      INTEGER REP, STEP, CN, YRHAR, YREND, YRDOY
      INTEGER MDATE, L, NLAYR
      INTEGER MULTI, FROP, SN, YEAR1
      INTEGER STGDOY(20), YRPLT

      REAL WUPT, EOP, EP, ET, TRWUP, SRAD, TMAX, TMIN, CO2
      REAL SNOW, KCAN, KEP, DEPMAX
      REAL NSTRES, XLAI, NFP, SLPF
      REAL DAYL, TWILEN, PORMIN, RAIN, RWUMX, SRFTEMP
      REAL CANHT, EO, TOTIR, WINDSP

      REAL, DIMENSION(NL) :: BD, DLAYR, DS, DUL, LL
      REAL, DIMENSION(NL) :: NH4, NO3, RLV, SAT, SHF
      REAL, DIMENSION(NL) :: ST, SW, UNO3, UNH4, UH2O
      REAL, DIMENSION(0:NL) :: SENC, SENN, SENLIG
      REAL, DIMENSION(0:NL) :: CRESC, CRESN, CRESLIG
      REAL, DIMENSION(0:NL) :: SOILTEMP
      REAL, DIMENSION(2)  :: HARVFRAC

      LOGICAL LEAP !Function in DATES subroutine

!-----Formal ORYZA parameters
      INTEGER       ITASK , IUNITD, IUNITL, CROPSTA, IDOY, I, NLO, TDATE
      LOGICAL       OR_OUTPUT, TERMNL
      CHARACTER (128) FILEI1, FILEIT, FILEI2
      CHARACTER (32) ESTAB
      REAL    YEAR, DOY , TIME, DELT  , LAT  , RDD, TRC, NFLV, NSLLV
      REAL    TMMN, TMMX, TKLT  , ZRTMS, LRSTRS, LDSTRS, LESTRS, NRT
      REAL    PCEW, CPEW, DAE , LAIROL, ZRT  , DVS, RNSTRS, WCL(10), WL0, DLDR
      REAL    LAI, LLV  , SLA , WLVG  , WST  , WSO, GSO, GGR, GST, GLV, PLTR 
      REAL    TRW, TRWL(10), TKL(10), WRT, WRR14

!     FOR EXPERIMENT FILE 
      CHARACTER*(128) OUTPUTFILE
      INTEGER YRSIM, EDATE, iPAGE, IRRCOD 
      CHARACTER*1, ISWWAT, ISWNIT, PLME,IIRRI
      REAL PLPH, PLTPOP, PLANTS, PLDP,IRRI, WL0MIN, KPAMIN, WLODAY, SLMIN
      REAL IRMTAB(300), RIRRIT(750),ISTAGET(900), TMCTB(750)

!     FILEIO data
      CHARACTER*1  PLDS
      INTEGER INCDAT
      REAL PAGE, ROWSPC, SDWTPL, ATEMP

      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP
      TYPE (SwitchType)  ISWITCH
      Type (ResidueType) HARVRES
      Type (ResidueType) SENESCE
      TYPE (WeatherType) WEATHER
      
      COMMON /FSECM1/ YEAR,DOY,IUNITD,IUNITL,TERMNL
      

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      CROP    = CONTROL % CROP
      FROP    = CONTROL % FROP
      MULTI   = CONTROL % MULTI
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

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

      TRC = EOP

      CALL YR_DOY(YRDOY, YEAR1, DOY1)

      !IF (DYNAMIC .EQ. RUNINIT .OR. DYNAMIC .EQ. SEASINIT) THEN
      IF (DYNAMIC .EQ. 1) THEN
        ALLOCATE(pv)                              !Added by TaoLi, 24 April 2011
        TN = 0
        RN = 0
        SN = 0
        ON = 0
        CN = 0  !Crop component
        REP = 1
        STEP = 1
        RUNI = 1
        TOTIR = 0.0
        ITASK = 1;TIME =0.0
      !ELSEIF (DYNAMIC == RATE) THEN
      ELSEIF (DYNAMIC == 2.OR. DYNAMIC ==3) THEN
        CALL GET('SPAM','EO',  EO)
        CALL GET('SPAM','EP',  EP)
        CALL GET('SPAM','UH2O',UH2O)
        ITASK = 2
      !ELSEIF (DYNAMIC == INTEGR) THEN
      ELSEIF (DYNAMIC == 4) THEN
        CALL GET('SPAM','ET',  ET)
        CALL Get('MGMT','TOTIR', TOTIR)
        ITASK = 3;TIME = TIME+DELT
      ELSE
        ITASK = 0
      ENDIF

      SOILTEMP(0) = SRFTEMP
      DO L = 1, NLAYR
        SOILTEMP(L) = ST(L)
      ENDDO

      CO2   = WEATHER % CO2
      DAYL  = WEATHER % DAYL 
      RAIN  = WEATHER % RAIN
      RDD   = WEATHER % SRAD*1000000.0
      TMMX  = WEATHER % TMAX
      TMMN  = WEATHER % TMIN
      TWILEN= WEATHER % TWILEN
      WINDSP= WEATHER % WINDSP
      LAT   = WEATHER % XLAT

!-----------------------------------------------------------------------
!    Read DSSAT cultivar and planting data from FILEIO
     CALL OR_IPRICE (CONTROL,                       &
          FILEI1, PLANTS, PLTPOP, PLME, PLDS,      &
          ROWSPC, PLDP, SDWTPL, PAGE, ATEMP, PLPH)
 
    IF(ITASK.EQ.1) THEN
        !initialize OBSSYS routine
        CALL OBSINI
        !get emergence date and transplanting date
        iPAGE = NINT(PAGE)
        EDATE = INCDAT(YRSIM,iPAGE)
        IF(INDEX(PLME,"T").GT.0) THEN
            CALL YR_DOY(EDATE, YRPLT,TDATE)
            TDATE = TDATE + IPAGE
            IF (LEAP(YRPLT)) THEN
                IF(TDATE.GT.366) THEN
                    YRPLT =YRPLT +1; TDATE = TDATE-366
                    TDATE = YRPLT *1000.0 + TDATE
                ELSE
                    TDATE = YRPLT *1000.0 + TDATE
                END IF
            ELSE
                IF(TDATE.GT.365) THEN
                    YRPLT =YRPLT +1; TDATE = TDATE-366
                    TDATE = YRPLT *1000.0 + TDATE
                ELSE
                    TDATE = YRPLT *1000.0 + TDATE
                END IF
            END IF
        END IF

!     Used STRING
!   ESSENTIAL INFORMATION MUST BE PROVIDED FROM UPPER LAYER
!   FILEI1 = 'D:\...\...\IR72.CRP
!   FILEIT = 'D:\...\...\...\N150.exp
    FILEIT = CONTROL % FILEX(1:8) // ".EXP"
    IIRRI  = ISWITCH % IIRRI
!    IRCOD  = 0  !Potential production  - need to get value for water limited. 
        !THE 'IRCOD', IT IS NOT USED IN THE ROUTINE YET, COMMENT IT OUT TEMPORARY.

        !GENERATE EXPERIMENT FILE
        CALL ExperimentFileEdit(FILEIT, YRSIM, EDATE,& 
                ISWWAT, ISWNIT, PLME, iPAGE, PLPH, PLTPOP, PLANTS, PLANTS, PLDP, &
                IIRRI, IRRCOD, IRMTAB, RIRRIT, IRRI, WL0MIN, KPAMIN, SLMIN, WLODAY, ISTAGET, TMCTB)
    
        TRW =TRC; OR_OUTPUT = .FALSE.; PV%PROOT_NUTRIENT = .FALSE.
        NLO = PV%PNL; TERMNL = .FALSE.

        CALL GETLUN("ORYZA1",IUNITD)
        CALL GETLUN("ORYZA2",IUNITL)
        IUNITD = IUNITD+10
        IUNITL = IUNITL+20
        FILEI2 = "" !;IUNITD = 30; IUNITL = 40
        DELT = 1.0  !TIME STEP IS 1.0
        IDOY = DOY
        DAE = 0.0
        DO I = 1, NLAYR
            PV%PDLAYER(I) = DLAYR(I)*10.0       !CONVERT LAYER THICKNESS FROM cm TO mm
            TKL(I) = PV%PDLAYER(I)/1000.0      !CONVERT SOIL LAYER THICKNESS FROM mm TO m
        END DO
    END IF

!   ESSENTIAL INFORMATION  
    CALL UPPERC(ISWWAT)
    IF(INDEX(ISWWAT, "N").GT.0) THEN        !POTENTIAL WATER CONDITION
        TRW = EP; TKLT = SUM(TKL); ZRTMS = TKLT                            !THE TOTAL TRANSPIRATION EQUALS TO POTENTIAL TRANSPIRATION
        CALL WNOSTRESS (NLAYR, TRW, TRWL, ZRT, TKL, LRSTRS, LDSTRS, LESTRS, PCEW, CPEW)
    END IF
  !-----Set CROPSTA: 0=before sowing; 1=day of sowing; 2=in seedbed;
!                  3=day of transplanting; 4=main growth period
!      IF (ITASK.EQ.1 .OR. ITASK.EQ.3) THEN
         IF(YRDOY.LT.EDATE) THEN
            CROPSTA = 0             !
         END IF
         IF (CROPSTA .EQ. 3) CROPSTA = 4
         IF (CROPSTA .EQ. 2) THEN
            IF (YRDOY.EQ.TDATE) CROPSTA = 3
         END IF
         IF (CROPSTA .EQ. 1) THEN
            IF (INDEX(PLME,"T").GT.0) THEN
               CROPSTA = 2
            ELSE 
               CROPSTA = 4
            END IF
        END IF
        IF (CROPSTA .EQ. 0) THEN
           IF (YRDOY.EQ.EDATE) CROPSTA = 1
        END IF
!    END IF
  
    DOY = REAL(DOY1);YEAR = real(YEAR1); IDOY = DOY1
    IF((CROPSTA.GE.1).AND.(ITASK.EQ.2)) DAE=DAE+1.0
    IF(.NOT.TERMNL) THEN
        CALL ORYZA1(ITASK,  IUNITD, IUNITL, FILEI1, FILEI2,FILEIT, &
                        OR_OUTPUT, TERMNL, IDOY  , DOY, &
                        TIME,   DELT,   LAT,    RDD,    TMMN,   TMMX, &
                        NFLV,   NSLLV,  NRT,	RNSTRS,                 &
                        ESTAB,  TKLT,   ZRTMS,  CROPSTA, &
                        LRSTRS, LDSTRS, LESTRS, PCEW,  CPEW, TRC, &
                        DAE,    SLA, LAI,    LAIROL, ZRT,    DVS, &
                        LLV,    DLDR, WLVG, WST, WSO, GSO, GGR, GST, GLV, &
                        PLTR, WCL, WL0, WRT, WRR14)
        CALL UPPERC(ISWNIT)

!       IF(INDEX(ISWNIT, "N").GT.0) THEN           !POTENTIAL NITROGEN CONDITION
            CALL NNOSTRESS2(DELT, IUNITD, IUNITL, ITASK, FILEI1, FILEIT, &
                           CROPSTA, DVS, WLVG, LAI, SLA, NFLV, NSLLV, RNSTRS)
!       ELSE
    
!       END IF
    ELSE
    !delete all temporary files
        CALL RDDTMP (IUNITD)
    ENDIF

!-----------------------------------------------------------------------
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
      IF(ITASK .EQ.3) ITASK = 2                
      IF (YREND.EQ.YRDOY .AND. DYNAMIC.EQ.INTEGR) THEN 
        !Transfer harvest residue from senescence variable to 
        !harvest residue variable on day of harvest.
        HARVRES = SENESCE
        SENESCE % ResWt  = 0.0
        SENESCE % ResLig = 0.0
        SENESCE % ResE   = 0.0
        DEALLOCATE(PV)      !Added by TaoLi, 24 April 2011
      ELSE
        MDATE = -99
      ENDIF

      RETURN
      END SUBROUTINE ORYZA_Interface

!=====================================================================

!=======================================================================
!  RI_IPGROSUB, Subroutine
!
!  Reads FILEIO for GROSUB routine
!  05/07/2002 CHP Written
!  08/12/2003 CHP Added I/O error checking
!=======================================================================
      SUBROUTINE OR_IPRICE (CONTROL,                       &
          FILEI1, PLANTS, PLTPOP, PLME, PLDS,      &
          ROWSPC, PLDP, SDWTPL, PAGE, ATEMP, PLPH)

      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT     NONE

      CHARACTER*1   PLME, PLDS
      CHARACTER*2   CROP
      CHARACTER*6   VARTY
      CHARACTER*20  VARNAME
      CHARACTER*30  FILEIO
      CHARACTER*78  MSG(2)
      CHARACTER*128 FILEI1

      CHARACTER*6  ERRKEY, SECTION
      PARAMETER (ERRKEY = 'IPRICE')

      INTEGER LINC, LNUM, LUNIO, ERR, FOUND
      REAL PLANTS, PLTPOP, ROWSPC, PLDP, SDWTPL, PAGE, ATEMP, PLPH

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL
      FILEIO = CONTROL % FILEIO
      LUNIO  = CONTROL % LUNIO

      LNUM = 0

!-----------------------------------------------------------------------
!       Read data from FILEIO for use in PLANT module
!-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)

!-----------------------------------------------------------------------
!    Read Cultivars Section
!-----------------------------------------------------------------------
!C CR INGENO CNAME
!  RI IB0118 IR 72   
        
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO, LNUM)
      READ(LUNIO,'(3X,A2,1X,A6,1X,A20)', IOSTAT=ERR) CROP, VARTY, VARNAME ; LNUM = LNUM + 1
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

      SELECT CASE(VARTY)
      CASE ('IB0118'); FILEI1 = 'IR72.CRP'
      CASE DEFAULT
        MSG(1) = 'Invalid ORYZA cultivar found.'
        MSG(2) = 'Program will stop.'
        CALL WARNING(2, ERRKEY, MSG)
        CALL ERROR(ERRKEY,36,FILEIO,LNUM)
      END SELECT

!-----------------------------------------------------------------------
!    Read Planting Details Section
!-----------------------------------------------------------------------
!P   PDATE   EDATE  PPOP  PPOE  PLME  PLDS  PLRS  PLRD  PLDP  PLWT  PAGE  PENV  PLPH  SPRL
!  1992195     -99 999.0 125.0     T     H   20.    0.   5.0    0.   12.  25.0   5.0   0.0

      SECTION = '*PLANT'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO, LNUM)
      READ (LUNIO,70, IOSTAT=ERR) PLANTS, PLTPOP, PLME, PLDS, ROWSPC, PLDP, SDWTPL, PAGE, ATEMP, PLPH
   70 FORMAT (18X,2F6.0,2(5X,A1),F6.0,6X,5F6.0)
      LNUM = LNUM + 1
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

      ROWSPC = ROWSPC / 100.0

      CLOSE (LUNIO)
      RETURN
      END SUBROUTINE OR_IPRICE
!=======================================================================
