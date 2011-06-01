!=======================================================================
!  ORYZA_Interface, Subroutine
!  DSSAT interface for ORYZA2000 rice growth routine.
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  01/26/2011 TL/CHP Written.
!=======================================================================
      SUBROUTINE ORYZA_Interface (CONTROL, ISWITCH,               &    !Input
          EOP, YREND, SOILPROP, TRWUP, WEATHER, YRPLT, HARVFRAC,  &    !Input
          CANHT, HARVRES, KCAN, KEP, MDATE, NSTRES, PORMIN,       &    !Output
          RWUMX, SENESCE, STGDOY, XLAI)                                !Output

!   Variables to be introduced with water
!         FLOODWAT, SW, SRFTEMP, RLV, !Input
!   Variables to be introduced with N
!         NH4, NO3, UNH4, UNO3, 
!         FLOODN,                                                &    !I/O

      USE ModuleDefs
      USE ModuleData
      USE Public_Module		!VARIABLES
	  use RootGrowth

      IMPLICIT NONE
      SAVE

      CHARACTER*30  FILEIO
      CHARACTER*120 FILEIOCS

      INTEGER DOY1, DYNAMIC, TN, RUNI, RN, ON
      INTEGER REP, STEP, CN, YRHAR, YREND, YRDOY
      INTEGER MDATE, L, NLAYR
      INTEGER SN, YEAR1
      INTEGER STGDOY(20), YRPLT

      REAL WUPT, EOP, EP, ET, TRWUP
      REAL KCAN, KEP, DEPMAX
      REAL NSTRES, XLAI, NFP
      REAL PORMIN, RWUMX
      REAL CANHT, TOTIR

      REAL, DIMENSION(NL) :: DLAYR
      REAL, DIMENSION(0:NL) :: SENC, SENN, SENLIG
      REAL, DIMENSION(0:NL) :: CRESC, CRESN, CRESLIG
      REAL, DIMENSION(2)  :: HARVFRAC

!     Soil water
!     REAL EO, SRFTEMP, SLPF
!     REAL, DIMENSION(NL) :: RLV, SW, ST, SOILTEMP   
!     REAL, DIMENSION(NL) :: BD, DUL, LL, SAT, SHF
!     Soil N
!     REAL, DIMENSION(0:NL) :: NH4, NO3, UNO3, UNH4, UH2O

!     LOGICAL LEAP !Function in DATES subroutine

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
      REAL    HU, WAGT, WLVD, WRR, NGR

!     FOR EXPERIMENT FILE 
!      CHARACTER*(128) OUTPUTFILE
      INTEGER YRSIM, EDATE, iPAGE, IRRCOD 
      CHARACTER*1, ISWWAT, ISWNIT, PLME,IIRRI
      REAL PLPH, PLTPOP, PLANTS, PLDP,IRRI, WL0MIN, KPAMIN, WLODAY, SLMIN
      REAL IRMTAB(300), RIRRIT(750),ISTAGET(900), TMCTB(750)

!     FILEIO data
      CHARACTER*1  PLDS
      INTEGER INCDAT, YEAR_PLT
      REAL PAGE, ROWSPC, SDWTPL, ATEMP

!     Output data needed for DSSAT output files

      CHARACTER*10 STNAME(20) 
      INTEGER ISDATE, ISTAGE 

!      REAL AGEFAC, APTNUP, CANNAA, CANWAA, DYIELD, G2, GNUP, GPP
!      REAL GPSM, GRAINN, GRNWT, LEAFNO, MAXLAI, PANWT, PBIOMS, PSTRES1, PSTRES2
!      REAL SKERWT, STOVER, STOVN, SWFAC, TILNO, TOTNUP, TURFAC, XGNP, BWAH
!      REAL PODWT, SDWT, SDWTAH, TOPWT, WTNSD

      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP
      TYPE (SwitchType)  ISWITCH
      Type (ResidueType) HARVRES
      Type (ResidueType) SENESCE
      TYPE (WeatherType) WEATHER
      
      COMMON /FSECM1/ YEAR,DOY,IUNITD,IUNITL,TERMNL
      
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY
      CALL YR_DOY(YRDOY, YEAR1, DOY1)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per planting season
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN

        ITASK = 1
        TIME =0.0
        ALLOCATE(pv)                              !Added by TaoLi, 24 April 2011

!       Variables required by DSSAT for root water uptake calculations
        PORMIN = 0.0  !Minimum pore space required for supplying oxygen to roots for 
!                      optimal growth and function (cm3/cm3)
        RWUMX  = 0.03 !Maximum water uptake per unit root length, constrained by soil 
!                      water (cm3[water] / cm [root])

        FILEIO  = CONTROL % FILEIO
        YRSIM   = CONTROL % YRSIM
      
        DLAYR  = SOILPROP % DLAYR  
        NLAYR  = SOILPROP % NLAYR  
        DEPMAX = SOILPROP % DS(NLAYR)

        ISWWAT = ISWITCH % ISWWAT

!       BD     = SOILPROP % BD     
!       DUL    = SOILPROP % DUL    
!       LL     = SOILPROP % LL     
!       SAT    = SOILPROP % SAT    
!       SHF    = SOILPROP % WR
!       SLPF   = SOILPROP % SLPF

        FILEIOCS(1:30) = FILEIO
        TN = 0
        RN = 0
        SN = 0
        ON = 0
        CN = 0  !Crop component
        REP = 1
        STEP = 1
        RUNI = 1
        TOTIR = 0.0

        HU = 0.0
        WAGT = 0.0
        WLVD = 0.0
        WRR = 0.0

!       Water & N stresses
        LRSTRS = 1.0
        LDSTRS = 1.0
        LESTRS = 1.0
        PCEW   = 1.0
        CPEW   = 1.0

        CANHT = 0.0   !Canopy height
        KCAN  = 0.85  !Canopy light extinction coef
        KEP   = 1.0   !Energy extinction coef

!       Read DSSAT cultivar and planting data from FILEIO
        CALL OR_IPRICE (CONTROL,                       &
          FILEI1, PLANTS, PLTPOP, PLME, PLDS,      &
          ROWSPC, PLDP, SDWTPL, PAGE, ATEMP, PLPH, &
          STGDOY, STNAME)
 
        STGDOY(14) = YRDOY !start of simulation
        ISTAGE = 14

!----------------------------------------------------------------
!       ORYZA initialization section - moved up to initialization section
!     IF(ITASK.EQ.1) THEN

        !initialize OBSSYS routine
        CALL OBSINI

!        !get emergence date and transplanting date
!        iPAGE = NINT(PAGE)
!        EDATE = INCDAT(YRSIM,iPAGE)
!        IF(INDEX(PLME,"T").GT.0) THEN
!            CALL YR_DOY(EDATE, YRPLT,TDATE)
!            TDATE = TDATE + IPAGE
!            TDATE = INCDAT(YRPLT,iPAGE)
!        END IF

        !get emergence date and transplanting date
        iPAGE = NINT(PAGE)
        EDATE = INCDAT(YRPLT,-iPAGE)
        IF(INDEX(PLME,"T").GT.0) THEN
            CALL YR_DOY(YRPLT, YEAR_PLT, TDATE)
        END IF

!        TDATE = 3274
!        YRPLT = 1992
        EDATE = 1992195
!        YRPLT = 1992195

!       Used STRING
!       ESSENTIAL INFORMATION MUST BE PROVIDED FROM UPPER LAYER
!       FILEI1 = 'D:\...\...\IR72.CRP
!       FILEIT = 'D:\...\...\...\N150.exp
        FILEIT = CONTROL % FILEX(1:8) // ".EXP"
        IIRRI  = ISWITCH % IIRRI
!       IRCOD  = 0  !Potential production  - need to get value for water limited. 
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
!    END IF
!----------------------------------------------------------------


!***********************************************************************
!***********************************************************************
!     Rate - daily
!***********************************************************************
      ELSEIF (DYNAMIC == RATE) THEN

        ITASK = 2

        CALL GET('SPAM','EP',  EP)
!       CALL GET('SPAM','EO',  EO)
!       CALL GET('SPAM','UH2O',UH2O)

!        SOILTEMP(0) = SRFTEMP
!        DO L = 1, NLAYR
!          SOILTEMP(L) = ST(L)
!        ENDDO

        RDD   = WEATHER % SRAD*1000000.0
        TMMX  = WEATHER % TMAX
        TMMN  = WEATHER % TMIN
        LAT   = WEATHER % XLAT

!-----Set CROPSTA: 0=before sowing; 1=day of sowing; 2=in seedbed;
!                  3=day of transplanting; 4=main growth period
!      IF (ITASK.EQ.1 .OR. ITASK.EQ.3) THEN

             IF (YRDOY == YRPLT) THEN
               STGDOY(7) = YRDOY
               ISTAGE = 7
             ENDIF

             IF(YRDOY.LT.EDATE) THEN
                CROPSTA = 0             
             END IF

             IF (CROPSTA .EQ. 3) CROPSTA = 4

             IF (CROPSTA .EQ. 2) THEN
                IF (YRDOY.EQ.TDATE) THEN
                   CROPSTA = 3
!                  Transplant
                   STGDOY(11) = YRDOY
                   ISTAGE = 11
                ENDIF
             END IF

             IF (CROPSTA .EQ. 1) THEN
                IF (INDEX(PLME,"T").GT.0) THEN
                   CROPSTA = 2
                ELSE 
                   CROPSTA = 4
                END IF
            END IF

            IF (CROPSTA .EQ. 0) THEN
               IF (YRDOY.EQ.EDATE) THEN
                   CROPSTA = 1
!                  Emergence
                   STGDOY(9) = YRDOY
                   ISTAGE = 9
                ENDIF
            END IF
    !    END IF

!       IF((CROPSTA.GE.1).AND.(ITASK.EQ.2)) DAE=DAE+1.0
        IF (CROPSTA.GE.1) DAE=DAE+1.0
      
        DOY = REAL(DOY1);YEAR = real(YEAR1); IDOY = DOY1
    
!       Check for potential production condition  
        CALL UPPERC(ISWWAT)
        IF(INDEX(ISWWAT, "N").GT.0) THEN              !POTENTIAL WATER CONDITION
            TRW = EP; TKLT = SUM(TKL); ZRTMS = TKLT   !THE TOTAL TRANSPIRATION EQUALS TO POTENTIAL TRANSPIRATION
            CALL WNOSTRESS (NLAYR, TRW, TRWL, ZRT, TKL, LRSTRS, LDSTRS, LESTRS, PCEW, CPEW)
        END IF

        CALL GET('SPAM','ET',  ET)
        CALL Get('MGMT','TOTIR', TOTIR)

!***********************************************************************
!***********************************************************************
!     Integration - daily
!***********************************************************************
     ELSEIF (DYNAMIC == INTEGR) THEN

        ITASK = 3
        TIME = TIME+DELT

        YRHAR = YREND
        WUPT  = TRWUP
        TRC = EOP


!***********************************************************************
!***********************************************************************
!     Daily or seasonal output
!***********************************************************************
      ELSE

        ITASK = 0

        IF (DYNAMIC == SEASEND) THEN
          STGDOY(20) = YRDOY
          ISTAGE = 20
          YREND = YRDOY
          TERMNL = .TRUE.
          DEALLOCATE(PV) 
          CALL RDDTMP (IUNITD)  !delete all temporary files
        ENDIF

      ENDIF

!***********************************************************************
!***********************************************************************

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

!   ***** Need to output
!   HU - heat units
!   NGR - number of grains / ha (or m2?)

!   Panicle initiation date
!   Anthesis date
!   Physiological maturity date  

        CALL UPPERC(ISWNIT)
!       IF(INDEX(ISWNIT, "N").GT.0) THEN           !POTENTIAL NITROGEN CONDITION
            CALL NNOSTRESS2(DELT, IUNITD, IUNITL, ITASK, FILEI1, FILEIT, &
                           CROPSTA, DVS, WLVG, LAI, SLA, NFLV, NSLLV, RNSTRS)
!       ELSE
    
!       END IF

        XLAI   = LAI
        NSTRES = NFP
        WAGT = WST + WLVG + WSO + WLVD
        WRR  = WRR14 * 0.86

        IF (DVS > 0.65 .AND. STGDOY(2) > YRDOY) THEN
          STGDOY(2) = YRDOY
        ELSEIF (DVS >= 1.9999999 .AND. STGDOY(6) > YRDOY) THEN
          STGDOY(6) = YRDOY
          MDATE = YRDOY
        ENDIF 
      
        IF (DVS > 1.9999999) THEN
          MDATE = YRDOY
          YREND = YRDOY
        ENDIF

!        if (DYNAMIC == INTEGR) then
!          IF (DVS > 0.00000001) THEN
!            WRITE(500,*) YRDOY, XLAI
!          ENDIF
!
!          DO L=0, NLAYR
!            SENESCE % ResWt(L)  = (SENC(L) + CRESC(L)) / 0.40
!            SENESCE % ResLig(L) = SENLIG(L) + CRESLIG(L)
!            SENESCE % ResE(L,1) = SENN(L) + CRESN(L)
!          ENDDO
!
!        endif
!
!        IF (YREND == YRDOY .AND. DYNAMIC == INTEGR) THEN 
!          !Transfer harvest residue from senescence variable to 
!          !harvest residue variable on day of harvest.
!          HARVRES = SENESCE
!          SENESCE % ResWt  = 0.0
!          SENESCE % ResLig = 0.0
!          SENESCE % ResE   = 0.0
!        ELSE
!          MDATE = -99
!        ENDIF

    ENDIF

    SELECT CASE(DYNAMIC)
    CASE(SEASINIT, OUTPUT, SEASEND)
      CALL OR_OPGROW (CONTROL, ISWITCH, SOILPROP,  &
         CPEW, DVS, HU, LAI, LDSTRS, LESTRS, LRSTRS,     &
         NFLV, NGR, NSLLV, PCEW, RDCL, SLA,              &
         WAGT, WLVD, WLVG, WRR, WRT, WSO, WST, YRPLT, ZRT)
    END SELECT

!-----------------------------------------------------------------------

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
      SUBROUTINE OR_IPRICE (CONTROL,               &
          FILEI1, PLANTS, PLTPOP, PLME, PLDS,      &
          ROWSPC, PLDP, SDWTPL, PAGE, ATEMP, PLPH, &
          STGDOY, STNAME)

      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT     NONE

      CHARACTER*1   PLME, PLDS
      CHARACTER*2   CROP
      CHARACTER*6   VARTY
      CHARACTER*10  STNAME(20)     
      CHARACTER*20  VARNAME
      CHARACTER*30  FILEIO
      CHARACTER*78  MSG(2)
      CHARACTER*128 FILEI1

      CHARACTER*6  ERRKEY, SECTION
      PARAMETER (ERRKEY = 'IPRICE')

      INTEGER LINC, LNUM, LUNIO, ERR, FOUND, STGDOY(20)
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

      CLOSE (LUNIO)

      ROWSPC = ROWSPC / 100.0

!     ORYZA life cycle:
!      DVS  Stage
!     0.00  Emergence 
!     0.40  Start of photoperiod sensitive phase
!     0.65  Panicle initiation
!     1.00  50% flowering
!     2.00  Physiological maturity

!     Stages from CERES-Rice:  (X indicates that stages are transferred to DSSAT
      STNAME(1)  = 'End Juveni'
      STNAME(2)  = 'Pan Init  '  !DVS = 0.65  !X
      STNAME(3)  = 'Heading   '
      STNAME(4)  = 'Beg Gr Fil'
      STNAME(5)  = 'End Mn Fil'
      STNAME(6)  = 'Maturity  '  !DVS = 2.0  !X
      STNAME(7)  = 'Sowing    '  !X
      STNAME(8)  = 'Germinate '
      STNAME(9)  = 'Emergence '  !DVS = 0.0  !X
      STNAME(10) = 'Prgerm Sow'
      STNAME(11) = 'Transplant'  !X
      STNAME(12) = 'End Ti Fil'
      STNAME(13) = '          '
      STNAME(14) = 'Start Sim '  !X
      STNAME(15) = '          '
      STNAME(16) = '          '
      STNAME(17) = '          '
      STNAME(18) = '          '
      STNAME(19) = '          '
      STNAME(20) = 'Harvest   '

      STGDOY     = 9999999  

       RETURN
       END SUBROUTINE OR_IPRICE
 !=======================================================================
