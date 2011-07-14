!=======================================================================
!  OR_OPGROW, Subroutine
!
!  Generates output for simulated data
!-----------------------------------------------------------------------
!  Revision history
!  05/26/2011 chp adapted for ORYZA2000
!=======================================================================

      SUBROUTINE OR_OPGROW (CONTROL, ISWITCH, SOILPROP,  &
         CPEW, DVS, HU, LAI, LDSTRS, LESTRS, LRSTRS,     &
         NFLV, NGR, NSLLV, PCEW, RDCL,               &
         WAGT, WLVD, WLVG, WRR, WRT, WSO, WST, YRPLT, ZRT)

!-----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT  NONE
      SAVE

      CHARACTER*8   CHAR8
      CHARACTER*30  LayerText
      CHARACTER*220 GROHEAD(4)  !, NITHEAD

      INTEGER DAP, DAS, YRDOY, YRPLT

      CHARACTER*1 IDETG, ISWNIT
      CHARACTER*12 OUTG, OUTPN
      INTEGER DYNAMIC, NOUTDG, FROP, I, J, L, NLAYR
      INTEGER MDATE, NOUTPN, RUN, ERRNUM, TIMDIF, YEAR, DOY

      REAL CPEW, DVS, HU, LAI, LDSTRS, LESTRS, LRSTRS,     &
         NFLV, NSLLV, PCEW,                                &
         WAGT, WLVD, WLVG, WRR, WRT, WSO, WST, ZRT 
      REAL, DIMENSION(10) :: RDCL

      REAL NGR, HIAD, HWUD, SLAD, SEEDNO
      REAL CUMSENSURF, CUMSENSOIL 

      LOGICAL FEXIST, FIRST

      TYPE (ResidueType) SENESCE
      TYPE (SoilType) SOILPROP

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH

!     Transfer values from constructed data types into local variables.
      IDETG  = ISWITCH % IDETG
      IF (IDETG .NE. 'Y') RETURN
      ISWNIT = ISWITCH % ISWNIT

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FROP    = CONTROL % FROP
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      IF (IDETG .EQ. 'Y') THEN
        OUTG  = 'PlantGro.OUT'
        CALL GETLUN('OUTG',  NOUTDG)
        OUTPN  = 'PlantN.OUT  '
        CALL GETLUN('OUTPN', NOUTPN)

!       Text describing soil layer depths
        LayerText = "                              "
        DO L = 1, MIN(5,NLAYR)
          SELECT CASE (L)
          CASE (1:4);CHAR8 = SoilProp % LayerText(L)
!         LayerText(11) is for layers 5 thru NLAYR
          CASE (5);  CHAR8 = SoilProp % LayerText(11)
          END SELECT
          I = (L-1)*6+1
          J = L*6
          LayerText(I:J) = CHAR8(3:8)
        ENDDO

      GROHEAD(1) = '!YR        Days  Days        Leaf  <------------- Dry Weight -------------> Grain Grain        <--------- Stress (0-1) --------->  Leaf   Spec  Root<Senesced Mat>   Phot'                                                   
      GROHEAD(2) = '!  and    after after  Grow  Area  <---------------- kg/Ha --------------->   per    wt  Harv  <--------- Water ---------->         Nit   Leaf Depth <--(kg/ha)-->  Therm'                                                   
      GROHEAD(3) = '!     DOY start plant Stage Index  Leaf  Leaf  Stem Grain  Root Panic  Crop    m2     g  Indx  Phot  Grow Trans  Roll Death  Nitr    %    Area    m    Surf   Soil   Days'                                                   
      GROHEAD(4) = '@YEAR DOY   DAS   DAP  GSTD  LAID  LWAD  LDAD  SWAD  GWAD  RWAD  EWAD  CWAD  G#AD  HWUD  HIAD  WSPD  WSGD  WFTD  WFRD  WFDD  NSTD  LN%D   SLAD  RDPD  SNW0C  SNW1C   DTTD'                                                   

!!-----------------------------------------------------------------------
!      NITHEAD = '@YEAR DOY   DAS   DAP  CNAD  GNAD  VNAD  GN%D  VN%D  NUPC  LNAD  SNAD  LN%D  SN%D  SHND  RN%D  SNN0C  SNN1C'

!-----------------------------------------------------------------------
!       Initialize daily growth output file
        INQUIRE (FILE = OUTG, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTDG, FILE = OUTG, STATUS = 'OLD',IOSTAT = ERRNUM, POSITION = 'APPEND')
          FIRST = .FALSE.  
        ELSE
          OPEN (UNIT = NOUTDG, FILE = OUTG, STATUS = 'NEW',IOSTAT = ERRNUM)
          WRITE(NOUTDG,'("*GROWTH ASPECTS OUTPUT FILE")')
          FIRST = .TRUE.  
        ENDIF

        !Write headers
        CALL HEADER(SEASINIT, NOUTDG, RUN)

!       Variable heading for GROWTH.OUT
        WRITE (NOUTDG,2192) GROHEAD(1)
        WRITE (NOUTDG,2192) GROHEAD(2)
        WRITE (NOUTDG,2192) GROHEAD(3)
        WRITE (NOUTDG,2192) GROHEAD(4)
 2192   FORMAT (A220)

!!-----------------------------------------------------------------------
!!     Initialize daily plant nitrogen output file
!        IF (ISWNIT .EQ. 'Y') THEN
!          INQUIRE (FILE = OUTPN, EXIST = FEXIST)
!          IF (FEXIST) THEN
!            OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS = 'OLD',
!     &        IOSTAT = ERRNUM, POSITION = 'APPEND')
!            FIRST = .FALSE.  
!          ELSE
!            OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS = 'NEW',
!     &        IOSTAT = ERRNUM)
!            WRITE(NOUTPN,'("*PLANT N OUTPUT FILE")')
!            FIRST = .TRUE.  
!          ENDIF
!
!          !Write headers
!          CALL HEADER(SEASINIT, NOUTPN, RUN)
!
!          WRITE (NOUTPN,2240) NITHEAD
! 2240     FORMAT (A140)
!        ENDIF
      ENDIF

!-----------------------------------------------------------------------

      CUMSENSURF  = 0.0
      CUMSENSOIL  = 0.0

!      WTNUP = 0.0
!      WTNST    = 0.0 
!      WTNSD    = 0.0 
!      WTNSH    = 0.0 
!      WTNRT    = 0.0 

!      CUMSENSURFN = 0.0
!      CUMSENSOILN = 0.0   

!***********************************************************************
!***********************************************************************
!     Daily OUTPUT
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
      IF (YRDOY .LT. YRPLT .AND. YRPLT .GT. 0) RETURN

!     Accumulate senesced matter for surface and soil.
      DO L = 1, 10
        SENESCE % ResWt(L) = RDCL(L)
        CUMSENSOIL  = CUMSENSOIL  + RDCL(L)
      ENDDO

!-----------------------------------------------------------------------
!     Check for output frequency
      IF ((MOD(DAS,FROP) .EQ. 0)    &      !Daily output every FROP days,
       .OR. (YRDOY .EQ. YRPLT)      &      !on planting date, and
       .OR. (YRDOY .EQ. MDATE)) THEN       !at harvest maturity 

!-----------------------------------------------------------------------
!        The following generates output for PlantGro.OUT
!-----------------------------------------------------------------------
!        LFWT = WTLF / PLTPOP  !do we need this?

        SEEDNO  = NGR / 1.E4    !#/m2 

        IF (NGR > 0.0) THEN
          HWUD = WRR / NGR * 1.E3  !g/grain
        ELSE
          HWUD = 0.0
        ENDIF

        IF (WAGT > 0.0 .AND. WRR > 0.0) THEN
          HIAD = WRR / WAGT
        ELSE
          HIAD = 0.0
        ENDIF 

        IF (WLVG > 1.E-6) THEN
          SLAD = LAI / WLVG * 1.E5  !cm2/g
        ELSE
          SLAD = 0.0
        ENDIF

        DAP = MAX(0,TIMDIF(YRPLT,YRDOY))
        IF (DAP > DAS) DAP = 0
        CALL YR_DOY(YRDOY, YEAR, DOY)

!        WRITE (NOUTDG,400)YEAR, DOY, DAS, DAP,VSTAGE,RSTAGE,LAI,         &
!            NINT(WTLF*10),NINT(STMWT*GM2KG),NINT(GRNWT*GM2KG),           &
!            NINT(RTWT*GM2KG),NINT(PANWT*GM2KG),NINT(BIOMAS*GM2KG),       &
!            NINT(SEEDNO),SDSIZE,HI,NINT((TILNO+1.)*PLTPOP),(1.0-SWFAC),  &
!            (1.0-TURFAC),SATFAC,(1.0-NSTRES),(1.0-KSTRES),PCNL,SHELPC,   &
!            SLA,CANHT,CANWH,ZRT,(RLV(I),I=1,5),                          &
!            NINT(CUMSENSURF), NINT(CUMSENSOIL), DTT

        WRITE (NOUTDG,400) YEAR, DOY, DAS, DAP,                    &
          DVS, LAI, NINT(WLVG), NINT(WLVD), NINT(WST), NINT(WRR),  &
          NINT(WRT), NINT(WSO), NINT(WAGT), NINT(SEEDNO), HWUD, HIAD, &
          1.0 - PCEW, 1.0 - LESTRS, 1.0 - CPEW, 1.0 - LRSTRS, 1.0 - LDSTRS, 1.0 - NSLLV,               &
          NFLV, SLAD, ZRT, NINT(WLVD), NINT(CUMSENSOIL), HU                        
 400    FORMAT (1X,I4, 1X,I3.3, 2(1X,I5), &
             1X,F5.3, 1X,F5.2, 4(1X,I5),  &
             4(1X,I5), 1X,F5.1, F6.3,     &
             6F6.3,                       &
             F6.2, F7.1, F6.2, 2I7, F7.2)

!!-----------------------------------------------------------------------
!!        The following generates output for file PlantN.OUT
!!-----------------------------------------------------------------------
!        IF (ISWNIT .EQ. 'Y') THEN
!          WTNCAN = (STOVN + GRAINN) * PLTPOP
!          IF ((LFWT+STMWT) .GT. 0.0) THEN
!             WTNST = STOVN * (STMWT / (LFWT + STMWT)) * PLTPOP
!          ENDIF
!          WTNSD = GRAINN * PLTPOP
!          WTNRT = ROOTN * PLTPOP        ! Is this right?
!          WTNSH = 0.0
!          WTNUP = (STOVN+GRAINN)*PLANTS
!          WTNFX = 0.0
!          NFIXN = 0.0
!
!          IF (LFWT .GT. 0.0) THEN
!             PCNL = WTNLF/(LFWT * PLTPOP) * 100.0
!           ELSE
!             PCNL = 0.0
!          ENDIF
!          IF (STMWT .GT. 0.0) THEN
!             PCNST = WTNST/(STMWT * PLTPOP) * 100.0
!           ELSE
!             PCNST = 0.0
!          ENDIF
!          IF (RTWT .GT. 0) THEN
!             PCNRT = ROOTN/RTWT * 100.0
!           ELSE
!             PCNRT = 0.0
!          ENDIF
!
!          WTNVEG  = (WTNLF + WTNST)
!          WTNGRN  = (WTNSH + WTNSD)
!          IF ((WTLF+STMWT).GT. 0.0) THEN
!             PCNVEG = (WTNLF+WTNST)/(WTLF+(STMWT * PLTPOP))*100
!           ELSE
!             PCNVEG = 0.0
!          ENDIF
!          IF (SDWT.GT. 0.001) THEN
!             PCNGRN = WTNSD / (SDWT * PLTPOP) * 100.0
!           ELSE
!             PCNGRN = 0.0
!          ENDIF
!
!          WRITE (NOUTPN,310)YEAR, DOY, DAS, DAP,
!     &    (WTNCAN*10.0), (WTNSD*10.0), (WTNVEG*10.0), PCNGRN, PCNVEG,
!     &    (WTNUP*10.0), (WTNLF*10.0), (WTNST*10.0), NRT, PCNL,
!     &    PCNST, PCNSH, PCNRT, CUMSENSURFN, CUMSENSOILN
! 
!!      NITHEAD = '@YEAR DOY   DAS   DAP' //
!!     &  '  CNAD  GNAD  VNAD  GN%D  VN%D  NUPC  LNAD' //
!!     &  '  SNAD  RNAD  LN%D  SN%D  SHND  RN%D  SNN0C  SNN1C'
!
!!       ADDED RNAD (= NRT KG/HA)
!
!  310     FORMAT (1X,I4,1X,I3.3,2(1X,I5),
!     &        3(1X,F5.1),2(1X,F5.2),1X,F5.1,
!     &        2(1X,F5.1),4(1X,F5.2),2(1X,F6.2))
!        ENDIF

      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal OUTPUT
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
!***********************************************************************
        CLOSE (NOUTDG)
!        CLOSE (NOUTPN)

!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OR_OPGROW
!=======================================================================
!=======================================================================
