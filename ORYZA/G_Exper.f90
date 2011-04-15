!CALL ExperimentFileEdit(OUTPUTFILE, YRSIM, EDATE,& 
!                ISWWAT, ISWNIT, PLME, PAGE, PLPH, PLYPOP,PLANTS, PLANTS, PLDP, &
!                 IIRRI, IRRCOD,IRMTAB, RIRRIT, IRRI, WL0MIN, KPAMIN, SLMIN, WLODAY, ISTAGET, TMCTB)

SUBROUTINE ExperimentFileEdit(OUTPUTFILE, YRSIM, EDATE, & 
                PRODENV, NITROENV, ESTAB,  SBDUR, NPLH, PLYPOP, NPLSB, NPLDS, ZRTTR, &  !PLANT ESTABLISHMENT AND DENSITY
                IIRRI, IRRCOD, IRMTAB, RIRRIT, IRRI, WL0MIN, KPAMIN, SLMIN, WLODAY, ISTAGET, &  !FOR IRRIGATION
                TMCTB)  !Other parameters 

CHARACTER*(*) OUTPUTFILE 
CHARACTER*(*) PRODENV, NITROENV, ESTAB, IIRRI
INTEGER IYEAR, SBDUR, ICOMBA, I, J, SLMIN, WL0DAY, YRSIM, EDATE, EMYR, EMD, STTIME, IRRCOD
REAL NPLH, NH, PLYPOP,NPLSB, NPLDS, ZRTTR, WL0MIN, KPAMIN, WCMIN, IRRI
REAL IRMTAB(300), RIRRIT(750),ISTAGET(900), TMCTB(750)

IYEAR = INT(YRSIM/1000.0);STTIME = TRSIM-IYEAR
EMYR = INT(EDATE/1000.0);EMD = EDATE-EMYR*1000
OPEN(UNIT=20, FILE = OUTPUTFILE)
    WRITE(20, 5000) "*--------------------------------------------------------------------*"
    WRITE(20, 5000) "* 1. Selection of modes of running                                   *"
    WRITE(20, 5000) "*--------------------------------------------------------------------*"
    WRITE(20, 5000) "*--  RUNMODE is mode of running ORYZA"
    WRITE(20, '(A)') "RUNMODE = 'EXPERIMENT'       ! ORYZA simulates particular experiment"
    WRITE(20, 5000) "*--  PRODENV is Water production situation setting"
    CALL UPPERC(PRODENV)
    IF(INDEX(PRODENV, "N").GT.0) THEN
        WRITE(20, '(A)') "PRODENV = 'POTENTIAL'       ! 'POTENTIAL' for potential production or 'WATER BALANCE' for production may be water-limited"
    ELSE
        WRITE(20, '(A)') "PRODENV = 'WATER BALANCE'       ! 'POTENTIAL' for potential production or 'WATER BALANCE' for production may be water-limited"
    ENDIF
    WRITE(20, 5000) "*WATBAL = 'PADDY'    ! PADDY water balance (for lowland soils)"
    WRITE(20, 5000) "*--  NITROENV is Nitrogen production situation setting"
    CALL UPPERC(NITROENV)
    IF(INDEX(NITROENV, "Y").GT.0) THEN
        WRITE(20, '(A)') "NITROENV = 'POTENTIAL'       ! for potential production OF NITROGEN"
    ELSE
        WRITE(20, '(A)') "NITROENV = 'NITROGEN BALANCE'       ! Production may be nitrogen-limited"
    ENDIF
    WRITE(20, 5000) "*--------------------------------------------------------------------*"
    WRITE(20, 5000) "* 2. Timer data for simulation                                       *"
    WRITE(20, 5000) "*--------------------------------------------------------------------*"
    WRITE(20, 6000) "IYEAR",  IYEAR, "      ! Start year of simulation (year)"
    WRITE(20, 6000) "STTIME",  STTIME,".      ! Start time  (day number)"
    WRITE(20, '(A)') "FINTIM = 7000"               ! Finish time (days after start)"
    WRITE(20, 7000) "DELT", DELT, "                  ! Time step   (day)"
    WRITE(20, 5000) "TMCTB =  0., 0.,             ! Table for temperature increase"
    WRITE(20, 5000) "366., 0.              ! Climatic Change studies"
    WRITE(20, 5000) "TMPSB = 0.          ! Temperature increase in seed-bed due to cover;"
    WRITE(20, 5000) "! Zero when no cover over seed-bed; 9.5 with seed-bed"
    
    WRITE(20, 5000) "*--------------------------------------------------------------------*"
    WRITE(20, 5000) "* 3. Establishment data"
    WRITE(20, 5000) "*--------------------------------------------------------------------*"
    WRITE(20, 5000) "*--  ESTAB is method of establishment; 'TRANSPLANT' or 'DIRECT-SEED'"
    CALL UPPERC(ESTAB)
    IF(INDEX(ESTAB, "T").GT.0) THEN
        WRITE(20, '(A)') "ESTAB='TRANSPLANT'    !TRANSPLANT or DIRECT SEEDING"
    ELSE
        WRITE(20, '(A)') "ESTAB='DIRECT SEEDING'    !TRANSPLANT or DIRECT SEEDING"    
    END IF
    WRITE(20, 5000) "* Transplanting date May 25 (145), 2001; sowing date April 15; 50% emergence April 29 (119)"
    WRITE(20, 6000) "EMD", EMD, "     ! Day of emergence (either direct, or in seed-bed)"
    WRITE(20, 6000) "EMYR", EMYR, "     ! Year of emergence"
    WRITE(20, 6000) "SBDUR", SBDUR, "      ! Seed-bed duration (days between emerging and transplanting)"
    
    WRITE(20, 5000) "*--------------------------------------------------------------------*"
    WRITE(20, 5000) "* 4. Management parameters                                           *"
    WRITE(20, 5000) "*--------------------------------------------------------------------*"
    WRITE(20, 7000) "NPLH", NPLH, "        ! Number of plants per hill"
    WRITE(20, 7000) "NH", PLTPOP/PLPH       ! Number of hills/m2 (13 x 27 cm)"
    WRITE(20, 7000) "NPLSB", NPLSB, "      ! Number of plants in seed-bed (???)"    
    WRITE(20, 7000) "NPLDS", NPLDS, "      ! Number of plants/m2 direct-seeded"
    
    WRITE(20, 5000) "*-- Initial data at emergence, for either direct-seeding or seed-bed"
    WRITE(20, 5000) "*   Standard data used."
    WRITE(20, 5000) "LAPE   = 0.0001     ! Initial leaf area per plant"
    WRITE(20, 5000) "DVSI   = 0.0        ! Initial development stage"
    WRITE(20, 5000) "WLVGI  = 0.0        ! Initial leaf weight"
    WRITE(20, 5000) "WSTI   = 0.0        ! Initial stem weight"
    WRITE(20, 5000) "WRTI   = 0.0        ! Initial stem weight"
    WRITE(20, 5000) "WSOI   = 0.0        ! Initial weight storage organs"
    WRITE(20, 5000) "ZRTI   = 0.0001     ! Initial root depth (m)"
    
    WRITE(20, 5000) "*-- Re-initialization at transplanting (standard data used)"
    WRITE(20, 7000) "ZRTTR", ZRTTR, "       ! Root depth at transplanting (m)"
    
    WRITE(20, 5000) "*---------------------------------------------------------------*"
    WRITE(20, 5000) "* 5. Irrigation parameters"
    WRITE(20, 5000) "* Need only to be filled-in when PRODENV = 'WATER BALANCE'"
    WRITE(20, 5000) "*---------------------------------------------------------------*"
    WRITE(20, 5000) "DVSIMAX = 2.0 ! Development stage after which no more irrigation is applied"
    WRITE(20, 5000) "* The determination for switch critical. 1; Use Julian day; 2; Use DVS and"
    WRITE(20, '(A)') "*3; Use mixture of DVS and Julian day, but the Julian day is not allowed to be smaller than 2"
    CALL UPPERC(IIRRI)
    IF((IIRRI.EQ."R").OR.(IIRRI.EQ."P").OR.(IIRRI.EQ."W").OR.(IIRRI.EQ."N")) THEN
        WRITE(20, '(A)') "ICOMBA = 1" 
    ELSEIF(IIRRI.EQ."D") THEN
        WRITE(20, '(A)') "ICOMBA = 4"
    ELSEIF((IIRRI.EQ."A").OR.(IIRRI.EQ."F")) THEN
        WRITE(20, '(A)') "ICOMBA = 2"
    END IF !CAN BE MORE OPTIONS
    WRITE(20, 5000) "*  Combining irrigation management methods table IRMTAB, it must have at least two lines,"
    WRITE(20, 5000) "*      X (Julian day or DVS or DVS+Julian, present the switching day), Y (methods in real number)"
    J = INT(SIZE(IRMTAB)/2.0)
    IF(J.LT.2) THEN
        PRINT *, "The data is only one row, check it!"
        STOP
    ELSEIF(J.EQ.2) THEN
        WRITE(20, 3000) "IRMTAB", IRMTAB(1),IRMTAB(2)
        WRITE(20, 3100) IRMTAB(3), IRMTAB(4)
    ELSE
        WRITE(20, 3000) "IRMTAB", IRMTAB(1),IRMTAB(2)
        DO I=2, J-1
            WRITE(20, 3200) IRMTAB((I-1)*2+1),IRMTAB((I-1)*2+2)
        END DO 
        WRITE(20, 3100) IRMTAB((J-1)*2+1), IRMTAB((J-1)*2+2)
    END IF
    
    WRITE(20, 5000) "** Select from the following options;"
    WRITE(20, 5000) "*SWITIR = 0 ! No irrigation; rainfed"
    WRITE(20, 5000) "*SWITIR = 1 ! Irrigation supplied as input data"
    WRITE(20, 5000) "*SWITIR = 2 ! Irrigation at minimum standing soil water depth"
    WRITE(20, 5000) "*SWITIR = 3 ! Irrigation at minimum soil water potential"
    WRITE(20, 5000) "*SWITIR = 4 ! Irrigation at minimum soil water content"
    WRITE(20, 5000) "*SWITIR = 5 ! Irrigation at x days after disapp. standing water"
    WRITE(20, 5000) "*SWITIR = 6 ! Irrigation at minimum soil water potential in defined periods only"
    WRITE(20, 5000) ""
    
    IF(SIZE(RIRRIT)>=4) THEN
        WRITE(20, 5000) "*If SWITIR = 1, supply irrigation table, amount of irrigation"
        WRITE(20, 5000) "*(y in mm) for a given calendar * day (x), used if"
        J = INT(SIZE(RIRRIT)/2.0)
        IF(J.LT.2) THEN
            PRINT *, "The data is only one row, check it!"
            STOP
        ELSEIF(J.EQ.2) THEN
            WRITE(20, 3000) "RIRRIT", RIRRIT(1),RIRRIT(2)
            WRITE(20, 3100) RIRRIT(3), RIRRIT(4)
        ELSE
            WRITE(20, 3000) "RIRRIT", RIRRIT(1),RIRRIT(2)
            DO I=2, J-1
                WRITE(20, 3200) RIRRIT((I-1)*2+1),RIRRIT((I-1)*2+2)
            END DO 
            WRITE(20, 3100) RIRRIT((J-1)*2+1), RIRRIT((J-1)*2+2)
        END IF
    END IF
    
    WRITE(20, 5000) "** If SWITIR = 2;"
    WRITE(20, 5000) "***1) supply amount of irrigation IRRI2 (mm)"
    WRITE(20, 5000) "***2) supply minimum standing water depth WL0MIN (mm) below which irrigation water is applied"
    
    IF(WL0MIN.GT.0.0) THEN
        WRITE(20, 5000) "** If SWITIR = 2;"
        WRITE(20, 5000) "***1) supply amount of irrigation IRRI2 (mm)"
        WRITE(20, 5000) "***2) supply minimum standing water depth WL0MIN (mm) below which irrigation water is applied"  
        WRITE(20, 7000) "IRRI2", IRRI, " ! Irrigation gift (mm) !IT MUST BE REAL DATA"  
        WRITE(20, 7000) "WL0MIN", WLOMIN, "   ! Minimum standing water depth (mm) !IT MUST BE REAL DATA"
    END IF
    IF((KPAMIN.GT.0.0).AND.(SLMIN.GT.0.0)) THEN
        WRITE(20, 5000) "** IF SWITIR =3;"
        WRITE(20, 5000) "***1) supply amount of irrigation IRRI3 (mm)"
        WRITE(20, 5000) "***2) supply minimum soil water potential KPAMIN (KPa)"
        WRITE(20, 5000) "***3) Supply soil layer for which KPAMIN aplied, SLMIN3"
        WRITE(20, 7000) "IRRI3", IRRI
        WRITE(20, 7000) "KPAMIN", KPAMIN,"            !IT MUST BE REAL DATA"
        WRITE(20, 6000) "SLMIN3", SLMIN, "            !IT MUST BE INTEGER DATA"
    END IF
    IF((WCMIN.GT.0.0).AND.(SLMIN.GT.0.0)) THEN
        WRITE(20, 5000) "** IF SWITIR = 4;"
        WRITE(20, 5000) "***1) supply amount of irrigation IRRI4 (mm)"
        WRITE(20, 5000) "***2) supply minimum soil water conten WCAMIN (-)"
        WRITE(20, 5000) "***3) Supply soil layer for which KPAMIN aplied, SLMIN4"
        WRITE(20, 7000) "IRRI4", IRRI
        WRITE(20, 7000) "WCMIN", WCMIN, "      !IT MUST BE REAL DATA"
        WRITE(20, 6000) "SLMIN4", SLMIN, "      !IT MUST BE INTEGER DATA"
    END IF
    IF(WLODAY.GT.0.0) THEN
        WRITE(20, 5000) "** IF SWITIR = 5;"
        WRITE(20, 5000) "***1) supply amount of irrigation IRRI5 (mm)"
        WRITE(20, 5000) "***2) supply number of days after disappearence of standing water (WL0DAY) at which irrigation water is applied"
        WRITE(20, 7000) "IRRI5", IRRI
        WRITE(20, 5000) "WL0DAY = 5    ! number of days after disappearence of (-) INTEGER!!"
    END IF
    IF(SIZE(ISTAGET).GT.1) THEN
        WRITE(20, 5000) "** IF SWITIR = 6;"
        WRITE(20, 5000) "***1) supply amount of irrigation IRRI6 (mm)"
        WRITE(20, 5000) "***2) Supply soil layer for which KPAMIN aplied, SLMIN6"
        WRITE(20, 5000) "***3) period table as; Start; DVS ' 'finish DVS' 'KPAMIN during period'"
        WRITE(20, 5000) "*       Irrigation will be applied in the periods between 'start DVs' to 'end DVS'"
        WRITE(20, 5000) "*       and only when the soil water tension in layer SLMIN is above KPAMIN in that period"
        WRITE(20, 5000) "*       Note; at maximum 5 stages can de defined (no more than 15 data in table)!" 
        WRITE(20, 7000) "IRRI6", IRRI
        WRITE(20, 6000) "SLMIN6", SLMIN, "   !Layer for threshold value!"
        j=int(SIZE(ISTAGET)/3)
        IF(J.LT.2) THEN
            PRINT *, "Data is not the multiple of 3. please chekc it"
        elseif (j.eq.2) then
            WRITE(20, 1000) "ISTAGET", ISTAGET(1), ISTAGET(2), ISTAGET(3)  
            WRITE(20, 1100) ISTAGET(4), ISTAGET(5), ISTAGET(6)
        ELSE
            WRITE(20, 1000) "ISTAGET", ISTAGET(1), ISTAGET(2), ISTAGET(3) 
            DO I=2, J-1
                WRITE(20, 1200) "ISTAGET", ISTAGET((I-1)*3+1), ISTAGET((I-1)*3+2), ISTAGET((I-1)*3+3)
            END DO 
            WRITE(20, 1100) ISTAGET((J-1)*3+1), ISTAGET((J-1)*3+2), ISTAGET((J-1)*3+3)
            WRITE(20, 5000) "1.50, 1.60, 50.,"
            WRITE(20, 5000) "1.70, 1.80, 5."
        END IF
    end if
    !WRITE(20, 5000) ""
    !WRITE(20, 5000) "*--------------------------------------------------------------------*"
    !WRITE(20, 5000) "* 6. Nitrogen parameters                                             *"
    !WRITE(20, 5000) "*--------------------------------------------------------------------*"
    !WRITE(20, 5000) "*TWO SOIL C AND N DYNAMICS"
    !WRITE(20, 5000) "NUTRIENT = '"//ADJUSTL(TRIM(NUTRIENT))//",    !GENERAL SOM' AND 'APSIM SOILN"     
    !WRITE(20, 5000) "* Table of recovery fraction of Nitrogen in the soil (-) second column"
    !WRITE(20, 5000) "* versus development stage (DVS) (first column) STANDARD VALUE"
    !WRITE(20, 5000) "RECNIT ="
    !WRITE(20, 5000) "0.0, 0.30,"
    !WRITE(20, 5000) "0.2, 0.35,"
    !WRITE(20, 5000) "0.4, 0.50,"
    !WRITE(20, 5000) "0.8, 0.75,"
    !WRITE(20, 5000) "1.0, 0.75,"
    !WRITE(20, 5000) "2.5, 0.75"
    !WRITE(20, 5000) ""
    !WRITE(20, 5000) "* NO DATA ON SOILSP; THIS 0.8 IS FOR IRRI CONDITIONS IN THE DS......"
    !WRITE(20, 5000) "SOILSP = 0.8  ! Soil N mineralization rate (kg N/ha/d)"
    !WRITE(20, 5000) ""
    !WRITE(20, 5000) "* Table of fertilizer rate (kg N/ha) (second column) versus days after sowing"
    !WRITE(20, 5000) "* in the seed-bed (!) (first column)"
    !IF(SIZE(FERTIL).GE.4) THEN
    !    J=INT(SIZE(FERTIL)/2.0)
    !    IF(J.LE.2) THEN
    !        WRITE(20, 4000) "FERTIL", FERTIL(1), FERTIL(2)
    !        WRITE(20,4100) FERTIL(3), FERTIL(4)
    !    ELSE
    !        WRITE(20, 4000) "FERTIL", FERTIL(1), FERTIL(2)
    !        DO I=2, J-1
    !            WRITE(20, 4200) FERTIL((I-1)*2+1), FERTIL((I-1)*2+2)
    !        END DO
    !        WRITE(20,4100) FERTIL((J-1)*2+1), FERTIL((J-1)*2+2)
    !    END IF
    !ELSE
    !    WRITE(20, 5000) "*FERTIL = "
    !END IF 
    Close (20)
1000 FORMAT(A8, "=",2(f6.2,","),f6.2)
1100 FORMAT(A8, "=",3(f6.2,","))
1200 FORMAT(3(F6.2,","))   
2000 FORMAT(A8, "=",2(I2,","),I2)
2100 FORMAT(A8, "=",3(i2,","))
2200 FORMAT(3(I2,","))
2300 FORMAT(2(I2,","),I2)
3000 FORMAT(A8, "=", 2(F8.4,","))       !FOR FIRST ROW OF TWO COLUMN TABLE VALUE WITH REAL VALUE
3100 FORMAT((F8.4,","), F8.4)          !FOR LAST ROW OF TWO COLUMN TABLE VALUE WITH REAL VALUE
3200 FORMAT(2(F8.4,","))          !FOR MIDDLE ROW OF TWO COLUMN TABLE VALUE WITH REAL VALUE
4000 FORMAT(A8, "=", 2(I2,","))       !FOR FIRST ROW OF TWO COLUMN TABLE VALUE  WITH INTEGER VALUE
4100 FORMAT((I2,","), I2)          !FOR LAST ROW OF TWO COLUMN TABLE VALUE WITH INTEGER VALUE
4200 FORMAT(2(I2,","))          !FOR MIDDLE ROW OF TWO COLUMN TABLE VALUE WITH INTEGER VALUE
5000 FORMAT('(A)')
6000 FORMAT(A8, "=",I4, A)
7000 FORMAT(A8, "=",F8.2, A)
END SUBROUTINE EXPERIMENTFILEEDIT