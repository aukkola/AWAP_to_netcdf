MODULE cable_bios_met_obs_params
! ******************************************************************************
! USAGE:
!
! INCLUDE: type_def_mod, bios_io_mod, cable_weathergenerator, IFPORT
! ******************************************************************************

    USE type_def_mod ! MMY
    USE bios_io_mod, ONLY: get_unit ! MMY
    USE cable_weathergenerator,ONLY: WEATHER_GENERATOR_TYPE, WGEN_INIT, &
                                   WGEN_DAILY_CONSTANTS, WGEN_SUBDIURNAL_MET

    IMPLICIT NONE

! == From cable_IO_vars_module in cable_iovars.f90, but POINTER -> ALLOCATABLE==

    REAL, DIMENSION(:),ALLOCATABLE   :: latitude !, longitude  ! Vectors for lat and long of each land cell

    INTEGER(i4b) :: MaskCols, MaskRows  ! Landmask col and row dimensions
    REAL(sp)     :: MaskCtrW, MaskCtrS
    REAL(sp)     :: MaskRes, NoDataVal  ! Landmask resolution (dec deg) and no-data value

! Define the daily bios met variables and file unit numbers which are required by multiple subroutines.

    REAL(sp),     ALLOCATABLE :: rain_day(:)          ! Packed vector of daily AWAP/BIOS rain (mm)
    REAL(sp),     ALLOCATABLE :: swdown_day(:)        ! Packed vector of daily AWAP/BIOS swdown (MJ)
    REAL(sp),     ALLOCATABLE :: wind_day(:)          ! MMY
    REAL(sp),     ALLOCATABLE :: tairmax_day(:)       ! Packed vector of daily AWAP/BIOS max air temp (deg C)
    REAL(sp),     ALLOCATABLE :: tairmin_day(:)       ! Packed vector of daily AWAP/BIOS min air temp (deg C)
    REAL(sp),     ALLOCATABLE :: vph_0900(:)          ! MMY 9:00 water vapour pressure [Pa]
    REAL(sp),     ALLOCATABLE :: vph_1500(:)          ! MMY 15:00 water vapour pressure [Pa]
    REAL(sp),     ALLOCATABLE :: prev_tairmax_day(:)  ! Packed vector of previous day's AWAP/BIOS max air temp (deg C)
    REAL(sp),     ALLOCATABLE :: next_tairmin_day(:)  ! Packed vector of next day's AWAP/BIOS min air temp (deg C)
    REAL(sp),     ALLOCATABLE :: prev_vph_1500(:)
    REAL(sp),     ALLOCATABLE :: next_vph_0900(:)
    REAL(sp),     ALLOCATABLE :: next_tmean(:)
    REAL(sp),     ALLOCATABLE :: next_tairmax_day(:)
    REAL(sp),     ALLOCATABLE :: next_sat_pressure0900(:)
    REAL(sp),     ALLOCATABLE :: next_act_pressure(:)
    REAL(sp),     ALLOCATABLE :: sat_pressure0900(:)
    REAL(sp),     ALLOCATABLE :: act_pressure(:)
    REAL(sp),     ALLOCATABLE :: tmean(:)

    

    INTEGER(i4b) :: rain_unit, swdown_unit, wind_unit, &
                    tairmax_unit, tairmin_unit, & ! Met file unit numbers
                    vph09_unit, vph15_unit,     & ! MMY
                    tairminnext_unit, tairmaxnext_unit, vph09next_unit ! MMY

    REAL(sp), PARAMETER :: SecDay = 86400.

  CONTAINS

! ============================= cable_bios_init ================================
  SUBROUTINE cable_bios_init(WG, dels, curyear, kend, ktauday, file_path, filename)

      USE bios_io_mod, ONLY: ReadArcFltHeader
      USE IFPORT ! for using systemqq

      IMPLICIT NONE

      REAL, INTENT(INOUT)    :: dels
      INTEGER, INTENT(INOUT) :: curyear, kend
      INTEGER, INTENT(IN)    :: ktauday
      CHARACTER(200)         :: file_path
      CHARACTER(500)         :: commandline

      INTEGER(i4b)   :: iunit, ok
      INTEGER(i4b)   :: irow, iland !icol ! Loop counters for cols, rows, land cells
      CHARACTER(200) :: hdr_file          ! hdr_file MMY
      REAL(sp)       :: MaskBndW, MaskBndS  ! Landmask outer bound dimensions in decimal degrees (West & South)
      INTEGER        :: file_num

      INTEGER, DIMENSION(:),ALLOCATABLE :: land_y  !land_x ! indicies of land in mask
      INTEGER(i4b), ALLOCATABLE         :: ColRowGrid(:,:) ! Temp grid to hold col or row numbers for packing to land_x or land_y

      TYPE(WEATHER_GENERATOR_TYPE) :: WG !,SAVE
      TYPE(FILE_NAME)              :: filename !,SAVE
! ___________________________________________________________________________

      ! ALLOCATE memory for filename

      commandline = 'find '//TRIM(file_path)//' -name "*.flt" -fls ./temp.txt'
      PRINT *,TRIM(commandline)
      ok = systemqq(commandline)

      commandline = 'wc -l <./temp.txt  >./filenum.txt'
      print*,TRIM(commandline)
      ok = systemqq(commandline)
      ok = systemqq('rm ./temp.txt')

      CALL GET_UNIT(iunit)
      OPEN (iunit, file="filenum.txt",status="old",action="read")
      READ (iunit, *) file_num
      CLOSE(iunit)

      PRINT *,"POINT 2 the number of met files ", file_num

      ALLOCATE ( filename%rain_file(file_num)    )
      ALLOCATE ( filename%swdown_file(file_num)  )
      ALLOCATE ( filename%wind_file(file_num)    )
      ALLOCATE ( filename%tairmax_file(file_num) )
      ALLOCATE ( filename%tairmin_file(file_num) )
      ALLOCATE ( filename%vph09_file(file_num)   )
      ALLOCATE ( filename%vph15_file(file_num)   )

    ! read in header
      hdr_file = "/g/data/w35/amu561/Steven_CABLE_runs/AWAP_to_netcdf/awra.hdr"
!"/g/data/w35/amu561/Steven_CABLE_runs/CABLE_inputs/Weather_generator_inputs/CNRM-CERFACS-CNRM-CM5/historical/CSIRO-CCAM-r3355-r240x120-ISIMIP2b-AWAP/pr/pr_19700101_19700101.hdr" ! MMY
      CALL GET_UNIT(iunit)
      CALL ReadArcFltHeader( iunit, hdr_file,    &
                             MaskCols, MaskRows, & ! Landmask col and row dimensions
                             MaskBndW, MaskBndS, & ! Landmask outer bound dimensions in decimal degrees (West & South)
                             MaskRes, NoDataVal )  ! Landmask resolution (dec deg) and no-data value

    ! Allocate memory for input data
      mland = MaskCols * MaskRows  ! the amount of land points, defined in type_def_mod
      PRINT *,"POINT 4 mland ", mland

      ALLOCATE( land_y (mland) ) !, land_x   (mland)
      ALLOCATE( ColRowGrid(MaskCols,MaskRows) )

    ! Populate a temporary integer grid with column numbers, then row numbers,
    ! packing them with the landmask into the cable vectors that record the
    ! column and row numbers of the land cells.

      !FORALL (icol = 1:MaskCols) ColRowGrid(icol,:) = icol
      !land_x = PACK(ColRowGrid,.true.)
      FORALL (irow = 1:MaskRows) ColRowGrid(:,irow) = irow
      land_y = PACK(ColRowGrid,.true.)
      DEALLOCATE (ColRowGrid)

    ! translate cols and rows of land_x and land_y into corresponding lats and longs.
      MaskCtrW = MaskBndW + (MaskRes / 2.0) ! Convert western and southern
      MaskCtrS = MaskBndS + (MaskRes / 2.0) ! boundaries to cell centres.

      ALLOCATE( latitude(mland) ) !, longitude(mland)
      DO iLand = 1,mland
         !longitude(iLand) = MaskRes * real((land_x(iLand) - 1)) + MaskCtrW
         latitude(iLand)  = MaskCtrS + (real(MaskRows - land_y(iLand)) * MaskRes)
      END DO
    ! Note that: the point(0,0) is the the westnorthest point on map, and
    !            the point(MaskRows-lat,MaskCols-lon) is the eastsouthest point.
      !DEALLOCATE (land_x)
      DEALLOCATE (land_y)

      PRINT *,"POINT 5 calculating latitude"

    ! call1 is unnecesery because cable_bios_init is only called once

      ALLOCATE (   rain_day(mland)     )
      ALLOCATE ( swdown_day(mland)     )
      ALLOCATE (   wind_day(mland)     ) ! MMY
      ALLOCATE (tairmax_day(mland)     )
      ALLOCATE (tairmin_day(mland)     )
      ALLOCATE (   vph_0900(mland)     ) ! MMY
      ALLOCATE (   vph_1500(mland)     ) ! MMY
      ALLOCATE (prev_tairmax_day(mland)) ! MMY
      ALLOCATE (next_tairmin_day(mland)) ! MMY
      ALLOCATE (prev_vph_1500(mland)   ) ! MMY
      ALLOCATE (next_vph_0900(mland)   ) ! MMY

      ALLOCATE (next_tmean(mland)   ) 
      ALLOCATE (next_tairmax_day(mland)   ) 
      ALLOCATE ( next_sat_pressure0900(mland)   ) 
      ALLOCATE (next_act_pressure(mland)   ) 

      ALLOCATE (sat_pressure0900(mland)   ) 
      ALLOCATE (act_pressure(mland)   ) 
      ALLOCATE (tmean(mland)   ) 





    ! Initialise Weather Generator
      CALL WGEN_INIT( WG, mland, latitude, dels )

  END SUBROUTINE cable_bios_init


! ============================ cable_bios_read_met =============================
  SUBROUTINE cable_bios_read_met( WG, filename, counter, CurYear, YearStart, &
                                  YearEnd, ktau, kend, dels )

	! Read a single day of meteorology from all bios met files, updating the bios_rundate

      IMPLICIT NONE

      INTEGER, INTENT(IN)    :: CurYear, YearStart, YearEnd, ktau, kend
      INTEGER, INTENT(INOUT) :: counter
      REAL, INTENT(IN)       :: dels

      LOGICAL(lgt)        :: newday
      INTEGER(i4b)        :: iland       ! Loop counter through mland land cells
      INTEGER(i4b)        :: error_status
      REAL                :: hod, doy, year

      CHARACTER(200)      :: vph09next_file, tairminnext_file, tairmaxnext_file ! MMY

      TYPE(WEATHER_GENERATOR_TYPE) :: WG
      TYPE(FILE_NAME)              :: filename
      


    ! CABLE is calculated in every grid's tile,and landpt(:)%cstart is the position
    ! of 1st gridcell veg patch in main arrays, but in weathergenerator we don't
    ! need to consider the veg patch, and the smallest unit shoulb be grid.

      hod  = REAL(MOD((ktau-1)*NINT(dels),INT(SecDay)))/3600.
      doy  = INT(REAL(ktau-1) * dels / SecDay ) + 1
      year = CurYear

      PRINT *,'POINT 12 hod, doy, year', hod, doy, year

      newday = ( hod == 0 )

      IF ( newday ) THEN
          counter = counter + 1
          PRINT *,'POINT 13 counter ',counter

! ************************** MMY **********************************
          PRINT *,'POINT 14 name of rain file',TRIM(filename%rain_file(counter))

          CALL GET_UNIT(rain_unit)  ! Rainfall
          OPEN (rain_unit, FILE=TRIM(filename%rain_file(counter)),        &
                FORM='BINARY', STATUS='OLD',IOSTAT=error_status)
          READ (rain_unit) rain_day          ! Packed vector of daily AWAP/BIOS rain (mm) ! romove bios_rundate,
          CLOSE(rain_unit)

          CALL GET_UNIT(swdown_unit)  ! Shortwave downward solar radiation
          OPEN (swdown_unit, FILE=TRIM(filename%swdown_file(counter)),    &
                FORM='BINARY', STATUS='OLD',IOSTAT=error_status)
          READ (swdown_unit) swdown_day        ! Packed vector of daily AWAP/BIOS swdown (MJ) ! romove bios_rundate,
          CLOSE(swdown_unit)

          CALL GET_UNIT(wind_unit)  ! Wind MMY
          OPEN (wind_unit, FILE=TRIM(filename%wind_file(counter)),        &
                FORM='BINARY', STATUS='OLD',IOSTAT=error_status)
          READ (wind_unit) wind_day            ! MMY
          CLOSE(wind_unit)

          CALL GET_UNIT(tairmax_unit)  ! Maximum air temperature
          OPEN (tairmax_unit, FILE=TRIM(filename%tairmax_file(counter)),  &
                FORM='BINARY', STATUS='OLD',IOSTAT=error_status)
          READ (tairmax_unit) tairmax_day       ! Packed vector of daily AWAP/BIOS max air temp (deg C) ! romove bios_rundate,
          CLOSE(tairmax_unit)

          CALL GET_UNIT(tairmin_unit)  ! Minimum air temperature
          OPEN (tairmin_unit, FILE=TRIM(filename%tairmin_file(counter)),  &
                FORM='BINARY', STATUS='OLD',IOSTAT=error_status)
          READ (tairmin_unit) tairmin_day       ! Packed vector of daily AWAP/BIOS min air temp (deg C) ! romove bios_rundate,
          CLOSE(tairmin_unit)

          ! CALL GET_UNIT(vph09_unit)
          ! OPEN (vph09_unit, FILE=TRIM(filename%vph09_file(counter)),      &
          !       FORM='BINARY', STATUS='OLD',IOSTAT=error_status)
          ! READ (vph09_unit) vph_0900
          ! CLOSE(vph09_unit)
          ! 
          ! CALL GET_UNIT(vph15_unit)
          ! OPEN (vph15_unit, FILE=TRIM(filename%vph15_file(counter)),      &
          !       FORM='BINARY', STATUS='OLD',IOSTAT=error_status)
          ! READ (vph15_unit) vph_1500
          ! CLOSE(vph15_unit)

    !**************************************
          !ANNA: unit conversions

          !Rain from mm/s to mm/day
          !print *, "rain_day before", rain_day(1005)
          
          rain_day = rain_day * SecDay
          
          !print *, "rain_day after", rain_day(1005)


          !Tmin and Tmax from K to C
          !print *, "tmax before", tairmax_day(1005)
          !print *, "tmin before", tairmin_day(1005)
          
          tairmax_day = tairmax_day - 273.15
          tairmin_day = tairmin_day - 273.15

          !print *, "tmax after", tairmax_day(1005)
          !print *, "tmin after", tairmin_day(1005)
      
          !SWdown from W m-2 to MJ/day     
          !print *, "swdown before", swdown_day(1005)
      
          swdown_day = swdown_day * SecDay / 10.**6
          
          !print *, "swdown after", swdown_day(1005)


          !VPD sanity check

          !print *, "VPD 9:00", vph_0900(1005)
          !print *, "VPD 15:00", vph_1500(1005)


    !**************************************

          WG%TempMaxDayPrev = WG%TempMaxDay
          WG%VapPPa1500Prev = WG%VapPPa1500

    !**************** MMY *****************
          IF (ktau /= kend .and. CurYear /= YearEnd) THEN

             CALL GET_UNIT(tairminnext_unit)  ! Minimum air temperature
             !CALL GET_UNIT(vph09next_unit)

             tairminnext_file = filename%tairmin_file(counter+1)
             tairmaxnext_file = filename%tairmax_file(counter+1)

             !vph09next_file   = filename%vph09_file(counter+1)

             !Next day min temp
             OPEN (tairminnext_unit, FILE=TRIM(tairminnext_file), &
                   FORM='BINARY', STATUS='OLD',IOSTAT=error_status)
             READ (tairminnext_unit) next_tairmin_day   ! Packed vector of daily AWAP/BIOS min air temp (deg C)
             CLOSE(tairminnext_unit)
             
             !ANNA add unit conversion
             next_tairmin_day = next_tairmin_day - 273.15
             
             !Max temp
             OPEN (tairmaxnext_unit, FILE=TRIM(tairmaxnext_file), &
                   FORM='BINARY', STATUS='OLD',IOSTAT=error_status)
             READ (tairmaxnext_unit) next_tairmax_day   ! Packed vector of daily AWAP/BIOS min air temp (deg C)
             CLOSE(tairmaxnext_unit)
             
             !ANNA add unit conversion
             next_tairmax_day = next_tairmax_day - 273.15
  
             
             ! 
             ! OPEN (vph09next_unit, FILE=TRIM(vph09next_file),     &
             !       FORM='BINARY', STATUS='OLD',IOSTAT=error_status)
             ! READ (vph09next_unit) next_vph_0900
             ! CLOSE(vph09next_unit)
            
             ! Next day 9am VPD
             !Use mean temp for 9am
             next_tmean = (next_tairmax_day + next_tairmin_day) / 2.
             
             next_sat_pressure0900 = 610.8 * exp((17.27 * next_tmean) / (237.3 + next_tmean))

             !Actual vapour pressure (eq. 36 in AWRA manual), in Pa
             !Always calculate actual pressure from tmin
             next_act_pressure = 610.8 * exp((17.27 * next_tairmin_day) / (237.3 + next_tairmin_day))
             
             !Calculate VPD as sat - actual, and convert from Pa to hPa
             next_vph_0900 =  (next_sat_pressure0900 - next_act_pressure) * 0.01

             

          ELSE

             next_tairmin_day = tairmin_day
             !next_vph_0900    = vph_0900
             
             !Calculate next VPD 9am
             !Use mean temp for 9am
             tmean = (tairmax_day + tairmin_day) / 2.
             
             sat_pressure0900 = 610.8 * exp((17.27 * tmean) / (237.3 + tmean))

             !Actual vapour pressure (eq. 36 in AWRA manual), in Pa
             !Always calculate actual pressure from tmin
             act_pressure = 610.8 * exp((17.27 * tairmin_day) / (237.3 + tairmin_day))
             
             !Calculate VPD as sat - actual, and convert from Pa to hPa
             next_vph_0900 =  (sat_pressure0900 - act_pressure) * 0.01

             

          END IF


    !***************************************

          WG%WindDay        = wind_day !MMY, from McVicar dataset
          WG%TempMinDay     = tairmin_day
          WG%TempMaxDay     = tairmax_day
          !WG%VapPPa0900     = vph_0900
          !WG%VapPPa1500     = vph_1500


    !**************** MMY *****************
          IF (ktau == 1 .AND. CurYear == YearStart) THEN
             WG%TempMaxDayPrev = tairmax_day
             WG%VapPPa1500Prev = vph_1500
          END IF

    !**************************************


          WG%TempMinDayNext = next_tairmin_day
          WG%VapPPa0900Next = next_vph_0900

          WG%SolarMJDay     = swdown_day
          WG%PrecipDay      = rain_day / 1000. ! ->[m/d]
          WHERE ( WG%TempMinDay < -2.0)
              WG%SnowDay    = rain_day / 1000. ! ->[m/d]
              WG%PrecipDay  = 0.0
          ELSEWHERE
              WG%SnowDay    = 0.0
          END WHERE

          WG%PPaDay = 1000.0 ! Air pressure in Pa fixed in space and time

          CALL WGEN_DAILY_CONSTANTS( WG, mland, INT(doy)+1 )

      END IF !newday

      PRINT *, 'POINT 15 finished cable_bios_read_met'

! follow one file such rain to go through the program and find every procedure needed for tranlating the data
! and can chance the data format by using array instead of ALLOCATE
! when finish follow the file and processes I will know which parts are lost and which parts are unneceserity
      CALL WGEN_SUBDIURNAL_MET( WG, mland, NINT(hod*3600./dels) )
    ! SUBROUTINE WGEN_SUBDIURNAL_MET(WG, np, itime)

!*******************************************************************************

  END SUBROUTINE cable_bios_read_met

END MODULE cable_bios_met_obs_params

! *************************************************************************************
