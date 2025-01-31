!--------------------------------------------------------------------------!
!             	     M A I N   P R O G R A M                               !
!--------------------------------------------------------------------------!
! USAGE: This is the main program for AWAP_TO_NETCDF
! INCLUDE:type_def_mod, bios_io_mod, cable_bios_met_obs_params, &
!         cable_weathergenerator, bios_output
!--------------------------------------------------------------------------!

PROGRAM awap_to_netcdf

    USE type_def_mod
    USE bios_io_mod              ! MMY
    USE cable_bios_met_obs_params
    USE cable_weathergenerator,ONLY: WEATHER_GENERATOR_TYPE, WGEN_INIT, &
                               WGEN_DAILY_CONSTANTS, WGEN_SUBDIURNAL_MET
    USE bios_output


    IMPLICIT NONE

    REAL        :: dels  ! time step size in seconds
    INTEGER     :: kstart, kend, ktau, ktauday, counter, YYYY
    INTEGER     :: CurYear, YearStart, YearEnd ! MMY
    INTEGER     :: LOY
    CHARACTER(4):: CurYear_CHAR

    REAL(sp),DIMENSION(:),ALLOCATABLE :: data_temp
    INTEGER(i4b)                      :: iunit, iunit_rain, iunit_snow, iunit_lwdown, &
                                         iunit_swdown, iunit_tair, iunit_wind,        &
                                         iunit_qair, iunit_psurf


    ! input rain file path
    CHARACTER(LEN = 200)   :: rain_path

    ! output nc file name
    CHARACTER(LEN = 200)   :: Rainf_name, Snow_name, LWdown_name, &
                              SWdown_name, Tair_name, Wind_name,  &
                              Qair_name, PSurf_name

    ! output data netcdf file ID
    INTEGER                :: ncid_rain, ncid_snow, ncid_lw, ncid_sw,&
                              ncid_tair, ncid_wind, ncid_qair, ncid_ps

    ! output variable ID
    INTEGER                :: rainID, snowID, lwID, swID, &
                              tairID, windID, qairID, psID

    ! time variable ID for output nc file
    INTEGER                :: raintID, snowtID, lwtID, swtID, &
                              tairtID, windtID, qairtID, pstID

    TYPE(WEATHER_GENERATOR_TYPE),SAVE :: WG
    TYPE(FILE_NAME),SAVE              :: filename


! ************ 1. Initialise variable, arrays to store things, etc *************

    dels      = 10800.  ! It should be 3 hours = 3600*3. dels is time step size
                        ! in seconds given by bios.nml
    kstart    = 1
    ktauday   = INT(24.0*3600.0/dels) ! ktauday = 8

    YearStart = 2000
    YearEnd   = 2000 ! 2017
    CurYear   = YearStart

    CALL inout_path(filename)

    !ANNA
    rain_path    = TRIM(filename%path_in)//"/pr/"

    CALL cable_bios_init(WG, dels, CurYear, kend, ktauday, rain_path, filename)
       ! INCLUDING:
       ! 1 CALL ReadArcFltHeader(iunit,landmaskhdr_file,MaskCols,MaskRows,MaskBndW,MaskBndS,MaskRes,NoDataVal)
       ! 2 CALL open_bios_met ! DELETED MMY
       ! 3 CALL WGEN_INIT( WG, mland, latitude, dels )

    CALL read_filename(filename)

    counter = 0

! *************************** 2. Loop over years *******************************
    DO YYYY = YearStart, YearEnd ! YYYY= CABLE_USER%YearStart,  CABLE_USER%YearEnd
       CurYear = YYYY
	     IF ((MOD(YYYY,4) == 0 .AND. MOD(YYYY,100) /= 0) .OR. MOD(YYYY,400) == 0)&
       THEN                     ! leap year
	        LOY = 366
	     ELSE
	        LOY = 365
       ENDIF

       kend = NINT(24.0*3600.0/dels) * LOY ! rounds its argument to the nearest whole number.
                                           ! kend is the total timesteps of the current year

       PRINT *,"POINT 8 CurYear, LOY, kend ", CurYear, LOY, kend ! Debug


       WRITE(CurYear_CHAR,'(I4)'), CurYear ! It is "(I4)" rather than "(A4)"
       ! PRINT *,"POINT 8.5 CurYear_CHAR ",CurYear_CHAR ! Debug

       Rainf_name   = TRIM(filename%path_out)//"/Rainf/AWAP.Rainf.3hr."//TRIM(CurYear_CHAR)//".nc"
       Snow_name    = TRIM(filename%path_out)//"/Snowf/AWAP.Snowf.3hr."//TRIM(CurYear_CHAR)//".nc"
       LWdown_name  = TRIM(filename%path_out)//"/LWdown/AWAP.LWdown.3hr."//TRIM(CurYear_CHAR)//".nc"
       SWdown_name  = TRIM(filename%path_out)//"/SWdown/AWAP.SWdown.3hr."//TRIM(CurYear_CHAR)//".nc"
       Tair_name    = TRIM(filename%path_out)//"/Tair/AWAP.Tair.3hr."//TRIM(CurYear_CHAR)//".nc"
       Wind_name    = TRIM(filename%path_out)//"/Wind/AWAP.Wind.3hr."//TRIM(CurYear_CHAR)//".nc"
       Qair_name    = TRIM(filename%path_out)//"/Qair/AWAP.Qair.3hr."//TRIM(CurYear_CHAR)//".nc"
       PSurf_name   = TRIM(filename%path_out)//"/PSurf/AWAP.PSurf.3hr."//TRIM(CurYear_CHAR)//".nc"

       PRINT *,"POINT 9 Rainf_name ", TRIM(Rainf_name) ! Debug

!       CALL create_output_file(Rainf_name, ncid_rain, rainID, raintID, "Rainf",&
!                              "Rainfall rate",                         &
!                              "rainfall_flux",                         &
!                              "Rainf", "kg m-2 s-1")

!       CALL create_output_file(Snow_name, ncid_snow, snowID, snowtID, "Snowf", &
!                              "Snowfall rate",                         &
!                              "snowfall_flux",                         &
!                              "Snowf","kg m-2 s-1")

!       CALL create_output_file(LWdown_name, ncid_lw, lwID, lwtID, "LWdown",    &
!                               "Downward Longwave Radiation",          &
!                               "surface_downwelling_longwave_flux_in_air", &
!                               "LWdown","W m-2")

!       CALL create_output_file(SWdown_name, ncid_sw, swID, swtID, "SWdown",    &
!                              "Downward Shortwave Radiation",          &
!                              "surface_downwelling_shortwave_flux_in_air", &
!                              "SWdown", "W m-2")

!       CALL create_output_file(Tair_name, ncid_tair, tairID, tairtID,"Tair",   &
!                              "Near surface air temperature",          &
!                              "air_temperature",                       &
!                              "Tair","K")

!       CALL create_output_file(Wind_name, ncid_wind, windID, windtID, "Wind",  &
!                              "Near surface wind speed",               &
!                              "wind_speed",                            &
!                              "Wind","m s-1")

!       CALL create_output_file(Qair_name, ncid_qair, qairID, qairtID, "Qair",  &
!                              "Near surface specific humidity",        &
!                              "specific_humidity",                     &
!                              "Qair", "kg kg-1")

!       CALL create_output_file(PSurf_name, ncid_ps, psID, pstID, "PSurf",      &
!                              "Surface Pressure",                      &
!                              "surface_air_pressure",                  &
!                              "PSurf", "Pa")

        CALL GET_UNIT(iunit_rain)
        OPEN (iunit_rain, file="rain.txt",action="write")

        CALL GET_UNIT(iunit_snow)
        OPEN (iunit_snow, file="snow.txt",action="write")

        !CALL GET_UNIT(iunit_lwdown)
        !OPEN (iunit_lwdown, file="lwdown.txt",action="write")

        CALL GET_UNIT(iunit_swdown)
        OPEN (iunit_swdown, file="swdown.txt",action="write")

        CALL GET_UNIT(iunit_tair)
        OPEN (iunit_tair, file="tair.txt",action="write")

        CALL GET_UNIT(iunit_wind)
        OPEN (iunit_wind, file="wind.txt",action="write")

        CALL GET_UNIT(iunit_qair)
        OPEN (iunit_qair, file="qair.txt",action="write")

        CALL GET_UNIT(iunit_psurf)
        OPEN (iunit_psurf, file="psurf.txt",action="write")


       DO ktau = kstart, kend !!!!!!

          PRINT *,"POINT 11 ktau ", ktau ! Debug

          CALL cable_bios_read_met( WG, filename, counter, CurYear, YearStart, &
                                    YearEnd, ktau, kend, dels )
             ! INCLUDING:
             ! 1 CALL WGEN_DAILY_CONSTANTS( WG, mland, INT(met%doy(1))+1 )
             ! 2 CALL WGEN_SUBDIURNAL_MET( WG, mland, NINT(met%hod(1)*3600./dels) )

          ALLOCATE(data_temp(mland))

          !PRINT *,"WG%Precip",data_temp
          !PRINT *,"WG%Snow",WG%Snow
          !PRINT *,"WG%Phild",WG%Phild
          !PRINT *,"WG%PhiSd",WG%PhiSd
          !PRINT *,"WG%Temp",WG%Temp
          !PRINT *,"WG%Wind",WG%Wind
          !PRINT *,"WG%QV",WG%QV
          !PRINT *,"WG%PPa",WG%PPa

          data_temp = WG%Precip
          WRITE (iunit_rain, '(F20.10)'), data_temp(335859) ! (400,300) 399*841+300
!          CALL write_output(filename, data_temp, dels, CurYear, ktau, kend, &
!                            .FALSE., ncid_rain, rainID, raintID)

          data_temp = WG%Snow
          WRITE (iunit_snow, '(F20.10)'), data_temp(335859)
!          CALL write_output(filename, data_temp, dels, CurYear, ktau, kend, &
!                            .FALSE., ncid_snow, snowID, snowtID)

          !data_temp = WG%PhiLd
          !WRITE (iunit_lwdown, '(F20.10)'), data_temp(335859)
!          CALL write_output(filename, data_temp, dels, CurYear, ktau, kend, &
!                            .TRUE., ncid_lw  , lwID  , lwtID  )

          data_temp = WG%PhiSd
          WRITE (iunit_swdown, '(F20.10)'), data_temp(335859)
!          CALL write_output(filename, data_temp, dels, CurYear, ktau, kend, &
!                            .TRUE., ncid_sw  , swID  , swtID  )

          data_temp = WG%Temp
          WRITE (iunit_tair, '(F20.10)'), data_temp(335839)
!          CALL write_output(filename, data_temp, dels, CurYear, ktau, kend, &
!                            .FALSE., ncid_tair, tairID, tairtID)

          data_temp = WG%Wind
          WRITE (iunit_wind, '(F20.10)'), data_temp(335859)
!          CALL write_output(filename, data_temp, dels, CurYear, ktau, kend, &
!                            .TRUE., ncid_wind, windID, windtID)

          data_temp = WG%QV
          WRITE (iunit_qair, '(F20.10)'), data_temp(335859)
!          !PRINT *,data_temp
!          CALL write_output(filename, data_temp, dels, CurYear, ktau, kend, &
!                            .FALSE., ncid_qair, qairID, qairtID)

          data_temp = WG%PPa
          WRITE (iunit_psurf, '(F20.10)'), data_temp(335859)
!          CALL write_output(filename, data_temp, dels, CurYear, ktau, kend, &
!                            .FALSE., ncid_ps  , psID  , pstID  )

          DEALLOCATE(data_temp)
          PRINT*,"POINT 17 Finish output ", CurYear, "-", ktau
       END DO ! END Do loop over timestep ktau

       CLOSE(iunit_rain)
       CLOSE(iunit_snow)
       !CLOSE(iunit_lwdown)
       CLOSE(iunit_swdown)
       CLOSE(iunit_tair)
       CLOSE(iunit_wind)
       CLOSE(iunit_qair)
       CLOSE(iunit_psurf)

!       ok = NF90_CLOSE(ncid_rain)
!       ok = NF90_CLOSE(ncid_snow)
!       ok = NF90_CLOSE(ncid_lw  )
!       ok = NF90_CLOSE(ncid_sw  )
!       ok = NF90_CLOSE(ncid_tair)
!       ok = NF90_CLOSE(ncid_wind)
!       ok = NF90_CLOSE(ncid_qair)
!       ok = NF90_CLOSE(ncid_ps  )
       PRINT*,"POINT 18 Finish translating the year of ", CurYear

    END DO !YEAR
    PRINT *,"POINT 19 Done (*^_^*) "
END PROGRAM awap_to_netcdf

!--------------------------------------------------------------------------!
!		     	            	E N D   P R O G R A M								               !
!--------------------------------------------------------------------------!
