program ffwi

    use FFWIndices

    implicit none

    character(len=20) :: fname
    real, dimension(12) :: el, fl
    integer, dimension(12) :: lmon
    integer :: i, j, m, l, ih, iw, nn, ndays, idays, reason, noonHumid, noonWind
    integer :: indexDC, indexFFM, indexDMC, indexSI, indexBUI, indexFWI, unitRead, unitWrite
    real :: initFine, initDuff, initDrought, initMoist, eqDry, dryFactDMC, effRain, funcRainEff
    real :: rain, r, temperature, t, ra, dc, ffm, dmc

    ! Prompt the user for an input file to read
    call readFile(fname, unitRead)

    ! Prompt the user for an output file to write
    call writeFile(fname, unitWrite)

    write (unitWrite,"(2X,A)") "PROGRAM NO.: F-95"

    ! Read length of months, and day length factors
    do j = 1,12
        read (unitRead,"(I2,F4.1,F4.1)") lmon(j), el(j), fl(j)
    end do

    ! Reads initial values of FFMC, DMC, DC starting month and number of days of data starting month. 
    read (unitRead,"(F4.1,F4.1,F5.1,I2,I2)") initFine, initDuff, initDrought, m, ndays

    do j = m,12
        nn = lmon(j)

        if (j == m) then
            idays = lmon(j) - ndays + 1
        else
            idays = 1
        end if

        ! Reads daily weather data 
        l = 0

        do i = idays,nn
            l = l + 1
            read(unitRead,"(F4.1,I4,I4,F4.1)",iostat=reason) t,ih,iw,r
            if (reason == 0) then

                if (L == 1) then
                    write (unitWrite,"(10(/),1X,'  DATE  TEMP  RH   WIND  RAIN   FFMC   DMC   DC  &
                    &   ISI   BUI   FWI'/)")
                end if

                temperature = t
                noonHumid = ih
                noonWind = iw
                rain = r
    
                ! Fine Fuel Moisture Code (FFMC)
                call fuelMoisture(noonHumid, noonWind, initFine, initMoist, eqDry, ffm, r, t)

                ! Duff Moisture Code (DMC)
                call duffMoisture(noonHumid, initDuff, dryFactDMC, effRain, funcRainEff, dmc, el, ra, t, j, r)

                ! Drought Code (DC)
                call droughtCode(initDrought, fl, dc, r, j, t)

                ! Initial spread index, buildup index, fire weather index
                call index(noonWind, ffm, dmc, dc, indexDC, indexFFM, indexDMC, indexSI, indexBUI, indexFWI)

                ! Print weather data
                write (unitWrite,"(1X,2I3,F6.1,I4,I6,F7.1,6I6)") j,i,temperature,ih,iw,rain,indexFFM,indexDMC,&
                &indexDC,indexSI,indexBUI,indexFWI

                initFine = ffm
                initDuff = dmc
                initDrought = dc
            end if
        end do
    end do
end program ffwi

subroutine readFile(fname, unitRead)
        
    character(len=20), intent(out) :: fname
    integer, intent(inout) :: unitRead
    logical :: lexist
    
    do ! Get user input until valid input entered
        write (*,*) "Enter the filename of the file you wish to INPUT:"
        read (*,"(A)") fname
        inquire (file=fname,exist=lexist)
        ! Check if the filename entered exists in the directory
        if (.not. lexist) then
            write (*,*) 'File does not exist. Try again.'
        else
            ! Open the file contents
            open (unit=unitRead,file=fname,status='old',action='read')
            exit
        end if
    end do

    return

end subroutine readFile

subroutine writeFile(fname, unitWrite)
    
    character(len=20), intent(out) :: fname
    integer, intent(inout) :: unitWrite
    logical :: lexist

    write (*,*) "Enter the filename of the file you wish to OUTPUT to:"
    read (*,"(A)") fname
    
    inquire(file=fname,exist=lexist)
    ! Check if the filename entered exists in the directory
    if (.not. lexist) then
        ! Create a new file for the output 
        open (unit=unitWrite,file=fname,status='new',action='write')
    else
        ! Replace the old file contents with the new output
        open (unit=unitWrite,file=fname,status='replace',action='write')
    end if

    return

end subroutine writeFile