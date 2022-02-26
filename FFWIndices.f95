module FFWIndices

    implicit none

    contains

    subroutine fuelMoisture(noonHumid, noonWind, initFine, initMoist, eqDry, ffm, r, t)

        integer, intent(in) :: noonHumid, noonWind
        real, intent(in) :: t, initFine 
        real, intent(out) :: initMoist, eqDry, ffm
        real, intent(inout) :: r
        real :: ra, rainFunc, corrRain, rainFine, eqWet, dayMoist, z, x

        if (r > 0.5) then 
            ra = r
            if (ra <= 1.45) then
                rainFunc = 123.85 - (55.6 * log(ra + 1.016))
            else
                if (ra - 5.75 <= 0.0) then
                    rainFunc = 57.87 - (18.2 * log(ra - 1.016))
                else
                    rainFunc = 40.69 - (8.25 * log(ra - 1.905))
                end if
            end if
            corrRain = 8.73 * exp(-0.1117 * initFine)
            rainFine = (initFine / 100.0) * rainFunc + (1.0 - corrRain)
            if (rainFine < 0.0) then
                rainFine = 0.0
            end if
        else 
            r = 0.0
            rainFine = initFine
        end if

        initMoist = 101.0 - rainFine
        eqDry = 0.942 * (noonHumid**0.679)+(11. * exp((noonHumid - 100.0) / 10.0)) + 0.18 * (21.1 - t)&
        & * (1.0 - 1.0 / exp(0.115 * noonHumid))

        if (initMoist-eqDry < 0.0) then
            eqWet = 0.618 * (noonHumid**0.753) + (10.0 * exp((noonHumid - 100.0) / 10.0)) + 0.18 * (21.1 - t)&
            & * (1.0 - 1.0 / exp(0.115 * noonHumid))
            if (initMoist >= eqWet) then
                dayMoist = initMoist
            else
                dayMoist = eqWet - (eqWet - initMoist) / 1.9953
            end if
        else if (initMoist-eqDry == 0.0) then
            dayMoist = initMoist
        else
            z = 0.424 * (1.0 - (noonHumid / 100.0)**1.7) + (0.0694 * (noonWind**0.5)) * (1.0 - (noonHumid / 100.0)**8)
            x = z * (0.463 * (exp(0.0365 * t)))
            dayMoist = eqDry + (initMoist - eqDry) / 10.0**x
        end if

        ffm = 101.0 - dayMoist

        if (ffm > 101.0) then
            ffm = 101.0
        else if(ffm < 0.0) then
            ffm = 0.0
        end if
        
        return
    
    end subroutine fuelMoisture

    subroutine duffMoisture(noonHumid, initDuff, dryFactDMC, effRain, funcRainEff, dmc, el, ra, t, j, r)

        real, dimension(12) :: el
        integer, intent(in) :: noonHumid, j
        real, intent(in) :: r, initDuff
        real, intent(out) :: dmc
        real, intent(inout) :: t
        real :: dryFactDMC, ra, effRain, funcRainEff, contMoist, rainMoist, rainDuff

        if (t + 1.1 < 0.0) then
            t = -1.1
        end if

        dryFactDMC = 1.894 * (t + 1.1) * (100.0 - noonHumid) * (el(j) * 0.0001)

        if (r > 1.5) then
            ra = r
            effRain = 0.92 * ra - 1.27
            contMoist = 20.0 + 280. / exp(0.023 * initDuff)
            if (initDuff <= 33.) then
                funcRainEff = 100. / (0.5 + 0.3 * initDuff)
            else if (initDuff - 65.0 <= 0.0) then
                funcRainEff = 14. - 1.3 * log(initDuff)
            else
                funcRainEff = 6.2 * log(initDuff) - 17.2
            end if
            rainMoist = contMoist + (1000.0 * effRain) / (48.77 + funcRainEff * effRain)
            rainDuff = 43.43 * (5.6348 - log(rainMoist - 20.0))
        else 
            rainDuff = initDuff
        end if

        if (rainDuff < 0.0) then
            rainDuff = 0.0
        end if

        dmc = rainDuff + dryFactDMC

        return
    
    end subroutine duffMoisture

    subroutine droughtCode(initDrought, fl, dc, r, j, t)

        real, dimension(12) :: fl
        integer, intent(in) :: j
        real, intent(in) :: r, initDrought
        real, intent(out) :: dc
        real, intent(inout) :: t
        real :: dryFactDC, effRain, moistEq, rainDrought, ra

        if (t + 2.8 < 0.0) then
            t = -2.8
        end if

        dryFactDC = (0.36 * (t + 2.8) + fl(j)) / 2.0

        if (r > 2.8) then
            ra = r
            effRain = 0.83 * ra - 1.27
            moistEq = 800.0 * exp(-initDrought / 400.0)
            rainDrought = initDrought - 400.0 * log(1.0 + ((3.937 * effRain) / moistEq))
            if (rainDrought <= 0.0) then
                rainDrought = 0.0
            end if
            dc = rainDrought + dryFactDC 
        else
            rainDrought = initDrought
            dc = rainDrought + dryFactDC 
        end if

        if (dc < 0.0) then
            dc = 0.0
        end if

        return

    end subroutine droughtCode

    subroutine index(noonWind, ffm, dmc, dc, indexDC, indexFFM, indexDMC, indexSI, indexBUI, indexFWI)

        integer, intent(in) :: noonWind
        integer, intent(out) :: indexDC, indexFFM, indexDMC, indexSI, indexBUI, indexFWI
        real, intent(inout) :: ffm, dmc, dc
        real :: dayFine, sf, ratio, cc, bb, logFWI, si, bui, fwi

        dayFine = 101.0 - ffm 
        sf = 19.1152 * exp(-0.1386 * dayFine) * (1.0 + dayFine**4.65 / 7950000.0)
        si = sf * exp(0.05039 * noonWind)
        bui = (0.8 * dc * dmc) / (dmc + 0.4 * dc)

        if (bui < dmc) then 
            ratio = (dmc - bui) / dmc
            cc = 0.92 + (0.0114 * dmc)**1.7
            bui = dmc - (cc * ratio)
            if (bui < 0.0) then
                    bui = 0.0
            end if
        end if
                
        if (bui > 80.0) then
            bb = 0.1 * si * (1000.0 / (25.0 + 108.64 / exp(0.023 * bui)))
        else
            bb = 0.1 * si * (0.626 * bui**0.809 + 2.0)
        end if

        if (bb - 1.0 <= 0.0) then
            fwi = bb
        else
            logFWI = 2.72 * (0.43 * log(bb))**0.647
            fwi = exp(logFWI)
        end if

        indexDC = int(dc + 0.5)
        indexFFM = int(ffm + 0.5)
        indexDMC = int(dmc + 0.5)
        indexSI = int(si + 0.5)
        indexBUI = int(bui + 0.5)
        indexFWI = int(fwi + 0.5)

        return 
    end subroutine index

end module FFWIndices