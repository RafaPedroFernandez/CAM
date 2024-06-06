      module mo_nln_matrix
      use shr_kind_mod, only : r8 => shr_kind_r8
      use chem_mods, only: veclen
      private
      public :: nlnmat
      contains
      subroutine nlnmat01( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,781) = -(rxt(k,469)*y(k,282))
         mat(k,2128) = -rxt(k,469)*y(k,1)
         mat(k,2268) = rxt(k,472)*y(k,253)
         mat(k,1020) = rxt(k,472)*y(k,154)
         mat(k,747) = -(rxt(k,473)*y(k,282))
         mat(k,2125) = -rxt(k,473)*y(k,2)
         mat(k,1019) = rxt(k,470)*y(k,267)
         mat(k,1819) = rxt(k,470)*y(k,253)
         mat(k,999) = -(rxt(k,552)*y(k,156) + rxt(k,553)*y(k,164) + rxt(k,554) &
                      *y(k,282))
         mat(k,2399) = -rxt(k,552)*y(k,6)
         mat(k,2636) = -rxt(k,553)*y(k,6)
         mat(k,2147) = -rxt(k,554)*y(k,6)
         mat(k,209) = -(rxt(k,511)*y(k,282))
         mat(k,2049) = -rxt(k,511)*y(k,7)
         mat(k,516) = -(rxt(k,514)*y(k,282))
         mat(k,2096) = -rxt(k,514)*y(k,8)
         mat(k,596) = rxt(k,512)*y(k,267)
         mat(k,1804) = rxt(k,512)*y(k,255)
         mat(k,210) = .120_r8*rxt(k,511)*y(k,282)
         mat(k,2050) = .120_r8*rxt(k,511)*y(k,7)
         mat(k,997) = .100_r8*rxt(k,553)*y(k,164)
         mat(k,1089) = .100_r8*rxt(k,556)*y(k,164)
         mat(k,2626) = .100_r8*rxt(k,553)*y(k,6) + .100_r8*rxt(k,556)*y(k,140)
         mat(k,2254) = .500_r8*rxt(k,513)*y(k,255) + .200_r8*rxt(k,540)*y(k,288) &
                      + .060_r8*rxt(k,546)*y(k,291)
         mat(k,597) = .500_r8*rxt(k,513)*y(k,154)
         mat(k,843) = .200_r8*rxt(k,540)*y(k,154)
         mat(k,859) = .060_r8*rxt(k,546)*y(k,154)
         mat(k,2248) = .200_r8*rxt(k,540)*y(k,288) + .200_r8*rxt(k,546)*y(k,291)
         mat(k,842) = .200_r8*rxt(k,540)*y(k,154)
         mat(k,857) = .200_r8*rxt(k,546)*y(k,154)
         mat(k,2264) = .200_r8*rxt(k,540)*y(k,288) + .150_r8*rxt(k,546)*y(k,291)
         mat(k,844) = .200_r8*rxt(k,540)*y(k,154)
         mat(k,860) = .150_r8*rxt(k,546)*y(k,154)
         mat(k,2250) = .210_r8*rxt(k,546)*y(k,291)
         mat(k,858) = .210_r8*rxt(k,546)*y(k,154)
         mat(k,284) = -(rxt(k,474)*y(k,282))
         mat(k,2060) = -rxt(k,474)*y(k,15)
         mat(k,996) = .050_r8*rxt(k,553)*y(k,164)
         mat(k,1088) = .050_r8*rxt(k,556)*y(k,164)
         mat(k,2625) = .050_r8*rxt(k,553)*y(k,6) + .050_r8*rxt(k,556)*y(k,140)
         mat(k,433) = -(rxt(k,440)*y(k,156) + rxt(k,441)*y(k,282))
         mat(k,2391) = -rxt(k,440)*y(k,16)
         mat(k,2085) = -rxt(k,441)*y(k,16)
         mat(k,2350) = -(rxt(k,263)*y(k,51) + rxt(k,264)*y(k,267) + rxt(k,265) &
                      *y(k,155) + rxt(k,266)*y(k,164) + rxt(k,273)*y(k,22) + rxt(k,302) &
                      *y(k,125))
         mat(k,1731) = -rxt(k,263)*y(k,17)
         mat(k,1880) = -rxt(k,264)*y(k,17)
         mat(k,2581) = -rxt(k,265)*y(k,17)
         mat(k,2676) = -rxt(k,266)*y(k,17)
         mat(k,961) = -rxt(k,273)*y(k,17)
         mat(k,1910) = -rxt(k,302)*y(k,17)
         mat(k,566) = rxt(k,262)*y(k,282)
         mat(k,2378) = 4.000_r8*rxt(k,267)*y(k,21) + (rxt(k,268)+rxt(k,269))*y(k,74) &
                      + rxt(k,575)*y(k,83) + rxt(k,292)*y(k,115) + (rxt(k,303) &
                       +rxt(k,304))*y(k,125) + rxt(k,272)*y(k,154) + rxt(k,277) &
                      *y(k,163) + rxt(k,586)*y(k,183) + rxt(k,278)*y(k,282)
         mat(k,185) = rxt(k,252)*y(k,281)
         mat(k,190) = rxt(k,282)*y(k,281)
         mat(k,580) = 2.000_r8*rxt(k,336)*y(k,70) + 2.000_r8*rxt(k,363)*y(k,281) &
                      + 2.000_r8*rxt(k,337)*y(k,282)
         mat(k,152) = rxt(k,338)*y(k,282)
         mat(k,698) = rxt(k,341)*y(k,70) + rxt(k,364)*y(k,281) + rxt(k,342)*y(k,282)
         mat(k,160) = 2.000_r8*rxt(k,348)*y(k,282)
         mat(k,509) = 3.000_r8*rxt(k,349)*y(k,70) + 3.000_r8*rxt(k,283)*y(k,281) &
                      + 3.000_r8*rxt(k,350)*y(k,282)
         mat(k,164) = rxt(k,351)*y(k,282)
         mat(k,1966) = 2.000_r8*rxt(k,336)*y(k,45) + rxt(k,341)*y(k,52) &
                      + 3.000_r8*rxt(k,349)*y(k,66)
         mat(k,2610) = (rxt(k,268)+rxt(k,269))*y(k,21)
         mat(k,1051) = rxt(k,575)*y(k,21)
         mat(k,168) = 2.000_r8*rxt(k,284)*y(k,281)
         mat(k,1548) = rxt(k,279)*y(k,163) + rxt(k,285)*y(k,281) + rxt(k,280)*y(k,282)
         mat(k,2476) = rxt(k,292)*y(k,21)
         mat(k,1910) = mat(k,1910) + (rxt(k,303)+rxt(k,304))*y(k,21)
         mat(k,2322) = rxt(k,272)*y(k,21)
         mat(k,2234) = rxt(k,277)*y(k,21) + rxt(k,279)*y(k,97)
         mat(k,1588) = rxt(k,586)*y(k,21)
         mat(k,2012) = rxt(k,252)*y(k,38) + rxt(k,282)*y(k,39) + 2.000_r8*rxt(k,363) &
                      *y(k,45) + rxt(k,364)*y(k,52) + 3.000_r8*rxt(k,283)*y(k,66) &
                      + 2.000_r8*rxt(k,284)*y(k,94) + rxt(k,285)*y(k,97)
         mat(k,2199) = rxt(k,262)*y(k,18) + rxt(k,278)*y(k,21) + 2.000_r8*rxt(k,337) &
                      *y(k,45) + rxt(k,338)*y(k,46) + rxt(k,342)*y(k,52) &
                      + 2.000_r8*rxt(k,348)*y(k,65) + 3.000_r8*rxt(k,350)*y(k,66) &
                      + rxt(k,351)*y(k,67) + rxt(k,280)*y(k,97)
         mat(k,563) = -(rxt(k,262)*y(k,282))
         mat(k,2101) = -rxt(k,262)*y(k,18)
         mat(k,2333) = rxt(k,273)*y(k,22)
         mat(k,954) = rxt(k,273)*y(k,17)
         mat(k,1538) = (rxt(k,603)+rxt(k,665)+rxt(k,678)+rxt(k,687))*y(k,108)
         mat(k,1614) = (rxt(k,603)+rxt(k,665)+rxt(k,678)+rxt(k,687))*y(k,97)
         mat(k,2362) = rxt(k,270)*y(k,74)
         mat(k,955) = rxt(k,274)*y(k,70)
         mat(k,1932) = rxt(k,274)*y(k,22)
         mat(k,2595) = rxt(k,270)*y(k,21)
         mat(k,1539) = (rxt(k,602)+rxt(k,667)+rxt(k,675)+rxt(k,684))*y(k,109)
         mat(k,1767) = (rxt(k,605)+rxt(k,664)+rxt(k,677)+rxt(k,686))*y(k,108)
         mat(k,1615) = (rxt(k,605)+rxt(k,664)+rxt(k,677)+rxt(k,686))*y(k,101)
         mat(k,1743) = (rxt(k,602)+rxt(k,667)+rxt(k,675)+rxt(k,684))*y(k,97)
         mat(k,2332) = rxt(k,265)*y(k,155)
         mat(k,2537) = rxt(k,265)*y(k,17)
         mat(k,2379) = -(4._r8*rxt(k,267)*y(k,21) + (rxt(k,268) + rxt(k,269) + rxt(k,270) &
                      ) * y(k,74) + rxt(k,271)*y(k,267) + rxt(k,272)*y(k,154) &
                      + rxt(k,275)*y(k,155) + rxt(k,277)*y(k,163) + rxt(k,278) &
                      *y(k,282) + rxt(k,292)*y(k,115) + (rxt(k,303) + rxt(k,304) &
                      ) * y(k,125) + rxt(k,575)*y(k,83) + rxt(k,586)*y(k,183))
         mat(k,2611) = -(rxt(k,268) + rxt(k,269) + rxt(k,270)) * y(k,21)
         mat(k,1881) = -rxt(k,271)*y(k,21)
         mat(k,2323) = -rxt(k,272)*y(k,21)
         mat(k,2582) = -rxt(k,275)*y(k,21)
         mat(k,2235) = -rxt(k,277)*y(k,21)
         mat(k,2200) = -rxt(k,278)*y(k,21)
         mat(k,2477) = -rxt(k,292)*y(k,21)
         mat(k,1911) = -(rxt(k,303) + rxt(k,304)) * y(k,21)
         mat(k,1052) = -rxt(k,575)*y(k,21)
         mat(k,1589) = -rxt(k,586)*y(k,21)
         mat(k,2351) = rxt(k,302)*y(k,125) + rxt(k,266)*y(k,164)
         mat(k,962) = rxt(k,276)*y(k,163)
         mat(k,1549) = rxt(k,286)*y(k,281)
         mat(k,1629) = rxt(k,281)*y(k,163)
         mat(k,1911) = mat(k,1911) + rxt(k,302)*y(k,17)
         mat(k,2235) = mat(k,2235) + rxt(k,276)*y(k,22) + rxt(k,281)*y(k,108)
         mat(k,2677) = rxt(k,266)*y(k,17)
         mat(k,2013) = rxt(k,286)*y(k,97)
         mat(k,956) = -(rxt(k,273)*y(k,17) + rxt(k,274)*y(k,70) + rxt(k,276)*y(k,163))
         mat(k,2335) = -rxt(k,273)*y(k,22)
         mat(k,1939) = -rxt(k,274)*y(k,22)
         mat(k,2212) = -rxt(k,276)*y(k,22)
         mat(k,2363) = rxt(k,275)*y(k,155)
         mat(k,2555) = rxt(k,275)*y(k,21)
         mat(k,287) = -(rxt(k,515)*y(k,282))
         mat(k,2061) = -rxt(k,515)*y(k,24)
         mat(k,2245) = rxt(k,518)*y(k,257)
         mat(k,528) = rxt(k,518)*y(k,154)
         mat(k,387) = -(rxt(k,517)*y(k,282))
         mat(k,2077) = -rxt(k,517)*y(k,25)
         mat(k,529) = rxt(k,516)*y(k,267)
         mat(k,1793) = rxt(k,516)*y(k,257)
         mat(k,231) = -(rxt(k,332)*y(k,70) + rxt(k,333)*y(k,282))
         mat(k,1920) = -rxt(k,332)*y(k,26)
         mat(k,2052) = -rxt(k,333)*y(k,26)
         mat(k,337) = -(rxt(k,389)*y(k,70) + rxt(k,390)*y(k,282))
         mat(k,1923) = -rxt(k,389)*y(k,27)
         mat(k,2072) = -rxt(k,390)*y(k,27)
         mat(k,663) = -(rxt(k,391)*y(k,70) + rxt(k,392)*y(k,164) + rxt(k,417)*y(k,282))
         mat(k,1934) = -rxt(k,391)*y(k,28)
         mat(k,2630) = -rxt(k,392)*y(k,28)
         mat(k,2114) = -rxt(k,417)*y(k,28)
         mat(k,293) = -(rxt(k,334)*y(k,70) + rxt(k,335)*y(k,282))
         mat(k,1922) = -rxt(k,334)*y(k,29)
         mat(k,2063) = -rxt(k,335)*y(k,29)
         mat(k,325) = -(rxt(k,397)*y(k,282))
         mat(k,2070) = -rxt(k,397)*y(k,30)
         mat(k,910) = .800_r8*rxt(k,393)*y(k,258) + .200_r8*rxt(k,394)*y(k,262)
         mat(k,1636) = .200_r8*rxt(k,394)*y(k,258)
         mat(k,397) = -(rxt(k,398)*y(k,282))
         mat(k,2079) = -rxt(k,398)*y(k,31)
         mat(k,911) = rxt(k,395)*y(k,267)
         mat(k,1795) = rxt(k,395)*y(k,258)
         mat(k,343) = -(rxt(k,399)*y(k,70) + rxt(k,400)*y(k,282))
         mat(k,1924) = -rxt(k,399)*y(k,32)
         mat(k,2073) = -rxt(k,400)*y(k,32)
         mat(k,1140) = -(rxt(k,420)*y(k,156) + rxt(k,421)*y(k,164) + rxt(k,438) &
                      *y(k,282))
         mat(k,2409) = -rxt(k,420)*y(k,33)
         mat(k,2644) = -rxt(k,421)*y(k,33)
         mat(k,2158) = -rxt(k,438)*y(k,33)
         mat(k,940) = .130_r8*rxt(k,498)*y(k,164)
         mat(k,2644) = mat(k,2644) + .130_r8*rxt(k,498)*y(k,127)
         mat(k,492) = -(rxt(k,425)*y(k,282))
         mat(k,2092) = -rxt(k,425)*y(k,34)
         mat(k,892) = rxt(k,423)*y(k,267)
         mat(k,1802) = rxt(k,423)*y(k,259)
         mat(k,133) = -(rxt(k,426)*y(k,282))
         mat(k,2043) = -rxt(k,426)*y(k,35)
         mat(k,329) = -(rxt(k,521)*y(k,282))
         mat(k,2071) = -rxt(k,521)*y(k,36)
         mat(k,738) = rxt(k,519)*y(k,267)
         mat(k,1790) = rxt(k,519)*y(k,260)
         mat(k,120) = -(rxt(k,251)*y(k,281))
         mat(k,1976) = -rxt(k,251)*y(k,37)
         mat(k,181) = -(rxt(k,252)*y(k,281))
         mat(k,1981) = -rxt(k,252)*y(k,38)
         mat(k,186) = -(rxt(k,282)*y(k,281))
         mat(k,1982) = -rxt(k,282)*y(k,39)
         mat(k,137) = -(rxt(k,253)*y(k,281))
         mat(k,1977) = -rxt(k,253)*y(k,40)
         mat(k,191) = -(rxt(k,254)*y(k,281))
         mat(k,1983) = -rxt(k,254)*y(k,41)
         mat(k,141) = -(rxt(k,255)*y(k,281))
         mat(k,1978) = -rxt(k,255)*y(k,42)
         mat(k,196) = -(rxt(k,256)*y(k,281))
         mat(k,1984) = -rxt(k,256)*y(k,43)
         mat(k,145) = -(rxt(k,257)*y(k,281))
         mat(k,1979) = -rxt(k,257)*y(k,44)
         mat(k,575) = -(rxt(k,336)*y(k,70) + rxt(k,337)*y(k,282) + rxt(k,363)*y(k,281))
         mat(k,1931) = -rxt(k,336)*y(k,45)
         mat(k,2103) = -rxt(k,337)*y(k,45)
         mat(k,1993) = -rxt(k,363)*y(k,45)
         mat(k,149) = -(rxt(k,338)*y(k,282))
         mat(k,2044) = -rxt(k,338)*y(k,46)
         mat(k,356) = -(rxt(k,339)*y(k,70) + rxt(k,340)*y(k,282))
         mat(k,1925) = -rxt(k,339)*y(k,47)
         mat(k,2075) = -rxt(k,340)*y(k,47)
         mat(k,1721) = -(rxt(k,224)*y(k,70) + rxt(k,263)*y(k,17) + rxt(k,368)*y(k,267) &
                      + rxt(k,369)*y(k,156) + rxt(k,370)*y(k,163) + rxt(k,371) &
                      *y(k,282))
         mat(k,1956) = -rxt(k,224)*y(k,51)
         mat(k,2340) = -rxt(k,263)*y(k,51)
         mat(k,1870) = -rxt(k,368)*y(k,51)
         mat(k,2438) = -rxt(k,369)*y(k,51)
         mat(k,2224) = -rxt(k,370)*y(k,51)
         mat(k,2189) = -rxt(k,371)*y(k,51)
         mat(k,787) = .400_r8*rxt(k,469)*y(k,282)
         mat(k,1012) = .340_r8*rxt(k,553)*y(k,164)
         mat(k,437) = .500_r8*rxt(k,440)*y(k,156)
         mat(k,667) = rxt(k,392)*y(k,164)
         mat(k,1148) = .500_r8*rxt(k,421)*y(k,164)
         mat(k,704) = .500_r8*rxt(k,409)*y(k,282)
         mat(k,904) = rxt(k,376)*y(k,282)
         mat(k,500) = .300_r8*rxt(k,377)*y(k,282)
         mat(k,1602) = (rxt(k,385)+rxt(k,386))*y(k,281)
         mat(k,1127) = rxt(k,352)*y(k,262)
         mat(k,2600) = rxt(k,233)*y(k,262)
         mat(k,1186) = .800_r8*rxt(k,414)*y(k,282)
         mat(k,949) = .910_r8*rxt(k,498)*y(k,164)
         mat(k,727) = .300_r8*rxt(k,489)*y(k,282)
         mat(k,1365) = .120_r8*rxt(k,451)*y(k,164)
         mat(k,718) = .500_r8*rxt(k,464)*y(k,282)
         mat(k,1106) = .340_r8*rxt(k,556)*y(k,164)
         mat(k,1475) = .600_r8*rxt(k,465)*y(k,164)
         mat(k,2312) = .100_r8*rxt(k,471)*y(k,253) + rxt(k,375)*y(k,262) &
                      + .500_r8*rxt(k,442)*y(k,264) + .500_r8*rxt(k,411)*y(k,266) &
                      + .920_r8*rxt(k,481)*y(k,269) + .250_r8*rxt(k,449)*y(k,274) &
                      + rxt(k,458)*y(k,276) + rxt(k,432)*y(k,284) + rxt(k,436) &
                      *y(k,285) + .340_r8*rxt(k,565)*y(k,286) + .320_r8*rxt(k,570) &
                      *y(k,287) + .250_r8*rxt(k,506)*y(k,290)
         mat(k,2438) = mat(k,2438) + .500_r8*rxt(k,440)*y(k,16) + rxt(k,482)*y(k,269) &
                      + .250_r8*rxt(k,448)*y(k,274) + rxt(k,459)*y(k,276)
         mat(k,2666) = .340_r8*rxt(k,553)*y(k,6) + rxt(k,392)*y(k,28) &
                      + .500_r8*rxt(k,421)*y(k,33) + .910_r8*rxt(k,498)*y(k,127) &
                      + .120_r8*rxt(k,451)*y(k,135) + .340_r8*rxt(k,556)*y(k,140) &
                      + .600_r8*rxt(k,465)*y(k,141)
         mat(k,659) = rxt(k,416)*y(k,282)
         mat(k,1213) = .680_r8*rxt(k,574)*y(k,282)
         mat(k,1028) = .100_r8*rxt(k,471)*y(k,154)
         mat(k,916) = .700_r8*rxt(k,394)*y(k,262)
         mat(k,897) = rxt(k,422)*y(k,262)
         mat(k,1527) = rxt(k,405)*y(k,262) + rxt(k,478)*y(k,269) + .250_r8*rxt(k,445) &
                      *y(k,274) + rxt(k,454)*y(k,276) + .250_r8*rxt(k,503)*y(k,290)
         mat(k,1674) = rxt(k,352)*y(k,68) + rxt(k,233)*y(k,74) + rxt(k,375)*y(k,154) &
                      + .700_r8*rxt(k,394)*y(k,258) + rxt(k,422)*y(k,259) + rxt(k,405) &
                      *y(k,261) + (4.000_r8*rxt(k,372)+2.000_r8*rxt(k,373))*y(k,262) &
                      + 1.500_r8*rxt(k,479)*y(k,269) + .750_r8*rxt(k,484)*y(k,270) &
                      + .800_r8*rxt(k,493)*y(k,271) + .880_r8*rxt(k,446)*y(k,274) &
                      + 2.000_r8*rxt(k,455)*y(k,276) + .750_r8*rxt(k,558)*y(k,280) &
                      + .800_r8*rxt(k,434)*y(k,285) + .930_r8*rxt(k,563)*y(k,286) &
                      + .950_r8*rxt(k,568)*y(k,287) + .800_r8*rxt(k,504)*y(k,290)
         mat(k,675) = .500_r8*rxt(k,442)*y(k,154)
         mat(k,831) = .500_r8*rxt(k,411)*y(k,154)
         mat(k,1870) = mat(k,1870) + .450_r8*rxt(k,456)*y(k,276) + .150_r8*rxt(k,435) &
                      *y(k,285)
         mat(k,1398) = .920_r8*rxt(k,481)*y(k,154) + rxt(k,482)*y(k,156) + rxt(k,478) &
                      *y(k,261) + 1.500_r8*rxt(k,479)*y(k,262)
         mat(k,1431) = .750_r8*rxt(k,484)*y(k,262)
         mat(k,1350) = .800_r8*rxt(k,493)*y(k,262)
         mat(k,1453) = .250_r8*rxt(k,449)*y(k,154) + .250_r8*rxt(k,448)*y(k,156) &
                      + .250_r8*rxt(k,445)*y(k,261) + .880_r8*rxt(k,446)*y(k,262)
         mat(k,1495) = rxt(k,458)*y(k,154) + rxt(k,459)*y(k,156) + rxt(k,454)*y(k,261) &
                      + 2.000_r8*rxt(k,455)*y(k,262) + .450_r8*rxt(k,456)*y(k,267) &
                      + 4.000_r8*rxt(k,457)*y(k,276)
         mat(k,1199) = .750_r8*rxt(k,558)*y(k,262)
         mat(k,2002) = (rxt(k,385)+rxt(k,386))*y(k,64)
         mat(k,2189) = mat(k,2189) + .400_r8*rxt(k,469)*y(k,1) + .500_r8*rxt(k,409) &
                      *y(k,60) + rxt(k,376)*y(k,62) + .300_r8*rxt(k,377)*y(k,63) &
                      + .800_r8*rxt(k,414)*y(k,90) + .300_r8*rxt(k,489)*y(k,128) &
                      + .500_r8*rxt(k,464)*y(k,139) + rxt(k,416)*y(k,170) &
                      + .680_r8*rxt(k,574)*y(k,242)
         mat(k,886) = rxt(k,432)*y(k,154)
         mat(k,1296) = rxt(k,436)*y(k,154) + .800_r8*rxt(k,434)*y(k,262) &
                      + .150_r8*rxt(k,435)*y(k,267)
         mat(k,1259) = .340_r8*rxt(k,565)*y(k,154) + .930_r8*rxt(k,563)*y(k,262)
         mat(k,1280) = .320_r8*rxt(k,570)*y(k,154) + .950_r8*rxt(k,568)*y(k,262)
         mat(k,1328) = .250_r8*rxt(k,506)*y(k,154) + .250_r8*rxt(k,503)*y(k,261) &
                      + .800_r8*rxt(k,504)*y(k,262)
      end do
      end subroutine nlnmat01
      subroutine nlnmat02( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,691) = -(rxt(k,341)*y(k,70) + rxt(k,342)*y(k,282) + rxt(k,364)*y(k,281))
         mat(k,1935) = -rxt(k,341)*y(k,52)
         mat(k,2118) = -rxt(k,342)*y(k,52)
         mat(k,1994) = -rxt(k,364)*y(k,52)
         mat(k,153) = -(rxt(k,343)*y(k,282))
         mat(k,2045) = -rxt(k,343)*y(k,53)
         mat(k,1173) = -(rxt(k,401)*y(k,156) + rxt(k,402)*y(k,282))
         mat(k,2411) = -rxt(k,401)*y(k,54)
         mat(k,2160) = -rxt(k,402)*y(k,54)
         mat(k,785) = .800_r8*rxt(k,469)*y(k,282)
         mat(k,436) = rxt(k,440)*y(k,156)
         mat(k,326) = rxt(k,397)*y(k,282)
         mat(k,399) = .500_r8*rxt(k,398)*y(k,282)
         mat(k,1141) = .500_r8*rxt(k,421)*y(k,164)
         mat(k,1465) = .100_r8*rxt(k,465)*y(k,164)
         mat(k,2288) = .400_r8*rxt(k,471)*y(k,253) + rxt(k,396)*y(k,258) &
                      + .270_r8*rxt(k,424)*y(k,259) + rxt(k,442)*y(k,264) + rxt(k,461) &
                      *y(k,278) + rxt(k,432)*y(k,284)
         mat(k,2411) = mat(k,2411) + rxt(k,440)*y(k,16)
         mat(k,2645) = .500_r8*rxt(k,421)*y(k,33) + .100_r8*rxt(k,465)*y(k,141)
         mat(k,1025) = .400_r8*rxt(k,471)*y(k,154)
         mat(k,914) = rxt(k,396)*y(k,154) + 3.200_r8*rxt(k,393)*y(k,258) &
                      + .800_r8*rxt(k,394)*y(k,262)
         mat(k,895) = .270_r8*rxt(k,424)*y(k,154)
         mat(k,1653) = .800_r8*rxt(k,394)*y(k,258)
         mat(k,673) = rxt(k,442)*y(k,154)
         mat(k,1843) = .200_r8*rxt(k,460)*y(k,278)
         mat(k,793) = rxt(k,461)*y(k,154) + .200_r8*rxt(k,460)*y(k,267)
         mat(k,2160) = mat(k,2160) + .800_r8*rxt(k,469)*y(k,1) + rxt(k,397)*y(k,30) &
                      + .500_r8*rxt(k,398)*y(k,31)
         mat(k,884) = rxt(k,432)*y(k,154)
         mat(k,449) = -(rxt(k,344)*y(k,70) + rxt(k,345)*y(k,282))
         mat(k,1928) = -rxt(k,344)*y(k,55)
         mat(k,2086) = -rxt(k,345)*y(k,55)
         mat(k,123) = -(rxt(k,403)*y(k,282))
         mat(k,2042) = -rxt(k,403)*y(k,56)
         mat(k,1063) = -(rxt(k,439)*y(k,282))
         mat(k,2152) = -rxt(k,439)*y(k,57)
         mat(k,784) = .800_r8*rxt(k,469)*y(k,282)
         mat(k,1001) = .520_r8*rxt(k,553)*y(k,164)
         mat(k,435) = .500_r8*rxt(k,440)*y(k,156)
         mat(k,1092) = .520_r8*rxt(k,556)*y(k,164)
         mat(k,2282) = .250_r8*rxt(k,471)*y(k,253) + .820_r8*rxt(k,424)*y(k,259) &
                      + .500_r8*rxt(k,442)*y(k,264) + .270_r8*rxt(k,565)*y(k,286) &
                      + .040_r8*rxt(k,570)*y(k,287)
         mat(k,2403) = .500_r8*rxt(k,440)*y(k,16)
         mat(k,2639) = .520_r8*rxt(k,553)*y(k,6) + .520_r8*rxt(k,556)*y(k,140)
         mat(k,1207) = .500_r8*rxt(k,574)*y(k,282)
         mat(k,1024) = .250_r8*rxt(k,471)*y(k,154)
         mat(k,894) = .820_r8*rxt(k,424)*y(k,154) + .820_r8*rxt(k,422)*y(k,262)
         mat(k,1648) = .820_r8*rxt(k,422)*y(k,259) + .150_r8*rxt(k,563)*y(k,286) &
                      + .025_r8*rxt(k,568)*y(k,287)
         mat(k,672) = .500_r8*rxt(k,442)*y(k,154)
         mat(k,2152) = mat(k,2152) + .800_r8*rxt(k,469)*y(k,1) + .500_r8*rxt(k,574) &
                      *y(k,242)
         mat(k,1251) = .270_r8*rxt(k,565)*y(k,154) + .150_r8*rxt(k,563)*y(k,262)
         mat(k,1270) = .040_r8*rxt(k,570)*y(k,154) + .025_r8*rxt(k,568)*y(k,262)
         mat(k,1372) = -(rxt(k,427)*y(k,156) + rxt(k,428)*y(k,282))
         mat(k,2425) = -rxt(k,427)*y(k,58)
         mat(k,2174) = -rxt(k,428)*y(k,58)
         mat(k,1242) = rxt(k,429)*y(k,282)
         mat(k,1361) = .880_r8*rxt(k,451)*y(k,164)
         mat(k,1468) = .500_r8*rxt(k,465)*y(k,164)
         mat(k,2301) = .170_r8*rxt(k,524)*y(k,263) + .050_r8*rxt(k,487)*y(k,270) &
                      + .250_r8*rxt(k,449)*y(k,274) + .170_r8*rxt(k,530)*y(k,277) &
                      + .400_r8*rxt(k,540)*y(k,288) + .250_r8*rxt(k,506)*y(k,290) &
                      + .540_r8*rxt(k,546)*y(k,291) + .510_r8*rxt(k,549)*y(k,293)
         mat(k,2425) = mat(k,2425) + .050_r8*rxt(k,488)*y(k,270) + .250_r8*rxt(k,448) &
                      *y(k,274) + .250_r8*rxt(k,507)*y(k,290)
         mat(k,967) = rxt(k,430)*y(k,282)
         mat(k,2656) = .880_r8*rxt(k,451)*y(k,135) + .500_r8*rxt(k,465)*y(k,141)
         mat(k,1518) = .250_r8*rxt(k,445)*y(k,274) + .250_r8*rxt(k,503)*y(k,290)
         mat(k,1665) = .240_r8*rxt(k,446)*y(k,274) + .500_r8*rxt(k,434)*y(k,285) &
                      + .100_r8*rxt(k,504)*y(k,290)
         mat(k,876) = .170_r8*rxt(k,524)*y(k,154) + .070_r8*rxt(k,523)*y(k,267)
         mat(k,1856) = .070_r8*rxt(k,523)*y(k,263) + .070_r8*rxt(k,529)*y(k,277)
         mat(k,1424) = .050_r8*rxt(k,487)*y(k,154) + .050_r8*rxt(k,488)*y(k,156)
         mat(k,1448) = .250_r8*rxt(k,449)*y(k,154) + .250_r8*rxt(k,448)*y(k,156) &
                      + .250_r8*rxt(k,445)*y(k,261) + .240_r8*rxt(k,446)*y(k,262)
         mat(k,975) = .170_r8*rxt(k,530)*y(k,154) + .070_r8*rxt(k,529)*y(k,267)
         mat(k,2174) = mat(k,2174) + rxt(k,429)*y(k,113) + rxt(k,430)*y(k,157)
         mat(k,1293) = .500_r8*rxt(k,434)*y(k,262)
         mat(k,852) = .400_r8*rxt(k,540)*y(k,154)
         mat(k,1325) = .250_r8*rxt(k,506)*y(k,154) + .250_r8*rxt(k,507)*y(k,156) &
                      + .250_r8*rxt(k,503)*y(k,261) + .100_r8*rxt(k,504)*y(k,262)
         mat(k,868) = .540_r8*rxt(k,546)*y(k,154)
         mat(k,608) = .510_r8*rxt(k,549)*y(k,154)
         mat(k,799) = -(rxt(k,408)*y(k,282))
         mat(k,2130) = -rxt(k,408)*y(k,59)
         mat(k,1135) = .120_r8*rxt(k,421)*y(k,164)
         mat(k,2632) = .120_r8*rxt(k,421)*y(k,33)
         mat(k,1508) = .100_r8*rxt(k,405)*y(k,262) + .150_r8*rxt(k,406)*y(k,267)
         mat(k,1642) = .100_r8*rxt(k,405)*y(k,261)
         mat(k,1823) = .150_r8*rxt(k,406)*y(k,261) + .150_r8*rxt(k,456)*y(k,276)
         mat(k,1487) = .150_r8*rxt(k,456)*y(k,267)
         mat(k,700) = -(rxt(k,409)*y(k,282))
         mat(k,2119) = -rxt(k,409)*y(k,60)
         mat(k,1507) = .400_r8*rxt(k,406)*y(k,267)
         mat(k,1816) = .400_r8*rxt(k,406)*y(k,261) + .400_r8*rxt(k,456)*y(k,276)
         mat(k,1485) = .400_r8*rxt(k,456)*y(k,267)
         mat(k,457) = -(rxt(k,346)*y(k,70) + rxt(k,347)*y(k,282))
         mat(k,1929) = -rxt(k,346)*y(k,61)
         mat(k,2087) = -rxt(k,347)*y(k,61)
         mat(k,903) = -(rxt(k,376)*y(k,282))
         mat(k,2140) = -rxt(k,376)*y(k,62)
         mat(k,912) = .300_r8*rxt(k,394)*y(k,262)
         mat(k,1644) = .300_r8*rxt(k,394)*y(k,258) + 2.000_r8*rxt(k,373)*y(k,262) &
                      + .250_r8*rxt(k,479)*y(k,269) + .250_r8*rxt(k,484)*y(k,270) &
                      + .200_r8*rxt(k,493)*y(k,271) + .250_r8*rxt(k,446)*y(k,274) &
                      + .250_r8*rxt(k,558)*y(k,280) + .500_r8*rxt(k,434)*y(k,285) &
                      + .250_r8*rxt(k,563)*y(k,286) + .250_r8*rxt(k,568)*y(k,287) &
                      + .300_r8*rxt(k,504)*y(k,290)
         mat(k,1382) = .250_r8*rxt(k,479)*y(k,262)
         mat(k,1413) = .250_r8*rxt(k,484)*y(k,262)
         mat(k,1338) = .200_r8*rxt(k,493)*y(k,262)
         mat(k,1442) = .250_r8*rxt(k,446)*y(k,262)
         mat(k,1192) = .250_r8*rxt(k,558)*y(k,262)
         mat(k,1290) = .500_r8*rxt(k,434)*y(k,262)
         mat(k,1249) = .250_r8*rxt(k,563)*y(k,262)
         mat(k,1269) = .250_r8*rxt(k,568)*y(k,262)
         mat(k,1318) = .300_r8*rxt(k,504)*y(k,262)
         mat(k,498) = -(rxt(k,377)*y(k,282))
         mat(k,2093) = -rxt(k,377)*y(k,63)
         mat(k,1639) = rxt(k,374)*y(k,267)
         mat(k,1803) = rxt(k,374)*y(k,262)
         mat(k,1600) = -(rxt(k,225)*y(k,70) + rxt(k,326)*y(k,89) + rxt(k,378)*y(k,282) &
                      + (rxt(k,384) + rxt(k,385) + rxt(k,386)) * y(k,281))
         mat(k,1952) = -rxt(k,225)*y(k,64)
         mat(k,984) = -rxt(k,326)*y(k,64)
         mat(k,2185) = -rxt(k,378)*y(k,64)
         mat(k,1998) = -(rxt(k,384) + rxt(k,385) + rxt(k,386)) * y(k,64)
         mat(k,1146) = .100_r8*rxt(k,421)*y(k,164)
         mat(k,2664) = .100_r8*rxt(k,421)*y(k,33)
         mat(k,157) = -(rxt(k,348)*y(k,282))
         mat(k,2046) = -rxt(k,348)*y(k,65)
         mat(k,504) = -(rxt(k,283)*y(k,281) + rxt(k,349)*y(k,70) + rxt(k,350)*y(k,282))
         mat(k,1992) = -rxt(k,283)*y(k,66)
         mat(k,1930) = -rxt(k,349)*y(k,66)
         mat(k,2094) = -rxt(k,350)*y(k,66)
         mat(k,161) = -(rxt(k,351)*y(k,282))
         mat(k,2047) = -rxt(k,351)*y(k,67)
         mat(k,1124) = -((rxt(k,352) + rxt(k,353)) * y(k,262) + (rxt(k,354) + rxt(k,355) &
                      ) * y(k,267) + rxt(k,356)*y(k,154) + rxt(k,357)*y(k,156))
         mat(k,1651) = -(rxt(k,352) + rxt(k,353)) * y(k,68)
         mat(k,1842) = -(rxt(k,354) + rxt(k,355)) * y(k,68)
         mat(k,2286) = -rxt(k,356)*y(k,68)
         mat(k,2408) = -rxt(k,357)*y(k,68)
         mat(k,357) = rxt(k,339)*y(k,70) + rxt(k,340)*y(k,282)
         mat(k,1945) = rxt(k,339)*y(k,47)
         mat(k,2157) = rxt(k,340)*y(k,47)
         mat(k,405) = -(rxt(k,358)*y(k,70) + rxt(k,359)*y(k,282))
         mat(k,1926) = -rxt(k,358)*y(k,69)
         mat(k,2081) = -rxt(k,359)*y(k,69)
         mat(k,1961) = -(rxt(k,224)*y(k,51) + rxt(k,225)*y(k,64) + rxt(k,226)*y(k,93) &
                      + rxt(k,227)*y(k,95) + (rxt(k,228) + rxt(k,229)) * y(k,267) &
                      + rxt(k,230)*y(k,155) + rxt(k,232)*y(k,164) + rxt(k,239)*y(k,75) &
                      + rxt(k,248)*y(k,109) + rxt(k,274)*y(k,22) + rxt(k,332)*y(k,26) &
                      + rxt(k,334)*y(k,29) + rxt(k,336)*y(k,45) + rxt(k,339)*y(k,47) &
                      + rxt(k,341)*y(k,52) + rxt(k,344)*y(k,55) + rxt(k,346)*y(k,61) &
                      + rxt(k,349)*y(k,66) + rxt(k,399)*y(k,32) + (rxt(k,576) &
                      + rxt(k,577)) * y(k,83))
         mat(k,1726) = -rxt(k,224)*y(k,70)
         mat(k,1605) = -rxt(k,225)*y(k,70)
         mat(k,1559) = -rxt(k,226)*y(k,70)
         mat(k,710) = -rxt(k,227)*y(k,70)
         mat(k,1875) = -(rxt(k,228) + rxt(k,229)) * y(k,70)
         mat(k,2576) = -rxt(k,230)*y(k,70)
         mat(k,2671) = -rxt(k,232)*y(k,70)
         mat(k,1037) = -rxt(k,239)*y(k,70)
         mat(k,1752) = -rxt(k,248)*y(k,70)
         mat(k,958) = -rxt(k,274)*y(k,70)
         mat(k,233) = -rxt(k,332)*y(k,70)
         mat(k,295) = -rxt(k,334)*y(k,70)
         mat(k,577) = -rxt(k,336)*y(k,70)
         mat(k,359) = -rxt(k,339)*y(k,70)
         mat(k,695) = -rxt(k,341)*y(k,70)
         mat(k,454) = -rxt(k,344)*y(k,70)
         mat(k,461) = -rxt(k,346)*y(k,70)
         mat(k,506) = -rxt(k,349)*y(k,70)
         mat(k,346) = -rxt(k,399)*y(k,70)
         mat(k,1049) = -(rxt(k,576) + rxt(k,577)) * y(k,70)
         mat(k,2373) = rxt(k,269)*y(k,74)
         mat(k,233) = mat(k,233) + 5.000_r8*rxt(k,332)*y(k,70) + 3.060_r8*rxt(k,333) &
                      *y(k,282)
         mat(k,295) = mat(k,295) + 2.000_r8*rxt(k,334)*y(k,70) + 2.000_r8*rxt(k,335) &
                      *y(k,282)
         mat(k,121) = 4.000_r8*rxt(k,251)*y(k,281)
         mat(k,183) = rxt(k,252)*y(k,281)
         mat(k,139) = 2.000_r8*rxt(k,253)*y(k,281)
         mat(k,194) = 2.000_r8*rxt(k,254)*y(k,281)
         mat(k,143) = 2.000_r8*rxt(k,255)*y(k,281)
         mat(k,199) = rxt(k,256)*y(k,281)
         mat(k,147) = 2.000_r8*rxt(k,257)*y(k,281)
         mat(k,150) = rxt(k,338)*y(k,282)
         mat(k,154) = 3.000_r8*rxt(k,343)*y(k,282)
         mat(k,454) = mat(k,454) + rxt(k,345)*y(k,282)
         mat(k,158) = rxt(k,348)*y(k,282)
         mat(k,162) = 2.000_r8*rxt(k,351)*y(k,282)
         mat(k,1130) = 2.000_r8*rxt(k,356)*y(k,154) + 2.000_r8*rxt(k,357)*y(k,156) &
                      + 2.000_r8*rxt(k,352)*y(k,262) + rxt(k,355)*y(k,267)
         mat(k,409) = rxt(k,359)*y(k,282)
         mat(k,1961) = mat(k,1961) + 5.000_r8*rxt(k,332)*y(k,26) + 2.000_r8*rxt(k,334) &
                      *y(k,29)
         mat(k,2605) = rxt(k,269)*y(k,21) + (4.000_r8*rxt(k,234)+2.000_r8*rxt(k,236)) &
                      *y(k,74) + rxt(k,306)*y(k,125) + rxt(k,238)*y(k,154) &
                      + rxt(k,243)*y(k,163) + rxt(k,587)*y(k,183) + rxt(k,233) &
                      *y(k,262) + rxt(k,244)*y(k,282)
         mat(k,305) = rxt(k,331)*y(k,281)
         mat(k,300) = rxt(k,365)*y(k,281) + rxt(k,360)*y(k,282)
         mat(k,314) = rxt(k,366)*y(k,281) + rxt(k,361)*y(k,282)
         mat(k,352) = rxt(k,367)*y(k,281) + rxt(k,362)*y(k,282)
         mat(k,1775) = rxt(k,246)*y(k,163) + rxt(k,258)*y(k,281) + rxt(k,247)*y(k,282)
         mat(k,1905) = rxt(k,306)*y(k,74)
         mat(k,2317) = 2.000_r8*rxt(k,356)*y(k,68) + rxt(k,238)*y(k,74)
         mat(k,2443) = 2.000_r8*rxt(k,357)*y(k,68)
         mat(k,2229) = rxt(k,243)*y(k,74) + rxt(k,246)*y(k,101)
         mat(k,1584) = rxt(k,587)*y(k,74)
         mat(k,1678) = 2.000_r8*rxt(k,352)*y(k,68) + rxt(k,233)*y(k,74)
         mat(k,1875) = mat(k,1875) + rxt(k,355)*y(k,68)
         mat(k,2007) = 4.000_r8*rxt(k,251)*y(k,37) + rxt(k,252)*y(k,38) &
                      + 2.000_r8*rxt(k,253)*y(k,40) + 2.000_r8*rxt(k,254)*y(k,41) &
                      + 2.000_r8*rxt(k,255)*y(k,42) + rxt(k,256)*y(k,43) &
                      + 2.000_r8*rxt(k,257)*y(k,44) + rxt(k,331)*y(k,81) + rxt(k,365) &
                      *y(k,98) + rxt(k,366)*y(k,99) + rxt(k,367)*y(k,100) + rxt(k,258) &
                      *y(k,101)
         mat(k,2194) = 3.060_r8*rxt(k,333)*y(k,26) + 2.000_r8*rxt(k,335)*y(k,29) &
                      + rxt(k,338)*y(k,46) + 3.000_r8*rxt(k,343)*y(k,53) + rxt(k,345) &
                      *y(k,55) + rxt(k,348)*y(k,65) + 2.000_r8*rxt(k,351)*y(k,67) &
                      + rxt(k,359)*y(k,69) + rxt(k,244)*y(k,74) + rxt(k,360)*y(k,98) &
                      + rxt(k,361)*y(k,99) + rxt(k,362)*y(k,100) + rxt(k,247)*y(k,101)
      end do
      end subroutine nlnmat02
      subroutine nlnmat03( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,1921) = rxt(k,239)*y(k,75)
         mat(k,2592) = 2.000_r8*rxt(k,235)*y(k,74)
         mat(k,1033) = rxt(k,239)*y(k,70) + (rxt(k,673)+rxt(k,682)+rxt(k,691)) &
                      *y(k,101)
         mat(k,1765) = (rxt(k,673)+rxt(k,682)+rxt(k,691))*y(k,75) + (rxt(k,604) &
                       +rxt(k,663)+rxt(k,674)+rxt(k,683))*y(k,109)
         mat(k,1741) = (rxt(k,604)+rxt(k,663)+rxt(k,674)+rxt(k,683))*y(k,101)
         mat(k,2591) = 2.000_r8*rxt(k,260)*y(k,74)
         mat(k,630) = -(rxt(k,231)*y(k,282))
         mat(k,2109) = -rxt(k,231)*y(k,73)
         mat(k,1933) = rxt(k,230)*y(k,155)
         mat(k,1768) = rxt(k,620)*y(k,144)
         mat(k,442) = rxt(k,620)*y(k,101)
         mat(k,2546) = rxt(k,230)*y(k,70)
         mat(k,2617) = -(rxt(k,233)*y(k,262) + (4._r8*rxt(k,234) + 4._r8*rxt(k,235) &
                      + 4._r8*rxt(k,236) + 4._r8*rxt(k,260)) * y(k,74) + rxt(k,237) &
                      *y(k,267) + rxt(k,238)*y(k,154) + rxt(k,240)*y(k,155) + rxt(k,243) &
                      *y(k,163) + (rxt(k,244) + rxt(k,245)) * y(k,282) + (rxt(k,268) &
                      + rxt(k,269) + rxt(k,270)) * y(k,21) + (rxt(k,305) + rxt(k,306) &
                      + rxt(k,307)) * y(k,125) + rxt(k,587)*y(k,183))
         mat(k,1687) = -rxt(k,233)*y(k,74)
         mat(k,1887) = -rxt(k,237)*y(k,74)
         mat(k,2329) = -rxt(k,238)*y(k,74)
         mat(k,2588) = -rxt(k,240)*y(k,74)
         mat(k,2241) = -rxt(k,243)*y(k,74)
         mat(k,2206) = -(rxt(k,244) + rxt(k,245)) * y(k,74)
         mat(k,2385) = -(rxt(k,268) + rxt(k,269) + rxt(k,270)) * y(k,74)
         mat(k,1917) = -(rxt(k,305) + rxt(k,306) + rxt(k,307)) * y(k,74)
         mat(k,1592) = -rxt(k,587)*y(k,74)
         mat(k,1973) = rxt(k,248)*y(k,109) + rxt(k,232)*y(k,164) + rxt(k,229)*y(k,267)
         mat(k,1043) = rxt(k,241)*y(k,163)
         mat(k,1786) = rxt(k,259)*y(k,281)
         mat(k,1763) = rxt(k,248)*y(k,70) + rxt(k,249)*y(k,163) + rxt(k,250)*y(k,282)
         mat(k,2241) = mat(k,2241) + rxt(k,241)*y(k,75) + rxt(k,249)*y(k,109)
         mat(k,2683) = rxt(k,232)*y(k,70)
         mat(k,559) = rxt(k,592)*y(k,183)
         mat(k,1592) = mat(k,1592) + rxt(k,592)*y(k,166)
         mat(k,1887) = mat(k,1887) + rxt(k,229)*y(k,70)
         mat(k,2019) = rxt(k,259)*y(k,101)
         mat(k,2206) = mat(k,2206) + rxt(k,250)*y(k,109)
         mat(k,1034) = -(rxt(k,239)*y(k,70) + rxt(k,241)*y(k,163) + rxt(k,242) &
                      *y(k,282) + (rxt(k,673) + rxt(k,682) + rxt(k,691)) * y(k,101))
         mat(k,1940) = -rxt(k,239)*y(k,75)
         mat(k,2213) = -rxt(k,241)*y(k,75)
         mat(k,2149) = -rxt(k,242)*y(k,75)
         mat(k,1769) = -(rxt(k,673) + rxt(k,682) + rxt(k,691)) * y(k,75)
         mat(k,2596) = rxt(k,240)*y(k,155)
         mat(k,2557) = rxt(k,240)*y(k,74)
         mat(k,1219) = -(rxt(k,388)*y(k,282))
         mat(k,2164) = -rxt(k,388)*y(k,77)
         mat(k,1007) = .230_r8*rxt(k,553)*y(k,164)
         mat(k,2336) = rxt(k,263)*y(k,51)
         mat(k,340) = .350_r8*rxt(k,390)*y(k,282)
         mat(k,666) = .630_r8*rxt(k,392)*y(k,164)
         mat(k,1142) = .560_r8*rxt(k,421)*y(k,164)
         mat(k,1714) = rxt(k,263)*y(k,17) + rxt(k,224)*y(k,70) + rxt(k,369)*y(k,156) &
                      + rxt(k,370)*y(k,163) + rxt(k,371)*y(k,282)
         mat(k,450) = rxt(k,344)*y(k,70)
         mat(k,1371) = rxt(k,427)*y(k,156) + rxt(k,428)*y(k,282)
         mat(k,1125) = rxt(k,356)*y(k,154) + rxt(k,357)*y(k,156) + (rxt(k,352) &
                       +rxt(k,353))*y(k,262) + rxt(k,355)*y(k,267)
         mat(k,1948) = rxt(k,224)*y(k,51) + rxt(k,344)*y(k,55)
         mat(k,1079) = rxt(k,415)*y(k,282)
         mat(k,941) = .620_r8*rxt(k,498)*y(k,164)
         mat(k,1359) = .650_r8*rxt(k,451)*y(k,164)
         mat(k,1100) = .230_r8*rxt(k,556)*y(k,164)
         mat(k,1466) = .560_r8*rxt(k,465)*y(k,164)
         mat(k,2292) = rxt(k,356)*y(k,68) + .170_r8*rxt(k,524)*y(k,263) &
                      + .220_r8*rxt(k,449)*y(k,274) + .400_r8*rxt(k,527)*y(k,275) &
                      + .350_r8*rxt(k,530)*y(k,277) + .225_r8*rxt(k,565)*y(k,286) &
                      + .250_r8*rxt(k,506)*y(k,290)
         mat(k,2415) = rxt(k,369)*y(k,51) + rxt(k,427)*y(k,58) + rxt(k,357)*y(k,68) &
                      + .220_r8*rxt(k,448)*y(k,274) + .500_r8*rxt(k,507)*y(k,290)
         mat(k,2215) = rxt(k,370)*y(k,51) + rxt(k,581)*y(k,167)
         mat(k,2649) = .230_r8*rxt(k,553)*y(k,6) + .630_r8*rxt(k,392)*y(k,28) &
                      + .560_r8*rxt(k,421)*y(k,33) + .620_r8*rxt(k,498)*y(k,127) &
                      + .650_r8*rxt(k,451)*y(k,135) + .230_r8*rxt(k,556)*y(k,140) &
                      + .560_r8*rxt(k,465)*y(k,141)
         mat(k,428) = rxt(k,581)*y(k,163) + rxt(k,582)*y(k,282)
         mat(k,1209) = .700_r8*rxt(k,574)*y(k,282)
         mat(k,1513) = .220_r8*rxt(k,445)*y(k,274) + .250_r8*rxt(k,503)*y(k,290)
         mat(k,1657) = (rxt(k,352)+rxt(k,353))*y(k,68) + .110_r8*rxt(k,446)*y(k,274) &
                      + .125_r8*rxt(k,563)*y(k,286) + .200_r8*rxt(k,504)*y(k,290)
         mat(k,875) = .170_r8*rxt(k,524)*y(k,154) + .070_r8*rxt(k,523)*y(k,267)
         mat(k,1847) = rxt(k,355)*y(k,68) + .070_r8*rxt(k,523)*y(k,263) &
                      + .160_r8*rxt(k,526)*y(k,275) + .140_r8*rxt(k,529)*y(k,277)
         mat(k,1444) = .220_r8*rxt(k,449)*y(k,154) + .220_r8*rxt(k,448)*y(k,156) &
                      + .220_r8*rxt(k,445)*y(k,261) + .110_r8*rxt(k,446)*y(k,262)
         mat(k,838) = .400_r8*rxt(k,527)*y(k,154) + .160_r8*rxt(k,526)*y(k,267)
         mat(k,974) = .350_r8*rxt(k,530)*y(k,154) + .140_r8*rxt(k,529)*y(k,267)
         mat(k,2164) = mat(k,2164) + .350_r8*rxt(k,390)*y(k,27) + rxt(k,371)*y(k,51) &
                      + rxt(k,428)*y(k,58) + rxt(k,415)*y(k,91) + rxt(k,582)*y(k,167) &
                      + .700_r8*rxt(k,574)*y(k,242)
         mat(k,1254) = .225_r8*rxt(k,565)*y(k,154) + .125_r8*rxt(k,563)*y(k,262)
         mat(k,1322) = .250_r8*rxt(k,506)*y(k,154) + .500_r8*rxt(k,507)*y(k,156) &
                      + .250_r8*rxt(k,503)*y(k,261) + .200_r8*rxt(k,504)*y(k,262)
         mat(k,998) = .270_r8*rxt(k,553)*y(k,164)
         mat(k,1137) = .200_r8*rxt(k,421)*y(k,164)
         mat(k,800) = rxt(k,408)*y(k,282)
         mat(k,701) = .500_r8*rxt(k,409)*y(k,282)
         mat(k,1218) = rxt(k,388)*y(k,282)
         mat(k,1182) = .800_r8*rxt(k,414)*y(k,282)
         mat(k,1077) = rxt(k,415)*y(k,282)
         mat(k,1069) = rxt(k,380)*y(k,282)
         mat(k,715) = .500_r8*rxt(k,464)*y(k,282)
         mat(k,1090) = .270_r8*rxt(k,556)*y(k,164)
         mat(k,1462) = .100_r8*rxt(k,465)*y(k,164)
         mat(k,2277) = rxt(k,407)*y(k,261) + .900_r8*rxt(k,565)*y(k,286)
         mat(k,2634) = .270_r8*rxt(k,553)*y(k,6) + .200_r8*rxt(k,421)*y(k,33) &
                      + .270_r8*rxt(k,556)*y(k,140) + .100_r8*rxt(k,465)*y(k,141)
         mat(k,1206) = 1.800_r8*rxt(k,574)*y(k,282)
         mat(k,1509) = rxt(k,407)*y(k,154) + 4.000_r8*rxt(k,404)*y(k,261) &
                      + .900_r8*rxt(k,405)*y(k,262) + rxt(k,478)*y(k,269) &
                      + 2.000_r8*rxt(k,454)*y(k,276) + rxt(k,503)*y(k,290)
         mat(k,1645) = .900_r8*rxt(k,405)*y(k,261) + rxt(k,455)*y(k,276) &
                      + .500_r8*rxt(k,563)*y(k,286)
         mat(k,1833) = .450_r8*rxt(k,456)*y(k,276)
         mat(k,1383) = rxt(k,478)*y(k,261)
         mat(k,1488) = 2.000_r8*rxt(k,454)*y(k,261) + rxt(k,455)*y(k,262) &
                      + .450_r8*rxt(k,456)*y(k,267) + 4.000_r8*rxt(k,457)*y(k,276)
         mat(k,2141) = rxt(k,408)*y(k,59) + .500_r8*rxt(k,409)*y(k,60) + rxt(k,388) &
                      *y(k,77) + .800_r8*rxt(k,414)*y(k,90) + rxt(k,415)*y(k,91) &
                      + rxt(k,380)*y(k,103) + .500_r8*rxt(k,464)*y(k,139) &
                      + 1.800_r8*rxt(k,574)*y(k,242)
         mat(k,1250) = .900_r8*rxt(k,565)*y(k,154) + .500_r8*rxt(k,563)*y(k,262)
         mat(k,1319) = rxt(k,503)*y(k,261)
         mat(k,232) = .470_r8*rxt(k,333)*y(k,282)
         mat(k,1123) = rxt(k,353)*y(k,262) + rxt(k,354)*y(k,267)
         mat(k,406) = rxt(k,358)*y(k,70) + rxt(k,359)*y(k,282)
         mat(k,1927) = rxt(k,358)*y(k,69)
         mat(k,1638) = rxt(k,353)*y(k,68)
         mat(k,1796) = rxt(k,354)*y(k,68)
         mat(k,2082) = .470_r8*rxt(k,333)*y(k,26) + rxt(k,359)*y(k,69)
         mat(k,317) = -(rxt(k,330)*y(k,281))
         mat(k,1990) = -rxt(k,330)*y(k,80)
         mat(k,182) = rxt(k,252)*y(k,281)
         mat(k,187) = rxt(k,282)*y(k,281)
         mat(k,193) = rxt(k,254)*y(k,281)
         mat(k,142) = 2.000_r8*rxt(k,255)*y(k,281)
         mat(k,197) = 2.000_r8*rxt(k,256)*y(k,281)
         mat(k,146) = rxt(k,257)*y(k,281)
         mat(k,166) = 2.000_r8*rxt(k,284)*y(k,281)
         mat(k,313) = rxt(k,366)*y(k,281) + rxt(k,361)*y(k,282)
         mat(k,349) = rxt(k,367)*y(k,281) + rxt(k,362)*y(k,282)
         mat(k,1990) = mat(k,1990) + rxt(k,252)*y(k,38) + rxt(k,282)*y(k,39) &
                      + rxt(k,254)*y(k,41) + 2.000_r8*rxt(k,255)*y(k,42) &
                      + 2.000_r8*rxt(k,256)*y(k,43) + rxt(k,257)*y(k,44) &
                      + 2.000_r8*rxt(k,284)*y(k,94) + rxt(k,366)*y(k,99) + rxt(k,367) &
                      *y(k,100)
         mat(k,2068) = rxt(k,361)*y(k,99) + rxt(k,362)*y(k,100)
         mat(k,303) = -(rxt(k,331)*y(k,281))
         mat(k,1988) = -rxt(k,331)*y(k,81)
         mat(k,138) = rxt(k,253)*y(k,281)
         mat(k,192) = rxt(k,254)*y(k,281)
         mat(k,299) = rxt(k,365)*y(k,281) + rxt(k,360)*y(k,282)
         mat(k,1988) = mat(k,1988) + rxt(k,253)*y(k,40) + rxt(k,254)*y(k,41) &
                      + rxt(k,365)*y(k,98)
         mat(k,2065) = rxt(k,360)*y(k,98)
         mat(k,253) = -(rxt(k,522)*y(k,282))
         mat(k,2055) = -rxt(k,522)*y(k,82)
         mat(k,247) = .180_r8*rxt(k,542)*y(k,282)
         mat(k,2055) = mat(k,2055) + .180_r8*rxt(k,542)*y(k,244)
         mat(k,1044) = -(rxt(k,575)*y(k,21) + (rxt(k,576) + rxt(k,577)) * y(k,70) &
                      + rxt(k,578)*y(k,125) + rxt(k,579)*y(k,156) + (rxt(k,580) &
                      + rxt(k,594)) * y(k,282))
         mat(k,2364) = -rxt(k,575)*y(k,83)
         mat(k,1941) = -(rxt(k,576) + rxt(k,577)) * y(k,83)
         mat(k,1896) = -rxt(k,578)*y(k,83)
         mat(k,2401) = -rxt(k,579)*y(k,83)
         mat(k,2150) = -(rxt(k,580) + rxt(k,594)) * y(k,83)
         mat(k,827) = rxt(k,410)*y(k,267)
         mat(k,1788) = rxt(k,410)*y(k,266)
         mat(k,982) = -(rxt(k,326)*y(k,64) + rxt(k,327)*y(k,93) + rxt(k,328)*y(k,294) &
                      + rxt(k,329)*y(k,106))
         mat(k,1597) = -rxt(k,326)*y(k,89)
         mat(k,1554) = -rxt(k,327)*y(k,89)
         mat(k,2689) = -rxt(k,328)*y(k,89)
         mat(k,2510) = -rxt(k,329)*y(k,89)
         mat(k,188) = rxt(k,282)*y(k,281)
         mat(k,198) = rxt(k,256)*y(k,281)
         mat(k,318) = 2.000_r8*rxt(k,330)*y(k,281)
         mat(k,304) = rxt(k,331)*y(k,281)
         mat(k,1995) = rxt(k,282)*y(k,39) + rxt(k,256)*y(k,43) + 2.000_r8*rxt(k,330) &
                      *y(k,80) + rxt(k,331)*y(k,81)
         mat(k,1184) = -(rxt(k,414)*y(k,282))
         mat(k,2161) = -rxt(k,414)*y(k,90)
         mat(k,724) = .700_r8*rxt(k,489)*y(k,282)
         mat(k,649) = .500_r8*rxt(k,490)*y(k,282)
         mat(k,512) = rxt(k,501)*y(k,282)
         mat(k,2289) = .050_r8*rxt(k,487)*y(k,270) + .530_r8*rxt(k,449)*y(k,274) &
                      + .225_r8*rxt(k,565)*y(k,286) + .250_r8*rxt(k,506)*y(k,290)
         mat(k,2412) = .050_r8*rxt(k,488)*y(k,270) + .530_r8*rxt(k,448)*y(k,274) &
                      + .250_r8*rxt(k,507)*y(k,290)
         mat(k,1512) = .530_r8*rxt(k,445)*y(k,274) + .250_r8*rxt(k,503)*y(k,290)
         mat(k,1654) = .260_r8*rxt(k,446)*y(k,274) + .125_r8*rxt(k,563)*y(k,286) &
                      + .100_r8*rxt(k,504)*y(k,290)
         mat(k,1417) = .050_r8*rxt(k,487)*y(k,154) + .050_r8*rxt(k,488)*y(k,156)
         mat(k,1443) = .530_r8*rxt(k,449)*y(k,154) + .530_r8*rxt(k,448)*y(k,156) &
                      + .530_r8*rxt(k,445)*y(k,261) + .260_r8*rxt(k,446)*y(k,262)
         mat(k,2161) = mat(k,2161) + .700_r8*rxt(k,489)*y(k,128) + .500_r8*rxt(k,490) &
                      *y(k,129) + rxt(k,501)*y(k,145)
         mat(k,1252) = .225_r8*rxt(k,565)*y(k,154) + .125_r8*rxt(k,563)*y(k,262)
         mat(k,1321) = .250_r8*rxt(k,506)*y(k,154) + .250_r8*rxt(k,507)*y(k,156) &
                      + .250_r8*rxt(k,503)*y(k,261) + .100_r8*rxt(k,504)*y(k,262)
      end do
      end subroutine nlnmat03
      subroutine nlnmat04( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,1078) = -(rxt(k,415)*y(k,282))
         mat(k,2154) = -rxt(k,415)*y(k,91)
         mat(k,339) = .650_r8*rxt(k,390)*y(k,282)
         mat(k,1183) = .200_r8*rxt(k,414)*y(k,282)
         mat(k,1160) = rxt(k,502)*y(k,282)
         mat(k,2284) = rxt(k,513)*y(k,255) + .050_r8*rxt(k,487)*y(k,270) &
                      + .400_r8*rxt(k,527)*y(k,275) + .170_r8*rxt(k,530)*y(k,277) &
                      + .700_r8*rxt(k,533)*y(k,283) + .600_r8*rxt(k,540)*y(k,288) &
                      + .250_r8*rxt(k,506)*y(k,290) + .340_r8*rxt(k,546)*y(k,291) &
                      + .170_r8*rxt(k,549)*y(k,293)
         mat(k,2405) = .050_r8*rxt(k,488)*y(k,270) + .250_r8*rxt(k,507)*y(k,290)
         mat(k,600) = rxt(k,513)*y(k,154)
         mat(k,1510) = .250_r8*rxt(k,503)*y(k,290)
         mat(k,1649) = .100_r8*rxt(k,504)*y(k,290)
         mat(k,1840) = .160_r8*rxt(k,526)*y(k,275) + .070_r8*rxt(k,529)*y(k,277)
         mat(k,1416) = .050_r8*rxt(k,487)*y(k,154) + .050_r8*rxt(k,488)*y(k,156)
         mat(k,837) = .400_r8*rxt(k,527)*y(k,154) + .160_r8*rxt(k,526)*y(k,267)
         mat(k,973) = .170_r8*rxt(k,530)*y(k,154) + .070_r8*rxt(k,529)*y(k,267)
         mat(k,2154) = mat(k,2154) + .650_r8*rxt(k,390)*y(k,27) + .200_r8*rxt(k,414) &
                      *y(k,90) + rxt(k,502)*y(k,146)
         mat(k,550) = .700_r8*rxt(k,533)*y(k,154)
         mat(k,850) = .600_r8*rxt(k,540)*y(k,154)
         mat(k,1320) = .250_r8*rxt(k,506)*y(k,154) + .250_r8*rxt(k,507)*y(k,156) &
                      + .250_r8*rxt(k,503)*y(k,261) + .100_r8*rxt(k,504)*y(k,262)
         mat(k,866) = .340_r8*rxt(k,546)*y(k,154)
         mat(k,607) = .170_r8*rxt(k,549)*y(k,154)
         mat(k,2503) = -((rxt(k,184) + rxt(k,185) + rxt(k,186)) * y(k,267) + rxt(k,190) &
                      *y(k,164))
         mat(k,1884) = -(rxt(k,184) + rxt(k,185) + rxt(k,186)) * y(k,92)
         mat(k,2680) = -rxt(k,190)*y(k,92)
         mat(k,1735) = rxt(k,371)*y(k,282)
         mat(k,1610) = rxt(k,385)*y(k,281)
         mat(k,1970) = rxt(k,226)*y(k,93)
         mat(k,988) = rxt(k,327)*y(k,93)
         mat(k,1564) = rxt(k,226)*y(k,70) + rxt(k,327)*y(k,89) + rxt(k,182)*y(k,163) &
                      + rxt(k,173)*y(k,281) + rxt(k,191)*y(k,282)
         mat(k,1551) = rxt(k,286)*y(k,281)
         mat(k,1783) = rxt(k,259)*y(k,281)
         mat(k,573) = rxt(k,212)*y(k,282)
         mat(k,2238) = rxt(k,182)*y(k,93) + rxt(k,194)*y(k,282)
         mat(k,432) = rxt(k,582)*y(k,282)
         mat(k,617) = rxt(k,588)*y(k,282)
         mat(k,1590) = rxt(k,593)*y(k,282)
         mat(k,2016) = rxt(k,385)*y(k,64) + rxt(k,173)*y(k,93) + rxt(k,286)*y(k,97) &
                      + rxt(k,259)*y(k,101)
         mat(k,2203) = rxt(k,371)*y(k,51) + rxt(k,191)*y(k,93) + rxt(k,212)*y(k,142) &
                      + rxt(k,194)*y(k,163) + rxt(k,582)*y(k,167) + rxt(k,588) &
                      *y(k,181) + rxt(k,593)*y(k,183)
         mat(k,1555) = -(rxt(k,173)*y(k,281) + rxt(k,182)*y(k,163) + rxt(k,191) &
                      *y(k,282) + rxt(k,226)*y(k,70) + rxt(k,327)*y(k,89))
         mat(k,1997) = -rxt(k,173)*y(k,93)
         mat(k,2217) = -rxt(k,182)*y(k,93)
         mat(k,2182) = -rxt(k,191)*y(k,93)
         mat(k,1950) = -rxt(k,226)*y(k,93)
         mat(k,983) = -rxt(k,327)*y(k,93)
         mat(k,1599) = rxt(k,386)*y(k,281)
         mat(k,2486) = rxt(k,184)*y(k,267)
         mat(k,1864) = rxt(k,184)*y(k,92)
         mat(k,1997) = mat(k,1997) + rxt(k,386)*y(k,64)
         mat(k,165) = -(rxt(k,284)*y(k,281))
         mat(k,1980) = -rxt(k,284)*y(k,94)
         mat(k,707) = -(rxt(k,183)*y(k,163) + rxt(k,192)*y(k,282) + rxt(k,227)*y(k,70))
         mat(k,2211) = -rxt(k,183)*y(k,95)
         mat(k,2120) = -rxt(k,192)*y(k,95)
         mat(k,1936) = -rxt(k,227)*y(k,95)
         mat(k,1817) = 2.000_r8*rxt(k,198)*y(k,267)
         mat(k,2120) = mat(k,2120) + 2.000_r8*rxt(k,197)*y(k,282)
         mat(k,320) = rxt(k,595)*y(k,294)
         mat(k,2686) = rxt(k,595)*y(k,185)
         mat(k,1540) = -(rxt(k,279)*y(k,163) + rxt(k,280)*y(k,282) + (rxt(k,285) &
                      + rxt(k,286)) * y(k,281) + (rxt(k,602) + rxt(k,667) + rxt(k,675) &
                      + rxt(k,684)) * y(k,109) + (rxt(k,603) + rxt(k,665) + rxt(k,678) &
                      + rxt(k,687)) * y(k,108) + (rxt(k,610) + rxt(k,694) + rxt(k,698) &
                      + rxt(k,702)) * y(k,110))
         mat(k,2216) = -rxt(k,279)*y(k,97)
         mat(k,2181) = -rxt(k,280)*y(k,97)
         mat(k,1996) = -(rxt(k,285) + rxt(k,286)) * y(k,97)
         mat(k,1745) = -(rxt(k,602) + rxt(k,667) + rxt(k,675) + rxt(k,684)) * y(k,97)
         mat(k,1617) = -(rxt(k,603) + rxt(k,665) + rxt(k,678) + rxt(k,687)) * y(k,97)
         mat(k,1693) = -(rxt(k,610) + rxt(k,694) + rxt(k,698) + rxt(k,702)) * y(k,97)
         mat(k,2337) = rxt(k,263)*y(k,51) + rxt(k,264)*y(k,267)
         mat(k,1715) = rxt(k,263)*y(k,17)
         mat(k,1863) = rxt(k,264)*y(k,17)
         mat(k,298) = -(rxt(k,360)*y(k,282) + rxt(k,365)*y(k,281))
         mat(k,2064) = -rxt(k,360)*y(k,98)
         mat(k,1987) = -rxt(k,365)*y(k,98)
         mat(k,312) = -(rxt(k,361)*y(k,282) + rxt(k,366)*y(k,281))
         mat(k,2067) = -rxt(k,361)*y(k,99)
         mat(k,1989) = -rxt(k,366)*y(k,99)
         mat(k,350) = -(rxt(k,362)*y(k,282) + rxt(k,367)*y(k,281))
         mat(k,2074) = -rxt(k,362)*y(k,100)
         mat(k,1991) = -rxt(k,367)*y(k,100)
         mat(k,1773) = -(rxt(k,246)*y(k,163) + rxt(k,247)*y(k,282) + (rxt(k,258) &
                      + rxt(k,259)) * y(k,281) + (rxt(k,604) + rxt(k,663) + rxt(k,674) &
                      + rxt(k,683)) * y(k,109) + (rxt(k,605) + rxt(k,664) + rxt(k,677) &
                      + rxt(k,686)) * y(k,108) + (rxt(k,609) + rxt(k,693) + rxt(k,697) &
                      + rxt(k,701)) * y(k,110) + rxt(k,620)*y(k,144) + (rxt(k,673) &
                      + rxt(k,682) + rxt(k,691)) * y(k,75))
         mat(k,2226) = -rxt(k,246)*y(k,101)
         mat(k,2191) = -rxt(k,247)*y(k,101)
         mat(k,2004) = -(rxt(k,258) + rxt(k,259)) * y(k,101)
         mat(k,1750) = -(rxt(k,604) + rxt(k,663) + rxt(k,674) + rxt(k,683)) * y(k,101)
         mat(k,1622) = -(rxt(k,605) + rxt(k,664) + rxt(k,677) + rxt(k,686)) * y(k,101)
         mat(k,1698) = -(rxt(k,609) + rxt(k,693) + rxt(k,697) + rxt(k,701)) * y(k,101)
         mat(k,443) = -rxt(k,620)*y(k,101)
         mat(k,1036) = -(rxt(k,673) + rxt(k,682) + rxt(k,691)) * y(k,101)
         mat(k,294) = rxt(k,334)*y(k,70)
         mat(k,345) = rxt(k,399)*y(k,70)
         mat(k,576) = rxt(k,336)*y(k,70)
         mat(k,358) = rxt(k,339)*y(k,70)
         mat(k,1723) = rxt(k,224)*y(k,70)
         mat(k,693) = rxt(k,341)*y(k,70)
         mat(k,452) = 2.000_r8*rxt(k,344)*y(k,70)
         mat(k,459) = rxt(k,346)*y(k,70)
         mat(k,1603) = rxt(k,225)*y(k,70)
         mat(k,505) = rxt(k,349)*y(k,70)
         mat(k,408) = rxt(k,358)*y(k,70)
         mat(k,1958) = rxt(k,334)*y(k,29) + rxt(k,399)*y(k,32) + rxt(k,336)*y(k,45) &
                      + rxt(k,339)*y(k,47) + rxt(k,224)*y(k,51) + rxt(k,341)*y(k,52) &
                      + 2.000_r8*rxt(k,344)*y(k,55) + rxt(k,346)*y(k,61) + rxt(k,225) &
                      *y(k,64) + rxt(k,349)*y(k,66) + rxt(k,358)*y(k,69) + rxt(k,577) &
                      *y(k,83) + rxt(k,226)*y(k,93) + rxt(k,227)*y(k,95) + rxt(k,248) &
                      *y(k,109) + rxt(k,228)*y(k,267)
         mat(k,2602) = rxt(k,245)*y(k,282)
         mat(k,1046) = rxt(k,577)*y(k,70)
         mat(k,1558) = rxt(k,226)*y(k,70)
         mat(k,708) = rxt(k,227)*y(k,70)
         mat(k,1750) = mat(k,1750) + rxt(k,248)*y(k,70)
         mat(k,1872) = rxt(k,228)*y(k,70)
         mat(k,2191) = mat(k,2191) + rxt(k,245)*y(k,74)
         mat(k,235) = -(rxt(k,379)*y(k,282) + rxt(k,387)*y(k,281))
         mat(k,2053) = -rxt(k,379)*y(k,102)
         mat(k,1986) = -rxt(k,387)*y(k,102)
         mat(k,1070) = -(rxt(k,380)*y(k,282))
         mat(k,2153) = -rxt(k,380)*y(k,103)
         mat(k,1002) = .050_r8*rxt(k,553)*y(k,164)
         mat(k,338) = .350_r8*rxt(k,390)*y(k,282)
         mat(k,665) = .370_r8*rxt(k,392)*y(k,164)
         mat(k,1139) = .120_r8*rxt(k,421)*y(k,164)
         mat(k,939) = .110_r8*rxt(k,498)*y(k,164)
         mat(k,1358) = .330_r8*rxt(k,451)*y(k,164)
         mat(k,1093) = .050_r8*rxt(k,556)*y(k,164)
         mat(k,1463) = .120_r8*rxt(k,465)*y(k,164)
         mat(k,2283) = rxt(k,383)*y(k,268)
         mat(k,2640) = .050_r8*rxt(k,553)*y(k,6) + .370_r8*rxt(k,392)*y(k,28) &
                      + .120_r8*rxt(k,421)*y(k,33) + .110_r8*rxt(k,498)*y(k,127) &
                      + .330_r8*rxt(k,451)*y(k,135) + .050_r8*rxt(k,556)*y(k,140) &
                      + .120_r8*rxt(k,465)*y(k,141)
         mat(k,1839) = rxt(k,381)*y(k,268)
         mat(k,537) = rxt(k,383)*y(k,154) + rxt(k,381)*y(k,267)
         mat(k,2153) = mat(k,2153) + .350_r8*rxt(k,390)*y(k,27)
         mat(k,1595) = rxt(k,326)*y(k,89)
         mat(k,981) = rxt(k,326)*y(k,64) + rxt(k,327)*y(k,93) + rxt(k,329)*y(k,106) &
                      + rxt(k,328)*y(k,294)
         mat(k,1553) = rxt(k,327)*y(k,89)
         mat(k,2509) = rxt(k,329)*y(k,89)
         mat(k,2688) = rxt(k,328)*y(k,89)
         mat(k,1304) = -(rxt(k,287)*y(k,156) + rxt(k,315)*y(k,282) + (rxt(k,606) &
                      + rxt(k,668) + rxt(k,676) + rxt(k,685)) * y(k,109) + (rxt(k,607) &
                      + rxt(k,666) + rxt(k,679) + rxt(k,688)) * y(k,108) + (rxt(k,611) &
                      + rxt(k,695) + rxt(k,699) + rxt(k,703)) * y(k,110))
         mat(k,2421) = -rxt(k,287)*y(k,105)
         mat(k,2170) = -rxt(k,315)*y(k,105)
         mat(k,1744) = -(rxt(k,606) + rxt(k,668) + rxt(k,676) + rxt(k,685)) * y(k,105)
         mat(k,1616) = -(rxt(k,607) + rxt(k,666) + rxt(k,679) + rxt(k,688)) * y(k,105)
         mat(k,1692) = -(rxt(k,611) + rxt(k,695) + rxt(k,699) + rxt(k,703)) * y(k,105)
         mat(k,2463) = rxt(k,293)*y(k,267)
         mat(k,1852) = rxt(k,293)*y(k,115)
         mat(k,2529) = -(rxt(k,221)*y(k,282) + rxt(k,329)*y(k,89))
         mat(k,2204) = -rxt(k,221)*y(k,106)
         mat(k,989) = -rxt(k,329)*y(k,106)
         mat(k,1736) = rxt(k,369)*y(k,156)
         mat(k,1180) = rxt(k,401)*y(k,156)
         mat(k,1378) = rxt(k,427)*y(k,156)
         mat(k,1041) = (rxt(k,673)+rxt(k,682)+rxt(k,691))*y(k,101)
         mat(k,1055) = rxt(k,579)*y(k,156)
         mat(k,1784) = (rxt(k,673)+rxt(k,682)+rxt(k,691))*y(k,75) + rxt(k,620) &
                      *y(k,144)
         mat(k,1315) = rxt(k,287)*y(k,156)
         mat(k,1709) = rxt(k,317)*y(k,156)
         mat(k,447) = rxt(k,620)*y(k,101)
         mat(k,2586) = rxt(k,220)*y(k,282)
         mat(k,2453) = rxt(k,369)*y(k,51) + rxt(k,401)*y(k,54) + rxt(k,427)*y(k,58) &
                      + rxt(k,579)*y(k,83) + rxt(k,287)*y(k,105) + rxt(k,317)*y(k,110)
         mat(k,2204) = mat(k,2204) + rxt(k,220)*y(k,155)
         mat(k,480) = -(rxt(k,199)*y(k,282))
         mat(k,2090) = -rxt(k,199)*y(k,107)
         mat(k,2540) = rxt(k,218)*y(k,267)
         mat(k,1800) = rxt(k,218)*y(k,155)
         mat(k,1619) = -(rxt(k,281)*y(k,163) + (rxt(k,603) + rxt(k,665) + rxt(k,678) &
                      + rxt(k,687)) * y(k,97) + (rxt(k,605) + rxt(k,664) + rxt(k,677) &
                      + rxt(k,686)) * y(k,101) + (rxt(k,607) + rxt(k,666) + rxt(k,679) &
                      + rxt(k,688)) * y(k,105))
         mat(k,2221) = -rxt(k,281)*y(k,108)
         mat(k,1541) = -(rxt(k,603) + rxt(k,665) + rxt(k,678) + rxt(k,687)) * y(k,108)
         mat(k,1770) = -(rxt(k,605) + rxt(k,664) + rxt(k,677) + rxt(k,686)) * y(k,108)
         mat(k,1306) = -(rxt(k,607) + rxt(k,666) + rxt(k,679) + rxt(k,688)) * y(k,108)
         mat(k,564) = rxt(k,262)*y(k,282)
         mat(k,2367) = rxt(k,271)*y(k,267)
         mat(k,1867) = rxt(k,271)*y(k,21)
         mat(k,2186) = rxt(k,262)*y(k,18)
      end do
      end subroutine nlnmat04
      subroutine nlnmat05( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,1749) = -(rxt(k,248)*y(k,70) + rxt(k,249)*y(k,163) + rxt(k,250) &
                      *y(k,282) + (rxt(k,602) + rxt(k,667) + rxt(k,675) + rxt(k,684) &
                      ) * y(k,97) + (rxt(k,604) + rxt(k,663) + rxt(k,674) + rxt(k,683) &
                      ) * y(k,101) + (rxt(k,606) + rxt(k,668) + rxt(k,676) + rxt(k,685) &
                      ) * y(k,105))
         mat(k,1957) = -rxt(k,248)*y(k,109)
         mat(k,2225) = -rxt(k,249)*y(k,109)
         mat(k,2190) = -rxt(k,250)*y(k,109)
         mat(k,1543) = -(rxt(k,602) + rxt(k,667) + rxt(k,675) + rxt(k,684)) * y(k,109)
         mat(k,1772) = -(rxt(k,604) + rxt(k,663) + rxt(k,674) + rxt(k,683)) * y(k,109)
         mat(k,1308) = -(rxt(k,606) + rxt(k,668) + rxt(k,676) + rxt(k,685)) * y(k,109)
         mat(k,1128) = rxt(k,355)*y(k,267)
         mat(k,631) = rxt(k,231)*y(k,282)
         mat(k,2601) = rxt(k,237)*y(k,267)
         mat(k,1035) = rxt(k,242)*y(k,282)
         mat(k,1871) = rxt(k,355)*y(k,68) + rxt(k,237)*y(k,74)
         mat(k,2190) = mat(k,2190) + rxt(k,231)*y(k,73) + rxt(k,242)*y(k,75)
         mat(k,1696) = -(rxt(k,288)*y(k,282) + rxt(k,317)*y(k,156) + (rxt(k,609) &
                      + rxt(k,693) + rxt(k,697) + rxt(k,701)) * y(k,101) + (rxt(k,610) &
                      + rxt(k,694) + rxt(k,698) + rxt(k,702)) * y(k,97) + (rxt(k,611) &
                      + rxt(k,695) + rxt(k,699) + rxt(k,703)) * y(k,105))
         mat(k,2188) = -rxt(k,288)*y(k,110)
         mat(k,2437) = -rxt(k,317)*y(k,110)
         mat(k,1771) = -(rxt(k,609) + rxt(k,693) + rxt(k,697) + rxt(k,701)) * y(k,110)
         mat(k,1542) = -(rxt(k,610) + rxt(k,694) + rxt(k,698) + rxt(k,702)) * y(k,110)
         mat(k,1307) = -(rxt(k,611) + rxt(k,695) + rxt(k,699) + rxt(k,703)) * y(k,110)
         mat(k,1569) = rxt(k,291)*y(k,282)
         mat(k,1900) = rxt(k,308)*y(k,267)
         mat(k,1869) = rxt(k,308)*y(k,125)
         mat(k,2188) = mat(k,2188) + rxt(k,291)*y(k,116)
         mat(k,1228) = -(rxt(k,444)*y(k,282))
         mat(k,2165) = -rxt(k,444)*y(k,111)
         mat(k,725) = .300_r8*rxt(k,489)*y(k,282)
         mat(k,650) = .500_r8*rxt(k,490)*y(k,282)
         mat(k,2293) = rxt(k,443)*y(k,264) + rxt(k,450)*y(k,274)
         mat(k,674) = rxt(k,443)*y(k,154)
         mat(k,1445) = rxt(k,450)*y(k,154)
         mat(k,2165) = mat(k,2165) + .300_r8*rxt(k,489)*y(k,128) + .500_r8*rxt(k,490) &
                      *y(k,129)
         mat(k,307) = -(rxt(k,475)*y(k,282))
         mat(k,2066) = -rxt(k,475)*y(k,112)
         mat(k,1241) = -(rxt(k,429)*y(k,282))
         mat(k,2166) = -rxt(k,429)*y(k,113)
         mat(k,726) = .700_r8*rxt(k,489)*y(k,282)
         mat(k,651) = .500_r8*rxt(k,490)*y(k,282)
         mat(k,716) = .500_r8*rxt(k,464)*y(k,282)
         mat(k,2294) = .050_r8*rxt(k,487)*y(k,270) + .220_r8*rxt(k,449)*y(k,274) &
                      + .250_r8*rxt(k,506)*y(k,290)
         mat(k,2417) = .050_r8*rxt(k,488)*y(k,270) + .220_r8*rxt(k,448)*y(k,274) &
                      + .250_r8*rxt(k,507)*y(k,290)
         mat(k,682) = .500_r8*rxt(k,433)*y(k,282)
         mat(k,1514) = .220_r8*rxt(k,445)*y(k,274) + .250_r8*rxt(k,503)*y(k,290)
         mat(k,1658) = .230_r8*rxt(k,446)*y(k,274) + .200_r8*rxt(k,434)*y(k,285) &
                      + .100_r8*rxt(k,504)*y(k,290)
         mat(k,1420) = .050_r8*rxt(k,487)*y(k,154) + .050_r8*rxt(k,488)*y(k,156)
         mat(k,1446) = .220_r8*rxt(k,449)*y(k,154) + .220_r8*rxt(k,448)*y(k,156) &
                      + .220_r8*rxt(k,445)*y(k,261) + .230_r8*rxt(k,446)*y(k,262)
         mat(k,2166) = mat(k,2166) + .700_r8*rxt(k,489)*y(k,128) + .500_r8*rxt(k,490) &
                      *y(k,129) + .500_r8*rxt(k,464)*y(k,139) + .500_r8*rxt(k,433) &
                      *y(k,179)
         mat(k,1291) = .200_r8*rxt(k,434)*y(k,262)
         mat(k,1323) = .250_r8*rxt(k,506)*y(k,154) + .250_r8*rxt(k,507)*y(k,156) &
                      + .250_r8*rxt(k,503)*y(k,261) + .100_r8*rxt(k,504)*y(k,262)
         mat(k,402) = -(rxt(k,476)*y(k,282))
         mat(k,2080) = -rxt(k,476)*y(k,114)
         mat(k,2249) = .870_r8*rxt(k,487)*y(k,270)
         mat(k,2390) = .950_r8*rxt(k,488)*y(k,270)
         mat(k,1505) = rxt(k,483)*y(k,270)
         mat(k,1637) = .750_r8*rxt(k,484)*y(k,270)
         mat(k,1409) = .870_r8*rxt(k,487)*y(k,154) + .950_r8*rxt(k,488)*y(k,156) &
                      + rxt(k,483)*y(k,261) + .750_r8*rxt(k,484)*y(k,262)
         mat(k,2479) = -(rxt(k,292)*y(k,21) + rxt(k,293)*y(k,267) + rxt(k,294) &
                      *y(k,126) + rxt(k,296)*y(k,155) + rxt(k,298)*y(k,156) + rxt(k,300) &
                      *y(k,154) + rxt(k,301)*y(k,164))
         mat(k,2381) = -rxt(k,292)*y(k,115)
         mat(k,1883) = -rxt(k,293)*y(k,115)
         mat(k,930) = -rxt(k,294)*y(k,115)
         mat(k,2584) = -rxt(k,296)*y(k,115)
         mat(k,2451) = -rxt(k,298)*y(k,115)
         mat(k,2325) = -rxt(k,300)*y(k,115)
         mat(k,2679) = -rxt(k,301)*y(k,115)
         mat(k,2353) = rxt(k,302)*y(k,125)
         mat(k,2381) = mat(k,2381) + rxt(k,303)*y(k,125)
         mat(k,463) = rxt(k,346)*y(k,70) + rxt(k,347)*y(k,282)
         mat(k,1969) = rxt(k,346)*y(k,61)
         mat(k,2613) = (rxt(k,305)+rxt(k,306))*y(k,125)
         mat(k,1054) = rxt(k,578)*y(k,125)
         mat(k,1313) = rxt(k,287)*y(k,156) + rxt(k,315)*y(k,282)
         mat(k,1576) = rxt(k,289)*y(k,156) + rxt(k,290)*y(k,163) + rxt(k,291)*y(k,282)
         mat(k,1913) = rxt(k,302)*y(k,17) + rxt(k,303)*y(k,21) + (rxt(k,305) &
                       +rxt(k,306))*y(k,74) + rxt(k,578)*y(k,83) + 2.000_r8*rxt(k,321) &
                      *y(k,125) + rxt(k,309)*y(k,154) + rxt(k,312)*y(k,163) &
                      + rxt(k,314)*y(k,282)
         mat(k,2325) = mat(k,2325) + rxt(k,309)*y(k,125)
         mat(k,2451) = mat(k,2451) + rxt(k,287)*y(k,105) + rxt(k,289)*y(k,116)
         mat(k,2237) = rxt(k,290)*y(k,116) + rxt(k,312)*y(k,125)
         mat(k,2202) = rxt(k,347)*y(k,61) + rxt(k,315)*y(k,105) + rxt(k,291)*y(k,116) &
                      + rxt(k,314)*y(k,125)
         mat(k,1568) = -(rxt(k,289)*y(k,156) + rxt(k,290)*y(k,163) + rxt(k,291) &
                      *y(k,282))
         mat(k,2432) = -rxt(k,289)*y(k,116)
         mat(k,2218) = -rxt(k,290)*y(k,116)
         mat(k,2183) = -rxt(k,291)*y(k,116)
         mat(k,1305) = (rxt(k,611)+rxt(k,695)+rxt(k,699)+rxt(k,703))*y(k,110)
         mat(k,1694) = (rxt(k,611)+rxt(k,695)+rxt(k,699)+rxt(k,703))*y(k,105)
         mat(k,2464) = rxt(k,294)*y(k,126)
         mat(k,225) = 2.000_r8*rxt(k,299)*y(k,123)
         mat(k,368) = 2.000_r8*rxt(k,295)*y(k,124)
         mat(k,924) = rxt(k,294)*y(k,115)
         mat(k,1890) = 2.000_r8*rxt(k,322)*y(k,125)
         mat(k,1891) = rxt(k,324)*y(k,168)
         mat(k,620) = rxt(k,324)*y(k,125)
         mat(k,619) = 2.000_r8*rxt(k,325)*y(k,168)
         mat(k,1537) = (rxt(k,610)+rxt(k,694)+rxt(k,698)+rxt(k,702))*y(k,110)
         mat(k,1302) = (rxt(k,607)+rxt(k,666)+rxt(k,679)+rxt(k,688))*y(k,108)
         mat(k,1613) = (rxt(k,607)+rxt(k,666)+rxt(k,679)+rxt(k,688))*y(k,105)
         mat(k,1690) = (rxt(k,610)+rxt(k,694)+rxt(k,698)+rxt(k,702))*y(k,97)
         mat(k,2594) = rxt(k,307)*y(k,125)
         mat(k,1766) = (rxt(k,609)+rxt(k,693)+rxt(k,697)+rxt(k,701))*y(k,110)
         mat(k,1303) = (rxt(k,606)+rxt(k,668)+rxt(k,676)+rxt(k,685))*y(k,109)
         mat(k,1742) = (rxt(k,606)+rxt(k,668)+rxt(k,676)+rxt(k,685))*y(k,105)
         mat(k,1691) = (rxt(k,609)+rxt(k,693)+rxt(k,697)+rxt(k,701))*y(k,101)
         mat(k,1893) = rxt(k,307)*y(k,74)
         mat(k,172) = -(rxt(k,477)*y(k,282))
         mat(k,2048) = -rxt(k,477)*y(k,122)
         mat(k,804) = .600_r8*rxt(k,500)*y(k,282)
         mat(k,2048) = mat(k,2048) + .600_r8*rxt(k,500)*y(k,131)
         mat(k,224) = -(4._r8*rxt(k,299)*y(k,123))
         mat(k,2458) = rxt(k,300)*y(k,154)
         mat(k,2244) = rxt(k,300)*y(k,115)
         mat(k,365) = -(4._r8*rxt(k,295)*y(k,124))
         mat(k,2459) = rxt(k,296)*y(k,155)
         mat(k,2536) = rxt(k,296)*y(k,115)
         mat(k,1904) = -(rxt(k,302)*y(k,17) + (rxt(k,303) + rxt(k,304)) * y(k,21) &
                      + (rxt(k,305) + rxt(k,306) + rxt(k,307)) * y(k,74) + rxt(k,308) &
                      *y(k,267) + rxt(k,309)*y(k,154) + rxt(k,310)*y(k,155) + rxt(k,311) &
                      *y(k,156) + rxt(k,312)*y(k,163) + rxt(k,313)*y(k,164) + rxt(k,314) &
                      *y(k,282) + (4._r8*rxt(k,321) + 4._r8*rxt(k,322)) * y(k,125) &
                      + rxt(k,324)*y(k,168) + rxt(k,578)*y(k,83))
         mat(k,2344) = -rxt(k,302)*y(k,125)
         mat(k,2372) = -(rxt(k,303) + rxt(k,304)) * y(k,125)
         mat(k,2604) = -(rxt(k,305) + rxt(k,306) + rxt(k,307)) * y(k,125)
         mat(k,1874) = -rxt(k,308)*y(k,125)
         mat(k,2316) = -rxt(k,309)*y(k,125)
         mat(k,2575) = -rxt(k,310)*y(k,125)
         mat(k,2442) = -rxt(k,311)*y(k,125)
         mat(k,2228) = -rxt(k,312)*y(k,125)
         mat(k,2670) = -rxt(k,313)*y(k,125)
         mat(k,2193) = -rxt(k,314)*y(k,125)
         mat(k,622) = -rxt(k,324)*y(k,125)
         mat(k,1048) = -rxt(k,578)*y(k,125)
         mat(k,2372) = mat(k,2372) + rxt(k,292)*y(k,115)
         mat(k,1699) = rxt(k,317)*y(k,156) + rxt(k,288)*y(k,282)
         mat(k,2470) = rxt(k,292)*y(k,21) + rxt(k,298)*y(k,156) + rxt(k,301)*y(k,164)
         mat(k,1570) = rxt(k,290)*y(k,163)
         mat(k,2316) = mat(k,2316) + rxt(k,316)*y(k,168)
         mat(k,2442) = mat(k,2442) + rxt(k,317)*y(k,110) + rxt(k,298)*y(k,115)
         mat(k,2228) = mat(k,2228) + rxt(k,290)*y(k,116)
         mat(k,2670) = mat(k,2670) + rxt(k,301)*y(k,115)
         mat(k,622) = mat(k,622) + rxt(k,316)*y(k,154)
         mat(k,2193) = mat(k,2193) + rxt(k,288)*y(k,110)
         mat(k,923) = -(rxt(k,294)*y(k,115))
         mat(k,2462) = -rxt(k,294)*y(k,126)
         mat(k,1567) = rxt(k,289)*y(k,156)
         mat(k,1895) = rxt(k,310)*y(k,155)
         mat(k,2554) = rxt(k,310)*y(k,125)
         mat(k,2396) = rxt(k,289)*y(k,116)
         mat(k,938) = -(rxt(k,491)*y(k,156) + rxt(k,498)*y(k,164) + rxt(k,499) &
                      *y(k,282))
         mat(k,2397) = -rxt(k,491)*y(k,127)
         mat(k,2635) = -rxt(k,498)*y(k,127)
         mat(k,2143) = -rxt(k,499)*y(k,127)
         mat(k,723) = -(rxt(k,489)*y(k,282))
         mat(k,2122) = -rxt(k,489)*y(k,128)
         mat(k,2265) = .080_r8*rxt(k,481)*y(k,269)
         mat(k,1380) = .080_r8*rxt(k,481)*y(k,154)
         mat(k,647) = -(rxt(k,490)*y(k,282))
         mat(k,2112) = -rxt(k,490)*y(k,129)
         mat(k,2262) = .080_r8*rxt(k,487)*y(k,270)
         mat(k,1410) = .080_r8*rxt(k,487)*y(k,154)
         mat(k,474) = -(rxt(k,497)*y(k,282))
         mat(k,2089) = -rxt(k,497)*y(k,130)
         mat(k,1799) = rxt(k,494)*y(k,271)
         mat(k,1335) = rxt(k,494)*y(k,267)
         mat(k,805) = -(rxt(k,500)*y(k,282))
         mat(k,2131) = -rxt(k,500)*y(k,131)
         mat(k,1824) = rxt(k,480)*y(k,269) + rxt(k,485)*y(k,270)
         mat(k,1381) = rxt(k,480)*y(k,267)
         mat(k,1412) = rxt(k,485)*y(k,267)
         mat(k,78) = -(rxt(k,652)*y(k,282))
         mat(k,2027) = -rxt(k,652)*y(k,132)
         mat(k,94) = -(rxt(k,653)*y(k,282))
         mat(k,2038) = -rxt(k,653)*y(k,133)
         mat(k,1360) = -(rxt(k,451)*y(k,164) + rxt(k,452)*y(k,282))
         mat(k,2655) = -rxt(k,451)*y(k,135)
         mat(k,2173) = -rxt(k,452)*y(k,135)
         mat(k,943) = .300_r8*rxt(k,498)*y(k,164)
         mat(k,2300) = .360_r8*rxt(k,481)*y(k,269)
         mat(k,2424) = .400_r8*rxt(k,482)*y(k,269)
         mat(k,2655) = mat(k,2655) + .300_r8*rxt(k,498)*y(k,127)
         mat(k,1517) = .390_r8*rxt(k,478)*y(k,269)
         mat(k,1664) = .310_r8*rxt(k,479)*y(k,269)
         mat(k,1390) = .360_r8*rxt(k,481)*y(k,154) + .400_r8*rxt(k,482)*y(k,156) &
                      + .390_r8*rxt(k,478)*y(k,261) + .310_r8*rxt(k,479)*y(k,262)
      end do
      end subroutine nlnmat05
      subroutine nlnmat06( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,415) = -(rxt(k,453)*y(k,282))
         mat(k,2083) = -rxt(k,453)*y(k,136)
         mat(k,1797) = rxt(k,447)*y(k,274)
         mat(k,1441) = rxt(k,447)*y(k,267)
         mat(k,635) = -(rxt(k,462)*y(k,282))
         mat(k,2110) = -rxt(k,462)*y(k,137)
         mat(k,2260) = .800_r8*rxt(k,471)*y(k,253)
         mat(k,1018) = .800_r8*rxt(k,471)*y(k,154)
         mat(k,392) = -(rxt(k,463)*y(k,282))
         mat(k,2078) = -rxt(k,463)*y(k,138)
         mat(k,1794) = .800_r8*rxt(k,460)*y(k,278)
         mat(k,791) = .800_r8*rxt(k,460)*y(k,267)
         mat(k,714) = -(rxt(k,464)*y(k,282))
         mat(k,2121) = -rxt(k,464)*y(k,139)
         mat(k,2549) = rxt(k,467)*y(k,276)
         mat(k,1486) = rxt(k,467)*y(k,155)
         mat(k,1095) = -(rxt(k,555)*y(k,156) + rxt(k,556)*y(k,164) + rxt(k,557) &
                      *y(k,282))
         mat(k,2406) = -rxt(k,555)*y(k,140)
         mat(k,2642) = -rxt(k,556)*y(k,140)
         mat(k,2155) = -rxt(k,557)*y(k,140)
         mat(k,1470) = -(rxt(k,465)*y(k,164) + rxt(k,466)*y(k,282))
         mat(k,2660) = -rxt(k,465)*y(k,141)
         mat(k,2178) = -rxt(k,466)*y(k,141)
         mat(k,946) = .200_r8*rxt(k,498)*y(k,164)
         mat(k,2305) = .560_r8*rxt(k,481)*y(k,269)
         mat(k,2429) = .600_r8*rxt(k,482)*y(k,269)
         mat(k,2660) = mat(k,2660) + .200_r8*rxt(k,498)*y(k,127)
         mat(k,1522) = .610_r8*rxt(k,478)*y(k,269)
         mat(k,1669) = .440_r8*rxt(k,479)*y(k,269)
         mat(k,1394) = .560_r8*rxt(k,481)*y(k,154) + .600_r8*rxt(k,482)*y(k,156) &
                      + .610_r8*rxt(k,478)*y(k,261) + .440_r8*rxt(k,479)*y(k,262)
         mat(k,568) = -(rxt(k,200)*y(k,154) + (rxt(k,201) + rxt(k,202) + rxt(k,203) &
                      ) * y(k,155) + rxt(k,212)*y(k,282))
         mat(k,2255) = -rxt(k,200)*y(k,142)
         mat(k,2542) = -(rxt(k,201) + rxt(k,202) + rxt(k,203)) * y(k,142)
         mat(k,2102) = -rxt(k,212)*y(k,142)
         mat(k,228) = -((rxt(k,216) + rxt(k,217)) * y(k,281))
         mat(k,1985) = -(rxt(k,216) + rxt(k,217)) * y(k,143)
         mat(k,567) = rxt(k,201)*y(k,155)
         mat(k,2535) = rxt(k,201)*y(k,142)
         mat(k,2538) = rxt(k,219)*y(k,156)
         mat(k,2392) = rxt(k,219)*y(k,155)
         mat(k,510) = -(rxt(k,501)*y(k,282))
         mat(k,2095) = -rxt(k,501)*y(k,145)
         mat(k,1640) = .200_r8*rxt(k,493)*y(k,271)
         mat(k,1336) = .200_r8*rxt(k,493)*y(k,262)
         mat(k,1161) = -(rxt(k,502)*y(k,282))
         mat(k,2159) = -rxt(k,502)*y(k,146)
         mat(k,2287) = rxt(k,495)*y(k,271)
         mat(k,2410) = rxt(k,496)*y(k,271)
         mat(k,1511) = rxt(k,492)*y(k,271)
         mat(k,1652) = .800_r8*rxt(k,493)*y(k,271)
         mat(k,1340) = rxt(k,495)*y(k,154) + rxt(k,496)*y(k,156) + rxt(k,492)*y(k,261) &
                      + .800_r8*rxt(k,493)*y(k,262)
         mat(k,117) = -(rxt(k,613)*y(k,282))
         mat(k,2041) = -rxt(k,613)*y(k,150)
         mat(k,2321) = -(rxt(k,200)*y(k,142) + rxt(k,209)*y(k,156) + rxt(k,213) &
                      *y(k,267) + rxt(k,214)*y(k,164) + rxt(k,215)*y(k,163) + rxt(k,238) &
                      *y(k,74) + rxt(k,272)*y(k,21) + rxt(k,300)*y(k,115) + rxt(k,309) &
                      *y(k,125) + rxt(k,316)*y(k,168) + rxt(k,356)*y(k,68) + rxt(k,375) &
                      *y(k,262) + rxt(k,383)*y(k,268) + rxt(k,396)*y(k,258) + rxt(k,407) &
                      *y(k,261) + rxt(k,411)*y(k,266) + rxt(k,424)*y(k,259) + rxt(k,432) &
                      *y(k,284) + rxt(k,436)*y(k,285) + (rxt(k,442) + rxt(k,443) &
                      ) * y(k,264) + (rxt(k,449) + rxt(k,450)) * y(k,274) + rxt(k,458) &
                      *y(k,276) + rxt(k,461)*y(k,278) + (rxt(k,471) + rxt(k,472) &
                      ) * y(k,253) + rxt(k,481)*y(k,269) + rxt(k,487)*y(k,270) &
                      + rxt(k,495)*y(k,271) + rxt(k,506)*y(k,290) + rxt(k,510) &
                      *y(k,252) + rxt(k,513)*y(k,255) + rxt(k,518)*y(k,257) + rxt(k,520) &
                      *y(k,260) + rxt(k,524)*y(k,263) + rxt(k,527)*y(k,275) + rxt(k,530) &
                      *y(k,277) + rxt(k,533)*y(k,283) + rxt(k,540)*y(k,288) + rxt(k,546) &
                      *y(k,291) + rxt(k,549)*y(k,293) + rxt(k,560)*y(k,280) + rxt(k,565) &
                      *y(k,286) + rxt(k,570)*y(k,287))
         mat(k,572) = -rxt(k,200)*y(k,154)
         mat(k,2447) = -rxt(k,209)*y(k,154)
         mat(k,1879) = -rxt(k,213)*y(k,154)
         mat(k,2675) = -rxt(k,214)*y(k,154)
         mat(k,2233) = -rxt(k,215)*y(k,154)
         mat(k,2609) = -rxt(k,238)*y(k,154)
         mat(k,2377) = -rxt(k,272)*y(k,154)
         mat(k,2475) = -rxt(k,300)*y(k,154)
         mat(k,1909) = -rxt(k,309)*y(k,154)
         mat(k,623) = -rxt(k,316)*y(k,154)
         mat(k,1131) = -rxt(k,356)*y(k,154)
         mat(k,1682) = -rxt(k,375)*y(k,154)
         mat(k,540) = -rxt(k,383)*y(k,154)
         mat(k,919) = -rxt(k,396)*y(k,154)
         mat(k,1531) = -rxt(k,407)*y(k,154)
         mat(k,834) = -rxt(k,411)*y(k,154)
         mat(k,900) = -rxt(k,424)*y(k,154)
         mat(k,889) = -rxt(k,432)*y(k,154)
         mat(k,1299) = -rxt(k,436)*y(k,154)
         mat(k,677) = -(rxt(k,442) + rxt(k,443)) * y(k,154)
         mat(k,1457) = -(rxt(k,449) + rxt(k,450)) * y(k,154)
         mat(k,1499) = -rxt(k,458)*y(k,154)
         mat(k,797) = -rxt(k,461)*y(k,154)
         mat(k,1031) = -(rxt(k,471) + rxt(k,472)) * y(k,154)
         mat(k,1402) = -rxt(k,481)*y(k,154)
         mat(k,1435) = -rxt(k,487)*y(k,154)
         mat(k,1354) = -rxt(k,495)*y(k,154)
         mat(k,1332) = -rxt(k,506)*y(k,154)
         mat(k,645) = -rxt(k,510)*y(k,154)
         mat(k,603) = -rxt(k,513)*y(k,154)
         mat(k,534) = -rxt(k,518)*y(k,154)
         mat(k,743) = -rxt(k,520)*y(k,154)
         mat(k,880) = -rxt(k,524)*y(k,154)
         mat(k,840) = -rxt(k,527)*y(k,154)
         mat(k,979) = -rxt(k,530)*y(k,154)
         mat(k,553) = -rxt(k,533)*y(k,154)
         mat(k,855) = -rxt(k,540)*y(k,154)
         mat(k,872) = -rxt(k,546)*y(k,154)
         mat(k,611) = -rxt(k,549)*y(k,154)
         mat(k,1202) = -rxt(k,560)*y(k,154)
         mat(k,1263) = -rxt(k,565)*y(k,154)
         mat(k,1284) = -rxt(k,570)*y(k,154)
         mat(k,226) = 4.000_r8*rxt(k,299)*y(k,123)
         mat(k,572) = mat(k,572) + 2.000_r8*rxt(k,202)*y(k,155) + rxt(k,212)*y(k,282)
         mat(k,230) = 2.000_r8*rxt(k,216)*y(k,281)
         mat(k,2580) = 2.000_r8*rxt(k,202)*y(k,142) + rxt(k,205)*y(k,163) + rxt(k,589) &
                      *y(k,183)
         mat(k,2233) = mat(k,2233) + rxt(k,205)*y(k,155)
         mat(k,1587) = rxt(k,589)*y(k,155)
         mat(k,2011) = 2.000_r8*rxt(k,216)*y(k,143)
         mat(k,2198) = rxt(k,212)*y(k,142)
         mat(k,2587) = -((rxt(k,201) + rxt(k,202) + rxt(k,203)) * y(k,142) + (rxt(k,205) &
                      + rxt(k,207)) * y(k,163) + rxt(k,206)*y(k,164) + rxt(k,218) &
                      *y(k,267) + rxt(k,219)*y(k,156) + rxt(k,220)*y(k,282) + rxt(k,230) &
                      *y(k,70) + rxt(k,240)*y(k,74) + rxt(k,265)*y(k,17) + rxt(k,275) &
                      *y(k,21) + rxt(k,296)*y(k,115) + rxt(k,310)*y(k,125) + rxt(k,418) &
                      *y(k,261) + rxt(k,467)*y(k,276) + rxt(k,525)*y(k,263) + rxt(k,528) &
                      *y(k,275) + rxt(k,531)*y(k,277) + rxt(k,535)*y(k,172) + rxt(k,538) &
                      *y(k,252) + rxt(k,589)*y(k,183))
         mat(k,574) = -(rxt(k,201) + rxt(k,202) + rxt(k,203)) * y(k,155)
         mat(k,2240) = -(rxt(k,205) + rxt(k,207)) * y(k,155)
         mat(k,2682) = -rxt(k,206)*y(k,155)
         mat(k,1886) = -rxt(k,218)*y(k,155)
         mat(k,2454) = -rxt(k,219)*y(k,155)
         mat(k,2205) = -rxt(k,220)*y(k,155)
         mat(k,1972) = -rxt(k,230)*y(k,155)
         mat(k,2616) = -rxt(k,240)*y(k,155)
         mat(k,2356) = -rxt(k,265)*y(k,155)
         mat(k,2384) = -rxt(k,275)*y(k,155)
         mat(k,2482) = -rxt(k,296)*y(k,155)
         mat(k,1916) = -rxt(k,310)*y(k,155)
         mat(k,1534) = -rxt(k,418)*y(k,155)
         mat(k,1502) = -rxt(k,467)*y(k,155)
         mat(k,881) = -rxt(k,525)*y(k,155)
         mat(k,841) = -rxt(k,528)*y(k,155)
         mat(k,980) = -rxt(k,531)*y(k,155)
         mat(k,587) = -rxt(k,535)*y(k,155)
         mat(k,646) = -rxt(k,538)*y(k,155)
         mat(k,1591) = -rxt(k,589)*y(k,155)
         mat(k,790) = rxt(k,469)*y(k,282)
         mat(k,440) = rxt(k,440)*y(k,156)
         mat(k,2384) = mat(k,2384) + rxt(k,272)*y(k,154)
         mat(k,1133) = rxt(k,356)*y(k,154) + rxt(k,357)*y(k,156)
         mat(k,634) = rxt(k,231)*y(k,282)
         mat(k,2616) = mat(k,2616) + rxt(k,238)*y(k,154)
         mat(k,484) = rxt(k,199)*y(k,282)
         mat(k,2482) = mat(k,2482) + rxt(k,298)*y(k,156)
         mat(k,370) = 4.000_r8*rxt(k,295)*y(k,124)
         mat(k,1916) = mat(k,1916) + rxt(k,309)*y(k,154) + rxt(k,311)*y(k,156)
         mat(k,731) = .700_r8*rxt(k,489)*y(k,282)
         mat(k,2328) = rxt(k,272)*y(k,21) + rxt(k,356)*y(k,68) + rxt(k,238)*y(k,74) &
                      + rxt(k,309)*y(k,125) + 2.000_r8*rxt(k,209)*y(k,156) &
                      + rxt(k,215)*y(k,163) + rxt(k,214)*y(k,164) + rxt(k,316) &
                      *y(k,168) + rxt(k,510)*y(k,252) + rxt(k,471)*y(k,253) &
                      + rxt(k,513)*y(k,255) + rxt(k,518)*y(k,257) + rxt(k,396) &
                      *y(k,258) + rxt(k,424)*y(k,259) + rxt(k,520)*y(k,260) &
                      + rxt(k,407)*y(k,261) + rxt(k,375)*y(k,262) + rxt(k,524) &
                      *y(k,263) + rxt(k,442)*y(k,264) + rxt(k,411)*y(k,266) &
                      + rxt(k,213)*y(k,267) + rxt(k,383)*y(k,268) + .920_r8*rxt(k,481) &
                      *y(k,269) + .920_r8*rxt(k,487)*y(k,270) + rxt(k,495)*y(k,271) &
                      + rxt(k,449)*y(k,274) + rxt(k,527)*y(k,275) + rxt(k,458) &
                      *y(k,276) + rxt(k,530)*y(k,277) + rxt(k,461)*y(k,278) &
                      + 1.600_r8*rxt(k,560)*y(k,280) + rxt(k,533)*y(k,283) &
                      + rxt(k,432)*y(k,284) + rxt(k,436)*y(k,285) + .900_r8*rxt(k,565) &
                      *y(k,286) + .800_r8*rxt(k,570)*y(k,287) + rxt(k,540)*y(k,288) &
                      + rxt(k,506)*y(k,290) + rxt(k,546)*y(k,291) + rxt(k,549) &
                      *y(k,293)
         mat(k,2454) = mat(k,2454) + rxt(k,440)*y(k,16) + rxt(k,357)*y(k,68) &
                      + rxt(k,298)*y(k,115) + rxt(k,311)*y(k,125) &
                      + 2.000_r8*rxt(k,209)*y(k,154) + rxt(k,210)*y(k,163) &
                      + rxt(k,208)*y(k,267) + rxt(k,482)*y(k,269) + rxt(k,488) &
                      *y(k,270) + rxt(k,496)*y(k,271) + rxt(k,448)*y(k,274) &
                      + rxt(k,459)*y(k,276) + 2.000_r8*rxt(k,561)*y(k,280) &
                      + rxt(k,211)*y(k,282) + rxt(k,507)*y(k,290)
         mat(k,971) = rxt(k,430)*y(k,282)
         mat(k,2240) = mat(k,2240) + rxt(k,215)*y(k,154) + rxt(k,210)*y(k,156)
         mat(k,2682) = mat(k,2682) + rxt(k,214)*y(k,154)
         mat(k,625) = rxt(k,316)*y(k,154)
         mat(k,737) = rxt(k,567)*y(k,282)
         mat(k,646) = mat(k,646) + rxt(k,510)*y(k,154)
         mat(k,1032) = rxt(k,471)*y(k,154)
         mat(k,604) = rxt(k,513)*y(k,154)
         mat(k,535) = rxt(k,518)*y(k,154)
         mat(k,920) = rxt(k,396)*y(k,154)
         mat(k,901) = rxt(k,424)*y(k,154)
         mat(k,744) = rxt(k,520)*y(k,154)
         mat(k,1534) = mat(k,1534) + rxt(k,407)*y(k,154)
         mat(k,1686) = rxt(k,375)*y(k,154) + .500_r8*rxt(k,558)*y(k,280)
         mat(k,881) = mat(k,881) + rxt(k,524)*y(k,154)
         mat(k,678) = rxt(k,442)*y(k,154)
         mat(k,835) = rxt(k,411)*y(k,154)
         mat(k,1886) = mat(k,1886) + rxt(k,213)*y(k,154) + rxt(k,208)*y(k,156)
         mat(k,541) = rxt(k,383)*y(k,154)
         mat(k,1405) = .920_r8*rxt(k,481)*y(k,154) + rxt(k,482)*y(k,156)
         mat(k,1438) = .920_r8*rxt(k,487)*y(k,154) + rxt(k,488)*y(k,156)
         mat(k,1357) = rxt(k,495)*y(k,154) + rxt(k,496)*y(k,156)
         mat(k,1460) = rxt(k,449)*y(k,154) + rxt(k,448)*y(k,156)
         mat(k,841) = mat(k,841) + rxt(k,527)*y(k,154)
         mat(k,1502) = mat(k,1502) + rxt(k,458)*y(k,154) + rxt(k,459)*y(k,156)
         mat(k,980) = mat(k,980) + rxt(k,530)*y(k,154)
         mat(k,798) = rxt(k,461)*y(k,154)
         mat(k,1205) = 1.600_r8*rxt(k,560)*y(k,154) + 2.000_r8*rxt(k,561)*y(k,156) &
                      + .500_r8*rxt(k,558)*y(k,262)
         mat(k,2205) = mat(k,2205) + rxt(k,469)*y(k,1) + rxt(k,231)*y(k,73) &
                      + rxt(k,199)*y(k,107) + .700_r8*rxt(k,489)*y(k,128) + rxt(k,211) &
                      *y(k,156) + rxt(k,430)*y(k,157) + rxt(k,567)*y(k,239)
         mat(k,554) = rxt(k,533)*y(k,154)
         mat(k,890) = rxt(k,432)*y(k,154)
         mat(k,1300) = rxt(k,436)*y(k,154)
         mat(k,1265) = .900_r8*rxt(k,565)*y(k,154)
         mat(k,1287) = .800_r8*rxt(k,570)*y(k,154)
         mat(k,856) = rxt(k,540)*y(k,154)
         mat(k,1334) = rxt(k,506)*y(k,154) + rxt(k,507)*y(k,156)
         mat(k,873) = rxt(k,546)*y(k,154)
         mat(k,612) = rxt(k,549)*y(k,154)
      end do
      end subroutine nlnmat06
      subroutine nlnmat07( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,2450) = -(rxt(k,208)*y(k,267) + rxt(k,209)*y(k,154) + rxt(k,210) &
                      *y(k,163) + rxt(k,211)*y(k,282) + rxt(k,219)*y(k,155) + rxt(k,287) &
                      *y(k,105) + rxt(k,289)*y(k,116) + rxt(k,298)*y(k,115) + rxt(k,311) &
                      *y(k,125) + rxt(k,317)*y(k,110) + rxt(k,357)*y(k,68) + rxt(k,369) &
                      *y(k,51) + rxt(k,401)*y(k,54) + rxt(k,420)*y(k,33) + rxt(k,427) &
                      *y(k,58) + rxt(k,440)*y(k,16) + rxt(k,448)*y(k,274) + rxt(k,459) &
                      *y(k,276) + rxt(k,482)*y(k,269) + rxt(k,488)*y(k,270) + rxt(k,491) &
                      *y(k,127) + rxt(k,496)*y(k,271) + rxt(k,507)*y(k,290) + rxt(k,552) &
                      *y(k,6) + rxt(k,555)*y(k,140) + rxt(k,561)*y(k,280) + rxt(k,572) &
                      *y(k,241) + rxt(k,579)*y(k,83))
         mat(k,1882) = -rxt(k,208)*y(k,156)
         mat(k,2324) = -rxt(k,209)*y(k,156)
         mat(k,2236) = -rxt(k,210)*y(k,156)
         mat(k,2201) = -rxt(k,211)*y(k,156)
         mat(k,2583) = -rxt(k,219)*y(k,156)
         mat(k,1312) = -rxt(k,287)*y(k,156)
         mat(k,1575) = -rxt(k,289)*y(k,156)
         mat(k,2478) = -rxt(k,298)*y(k,156)
         mat(k,1912) = -rxt(k,311)*y(k,156)
         mat(k,1706) = -rxt(k,317)*y(k,156)
         mat(k,1132) = -rxt(k,357)*y(k,156)
         mat(k,1733) = -rxt(k,369)*y(k,156)
         mat(k,1179) = -rxt(k,401)*y(k,156)
         mat(k,1153) = -rxt(k,420)*y(k,156)
         mat(k,1377) = -rxt(k,427)*y(k,156)
         mat(k,439) = -rxt(k,440)*y(k,156)
         mat(k,1458) = -rxt(k,448)*y(k,156)
         mat(k,1500) = -rxt(k,459)*y(k,156)
         mat(k,1403) = -rxt(k,482)*y(k,156)
         mat(k,1436) = -rxt(k,488)*y(k,156)
         mat(k,952) = -rxt(k,491)*y(k,156)
         mat(k,1355) = -rxt(k,496)*y(k,156)
         mat(k,1333) = -rxt(k,507)*y(k,156)
         mat(k,1016) = -rxt(k,552)*y(k,156)
         mat(k,1111) = -rxt(k,555)*y(k,156)
         mat(k,1203) = -rxt(k,561)*y(k,156)
         mat(k,1122) = -rxt(k,572)*y(k,156)
         mat(k,1053) = -rxt(k,579)*y(k,156)
         mat(k,2352) = rxt(k,273)*y(k,22)
         mat(k,963) = rxt(k,273)*y(k,17) + rxt(k,274)*y(k,70) + rxt(k,276)*y(k,163)
         mat(k,1968) = rxt(k,274)*y(k,22) + rxt(k,239)*y(k,75)
         mat(k,1040) = rxt(k,239)*y(k,70) + rxt(k,241)*y(k,163) + rxt(k,242)*y(k,282)
         mat(k,987) = rxt(k,329)*y(k,106)
         mat(k,2526) = rxt(k,329)*y(k,89) + rxt(k,221)*y(k,282)
         mat(k,2478) = mat(k,2478) + rxt(k,294)*y(k,126)
         mat(k,929) = rxt(k,294)*y(k,115)
         mat(k,721) = .500_r8*rxt(k,464)*y(k,282)
         mat(k,2583) = mat(k,2583) + rxt(k,207)*y(k,163) + rxt(k,206)*y(k,164)
         mat(k,2236) = mat(k,2236) + rxt(k,276)*y(k,22) + rxt(k,241)*y(k,75) &
                      + rxt(k,207)*y(k,155)
         mat(k,2678) = rxt(k,206)*y(k,155)
         mat(k,661) = rxt(k,416)*y(k,282)
         mat(k,2201) = mat(k,2201) + rxt(k,242)*y(k,75) + rxt(k,221)*y(k,106) &
                      + .500_r8*rxt(k,464)*y(k,139) + rxt(k,416)*y(k,170)
         mat(k,966) = -(rxt(k,430)*y(k,282))
         mat(k,2144) = -rxt(k,430)*y(k,157)
         mat(k,1138) = rxt(k,420)*y(k,156)
         mat(k,648) = .500_r8*rxt(k,490)*y(k,282)
         mat(k,476) = rxt(k,497)*y(k,282)
         mat(k,511) = rxt(k,501)*y(k,282)
         mat(k,1158) = rxt(k,502)*y(k,282)
         mat(k,2398) = rxt(k,420)*y(k,33)
         mat(k,2144) = mat(k,2144) + .500_r8*rxt(k,490)*y(k,129) + rxt(k,497)*y(k,130) &
                      + rxt(k,501)*y(k,145) + rxt(k,502)*y(k,146)
         mat(k,468) = -(rxt(k,562)*y(k,282))
         mat(k,2088) = -rxt(k,562)*y(k,158)
         mat(k,1798) = rxt(k,559)*y(k,280)
         mat(k,1190) = rxt(k,559)*y(k,267)
         mat(k,2232) = -(rxt(k,179)*y(k,164) + 4._r8*rxt(k,180)*y(k,163) + rxt(k,182) &
                      *y(k,93) + rxt(k,183)*y(k,95) + rxt(k,188)*y(k,267) + rxt(k,194) &
                      *y(k,282) + (rxt(k,205) + rxt(k,207)) * y(k,155) + rxt(k,210) &
                      *y(k,156) + rxt(k,215)*y(k,154) + rxt(k,241)*y(k,75) + rxt(k,243) &
                      *y(k,74) + rxt(k,246)*y(k,101) + rxt(k,249)*y(k,109) + rxt(k,276) &
                      *y(k,22) + rxt(k,277)*y(k,21) + rxt(k,279)*y(k,97) + rxt(k,281) &
                      *y(k,108) + rxt(k,290)*y(k,116) + rxt(k,312)*y(k,125) + rxt(k,370) &
                      *y(k,51) + rxt(k,581)*y(k,167))
         mat(k,2674) = -rxt(k,179)*y(k,163)
         mat(k,1562) = -rxt(k,182)*y(k,163)
         mat(k,712) = -rxt(k,183)*y(k,163)
         mat(k,1878) = -rxt(k,188)*y(k,163)
         mat(k,2197) = -rxt(k,194)*y(k,163)
         mat(k,2579) = -(rxt(k,205) + rxt(k,207)) * y(k,163)
         mat(k,2446) = -rxt(k,210)*y(k,163)
         mat(k,2320) = -rxt(k,215)*y(k,163)
         mat(k,1039) = -rxt(k,241)*y(k,163)
         mat(k,2608) = -rxt(k,243)*y(k,163)
         mat(k,1778) = -rxt(k,246)*y(k,163)
         mat(k,1755) = -rxt(k,249)*y(k,163)
         mat(k,960) = -rxt(k,276)*y(k,163)
         mat(k,2376) = -rxt(k,277)*y(k,163)
         mat(k,1547) = -rxt(k,279)*y(k,163)
         mat(k,1627) = -rxt(k,281)*y(k,163)
         mat(k,1573) = -rxt(k,290)*y(k,163)
         mat(k,1908) = -rxt(k,312)*y(k,163)
         mat(k,1729) = -rxt(k,370)*y(k,163)
         mat(k,431) = -rxt(k,581)*y(k,163)
         mat(k,2497) = rxt(k,186)*y(k,267)
         mat(k,571) = rxt(k,200)*y(k,154) + rxt(k,201)*y(k,155)
         mat(k,2320) = mat(k,2320) + rxt(k,200)*y(k,142)
         mat(k,2579) = mat(k,2579) + rxt(k,201)*y(k,142)
         mat(k,2674) = mat(k,2674) + 2.000_r8*rxt(k,178)*y(k,281)
         mat(k,1878) = mat(k,1878) + rxt(k,186)*y(k,92)
         mat(k,2010) = 2.000_r8*rxt(k,178)*y(k,164)
         mat(k,2197) = mat(k,2197) + 2.000_r8*rxt(k,196)*y(k,282)
         mat(k,2684) = -((rxt(k,177) + rxt(k,178)) * y(k,281) + rxt(k,179)*y(k,163) &
                      + rxt(k,189)*y(k,267) + rxt(k,190)*y(k,92) + rxt(k,195)*y(k,282) &
                      + rxt(k,206)*y(k,155) + rxt(k,214)*y(k,154) + rxt(k,232)*y(k,70) &
                      + rxt(k,266)*y(k,17) + rxt(k,301)*y(k,115) + rxt(k,313)*y(k,125) &
                      + rxt(k,392)*y(k,28) + rxt(k,421)*y(k,33) + rxt(k,451)*y(k,135) &
                      + rxt(k,465)*y(k,141) + rxt(k,498)*y(k,127) + rxt(k,536) &
                      *y(k,172) + rxt(k,553)*y(k,6) + rxt(k,556)*y(k,140) + rxt(k,585) &
                      *y(k,181) + rxt(k,591)*y(k,183))
         mat(k,2020) = -(rxt(k,177) + rxt(k,178)) * y(k,164)
         mat(k,2242) = -rxt(k,179)*y(k,164)
         mat(k,1888) = -rxt(k,189)*y(k,164)
         mat(k,2507) = -rxt(k,190)*y(k,164)
         mat(k,2207) = -rxt(k,195)*y(k,164)
         mat(k,2589) = -rxt(k,206)*y(k,164)
         mat(k,2330) = -rxt(k,214)*y(k,164)
         mat(k,1974) = -rxt(k,232)*y(k,164)
         mat(k,2358) = -rxt(k,266)*y(k,164)
         mat(k,2484) = -rxt(k,301)*y(k,164)
         mat(k,1918) = -rxt(k,313)*y(k,164)
         mat(k,670) = -rxt(k,392)*y(k,164)
         mat(k,1155) = -rxt(k,421)*y(k,164)
         mat(k,1369) = -rxt(k,451)*y(k,164)
         mat(k,1483) = -rxt(k,465)*y(k,164)
         mat(k,953) = -rxt(k,498)*y(k,164)
         mat(k,588) = -rxt(k,536)*y(k,164)
         mat(k,1017) = -rxt(k,553)*y(k,164)
         mat(k,1113) = -rxt(k,556)*y(k,164)
         mat(k,618) = -rxt(k,585)*y(k,164)
         mat(k,1593) = -rxt(k,591)*y(k,164)
         mat(k,1535) = .150_r8*rxt(k,406)*y(k,267)
         mat(k,1888) = mat(k,1888) + .150_r8*rxt(k,406)*y(k,261) + .150_r8*rxt(k,456) &
                      *y(k,276)
         mat(k,1503) = .150_r8*rxt(k,456)*y(k,267)
         mat(k,555) = -(rxt(k,592)*y(k,183))
         mat(k,1579) = -rxt(k,592)*y(k,166)
         mat(k,2360) = rxt(k,268)*y(k,74)
         mat(k,2593) = rxt(k,268)*y(k,21) + 2.000_r8*rxt(k,236)*y(k,74) + rxt(k,305) &
                      *y(k,125)
         mat(k,1892) = rxt(k,305)*y(k,74)
         mat(k,425) = -(rxt(k,581)*y(k,163) + rxt(k,582)*y(k,282))
         mat(k,2209) = -rxt(k,581)*y(k,167)
         mat(k,2084) = -rxt(k,582)*y(k,167)
         mat(k,621) = -(rxt(k,316)*y(k,154) + rxt(k,324)*y(k,125) + 4._r8*rxt(k,325) &
                      *y(k,168))
         mat(k,2259) = -rxt(k,316)*y(k,168)
         mat(k,1894) = -rxt(k,324)*y(k,168)
         mat(k,2361) = rxt(k,304)*y(k,125)
         mat(k,1894) = mat(k,1894) + rxt(k,304)*y(k,21) + 2.000_r8*rxt(k,321)*y(k,125) &
                      + rxt(k,311)*y(k,156) + rxt(k,313)*y(k,164)
         mat(k,2393) = rxt(k,311)*y(k,125)
         mat(k,2629) = rxt(k,313)*y(k,125)
         mat(k,1223) = rxt(k,444)*y(k,282)
         mat(k,2246) = .100_r8*rxt(k,565)*y(k,286)
         mat(k,2062) = rxt(k,444)*y(k,111)
         mat(k,1247) = .100_r8*rxt(k,565)*y(k,154)
         mat(k,655) = -(rxt(k,416)*y(k,282))
         mat(k,2113) = -rxt(k,416)*y(k,170)
         mat(k,2548) = rxt(k,418)*y(k,261)
         mat(k,1506) = rxt(k,418)*y(k,155)
         mat(k,2534) = rxt(k,538)*y(k,252)
         mat(k,640) = rxt(k,538)*y(k,155)
         mat(k,585) = -(rxt(k,535)*y(k,155) + rxt(k,536)*y(k,164))
         mat(k,2544) = -rxt(k,535)*y(k,172)
         mat(k,2627) = -rxt(k,536)*y(k,172)
         mat(k,255) = .070_r8*rxt(k,522)*y(k,282)
         mat(k,2256) = rxt(k,520)*y(k,260)
         mat(k,221) = .060_r8*rxt(k,534)*y(k,282)
         mat(k,280) = .070_r8*rxt(k,550)*y(k,282)
         mat(k,739) = rxt(k,520)*y(k,154)
         mat(k,2104) = .070_r8*rxt(k,522)*y(k,82) + .060_r8*rxt(k,534)*y(k,173) &
                      + .070_r8*rxt(k,550)*y(k,248)
         mat(k,219) = -(rxt(k,534)*y(k,282))
         mat(k,2051) = -rxt(k,534)*y(k,173)
         mat(k,211) = .530_r8*rxt(k,511)*y(k,282)
         mat(k,2051) = mat(k,2051) + .530_r8*rxt(k,511)*y(k,7)
         mat(k,381) = -(rxt(k,537)*y(k,282))
         mat(k,2076) = -rxt(k,537)*y(k,174)
         mat(k,1792) = rxt(k,532)*y(k,283)
         mat(k,548) = rxt(k,532)*y(k,267)
         mat(k,679) = -(rxt(k,433)*y(k,282))
         mat(k,2116) = -rxt(k,433)*y(k,179)
         mat(k,1815) = rxt(k,431)*y(k,284)
         mat(k,882) = rxt(k,431)*y(k,267)
         mat(k,486) = -(rxt(k,437)*y(k,282))
         mat(k,2091) = -rxt(k,437)*y(k,180)
         mat(k,1801) = .850_r8*rxt(k,435)*y(k,285)
         mat(k,1289) = .850_r8*rxt(k,435)*y(k,267)
         mat(k,613) = -(rxt(k,585)*y(k,164) + rxt(k,588)*y(k,282))
         mat(k,2628) = -rxt(k,585)*y(k,181)
         mat(k,2108) = -rxt(k,588)*y(k,181)
         mat(k,1582) = -(rxt(k,586)*y(k,21) + rxt(k,587)*y(k,74) + rxt(k,589)*y(k,155) &
                      + rxt(k,591)*y(k,164) + rxt(k,592)*y(k,166) + rxt(k,593) &
                      *y(k,282))
         mat(k,2366) = -rxt(k,586)*y(k,183)
         mat(k,2598) = -rxt(k,587)*y(k,183)
         mat(k,2566) = -rxt(k,589)*y(k,183)
         mat(k,2663) = -rxt(k,591)*y(k,183)
         mat(k,557) = -rxt(k,592)*y(k,183)
         mat(k,2184) = -rxt(k,593)*y(k,183)
         mat(k,2219) = rxt(k,581)*y(k,167)
         mat(k,2663) = mat(k,2663) + rxt(k,585)*y(k,181)
         mat(k,429) = rxt(k,581)*y(k,163)
         mat(k,614) = rxt(k,585)*y(k,164) + rxt(k,588)*y(k,282)
         mat(k,2184) = mat(k,2184) + rxt(k,588)*y(k,181)
      end do
      end subroutine nlnmat07
      subroutine nlnmat08( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,1057) = -(rxt(k,584)*y(k,282))
         mat(k,2151) = -rxt(k,584)*y(k,184)
         mat(k,2365) = rxt(k,575)*y(k,83) + rxt(k,586)*y(k,183)
         mat(k,1942) = rxt(k,577)*y(k,83)
         mat(k,2597) = rxt(k,587)*y(k,183)
         mat(k,1045) = rxt(k,575)*y(k,21) + rxt(k,577)*y(k,70) + rxt(k,578)*y(k,125) &
                      + rxt(k,579)*y(k,156) + (rxt(k,580)+.500_r8*rxt(k,594))*y(k,282)
         mat(k,1897) = rxt(k,578)*y(k,83)
         mat(k,2558) = rxt(k,589)*y(k,183)
         mat(k,2402) = rxt(k,579)*y(k,83)
         mat(k,2638) = rxt(k,591)*y(k,183)
         mat(k,556) = rxt(k,592)*y(k,183)
         mat(k,427) = rxt(k,582)*y(k,282)
         mat(k,1581) = rxt(k,586)*y(k,21) + rxt(k,587)*y(k,74) + rxt(k,589)*y(k,155) &
                      + rxt(k,591)*y(k,164) + rxt(k,592)*y(k,166) + rxt(k,593) &
                      *y(k,282)
         mat(k,2151) = mat(k,2151) + (rxt(k,580)+.500_r8*rxt(k,594))*y(k,83) &
                      + rxt(k,582)*y(k,167) + rxt(k,593)*y(k,183)
         mat(k,321) = -(rxt(k,595)*y(k,294))
         mat(k,2687) = -rxt(k,595)*y(k,185)
         mat(k,1056) = rxt(k,584)*y(k,282)
         mat(k,2069) = rxt(k,584)*y(k,184)
         mat(k,73) = .2381005_r8*rxt(k,652)*y(k,282)
         mat(k,95) = .5931005_r8*rxt(k,657)*y(k,282)
         mat(k,2022) = .2381005_r8*rxt(k,652)*y(k,132) + .5931005_r8*rxt(k,657) &
                      *y(k,234)
         mat(k,74) = .1308005_r8*rxt(k,652)*y(k,282)
         mat(k,96) = .1534005_r8*rxt(k,657)*y(k,282)
         mat(k,2023) = .1308005_r8*rxt(k,652)*y(k,132) + .1534005_r8*rxt(k,657) &
                      *y(k,234)
         mat(k,75) = .0348005_r8*rxt(k,652)*y(k,282)
         mat(k,97) = .0459005_r8*rxt(k,657)*y(k,282)
         mat(k,2024) = .0348005_r8*rxt(k,652)*y(k,132) + .0459005_r8*rxt(k,657) &
                      *y(k,234)
         mat(k,76) = .0076005_r8*rxt(k,652)*y(k,282)
         mat(k,98) = .0085005_r8*rxt(k,657)*y(k,282)
         mat(k,2025) = .0076005_r8*rxt(k,652)*y(k,132) + .0085005_r8*rxt(k,657) &
                      *y(k,234)
         mat(k,77) = .0113005_r8*rxt(k,652)*y(k,282)
         mat(k,99) = .0128005_r8*rxt(k,657)*y(k,282)
         mat(k,2026) = .0113005_r8*rxt(k,652)*y(k,132) + .0128005_r8*rxt(k,657) &
                      *y(k,234)
         mat(k,991) = .2202005_r8*rxt(k,646)*y(k,164) + .2202005_r8*rxt(k,647) &
                      *y(k,282)
         mat(k,933) = .0031005_r8*rxt(k,651)*y(k,282)
         mat(k,1083) = .0508005_r8*rxt(k,655)*y(k,164) + .0508005_r8*rxt(k,656) &
                      *y(k,282)
         mat(k,2620) = .2202005_r8*rxt(k,646)*y(k,6) + .0508005_r8*rxt(k,655)*y(k,140)
         mat(k,2028) = .2202005_r8*rxt(k,647)*y(k,6) + .0031005_r8*rxt(k,651)*y(k,127) &
                      + .0508005_r8*rxt(k,656)*y(k,140)
         mat(k,992) = .2067005_r8*rxt(k,646)*y(k,164) + .2067005_r8*rxt(k,647) &
                      *y(k,282)
         mat(k,934) = .0035005_r8*rxt(k,651)*y(k,282)
         mat(k,1084) = .1149005_r8*rxt(k,655)*y(k,164) + .1149005_r8*rxt(k,656) &
                      *y(k,282)
         mat(k,2621) = .2067005_r8*rxt(k,646)*y(k,6) + .1149005_r8*rxt(k,655)*y(k,140)
         mat(k,2029) = .2067005_r8*rxt(k,647)*y(k,6) + .0035005_r8*rxt(k,651)*y(k,127) &
                      + .1149005_r8*rxt(k,656)*y(k,140)
         mat(k,993) = .0653005_r8*rxt(k,646)*y(k,164) + .0653005_r8*rxt(k,647) &
                      *y(k,282)
         mat(k,935) = .0003005_r8*rxt(k,651)*y(k,282)
         mat(k,1085) = .0348005_r8*rxt(k,655)*y(k,164) + .0348005_r8*rxt(k,656) &
                      *y(k,282)
         mat(k,2622) = .0653005_r8*rxt(k,646)*y(k,6) + .0348005_r8*rxt(k,655)*y(k,140)
         mat(k,2030) = .0653005_r8*rxt(k,647)*y(k,6) + .0003005_r8*rxt(k,651)*y(k,127) &
                      + .0348005_r8*rxt(k,656)*y(k,140)
         mat(k,994) = .1749305_r8*rxt(k,645)*y(k,156) + .1284005_r8*rxt(k,646) &
                      *y(k,164) + .1284005_r8*rxt(k,647)*y(k,282)
         mat(k,936) = .0590245_r8*rxt(k,649)*y(k,156) + .0033005_r8*rxt(k,650) &
                      *y(k,164) + .0271005_r8*rxt(k,651)*y(k,282)
         mat(k,1086) = .1749305_r8*rxt(k,654)*y(k,156) + .0554005_r8*rxt(k,655) &
                      *y(k,164) + .0554005_r8*rxt(k,656)*y(k,282)
         mat(k,2388) = .1749305_r8*rxt(k,645)*y(k,6) + .0590245_r8*rxt(k,649)*y(k,127) &
                      + .1749305_r8*rxt(k,654)*y(k,140)
         mat(k,2623) = .1284005_r8*rxt(k,646)*y(k,6) + .0033005_r8*rxt(k,650)*y(k,127) &
                      + .0554005_r8*rxt(k,655)*y(k,140)
         mat(k,2031) = .1284005_r8*rxt(k,647)*y(k,6) + .0271005_r8*rxt(k,651)*y(k,127) &
                      + .0554005_r8*rxt(k,656)*y(k,140)
         mat(k,995) = .5901905_r8*rxt(k,645)*y(k,156) + .114_r8*rxt(k,646)*y(k,164) &
                      + .114_r8*rxt(k,647)*y(k,282)
         mat(k,937) = .0250245_r8*rxt(k,649)*y(k,156) + .0474005_r8*rxt(k,651) &
                      *y(k,282)
         mat(k,1087) = .5901905_r8*rxt(k,654)*y(k,156) + .1278005_r8*rxt(k,655) &
                      *y(k,164) + .1278005_r8*rxt(k,656)*y(k,282)
         mat(k,2389) = .5901905_r8*rxt(k,645)*y(k,6) + .0250245_r8*rxt(k,649)*y(k,127) &
                      + .5901905_r8*rxt(k,654)*y(k,140)
         mat(k,2624) = .114_r8*rxt(k,646)*y(k,6) + .1278005_r8*rxt(k,655)*y(k,140)
         mat(k,2032) = .114_r8*rxt(k,647)*y(k,6) + .0474005_r8*rxt(k,651)*y(k,127) &
                      + .1278005_r8*rxt(k,656)*y(k,140)
         mat(k,204) = .0023005_r8*rxt(k,648)*y(k,282)
         mat(k,89) = .2381005_r8*rxt(k,653)*y(k,282)
         mat(k,101) = .5931005_r8*rxt(k,658)*y(k,282)
         mat(k,241) = .1364005_r8*rxt(k,660)*y(k,282)
         mat(k,265) = .1677005_r8*rxt(k,661)*y(k,282)
         mat(k,2033) = .0023005_r8*rxt(k,648)*y(k,7) + .2381005_r8*rxt(k,653)*y(k,133) &
                      + .5931005_r8*rxt(k,658)*y(k,235) + .1364005_r8*rxt(k,660) &
                      *y(k,244) + .1677005_r8*rxt(k,661)*y(k,246)
         mat(k,205) = .0008005_r8*rxt(k,648)*y(k,282)
         mat(k,90) = .1308005_r8*rxt(k,653)*y(k,282)
         mat(k,102) = .1534005_r8*rxt(k,658)*y(k,282)
         mat(k,242) = .0101005_r8*rxt(k,660)*y(k,282)
         mat(k,266) = .0174005_r8*rxt(k,661)*y(k,282)
         mat(k,2034) = .0008005_r8*rxt(k,648)*y(k,7) + .1308005_r8*rxt(k,653)*y(k,133) &
                      + .1534005_r8*rxt(k,658)*y(k,235) + .0101005_r8*rxt(k,660) &
                      *y(k,244) + .0174005_r8*rxt(k,661)*y(k,246)
         mat(k,206) = .0843005_r8*rxt(k,648)*y(k,282)
         mat(k,91) = .0348005_r8*rxt(k,653)*y(k,282)
         mat(k,103) = .0459005_r8*rxt(k,658)*y(k,282)
         mat(k,243) = .0763005_r8*rxt(k,660)*y(k,282)
         mat(k,267) = .086_r8*rxt(k,661)*y(k,282)
         mat(k,2035) = .0843005_r8*rxt(k,648)*y(k,7) + .0348005_r8*rxt(k,653)*y(k,133) &
                      + .0459005_r8*rxt(k,658)*y(k,235) + .0763005_r8*rxt(k,660) &
                      *y(k,244) + .086_r8*rxt(k,661)*y(k,246)
         mat(k,207) = .0443005_r8*rxt(k,648)*y(k,282)
         mat(k,92) = .0076005_r8*rxt(k,653)*y(k,282)
         mat(k,104) = .0085005_r8*rxt(k,658)*y(k,282)
         mat(k,244) = .2157005_r8*rxt(k,660)*y(k,282)
         mat(k,268) = .0512005_r8*rxt(k,661)*y(k,282)
         mat(k,2036) = .0443005_r8*rxt(k,648)*y(k,7) + .0076005_r8*rxt(k,653)*y(k,133) &
                      + .0085005_r8*rxt(k,658)*y(k,235) + .2157005_r8*rxt(k,660) &
                      *y(k,244) + .0512005_r8*rxt(k,661)*y(k,246)
         mat(k,208) = .1621005_r8*rxt(k,648)*y(k,282)
         mat(k,93) = .0113005_r8*rxt(k,653)*y(k,282)
         mat(k,105) = .0128005_r8*rxt(k,658)*y(k,282)
         mat(k,245) = .0738005_r8*rxt(k,660)*y(k,282)
         mat(k,269) = .1598005_r8*rxt(k,661)*y(k,282)
         mat(k,2037) = .1621005_r8*rxt(k,648)*y(k,7) + .0113005_r8*rxt(k,653)*y(k,133) &
                      + .0128005_r8*rxt(k,658)*y(k,235) + .0738005_r8*rxt(k,660) &
                      *y(k,244) + .1598005_r8*rxt(k,661)*y(k,246)
         mat(k,100) = -(rxt(k,657)*y(k,282))
         mat(k,2039) = -rxt(k,657)*y(k,234)
         mat(k,106) = -(rxt(k,658)*y(k,282))
         mat(k,2040) = -rxt(k,658)*y(k,235)
         mat(k,248) = .100_r8*rxt(k,542)*y(k,282)
         mat(k,270) = .230_r8*rxt(k,544)*y(k,282)
         mat(k,2056) = .100_r8*rxt(k,542)*y(k,244) + .230_r8*rxt(k,544)*y(k,246)
         mat(k,757) = -(rxt(k,566)*y(k,282))
         mat(k,2126) = -rxt(k,566)*y(k,238)
         mat(k,1820) = rxt(k,564)*y(k,286)
         mat(k,1248) = rxt(k,564)*y(k,267)
         mat(k,732) = -(rxt(k,567)*y(k,282))
         mat(k,2123) = -rxt(k,567)*y(k,239)
         mat(k,2266) = .200_r8*rxt(k,560)*y(k,280) + .200_r8*rxt(k,570)*y(k,287)
         mat(k,1641) = .500_r8*rxt(k,558)*y(k,280)
         mat(k,1191) = .200_r8*rxt(k,560)*y(k,154) + .500_r8*rxt(k,558)*y(k,262)
         mat(k,1268) = .200_r8*rxt(k,570)*y(k,154)
         mat(k,589) = -(rxt(k,571)*y(k,282))
         mat(k,2105) = -rxt(k,571)*y(k,240)
         mat(k,1811) = rxt(k,569)*y(k,287)
         mat(k,1267) = rxt(k,569)*y(k,267)
         mat(k,1115) = -(rxt(k,572)*y(k,156) + rxt(k,573)*y(k,282))
         mat(k,2407) = -rxt(k,572)*y(k,241)
         mat(k,2156) = -rxt(k,573)*y(k,241)
         mat(k,1004) = .330_r8*rxt(k,553)*y(k,164)
         mat(k,1096) = .330_r8*rxt(k,556)*y(k,164)
         mat(k,2285) = .800_r8*rxt(k,560)*y(k,280) + .800_r8*rxt(k,570)*y(k,287)
         mat(k,2407) = mat(k,2407) + rxt(k,561)*y(k,280)
         mat(k,2643) = .330_r8*rxt(k,553)*y(k,6) + .330_r8*rxt(k,556)*y(k,140)
         mat(k,733) = rxt(k,567)*y(k,282)
         mat(k,1650) = .500_r8*rxt(k,558)*y(k,280) + rxt(k,568)*y(k,287)
         mat(k,1193) = .800_r8*rxt(k,560)*y(k,154) + rxt(k,561)*y(k,156) &
                      + .500_r8*rxt(k,558)*y(k,262)
         mat(k,2156) = mat(k,2156) + rxt(k,567)*y(k,239)
         mat(k,1271) = .800_r8*rxt(k,570)*y(k,154) + rxt(k,568)*y(k,262)
         mat(k,1208) = -(rxt(k,574)*y(k,282))
         mat(k,2163) = -rxt(k,574)*y(k,242)
         mat(k,1006) = .300_r8*rxt(k,553)*y(k,164)
         mat(k,1099) = .300_r8*rxt(k,556)*y(k,164)
         mat(k,2291) = .900_r8*rxt(k,565)*y(k,286)
         mat(k,2648) = .300_r8*rxt(k,553)*y(k,6) + .300_r8*rxt(k,556)*y(k,140)
         mat(k,1656) = rxt(k,563)*y(k,286)
         mat(k,1253) = .900_r8*rxt(k,565)*y(k,154) + rxt(k,563)*y(k,262)
         mat(k,770) = -(rxt(k,541)*y(k,282))
         mat(k,2127) = -rxt(k,541)*y(k,243)
         mat(k,1821) = rxt(k,539)*y(k,288)
         mat(k,845) = rxt(k,539)*y(k,267)
         mat(k,246) = -(rxt(k,542)*y(k,282))
         mat(k,2054) = -rxt(k,542)*y(k,244)
         mat(k,262) = -(rxt(k,508)*y(k,282))
         mat(k,2057) = -rxt(k,508)*y(k,245)
         mat(k,1789) = rxt(k,505)*y(k,290)
         mat(k,1317) = rxt(k,505)*y(k,267)
         mat(k,271) = -(rxt(k,544)*y(k,282))
         mat(k,2058) = -rxt(k,544)*y(k,246)
         mat(k,816) = -(rxt(k,547)*y(k,282))
         mat(k,2132) = -rxt(k,547)*y(k,247)
         mat(k,1825) = rxt(k,545)*y(k,291)
         mat(k,861) = rxt(k,545)*y(k,267)
         mat(k,279) = -(rxt(k,550)*y(k,282))
         mat(k,2059) = -rxt(k,550)*y(k,248)
         mat(k,272) = .150_r8*rxt(k,544)*y(k,282)
         mat(k,2059) = mat(k,2059) + .150_r8*rxt(k,544)*y(k,246)
         mat(k,522) = -(rxt(k,551)*y(k,282))
         mat(k,2097) = -rxt(k,551)*y(k,249)
         mat(k,1805) = rxt(k,548)*y(k,293)
         mat(k,605) = rxt(k,548)*y(k,267)
         mat(k,641) = -(rxt(k,509)*y(k,267) + rxt(k,510)*y(k,154) + rxt(k,538) &
                      *y(k,155))
         mat(k,1814) = -rxt(k,509)*y(k,252)
         mat(k,2261) = -rxt(k,510)*y(k,252)
         mat(k,2547) = -rxt(k,538)*y(k,252)
         mat(k,288) = rxt(k,515)*y(k,282)
         mat(k,2111) = rxt(k,515)*y(k,24)
      end do
      end subroutine nlnmat08
      subroutine nlnmat09( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,1023) = -(rxt(k,470)*y(k,267) + (rxt(k,471) + rxt(k,472)) * y(k,154))
         mat(k,1837) = -rxt(k,470)*y(k,253)
         mat(k,2281) = -(rxt(k,471) + rxt(k,472)) * y(k,253)
         mat(k,750) = rxt(k,473)*y(k,282)
         mat(k,285) = rxt(k,474)*y(k,282)
         mat(k,2148) = rxt(k,473)*y(k,2) + rxt(k,474)*y(k,15)
         mat(k,598) = -(rxt(k,512)*y(k,267) + rxt(k,513)*y(k,154))
         mat(k,1812) = -rxt(k,512)*y(k,255)
         mat(k,2257) = -rxt(k,513)*y(k,255)
         mat(k,212) = .350_r8*rxt(k,511)*y(k,282)
         mat(k,518) = rxt(k,514)*y(k,282)
         mat(k,2106) = .350_r8*rxt(k,511)*y(k,7) + rxt(k,514)*y(k,8)
         mat(k,530) = -(rxt(k,516)*y(k,267) + rxt(k,518)*y(k,154))
         mat(k,1806) = -rxt(k,516)*y(k,257)
         mat(k,2251) = -rxt(k,518)*y(k,257)
         mat(k,388) = rxt(k,517)*y(k,282)
         mat(k,249) = .070_r8*rxt(k,542)*y(k,282)
         mat(k,273) = .060_r8*rxt(k,544)*y(k,282)
         mat(k,2098) = rxt(k,517)*y(k,25) + .070_r8*rxt(k,542)*y(k,244) &
                      + .060_r8*rxt(k,544)*y(k,246)
         mat(k,913) = -(4._r8*rxt(k,393)*y(k,258) + rxt(k,394)*y(k,262) + rxt(k,395) &
                      *y(k,267) + rxt(k,396)*y(k,154))
         mat(k,1646) = -rxt(k,394)*y(k,258)
         mat(k,1834) = -rxt(k,395)*y(k,258)
         mat(k,2278) = -rxt(k,396)*y(k,258)
         mat(k,398) = .500_r8*rxt(k,398)*y(k,282)
         mat(k,344) = rxt(k,399)*y(k,70) + rxt(k,400)*y(k,282)
         mat(k,1938) = rxt(k,399)*y(k,32)
         mat(k,2142) = .500_r8*rxt(k,398)*y(k,31) + rxt(k,400)*y(k,32)
         mat(k,893) = -(rxt(k,422)*y(k,262) + rxt(k,423)*y(k,267) + rxt(k,424) &
                      *y(k,154))
         mat(k,1643) = -rxt(k,422)*y(k,259)
         mat(k,1832) = -rxt(k,423)*y(k,259)
         mat(k,2276) = -rxt(k,424)*y(k,259)
         mat(k,493) = rxt(k,425)*y(k,282)
         mat(k,134) = rxt(k,426)*y(k,282)
         mat(k,2139) = rxt(k,425)*y(k,34) + rxt(k,426)*y(k,35)
         mat(k,740) = -(rxt(k,519)*y(k,267) + rxt(k,520)*y(k,154))
         mat(k,1818) = -rxt(k,519)*y(k,260)
         mat(k,2267) = -rxt(k,520)*y(k,260)
         mat(k,331) = rxt(k,521)*y(k,282)
         mat(k,2267) = mat(k,2267) + rxt(k,510)*y(k,252)
         mat(k,2631) = rxt(k,536)*y(k,172)
         mat(k,586) = rxt(k,536)*y(k,164)
         mat(k,642) = rxt(k,510)*y(k,154) + .400_r8*rxt(k,509)*y(k,267)
         mat(k,1818) = mat(k,1818) + .400_r8*rxt(k,509)*y(k,252)
         mat(k,2124) = rxt(k,521)*y(k,36)
         mat(k,1524) = -(4._r8*rxt(k,404)*y(k,261) + rxt(k,405)*y(k,262) + rxt(k,406) &
                      *y(k,267) + rxt(k,407)*y(k,154) + rxt(k,418)*y(k,155) + rxt(k,445) &
                      *y(k,274) + rxt(k,478)*y(k,269) + rxt(k,483)*y(k,270) + rxt(k,492) &
                      *y(k,271) + rxt(k,503)*y(k,290))
         mat(k,1671) = -rxt(k,405)*y(k,261)
         mat(k,1862) = -rxt(k,406)*y(k,261)
         mat(k,2307) = -rxt(k,407)*y(k,261)
         mat(k,2564) = -rxt(k,418)*y(k,261)
         mat(k,1451) = -rxt(k,445)*y(k,261)
         mat(k,1396) = -rxt(k,478)*y(k,261)
         mat(k,1429) = -rxt(k,483)*y(k,261)
         mat(k,1348) = -rxt(k,492)*y(k,261)
         mat(k,1326) = -rxt(k,503)*y(k,261)
         mat(k,1011) = .060_r8*rxt(k,553)*y(k,164)
         mat(k,1175) = rxt(k,401)*y(k,156) + rxt(k,402)*y(k,282)
         mat(k,1373) = rxt(k,427)*y(k,156) + rxt(k,428)*y(k,282)
         mat(k,702) = .500_r8*rxt(k,409)*y(k,282)
         mat(k,947) = .080_r8*rxt(k,498)*y(k,164)
         mat(k,1364) = .100_r8*rxt(k,451)*y(k,164)
         mat(k,1104) = .060_r8*rxt(k,556)*y(k,164)
         mat(k,1472) = .280_r8*rxt(k,465)*y(k,164)
         mat(k,2307) = mat(k,2307) + .530_r8*rxt(k,449)*y(k,274) + rxt(k,458)*y(k,276) &
                      + rxt(k,461)*y(k,278) + rxt(k,436)*y(k,285)
         mat(k,2431) = rxt(k,401)*y(k,54) + rxt(k,427)*y(k,58) + .530_r8*rxt(k,448) &
                      *y(k,274) + rxt(k,459)*y(k,276)
         mat(k,2662) = .060_r8*rxt(k,553)*y(k,6) + .080_r8*rxt(k,498)*y(k,127) &
                      + .100_r8*rxt(k,451)*y(k,135) + .060_r8*rxt(k,556)*y(k,140) &
                      + .280_r8*rxt(k,465)*y(k,141)
         mat(k,1211) = .650_r8*rxt(k,574)*y(k,282)
         mat(k,1524) = mat(k,1524) + .530_r8*rxt(k,445)*y(k,274)
         mat(k,1671) = mat(k,1671) + .260_r8*rxt(k,446)*y(k,274) + rxt(k,455)*y(k,276) &
                      + .300_r8*rxt(k,434)*y(k,285)
         mat(k,1862) = mat(k,1862) + .450_r8*rxt(k,456)*y(k,276) + .200_r8*rxt(k,460) &
                      *y(k,278) + .150_r8*rxt(k,435)*y(k,285)
         mat(k,1451) = mat(k,1451) + .530_r8*rxt(k,449)*y(k,154) + .530_r8*rxt(k,448) &
                      *y(k,156) + .530_r8*rxt(k,445)*y(k,261) + .260_r8*rxt(k,446) &
                      *y(k,262)
         mat(k,1493) = rxt(k,458)*y(k,154) + rxt(k,459)*y(k,156) + rxt(k,455)*y(k,262) &
                      + .450_r8*rxt(k,456)*y(k,267) + 4.000_r8*rxt(k,457)*y(k,276)
         mat(k,794) = rxt(k,461)*y(k,154) + .200_r8*rxt(k,460)*y(k,267)
         mat(k,2180) = rxt(k,402)*y(k,54) + rxt(k,428)*y(k,58) + .500_r8*rxt(k,409) &
                      *y(k,60) + .650_r8*rxt(k,574)*y(k,242)
         mat(k,1294) = rxt(k,436)*y(k,154) + .300_r8*rxt(k,434)*y(k,262) &
                      + .150_r8*rxt(k,435)*y(k,267)
         mat(k,1673) = -(rxt(k,233)*y(k,74) + (rxt(k,352) + rxt(k,353)) * y(k,68) &
                      + (4._r8*rxt(k,372) + 4._r8*rxt(k,373)) * y(k,262) + rxt(k,374) &
                      *y(k,267) + rxt(k,375)*y(k,154) + rxt(k,394)*y(k,258) + rxt(k,405) &
                      *y(k,261) + rxt(k,422)*y(k,259) + rxt(k,434)*y(k,285) + rxt(k,446) &
                      *y(k,274) + rxt(k,455)*y(k,276) + rxt(k,479)*y(k,269) + rxt(k,484) &
                      *y(k,270) + rxt(k,493)*y(k,271) + rxt(k,504)*y(k,290) + rxt(k,558) &
                      *y(k,280) + rxt(k,563)*y(k,286) + rxt(k,568)*y(k,287))
         mat(k,2599) = -rxt(k,233)*y(k,262)
         mat(k,1126) = -(rxt(k,352) + rxt(k,353)) * y(k,262)
         mat(k,1868) = -rxt(k,374)*y(k,262)
         mat(k,2310) = -rxt(k,375)*y(k,262)
         mat(k,915) = -rxt(k,394)*y(k,262)
         mat(k,1526) = -rxt(k,405)*y(k,262)
         mat(k,896) = -rxt(k,422)*y(k,262)
         mat(k,1295) = -rxt(k,434)*y(k,262)
         mat(k,1452) = -rxt(k,446)*y(k,262)
         mat(k,1494) = -rxt(k,455)*y(k,262)
         mat(k,1397) = -rxt(k,479)*y(k,262)
         mat(k,1430) = -rxt(k,484)*y(k,262)
         mat(k,1349) = -rxt(k,493)*y(k,262)
         mat(k,1327) = -rxt(k,504)*y(k,262)
         mat(k,1198) = -rxt(k,558)*y(k,262)
         mat(k,1258) = -rxt(k,563)*y(k,262)
         mat(k,1279) = -rxt(k,568)*y(k,262)
         mat(k,1147) = .280_r8*rxt(k,421)*y(k,164)
         mat(k,801) = rxt(k,408)*y(k,282)
         mat(k,499) = .700_r8*rxt(k,377)*y(k,282)
         mat(k,1601) = rxt(k,225)*y(k,70) + rxt(k,326)*y(k,89) + rxt(k,384)*y(k,281) &
                      + rxt(k,378)*y(k,282)
         mat(k,1954) = rxt(k,225)*y(k,64)
         mat(k,985) = rxt(k,326)*y(k,64)
         mat(k,948) = .050_r8*rxt(k,498)*y(k,164)
         mat(k,2310) = mat(k,2310) + rxt(k,407)*y(k,261) + .830_r8*rxt(k,524)*y(k,263) &
                      + .170_r8*rxt(k,530)*y(k,277)
         mat(k,2665) = .280_r8*rxt(k,421)*y(k,33) + .050_r8*rxt(k,498)*y(k,127)
         mat(k,1526) = mat(k,1526) + rxt(k,407)*y(k,154) + 4.000_r8*rxt(k,404) &
                      *y(k,261) + .900_r8*rxt(k,405)*y(k,262) + .450_r8*rxt(k,406) &
                      *y(k,267) + rxt(k,478)*y(k,269) + rxt(k,483)*y(k,270) &
                      + rxt(k,492)*y(k,271) + rxt(k,445)*y(k,274) + rxt(k,454) &
                      *y(k,276) + rxt(k,503)*y(k,290)
         mat(k,1673) = mat(k,1673) + .900_r8*rxt(k,405)*y(k,261)
         mat(k,877) = .830_r8*rxt(k,524)*y(k,154) + .330_r8*rxt(k,523)*y(k,267)
         mat(k,1868) = mat(k,1868) + .450_r8*rxt(k,406)*y(k,261) + .330_r8*rxt(k,523) &
                      *y(k,263) + .070_r8*rxt(k,529)*y(k,277)
         mat(k,1397) = mat(k,1397) + rxt(k,478)*y(k,261)
         mat(k,1430) = mat(k,1430) + rxt(k,483)*y(k,261)
         mat(k,1349) = mat(k,1349) + rxt(k,492)*y(k,261)
         mat(k,1452) = mat(k,1452) + rxt(k,445)*y(k,261)
         mat(k,1494) = mat(k,1494) + rxt(k,454)*y(k,261)
         mat(k,976) = .170_r8*rxt(k,530)*y(k,154) + .070_r8*rxt(k,529)*y(k,267)
         mat(k,2000) = rxt(k,384)*y(k,64)
         mat(k,2187) = rxt(k,408)*y(k,59) + .700_r8*rxt(k,377)*y(k,63) + rxt(k,378) &
                      *y(k,64)
         mat(k,1327) = mat(k,1327) + rxt(k,503)*y(k,261)
         mat(k,874) = -(rxt(k,523)*y(k,267) + rxt(k,524)*y(k,154) + rxt(k,525) &
                      *y(k,155))
         mat(k,1830) = -rxt(k,523)*y(k,263)
         mat(k,2274) = -rxt(k,524)*y(k,263)
         mat(k,2552) = -rxt(k,525)*y(k,263)
         mat(k,671) = -((rxt(k,442) + rxt(k,443)) * y(k,154))
         mat(k,2263) = -(rxt(k,442) + rxt(k,443)) * y(k,264)
         mat(k,434) = rxt(k,441)*y(k,282)
         mat(k,2115) = rxt(k,441)*y(k,16)
         mat(k,2247) = .750_r8*rxt(k,411)*y(k,266)
         mat(k,828) = .750_r8*rxt(k,411)*y(k,154)
         mat(k,829) = -(rxt(k,410)*y(k,267) + rxt(k,411)*y(k,154))
         mat(k,1826) = -rxt(k,410)*y(k,266)
         mat(k,2270) = -rxt(k,411)*y(k,266)
         mat(k,664) = rxt(k,417)*y(k,282)
         mat(k,2133) = rxt(k,417)*y(k,28)
         mat(k,1873) = -((rxt(k,184) + rxt(k,185) + rxt(k,186)) * y(k,92) + rxt(k,188) &
                      *y(k,163) + rxt(k,189)*y(k,164) + rxt(k,193)*y(k,282) &
                      + 4._r8*rxt(k,198)*y(k,267) + rxt(k,208)*y(k,156) + rxt(k,213) &
                      *y(k,154) + rxt(k,218)*y(k,155) + (rxt(k,228) + rxt(k,229) &
                      ) * y(k,70) + rxt(k,237)*y(k,74) + rxt(k,264)*y(k,17) + rxt(k,271) &
                      *y(k,21) + rxt(k,293)*y(k,115) + rxt(k,308)*y(k,125) + rxt(k,354) &
                      *y(k,68) + rxt(k,368)*y(k,51) + rxt(k,374)*y(k,262) + rxt(k,381) &
                      *y(k,268) + rxt(k,395)*y(k,258) + rxt(k,406)*y(k,261) + rxt(k,410) &
                      *y(k,266) + rxt(k,423)*y(k,259) + rxt(k,431)*y(k,284) + rxt(k,435) &
                      *y(k,285) + rxt(k,447)*y(k,274) + rxt(k,456)*y(k,276) + rxt(k,460) &
                      *y(k,278) + rxt(k,470)*y(k,253) + rxt(k,480)*y(k,269) + rxt(k,485) &
                      *y(k,270) + rxt(k,494)*y(k,271) + rxt(k,505)*y(k,290) + rxt(k,509) &
                      *y(k,252) + rxt(k,512)*y(k,255) + rxt(k,516)*y(k,257) + rxt(k,519) &
                      *y(k,260) + rxt(k,523)*y(k,263) + rxt(k,526)*y(k,275) + rxt(k,529) &
                      *y(k,277) + rxt(k,532)*y(k,283) + rxt(k,539)*y(k,288) + rxt(k,545) &
                      *y(k,291) + rxt(k,548)*y(k,293) + rxt(k,559)*y(k,280) + rxt(k,564) &
                      *y(k,286) + rxt(k,569)*y(k,287))
         mat(k,2492) = -(rxt(k,184) + rxt(k,185) + rxt(k,186)) * y(k,267)
         mat(k,2227) = -rxt(k,188)*y(k,267)
         mat(k,2669) = -rxt(k,189)*y(k,267)
         mat(k,2192) = -rxt(k,193)*y(k,267)
         mat(k,2441) = -rxt(k,208)*y(k,267)
         mat(k,2315) = -rxt(k,213)*y(k,267)
         mat(k,2574) = -rxt(k,218)*y(k,267)
         mat(k,1959) = -(rxt(k,228) + rxt(k,229)) * y(k,267)
         mat(k,2603) = -rxt(k,237)*y(k,267)
         mat(k,2343) = -rxt(k,264)*y(k,267)
         mat(k,2371) = -rxt(k,271)*y(k,267)
         mat(k,2469) = -rxt(k,293)*y(k,267)
         mat(k,1903) = -rxt(k,308)*y(k,267)
         mat(k,1129) = -rxt(k,354)*y(k,267)
         mat(k,1724) = -rxt(k,368)*y(k,267)
         mat(k,1677) = -rxt(k,374)*y(k,267)
         mat(k,539) = -rxt(k,381)*y(k,267)
         mat(k,917) = -rxt(k,395)*y(k,267)
         mat(k,1528) = -rxt(k,406)*y(k,267)
         mat(k,832) = -rxt(k,410)*y(k,267)
         mat(k,898) = -rxt(k,423)*y(k,267)
         mat(k,887) = -rxt(k,431)*y(k,267)
         mat(k,1297) = -rxt(k,435)*y(k,267)
         mat(k,1454) = -rxt(k,447)*y(k,267)
         mat(k,1496) = -rxt(k,456)*y(k,267)
         mat(k,795) = -rxt(k,460)*y(k,267)
         mat(k,1029) = -rxt(k,470)*y(k,267)
         mat(k,1399) = -rxt(k,480)*y(k,267)
         mat(k,1432) = -rxt(k,485)*y(k,267)
         mat(k,1351) = -rxt(k,494)*y(k,267)
         mat(k,1329) = -rxt(k,505)*y(k,267)
         mat(k,643) = -rxt(k,509)*y(k,267)
         mat(k,601) = -rxt(k,512)*y(k,267)
         mat(k,532) = -rxt(k,516)*y(k,267)
         mat(k,741) = -rxt(k,519)*y(k,267)
         mat(k,878) = -rxt(k,523)*y(k,267)
         mat(k,839) = -rxt(k,526)*y(k,267)
         mat(k,977) = -rxt(k,529)*y(k,267)
         mat(k,551) = -rxt(k,532)*y(k,267)
         mat(k,853) = -rxt(k,539)*y(k,267)
         mat(k,870) = -rxt(k,545)*y(k,267)
         mat(k,609) = -rxt(k,548)*y(k,267)
         mat(k,1200) = -rxt(k,559)*y(k,267)
         mat(k,1260) = -rxt(k,564)*y(k,267)
         mat(k,1281) = -rxt(k,569)*y(k,267)
         mat(k,1013) = .570_r8*rxt(k,553)*y(k,164)
         mat(k,213) = .650_r8*rxt(k,511)*y(k,282)
         mat(k,2343) = mat(k,2343) + rxt(k,263)*y(k,51)
         mat(k,2371) = mat(k,2371) + rxt(k,278)*y(k,282)
         mat(k,341) = .350_r8*rxt(k,390)*y(k,282)
         mat(k,668) = .130_r8*rxt(k,392)*y(k,164)
         mat(k,327) = rxt(k,397)*y(k,282)
         mat(k,1149) = .280_r8*rxt(k,421)*y(k,164)
         mat(k,1724) = mat(k,1724) + rxt(k,263)*y(k,17) + rxt(k,224)*y(k,70) &
                      + rxt(k,369)*y(k,156) + rxt(k,370)*y(k,163)
         mat(k,694) = rxt(k,341)*y(k,70) + rxt(k,342)*y(k,282)
         mat(k,453) = rxt(k,344)*y(k,70) + rxt(k,345)*y(k,282)
         mat(k,124) = rxt(k,403)*y(k,282)
         mat(k,460) = rxt(k,346)*y(k,70) + rxt(k,347)*y(k,282)
         mat(k,905) = rxt(k,376)*y(k,282)
         mat(k,1604) = rxt(k,385)*y(k,281)
         mat(k,1129) = mat(k,1129) + rxt(k,356)*y(k,154) + rxt(k,357)*y(k,156) + ( &
                      + 2.000_r8*rxt(k,352)+rxt(k,353))*y(k,262)
         mat(k,1959) = mat(k,1959) + rxt(k,224)*y(k,51) + rxt(k,341)*y(k,52) &
                      + rxt(k,344)*y(k,55) + rxt(k,346)*y(k,61) + rxt(k,227)*y(k,95)
         mat(k,2603) = mat(k,2603) + rxt(k,233)*y(k,262) + rxt(k,244)*y(k,282)
         mat(k,1220) = rxt(k,388)*y(k,282)
         mat(k,256) = .730_r8*rxt(k,522)*y(k,282)
         mat(k,1047) = .500_r8*rxt(k,594)*y(k,282)
         mat(k,1187) = rxt(k,414)*y(k,282)
         mat(k,1080) = rxt(k,415)*y(k,282)
         mat(k,709) = rxt(k,227)*y(k,70) + rxt(k,183)*y(k,163) + rxt(k,192)*y(k,282)
         mat(k,236) = rxt(k,379)*y(k,282)
         mat(k,1072) = rxt(k,380)*y(k,282)
         mat(k,1234) = rxt(k,444)*y(k,282)
         mat(k,1245) = rxt(k,429)*y(k,282)
         mat(k,1903) = mat(k,1903) + rxt(k,314)*y(k,282)
         mat(k,950) = .370_r8*rxt(k,498)*y(k,164)
         mat(k,728) = .300_r8*rxt(k,489)*y(k,282)
         mat(k,652) = rxt(k,490)*y(k,282)
         mat(k,477) = rxt(k,497)*y(k,282)
         mat(k,1366) = .140_r8*rxt(k,451)*y(k,164)
         mat(k,418) = .200_r8*rxt(k,453)*y(k,282)
         mat(k,719) = .500_r8*rxt(k,464)*y(k,282)
         mat(k,1107) = .570_r8*rxt(k,556)*y(k,164)
         mat(k,1476) = .280_r8*rxt(k,465)*y(k,164)
         mat(k,513) = rxt(k,501)*y(k,282)
         mat(k,1167) = rxt(k,502)*y(k,282)
         mat(k,2315) = mat(k,2315) + rxt(k,356)*y(k,68) + rxt(k,471)*y(k,253) &
                      + rxt(k,513)*y(k,255) + rxt(k,518)*y(k,257) + rxt(k,396) &
                      *y(k,258) + rxt(k,424)*y(k,259) + rxt(k,375)*y(k,262) &
                      + .170_r8*rxt(k,524)*y(k,263) + rxt(k,442)*y(k,264) &
                      + .250_r8*rxt(k,411)*y(k,266) + rxt(k,383)*y(k,268) &
                      + .920_r8*rxt(k,481)*y(k,269) + .920_r8*rxt(k,487)*y(k,270) &
                      + rxt(k,495)*y(k,271) + .470_r8*rxt(k,449)*y(k,274) &
                      + .400_r8*rxt(k,527)*y(k,275) + .830_r8*rxt(k,530)*y(k,277) &
                      + rxt(k,533)*y(k,283) + rxt(k,432)*y(k,284) + .900_r8*rxt(k,565) &
                      *y(k,286) + .800_r8*rxt(k,570)*y(k,287) + rxt(k,540)*y(k,288) &
                      + rxt(k,506)*y(k,290) + rxt(k,546)*y(k,291) + rxt(k,549) &
                      *y(k,293)
         mat(k,2441) = mat(k,2441) + rxt(k,369)*y(k,51) + rxt(k,357)*y(k,68) &
                      + rxt(k,482)*y(k,269) + rxt(k,488)*y(k,270) + rxt(k,496) &
                      *y(k,271) + .470_r8*rxt(k,448)*y(k,274) + rxt(k,211)*y(k,282) &
                      + rxt(k,507)*y(k,290)
         mat(k,2227) = mat(k,2227) + rxt(k,370)*y(k,51) + rxt(k,183)*y(k,95)
         mat(k,2669) = mat(k,2669) + .570_r8*rxt(k,553)*y(k,6) + .130_r8*rxt(k,392) &
                      *y(k,28) + .280_r8*rxt(k,421)*y(k,33) + .370_r8*rxt(k,498) &
                      *y(k,127) + .140_r8*rxt(k,451)*y(k,135) + .570_r8*rxt(k,556) &
                      *y(k,140) + .280_r8*rxt(k,465)*y(k,141) + rxt(k,195)*y(k,282)
         mat(k,222) = .800_r8*rxt(k,534)*y(k,282)
         mat(k,1059) = rxt(k,584)*y(k,282)
         mat(k,1214) = .200_r8*rxt(k,574)*y(k,282)
         mat(k,251) = .280_r8*rxt(k,542)*y(k,282)
         mat(k,277) = .380_r8*rxt(k,544)*y(k,282)
         mat(k,282) = .630_r8*rxt(k,550)*y(k,282)
         mat(k,1029) = mat(k,1029) + rxt(k,471)*y(k,154)
         mat(k,601) = mat(k,601) + rxt(k,513)*y(k,154)
         mat(k,532) = mat(k,532) + rxt(k,518)*y(k,154)
         mat(k,917) = mat(k,917) + rxt(k,396)*y(k,154) + 2.400_r8*rxt(k,393)*y(k,258) &
                      + rxt(k,394)*y(k,262)
         mat(k,898) = mat(k,898) + rxt(k,424)*y(k,154) + rxt(k,422)*y(k,262)
         mat(k,1528) = mat(k,1528) + .900_r8*rxt(k,405)*y(k,262) + rxt(k,478)*y(k,269) &
                      + rxt(k,483)*y(k,270) + rxt(k,492)*y(k,271) + .470_r8*rxt(k,445) &
                      *y(k,274) + rxt(k,503)*y(k,290)
         mat(k,1677) = mat(k,1677) + (2.000_r8*rxt(k,352)+rxt(k,353))*y(k,68) &
                      + rxt(k,233)*y(k,74) + rxt(k,375)*y(k,154) + rxt(k,394)*y(k,258) &
                      + rxt(k,422)*y(k,259) + .900_r8*rxt(k,405)*y(k,261) &
                      + 4.000_r8*rxt(k,372)*y(k,262) + rxt(k,479)*y(k,269) &
                      + rxt(k,484)*y(k,270) + 1.200_r8*rxt(k,493)*y(k,271) &
                      + .730_r8*rxt(k,446)*y(k,274) + rxt(k,455)*y(k,276) &
                      + .500_r8*rxt(k,558)*y(k,280) + .300_r8*rxt(k,434)*y(k,285) &
                      + rxt(k,563)*y(k,286) + rxt(k,568)*y(k,287) + .800_r8*rxt(k,504) &
                      *y(k,290)
         mat(k,878) = mat(k,878) + .170_r8*rxt(k,524)*y(k,154) + .070_r8*rxt(k,523) &
                      *y(k,267)
         mat(k,676) = rxt(k,442)*y(k,154)
         mat(k,832) = mat(k,832) + .250_r8*rxt(k,411)*y(k,154)
         mat(k,1873) = mat(k,1873) + .070_r8*rxt(k,523)*y(k,263) + .160_r8*rxt(k,526) &
                      *y(k,275) + .330_r8*rxt(k,529)*y(k,277)
         mat(k,539) = mat(k,539) + rxt(k,383)*y(k,154)
         mat(k,1399) = mat(k,1399) + .920_r8*rxt(k,481)*y(k,154) + rxt(k,482)*y(k,156) &
                      + rxt(k,478)*y(k,261) + rxt(k,479)*y(k,262)
         mat(k,1432) = mat(k,1432) + .920_r8*rxt(k,487)*y(k,154) + rxt(k,488)*y(k,156) &
                      + rxt(k,483)*y(k,261) + rxt(k,484)*y(k,262)
         mat(k,1351) = mat(k,1351) + rxt(k,495)*y(k,154) + rxt(k,496)*y(k,156) &
                      + rxt(k,492)*y(k,261) + 1.200_r8*rxt(k,493)*y(k,262)
         mat(k,1454) = mat(k,1454) + .470_r8*rxt(k,449)*y(k,154) + .470_r8*rxt(k,448) &
                      *y(k,156) + .470_r8*rxt(k,445)*y(k,261) + .730_r8*rxt(k,446) &
                      *y(k,262)
         mat(k,839) = mat(k,839) + .400_r8*rxt(k,527)*y(k,154) + .160_r8*rxt(k,526) &
                      *y(k,267)
         mat(k,1496) = mat(k,1496) + rxt(k,455)*y(k,262)
         mat(k,977) = mat(k,977) + .830_r8*rxt(k,530)*y(k,154) + .330_r8*rxt(k,529) &
                      *y(k,267)
         mat(k,1200) = mat(k,1200) + .500_r8*rxt(k,558)*y(k,262)
         mat(k,2005) = rxt(k,385)*y(k,64)
         mat(k,2192) = mat(k,2192) + .650_r8*rxt(k,511)*y(k,7) + rxt(k,278)*y(k,21) &
                      + .350_r8*rxt(k,390)*y(k,27) + rxt(k,397)*y(k,30) + rxt(k,342) &
                      *y(k,52) + rxt(k,345)*y(k,55) + rxt(k,403)*y(k,56) + rxt(k,347) &
                      *y(k,61) + rxt(k,376)*y(k,62) + rxt(k,244)*y(k,74) + rxt(k,388) &
                      *y(k,77) + .730_r8*rxt(k,522)*y(k,82) + .500_r8*rxt(k,594) &
                      *y(k,83) + rxt(k,414)*y(k,90) + rxt(k,415)*y(k,91) + rxt(k,192) &
                      *y(k,95) + rxt(k,379)*y(k,102) + rxt(k,380)*y(k,103) &
                      + rxt(k,444)*y(k,111) + rxt(k,429)*y(k,113) + rxt(k,314) &
                      *y(k,125) + .300_r8*rxt(k,489)*y(k,128) + rxt(k,490)*y(k,129) &
                      + rxt(k,497)*y(k,130) + .200_r8*rxt(k,453)*y(k,136) &
                      + .500_r8*rxt(k,464)*y(k,139) + rxt(k,501)*y(k,145) + rxt(k,502) &
                      *y(k,146) + rxt(k,211)*y(k,156) + rxt(k,195)*y(k,164) &
                      + .800_r8*rxt(k,534)*y(k,173) + rxt(k,584)*y(k,184) &
                      + .200_r8*rxt(k,574)*y(k,242) + .280_r8*rxt(k,542)*y(k,244) &
                      + .380_r8*rxt(k,544)*y(k,246) + .630_r8*rxt(k,550)*y(k,248)
         mat(k,551) = mat(k,551) + rxt(k,533)*y(k,154)
         mat(k,887) = mat(k,887) + rxt(k,432)*y(k,154)
         mat(k,1297) = mat(k,1297) + .300_r8*rxt(k,434)*y(k,262)
         mat(k,1260) = mat(k,1260) + .900_r8*rxt(k,565)*y(k,154) + rxt(k,563)*y(k,262)
         mat(k,1281) = mat(k,1281) + .800_r8*rxt(k,570)*y(k,154) + rxt(k,568)*y(k,262)
         mat(k,853) = mat(k,853) + rxt(k,540)*y(k,154)
         mat(k,1329) = mat(k,1329) + rxt(k,506)*y(k,154) + rxt(k,507)*y(k,156) &
                      + rxt(k,503)*y(k,261) + .800_r8*rxt(k,504)*y(k,262)
         mat(k,870) = mat(k,870) + rxt(k,546)*y(k,154)
         mat(k,609) = mat(k,609) + rxt(k,549)*y(k,154)
      end do
      end subroutine nlnmat09
      subroutine nlnmat10( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,536) = -(rxt(k,381)*y(k,267) + rxt(k,383)*y(k,154))
         mat(k,1807) = -rxt(k,381)*y(k,268)
         mat(k,2252) = -rxt(k,383)*y(k,268)
         mat(k,1712) = rxt(k,368)*y(k,267)
         mat(k,1807) = mat(k,1807) + rxt(k,368)*y(k,51)
         mat(k,1392) = -(rxt(k,478)*y(k,261) + rxt(k,479)*y(k,262) + rxt(k,480) &
                      *y(k,267) + rxt(k,481)*y(k,154) + rxt(k,482)*y(k,156))
         mat(k,1519) = -rxt(k,478)*y(k,269)
         mat(k,1666) = -rxt(k,479)*y(k,269)
         mat(k,1857) = -rxt(k,480)*y(k,269)
         mat(k,2302) = -rxt(k,481)*y(k,269)
         mat(k,2426) = -rxt(k,482)*y(k,269)
         mat(k,944) = .600_r8*rxt(k,499)*y(k,282)
         mat(k,2175) = .600_r8*rxt(k,499)*y(k,127)
         mat(k,1425) = -(rxt(k,483)*y(k,261) + rxt(k,484)*y(k,262) + rxt(k,485) &
                      *y(k,267) + rxt(k,487)*y(k,154) + rxt(k,488)*y(k,156))
         mat(k,1520) = -rxt(k,483)*y(k,270)
         mat(k,1667) = -rxt(k,484)*y(k,270)
         mat(k,1858) = -rxt(k,485)*y(k,270)
         mat(k,2303) = -rxt(k,487)*y(k,270)
         mat(k,2427) = -rxt(k,488)*y(k,270)
         mat(k,945) = .400_r8*rxt(k,499)*y(k,282)
         mat(k,2176) = .400_r8*rxt(k,499)*y(k,127)
         mat(k,1344) = -(rxt(k,492)*y(k,261) + rxt(k,493)*y(k,262) + rxt(k,494) &
                      *y(k,267) + rxt(k,495)*y(k,154) + rxt(k,496)*y(k,156))
         mat(k,1516) = -rxt(k,492)*y(k,271)
         mat(k,1663) = -rxt(k,493)*y(k,271)
         mat(k,1854) = -rxt(k,494)*y(k,271)
         mat(k,2299) = -rxt(k,495)*y(k,271)
         mat(k,2423) = -rxt(k,496)*y(k,271)
         mat(k,942) = rxt(k,491)*y(k,156)
         mat(k,2423) = mat(k,2423) + rxt(k,491)*y(k,127)
         mat(k,1449) = -(rxt(k,445)*y(k,261) + rxt(k,446)*y(k,262) + rxt(k,447) &
                      *y(k,267) + rxt(k,448)*y(k,156) + (rxt(k,449) + rxt(k,450) &
                      ) * y(k,154))
         mat(k,1521) = -rxt(k,445)*y(k,274)
         mat(k,1668) = -rxt(k,446)*y(k,274)
         mat(k,1859) = -rxt(k,447)*y(k,274)
         mat(k,2428) = -rxt(k,448)*y(k,274)
         mat(k,2304) = -(rxt(k,449) + rxt(k,450)) * y(k,274)
         mat(k,1362) = .500_r8*rxt(k,452)*y(k,282)
         mat(k,416) = .200_r8*rxt(k,453)*y(k,282)
         mat(k,1469) = rxt(k,466)*y(k,282)
         mat(k,2177) = .500_r8*rxt(k,452)*y(k,135) + .200_r8*rxt(k,453)*y(k,136) &
                      + rxt(k,466)*y(k,141)
         mat(k,836) = -(rxt(k,526)*y(k,267) + rxt(k,527)*y(k,154) + rxt(k,528) &
                      *y(k,155))
         mat(k,1827) = -rxt(k,526)*y(k,275)
         mat(k,2271) = -rxt(k,527)*y(k,275)
         mat(k,2551) = -rxt(k,528)*y(k,275)
         mat(k,1492) = -(rxt(k,454)*y(k,261) + rxt(k,455)*y(k,262) + rxt(k,456) &
                      *y(k,267) + 4._r8*rxt(k,457)*y(k,276) + rxt(k,458)*y(k,154) &
                      + rxt(k,459)*y(k,156) + rxt(k,467)*y(k,155))
         mat(k,1523) = -rxt(k,454)*y(k,276)
         mat(k,1670) = -rxt(k,455)*y(k,276)
         mat(k,1861) = -rxt(k,456)*y(k,276)
         mat(k,2306) = -rxt(k,458)*y(k,276)
         mat(k,2430) = -rxt(k,459)*y(k,276)
         mat(k,2563) = -rxt(k,467)*y(k,276)
         mat(k,1363) = .500_r8*rxt(k,452)*y(k,282)
         mat(k,417) = .500_r8*rxt(k,453)*y(k,282)
         mat(k,2179) = .500_r8*rxt(k,452)*y(k,135) + .500_r8*rxt(k,453)*y(k,136)
         mat(k,972) = -(rxt(k,529)*y(k,267) + rxt(k,530)*y(k,154) + rxt(k,531) &
                      *y(k,155))
         mat(k,1836) = -rxt(k,529)*y(k,277)
         mat(k,2280) = -rxt(k,530)*y(k,277)
         mat(k,2556) = -rxt(k,531)*y(k,277)
         mat(k,792) = -(rxt(k,460)*y(k,267) + rxt(k,461)*y(k,154))
         mat(k,1822) = -rxt(k,460)*y(k,278)
         mat(k,2269) = -rxt(k,461)*y(k,278)
         mat(k,636) = rxt(k,462)*y(k,282)
         mat(k,393) = rxt(k,463)*y(k,282)
         mat(k,2129) = rxt(k,462)*y(k,137) + rxt(k,463)*y(k,138)
         mat(k,1194) = -(rxt(k,558)*y(k,262) + rxt(k,559)*y(k,267) + rxt(k,560) &
                      *y(k,154) + rxt(k,561)*y(k,156))
         mat(k,1655) = -rxt(k,558)*y(k,280)
         mat(k,1845) = -rxt(k,559)*y(k,280)
         mat(k,2290) = -rxt(k,560)*y(k,280)
         mat(k,2413) = -rxt(k,561)*y(k,280)
         mat(k,1005) = rxt(k,552)*y(k,156)
         mat(k,1098) = rxt(k,555)*y(k,156)
         mat(k,2413) = mat(k,2413) + rxt(k,552)*y(k,6) + rxt(k,555)*y(k,140) &
                      + .500_r8*rxt(k,572)*y(k,241)
         mat(k,470) = rxt(k,562)*y(k,282)
         mat(k,1116) = .500_r8*rxt(k,572)*y(k,156)
         mat(k,2162) = rxt(k,562)*y(k,158)
         mat(k,2008) = -(rxt(k,173)*y(k,93) + rxt(k,174)*y(k,294) + (rxt(k,177) &
                      + rxt(k,178)) * y(k,164) + (rxt(k,216) + rxt(k,217)) * y(k,143) &
                      + rxt(k,251)*y(k,37) + rxt(k,252)*y(k,38) + rxt(k,253)*y(k,40) &
                      + rxt(k,254)*y(k,41) + rxt(k,255)*y(k,42) + rxt(k,256)*y(k,43) &
                      + rxt(k,257)*y(k,44) + (rxt(k,258) + rxt(k,259)) * y(k,101) &
                      + rxt(k,282)*y(k,39) + rxt(k,283)*y(k,66) + rxt(k,284)*y(k,94) &
                      + (rxt(k,285) + rxt(k,286)) * y(k,97) + rxt(k,330)*y(k,80) &
                      + rxt(k,331)*y(k,81) + rxt(k,363)*y(k,45) + rxt(k,364)*y(k,52) &
                      + rxt(k,365)*y(k,98) + rxt(k,366)*y(k,99) + rxt(k,367)*y(k,100) &
                      + (rxt(k,384) + rxt(k,385) + rxt(k,386)) * y(k,64) + rxt(k,387) &
                      *y(k,102))
         mat(k,1560) = -rxt(k,173)*y(k,281)
         mat(k,2701) = -rxt(k,174)*y(k,281)
         mat(k,2672) = -(rxt(k,177) + rxt(k,178)) * y(k,281)
         mat(k,229) = -(rxt(k,216) + rxt(k,217)) * y(k,281)
         mat(k,122) = -rxt(k,251)*y(k,281)
         mat(k,184) = -rxt(k,252)*y(k,281)
         mat(k,140) = -rxt(k,253)*y(k,281)
         mat(k,195) = -rxt(k,254)*y(k,281)
         mat(k,144) = -rxt(k,255)*y(k,281)
         mat(k,200) = -rxt(k,256)*y(k,281)
         mat(k,148) = -rxt(k,257)*y(k,281)
         mat(k,1776) = -(rxt(k,258) + rxt(k,259)) * y(k,281)
         mat(k,189) = -rxt(k,282)*y(k,281)
         mat(k,507) = -rxt(k,283)*y(k,281)
         mat(k,167) = -rxt(k,284)*y(k,281)
         mat(k,1545) = -(rxt(k,285) + rxt(k,286)) * y(k,281)
         mat(k,319) = -rxt(k,330)*y(k,281)
         mat(k,306) = -rxt(k,331)*y(k,281)
         mat(k,578) = -rxt(k,363)*y(k,281)
         mat(k,696) = -rxt(k,364)*y(k,281)
         mat(k,301) = -rxt(k,365)*y(k,281)
         mat(k,315) = -rxt(k,366)*y(k,281)
         mat(k,353) = -rxt(k,367)*y(k,281)
         mat(k,1606) = -(rxt(k,384) + rxt(k,385) + rxt(k,386)) * y(k,281)
         mat(k,237) = -rxt(k,387)*y(k,281)
         mat(k,2196) = -(rxt(k,191)*y(k,93) + rxt(k,192)*y(k,95) + rxt(k,193)*y(k,267) &
                      + rxt(k,194)*y(k,163) + rxt(k,195)*y(k,164) + (4._r8*rxt(k,196) &
                      + 4._r8*rxt(k,197)) * y(k,282) + rxt(k,199)*y(k,107) + rxt(k,211) &
                      *y(k,156) + rxt(k,212)*y(k,142) + rxt(k,220)*y(k,155) + rxt(k,221) &
                      *y(k,106) + rxt(k,231)*y(k,73) + rxt(k,242)*y(k,75) + (rxt(k,244) &
                      + rxt(k,245)) * y(k,74) + rxt(k,247)*y(k,101) + rxt(k,250) &
                      *y(k,109) + rxt(k,262)*y(k,18) + rxt(k,278)*y(k,21) + rxt(k,280) &
                      *y(k,97) + rxt(k,288)*y(k,110) + rxt(k,291)*y(k,116) + rxt(k,314) &
                      *y(k,125) + rxt(k,315)*y(k,105) + rxt(k,333)*y(k,26) + rxt(k,335) &
                      *y(k,29) + rxt(k,337)*y(k,45) + rxt(k,338)*y(k,46) + rxt(k,340) &
                      *y(k,47) + rxt(k,342)*y(k,52) + rxt(k,343)*y(k,53) + rxt(k,345) &
                      *y(k,55) + rxt(k,347)*y(k,61) + rxt(k,348)*y(k,65) + rxt(k,350) &
                      *y(k,66) + rxt(k,351)*y(k,67) + rxt(k,359)*y(k,69) + rxt(k,360) &
                      *y(k,98) + rxt(k,361)*y(k,99) + rxt(k,362)*y(k,100) + rxt(k,371) &
                      *y(k,51) + rxt(k,376)*y(k,62) + rxt(k,377)*y(k,63) + rxt(k,378) &
                      *y(k,64) + rxt(k,379)*y(k,102) + rxt(k,380)*y(k,103) + rxt(k,388) &
                      *y(k,77) + rxt(k,390)*y(k,27) + rxt(k,397)*y(k,30) + rxt(k,398) &
                      *y(k,31) + rxt(k,400)*y(k,32) + rxt(k,402)*y(k,54) + rxt(k,403) &
                      *y(k,56) + rxt(k,408)*y(k,59) + rxt(k,409)*y(k,60) + rxt(k,414) &
                      *y(k,90) + rxt(k,415)*y(k,91) + rxt(k,416)*y(k,170) + rxt(k,417) &
                      *y(k,28) + rxt(k,425)*y(k,34) + rxt(k,426)*y(k,35) + rxt(k,428) &
                      *y(k,58) + rxt(k,429)*y(k,113) + rxt(k,430)*y(k,157) + rxt(k,433) &
                      *y(k,179) + rxt(k,437)*y(k,180) + rxt(k,438)*y(k,33) + rxt(k,439) &
                      *y(k,57) + rxt(k,441)*y(k,16) + rxt(k,444)*y(k,111) + rxt(k,452) &
                      *y(k,135) + rxt(k,453)*y(k,136) + rxt(k,462)*y(k,137) + rxt(k,463) &
                      *y(k,138) + rxt(k,464)*y(k,139) + rxt(k,466)*y(k,141) + rxt(k,469) &
                      *y(k,1) + rxt(k,473)*y(k,2) + rxt(k,474)*y(k,15) + rxt(k,475) &
                      *y(k,112) + rxt(k,476)*y(k,114) + rxt(k,477)*y(k,122) + rxt(k,489) &
                      *y(k,128) + rxt(k,490)*y(k,129) + rxt(k,497)*y(k,130) + rxt(k,499) &
                      *y(k,127) + rxt(k,500)*y(k,131) + rxt(k,501)*y(k,145) + rxt(k,502) &
                      *y(k,146) + rxt(k,508)*y(k,245) + rxt(k,511)*y(k,7) + rxt(k,514) &
                      *y(k,8) + rxt(k,515)*y(k,24) + rxt(k,517)*y(k,25) + rxt(k,521) &
                      *y(k,36) + rxt(k,522)*y(k,82) + rxt(k,534)*y(k,173) + rxt(k,537) &
                      *y(k,174) + rxt(k,541)*y(k,243) + rxt(k,542)*y(k,244) + rxt(k,544) &
                      *y(k,246) + rxt(k,547)*y(k,247) + rxt(k,550)*y(k,248) + rxt(k,551) &
                      *y(k,249) + rxt(k,554)*y(k,6) + rxt(k,557)*y(k,140) + rxt(k,562) &
                      *y(k,158) + rxt(k,566)*y(k,238) + rxt(k,567)*y(k,239) + rxt(k,571) &
                      *y(k,240) + rxt(k,573)*y(k,241) + rxt(k,574)*y(k,242) + (rxt(k,580) &
                      + rxt(k,594)) * y(k,83) + rxt(k,582)*y(k,167) + rxt(k,584) &
                      *y(k,184) + rxt(k,588)*y(k,181) + rxt(k,593)*y(k,183) + rxt(k,613) &
                      *y(k,150))
         mat(k,1561) = -rxt(k,191)*y(k,282)
         mat(k,711) = -rxt(k,192)*y(k,282)
         mat(k,1877) = -rxt(k,193)*y(k,282)
         mat(k,2231) = -rxt(k,194)*y(k,282)
         mat(k,2673) = -rxt(k,195)*y(k,282)
         mat(k,482) = -rxt(k,199)*y(k,282)
         mat(k,2445) = -rxt(k,211)*y(k,282)
         mat(k,570) = -rxt(k,212)*y(k,282)
         mat(k,2578) = -rxt(k,220)*y(k,282)
         mat(k,2521) = -rxt(k,221)*y(k,282)
         mat(k,633) = -rxt(k,231)*y(k,282)
         mat(k,1038) = -rxt(k,242)*y(k,282)
         mat(k,2607) = -(rxt(k,244) + rxt(k,245)) * y(k,282)
         mat(k,1777) = -rxt(k,247)*y(k,282)
         mat(k,1754) = -rxt(k,250)*y(k,282)
         mat(k,565) = -rxt(k,262)*y(k,282)
         mat(k,2375) = -rxt(k,278)*y(k,282)
         mat(k,1546) = -rxt(k,280)*y(k,282)
         mat(k,1702) = -rxt(k,288)*y(k,282)
         mat(k,1572) = -rxt(k,291)*y(k,282)
         mat(k,1907) = -rxt(k,314)*y(k,282)
         mat(k,1310) = -rxt(k,315)*y(k,282)
         mat(k,234) = -rxt(k,333)*y(k,282)
         mat(k,296) = -rxt(k,335)*y(k,282)
         mat(k,579) = -rxt(k,337)*y(k,282)
         mat(k,151) = -rxt(k,338)*y(k,282)
         mat(k,360) = -rxt(k,340)*y(k,282)
         mat(k,697) = -rxt(k,342)*y(k,282)
         mat(k,155) = -rxt(k,343)*y(k,282)
         mat(k,455) = -rxt(k,345)*y(k,282)
         mat(k,462) = -rxt(k,347)*y(k,282)
         mat(k,159) = -rxt(k,348)*y(k,282)
         mat(k,508) = -rxt(k,350)*y(k,282)
         mat(k,163) = -rxt(k,351)*y(k,282)
         mat(k,410) = -rxt(k,359)*y(k,282)
         mat(k,302) = -rxt(k,360)*y(k,282)
         mat(k,316) = -rxt(k,361)*y(k,282)
         mat(k,354) = -rxt(k,362)*y(k,282)
         mat(k,1728) = -rxt(k,371)*y(k,282)
         mat(k,906) = -rxt(k,376)*y(k,282)
         mat(k,501) = -rxt(k,377)*y(k,282)
         mat(k,1607) = -rxt(k,378)*y(k,282)
         mat(k,238) = -rxt(k,379)*y(k,282)
         mat(k,1073) = -rxt(k,380)*y(k,282)
         mat(k,1221) = -rxt(k,388)*y(k,282)
         mat(k,342) = -rxt(k,390)*y(k,282)
         mat(k,328) = -rxt(k,397)*y(k,282)
         mat(k,401) = -rxt(k,398)*y(k,282)
         mat(k,347) = -rxt(k,400)*y(k,282)
         mat(k,1178) = -rxt(k,402)*y(k,282)
         mat(k,125) = -rxt(k,403)*y(k,282)
         mat(k,802) = -rxt(k,408)*y(k,282)
         mat(k,705) = -rxt(k,409)*y(k,282)
         mat(k,1188) = -rxt(k,414)*y(k,282)
         mat(k,1081) = -rxt(k,415)*y(k,282)
         mat(k,660) = -rxt(k,416)*y(k,282)
         mat(k,669) = -rxt(k,417)*y(k,282)
         mat(k,496) = -rxt(k,425)*y(k,282)
         mat(k,135) = -rxt(k,426)*y(k,282)
         mat(k,1375) = -rxt(k,428)*y(k,282)
         mat(k,1246) = -rxt(k,429)*y(k,282)
         mat(k,970) = -rxt(k,430)*y(k,282)
         mat(k,685) = -rxt(k,433)*y(k,282)
         mat(k,490) = -rxt(k,437)*y(k,282)
         mat(k,1150) = -rxt(k,438)*y(k,282)
         mat(k,1067) = -rxt(k,439)*y(k,282)
         mat(k,438) = -rxt(k,441)*y(k,282)
         mat(k,1235) = -rxt(k,444)*y(k,282)
         mat(k,1367) = -rxt(k,452)*y(k,282)
         mat(k,419) = -rxt(k,453)*y(k,282)
         mat(k,639) = -rxt(k,462)*y(k,282)
         mat(k,396) = -rxt(k,463)*y(k,282)
         mat(k,720) = -rxt(k,464)*y(k,282)
         mat(k,1477) = -rxt(k,466)*y(k,282)
         mat(k,789) = -rxt(k,469)*y(k,282)
         mat(k,756) = -rxt(k,473)*y(k,282)
         mat(k,286) = -rxt(k,474)*y(k,282)
         mat(k,311) = -rxt(k,475)*y(k,282)
         mat(k,404) = -rxt(k,476)*y(k,282)
         mat(k,174) = -rxt(k,477)*y(k,282)
         mat(k,729) = -rxt(k,489)*y(k,282)
         mat(k,653) = -rxt(k,490)*y(k,282)
         mat(k,478) = -rxt(k,497)*y(k,282)
         mat(k,951) = -rxt(k,499)*y(k,282)
         mat(k,811) = -rxt(k,500)*y(k,282)
         mat(k,514) = -rxt(k,501)*y(k,282)
         mat(k,1168) = -rxt(k,502)*y(k,282)
         mat(k,264) = -rxt(k,508)*y(k,282)
         mat(k,214) = -rxt(k,511)*y(k,282)
         mat(k,521) = -rxt(k,514)*y(k,282)
         mat(k,289) = -rxt(k,515)*y(k,282)
         mat(k,391) = -rxt(k,517)*y(k,282)
         mat(k,332) = -rxt(k,521)*y(k,282)
         mat(k,257) = -rxt(k,522)*y(k,282)
         mat(k,223) = -rxt(k,534)*y(k,282)
         mat(k,385) = -rxt(k,537)*y(k,282)
         mat(k,779) = -rxt(k,541)*y(k,282)
         mat(k,252) = -rxt(k,542)*y(k,282)
         mat(k,278) = -rxt(k,544)*y(k,282)
         mat(k,826) = -rxt(k,547)*y(k,282)
         mat(k,283) = -rxt(k,550)*y(k,282)
         mat(k,527) = -rxt(k,551)*y(k,282)
         mat(k,1014) = -rxt(k,554)*y(k,282)
         mat(k,1108) = -rxt(k,557)*y(k,282)
         mat(k,471) = -rxt(k,562)*y(k,282)
         mat(k,766) = -rxt(k,566)*y(k,282)
         mat(k,735) = -rxt(k,567)*y(k,282)
         mat(k,595) = -rxt(k,571)*y(k,282)
         mat(k,1121) = -rxt(k,573)*y(k,282)
         mat(k,1215) = -rxt(k,574)*y(k,282)
         mat(k,1050) = -(rxt(k,580) + rxt(k,594)) * y(k,282)
         mat(k,430) = -rxt(k,582)*y(k,282)
         mat(k,1060) = -rxt(k,584)*y(k,282)
         mat(k,615) = -rxt(k,588)*y(k,282)
         mat(k,1585) = -rxt(k,593)*y(k,282)
         mat(k,118) = -rxt(k,613)*y(k,282)
         mat(k,1014) = mat(k,1014) + .630_r8*rxt(k,553)*y(k,164)
         mat(k,342) = mat(k,342) + .650_r8*rxt(k,390)*y(k,282)
         mat(k,669) = mat(k,669) + .130_r8*rxt(k,392)*y(k,164)
         mat(k,401) = mat(k,401) + .500_r8*rxt(k,398)*y(k,282)
         mat(k,1150) = mat(k,1150) + .360_r8*rxt(k,421)*y(k,164)
         mat(k,1728) = mat(k,1728) + rxt(k,370)*y(k,163)
         mat(k,501) = mat(k,501) + .300_r8*rxt(k,377)*y(k,282)
         mat(k,1607) = mat(k,1607) + rxt(k,384)*y(k,281)
         mat(k,1963) = rxt(k,229)*y(k,267)
         mat(k,986) = rxt(k,328)*y(k,294)
         mat(k,2496) = rxt(k,190)*y(k,164) + 2.000_r8*rxt(k,185)*y(k,267)
         mat(k,1561) = mat(k,1561) + rxt(k,182)*y(k,163) + rxt(k,173)*y(k,281)
         mat(k,711) = mat(k,711) + rxt(k,183)*y(k,163)
         mat(k,1546) = mat(k,1546) + rxt(k,279)*y(k,163) + rxt(k,285)*y(k,281)
         mat(k,1777) = mat(k,1777) + rxt(k,246)*y(k,163) + rxt(k,258)*y(k,281)
         mat(k,238) = mat(k,238) + rxt(k,387)*y(k,281)
         mat(k,1626) = rxt(k,281)*y(k,163)
         mat(k,1754) = mat(k,1754) + rxt(k,249)*y(k,163)
         mat(k,951) = mat(k,951) + .320_r8*rxt(k,498)*y(k,164)
         mat(k,811) = mat(k,811) + .600_r8*rxt(k,500)*y(k,282)
         mat(k,1367) = mat(k,1367) + .240_r8*rxt(k,451)*y(k,164)
         mat(k,419) = mat(k,419) + .100_r8*rxt(k,453)*y(k,282)
         mat(k,1108) = mat(k,1108) + .630_r8*rxt(k,556)*y(k,164)
         mat(k,1477) = mat(k,1477) + .360_r8*rxt(k,465)*y(k,164)
         mat(k,2319) = rxt(k,213)*y(k,267)
         mat(k,2445) = mat(k,2445) + rxt(k,208)*y(k,267)
         mat(k,2231) = mat(k,2231) + rxt(k,370)*y(k,51) + rxt(k,182)*y(k,93) &
                      + rxt(k,183)*y(k,95) + rxt(k,279)*y(k,97) + rxt(k,246)*y(k,101) &
                      + rxt(k,281)*y(k,108) + rxt(k,249)*y(k,109) + rxt(k,188) &
                      *y(k,267)
         mat(k,2673) = mat(k,2673) + .630_r8*rxt(k,553)*y(k,6) + .130_r8*rxt(k,392) &
                      *y(k,28) + .360_r8*rxt(k,421)*y(k,33) + rxt(k,190)*y(k,92) &
                      + .320_r8*rxt(k,498)*y(k,127) + .240_r8*rxt(k,451)*y(k,135) &
                      + .630_r8*rxt(k,556)*y(k,140) + .360_r8*rxt(k,465)*y(k,141) &
                      + rxt(k,189)*y(k,267)
         mat(k,685) = mat(k,685) + .500_r8*rxt(k,433)*y(k,282)
         mat(k,264) = mat(k,264) + .500_r8*rxt(k,508)*y(k,282)
         mat(k,644) = .400_r8*rxt(k,509)*y(k,267)
         mat(k,1529) = .450_r8*rxt(k,406)*y(k,267)
         mat(k,879) = .400_r8*rxt(k,523)*y(k,267)
         mat(k,1877) = mat(k,1877) + rxt(k,229)*y(k,70) + 2.000_r8*rxt(k,185)*y(k,92) &
                      + rxt(k,213)*y(k,154) + rxt(k,208)*y(k,156) + rxt(k,188) &
                      *y(k,163) + rxt(k,189)*y(k,164) + .400_r8*rxt(k,509)*y(k,252) &
                      + .450_r8*rxt(k,406)*y(k,261) + .400_r8*rxt(k,523)*y(k,263) &
                      + .450_r8*rxt(k,456)*y(k,276) + .400_r8*rxt(k,529)*y(k,277) &
                      + .200_r8*rxt(k,460)*y(k,278) + .150_r8*rxt(k,435)*y(k,285)
         mat(k,1497) = .450_r8*rxt(k,456)*y(k,267)
         mat(k,978) = .400_r8*rxt(k,529)*y(k,267)
         mat(k,796) = .200_r8*rxt(k,460)*y(k,267)
         mat(k,2009) = rxt(k,384)*y(k,64) + rxt(k,173)*y(k,93) + rxt(k,285)*y(k,97) &
                      + rxt(k,258)*y(k,101) + rxt(k,387)*y(k,102) &
                      + 2.000_r8*rxt(k,174)*y(k,294)
         mat(k,2196) = mat(k,2196) + .650_r8*rxt(k,390)*y(k,27) + .500_r8*rxt(k,398) &
                      *y(k,31) + .300_r8*rxt(k,377)*y(k,63) + .600_r8*rxt(k,500) &
                      *y(k,131) + .100_r8*rxt(k,453)*y(k,136) + .500_r8*rxt(k,433) &
                      *y(k,179) + .500_r8*rxt(k,508)*y(k,245)
         mat(k,1298) = .150_r8*rxt(k,435)*y(k,267)
         mat(k,2702) = rxt(k,328)*y(k,89) + 2.000_r8*rxt(k,174)*y(k,281)
      end do
      end subroutine nlnmat10
      subroutine nlnmat11( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,549) = -(rxt(k,532)*y(k,267) + rxt(k,533)*y(k,154))
         mat(k,1808) = -rxt(k,532)*y(k,283)
         mat(k,2253) = -rxt(k,533)*y(k,283)
         mat(k,254) = .200_r8*rxt(k,522)*y(k,282)
         mat(k,220) = .140_r8*rxt(k,534)*y(k,282)
         mat(k,382) = rxt(k,537)*y(k,282)
         mat(k,2099) = .200_r8*rxt(k,522)*y(k,82) + .140_r8*rxt(k,534)*y(k,173) &
                      + rxt(k,537)*y(k,174)
         mat(k,883) = -(rxt(k,431)*y(k,267) + rxt(k,432)*y(k,154))
         mat(k,1831) = -rxt(k,431)*y(k,284)
         mat(k,2275) = -rxt(k,432)*y(k,284)
         mat(k,1136) = rxt(k,438)*y(k,282)
         mat(k,680) = .500_r8*rxt(k,433)*y(k,282)
         mat(k,2138) = rxt(k,438)*y(k,33) + .500_r8*rxt(k,433)*y(k,179)
         mat(k,1292) = -(rxt(k,434)*y(k,262) + rxt(k,435)*y(k,267) + rxt(k,436) &
                      *y(k,154))
         mat(k,1661) = -rxt(k,434)*y(k,285)
         mat(k,1851) = -rxt(k,435)*y(k,285)
         mat(k,2297) = -rxt(k,436)*y(k,285)
         mat(k,1009) = .060_r8*rxt(k,553)*y(k,164)
         mat(k,1064) = rxt(k,439)*y(k,282)
         mat(k,1102) = .060_r8*rxt(k,556)*y(k,164)
         mat(k,2653) = .060_r8*rxt(k,553)*y(k,6) + .060_r8*rxt(k,556)*y(k,140)
         mat(k,487) = rxt(k,437)*y(k,282)
         mat(k,1210) = .150_r8*rxt(k,574)*y(k,282)
         mat(k,2169) = rxt(k,439)*y(k,57) + rxt(k,437)*y(k,180) + .150_r8*rxt(k,574) &
                      *y(k,242)
         mat(k,1255) = -(rxt(k,563)*y(k,262) + rxt(k,564)*y(k,267) + rxt(k,565) &
                      *y(k,154))
         mat(k,1659) = -rxt(k,563)*y(k,286)
         mat(k,1849) = -rxt(k,564)*y(k,286)
         mat(k,2295) = -rxt(k,565)*y(k,286)
         mat(k,2418) = .500_r8*rxt(k,572)*y(k,241)
         mat(k,763) = rxt(k,566)*y(k,282)
         mat(k,1119) = .500_r8*rxt(k,572)*y(k,156) + rxt(k,573)*y(k,282)
         mat(k,2167) = rxt(k,566)*y(k,238) + rxt(k,573)*y(k,241)
         mat(k,1276) = -(rxt(k,568)*y(k,262) + rxt(k,569)*y(k,267) + rxt(k,570) &
                      *y(k,154))
         mat(k,1660) = -rxt(k,568)*y(k,287)
         mat(k,1850) = -rxt(k,569)*y(k,287)
         mat(k,2296) = -rxt(k,570)*y(k,287)
         mat(k,1008) = rxt(k,554)*y(k,282)
         mat(k,1101) = rxt(k,557)*y(k,282)
         mat(k,592) = rxt(k,571)*y(k,282)
         mat(k,2168) = rxt(k,554)*y(k,6) + rxt(k,557)*y(k,140) + rxt(k,571)*y(k,240)
         mat(k,847) = -(rxt(k,539)*y(k,267) + rxt(k,540)*y(k,154))
         mat(k,1828) = -rxt(k,539)*y(k,288)
         mat(k,2272) = -rxt(k,540)*y(k,288)
         mat(k,772) = rxt(k,541)*y(k,282)
         mat(k,250) = .650_r8*rxt(k,542)*y(k,282)
         mat(k,2135) = rxt(k,541)*y(k,243) + .650_r8*rxt(k,542)*y(k,244)
         mat(k,1324) = -(rxt(k,503)*y(k,261) + rxt(k,504)*y(k,262) + rxt(k,505) &
                      *y(k,267) + rxt(k,506)*y(k,154) + rxt(k,507)*y(k,156))
         mat(k,1515) = -rxt(k,503)*y(k,290)
         mat(k,1662) = -rxt(k,504)*y(k,290)
         mat(k,1853) = -rxt(k,505)*y(k,290)
         mat(k,2298) = -rxt(k,506)*y(k,290)
         mat(k,2422) = -rxt(k,507)*y(k,290)
         mat(k,309) = rxt(k,475)*y(k,282)
         mat(k,403) = rxt(k,476)*y(k,282)
         mat(k,173) = rxt(k,477)*y(k,282)
         mat(k,806) = .400_r8*rxt(k,500)*y(k,282)
         mat(k,263) = .500_r8*rxt(k,508)*y(k,282)
         mat(k,2171) = rxt(k,475)*y(k,112) + rxt(k,476)*y(k,114) + rxt(k,477)*y(k,122) &
                      + .400_r8*rxt(k,500)*y(k,131) + .500_r8*rxt(k,508)*y(k,245)
         mat(k,863) = -(rxt(k,545)*y(k,267) + rxt(k,546)*y(k,154))
         mat(k,1829) = -rxt(k,545)*y(k,291)
         mat(k,2273) = -rxt(k,546)*y(k,291)
         mat(k,274) = .560_r8*rxt(k,544)*y(k,282)
         mat(k,818) = rxt(k,547)*y(k,282)
         mat(k,2136) = .560_r8*rxt(k,544)*y(k,246) + rxt(k,547)*y(k,247)
         mat(k,606) = -(rxt(k,548)*y(k,267) + rxt(k,549)*y(k,154))
         mat(k,1813) = -rxt(k,548)*y(k,293)
         mat(k,2258) = -rxt(k,549)*y(k,293)
         mat(k,281) = .300_r8*rxt(k,550)*y(k,282)
         mat(k,523) = rxt(k,551)*y(k,282)
         mat(k,2107) = .300_r8*rxt(k,550)*y(k,248) + rxt(k,551)*y(k,249)
         mat(k,2714) = -(rxt(k,174)*y(k,281) + rxt(k,328)*y(k,89) + rxt(k,595) &
                      *y(k,185))
         mat(k,2021) = -rxt(k,174)*y(k,294)
         mat(k,990) = -rxt(k,328)*y(k,294)
         mat(k,324) = -rxt(k,595)*y(k,294)
         mat(k,297) = rxt(k,335)*y(k,282)
         mat(k,348) = rxt(k,400)*y(k,282)
         mat(k,497) = rxt(k,425)*y(k,282)
         mat(k,136) = rxt(k,426)*y(k,282)
         mat(k,581) = rxt(k,337)*y(k,282)
         mat(k,361) = rxt(k,340)*y(k,282)
         mat(k,1740) = rxt(k,371)*y(k,282)
         mat(k,699) = rxt(k,342)*y(k,282)
         mat(k,156) = rxt(k,343)*y(k,282)
         mat(k,1181) = rxt(k,402)*y(k,282)
         mat(k,456) = rxt(k,345)*y(k,282)
         mat(k,1068) = rxt(k,439)*y(k,282)
         mat(k,1379) = rxt(k,428)*y(k,282)
         mat(k,803) = rxt(k,408)*y(k,282)
         mat(k,706) = rxt(k,409)*y(k,282)
         mat(k,464) = rxt(k,347)*y(k,282)
         mat(k,503) = rxt(k,377)*y(k,282)
         mat(k,1612) = rxt(k,378)*y(k,282)
         mat(k,1134) = rxt(k,354)*y(k,267)
         mat(k,411) = rxt(k,359)*y(k,282)
         mat(k,2508) = rxt(k,186)*y(k,267)
         mat(k,1566) = rxt(k,191)*y(k,282)
         mat(k,713) = rxt(k,192)*y(k,282)
         mat(k,1552) = (rxt(k,603)+rxt(k,665)+rxt(k,678)+rxt(k,687))*y(k,108) + ( &
                      + rxt(k,602)+rxt(k,667)+rxt(k,675)+rxt(k,684))*y(k,109) + ( &
                      + rxt(k,610)+rxt(k,694)+rxt(k,698)+rxt(k,702))*y(k,110) &
                      + rxt(k,280)*y(k,282)
         mat(k,355) = rxt(k,362)*y(k,282)
         mat(k,1787) = (rxt(k,605)+rxt(k,664)+rxt(k,677)+rxt(k,686))*y(k,108) + ( &
                      + rxt(k,604)+rxt(k,663)+rxt(k,674)+rxt(k,683))*y(k,109) + ( &
                      + rxt(k,609)+rxt(k,693)+rxt(k,697)+rxt(k,701))*y(k,110) &
                      + rxt(k,247)*y(k,282)
         mat(k,1075) = rxt(k,380)*y(k,282)
         mat(k,1316) = (rxt(k,607)+rxt(k,666)+rxt(k,679)+rxt(k,688))*y(k,108) + ( &
                      + rxt(k,606)+rxt(k,668)+rxt(k,676)+rxt(k,685))*y(k,109) + ( &
                      + rxt(k,611)+rxt(k,695)+rxt(k,699)+rxt(k,703))*y(k,110) &
                      + rxt(k,315)*y(k,282)
         mat(k,2533) = rxt(k,221)*y(k,282)
         mat(k,485) = rxt(k,199)*y(k,282)
         mat(k,1635) = (rxt(k,603)+rxt(k,665)+rxt(k,678)+rxt(k,687))*y(k,97) + ( &
                      + rxt(k,605)+rxt(k,664)+rxt(k,677)+rxt(k,686))*y(k,101) + ( &
                      + rxt(k,607)+rxt(k,666)+rxt(k,679)+rxt(k,688))*y(k,105)
         mat(k,1764) = (rxt(k,602)+rxt(k,667)+rxt(k,675)+rxt(k,684))*y(k,97) + ( &
                      + rxt(k,604)+rxt(k,663)+rxt(k,674)+rxt(k,683))*y(k,101) + ( &
                      + rxt(k,606)+rxt(k,668)+rxt(k,676)+rxt(k,685))*y(k,105) &
                      + rxt(k,250)*y(k,282)
         mat(k,1711) = (rxt(k,610)+rxt(k,694)+rxt(k,698)+rxt(k,702))*y(k,97) + ( &
                      + rxt(k,609)+rxt(k,693)+rxt(k,697)+rxt(k,701))*y(k,101) + ( &
                      + rxt(k,611)+rxt(k,695)+rxt(k,699)+rxt(k,703))*y(k,105) &
                      + rxt(k,288)*y(k,282)
         mat(k,1370) = .500_r8*rxt(k,452)*y(k,282)
         mat(k,119) = rxt(k,613)*y(k,282)
         mat(k,686) = rxt(k,433)*y(k,282)
         mat(k,491) = rxt(k,437)*y(k,282)
         mat(k,1889) = rxt(k,354)*y(k,68) + rxt(k,186)*y(k,92) + rxt(k,193)*y(k,282)
         mat(k,2208) = rxt(k,335)*y(k,29) + rxt(k,400)*y(k,32) + rxt(k,425)*y(k,34) &
                      + rxt(k,426)*y(k,35) + rxt(k,337)*y(k,45) + rxt(k,340)*y(k,47) &
                      + rxt(k,371)*y(k,51) + rxt(k,342)*y(k,52) + rxt(k,343)*y(k,53) &
                      + rxt(k,402)*y(k,54) + rxt(k,345)*y(k,55) + rxt(k,439)*y(k,57) &
                      + rxt(k,428)*y(k,58) + rxt(k,408)*y(k,59) + rxt(k,409)*y(k,60) &
                      + rxt(k,347)*y(k,61) + rxt(k,377)*y(k,63) + rxt(k,378)*y(k,64) &
                      + rxt(k,359)*y(k,69) + rxt(k,191)*y(k,93) + rxt(k,192)*y(k,95) &
                      + rxt(k,280)*y(k,97) + rxt(k,362)*y(k,100) + rxt(k,247)*y(k,101) &
                      + rxt(k,380)*y(k,103) + rxt(k,315)*y(k,105) + rxt(k,221) &
                      *y(k,106) + rxt(k,199)*y(k,107) + rxt(k,250)*y(k,109) &
                      + rxt(k,288)*y(k,110) + .500_r8*rxt(k,452)*y(k,135) + rxt(k,613) &
                      *y(k,150) + rxt(k,433)*y(k,179) + rxt(k,437)*y(k,180) &
                      + rxt(k,193)*y(k,267) + 2.000_r8*rxt(k,196)*y(k,282)
      end do
      end subroutine nlnmat11
      subroutine nlnmat_finit( avec_len, mat, lmat, dti )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: dti(veclen)
      real(r8), intent(in) :: lmat(veclen,nzcnt)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k, 1) = lmat(k, 1)
         mat(k, 2) = lmat(k, 2)
         mat(k, 3) = lmat(k, 3)
         mat(k, 4) = lmat(k, 4)
         mat(k, 5) = lmat(k, 5)
         mat(k, 6) = lmat(k, 6)
         mat(k, 7) = lmat(k, 7)
         mat(k, 8) = lmat(k, 8)
         mat(k, 9) = lmat(k, 9)
         mat(k, 10) = lmat(k, 10)
         mat(k, 11) = lmat(k, 11)
         mat(k, 12) = lmat(k, 12)
         mat(k, 13) = lmat(k, 13)
         mat(k, 14) = lmat(k, 14)
         mat(k, 15) = lmat(k, 15)
         mat(k, 16) = lmat(k, 16)
         mat(k, 17) = lmat(k, 17)
         mat(k, 18) = lmat(k, 18)
         mat(k, 19) = lmat(k, 19)
         mat(k, 20) = lmat(k, 20)
         mat(k, 21) = lmat(k, 21)
         mat(k, 22) = lmat(k, 22)
         mat(k, 23) = lmat(k, 23)
         mat(k, 24) = lmat(k, 24)
         mat(k, 25) = lmat(k, 25)
         mat(k, 26) = lmat(k, 26)
         mat(k, 27) = lmat(k, 27)
         mat(k, 28) = lmat(k, 28)
         mat(k, 29) = lmat(k, 29)
         mat(k, 30) = lmat(k, 30)
         mat(k, 31) = lmat(k, 31)
         mat(k, 32) = lmat(k, 32)
         mat(k, 33) = lmat(k, 33)
         mat(k, 34) = lmat(k, 34)
         mat(k, 35) = lmat(k, 35)
         mat(k, 36) = lmat(k, 36)
         mat(k, 37) = lmat(k, 37)
         mat(k, 38) = lmat(k, 38)
         mat(k, 39) = lmat(k, 39)
         mat(k, 40) = lmat(k, 40)
         mat(k, 41) = lmat(k, 41)
         mat(k, 42) = lmat(k, 42)
         mat(k, 43) = lmat(k, 43)
         mat(k, 44) = lmat(k, 44)
         mat(k, 45) = lmat(k, 45)
         mat(k, 46) = lmat(k, 46)
         mat(k, 47) = lmat(k, 47)
         mat(k, 48) = lmat(k, 48)
         mat(k, 49) = lmat(k, 49)
         mat(k, 50) = lmat(k, 50)
         mat(k, 51) = lmat(k, 51)
         mat(k, 52) = lmat(k, 52)
         mat(k, 53) = lmat(k, 53)
         mat(k, 54) = lmat(k, 54)
         mat(k, 55) = lmat(k, 55)
         mat(k, 56) = lmat(k, 56)
         mat(k, 57) = lmat(k, 57)
         mat(k, 58) = lmat(k, 58)
         mat(k, 59) = lmat(k, 59)
         mat(k, 60) = lmat(k, 60)
         mat(k, 61) = lmat(k, 61)
         mat(k, 62) = lmat(k, 62)
         mat(k, 63) = lmat(k, 63)
         mat(k, 64) = lmat(k, 64)
         mat(k, 65) = lmat(k, 65)
         mat(k, 66) = lmat(k, 66)
         mat(k, 67) = lmat(k, 67)
         mat(k, 68) = lmat(k, 68)
         mat(k, 69) = lmat(k, 69)
         mat(k, 70) = lmat(k, 70)
         mat(k, 71) = lmat(k, 71)
         mat(k, 72) = lmat(k, 72)
         mat(k, 78) = mat(k, 78) + lmat(k, 78)
         mat(k, 79) = lmat(k, 79)
         mat(k, 80) = lmat(k, 80)
         mat(k, 81) = lmat(k, 81)
         mat(k, 82) = lmat(k, 82)
         mat(k, 83) = lmat(k, 83)
         mat(k, 84) = lmat(k, 84)
         mat(k, 85) = lmat(k, 85)
         mat(k, 86) = lmat(k, 86)
         mat(k, 87) = lmat(k, 87)
         mat(k, 88) = lmat(k, 88)
         mat(k, 94) = mat(k, 94) + lmat(k, 94)
         mat(k, 100) = mat(k, 100) + lmat(k, 100)
         mat(k, 106) = mat(k, 106) + lmat(k, 106)
         mat(k, 107) = lmat(k, 107)
         mat(k, 108) = lmat(k, 108)
         mat(k, 109) = lmat(k, 109)
         mat(k, 110) = lmat(k, 110)
         mat(k, 111) = lmat(k, 111)
         mat(k, 112) = lmat(k, 112)
         mat(k, 113) = lmat(k, 113)
         mat(k, 114) = lmat(k, 114)
         mat(k, 115) = lmat(k, 115)
         mat(k, 116) = lmat(k, 116)
         mat(k, 117) = mat(k, 117) + lmat(k, 117)
         mat(k, 120) = mat(k, 120) + lmat(k, 120)
         mat(k, 121) = mat(k, 121) + lmat(k, 121)
         mat(k, 123) = mat(k, 123) + lmat(k, 123)
         mat(k, 126) = lmat(k, 126)
         mat(k, 127) = lmat(k, 127)
         mat(k, 128) = lmat(k, 128)
         mat(k, 129) = lmat(k, 129)
         mat(k, 130) = lmat(k, 130)
         mat(k, 131) = lmat(k, 131)
         mat(k, 132) = lmat(k, 132)
         mat(k, 133) = mat(k, 133) + lmat(k, 133)
         mat(k, 137) = mat(k, 137) + lmat(k, 137)
         mat(k, 138) = mat(k, 138) + lmat(k, 138)
         mat(k, 139) = mat(k, 139) + lmat(k, 139)
         mat(k, 141) = mat(k, 141) + lmat(k, 141)
         mat(k, 142) = mat(k, 142) + lmat(k, 142)
         mat(k, 143) = mat(k, 143) + lmat(k, 143)
         mat(k, 145) = mat(k, 145) + lmat(k, 145)
         mat(k, 146) = mat(k, 146) + lmat(k, 146)
         mat(k, 147) = mat(k, 147) + lmat(k, 147)
         mat(k, 149) = mat(k, 149) + lmat(k, 149)
         mat(k, 150) = mat(k, 150) + lmat(k, 150)
         mat(k, 152) = mat(k, 152) + lmat(k, 152)
         mat(k, 153) = mat(k, 153) + lmat(k, 153)
         mat(k, 154) = mat(k, 154) + lmat(k, 154)
         mat(k, 157) = mat(k, 157) + lmat(k, 157)
         mat(k, 158) = mat(k, 158) + lmat(k, 158)
         mat(k, 160) = mat(k, 160) + lmat(k, 160)
         mat(k, 161) = mat(k, 161) + lmat(k, 161)
         mat(k, 162) = mat(k, 162) + lmat(k, 162)
         mat(k, 164) = mat(k, 164) + lmat(k, 164)
         mat(k, 165) = mat(k, 165) + lmat(k, 165)
         mat(k, 166) = mat(k, 166) + lmat(k, 166)
         mat(k, 168) = mat(k, 168) + lmat(k, 168)
         mat(k, 169) = lmat(k, 169)
         mat(k, 170) = lmat(k, 170)
         mat(k, 171) = lmat(k, 171)
         mat(k, 172) = mat(k, 172) + lmat(k, 172)
         mat(k, 175) = lmat(k, 175)
         mat(k, 176) = lmat(k, 176)
         mat(k, 177) = lmat(k, 177)
         mat(k, 178) = lmat(k, 178)
         mat(k, 179) = lmat(k, 179)
         mat(k, 180) = lmat(k, 180)
         mat(k, 181) = mat(k, 181) + lmat(k, 181)
         mat(k, 182) = mat(k, 182) + lmat(k, 182)
         mat(k, 183) = mat(k, 183) + lmat(k, 183)
         mat(k, 185) = mat(k, 185) + lmat(k, 185)
         mat(k, 186) = mat(k, 186) + lmat(k, 186)
         mat(k, 187) = mat(k, 187) + lmat(k, 187)
         mat(k, 188) = mat(k, 188) + lmat(k, 188)
         mat(k, 190) = mat(k, 190) + lmat(k, 190)
         mat(k, 191) = mat(k, 191) + lmat(k, 191)
         mat(k, 192) = mat(k, 192) + lmat(k, 192)
         mat(k, 193) = mat(k, 193) + lmat(k, 193)
         mat(k, 194) = mat(k, 194) + lmat(k, 194)
         mat(k, 196) = mat(k, 196) + lmat(k, 196)
         mat(k, 197) = mat(k, 197) + lmat(k, 197)
         mat(k, 198) = mat(k, 198) + lmat(k, 198)
         mat(k, 199) = mat(k, 199) + lmat(k, 199)
         mat(k, 201) = lmat(k, 201)
         mat(k, 202) = lmat(k, 202)
         mat(k, 203) = lmat(k, 203)
         mat(k, 209) = mat(k, 209) + lmat(k, 209)
         mat(k, 215) = lmat(k, 215)
         mat(k, 216) = lmat(k, 216)
         mat(k, 217) = lmat(k, 217)
         mat(k, 218) = lmat(k, 218)
         mat(k, 219) = mat(k, 219) + lmat(k, 219)
         mat(k, 224) = mat(k, 224) + lmat(k, 224)
         mat(k, 226) = mat(k, 226) + lmat(k, 226)
         mat(k, 227) = lmat(k, 227)
         mat(k, 228) = mat(k, 228) + lmat(k, 228)
         mat(k, 229) = mat(k, 229) + lmat(k, 229)
         mat(k, 231) = mat(k, 231) + lmat(k, 231)
         mat(k, 233) = mat(k, 233) + lmat(k, 233)
         mat(k, 235) = mat(k, 235) + lmat(k, 235)
         mat(k, 239) = lmat(k, 239)
         mat(k, 240) = lmat(k, 240)
         mat(k, 246) = mat(k, 246) + lmat(k, 246)
         mat(k, 253) = mat(k, 253) + lmat(k, 253)
         mat(k, 258) = lmat(k, 258)
         mat(k, 259) = lmat(k, 259)
         mat(k, 260) = lmat(k, 260)
         mat(k, 261) = lmat(k, 261)
         mat(k, 262) = mat(k, 262) + lmat(k, 262)
         mat(k, 264) = mat(k, 264) + lmat(k, 264)
         mat(k, 271) = mat(k, 271) + lmat(k, 271)
         mat(k, 279) = mat(k, 279) + lmat(k, 279)
         mat(k, 284) = mat(k, 284) + lmat(k, 284)
         mat(k, 287) = mat(k, 287) + lmat(k, 287)
         mat(k, 290) = lmat(k, 290)
         mat(k, 291) = lmat(k, 291)
         mat(k, 292) = lmat(k, 292)
         mat(k, 293) = mat(k, 293) + lmat(k, 293)
         mat(k, 295) = mat(k, 295) + lmat(k, 295)
         mat(k, 298) = mat(k, 298) + lmat(k, 298)
         mat(k, 299) = mat(k, 299) + lmat(k, 299)
         mat(k, 300) = mat(k, 300) + lmat(k, 300)
         mat(k, 303) = mat(k, 303) + lmat(k, 303)
         mat(k, 304) = mat(k, 304) + lmat(k, 304)
         mat(k, 305) = mat(k, 305) + lmat(k, 305)
         mat(k, 307) = mat(k, 307) + lmat(k, 307)
         mat(k, 308) = lmat(k, 308)
         mat(k, 310) = lmat(k, 310)
         mat(k, 311) = mat(k, 311) + lmat(k, 311)
         mat(k, 312) = mat(k, 312) + lmat(k, 312)
         mat(k, 313) = mat(k, 313) + lmat(k, 313)
         mat(k, 314) = mat(k, 314) + lmat(k, 314)
         mat(k, 317) = mat(k, 317) + lmat(k, 317)
         mat(k, 318) = mat(k, 318) + lmat(k, 318)
         mat(k, 321) = mat(k, 321) + lmat(k, 321)
         mat(k, 322) = lmat(k, 322)
         mat(k, 323) = lmat(k, 323)
         mat(k, 325) = mat(k, 325) + lmat(k, 325)
         mat(k, 329) = mat(k, 329) + lmat(k, 329)
         mat(k, 330) = lmat(k, 330)
         mat(k, 332) = mat(k, 332) + lmat(k, 332)
         mat(k, 333) = lmat(k, 333)
         mat(k, 334) = lmat(k, 334)
         mat(k, 335) = lmat(k, 335)
         mat(k, 336) = lmat(k, 336)
         mat(k, 337) = mat(k, 337) + lmat(k, 337)
         mat(k, 343) = mat(k, 343) + lmat(k, 343)
         mat(k, 349) = mat(k, 349) + lmat(k, 349)
         mat(k, 350) = mat(k, 350) + lmat(k, 350)
         mat(k, 352) = mat(k, 352) + lmat(k, 352)
         mat(k, 356) = mat(k, 356) + lmat(k, 356)
         mat(k, 359) = mat(k, 359) + lmat(k, 359)
         mat(k, 362) = lmat(k, 362)
         mat(k, 363) = lmat(k, 363)
         mat(k, 364) = lmat(k, 364)
         mat(k, 365) = mat(k, 365) + lmat(k, 365)
         mat(k, 366) = lmat(k, 366)
         mat(k, 367) = lmat(k, 367)
         mat(k, 369) = lmat(k, 369)
         mat(k, 370) = mat(k, 370) + lmat(k, 370)
         mat(k, 371) = lmat(k, 371)
         mat(k, 372) = lmat(k, 372)
         mat(k, 373) = lmat(k, 373)
         mat(k, 374) = lmat(k, 374)
         mat(k, 375) = lmat(k, 375)
         mat(k, 376) = lmat(k, 376)
         mat(k, 377) = lmat(k, 377)
         mat(k, 378) = lmat(k, 378)
         mat(k, 379) = lmat(k, 379)
         mat(k, 380) = lmat(k, 380)
         mat(k, 381) = mat(k, 381) + lmat(k, 381)
         mat(k, 383) = lmat(k, 383)
         mat(k, 384) = lmat(k, 384)
         mat(k, 385) = mat(k, 385) + lmat(k, 385)
         mat(k, 386) = lmat(k, 386)
         mat(k, 387) = mat(k, 387) + lmat(k, 387)
         mat(k, 390) = lmat(k, 390)
         mat(k, 391) = mat(k, 391) + lmat(k, 391)
         mat(k, 392) = mat(k, 392) + lmat(k, 392)
         mat(k, 394) = lmat(k, 394)
         mat(k, 395) = lmat(k, 395)
         mat(k, 396) = mat(k, 396) + lmat(k, 396)
         mat(k, 397) = mat(k, 397) + lmat(k, 397)
         mat(k, 399) = mat(k, 399) + lmat(k, 399)
         mat(k, 400) = lmat(k, 400)
         mat(k, 401) = mat(k, 401) + lmat(k, 401)
         mat(k, 402) = mat(k, 402) + lmat(k, 402)
         mat(k, 405) = mat(k, 405) + lmat(k, 405)
         mat(k, 407) = lmat(k, 407)
         mat(k, 409) = mat(k, 409) + lmat(k, 409)
         mat(k, 412) = lmat(k, 412)
         mat(k, 413) = lmat(k, 413)
         mat(k, 414) = lmat(k, 414)
         mat(k, 415) = mat(k, 415) + lmat(k, 415)
         mat(k, 420) = lmat(k, 420)
         mat(k, 421) = lmat(k, 421)
         mat(k, 422) = lmat(k, 422)
         mat(k, 423) = lmat(k, 423)
         mat(k, 424) = lmat(k, 424)
         mat(k, 425) = mat(k, 425) + lmat(k, 425)
         mat(k, 426) = lmat(k, 426)
         mat(k, 428) = mat(k, 428) + lmat(k, 428)
         mat(k, 433) = mat(k, 433) + lmat(k, 433)
         mat(k, 441) = lmat(k, 441)
         mat(k, 442) = mat(k, 442) + lmat(k, 442)
         mat(k, 444) = lmat(k, 444)
         mat(k, 445) = lmat(k, 445)
         mat(k, 446) = lmat(k, 446)
         mat(k, 447) = mat(k, 447) + lmat(k, 447)
         mat(k, 448) = lmat(k, 448)
         mat(k, 449) = mat(k, 449) + lmat(k, 449)
         mat(k, 451) = lmat(k, 451)
         mat(k, 454) = mat(k, 454) + lmat(k, 454)
         mat(k, 457) = mat(k, 457) + lmat(k, 457)
         mat(k, 458) = lmat(k, 458)
         mat(k, 463) = mat(k, 463) + lmat(k, 463)
         mat(k, 465) = lmat(k, 465)
         mat(k, 466) = lmat(k, 466)
         mat(k, 467) = lmat(k, 467)
         mat(k, 468) = mat(k, 468) + lmat(k, 468)
         mat(k, 469) = lmat(k, 469)
         mat(k, 471) = mat(k, 471) + lmat(k, 471)
         mat(k, 472) = lmat(k, 472)
         mat(k, 473) = lmat(k, 473)
         mat(k, 474) = mat(k, 474) + lmat(k, 474)
         mat(k, 475) = lmat(k, 475)
         mat(k, 477) = mat(k, 477) + lmat(k, 477)
         mat(k, 479) = lmat(k, 479)
         mat(k, 480) = mat(k, 480) + lmat(k, 480)
         mat(k, 481) = lmat(k, 481)
         mat(k, 482) = mat(k, 482) + lmat(k, 482)
         mat(k, 483) = lmat(k, 483)
         mat(k, 484) = mat(k, 484) + lmat(k, 484)
         mat(k, 486) = mat(k, 486) + lmat(k, 486)
         mat(k, 488) = lmat(k, 488)
         mat(k, 489) = lmat(k, 489)
         mat(k, 490) = mat(k, 490) + lmat(k, 490)
         mat(k, 492) = mat(k, 492) + lmat(k, 492)
         mat(k, 494) = lmat(k, 494)
         mat(k, 495) = lmat(k, 495)
         mat(k, 496) = mat(k, 496) + lmat(k, 496)
         mat(k, 498) = mat(k, 498) + lmat(k, 498)
         mat(k, 500) = mat(k, 500) + lmat(k, 500)
         mat(k, 501) = mat(k, 501) + lmat(k, 501)
         mat(k, 502) = lmat(k, 502)
         mat(k, 504) = mat(k, 504) + lmat(k, 504)
         mat(k, 509) = mat(k, 509) + lmat(k, 509)
         mat(k, 510) = mat(k, 510) + lmat(k, 510)
         mat(k, 515) = lmat(k, 515)
         mat(k, 516) = mat(k, 516) + lmat(k, 516)
         mat(k, 517) = lmat(k, 517)
         mat(k, 519) = lmat(k, 519)
         mat(k, 520) = lmat(k, 520)
         mat(k, 521) = mat(k, 521) + lmat(k, 521)
         mat(k, 522) = mat(k, 522) + lmat(k, 522)
         mat(k, 524) = lmat(k, 524)
         mat(k, 525) = lmat(k, 525)
         mat(k, 526) = lmat(k, 526)
         mat(k, 527) = mat(k, 527) + lmat(k, 527)
         mat(k, 530) = mat(k, 530) + lmat(k, 530)
         mat(k, 536) = mat(k, 536) + lmat(k, 536)
         mat(k, 538) = lmat(k, 538)
         mat(k, 539) = mat(k, 539) + lmat(k, 539)
         mat(k, 542) = lmat(k, 542)
         mat(k, 543) = lmat(k, 543)
         mat(k, 544) = lmat(k, 544)
         mat(k, 545) = lmat(k, 545)
         mat(k, 546) = lmat(k, 546)
         mat(k, 547) = lmat(k, 547)
         mat(k, 549) = mat(k, 549) + lmat(k, 549)
         mat(k, 555) = mat(k, 555) + lmat(k, 555)
         mat(k, 558) = lmat(k, 558)
         mat(k, 559) = mat(k, 559) + lmat(k, 559)
         mat(k, 560) = lmat(k, 560)
         mat(k, 561) = lmat(k, 561)
         mat(k, 562) = lmat(k, 562)
         mat(k, 563) = mat(k, 563) + lmat(k, 563)
         mat(k, 566) = mat(k, 566) + lmat(k, 566)
         mat(k, 568) = mat(k, 568) + lmat(k, 568)
         mat(k, 571) = mat(k, 571) + lmat(k, 571)
         mat(k, 572) = mat(k, 572) + lmat(k, 572)
         mat(k, 575) = mat(k, 575) + lmat(k, 575)
         mat(k, 580) = mat(k, 580) + lmat(k, 580)
         mat(k, 582) = lmat(k, 582)
         mat(k, 583) = lmat(k, 583)
         mat(k, 584) = lmat(k, 584)
         mat(k, 585) = mat(k, 585) + lmat(k, 585)
         mat(k, 589) = mat(k, 589) + lmat(k, 589)
         mat(k, 590) = lmat(k, 590)
         mat(k, 591) = lmat(k, 591)
         mat(k, 593) = lmat(k, 593)
         mat(k, 594) = lmat(k, 594)
         mat(k, 595) = mat(k, 595) + lmat(k, 595)
         mat(k, 598) = mat(k, 598) + lmat(k, 598)
         mat(k, 606) = mat(k, 606) + lmat(k, 606)
         mat(k, 613) = mat(k, 613) + lmat(k, 613)
         mat(k, 614) = mat(k, 614) + lmat(k, 614)
         mat(k, 616) = lmat(k, 616)
         mat(k, 621) = mat(k, 621) + lmat(k, 621)
         mat(k, 624) = lmat(k, 624)
         mat(k, 626) = lmat(k, 626)
         mat(k, 627) = lmat(k, 627)
         mat(k, 628) = lmat(k, 628)
         mat(k, 629) = lmat(k, 629)
         mat(k, 630) = mat(k, 630) + lmat(k, 630)
         mat(k, 632) = lmat(k, 632)
         mat(k, 634) = mat(k, 634) + lmat(k, 634)
         mat(k, 635) = mat(k, 635) + lmat(k, 635)
         mat(k, 637) = lmat(k, 637)
         mat(k, 638) = lmat(k, 638)
         mat(k, 641) = mat(k, 641) + lmat(k, 641)
         mat(k, 647) = mat(k, 647) + lmat(k, 647)
         mat(k, 654) = lmat(k, 654)
         mat(k, 655) = mat(k, 655) + lmat(k, 655)
         mat(k, 656) = lmat(k, 656)
         mat(k, 657) = lmat(k, 657)
         mat(k, 658) = lmat(k, 658)
         mat(k, 661) = mat(k, 661) + lmat(k, 661)
         mat(k, 662) = lmat(k, 662)
         mat(k, 663) = mat(k, 663) + lmat(k, 663)
         mat(k, 671) = mat(k, 671) + lmat(k, 671)
         mat(k, 679) = mat(k, 679) + lmat(k, 679)
         mat(k, 681) = lmat(k, 681)
         mat(k, 683) = lmat(k, 683)
         mat(k, 684) = lmat(k, 684)
         mat(k, 685) = mat(k, 685) + lmat(k, 685)
         mat(k, 687) = lmat(k, 687)
         mat(k, 688) = lmat(k, 688)
         mat(k, 689) = lmat(k, 689)
         mat(k, 690) = lmat(k, 690)
         mat(k, 691) = mat(k, 691) + lmat(k, 691)
         mat(k, 692) = lmat(k, 692)
         mat(k, 698) = mat(k, 698) + lmat(k, 698)
         mat(k, 700) = mat(k, 700) + lmat(k, 700)
         mat(k, 701) = mat(k, 701) + lmat(k, 701)
         mat(k, 703) = lmat(k, 703)
         mat(k, 705) = mat(k, 705) + lmat(k, 705)
         mat(k, 707) = mat(k, 707) + lmat(k, 707)
         mat(k, 711) = mat(k, 711) + lmat(k, 711)
         mat(k, 714) = mat(k, 714) + lmat(k, 714)
         mat(k, 717) = lmat(k, 717)
         mat(k, 722) = lmat(k, 722)
         mat(k, 723) = mat(k, 723) + lmat(k, 723)
         mat(k, 730) = lmat(k, 730)
         mat(k, 732) = mat(k, 732) + lmat(k, 732)
         mat(k, 733) = mat(k, 733) + lmat(k, 733)
         mat(k, 734) = lmat(k, 734)
         mat(k, 736) = lmat(k, 736)
         mat(k, 737) = mat(k, 737) + lmat(k, 737)
         mat(k, 740) = mat(k, 740) + lmat(k, 740)
         mat(k, 746) = lmat(k, 746)
         mat(k, 747) = mat(k, 747) + lmat(k, 747)
         mat(k, 751) = lmat(k, 751)
         mat(k, 752) = lmat(k, 752)
         mat(k, 754) = lmat(k, 754)
         mat(k, 755) = lmat(k, 755)
         mat(k, 756) = mat(k, 756) + lmat(k, 756)
         mat(k, 757) = mat(k, 757) + lmat(k, 757)
         mat(k, 758) = lmat(k, 758)
         mat(k, 759) = lmat(k, 759)
         mat(k, 760) = lmat(k, 760)
         mat(k, 761) = lmat(k, 761)
         mat(k, 762) = lmat(k, 762)
         mat(k, 764) = lmat(k, 764)
         mat(k, 765) = lmat(k, 765)
         mat(k, 766) = mat(k, 766) + lmat(k, 766)
         mat(k, 767) = lmat(k, 767)
         mat(k, 768) = lmat(k, 768)
         mat(k, 769) = lmat(k, 769)
         mat(k, 770) = mat(k, 770) + lmat(k, 770)
         mat(k, 775) = lmat(k, 775)
         mat(k, 777) = lmat(k, 777)
         mat(k, 778) = lmat(k, 778)
         mat(k, 779) = mat(k, 779) + lmat(k, 779)
         mat(k, 780) = lmat(k, 780)
         mat(k, 781) = mat(k, 781) + lmat(k, 781)
         mat(k, 784) = mat(k, 784) + lmat(k, 784)
         mat(k, 785) = mat(k, 785) + lmat(k, 785)
         mat(k, 787) = mat(k, 787) + lmat(k, 787)
         mat(k, 788) = lmat(k, 788)
         mat(k, 790) = mat(k, 790) + lmat(k, 790)
         mat(k, 792) = mat(k, 792) + lmat(k, 792)
         mat(k, 799) = mat(k, 799) + lmat(k, 799)
         mat(k, 805) = mat(k, 805) + lmat(k, 805)
         mat(k, 807) = lmat(k, 807)
         mat(k, 808) = lmat(k, 808)
         mat(k, 809) = lmat(k, 809)
         mat(k, 810) = lmat(k, 810)
         mat(k, 811) = mat(k, 811) + lmat(k, 811)
         mat(k, 812) = lmat(k, 812)
         mat(k, 813) = lmat(k, 813)
         mat(k, 814) = lmat(k, 814)
         mat(k, 815) = lmat(k, 815)
         mat(k, 816) = mat(k, 816) + lmat(k, 816)
         mat(k, 821) = lmat(k, 821)
         mat(k, 823) = lmat(k, 823)
         mat(k, 825) = lmat(k, 825)
         mat(k, 826) = mat(k, 826) + lmat(k, 826)
         mat(k, 829) = mat(k, 829) + lmat(k, 829)
         mat(k, 836) = mat(k, 836) + lmat(k, 836)
         mat(k, 847) = mat(k, 847) + lmat(k, 847)
         mat(k, 863) = mat(k, 863) + lmat(k, 863)
         mat(k, 874) = mat(k, 874) + lmat(k, 874)
         mat(k, 883) = mat(k, 883) + lmat(k, 883)
         mat(k, 893) = mat(k, 893) + lmat(k, 893)
         mat(k, 903) = mat(k, 903) + lmat(k, 903)
         mat(k, 907) = lmat(k, 907)
         mat(k, 908) = lmat(k, 908)
         mat(k, 909) = lmat(k, 909)
         mat(k, 913) = mat(k, 913) + lmat(k, 913)
         mat(k, 921) = lmat(k, 921)
         mat(k, 922) = lmat(k, 922)
         mat(k, 923) = mat(k, 923) + lmat(k, 923)
         mat(k, 925) = lmat(k, 925)
         mat(k, 926) = lmat(k, 926)
         mat(k, 929) = mat(k, 929) + lmat(k, 929)
         mat(k, 930) = mat(k, 930) + lmat(k, 930)
         mat(k, 931) = lmat(k, 931)
         mat(k, 932) = lmat(k, 932)
         mat(k, 938) = mat(k, 938) + lmat(k, 938)
         mat(k, 954) = mat(k, 954) + lmat(k, 954)
         mat(k, 955) = mat(k, 955) + lmat(k, 955)
         mat(k, 956) = mat(k, 956) + lmat(k, 956)
         mat(k, 957) = lmat(k, 957)
         mat(k, 961) = mat(k, 961) + lmat(k, 961)
         mat(k, 962) = mat(k, 962) + lmat(k, 962)
         mat(k, 963) = mat(k, 963) + lmat(k, 963)
         mat(k, 964) = lmat(k, 964)
         mat(k, 965) = lmat(k, 965)
         mat(k, 966) = mat(k, 966) + lmat(k, 966)
         mat(k, 968) = lmat(k, 968)
         mat(k, 969) = lmat(k, 969)
         mat(k, 971) = mat(k, 971) + lmat(k, 971)
         mat(k, 972) = mat(k, 972) + lmat(k, 972)
         mat(k, 982) = mat(k, 982) + lmat(k, 982)
         mat(k, 999) = mat(k, 999) + lmat(k, 999)
         mat(k,1023) = mat(k,1023) + lmat(k,1023)
         mat(k,1033) = mat(k,1033) + lmat(k,1033)
         mat(k,1034) = mat(k,1034) + lmat(k,1034)
         mat(k,1035) = mat(k,1035) + lmat(k,1035)
         mat(k,1037) = mat(k,1037) + lmat(k,1037)
         mat(k,1040) = mat(k,1040) + lmat(k,1040)
         mat(k,1041) = mat(k,1041) + lmat(k,1041)
         mat(k,1042) = lmat(k,1042)
         mat(k,1043) = mat(k,1043) + lmat(k,1043)
         mat(k,1044) = mat(k,1044) + lmat(k,1044)
         mat(k,1057) = mat(k,1057) + lmat(k,1057)
         mat(k,1058) = lmat(k,1058)
         mat(k,1061) = lmat(k,1061)
         mat(k,1063) = mat(k,1063) + lmat(k,1063)
         mat(k,1065) = lmat(k,1065)
         mat(k,1066) = lmat(k,1066)
         mat(k,1070) = mat(k,1070) + lmat(k,1070)
         mat(k,1076) = lmat(k,1076)
         mat(k,1078) = mat(k,1078) + lmat(k,1078)
         mat(k,1079) = mat(k,1079) + lmat(k,1079)
         mat(k,1080) = mat(k,1080) + lmat(k,1080)
         mat(k,1095) = mat(k,1095) + lmat(k,1095)
         mat(k,1115) = mat(k,1115) + lmat(k,1115)
         mat(k,1117) = lmat(k,1117)
         mat(k,1118) = lmat(k,1118)
         mat(k,1120) = lmat(k,1120)
         mat(k,1124) = mat(k,1124) + lmat(k,1124)
         mat(k,1140) = mat(k,1140) + lmat(k,1140)
         mat(k,1157) = lmat(k,1157)
         mat(k,1161) = mat(k,1161) + lmat(k,1161)
         mat(k,1167) = mat(k,1167) + lmat(k,1167)
         mat(k,1171) = lmat(k,1171)
         mat(k,1172) = lmat(k,1172)
         mat(k,1173) = mat(k,1173) + lmat(k,1173)
         mat(k,1174) = lmat(k,1174)
         mat(k,1176) = lmat(k,1176)
         mat(k,1177) = lmat(k,1177)
         mat(k,1184) = mat(k,1184) + lmat(k,1184)
         mat(k,1185) = lmat(k,1185)
         mat(k,1186) = mat(k,1186) + lmat(k,1186)
         mat(k,1187) = mat(k,1187) + lmat(k,1187)
         mat(k,1194) = mat(k,1194) + lmat(k,1194)
         mat(k,1206) = mat(k,1206) + lmat(k,1206)
         mat(k,1207) = mat(k,1207) + lmat(k,1207)
         mat(k,1208) = mat(k,1208) + lmat(k,1208)
         mat(k,1209) = mat(k,1209) + lmat(k,1209)
         mat(k,1210) = mat(k,1210) + lmat(k,1210)
         mat(k,1211) = mat(k,1211) + lmat(k,1211)
         mat(k,1213) = mat(k,1213) + lmat(k,1213)
         mat(k,1214) = mat(k,1214) + lmat(k,1214)
         mat(k,1219) = mat(k,1219) + lmat(k,1219)
         mat(k,1224) = lmat(k,1224)
         mat(k,1225) = lmat(k,1225)
         mat(k,1226) = lmat(k,1226)
         mat(k,1227) = lmat(k,1227)
         mat(k,1228) = mat(k,1228) + lmat(k,1228)
         mat(k,1229) = lmat(k,1229)
         mat(k,1231) = lmat(k,1231)
         mat(k,1233) = lmat(k,1233)
         mat(k,1234) = mat(k,1234) + lmat(k,1234)
         mat(k,1238) = lmat(k,1238)
         mat(k,1239) = lmat(k,1239)
         mat(k,1241) = mat(k,1241) + lmat(k,1241)
         mat(k,1243) = lmat(k,1243)
         mat(k,1244) = lmat(k,1244)
         mat(k,1245) = mat(k,1245) + lmat(k,1245)
         mat(k,1255) = mat(k,1255) + lmat(k,1255)
         mat(k,1276) = mat(k,1276) + lmat(k,1276)
         mat(k,1292) = mat(k,1292) + lmat(k,1292)
         mat(k,1304) = mat(k,1304) + lmat(k,1304)
         mat(k,1313) = mat(k,1313) + lmat(k,1313)
         mat(k,1314) = lmat(k,1314)
         mat(k,1324) = mat(k,1324) + lmat(k,1324)
         mat(k,1344) = mat(k,1344) + lmat(k,1344)
         mat(k,1359) = mat(k,1359) + lmat(k,1359)
         mat(k,1360) = mat(k,1360) + lmat(k,1360)
         mat(k,1363) = mat(k,1363) + lmat(k,1363)
         mat(k,1364) = mat(k,1364) + lmat(k,1364)
         mat(k,1365) = mat(k,1365) + lmat(k,1365)
         mat(k,1366) = mat(k,1366) + lmat(k,1366)
         mat(k,1371) = mat(k,1371) + lmat(k,1371)
         mat(k,1372) = mat(k,1372) + lmat(k,1372)
         mat(k,1373) = mat(k,1373) + lmat(k,1373)
         mat(k,1374) = lmat(k,1374)
         mat(k,1392) = mat(k,1392) + lmat(k,1392)
         mat(k,1408) = lmat(k,1408)
         mat(k,1425) = mat(k,1425) + lmat(k,1425)
         mat(k,1432) = mat(k,1432) + lmat(k,1432)
         mat(k,1449) = mat(k,1449) + lmat(k,1449)
         mat(k,1464) = lmat(k,1464)
         mat(k,1466) = mat(k,1466) + lmat(k,1466)
         mat(k,1470) = mat(k,1470) + lmat(k,1470)
         mat(k,1472) = mat(k,1472) + lmat(k,1472)
         mat(k,1474) = lmat(k,1474)
         mat(k,1492) = mat(k,1492) + lmat(k,1492)
         mat(k,1524) = mat(k,1524) + lmat(k,1524)
         mat(k,1540) = mat(k,1540) + lmat(k,1540)
         mat(k,1548) = mat(k,1548) + lmat(k,1548)
         mat(k,1551) = mat(k,1551) + lmat(k,1551)
         mat(k,1555) = mat(k,1555) + lmat(k,1555)
         mat(k,1568) = mat(k,1568) + lmat(k,1568)
         mat(k,1576) = mat(k,1576) + lmat(k,1576)
         mat(k,1580) = lmat(k,1580)
         mat(k,1581) = mat(k,1581) + lmat(k,1581)
         mat(k,1582) = mat(k,1582) + lmat(k,1582)
         mat(k,1586) = lmat(k,1586)
         mat(k,1596) = lmat(k,1596)
         mat(k,1598) = lmat(k,1598)
         mat(k,1599) = mat(k,1599) + lmat(k,1599)
         mat(k,1600) = mat(k,1600) + lmat(k,1600)
         mat(k,1601) = mat(k,1601) + lmat(k,1601)
         mat(k,1602) = mat(k,1602) + lmat(k,1602)
         mat(k,1607) = mat(k,1607) + lmat(k,1607)
         mat(k,1608) = lmat(k,1608)
         mat(k,1610) = mat(k,1610) + lmat(k,1610)
         mat(k,1612) = mat(k,1612) + lmat(k,1612)
         mat(k,1614) = mat(k,1614) + lmat(k,1614)
         mat(k,1615) = mat(k,1615) + lmat(k,1615)
         mat(k,1619) = mat(k,1619) + lmat(k,1619)
         mat(k,1626) = mat(k,1626) + lmat(k,1626)
         mat(k,1628) = lmat(k,1628)
         mat(k,1673) = mat(k,1673) + lmat(k,1673)
         mat(k,1690) = mat(k,1690) + lmat(k,1690)
         mat(k,1691) = mat(k,1691) + lmat(k,1691)
         mat(k,1696) = mat(k,1696) + lmat(k,1696)
         mat(k,1702) = mat(k,1702) + lmat(k,1702)
         mat(k,1707) = lmat(k,1707)
         mat(k,1714) = mat(k,1714) + lmat(k,1714)
         mat(k,1716) = lmat(k,1716)
         mat(k,1721) = mat(k,1721) + lmat(k,1721)
         mat(k,1735) = mat(k,1735) + lmat(k,1735)
         mat(k,1741) = mat(k,1741) + lmat(k,1741)
         mat(k,1749) = mat(k,1749) + lmat(k,1749)
         mat(k,1752) = mat(k,1752) + lmat(k,1752)
         mat(k,1754) = mat(k,1754) + lmat(k,1754)
         mat(k,1773) = mat(k,1773) + lmat(k,1773)
         mat(k,1775) = mat(k,1775) + lmat(k,1775)
         mat(k,1783) = mat(k,1783) + lmat(k,1783)
         mat(k,1873) = mat(k,1873) + lmat(k,1873)
         mat(k,1889) = mat(k,1889) + lmat(k,1889)
         mat(k,1904) = mat(k,1904) + lmat(k,1904)
         mat(k,1908) = mat(k,1908) + lmat(k,1908)
         mat(k,1913) = mat(k,1913) + lmat(k,1913)
         mat(k,1961) = mat(k,1961) + lmat(k,1961)
         mat(k,2008) = mat(k,2008) + lmat(k,2008)
         mat(k,2010) = mat(k,2010) + lmat(k,2010)
         mat(k,2196) = mat(k,2196) + lmat(k,2196)
         mat(k,2232) = mat(k,2232) + lmat(k,2232)
         mat(k,2242) = mat(k,2242) + lmat(k,2242)
         mat(k,2255) = mat(k,2255) + lmat(k,2255)
         mat(k,2320) = mat(k,2320) + lmat(k,2320)
         mat(k,2321) = mat(k,2321) + lmat(k,2321)
         mat(k,2350) = mat(k,2350) + lmat(k,2350)
         mat(k,2376) = mat(k,2376) + lmat(k,2376)
         mat(k,2378) = mat(k,2378) + lmat(k,2378)
         mat(k,2379) = mat(k,2379) + lmat(k,2379)
         mat(k,2446) = mat(k,2446) + lmat(k,2446)
         mat(k,2447) = mat(k,2447) + lmat(k,2447)
         mat(k,2450) = mat(k,2450) + lmat(k,2450)
         mat(k,2453) = mat(k,2453) + lmat(k,2453)
         mat(k,2454) = mat(k,2454) + lmat(k,2454)
         mat(k,2479) = mat(k,2479) + lmat(k,2479)
         mat(k,2492) = mat(k,2492) + lmat(k,2492)
         mat(k,2503) = mat(k,2503) + lmat(k,2503)
         mat(k,2516) = lmat(k,2516)
         mat(k,2521) = mat(k,2521) + lmat(k,2521)
         mat(k,2529) = mat(k,2529) + lmat(k,2529)
         mat(k,2530) = lmat(k,2530)
         mat(k,2578) = mat(k,2578) + lmat(k,2578)
         mat(k,2579) = mat(k,2579) + lmat(k,2579)
         mat(k,2580) = mat(k,2580) + lmat(k,2580)
         mat(k,2586) = mat(k,2586) + lmat(k,2586)
         mat(k,2587) = mat(k,2587) + lmat(k,2587)
         mat(k,2605) = mat(k,2605) + lmat(k,2605)
         mat(k,2608) = mat(k,2608) + lmat(k,2608)
         mat(k,2617) = mat(k,2617) + lmat(k,2617)
         mat(k,2672) = mat(k,2672) + lmat(k,2672)
         mat(k,2674) = mat(k,2674) + lmat(k,2674)
         mat(k,2684) = mat(k,2684) + lmat(k,2684)
         mat(k,2691) = lmat(k,2691)
         mat(k,2701) = mat(k,2701) + lmat(k,2701)
         mat(k,2702) = mat(k,2702) + lmat(k,2702)
         mat(k,2703) = lmat(k,2703)
         mat(k,2709) = lmat(k,2709)
         mat(k,2714) = mat(k,2714) + lmat(k,2714)
         mat(k, 275) = 0._r8
         mat(k, 276) = 0._r8
         mat(k, 351) = 0._r8
         mat(k, 389) = 0._r8
         mat(k, 531) = 0._r8
         mat(k, 533) = 0._r8
         mat(k, 552) = 0._r8
         mat(k, 569) = 0._r8
         mat(k, 599) = 0._r8
         mat(k, 602) = 0._r8
         mat(k, 610) = 0._r8
         mat(k, 742) = 0._r8
         mat(k, 745) = 0._r8
         mat(k, 748) = 0._r8
         mat(k, 749) = 0._r8
         mat(k, 753) = 0._r8
         mat(k, 771) = 0._r8
         mat(k, 773) = 0._r8
         mat(k, 774) = 0._r8
         mat(k, 776) = 0._r8
         mat(k, 782) = 0._r8
         mat(k, 783) = 0._r8
         mat(k, 786) = 0._r8
         mat(k, 817) = 0._r8
         mat(k, 819) = 0._r8
         mat(k, 820) = 0._r8
         mat(k, 822) = 0._r8
         mat(k, 824) = 0._r8
         mat(k, 830) = 0._r8
         mat(k, 833) = 0._r8
         mat(k, 846) = 0._r8
         mat(k, 848) = 0._r8
         mat(k, 849) = 0._r8
         mat(k, 851) = 0._r8
         mat(k, 854) = 0._r8
         mat(k, 862) = 0._r8
         mat(k, 864) = 0._r8
         mat(k, 865) = 0._r8
         mat(k, 867) = 0._r8
         mat(k, 869) = 0._r8
         mat(k, 871) = 0._r8
         mat(k, 885) = 0._r8
         mat(k, 888) = 0._r8
         mat(k, 891) = 0._r8
         mat(k, 899) = 0._r8
         mat(k, 902) = 0._r8
         mat(k, 918) = 0._r8
         mat(k, 927) = 0._r8
         mat(k, 928) = 0._r8
         mat(k, 959) = 0._r8
         mat(k,1000) = 0._r8
         mat(k,1003) = 0._r8
         mat(k,1010) = 0._r8
         mat(k,1015) = 0._r8
         mat(k,1021) = 0._r8
         mat(k,1022) = 0._r8
         mat(k,1026) = 0._r8
         mat(k,1027) = 0._r8
         mat(k,1030) = 0._r8
         mat(k,1062) = 0._r8
         mat(k,1071) = 0._r8
         mat(k,1074) = 0._r8
         mat(k,1082) = 0._r8
         mat(k,1091) = 0._r8
         mat(k,1094) = 0._r8
         mat(k,1097) = 0._r8
         mat(k,1103) = 0._r8
         mat(k,1105) = 0._r8
         mat(k,1109) = 0._r8
         mat(k,1110) = 0._r8
         mat(k,1112) = 0._r8
         mat(k,1114) = 0._r8
         mat(k,1143) = 0._r8
         mat(k,1144) = 0._r8
         mat(k,1145) = 0._r8
         mat(k,1151) = 0._r8
         mat(k,1152) = 0._r8
         mat(k,1154) = 0._r8
         mat(k,1156) = 0._r8
         mat(k,1159) = 0._r8
         mat(k,1162) = 0._r8
         mat(k,1163) = 0._r8
         mat(k,1164) = 0._r8
         mat(k,1165) = 0._r8
         mat(k,1166) = 0._r8
         mat(k,1169) = 0._r8
         mat(k,1170) = 0._r8
         mat(k,1189) = 0._r8
         mat(k,1195) = 0._r8
         mat(k,1196) = 0._r8
         mat(k,1197) = 0._r8
         mat(k,1201) = 0._r8
         mat(k,1204) = 0._r8
         mat(k,1212) = 0._r8
         mat(k,1216) = 0._r8
         mat(k,1217) = 0._r8
         mat(k,1222) = 0._r8
         mat(k,1230) = 0._r8
         mat(k,1232) = 0._r8
         mat(k,1236) = 0._r8
         mat(k,1237) = 0._r8
         mat(k,1240) = 0._r8
         mat(k,1256) = 0._r8
         mat(k,1257) = 0._r8
         mat(k,1261) = 0._r8
         mat(k,1262) = 0._r8
         mat(k,1264) = 0._r8
         mat(k,1266) = 0._r8
         mat(k,1272) = 0._r8
         mat(k,1273) = 0._r8
         mat(k,1274) = 0._r8
         mat(k,1275) = 0._r8
         mat(k,1277) = 0._r8
         mat(k,1278) = 0._r8
         mat(k,1282) = 0._r8
         mat(k,1283) = 0._r8
         mat(k,1285) = 0._r8
         mat(k,1286) = 0._r8
         mat(k,1288) = 0._r8
         mat(k,1301) = 0._r8
         mat(k,1309) = 0._r8
         mat(k,1311) = 0._r8
         mat(k,1330) = 0._r8
         mat(k,1331) = 0._r8
         mat(k,1337) = 0._r8
         mat(k,1339) = 0._r8
         mat(k,1341) = 0._r8
         mat(k,1342) = 0._r8
         mat(k,1343) = 0._r8
         mat(k,1345) = 0._r8
         mat(k,1346) = 0._r8
         mat(k,1347) = 0._r8
         mat(k,1352) = 0._r8
         mat(k,1353) = 0._r8
         mat(k,1356) = 0._r8
         mat(k,1368) = 0._r8
         mat(k,1376) = 0._r8
         mat(k,1384) = 0._r8
         mat(k,1385) = 0._r8
         mat(k,1386) = 0._r8
         mat(k,1387) = 0._r8
         mat(k,1388) = 0._r8
         mat(k,1389) = 0._r8
         mat(k,1391) = 0._r8
         mat(k,1393) = 0._r8
         mat(k,1395) = 0._r8
         mat(k,1400) = 0._r8
         mat(k,1401) = 0._r8
         mat(k,1404) = 0._r8
         mat(k,1406) = 0._r8
         mat(k,1407) = 0._r8
         mat(k,1411) = 0._r8
         mat(k,1414) = 0._r8
         mat(k,1415) = 0._r8
         mat(k,1418) = 0._r8
         mat(k,1419) = 0._r8
         mat(k,1421) = 0._r8
         mat(k,1422) = 0._r8
         mat(k,1423) = 0._r8
         mat(k,1426) = 0._r8
         mat(k,1427) = 0._r8
         mat(k,1428) = 0._r8
         mat(k,1433) = 0._r8
         mat(k,1434) = 0._r8
         mat(k,1437) = 0._r8
         mat(k,1439) = 0._r8
         mat(k,1440) = 0._r8
         mat(k,1447) = 0._r8
         mat(k,1450) = 0._r8
         mat(k,1455) = 0._r8
         mat(k,1456) = 0._r8
         mat(k,1459) = 0._r8
         mat(k,1461) = 0._r8
         mat(k,1467) = 0._r8
         mat(k,1471) = 0._r8
         mat(k,1473) = 0._r8
         mat(k,1478) = 0._r8
         mat(k,1479) = 0._r8
         mat(k,1480) = 0._r8
         mat(k,1481) = 0._r8
         mat(k,1482) = 0._r8
         mat(k,1484) = 0._r8
         mat(k,1489) = 0._r8
         mat(k,1490) = 0._r8
         mat(k,1491) = 0._r8
         mat(k,1498) = 0._r8
         mat(k,1501) = 0._r8
         mat(k,1504) = 0._r8
         mat(k,1525) = 0._r8
         mat(k,1530) = 0._r8
         mat(k,1532) = 0._r8
         mat(k,1533) = 0._r8
         mat(k,1536) = 0._r8
         mat(k,1544) = 0._r8
         mat(k,1550) = 0._r8
         mat(k,1556) = 0._r8
         mat(k,1557) = 0._r8
         mat(k,1563) = 0._r8
         mat(k,1565) = 0._r8
         mat(k,1571) = 0._r8
         mat(k,1574) = 0._r8
         mat(k,1577) = 0._r8
         mat(k,1578) = 0._r8
         mat(k,1583) = 0._r8
         mat(k,1594) = 0._r8
         mat(k,1609) = 0._r8
         mat(k,1611) = 0._r8
         mat(k,1618) = 0._r8
         mat(k,1620) = 0._r8
         mat(k,1621) = 0._r8
         mat(k,1623) = 0._r8
         mat(k,1624) = 0._r8
         mat(k,1625) = 0._r8
         mat(k,1630) = 0._r8
         mat(k,1631) = 0._r8
         mat(k,1632) = 0._r8
         mat(k,1633) = 0._r8
         mat(k,1634) = 0._r8
         mat(k,1647) = 0._r8
         mat(k,1672) = 0._r8
         mat(k,1675) = 0._r8
         mat(k,1676) = 0._r8
         mat(k,1679) = 0._r8
         mat(k,1680) = 0._r8
         mat(k,1681) = 0._r8
         mat(k,1683) = 0._r8
         mat(k,1684) = 0._r8
         mat(k,1685) = 0._r8
         mat(k,1688) = 0._r8
         mat(k,1689) = 0._r8
         mat(k,1695) = 0._r8
         mat(k,1697) = 0._r8
         mat(k,1700) = 0._r8
         mat(k,1701) = 0._r8
         mat(k,1703) = 0._r8
         mat(k,1704) = 0._r8
         mat(k,1705) = 0._r8
         mat(k,1708) = 0._r8
         mat(k,1710) = 0._r8
         mat(k,1713) = 0._r8
         mat(k,1717) = 0._r8
         mat(k,1718) = 0._r8
         mat(k,1719) = 0._r8
         mat(k,1720) = 0._r8
         mat(k,1722) = 0._r8
         mat(k,1725) = 0._r8
         mat(k,1727) = 0._r8
         mat(k,1730) = 0._r8
         mat(k,1732) = 0._r8
         mat(k,1734) = 0._r8
         mat(k,1737) = 0._r8
         mat(k,1738) = 0._r8
         mat(k,1739) = 0._r8
         mat(k,1746) = 0._r8
         mat(k,1747) = 0._r8
         mat(k,1748) = 0._r8
         mat(k,1751) = 0._r8
         mat(k,1753) = 0._r8
         mat(k,1756) = 0._r8
         mat(k,1757) = 0._r8
         mat(k,1758) = 0._r8
         mat(k,1759) = 0._r8
         mat(k,1760) = 0._r8
         mat(k,1761) = 0._r8
         mat(k,1762) = 0._r8
         mat(k,1774) = 0._r8
         mat(k,1779) = 0._r8
         mat(k,1780) = 0._r8
         mat(k,1781) = 0._r8
         mat(k,1782) = 0._r8
         mat(k,1785) = 0._r8
         mat(k,1791) = 0._r8
         mat(k,1809) = 0._r8
         mat(k,1810) = 0._r8
         mat(k,1835) = 0._r8
         mat(k,1838) = 0._r8
         mat(k,1841) = 0._r8
         mat(k,1844) = 0._r8
         mat(k,1846) = 0._r8
         mat(k,1848) = 0._r8
         mat(k,1855) = 0._r8
         mat(k,1860) = 0._r8
         mat(k,1865) = 0._r8
         mat(k,1866) = 0._r8
         mat(k,1876) = 0._r8
         mat(k,1885) = 0._r8
         mat(k,1898) = 0._r8
         mat(k,1899) = 0._r8
         mat(k,1901) = 0._r8
         mat(k,1902) = 0._r8
         mat(k,1906) = 0._r8
         mat(k,1914) = 0._r8
         mat(k,1915) = 0._r8
         mat(k,1919) = 0._r8
         mat(k,1937) = 0._r8
         mat(k,1943) = 0._r8
         mat(k,1944) = 0._r8
         mat(k,1946) = 0._r8
         mat(k,1947) = 0._r8
         mat(k,1949) = 0._r8
         mat(k,1951) = 0._r8
         mat(k,1953) = 0._r8
         mat(k,1955) = 0._r8
         mat(k,1960) = 0._r8
         mat(k,1962) = 0._r8
         mat(k,1964) = 0._r8
         mat(k,1965) = 0._r8
         mat(k,1967) = 0._r8
         mat(k,1971) = 0._r8
         mat(k,1975) = 0._r8
         mat(k,1999) = 0._r8
         mat(k,2001) = 0._r8
         mat(k,2003) = 0._r8
         mat(k,2006) = 0._r8
         mat(k,2014) = 0._r8
         mat(k,2015) = 0._r8
         mat(k,2017) = 0._r8
         mat(k,2018) = 0._r8
         mat(k,2100) = 0._r8
         mat(k,2117) = 0._r8
         mat(k,2134) = 0._r8
         mat(k,2137) = 0._r8
         mat(k,2145) = 0._r8
         mat(k,2146) = 0._r8
         mat(k,2172) = 0._r8
         mat(k,2195) = 0._r8
         mat(k,2210) = 0._r8
         mat(k,2214) = 0._r8
         mat(k,2220) = 0._r8
         mat(k,2222) = 0._r8
         mat(k,2223) = 0._r8
         mat(k,2230) = 0._r8
         mat(k,2239) = 0._r8
         mat(k,2243) = 0._r8
         mat(k,2279) = 0._r8
         mat(k,2308) = 0._r8
         mat(k,2309) = 0._r8
         mat(k,2311) = 0._r8
         mat(k,2313) = 0._r8
         mat(k,2314) = 0._r8
         mat(k,2318) = 0._r8
         mat(k,2326) = 0._r8
         mat(k,2327) = 0._r8
         mat(k,2331) = 0._r8
         mat(k,2334) = 0._r8
         mat(k,2338) = 0._r8
         mat(k,2339) = 0._r8
         mat(k,2341) = 0._r8
         mat(k,2342) = 0._r8
         mat(k,2345) = 0._r8
         mat(k,2346) = 0._r8
         mat(k,2347) = 0._r8
         mat(k,2348) = 0._r8
         mat(k,2349) = 0._r8
         mat(k,2354) = 0._r8
         mat(k,2355) = 0._r8
         mat(k,2357) = 0._r8
         mat(k,2359) = 0._r8
         mat(k,2368) = 0._r8
         mat(k,2369) = 0._r8
         mat(k,2370) = 0._r8
         mat(k,2374) = 0._r8
         mat(k,2380) = 0._r8
         mat(k,2382) = 0._r8
         mat(k,2383) = 0._r8
         mat(k,2386) = 0._r8
         mat(k,2387) = 0._r8
         mat(k,2394) = 0._r8
         mat(k,2395) = 0._r8
         mat(k,2400) = 0._r8
         mat(k,2404) = 0._r8
         mat(k,2414) = 0._r8
         mat(k,2416) = 0._r8
         mat(k,2419) = 0._r8
         mat(k,2420) = 0._r8
         mat(k,2433) = 0._r8
         mat(k,2434) = 0._r8
         mat(k,2435) = 0._r8
         mat(k,2436) = 0._r8
         mat(k,2439) = 0._r8
         mat(k,2440) = 0._r8
         mat(k,2444) = 0._r8
         mat(k,2448) = 0._r8
         mat(k,2449) = 0._r8
         mat(k,2452) = 0._r8
         mat(k,2455) = 0._r8
         mat(k,2456) = 0._r8
         mat(k,2457) = 0._r8
         mat(k,2460) = 0._r8
         mat(k,2461) = 0._r8
         mat(k,2465) = 0._r8
         mat(k,2466) = 0._r8
         mat(k,2467) = 0._r8
         mat(k,2468) = 0._r8
         mat(k,2471) = 0._r8
         mat(k,2472) = 0._r8
         mat(k,2473) = 0._r8
         mat(k,2474) = 0._r8
         mat(k,2480) = 0._r8
         mat(k,2481) = 0._r8
         mat(k,2483) = 0._r8
         mat(k,2485) = 0._r8
         mat(k,2487) = 0._r8
         mat(k,2488) = 0._r8
         mat(k,2489) = 0._r8
         mat(k,2490) = 0._r8
         mat(k,2491) = 0._r8
         mat(k,2493) = 0._r8
         mat(k,2494) = 0._r8
         mat(k,2495) = 0._r8
         mat(k,2498) = 0._r8
         mat(k,2499) = 0._r8
         mat(k,2500) = 0._r8
         mat(k,2501) = 0._r8
         mat(k,2502) = 0._r8
         mat(k,2504) = 0._r8
         mat(k,2505) = 0._r8
         mat(k,2506) = 0._r8
         mat(k,2511) = 0._r8
         mat(k,2512) = 0._r8
         mat(k,2513) = 0._r8
         mat(k,2514) = 0._r8
         mat(k,2515) = 0._r8
         mat(k,2517) = 0._r8
         mat(k,2518) = 0._r8
         mat(k,2519) = 0._r8
         mat(k,2520) = 0._r8
         mat(k,2522) = 0._r8
         mat(k,2523) = 0._r8
         mat(k,2524) = 0._r8
         mat(k,2525) = 0._r8
         mat(k,2527) = 0._r8
         mat(k,2528) = 0._r8
         mat(k,2531) = 0._r8
         mat(k,2532) = 0._r8
         mat(k,2539) = 0._r8
         mat(k,2541) = 0._r8
         mat(k,2543) = 0._r8
         mat(k,2545) = 0._r8
         mat(k,2550) = 0._r8
         mat(k,2553) = 0._r8
         mat(k,2559) = 0._r8
         mat(k,2560) = 0._r8
         mat(k,2561) = 0._r8
         mat(k,2562) = 0._r8
         mat(k,2565) = 0._r8
         mat(k,2567) = 0._r8
         mat(k,2568) = 0._r8
         mat(k,2569) = 0._r8
         mat(k,2570) = 0._r8
         mat(k,2571) = 0._r8
         mat(k,2572) = 0._r8
         mat(k,2573) = 0._r8
         mat(k,2577) = 0._r8
         mat(k,2585) = 0._r8
         mat(k,2590) = 0._r8
         mat(k,2606) = 0._r8
         mat(k,2612) = 0._r8
         mat(k,2614) = 0._r8
         mat(k,2615) = 0._r8
         mat(k,2618) = 0._r8
         mat(k,2619) = 0._r8
         mat(k,2633) = 0._r8
         mat(k,2637) = 0._r8
         mat(k,2641) = 0._r8
         mat(k,2646) = 0._r8
         mat(k,2647) = 0._r8
         mat(k,2650) = 0._r8
         mat(k,2651) = 0._r8
         mat(k,2652) = 0._r8
         mat(k,2654) = 0._r8
         mat(k,2657) = 0._r8
         mat(k,2658) = 0._r8
         mat(k,2659) = 0._r8
         mat(k,2661) = 0._r8
         mat(k,2667) = 0._r8
         mat(k,2668) = 0._r8
         mat(k,2681) = 0._r8
         mat(k,2685) = 0._r8
         mat(k,2690) = 0._r8
         mat(k,2692) = 0._r8
         mat(k,2693) = 0._r8
         mat(k,2694) = 0._r8
         mat(k,2695) = 0._r8
         mat(k,2696) = 0._r8
         mat(k,2697) = 0._r8
         mat(k,2698) = 0._r8
         mat(k,2699) = 0._r8
         mat(k,2700) = 0._r8
         mat(k,2704) = 0._r8
         mat(k,2705) = 0._r8
         mat(k,2706) = 0._r8
         mat(k,2707) = 0._r8
         mat(k,2708) = 0._r8
         mat(k,2710) = 0._r8
         mat(k,2711) = 0._r8
         mat(k,2712) = 0._r8
         mat(k,2713) = 0._r8
         mat(k, 1) = mat(k, 1) - dti(k)
         mat(k, 2) = mat(k, 2) - dti(k)
         mat(k, 3) = mat(k, 3) - dti(k)
         mat(k, 4) = mat(k, 4) - dti(k)
         mat(k, 5) = mat(k, 5) - dti(k)
         mat(k, 7) = mat(k, 7) - dti(k)
         mat(k, 10) = mat(k, 10) - dti(k)
         mat(k, 13) = mat(k, 13) - dti(k)
         mat(k, 14) = mat(k, 14) - dti(k)
         mat(k, 15) = mat(k, 15) - dti(k)
         mat(k, 16) = mat(k, 16) - dti(k)
         mat(k, 17) = mat(k, 17) - dti(k)
         mat(k, 18) = mat(k, 18) - dti(k)
         mat(k, 19) = mat(k, 19) - dti(k)
         mat(k, 20) = mat(k, 20) - dti(k)
         mat(k, 21) = mat(k, 21) - dti(k)
         mat(k, 22) = mat(k, 22) - dti(k)
         mat(k, 23) = mat(k, 23) - dti(k)
         mat(k, 24) = mat(k, 24) - dti(k)
         mat(k, 25) = mat(k, 25) - dti(k)
         mat(k, 26) = mat(k, 26) - dti(k)
         mat(k, 27) = mat(k, 27) - dti(k)
         mat(k, 28) = mat(k, 28) - dti(k)
         mat(k, 29) = mat(k, 29) - dti(k)
         mat(k, 30) = mat(k, 30) - dti(k)
         mat(k, 31) = mat(k, 31) - dti(k)
         mat(k, 32) = mat(k, 32) - dti(k)
         mat(k, 33) = mat(k, 33) - dti(k)
         mat(k, 34) = mat(k, 34) - dti(k)
         mat(k, 35) = mat(k, 35) - dti(k)
         mat(k, 36) = mat(k, 36) - dti(k)
         mat(k, 37) = mat(k, 37) - dti(k)
         mat(k, 38) = mat(k, 38) - dti(k)
         mat(k, 39) = mat(k, 39) - dti(k)
         mat(k, 40) = mat(k, 40) - dti(k)
         mat(k, 41) = mat(k, 41) - dti(k)
         mat(k, 42) = mat(k, 42) - dti(k)
         mat(k, 43) = mat(k, 43) - dti(k)
         mat(k, 44) = mat(k, 44) - dti(k)
         mat(k, 45) = mat(k, 45) - dti(k)
         mat(k, 46) = mat(k, 46) - dti(k)
         mat(k, 47) = mat(k, 47) - dti(k)
         mat(k, 48) = mat(k, 48) - dti(k)
         mat(k, 49) = mat(k, 49) - dti(k)
         mat(k, 50) = mat(k, 50) - dti(k)
         mat(k, 51) = mat(k, 51) - dti(k)
         mat(k, 52) = mat(k, 52) - dti(k)
         mat(k, 53) = mat(k, 53) - dti(k)
         mat(k, 54) = mat(k, 54) - dti(k)
         mat(k, 55) = mat(k, 55) - dti(k)
         mat(k, 56) = mat(k, 56) - dti(k)
         mat(k, 57) = mat(k, 57) - dti(k)
         mat(k, 58) = mat(k, 58) - dti(k)
         mat(k, 59) = mat(k, 59) - dti(k)
         mat(k, 60) = mat(k, 60) - dti(k)
         mat(k, 61) = mat(k, 61) - dti(k)
         mat(k, 62) = mat(k, 62) - dti(k)
         mat(k, 63) = mat(k, 63) - dti(k)
         mat(k, 64) = mat(k, 64) - dti(k)
         mat(k, 65) = mat(k, 65) - dti(k)
         mat(k, 66) = mat(k, 66) - dti(k)
         mat(k, 67) = mat(k, 67) - dti(k)
         mat(k, 68) = mat(k, 68) - dti(k)
         mat(k, 69) = mat(k, 69) - dti(k)
         mat(k, 70) = mat(k, 70) - dti(k)
         mat(k, 71) = mat(k, 71) - dti(k)
         mat(k, 72) = mat(k, 72) - dti(k)
         mat(k, 78) = mat(k, 78) - dti(k)
         mat(k, 79) = mat(k, 79) - dti(k)
         mat(k, 80) = mat(k, 80) - dti(k)
         mat(k, 81) = mat(k, 81) - dti(k)
         mat(k, 82) = mat(k, 82) - dti(k)
         mat(k, 83) = mat(k, 83) - dti(k)
         mat(k, 84) = mat(k, 84) - dti(k)
         mat(k, 85) = mat(k, 85) - dti(k)
         mat(k, 86) = mat(k, 86) - dti(k)
         mat(k, 87) = mat(k, 87) - dti(k)
         mat(k, 88) = mat(k, 88) - dti(k)
         mat(k, 94) = mat(k, 94) - dti(k)
         mat(k, 100) = mat(k, 100) - dti(k)
         mat(k, 106) = mat(k, 106) - dti(k)
         mat(k, 107) = mat(k, 107) - dti(k)
         mat(k, 108) = mat(k, 108) - dti(k)
         mat(k, 109) = mat(k, 109) - dti(k)
         mat(k, 110) = mat(k, 110) - dti(k)
         mat(k, 111) = mat(k, 111) - dti(k)
         mat(k, 112) = mat(k, 112) - dti(k)
         mat(k, 113) = mat(k, 113) - dti(k)
         mat(k, 114) = mat(k, 114) - dti(k)
         mat(k, 115) = mat(k, 115) - dti(k)
         mat(k, 117) = mat(k, 117) - dti(k)
         mat(k, 120) = mat(k, 120) - dti(k)
         mat(k, 123) = mat(k, 123) - dti(k)
         mat(k, 126) = mat(k, 126) - dti(k)
         mat(k, 129) = mat(k, 129) - dti(k)
         mat(k, 133) = mat(k, 133) - dti(k)
         mat(k, 137) = mat(k, 137) - dti(k)
         mat(k, 141) = mat(k, 141) - dti(k)
         mat(k, 145) = mat(k, 145) - dti(k)
         mat(k, 149) = mat(k, 149) - dti(k)
         mat(k, 153) = mat(k, 153) - dti(k)
         mat(k, 157) = mat(k, 157) - dti(k)
         mat(k, 161) = mat(k, 161) - dti(k)
         mat(k, 165) = mat(k, 165) - dti(k)
         mat(k, 169) = mat(k, 169) - dti(k)
         mat(k, 172) = mat(k, 172) - dti(k)
         mat(k, 175) = mat(k, 175) - dti(k)
         mat(k, 178) = mat(k, 178) - dti(k)
         mat(k, 181) = mat(k, 181) - dti(k)
         mat(k, 186) = mat(k, 186) - dti(k)
         mat(k, 191) = mat(k, 191) - dti(k)
         mat(k, 196) = mat(k, 196) - dti(k)
         mat(k, 201) = mat(k, 201) - dti(k)
         mat(k, 209) = mat(k, 209) - dti(k)
         mat(k, 215) = mat(k, 215) - dti(k)
         mat(k, 219) = mat(k, 219) - dti(k)
         mat(k, 224) = mat(k, 224) - dti(k)
         mat(k, 228) = mat(k, 228) - dti(k)
         mat(k, 231) = mat(k, 231) - dti(k)
         mat(k, 235) = mat(k, 235) - dti(k)
         mat(k, 239) = mat(k, 239) - dti(k)
         mat(k, 246) = mat(k, 246) - dti(k)
         mat(k, 253) = mat(k, 253) - dti(k)
         mat(k, 258) = mat(k, 258) - dti(k)
         mat(k, 262) = mat(k, 262) - dti(k)
         mat(k, 271) = mat(k, 271) - dti(k)
         mat(k, 279) = mat(k, 279) - dti(k)
         mat(k, 284) = mat(k, 284) - dti(k)
         mat(k, 287) = mat(k, 287) - dti(k)
         mat(k, 290) = mat(k, 290) - dti(k)
         mat(k, 293) = mat(k, 293) - dti(k)
         mat(k, 298) = mat(k, 298) - dti(k)
         mat(k, 303) = mat(k, 303) - dti(k)
         mat(k, 307) = mat(k, 307) - dti(k)
         mat(k, 312) = mat(k, 312) - dti(k)
         mat(k, 317) = mat(k, 317) - dti(k)
         mat(k, 321) = mat(k, 321) - dti(k)
         mat(k, 325) = mat(k, 325) - dti(k)
         mat(k, 329) = mat(k, 329) - dti(k)
         mat(k, 333) = mat(k, 333) - dti(k)
         mat(k, 337) = mat(k, 337) - dti(k)
         mat(k, 343) = mat(k, 343) - dti(k)
         mat(k, 350) = mat(k, 350) - dti(k)
         mat(k, 356) = mat(k, 356) - dti(k)
         mat(k, 362) = mat(k, 362) - dti(k)
         mat(k, 365) = mat(k, 365) - dti(k)
         mat(k, 371) = mat(k, 371) - dti(k)
         mat(k, 374) = mat(k, 374) - dti(k)
         mat(k, 381) = mat(k, 381) - dti(k)
         mat(k, 387) = mat(k, 387) - dti(k)
         mat(k, 392) = mat(k, 392) - dti(k)
         mat(k, 397) = mat(k, 397) - dti(k)
         mat(k, 402) = mat(k, 402) - dti(k)
         mat(k, 405) = mat(k, 405) - dti(k)
         mat(k, 412) = mat(k, 412) - dti(k)
         mat(k, 415) = mat(k, 415) - dti(k)
         mat(k, 420) = mat(k, 420) - dti(k)
         mat(k, 425) = mat(k, 425) - dti(k)
         mat(k, 433) = mat(k, 433) - dti(k)
         mat(k, 441) = mat(k, 441) - dti(k)
         mat(k, 449) = mat(k, 449) - dti(k)
         mat(k, 457) = mat(k, 457) - dti(k)
         mat(k, 465) = mat(k, 465) - dti(k)
         mat(k, 468) = mat(k, 468) - dti(k)
         mat(k, 474) = mat(k, 474) - dti(k)
         mat(k, 480) = mat(k, 480) - dti(k)
         mat(k, 486) = mat(k, 486) - dti(k)
         mat(k, 492) = mat(k, 492) - dti(k)
         mat(k, 498) = mat(k, 498) - dti(k)
         mat(k, 504) = mat(k, 504) - dti(k)
         mat(k, 510) = mat(k, 510) - dti(k)
         mat(k, 516) = mat(k, 516) - dti(k)
         mat(k, 522) = mat(k, 522) - dti(k)
         mat(k, 530) = mat(k, 530) - dti(k)
         mat(k, 536) = mat(k, 536) - dti(k)
         mat(k, 542) = mat(k, 542) - dti(k)
         mat(k, 549) = mat(k, 549) - dti(k)
         mat(k, 555) = mat(k, 555) - dti(k)
         mat(k, 560) = mat(k, 560) - dti(k)
         mat(k, 563) = mat(k, 563) - dti(k)
         mat(k, 568) = mat(k, 568) - dti(k)
         mat(k, 575) = mat(k, 575) - dti(k)
         mat(k, 582) = mat(k, 582) - dti(k)
         mat(k, 585) = mat(k, 585) - dti(k)
         mat(k, 589) = mat(k, 589) - dti(k)
         mat(k, 598) = mat(k, 598) - dti(k)
         mat(k, 606) = mat(k, 606) - dti(k)
         mat(k, 613) = mat(k, 613) - dti(k)
         mat(k, 621) = mat(k, 621) - dti(k)
         mat(k, 626) = mat(k, 626) - dti(k)
         mat(k, 630) = mat(k, 630) - dti(k)
         mat(k, 635) = mat(k, 635) - dti(k)
         mat(k, 641) = mat(k, 641) - dti(k)
         mat(k, 647) = mat(k, 647) - dti(k)
         mat(k, 655) = mat(k, 655) - dti(k)
         mat(k, 663) = mat(k, 663) - dti(k)
         mat(k, 671) = mat(k, 671) - dti(k)
         mat(k, 679) = mat(k, 679) - dti(k)
         mat(k, 687) = mat(k, 687) - dti(k)
         mat(k, 691) = mat(k, 691) - dti(k)
         mat(k, 700) = mat(k, 700) - dti(k)
         mat(k, 707) = mat(k, 707) - dti(k)
         mat(k, 714) = mat(k, 714) - dti(k)
         mat(k, 723) = mat(k, 723) - dti(k)
         mat(k, 732) = mat(k, 732) - dti(k)
         mat(k, 740) = mat(k, 740) - dti(k)
         mat(k, 747) = mat(k, 747) - dti(k)
         mat(k, 757) = mat(k, 757) - dti(k)
         mat(k, 770) = mat(k, 770) - dti(k)
         mat(k, 781) = mat(k, 781) - dti(k)
         mat(k, 792) = mat(k, 792) - dti(k)
         mat(k, 799) = mat(k, 799) - dti(k)
         mat(k, 805) = mat(k, 805) - dti(k)
         mat(k, 816) = mat(k, 816) - dti(k)
         mat(k, 829) = mat(k, 829) - dti(k)
         mat(k, 836) = mat(k, 836) - dti(k)
         mat(k, 847) = mat(k, 847) - dti(k)
         mat(k, 863) = mat(k, 863) - dti(k)
         mat(k, 874) = mat(k, 874) - dti(k)
         mat(k, 883) = mat(k, 883) - dti(k)
         mat(k, 893) = mat(k, 893) - dti(k)
         mat(k, 903) = mat(k, 903) - dti(k)
         mat(k, 907) = mat(k, 907) - dti(k)
         mat(k, 913) = mat(k, 913) - dti(k)
         mat(k, 923) = mat(k, 923) - dti(k)
         mat(k, 938) = mat(k, 938) - dti(k)
         mat(k, 956) = mat(k, 956) - dti(k)
         mat(k, 966) = mat(k, 966) - dti(k)
         mat(k, 972) = mat(k, 972) - dti(k)
         mat(k, 982) = mat(k, 982) - dti(k)
         mat(k, 999) = mat(k, 999) - dti(k)
         mat(k,1023) = mat(k,1023) - dti(k)
         mat(k,1034) = mat(k,1034) - dti(k)
         mat(k,1044) = mat(k,1044) - dti(k)
         mat(k,1057) = mat(k,1057) - dti(k)
         mat(k,1063) = mat(k,1063) - dti(k)
         mat(k,1070) = mat(k,1070) - dti(k)
         mat(k,1078) = mat(k,1078) - dti(k)
         mat(k,1095) = mat(k,1095) - dti(k)
         mat(k,1115) = mat(k,1115) - dti(k)
         mat(k,1124) = mat(k,1124) - dti(k)
         mat(k,1140) = mat(k,1140) - dti(k)
         mat(k,1161) = mat(k,1161) - dti(k)
         mat(k,1173) = mat(k,1173) - dti(k)
         mat(k,1184) = mat(k,1184) - dti(k)
         mat(k,1194) = mat(k,1194) - dti(k)
         mat(k,1208) = mat(k,1208) - dti(k)
         mat(k,1219) = mat(k,1219) - dti(k)
         mat(k,1228) = mat(k,1228) - dti(k)
         mat(k,1241) = mat(k,1241) - dti(k)
         mat(k,1255) = mat(k,1255) - dti(k)
         mat(k,1276) = mat(k,1276) - dti(k)
         mat(k,1292) = mat(k,1292) - dti(k)
         mat(k,1304) = mat(k,1304) - dti(k)
         mat(k,1324) = mat(k,1324) - dti(k)
         mat(k,1344) = mat(k,1344) - dti(k)
         mat(k,1360) = mat(k,1360) - dti(k)
         mat(k,1372) = mat(k,1372) - dti(k)
         mat(k,1392) = mat(k,1392) - dti(k)
         mat(k,1425) = mat(k,1425) - dti(k)
         mat(k,1449) = mat(k,1449) - dti(k)
         mat(k,1470) = mat(k,1470) - dti(k)
         mat(k,1492) = mat(k,1492) - dti(k)
         mat(k,1524) = mat(k,1524) - dti(k)
         mat(k,1540) = mat(k,1540) - dti(k)
         mat(k,1555) = mat(k,1555) - dti(k)
         mat(k,1568) = mat(k,1568) - dti(k)
         mat(k,1582) = mat(k,1582) - dti(k)
         mat(k,1600) = mat(k,1600) - dti(k)
         mat(k,1619) = mat(k,1619) - dti(k)
         mat(k,1673) = mat(k,1673) - dti(k)
         mat(k,1696) = mat(k,1696) - dti(k)
         mat(k,1721) = mat(k,1721) - dti(k)
         mat(k,1749) = mat(k,1749) - dti(k)
         mat(k,1773) = mat(k,1773) - dti(k)
         mat(k,1873) = mat(k,1873) - dti(k)
         mat(k,1904) = mat(k,1904) - dti(k)
         mat(k,1961) = mat(k,1961) - dti(k)
         mat(k,2008) = mat(k,2008) - dti(k)
         mat(k,2196) = mat(k,2196) - dti(k)
         mat(k,2232) = mat(k,2232) - dti(k)
         mat(k,2321) = mat(k,2321) - dti(k)
         mat(k,2350) = mat(k,2350) - dti(k)
         mat(k,2379) = mat(k,2379) - dti(k)
         mat(k,2450) = mat(k,2450) - dti(k)
         mat(k,2479) = mat(k,2479) - dti(k)
         mat(k,2503) = mat(k,2503) - dti(k)
         mat(k,2529) = mat(k,2529) - dti(k)
         mat(k,2587) = mat(k,2587) - dti(k)
         mat(k,2617) = mat(k,2617) - dti(k)
         mat(k,2684) = mat(k,2684) - dti(k)
         mat(k,2714) = mat(k,2714) - dti(k)
      end do
      end subroutine nlnmat_finit
      subroutine nlnmat( avec_len, mat, y, rxt, lmat, dti )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: dti(veclen)
      real(r8), intent(in) :: lmat(veclen,nzcnt)
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
      call nlnmat01( avec_len, mat, y, rxt )
      call nlnmat02( avec_len, mat, y, rxt )
      call nlnmat03( avec_len, mat, y, rxt )
      call nlnmat04( avec_len, mat, y, rxt )
      call nlnmat05( avec_len, mat, y, rxt )
      call nlnmat06( avec_len, mat, y, rxt )
      call nlnmat07( avec_len, mat, y, rxt )
      call nlnmat08( avec_len, mat, y, rxt )
      call nlnmat09( avec_len, mat, y, rxt )
      call nlnmat10( avec_len, mat, y, rxt )
      call nlnmat11( avec_len, mat, y, rxt )
      call nlnmat_finit( avec_len, mat, lmat, dti )
      end subroutine nlnmat
      end module mo_nln_matrix
