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
         mat(k,767) = -(rxt(k,496)*y(k,263))
         mat(k,2238) = -rxt(k,496)*y(k,1)
         mat(k,2069) = rxt(k,499)*y(k,227)
         mat(k,1130) = rxt(k,499)*y(k,158)
         mat(k,756) = -(rxt(k,500)*y(k,263))
         mat(k,2237) = -rxt(k,500)*y(k,2)
         mat(k,2725) = rxt(k,497)*y(k,227)
         mat(k,1129) = rxt(k,497)*y(k,108)
         mat(k,1109) = -(rxt(k,579)*y(k,160) + rxt(k,580)*y(k,170) + rxt(k,581) &
                      *y(k,263))
         mat(k,1917) = -rxt(k,579)*y(k,6)
         mat(k,2376) = -rxt(k,580)*y(k,6)
         mat(k,2263) = -rxt(k,581)*y(k,6)
         mat(k,189) = -(rxt(k,538)*y(k,263))
         mat(k,2161) = -rxt(k,538)*y(k,7)
         mat(k,527) = -(rxt(k,541)*y(k,263))
         mat(k,2210) = -rxt(k,541)*y(k,8)
         mat(k,2709) = rxt(k,539)*y(k,229)
         mat(k,574) = rxt(k,539)*y(k,108)
         mat(k,190) = .120_r8*rxt(k,538)*y(k,263)
         mat(k,2162) = .120_r8*rxt(k,538)*y(k,7)
         mat(k,1107) = .100_r8*rxt(k,580)*y(k,170)
         mat(k,1052) = .100_r8*rxt(k,583)*y(k,170)
         mat(k,2365) = .100_r8*rxt(k,580)*y(k,6) + .100_r8*rxt(k,583)*y(k,144)
         mat(k,2056) = .500_r8*rxt(k,540)*y(k,229) + .200_r8*rxt(k,567)*y(k,270) &
                      + .060_r8*rxt(k,573)*y(k,273)
         mat(k,575) = .500_r8*rxt(k,540)*y(k,158)
         mat(k,829) = .200_r8*rxt(k,567)*y(k,158)
         mat(k,853) = .060_r8*rxt(k,573)*y(k,158)
         mat(k,2049) = .200_r8*rxt(k,567)*y(k,270) + .200_r8*rxt(k,573)*y(k,273)
         mat(k,828) = .200_r8*rxt(k,567)*y(k,158)
         mat(k,851) = .200_r8*rxt(k,573)*y(k,158)
         mat(k,2064) = .200_r8*rxt(k,567)*y(k,270) + .150_r8*rxt(k,573)*y(k,273)
         mat(k,831) = .200_r8*rxt(k,567)*y(k,158)
         mat(k,854) = .150_r8*rxt(k,573)*y(k,158)
         mat(k,2050) = .210_r8*rxt(k,573)*y(k,273)
         mat(k,852) = .210_r8*rxt(k,573)*y(k,158)
         mat(k,285) = -(rxt(k,501)*y(k,263))
         mat(k,2177) = -rxt(k,501)*y(k,15)
         mat(k,1106) = .050_r8*rxt(k,580)*y(k,170)
         mat(k,1051) = .050_r8*rxt(k,583)*y(k,170)
         mat(k,2363) = .050_r8*rxt(k,580)*y(k,6) + .050_r8*rxt(k,583)*y(k,144)
         mat(k,424) = -(rxt(k,467)*y(k,160) + rxt(k,468)*y(k,263))
         mat(k,1907) = -rxt(k,467)*y(k,16)
         mat(k,2198) = -rxt(k,468)*y(k,16)
         mat(k,2823) = -(rxt(k,286)*y(k,51) + rxt(k,287)*y(k,108) + rxt(k,288) &
                      *y(k,159) + rxt(k,289)*y(k,170) + rxt(k,296)*y(k,22) + rxt(k,325) &
                      *y(k,128))
         mat(k,2352) = -rxt(k,286)*y(k,17)
         mat(k,2793) = -rxt(k,287)*y(k,17)
         mat(k,2031) = -rxt(k,288)*y(k,17)
         mat(k,2424) = -rxt(k,289)*y(k,17)
         mat(k,971) = -rxt(k,296)*y(k,17)
         mat(k,2617) = -rxt(k,325)*y(k,17)
         mat(k,600) = rxt(k,285)*y(k,263)
         mat(k,2647) = 4.000_r8*rxt(k,290)*y(k,21) + (rxt(k,291)+rxt(k,292))*y(k,74) &
                      + rxt(k,602)*y(k,83) + rxt(k,315)*y(k,117) + (rxt(k,326) &
                       +rxt(k,327))*y(k,128) + rxt(k,295)*y(k,158) + rxt(k,300) &
                      *y(k,168) + rxt(k,613)*y(k,187) + rxt(k,301)*y(k,263)
         mat(k,172) = rxt(k,275)*y(k,259)
         mat(k,177) = rxt(k,305)*y(k,259)
         mat(k,565) = 2.000_r8*rxt(k,362)*y(k,70) + 2.000_r8*rxt(k,389)*y(k,259) &
                      + 2.000_r8*rxt(k,363)*y(k,263)
         mat(k,144) = rxt(k,364)*y(k,263)
         mat(k,686) = rxt(k,367)*y(k,70) + rxt(k,390)*y(k,259) + rxt(k,368)*y(k,263)
         mat(k,152) = 2.000_r8*rxt(k,374)*y(k,263)
         mat(k,506) = 3.000_r8*rxt(k,375)*y(k,70) + 3.000_r8*rxt(k,306)*y(k,259) &
                      + 3.000_r8*rxt(k,376)*y(k,263)
         mat(k,124) = rxt(k,377)*y(k,263)
         mat(k,2584) = 2.000_r8*rxt(k,362)*y(k,45) + rxt(k,367)*y(k,52) &
                      + 3.000_r8*rxt(k,375)*y(k,66)
         mat(k,2853) = (rxt(k,291)+rxt(k,292))*y(k,21)
         mat(k,1188) = rxt(k,602)*y(k,21)
         mat(k,128) = 2.000_r8*rxt(k,307)*y(k,259)
         mat(k,1611) = rxt(k,302)*y(k,168) + rxt(k,308)*y(k,259) + rxt(k,303)*y(k,263)
         mat(k,2677) = rxt(k,315)*y(k,21)
         mat(k,2617) = mat(k,2617) + (rxt(k,326)+rxt(k,327))*y(k,21)
         mat(k,2136) = rxt(k,295)*y(k,21)
         mat(k,2521) = rxt(k,300)*y(k,21) + rxt(k,302)*y(k,98)
         mat(k,1653) = rxt(k,613)*y(k,21)
         mat(k,1901) = rxt(k,275)*y(k,38) + rxt(k,305)*y(k,39) + 2.000_r8*rxt(k,389) &
                      *y(k,45) + rxt(k,390)*y(k,52) + 3.000_r8*rxt(k,306)*y(k,66) &
                      + 2.000_r8*rxt(k,307)*y(k,94) + rxt(k,308)*y(k,98)
         mat(k,2321) = rxt(k,285)*y(k,18) + rxt(k,301)*y(k,21) + 2.000_r8*rxt(k,363) &
                      *y(k,45) + rxt(k,364)*y(k,46) + rxt(k,368)*y(k,52) &
                      + 2.000_r8*rxt(k,374)*y(k,65) + 3.000_r8*rxt(k,376)*y(k,66) &
                      + rxt(k,377)*y(k,67) + rxt(k,303)*y(k,98)
         mat(k,597) = -(rxt(k,285)*y(k,263))
         mat(k,2218) = -rxt(k,285)*y(k,18)
         mat(k,2797) = rxt(k,296)*y(k,22)
         mat(k,960) = rxt(k,296)*y(k,17)
         mat(k,1598) = (rxt(k,630)+rxt(k,704)+rxt(k,717)+rxt(k,726))*y(k,110)
         mat(k,1678) = (rxt(k,630)+rxt(k,704)+rxt(k,717)+rxt(k,726))*y(k,98)
         mat(k,2621) = rxt(k,293)*y(k,74)
         mat(k,961) = rxt(k,297)*y(k,70)
         mat(k,2537) = rxt(k,297)*y(k,22)
         mat(k,2830) = rxt(k,293)*y(k,21)
         mat(k,1599) = (rxt(k,629)+rxt(k,706)+rxt(k,714)+rxt(k,723))*y(k,111)
         mat(k,1835) = (rxt(k,632)+rxt(k,703)+rxt(k,716)+rxt(k,725))*y(k,110)
         mat(k,1679) = (rxt(k,632)+rxt(k,703)+rxt(k,716)+rxt(k,725))*y(k,102)
         mat(k,1811) = (rxt(k,629)+rxt(k,706)+rxt(k,714)+rxt(k,723))*y(k,98)
         mat(k,2796) = rxt(k,288)*y(k,159)
         mat(k,1979) = rxt(k,288)*y(k,17)
         mat(k,2644) = -(4._r8*rxt(k,290)*y(k,21) + (rxt(k,291) + rxt(k,292) + rxt(k,293) &
                      ) * y(k,74) + rxt(k,294)*y(k,108) + rxt(k,295)*y(k,158) &
                      + rxt(k,298)*y(k,159) + rxt(k,300)*y(k,168) + rxt(k,301) &
                      *y(k,263) + rxt(k,315)*y(k,117) + (rxt(k,326) + rxt(k,327) &
                      ) * y(k,128) + rxt(k,602)*y(k,83) + rxt(k,613)*y(k,187))
         mat(k,2850) = -(rxt(k,291) + rxt(k,292) + rxt(k,293)) * y(k,21)
         mat(k,2790) = -rxt(k,294)*y(k,21)
         mat(k,2133) = -rxt(k,295)*y(k,21)
         mat(k,2028) = -rxt(k,298)*y(k,21)
         mat(k,2518) = -rxt(k,300)*y(k,21)
         mat(k,2318) = -rxt(k,301)*y(k,21)
         mat(k,2674) = -rxt(k,315)*y(k,21)
         mat(k,2614) = -(rxt(k,326) + rxt(k,327)) * y(k,21)
         mat(k,1185) = -rxt(k,602)*y(k,21)
         mat(k,1651) = -rxt(k,613)*y(k,21)
         mat(k,2820) = rxt(k,325)*y(k,128) + rxt(k,289)*y(k,170)
         mat(k,970) = rxt(k,299)*y(k,168)
         mat(k,1609) = rxt(k,309)*y(k,259)
         mat(k,1696) = rxt(k,304)*y(k,168)
         mat(k,2614) = mat(k,2614) + rxt(k,325)*y(k,17)
         mat(k,2518) = mat(k,2518) + rxt(k,299)*y(k,22) + rxt(k,304)*y(k,110)
         mat(k,2421) = rxt(k,289)*y(k,17)
         mat(k,1898) = rxt(k,309)*y(k,98)
         mat(k,962) = -(rxt(k,296)*y(k,17) + rxt(k,297)*y(k,70) + rxt(k,299)*y(k,168))
         mat(k,2799) = -rxt(k,296)*y(k,22)
         mat(k,2543) = -rxt(k,297)*y(k,22)
         mat(k,2489) = -rxt(k,299)*y(k,22)
         mat(k,2623) = rxt(k,298)*y(k,159)
         mat(k,1995) = rxt(k,298)*y(k,21)
         mat(k,288) = -(rxt(k,542)*y(k,263))
         mat(k,2178) = -rxt(k,542)*y(k,24)
         mat(k,2047) = rxt(k,545)*y(k,231)
         mat(k,513) = rxt(k,545)*y(k,158)
         mat(k,387) = -(rxt(k,544)*y(k,263))
         mat(k,2191) = -rxt(k,544)*y(k,25)
         mat(k,2697) = rxt(k,543)*y(k,231)
         mat(k,514) = rxt(k,543)*y(k,108)
         mat(k,217) = -(rxt(k,358)*y(k,70) + rxt(k,359)*y(k,263))
         mat(k,2524) = -rxt(k,358)*y(k,26)
         mat(k,2165) = -rxt(k,359)*y(k,26)
         mat(k,330) = -(rxt(k,415)*y(k,70) + rxt(k,416)*y(k,263))
         mat(k,2527) = -rxt(k,415)*y(k,27)
         mat(k,2184) = -rxt(k,416)*y(k,27)
         mat(k,630) = -(rxt(k,417)*y(k,70) + rxt(k,418)*y(k,170) + rxt(k,443)*y(k,263))
         mat(k,2539) = -rxt(k,417)*y(k,28)
         mat(k,2367) = -rxt(k,418)*y(k,28)
         mat(k,2223) = -rxt(k,443)*y(k,28)
         mat(k,294) = -(rxt(k,360)*y(k,70) + rxt(k,361)*y(k,263))
         mat(k,2526) = -rxt(k,360)*y(k,29)
         mat(k,2180) = -rxt(k,361)*y(k,29)
         mat(k,308) = -(rxt(k,423)*y(k,263))
         mat(k,2181) = -rxt(k,423)*y(k,30)
         mat(k,997) = .800_r8*rxt(k,419)*y(k,232) + .200_r8*rxt(k,420)*y(k,236)
         mat(k,1700) = .200_r8*rxt(k,420)*y(k,232)
         mat(k,397) = -(rxt(k,424)*y(k,263))
         mat(k,2193) = -rxt(k,424)*y(k,31)
         mat(k,2699) = rxt(k,421)*y(k,232)
         mat(k,998) = rxt(k,421)*y(k,108)
         mat(k,336) = -(rxt(k,425)*y(k,70) + rxt(k,426)*y(k,263))
         mat(k,2528) = -rxt(k,425)*y(k,32)
         mat(k,2185) = -rxt(k,426)*y(k,32)
         mat(k,1256) = -(rxt(k,446)*y(k,160) + rxt(k,447)*y(k,170) + rxt(k,465) &
                      *y(k,263))
         mat(k,1929) = -rxt(k,446)*y(k,33)
         mat(k,2385) = -rxt(k,447)*y(k,33)
         mat(k,2275) = -rxt(k,465)*y(k,33)
         mat(k,977) = .130_r8*rxt(k,525)*y(k,170)
         mat(k,2385) = mat(k,2385) + .130_r8*rxt(k,525)*y(k,130)
         mat(k,495) = -(rxt(k,451)*y(k,263))
         mat(k,2206) = -rxt(k,451)*y(k,34)
         mat(k,2705) = rxt(k,449)*y(k,233)
         mat(k,1033) = rxt(k,449)*y(k,108)
         mat(k,342) = -(rxt(k,452)*y(k,263) + rxt(k,455)*y(k,70))
         mat(k,2186) = -rxt(k,452)*y(k,35)
         mat(k,2529) = -rxt(k,455)*y(k,35)
         mat(k,317) = -(rxt(k,548)*y(k,263))
         mat(k,2183) = -rxt(k,548)*y(k,36)
         mat(k,2694) = rxt(k,546)*y(k,234)
         mat(k,741) = rxt(k,546)*y(k,108)
         mat(k,115) = -(rxt(k,274)*y(k,259))
         mat(k,1856) = -rxt(k,274)*y(k,37)
         mat(k,168) = -(rxt(k,275)*y(k,259))
         mat(k,1861) = -rxt(k,275)*y(k,38)
         mat(k,173) = -(rxt(k,305)*y(k,259))
         mat(k,1862) = -rxt(k,305)*y(k,39)
         mat(k,129) = -(rxt(k,276)*y(k,259))
         mat(k,1858) = -rxt(k,276)*y(k,40)
         mat(k,178) = -(rxt(k,277)*y(k,259))
         mat(k,1863) = -rxt(k,277)*y(k,41)
         mat(k,133) = -(rxt(k,278)*y(k,259))
         mat(k,1859) = -rxt(k,278)*y(k,42)
         mat(k,183) = -(rxt(k,279)*y(k,259))
         mat(k,1864) = -rxt(k,279)*y(k,43)
         mat(k,137) = -(rxt(k,280)*y(k,259))
         mat(k,1860) = -rxt(k,280)*y(k,44)
         mat(k,560) = -(rxt(k,362)*y(k,70) + rxt(k,363)*y(k,263) + rxt(k,389)*y(k,259))
         mat(k,2536) = -rxt(k,362)*y(k,45)
         mat(k,2214) = -rxt(k,363)*y(k,45)
         mat(k,1874) = -rxt(k,389)*y(k,45)
         mat(k,141) = -(rxt(k,364)*y(k,263))
         mat(k,2157) = -rxt(k,364)*y(k,46)
         mat(k,355) = -(rxt(k,365)*y(k,70) + rxt(k,366)*y(k,263))
         mat(k,2530) = -rxt(k,365)*y(k,47)
         mat(k,2188) = -rxt(k,366)*y(k,47)
         mat(k,2342) = -(rxt(k,247)*y(k,70) + rxt(k,286)*y(k,17) + rxt(k,394)*y(k,108) &
                      + rxt(k,395)*y(k,160) + rxt(k,396)*y(k,168) + rxt(k,397) &
                      *y(k,263))
         mat(k,2574) = -rxt(k,247)*y(k,51)
         mat(k,2813) = -rxt(k,286)*y(k,51)
         mat(k,2783) = -rxt(k,394)*y(k,51)
         mat(k,1963) = -rxt(k,395)*y(k,51)
         mat(k,2511) = -rxt(k,396)*y(k,51)
         mat(k,2311) = -rxt(k,397)*y(k,51)
         mat(k,775) = .400_r8*rxt(k,496)*y(k,263)
         mat(k,1124) = .340_r8*rxt(k,580)*y(k,170)
         mat(k,431) = .500_r8*rxt(k,467)*y(k,160)
         mat(k,635) = rxt(k,418)*y(k,170)
         mat(k,1270) = .500_r8*rxt(k,447)*y(k,170)
         mat(k,733) = .500_r8*rxt(k,435)*y(k,263)
         mat(k,906) = rxt(k,402)*y(k,263)
         mat(k,474) = .300_r8*rxt(k,403)*y(k,263)
         mat(k,1670) = (rxt(k,411)+rxt(k,412))*y(k,259)
         mat(k,1248) = rxt(k,378)*y(k,236)
         mat(k,2843) = rxt(k,256)*y(k,236)
         mat(k,1292) = .800_r8*rxt(k,440)*y(k,263)
         mat(k,2783) = mat(k,2783) + .450_r8*rxt(k,483)*y(k,250) + .150_r8*rxt(k,462) &
                      *y(k,267)
         mat(k,988) = .910_r8*rxt(k,525)*y(k,170)
         mat(k,716) = .300_r8*rxt(k,516)*y(k,263)
         mat(k,1414) = .120_r8*rxt(k,478)*y(k,170)
         mat(k,677) = .500_r8*rxt(k,491)*y(k,263)
         mat(k,1069) = .340_r8*rxt(k,583)*y(k,170)
         mat(k,1525) = .600_r8*rxt(k,492)*y(k,170)
         mat(k,2126) = .100_r8*rxt(k,498)*y(k,227) + rxt(k,401)*y(k,236) &
                      + .500_r8*rxt(k,469)*y(k,239) + .500_r8*rxt(k,437)*y(k,241) &
                      + .920_r8*rxt(k,508)*y(k,243) + .250_r8*rxt(k,476)*y(k,248) &
                      + rxt(k,485)*y(k,250) + rxt(k,459)*y(k,266) + rxt(k,463) &
                      *y(k,267) + .340_r8*rxt(k,592)*y(k,268) + .320_r8*rxt(k,597) &
                      *y(k,269) + .250_r8*rxt(k,533)*y(k,272)
         mat(k,1963) = mat(k,1963) + .500_r8*rxt(k,467)*y(k,16) + rxt(k,509)*y(k,243) &
                      + .250_r8*rxt(k,475)*y(k,248) + rxt(k,486)*y(k,250)
         mat(k,2414) = .340_r8*rxt(k,580)*y(k,6) + rxt(k,418)*y(k,28) &
                      + .500_r8*rxt(k,447)*y(k,33) + .910_r8*rxt(k,525)*y(k,130) &
                      + .120_r8*rxt(k,478)*y(k,139) + .340_r8*rxt(k,583)*y(k,144) &
                      + .600_r8*rxt(k,492)*y(k,145)
         mat(k,645) = rxt(k,442)*y(k,263)
         mat(k,1236) = .680_r8*rxt(k,601)*y(k,263)
         mat(k,1142) = .100_r8*rxt(k,498)*y(k,158)
         mat(k,1007) = .700_r8*rxt(k,420)*y(k,236)
         mat(k,1042) = rxt(k,448)*y(k,236)
         mat(k,1575) = rxt(k,431)*y(k,236) + rxt(k,505)*y(k,243) + .250_r8*rxt(k,472) &
                      *y(k,248) + rxt(k,481)*y(k,250) + .250_r8*rxt(k,530)*y(k,272)
         mat(k,1746) = rxt(k,378)*y(k,68) + rxt(k,256)*y(k,74) + rxt(k,401)*y(k,158) &
                      + .700_r8*rxt(k,420)*y(k,232) + rxt(k,448)*y(k,233) + rxt(k,431) &
                      *y(k,235) + (4.000_r8*rxt(k,398)+2.000_r8*rxt(k,399))*y(k,236) &
                      + 1.500_r8*rxt(k,506)*y(k,243) + .750_r8*rxt(k,511)*y(k,244) &
                      + .800_r8*rxt(k,520)*y(k,245) + .880_r8*rxt(k,473)*y(k,248) &
                      + 2.000_r8*rxt(k,482)*y(k,250) + .750_r8*rxt(k,585)*y(k,258) &
                      + .800_r8*rxt(k,461)*y(k,267) + .930_r8*rxt(k,590)*y(k,268) &
                      + .950_r8*rxt(k,595)*y(k,269) + .800_r8*rxt(k,531)*y(k,272)
         mat(k,660) = .500_r8*rxt(k,469)*y(k,158)
         mat(k,884) = .500_r8*rxt(k,437)*y(k,158)
         mat(k,1448) = .920_r8*rxt(k,508)*y(k,158) + rxt(k,509)*y(k,160) + rxt(k,505) &
                      *y(k,235) + 1.500_r8*rxt(k,506)*y(k,236)
         mat(k,1481) = .750_r8*rxt(k,511)*y(k,236)
         mat(k,1402) = .800_r8*rxt(k,520)*y(k,236)
         mat(k,1503) = .250_r8*rxt(k,476)*y(k,158) + .250_r8*rxt(k,475)*y(k,160) &
                      + .250_r8*rxt(k,472)*y(k,235) + .880_r8*rxt(k,473)*y(k,236)
         mat(k,1543) = .450_r8*rxt(k,483)*y(k,108) + rxt(k,485)*y(k,158) + rxt(k,486) &
                      *y(k,160) + rxt(k,481)*y(k,235) + 2.000_r8*rxt(k,482)*y(k,236) &
                      + 4.000_r8*rxt(k,484)*y(k,250)
         mat(k,1225) = .750_r8*rxt(k,585)*y(k,236)
         mat(k,1891) = (rxt(k,411)+rxt(k,412))*y(k,64)
         mat(k,2311) = mat(k,2311) + .400_r8*rxt(k,496)*y(k,1) + .500_r8*rxt(k,435) &
                      *y(k,60) + rxt(k,402)*y(k,62) + .300_r8*rxt(k,403)*y(k,63) &
                      + .800_r8*rxt(k,440)*y(k,90) + .300_r8*rxt(k,516)*y(k,131) &
                      + .500_r8*rxt(k,491)*y(k,143) + rxt(k,442)*y(k,176) &
                      + .680_r8*rxt(k,601)*y(k,216)
         mat(k,916) = rxt(k,459)*y(k,158)
         mat(k,1362) = .150_r8*rxt(k,462)*y(k,108) + rxt(k,463)*y(k,158) &
                      + .800_r8*rxt(k,461)*y(k,236)
         mat(k,1309) = .340_r8*rxt(k,592)*y(k,158) + .930_r8*rxt(k,590)*y(k,236)
         mat(k,1160) = .320_r8*rxt(k,597)*y(k,158) + .950_r8*rxt(k,595)*y(k,236)
         mat(k,1380) = .250_r8*rxt(k,533)*y(k,158) + .250_r8*rxt(k,530)*y(k,235) &
                      + .800_r8*rxt(k,531)*y(k,236)
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
         mat(k,679) = -(rxt(k,367)*y(k,70) + rxt(k,368)*y(k,263) + rxt(k,390)*y(k,259))
         mat(k,2540) = -rxt(k,367)*y(k,52)
         mat(k,2229) = -rxt(k,368)*y(k,52)
         mat(k,1875) = -rxt(k,390)*y(k,52)
         mat(k,145) = -(rxt(k,369)*y(k,263))
         mat(k,2158) = -rxt(k,369)*y(k,53)
         mat(k,1274) = -(rxt(k,427)*y(k,160) + rxt(k,428)*y(k,263))
         mat(k,1930) = -rxt(k,427)*y(k,54)
         mat(k,2276) = -rxt(k,428)*y(k,54)
         mat(k,771) = .800_r8*rxt(k,496)*y(k,263)
         mat(k,427) = rxt(k,467)*y(k,160)
         mat(k,309) = rxt(k,423)*y(k,263)
         mat(k,399) = .500_r8*rxt(k,424)*y(k,263)
         mat(k,1257) = .500_r8*rxt(k,447)*y(k,170)
         mat(k,2750) = .200_r8*rxt(k,487)*y(k,252)
         mat(k,1509) = .100_r8*rxt(k,492)*y(k,170)
         mat(k,2096) = .400_r8*rxt(k,498)*y(k,227) + rxt(k,422)*y(k,232) &
                      + .270_r8*rxt(k,450)*y(k,233) + rxt(k,469)*y(k,239) + rxt(k,488) &
                      *y(k,252) + rxt(k,459)*y(k,266)
         mat(k,1930) = mat(k,1930) + rxt(k,467)*y(k,16)
         mat(k,2386) = .500_r8*rxt(k,447)*y(k,33) + .100_r8*rxt(k,492)*y(k,145)
         mat(k,1135) = .400_r8*rxt(k,498)*y(k,158)
         mat(k,1001) = rxt(k,422)*y(k,158) + 3.200_r8*rxt(k,419)*y(k,232) &
                      + .800_r8*rxt(k,420)*y(k,236)
         mat(k,1036) = .270_r8*rxt(k,450)*y(k,158)
         mat(k,1719) = .800_r8*rxt(k,420)*y(k,232)
         mat(k,656) = rxt(k,469)*y(k,158)
         mat(k,779) = .200_r8*rxt(k,487)*y(k,108) + rxt(k,488)*y(k,158)
         mat(k,2276) = mat(k,2276) + .800_r8*rxt(k,496)*y(k,1) + rxt(k,423)*y(k,30) &
                      + .500_r8*rxt(k,424)*y(k,31)
         mat(k,910) = rxt(k,459)*y(k,158)
         mat(k,451) = -(rxt(k,370)*y(k,70) + rxt(k,371)*y(k,263))
         mat(k,2534) = -rxt(k,370)*y(k,55)
         mat(k,2200) = -rxt(k,371)*y(k,55)
         mat(k,118) = -(rxt(k,429)*y(k,263))
         mat(k,2155) = -rxt(k,429)*y(k,56)
         mat(k,1171) = -(rxt(k,466)*y(k,263))
         mat(k,2268) = -rxt(k,466)*y(k,57)
         mat(k,770) = .800_r8*rxt(k,496)*y(k,263)
         mat(k,1114) = .520_r8*rxt(k,580)*y(k,170)
         mat(k,426) = .500_r8*rxt(k,467)*y(k,160)
         mat(k,1059) = .520_r8*rxt(k,583)*y(k,170)
         mat(k,2091) = .250_r8*rxt(k,498)*y(k,227) + .820_r8*rxt(k,450)*y(k,233) &
                      + .500_r8*rxt(k,469)*y(k,239) + .270_r8*rxt(k,592)*y(k,268) &
                      + .040_r8*rxt(k,597)*y(k,269)
         mat(k,1922) = .500_r8*rxt(k,467)*y(k,16)
         mat(k,2381) = .520_r8*rxt(k,580)*y(k,6) + .520_r8*rxt(k,583)*y(k,144)
         mat(k,1228) = .500_r8*rxt(k,601)*y(k,263)
         mat(k,1134) = .250_r8*rxt(k,498)*y(k,158)
         mat(k,1035) = .820_r8*rxt(k,450)*y(k,158) + .820_r8*rxt(k,448)*y(k,236)
         mat(k,1714) = .820_r8*rxt(k,448)*y(k,233) + .150_r8*rxt(k,590)*y(k,268) &
                      + .025_r8*rxt(k,595)*y(k,269)
         mat(k,655) = .500_r8*rxt(k,469)*y(k,158)
         mat(k,2268) = mat(k,2268) + .800_r8*rxt(k,496)*y(k,1) + .500_r8*rxt(k,601) &
                      *y(k,216)
         mat(k,1297) = .270_r8*rxt(k,592)*y(k,158) + .150_r8*rxt(k,590)*y(k,236)
         mat(k,1155) = .040_r8*rxt(k,597)*y(k,158) + .025_r8*rxt(k,595)*y(k,236)
         mat(k,1419) = -(rxt(k,453)*y(k,160) + rxt(k,454)*y(k,263))
         mat(k,1941) = -rxt(k,453)*y(k,58)
         mat(k,2287) = -rxt(k,454)*y(k,58)
         mat(k,2760) = .070_r8*rxt(k,550)*y(k,237) + .070_r8*rxt(k,556)*y(k,251)
         mat(k,1332) = rxt(k,456)*y(k,263)
         mat(k,1408) = .880_r8*rxt(k,478)*y(k,170)
         mat(k,1512) = .500_r8*rxt(k,492)*y(k,170)
         mat(k,2106) = .170_r8*rxt(k,551)*y(k,237) + .050_r8*rxt(k,514)*y(k,244) &
                      + .250_r8*rxt(k,476)*y(k,248) + .170_r8*rxt(k,557)*y(k,251) &
                      + .400_r8*rxt(k,567)*y(k,270) + .250_r8*rxt(k,533)*y(k,272) &
                      + .540_r8*rxt(k,573)*y(k,273) + .510_r8*rxt(k,576)*y(k,275)
         mat(k,1941) = mat(k,1941) + .050_r8*rxt(k,515)*y(k,244) + .250_r8*rxt(k,475) &
                      *y(k,248) + .250_r8*rxt(k,534)*y(k,272)
         mat(k,992) = rxt(k,457)*y(k,263)
         mat(k,2394) = .880_r8*rxt(k,478)*y(k,139) + .500_r8*rxt(k,492)*y(k,145)
         mat(k,1560) = .250_r8*rxt(k,472)*y(k,248) + .250_r8*rxt(k,530)*y(k,272)
         mat(k,1728) = .240_r8*rxt(k,473)*y(k,248) + .500_r8*rxt(k,461)*y(k,267) &
                      + .100_r8*rxt(k,531)*y(k,272)
         mat(k,870) = .070_r8*rxt(k,550)*y(k,108) + .170_r8*rxt(k,551)*y(k,158)
         mat(k,1469) = .050_r8*rxt(k,514)*y(k,158) + .050_r8*rxt(k,515)*y(k,160)
         mat(k,1493) = .250_r8*rxt(k,476)*y(k,158) + .250_r8*rxt(k,475)*y(k,160) &
                      + .250_r8*rxt(k,472)*y(k,235) + .240_r8*rxt(k,473)*y(k,236)
         mat(k,1022) = .070_r8*rxt(k,556)*y(k,108) + .170_r8*rxt(k,557)*y(k,158)
         mat(k,2287) = mat(k,2287) + rxt(k,456)*y(k,115) + rxt(k,457)*y(k,161)
         mat(k,1356) = .500_r8*rxt(k,461)*y(k,236)
         mat(k,838) = .400_r8*rxt(k,567)*y(k,158)
         mat(k,1372) = .250_r8*rxt(k,533)*y(k,158) + .250_r8*rxt(k,534)*y(k,160) &
                      + .250_r8*rxt(k,530)*y(k,235) + .100_r8*rxt(k,531)*y(k,236)
         mat(k,862) = .540_r8*rxt(k,573)*y(k,158)
         mat(k,604) = .510_r8*rxt(k,576)*y(k,158)
         mat(k,795) = -(rxt(k,434)*y(k,263))
         mat(k,2241) = -rxt(k,434)*y(k,59)
         mat(k,1252) = .120_r8*rxt(k,447)*y(k,170)
         mat(k,2728) = .150_r8*rxt(k,432)*y(k,235) + .150_r8*rxt(k,483)*y(k,250)
         mat(k,2369) = .120_r8*rxt(k,447)*y(k,33)
         mat(k,1551) = .150_r8*rxt(k,432)*y(k,108) + .100_r8*rxt(k,431)*y(k,236)
         mat(k,1706) = .100_r8*rxt(k,431)*y(k,235)
         mat(k,1532) = .150_r8*rxt(k,483)*y(k,108)
         mat(k,728) = -(rxt(k,435)*y(k,263))
         mat(k,2234) = -rxt(k,435)*y(k,60)
         mat(k,2723) = .360_r8*rxt(k,432)*y(k,235) + .400_r8*rxt(k,483)*y(k,250)
         mat(k,1550) = .360_r8*rxt(k,432)*y(k,108)
         mat(k,1531) = .400_r8*rxt(k,483)*y(k,108)
         mat(k,416) = -(rxt(k,372)*y(k,70) + rxt(k,373)*y(k,263))
         mat(k,2533) = -rxt(k,372)*y(k,61)
         mat(k,2197) = -rxt(k,373)*y(k,61)
         mat(k,904) = -(rxt(k,402)*y(k,263))
         mat(k,2251) = -rxt(k,402)*y(k,62)
         mat(k,999) = .300_r8*rxt(k,420)*y(k,236)
         mat(k,1707) = .300_r8*rxt(k,420)*y(k,232) + 2.000_r8*rxt(k,399)*y(k,236) &
                      + .250_r8*rxt(k,506)*y(k,243) + .250_r8*rxt(k,511)*y(k,244) &
                      + .200_r8*rxt(k,520)*y(k,245) + .250_r8*rxt(k,473)*y(k,248) &
                      + .250_r8*rxt(k,585)*y(k,258) + .500_r8*rxt(k,461)*y(k,267) &
                      + .250_r8*rxt(k,590)*y(k,268) + .250_r8*rxt(k,595)*y(k,269) &
                      + .300_r8*rxt(k,531)*y(k,272)
         mat(k,1429) = .250_r8*rxt(k,506)*y(k,236)
         mat(k,1458) = .250_r8*rxt(k,511)*y(k,236)
         mat(k,1385) = .200_r8*rxt(k,520)*y(k,236)
         mat(k,1487) = .250_r8*rxt(k,473)*y(k,236)
         mat(k,1214) = .250_r8*rxt(k,585)*y(k,236)
         mat(k,1353) = .500_r8*rxt(k,461)*y(k,236)
         mat(k,1296) = .250_r8*rxt(k,590)*y(k,236)
         mat(k,1152) = .250_r8*rxt(k,595)*y(k,236)
         mat(k,1366) = .300_r8*rxt(k,531)*y(k,236)
         mat(k,471) = -(rxt(k,403)*y(k,263))
         mat(k,2203) = -rxt(k,403)*y(k,63)
         mat(k,2702) = rxt(k,400)*y(k,236)
         mat(k,1704) = rxt(k,400)*y(k,108)
         mat(k,1661) = -(rxt(k,248)*y(k,70) + rxt(k,352)*y(k,89) + rxt(k,404)*y(k,263) &
                      + (rxt(k,410) + rxt(k,411) + rxt(k,412)) * y(k,259))
         mat(k,2562) = -rxt(k,248)*y(k,64)
         mat(k,1012) = -rxt(k,352)*y(k,64)
         mat(k,2299) = -rxt(k,404)*y(k,64)
         mat(k,1879) = -(rxt(k,410) + rxt(k,411) + rxt(k,412)) * y(k,64)
         mat(k,1263) = .100_r8*rxt(k,447)*y(k,170)
         mat(k,2403) = .100_r8*rxt(k,447)*y(k,33)
         mat(k,149) = -(rxt(k,374)*y(k,263))
         mat(k,2159) = -rxt(k,374)*y(k,65)
         mat(k,501) = -(rxt(k,306)*y(k,259) + rxt(k,375)*y(k,70) + rxt(k,376)*y(k,263))
         mat(k,1873) = -rxt(k,306)*y(k,66)
         mat(k,2535) = -rxt(k,375)*y(k,66)
         mat(k,2207) = -rxt(k,376)*y(k,66)
         mat(k,121) = -(rxt(k,377)*y(k,263))
         mat(k,2156) = -rxt(k,377)*y(k,67)
         mat(k,1240) = -((rxt(k,378) + rxt(k,379)) * y(k,236) + (rxt(k,380) + rxt(k,381) &
                      ) * y(k,108) + rxt(k,382)*y(k,158) + rxt(k,383)*y(k,160))
         mat(k,1718) = -(rxt(k,378) + rxt(k,379)) * y(k,68)
         mat(k,2749) = -(rxt(k,380) + rxt(k,381)) * y(k,68)
         mat(k,2095) = -rxt(k,382)*y(k,68)
         mat(k,1928) = -rxt(k,383)*y(k,68)
         mat(k,356) = rxt(k,365)*y(k,70) + rxt(k,366)*y(k,263)
         mat(k,2552) = rxt(k,365)*y(k,47)
         mat(k,2274) = rxt(k,366)*y(k,47)
         mat(k,409) = -(rxt(k,384)*y(k,70) + rxt(k,385)*y(k,263))
         mat(k,2532) = -rxt(k,384)*y(k,69)
         mat(k,2196) = -rxt(k,385)*y(k,69)
         mat(k,2579) = -(rxt(k,247)*y(k,51) + rxt(k,248)*y(k,64) + rxt(k,249)*y(k,93) &
                      + rxt(k,250)*y(k,95) + (rxt(k,251) + rxt(k,252)) * y(k,108) &
                      + rxt(k,253)*y(k,159) + rxt(k,255)*y(k,170) + rxt(k,262)*y(k,75) &
                      + rxt(k,271)*y(k,111) + rxt(k,297)*y(k,22) + rxt(k,358)*y(k,26) &
                      + rxt(k,360)*y(k,29) + rxt(k,362)*y(k,45) + rxt(k,365)*y(k,47) &
                      + rxt(k,367)*y(k,52) + rxt(k,370)*y(k,55) + rxt(k,372)*y(k,61) &
                      + rxt(k,375)*y(k,66) + rxt(k,425)*y(k,32) + rxt(k,455)*y(k,35) &
                      + (rxt(k,603) + rxt(k,604)) * y(k,83))
         mat(k,2347) = -rxt(k,247)*y(k,70)
         mat(k,1674) = -rxt(k,248)*y(k,70)
         mat(k,1625) = -rxt(k,249)*y(k,70)
         mat(k,703) = -rxt(k,250)*y(k,70)
         mat(k,2788) = -(rxt(k,251) + rxt(k,252)) * y(k,70)
         mat(k,2026) = -rxt(k,253)*y(k,70)
         mat(k,2419) = -rxt(k,255)*y(k,70)
         mat(k,1082) = -rxt(k,262)*y(k,70)
         mat(k,1826) = -rxt(k,271)*y(k,70)
         mat(k,969) = -rxt(k,297)*y(k,70)
         mat(k,220) = -rxt(k,358)*y(k,70)
         mat(k,297) = -rxt(k,360)*y(k,70)
         mat(k,564) = -rxt(k,362)*y(k,70)
         mat(k,359) = -rxt(k,365)*y(k,70)
         mat(k,684) = -rxt(k,367)*y(k,70)
         mat(k,456) = -rxt(k,370)*y(k,70)
         mat(k,420) = -rxt(k,372)*y(k,70)
         mat(k,505) = -rxt(k,375)*y(k,70)
         mat(k,340) = -rxt(k,425)*y(k,70)
         mat(k,346) = -rxt(k,455)*y(k,70)
         mat(k,1183) = -(rxt(k,603) + rxt(k,604)) * y(k,70)
         mat(k,2642) = rxt(k,292)*y(k,74)
         mat(k,220) = mat(k,220) + 5.000_r8*rxt(k,358)*y(k,70) + 3.060_r8*rxt(k,359) &
                      *y(k,263)
         mat(k,297) = mat(k,297) + 2.000_r8*rxt(k,360)*y(k,70) + 2.000_r8*rxt(k,361) &
                      *y(k,263)
         mat(k,117) = 4.000_r8*rxt(k,274)*y(k,259)
         mat(k,171) = rxt(k,275)*y(k,259)
         mat(k,132) = 2.000_r8*rxt(k,276)*y(k,259)
         mat(k,182) = 2.000_r8*rxt(k,277)*y(k,259)
         mat(k,136) = 2.000_r8*rxt(k,278)*y(k,259)
         mat(k,187) = rxt(k,279)*y(k,259)
         mat(k,140) = 2.000_r8*rxt(k,280)*y(k,259)
         mat(k,143) = rxt(k,364)*y(k,263)
         mat(k,147) = 3.000_r8*rxt(k,369)*y(k,263)
         mat(k,456) = mat(k,456) + rxt(k,371)*y(k,263)
         mat(k,151) = rxt(k,374)*y(k,263)
         mat(k,123) = 2.000_r8*rxt(k,377)*y(k,263)
         mat(k,1249) = rxt(k,381)*y(k,108) + 2.000_r8*rxt(k,382)*y(k,158) &
                      + 2.000_r8*rxt(k,383)*y(k,160) + 2.000_r8*rxt(k,378)*y(k,236)
         mat(k,414) = rxt(k,385)*y(k,263)
         mat(k,2579) = mat(k,2579) + 5.000_r8*rxt(k,358)*y(k,26) + 2.000_r8*rxt(k,360) &
                      *y(k,29)
         mat(k,2848) = rxt(k,292)*y(k,21) + (4.000_r8*rxt(k,257)+2.000_r8*rxt(k,259)) &
                      *y(k,74) + rxt(k,329)*y(k,128) + rxt(k,261)*y(k,158) &
                      + rxt(k,266)*y(k,168) + rxt(k,614)*y(k,187) + rxt(k,256) &
                      *y(k,236) + rxt(k,267)*y(k,263)
         mat(k,266) = rxt(k,357)*y(k,259)
         mat(k,262) = rxt(k,391)*y(k,259) + rxt(k,386)*y(k,263)
         mat(k,271) = rxt(k,392)*y(k,259) + rxt(k,387)*y(k,263)
         mat(k,353) = rxt(k,393)*y(k,259) + rxt(k,388)*y(k,263)
         mat(k,1849) = rxt(k,269)*y(k,168) + rxt(k,281)*y(k,259) + rxt(k,270)*y(k,263)
         mat(k,2788) = mat(k,2788) + rxt(k,381)*y(k,68)
         mat(k,2612) = rxt(k,329)*y(k,74)
         mat(k,2131) = 2.000_r8*rxt(k,382)*y(k,68) + rxt(k,261)*y(k,74)
         mat(k,1968) = 2.000_r8*rxt(k,383)*y(k,68)
         mat(k,2516) = rxt(k,266)*y(k,74) + rxt(k,269)*y(k,102)
         mat(k,1650) = rxt(k,614)*y(k,74)
         mat(k,1751) = 2.000_r8*rxt(k,378)*y(k,68) + rxt(k,256)*y(k,74)
         mat(k,1896) = 4.000_r8*rxt(k,274)*y(k,37) + rxt(k,275)*y(k,38) &
                      + 2.000_r8*rxt(k,276)*y(k,40) + 2.000_r8*rxt(k,277)*y(k,41) &
                      + 2.000_r8*rxt(k,278)*y(k,42) + rxt(k,279)*y(k,43) &
                      + 2.000_r8*rxt(k,280)*y(k,44) + rxt(k,357)*y(k,81) + rxt(k,391) &
                      *y(k,99) + rxt(k,392)*y(k,100) + rxt(k,393)*y(k,101) &
                      + rxt(k,281)*y(k,102)
         mat(k,2316) = 3.060_r8*rxt(k,359)*y(k,26) + 2.000_r8*rxt(k,361)*y(k,29) &
                      + rxt(k,364)*y(k,46) + 3.000_r8*rxt(k,369)*y(k,53) + rxt(k,371) &
                      *y(k,55) + rxt(k,374)*y(k,65) + 2.000_r8*rxt(k,377)*y(k,67) &
                      + rxt(k,385)*y(k,69) + rxt(k,267)*y(k,74) + rxt(k,386)*y(k,99) &
                      + rxt(k,387)*y(k,100) + rxt(k,388)*y(k,101) + rxt(k,270) &
                      *y(k,102)
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
         mat(k,2525) = rxt(k,262)*y(k,75)
         mat(k,2827) = 2.000_r8*rxt(k,258)*y(k,74)
         mat(k,1073) = rxt(k,262)*y(k,70) + (rxt(k,712)+rxt(k,721)+rxt(k,730)) &
                      *y(k,102)
         mat(k,1833) = (rxt(k,712)+rxt(k,721)+rxt(k,730))*y(k,75) + (rxt(k,631) &
                       +rxt(k,702)+rxt(k,713)+rxt(k,722))*y(k,111)
         mat(k,1809) = (rxt(k,631)+rxt(k,702)+rxt(k,713)+rxt(k,722))*y(k,102)
         mat(k,2826) = 2.000_r8*rxt(k,283)*y(k,74)
         mat(k,625) = -(rxt(k,254)*y(k,263))
         mat(k,2222) = -rxt(k,254)*y(k,73)
         mat(k,2538) = rxt(k,253)*y(k,159)
         mat(k,1836) = rxt(k,647)*y(k,148)
         mat(k,436) = rxt(k,647)*y(k,102)
         mat(k,1988) = rxt(k,253)*y(k,70)
         mat(k,2854) = -(rxt(k,256)*y(k,236) + (4._r8*rxt(k,257) + 4._r8*rxt(k,258) &
                      + 4._r8*rxt(k,259) + 4._r8*rxt(k,283)) * y(k,74) + rxt(k,260) &
                      *y(k,108) + rxt(k,261)*y(k,158) + rxt(k,263)*y(k,159) + rxt(k,266) &
                      *y(k,168) + (rxt(k,267) + rxt(k,268)) * y(k,263) + (rxt(k,291) &
                      + rxt(k,292) + rxt(k,293)) * y(k,21) + (rxt(k,328) + rxt(k,329) &
                      + rxt(k,330)) * y(k,128) + rxt(k,614)*y(k,187))
         mat(k,1753) = -rxt(k,256)*y(k,74)
         mat(k,2794) = -rxt(k,260)*y(k,74)
         mat(k,2137) = -rxt(k,261)*y(k,74)
         mat(k,2032) = -rxt(k,263)*y(k,74)
         mat(k,2522) = -rxt(k,266)*y(k,74)
         mat(k,2322) = -(rxt(k,267) + rxt(k,268)) * y(k,74)
         mat(k,2648) = -(rxt(k,291) + rxt(k,292) + rxt(k,293)) * y(k,74)
         mat(k,2618) = -(rxt(k,328) + rxt(k,329) + rxt(k,330)) * y(k,74)
         mat(k,1654) = -rxt(k,614)*y(k,74)
         mat(k,2585) = rxt(k,252)*y(k,108) + rxt(k,271)*y(k,111) + rxt(k,255)*y(k,170)
         mat(k,1083) = rxt(k,264)*y(k,168)
         mat(k,1854) = rxt(k,282)*y(k,259)
         mat(k,2794) = mat(k,2794) + rxt(k,252)*y(k,70)
         mat(k,1831) = rxt(k,271)*y(k,70) + rxt(k,272)*y(k,168) + rxt(k,273)*y(k,263)
         mat(k,2522) = mat(k,2522) + rxt(k,264)*y(k,75) + rxt(k,272)*y(k,111)
         mat(k,2425) = rxt(k,255)*y(k,70)
         mat(k,544) = rxt(k,619)*y(k,187)
         mat(k,1654) = mat(k,1654) + rxt(k,619)*y(k,172)
         mat(k,1902) = rxt(k,282)*y(k,102)
         mat(k,2322) = mat(k,2322) + rxt(k,273)*y(k,111)
         mat(k,1074) = -(rxt(k,262)*y(k,70) + rxt(k,264)*y(k,168) + rxt(k,265) &
                      *y(k,263) + (rxt(k,712) + rxt(k,721) + rxt(k,730)) * y(k,102))
         mat(k,2547) = -rxt(k,262)*y(k,75)
         mat(k,2490) = -rxt(k,264)*y(k,75)
         mat(k,2261) = -rxt(k,265)*y(k,75)
         mat(k,1837) = -(rxt(k,712) + rxt(k,721) + rxt(k,730)) * y(k,75)
         mat(k,2831) = rxt(k,263)*y(k,159)
         mat(k,1997) = rxt(k,263)*y(k,74)
         mat(k,1283) = -(rxt(k,414)*y(k,263))
         mat(k,2277) = -rxt(k,414)*y(k,77)
         mat(k,1117) = .230_r8*rxt(k,580)*y(k,170)
         mat(k,2800) = rxt(k,286)*y(k,51)
         mat(k,333) = .350_r8*rxt(k,416)*y(k,263)
         mat(k,633) = .630_r8*rxt(k,418)*y(k,170)
         mat(k,1258) = .560_r8*rxt(k,447)*y(k,170)
         mat(k,2326) = rxt(k,286)*y(k,17) + rxt(k,247)*y(k,70) + rxt(k,395)*y(k,160) &
                      + rxt(k,396)*y(k,168) + rxt(k,397)*y(k,263)
         mat(k,452) = rxt(k,370)*y(k,70)
         mat(k,1418) = rxt(k,453)*y(k,160) + rxt(k,454)*y(k,263)
         mat(k,1241) = rxt(k,381)*y(k,108) + rxt(k,382)*y(k,158) + rxt(k,383)*y(k,160) + ( &
                      + rxt(k,378)+rxt(k,379))*y(k,236)
         mat(k,2554) = rxt(k,247)*y(k,51) + rxt(k,370)*y(k,55)
         mat(k,1587) = rxt(k,755)*y(k,264)
         mat(k,1146) = rxt(k,441)*y(k,263)
         mat(k,2751) = rxt(k,381)*y(k,68) + .070_r8*rxt(k,550)*y(k,237) &
                      + .160_r8*rxt(k,553)*y(k,249) + .140_r8*rxt(k,556)*y(k,251)
         mat(k,978) = .620_r8*rxt(k,525)*y(k,170)
         mat(k,1406) = .650_r8*rxt(k,478)*y(k,170)
         mat(k,1062) = .230_r8*rxt(k,583)*y(k,170)
         mat(k,1510) = .560_r8*rxt(k,492)*y(k,170)
         mat(k,2097) = rxt(k,382)*y(k,68) + .170_r8*rxt(k,551)*y(k,237) &
                      + .220_r8*rxt(k,476)*y(k,248) + .400_r8*rxt(k,554)*y(k,249) &
                      + .350_r8*rxt(k,557)*y(k,251) + .225_r8*rxt(k,592)*y(k,268) &
                      + .250_r8*rxt(k,533)*y(k,272)
         mat(k,1931) = rxt(k,395)*y(k,51) + rxt(k,453)*y(k,58) + rxt(k,383)*y(k,68) &
                      + .220_r8*rxt(k,475)*y(k,248) + .500_r8*rxt(k,534)*y(k,272)
         mat(k,2493) = rxt(k,396)*y(k,51) + rxt(k,608)*y(k,173)
         mat(k,2387) = .230_r8*rxt(k,580)*y(k,6) + .630_r8*rxt(k,418)*y(k,28) &
                      + .560_r8*rxt(k,447)*y(k,33) + .620_r8*rxt(k,525)*y(k,130) &
                      + .650_r8*rxt(k,478)*y(k,139) + .230_r8*rxt(k,583)*y(k,144) &
                      + .560_r8*rxt(k,492)*y(k,145)
         mat(k,446) = rxt(k,608)*y(k,168) + rxt(k,609)*y(k,263)
         mat(k,1230) = .700_r8*rxt(k,601)*y(k,263)
         mat(k,1554) = .220_r8*rxt(k,472)*y(k,248) + .250_r8*rxt(k,530)*y(k,272)
         mat(k,1720) = (rxt(k,378)+rxt(k,379))*y(k,68) + .110_r8*rxt(k,473)*y(k,248) &
                      + .125_r8*rxt(k,590)*y(k,268) + .200_r8*rxt(k,531)*y(k,272)
         mat(k,869) = .070_r8*rxt(k,550)*y(k,108) + .170_r8*rxt(k,551)*y(k,158)
         mat(k,1488) = .220_r8*rxt(k,476)*y(k,158) + .220_r8*rxt(k,475)*y(k,160) &
                      + .220_r8*rxt(k,472)*y(k,235) + .110_r8*rxt(k,473)*y(k,236)
         mat(k,824) = .160_r8*rxt(k,553)*y(k,108) + .400_r8*rxt(k,554)*y(k,158)
         mat(k,1021) = .140_r8*rxt(k,556)*y(k,108) + .350_r8*rxt(k,557)*y(k,158)
         mat(k,2277) = mat(k,2277) + .350_r8*rxt(k,416)*y(k,27) + rxt(k,397)*y(k,51) &
                      + rxt(k,454)*y(k,58) + rxt(k,441)*y(k,91) + rxt(k,609)*y(k,173) &
                      + .700_r8*rxt(k,601)*y(k,216)
         mat(k,890) = rxt(k,755)*y(k,78)
         mat(k,1299) = .225_r8*rxt(k,592)*y(k,158) + .125_r8*rxt(k,590)*y(k,236)
         mat(k,1368) = .250_r8*rxt(k,533)*y(k,158) + .500_r8*rxt(k,534)*y(k,160) &
                      + .250_r8*rxt(k,530)*y(k,235) + .200_r8*rxt(k,531)*y(k,236)
         mat(k,1588) = -(rxt(k,755)*y(k,264))
         mat(k,891) = -rxt(k,755)*y(k,78)
         mat(k,1121) = .270_r8*rxt(k,580)*y(k,170)
         mat(k,1262) = .200_r8*rxt(k,447)*y(k,170)
         mat(k,796) = rxt(k,434)*y(k,263)
         mat(k,730) = .500_r8*rxt(k,435)*y(k,263)
         mat(k,1284) = rxt(k,414)*y(k,263)
         mat(k,1290) = .800_r8*rxt(k,440)*y(k,263)
         mat(k,1147) = rxt(k,441)*y(k,263)
         mat(k,1029) = rxt(k,406)*y(k,263)
         mat(k,2767) = .490_r8*rxt(k,432)*y(k,235) + .450_r8*rxt(k,483)*y(k,250)
         mat(k,673) = .500_r8*rxt(k,491)*y(k,263)
         mat(k,1066) = .270_r8*rxt(k,583)*y(k,170)
         mat(k,1517) = .100_r8*rxt(k,492)*y(k,170)
         mat(k,2113) = rxt(k,433)*y(k,235) + .900_r8*rxt(k,592)*y(k,268)
         mat(k,2401) = .270_r8*rxt(k,580)*y(k,6) + .200_r8*rxt(k,447)*y(k,33) &
                      + .270_r8*rxt(k,583)*y(k,144) + .100_r8*rxt(k,492)*y(k,145)
         mat(k,1233) = 1.800_r8*rxt(k,601)*y(k,263)
         mat(k,1567) = .490_r8*rxt(k,432)*y(k,108) + rxt(k,433)*y(k,158) &
                      + 4.000_r8*rxt(k,430)*y(k,235) + .900_r8*rxt(k,431)*y(k,236) &
                      + rxt(k,505)*y(k,243) + 2.000_r8*rxt(k,481)*y(k,250) &
                      + rxt(k,530)*y(k,272)
         mat(k,1735) = .900_r8*rxt(k,431)*y(k,235) + rxt(k,482)*y(k,250) &
                      + .500_r8*rxt(k,590)*y(k,268)
         mat(k,1442) = rxt(k,505)*y(k,235)
         mat(k,1537) = .450_r8*rxt(k,483)*y(k,108) + 2.000_r8*rxt(k,481)*y(k,235) &
                      + rxt(k,482)*y(k,236) + 4.000_r8*rxt(k,484)*y(k,250)
         mat(k,2294) = rxt(k,434)*y(k,59) + .500_r8*rxt(k,435)*y(k,60) + rxt(k,414) &
                      *y(k,77) + .800_r8*rxt(k,440)*y(k,90) + rxt(k,441)*y(k,91) &
                      + rxt(k,406)*y(k,104) + .500_r8*rxt(k,491)*y(k,143) &
                      + 1.800_r8*rxt(k,601)*y(k,216)
         mat(k,1304) = .900_r8*rxt(k,592)*y(k,158) + .500_r8*rxt(k,590)*y(k,236)
         mat(k,1374) = rxt(k,530)*y(k,235)
         mat(k,218) = .470_r8*rxt(k,359)*y(k,263)
         mat(k,1239) = rxt(k,380)*y(k,108) + rxt(k,379)*y(k,236)
         mat(k,408) = rxt(k,384)*y(k,70) + rxt(k,385)*y(k,263)
         mat(k,2531) = rxt(k,384)*y(k,69)
         mat(k,2700) = rxt(k,380)*y(k,68)
         mat(k,1702) = rxt(k,379)*y(k,68)
         mat(k,2195) = .470_r8*rxt(k,359)*y(k,26) + rxt(k,385)*y(k,69)
         mat(k,272) = -(rxt(k,356)*y(k,259))
         mat(k,1870) = -rxt(k,356)*y(k,80)
         mat(k,169) = rxt(k,275)*y(k,259)
         mat(k,174) = rxt(k,305)*y(k,259)
         mat(k,180) = rxt(k,277)*y(k,259)
         mat(k,134) = 2.000_r8*rxt(k,278)*y(k,259)
         mat(k,184) = 2.000_r8*rxt(k,279)*y(k,259)
         mat(k,138) = rxt(k,280)*y(k,259)
         mat(k,126) = 2.000_r8*rxt(k,307)*y(k,259)
         mat(k,268) = rxt(k,392)*y(k,259) + rxt(k,387)*y(k,263)
         mat(k,348) = rxt(k,393)*y(k,259) + rxt(k,388)*y(k,263)
         mat(k,1870) = mat(k,1870) + rxt(k,275)*y(k,38) + rxt(k,305)*y(k,39) &
                      + rxt(k,277)*y(k,41) + 2.000_r8*rxt(k,278)*y(k,42) &
                      + 2.000_r8*rxt(k,279)*y(k,43) + rxt(k,280)*y(k,44) &
                      + 2.000_r8*rxt(k,307)*y(k,94) + rxt(k,392)*y(k,100) + rxt(k,393) &
                      *y(k,101)
         mat(k,2175) = rxt(k,387)*y(k,100) + rxt(k,388)*y(k,101)
         mat(k,263) = -(rxt(k,357)*y(k,259))
         mat(k,1868) = -rxt(k,357)*y(k,81)
         mat(k,130) = rxt(k,276)*y(k,259)
         mat(k,179) = rxt(k,277)*y(k,259)
         mat(k,259) = rxt(k,391)*y(k,259) + rxt(k,386)*y(k,263)
         mat(k,1868) = mat(k,1868) + rxt(k,276)*y(k,40) + rxt(k,277)*y(k,41) &
                      + rxt(k,391)*y(k,99)
         mat(k,2173) = rxt(k,386)*y(k,99)
         mat(k,231) = -(rxt(k,549)*y(k,263))
         mat(k,2167) = -rxt(k,549)*y(k,82)
         mat(k,225) = .180_r8*rxt(k,569)*y(k,263)
         mat(k,2167) = mat(k,2167) + .180_r8*rxt(k,569)*y(k,218)
         mat(k,1177) = -(rxt(k,602)*y(k,21) + (rxt(k,603) + rxt(k,604)) * y(k,70) &
                      + rxt(k,605)*y(k,128) + rxt(k,606)*y(k,160) + (rxt(k,607) &
                      + rxt(k,621)) * y(k,263))
         mat(k,2624) = -rxt(k,602)*y(k,83)
         mat(k,2550) = -(rxt(k,603) + rxt(k,604)) * y(k,83)
         mat(k,2594) = -rxt(k,605)*y(k,83)
         mat(k,1923) = -rxt(k,606)*y(k,83)
         mat(k,2269) = -(rxt(k,607) + rxt(k,621)) * y(k,83)
         mat(k,2692) = rxt(k,436)*y(k,241)
         mat(k,876) = rxt(k,436)*y(k,108)
         mat(k,1010) = -(rxt(k,352)*y(k,64) + rxt(k,353)*y(k,93) + rxt(k,354)*y(k,276) &
                      + rxt(k,355)*y(k,107))
         mat(k,1657) = -rxt(k,352)*y(k,89)
         mat(k,1614) = -rxt(k,353)*y(k,89)
         mat(k,2861) = -rxt(k,354)*y(k,89)
         mat(k,2452) = -rxt(k,355)*y(k,89)
         mat(k,175) = rxt(k,305)*y(k,259)
         mat(k,185) = rxt(k,279)*y(k,259)
         mat(k,273) = 2.000_r8*rxt(k,356)*y(k,259)
         mat(k,264) = rxt(k,357)*y(k,259)
         mat(k,1876) = rxt(k,305)*y(k,39) + rxt(k,279)*y(k,43) + 2.000_r8*rxt(k,356) &
                      *y(k,80) + rxt(k,357)*y(k,81)
         mat(k,1289) = -(rxt(k,440)*y(k,263))
         mat(k,2278) = -rxt(k,440)*y(k,90)
         mat(k,711) = .700_r8*rxt(k,516)*y(k,263)
         mat(k,664) = .500_r8*rxt(k,517)*y(k,263)
         mat(k,467) = rxt(k,528)*y(k,263)
         mat(k,2098) = .050_r8*rxt(k,514)*y(k,244) + .530_r8*rxt(k,476)*y(k,248) &
                      + .225_r8*rxt(k,592)*y(k,268) + .250_r8*rxt(k,533)*y(k,272)
         mat(k,1932) = .050_r8*rxt(k,515)*y(k,244) + .530_r8*rxt(k,475)*y(k,248) &
                      + .250_r8*rxt(k,534)*y(k,272)
         mat(k,1769) = rxt(k,439)*y(k,240)
         mat(k,1555) = .530_r8*rxt(k,472)*y(k,248) + .250_r8*rxt(k,530)*y(k,272)
         mat(k,1721) = .260_r8*rxt(k,473)*y(k,248) + .125_r8*rxt(k,590)*y(k,268) &
                      + .100_r8*rxt(k,531)*y(k,272)
         mat(k,546) = rxt(k,439)*y(k,169)
         mat(k,1463) = .050_r8*rxt(k,514)*y(k,158) + .050_r8*rxt(k,515)*y(k,160)
         mat(k,1489) = .530_r8*rxt(k,476)*y(k,158) + .530_r8*rxt(k,475)*y(k,160) &
                      + .530_r8*rxt(k,472)*y(k,235) + .260_r8*rxt(k,473)*y(k,236)
         mat(k,2278) = mat(k,2278) + .700_r8*rxt(k,516)*y(k,131) + .500_r8*rxt(k,517) &
                      *y(k,132) + rxt(k,528)*y(k,149)
         mat(k,1300) = .225_r8*rxt(k,592)*y(k,158) + .125_r8*rxt(k,590)*y(k,236)
         mat(k,1369) = .250_r8*rxt(k,533)*y(k,158) + .250_r8*rxt(k,534)*y(k,160) &
                      + .250_r8*rxt(k,530)*y(k,235) + .100_r8*rxt(k,531)*y(k,236)
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
         mat(k,1145) = -(rxt(k,441)*y(k,263))
         mat(k,2265) = -rxt(k,441)*y(k,91)
         mat(k,332) = .650_r8*rxt(k,416)*y(k,263)
         mat(k,1287) = .200_r8*rxt(k,440)*y(k,263)
         mat(k,2743) = .160_r8*rxt(k,553)*y(k,249) + .070_r8*rxt(k,556)*y(k,251)
         mat(k,1199) = rxt(k,529)*y(k,263)
         mat(k,2088) = rxt(k,540)*y(k,229) + .050_r8*rxt(k,514)*y(k,244) &
                      + .400_r8*rxt(k,554)*y(k,249) + .170_r8*rxt(k,557)*y(k,251) &
                      + .700_r8*rxt(k,560)*y(k,265) + .600_r8*rxt(k,567)*y(k,270) &
                      + .250_r8*rxt(k,533)*y(k,272) + .340_r8*rxt(k,573)*y(k,273) &
                      + .170_r8*rxt(k,576)*y(k,275)
         mat(k,1919) = .050_r8*rxt(k,515)*y(k,244) + .250_r8*rxt(k,534)*y(k,272)
         mat(k,578) = rxt(k,540)*y(k,158)
         mat(k,1552) = .250_r8*rxt(k,530)*y(k,272)
         mat(k,1711) = .100_r8*rxt(k,531)*y(k,272)
         mat(k,1461) = .050_r8*rxt(k,514)*y(k,158) + .050_r8*rxt(k,515)*y(k,160)
         mat(k,823) = .160_r8*rxt(k,553)*y(k,108) + .400_r8*rxt(k,554)*y(k,158)
         mat(k,1020) = .070_r8*rxt(k,556)*y(k,108) + .170_r8*rxt(k,557)*y(k,158)
         mat(k,2265) = mat(k,2265) + .650_r8*rxt(k,416)*y(k,27) + .200_r8*rxt(k,440) &
                      *y(k,90) + rxt(k,529)*y(k,150)
         mat(k,535) = .700_r8*rxt(k,560)*y(k,158)
         mat(k,836) = .600_r8*rxt(k,567)*y(k,158)
         mat(k,1367) = .250_r8*rxt(k,533)*y(k,158) + .250_r8*rxt(k,534)*y(k,160) &
                      + .250_r8*rxt(k,530)*y(k,235) + .100_r8*rxt(k,531)*y(k,236)
         mat(k,860) = .340_r8*rxt(k,573)*y(k,158)
         mat(k,603) = .170_r8*rxt(k,576)*y(k,158)
         mat(k,2440) = -((rxt(k,202) + rxt(k,203) + rxt(k,204)) * y(k,108) + rxt(k,205) &
                      *y(k,169) + rxt(k,208)*y(k,170))
         mat(k,2785) = -(rxt(k,202) + rxt(k,203) + rxt(k,204)) * y(k,92)
         mat(k,1779) = -rxt(k,205)*y(k,92)
         mat(k,2416) = -rxt(k,208)*y(k,92)
         mat(k,2344) = rxt(k,397)*y(k,263)
         mat(k,1671) = rxt(k,411)*y(k,259)
         mat(k,2576) = rxt(k,249)*y(k,93)
         mat(k,1016) = rxt(k,353)*y(k,93)
         mat(k,1622) = rxt(k,249)*y(k,70) + rxt(k,353)*y(k,89) + rxt(k,200)*y(k,168) &
                      + rxt(k,182)*y(k,259) + rxt(k,209)*y(k,263)
         mat(k,1606) = rxt(k,309)*y(k,259)
         mat(k,1846) = rxt(k,282)*y(k,259)
         mat(k,1098) = rxt(k,232)*y(k,263)
         mat(k,2513) = rxt(k,200)*y(k,93) + rxt(k,212)*y(k,263)
         mat(k,449) = rxt(k,609)*y(k,263)
         mat(k,805) = rxt(k,615)*y(k,263)
         mat(k,1648) = rxt(k,620)*y(k,263)
         mat(k,1893) = rxt(k,411)*y(k,64) + rxt(k,182)*y(k,93) + rxt(k,309)*y(k,98) &
                      + rxt(k,282)*y(k,102)
         mat(k,2313) = rxt(k,397)*y(k,51) + rxt(k,209)*y(k,93) + rxt(k,232)*y(k,146) &
                      + rxt(k,212)*y(k,168) + rxt(k,609)*y(k,173) + rxt(k,615) &
                      *y(k,185) + rxt(k,620)*y(k,187)
         mat(k,1615) = -(rxt(k,182)*y(k,259) + rxt(k,200)*y(k,168) + rxt(k,209) &
                      *y(k,263) + rxt(k,249)*y(k,70) + rxt(k,353)*y(k,89))
         mat(k,1878) = -rxt(k,182)*y(k,93)
         mat(k,2496) = -rxt(k,200)*y(k,93)
         mat(k,2296) = -rxt(k,209)*y(k,93)
         mat(k,2560) = -rxt(k,249)*y(k,93)
         mat(k,1011) = -rxt(k,353)*y(k,93)
         mat(k,1660) = rxt(k,412)*y(k,259)
         mat(k,2427) = rxt(k,202)*y(k,108)
         mat(k,2769) = rxt(k,202)*y(k,92)
         mat(k,1878) = mat(k,1878) + rxt(k,412)*y(k,64)
         mat(k,125) = -(rxt(k,307)*y(k,259))
         mat(k,1857) = -rxt(k,307)*y(k,94)
         mat(k,699) = -(rxt(k,201)*y(k,168) + rxt(k,210)*y(k,263) + rxt(k,250)*y(k,70))
         mat(k,2481) = -rxt(k,201)*y(k,95)
         mat(k,2231) = -rxt(k,210)*y(k,95)
         mat(k,2541) = -rxt(k,250)*y(k,95)
         mat(k,2721) = 2.000_r8*rxt(k,216)*y(k,108)
         mat(k,2231) = mat(k,2231) + 2.000_r8*rxt(k,215)*y(k,263)
         mat(k,312) = rxt(k,622)*y(k,276)
         mat(k,2857) = rxt(k,622)*y(k,189)
         mat(k,299) = rxt(k,341)*y(k,276)
         mat(k,894) = rxt(k,342)*y(k,263)
         mat(k,2139) = rxt(k,342)*y(k,174)
         mat(k,2856) = rxt(k,341)*y(k,122)
         mat(k,1600) = -(rxt(k,302)*y(k,168) + rxt(k,303)*y(k,263) + (rxt(k,308) &
                      + rxt(k,309)) * y(k,259) + (rxt(k,629) + rxt(k,706) + rxt(k,714) &
                      + rxt(k,723)) * y(k,111) + (rxt(k,630) + rxt(k,704) + rxt(k,717) &
                      + rxt(k,726)) * y(k,110) + (rxt(k,637) + rxt(k,733) + rxt(k,737) &
                      + rxt(k,741)) * y(k,112))
         mat(k,2495) = -rxt(k,302)*y(k,98)
         mat(k,2295) = -rxt(k,303)*y(k,98)
         mat(k,1877) = -(rxt(k,308) + rxt(k,309)) * y(k,98)
         mat(k,1813) = -(rxt(k,629) + rxt(k,706) + rxt(k,714) + rxt(k,723)) * y(k,98)
         mat(k,1681) = -(rxt(k,630) + rxt(k,704) + rxt(k,717) + rxt(k,726)) * y(k,98)
         mat(k,1790) = -(rxt(k,637) + rxt(k,733) + rxt(k,737) + rxt(k,741)) * y(k,98)
         mat(k,2802) = rxt(k,286)*y(k,51) + rxt(k,287)*y(k,108)
         mat(k,2328) = rxt(k,286)*y(k,17)
         mat(k,2768) = rxt(k,287)*y(k,17)
         mat(k,258) = -(rxt(k,386)*y(k,263) + rxt(k,391)*y(k,259))
         mat(k,2172) = -rxt(k,386)*y(k,99)
         mat(k,1867) = -rxt(k,391)*y(k,99)
         mat(k,267) = -(rxt(k,387)*y(k,263) + rxt(k,392)*y(k,259))
         mat(k,2174) = -rxt(k,387)*y(k,100)
         mat(k,1869) = -rxt(k,392)*y(k,100)
         mat(k,349) = -(rxt(k,388)*y(k,263) + rxt(k,393)*y(k,259))
         mat(k,2187) = -rxt(k,388)*y(k,101)
         mat(k,1872) = -rxt(k,393)*y(k,101)
         mat(k,1841) = -(rxt(k,269)*y(k,168) + rxt(k,270)*y(k,263) + (rxt(k,281) &
                      + rxt(k,282)) * y(k,259) + (rxt(k,631) + rxt(k,702) + rxt(k,713) &
                      + rxt(k,722)) * y(k,111) + (rxt(k,632) + rxt(k,703) + rxt(k,716) &
                      + rxt(k,725)) * y(k,110) + (rxt(k,636) + rxt(k,732) + rxt(k,736) &
                      + rxt(k,740)) * y(k,112) + rxt(k,647)*y(k,148) + (rxt(k,712) &
                      + rxt(k,721) + rxt(k,730)) * y(k,75))
         mat(k,2505) = -rxt(k,269)*y(k,102)
         mat(k,2305) = -rxt(k,270)*y(k,102)
         mat(k,1885) = -(rxt(k,281) + rxt(k,282)) * y(k,102)
         mat(k,1818) = -(rxt(k,631) + rxt(k,702) + rxt(k,713) + rxt(k,722)) * y(k,102)
         mat(k,1686) = -(rxt(k,632) + rxt(k,703) + rxt(k,716) + rxt(k,725)) * y(k,102)
         mat(k,1795) = -(rxt(k,636) + rxt(k,732) + rxt(k,736) + rxt(k,740)) * y(k,102)
         mat(k,437) = -rxt(k,647)*y(k,102)
         mat(k,1076) = -(rxt(k,712) + rxt(k,721) + rxt(k,730)) * y(k,102)
         mat(k,295) = rxt(k,360)*y(k,70)
         mat(k,338) = rxt(k,425)*y(k,70)
         mat(k,344) = rxt(k,455)*y(k,70)
         mat(k,561) = rxt(k,362)*y(k,70)
         mat(k,357) = rxt(k,365)*y(k,70)
         mat(k,2336) = rxt(k,247)*y(k,70)
         mat(k,681) = rxt(k,367)*y(k,70)
         mat(k,454) = 2.000_r8*rxt(k,370)*y(k,70)
         mat(k,418) = rxt(k,372)*y(k,70)
         mat(k,1664) = rxt(k,248)*y(k,70)
         mat(k,502) = rxt(k,375)*y(k,70)
         mat(k,412) = rxt(k,384)*y(k,70)
         mat(k,2568) = rxt(k,360)*y(k,29) + rxt(k,425)*y(k,32) + rxt(k,455)*y(k,35) &
                      + rxt(k,362)*y(k,45) + rxt(k,365)*y(k,47) + rxt(k,247)*y(k,51) &
                      + rxt(k,367)*y(k,52) + 2.000_r8*rxt(k,370)*y(k,55) + rxt(k,372) &
                      *y(k,61) + rxt(k,248)*y(k,64) + rxt(k,375)*y(k,66) + rxt(k,384) &
                      *y(k,69) + rxt(k,604)*y(k,83) + rxt(k,249)*y(k,93) + rxt(k,250) &
                      *y(k,95) + rxt(k,251)*y(k,108) + rxt(k,271)*y(k,111)
         mat(k,2837) = rxt(k,268)*y(k,263)
         mat(k,1179) = rxt(k,604)*y(k,70)
         mat(k,1618) = rxt(k,249)*y(k,70)
         mat(k,700) = rxt(k,250)*y(k,70)
         mat(k,2777) = rxt(k,251)*y(k,70)
         mat(k,1818) = mat(k,1818) + rxt(k,271)*y(k,70)
         mat(k,2305) = mat(k,2305) + rxt(k,268)*y(k,74)
         mat(k,209) = -(rxt(k,405)*y(k,263) + rxt(k,413)*y(k,259))
         mat(k,2164) = -rxt(k,405)*y(k,103)
         mat(k,1866) = -rxt(k,413)*y(k,103)
         mat(k,1028) = -(rxt(k,406)*y(k,263))
         mat(k,2258) = -rxt(k,406)*y(k,104)
         mat(k,1108) = .050_r8*rxt(k,580)*y(k,170)
         mat(k,331) = .350_r8*rxt(k,416)*y(k,263)
         mat(k,632) = .370_r8*rxt(k,418)*y(k,170)
         mat(k,1255) = .120_r8*rxt(k,447)*y(k,170)
         mat(k,2740) = rxt(k,407)*y(k,242)
         mat(k,976) = .110_r8*rxt(k,525)*y(k,170)
         mat(k,1405) = .330_r8*rxt(k,478)*y(k,170)
         mat(k,1053) = .050_r8*rxt(k,583)*y(k,170)
         mat(k,1507) = .120_r8*rxt(k,492)*y(k,170)
         mat(k,2084) = rxt(k,409)*y(k,242)
         mat(k,2374) = .050_r8*rxt(k,580)*y(k,6) + .370_r8*rxt(k,418)*y(k,28) &
                      + .120_r8*rxt(k,447)*y(k,33) + .110_r8*rxt(k,525)*y(k,130) &
                      + .330_r8*rxt(k,478)*y(k,139) + .050_r8*rxt(k,583)*y(k,144) &
                      + .120_r8*rxt(k,492)*y(k,145)
         mat(k,522) = rxt(k,407)*y(k,108) + rxt(k,409)*y(k,158)
         mat(k,2258) = mat(k,2258) + .350_r8*rxt(k,416)*y(k,27)
         mat(k,1656) = rxt(k,352)*y(k,89)
         mat(k,1009) = rxt(k,352)*y(k,64) + rxt(k,353)*y(k,93) + rxt(k,355)*y(k,107) &
                      + rxt(k,354)*y(k,276)
         mat(k,1613) = rxt(k,353)*y(k,89)
         mat(k,2451) = rxt(k,355)*y(k,89)
         mat(k,2860) = rxt(k,354)*y(k,89)
         mat(k,1339) = -(rxt(k,310)*y(k,160) + rxt(k,338)*y(k,263) + (rxt(k,633) &
                      + rxt(k,707) + rxt(k,715) + rxt(k,724)) * y(k,111) + (rxt(k,634) &
                      + rxt(k,705) + rxt(k,718) + rxt(k,727)) * y(k,110) + (rxt(k,638) &
                      + rxt(k,734) + rxt(k,738) + rxt(k,742)) * y(k,112))
         mat(k,1936) = -rxt(k,310)*y(k,106)
         mat(k,2282) = -rxt(k,338)*y(k,106)
         mat(k,1812) = -(rxt(k,633) + rxt(k,707) + rxt(k,715) + rxt(k,724)) * y(k,106)
         mat(k,1680) = -(rxt(k,634) + rxt(k,705) + rxt(k,718) + rxt(k,727)) * y(k,106)
         mat(k,1789) = -(rxt(k,638) + rxt(k,734) + rxt(k,738) + rxt(k,742)) * y(k,106)
         mat(k,2755) = rxt(k,316)*y(k,117)
         mat(k,2655) = rxt(k,316)*y(k,108)
         mat(k,2467) = -(rxt(k,241)*y(k,263) + rxt(k,355)*y(k,89))
         mat(k,2314) = -rxt(k,241)*y(k,107)
         mat(k,1017) = -rxt(k,355)*y(k,107)
         mat(k,2345) = rxt(k,395)*y(k,160)
         mat(k,1280) = rxt(k,427)*y(k,160)
         mat(k,1424) = rxt(k,453)*y(k,160)
         mat(k,1080) = (rxt(k,712)+rxt(k,721)+rxt(k,730))*y(k,102)
         mat(k,1182) = rxt(k,606)*y(k,160)
         mat(k,1847) = (rxt(k,712)+rxt(k,721)+rxt(k,730))*y(k,75) + rxt(k,647) &
                      *y(k,148)
         mat(k,1347) = rxt(k,310)*y(k,160)
         mat(k,1801) = rxt(k,343)*y(k,160)
         mat(k,441) = rxt(k,647)*y(k,102)
         mat(k,2024) = rxt(k,240)*y(k,263)
         mat(k,1966) = rxt(k,395)*y(k,51) + rxt(k,427)*y(k,54) + rxt(k,453)*y(k,58) &
                      + rxt(k,606)*y(k,83) + rxt(k,310)*y(k,106) + rxt(k,343)*y(k,112)
         mat(k,2314) = mat(k,2314) + rxt(k,240)*y(k,159)
         mat(k,2792) = -((rxt(k,202) + rxt(k,203) + rxt(k,204)) * y(k,92) + rxt(k,206) &
                      *y(k,168) + rxt(k,207)*y(k,170) + rxt(k,211)*y(k,263) &
                      + 4._r8*rxt(k,216)*y(k,108) + rxt(k,228)*y(k,160) + rxt(k,233) &
                      *y(k,158) + rxt(k,238)*y(k,159) + (rxt(k,251) + rxt(k,252) &
                      ) * y(k,70) + rxt(k,260)*y(k,74) + rxt(k,287)*y(k,17) + rxt(k,294) &
                      *y(k,21) + rxt(k,316)*y(k,117) + rxt(k,331)*y(k,128) + rxt(k,380) &
                      *y(k,68) + rxt(k,394)*y(k,51) + rxt(k,400)*y(k,236) + rxt(k,407) &
                      *y(k,242) + rxt(k,421)*y(k,232) + rxt(k,432)*y(k,235) + rxt(k,436) &
                      *y(k,241) + rxt(k,449)*y(k,233) + rxt(k,458)*y(k,266) + rxt(k,462) &
                      *y(k,267) + rxt(k,474)*y(k,248) + rxt(k,483)*y(k,250) + rxt(k,487) &
                      *y(k,252) + rxt(k,497)*y(k,227) + rxt(k,507)*y(k,243) + rxt(k,512) &
                      *y(k,244) + rxt(k,521)*y(k,245) + rxt(k,532)*y(k,272) + rxt(k,536) &
                      *y(k,226) + rxt(k,539)*y(k,229) + rxt(k,543)*y(k,231) + rxt(k,546) &
                      *y(k,234) + rxt(k,550)*y(k,237) + rxt(k,553)*y(k,249) + rxt(k,556) &
                      *y(k,251) + rxt(k,559)*y(k,265) + rxt(k,566)*y(k,270) + rxt(k,572) &
                      *y(k,273) + rxt(k,575)*y(k,275) + rxt(k,586)*y(k,258) + rxt(k,591) &
                      *y(k,268) + rxt(k,596)*y(k,269))
         mat(k,2447) = -(rxt(k,202) + rxt(k,203) + rxt(k,204)) * y(k,108)
         mat(k,2520) = -rxt(k,206)*y(k,108)
         mat(k,2423) = -rxt(k,207)*y(k,108)
         mat(k,2320) = -rxt(k,211)*y(k,108)
         mat(k,1972) = -rxt(k,228)*y(k,108)
         mat(k,2135) = -rxt(k,233)*y(k,108)
         mat(k,2030) = -rxt(k,238)*y(k,108)
         mat(k,2583) = -(rxt(k,251) + rxt(k,252)) * y(k,108)
         mat(k,2852) = -rxt(k,260)*y(k,108)
         mat(k,2822) = -rxt(k,287)*y(k,108)
         mat(k,2646) = -rxt(k,294)*y(k,108)
         mat(k,2676) = -rxt(k,316)*y(k,108)
         mat(k,2616) = -rxt(k,331)*y(k,108)
         mat(k,1250) = -rxt(k,380)*y(k,108)
         mat(k,2351) = -rxt(k,394)*y(k,108)
         mat(k,1752) = -rxt(k,400)*y(k,108)
         mat(k,526) = -rxt(k,407)*y(k,108)
         mat(k,1008) = -rxt(k,421)*y(k,108)
         mat(k,1578) = -rxt(k,432)*y(k,108)
         mat(k,885) = -rxt(k,436)*y(k,108)
         mat(k,1043) = -rxt(k,449)*y(k,108)
         mat(k,917) = -rxt(k,458)*y(k,108)
         mat(k,1363) = -rxt(k,462)*y(k,108)
         mat(k,1505) = -rxt(k,474)*y(k,108)
         mat(k,1546) = -rxt(k,483)*y(k,108)
         mat(k,784) = -rxt(k,487)*y(k,108)
         mat(k,1143) = -rxt(k,497)*y(k,108)
         mat(k,1451) = -rxt(k,507)*y(k,108)
         mat(k,1484) = -rxt(k,512)*y(k,108)
         mat(k,1404) = -rxt(k,521)*y(k,108)
         mat(k,1381) = -rxt(k,532)*y(k,108)
         mat(k,623) = -rxt(k,536)*y(k,108)
         mat(k,582) = -rxt(k,539)*y(k,108)
         mat(k,520) = -rxt(k,543)*y(k,108)
         mat(k,748) = -rxt(k,546)*y(k,108)
         mat(k,875) = -rxt(k,550)*y(k,108)
         mat(k,827) = -rxt(k,553)*y(k,108)
         mat(k,1027) = -rxt(k,556)*y(k,108)
         mat(k,539) = -rxt(k,559)*y(k,108)
         mat(k,842) = -rxt(k,566)*y(k,108)
         mat(k,867) = -rxt(k,572)*y(k,108)
         mat(k,608) = -rxt(k,575)*y(k,108)
         mat(k,1227) = -rxt(k,586)*y(k,108)
         mat(k,1311) = -rxt(k,591)*y(k,108)
         mat(k,1162) = -rxt(k,596)*y(k,108)
         mat(k,1126) = .570_r8*rxt(k,580)*y(k,170)
         mat(k,194) = .650_r8*rxt(k,538)*y(k,263)
         mat(k,2822) = mat(k,2822) + rxt(k,286)*y(k,51)
         mat(k,2646) = mat(k,2646) + rxt(k,301)*y(k,263)
         mat(k,335) = .350_r8*rxt(k,416)*y(k,263)
         mat(k,637) = .130_r8*rxt(k,418)*y(k,170)
         mat(k,311) = rxt(k,423)*y(k,263)
         mat(k,1272) = .280_r8*rxt(k,447)*y(k,170)
         mat(k,2351) = mat(k,2351) + rxt(k,286)*y(k,17) + rxt(k,247)*y(k,70) &
                      + rxt(k,395)*y(k,160) + rxt(k,396)*y(k,168)
         mat(k,685) = rxt(k,367)*y(k,70) + rxt(k,368)*y(k,263)
         mat(k,457) = rxt(k,370)*y(k,70) + rxt(k,371)*y(k,263)
         mat(k,120) = rxt(k,429)*y(k,263)
         mat(k,422) = rxt(k,372)*y(k,70) + rxt(k,373)*y(k,263)
         mat(k,907) = rxt(k,402)*y(k,263)
         mat(k,1675) = rxt(k,411)*y(k,259)
         mat(k,1250) = mat(k,1250) + rxt(k,382)*y(k,158) + rxt(k,383)*y(k,160) + ( &
                      + 2.000_r8*rxt(k,378)+rxt(k,379))*y(k,236)
         mat(k,2583) = mat(k,2583) + rxt(k,247)*y(k,51) + rxt(k,367)*y(k,52) &
                      + rxt(k,370)*y(k,55) + rxt(k,372)*y(k,61) + rxt(k,250)*y(k,95)
         mat(k,2852) = mat(k,2852) + rxt(k,256)*y(k,236) + rxt(k,267)*y(k,263)
         mat(k,1286) = rxt(k,414)*y(k,263)
         mat(k,235) = .730_r8*rxt(k,549)*y(k,263)
         mat(k,1187) = .500_r8*rxt(k,621)*y(k,263)
         mat(k,1293) = rxt(k,440)*y(k,263)
         mat(k,1149) = rxt(k,441)*y(k,263)
         mat(k,2447) = mat(k,2447) + rxt(k,205)*y(k,169)
         mat(k,704) = rxt(k,250)*y(k,70) + rxt(k,201)*y(k,168) + rxt(k,210)*y(k,263)
         mat(k,212) = rxt(k,405)*y(k,263)
         mat(k,1031) = rxt(k,406)*y(k,263)
         mat(k,2792) = mat(k,2792) + .070_r8*rxt(k,550)*y(k,237) + .160_r8*rxt(k,553) &
                      *y(k,249) + .330_r8*rxt(k,556)*y(k,251)
         mat(k,1329) = rxt(k,471)*y(k,263)
         mat(k,1336) = rxt(k,456)*y(k,263)
         mat(k,2616) = mat(k,2616) + rxt(k,337)*y(k,263)
         mat(k,990) = .370_r8*rxt(k,525)*y(k,170)
         mat(k,718) = .300_r8*rxt(k,516)*y(k,263)
         mat(k,669) = rxt(k,517)*y(k,263)
         mat(k,464) = rxt(k,524)*y(k,263)
         mat(k,1416) = .140_r8*rxt(k,478)*y(k,170)
         mat(k,373) = .200_r8*rxt(k,480)*y(k,263)
         mat(k,678) = .500_r8*rxt(k,491)*y(k,263)
         mat(k,1071) = .570_r8*rxt(k,583)*y(k,170)
         mat(k,1528) = .280_r8*rxt(k,492)*y(k,170)
         mat(k,470) = rxt(k,528)*y(k,263)
         mat(k,1211) = rxt(k,529)*y(k,263)
         mat(k,2135) = mat(k,2135) + rxt(k,382)*y(k,68) + rxt(k,498)*y(k,227) &
                      + rxt(k,540)*y(k,229) + rxt(k,545)*y(k,231) + rxt(k,422) &
                      *y(k,232) + rxt(k,450)*y(k,233) + rxt(k,401)*y(k,236) &
                      + .170_r8*rxt(k,551)*y(k,237) + rxt(k,469)*y(k,239) &
                      + .250_r8*rxt(k,437)*y(k,241) + rxt(k,409)*y(k,242) &
                      + .920_r8*rxt(k,508)*y(k,243) + .920_r8*rxt(k,514)*y(k,244) &
                      + rxt(k,522)*y(k,245) + .470_r8*rxt(k,476)*y(k,248) &
                      + .400_r8*rxt(k,554)*y(k,249) + .830_r8*rxt(k,557)*y(k,251) &
                      + rxt(k,560)*y(k,265) + rxt(k,459)*y(k,266) + .900_r8*rxt(k,592) &
                      *y(k,268) + .800_r8*rxt(k,597)*y(k,269) + rxt(k,567)*y(k,270) &
                      + rxt(k,533)*y(k,272) + rxt(k,573)*y(k,273) + rxt(k,576) &
                      *y(k,275)
         mat(k,1972) = mat(k,1972) + rxt(k,395)*y(k,51) + rxt(k,383)*y(k,68) &
                      + rxt(k,509)*y(k,243) + rxt(k,515)*y(k,244) + rxt(k,523) &
                      *y(k,245) + .470_r8*rxt(k,475)*y(k,248) + rxt(k,231)*y(k,263) &
                      + rxt(k,534)*y(k,272)
         mat(k,2520) = mat(k,2520) + rxt(k,396)*y(k,51) + rxt(k,201)*y(k,95)
         mat(k,1783) = rxt(k,205)*y(k,92) + rxt(k,439)*y(k,240)
         mat(k,2423) = mat(k,2423) + .570_r8*rxt(k,580)*y(k,6) + .130_r8*rxt(k,418) &
                      *y(k,28) + .280_r8*rxt(k,447)*y(k,33) + .370_r8*rxt(k,525) &
                      *y(k,130) + .140_r8*rxt(k,478)*y(k,139) + .570_r8*rxt(k,583) &
                      *y(k,144) + .280_r8*rxt(k,492)*y(k,145) + rxt(k,213)*y(k,263)
         mat(k,203) = .800_r8*rxt(k,561)*y(k,263)
         mat(k,1194) = rxt(k,611)*y(k,263)
         mat(k,1237) = .200_r8*rxt(k,601)*y(k,263)
         mat(k,230) = .280_r8*rxt(k,569)*y(k,263)
         mat(k,252) = .380_r8*rxt(k,571)*y(k,263)
         mat(k,257) = .630_r8*rxt(k,577)*y(k,263)
         mat(k,1143) = mat(k,1143) + rxt(k,498)*y(k,158)
         mat(k,582) = mat(k,582) + rxt(k,540)*y(k,158)
         mat(k,520) = mat(k,520) + rxt(k,545)*y(k,158)
         mat(k,1008) = mat(k,1008) + rxt(k,422)*y(k,158) + 2.400_r8*rxt(k,419) &
                      *y(k,232) + rxt(k,420)*y(k,236)
         mat(k,1043) = mat(k,1043) + rxt(k,450)*y(k,158) + rxt(k,448)*y(k,236)
         mat(k,1578) = mat(k,1578) + .900_r8*rxt(k,431)*y(k,236) + rxt(k,505)*y(k,243) &
                      + rxt(k,510)*y(k,244) + rxt(k,519)*y(k,245) + .470_r8*rxt(k,472) &
                      *y(k,248) + rxt(k,530)*y(k,272)
         mat(k,1752) = mat(k,1752) + (2.000_r8*rxt(k,378)+rxt(k,379))*y(k,68) &
                      + rxt(k,256)*y(k,74) + rxt(k,401)*y(k,158) + rxt(k,420)*y(k,232) &
                      + rxt(k,448)*y(k,233) + .900_r8*rxt(k,431)*y(k,235) &
                      + 4.000_r8*rxt(k,398)*y(k,236) + rxt(k,506)*y(k,243) &
                      + rxt(k,511)*y(k,244) + 1.200_r8*rxt(k,520)*y(k,245) &
                      + .730_r8*rxt(k,473)*y(k,248) + rxt(k,482)*y(k,250) &
                      + .500_r8*rxt(k,585)*y(k,258) + .300_r8*rxt(k,461)*y(k,267) &
                      + rxt(k,590)*y(k,268) + rxt(k,595)*y(k,269) + .800_r8*rxt(k,531) &
                      *y(k,272)
         mat(k,875) = mat(k,875) + .070_r8*rxt(k,550)*y(k,108) + .170_r8*rxt(k,551) &
                      *y(k,158)
         mat(k,661) = rxt(k,469)*y(k,158)
         mat(k,549) = rxt(k,439)*y(k,169)
         mat(k,885) = mat(k,885) + .250_r8*rxt(k,437)*y(k,158)
         mat(k,526) = mat(k,526) + rxt(k,409)*y(k,158)
         mat(k,1451) = mat(k,1451) + .920_r8*rxt(k,508)*y(k,158) + rxt(k,509)*y(k,160) &
                      + rxt(k,505)*y(k,235) + rxt(k,506)*y(k,236)
         mat(k,1484) = mat(k,1484) + .920_r8*rxt(k,514)*y(k,158) + rxt(k,515)*y(k,160) &
                      + rxt(k,510)*y(k,235) + rxt(k,511)*y(k,236)
         mat(k,1404) = mat(k,1404) + rxt(k,522)*y(k,158) + rxt(k,523)*y(k,160) &
                      + rxt(k,519)*y(k,235) + 1.200_r8*rxt(k,520)*y(k,236)
         mat(k,1505) = mat(k,1505) + .470_r8*rxt(k,476)*y(k,158) + .470_r8*rxt(k,475) &
                      *y(k,160) + .470_r8*rxt(k,472)*y(k,235) + .730_r8*rxt(k,473) &
                      *y(k,236)
         mat(k,827) = mat(k,827) + .160_r8*rxt(k,553)*y(k,108) + .400_r8*rxt(k,554) &
                      *y(k,158)
         mat(k,1546) = mat(k,1546) + rxt(k,482)*y(k,236)
         mat(k,1027) = mat(k,1027) + .330_r8*rxt(k,556)*y(k,108) + .830_r8*rxt(k,557) &
                      *y(k,158)
         mat(k,1227) = mat(k,1227) + .500_r8*rxt(k,585)*y(k,236)
         mat(k,1900) = rxt(k,411)*y(k,64)
         mat(k,2320) = mat(k,2320) + .650_r8*rxt(k,538)*y(k,7) + rxt(k,301)*y(k,21) &
                      + .350_r8*rxt(k,416)*y(k,27) + rxt(k,423)*y(k,30) + rxt(k,368) &
                      *y(k,52) + rxt(k,371)*y(k,55) + rxt(k,429)*y(k,56) + rxt(k,373) &
                      *y(k,61) + rxt(k,402)*y(k,62) + rxt(k,267)*y(k,74) + rxt(k,414) &
                      *y(k,77) + .730_r8*rxt(k,549)*y(k,82) + .500_r8*rxt(k,621) &
                      *y(k,83) + rxt(k,440)*y(k,90) + rxt(k,441)*y(k,91) + rxt(k,210) &
                      *y(k,95) + rxt(k,405)*y(k,103) + rxt(k,406)*y(k,104) &
                      + rxt(k,471)*y(k,113) + rxt(k,456)*y(k,115) + rxt(k,337) &
                      *y(k,128) + .300_r8*rxt(k,516)*y(k,131) + rxt(k,517)*y(k,132) &
                      + rxt(k,524)*y(k,133) + .200_r8*rxt(k,480)*y(k,140) &
                      + .500_r8*rxt(k,491)*y(k,143) + rxt(k,528)*y(k,149) + rxt(k,529) &
                      *y(k,150) + rxt(k,231)*y(k,160) + rxt(k,213)*y(k,170) &
                      + .800_r8*rxt(k,561)*y(k,179) + rxt(k,611)*y(k,188) &
                      + .200_r8*rxt(k,601)*y(k,216) + .280_r8*rxt(k,569)*y(k,218) &
                      + .380_r8*rxt(k,571)*y(k,220) + .630_r8*rxt(k,577)*y(k,222)
         mat(k,539) = mat(k,539) + rxt(k,560)*y(k,158)
         mat(k,917) = mat(k,917) + rxt(k,459)*y(k,158)
         mat(k,1363) = mat(k,1363) + .300_r8*rxt(k,461)*y(k,236)
         mat(k,1311) = mat(k,1311) + .900_r8*rxt(k,592)*y(k,158) + rxt(k,590)*y(k,236)
         mat(k,1162) = mat(k,1162) + .800_r8*rxt(k,597)*y(k,158) + rxt(k,595)*y(k,236)
         mat(k,842) = mat(k,842) + rxt(k,567)*y(k,158)
         mat(k,1381) = mat(k,1381) + rxt(k,533)*y(k,158) + rxt(k,534)*y(k,160) &
                      + rxt(k,530)*y(k,235) + .800_r8*rxt(k,531)*y(k,236)
         mat(k,867) = mat(k,867) + rxt(k,573)*y(k,158)
         mat(k,608) = mat(k,608) + rxt(k,576)*y(k,158)
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
         mat(k,583) = -(rxt(k,218)*y(k,263))
         mat(k,2217) = -rxt(k,218)*y(k,109)
         mat(k,2716) = rxt(k,238)*y(k,159)
         mat(k,1984) = rxt(k,238)*y(k,108)
         mat(k,1683) = -(rxt(k,304)*y(k,168) + (rxt(k,630) + rxt(k,704) + rxt(k,717) &
                      + rxt(k,726)) * y(k,98) + (rxt(k,632) + rxt(k,703) + rxt(k,716) &
                      + rxt(k,725)) * y(k,102) + (rxt(k,634) + rxt(k,705) + rxt(k,718) &
                      + rxt(k,727)) * y(k,106))
         mat(k,2500) = -rxt(k,304)*y(k,110)
         mat(k,1601) = -(rxt(k,630) + rxt(k,704) + rxt(k,717) + rxt(k,726)) * y(k,110)
         mat(k,1838) = -(rxt(k,632) + rxt(k,703) + rxt(k,716) + rxt(k,725)) * y(k,110)
         mat(k,1341) = -(rxt(k,634) + rxt(k,705) + rxt(k,718) + rxt(k,727)) * y(k,110)
         mat(k,598) = rxt(k,285)*y(k,263)
         mat(k,2627) = rxt(k,294)*y(k,108)
         mat(k,2772) = rxt(k,294)*y(k,21)
         mat(k,2300) = rxt(k,285)*y(k,18)
         mat(k,1817) = -(rxt(k,271)*y(k,70) + rxt(k,272)*y(k,168) + rxt(k,273) &
                      *y(k,263) + (rxt(k,629) + rxt(k,706) + rxt(k,714) + rxt(k,723) &
                      ) * y(k,98) + (rxt(k,631) + rxt(k,702) + rxt(k,713) + rxt(k,722) &
                      ) * y(k,102) + (rxt(k,633) + rxt(k,707) + rxt(k,715) + rxt(k,724) &
                      ) * y(k,106))
         mat(k,2567) = -rxt(k,271)*y(k,111)
         mat(k,2504) = -rxt(k,272)*y(k,111)
         mat(k,2304) = -rxt(k,273)*y(k,111)
         mat(k,1603) = -(rxt(k,629) + rxt(k,706) + rxt(k,714) + rxt(k,723)) * y(k,111)
         mat(k,1840) = -(rxt(k,631) + rxt(k,702) + rxt(k,713) + rxt(k,722)) * y(k,111)
         mat(k,1343) = -(rxt(k,633) + rxt(k,707) + rxt(k,715) + rxt(k,724)) * y(k,111)
         mat(k,1244) = rxt(k,381)*y(k,108)
         mat(k,626) = rxt(k,254)*y(k,263)
         mat(k,2836) = rxt(k,260)*y(k,108)
         mat(k,1075) = rxt(k,265)*y(k,263)
         mat(k,2776) = rxt(k,381)*y(k,68) + rxt(k,260)*y(k,74)
         mat(k,2304) = mat(k,2304) + rxt(k,254)*y(k,73) + rxt(k,265)*y(k,75)
         mat(k,1793) = -(rxt(k,311)*y(k,263) + rxt(k,343)*y(k,160) + (rxt(k,636) &
                      + rxt(k,732) + rxt(k,736) + rxt(k,740)) * y(k,102) + (rxt(k,637) &
                      + rxt(k,733) + rxt(k,737) + rxt(k,741)) * y(k,98) + (rxt(k,638) &
                      + rxt(k,734) + rxt(k,738) + rxt(k,742)) * y(k,106))
         mat(k,2303) = -rxt(k,311)*y(k,112)
         mat(k,1955) = -rxt(k,343)*y(k,112)
         mat(k,1839) = -(rxt(k,636) + rxt(k,732) + rxt(k,736) + rxt(k,740)) * y(k,112)
         mat(k,1602) = -(rxt(k,637) + rxt(k,733) + rxt(k,737) + rxt(k,741)) * y(k,112)
         mat(k,1342) = -(rxt(k,638) + rxt(k,734) + rxt(k,738) + rxt(k,742)) * y(k,112)
         mat(k,2775) = rxt(k,331)*y(k,128)
         mat(k,1629) = rxt(k,314)*y(k,263)
         mat(k,302) = rxt(k,341)*y(k,276)
         mat(k,2599) = rxt(k,331)*y(k,108)
         mat(k,2303) = mat(k,2303) + rxt(k,314)*y(k,118)
         mat(k,2868) = rxt(k,341)*y(k,122)
         mat(k,1318) = -(rxt(k,471)*y(k,263))
         mat(k,2280) = -rxt(k,471)*y(k,113)
         mat(k,712) = .300_r8*rxt(k,516)*y(k,263)
         mat(k,665) = .500_r8*rxt(k,517)*y(k,263)
         mat(k,2100) = rxt(k,470)*y(k,239) + rxt(k,477)*y(k,248)
         mat(k,657) = rxt(k,470)*y(k,158)
         mat(k,1490) = rxt(k,477)*y(k,158)
         mat(k,2280) = mat(k,2280) + .300_r8*rxt(k,516)*y(k,131) + .500_r8*rxt(k,517) &
                      *y(k,132)
         mat(k,275) = -(rxt(k,502)*y(k,263))
         mat(k,2176) = -rxt(k,502)*y(k,114)
         mat(k,1331) = -(rxt(k,456)*y(k,263))
         mat(k,2281) = -rxt(k,456)*y(k,115)
         mat(k,713) = .700_r8*rxt(k,516)*y(k,263)
         mat(k,666) = .500_r8*rxt(k,517)*y(k,263)
         mat(k,671) = .500_r8*rxt(k,491)*y(k,263)
         mat(k,2101) = .050_r8*rxt(k,514)*y(k,244) + .220_r8*rxt(k,476)*y(k,248) &
                      + .250_r8*rxt(k,533)*y(k,272)
         mat(k,1935) = .050_r8*rxt(k,515)*y(k,244) + .220_r8*rxt(k,475)*y(k,248) &
                      + .250_r8*rxt(k,534)*y(k,272)
         mat(k,649) = .500_r8*rxt(k,460)*y(k,263)
         mat(k,1556) = .220_r8*rxt(k,472)*y(k,248) + .250_r8*rxt(k,530)*y(k,272)
         mat(k,1723) = .230_r8*rxt(k,473)*y(k,248) + .200_r8*rxt(k,461)*y(k,267) &
                      + .100_r8*rxt(k,531)*y(k,272)
         mat(k,1465) = .050_r8*rxt(k,514)*y(k,158) + .050_r8*rxt(k,515)*y(k,160)
         mat(k,1491) = .220_r8*rxt(k,476)*y(k,158) + .220_r8*rxt(k,475)*y(k,160) &
                      + .220_r8*rxt(k,472)*y(k,235) + .230_r8*rxt(k,473)*y(k,236)
         mat(k,2281) = mat(k,2281) + .700_r8*rxt(k,516)*y(k,131) + .500_r8*rxt(k,517) &
                      *y(k,132) + .500_r8*rxt(k,491)*y(k,143) + .500_r8*rxt(k,460) &
                      *y(k,183)
         mat(k,1354) = .200_r8*rxt(k,461)*y(k,236)
         mat(k,1370) = .250_r8*rxt(k,533)*y(k,158) + .250_r8*rxt(k,534)*y(k,160) &
                      + .250_r8*rxt(k,530)*y(k,235) + .100_r8*rxt(k,531)*y(k,236)
         mat(k,402) = -(rxt(k,503)*y(k,263))
         mat(k,2194) = -rxt(k,503)*y(k,116)
         mat(k,2051) = .870_r8*rxt(k,514)*y(k,244)
         mat(k,1906) = .950_r8*rxt(k,515)*y(k,244)
         mat(k,1548) = rxt(k,510)*y(k,244)
         mat(k,1701) = .750_r8*rxt(k,511)*y(k,244)
         mat(k,1454) = .870_r8*rxt(k,514)*y(k,158) + .950_r8*rxt(k,515)*y(k,160) &
                      + rxt(k,510)*y(k,235) + .750_r8*rxt(k,511)*y(k,236)
         mat(k,2675) = -(rxt(k,315)*y(k,21) + rxt(k,316)*y(k,108) + rxt(k,317) &
                      *y(k,129) + rxt(k,319)*y(k,159) + rxt(k,321)*y(k,160) + rxt(k,323) &
                      *y(k,158) + rxt(k,324)*y(k,170))
         mat(k,2645) = -rxt(k,315)*y(k,117)
         mat(k,2791) = -rxt(k,316)*y(k,117)
         mat(k,958) = -rxt(k,317)*y(k,117)
         mat(k,2029) = -rxt(k,319)*y(k,117)
         mat(k,1971) = -rxt(k,321)*y(k,117)
         mat(k,2134) = -rxt(k,323)*y(k,117)
         mat(k,2422) = -rxt(k,324)*y(k,117)
         mat(k,2821) = rxt(k,325)*y(k,128)
         mat(k,2645) = mat(k,2645) + rxt(k,326)*y(k,128)
         mat(k,421) = rxt(k,372)*y(k,70) + rxt(k,373)*y(k,263)
         mat(k,2582) = rxt(k,372)*y(k,61)
         mat(k,2851) = (rxt(k,328)+rxt(k,329))*y(k,128)
         mat(k,1186) = rxt(k,605)*y(k,128)
         mat(k,1349) = rxt(k,310)*y(k,160) + rxt(k,338)*y(k,263)
         mat(k,1637) = rxt(k,312)*y(k,160) + rxt(k,313)*y(k,168) + rxt(k,314)*y(k,263)
         mat(k,2615) = rxt(k,325)*y(k,17) + rxt(k,326)*y(k,21) + (rxt(k,328) &
                       +rxt(k,329))*y(k,74) + rxt(k,605)*y(k,83) + 2.000_r8*rxt(k,347) &
                      *y(k,128) + rxt(k,332)*y(k,158) + rxt(k,335)*y(k,168) &
                      + rxt(k,337)*y(k,263)
         mat(k,2134) = mat(k,2134) + rxt(k,332)*y(k,128)
         mat(k,1971) = mat(k,1971) + rxt(k,310)*y(k,106) + rxt(k,312)*y(k,118)
         mat(k,2519) = rxt(k,313)*y(k,118) + rxt(k,335)*y(k,128)
         mat(k,2319) = rxt(k,373)*y(k,61) + rxt(k,338)*y(k,106) + rxt(k,314)*y(k,118) &
                      + rxt(k,337)*y(k,128)
         mat(k,1628) = -(rxt(k,312)*y(k,160) + rxt(k,313)*y(k,168) + rxt(k,314) &
                      *y(k,263))
         mat(k,1949) = -rxt(k,312)*y(k,118)
         mat(k,2497) = -rxt(k,313)*y(k,118)
         mat(k,2297) = -rxt(k,314)*y(k,118)
         mat(k,1340) = (rxt(k,638)+rxt(k,734)+rxt(k,738)+rxt(k,742))*y(k,112)
         mat(k,1791) = (rxt(k,638)+rxt(k,734)+rxt(k,738)+rxt(k,742))*y(k,106)
         mat(k,2656) = rxt(k,317)*y(k,129)
         mat(k,214) = 2.000_r8*rxt(k,322)*y(k,126)
         mat(k,324) = 2.000_r8*rxt(k,318)*y(k,127)
         mat(k,951) = rxt(k,317)*y(k,117)
         mat(k,280) = -(rxt(k,340)*y(k,170))
         mat(k,2362) = -rxt(k,340)*y(k,119)
         mat(k,2588) = 2.000_r8*rxt(k,348)*y(k,128)
         mat(k,2587) = rxt(k,350)*y(k,174)
         mat(k,896) = rxt(k,350)*y(k,128)
         mat(k,895) = 2.000_r8*rxt(k,351)*y(k,174)
         mat(k,300) = -(rxt(k,341)*y(k,276))
         mat(k,2858) = -rxt(k,341)*y(k,122)
         mat(k,281) = rxt(k,340)*y(k,170)
         mat(k,2364) = rxt(k,340)*y(k,119)
         mat(k,1597) = (rxt(k,637)+rxt(k,733)+rxt(k,737)+rxt(k,741))*y(k,112)
         mat(k,1337) = (rxt(k,634)+rxt(k,705)+rxt(k,718)+rxt(k,727))*y(k,110)
         mat(k,1677) = (rxt(k,634)+rxt(k,705)+rxt(k,718)+rxt(k,727))*y(k,106)
         mat(k,1787) = (rxt(k,637)+rxt(k,733)+rxt(k,737)+rxt(k,741))*y(k,98)
         mat(k,2829) = rxt(k,330)*y(k,128)
         mat(k,1834) = (rxt(k,636)+rxt(k,732)+rxt(k,736)+rxt(k,740))*y(k,112)
         mat(k,1338) = (rxt(k,633)+rxt(k,707)+rxt(k,715)+rxt(k,724))*y(k,111)
         mat(k,1810) = (rxt(k,633)+rxt(k,707)+rxt(k,715)+rxt(k,724))*y(k,106)
         mat(k,1788) = (rxt(k,636)+rxt(k,732)+rxt(k,736)+rxt(k,740))*y(k,102)
         mat(k,2591) = rxt(k,330)*y(k,74)
         mat(k,162) = -(rxt(k,504)*y(k,263))
         mat(k,2160) = -rxt(k,504)*y(k,125)
         mat(k,843) = .600_r8*rxt(k,527)*y(k,263)
         mat(k,2160) = mat(k,2160) + .600_r8*rxt(k,527)*y(k,134)
         mat(k,213) = -(4._r8*rxt(k,322)*y(k,126))
         mat(k,2650) = rxt(k,323)*y(k,158)
         mat(k,2046) = rxt(k,323)*y(k,117)
         mat(k,321) = -(4._r8*rxt(k,318)*y(k,127))
         mat(k,2651) = rxt(k,319)*y(k,159)
         mat(k,1978) = rxt(k,319)*y(k,117)
         mat(k,2613) = -(rxt(k,325)*y(k,17) + (rxt(k,326) + rxt(k,327)) * y(k,21) &
                      + (rxt(k,328) + rxt(k,329) + rxt(k,330)) * y(k,74) + rxt(k,331) &
                      *y(k,108) + rxt(k,332)*y(k,158) + rxt(k,333)*y(k,159) + rxt(k,334) &
                      *y(k,160) + rxt(k,335)*y(k,168) + rxt(k,336)*y(k,170) + rxt(k,337) &
                      *y(k,263) + (4._r8*rxt(k,347) + 4._r8*rxt(k,348)) * y(k,128) &
                      + rxt(k,350)*y(k,174) + rxt(k,605)*y(k,83))
         mat(k,2819) = -rxt(k,325)*y(k,128)
         mat(k,2643) = -(rxt(k,326) + rxt(k,327)) * y(k,128)
         mat(k,2849) = -(rxt(k,328) + rxt(k,329) + rxt(k,330)) * y(k,128)
         mat(k,2789) = -rxt(k,331)*y(k,128)
         mat(k,2132) = -rxt(k,332)*y(k,128)
         mat(k,2027) = -rxt(k,333)*y(k,128)
         mat(k,1969) = -rxt(k,334)*y(k,128)
         mat(k,2517) = -rxt(k,335)*y(k,128)
         mat(k,2420) = -rxt(k,336)*y(k,128)
         mat(k,2317) = -rxt(k,337)*y(k,128)
         mat(k,902) = -rxt(k,350)*y(k,128)
         mat(k,1184) = -rxt(k,605)*y(k,128)
         mat(k,2643) = mat(k,2643) + rxt(k,315)*y(k,117)
         mat(k,1804) = rxt(k,343)*y(k,160) + rxt(k,311)*y(k,263)
         mat(k,2673) = rxt(k,315)*y(k,21) + rxt(k,321)*y(k,160) + rxt(k,324)*y(k,170)
         mat(k,1636) = rxt(k,313)*y(k,168)
         mat(k,2132) = mat(k,2132) + rxt(k,339)*y(k,174)
         mat(k,1969) = mat(k,1969) + rxt(k,343)*y(k,112) + rxt(k,321)*y(k,117)
         mat(k,2517) = mat(k,2517) + rxt(k,313)*y(k,118)
         mat(k,2420) = mat(k,2420) + rxt(k,324)*y(k,117)
         mat(k,902) = mat(k,902) + rxt(k,339)*y(k,158)
         mat(k,2317) = mat(k,2317) + rxt(k,311)*y(k,112)
         mat(k,950) = -(rxt(k,317)*y(k,117))
         mat(k,2654) = -rxt(k,317)*y(k,129)
         mat(k,1627) = rxt(k,312)*y(k,160)
         mat(k,2593) = rxt(k,333)*y(k,159)
         mat(k,1994) = rxt(k,333)*y(k,128)
         mat(k,1912) = rxt(k,312)*y(k,118)
         mat(k,975) = -(rxt(k,518)*y(k,160) + rxt(k,525)*y(k,170) + rxt(k,526) &
                      *y(k,263))
         mat(k,1913) = -rxt(k,518)*y(k,130)
         mat(k,2373) = -rxt(k,525)*y(k,130)
         mat(k,2253) = -rxt(k,526)*y(k,130)
         mat(k,710) = -(rxt(k,516)*y(k,263))
         mat(k,2233) = -rxt(k,516)*y(k,131)
         mat(k,2065) = .080_r8*rxt(k,508)*y(k,243)
         mat(k,1427) = .080_r8*rxt(k,508)*y(k,158)
         mat(k,662) = -(rxt(k,517)*y(k,263))
         mat(k,2227) = -rxt(k,517)*y(k,132)
         mat(k,2063) = .080_r8*rxt(k,514)*y(k,244)
         mat(k,1455) = .080_r8*rxt(k,514)*y(k,158)
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
         mat(k,459) = -(rxt(k,524)*y(k,263))
         mat(k,2201) = -rxt(k,524)*y(k,133)
         mat(k,2701) = rxt(k,521)*y(k,245)
         mat(k,1382) = rxt(k,521)*y(k,108)
         mat(k,844) = -(rxt(k,527)*y(k,263))
         mat(k,2246) = -rxt(k,527)*y(k,134)
         mat(k,2732) = rxt(k,507)*y(k,243) + rxt(k,512)*y(k,244)
         mat(k,1428) = rxt(k,507)*y(k,108)
         mat(k,1457) = rxt(k,512)*y(k,108)
         mat(k,88) = -(rxt(k,687)*y(k,263))
         mat(k,2150) = -rxt(k,687)*y(k,135)
         mat(k,1407) = -(rxt(k,478)*y(k,170) + rxt(k,479)*y(k,263))
         mat(k,2393) = -rxt(k,478)*y(k,139)
         mat(k,2286) = -rxt(k,479)*y(k,139)
         mat(k,980) = .300_r8*rxt(k,525)*y(k,170)
         mat(k,2105) = .360_r8*rxt(k,508)*y(k,243)
         mat(k,1940) = .400_r8*rxt(k,509)*y(k,243)
         mat(k,2393) = mat(k,2393) + .300_r8*rxt(k,525)*y(k,130)
         mat(k,1559) = .390_r8*rxt(k,505)*y(k,243)
         mat(k,1727) = .310_r8*rxt(k,506)*y(k,243)
         mat(k,1435) = .360_r8*rxt(k,508)*y(k,158) + .400_r8*rxt(k,509)*y(k,160) &
                      + .390_r8*rxt(k,505)*y(k,235) + .310_r8*rxt(k,506)*y(k,236)
         mat(k,369) = -(rxt(k,480)*y(k,263))
         mat(k,2189) = -rxt(k,480)*y(k,140)
         mat(k,2695) = rxt(k,474)*y(k,248)
         mat(k,1486) = rxt(k,474)*y(k,108)
         mat(k,609) = -(rxt(k,489)*y(k,263))
         mat(k,2220) = -rxt(k,489)*y(k,141)
         mat(k,2060) = .800_r8*rxt(k,498)*y(k,227)
         mat(k,1128) = .800_r8*rxt(k,498)*y(k,158)
         mat(k,374) = -(rxt(k,490)*y(k,263))
         mat(k,2190) = -rxt(k,490)*y(k,142)
         mat(k,2696) = .800_r8*rxt(k,487)*y(k,252)
         mat(k,777) = .800_r8*rxt(k,487)*y(k,108)
         mat(k,670) = -(rxt(k,491)*y(k,263))
         mat(k,2228) = -rxt(k,491)*y(k,143)
         mat(k,1990) = rxt(k,494)*y(k,250)
         mat(k,1530) = rxt(k,494)*y(k,159)
         mat(k,1054) = -(rxt(k,582)*y(k,160) + rxt(k,583)*y(k,170) + rxt(k,584) &
                      *y(k,263))
         mat(k,1916) = -rxt(k,582)*y(k,144)
         mat(k,2375) = -rxt(k,583)*y(k,144)
         mat(k,2260) = -rxt(k,584)*y(k,144)
         mat(k,1514) = -(rxt(k,492)*y(k,170) + rxt(k,493)*y(k,263))
         mat(k,2398) = -rxt(k,492)*y(k,145)
         mat(k,2291) = -rxt(k,493)*y(k,145)
         mat(k,983) = .200_r8*rxt(k,525)*y(k,170)
         mat(k,2110) = .560_r8*rxt(k,508)*y(k,243)
         mat(k,1945) = .600_r8*rxt(k,509)*y(k,243)
         mat(k,2398) = mat(k,2398) + .200_r8*rxt(k,525)*y(k,130)
         mat(k,1564) = .610_r8*rxt(k,505)*y(k,243)
         mat(k,1732) = .440_r8*rxt(k,506)*y(k,243)
         mat(k,1439) = .560_r8*rxt(k,508)*y(k,158) + .600_r8*rxt(k,509)*y(k,160) &
                      + .610_r8*rxt(k,505)*y(k,235) + .440_r8*rxt(k,506)*y(k,236)
         mat(k,1090) = -(rxt(k,221)*y(k,158) + (rxt(k,222) + rxt(k,223) + rxt(k,224) &
                      ) * y(k,159) + rxt(k,232)*y(k,263) + rxt(k,246)*y(k,169) &
                      + rxt(k,752)*y(k,262))
         mat(k,2086) = -rxt(k,221)*y(k,146)
         mat(k,1998) = -(rxt(k,222) + rxt(k,223) + rxt(k,224)) * y(k,146)
         mat(k,2262) = -rxt(k,232)*y(k,146)
         mat(k,1766) = -rxt(k,246)*y(k,146)
         mat(k,930) = -rxt(k,752)*y(k,146)
         mat(k,2491) = rxt(k,220)*y(k,254) + rxt(k,749)*y(k,257)
         mat(k,1766) = mat(k,1766) + rxt(k,750)*y(k,257)
         mat(k,941) = rxt(k,243)*y(k,254) + 1.100_r8*rxt(k,745)*y(k,255) &
                      + .200_r8*rxt(k,743)*y(k,256)
         mat(k,750) = rxt(k,220)*y(k,168) + rxt(k,243)*y(k,238)
         mat(k,725) = 1.100_r8*rxt(k,745)*y(k,238)
         mat(k,922) = .200_r8*rxt(k,743)*y(k,238)
         mat(k,594) = rxt(k,749)*y(k,168) + rxt(k,750)*y(k,169)
         mat(k,304) = -((rxt(k,236) + rxt(k,237)) * y(k,259))
         mat(k,1871) = -(rxt(k,236) + rxt(k,237)) * y(k,147)
         mat(k,1084) = rxt(k,222)*y(k,159)
         mat(k,1977) = rxt(k,222)*y(k,146)
         mat(k,1981) = rxt(k,239)*y(k,160)
         mat(k,1908) = rxt(k,239)*y(k,159)
         mat(k,465) = -(rxt(k,528)*y(k,263))
         mat(k,2202) = -rxt(k,528)*y(k,149)
         mat(k,1703) = .200_r8*rxt(k,520)*y(k,245)
         mat(k,1383) = .200_r8*rxt(k,520)*y(k,236)
         mat(k,1200) = -(rxt(k,529)*y(k,263))
         mat(k,2271) = -rxt(k,529)*y(k,150)
         mat(k,2092) = rxt(k,522)*y(k,245)
         mat(k,1925) = rxt(k,523)*y(k,245)
         mat(k,1553) = rxt(k,519)*y(k,245)
         mat(k,1715) = .800_r8*rxt(k,520)*y(k,245)
         mat(k,1387) = rxt(k,522)*y(k,158) + rxt(k,523)*y(k,160) + rxt(k,519)*y(k,235) &
                      + .800_r8*rxt(k,520)*y(k,236)
         mat(k,112) = -(rxt(k,640)*y(k,263))
         mat(k,2154) = -rxt(k,640)*y(k,154)
         mat(k,2124) = -(rxt(k,219)*y(k,254) + rxt(k,221)*y(k,146) + rxt(k,229) &
                      *y(k,160) + rxt(k,233)*y(k,108) + rxt(k,234)*y(k,170) + rxt(k,235) &
                      *y(k,168) + rxt(k,261)*y(k,74) + rxt(k,295)*y(k,21) + rxt(k,323) &
                      *y(k,117) + rxt(k,332)*y(k,128) + rxt(k,339)*y(k,174) + rxt(k,382) &
                      *y(k,68) + rxt(k,401)*y(k,236) + rxt(k,409)*y(k,242) + rxt(k,422) &
                      *y(k,232) + rxt(k,433)*y(k,235) + rxt(k,437)*y(k,241) + rxt(k,450) &
                      *y(k,233) + rxt(k,459)*y(k,266) + rxt(k,463)*y(k,267) + (rxt(k,469) &
                      + rxt(k,470)) * y(k,239) + (rxt(k,476) + rxt(k,477)) * y(k,248) &
                      + rxt(k,485)*y(k,250) + rxt(k,488)*y(k,252) + (rxt(k,498) &
                      + rxt(k,499)) * y(k,227) + rxt(k,508)*y(k,243) + rxt(k,514) &
                      *y(k,244) + rxt(k,522)*y(k,245) + rxt(k,533)*y(k,272) + rxt(k,537) &
                      *y(k,226) + rxt(k,540)*y(k,229) + rxt(k,545)*y(k,231) + rxt(k,547) &
                      *y(k,234) + rxt(k,551)*y(k,237) + rxt(k,554)*y(k,249) + rxt(k,557) &
                      *y(k,251) + rxt(k,560)*y(k,265) + rxt(k,567)*y(k,270) + rxt(k,573) &
                      *y(k,273) + rxt(k,576)*y(k,275) + rxt(k,587)*y(k,258) + rxt(k,592) &
                      *y(k,268) + rxt(k,597)*y(k,269) + rxt(k,754)*y(k,262))
         mat(k,753) = -rxt(k,219)*y(k,158)
         mat(k,1096) = -rxt(k,221)*y(k,158)
         mat(k,1961) = -rxt(k,229)*y(k,158)
         mat(k,2781) = -rxt(k,233)*y(k,158)
         mat(k,2412) = -rxt(k,234)*y(k,158)
         mat(k,2509) = -rxt(k,235)*y(k,158)
         mat(k,2841) = -rxt(k,261)*y(k,158)
         mat(k,2635) = -rxt(k,295)*y(k,158)
         mat(k,2665) = -rxt(k,323)*y(k,158)
         mat(k,2605) = -rxt(k,332)*y(k,158)
         mat(k,900) = -rxt(k,339)*y(k,158)
         mat(k,1247) = -rxt(k,382)*y(k,158)
         mat(k,1744) = -rxt(k,401)*y(k,158)
         mat(k,524) = -rxt(k,409)*y(k,158)
         mat(k,1005) = -rxt(k,422)*y(k,158)
         mat(k,1573) = -rxt(k,433)*y(k,158)
         mat(k,882) = -rxt(k,437)*y(k,158)
         mat(k,1040) = -rxt(k,450)*y(k,158)
         mat(k,914) = -rxt(k,459)*y(k,158)
         mat(k,1360) = -rxt(k,463)*y(k,158)
         mat(k,659) = -(rxt(k,469) + rxt(k,470)) * y(k,158)
         mat(k,1501) = -(rxt(k,476) + rxt(k,477)) * y(k,158)
         mat(k,1541) = -rxt(k,485)*y(k,158)
         mat(k,782) = -rxt(k,488)*y(k,158)
         mat(k,1140) = -(rxt(k,498) + rxt(k,499)) * y(k,158)
         mat(k,1446) = -rxt(k,508)*y(k,158)
         mat(k,1479) = -rxt(k,514)*y(k,158)
         mat(k,1400) = -rxt(k,522)*y(k,158)
         mat(k,1378) = -rxt(k,533)*y(k,158)
         mat(k,621) = -rxt(k,537)*y(k,158)
         mat(k,580) = -rxt(k,540)*y(k,158)
         mat(k,518) = -rxt(k,545)*y(k,158)
         mat(k,745) = -rxt(k,547)*y(k,158)
         mat(k,873) = -rxt(k,551)*y(k,158)
         mat(k,826) = -rxt(k,554)*y(k,158)
         mat(k,1025) = -rxt(k,557)*y(k,158)
         mat(k,537) = -rxt(k,560)*y(k,158)
         mat(k,840) = -rxt(k,567)*y(k,158)
         mat(k,865) = -rxt(k,573)*y(k,158)
         mat(k,606) = -rxt(k,576)*y(k,158)
         mat(k,1223) = -rxt(k,587)*y(k,158)
         mat(k,1307) = -rxt(k,592)*y(k,158)
         mat(k,1158) = -rxt(k,597)*y(k,158)
         mat(k,933) = -rxt(k,754)*y(k,158)
         mat(k,215) = 4.000_r8*rxt(k,322)*y(k,126)
         mat(k,1096) = mat(k,1096) + 2.000_r8*rxt(k,223)*y(k,159) + rxt(k,246) &
                      *y(k,169) + rxt(k,232)*y(k,263)
         mat(k,307) = 2.000_r8*rxt(k,236)*y(k,259)
         mat(k,2019) = 2.000_r8*rxt(k,223)*y(k,146) + rxt(k,225)*y(k,168) + rxt(k,616) &
                      *y(k,187)
         mat(k,2509) = mat(k,2509) + rxt(k,225)*y(k,159)
         mat(k,1775) = rxt(k,246)*y(k,146) + rxt(k,244)*y(k,254)
         mat(k,1645) = rxt(k,616)*y(k,159)
         mat(k,753) = mat(k,753) + rxt(k,244)*y(k,169)
         mat(k,1889) = 2.000_r8*rxt(k,236)*y(k,147)
         mat(k,2309) = rxt(k,232)*y(k,146)
         mat(k,2018) = -((rxt(k,222) + rxt(k,223) + rxt(k,224)) * y(k,146) + (rxt(k,225) &
                      + rxt(k,227)) * y(k,168) + rxt(k,226)*y(k,170) + rxt(k,238) &
                      *y(k,108) + rxt(k,239)*y(k,160) + rxt(k,240)*y(k,263) + rxt(k,253) &
                      *y(k,70) + rxt(k,263)*y(k,74) + rxt(k,288)*y(k,17) + rxt(k,298) &
                      *y(k,21) + rxt(k,319)*y(k,117) + rxt(k,333)*y(k,128) + rxt(k,444) &
                      *y(k,235) + rxt(k,494)*y(k,250) + rxt(k,552)*y(k,237) + rxt(k,555) &
                      *y(k,249) + rxt(k,558)*y(k,251) + rxt(k,562)*y(k,178) + rxt(k,565) &
                      *y(k,226) + rxt(k,616)*y(k,187))
         mat(k,1095) = -(rxt(k,222) + rxt(k,223) + rxt(k,224)) * y(k,159)
         mat(k,2508) = -(rxt(k,225) + rxt(k,227)) * y(k,159)
         mat(k,2411) = -rxt(k,226)*y(k,159)
         mat(k,2780) = -rxt(k,238)*y(k,159)
         mat(k,1960) = -rxt(k,239)*y(k,159)
         mat(k,2308) = -rxt(k,240)*y(k,159)
         mat(k,2571) = -rxt(k,253)*y(k,159)
         mat(k,2840) = -rxt(k,263)*y(k,159)
         mat(k,2810) = -rxt(k,288)*y(k,159)
         mat(k,2634) = -rxt(k,298)*y(k,159)
         mat(k,2664) = -rxt(k,319)*y(k,159)
         mat(k,2604) = -rxt(k,333)*y(k,159)
         mat(k,1572) = -rxt(k,444)*y(k,159)
         mat(k,1540) = -rxt(k,494)*y(k,159)
         mat(k,872) = -rxt(k,552)*y(k,159)
         mat(k,825) = -rxt(k,555)*y(k,159)
         mat(k,1024) = -rxt(k,558)*y(k,159)
         mat(k,558) = -rxt(k,562)*y(k,159)
         mat(k,620) = -rxt(k,565)*y(k,159)
         mat(k,1644) = -rxt(k,616)*y(k,159)
         mat(k,773) = rxt(k,496)*y(k,263)
         mat(k,429) = rxt(k,467)*y(k,160)
         mat(k,2634) = mat(k,2634) + rxt(k,295)*y(k,158)
         mat(k,1246) = rxt(k,382)*y(k,158) + rxt(k,383)*y(k,160)
         mat(k,627) = rxt(k,254)*y(k,263)
         mat(k,2840) = mat(k,2840) + rxt(k,261)*y(k,158)
         mat(k,2780) = mat(k,2780) + rxt(k,233)*y(k,158) + rxt(k,228)*y(k,160)
         mat(k,586) = rxt(k,218)*y(k,263)
         mat(k,2664) = mat(k,2664) + rxt(k,321)*y(k,160)
         mat(k,325) = 4.000_r8*rxt(k,318)*y(k,127)
         mat(k,2604) = mat(k,2604) + rxt(k,332)*y(k,158) + rxt(k,334)*y(k,160)
         mat(k,714) = .700_r8*rxt(k,516)*y(k,263)
         mat(k,2123) = rxt(k,295)*y(k,21) + rxt(k,382)*y(k,68) + rxt(k,261)*y(k,74) &
                      + rxt(k,233)*y(k,108) + rxt(k,332)*y(k,128) &
                      + 2.000_r8*rxt(k,229)*y(k,160) + rxt(k,235)*y(k,168) &
                      + rxt(k,234)*y(k,170) + rxt(k,339)*y(k,174) + rxt(k,537) &
                      *y(k,226) + rxt(k,498)*y(k,227) + rxt(k,540)*y(k,229) &
                      + rxt(k,545)*y(k,231) + rxt(k,422)*y(k,232) + rxt(k,450) &
                      *y(k,233) + rxt(k,547)*y(k,234) + rxt(k,433)*y(k,235) &
                      + rxt(k,401)*y(k,236) + rxt(k,551)*y(k,237) + rxt(k,469) &
                      *y(k,239) + rxt(k,437)*y(k,241) + rxt(k,409)*y(k,242) &
                      + .920_r8*rxt(k,508)*y(k,243) + .920_r8*rxt(k,514)*y(k,244) &
                      + rxt(k,522)*y(k,245) + rxt(k,476)*y(k,248) + rxt(k,554) &
                      *y(k,249) + rxt(k,485)*y(k,250) + rxt(k,557)*y(k,251) &
                      + rxt(k,488)*y(k,252) + 1.600_r8*rxt(k,587)*y(k,258) &
                      + rxt(k,560)*y(k,265) + rxt(k,459)*y(k,266) + rxt(k,463) &
                      *y(k,267) + .900_r8*rxt(k,592)*y(k,268) + .800_r8*rxt(k,597) &
                      *y(k,269) + rxt(k,567)*y(k,270) + rxt(k,533)*y(k,272) &
                      + rxt(k,573)*y(k,273) + rxt(k,576)*y(k,275)
         mat(k,1960) = mat(k,1960) + rxt(k,467)*y(k,16) + rxt(k,383)*y(k,68) &
                      + rxt(k,228)*y(k,108) + rxt(k,321)*y(k,117) + rxt(k,334) &
                      *y(k,128) + 2.000_r8*rxt(k,229)*y(k,158) + rxt(k,230)*y(k,168) &
                      + rxt(k,509)*y(k,243) + rxt(k,515)*y(k,244) + rxt(k,523) &
                      *y(k,245) + rxt(k,475)*y(k,248) + rxt(k,486)*y(k,250) &
                      + 2.000_r8*rxt(k,588)*y(k,258) + rxt(k,231)*y(k,263) &
                      + rxt(k,534)*y(k,272)
         mat(k,994) = rxt(k,457)*y(k,263)
         mat(k,2508) = mat(k,2508) + rxt(k,235)*y(k,158) + rxt(k,230)*y(k,160)
         mat(k,2411) = mat(k,2411) + rxt(k,234)*y(k,158)
         mat(k,899) = rxt(k,339)*y(k,158)
         mat(k,737) = rxt(k,594)*y(k,263)
         mat(k,620) = mat(k,620) + rxt(k,537)*y(k,158)
         mat(k,1139) = rxt(k,498)*y(k,158)
         mat(k,579) = rxt(k,540)*y(k,158)
         mat(k,517) = rxt(k,545)*y(k,158)
         mat(k,1004) = rxt(k,422)*y(k,158)
         mat(k,1039) = rxt(k,450)*y(k,158)
         mat(k,744) = rxt(k,547)*y(k,158)
         mat(k,1572) = mat(k,1572) + rxt(k,433)*y(k,158)
         mat(k,1743) = rxt(k,401)*y(k,158) + .500_r8*rxt(k,585)*y(k,258)
         mat(k,872) = mat(k,872) + rxt(k,551)*y(k,158)
         mat(k,658) = rxt(k,469)*y(k,158)
         mat(k,881) = rxt(k,437)*y(k,158)
         mat(k,523) = rxt(k,409)*y(k,158)
         mat(k,1445) = .920_r8*rxt(k,508)*y(k,158) + rxt(k,509)*y(k,160)
         mat(k,1478) = .920_r8*rxt(k,514)*y(k,158) + rxt(k,515)*y(k,160)
         mat(k,1399) = rxt(k,522)*y(k,158) + rxt(k,523)*y(k,160)
         mat(k,1500) = rxt(k,476)*y(k,158) + rxt(k,475)*y(k,160)
         mat(k,825) = mat(k,825) + rxt(k,554)*y(k,158)
         mat(k,1540) = mat(k,1540) + rxt(k,485)*y(k,158) + rxt(k,486)*y(k,160)
         mat(k,1024) = mat(k,1024) + rxt(k,557)*y(k,158)
         mat(k,781) = rxt(k,488)*y(k,158)
         mat(k,1222) = 1.600_r8*rxt(k,587)*y(k,158) + 2.000_r8*rxt(k,588)*y(k,160) &
                      + .500_r8*rxt(k,585)*y(k,236)
         mat(k,2308) = mat(k,2308) + rxt(k,496)*y(k,1) + rxt(k,254)*y(k,73) &
                      + rxt(k,218)*y(k,109) + .700_r8*rxt(k,516)*y(k,131) + rxt(k,231) &
                      *y(k,160) + rxt(k,457)*y(k,161) + rxt(k,594)*y(k,213)
         mat(k,536) = rxt(k,560)*y(k,158)
         mat(k,913) = rxt(k,459)*y(k,158)
         mat(k,1359) = rxt(k,463)*y(k,158)
         mat(k,1306) = .900_r8*rxt(k,592)*y(k,158)
         mat(k,1157) = .800_r8*rxt(k,597)*y(k,158)
         mat(k,839) = rxt(k,567)*y(k,158)
         mat(k,1377) = rxt(k,533)*y(k,158) + rxt(k,534)*y(k,160)
         mat(k,864) = rxt(k,573)*y(k,158)
         mat(k,605) = rxt(k,576)*y(k,158)
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
         mat(k,1959) = -(rxt(k,228)*y(k,108) + rxt(k,229)*y(k,158) + rxt(k,230) &
                      *y(k,168) + rxt(k,231)*y(k,263) + rxt(k,239)*y(k,159) + rxt(k,310) &
                      *y(k,106) + rxt(k,312)*y(k,118) + rxt(k,321)*y(k,117) + rxt(k,334) &
                      *y(k,128) + rxt(k,343)*y(k,112) + rxt(k,383)*y(k,68) + rxt(k,395) &
                      *y(k,51) + rxt(k,427)*y(k,54) + rxt(k,446)*y(k,33) + rxt(k,453) &
                      *y(k,58) + rxt(k,467)*y(k,16) + rxt(k,475)*y(k,248) + rxt(k,486) &
                      *y(k,250) + rxt(k,509)*y(k,243) + rxt(k,515)*y(k,244) + rxt(k,518) &
                      *y(k,130) + rxt(k,523)*y(k,245) + rxt(k,534)*y(k,272) + rxt(k,579) &
                      *y(k,6) + rxt(k,582)*y(k,144) + rxt(k,588)*y(k,258) + rxt(k,599) &
                      *y(k,215) + rxt(k,606)*y(k,83))
         mat(k,2779) = -rxt(k,228)*y(k,160)
         mat(k,2122) = -rxt(k,229)*y(k,160)
         mat(k,2507) = -rxt(k,230)*y(k,160)
         mat(k,2307) = -rxt(k,231)*y(k,160)
         mat(k,2017) = -rxt(k,239)*y(k,160)
         mat(k,1344) = -rxt(k,310)*y(k,160)
         mat(k,1630) = -rxt(k,312)*y(k,160)
         mat(k,2663) = -rxt(k,321)*y(k,160)
         mat(k,2603) = -rxt(k,334)*y(k,160)
         mat(k,1797) = -rxt(k,343)*y(k,160)
         mat(k,1245) = -rxt(k,383)*y(k,160)
         mat(k,2338) = -rxt(k,395)*y(k,160)
         mat(k,1278) = -rxt(k,427)*y(k,160)
         mat(k,1266) = -rxt(k,446)*y(k,160)
         mat(k,1422) = -rxt(k,453)*y(k,160)
         mat(k,428) = -rxt(k,467)*y(k,160)
         mat(k,1499) = -rxt(k,475)*y(k,160)
         mat(k,1539) = -rxt(k,486)*y(k,160)
         mat(k,1444) = -rxt(k,509)*y(k,160)
         mat(k,1477) = -rxt(k,515)*y(k,160)
         mat(k,986) = -rxt(k,518)*y(k,160)
         mat(k,1398) = -rxt(k,523)*y(k,160)
         mat(k,1376) = -rxt(k,534)*y(k,160)
         mat(k,1122) = -rxt(k,579)*y(k,160)
         mat(k,1067) = -rxt(k,582)*y(k,160)
         mat(k,1221) = -rxt(k,588)*y(k,160)
         mat(k,1168) = -rxt(k,599)*y(k,160)
         mat(k,1180) = -rxt(k,606)*y(k,160)
         mat(k,2809) = rxt(k,296)*y(k,22)
         mat(k,964) = rxt(k,296)*y(k,17) + rxt(k,297)*y(k,70) + rxt(k,299)*y(k,168)
         mat(k,2570) = rxt(k,297)*y(k,22) + rxt(k,262)*y(k,75)
         mat(k,1077) = rxt(k,262)*y(k,70) + rxt(k,264)*y(k,168) + rxt(k,265)*y(k,263)
         mat(k,1014) = rxt(k,355)*y(k,107)
         mat(k,2460) = rxt(k,355)*y(k,89) + rxt(k,241)*y(k,263)
         mat(k,2663) = mat(k,2663) + rxt(k,317)*y(k,129)
         mat(k,953) = rxt(k,317)*y(k,117)
         mat(k,674) = .500_r8*rxt(k,491)*y(k,263)
         mat(k,2017) = mat(k,2017) + rxt(k,227)*y(k,168) + rxt(k,226)*y(k,170)
         mat(k,2507) = mat(k,2507) + rxt(k,299)*y(k,22) + rxt(k,264)*y(k,75) &
                      + rxt(k,227)*y(k,159)
         mat(k,2410) = rxt(k,226)*y(k,159)
         mat(k,642) = rxt(k,442)*y(k,263)
         mat(k,2307) = mat(k,2307) + rxt(k,265)*y(k,75) + rxt(k,241)*y(k,107) &
                      + .500_r8*rxt(k,491)*y(k,143) + rxt(k,442)*y(k,176)
         mat(k,991) = -(rxt(k,457)*y(k,263))
         mat(k,2254) = -rxt(k,457)*y(k,161)
         mat(k,1254) = rxt(k,446)*y(k,160)
         mat(k,663) = .500_r8*rxt(k,517)*y(k,263)
         mat(k,461) = rxt(k,524)*y(k,263)
         mat(k,466) = rxt(k,528)*y(k,263)
         mat(k,1197) = rxt(k,529)*y(k,263)
         mat(k,1914) = rxt(k,446)*y(k,33)
         mat(k,2254) = mat(k,2254) + .500_r8*rxt(k,517)*y(k,132) + rxt(k,524)*y(k,133) &
                      + rxt(k,528)*y(k,149) + rxt(k,529)*y(k,150)
         mat(k,477) = -(rxt(k,589)*y(k,263))
         mat(k,2204) = -rxt(k,589)*y(k,162)
         mat(k,2703) = rxt(k,586)*y(k,258)
         mat(k,1212) = rxt(k,586)*y(k,108)
         mat(k,2515) = -(rxt(k,197)*y(k,170) + 4._r8*rxt(k,198)*y(k,168) + rxt(k,199) &
                      *y(k,169) + rxt(k,200)*y(k,93) + rxt(k,201)*y(k,95) + rxt(k,206) &
                      *y(k,108) + rxt(k,212)*y(k,263) + (rxt(k,225) + rxt(k,227) &
                      ) * y(k,159) + rxt(k,230)*y(k,160) + rxt(k,235)*y(k,158) &
                      + rxt(k,264)*y(k,75) + rxt(k,266)*y(k,74) + rxt(k,269)*y(k,102) &
                      + rxt(k,272)*y(k,111) + rxt(k,299)*y(k,22) + rxt(k,300)*y(k,21) &
                      + rxt(k,302)*y(k,98) + rxt(k,304)*y(k,110) + rxt(k,313)*y(k,118) &
                      + rxt(k,335)*y(k,128) + rxt(k,396)*y(k,51) + rxt(k,608)*y(k,173) &
                      + (rxt(k,747) + rxt(k,748)) * y(k,255) + rxt(k,749)*y(k,257))
         mat(k,2418) = -rxt(k,197)*y(k,168)
         mat(k,1780) = -rxt(k,199)*y(k,168)
         mat(k,1624) = -rxt(k,200)*y(k,168)
         mat(k,702) = -rxt(k,201)*y(k,168)
         mat(k,2787) = -rxt(k,206)*y(k,168)
         mat(k,2315) = -rxt(k,212)*y(k,168)
         mat(k,2025) = -(rxt(k,225) + rxt(k,227)) * y(k,168)
         mat(k,1967) = -rxt(k,230)*y(k,168)
         mat(k,2130) = -rxt(k,235)*y(k,168)
         mat(k,1081) = -rxt(k,264)*y(k,168)
         mat(k,2847) = -rxt(k,266)*y(k,168)
         mat(k,1848) = -rxt(k,269)*y(k,168)
         mat(k,1825) = -rxt(k,272)*y(k,168)
         mat(k,968) = -rxt(k,299)*y(k,168)
         mat(k,2641) = -rxt(k,300)*y(k,168)
         mat(k,1607) = -rxt(k,302)*y(k,168)
         mat(k,1693) = -rxt(k,304)*y(k,168)
         mat(k,1634) = -rxt(k,313)*y(k,168)
         mat(k,2611) = -rxt(k,335)*y(k,168)
         mat(k,2346) = -rxt(k,396)*y(k,168)
         mat(k,450) = -rxt(k,608)*y(k,168)
         mat(k,727) = -(rxt(k,747) + rxt(k,748)) * y(k,168)
         mat(k,596) = -rxt(k,749)*y(k,168)
         mat(k,2442) = rxt(k,204)*y(k,108)
         mat(k,2787) = mat(k,2787) + rxt(k,204)*y(k,92)
         mat(k,1099) = rxt(k,221)*y(k,158) + rxt(k,222)*y(k,159) + rxt(k,246)*y(k,169) &
                      + rxt(k,752)*y(k,262)
         mat(k,2130) = mat(k,2130) + rxt(k,221)*y(k,146) + rxt(k,219)*y(k,254)
         mat(k,2025) = mat(k,2025) + rxt(k,222)*y(k,146)
         mat(k,1780) = mat(k,1780) + rxt(k,246)*y(k,146) + rxt(k,610)*y(k,185) &
                      + rxt(k,617)*y(k,187) + rxt(k,751)*y(k,257) + (rxt(k,185) &
                       +rxt(k,186))*y(k,259) + rxt(k,757)*y(k,264)
         mat(k,2418) = mat(k,2418) + 2.000_r8*rxt(k,188)*y(k,259)
         mat(k,806) = rxt(k,610)*y(k,169)
         mat(k,1649) = rxt(k,617)*y(k,169)
         mat(k,947) = rxt(k,743)*y(k,256) + 1.150_r8*rxt(k,744)*y(k,262)
         mat(k,754) = rxt(k,219)*y(k,158)
         mat(k,926) = rxt(k,743)*y(k,238)
         mat(k,596) = mat(k,596) + rxt(k,751)*y(k,169)
         mat(k,1895) = (rxt(k,185)+rxt(k,186))*y(k,169) + 2.000_r8*rxt(k,188)*y(k,170)
         mat(k,934) = rxt(k,752)*y(k,146) + 1.150_r8*rxt(k,744)*y(k,238)
         mat(k,2315) = mat(k,2315) + 2.000_r8*rxt(k,214)*y(k,263)
         mat(k,893) = rxt(k,757)*y(k,169)
         mat(k,1772) = -(rxt(k,185)*y(k,259) + rxt(k,191)*y(k,260) + rxt(k,199) &
                      *y(k,168) + rxt(k,205)*y(k,92) + rxt(k,244)*y(k,254) + rxt(k,246) &
                      *y(k,146) + rxt(k,439)*y(k,240) + rxt(k,610)*y(k,185) + rxt(k,617) &
                      *y(k,187) + rxt(k,746)*y(k,255) + (rxt(k,750) + rxt(k,751) &
                      ) * y(k,257) + rxt(k,757)*y(k,264))
         mat(k,1882) = -rxt(k,185)*y(k,169)
         mat(k,205) = -rxt(k,191)*y(k,169)
         mat(k,2502) = -rxt(k,199)*y(k,169)
         mat(k,2430) = -rxt(k,205)*y(k,169)
         mat(k,751) = -rxt(k,244)*y(k,169)
         mat(k,1093) = -rxt(k,246)*y(k,169)
         mat(k,547) = -rxt(k,439)*y(k,169)
         mat(k,802) = -rxt(k,610)*y(k,169)
         mat(k,1643) = -rxt(k,617)*y(k,169)
         mat(k,726) = -rxt(k,746)*y(k,169)
         mat(k,595) = -(rxt(k,750) + rxt(k,751)) * y(k,169)
         mat(k,892) = -rxt(k,757)*y(k,169)
         mat(k,2804) = rxt(k,287)*y(k,108) + rxt(k,289)*y(k,170)
         mat(k,2628) = 2.000_r8*rxt(k,290)*y(k,21) + (rxt(k,292)+rxt(k,293))*y(k,74) &
                      + rxt(k,294)*y(k,108) + rxt(k,326)*y(k,128) + rxt(k,300) &
                      *y(k,168)
         mat(k,1243) = rxt(k,380)*y(k,108)
         mat(k,2565) = rxt(k,251)*y(k,108) + rxt(k,255)*y(k,170)
         mat(k,2835) = (rxt(k,292)+rxt(k,293))*y(k,21) + (2.000_r8*rxt(k,257) &
                       +2.000_r8*rxt(k,258))*y(k,74) + rxt(k,260)*y(k,108) + ( &
                      + rxt(k,329)+rxt(k,330))*y(k,128) + rxt(k,266)*y(k,168) &
                      + rxt(k,268)*y(k,263)
         mat(k,2430) = mat(k,2430) + rxt(k,202)*y(k,108) + rxt(k,208)*y(k,170)
         mat(k,2774) = rxt(k,287)*y(k,17) + rxt(k,294)*y(k,21) + rxt(k,380)*y(k,68) &
                      + rxt(k,251)*y(k,70) + rxt(k,260)*y(k,74) + rxt(k,202)*y(k,92) &
                      + 2.000_r8*rxt(k,216)*y(k,108) + rxt(k,316)*y(k,117) &
                      + rxt(k,331)*y(k,128) + rxt(k,228)*y(k,160) + rxt(k,206) &
                      *y(k,168) + 2.000_r8*rxt(k,207)*y(k,170) + rxt(k,421)*y(k,232) &
                      + rxt(k,449)*y(k,233) + rxt(k,400)*y(k,236) + rxt(k,211) &
                      *y(k,263) + rxt(k,458)*y(k,266)
         mat(k,584) = rxt(k,218)*y(k,263)
         mat(k,2658) = rxt(k,316)*y(k,108) + rxt(k,324)*y(k,170)
         mat(k,301) = rxt(k,341)*y(k,276)
         mat(k,2598) = rxt(k,326)*y(k,21) + (rxt(k,329)+rxt(k,330))*y(k,74) &
                      + rxt(k,331)*y(k,108) + rxt(k,335)*y(k,168) + rxt(k,336) &
                      *y(k,170)
         mat(k,1093) = mat(k,1093) + rxt(k,224)*y(k,159)
         mat(k,305) = rxt(k,237)*y(k,259)
         mat(k,2117) = rxt(k,234)*y(k,170) + rxt(k,754)*y(k,262)
         mat(k,2012) = rxt(k,224)*y(k,146) + rxt(k,225)*y(k,168) + rxt(k,226)*y(k,170)
         mat(k,1954) = rxt(k,228)*y(k,108) + rxt(k,230)*y(k,168)
         mat(k,2502) = mat(k,2502) + rxt(k,300)*y(k,21) + rxt(k,266)*y(k,74) &
                      + rxt(k,206)*y(k,108) + rxt(k,335)*y(k,128) + rxt(k,225) &
                      *y(k,159) + rxt(k,230)*y(k,160) + 2.000_r8*rxt(k,198)*y(k,168) &
                      + 2.000_r8*rxt(k,197)*y(k,170) + rxt(k,190)*y(k,260) &
                      + rxt(k,212)*y(k,263)
         mat(k,1772) = mat(k,1772) + 2.000_r8*rxt(k,191)*y(k,260)
         mat(k,2405) = rxt(k,289)*y(k,17) + rxt(k,255)*y(k,70) + rxt(k,208)*y(k,92) &
                      + 2.000_r8*rxt(k,207)*y(k,108) + rxt(k,324)*y(k,117) &
                      + rxt(k,336)*y(k,128) + rxt(k,234)*y(k,158) + rxt(k,226) &
                      *y(k,159) + 2.000_r8*rxt(k,197)*y(k,168) + rxt(k,612)*y(k,185) &
                      + rxt(k,618)*y(k,187) + (2.000_r8*rxt(k,187)+rxt(k,188)) &
                      *y(k,259) + rxt(k,213)*y(k,263)
         mat(k,802) = mat(k,802) + rxt(k,612)*y(k,170)
         mat(k,1643) = mat(k,1643) + rxt(k,618)*y(k,170)
         mat(k,1003) = rxt(k,421)*y(k,108)
         mat(k,1038) = rxt(k,449)*y(k,108)
         mat(k,1738) = rxt(k,400)*y(k,108)
         mat(k,1882) = mat(k,1882) + rxt(k,237)*y(k,147) + (2.000_r8*rxt(k,187) &
                       +rxt(k,188))*y(k,170)
         mat(k,205) = mat(k,205) + rxt(k,190)*y(k,168) + 2.000_r8*rxt(k,191)*y(k,169)
         mat(k,931) = rxt(k,754)*y(k,158)
         mat(k,2302) = rxt(k,268)*y(k,74) + rxt(k,211)*y(k,108) + rxt(k,218)*y(k,109) &
                      + rxt(k,212)*y(k,168) + rxt(k,213)*y(k,170)
         mat(k,912) = rxt(k,458)*y(k,108)
         mat(k,2867) = rxt(k,341)*y(k,122)
         mat(k,2415) = -((rxt(k,187) + rxt(k,188)) * y(k,259) + rxt(k,197)*y(k,168) &
                      + rxt(k,207)*y(k,108) + rxt(k,208)*y(k,92) + rxt(k,213)*y(k,263) &
                      + rxt(k,226)*y(k,159) + rxt(k,234)*y(k,158) + rxt(k,255)*y(k,70) &
                      + rxt(k,289)*y(k,17) + rxt(k,324)*y(k,117) + rxt(k,336)*y(k,128) &
                      + rxt(k,418)*y(k,28) + rxt(k,447)*y(k,33) + rxt(k,478)*y(k,139) &
                      + rxt(k,492)*y(k,145) + rxt(k,525)*y(k,130) + rxt(k,563) &
                      *y(k,178) + rxt(k,580)*y(k,6) + rxt(k,583)*y(k,144) + rxt(k,612) &
                      *y(k,185) + rxt(k,618)*y(k,187))
         mat(k,1892) = -(rxt(k,187) + rxt(k,188)) * y(k,170)
         mat(k,2512) = -rxt(k,197)*y(k,170)
         mat(k,2784) = -rxt(k,207)*y(k,170)
         mat(k,2439) = -rxt(k,208)*y(k,170)
         mat(k,2312) = -rxt(k,213)*y(k,170)
         mat(k,2022) = -rxt(k,226)*y(k,170)
         mat(k,2127) = -rxt(k,234)*y(k,170)
         mat(k,2575) = -rxt(k,255)*y(k,170)
         mat(k,2814) = -rxt(k,289)*y(k,170)
         mat(k,2668) = -rxt(k,324)*y(k,170)
         mat(k,2608) = -rxt(k,336)*y(k,170)
         mat(k,636) = -rxt(k,418)*y(k,170)
         mat(k,1271) = -rxt(k,447)*y(k,170)
         mat(k,1415) = -rxt(k,478)*y(k,170)
         mat(k,1526) = -rxt(k,492)*y(k,170)
         mat(k,989) = -rxt(k,525)*y(k,170)
         mat(k,559) = -rxt(k,563)*y(k,170)
         mat(k,1125) = -rxt(k,580)*y(k,170)
         mat(k,1070) = -rxt(k,583)*y(k,170)
         mat(k,804) = -rxt(k,612)*y(k,170)
         mat(k,1647) = -rxt(k,618)*y(k,170)
         mat(k,2784) = mat(k,2784) + .150_r8*rxt(k,432)*y(k,235) + .150_r8*rxt(k,483) &
                      *y(k,250)
         mat(k,2512) = mat(k,2512) + rxt(k,199)*y(k,169)
         mat(k,1778) = rxt(k,199)*y(k,168)
         mat(k,1576) = .150_r8*rxt(k,432)*y(k,108)
         mat(k,1544) = .150_r8*rxt(k,483)*y(k,108)
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
         mat(k,540) = -(rxt(k,619)*y(k,187))
         mat(k,1639) = -rxt(k,619)*y(k,172)
         mat(k,2620) = rxt(k,291)*y(k,74)
         mat(k,2828) = rxt(k,291)*y(k,21) + 2.000_r8*rxt(k,259)*y(k,74) + rxt(k,328) &
                      *y(k,128)
         mat(k,2590) = rxt(k,328)*y(k,74)
         mat(k,443) = -(rxt(k,608)*y(k,168) + rxt(k,609)*y(k,263))
         mat(k,2479) = -rxt(k,608)*y(k,173)
         mat(k,2199) = -rxt(k,609)*y(k,173)
         mat(k,897) = -(rxt(k,339)*y(k,158) + rxt(k,342)*y(k,263) + rxt(k,350) &
                      *y(k,128) + 4._r8*rxt(k,351)*y(k,174))
         mat(k,2076) = -rxt(k,339)*y(k,174)
         mat(k,2250) = -rxt(k,342)*y(k,174)
         mat(k,2592) = -rxt(k,350)*y(k,174)
         mat(k,2622) = rxt(k,327)*y(k,128)
         mat(k,2592) = mat(k,2592) + rxt(k,327)*y(k,21) + 2.000_r8*rxt(k,347)*y(k,128) &
                      + rxt(k,334)*y(k,160) + rxt(k,336)*y(k,170)
         mat(k,1911) = rxt(k,334)*y(k,128)
         mat(k,2372) = rxt(k,336)*y(k,128)
         mat(k,1313) = rxt(k,471)*y(k,263)
         mat(k,2048) = .100_r8*rxt(k,592)*y(k,268)
         mat(k,2179) = rxt(k,471)*y(k,113)
         mat(k,1294) = .100_r8*rxt(k,592)*y(k,158)
         mat(k,638) = -(rxt(k,442)*y(k,263))
         mat(k,2224) = -rxt(k,442)*y(k,176)
         mat(k,1989) = rxt(k,444)*y(k,235)
         mat(k,1549) = rxt(k,444)*y(k,159)
         mat(k,1976) = rxt(k,565)*y(k,226)
         mat(k,617) = rxt(k,565)*y(k,159)
         mat(k,556) = -(rxt(k,562)*y(k,159) + rxt(k,563)*y(k,170))
         mat(k,1983) = -rxt(k,562)*y(k,178)
         mat(k,2366) = -rxt(k,563)*y(k,178)
         mat(k,233) = .070_r8*rxt(k,549)*y(k,263)
         mat(k,2057) = rxt(k,547)*y(k,234)
         mat(k,201) = .060_r8*rxt(k,561)*y(k,263)
         mat(k,254) = .070_r8*rxt(k,577)*y(k,263)
         mat(k,742) = rxt(k,547)*y(k,158)
         mat(k,2213) = .070_r8*rxt(k,549)*y(k,82) + .060_r8*rxt(k,561)*y(k,179) &
                      + .070_r8*rxt(k,577)*y(k,222)
         mat(k,199) = -(rxt(k,561)*y(k,263))
         mat(k,2163) = -rxt(k,561)*y(k,179)
         mat(k,191) = .530_r8*rxt(k,538)*y(k,263)
         mat(k,2163) = mat(k,2163) + .530_r8*rxt(k,538)*y(k,7)
         mat(k,392) = -(rxt(k,564)*y(k,263))
         mat(k,2192) = -rxt(k,564)*y(k,180)
         mat(k,2698) = rxt(k,559)*y(k,265)
         mat(k,533) = rxt(k,559)*y(k,108)
         mat(k,646) = -(rxt(k,460)*y(k,263))
         mat(k,2225) = -rxt(k,460)*y(k,183)
         mat(k,2719) = rxt(k,458)*y(k,266)
         mat(k,908) = rxt(k,458)*y(k,108)
         mat(k,489) = -(rxt(k,464)*y(k,263))
         mat(k,2205) = -rxt(k,464)*y(k,184)
         mat(k,2704) = .850_r8*rxt(k,462)*y(k,267)
         mat(k,1352) = .850_r8*rxt(k,462)*y(k,108)
         mat(k,800) = -(rxt(k,610)*y(k,169) + rxt(k,612)*y(k,170) + rxt(k,615) &
                      *y(k,263))
         mat(k,1761) = -rxt(k,610)*y(k,185)
         mat(k,2370) = -rxt(k,612)*y(k,185)
         mat(k,2242) = -rxt(k,615)*y(k,185)
         mat(k,1642) = -(rxt(k,613)*y(k,21) + rxt(k,614)*y(k,74) + rxt(k,616)*y(k,159) &
                      + rxt(k,617)*y(k,169) + rxt(k,618)*y(k,170) + rxt(k,619) &
                      *y(k,172) + rxt(k,620)*y(k,263))
         mat(k,2626) = -rxt(k,613)*y(k,187)
         mat(k,2833) = -rxt(k,614)*y(k,187)
         mat(k,2008) = -rxt(k,616)*y(k,187)
         mat(k,1771) = -rxt(k,617)*y(k,187)
         mat(k,2402) = -rxt(k,618)*y(k,187)
         mat(k,542) = -rxt(k,619)*y(k,187)
         mat(k,2298) = -rxt(k,620)*y(k,187)
         mat(k,2498) = rxt(k,608)*y(k,173)
         mat(k,1771) = mat(k,1771) + rxt(k,610)*y(k,185)
         mat(k,2402) = mat(k,2402) + rxt(k,612)*y(k,185)
         mat(k,447) = rxt(k,608)*y(k,168)
         mat(k,801) = rxt(k,610)*y(k,169) + rxt(k,612)*y(k,170) + rxt(k,615)*y(k,263)
         mat(k,2298) = mat(k,2298) + rxt(k,615)*y(k,185)
         mat(k,1190) = -(rxt(k,611)*y(k,263))
         mat(k,2270) = -rxt(k,611)*y(k,188)
         mat(k,2625) = rxt(k,602)*y(k,83) + rxt(k,613)*y(k,187)
         mat(k,2551) = rxt(k,604)*y(k,83)
         mat(k,2832) = rxt(k,614)*y(k,187)
         mat(k,1178) = rxt(k,602)*y(k,21) + rxt(k,604)*y(k,70) + rxt(k,605)*y(k,128) &
                      + rxt(k,606)*y(k,160) + (rxt(k,607)+.500_r8*rxt(k,621))*y(k,263)
         mat(k,2595) = rxt(k,605)*y(k,83)
         mat(k,2000) = rxt(k,616)*y(k,187)
         mat(k,1924) = rxt(k,606)*y(k,83)
         mat(k,1767) = rxt(k,617)*y(k,187)
         mat(k,2382) = rxt(k,618)*y(k,187)
         mat(k,541) = rxt(k,619)*y(k,187)
         mat(k,445) = rxt(k,609)*y(k,263)
         mat(k,1641) = rxt(k,613)*y(k,21) + rxt(k,614)*y(k,74) + rxt(k,616)*y(k,159) &
                      + rxt(k,617)*y(k,169) + rxt(k,618)*y(k,170) + rxt(k,619) &
                      *y(k,172) + rxt(k,620)*y(k,263)
         mat(k,2270) = mat(k,2270) + (rxt(k,607)+.500_r8*rxt(k,621))*y(k,83) &
                      + rxt(k,609)*y(k,173) + rxt(k,620)*y(k,187)
         mat(k,313) = -(rxt(k,622)*y(k,276))
         mat(k,2859) = -rxt(k,622)*y(k,189)
         mat(k,1189) = rxt(k,611)*y(k,263)
         mat(k,2182) = rxt(k,611)*y(k,188)
         mat(k,1100) = .2202005_r8*rxt(k,675)*y(k,170)
         mat(k,2680) = .2202005_r8*rxt(k,673)*y(k,228) + .0023005_r8*rxt(k,678) &
                      *y(k,230) + .0031005_r8*rxt(k,681)*y(k,246) &
                      + .2381005_r8*rxt(k,685)*y(k,247) + .0508005_r8*rxt(k,689) &
                      *y(k,253) + .1364005_r8*rxt(k,695)*y(k,271) &
                      + .1677005_r8*rxt(k,699)*y(k,274)
         mat(k,1045) = .0508005_r8*rxt(k,691)*y(k,170)
         mat(k,2034) = .1279005_r8*rxt(k,674)*y(k,228) + .0097005_r8*rxt(k,679) &
                      *y(k,230) + .0003005_r8*rxt(k,682)*y(k,246) &
                      + .1056005_r8*rxt(k,686)*y(k,247) + .0245005_r8*rxt(k,690) &
                      *y(k,253) + .0154005_r8*rxt(k,696)*y(k,271) &
                      + .0063005_r8*rxt(k,700)*y(k,274)
         mat(k,2355) = .2202005_r8*rxt(k,675)*y(k,6) + .0508005_r8*rxt(k,691)*y(k,144)
         mat(k,57) = .5931005_r8*rxt(k,693)*y(k,263)
         mat(k,63) = .2202005_r8*rxt(k,673)*y(k,108) + .1279005_r8*rxt(k,674)*y(k,158)
         mat(k,69) = .0023005_r8*rxt(k,678)*y(k,108) + .0097005_r8*rxt(k,679)*y(k,158)
         mat(k,75) = .0031005_r8*rxt(k,681)*y(k,108) + .0003005_r8*rxt(k,682)*y(k,158)
         mat(k,81) = .2381005_r8*rxt(k,685)*y(k,108) + .1056005_r8*rxt(k,686)*y(k,158)
         mat(k,89) = .0508005_r8*rxt(k,689)*y(k,108) + .0245005_r8*rxt(k,690)*y(k,158)
         mat(k,2140) = .5931005_r8*rxt(k,693)*y(k,210)
         mat(k,95) = .1364005_r8*rxt(k,695)*y(k,108) + .0154005_r8*rxt(k,696)*y(k,158)
         mat(k,101) = .1677005_r8*rxt(k,699)*y(k,108) + .0063005_r8*rxt(k,700) &
                      *y(k,158)
         mat(k,1101) = .2067005_r8*rxt(k,675)*y(k,170)
         mat(k,2681) = .2067005_r8*rxt(k,673)*y(k,228) + .0008005_r8*rxt(k,678) &
                      *y(k,230) + .0035005_r8*rxt(k,681)*y(k,246) &
                      + .1308005_r8*rxt(k,685)*y(k,247) + .1149005_r8*rxt(k,689) &
                      *y(k,253) + .0101005_r8*rxt(k,695)*y(k,271) &
                      + .0174005_r8*rxt(k,699)*y(k,274)
         mat(k,1046) = .1149005_r8*rxt(k,691)*y(k,170)
         mat(k,2035) = .1792005_r8*rxt(k,674)*y(k,228) + .0034005_r8*rxt(k,679) &
                      *y(k,230) + .0003005_r8*rxt(k,682)*y(k,246) &
                      + .1026005_r8*rxt(k,686)*y(k,247) + .0082005_r8*rxt(k,690) &
                      *y(k,253) + .0452005_r8*rxt(k,696)*y(k,271) &
                      + .0237005_r8*rxt(k,700)*y(k,274)
         mat(k,2356) = .2067005_r8*rxt(k,675)*y(k,6) + .1149005_r8*rxt(k,691)*y(k,144)
         mat(k,58) = .1534005_r8*rxt(k,693)*y(k,263)
         mat(k,64) = .2067005_r8*rxt(k,673)*y(k,108) + .1792005_r8*rxt(k,674)*y(k,158)
         mat(k,70) = .0008005_r8*rxt(k,678)*y(k,108) + .0034005_r8*rxt(k,679)*y(k,158)
         mat(k,76) = .0035005_r8*rxt(k,681)*y(k,108) + .0003005_r8*rxt(k,682)*y(k,158)
         mat(k,82) = .1308005_r8*rxt(k,685)*y(k,108) + .1026005_r8*rxt(k,686)*y(k,158)
         mat(k,90) = .1149005_r8*rxt(k,689)*y(k,108) + .0082005_r8*rxt(k,690)*y(k,158)
         mat(k,2141) = .1534005_r8*rxt(k,693)*y(k,210)
         mat(k,96) = .0101005_r8*rxt(k,695)*y(k,108) + .0452005_r8*rxt(k,696)*y(k,158)
         mat(k,102) = .0174005_r8*rxt(k,699)*y(k,108) + .0237005_r8*rxt(k,700) &
                      *y(k,158)
         mat(k,1102) = .0653005_r8*rxt(k,675)*y(k,170)
         mat(k,2682) = .0653005_r8*rxt(k,673)*y(k,228) + .0843005_r8*rxt(k,678) &
                      *y(k,230) + .0003005_r8*rxt(k,681)*y(k,246) &
                      + .0348005_r8*rxt(k,685)*y(k,247) + .0348005_r8*rxt(k,689) &
                      *y(k,253) + .0763005_r8*rxt(k,695)*y(k,271) + .086_r8*rxt(k,699) &
                      *y(k,274)
         mat(k,1047) = .0348005_r8*rxt(k,691)*y(k,170)
         mat(k,2036) = .0676005_r8*rxt(k,674)*y(k,228) + .1579005_r8*rxt(k,679) &
                      *y(k,230) + .0073005_r8*rxt(k,682)*y(k,246) &
                      + .0521005_r8*rxt(k,686)*y(k,247) + .0772005_r8*rxt(k,690) &
                      *y(k,253) + .0966005_r8*rxt(k,696)*y(k,271) &
                      + .0025005_r8*rxt(k,700)*y(k,274)
         mat(k,2357) = .0653005_r8*rxt(k,675)*y(k,6) + .0348005_r8*rxt(k,691)*y(k,144)
         mat(k,59) = .0459005_r8*rxt(k,693)*y(k,263)
         mat(k,65) = .0653005_r8*rxt(k,673)*y(k,108) + .0676005_r8*rxt(k,674)*y(k,158)
         mat(k,71) = .0843005_r8*rxt(k,678)*y(k,108) + .1579005_r8*rxt(k,679)*y(k,158)
         mat(k,77) = .0003005_r8*rxt(k,681)*y(k,108) + .0073005_r8*rxt(k,682)*y(k,158)
         mat(k,83) = .0348005_r8*rxt(k,685)*y(k,108) + .0521005_r8*rxt(k,686)*y(k,158)
         mat(k,91) = .0348005_r8*rxt(k,689)*y(k,108) + .0772005_r8*rxt(k,690)*y(k,158)
         mat(k,2142) = .0459005_r8*rxt(k,693)*y(k,210)
         mat(k,97) = .0763005_r8*rxt(k,695)*y(k,108) + .0966005_r8*rxt(k,696)*y(k,158)
         mat(k,103) = .086_r8*rxt(k,699)*y(k,108) + .0025005_r8*rxt(k,700)*y(k,158)
         mat(k,1103) = .1749305_r8*rxt(k,672)*y(k,160) + .1284005_r8*rxt(k,675) &
                      *y(k,170)
         mat(k,2683) = .1284005_r8*rxt(k,673)*y(k,228) + .0443005_r8*rxt(k,678) &
                      *y(k,230) + .0271005_r8*rxt(k,681)*y(k,246) &
                      + .0076005_r8*rxt(k,685)*y(k,247) + .0554005_r8*rxt(k,689) &
                      *y(k,253) + .2157005_r8*rxt(k,695)*y(k,271) &
                      + .0512005_r8*rxt(k,699)*y(k,274)
         mat(k,972) = .0590245_r8*rxt(k,680)*y(k,160) + .0033005_r8*rxt(k,683) &
                      *y(k,170)
         mat(k,1048) = .1749305_r8*rxt(k,688)*y(k,160) + .0554005_r8*rxt(k,691) &
                      *y(k,170)
         mat(k,2037) = .079_r8*rxt(k,674)*y(k,228) + .0059005_r8*rxt(k,679)*y(k,230) &
                      + .0057005_r8*rxt(k,682)*y(k,246) + .0143005_r8*rxt(k,686) &
                      *y(k,247) + .0332005_r8*rxt(k,690)*y(k,253) &
                      + .0073005_r8*rxt(k,696)*y(k,271) + .011_r8*rxt(k,700)*y(k,274)
         mat(k,1904) = .1749305_r8*rxt(k,672)*y(k,6) + .0590245_r8*rxt(k,680)*y(k,130) &
                      + .1749305_r8*rxt(k,688)*y(k,144)
         mat(k,2358) = .1284005_r8*rxt(k,675)*y(k,6) + .0033005_r8*rxt(k,683)*y(k,130) &
                      + .0554005_r8*rxt(k,691)*y(k,144)
         mat(k,60) = .0085005_r8*rxt(k,693)*y(k,263)
         mat(k,66) = .1284005_r8*rxt(k,673)*y(k,108) + .079_r8*rxt(k,674)*y(k,158)
         mat(k,72) = .0443005_r8*rxt(k,678)*y(k,108) + .0059005_r8*rxt(k,679)*y(k,158)
         mat(k,78) = .0271005_r8*rxt(k,681)*y(k,108) + .0057005_r8*rxt(k,682)*y(k,158)
         mat(k,84) = .0076005_r8*rxt(k,685)*y(k,108) + .0143005_r8*rxt(k,686)*y(k,158)
         mat(k,92) = .0554005_r8*rxt(k,689)*y(k,108) + .0332005_r8*rxt(k,690)*y(k,158)
         mat(k,2143) = .0085005_r8*rxt(k,693)*y(k,210)
         mat(k,98) = .2157005_r8*rxt(k,695)*y(k,108) + .0073005_r8*rxt(k,696)*y(k,158)
         mat(k,104) = .0512005_r8*rxt(k,699)*y(k,108) + .011_r8*rxt(k,700)*y(k,158)
         mat(k,1104) = .5901905_r8*rxt(k,672)*y(k,160) + .114_r8*rxt(k,675)*y(k,170)
         mat(k,2684) = .114_r8*rxt(k,673)*y(k,228) + .1621005_r8*rxt(k,678)*y(k,230) &
                      + .0474005_r8*rxt(k,681)*y(k,246) + .0113005_r8*rxt(k,685) &
                      *y(k,247) + .1278005_r8*rxt(k,689)*y(k,253) &
                      + .0738005_r8*rxt(k,695)*y(k,271) + .1598005_r8*rxt(k,699) &
                      *y(k,274)
         mat(k,973) = .0250245_r8*rxt(k,680)*y(k,160)
         mat(k,1049) = .5901905_r8*rxt(k,688)*y(k,160) + .1278005_r8*rxt(k,691) &
                      *y(k,170)
         mat(k,2038) = .1254005_r8*rxt(k,674)*y(k,228) + .0536005_r8*rxt(k,679) &
                      *y(k,230) + .0623005_r8*rxt(k,682)*y(k,246) &
                      + .0166005_r8*rxt(k,686)*y(k,247) + .130_r8*rxt(k,690)*y(k,253) &
                      + .238_r8*rxt(k,696)*y(k,271) + .1185005_r8*rxt(k,700)*y(k,274)
         mat(k,1905) = .5901905_r8*rxt(k,672)*y(k,6) + .0250245_r8*rxt(k,680)*y(k,130) &
                      + .5901905_r8*rxt(k,688)*y(k,144)
         mat(k,2359) = .114_r8*rxt(k,675)*y(k,6) + .1278005_r8*rxt(k,691)*y(k,144)
         mat(k,61) = .0128005_r8*rxt(k,693)*y(k,263)
         mat(k,67) = .114_r8*rxt(k,673)*y(k,108) + .1254005_r8*rxt(k,674)*y(k,158)
         mat(k,73) = .1621005_r8*rxt(k,678)*y(k,108) + .0536005_r8*rxt(k,679)*y(k,158)
         mat(k,79) = .0474005_r8*rxt(k,681)*y(k,108) + .0623005_r8*rxt(k,682)*y(k,158)
         mat(k,85) = .0113005_r8*rxt(k,685)*y(k,108) + .0166005_r8*rxt(k,686)*y(k,158)
         mat(k,93) = .1278005_r8*rxt(k,689)*y(k,108) + .130_r8*rxt(k,690)*y(k,158)
         mat(k,2144) = .0128005_r8*rxt(k,693)*y(k,210)
         mat(k,99) = .0738005_r8*rxt(k,695)*y(k,108) + .238_r8*rxt(k,696)*y(k,158)
         mat(k,105) = .1598005_r8*rxt(k,699)*y(k,108) + .1185005_r8*rxt(k,700) &
                      *y(k,158)
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
         mat(k,62) = -(rxt(k,693)*y(k,263))
         mat(k,2145) = -rxt(k,693)*y(k,210)
         mat(k,226) = .100_r8*rxt(k,569)*y(k,263)
         mat(k,244) = .230_r8*rxt(k,571)*y(k,263)
         mat(k,2168) = .100_r8*rxt(k,569)*y(k,218) + .230_r8*rxt(k,571)*y(k,220)
         mat(k,785) = -(rxt(k,593)*y(k,263))
         mat(k,2240) = -rxt(k,593)*y(k,212)
         mat(k,2727) = rxt(k,591)*y(k,268)
         mat(k,1295) = rxt(k,591)*y(k,108)
         mat(k,735) = -(rxt(k,594)*y(k,263))
         mat(k,2235) = -rxt(k,594)*y(k,213)
         mat(k,2066) = .200_r8*rxt(k,587)*y(k,258) + .200_r8*rxt(k,597)*y(k,269)
         mat(k,1705) = .500_r8*rxt(k,585)*y(k,258)
         mat(k,1213) = .200_r8*rxt(k,587)*y(k,158) + .500_r8*rxt(k,585)*y(k,236)
         mat(k,1151) = .200_r8*rxt(k,597)*y(k,158)
         mat(k,567) = -(rxt(k,598)*y(k,263))
         mat(k,2215) = -rxt(k,598)*y(k,214)
         mat(k,2714) = rxt(k,596)*y(k,269)
         mat(k,1150) = rxt(k,596)*y(k,108)
         mat(k,1163) = -(rxt(k,599)*y(k,160) + rxt(k,600)*y(k,263))
         mat(k,1921) = -rxt(k,599)*y(k,215)
         mat(k,2267) = -rxt(k,600)*y(k,215)
         mat(k,1113) = .330_r8*rxt(k,580)*y(k,170)
         mat(k,1058) = .330_r8*rxt(k,583)*y(k,170)
         mat(k,2090) = .800_r8*rxt(k,587)*y(k,258) + .800_r8*rxt(k,597)*y(k,269)
         mat(k,1921) = mat(k,1921) + rxt(k,588)*y(k,258)
         mat(k,2380) = .330_r8*rxt(k,580)*y(k,6) + .330_r8*rxt(k,583)*y(k,144)
         mat(k,736) = rxt(k,594)*y(k,263)
         mat(k,1713) = .500_r8*rxt(k,585)*y(k,258) + rxt(k,595)*y(k,269)
         mat(k,1215) = .800_r8*rxt(k,587)*y(k,158) + rxt(k,588)*y(k,160) &
                      + .500_r8*rxt(k,585)*y(k,236)
         mat(k,2267) = mat(k,2267) + rxt(k,594)*y(k,213)
         mat(k,1154) = .800_r8*rxt(k,597)*y(k,158) + rxt(k,595)*y(k,236)
         mat(k,1229) = -(rxt(k,601)*y(k,263))
         mat(k,2273) = -rxt(k,601)*y(k,216)
         mat(k,1116) = .300_r8*rxt(k,580)*y(k,170)
         mat(k,1061) = .300_r8*rxt(k,583)*y(k,170)
         mat(k,2094) = .900_r8*rxt(k,592)*y(k,268)
         mat(k,2384) = .300_r8*rxt(k,580)*y(k,6) + .300_r8*rxt(k,583)*y(k,144)
         mat(k,1717) = rxt(k,590)*y(k,268)
         mat(k,1298) = .900_r8*rxt(k,592)*y(k,158) + rxt(k,590)*y(k,236)
         mat(k,690) = -(rxt(k,568)*y(k,263))
         mat(k,2230) = -rxt(k,568)*y(k,217)
         mat(k,2720) = rxt(k,566)*y(k,270)
         mat(k,830) = rxt(k,566)*y(k,108)
         mat(k,224) = -(rxt(k,569)*y(k,263))
         mat(k,2166) = -rxt(k,569)*y(k,218)
         mat(k,240) = -(rxt(k,535)*y(k,263))
         mat(k,2169) = -rxt(k,535)*y(k,219)
         mat(k,2693) = rxt(k,532)*y(k,272)
         mat(k,1365) = rxt(k,532)*y(k,108)
         mat(k,245) = -(rxt(k,571)*y(k,263))
         mat(k,2170) = -rxt(k,571)*y(k,220)
         mat(k,811) = -(rxt(k,574)*y(k,263))
         mat(k,2243) = -rxt(k,574)*y(k,221)
         mat(k,2729) = rxt(k,572)*y(k,273)
         mat(k,855) = rxt(k,572)*y(k,108)
         mat(k,253) = -(rxt(k,577)*y(k,263))
         mat(k,2171) = -rxt(k,577)*y(k,222)
         mat(k,246) = .150_r8*rxt(k,571)*y(k,263)
         mat(k,2171) = mat(k,2171) + .150_r8*rxt(k,571)*y(k,220)
         mat(k,507) = -(rxt(k,578)*y(k,263))
         mat(k,2208) = -rxt(k,578)*y(k,223)
         mat(k,2706) = rxt(k,575)*y(k,275)
         mat(k,601) = rxt(k,575)*y(k,108)
         mat(k,618) = -(rxt(k,536)*y(k,108) + rxt(k,537)*y(k,158) + rxt(k,565) &
                      *y(k,159))
         mat(k,2718) = -rxt(k,536)*y(k,226)
         mat(k,2061) = -rxt(k,537)*y(k,226)
         mat(k,1987) = -rxt(k,565)*y(k,226)
         mat(k,289) = rxt(k,542)*y(k,263)
         mat(k,2221) = rxt(k,542)*y(k,24)
         mat(k,1133) = -(rxt(k,497)*y(k,108) + (rxt(k,498) + rxt(k,499)) * y(k,158))
         mat(k,2742) = -rxt(k,497)*y(k,227)
         mat(k,2087) = -(rxt(k,498) + rxt(k,499)) * y(k,227)
         mat(k,759) = rxt(k,500)*y(k,263)
         mat(k,286) = rxt(k,501)*y(k,263)
         mat(k,2264) = rxt(k,500)*y(k,2) + rxt(k,501)*y(k,15)
         mat(k,68) = -(rxt(k,673)*y(k,108) + rxt(k,674)*y(k,158))
         mat(k,2685) = -rxt(k,673)*y(k,228)
         mat(k,2039) = -rxt(k,674)*y(k,228)
         mat(k,1105) = rxt(k,676)*y(k,263)
         mat(k,2146) = rxt(k,676)*y(k,6)
         mat(k,576) = -(rxt(k,539)*y(k,108) + rxt(k,540)*y(k,158))
         mat(k,2715) = -rxt(k,539)*y(k,229)
         mat(k,2058) = -rxt(k,540)*y(k,229)
         mat(k,192) = .350_r8*rxt(k,538)*y(k,263)
         mat(k,529) = rxt(k,541)*y(k,263)
         mat(k,2216) = .350_r8*rxt(k,538)*y(k,7) + rxt(k,541)*y(k,8)
         mat(k,74) = -(rxt(k,678)*y(k,108) + rxt(k,679)*y(k,158))
         mat(k,2686) = -rxt(k,678)*y(k,230)
         mat(k,2040) = -rxt(k,679)*y(k,230)
         mat(k,188) = rxt(k,677)*y(k,263)
         mat(k,2147) = rxt(k,677)*y(k,7)
         mat(k,515) = -(rxt(k,543)*y(k,108) + rxt(k,545)*y(k,158))
         mat(k,2707) = -rxt(k,543)*y(k,231)
         mat(k,2052) = -rxt(k,545)*y(k,231)
         mat(k,388) = rxt(k,544)*y(k,263)
         mat(k,227) = .070_r8*rxt(k,569)*y(k,263)
         mat(k,247) = .060_r8*rxt(k,571)*y(k,263)
         mat(k,2209) = rxt(k,544)*y(k,25) + .070_r8*rxt(k,569)*y(k,218) &
                      + .060_r8*rxt(k,571)*y(k,220)
         mat(k,1000) = -(4._r8*rxt(k,419)*y(k,232) + rxt(k,420)*y(k,236) + rxt(k,421) &
                      *y(k,108) + rxt(k,422)*y(k,158))
         mat(k,1709) = -rxt(k,420)*y(k,232)
         mat(k,2738) = -rxt(k,421)*y(k,232)
         mat(k,2082) = -rxt(k,422)*y(k,232)
         mat(k,398) = .500_r8*rxt(k,424)*y(k,263)
         mat(k,337) = rxt(k,425)*y(k,70) + rxt(k,426)*y(k,263)
         mat(k,2544) = rxt(k,425)*y(k,32)
         mat(k,2255) = .500_r8*rxt(k,424)*y(k,31) + rxt(k,426)*y(k,32)
         mat(k,1034) = -(rxt(k,448)*y(k,236) + rxt(k,449)*y(k,108) + rxt(k,450) &
                      *y(k,158))
         mat(k,1710) = -rxt(k,448)*y(k,233)
         mat(k,2741) = -rxt(k,449)*y(k,233)
         mat(k,2085) = -rxt(k,450)*y(k,233)
         mat(k,496) = rxt(k,451)*y(k,263)
         mat(k,343) = rxt(k,455)*y(k,70) + rxt(k,452)*y(k,263)
         mat(k,2546) = rxt(k,455)*y(k,35)
         mat(k,2259) = rxt(k,451)*y(k,34) + rxt(k,452)*y(k,35)
         mat(k,743) = -(rxt(k,546)*y(k,108) + rxt(k,547)*y(k,158))
         mat(k,2724) = -rxt(k,546)*y(k,234)
         mat(k,2067) = -rxt(k,547)*y(k,234)
         mat(k,319) = rxt(k,548)*y(k,263)
         mat(k,2724) = mat(k,2724) + .400_r8*rxt(k,536)*y(k,226)
         mat(k,2067) = mat(k,2067) + rxt(k,537)*y(k,226)
         mat(k,2368) = rxt(k,563)*y(k,178)
         mat(k,557) = rxt(k,563)*y(k,170)
         mat(k,619) = .400_r8*rxt(k,536)*y(k,108) + rxt(k,537)*y(k,158)
         mat(k,2236) = rxt(k,548)*y(k,36)
         mat(k,1566) = -(4._r8*rxt(k,430)*y(k,235) + rxt(k,431)*y(k,236) + rxt(k,432) &
                      *y(k,108) + rxt(k,433)*y(k,158) + rxt(k,444)*y(k,159) + rxt(k,472) &
                      *y(k,248) + rxt(k,505)*y(k,243) + rxt(k,510)*y(k,244) + rxt(k,519) &
                      *y(k,245) + rxt(k,530)*y(k,272))
         mat(k,1734) = -rxt(k,431)*y(k,235)
         mat(k,2766) = -rxt(k,432)*y(k,235)
         mat(k,2112) = -rxt(k,433)*y(k,235)
         mat(k,2005) = -rxt(k,444)*y(k,235)
         mat(k,1496) = -rxt(k,472)*y(k,235)
         mat(k,1441) = -rxt(k,505)*y(k,235)
         mat(k,1474) = -rxt(k,510)*y(k,235)
         mat(k,1395) = -rxt(k,519)*y(k,235)
         mat(k,1373) = -rxt(k,530)*y(k,235)
         mat(k,1120) = .060_r8*rxt(k,580)*y(k,170)
         mat(k,1276) = rxt(k,427)*y(k,160) + rxt(k,428)*y(k,263)
         mat(k,1420) = rxt(k,453)*y(k,160) + rxt(k,454)*y(k,263)
         mat(k,729) = .500_r8*rxt(k,435)*y(k,263)
         mat(k,2766) = mat(k,2766) + .450_r8*rxt(k,483)*y(k,250) + .200_r8*rxt(k,487) &
                      *y(k,252) + .150_r8*rxt(k,462)*y(k,267)
         mat(k,984) = .080_r8*rxt(k,525)*y(k,170)
         mat(k,1411) = .100_r8*rxt(k,478)*y(k,170)
         mat(k,1065) = .060_r8*rxt(k,583)*y(k,170)
         mat(k,1516) = .280_r8*rxt(k,492)*y(k,170)
         mat(k,2112) = mat(k,2112) + .530_r8*rxt(k,476)*y(k,248) + rxt(k,485)*y(k,250) &
                      + rxt(k,488)*y(k,252) + rxt(k,463)*y(k,267)
         mat(k,1947) = rxt(k,427)*y(k,54) + rxt(k,453)*y(k,58) + .530_r8*rxt(k,475) &
                      *y(k,248) + rxt(k,486)*y(k,250)
         mat(k,2400) = .060_r8*rxt(k,580)*y(k,6) + .080_r8*rxt(k,525)*y(k,130) &
                      + .100_r8*rxt(k,478)*y(k,139) + .060_r8*rxt(k,583)*y(k,144) &
                      + .280_r8*rxt(k,492)*y(k,145)
         mat(k,1232) = .650_r8*rxt(k,601)*y(k,263)
         mat(k,1566) = mat(k,1566) + .530_r8*rxt(k,472)*y(k,248)
         mat(k,1734) = mat(k,1734) + .260_r8*rxt(k,473)*y(k,248) + rxt(k,482)*y(k,250) &
                      + .300_r8*rxt(k,461)*y(k,267)
         mat(k,1496) = mat(k,1496) + .530_r8*rxt(k,476)*y(k,158) + .530_r8*rxt(k,475) &
                      *y(k,160) + .530_r8*rxt(k,472)*y(k,235) + .260_r8*rxt(k,473) &
                      *y(k,236)
         mat(k,1536) = .450_r8*rxt(k,483)*y(k,108) + rxt(k,485)*y(k,158) + rxt(k,486) &
                      *y(k,160) + rxt(k,482)*y(k,236) + 4.000_r8*rxt(k,484)*y(k,250)
         mat(k,780) = .200_r8*rxt(k,487)*y(k,108) + rxt(k,488)*y(k,158)
         mat(k,2293) = rxt(k,428)*y(k,54) + rxt(k,454)*y(k,58) + .500_r8*rxt(k,435) &
                      *y(k,60) + .650_r8*rxt(k,601)*y(k,216)
         mat(k,1357) = .150_r8*rxt(k,462)*y(k,108) + rxt(k,463)*y(k,158) &
                      + .300_r8*rxt(k,461)*y(k,236)
         mat(k,1737) = -(rxt(k,256)*y(k,74) + (rxt(k,378) + rxt(k,379)) * y(k,68) &
                      + (4._r8*rxt(k,398) + 4._r8*rxt(k,399)) * y(k,236) + rxt(k,400) &
                      *y(k,108) + rxt(k,401)*y(k,158) + rxt(k,420)*y(k,232) + rxt(k,431) &
                      *y(k,235) + rxt(k,448)*y(k,233) + rxt(k,461)*y(k,267) + rxt(k,473) &
                      *y(k,248) + rxt(k,482)*y(k,250) + rxt(k,506)*y(k,243) + rxt(k,511) &
                      *y(k,244) + rxt(k,520)*y(k,245) + rxt(k,531)*y(k,272) + rxt(k,585) &
                      *y(k,258) + rxt(k,590)*y(k,268) + rxt(k,595)*y(k,269))
         mat(k,2834) = -rxt(k,256)*y(k,236)
         mat(k,1242) = -(rxt(k,378) + rxt(k,379)) * y(k,236)
         mat(k,2773) = -rxt(k,400)*y(k,236)
         mat(k,2116) = -rxt(k,401)*y(k,236)
         mat(k,1002) = -rxt(k,420)*y(k,236)
         mat(k,1569) = -rxt(k,431)*y(k,236)
         mat(k,1037) = -rxt(k,448)*y(k,236)
         mat(k,1358) = -rxt(k,461)*y(k,236)
         mat(k,1498) = -rxt(k,473)*y(k,236)
         mat(k,1538) = -rxt(k,482)*y(k,236)
         mat(k,1443) = -rxt(k,506)*y(k,236)
         mat(k,1476) = -rxt(k,511)*y(k,236)
         mat(k,1397) = -rxt(k,520)*y(k,236)
         mat(k,1375) = -rxt(k,531)*y(k,236)
         mat(k,1220) = -rxt(k,585)*y(k,236)
         mat(k,1305) = -rxt(k,590)*y(k,236)
         mat(k,1156) = -rxt(k,595)*y(k,236)
         mat(k,1264) = .280_r8*rxt(k,447)*y(k,170)
         mat(k,797) = rxt(k,434)*y(k,263)
         mat(k,472) = .700_r8*rxt(k,403)*y(k,263)
         mat(k,1662) = rxt(k,248)*y(k,70) + rxt(k,352)*y(k,89) + rxt(k,410)*y(k,259) &
                      + rxt(k,404)*y(k,263)
         mat(k,2564) = rxt(k,248)*y(k,64)
         mat(k,1013) = rxt(k,352)*y(k,64)
         mat(k,2773) = mat(k,2773) + .490_r8*rxt(k,432)*y(k,235) + .330_r8*rxt(k,550) &
                      *y(k,237) + .070_r8*rxt(k,556)*y(k,251)
         mat(k,985) = .050_r8*rxt(k,525)*y(k,170)
         mat(k,2116) = mat(k,2116) + rxt(k,433)*y(k,235) + .830_r8*rxt(k,551)*y(k,237) &
                      + .170_r8*rxt(k,557)*y(k,251)
         mat(k,2404) = .280_r8*rxt(k,447)*y(k,33) + .050_r8*rxt(k,525)*y(k,130)
         mat(k,1569) = mat(k,1569) + .490_r8*rxt(k,432)*y(k,108) + rxt(k,433)*y(k,158) &
                      + 4.000_r8*rxt(k,430)*y(k,235) + .900_r8*rxt(k,431)*y(k,236) &
                      + rxt(k,505)*y(k,243) + rxt(k,510)*y(k,244) + rxt(k,519) &
                      *y(k,245) + rxt(k,472)*y(k,248) + rxt(k,481)*y(k,250) &
                      + rxt(k,530)*y(k,272)
         mat(k,1737) = mat(k,1737) + .900_r8*rxt(k,431)*y(k,235)
         mat(k,871) = .330_r8*rxt(k,550)*y(k,108) + .830_r8*rxt(k,551)*y(k,158)
         mat(k,1443) = mat(k,1443) + rxt(k,505)*y(k,235)
         mat(k,1476) = mat(k,1476) + rxt(k,510)*y(k,235)
         mat(k,1397) = mat(k,1397) + rxt(k,519)*y(k,235)
         mat(k,1498) = mat(k,1498) + rxt(k,472)*y(k,235)
         mat(k,1538) = mat(k,1538) + rxt(k,481)*y(k,235)
         mat(k,1023) = .070_r8*rxt(k,556)*y(k,108) + .170_r8*rxt(k,557)*y(k,158)
         mat(k,1881) = rxt(k,410)*y(k,64)
         mat(k,2301) = rxt(k,434)*y(k,59) + .700_r8*rxt(k,403)*y(k,63) + rxt(k,404) &
                      *y(k,64)
         mat(k,1375) = mat(k,1375) + rxt(k,530)*y(k,235)
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
         mat(k,868) = -(rxt(k,550)*y(k,108) + rxt(k,551)*y(k,158) + rxt(k,552) &
                      *y(k,159))
         mat(k,2734) = -rxt(k,550)*y(k,237)
         mat(k,2074) = -rxt(k,551)*y(k,237)
         mat(k,1993) = -rxt(k,552)*y(k,237)
         mat(k,940) = -(rxt(k,743)*y(k,256) + rxt(k,744)*y(k,262) + rxt(k,745) &
                      *y(k,255))
         mat(k,921) = -rxt(k,743)*y(k,238)
         mat(k,929) = -rxt(k,744)*y(k,238)
         mat(k,724) = -rxt(k,745)*y(k,238)
         mat(k,654) = -((rxt(k,469) + rxt(k,470)) * y(k,158))
         mat(k,2062) = -(rxt(k,469) + rxt(k,470)) * y(k,239)
         mat(k,425) = rxt(k,468)*y(k,263)
         mat(k,2226) = rxt(k,468)*y(k,16)
         mat(k,545) = -(rxt(k,439)*y(k,169))
         mat(k,1757) = -rxt(k,439)*y(k,240)
         mat(k,2055) = .750_r8*rxt(k,437)*y(k,241)
         mat(k,877) = .750_r8*rxt(k,437)*y(k,158)
         mat(k,878) = -(rxt(k,436)*y(k,108) + rxt(k,437)*y(k,158))
         mat(k,2735) = -rxt(k,436)*y(k,241)
         mat(k,2075) = -rxt(k,437)*y(k,241)
         mat(k,631) = rxt(k,443)*y(k,263)
         mat(k,2249) = rxt(k,443)*y(k,28)
         mat(k,521) = -(rxt(k,407)*y(k,108) + rxt(k,409)*y(k,158))
         mat(k,2708) = -rxt(k,407)*y(k,242)
         mat(k,2053) = -rxt(k,409)*y(k,242)
         mat(k,2324) = rxt(k,394)*y(k,108)
         mat(k,2708) = mat(k,2708) + rxt(k,394)*y(k,51)
         mat(k,1437) = -(rxt(k,505)*y(k,235) + rxt(k,506)*y(k,236) + rxt(k,507) &
                      *y(k,108) + rxt(k,508)*y(k,158) + rxt(k,509)*y(k,160))
         mat(k,1561) = -rxt(k,505)*y(k,243)
         mat(k,1729) = -rxt(k,506)*y(k,243)
         mat(k,2761) = -rxt(k,507)*y(k,243)
         mat(k,2107) = -rxt(k,508)*y(k,243)
         mat(k,1942) = -rxt(k,509)*y(k,243)
         mat(k,981) = .600_r8*rxt(k,526)*y(k,263)
         mat(k,2288) = .600_r8*rxt(k,526)*y(k,130)
         mat(k,1470) = -(rxt(k,510)*y(k,235) + rxt(k,511)*y(k,236) + rxt(k,512) &
                      *y(k,108) + rxt(k,514)*y(k,158) + rxt(k,515)*y(k,160))
         mat(k,1562) = -rxt(k,510)*y(k,244)
         mat(k,1730) = -rxt(k,511)*y(k,244)
         mat(k,2762) = -rxt(k,512)*y(k,244)
         mat(k,2108) = -rxt(k,514)*y(k,244)
         mat(k,1943) = -rxt(k,515)*y(k,244)
         mat(k,982) = .400_r8*rxt(k,526)*y(k,263)
         mat(k,2289) = .400_r8*rxt(k,526)*y(k,130)
         mat(k,1391) = -(rxt(k,519)*y(k,235) + rxt(k,520)*y(k,236) + rxt(k,521) &
                      *y(k,108) + rxt(k,522)*y(k,158) + rxt(k,523)*y(k,160))
         mat(k,1558) = -rxt(k,519)*y(k,245)
         mat(k,1726) = -rxt(k,520)*y(k,245)
         mat(k,2758) = -rxt(k,521)*y(k,245)
         mat(k,2104) = -rxt(k,522)*y(k,245)
         mat(k,1939) = -rxt(k,523)*y(k,245)
         mat(k,979) = rxt(k,518)*y(k,160)
         mat(k,1939) = mat(k,1939) + rxt(k,518)*y(k,130)
         mat(k,80) = -(rxt(k,681)*y(k,108) + rxt(k,682)*y(k,158))
         mat(k,2687) = -rxt(k,681)*y(k,246)
         mat(k,2041) = -rxt(k,682)*y(k,246)
         mat(k,974) = rxt(k,684)*y(k,263)
         mat(k,2148) = rxt(k,684)*y(k,130)
         mat(k,86) = -(rxt(k,685)*y(k,108) + rxt(k,686)*y(k,158))
         mat(k,2688) = -rxt(k,685)*y(k,247)
         mat(k,2042) = -rxt(k,686)*y(k,247)
         mat(k,87) = rxt(k,687)*y(k,263)
         mat(k,2149) = rxt(k,687)*y(k,135)
         mat(k,1494) = -(rxt(k,472)*y(k,235) + rxt(k,473)*y(k,236) + rxt(k,474) &
                      *y(k,108) + rxt(k,475)*y(k,160) + (rxt(k,476) + rxt(k,477) &
                      ) * y(k,158))
         mat(k,1563) = -rxt(k,472)*y(k,248)
         mat(k,1731) = -rxt(k,473)*y(k,248)
         mat(k,2763) = -rxt(k,474)*y(k,248)
         mat(k,1944) = -rxt(k,475)*y(k,248)
         mat(k,2109) = -(rxt(k,476) + rxt(k,477)) * y(k,248)
         mat(k,1409) = .500_r8*rxt(k,479)*y(k,263)
         mat(k,370) = .200_r8*rxt(k,480)*y(k,263)
         mat(k,1513) = rxt(k,493)*y(k,263)
         mat(k,2290) = .500_r8*rxt(k,479)*y(k,139) + .200_r8*rxt(k,480)*y(k,140) &
                      + rxt(k,493)*y(k,145)
         mat(k,822) = -(rxt(k,553)*y(k,108) + rxt(k,554)*y(k,158) + rxt(k,555) &
                      *y(k,159))
         mat(k,2730) = -rxt(k,553)*y(k,249)
         mat(k,2071) = -rxt(k,554)*y(k,249)
         mat(k,1992) = -rxt(k,555)*y(k,249)
         mat(k,1535) = -(rxt(k,481)*y(k,235) + rxt(k,482)*y(k,236) + rxt(k,483) &
                      *y(k,108) + 4._r8*rxt(k,484)*y(k,250) + rxt(k,485)*y(k,158) &
                      + rxt(k,486)*y(k,160) + rxt(k,494)*y(k,159))
         mat(k,1565) = -rxt(k,481)*y(k,250)
         mat(k,1733) = -rxt(k,482)*y(k,250)
         mat(k,2765) = -rxt(k,483)*y(k,250)
         mat(k,2111) = -rxt(k,485)*y(k,250)
         mat(k,1946) = -rxt(k,486)*y(k,250)
         mat(k,2004) = -rxt(k,494)*y(k,250)
         mat(k,1410) = .500_r8*rxt(k,479)*y(k,263)
         mat(k,371) = .500_r8*rxt(k,480)*y(k,263)
         mat(k,2292) = .500_r8*rxt(k,479)*y(k,139) + .500_r8*rxt(k,480)*y(k,140)
         mat(k,1019) = -(rxt(k,556)*y(k,108) + rxt(k,557)*y(k,158) + rxt(k,558) &
                      *y(k,159))
         mat(k,2739) = -rxt(k,556)*y(k,251)
         mat(k,2083) = -rxt(k,557)*y(k,251)
         mat(k,1996) = -rxt(k,558)*y(k,251)
         mat(k,778) = -(rxt(k,487)*y(k,108) + rxt(k,488)*y(k,158))
         mat(k,2726) = -rxt(k,487)*y(k,252)
         mat(k,2070) = -rxt(k,488)*y(k,252)
         mat(k,610) = rxt(k,489)*y(k,263)
         mat(k,375) = rxt(k,490)*y(k,263)
         mat(k,2239) = rxt(k,489)*y(k,141) + rxt(k,490)*y(k,142)
         mat(k,94) = -(rxt(k,689)*y(k,108) + rxt(k,690)*y(k,158))
         mat(k,2689) = -rxt(k,689)*y(k,253)
         mat(k,2043) = -rxt(k,690)*y(k,253)
         mat(k,1050) = rxt(k,692)*y(k,263)
         mat(k,2151) = rxt(k,692)*y(k,144)
         mat(k,749) = -(rxt(k,219)*y(k,158) + rxt(k,220)*y(k,168) + rxt(k,243) &
                      *y(k,238) + rxt(k,244)*y(k,169))
         mat(k,2068) = -rxt(k,219)*y(k,254)
         mat(k,2483) = -rxt(k,220)*y(k,254)
         mat(k,936) = -rxt(k,243)*y(k,254)
         mat(k,1760) = -rxt(k,244)*y(k,254)
         mat(k,2483) = mat(k,2483) + rxt(k,747)*y(k,255)
         mat(k,936) = mat(k,936) + .900_r8*rxt(k,745)*y(k,255) + .800_r8*rxt(k,743) &
                      *y(k,256)
         mat(k,720) = rxt(k,747)*y(k,168) + .900_r8*rxt(k,745)*y(k,238)
         mat(k,919) = .800_r8*rxt(k,743)*y(k,238)
         mat(k,719) = -(rxt(k,745)*y(k,238) + rxt(k,746)*y(k,169) + (rxt(k,747) &
                      + rxt(k,748)) * y(k,168))
         mat(k,935) = -rxt(k,745)*y(k,255)
         mat(k,1759) = -rxt(k,746)*y(k,255)
         mat(k,2482) = -(rxt(k,747) + rxt(k,748)) * y(k,255)
         mat(k,920) = -(rxt(k,743)*y(k,238))
         mat(k,938) = -rxt(k,743)*y(k,256)
         mat(k,1087) = rxt(k,752)*y(k,262)
         mat(k,2078) = rxt(k,754)*y(k,262)
         mat(k,2486) = rxt(k,747)*y(k,255)
         mat(k,1763) = rxt(k,751)*y(k,257)
         mat(k,722) = rxt(k,747)*y(k,168)
         mat(k,592) = rxt(k,751)*y(k,169)
         mat(k,927) = rxt(k,752)*y(k,146) + rxt(k,754)*y(k,158)
         mat(k,590) = -(rxt(k,749)*y(k,168) + (rxt(k,750) + rxt(k,751)) * y(k,169))
         mat(k,2480) = -rxt(k,749)*y(k,257)
         mat(k,1758) = -(rxt(k,750) + rxt(k,751)) * y(k,257)
         mat(k,1216) = -(rxt(k,585)*y(k,236) + rxt(k,586)*y(k,108) + rxt(k,587) &
                      *y(k,158) + rxt(k,588)*y(k,160))
         mat(k,1716) = -rxt(k,585)*y(k,258)
         mat(k,2747) = -rxt(k,586)*y(k,258)
         mat(k,2093) = -rxt(k,587)*y(k,258)
         mat(k,1926) = -rxt(k,588)*y(k,258)
         mat(k,1115) = rxt(k,579)*y(k,160)
         mat(k,1060) = rxt(k,582)*y(k,160)
         mat(k,1926) = mat(k,1926) + rxt(k,579)*y(k,6) + rxt(k,582)*y(k,144) &
                      + .500_r8*rxt(k,599)*y(k,215)
         mat(k,479) = rxt(k,589)*y(k,263)
         mat(k,1164) = .500_r8*rxt(k,599)*y(k,160)
         mat(k,2272) = rxt(k,589)*y(k,162)
         mat(k,1886) = -(rxt(k,182)*y(k,93) + rxt(k,183)*y(k,276) + (rxt(k,185) &
                      + rxt(k,186)) * y(k,169) + (rxt(k,187) + rxt(k,188)) * y(k,170) &
                      + (rxt(k,236) + rxt(k,237)) * y(k,147) + rxt(k,274)*y(k,37) &
                      + rxt(k,275)*y(k,38) + rxt(k,276)*y(k,40) + rxt(k,277)*y(k,41) &
                      + rxt(k,278)*y(k,42) + rxt(k,279)*y(k,43) + rxt(k,280)*y(k,44) &
                      + (rxt(k,281) + rxt(k,282)) * y(k,102) + rxt(k,305)*y(k,39) &
                      + rxt(k,306)*y(k,66) + rxt(k,307)*y(k,94) + (rxt(k,308) &
                      + rxt(k,309)) * y(k,98) + rxt(k,356)*y(k,80) + rxt(k,357) &
                      *y(k,81) + rxt(k,389)*y(k,45) + rxt(k,390)*y(k,52) + rxt(k,391) &
                      *y(k,99) + rxt(k,392)*y(k,100) + rxt(k,393)*y(k,101) + (rxt(k,410) &
                      + rxt(k,411) + rxt(k,412)) * y(k,64) + rxt(k,413)*y(k,103))
         mat(k,1619) = -rxt(k,182)*y(k,259)
         mat(k,2871) = -rxt(k,183)*y(k,259)
         mat(k,1773) = -(rxt(k,185) + rxt(k,186)) * y(k,259)
         mat(k,2409) = -(rxt(k,187) + rxt(k,188)) * y(k,259)
         mat(k,306) = -(rxt(k,236) + rxt(k,237)) * y(k,259)
         mat(k,116) = -rxt(k,274)*y(k,259)
         mat(k,170) = -rxt(k,275)*y(k,259)
         mat(k,131) = -rxt(k,276)*y(k,259)
         mat(k,181) = -rxt(k,277)*y(k,259)
         mat(k,135) = -rxt(k,278)*y(k,259)
         mat(k,186) = -rxt(k,279)*y(k,259)
         mat(k,139) = -rxt(k,280)*y(k,259)
         mat(k,1842) = -(rxt(k,281) + rxt(k,282)) * y(k,259)
         mat(k,176) = -rxt(k,305)*y(k,259)
         mat(k,503) = -rxt(k,306)*y(k,259)
         mat(k,127) = -rxt(k,307)*y(k,259)
         mat(k,1604) = -(rxt(k,308) + rxt(k,309)) * y(k,259)
         mat(k,274) = -rxt(k,356)*y(k,259)
         mat(k,265) = -rxt(k,357)*y(k,259)
         mat(k,562) = -rxt(k,389)*y(k,259)
         mat(k,682) = -rxt(k,390)*y(k,259)
         mat(k,260) = -rxt(k,391)*y(k,259)
         mat(k,269) = -rxt(k,392)*y(k,259)
         mat(k,351) = -rxt(k,393)*y(k,259)
         mat(k,1665) = -(rxt(k,410) + rxt(k,411) + rxt(k,412)) * y(k,259)
         mat(k,210) = -rxt(k,413)*y(k,259)
         mat(k,1773) = mat(k,1773) + rxt(k,244)*y(k,254)
         mat(k,945) = .850_r8*rxt(k,744)*y(k,262)
         mat(k,752) = rxt(k,244)*y(k,169)
         mat(k,932) = .850_r8*rxt(k,744)*y(k,238)
         mat(k,204) = -(rxt(k,190)*y(k,168) + rxt(k,191)*y(k,169))
         mat(k,2477) = -rxt(k,190)*y(k,260)
         mat(k,1755) = -rxt(k,191)*y(k,260)
         mat(k,1580) = rxt(k,192)*y(k,261)
         mat(k,2477) = mat(k,2477) + rxt(k,194)*y(k,261)
         mat(k,1755) = mat(k,1755) + rxt(k,195)*y(k,261)
         mat(k,2360) = rxt(k,196)*y(k,261)
         mat(k,206) = rxt(k,192)*y(k,78) + rxt(k,194)*y(k,168) + rxt(k,195)*y(k,169) &
                      + rxt(k,196)*y(k,170)
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
         mat(k,207) = -(rxt(k,192)*y(k,78) + rxt(k,194)*y(k,168) + rxt(k,195)*y(k,169) &
                      + rxt(k,196)*y(k,170))
         mat(k,1581) = -rxt(k,192)*y(k,261)
         mat(k,2478) = -rxt(k,194)*y(k,261)
         mat(k,1756) = -rxt(k,195)*y(k,261)
         mat(k,2361) = -rxt(k,196)*y(k,261)
         mat(k,1756) = mat(k,1756) + rxt(k,185)*y(k,259)
         mat(k,1865) = rxt(k,185)*y(k,169)
         mat(k,928) = -(rxt(k,744)*y(k,238) + rxt(k,752)*y(k,146) + rxt(k,754) &
                      *y(k,158))
         mat(k,939) = -rxt(k,744)*y(k,262)
         mat(k,1088) = -rxt(k,752)*y(k,262)
         mat(k,2079) = -rxt(k,754)*y(k,262)
         mat(k,1584) = rxt(k,755)*y(k,264)
         mat(k,1764) = rxt(k,746)*y(k,255) + rxt(k,750)*y(k,257) + rxt(k,757)*y(k,264)
         mat(k,723) = rxt(k,746)*y(k,169)
         mat(k,593) = rxt(k,750)*y(k,169)
         mat(k,888) = rxt(k,755)*y(k,78) + rxt(k,757)*y(k,169)
         mat(k,2310) = -(rxt(k,209)*y(k,93) + rxt(k,210)*y(k,95) + rxt(k,211)*y(k,108) &
                      + rxt(k,212)*y(k,168) + rxt(k,213)*y(k,170) + (4._r8*rxt(k,214) &
                      + 4._r8*rxt(k,215)) * y(k,263) + rxt(k,218)*y(k,109) + rxt(k,231) &
                      *y(k,160) + rxt(k,232)*y(k,146) + rxt(k,240)*y(k,159) + rxt(k,241) &
                      *y(k,107) + rxt(k,254)*y(k,73) + rxt(k,265)*y(k,75) + (rxt(k,267) &
                      + rxt(k,268)) * y(k,74) + rxt(k,270)*y(k,102) + rxt(k,273) &
                      *y(k,111) + rxt(k,285)*y(k,18) + rxt(k,301)*y(k,21) + rxt(k,303) &
                      *y(k,98) + rxt(k,311)*y(k,112) + rxt(k,314)*y(k,118) + rxt(k,337) &
                      *y(k,128) + rxt(k,338)*y(k,106) + rxt(k,342)*y(k,174) + rxt(k,359) &
                      *y(k,26) + rxt(k,361)*y(k,29) + rxt(k,363)*y(k,45) + rxt(k,364) &
                      *y(k,46) + rxt(k,366)*y(k,47) + rxt(k,368)*y(k,52) + rxt(k,369) &
                      *y(k,53) + rxt(k,371)*y(k,55) + rxt(k,373)*y(k,61) + rxt(k,374) &
                      *y(k,65) + rxt(k,376)*y(k,66) + rxt(k,377)*y(k,67) + rxt(k,385) &
                      *y(k,69) + rxt(k,386)*y(k,99) + rxt(k,387)*y(k,100) + rxt(k,388) &
                      *y(k,101) + rxt(k,397)*y(k,51) + rxt(k,402)*y(k,62) + rxt(k,403) &
                      *y(k,63) + rxt(k,404)*y(k,64) + rxt(k,405)*y(k,103) + rxt(k,406) &
                      *y(k,104) + rxt(k,414)*y(k,77) + rxt(k,416)*y(k,27) + rxt(k,423) &
                      *y(k,30) + rxt(k,424)*y(k,31) + rxt(k,426)*y(k,32) + rxt(k,428) &
                      *y(k,54) + rxt(k,429)*y(k,56) + rxt(k,434)*y(k,59) + rxt(k,435) &
                      *y(k,60) + rxt(k,440)*y(k,90) + rxt(k,441)*y(k,91) + rxt(k,442) &
                      *y(k,176) + rxt(k,443)*y(k,28) + rxt(k,451)*y(k,34) + rxt(k,452) &
                      *y(k,35) + rxt(k,454)*y(k,58) + rxt(k,456)*y(k,115) + rxt(k,457) &
                      *y(k,161) + rxt(k,460)*y(k,183) + rxt(k,464)*y(k,184) + rxt(k,465) &
                      *y(k,33) + rxt(k,466)*y(k,57) + rxt(k,468)*y(k,16) + rxt(k,471) &
                      *y(k,113) + rxt(k,479)*y(k,139) + rxt(k,480)*y(k,140) + rxt(k,489) &
                      *y(k,141) + rxt(k,490)*y(k,142) + rxt(k,491)*y(k,143) + rxt(k,493) &
                      *y(k,145) + rxt(k,496)*y(k,1) + rxt(k,500)*y(k,2) + rxt(k,501) &
                      *y(k,15) + rxt(k,502)*y(k,114) + rxt(k,503)*y(k,116) + rxt(k,504) &
                      *y(k,125) + rxt(k,516)*y(k,131) + rxt(k,517)*y(k,132) + rxt(k,524) &
                      *y(k,133) + rxt(k,526)*y(k,130) + rxt(k,527)*y(k,134) + rxt(k,528) &
                      *y(k,149) + rxt(k,529)*y(k,150) + rxt(k,535)*y(k,219) + rxt(k,538) &
                      *y(k,7) + rxt(k,541)*y(k,8) + rxt(k,542)*y(k,24) + rxt(k,544) &
                      *y(k,25) + rxt(k,548)*y(k,36) + rxt(k,549)*y(k,82) + rxt(k,561) &
                      *y(k,179) + rxt(k,564)*y(k,180) + rxt(k,568)*y(k,217) + rxt(k,569) &
                      *y(k,218) + rxt(k,571)*y(k,220) + rxt(k,574)*y(k,221) + rxt(k,577) &
                      *y(k,222) + rxt(k,578)*y(k,223) + rxt(k,581)*y(k,6) + rxt(k,584) &
                      *y(k,144) + rxt(k,589)*y(k,162) + rxt(k,593)*y(k,212) + rxt(k,594) &
                      *y(k,213) + rxt(k,598)*y(k,214) + rxt(k,600)*y(k,215) + rxt(k,601) &
                      *y(k,216) + (rxt(k,607) + rxt(k,621)) * y(k,83) + rxt(k,609) &
                      *y(k,173) + rxt(k,611)*y(k,188) + rxt(k,615)*y(k,185) + rxt(k,620) &
                      *y(k,187) + rxt(k,640)*y(k,154))
         mat(k,1621) = -rxt(k,209)*y(k,263)
         mat(k,701) = -rxt(k,210)*y(k,263)
         mat(k,2782) = -rxt(k,211)*y(k,263)
         mat(k,2510) = -rxt(k,212)*y(k,263)
         mat(k,2413) = -rxt(k,213)*y(k,263)
         mat(k,587) = -rxt(k,218)*y(k,263)
         mat(k,1962) = -rxt(k,231)*y(k,263)
         mat(k,1097) = -rxt(k,232)*y(k,263)
         mat(k,2020) = -rxt(k,240)*y(k,263)
         mat(k,2463) = -rxt(k,241)*y(k,263)
         mat(k,628) = -rxt(k,254)*y(k,263)
         mat(k,1079) = -rxt(k,265)*y(k,263)
         mat(k,2842) = -(rxt(k,267) + rxt(k,268)) * y(k,263)
         mat(k,1845) = -rxt(k,270)*y(k,263)
         mat(k,1822) = -rxt(k,273)*y(k,263)
         mat(k,599) = -rxt(k,285)*y(k,263)
         mat(k,2636) = -rxt(k,301)*y(k,263)
         mat(k,1605) = -rxt(k,303)*y(k,263)
         mat(k,1799) = -rxt(k,311)*y(k,263)
         mat(k,1632) = -rxt(k,314)*y(k,263)
         mat(k,2606) = -rxt(k,337)*y(k,263)
         mat(k,1345) = -rxt(k,338)*y(k,263)
         mat(k,901) = -rxt(k,342)*y(k,263)
         mat(k,219) = -rxt(k,359)*y(k,263)
         mat(k,296) = -rxt(k,361)*y(k,263)
         mat(k,563) = -rxt(k,363)*y(k,263)
         mat(k,142) = -rxt(k,364)*y(k,263)
         mat(k,358) = -rxt(k,366)*y(k,263)
         mat(k,683) = -rxt(k,368)*y(k,263)
         mat(k,146) = -rxt(k,369)*y(k,263)
         mat(k,455) = -rxt(k,371)*y(k,263)
         mat(k,419) = -rxt(k,373)*y(k,263)
         mat(k,150) = -rxt(k,374)*y(k,263)
         mat(k,504) = -rxt(k,376)*y(k,263)
         mat(k,122) = -rxt(k,377)*y(k,263)
         mat(k,413) = -rxt(k,385)*y(k,263)
         mat(k,261) = -rxt(k,386)*y(k,263)
         mat(k,270) = -rxt(k,387)*y(k,263)
         mat(k,352) = -rxt(k,388)*y(k,263)
         mat(k,2341) = -rxt(k,397)*y(k,263)
         mat(k,905) = -rxt(k,402)*y(k,263)
         mat(k,473) = -rxt(k,403)*y(k,263)
         mat(k,1669) = -rxt(k,404)*y(k,263)
         mat(k,211) = -rxt(k,405)*y(k,263)
         mat(k,1030) = -rxt(k,406)*y(k,263)
         mat(k,1285) = -rxt(k,414)*y(k,263)
         mat(k,334) = -rxt(k,416)*y(k,263)
         mat(k,310) = -rxt(k,423)*y(k,263)
         mat(k,400) = -rxt(k,424)*y(k,263)
         mat(k,339) = -rxt(k,426)*y(k,263)
         mat(k,1279) = -rxt(k,428)*y(k,263)
         mat(k,119) = -rxt(k,429)*y(k,263)
         mat(k,798) = -rxt(k,434)*y(k,263)
         mat(k,732) = -rxt(k,435)*y(k,263)
         mat(k,1291) = -rxt(k,440)*y(k,263)
         mat(k,1148) = -rxt(k,441)*y(k,263)
         mat(k,644) = -rxt(k,442)*y(k,263)
         mat(k,634) = -rxt(k,443)*y(k,263)
         mat(k,498) = -rxt(k,451)*y(k,263)
         mat(k,345) = -rxt(k,452)*y(k,263)
         mat(k,1423) = -rxt(k,454)*y(k,263)
         mat(k,1334) = -rxt(k,456)*y(k,263)
         mat(k,995) = -rxt(k,457)*y(k,263)
         mat(k,650) = -rxt(k,460)*y(k,263)
         mat(k,492) = -rxt(k,464)*y(k,263)
         mat(k,1269) = -rxt(k,465)*y(k,263)
         mat(k,1175) = -rxt(k,466)*y(k,263)
         mat(k,430) = -rxt(k,468)*y(k,263)
         mat(k,1326) = -rxt(k,471)*y(k,263)
         mat(k,1413) = -rxt(k,479)*y(k,263)
         mat(k,372) = -rxt(k,480)*y(k,263)
         mat(k,613) = -rxt(k,489)*y(k,263)
         mat(k,378) = -rxt(k,490)*y(k,263)
         mat(k,676) = -rxt(k,491)*y(k,263)
         mat(k,1524) = -rxt(k,493)*y(k,263)
         mat(k,774) = -rxt(k,496)*y(k,263)
         mat(k,763) = -rxt(k,500)*y(k,263)
         mat(k,287) = -rxt(k,501)*y(k,263)
         mat(k,278) = -rxt(k,502)*y(k,263)
         mat(k,404) = -rxt(k,503)*y(k,263)
         mat(k,164) = -rxt(k,504)*y(k,263)
         mat(k,715) = -rxt(k,516)*y(k,263)
         mat(k,667) = -rxt(k,517)*y(k,263)
         mat(k,463) = -rxt(k,524)*y(k,263)
         mat(k,987) = -rxt(k,526)*y(k,263)
         mat(k,848) = -rxt(k,527)*y(k,263)
         mat(k,468) = -rxt(k,528)*y(k,263)
         mat(k,1208) = -rxt(k,529)*y(k,263)
         mat(k,242) = -rxt(k,535)*y(k,263)
         mat(k,193) = -rxt(k,538)*y(k,263)
         mat(k,531) = -rxt(k,541)*y(k,263)
         mat(k,290) = -rxt(k,542)*y(k,263)
         mat(k,390) = -rxt(k,544)*y(k,263)
         mat(k,320) = -rxt(k,548)*y(k,263)
         mat(k,234) = -rxt(k,549)*y(k,263)
         mat(k,202) = -rxt(k,561)*y(k,263)
         mat(k,395) = -rxt(k,564)*y(k,263)
         mat(k,697) = -rxt(k,568)*y(k,263)
         mat(k,229) = -rxt(k,569)*y(k,263)
         mat(k,251) = -rxt(k,571)*y(k,263)
         mat(k,820) = -rxt(k,574)*y(k,263)
         mat(k,256) = -rxt(k,577)*y(k,263)
         mat(k,511) = -rxt(k,578)*y(k,263)
         mat(k,1123) = -rxt(k,581)*y(k,263)
         mat(k,1068) = -rxt(k,584)*y(k,263)
         mat(k,481) = -rxt(k,589)*y(k,263)
         mat(k,792) = -rxt(k,593)*y(k,263)
         mat(k,738) = -rxt(k,594)*y(k,263)
         mat(k,571) = -rxt(k,598)*y(k,263)
         mat(k,1169) = -rxt(k,600)*y(k,263)
         mat(k,1235) = -rxt(k,601)*y(k,263)
         mat(k,1181) = -(rxt(k,607) + rxt(k,621)) * y(k,263)
         mat(k,448) = -rxt(k,609)*y(k,263)
         mat(k,1192) = -rxt(k,611)*y(k,263)
         mat(k,803) = -rxt(k,615)*y(k,263)
         mat(k,1646) = -rxt(k,620)*y(k,263)
         mat(k,113) = -rxt(k,640)*y(k,263)
         mat(k,1123) = mat(k,1123) + .630_r8*rxt(k,580)*y(k,170)
         mat(k,334) = mat(k,334) + .650_r8*rxt(k,416)*y(k,263)
         mat(k,634) = mat(k,634) + .130_r8*rxt(k,418)*y(k,170)
         mat(k,400) = mat(k,400) + .500_r8*rxt(k,424)*y(k,263)
         mat(k,1269) = mat(k,1269) + .360_r8*rxt(k,447)*y(k,170)
         mat(k,2341) = mat(k,2341) + rxt(k,396)*y(k,168)
         mat(k,473) = mat(k,473) + .300_r8*rxt(k,403)*y(k,263)
         mat(k,1669) = mat(k,1669) + rxt(k,410)*y(k,259)
         mat(k,2573) = rxt(k,252)*y(k,108)
         mat(k,1015) = rxt(k,354)*y(k,276)
         mat(k,2437) = 2.000_r8*rxt(k,203)*y(k,108) + rxt(k,208)*y(k,170)
         mat(k,1621) = mat(k,1621) + rxt(k,200)*y(k,168) + rxt(k,182)*y(k,259)
         mat(k,701) = mat(k,701) + rxt(k,201)*y(k,168)
         mat(k,1605) = mat(k,1605) + rxt(k,302)*y(k,168) + rxt(k,308)*y(k,259)
         mat(k,1845) = mat(k,1845) + rxt(k,269)*y(k,168) + rxt(k,281)*y(k,259)
         mat(k,211) = mat(k,211) + rxt(k,413)*y(k,259)
         mat(k,2782) = mat(k,2782) + rxt(k,252)*y(k,70) + 2.000_r8*rxt(k,203)*y(k,92) &
                      + rxt(k,233)*y(k,158) + rxt(k,228)*y(k,160) + rxt(k,206) &
                      *y(k,168) + rxt(k,207)*y(k,170) + .400_r8*rxt(k,536)*y(k,226) &
                      + .490_r8*rxt(k,432)*y(k,235) + .400_r8*rxt(k,550)*y(k,237) &
                      + .450_r8*rxt(k,483)*y(k,250) + .400_r8*rxt(k,556)*y(k,251) &
                      + .200_r8*rxt(k,487)*y(k,252) + .150_r8*rxt(k,462)*y(k,267)
         mat(k,1690) = rxt(k,304)*y(k,168)
         mat(k,1822) = mat(k,1822) + rxt(k,272)*y(k,168)
         mat(k,987) = mat(k,987) + .320_r8*rxt(k,525)*y(k,170)
         mat(k,848) = mat(k,848) + .600_r8*rxt(k,527)*y(k,263)
         mat(k,1413) = mat(k,1413) + .240_r8*rxt(k,478)*y(k,170)
         mat(k,372) = mat(k,372) + .100_r8*rxt(k,480)*y(k,263)
         mat(k,1068) = mat(k,1068) + .630_r8*rxt(k,583)*y(k,170)
         mat(k,1524) = mat(k,1524) + .360_r8*rxt(k,492)*y(k,170)
         mat(k,2125) = rxt(k,233)*y(k,108)
         mat(k,1962) = mat(k,1962) + rxt(k,228)*y(k,108)
         mat(k,2510) = mat(k,2510) + rxt(k,396)*y(k,51) + rxt(k,200)*y(k,93) &
                      + rxt(k,201)*y(k,95) + rxt(k,302)*y(k,98) + rxt(k,269)*y(k,102) &
                      + rxt(k,206)*y(k,108) + rxt(k,304)*y(k,110) + rxt(k,272) &
                      *y(k,111)
         mat(k,2413) = mat(k,2413) + .630_r8*rxt(k,580)*y(k,6) + .130_r8*rxt(k,418) &
                      *y(k,28) + .360_r8*rxt(k,447)*y(k,33) + rxt(k,208)*y(k,92) &
                      + rxt(k,207)*y(k,108) + .320_r8*rxt(k,525)*y(k,130) &
                      + .240_r8*rxt(k,478)*y(k,139) + .630_r8*rxt(k,583)*y(k,144) &
                      + .360_r8*rxt(k,492)*y(k,145)
         mat(k,650) = mat(k,650) + .500_r8*rxt(k,460)*y(k,263)
         mat(k,242) = mat(k,242) + .500_r8*rxt(k,535)*y(k,263)
         mat(k,622) = .400_r8*rxt(k,536)*y(k,108)
         mat(k,1574) = .490_r8*rxt(k,432)*y(k,108)
         mat(k,874) = .400_r8*rxt(k,550)*y(k,108)
         mat(k,1542) = .450_r8*rxt(k,483)*y(k,108)
         mat(k,1026) = .400_r8*rxt(k,556)*y(k,108)
         mat(k,783) = .200_r8*rxt(k,487)*y(k,108)
         mat(k,1890) = rxt(k,410)*y(k,64) + rxt(k,182)*y(k,93) + rxt(k,308)*y(k,98) &
                      + rxt(k,281)*y(k,102) + rxt(k,413)*y(k,103) &
                      + 2.000_r8*rxt(k,183)*y(k,276)
         mat(k,2310) = mat(k,2310) + .650_r8*rxt(k,416)*y(k,27) + .500_r8*rxt(k,424) &
                      *y(k,31) + .300_r8*rxt(k,403)*y(k,63) + .600_r8*rxt(k,527) &
                      *y(k,134) + .100_r8*rxt(k,480)*y(k,140) + .500_r8*rxt(k,460) &
                      *y(k,183) + .500_r8*rxt(k,535)*y(k,219)
         mat(k,1361) = .150_r8*rxt(k,462)*y(k,108)
         mat(k,2875) = rxt(k,354)*y(k,89) + 2.000_r8*rxt(k,183)*y(k,259)
      end do
      end subroutine nlnmat11
      subroutine nlnmat12( avec_len, mat, y, rxt )
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
         mat(k,886) = -(rxt(k,755)*y(k,78) + rxt(k,757)*y(k,169))
         mat(k,1582) = -rxt(k,755)*y(k,264)
         mat(k,1762) = -rxt(k,757)*y(k,264)
         mat(k,2485) = rxt(k,748)*y(k,255) + rxt(k,749)*y(k,257)
         mat(k,721) = rxt(k,748)*y(k,168)
         mat(k,591) = rxt(k,749)*y(k,168)
         mat(k,534) = -(rxt(k,559)*y(k,108) + rxt(k,560)*y(k,158))
         mat(k,2710) = -rxt(k,559)*y(k,265)
         mat(k,2054) = -rxt(k,560)*y(k,265)
         mat(k,232) = .200_r8*rxt(k,549)*y(k,263)
         mat(k,200) = .140_r8*rxt(k,561)*y(k,263)
         mat(k,393) = rxt(k,564)*y(k,263)
         mat(k,2211) = .200_r8*rxt(k,549)*y(k,82) + .140_r8*rxt(k,561)*y(k,179) &
                      + rxt(k,564)*y(k,180)
         mat(k,909) = -(rxt(k,458)*y(k,108) + rxt(k,459)*y(k,158))
         mat(k,2736) = -rxt(k,458)*y(k,266)
         mat(k,2077) = -rxt(k,459)*y(k,266)
         mat(k,1253) = rxt(k,465)*y(k,263)
         mat(k,647) = .500_r8*rxt(k,460)*y(k,263)
         mat(k,2252) = rxt(k,465)*y(k,33) + .500_r8*rxt(k,460)*y(k,183)
         mat(k,1355) = -(rxt(k,461)*y(k,236) + rxt(k,462)*y(k,108) + rxt(k,463) &
                      *y(k,158))
         mat(k,1724) = -rxt(k,461)*y(k,267)
         mat(k,2756) = -rxt(k,462)*y(k,267)
         mat(k,2102) = -rxt(k,463)*y(k,267)
         mat(k,1118) = .060_r8*rxt(k,580)*y(k,170)
         mat(k,1172) = rxt(k,466)*y(k,263)
         mat(k,1063) = .060_r8*rxt(k,583)*y(k,170)
         mat(k,2391) = .060_r8*rxt(k,580)*y(k,6) + .060_r8*rxt(k,583)*y(k,144)
         mat(k,490) = rxt(k,464)*y(k,263)
         mat(k,1231) = .150_r8*rxt(k,601)*y(k,263)
         mat(k,2283) = rxt(k,466)*y(k,57) + rxt(k,464)*y(k,184) + .150_r8*rxt(k,601) &
                      *y(k,216)
         mat(k,1301) = -(rxt(k,590)*y(k,236) + rxt(k,591)*y(k,108) + rxt(k,592) &
                      *y(k,158))
         mat(k,1722) = -rxt(k,590)*y(k,268)
         mat(k,2753) = -rxt(k,591)*y(k,268)
         mat(k,2099) = -rxt(k,592)*y(k,268)
         mat(k,1933) = .500_r8*rxt(k,599)*y(k,215)
         mat(k,790) = rxt(k,593)*y(k,263)
         mat(k,1167) = .500_r8*rxt(k,599)*y(k,160) + rxt(k,600)*y(k,263)
         mat(k,2279) = rxt(k,593)*y(k,212) + rxt(k,600)*y(k,215)
         mat(k,1153) = -(rxt(k,595)*y(k,236) + rxt(k,596)*y(k,108) + rxt(k,597) &
                      *y(k,158))
         mat(k,1712) = -rxt(k,595)*y(k,269)
         mat(k,2744) = -rxt(k,596)*y(k,269)
         mat(k,2089) = -rxt(k,597)*y(k,269)
         mat(k,1112) = rxt(k,581)*y(k,263)
         mat(k,1057) = rxt(k,584)*y(k,263)
         mat(k,568) = rxt(k,598)*y(k,263)
         mat(k,2266) = rxt(k,581)*y(k,6) + rxt(k,584)*y(k,144) + rxt(k,598)*y(k,214)
         mat(k,833) = -(rxt(k,566)*y(k,108) + rxt(k,567)*y(k,158))
         mat(k,2731) = -rxt(k,566)*y(k,270)
         mat(k,2072) = -rxt(k,567)*y(k,270)
         mat(k,693) = rxt(k,568)*y(k,263)
         mat(k,228) = .650_r8*rxt(k,569)*y(k,263)
         mat(k,2245) = rxt(k,568)*y(k,217) + .650_r8*rxt(k,569)*y(k,218)
         mat(k,100) = -(rxt(k,695)*y(k,108) + rxt(k,696)*y(k,158))
         mat(k,2690) = -rxt(k,695)*y(k,271)
         mat(k,2044) = -rxt(k,696)*y(k,271)
         mat(k,223) = rxt(k,694)*y(k,263)
         mat(k,2152) = rxt(k,694)*y(k,218)
         mat(k,1371) = -(rxt(k,530)*y(k,235) + rxt(k,531)*y(k,236) + rxt(k,532) &
                      *y(k,108) + rxt(k,533)*y(k,158) + rxt(k,534)*y(k,160))
         mat(k,1557) = -rxt(k,530)*y(k,272)
         mat(k,1725) = -rxt(k,531)*y(k,272)
         mat(k,2757) = -rxt(k,532)*y(k,272)
         mat(k,2103) = -rxt(k,533)*y(k,272)
         mat(k,1938) = -rxt(k,534)*y(k,272)
         mat(k,277) = rxt(k,502)*y(k,263)
         mat(k,403) = rxt(k,503)*y(k,263)
         mat(k,163) = rxt(k,504)*y(k,263)
         mat(k,845) = .400_r8*rxt(k,527)*y(k,263)
         mat(k,241) = .500_r8*rxt(k,535)*y(k,263)
         mat(k,2284) = rxt(k,502)*y(k,114) + rxt(k,503)*y(k,116) + rxt(k,504)*y(k,125) &
                      + .400_r8*rxt(k,527)*y(k,134) + .500_r8*rxt(k,535)*y(k,219)
         mat(k,857) = -(rxt(k,572)*y(k,108) + rxt(k,573)*y(k,158))
         mat(k,2733) = -rxt(k,572)*y(k,273)
         mat(k,2073) = -rxt(k,573)*y(k,273)
         mat(k,248) = .560_r8*rxt(k,571)*y(k,263)
         mat(k,813) = rxt(k,574)*y(k,263)
         mat(k,2247) = .560_r8*rxt(k,571)*y(k,220) + rxt(k,574)*y(k,221)
         mat(k,106) = -(rxt(k,699)*y(k,108) + rxt(k,700)*y(k,158))
         mat(k,2691) = -rxt(k,699)*y(k,274)
         mat(k,2045) = -rxt(k,700)*y(k,274)
         mat(k,243) = rxt(k,698)*y(k,263)
         mat(k,2153) = rxt(k,698)*y(k,220)
         mat(k,602) = -(rxt(k,575)*y(k,108) + rxt(k,576)*y(k,158))
         mat(k,2717) = -rxt(k,575)*y(k,275)
         mat(k,2059) = -rxt(k,576)*y(k,275)
         mat(k,255) = .300_r8*rxt(k,577)*y(k,263)
         mat(k,508) = rxt(k,578)*y(k,263)
         mat(k,2219) = .300_r8*rxt(k,577)*y(k,222) + rxt(k,578)*y(k,223)
         mat(k,2888) = -(rxt(k,183)*y(k,259) + rxt(k,341)*y(k,122) + rxt(k,354) &
                      *y(k,89) + rxt(k,622)*y(k,189))
         mat(k,1903) = -rxt(k,183)*y(k,276)
         mat(k,303) = -rxt(k,341)*y(k,276)
         mat(k,1018) = -rxt(k,354)*y(k,276)
         mat(k,316) = -rxt(k,622)*y(k,276)
         mat(k,298) = rxt(k,361)*y(k,263)
         mat(k,341) = rxt(k,426)*y(k,263)
         mat(k,500) = rxt(k,451)*y(k,263)
         mat(k,347) = rxt(k,452)*y(k,263)
         mat(k,566) = rxt(k,363)*y(k,263)
         mat(k,360) = rxt(k,366)*y(k,263)
         mat(k,2354) = rxt(k,397)*y(k,263)
         mat(k,687) = rxt(k,368)*y(k,263)
         mat(k,148) = rxt(k,369)*y(k,263)
         mat(k,1282) = rxt(k,428)*y(k,263)
         mat(k,458) = rxt(k,371)*y(k,263)
         mat(k,1176) = rxt(k,466)*y(k,263)
         mat(k,1426) = rxt(k,454)*y(k,263)
         mat(k,799) = rxt(k,434)*y(k,263)
         mat(k,734) = rxt(k,435)*y(k,263)
         mat(k,423) = rxt(k,373)*y(k,263)
         mat(k,476) = rxt(k,403)*y(k,263)
         mat(k,1676) = rxt(k,404)*y(k,263)
         mat(k,1251) = rxt(k,380)*y(k,108)
         mat(k,415) = rxt(k,385)*y(k,263)
         mat(k,2450) = rxt(k,204)*y(k,108)
         mat(k,1626) = rxt(k,209)*y(k,263)
         mat(k,705) = rxt(k,210)*y(k,263)
         mat(k,1612) = (rxt(k,630)+rxt(k,704)+rxt(k,717)+rxt(k,726))*y(k,110) + ( &
                      + rxt(k,629)+rxt(k,706)+rxt(k,714)+rxt(k,723))*y(k,111) + ( &
                      + rxt(k,637)+rxt(k,733)+rxt(k,737)+rxt(k,741))*y(k,112) &
                      + rxt(k,303)*y(k,263)
         mat(k,354) = rxt(k,388)*y(k,263)
         mat(k,1855) = (rxt(k,632)+rxt(k,703)+rxt(k,716)+rxt(k,725))*y(k,110) + ( &
                      + rxt(k,631)+rxt(k,702)+rxt(k,713)+rxt(k,722))*y(k,111) + ( &
                      + rxt(k,636)+rxt(k,732)+rxt(k,736)+rxt(k,740))*y(k,112) &
                      + rxt(k,270)*y(k,263)
         mat(k,1032) = rxt(k,406)*y(k,263)
         mat(k,1351) = (rxt(k,634)+rxt(k,705)+rxt(k,718)+rxt(k,727))*y(k,110) + ( &
                      + rxt(k,633)+rxt(k,707)+rxt(k,715)+rxt(k,724))*y(k,111) + ( &
                      + rxt(k,638)+rxt(k,734)+rxt(k,738)+rxt(k,742))*y(k,112) &
                      + rxt(k,338)*y(k,263)
         mat(k,2476) = rxt(k,241)*y(k,263)
         mat(k,2795) = rxt(k,380)*y(k,68) + rxt(k,204)*y(k,92) + rxt(k,211)*y(k,263)
         mat(k,589) = rxt(k,218)*y(k,263)
         mat(k,1699) = (rxt(k,630)+rxt(k,704)+rxt(k,717)+rxt(k,726))*y(k,98) + ( &
                      + rxt(k,632)+rxt(k,703)+rxt(k,716)+rxt(k,725))*y(k,102) + ( &
                      + rxt(k,634)+rxt(k,705)+rxt(k,718)+rxt(k,727))*y(k,106)
         mat(k,1832) = (rxt(k,629)+rxt(k,706)+rxt(k,714)+rxt(k,723))*y(k,98) + ( &
                      + rxt(k,631)+rxt(k,702)+rxt(k,713)+rxt(k,722))*y(k,102) + ( &
                      + rxt(k,633)+rxt(k,707)+rxt(k,715)+rxt(k,724))*y(k,106) &
                      + rxt(k,273)*y(k,263)
         mat(k,1808) = (rxt(k,637)+rxt(k,733)+rxt(k,737)+rxt(k,741))*y(k,98) + ( &
                      + rxt(k,636)+rxt(k,732)+rxt(k,736)+rxt(k,740))*y(k,102) + ( &
                      + rxt(k,638)+rxt(k,734)+rxt(k,738)+rxt(k,742))*y(k,106) &
                      + rxt(k,311)*y(k,263)
         mat(k,1417) = .500_r8*rxt(k,479)*y(k,263)
         mat(k,114) = rxt(k,640)*y(k,263)
         mat(k,653) = rxt(k,460)*y(k,263)
         mat(k,494) = rxt(k,464)*y(k,263)
         mat(k,2323) = rxt(k,361)*y(k,29) + rxt(k,426)*y(k,32) + rxt(k,451)*y(k,34) &
                      + rxt(k,452)*y(k,35) + rxt(k,363)*y(k,45) + rxt(k,366)*y(k,47) &
                      + rxt(k,397)*y(k,51) + rxt(k,368)*y(k,52) + rxt(k,369)*y(k,53) &
                      + rxt(k,428)*y(k,54) + rxt(k,371)*y(k,55) + rxt(k,466)*y(k,57) &
                      + rxt(k,454)*y(k,58) + rxt(k,434)*y(k,59) + rxt(k,435)*y(k,60) &
                      + rxt(k,373)*y(k,61) + rxt(k,403)*y(k,63) + rxt(k,404)*y(k,64) &
                      + rxt(k,385)*y(k,69) + rxt(k,209)*y(k,93) + rxt(k,210)*y(k,95) &
                      + rxt(k,303)*y(k,98) + rxt(k,388)*y(k,101) + rxt(k,270)*y(k,102) &
                      + rxt(k,406)*y(k,104) + rxt(k,338)*y(k,106) + rxt(k,241) &
                      *y(k,107) + rxt(k,211)*y(k,108) + rxt(k,218)*y(k,109) &
                      + rxt(k,273)*y(k,111) + rxt(k,311)*y(k,112) + .500_r8*rxt(k,479) &
                      *y(k,139) + rxt(k,640)*y(k,154) + rxt(k,460)*y(k,183) &
                      + rxt(k,464)*y(k,184) + 2.000_r8*rxt(k,214)*y(k,263)
      end do
      end subroutine nlnmat12
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
         mat(k, 62) = mat(k, 62) + lmat(k, 62)
         mat(k, 68) = mat(k, 68) + lmat(k, 68)
         mat(k, 74) = mat(k, 74) + lmat(k, 74)
         mat(k, 80) = mat(k, 80) + lmat(k, 80)
         mat(k, 86) = mat(k, 86) + lmat(k, 86)
         mat(k, 88) = mat(k, 88) + lmat(k, 88)
         mat(k, 94) = mat(k, 94) + lmat(k, 94)
         mat(k, 100) = mat(k, 100) + lmat(k, 100)
         mat(k, 106) = mat(k, 106) + lmat(k, 106)
         mat(k, 107) = lmat(k, 107)
         mat(k, 108) = lmat(k, 108)
         mat(k, 109) = lmat(k, 109)
         mat(k, 110) = lmat(k, 110)
         mat(k, 111) = lmat(k, 111)
         mat(k, 112) = mat(k, 112) + lmat(k, 112)
         mat(k, 115) = mat(k, 115) + lmat(k, 115)
         mat(k, 117) = mat(k, 117) + lmat(k, 117)
         mat(k, 118) = mat(k, 118) + lmat(k, 118)
         mat(k, 121) = mat(k, 121) + lmat(k, 121)
         mat(k, 123) = mat(k, 123) + lmat(k, 123)
         mat(k, 124) = mat(k, 124) + lmat(k, 124)
         mat(k, 125) = mat(k, 125) + lmat(k, 125)
         mat(k, 126) = mat(k, 126) + lmat(k, 126)
         mat(k, 128) = mat(k, 128) + lmat(k, 128)
         mat(k, 129) = mat(k, 129) + lmat(k, 129)
         mat(k, 130) = mat(k, 130) + lmat(k, 130)
         mat(k, 132) = mat(k, 132) + lmat(k, 132)
         mat(k, 133) = mat(k, 133) + lmat(k, 133)
         mat(k, 134) = mat(k, 134) + lmat(k, 134)
         mat(k, 136) = mat(k, 136) + lmat(k, 136)
         mat(k, 137) = mat(k, 137) + lmat(k, 137)
         mat(k, 138) = mat(k, 138) + lmat(k, 138)
         mat(k, 140) = mat(k, 140) + lmat(k, 140)
         mat(k, 141) = mat(k, 141) + lmat(k, 141)
         mat(k, 143) = mat(k, 143) + lmat(k, 143)
         mat(k, 144) = mat(k, 144) + lmat(k, 144)
         mat(k, 145) = mat(k, 145) + lmat(k, 145)
         mat(k, 147) = mat(k, 147) + lmat(k, 147)
         mat(k, 149) = mat(k, 149) + lmat(k, 149)
         mat(k, 151) = mat(k, 151) + lmat(k, 151)
         mat(k, 152) = mat(k, 152) + lmat(k, 152)
         mat(k, 153) = lmat(k, 153)
         mat(k, 154) = lmat(k, 154)
         mat(k, 155) = lmat(k, 155)
         mat(k, 156) = lmat(k, 156)
         mat(k, 157) = lmat(k, 157)
         mat(k, 158) = lmat(k, 158)
         mat(k, 159) = lmat(k, 159)
         mat(k, 160) = lmat(k, 160)
         mat(k, 161) = lmat(k, 161)
         mat(k, 162) = mat(k, 162) + lmat(k, 162)
         mat(k, 165) = lmat(k, 165)
         mat(k, 166) = lmat(k, 166)
         mat(k, 167) = lmat(k, 167)
         mat(k, 168) = mat(k, 168) + lmat(k, 168)
         mat(k, 169) = mat(k, 169) + lmat(k, 169)
         mat(k, 171) = mat(k, 171) + lmat(k, 171)
         mat(k, 172) = mat(k, 172) + lmat(k, 172)
         mat(k, 173) = mat(k, 173) + lmat(k, 173)
         mat(k, 174) = mat(k, 174) + lmat(k, 174)
         mat(k, 175) = mat(k, 175) + lmat(k, 175)
         mat(k, 177) = mat(k, 177) + lmat(k, 177)
         mat(k, 178) = mat(k, 178) + lmat(k, 178)
         mat(k, 179) = mat(k, 179) + lmat(k, 179)
         mat(k, 180) = mat(k, 180) + lmat(k, 180)
         mat(k, 182) = mat(k, 182) + lmat(k, 182)
         mat(k, 183) = mat(k, 183) + lmat(k, 183)
         mat(k, 184) = mat(k, 184) + lmat(k, 184)
         mat(k, 185) = mat(k, 185) + lmat(k, 185)
         mat(k, 187) = mat(k, 187) + lmat(k, 187)
         mat(k, 189) = mat(k, 189) + lmat(k, 189)
         mat(k, 195) = lmat(k, 195)
         mat(k, 196) = lmat(k, 196)
         mat(k, 197) = lmat(k, 197)
         mat(k, 198) = lmat(k, 198)
         mat(k, 199) = mat(k, 199) + lmat(k, 199)
         mat(k, 204) = mat(k, 204) + lmat(k, 204)
         mat(k, 205) = mat(k, 205) + lmat(k, 205)
         mat(k, 206) = mat(k, 206) + lmat(k, 206)
         mat(k, 207) = mat(k, 207) + lmat(k, 207)
         mat(k, 208) = lmat(k, 208)
         mat(k, 209) = mat(k, 209) + lmat(k, 209)
         mat(k, 213) = mat(k, 213) + lmat(k, 213)
         mat(k, 215) = mat(k, 215) + lmat(k, 215)
         mat(k, 216) = lmat(k, 216)
         mat(k, 217) = mat(k, 217) + lmat(k, 217)
         mat(k, 220) = mat(k, 220) + lmat(k, 220)
         mat(k, 221) = lmat(k, 221)
         mat(k, 222) = lmat(k, 222)
         mat(k, 224) = mat(k, 224) + lmat(k, 224)
         mat(k, 231) = mat(k, 231) + lmat(k, 231)
         mat(k, 236) = lmat(k, 236)
         mat(k, 237) = lmat(k, 237)
         mat(k, 238) = lmat(k, 238)
         mat(k, 239) = lmat(k, 239)
         mat(k, 240) = mat(k, 240) + lmat(k, 240)
         mat(k, 242) = mat(k, 242) + lmat(k, 242)
         mat(k, 245) = mat(k, 245) + lmat(k, 245)
         mat(k, 253) = mat(k, 253) + lmat(k, 253)
         mat(k, 258) = mat(k, 258) + lmat(k, 258)
         mat(k, 259) = mat(k, 259) + lmat(k, 259)
         mat(k, 262) = mat(k, 262) + lmat(k, 262)
         mat(k, 263) = mat(k, 263) + lmat(k, 263)
         mat(k, 264) = mat(k, 264) + lmat(k, 264)
         mat(k, 266) = mat(k, 266) + lmat(k, 266)
         mat(k, 267) = mat(k, 267) + lmat(k, 267)
         mat(k, 268) = mat(k, 268) + lmat(k, 268)
         mat(k, 271) = mat(k, 271) + lmat(k, 271)
         mat(k, 272) = mat(k, 272) + lmat(k, 272)
         mat(k, 273) = mat(k, 273) + lmat(k, 273)
         mat(k, 275) = mat(k, 275) + lmat(k, 275)
         mat(k, 276) = lmat(k, 276)
         mat(k, 278) = mat(k, 278) + lmat(k, 278)
         mat(k, 279) = lmat(k, 279)
         mat(k, 280) = mat(k, 280) + lmat(k, 280)
         mat(k, 282) = lmat(k, 282)
         mat(k, 283) = lmat(k, 283)
         mat(k, 284) = lmat(k, 284)
         mat(k, 285) = mat(k, 285) + lmat(k, 285)
         mat(k, 288) = mat(k, 288) + lmat(k, 288)
         mat(k, 291) = lmat(k, 291)
         mat(k, 292) = lmat(k, 292)
         mat(k, 293) = lmat(k, 293)
         mat(k, 294) = mat(k, 294) + lmat(k, 294)
         mat(k, 297) = mat(k, 297) + lmat(k, 297)
         mat(k, 300) = mat(k, 300) + lmat(k, 300)
         mat(k, 304) = mat(k, 304) + lmat(k, 304)
         mat(k, 306) = mat(k, 306) + lmat(k, 306)
         mat(k, 308) = mat(k, 308) + lmat(k, 308)
         mat(k, 313) = mat(k, 313) + lmat(k, 313)
         mat(k, 314) = lmat(k, 314)
         mat(k, 315) = lmat(k, 315)
         mat(k, 317) = mat(k, 317) + lmat(k, 317)
         mat(k, 318) = lmat(k, 318)
         mat(k, 320) = mat(k, 320) + lmat(k, 320)
         mat(k, 321) = mat(k, 321) + lmat(k, 321)
         mat(k, 322) = lmat(k, 322)
         mat(k, 323) = lmat(k, 323)
         mat(k, 325) = mat(k, 325) + lmat(k, 325)
         mat(k, 326) = lmat(k, 326)
         mat(k, 327) = lmat(k, 327)
         mat(k, 328) = lmat(k, 328)
         mat(k, 329) = lmat(k, 329)
         mat(k, 330) = mat(k, 330) + lmat(k, 330)
         mat(k, 336) = mat(k, 336) + lmat(k, 336)
         mat(k, 342) = mat(k, 342) + lmat(k, 342)
         mat(k, 348) = mat(k, 348) + lmat(k, 348)
         mat(k, 349) = mat(k, 349) + lmat(k, 349)
         mat(k, 353) = mat(k, 353) + lmat(k, 353)
         mat(k, 355) = mat(k, 355) + lmat(k, 355)
         mat(k, 359) = mat(k, 359) + lmat(k, 359)
         mat(k, 361) = lmat(k, 361)
         mat(k, 362) = lmat(k, 362)
         mat(k, 363) = lmat(k, 363)
         mat(k, 364) = lmat(k, 364)
         mat(k, 365) = lmat(k, 365)
         mat(k, 366) = lmat(k, 366)
         mat(k, 367) = lmat(k, 367)
         mat(k, 368) = lmat(k, 368)
         mat(k, 369) = mat(k, 369) + lmat(k, 369)
         mat(k, 374) = mat(k, 374) + lmat(k, 374)
         mat(k, 376) = lmat(k, 376)
         mat(k, 377) = lmat(k, 377)
         mat(k, 378) = mat(k, 378) + lmat(k, 378)
         mat(k, 379) = lmat(k, 379)
         mat(k, 380) = lmat(k, 380)
         mat(k, 381) = lmat(k, 381)
         mat(k, 382) = lmat(k, 382)
         mat(k, 383) = lmat(k, 383)
         mat(k, 384) = lmat(k, 384)
         mat(k, 385) = lmat(k, 385)
         mat(k, 386) = lmat(k, 386)
         mat(k, 387) = mat(k, 387) + lmat(k, 387)
         mat(k, 390) = mat(k, 390) + lmat(k, 390)
         mat(k, 391) = lmat(k, 391)
         mat(k, 392) = mat(k, 392) + lmat(k, 392)
         mat(k, 394) = lmat(k, 394)
         mat(k, 395) = mat(k, 395) + lmat(k, 395)
         mat(k, 396) = lmat(k, 396)
         mat(k, 397) = mat(k, 397) + lmat(k, 397)
         mat(k, 399) = mat(k, 399) + lmat(k, 399)
         mat(k, 400) = mat(k, 400) + lmat(k, 400)
         mat(k, 401) = lmat(k, 401)
         mat(k, 402) = mat(k, 402) + lmat(k, 402)
         mat(k, 405) = lmat(k, 405)
         mat(k, 406) = lmat(k, 406)
         mat(k, 407) = lmat(k, 407)
         mat(k, 409) = mat(k, 409) + lmat(k, 409)
         mat(k, 410) = lmat(k, 410)
         mat(k, 414) = mat(k, 414) + lmat(k, 414)
         mat(k, 416) = mat(k, 416) + lmat(k, 416)
         mat(k, 417) = lmat(k, 417)
         mat(k, 421) = mat(k, 421) + lmat(k, 421)
         mat(k, 424) = mat(k, 424) + lmat(k, 424)
         mat(k, 432) = lmat(k, 432)
         mat(k, 433) = lmat(k, 433)
         mat(k, 434) = lmat(k, 434)
         mat(k, 435) = lmat(k, 435)
         mat(k, 436) = mat(k, 436) + lmat(k, 436)
         mat(k, 438) = lmat(k, 438)
         mat(k, 439) = lmat(k, 439)
         mat(k, 440) = lmat(k, 440)
         mat(k, 441) = mat(k, 441) + lmat(k, 441)
         mat(k, 442) = lmat(k, 442)
         mat(k, 443) = mat(k, 443) + lmat(k, 443)
         mat(k, 444) = lmat(k, 444)
         mat(k, 446) = mat(k, 446) + lmat(k, 446)
         mat(k, 451) = mat(k, 451) + lmat(k, 451)
         mat(k, 453) = lmat(k, 453)
         mat(k, 456) = mat(k, 456) + lmat(k, 456)
         mat(k, 459) = mat(k, 459) + lmat(k, 459)
         mat(k, 460) = lmat(k, 460)
         mat(k, 462) = lmat(k, 462)
         mat(k, 464) = mat(k, 464) + lmat(k, 464)
         mat(k, 465) = mat(k, 465) + lmat(k, 465)
         mat(k, 469) = lmat(k, 469)
         mat(k, 471) = mat(k, 471) + lmat(k, 471)
         mat(k, 473) = mat(k, 473) + lmat(k, 473)
         mat(k, 474) = mat(k, 474) + lmat(k, 474)
         mat(k, 475) = lmat(k, 475)
         mat(k, 477) = mat(k, 477) + lmat(k, 477)
         mat(k, 478) = lmat(k, 478)
         mat(k, 480) = lmat(k, 480)
         mat(k, 481) = mat(k, 481) + lmat(k, 481)
         mat(k, 482) = lmat(k, 482)
         mat(k, 483) = lmat(k, 483)
         mat(k, 484) = lmat(k, 484)
         mat(k, 485) = lmat(k, 485)
         mat(k, 486) = lmat(k, 486)
         mat(k, 487) = lmat(k, 487)
         mat(k, 488) = lmat(k, 488)
         mat(k, 489) = mat(k, 489) + lmat(k, 489)
         mat(k, 491) = lmat(k, 491)
         mat(k, 492) = mat(k, 492) + lmat(k, 492)
         mat(k, 493) = lmat(k, 493)
         mat(k, 495) = mat(k, 495) + lmat(k, 495)
         mat(k, 497) = lmat(k, 497)
         mat(k, 498) = mat(k, 498) + lmat(k, 498)
         mat(k, 499) = lmat(k, 499)
         mat(k, 501) = mat(k, 501) + lmat(k, 501)
         mat(k, 506) = mat(k, 506) + lmat(k, 506)
         mat(k, 507) = mat(k, 507) + lmat(k, 507)
         mat(k, 509) = lmat(k, 509)
         mat(k, 510) = lmat(k, 510)
         mat(k, 511) = mat(k, 511) + lmat(k, 511)
         mat(k, 512) = lmat(k, 512)
         mat(k, 515) = mat(k, 515) + lmat(k, 515)
         mat(k, 521) = mat(k, 521) + lmat(k, 521)
         mat(k, 525) = lmat(k, 525)
         mat(k, 526) = mat(k, 526) + lmat(k, 526)
         mat(k, 527) = mat(k, 527) + lmat(k, 527)
         mat(k, 528) = lmat(k, 528)
         mat(k, 530) = lmat(k, 530)
         mat(k, 531) = mat(k, 531) + lmat(k, 531)
         mat(k, 532) = lmat(k, 532)
         mat(k, 534) = mat(k, 534) + lmat(k, 534)
         mat(k, 540) = mat(k, 540) + lmat(k, 540)
         mat(k, 543) = lmat(k, 543)
         mat(k, 544) = mat(k, 544) + lmat(k, 544)
         mat(k, 545) = mat(k, 545) + lmat(k, 545)
         mat(k, 548) = lmat(k, 548)
         mat(k, 549) = mat(k, 549) + lmat(k, 549)
         mat(k, 550) = lmat(k, 550)
         mat(k, 551) = lmat(k, 551)
         mat(k, 552) = lmat(k, 552)
         mat(k, 553) = lmat(k, 553)
         mat(k, 554) = lmat(k, 554)
         mat(k, 555) = lmat(k, 555)
         mat(k, 556) = mat(k, 556) + lmat(k, 556)
         mat(k, 560) = mat(k, 560) + lmat(k, 560)
         mat(k, 565) = mat(k, 565) + lmat(k, 565)
         mat(k, 567) = mat(k, 567) + lmat(k, 567)
         mat(k, 569) = lmat(k, 569)
         mat(k, 570) = lmat(k, 570)
         mat(k, 571) = mat(k, 571) + lmat(k, 571)
         mat(k, 572) = lmat(k, 572)
         mat(k, 573) = lmat(k, 573)
         mat(k, 576) = mat(k, 576) + lmat(k, 576)
         mat(k, 583) = mat(k, 583) + lmat(k, 583)
         mat(k, 585) = lmat(k, 585)
         mat(k, 586) = mat(k, 586) + lmat(k, 586)
         mat(k, 587) = mat(k, 587) + lmat(k, 587)
         mat(k, 588) = lmat(k, 588)
         mat(k, 590) = mat(k, 590) + lmat(k, 590)
         mat(k, 597) = mat(k, 597) + lmat(k, 597)
         mat(k, 600) = mat(k, 600) + lmat(k, 600)
         mat(k, 602) = mat(k, 602) + lmat(k, 602)
         mat(k, 609) = mat(k, 609) + lmat(k, 609)
         mat(k, 611) = lmat(k, 611)
         mat(k, 612) = lmat(k, 612)
         mat(k, 614) = lmat(k, 614)
         mat(k, 615) = lmat(k, 615)
         mat(k, 616) = lmat(k, 616)
         mat(k, 618) = mat(k, 618) + lmat(k, 618)
         mat(k, 624) = lmat(k, 624)
         mat(k, 625) = mat(k, 625) + lmat(k, 625)
         mat(k, 627) = mat(k, 627) + lmat(k, 627)
         mat(k, 629) = lmat(k, 629)
         mat(k, 630) = mat(k, 630) + lmat(k, 630)
         mat(k, 638) = mat(k, 638) + lmat(k, 638)
         mat(k, 639) = lmat(k, 639)
         mat(k, 640) = lmat(k, 640)
         mat(k, 641) = lmat(k, 641)
         mat(k, 642) = mat(k, 642) + lmat(k, 642)
         mat(k, 643) = lmat(k, 643)
         mat(k, 646) = mat(k, 646) + lmat(k, 646)
         mat(k, 648) = lmat(k, 648)
         mat(k, 650) = mat(k, 650) + lmat(k, 650)
         mat(k, 651) = lmat(k, 651)
         mat(k, 652) = lmat(k, 652)
         mat(k, 654) = mat(k, 654) + lmat(k, 654)
         mat(k, 662) = mat(k, 662) + lmat(k, 662)
         mat(k, 668) = lmat(k, 668)
         mat(k, 670) = mat(k, 670) + lmat(k, 670)
         mat(k, 672) = lmat(k, 672)
         mat(k, 675) = lmat(k, 675)
         mat(k, 679) = mat(k, 679) + lmat(k, 679)
         mat(k, 680) = lmat(k, 680)
         mat(k, 686) = mat(k, 686) + lmat(k, 686)
         mat(k, 688) = lmat(k, 688)
         mat(k, 689) = lmat(k, 689)
         mat(k, 690) = mat(k, 690) + lmat(k, 690)
         mat(k, 691) = lmat(k, 691)
         mat(k, 695) = lmat(k, 695)
         mat(k, 696) = lmat(k, 696)
         mat(k, 697) = mat(k, 697) + lmat(k, 697)
         mat(k, 698) = lmat(k, 698)
         mat(k, 699) = mat(k, 699) + lmat(k, 699)
         mat(k, 701) = mat(k, 701) + lmat(k, 701)
         mat(k, 706) = lmat(k, 706)
         mat(k, 707) = lmat(k, 707)
         mat(k, 708) = lmat(k, 708)
         mat(k, 709) = lmat(k, 709)
         mat(k, 710) = mat(k, 710) + lmat(k, 710)
         mat(k, 717) = lmat(k, 717)
         mat(k, 719) = mat(k, 719) + lmat(k, 719)
         mat(k, 728) = mat(k, 728) + lmat(k, 728)
         mat(k, 730) = mat(k, 730) + lmat(k, 730)
         mat(k, 731) = lmat(k, 731)
         mat(k, 732) = mat(k, 732) + lmat(k, 732)
         mat(k, 735) = mat(k, 735) + lmat(k, 735)
         mat(k, 736) = mat(k, 736) + lmat(k, 736)
         mat(k, 737) = mat(k, 737) + lmat(k, 737)
         mat(k, 739) = lmat(k, 739)
         mat(k, 740) = lmat(k, 740)
         mat(k, 743) = mat(k, 743) + lmat(k, 743)
         mat(k, 749) = mat(k, 749) + lmat(k, 749)
         mat(k, 750) = mat(k, 750) + lmat(k, 750)
         mat(k, 755) = lmat(k, 755)
         mat(k, 756) = mat(k, 756) + lmat(k, 756)
         mat(k, 760) = lmat(k, 760)
         mat(k, 761) = lmat(k, 761)
         mat(k, 763) = mat(k, 763) + lmat(k, 763)
         mat(k, 764) = lmat(k, 764)
         mat(k, 765) = lmat(k, 765)
         mat(k, 766) = lmat(k, 766)
         mat(k, 767) = mat(k, 767) + lmat(k, 767)
         mat(k, 770) = mat(k, 770) + lmat(k, 770)
         mat(k, 771) = mat(k, 771) + lmat(k, 771)
         mat(k, 773) = mat(k, 773) + lmat(k, 773)
         mat(k, 775) = mat(k, 775) + lmat(k, 775)
         mat(k, 776) = lmat(k, 776)
         mat(k, 778) = mat(k, 778) + lmat(k, 778)
         mat(k, 785) = mat(k, 785) + lmat(k, 785)
         mat(k, 786) = lmat(k, 786)
         mat(k, 787) = lmat(k, 787)
         mat(k, 788) = lmat(k, 788)
         mat(k, 789) = lmat(k, 789)
         mat(k, 791) = lmat(k, 791)
         mat(k, 792) = mat(k, 792) + lmat(k, 792)
         mat(k, 793) = lmat(k, 793)
         mat(k, 794) = lmat(k, 794)
         mat(k, 795) = mat(k, 795) + lmat(k, 795)
         mat(k, 800) = mat(k, 800) + lmat(k, 800)
         mat(k, 807) = lmat(k, 807)
         mat(k, 808) = lmat(k, 808)
         mat(k, 809) = lmat(k, 809)
         mat(k, 810) = lmat(k, 810)
         mat(k, 811) = mat(k, 811) + lmat(k, 811)
         mat(k, 816) = lmat(k, 816)
         mat(k, 818) = lmat(k, 818)
         mat(k, 820) = mat(k, 820) + lmat(k, 820)
         mat(k, 821) = lmat(k, 821)
         mat(k, 822) = mat(k, 822) + lmat(k, 822)
         mat(k, 833) = mat(k, 833) + lmat(k, 833)
         mat(k, 844) = mat(k, 844) + lmat(k, 844)
         mat(k, 846) = lmat(k, 846)
         mat(k, 847) = lmat(k, 847)
         mat(k, 848) = mat(k, 848) + lmat(k, 848)
         mat(k, 849) = lmat(k, 849)
         mat(k, 850) = lmat(k, 850)
         mat(k, 857) = mat(k, 857) + lmat(k, 857)
         mat(k, 868) = mat(k, 868) + lmat(k, 868)
         mat(k, 878) = mat(k, 878) + lmat(k, 878)
         mat(k, 886) = mat(k, 886) + lmat(k, 886)
         mat(k, 887) = lmat(k, 887)
         mat(k, 889) = lmat(k, 889)
         mat(k, 897) = mat(k, 897) + lmat(k, 897)
         mat(k, 898) = lmat(k, 898)
         mat(k, 903) = lmat(k, 903)
         mat(k, 904) = mat(k, 904) + lmat(k, 904)
         mat(k, 909) = mat(k, 909) + lmat(k, 909)
         mat(k, 920) = mat(k, 920) + lmat(k, 920)
         mat(k, 927) = mat(k, 927) + lmat(k, 927)
         mat(k, 928) = mat(k, 928) + lmat(k, 928)
         mat(k, 933) = mat(k, 933) + lmat(k, 933)
         mat(k, 940) = mat(k, 940) + lmat(k, 940)
         mat(k, 948) = lmat(k, 948)
         mat(k, 949) = lmat(k, 949)
         mat(k, 950) = mat(k, 950) + lmat(k, 950)
         mat(k, 952) = lmat(k, 952)
         mat(k, 953) = mat(k, 953) + lmat(k, 953)
         mat(k, 954) = lmat(k, 954)
         mat(k, 955) = lmat(k, 955)
         mat(k, 957) = lmat(k, 957)
         mat(k, 958) = mat(k, 958) + lmat(k, 958)
         mat(k, 960) = mat(k, 960) + lmat(k, 960)
         mat(k, 961) = mat(k, 961) + lmat(k, 961)
         mat(k, 962) = mat(k, 962) + lmat(k, 962)
         mat(k, 963) = lmat(k, 963)
         mat(k, 964) = mat(k, 964) + lmat(k, 964)
         mat(k, 965) = lmat(k, 965)
         mat(k, 967) = lmat(k, 967)
         mat(k, 970) = mat(k, 970) + lmat(k, 970)
         mat(k, 971) = mat(k, 971) + lmat(k, 971)
         mat(k, 975) = mat(k, 975) + lmat(k, 975)
         mat(k, 991) = mat(k, 991) + lmat(k, 991)
         mat(k, 993) = lmat(k, 993)
         mat(k, 994) = mat(k, 994) + lmat(k, 994)
         mat(k, 996) = lmat(k, 996)
         mat(k,1000) = mat(k,1000) + lmat(k,1000)
         mat(k,1010) = mat(k,1010) + lmat(k,1010)
         mat(k,1019) = mat(k,1019) + lmat(k,1019)
         mat(k,1028) = mat(k,1028) + lmat(k,1028)
         mat(k,1034) = mat(k,1034) + lmat(k,1034)
         mat(k,1054) = mat(k,1054) + lmat(k,1054)
         mat(k,1073) = mat(k,1073) + lmat(k,1073)
         mat(k,1074) = mat(k,1074) + lmat(k,1074)
         mat(k,1075) = mat(k,1075) + lmat(k,1075)
         mat(k,1077) = mat(k,1077) + lmat(k,1077)
         mat(k,1078) = lmat(k,1078)
         mat(k,1080) = mat(k,1080) + lmat(k,1080)
         mat(k,1082) = mat(k,1082) + lmat(k,1082)
         mat(k,1083) = mat(k,1083) + lmat(k,1083)
         mat(k,1085) = lmat(k,1085)
         mat(k,1089) = lmat(k,1089)
         mat(k,1090) = mat(k,1090) + lmat(k,1090)
         mat(k,1109) = mat(k,1109) + lmat(k,1109)
         mat(k,1133) = mat(k,1133) + lmat(k,1133)
         mat(k,1144) = lmat(k,1144)
         mat(k,1145) = mat(k,1145) + lmat(k,1145)
         mat(k,1146) = mat(k,1146) + lmat(k,1146)
         mat(k,1149) = mat(k,1149) + lmat(k,1149)
         mat(k,1153) = mat(k,1153) + lmat(k,1153)
         mat(k,1163) = mat(k,1163) + lmat(k,1163)
         mat(k,1165) = lmat(k,1165)
         mat(k,1166) = lmat(k,1166)
         mat(k,1170) = lmat(k,1170)
         mat(k,1171) = mat(k,1171) + lmat(k,1171)
         mat(k,1173) = lmat(k,1173)
         mat(k,1174) = lmat(k,1174)
         mat(k,1177) = mat(k,1177) + lmat(k,1177)
         mat(k,1190) = mat(k,1190) + lmat(k,1190)
         mat(k,1191) = lmat(k,1191)
         mat(k,1193) = lmat(k,1193)
         mat(k,1196) = lmat(k,1196)
         mat(k,1200) = mat(k,1200) + lmat(k,1200)
         mat(k,1206) = lmat(k,1206)
         mat(k,1210) = lmat(k,1210)
         mat(k,1211) = mat(k,1211) + lmat(k,1211)
         mat(k,1216) = mat(k,1216) + lmat(k,1216)
         mat(k,1228) = mat(k,1228) + lmat(k,1228)
         mat(k,1229) = mat(k,1229) + lmat(k,1229)
         mat(k,1230) = mat(k,1230) + lmat(k,1230)
         mat(k,1231) = mat(k,1231) + lmat(k,1231)
         mat(k,1232) = mat(k,1232) + lmat(k,1232)
         mat(k,1233) = mat(k,1233) + lmat(k,1233)
         mat(k,1236) = mat(k,1236) + lmat(k,1236)
         mat(k,1237) = mat(k,1237) + lmat(k,1237)
         mat(k,1240) = mat(k,1240) + lmat(k,1240)
         mat(k,1256) = mat(k,1256) + lmat(k,1256)
         mat(k,1274) = mat(k,1274) + lmat(k,1274)
         mat(k,1275) = lmat(k,1275)
         mat(k,1277) = lmat(k,1277)
         mat(k,1281) = lmat(k,1281)
         mat(k,1283) = mat(k,1283) + lmat(k,1283)
         mat(k,1288) = lmat(k,1288)
         mat(k,1289) = mat(k,1289) + lmat(k,1289)
         mat(k,1292) = mat(k,1292) + lmat(k,1292)
         mat(k,1293) = mat(k,1293) + lmat(k,1293)
         mat(k,1301) = mat(k,1301) + lmat(k,1301)
         mat(k,1314) = lmat(k,1314)
         mat(k,1315) = lmat(k,1315)
         mat(k,1316) = lmat(k,1316)
         mat(k,1317) = lmat(k,1317)
         mat(k,1318) = mat(k,1318) + lmat(k,1318)
         mat(k,1319) = lmat(k,1319)
         mat(k,1321) = lmat(k,1321)
         mat(k,1325) = lmat(k,1325)
         mat(k,1327) = lmat(k,1327)
         mat(k,1328) = lmat(k,1328)
         mat(k,1329) = mat(k,1329) + lmat(k,1329)
         mat(k,1331) = mat(k,1331) + lmat(k,1331)
         mat(k,1333) = lmat(k,1333)
         mat(k,1335) = lmat(k,1335)
         mat(k,1336) = mat(k,1336) + lmat(k,1336)
         mat(k,1339) = mat(k,1339) + lmat(k,1339)
         mat(k,1346) = lmat(k,1346)
         mat(k,1349) = mat(k,1349) + lmat(k,1349)
         mat(k,1355) = mat(k,1355) + lmat(k,1355)
         mat(k,1371) = mat(k,1371) + lmat(k,1371)
         mat(k,1391) = mat(k,1391) + lmat(k,1391)
         mat(k,1406) = mat(k,1406) + lmat(k,1406)
         mat(k,1407) = mat(k,1407) + lmat(k,1407)
         mat(k,1410) = mat(k,1410) + lmat(k,1410)
         mat(k,1411) = mat(k,1411) + lmat(k,1411)
         mat(k,1414) = mat(k,1414) + lmat(k,1414)
         mat(k,1416) = mat(k,1416) + lmat(k,1416)
         mat(k,1418) = mat(k,1418) + lmat(k,1418)
         mat(k,1419) = mat(k,1419) + lmat(k,1419)
         mat(k,1420) = mat(k,1420) + lmat(k,1420)
         mat(k,1425) = lmat(k,1425)
         mat(k,1437) = mat(k,1437) + lmat(k,1437)
         mat(k,1453) = lmat(k,1453)
         mat(k,1470) = mat(k,1470) + lmat(k,1470)
         mat(k,1484) = mat(k,1484) + lmat(k,1484)
         mat(k,1494) = mat(k,1494) + lmat(k,1494)
         mat(k,1508) = lmat(k,1508)
         mat(k,1510) = mat(k,1510) + lmat(k,1510)
         mat(k,1514) = mat(k,1514) + lmat(k,1514)
         mat(k,1516) = mat(k,1516) + lmat(k,1516)
         mat(k,1519) = lmat(k,1519)
         mat(k,1535) = mat(k,1535) + lmat(k,1535)
         mat(k,1566) = mat(k,1566) + lmat(k,1566)
         mat(k,1587) = mat(k,1587) + lmat(k,1587)
         mat(k,1588) = mat(k,1588) + lmat(k,1588)
         mat(k,1595) = lmat(k,1595)
         mat(k,1600) = mat(k,1600) + lmat(k,1600)
         mat(k,1606) = mat(k,1606) + lmat(k,1606)
         mat(k,1611) = mat(k,1611) + lmat(k,1611)
         mat(k,1615) = mat(k,1615) + lmat(k,1615)
         mat(k,1628) = mat(k,1628) + lmat(k,1628)
         mat(k,1637) = mat(k,1637) + lmat(k,1637)
         mat(k,1640) = lmat(k,1640)
         mat(k,1642) = mat(k,1642) + lmat(k,1642)
         mat(k,1649) = mat(k,1649) + lmat(k,1649)
         mat(k,1658) = lmat(k,1658)
         mat(k,1659) = lmat(k,1659)
         mat(k,1660) = mat(k,1660) + lmat(k,1660)
         mat(k,1661) = mat(k,1661) + lmat(k,1661)
         mat(k,1662) = mat(k,1662) + lmat(k,1662)
         mat(k,1669) = mat(k,1669) + lmat(k,1669)
         mat(k,1670) = mat(k,1670) + lmat(k,1670)
         mat(k,1671) = mat(k,1671) + lmat(k,1671)
         mat(k,1673) = lmat(k,1673)
         mat(k,1676) = mat(k,1676) + lmat(k,1676)
         mat(k,1678) = mat(k,1678) + lmat(k,1678)
         mat(k,1679) = mat(k,1679) + lmat(k,1679)
         mat(k,1683) = mat(k,1683) + lmat(k,1683)
         mat(k,1690) = mat(k,1690) + lmat(k,1690)
         mat(k,1698) = lmat(k,1698)
         mat(k,1737) = mat(k,1737) + lmat(k,1737)
         mat(k,1762) = mat(k,1762) + lmat(k,1762)
         mat(k,1764) = mat(k,1764) + lmat(k,1764)
         mat(k,1765) = lmat(k,1765)
         mat(k,1772) = mat(k,1772) + lmat(k,1772)
         mat(k,1773) = mat(k,1773) + lmat(k,1773)
         mat(k,1780) = mat(k,1780) + lmat(k,1780)
         mat(k,1787) = mat(k,1787) + lmat(k,1787)
         mat(k,1788) = mat(k,1788) + lmat(k,1788)
         mat(k,1793) = mat(k,1793) + lmat(k,1793)
         mat(k,1799) = mat(k,1799) + lmat(k,1799)
         mat(k,1806) = lmat(k,1806)
         mat(k,1809) = mat(k,1809) + lmat(k,1809)
         mat(k,1817) = mat(k,1817) + lmat(k,1817)
         mat(k,1822) = mat(k,1822) + lmat(k,1822)
         mat(k,1826) = mat(k,1826) + lmat(k,1826)
         mat(k,1841) = mat(k,1841) + lmat(k,1841)
         mat(k,1846) = mat(k,1846) + lmat(k,1846)
         mat(k,1849) = mat(k,1849) + lmat(k,1849)
         mat(k,1886) = mat(k,1886) + lmat(k,1886)
         mat(k,1895) = mat(k,1895) + lmat(k,1895)
         mat(k,1954) = mat(k,1954) + lmat(k,1954)
         mat(k,1959) = mat(k,1959) + lmat(k,1959)
         mat(k,1960) = mat(k,1960) + lmat(k,1960)
         mat(k,1961) = mat(k,1961) + lmat(k,1961)
         mat(k,1966) = mat(k,1966) + lmat(k,1966)
         mat(k,1967) = mat(k,1967) + lmat(k,1967)
         mat(k,2018) = mat(k,2018) + lmat(k,2018)
         mat(k,2019) = mat(k,2019) + lmat(k,2019)
         mat(k,2020) = mat(k,2020) + lmat(k,2020)
         mat(k,2024) = mat(k,2024) + lmat(k,2024)
         mat(k,2025) = mat(k,2025) + lmat(k,2025)
         mat(k,2078) = mat(k,2078) + lmat(k,2078)
         mat(k,2080) = lmat(k,2080)
         mat(k,2086) = mat(k,2086) + lmat(k,2086)
         mat(k,2124) = mat(k,2124) + lmat(k,2124)
         mat(k,2130) = mat(k,2130) + lmat(k,2130)
         mat(k,2310) = mat(k,2310) + lmat(k,2310)
         mat(k,2326) = mat(k,2326) + lmat(k,2326)
         mat(k,2329) = lmat(k,2329)
         mat(k,2342) = mat(k,2342) + lmat(k,2342)
         mat(k,2344) = mat(k,2344) + lmat(k,2344)
         mat(k,2360) = mat(k,2360) + lmat(k,2360)
         mat(k,2405) = mat(k,2405) + lmat(k,2405)
         mat(k,2409) = mat(k,2409) + lmat(k,2409)
         mat(k,2415) = mat(k,2415) + lmat(k,2415)
         mat(k,2418) = mat(k,2418) + lmat(k,2418)
         mat(k,2440) = mat(k,2440) + lmat(k,2440)
         mat(k,2458) = lmat(k,2458)
         mat(k,2461) = lmat(k,2461)
         mat(k,2463) = mat(k,2463) + lmat(k,2463)
         mat(k,2467) = mat(k,2467) + lmat(k,2467)
         mat(k,2485) = mat(k,2485) + lmat(k,2485)
         mat(k,2488) = lmat(k,2488)
         mat(k,2515) = mat(k,2515) + lmat(k,2515)
         mat(k,2579) = mat(k,2579) + lmat(k,2579)
         mat(k,2611) = mat(k,2611) + lmat(k,2611)
         mat(k,2613) = mat(k,2613) + lmat(k,2613)
         mat(k,2615) = mat(k,2615) + lmat(k,2615)
         mat(k,2641) = mat(k,2641) + lmat(k,2641)
         mat(k,2644) = mat(k,2644) + lmat(k,2644)
         mat(k,2647) = mat(k,2647) + lmat(k,2647)
         mat(k,2675) = mat(k,2675) + lmat(k,2675)
         mat(k,2792) = mat(k,2792) + lmat(k,2792)
         mat(k,2795) = mat(k,2795) + lmat(k,2795)
         mat(k,2823) = mat(k,2823) + lmat(k,2823)
         mat(k,2847) = mat(k,2847) + lmat(k,2847)
         mat(k,2848) = mat(k,2848) + lmat(k,2848)
         mat(k,2854) = mat(k,2854) + lmat(k,2854)
         mat(k,2863) = lmat(k,2863)
         mat(k,2871) = mat(k,2871) + lmat(k,2871)
         mat(k,2875) = mat(k,2875) + lmat(k,2875)
         mat(k,2878) = lmat(k,2878)
         mat(k,2880) = lmat(k,2880)
         mat(k,2888) = mat(k,2888) + lmat(k,2888)
         mat(k, 249) = 0._r8
         mat(k, 250) = 0._r8
         mat(k, 350) = 0._r8
         mat(k, 389) = 0._r8
         mat(k, 411) = 0._r8
         mat(k, 516) = 0._r8
         mat(k, 519) = 0._r8
         mat(k, 538) = 0._r8
         mat(k, 577) = 0._r8
         mat(k, 581) = 0._r8
         mat(k, 607) = 0._r8
         mat(k, 692) = 0._r8
         mat(k, 694) = 0._r8
         mat(k, 746) = 0._r8
         mat(k, 747) = 0._r8
         mat(k, 757) = 0._r8
         mat(k, 758) = 0._r8
         mat(k, 762) = 0._r8
         mat(k, 768) = 0._r8
         mat(k, 769) = 0._r8
         mat(k, 772) = 0._r8
         mat(k, 812) = 0._r8
         mat(k, 814) = 0._r8
         mat(k, 815) = 0._r8
         mat(k, 817) = 0._r8
         mat(k, 819) = 0._r8
         mat(k, 832) = 0._r8
         mat(k, 834) = 0._r8
         mat(k, 835) = 0._r8
         mat(k, 837) = 0._r8
         mat(k, 841) = 0._r8
         mat(k, 856) = 0._r8
         mat(k, 858) = 0._r8
         mat(k, 859) = 0._r8
         mat(k, 861) = 0._r8
         mat(k, 863) = 0._r8
         mat(k, 866) = 0._r8
         mat(k, 879) = 0._r8
         mat(k, 880) = 0._r8
         mat(k, 883) = 0._r8
         mat(k, 911) = 0._r8
         mat(k, 915) = 0._r8
         mat(k, 918) = 0._r8
         mat(k, 923) = 0._r8
         mat(k, 924) = 0._r8
         mat(k, 925) = 0._r8
         mat(k, 937) = 0._r8
         mat(k, 942) = 0._r8
         mat(k, 943) = 0._r8
         mat(k, 944) = 0._r8
         mat(k, 946) = 0._r8
         mat(k, 956) = 0._r8
         mat(k, 959) = 0._r8
         mat(k, 966) = 0._r8
         mat(k,1006) = 0._r8
         mat(k,1041) = 0._r8
         mat(k,1044) = 0._r8
         mat(k,1055) = 0._r8
         mat(k,1056) = 0._r8
         mat(k,1064) = 0._r8
         mat(k,1072) = 0._r8
         mat(k,1086) = 0._r8
         mat(k,1091) = 0._r8
         mat(k,1092) = 0._r8
         mat(k,1094) = 0._r8
         mat(k,1110) = 0._r8
         mat(k,1111) = 0._r8
         mat(k,1119) = 0._r8
         mat(k,1127) = 0._r8
         mat(k,1131) = 0._r8
         mat(k,1132) = 0._r8
         mat(k,1136) = 0._r8
         mat(k,1137) = 0._r8
         mat(k,1138) = 0._r8
         mat(k,1141) = 0._r8
         mat(k,1159) = 0._r8
         mat(k,1161) = 0._r8
         mat(k,1195) = 0._r8
         mat(k,1198) = 0._r8
         mat(k,1201) = 0._r8
         mat(k,1202) = 0._r8
         mat(k,1203) = 0._r8
         mat(k,1204) = 0._r8
         mat(k,1205) = 0._r8
         mat(k,1207) = 0._r8
         mat(k,1209) = 0._r8
         mat(k,1217) = 0._r8
         mat(k,1218) = 0._r8
         mat(k,1219) = 0._r8
         mat(k,1224) = 0._r8
         mat(k,1226) = 0._r8
         mat(k,1234) = 0._r8
         mat(k,1238) = 0._r8
         mat(k,1259) = 0._r8
         mat(k,1260) = 0._r8
         mat(k,1261) = 0._r8
         mat(k,1265) = 0._r8
         mat(k,1267) = 0._r8
         mat(k,1268) = 0._r8
         mat(k,1273) = 0._r8
         mat(k,1302) = 0._r8
         mat(k,1303) = 0._r8
         mat(k,1308) = 0._r8
         mat(k,1310) = 0._r8
         mat(k,1312) = 0._r8
         mat(k,1320) = 0._r8
         mat(k,1322) = 0._r8
         mat(k,1323) = 0._r8
         mat(k,1324) = 0._r8
         mat(k,1330) = 0._r8
         mat(k,1348) = 0._r8
         mat(k,1350) = 0._r8
         mat(k,1364) = 0._r8
         mat(k,1379) = 0._r8
         mat(k,1384) = 0._r8
         mat(k,1386) = 0._r8
         mat(k,1388) = 0._r8
         mat(k,1389) = 0._r8
         mat(k,1390) = 0._r8
         mat(k,1392) = 0._r8
         mat(k,1393) = 0._r8
         mat(k,1394) = 0._r8
         mat(k,1396) = 0._r8
         mat(k,1401) = 0._r8
         mat(k,1403) = 0._r8
         mat(k,1412) = 0._r8
         mat(k,1421) = 0._r8
         mat(k,1430) = 0._r8
         mat(k,1431) = 0._r8
         mat(k,1432) = 0._r8
         mat(k,1433) = 0._r8
         mat(k,1434) = 0._r8
         mat(k,1436) = 0._r8
         mat(k,1438) = 0._r8
         mat(k,1440) = 0._r8
         mat(k,1447) = 0._r8
         mat(k,1449) = 0._r8
         mat(k,1450) = 0._r8
         mat(k,1452) = 0._r8
         mat(k,1456) = 0._r8
         mat(k,1459) = 0._r8
         mat(k,1460) = 0._r8
         mat(k,1462) = 0._r8
         mat(k,1464) = 0._r8
         mat(k,1466) = 0._r8
         mat(k,1467) = 0._r8
         mat(k,1468) = 0._r8
         mat(k,1471) = 0._r8
         mat(k,1472) = 0._r8
         mat(k,1473) = 0._r8
         mat(k,1475) = 0._r8
         mat(k,1480) = 0._r8
         mat(k,1482) = 0._r8
         mat(k,1483) = 0._r8
         mat(k,1485) = 0._r8
         mat(k,1492) = 0._r8
         mat(k,1495) = 0._r8
         mat(k,1497) = 0._r8
         mat(k,1502) = 0._r8
         mat(k,1504) = 0._r8
         mat(k,1506) = 0._r8
         mat(k,1511) = 0._r8
         mat(k,1515) = 0._r8
         mat(k,1518) = 0._r8
         mat(k,1520) = 0._r8
         mat(k,1521) = 0._r8
         mat(k,1522) = 0._r8
         mat(k,1523) = 0._r8
         mat(k,1527) = 0._r8
         mat(k,1529) = 0._r8
         mat(k,1533) = 0._r8
         mat(k,1534) = 0._r8
         mat(k,1545) = 0._r8
         mat(k,1547) = 0._r8
         mat(k,1568) = 0._r8
         mat(k,1570) = 0._r8
         mat(k,1571) = 0._r8
         mat(k,1577) = 0._r8
         mat(k,1579) = 0._r8
         mat(k,1583) = 0._r8
         mat(k,1585) = 0._r8
         mat(k,1586) = 0._r8
         mat(k,1589) = 0._r8
         mat(k,1590) = 0._r8
         mat(k,1591) = 0._r8
         mat(k,1592) = 0._r8
         mat(k,1593) = 0._r8
         mat(k,1594) = 0._r8
         mat(k,1596) = 0._r8
         mat(k,1608) = 0._r8
         mat(k,1610) = 0._r8
         mat(k,1616) = 0._r8
         mat(k,1617) = 0._r8
         mat(k,1620) = 0._r8
         mat(k,1623) = 0._r8
         mat(k,1631) = 0._r8
         mat(k,1633) = 0._r8
         mat(k,1635) = 0._r8
         mat(k,1638) = 0._r8
         mat(k,1652) = 0._r8
         mat(k,1655) = 0._r8
         mat(k,1663) = 0._r8
         mat(k,1666) = 0._r8
         mat(k,1667) = 0._r8
         mat(k,1668) = 0._r8
         mat(k,1672) = 0._r8
         mat(k,1682) = 0._r8
         mat(k,1684) = 0._r8
         mat(k,1685) = 0._r8
         mat(k,1687) = 0._r8
         mat(k,1688) = 0._r8
         mat(k,1689) = 0._r8
         mat(k,1691) = 0._r8
         mat(k,1692) = 0._r8
         mat(k,1694) = 0._r8
         mat(k,1695) = 0._r8
         mat(k,1697) = 0._r8
         mat(k,1708) = 0._r8
         mat(k,1736) = 0._r8
         mat(k,1739) = 0._r8
         mat(k,1740) = 0._r8
         mat(k,1741) = 0._r8
         mat(k,1742) = 0._r8
         mat(k,1745) = 0._r8
         mat(k,1747) = 0._r8
         mat(k,1748) = 0._r8
         mat(k,1749) = 0._r8
         mat(k,1750) = 0._r8
         mat(k,1754) = 0._r8
         mat(k,1768) = 0._r8
         mat(k,1770) = 0._r8
         mat(k,1774) = 0._r8
         mat(k,1776) = 0._r8
         mat(k,1777) = 0._r8
         mat(k,1781) = 0._r8
         mat(k,1782) = 0._r8
         mat(k,1784) = 0._r8
         mat(k,1785) = 0._r8
         mat(k,1786) = 0._r8
         mat(k,1792) = 0._r8
         mat(k,1794) = 0._r8
         mat(k,1796) = 0._r8
         mat(k,1798) = 0._r8
         mat(k,1800) = 0._r8
         mat(k,1802) = 0._r8
         mat(k,1803) = 0._r8
         mat(k,1805) = 0._r8
         mat(k,1807) = 0._r8
         mat(k,1814) = 0._r8
         mat(k,1815) = 0._r8
         mat(k,1816) = 0._r8
         mat(k,1819) = 0._r8
         mat(k,1820) = 0._r8
         mat(k,1821) = 0._r8
         mat(k,1823) = 0._r8
         mat(k,1824) = 0._r8
         mat(k,1827) = 0._r8
         mat(k,1828) = 0._r8
         mat(k,1829) = 0._r8
         mat(k,1830) = 0._r8
         mat(k,1843) = 0._r8
         mat(k,1844) = 0._r8
         mat(k,1850) = 0._r8
         mat(k,1851) = 0._r8
         mat(k,1852) = 0._r8
         mat(k,1853) = 0._r8
         mat(k,1880) = 0._r8
         mat(k,1883) = 0._r8
         mat(k,1884) = 0._r8
         mat(k,1887) = 0._r8
         mat(k,1888) = 0._r8
         mat(k,1894) = 0._r8
         mat(k,1897) = 0._r8
         mat(k,1899) = 0._r8
         mat(k,1909) = 0._r8
         mat(k,1910) = 0._r8
         mat(k,1915) = 0._r8
         mat(k,1918) = 0._r8
         mat(k,1920) = 0._r8
         mat(k,1927) = 0._r8
         mat(k,1934) = 0._r8
         mat(k,1937) = 0._r8
         mat(k,1948) = 0._r8
         mat(k,1950) = 0._r8
         mat(k,1951) = 0._r8
         mat(k,1952) = 0._r8
         mat(k,1953) = 0._r8
         mat(k,1956) = 0._r8
         mat(k,1957) = 0._r8
         mat(k,1958) = 0._r8
         mat(k,1964) = 0._r8
         mat(k,1965) = 0._r8
         mat(k,1970) = 0._r8
         mat(k,1973) = 0._r8
         mat(k,1974) = 0._r8
         mat(k,1975) = 0._r8
         mat(k,1980) = 0._r8
         mat(k,1982) = 0._r8
         mat(k,1985) = 0._r8
         mat(k,1986) = 0._r8
         mat(k,1991) = 0._r8
         mat(k,1999) = 0._r8
         mat(k,2001) = 0._r8
         mat(k,2002) = 0._r8
         mat(k,2003) = 0._r8
         mat(k,2006) = 0._r8
         mat(k,2007) = 0._r8
         mat(k,2009) = 0._r8
         mat(k,2010) = 0._r8
         mat(k,2011) = 0._r8
         mat(k,2013) = 0._r8
         mat(k,2014) = 0._r8
         mat(k,2015) = 0._r8
         mat(k,2016) = 0._r8
         mat(k,2021) = 0._r8
         mat(k,2023) = 0._r8
         mat(k,2033) = 0._r8
         mat(k,2081) = 0._r8
         mat(k,2114) = 0._r8
         mat(k,2115) = 0._r8
         mat(k,2118) = 0._r8
         mat(k,2119) = 0._r8
         mat(k,2120) = 0._r8
         mat(k,2121) = 0._r8
         mat(k,2128) = 0._r8
         mat(k,2129) = 0._r8
         mat(k,2138) = 0._r8
         mat(k,2212) = 0._r8
         mat(k,2232) = 0._r8
         mat(k,2244) = 0._r8
         mat(k,2248) = 0._r8
         mat(k,2256) = 0._r8
         mat(k,2257) = 0._r8
         mat(k,2285) = 0._r8
         mat(k,2306) = 0._r8
         mat(k,2325) = 0._r8
         mat(k,2327) = 0._r8
         mat(k,2330) = 0._r8
         mat(k,2331) = 0._r8
         mat(k,2332) = 0._r8
         mat(k,2333) = 0._r8
         mat(k,2334) = 0._r8
         mat(k,2335) = 0._r8
         mat(k,2337) = 0._r8
         mat(k,2339) = 0._r8
         mat(k,2340) = 0._r8
         mat(k,2343) = 0._r8
         mat(k,2348) = 0._r8
         mat(k,2349) = 0._r8
         mat(k,2350) = 0._r8
         mat(k,2353) = 0._r8
         mat(k,2371) = 0._r8
         mat(k,2377) = 0._r8
         mat(k,2378) = 0._r8
         mat(k,2379) = 0._r8
         mat(k,2383) = 0._r8
         mat(k,2388) = 0._r8
         mat(k,2389) = 0._r8
         mat(k,2390) = 0._r8
         mat(k,2392) = 0._r8
         mat(k,2395) = 0._r8
         mat(k,2396) = 0._r8
         mat(k,2397) = 0._r8
         mat(k,2399) = 0._r8
         mat(k,2406) = 0._r8
         mat(k,2407) = 0._r8
         mat(k,2408) = 0._r8
         mat(k,2417) = 0._r8
         mat(k,2426) = 0._r8
         mat(k,2428) = 0._r8
         mat(k,2429) = 0._r8
         mat(k,2431) = 0._r8
         mat(k,2432) = 0._r8
         mat(k,2433) = 0._r8
         mat(k,2434) = 0._r8
         mat(k,2435) = 0._r8
         mat(k,2436) = 0._r8
         mat(k,2438) = 0._r8
         mat(k,2441) = 0._r8
         mat(k,2443) = 0._r8
         mat(k,2444) = 0._r8
         mat(k,2445) = 0._r8
         mat(k,2446) = 0._r8
         mat(k,2448) = 0._r8
         mat(k,2449) = 0._r8
         mat(k,2453) = 0._r8
         mat(k,2454) = 0._r8
         mat(k,2455) = 0._r8
         mat(k,2456) = 0._r8
         mat(k,2457) = 0._r8
         mat(k,2459) = 0._r8
         mat(k,2462) = 0._r8
         mat(k,2464) = 0._r8
         mat(k,2465) = 0._r8
         mat(k,2466) = 0._r8
         mat(k,2468) = 0._r8
         mat(k,2469) = 0._r8
         mat(k,2470) = 0._r8
         mat(k,2471) = 0._r8
         mat(k,2472) = 0._r8
         mat(k,2473) = 0._r8
         mat(k,2474) = 0._r8
         mat(k,2475) = 0._r8
         mat(k,2484) = 0._r8
         mat(k,2487) = 0._r8
         mat(k,2492) = 0._r8
         mat(k,2494) = 0._r8
         mat(k,2499) = 0._r8
         mat(k,2501) = 0._r8
         mat(k,2503) = 0._r8
         mat(k,2506) = 0._r8
         mat(k,2514) = 0._r8
         mat(k,2523) = 0._r8
         mat(k,2542) = 0._r8
         mat(k,2545) = 0._r8
         mat(k,2548) = 0._r8
         mat(k,2549) = 0._r8
         mat(k,2553) = 0._r8
         mat(k,2555) = 0._r8
         mat(k,2556) = 0._r8
         mat(k,2557) = 0._r8
         mat(k,2558) = 0._r8
         mat(k,2559) = 0._r8
         mat(k,2561) = 0._r8
         mat(k,2563) = 0._r8
         mat(k,2566) = 0._r8
         mat(k,2569) = 0._r8
         mat(k,2572) = 0._r8
         mat(k,2577) = 0._r8
         mat(k,2578) = 0._r8
         mat(k,2580) = 0._r8
         mat(k,2581) = 0._r8
         mat(k,2586) = 0._r8
         mat(k,2589) = 0._r8
         mat(k,2596) = 0._r8
         mat(k,2597) = 0._r8
         mat(k,2600) = 0._r8
         mat(k,2601) = 0._r8
         mat(k,2602) = 0._r8
         mat(k,2607) = 0._r8
         mat(k,2609) = 0._r8
         mat(k,2610) = 0._r8
         mat(k,2619) = 0._r8
         mat(k,2629) = 0._r8
         mat(k,2630) = 0._r8
         mat(k,2631) = 0._r8
         mat(k,2632) = 0._r8
         mat(k,2633) = 0._r8
         mat(k,2637) = 0._r8
         mat(k,2638) = 0._r8
         mat(k,2639) = 0._r8
         mat(k,2640) = 0._r8
         mat(k,2649) = 0._r8
         mat(k,2652) = 0._r8
         mat(k,2653) = 0._r8
         mat(k,2657) = 0._r8
         mat(k,2659) = 0._r8
         mat(k,2660) = 0._r8
         mat(k,2661) = 0._r8
         mat(k,2662) = 0._r8
         mat(k,2666) = 0._r8
         mat(k,2667) = 0._r8
         mat(k,2669) = 0._r8
         mat(k,2670) = 0._r8
         mat(k,2671) = 0._r8
         mat(k,2672) = 0._r8
         mat(k,2678) = 0._r8
         mat(k,2679) = 0._r8
         mat(k,2711) = 0._r8
         mat(k,2712) = 0._r8
         mat(k,2713) = 0._r8
         mat(k,2722) = 0._r8
         mat(k,2737) = 0._r8
         mat(k,2745) = 0._r8
         mat(k,2746) = 0._r8
         mat(k,2748) = 0._r8
         mat(k,2752) = 0._r8
         mat(k,2754) = 0._r8
         mat(k,2759) = 0._r8
         mat(k,2764) = 0._r8
         mat(k,2770) = 0._r8
         mat(k,2771) = 0._r8
         mat(k,2778) = 0._r8
         mat(k,2786) = 0._r8
         mat(k,2798) = 0._r8
         mat(k,2801) = 0._r8
         mat(k,2803) = 0._r8
         mat(k,2805) = 0._r8
         mat(k,2806) = 0._r8
         mat(k,2807) = 0._r8
         mat(k,2808) = 0._r8
         mat(k,2811) = 0._r8
         mat(k,2812) = 0._r8
         mat(k,2815) = 0._r8
         mat(k,2816) = 0._r8
         mat(k,2817) = 0._r8
         mat(k,2818) = 0._r8
         mat(k,2824) = 0._r8
         mat(k,2825) = 0._r8
         mat(k,2838) = 0._r8
         mat(k,2839) = 0._r8
         mat(k,2844) = 0._r8
         mat(k,2845) = 0._r8
         mat(k,2846) = 0._r8
         mat(k,2855) = 0._r8
         mat(k,2862) = 0._r8
         mat(k,2864) = 0._r8
         mat(k,2865) = 0._r8
         mat(k,2866) = 0._r8
         mat(k,2869) = 0._r8
         mat(k,2870) = 0._r8
         mat(k,2872) = 0._r8
         mat(k,2873) = 0._r8
         mat(k,2874) = 0._r8
         mat(k,2876) = 0._r8
         mat(k,2877) = 0._r8
         mat(k,2879) = 0._r8
         mat(k,2881) = 0._r8
         mat(k,2882) = 0._r8
         mat(k,2883) = 0._r8
         mat(k,2884) = 0._r8
         mat(k,2885) = 0._r8
         mat(k,2886) = 0._r8
         mat(k,2887) = 0._r8
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
         mat(k, 62) = mat(k, 62) - dti(k)
         mat(k, 68) = mat(k, 68) - dti(k)
         mat(k, 74) = mat(k, 74) - dti(k)
         mat(k, 80) = mat(k, 80) - dti(k)
         mat(k, 86) = mat(k, 86) - dti(k)
         mat(k, 88) = mat(k, 88) - dti(k)
         mat(k, 94) = mat(k, 94) - dti(k)
         mat(k, 100) = mat(k, 100) - dti(k)
         mat(k, 106) = mat(k, 106) - dti(k)
         mat(k, 107) = mat(k, 107) - dti(k)
         mat(k, 109) = mat(k, 109) - dti(k)
         mat(k, 112) = mat(k, 112) - dti(k)
         mat(k, 115) = mat(k, 115) - dti(k)
         mat(k, 118) = mat(k, 118) - dti(k)
         mat(k, 121) = mat(k, 121) - dti(k)
         mat(k, 125) = mat(k, 125) - dti(k)
         mat(k, 129) = mat(k, 129) - dti(k)
         mat(k, 133) = mat(k, 133) - dti(k)
         mat(k, 137) = mat(k, 137) - dti(k)
         mat(k, 141) = mat(k, 141) - dti(k)
         mat(k, 145) = mat(k, 145) - dti(k)
         mat(k, 149) = mat(k, 149) - dti(k)
         mat(k, 153) = mat(k, 153) - dti(k)
         mat(k, 156) = mat(k, 156) - dti(k)
         mat(k, 159) = mat(k, 159) - dti(k)
         mat(k, 162) = mat(k, 162) - dti(k)
         mat(k, 165) = mat(k, 165) - dti(k)
         mat(k, 168) = mat(k, 168) - dti(k)
         mat(k, 173) = mat(k, 173) - dti(k)
         mat(k, 178) = mat(k, 178) - dti(k)
         mat(k, 183) = mat(k, 183) - dti(k)
         mat(k, 189) = mat(k, 189) - dti(k)
         mat(k, 195) = mat(k, 195) - dti(k)
         mat(k, 199) = mat(k, 199) - dti(k)
         mat(k, 204) = mat(k, 204) - dti(k)
         mat(k, 207) = mat(k, 207) - dti(k)
         mat(k, 209) = mat(k, 209) - dti(k)
         mat(k, 213) = mat(k, 213) - dti(k)
         mat(k, 217) = mat(k, 217) - dti(k)
         mat(k, 221) = mat(k, 221) - dti(k)
         mat(k, 224) = mat(k, 224) - dti(k)
         mat(k, 231) = mat(k, 231) - dti(k)
         mat(k, 236) = mat(k, 236) - dti(k)
         mat(k, 240) = mat(k, 240) - dti(k)
         mat(k, 245) = mat(k, 245) - dti(k)
         mat(k, 253) = mat(k, 253) - dti(k)
         mat(k, 258) = mat(k, 258) - dti(k)
         mat(k, 263) = mat(k, 263) - dti(k)
         mat(k, 267) = mat(k, 267) - dti(k)
         mat(k, 272) = mat(k, 272) - dti(k)
         mat(k, 275) = mat(k, 275) - dti(k)
         mat(k, 280) = mat(k, 280) - dti(k)
         mat(k, 285) = mat(k, 285) - dti(k)
         mat(k, 288) = mat(k, 288) - dti(k)
         mat(k, 291) = mat(k, 291) - dti(k)
         mat(k, 294) = mat(k, 294) - dti(k)
         mat(k, 300) = mat(k, 300) - dti(k)
         mat(k, 304) = mat(k, 304) - dti(k)
         mat(k, 308) = mat(k, 308) - dti(k)
         mat(k, 313) = mat(k, 313) - dti(k)
         mat(k, 317) = mat(k, 317) - dti(k)
         mat(k, 321) = mat(k, 321) - dti(k)
         mat(k, 327) = mat(k, 327) - dti(k)
         mat(k, 330) = mat(k, 330) - dti(k)
         mat(k, 336) = mat(k, 336) - dti(k)
         mat(k, 342) = mat(k, 342) - dti(k)
         mat(k, 349) = mat(k, 349) - dti(k)
         mat(k, 355) = mat(k, 355) - dti(k)
         mat(k, 361) = mat(k, 361) - dti(k)
         mat(k, 364) = mat(k, 364) - dti(k)
         mat(k, 369) = mat(k, 369) - dti(k)
         mat(k, 374) = mat(k, 374) - dti(k)
         mat(k, 379) = mat(k, 379) - dti(k)
         mat(k, 387) = mat(k, 387) - dti(k)
         mat(k, 392) = mat(k, 392) - dti(k)
         mat(k, 397) = mat(k, 397) - dti(k)
         mat(k, 402) = mat(k, 402) - dti(k)
         mat(k, 405) = mat(k, 405) - dti(k)
         mat(k, 409) = mat(k, 409) - dti(k)
         mat(k, 416) = mat(k, 416) - dti(k)
         mat(k, 424) = mat(k, 424) - dti(k)
         mat(k, 432) = mat(k, 432) - dti(k)
         mat(k, 435) = mat(k, 435) - dti(k)
         mat(k, 443) = mat(k, 443) - dti(k)
         mat(k, 451) = mat(k, 451) - dti(k)
         mat(k, 459) = mat(k, 459) - dti(k)
         mat(k, 465) = mat(k, 465) - dti(k)
         mat(k, 471) = mat(k, 471) - dti(k)
         mat(k, 477) = mat(k, 477) - dti(k)
         mat(k, 483) = mat(k, 483) - dti(k)
         mat(k, 489) = mat(k, 489) - dti(k)
         mat(k, 495) = mat(k, 495) - dti(k)
         mat(k, 501) = mat(k, 501) - dti(k)
         mat(k, 507) = mat(k, 507) - dti(k)
         mat(k, 515) = mat(k, 515) - dti(k)
         mat(k, 521) = mat(k, 521) - dti(k)
         mat(k, 527) = mat(k, 527) - dti(k)
         mat(k, 534) = mat(k, 534) - dti(k)
         mat(k, 540) = mat(k, 540) - dti(k)
         mat(k, 545) = mat(k, 545) - dti(k)
         mat(k, 550) = mat(k, 550) - dti(k)
         mat(k, 553) = mat(k, 553) - dti(k)
         mat(k, 556) = mat(k, 556) - dti(k)
         mat(k, 560) = mat(k, 560) - dti(k)
         mat(k, 567) = mat(k, 567) - dti(k)
         mat(k, 576) = mat(k, 576) - dti(k)
         mat(k, 583) = mat(k, 583) - dti(k)
         mat(k, 590) = mat(k, 590) - dti(k)
         mat(k, 597) = mat(k, 597) - dti(k)
         mat(k, 602) = mat(k, 602) - dti(k)
         mat(k, 609) = mat(k, 609) - dti(k)
         mat(k, 614) = mat(k, 614) - dti(k)
         mat(k, 618) = mat(k, 618) - dti(k)
         mat(k, 625) = mat(k, 625) - dti(k)
         mat(k, 630) = mat(k, 630) - dti(k)
         mat(k, 638) = mat(k, 638) - dti(k)
         mat(k, 646) = mat(k, 646) - dti(k)
         mat(k, 654) = mat(k, 654) - dti(k)
         mat(k, 662) = mat(k, 662) - dti(k)
         mat(k, 670) = mat(k, 670) - dti(k)
         mat(k, 679) = mat(k, 679) - dti(k)
         mat(k, 690) = mat(k, 690) - dti(k)
         mat(k, 699) = mat(k, 699) - dti(k)
         mat(k, 706) = mat(k, 706) - dti(k)
         mat(k, 710) = mat(k, 710) - dti(k)
         mat(k, 719) = mat(k, 719) - dti(k)
         mat(k, 728) = mat(k, 728) - dti(k)
         mat(k, 735) = mat(k, 735) - dti(k)
         mat(k, 743) = mat(k, 743) - dti(k)
         mat(k, 749) = mat(k, 749) - dti(k)
         mat(k, 756) = mat(k, 756) - dti(k)
         mat(k, 767) = mat(k, 767) - dti(k)
         mat(k, 778) = mat(k, 778) - dti(k)
         mat(k, 785) = mat(k, 785) - dti(k)
         mat(k, 795) = mat(k, 795) - dti(k)
         mat(k, 800) = mat(k, 800) - dti(k)
         mat(k, 811) = mat(k, 811) - dti(k)
         mat(k, 822) = mat(k, 822) - dti(k)
         mat(k, 833) = mat(k, 833) - dti(k)
         mat(k, 844) = mat(k, 844) - dti(k)
         mat(k, 857) = mat(k, 857) - dti(k)
         mat(k, 868) = mat(k, 868) - dti(k)
         mat(k, 878) = mat(k, 878) - dti(k)
         mat(k, 886) = mat(k, 886) - dti(k)
         mat(k, 897) = mat(k, 897) - dti(k)
         mat(k, 904) = mat(k, 904) - dti(k)
         mat(k, 909) = mat(k, 909) - dti(k)
         mat(k, 920) = mat(k, 920) - dti(k)
         mat(k, 928) = mat(k, 928) - dti(k)
         mat(k, 940) = mat(k, 940) - dti(k)
         mat(k, 950) = mat(k, 950) - dti(k)
         mat(k, 962) = mat(k, 962) - dti(k)
         mat(k, 975) = mat(k, 975) - dti(k)
         mat(k, 991) = mat(k, 991) - dti(k)
         mat(k,1000) = mat(k,1000) - dti(k)
         mat(k,1010) = mat(k,1010) - dti(k)
         mat(k,1019) = mat(k,1019) - dti(k)
         mat(k,1028) = mat(k,1028) - dti(k)
         mat(k,1034) = mat(k,1034) - dti(k)
         mat(k,1054) = mat(k,1054) - dti(k)
         mat(k,1074) = mat(k,1074) - dti(k)
         mat(k,1090) = mat(k,1090) - dti(k)
         mat(k,1109) = mat(k,1109) - dti(k)
         mat(k,1133) = mat(k,1133) - dti(k)
         mat(k,1145) = mat(k,1145) - dti(k)
         mat(k,1153) = mat(k,1153) - dti(k)
         mat(k,1163) = mat(k,1163) - dti(k)
         mat(k,1171) = mat(k,1171) - dti(k)
         mat(k,1177) = mat(k,1177) - dti(k)
         mat(k,1190) = mat(k,1190) - dti(k)
         mat(k,1200) = mat(k,1200) - dti(k)
         mat(k,1216) = mat(k,1216) - dti(k)
         mat(k,1229) = mat(k,1229) - dti(k)
         mat(k,1240) = mat(k,1240) - dti(k)
         mat(k,1256) = mat(k,1256) - dti(k)
         mat(k,1274) = mat(k,1274) - dti(k)
         mat(k,1283) = mat(k,1283) - dti(k)
         mat(k,1289) = mat(k,1289) - dti(k)
         mat(k,1301) = mat(k,1301) - dti(k)
         mat(k,1318) = mat(k,1318) - dti(k)
         mat(k,1331) = mat(k,1331) - dti(k)
         mat(k,1339) = mat(k,1339) - dti(k)
         mat(k,1355) = mat(k,1355) - dti(k)
         mat(k,1371) = mat(k,1371) - dti(k)
         mat(k,1391) = mat(k,1391) - dti(k)
         mat(k,1407) = mat(k,1407) - dti(k)
         mat(k,1419) = mat(k,1419) - dti(k)
         mat(k,1437) = mat(k,1437) - dti(k)
         mat(k,1470) = mat(k,1470) - dti(k)
         mat(k,1494) = mat(k,1494) - dti(k)
         mat(k,1514) = mat(k,1514) - dti(k)
         mat(k,1535) = mat(k,1535) - dti(k)
         mat(k,1566) = mat(k,1566) - dti(k)
         mat(k,1588) = mat(k,1588) - dti(k)
         mat(k,1600) = mat(k,1600) - dti(k)
         mat(k,1615) = mat(k,1615) - dti(k)
         mat(k,1628) = mat(k,1628) - dti(k)
         mat(k,1642) = mat(k,1642) - dti(k)
         mat(k,1661) = mat(k,1661) - dti(k)
         mat(k,1683) = mat(k,1683) - dti(k)
         mat(k,1737) = mat(k,1737) - dti(k)
         mat(k,1772) = mat(k,1772) - dti(k)
         mat(k,1793) = mat(k,1793) - dti(k)
         mat(k,1817) = mat(k,1817) - dti(k)
         mat(k,1841) = mat(k,1841) - dti(k)
         mat(k,1886) = mat(k,1886) - dti(k)
         mat(k,1959) = mat(k,1959) - dti(k)
         mat(k,2018) = mat(k,2018) - dti(k)
         mat(k,2124) = mat(k,2124) - dti(k)
         mat(k,2310) = mat(k,2310) - dti(k)
         mat(k,2342) = mat(k,2342) - dti(k)
         mat(k,2415) = mat(k,2415) - dti(k)
         mat(k,2440) = mat(k,2440) - dti(k)
         mat(k,2467) = mat(k,2467) - dti(k)
         mat(k,2515) = mat(k,2515) - dti(k)
         mat(k,2579) = mat(k,2579) - dti(k)
         mat(k,2613) = mat(k,2613) - dti(k)
         mat(k,2644) = mat(k,2644) - dti(k)
         mat(k,2675) = mat(k,2675) - dti(k)
         mat(k,2792) = mat(k,2792) - dti(k)
         mat(k,2823) = mat(k,2823) - dti(k)
         mat(k,2854) = mat(k,2854) - dti(k)
         mat(k,2888) = mat(k,2888) - dti(k)
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
      call nlnmat12( avec_len, mat, y, rxt )
      call nlnmat_finit( avec_len, mat, lmat, dti )
      end subroutine nlnmat
      end module mo_nln_matrix
