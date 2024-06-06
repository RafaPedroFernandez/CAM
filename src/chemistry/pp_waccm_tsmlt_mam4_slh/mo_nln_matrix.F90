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
         mat(k,711) = -(rxt(k,489)*y(k,259))
         mat(k,2122) = -rxt(k,489)*y(k,1)
         mat(k,2512) = rxt(k,492)*y(k,222)
         mat(k,1071) = rxt(k,492)*y(k,155)
         mat(k,745) = -(rxt(k,493)*y(k,259))
         mat(k,2125) = -rxt(k,493)*y(k,2)
         mat(k,1072) = rxt(k,490)*y(k,237)
         mat(k,2359) = rxt(k,490)*y(k,222)
         mat(k,1051) = -(rxt(k,572)*y(k,157) + rxt(k,573)*y(k,166) + rxt(k,574) &
                      *y(k,259))
         mat(k,2731) = -rxt(k,572)*y(k,8)
         mat(k,1983) = -rxt(k,573)*y(k,8)
         mat(k,2146) = -rxt(k,574)*y(k,8)
         mat(k,165) = -(rxt(k,531)*y(k,259))
         mat(k,2048) = -rxt(k,531)*y(k,9)
         mat(k,463) = -(rxt(k,534)*y(k,259))
         mat(k,2092) = -rxt(k,534)*y(k,10)
         mat(k,553) = rxt(k,532)*y(k,237)
         mat(k,2338) = rxt(k,532)*y(k,224)
         mat(k,166) = .120_r8*rxt(k,531)*y(k,259)
         mat(k,2049) = .120_r8*rxt(k,531)*y(k,9)
         mat(k,1049) = .100_r8*rxt(k,573)*y(k,166)
         mat(k,1022) = .100_r8*rxt(k,576)*y(k,166)
         mat(k,1972) = .100_r8*rxt(k,573)*y(k,8) + .100_r8*rxt(k,576)*y(k,141)
         mat(k,2498) = .500_r8*rxt(k,533)*y(k,224) + .200_r8*rxt(k,560)*y(k,266) &
                      + .060_r8*rxt(k,566)*y(k,269)
         mat(k,554) = .500_r8*rxt(k,533)*y(k,155)
         mat(k,809) = .200_r8*rxt(k,560)*y(k,155)
         mat(k,832) = .060_r8*rxt(k,566)*y(k,155)
         mat(k,2492) = .200_r8*rxt(k,560)*y(k,266) + .200_r8*rxt(k,566)*y(k,269)
         mat(k,808) = .200_r8*rxt(k,560)*y(k,155)
         mat(k,830) = .200_r8*rxt(k,566)*y(k,155)
         mat(k,2508) = .200_r8*rxt(k,560)*y(k,266) + .150_r8*rxt(k,566)*y(k,269)
         mat(k,810) = .200_r8*rxt(k,560)*y(k,155)
         mat(k,833) = .150_r8*rxt(k,566)*y(k,155)
         mat(k,2493) = .210_r8*rxt(k,566)*y(k,269)
         mat(k,831) = .210_r8*rxt(k,566)*y(k,155)
         mat(k,264) = -(rxt(k,494)*y(k,259))
         mat(k,2064) = -rxt(k,494)*y(k,17)
         mat(k,1048) = .050_r8*rxt(k,573)*y(k,166)
         mat(k,1021) = .050_r8*rxt(k,576)*y(k,166)
         mat(k,1971) = .050_r8*rxt(k,573)*y(k,8) + .050_r8*rxt(k,576)*y(k,141)
         mat(k,400) = -(rxt(k,460)*y(k,157) + rxt(k,461)*y(k,259))
         mat(k,2722) = -rxt(k,460)*y(k,18)
         mat(k,2084) = -rxt(k,461)*y(k,18)
         mat(k,2315) = -(rxt(k,283)*y(k,53) + rxt(k,284)*y(k,237) + rxt(k,285) &
                      *y(k,156) + rxt(k,286)*y(k,166) + rxt(k,293)*y(k,24) + rxt(k,322) &
                      *y(k,127))
         mat(k,2478) = -rxt(k,283)*y(k,19)
         mat(k,2418) = -rxt(k,284)*y(k,19)
         mat(k,1953) = -rxt(k,285)*y(k,19)
         mat(k,2022) = -rxt(k,286)*y(k,19)
         mat(k,963) = -rxt(k,293)*y(k,19)
         mat(k,2630) = -rxt(k,322)*y(k,19)
         mat(k,524) = rxt(k,282)*y(k,259)
         mat(k,2447) = 4.000_r8*rxt(k,287)*y(k,23) + (rxt(k,288)+rxt(k,289))*y(k,76) &
                      + rxt(k,595)*y(k,85) + rxt(k,312)*y(k,117) + (rxt(k,323) &
                       +rxt(k,324))*y(k,127) + rxt(k,292)*y(k,155) + rxt(k,297) &
                      *y(k,164) + rxt(k,606)*y(k,183) + rxt(k,298)*y(k,259)
         mat(k,144) = rxt(k,272)*y(k,255)
         mat(k,149) = rxt(k,302)*y(k,255)
         mat(k,544) = 2.000_r8*rxt(k,356)*y(k,72) + 2.000_r8*rxt(k,383)*y(k,255) &
                      + 2.000_r8*rxt(k,357)*y(k,259)
         mat(k,104) = rxt(k,358)*y(k,259)
         mat(k,685) = rxt(k,361)*y(k,72) + rxt(k,384)*y(k,255) + rxt(k,362)*y(k,259)
         mat(k,88) = 2.000_r8*rxt(k,368)*y(k,259)
         mat(k,462) = 3.000_r8*rxt(k,369)*y(k,72) + 3.000_r8*rxt(k,303)*y(k,255) &
                      + 3.000_r8*rxt(k,370)*y(k,259)
         mat(k,92) = rxt(k,371)*y(k,259)
         mat(k,2285) = 2.000_r8*rxt(k,356)*y(k,47) + rxt(k,361)*y(k,54) &
                      + 3.000_r8*rxt(k,369)*y(k,68)
         mat(k,2707) = (rxt(k,288)+rxt(k,289))*y(k,23)
         mat(k,1131) = rxt(k,595)*y(k,23)
         mat(k,96) = 2.000_r8*rxt(k,304)*y(k,255)
         mat(k,1587) = rxt(k,299)*y(k,164) + rxt(k,305)*y(k,255) + rxt(k,300)*y(k,259)
         mat(k,2228) = rxt(k,312)*y(k,23)
         mat(k,2630) = mat(k,2630) + (rxt(k,323)+rxt(k,324))*y(k,23)
         mat(k,2571) = rxt(k,292)*y(k,23)
         mat(k,2677) = rxt(k,297)*y(k,23) + rxt(k,299)*y(k,99)
         mat(k,1627) = rxt(k,606)*y(k,23)
         mat(k,1871) = rxt(k,272)*y(k,40) + rxt(k,302)*y(k,41) + 2.000_r8*rxt(k,383) &
                      *y(k,47) + rxt(k,384)*y(k,54) + 3.000_r8*rxt(k,303)*y(k,68) &
                      + 2.000_r8*rxt(k,304)*y(k,96) + rxt(k,305)*y(k,99)
         mat(k,2198) = rxt(k,282)*y(k,20) + rxt(k,298)*y(k,23) + 2.000_r8*rxt(k,357) &
                      *y(k,47) + rxt(k,358)*y(k,48) + rxt(k,362)*y(k,54) &
                      + 2.000_r8*rxt(k,368)*y(k,67) + 3.000_r8*rxt(k,370)*y(k,68) &
                      + rxt(k,371)*y(k,69) + rxt(k,300)*y(k,99)
         mat(k,521) = -(rxt(k,282)*y(k,259))
         mat(k,2099) = -rxt(k,282)*y(k,20)
         mat(k,2297) = rxt(k,293)*y(k,24)
         mat(k,956) = rxt(k,293)*y(k,19)
         mat(k,1576) = (rxt(k,623)+rxt(k,683)+rxt(k,696)+rxt(k,705))*y(k,110)
         mat(k,1678) = (rxt(k,623)+rxt(k,683)+rxt(k,696)+rxt(k,705))*y(k,99)
         mat(k,2430) = rxt(k,290)*y(k,76)
         mat(k,957) = rxt(k,294)*y(k,72)
         mat(k,2252) = rxt(k,294)*y(k,24)
         mat(k,2692) = rxt(k,290)*y(k,23)
         mat(k,1577) = (rxt(k,622)+rxt(k,685)+rxt(k,693)+rxt(k,702))*y(k,111)
         mat(k,1814) = (rxt(k,625)+rxt(k,682)+rxt(k,695)+rxt(k,704))*y(k,110)
         mat(k,1679) = (rxt(k,625)+rxt(k,682)+rxt(k,695)+rxt(k,704))*y(k,103)
         mat(k,1789) = (rxt(k,622)+rxt(k,685)+rxt(k,693)+rxt(k,702))*y(k,99)
         mat(k,2296) = rxt(k,285)*y(k,156)
         mat(k,1909) = rxt(k,285)*y(k,19)
         mat(k,2449) = -(4._r8*rxt(k,287)*y(k,23) + (rxt(k,288) + rxt(k,289) + rxt(k,290) &
                      ) * y(k,76) + rxt(k,291)*y(k,237) + rxt(k,292)*y(k,155) &
                      + rxt(k,295)*y(k,156) + rxt(k,297)*y(k,164) + rxt(k,298) &
                      *y(k,259) + rxt(k,312)*y(k,117) + (rxt(k,323) + rxt(k,324) &
                      ) * y(k,127) + rxt(k,595)*y(k,85) + rxt(k,606)*y(k,183))
         mat(k,2709) = -(rxt(k,288) + rxt(k,289) + rxt(k,290)) * y(k,23)
         mat(k,2420) = -rxt(k,291)*y(k,23)
         mat(k,2573) = -rxt(k,292)*y(k,23)
         mat(k,1955) = -rxt(k,295)*y(k,23)
         mat(k,2679) = -rxt(k,297)*y(k,23)
         mat(k,2200) = -rxt(k,298)*y(k,23)
         mat(k,2230) = -rxt(k,312)*y(k,23)
         mat(k,2632) = -(rxt(k,323) + rxt(k,324)) * y(k,23)
         mat(k,1133) = -rxt(k,595)*y(k,23)
         mat(k,1629) = -rxt(k,606)*y(k,23)
         mat(k,2317) = rxt(k,322)*y(k,127) + rxt(k,286)*y(k,166)
         mat(k,964) = rxt(k,296)*y(k,164)
         mat(k,1588) = rxt(k,306)*y(k,255)
         mat(k,1694) = rxt(k,301)*y(k,164)
         mat(k,2632) = mat(k,2632) + rxt(k,322)*y(k,19)
         mat(k,2679) = mat(k,2679) + rxt(k,296)*y(k,24) + rxt(k,301)*y(k,110)
         mat(k,2024) = rxt(k,286)*y(k,19)
         mat(k,1873) = rxt(k,306)*y(k,99)
         mat(k,958) = -(rxt(k,293)*y(k,19) + rxt(k,294)*y(k,72) + rxt(k,296)*y(k,164))
         mat(k,2299) = -rxt(k,293)*y(k,24)
         mat(k,2257) = -rxt(k,294)*y(k,24)
         mat(k,2653) = -rxt(k,296)*y(k,24)
         mat(k,2432) = rxt(k,295)*y(k,156)
         mat(k,1924) = rxt(k,295)*y(k,23)
         mat(k,267) = -(rxt(k,535)*y(k,259))
         mat(k,2065) = -rxt(k,535)*y(k,26)
         mat(k,2490) = rxt(k,538)*y(k,226)
         mat(k,481) = rxt(k,538)*y(k,155)
         mat(k,360) = -(rxt(k,537)*y(k,259))
         mat(k,2078) = -rxt(k,537)*y(k,27)
         mat(k,482) = rxt(k,536)*y(k,237)
         mat(k,2332) = rxt(k,536)*y(k,226)
         mat(k,195) = -(rxt(k,352)*y(k,72) + rxt(k,353)*y(k,259))
         mat(k,2240) = -rxt(k,352)*y(k,28)
         mat(k,2052) = -rxt(k,353)*y(k,28)
         mat(k,314) = -(rxt(k,409)*y(k,72) + rxt(k,410)*y(k,259))
         mat(k,2242) = -rxt(k,409)*y(k,29)
         mat(k,2072) = -rxt(k,410)*y(k,29)
         mat(k,627) = -(rxt(k,411)*y(k,72) + rxt(k,412)*y(k,166) + rxt(k,437)*y(k,259))
         mat(k,2253) = -rxt(k,411)*y(k,30)
         mat(k,1974) = -rxt(k,412)*y(k,30)
         mat(k,2112) = -rxt(k,437)*y(k,30)
         mat(k,273) = -(rxt(k,354)*y(k,72) + rxt(k,355)*y(k,259))
         mat(k,2241) = -rxt(k,354)*y(k,31)
         mat(k,2067) = -rxt(k,355)*y(k,31)
         mat(k,287) = -(rxt(k,417)*y(k,259))
         mat(k,2069) = -rxt(k,417)*y(k,32)
         mat(k,990) = .800_r8*rxt(k,413)*y(k,227) + .200_r8*rxt(k,414)*y(k,231)
         mat(k,1700) = .200_r8*rxt(k,414)*y(k,227)
         mat(k,365) = -(rxt(k,418)*y(k,259))
         mat(k,2079) = -rxt(k,418)*y(k,33)
         mat(k,991) = rxt(k,415)*y(k,237)
         mat(k,2333) = rxt(k,415)*y(k,227)
         mat(k,320) = -(rxt(k,419)*y(k,72) + rxt(k,420)*y(k,259))
         mat(k,2243) = -rxt(k,419)*y(k,34)
         mat(k,2073) = -rxt(k,420)*y(k,34)
         mat(k,1185) = -(rxt(k,440)*y(k,157) + rxt(k,441)*y(k,166) + rxt(k,458) &
                      *y(k,259))
         mat(k,2740) = -rxt(k,440)*y(k,35)
         mat(k,1989) = -rxt(k,441)*y(k,35)
         mat(k,2157) = -rxt(k,458)*y(k,35)
         mat(k,942) = .130_r8*rxt(k,518)*y(k,166)
         mat(k,1989) = mat(k,1989) + .130_r8*rxt(k,518)*y(k,129)
         mat(k,451) = -(rxt(k,445)*y(k,259))
         mat(k,2090) = -rxt(k,445)*y(k,36)
         mat(k,888) = rxt(k,443)*y(k,237)
         mat(k,2337) = rxt(k,443)*y(k,228)
         mat(k,105) = -(rxt(k,446)*y(k,259))
         mat(k,2045) = -rxt(k,446)*y(k,37)
         mat(k,291) = -(rxt(k,541)*y(k,259))
         mat(k,2070) = -rxt(k,541)*y(k,38)
         mat(k,702) = rxt(k,539)*y(k,237)
         mat(k,2328) = rxt(k,539)*y(k,229)
         mat(k,82) = -(rxt(k,271)*y(k,255))
         mat(k,1834) = -rxt(k,271)*y(k,39)
         mat(k,140) = -(rxt(k,272)*y(k,255))
         mat(k,1839) = -rxt(k,272)*y(k,40)
         mat(k,145) = -(rxt(k,302)*y(k,255))
         mat(k,1840) = -rxt(k,302)*y(k,41)
         mat(k,109) = -(rxt(k,273)*y(k,255))
         mat(k,1836) = -rxt(k,273)*y(k,42)
         mat(k,150) = -(rxt(k,274)*y(k,255))
         mat(k,1841) = -rxt(k,274)*y(k,43)
         mat(k,113) = -(rxt(k,275)*y(k,255))
         mat(k,1837) = -rxt(k,275)*y(k,44)
         mat(k,155) = -(rxt(k,276)*y(k,255))
         mat(k,1842) = -rxt(k,276)*y(k,45)
         mat(k,117) = -(rxt(k,277)*y(k,255))
         mat(k,1838) = -rxt(k,277)*y(k,46)
         mat(k,539) = -(rxt(k,356)*y(k,72) + rxt(k,357)*y(k,259) + rxt(k,383)*y(k,255))
         mat(k,2250) = -rxt(k,356)*y(k,47)
         mat(k,2102) = -rxt(k,357)*y(k,47)
         mat(k,1852) = -rxt(k,383)*y(k,47)
         mat(k,101) = -(rxt(k,358)*y(k,259))
         mat(k,2044) = -rxt(k,358)*y(k,48)
         mat(k,326) = -(rxt(k,359)*y(k,72) + rxt(k,360)*y(k,259))
         mat(k,2244) = -rxt(k,359)*y(k,49)
         mat(k,2074) = -rxt(k,360)*y(k,49)
         mat(k,2481) = -(rxt(k,244)*y(k,72) + rxt(k,283)*y(k,19) + rxt(k,388)*y(k,237) &
                      + rxt(k,389)*y(k,157) + rxt(k,390)*y(k,164) + rxt(k,391) &
                      *y(k,259))
         mat(k,2288) = -rxt(k,244)*y(k,53)
         mat(k,2318) = -rxt(k,283)*y(k,53)
         mat(k,2421) = -rxt(k,388)*y(k,53)
         mat(k,2782) = -rxt(k,389)*y(k,53)
         mat(k,2680) = -rxt(k,390)*y(k,53)
         mat(k,2201) = -rxt(k,391)*y(k,53)
         mat(k,720) = .400_r8*rxt(k,489)*y(k,259)
         mat(k,1067) = .340_r8*rxt(k,573)*y(k,166)
         mat(k,406) = .500_r8*rxt(k,460)*y(k,157)
         mat(k,634) = rxt(k,412)*y(k,166)
         mat(k,1199) = .500_r8*rxt(k,441)*y(k,166)
         mat(k,661) = .500_r8*rxt(k,429)*y(k,259)
         mat(k,876) = rxt(k,396)*y(k,259)
         mat(k,473) = .300_r8*rxt(k,397)*y(k,259)
         mat(k,1649) = (rxt(k,405)+rxt(k,406))*y(k,255)
         mat(k,1177) = rxt(k,372)*y(k,231)
         mat(k,2710) = rxt(k,253)*y(k,231)
         mat(k,1268) = .800_r8*rxt(k,434)*y(k,259)
         mat(k,954) = .910_r8*rxt(k,518)*y(k,166)
         mat(k,650) = .300_r8*rxt(k,509)*y(k,259)
         mat(k,1393) = .120_r8*rxt(k,471)*y(k,166)
         mat(k,670) = .500_r8*rxt(k,484)*y(k,259)
         mat(k,1040) = .340_r8*rxt(k,576)*y(k,166)
         mat(k,1503) = .600_r8*rxt(k,485)*y(k,166)
         mat(k,2574) = .100_r8*rxt(k,491)*y(k,222) + rxt(k,395)*y(k,231) &
                      + .500_r8*rxt(k,462)*y(k,234) + .500_r8*rxt(k,431)*y(k,236) &
                      + .920_r8*rxt(k,501)*y(k,239) + .250_r8*rxt(k,469)*y(k,244) &
                      + rxt(k,478)*y(k,246) + rxt(k,452)*y(k,262) + rxt(k,456) &
                      *y(k,263) + .340_r8*rxt(k,585)*y(k,264) + .320_r8*rxt(k,590) &
                      *y(k,265) + .250_r8*rxt(k,526)*y(k,268)
         mat(k,2782) = mat(k,2782) + .500_r8*rxt(k,460)*y(k,18) + rxt(k,502)*y(k,239) &
                      + .250_r8*rxt(k,468)*y(k,244) + rxt(k,479)*y(k,246)
         mat(k,2025) = .340_r8*rxt(k,573)*y(k,8) + rxt(k,412)*y(k,30) &
                      + .500_r8*rxt(k,441)*y(k,35) + .910_r8*rxt(k,518)*y(k,129) &
                      + .120_r8*rxt(k,471)*y(k,136) + .340_r8*rxt(k,576)*y(k,141) &
                      + .600_r8*rxt(k,485)*y(k,142)
         mat(k,609) = rxt(k,436)*y(k,259)
         mat(k,1237) = .680_r8*rxt(k,594)*y(k,259)
         mat(k,1084) = .100_r8*rxt(k,491)*y(k,155)
         mat(k,1000) = .700_r8*rxt(k,414)*y(k,231)
         mat(k,897) = rxt(k,442)*y(k,231)
         mat(k,1553) = rxt(k,425)*y(k,231) + rxt(k,498)*y(k,239) + .250_r8*rxt(k,465) &
                      *y(k,244) + rxt(k,474)*y(k,246) + .250_r8*rxt(k,523)*y(k,268)
         mat(k,1748) = rxt(k,372)*y(k,70) + rxt(k,253)*y(k,76) + rxt(k,395)*y(k,155) &
                      + .700_r8*rxt(k,414)*y(k,227) + rxt(k,442)*y(k,228) + rxt(k,425) &
                      *y(k,230) + (4.000_r8*rxt(k,392)+2.000_r8*rxt(k,393))*y(k,231) &
                      + 1.500_r8*rxt(k,499)*y(k,239) + .750_r8*rxt(k,504)*y(k,240) &
                      + .800_r8*rxt(k,513)*y(k,241) + .880_r8*rxt(k,466)*y(k,244) &
                      + 2.000_r8*rxt(k,475)*y(k,246) + .750_r8*rxt(k,578)*y(k,254) &
                      + .800_r8*rxt(k,454)*y(k,263) + .930_r8*rxt(k,583)*y(k,264) &
                      + .950_r8*rxt(k,588)*y(k,265) + .800_r8*rxt(k,524)*y(k,268)
         mat(k,641) = .500_r8*rxt(k,462)*y(k,155)
         mat(k,863) = .500_r8*rxt(k,431)*y(k,155)
         mat(k,2421) = mat(k,2421) + .450_r8*rxt(k,476)*y(k,246) + .150_r8*rxt(k,455) &
                      *y(k,263)
         mat(k,1426) = .920_r8*rxt(k,501)*y(k,155) + rxt(k,502)*y(k,157) + rxt(k,498) &
                      *y(k,230) + 1.500_r8*rxt(k,499)*y(k,231)
         mat(k,1459) = .750_r8*rxt(k,504)*y(k,231)
         mat(k,1378) = .800_r8*rxt(k,513)*y(k,231)
         mat(k,1480) = .250_r8*rxt(k,469)*y(k,155) + .250_r8*rxt(k,468)*y(k,157) &
                      + .250_r8*rxt(k,465)*y(k,230) + .880_r8*rxt(k,466)*y(k,231)
         mat(k,1521) = rxt(k,478)*y(k,155) + rxt(k,479)*y(k,157) + rxt(k,474)*y(k,230) &
                      + 2.000_r8*rxt(k,475)*y(k,231) + .450_r8*rxt(k,476)*y(k,237) &
                      + 4.000_r8*rxt(k,477)*y(k,246)
         mat(k,1224) = .750_r8*rxt(k,578)*y(k,231)
         mat(k,1874) = (rxt(k,405)+rxt(k,406))*y(k,66)
         mat(k,2201) = mat(k,2201) + .400_r8*rxt(k,489)*y(k,1) + .500_r8*rxt(k,429) &
                      *y(k,62) + rxt(k,396)*y(k,64) + .300_r8*rxt(k,397)*y(k,65) &
                      + .800_r8*rxt(k,434)*y(k,92) + .300_r8*rxt(k,509)*y(k,130) &
                      + .500_r8*rxt(k,484)*y(k,140) + rxt(k,436)*y(k,172) &
                      + .680_r8*rxt(k,594)*y(k,211)
         mat(k,885) = rxt(k,452)*y(k,155)
         mat(k,1324) = rxt(k,456)*y(k,155) + .800_r8*rxt(k,454)*y(k,231) &
                      + .150_r8*rxt(k,455)*y(k,237)
         mat(k,1288) = .340_r8*rxt(k,585)*y(k,155) + .930_r8*rxt(k,583)*y(k,231)
         mat(k,1309) = .320_r8*rxt(k,590)*y(k,155) + .950_r8*rxt(k,588)*y(k,231)
         mat(k,1356) = .250_r8*rxt(k,526)*y(k,155) + .250_r8*rxt(k,523)*y(k,230) &
                      + .800_r8*rxt(k,524)*y(k,231)
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
         mat(k,679) = -(rxt(k,361)*y(k,72) + rxt(k,362)*y(k,259) + rxt(k,384)*y(k,255))
         mat(k,2255) = -rxt(k,361)*y(k,54)
         mat(k,2119) = -rxt(k,362)*y(k,54)
         mat(k,1853) = -rxt(k,384)*y(k,54)
         mat(k,121) = -(rxt(k,363)*y(k,259))
         mat(k,2046) = -rxt(k,363)*y(k,55)
         mat(k,1203) = -(rxt(k,421)*y(k,157) + rxt(k,422)*y(k,259))
         mat(k,2741) = -rxt(k,421)*y(k,56)
         mat(k,2158) = -rxt(k,422)*y(k,56)
         mat(k,715) = .800_r8*rxt(k,489)*y(k,259)
         mat(k,403) = rxt(k,460)*y(k,157)
         mat(k,288) = rxt(k,417)*y(k,259)
         mat(k,367) = .500_r8*rxt(k,418)*y(k,259)
         mat(k,1186) = .500_r8*rxt(k,441)*y(k,166)
         mat(k,1487) = .100_r8*rxt(k,485)*y(k,166)
         mat(k,2535) = .400_r8*rxt(k,491)*y(k,222) + rxt(k,416)*y(k,227) &
                      + .270_r8*rxt(k,444)*y(k,228) + rxt(k,462)*y(k,234) + rxt(k,481) &
                      *y(k,248) + rxt(k,452)*y(k,262)
         mat(k,2741) = mat(k,2741) + rxt(k,460)*y(k,18)
         mat(k,1990) = .500_r8*rxt(k,441)*y(k,35) + .100_r8*rxt(k,485)*y(k,142)
         mat(k,1077) = .400_r8*rxt(k,491)*y(k,155)
         mat(k,994) = rxt(k,416)*y(k,155) + 3.200_r8*rxt(k,413)*y(k,227) &
                      + .800_r8*rxt(k,414)*y(k,231)
         mat(k,891) = .270_r8*rxt(k,444)*y(k,155)
         mat(k,1716) = .800_r8*rxt(k,414)*y(k,227)
         mat(k,637) = rxt(k,462)*y(k,155)
         mat(k,2380) = .200_r8*rxt(k,480)*y(k,248)
         mat(k,757) = rxt(k,481)*y(k,155) + .200_r8*rxt(k,480)*y(k,237)
         mat(k,2158) = mat(k,2158) + .800_r8*rxt(k,489)*y(k,1) + rxt(k,417)*y(k,32) &
                      + .500_r8*rxt(k,418)*y(k,33)
         mat(k,879) = rxt(k,452)*y(k,155)
         mat(k,419) = -(rxt(k,364)*y(k,72) + rxt(k,365)*y(k,259))
         mat(k,2248) = -rxt(k,364)*y(k,57)
         mat(k,2086) = -rxt(k,365)*y(k,57)
         mat(k,76) = -(rxt(k,423)*y(k,259))
         mat(k,2040) = -rxt(k,423)*y(k,58)
         mat(k,1086) = -(rxt(k,459)*y(k,259))
         mat(k,2148) = -rxt(k,459)*y(k,59)
         mat(k,714) = .800_r8*rxt(k,489)*y(k,259)
         mat(k,1053) = .520_r8*rxt(k,573)*y(k,166)
         mat(k,402) = .500_r8*rxt(k,460)*y(k,157)
         mat(k,1026) = .520_r8*rxt(k,576)*y(k,166)
         mat(k,2529) = .250_r8*rxt(k,491)*y(k,222) + .820_r8*rxt(k,444)*y(k,228) &
                      + .500_r8*rxt(k,462)*y(k,234) + .270_r8*rxt(k,585)*y(k,264) &
                      + .040_r8*rxt(k,590)*y(k,265)
         mat(k,2733) = .500_r8*rxt(k,460)*y(k,18)
         mat(k,1985) = .520_r8*rxt(k,573)*y(k,8) + .520_r8*rxt(k,576)*y(k,141)
         mat(k,1228) = .500_r8*rxt(k,594)*y(k,259)
         mat(k,1076) = .250_r8*rxt(k,491)*y(k,155)
         mat(k,890) = .820_r8*rxt(k,444)*y(k,155) + .820_r8*rxt(k,442)*y(k,231)
         mat(k,1711) = .820_r8*rxt(k,442)*y(k,228) + .150_r8*rxt(k,583)*y(k,264) &
                      + .025_r8*rxt(k,588)*y(k,265)
         mat(k,636) = .500_r8*rxt(k,462)*y(k,155)
         mat(k,2148) = mat(k,2148) + .800_r8*rxt(k,489)*y(k,1) + .500_r8*rxt(k,594) &
                      *y(k,211)
         mat(k,1276) = .270_r8*rxt(k,585)*y(k,155) + .150_r8*rxt(k,583)*y(k,231)
         mat(k,1295) = .040_r8*rxt(k,590)*y(k,155) + .025_r8*rxt(k,588)*y(k,231)
         mat(k,1396) = -(rxt(k,447)*y(k,157) + rxt(k,448)*y(k,259))
         mat(k,2755) = -rxt(k,447)*y(k,60)
         mat(k,2172) = -rxt(k,448)*y(k,60)
         mat(k,1257) = rxt(k,449)*y(k,259)
         mat(k,1385) = .880_r8*rxt(k,471)*y(k,166)
         mat(k,1490) = .500_r8*rxt(k,485)*y(k,166)
         mat(k,2548) = .170_r8*rxt(k,544)*y(k,232) + .050_r8*rxt(k,507)*y(k,240) &
                      + .250_r8*rxt(k,469)*y(k,244) + .170_r8*rxt(k,550)*y(k,247) &
                      + .400_r8*rxt(k,560)*y(k,266) + .250_r8*rxt(k,526)*y(k,268) &
                      + .540_r8*rxt(k,566)*y(k,269) + .510_r8*rxt(k,569)*y(k,271)
         mat(k,2755) = mat(k,2755) + .050_r8*rxt(k,508)*y(k,240) + .250_r8*rxt(k,468) &
                      *y(k,244) + .250_r8*rxt(k,527)*y(k,268)
         mat(k,930) = rxt(k,450)*y(k,259)
         mat(k,2001) = .880_r8*rxt(k,471)*y(k,136) + .500_r8*rxt(k,485)*y(k,142)
         mat(k,1538) = .250_r8*rxt(k,465)*y(k,244) + .250_r8*rxt(k,523)*y(k,268)
         mat(k,1728) = .240_r8*rxt(k,466)*y(k,244) + .500_r8*rxt(k,454)*y(k,263) &
                      + .100_r8*rxt(k,524)*y(k,268)
         mat(k,849) = .170_r8*rxt(k,544)*y(k,155) + .070_r8*rxt(k,543)*y(k,237)
         mat(k,2393) = .070_r8*rxt(k,543)*y(k,232) + .070_r8*rxt(k,549)*y(k,247)
         mat(k,1447) = .050_r8*rxt(k,507)*y(k,155) + .050_r8*rxt(k,508)*y(k,157)
         mat(k,1471) = .250_r8*rxt(k,469)*y(k,155) + .250_r8*rxt(k,468)*y(k,157) &
                      + .250_r8*rxt(k,465)*y(k,230) + .240_r8*rxt(k,466)*y(k,231)
         mat(k,1005) = .170_r8*rxt(k,550)*y(k,155) + .070_r8*rxt(k,549)*y(k,237)
         mat(k,2172) = mat(k,2172) + rxt(k,449)*y(k,115) + rxt(k,450)*y(k,158)
         mat(k,1318) = .500_r8*rxt(k,454)*y(k,231)
         mat(k,818) = .400_r8*rxt(k,560)*y(k,155)
         mat(k,1349) = .250_r8*rxt(k,526)*y(k,155) + .250_r8*rxt(k,527)*y(k,157) &
                      + .250_r8*rxt(k,523)*y(k,230) + .100_r8*rxt(k,524)*y(k,231)
         mat(k,841) = .540_r8*rxt(k,566)*y(k,155)
         mat(k,572) = .510_r8*rxt(k,569)*y(k,155)
         mat(k,774) = -(rxt(k,428)*y(k,259))
         mat(k,2127) = -rxt(k,428)*y(k,61)
         mat(k,1181) = .120_r8*rxt(k,441)*y(k,166)
         mat(k,1977) = .120_r8*rxt(k,441)*y(k,35)
         mat(k,1529) = .100_r8*rxt(k,425)*y(k,231) + .150_r8*rxt(k,426)*y(k,237)
         mat(k,1706) = .100_r8*rxt(k,425)*y(k,230)
         mat(k,2361) = .150_r8*rxt(k,426)*y(k,230) + .150_r8*rxt(k,476)*y(k,246)
         mat(k,1510) = .150_r8*rxt(k,476)*y(k,237)
         mat(k,656) = -(rxt(k,429)*y(k,259))
         mat(k,2116) = -rxt(k,429)*y(k,62)
         mat(k,1528) = .400_r8*rxt(k,426)*y(k,237)
         mat(k,2354) = .400_r8*rxt(k,426)*y(k,230) + .400_r8*rxt(k,476)*y(k,246)
         mat(k,1508) = .400_r8*rxt(k,476)*y(k,237)
         mat(k,384) = -(rxt(k,366)*y(k,72) + rxt(k,367)*y(k,259))
         mat(k,2247) = -rxt(k,366)*y(k,63)
         mat(k,2083) = -rxt(k,367)*y(k,63)
         mat(k,873) = -(rxt(k,396)*y(k,259))
         mat(k,2136) = -rxt(k,396)*y(k,64)
         mat(k,992) = .300_r8*rxt(k,414)*y(k,231)
         mat(k,1707) = .300_r8*rxt(k,414)*y(k,227) + 2.000_r8*rxt(k,393)*y(k,231) &
                      + .250_r8*rxt(k,499)*y(k,239) + .250_r8*rxt(k,504)*y(k,240) &
                      + .200_r8*rxt(k,513)*y(k,241) + .250_r8*rxt(k,466)*y(k,244) &
                      + .250_r8*rxt(k,578)*y(k,254) + .500_r8*rxt(k,454)*y(k,263) &
                      + .250_r8*rxt(k,583)*y(k,264) + .250_r8*rxt(k,588)*y(k,265) &
                      + .300_r8*rxt(k,524)*y(k,268)
         mat(k,1406) = .250_r8*rxt(k,499)*y(k,231)
         mat(k,1436) = .250_r8*rxt(k,504)*y(k,231)
         mat(k,1362) = .200_r8*rxt(k,513)*y(k,231)
         mat(k,1465) = .250_r8*rxt(k,466)*y(k,231)
         mat(k,1214) = .250_r8*rxt(k,578)*y(k,231)
         mat(k,1315) = .500_r8*rxt(k,454)*y(k,231)
         mat(k,1275) = .250_r8*rxt(k,583)*y(k,231)
         mat(k,1294) = .250_r8*rxt(k,588)*y(k,231)
         mat(k,1343) = .300_r8*rxt(k,524)*y(k,231)
         mat(k,469) = -(rxt(k,397)*y(k,259))
         mat(k,2093) = -rxt(k,397)*y(k,65)
         mat(k,1704) = rxt(k,394)*y(k,237)
         mat(k,2339) = rxt(k,394)*y(k,231)
         mat(k,1639) = -(rxt(k,245)*y(k,72) + rxt(k,346)*y(k,91) + rxt(k,398)*y(k,259) &
                      + (rxt(k,404) + rxt(k,405) + rxt(k,406)) * y(k,255))
         mat(k,2272) = -rxt(k,245)*y(k,66)
         mat(k,983) = -rxt(k,346)*y(k,66)
         mat(k,2184) = -rxt(k,398)*y(k,66)
         mat(k,1857) = -(rxt(k,404) + rxt(k,405) + rxt(k,406)) * y(k,66)
         mat(k,1192) = .100_r8*rxt(k,441)*y(k,166)
         mat(k,2010) = .100_r8*rxt(k,441)*y(k,35)
         mat(k,85) = -(rxt(k,368)*y(k,259))
         mat(k,2042) = -rxt(k,368)*y(k,67)
         mat(k,457) = -(rxt(k,303)*y(k,255) + rxt(k,369)*y(k,72) + rxt(k,370)*y(k,259))
         mat(k,1851) = -rxt(k,303)*y(k,68)
         mat(k,2249) = -rxt(k,369)*y(k,68)
         mat(k,2091) = -rxt(k,370)*y(k,68)
         mat(k,89) = -(rxt(k,371)*y(k,259))
         mat(k,2043) = -rxt(k,371)*y(k,69)
         mat(k,1169) = -((rxt(k,372) + rxt(k,373)) * y(k,231) + (rxt(k,374) + rxt(k,375) &
                      ) * y(k,237) + rxt(k,376)*y(k,155) + rxt(k,377)*y(k,157))
         mat(k,1715) = -(rxt(k,372) + rxt(k,373)) * y(k,70)
         mat(k,2379) = -(rxt(k,374) + rxt(k,375)) * y(k,70)
         mat(k,2534) = -rxt(k,376)*y(k,70)
         mat(k,2739) = -rxt(k,377)*y(k,70)
         mat(k,327) = rxt(k,359)*y(k,72) + rxt(k,360)*y(k,259)
         mat(k,2264) = rxt(k,359)*y(k,49)
         mat(k,2156) = rxt(k,360)*y(k,49)
         mat(k,374) = -(rxt(k,378)*y(k,72) + rxt(k,379)*y(k,259))
         mat(k,2246) = -rxt(k,378)*y(k,71)
         mat(k,2081) = -rxt(k,379)*y(k,71)
         mat(k,2284) = -(rxt(k,244)*y(k,53) + rxt(k,245)*y(k,66) + rxt(k,246)*y(k,95) &
                      + rxt(k,247)*y(k,97) + (rxt(k,248) + rxt(k,249)) * y(k,237) &
                      + rxt(k,250)*y(k,156) + rxt(k,252)*y(k,166) + rxt(k,259)*y(k,77) &
                      + rxt(k,268)*y(k,111) + rxt(k,294)*y(k,24) + rxt(k,352)*y(k,28) &
                      + rxt(k,354)*y(k,31) + rxt(k,356)*y(k,47) + rxt(k,359)*y(k,49) &
                      + rxt(k,361)*y(k,54) + rxt(k,364)*y(k,57) + rxt(k,366)*y(k,63) &
                      + rxt(k,369)*y(k,68) + rxt(k,419)*y(k,34) + (rxt(k,596) &
                      + rxt(k,597)) * y(k,85))
         mat(k,2477) = -rxt(k,244)*y(k,72)
         mat(k,1647) = -rxt(k,245)*y(k,72)
         mat(k,1612) = -rxt(k,246)*y(k,72)
         mat(k,675) = -rxt(k,247)*y(k,72)
         mat(k,2417) = -(rxt(k,248) + rxt(k,249)) * y(k,72)
         mat(k,1952) = -rxt(k,250)*y(k,72)
         mat(k,2021) = -rxt(k,252)*y(k,72)
         mat(k,1120) = -rxt(k,259)*y(k,72)
         mat(k,1802) = -rxt(k,268)*y(k,72)
         mat(k,962) = -rxt(k,294)*y(k,72)
         mat(k,198) = -rxt(k,352)*y(k,72)
         mat(k,276) = -rxt(k,354)*y(k,72)
         mat(k,543) = -rxt(k,356)*y(k,72)
         mat(k,330) = -rxt(k,359)*y(k,72)
         mat(k,684) = -rxt(k,361)*y(k,72)
         mat(k,424) = -rxt(k,364)*y(k,72)
         mat(k,389) = -rxt(k,366)*y(k,72)
         mat(k,461) = -rxt(k,369)*y(k,72)
         mat(k,324) = -rxt(k,419)*y(k,72)
         mat(k,1130) = -(rxt(k,596) + rxt(k,597)) * y(k,72)
         mat(k,2446) = rxt(k,289)*y(k,76)
         mat(k,198) = mat(k,198) + 5.000_r8*rxt(k,352)*y(k,72) + 3.060_r8*rxt(k,353) &
                      *y(k,259)
         mat(k,276) = mat(k,276) + 2.000_r8*rxt(k,354)*y(k,72) + 2.000_r8*rxt(k,355) &
                      *y(k,259)
         mat(k,84) = 4.000_r8*rxt(k,271)*y(k,255)
         mat(k,143) = rxt(k,272)*y(k,255)
         mat(k,112) = 2.000_r8*rxt(k,273)*y(k,255)
         mat(k,154) = 2.000_r8*rxt(k,274)*y(k,255)
         mat(k,116) = 2.000_r8*rxt(k,275)*y(k,255)
         mat(k,159) = rxt(k,276)*y(k,255)
         mat(k,120) = 2.000_r8*rxt(k,277)*y(k,255)
         mat(k,103) = rxt(k,358)*y(k,259)
         mat(k,123) = 3.000_r8*rxt(k,363)*y(k,259)
         mat(k,424) = mat(k,424) + rxt(k,365)*y(k,259)
         mat(k,87) = rxt(k,368)*y(k,259)
         mat(k,91) = 2.000_r8*rxt(k,371)*y(k,259)
         mat(k,1175) = 2.000_r8*rxt(k,376)*y(k,155) + 2.000_r8*rxt(k,377)*y(k,157) &
                      + 2.000_r8*rxt(k,372)*y(k,231) + rxt(k,375)*y(k,237)
         mat(k,379) = rxt(k,379)*y(k,259)
         mat(k,2284) = mat(k,2284) + 5.000_r8*rxt(k,352)*y(k,28) + 2.000_r8*rxt(k,354) &
                      *y(k,31)
         mat(k,2706) = rxt(k,289)*y(k,23) + (4.000_r8*rxt(k,254)+2.000_r8*rxt(k,256)) &
                      *y(k,76) + rxt(k,326)*y(k,127) + rxt(k,258)*y(k,155) &
                      + rxt(k,263)*y(k,164) + rxt(k,607)*y(k,183) + rxt(k,253) &
                      *y(k,231) + rxt(k,264)*y(k,259)
         mat(k,250) = rxt(k,351)*y(k,255)
         mat(k,246) = rxt(k,385)*y(k,255) + rxt(k,380)*y(k,259)
         mat(k,255) = rxt(k,386)*y(k,255) + rxt(k,381)*y(k,259)
         mat(k,312) = rxt(k,387)*y(k,255) + rxt(k,382)*y(k,259)
         mat(k,1825) = rxt(k,266)*y(k,164) + rxt(k,278)*y(k,255) + rxt(k,267)*y(k,259)
         mat(k,2629) = rxt(k,326)*y(k,76)
         mat(k,2570) = 2.000_r8*rxt(k,376)*y(k,70) + rxt(k,258)*y(k,76)
         mat(k,2778) = 2.000_r8*rxt(k,377)*y(k,70)
         mat(k,2676) = rxt(k,263)*y(k,76) + rxt(k,266)*y(k,103)
         mat(k,1626) = rxt(k,607)*y(k,76)
         mat(k,1746) = 2.000_r8*rxt(k,372)*y(k,70) + rxt(k,253)*y(k,76)
         mat(k,2417) = mat(k,2417) + rxt(k,375)*y(k,70)
         mat(k,1870) = 4.000_r8*rxt(k,271)*y(k,39) + rxt(k,272)*y(k,40) &
                      + 2.000_r8*rxt(k,273)*y(k,42) + 2.000_r8*rxt(k,274)*y(k,43) &
                      + 2.000_r8*rxt(k,275)*y(k,44) + rxt(k,276)*y(k,45) &
                      + 2.000_r8*rxt(k,277)*y(k,46) + rxt(k,351)*y(k,83) + rxt(k,385) &
                      *y(k,100) + rxt(k,386)*y(k,101) + rxt(k,387)*y(k,102) &
                      + rxt(k,278)*y(k,103)
         mat(k,2197) = 3.060_r8*rxt(k,353)*y(k,28) + 2.000_r8*rxt(k,355)*y(k,31) &
                      + rxt(k,358)*y(k,48) + 3.000_r8*rxt(k,363)*y(k,55) + rxt(k,365) &
                      *y(k,57) + rxt(k,368)*y(k,67) + 2.000_r8*rxt(k,371)*y(k,69) &
                      + rxt(k,379)*y(k,71) + rxt(k,264)*y(k,76) + rxt(k,380)*y(k,100) &
                      + rxt(k,381)*y(k,101) + rxt(k,382)*y(k,102) + rxt(k,267) &
                      *y(k,103)
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
         mat(k,2239) = rxt(k,259)*y(k,77)
         mat(k,2689) = 2.000_r8*rxt(k,255)*y(k,76)
         mat(k,1114) = rxt(k,259)*y(k,72) + (rxt(k,691)+rxt(k,700)+rxt(k,709)) &
                      *y(k,103)
         mat(k,1811) = (rxt(k,691)+rxt(k,700)+rxt(k,709))*y(k,77) + (rxt(k,624) &
                       +rxt(k,681)+rxt(k,692)+rxt(k,701))*y(k,111)
         mat(k,1787) = (rxt(k,624)+rxt(k,681)+rxt(k,692)+rxt(k,701))*y(k,103)
         mat(k,2688) = 2.000_r8*rxt(k,280)*y(k,76)
         mat(k,578) = -(rxt(k,251)*y(k,259))
         mat(k,2106) = -rxt(k,251)*y(k,75)
         mat(k,2251) = rxt(k,250)*y(k,156)
         mat(k,1813) = rxt(k,640)*y(k,145)
         mat(k,393) = rxt(k,640)*y(k,103)
         mat(k,1916) = rxt(k,250)*y(k,72)
         mat(k,2715) = -(rxt(k,253)*y(k,231) + (4._r8*rxt(k,254) + 4._r8*rxt(k,255) &
                      + 4._r8*rxt(k,256) + 4._r8*rxt(k,280)) * y(k,76) + rxt(k,257) &
                      *y(k,237) + rxt(k,258)*y(k,155) + rxt(k,260)*y(k,156) + rxt(k,263) &
                      *y(k,164) + (rxt(k,264) + rxt(k,265)) * y(k,259) + (rxt(k,288) &
                      + rxt(k,289) + rxt(k,290)) * y(k,23) + (rxt(k,325) + rxt(k,326) &
                      + rxt(k,327)) * y(k,127) + rxt(k,607)*y(k,183))
         mat(k,1752) = -rxt(k,253)*y(k,76)
         mat(k,2426) = -rxt(k,257)*y(k,76)
         mat(k,2579) = -rxt(k,258)*y(k,76)
         mat(k,1961) = -rxt(k,260)*y(k,76)
         mat(k,2685) = -rxt(k,263)*y(k,76)
         mat(k,2206) = -(rxt(k,264) + rxt(k,265)) * y(k,76)
         mat(k,2455) = -(rxt(k,288) + rxt(k,289) + rxt(k,290)) * y(k,76)
         mat(k,2638) = -(rxt(k,325) + rxt(k,326) + rxt(k,327)) * y(k,76)
         mat(k,1632) = -rxt(k,607)*y(k,76)
         mat(k,2293) = rxt(k,268)*y(k,111) + rxt(k,252)*y(k,166) + rxt(k,249)*y(k,237)
         mat(k,1123) = rxt(k,261)*y(k,164)
         mat(k,1831) = rxt(k,279)*y(k,255)
         mat(k,1808) = rxt(k,268)*y(k,72) + rxt(k,269)*y(k,164) + rxt(k,270)*y(k,259)
         mat(k,2685) = mat(k,2685) + rxt(k,261)*y(k,77) + rxt(k,269)*y(k,111)
         mat(k,2030) = rxt(k,252)*y(k,72)
         mat(k,512) = rxt(k,612)*y(k,183)
         mat(k,1632) = mat(k,1632) + rxt(k,612)*y(k,168)
         mat(k,2426) = mat(k,2426) + rxt(k,249)*y(k,72)
         mat(k,1879) = rxt(k,279)*y(k,103)
         mat(k,2206) = mat(k,2206) + rxt(k,270)*y(k,111)
         mat(k,1115) = -(rxt(k,259)*y(k,72) + rxt(k,261)*y(k,164) + rxt(k,262) &
                      *y(k,259) + (rxt(k,691) + rxt(k,700) + rxt(k,709)) * y(k,103))
         mat(k,2261) = -rxt(k,259)*y(k,77)
         mat(k,2655) = -rxt(k,261)*y(k,77)
         mat(k,2151) = -rxt(k,262)*y(k,77)
         mat(k,1815) = -(rxt(k,691) + rxt(k,700) + rxt(k,709)) * y(k,77)
         mat(k,2693) = rxt(k,260)*y(k,156)
         mat(k,1929) = rxt(k,260)*y(k,76)
         mat(k,1269) = -(rxt(k,408)*y(k,259))
         mat(k,2164) = -rxt(k,408)*y(k,79)
         mat(k,1058) = .230_r8*rxt(k,573)*y(k,166)
         mat(k,2300) = rxt(k,283)*y(k,53)
         mat(k,317) = .350_r8*rxt(k,410)*y(k,259)
         mat(k,630) = .630_r8*rxt(k,412)*y(k,166)
         mat(k,1188) = .560_r8*rxt(k,441)*y(k,166)
         mat(k,2460) = rxt(k,283)*y(k,19) + rxt(k,244)*y(k,72) + rxt(k,389)*y(k,157) &
                      + rxt(k,390)*y(k,164) + rxt(k,391)*y(k,259)
         mat(k,420) = rxt(k,364)*y(k,72)
         mat(k,1395) = rxt(k,447)*y(k,157) + rxt(k,448)*y(k,259)
         mat(k,1170) = rxt(k,376)*y(k,155) + rxt(k,377)*y(k,157) + (rxt(k,372) &
                       +rxt(k,373))*y(k,231) + rxt(k,375)*y(k,237)
         mat(k,2267) = rxt(k,244)*y(k,53) + rxt(k,364)*y(k,57)
         mat(k,1565) = rxt(k,734)*y(k,260)
         mat(k,1094) = rxt(k,435)*y(k,259)
         mat(k,943) = .620_r8*rxt(k,518)*y(k,166)
         mat(k,1383) = .650_r8*rxt(k,471)*y(k,166)
         mat(k,1031) = .230_r8*rxt(k,576)*y(k,166)
         mat(k,1489) = .560_r8*rxt(k,485)*y(k,166)
         mat(k,2541) = rxt(k,376)*y(k,70) + .170_r8*rxt(k,544)*y(k,232) &
                      + .220_r8*rxt(k,469)*y(k,244) + .400_r8*rxt(k,547)*y(k,245) &
                      + .350_r8*rxt(k,550)*y(k,247) + .225_r8*rxt(k,585)*y(k,264) &
                      + .250_r8*rxt(k,526)*y(k,268)
         mat(k,2747) = rxt(k,389)*y(k,53) + rxt(k,447)*y(k,60) + rxt(k,377)*y(k,70) &
                      + .220_r8*rxt(k,468)*y(k,244) + .500_r8*rxt(k,527)*y(k,268)
         mat(k,2657) = rxt(k,390)*y(k,53) + rxt(k,601)*y(k,169)
         mat(k,1995) = .230_r8*rxt(k,573)*y(k,8) + .630_r8*rxt(k,412)*y(k,30) &
                      + .560_r8*rxt(k,441)*y(k,35) + .620_r8*rxt(k,518)*y(k,129) &
                      + .650_r8*rxt(k,471)*y(k,136) + .230_r8*rxt(k,576)*y(k,141) &
                      + .560_r8*rxt(k,485)*y(k,142)
         mat(k,411) = rxt(k,601)*y(k,164) + rxt(k,602)*y(k,259)
         mat(k,1230) = .700_r8*rxt(k,594)*y(k,259)
         mat(k,1534) = .220_r8*rxt(k,465)*y(k,244) + .250_r8*rxt(k,523)*y(k,268)
         mat(k,1721) = (rxt(k,372)+rxt(k,373))*y(k,70) + .110_r8*rxt(k,466)*y(k,244) &
                      + .125_r8*rxt(k,583)*y(k,264) + .200_r8*rxt(k,524)*y(k,268)
         mat(k,848) = .170_r8*rxt(k,544)*y(k,155) + .070_r8*rxt(k,543)*y(k,237)
         mat(k,2385) = rxt(k,375)*y(k,70) + .070_r8*rxt(k,543)*y(k,232) &
                      + .160_r8*rxt(k,546)*y(k,245) + .140_r8*rxt(k,549)*y(k,247)
         mat(k,1469) = .220_r8*rxt(k,469)*y(k,155) + .220_r8*rxt(k,468)*y(k,157) &
                      + .220_r8*rxt(k,465)*y(k,230) + .110_r8*rxt(k,466)*y(k,231)
         mat(k,804) = .400_r8*rxt(k,547)*y(k,155) + .160_r8*rxt(k,546)*y(k,237)
         mat(k,1004) = .350_r8*rxt(k,550)*y(k,155) + .140_r8*rxt(k,549)*y(k,237)
         mat(k,2164) = mat(k,2164) + .350_r8*rxt(k,410)*y(k,29) + rxt(k,391)*y(k,53) &
                      + rxt(k,448)*y(k,60) + rxt(k,435)*y(k,93) + rxt(k,602)*y(k,169) &
                      + .700_r8*rxt(k,594)*y(k,211)
         mat(k,869) = rxt(k,734)*y(k,80)
         mat(k,1279) = .225_r8*rxt(k,585)*y(k,155) + .125_r8*rxt(k,583)*y(k,231)
         mat(k,1347) = .250_r8*rxt(k,526)*y(k,155) + .500_r8*rxt(k,527)*y(k,157) &
                      + .250_r8*rxt(k,523)*y(k,230) + .200_r8*rxt(k,524)*y(k,231)
         mat(k,1566) = -(rxt(k,734)*y(k,260))
         mat(k,870) = -rxt(k,734)*y(k,80)
         mat(k,1063) = .270_r8*rxt(k,573)*y(k,166)
         mat(k,1191) = .200_r8*rxt(k,441)*y(k,166)
         mat(k,775) = rxt(k,428)*y(k,259)
         mat(k,658) = .500_r8*rxt(k,429)*y(k,259)
         mat(k,1270) = rxt(k,408)*y(k,259)
         mat(k,1265) = .800_r8*rxt(k,434)*y(k,259)
         mat(k,1095) = rxt(k,435)*y(k,259)
         mat(k,1012) = rxt(k,400)*y(k,259)
         mat(k,666) = .500_r8*rxt(k,484)*y(k,259)
         mat(k,1036) = .270_r8*rxt(k,576)*y(k,166)
         mat(k,1495) = .100_r8*rxt(k,485)*y(k,166)
         mat(k,2555) = rxt(k,427)*y(k,230) + .900_r8*rxt(k,585)*y(k,264)
         mat(k,2008) = .270_r8*rxt(k,573)*y(k,8) + .200_r8*rxt(k,441)*y(k,35) &
                      + .270_r8*rxt(k,576)*y(k,141) + .100_r8*rxt(k,485)*y(k,142)
         mat(k,1233) = 1.800_r8*rxt(k,594)*y(k,259)
         mat(k,1545) = rxt(k,427)*y(k,155) + 4.000_r8*rxt(k,424)*y(k,230) &
                      + .900_r8*rxt(k,425)*y(k,231) + rxt(k,498)*y(k,239) &
                      + 2.000_r8*rxt(k,474)*y(k,246) + rxt(k,523)*y(k,268)
         mat(k,1735) = .900_r8*rxt(k,425)*y(k,230) + rxt(k,475)*y(k,246) &
                      + .500_r8*rxt(k,583)*y(k,264)
         mat(k,2400) = .450_r8*rxt(k,476)*y(k,246)
         mat(k,1420) = rxt(k,498)*y(k,230)
         mat(k,1515) = 2.000_r8*rxt(k,474)*y(k,230) + rxt(k,475)*y(k,231) &
                      + .450_r8*rxt(k,476)*y(k,237) + 4.000_r8*rxt(k,477)*y(k,246)
         mat(k,2179) = rxt(k,428)*y(k,61) + .500_r8*rxt(k,429)*y(k,62) + rxt(k,408) &
                      *y(k,79) + .800_r8*rxt(k,434)*y(k,92) + rxt(k,435)*y(k,93) &
                      + rxt(k,400)*y(k,105) + .500_r8*rxt(k,484)*y(k,140) &
                      + 1.800_r8*rxt(k,594)*y(k,211)
         mat(k,1283) = .900_r8*rxt(k,585)*y(k,155) + .500_r8*rxt(k,583)*y(k,231)
         mat(k,1351) = rxt(k,523)*y(k,230)
         mat(k,196) = .470_r8*rxt(k,353)*y(k,259)
         mat(k,1168) = rxt(k,373)*y(k,231) + rxt(k,374)*y(k,237)
         mat(k,373) = rxt(k,378)*y(k,72) + rxt(k,379)*y(k,259)
         mat(k,2245) = rxt(k,378)*y(k,71)
         mat(k,1701) = rxt(k,373)*y(k,70)
         mat(k,2334) = rxt(k,374)*y(k,70)
         mat(k,2080) = .470_r8*rxt(k,353)*y(k,28) + rxt(k,379)*y(k,71)
         mat(k,256) = -(rxt(k,350)*y(k,255))
         mat(k,1848) = -rxt(k,350)*y(k,82)
         mat(k,141) = rxt(k,272)*y(k,255)
         mat(k,146) = rxt(k,302)*y(k,255)
         mat(k,152) = rxt(k,274)*y(k,255)
         mat(k,114) = 2.000_r8*rxt(k,275)*y(k,255)
         mat(k,156) = 2.000_r8*rxt(k,276)*y(k,255)
         mat(k,118) = rxt(k,277)*y(k,255)
         mat(k,94) = 2.000_r8*rxt(k,304)*y(k,255)
         mat(k,252) = rxt(k,386)*y(k,255) + rxt(k,381)*y(k,259)
         mat(k,307) = rxt(k,387)*y(k,255) + rxt(k,382)*y(k,259)
         mat(k,1848) = mat(k,1848) + rxt(k,272)*y(k,40) + rxt(k,302)*y(k,41) &
                      + rxt(k,274)*y(k,43) + 2.000_r8*rxt(k,275)*y(k,44) &
                      + 2.000_r8*rxt(k,276)*y(k,45) + rxt(k,277)*y(k,46) &
                      + 2.000_r8*rxt(k,304)*y(k,96) + rxt(k,386)*y(k,101) + rxt(k,387) &
                      *y(k,102)
         mat(k,2062) = rxt(k,381)*y(k,101) + rxt(k,382)*y(k,102)
         mat(k,247) = -(rxt(k,351)*y(k,255))
         mat(k,1846) = -rxt(k,351)*y(k,83)
         mat(k,110) = rxt(k,273)*y(k,255)
         mat(k,151) = rxt(k,274)*y(k,255)
         mat(k,243) = rxt(k,385)*y(k,255) + rxt(k,380)*y(k,259)
         mat(k,1846) = mat(k,1846) + rxt(k,273)*y(k,42) + rxt(k,274)*y(k,43) &
                      + rxt(k,385)*y(k,100)
         mat(k,2060) = rxt(k,380)*y(k,100)
         mat(k,211) = -(rxt(k,542)*y(k,259))
         mat(k,2054) = -rxt(k,542)*y(k,84)
         mat(k,205) = .180_r8*rxt(k,562)*y(k,259)
         mat(k,2054) = mat(k,2054) + .180_r8*rxt(k,562)*y(k,213)
         mat(k,1125) = -(rxt(k,595)*y(k,23) + (rxt(k,596) + rxt(k,597)) * y(k,72) &
                      + rxt(k,598)*y(k,127) + rxt(k,599)*y(k,157) + (rxt(k,600) &
                      + rxt(k,614)) * y(k,259))
         mat(k,2433) = -rxt(k,595)*y(k,85)
         mat(k,2262) = -(rxt(k,596) + rxt(k,597)) * y(k,85)
         mat(k,2614) = -rxt(k,598)*y(k,85)
         mat(k,2735) = -rxt(k,599)*y(k,85)
         mat(k,2152) = -(rxt(k,600) + rxt(k,614)) * y(k,85)
         mat(k,855) = rxt(k,430)*y(k,237)
         mat(k,2326) = rxt(k,430)*y(k,236)
         mat(k,981) = -(rxt(k,346)*y(k,66) + rxt(k,347)*y(k,95) + rxt(k,348)*y(k,272) &
                      + rxt(k,349)*y(k,108))
         mat(k,1635) = -rxt(k,346)*y(k,91)
         mat(k,1604) = -rxt(k,347)*y(k,91)
         mat(k,2793) = -rxt(k,348)*y(k,91)
         mat(k,2583) = -rxt(k,349)*y(k,91)
         mat(k,147) = rxt(k,302)*y(k,255)
         mat(k,157) = rxt(k,276)*y(k,255)
         mat(k,257) = 2.000_r8*rxt(k,350)*y(k,255)
         mat(k,248) = rxt(k,351)*y(k,255)
         mat(k,1854) = rxt(k,302)*y(k,41) + rxt(k,276)*y(k,45) + 2.000_r8*rxt(k,350) &
                      *y(k,82) + rxt(k,351)*y(k,83)
         mat(k,1263) = -(rxt(k,434)*y(k,259))
         mat(k,2163) = -rxt(k,434)*y(k,92)
         mat(k,646) = .700_r8*rxt(k,509)*y(k,259)
         mat(k,615) = .500_r8*rxt(k,510)*y(k,259)
         mat(k,429) = rxt(k,521)*y(k,259)
         mat(k,2540) = .050_r8*rxt(k,507)*y(k,240) + .530_r8*rxt(k,469)*y(k,244) &
                      + .225_r8*rxt(k,585)*y(k,264) + .250_r8*rxt(k,526)*y(k,268)
         mat(k,2746) = .050_r8*rxt(k,508)*y(k,240) + .530_r8*rxt(k,468)*y(k,244) &
                      + .250_r8*rxt(k,527)*y(k,268)
         mat(k,1768) = rxt(k,433)*y(k,235)
         mat(k,1533) = .530_r8*rxt(k,465)*y(k,244) + .250_r8*rxt(k,523)*y(k,268)
         mat(k,1720) = .260_r8*rxt(k,466)*y(k,244) + .125_r8*rxt(k,583)*y(k,264) &
                      + .100_r8*rxt(k,524)*y(k,268)
         mat(k,517) = rxt(k,433)*y(k,165)
         mat(k,1442) = .050_r8*rxt(k,507)*y(k,155) + .050_r8*rxt(k,508)*y(k,157)
         mat(k,1468) = .530_r8*rxt(k,469)*y(k,155) + .530_r8*rxt(k,468)*y(k,157) &
                      + .530_r8*rxt(k,465)*y(k,230) + .260_r8*rxt(k,466)*y(k,231)
         mat(k,2163) = mat(k,2163) + .700_r8*rxt(k,509)*y(k,130) + .500_r8*rxt(k,510) &
                      *y(k,131) + rxt(k,521)*y(k,146)
         mat(k,1278) = .225_r8*rxt(k,585)*y(k,155) + .125_r8*rxt(k,583)*y(k,231)
         mat(k,1346) = .250_r8*rxt(k,526)*y(k,155) + .250_r8*rxt(k,527)*y(k,157) &
                      + .250_r8*rxt(k,523)*y(k,230) + .100_r8*rxt(k,524)*y(k,231)
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
         mat(k,1093) = -(rxt(k,435)*y(k,259))
         mat(k,2149) = -rxt(k,435)*y(k,93)
         mat(k,316) = .650_r8*rxt(k,410)*y(k,259)
         mat(k,1262) = .200_r8*rxt(k,434)*y(k,259)
         mat(k,1155) = rxt(k,522)*y(k,259)
         mat(k,2530) = rxt(k,533)*y(k,224) + .050_r8*rxt(k,507)*y(k,240) &
                      + .400_r8*rxt(k,547)*y(k,245) + .170_r8*rxt(k,550)*y(k,247) &
                      + .700_r8*rxt(k,553)*y(k,261) + .600_r8*rxt(k,560)*y(k,266) &
                      + .250_r8*rxt(k,526)*y(k,268) + .340_r8*rxt(k,566)*y(k,269) &
                      + .170_r8*rxt(k,569)*y(k,271)
         mat(k,2734) = .050_r8*rxt(k,508)*y(k,240) + .250_r8*rxt(k,527)*y(k,268)
         mat(k,557) = rxt(k,533)*y(k,155)
         mat(k,1530) = .250_r8*rxt(k,523)*y(k,268)
         mat(k,1712) = .100_r8*rxt(k,524)*y(k,268)
         mat(k,2377) = .160_r8*rxt(k,546)*y(k,245) + .070_r8*rxt(k,549)*y(k,247)
         mat(k,1439) = .050_r8*rxt(k,507)*y(k,155) + .050_r8*rxt(k,508)*y(k,157)
         mat(k,803) = .400_r8*rxt(k,547)*y(k,155) + .160_r8*rxt(k,546)*y(k,237)
         mat(k,1003) = .170_r8*rxt(k,550)*y(k,155) + .070_r8*rxt(k,549)*y(k,237)
         mat(k,2149) = mat(k,2149) + .650_r8*rxt(k,410)*y(k,29) + .200_r8*rxt(k,434) &
                      *y(k,92) + rxt(k,522)*y(k,147)
         mat(k,497) = .700_r8*rxt(k,553)*y(k,155)
         mat(k,816) = .600_r8*rxt(k,560)*y(k,155)
         mat(k,1344) = .250_r8*rxt(k,526)*y(k,155) + .250_r8*rxt(k,527)*y(k,157) &
                      + .250_r8*rxt(k,523)*y(k,230) + .100_r8*rxt(k,524)*y(k,231)
         mat(k,839) = .340_r8*rxt(k,566)*y(k,155)
         mat(k,571) = .170_r8*rxt(k,569)*y(k,155)
         mat(k,1889) = -((rxt(k,202) + rxt(k,203) + rxt(k,204)) * y(k,237) + rxt(k,205) &
                      *y(k,165) + rxt(k,208)*y(k,166))
         mat(k,2412) = -(rxt(k,202) + rxt(k,203) + rxt(k,204)) * y(k,94)
         mat(k,1774) = -rxt(k,205)*y(k,94)
         mat(k,2016) = -rxt(k,208)*y(k,94)
         mat(k,2472) = rxt(k,391)*y(k,259)
         mat(k,1644) = rxt(k,405)*y(k,255)
         mat(k,2279) = rxt(k,246)*y(k,95)
         mat(k,985) = rxt(k,347)*y(k,95)
         mat(k,1610) = rxt(k,246)*y(k,72) + rxt(k,347)*y(k,91) + rxt(k,200)*y(k,164) &
                      + rxt(k,183)*y(k,255) + rxt(k,209)*y(k,259)
         mat(k,1583) = rxt(k,306)*y(k,255)
         mat(k,1821) = rxt(k,279)*y(k,255)
         mat(k,1109) = rxt(k,232)*y(k,259)
         mat(k,2671) = rxt(k,200)*y(k,95) + rxt(k,212)*y(k,259)
         mat(k,413) = rxt(k,602)*y(k,259)
         mat(k,826) = rxt(k,608)*y(k,259)
         mat(k,1622) = rxt(k,613)*y(k,259)
         mat(k,1865) = rxt(k,405)*y(k,66) + rxt(k,183)*y(k,95) + rxt(k,306)*y(k,99) &
                      + rxt(k,279)*y(k,103)
         mat(k,2192) = rxt(k,391)*y(k,53) + rxt(k,209)*y(k,95) + rxt(k,232)*y(k,143) &
                      + rxt(k,212)*y(k,164) + rxt(k,602)*y(k,169) + rxt(k,608) &
                      *y(k,181) + rxt(k,613)*y(k,183)
         mat(k,1605) = -(rxt(k,183)*y(k,255) + rxt(k,200)*y(k,164) + rxt(k,209) &
                      *y(k,259) + rxt(k,246)*y(k,72) + rxt(k,347)*y(k,91))
         mat(k,1856) = -rxt(k,183)*y(k,95)
         mat(k,2661) = -rxt(k,200)*y(k,95)
         mat(k,2182) = -rxt(k,209)*y(k,95)
         mat(k,2270) = -rxt(k,246)*y(k,95)
         mat(k,982) = -rxt(k,347)*y(k,95)
         mat(k,1638) = rxt(k,406)*y(k,255)
         mat(k,1882) = rxt(k,202)*y(k,237)
         mat(k,2403) = rxt(k,202)*y(k,94)
         mat(k,1856) = mat(k,1856) + rxt(k,406)*y(k,66)
         mat(k,93) = -(rxt(k,304)*y(k,255))
         mat(k,1835) = -rxt(k,304)*y(k,96)
         mat(k,672) = -(rxt(k,201)*y(k,164) + rxt(k,210)*y(k,259) + rxt(k,247)*y(k,72))
         mat(k,2646) = -rxt(k,201)*y(k,97)
         mat(k,2118) = -rxt(k,210)*y(k,97)
         mat(k,2254) = -rxt(k,247)*y(k,97)
         mat(k,2355) = 2.000_r8*rxt(k,216)*y(k,237)
         mat(k,2118) = mat(k,2118) + 2.000_r8*rxt(k,215)*y(k,259)
         mat(k,282) = rxt(k,615)*y(k,272)
         mat(k,2790) = rxt(k,615)*y(k,185)
         mat(k,1578) = -(rxt(k,299)*y(k,164) + rxt(k,300)*y(k,259) + (rxt(k,305) &
                      + rxt(k,306)) * y(k,255) + (rxt(k,622) + rxt(k,685) + rxt(k,693) &
                      + rxt(k,702)) * y(k,111) + (rxt(k,623) + rxt(k,683) + rxt(k,696) &
                      + rxt(k,705)) * y(k,110) + (rxt(k,630) + rxt(k,712) + rxt(k,716) &
                      + rxt(k,720)) * y(k,112))
         mat(k,2659) = -rxt(k,299)*y(k,99)
         mat(k,2180) = -rxt(k,300)*y(k,99)
         mat(k,1855) = -(rxt(k,305) + rxt(k,306)) * y(k,99)
         mat(k,1791) = -(rxt(k,622) + rxt(k,685) + rxt(k,693) + rxt(k,702)) * y(k,99)
         mat(k,1681) = -(rxt(k,623) + rxt(k,683) + rxt(k,696) + rxt(k,705)) * y(k,99)
         mat(k,1658) = -(rxt(k,630) + rxt(k,712) + rxt(k,716) + rxt(k,720)) * y(k,99)
         mat(k,2302) = rxt(k,283)*y(k,53) + rxt(k,284)*y(k,237)
         mat(k,2462) = rxt(k,283)*y(k,19)
         mat(k,2401) = rxt(k,284)*y(k,19)
         mat(k,242) = -(rxt(k,380)*y(k,259) + rxt(k,385)*y(k,255))
         mat(k,2059) = -rxt(k,380)*y(k,100)
         mat(k,1845) = -rxt(k,385)*y(k,100)
         mat(k,251) = -(rxt(k,381)*y(k,259) + rxt(k,386)*y(k,255))
         mat(k,2061) = -rxt(k,381)*y(k,101)
         mat(k,1847) = -rxt(k,386)*y(k,101)
         mat(k,308) = -(rxt(k,382)*y(k,259) + rxt(k,387)*y(k,255))
         mat(k,2071) = -rxt(k,382)*y(k,102)
         mat(k,1850) = -rxt(k,387)*y(k,102)
         mat(k,1819) = -(rxt(k,266)*y(k,164) + rxt(k,267)*y(k,259) + (rxt(k,278) &
                      + rxt(k,279)) * y(k,255) + (rxt(k,624) + rxt(k,681) + rxt(k,692) &
                      + rxt(k,701)) * y(k,111) + (rxt(k,625) + rxt(k,682) + rxt(k,695) &
                      + rxt(k,704)) * y(k,110) + (rxt(k,629) + rxt(k,711) + rxt(k,715) &
                      + rxt(k,719)) * y(k,112) + rxt(k,640)*y(k,145) + (rxt(k,691) &
                      + rxt(k,700) + rxt(k,709)) * y(k,77))
         mat(k,2669) = -rxt(k,266)*y(k,103)
         mat(k,2190) = -rxt(k,267)*y(k,103)
         mat(k,1863) = -(rxt(k,278) + rxt(k,279)) * y(k,103)
         mat(k,1796) = -(rxt(k,624) + rxt(k,681) + rxt(k,692) + rxt(k,701)) * y(k,103)
         mat(k,1686) = -(rxt(k,625) + rxt(k,682) + rxt(k,695) + rxt(k,704)) * y(k,103)
         mat(k,1663) = -(rxt(k,629) + rxt(k,711) + rxt(k,715) + rxt(k,719)) * y(k,103)
         mat(k,394) = -rxt(k,640)*y(k,103)
         mat(k,1117) = -(rxt(k,691) + rxt(k,700) + rxt(k,709)) * y(k,103)
         mat(k,274) = rxt(k,354)*y(k,72)
         mat(k,322) = rxt(k,419)*y(k,72)
         mat(k,540) = rxt(k,356)*y(k,72)
         mat(k,328) = rxt(k,359)*y(k,72)
         mat(k,2470) = rxt(k,244)*y(k,72)
         mat(k,681) = rxt(k,361)*y(k,72)
         mat(k,422) = 2.000_r8*rxt(k,364)*y(k,72)
         mat(k,386) = rxt(k,366)*y(k,72)
         mat(k,1642) = rxt(k,245)*y(k,72)
         mat(k,458) = rxt(k,369)*y(k,72)
         mat(k,377) = rxt(k,378)*y(k,72)
         mat(k,2277) = rxt(k,354)*y(k,31) + rxt(k,419)*y(k,34) + rxt(k,356)*y(k,47) &
                      + rxt(k,359)*y(k,49) + rxt(k,244)*y(k,53) + rxt(k,361)*y(k,54) &
                      + 2.000_r8*rxt(k,364)*y(k,57) + rxt(k,366)*y(k,63) + rxt(k,245) &
                      *y(k,66) + rxt(k,369)*y(k,68) + rxt(k,378)*y(k,71) + rxt(k,597) &
                      *y(k,85) + rxt(k,246)*y(k,95) + rxt(k,247)*y(k,97) + rxt(k,268) &
                      *y(k,111) + rxt(k,248)*y(k,237)
         mat(k,2699) = rxt(k,265)*y(k,259)
         mat(k,1127) = rxt(k,597)*y(k,72)
         mat(k,1608) = rxt(k,246)*y(k,72)
         mat(k,673) = rxt(k,247)*y(k,72)
         mat(k,1796) = mat(k,1796) + rxt(k,268)*y(k,72)
         mat(k,2410) = rxt(k,248)*y(k,72)
         mat(k,2190) = mat(k,2190) + rxt(k,265)*y(k,76)
         mat(k,187) = -(rxt(k,399)*y(k,259) + rxt(k,407)*y(k,255))
         mat(k,2051) = -rxt(k,399)*y(k,104)
         mat(k,1844) = -rxt(k,407)*y(k,104)
         mat(k,1011) = -(rxt(k,400)*y(k,259))
         mat(k,2144) = -rxt(k,400)*y(k,105)
         mat(k,1050) = .050_r8*rxt(k,573)*y(k,166)
         mat(k,315) = .350_r8*rxt(k,410)*y(k,259)
         mat(k,629) = .370_r8*rxt(k,412)*y(k,166)
         mat(k,1184) = .120_r8*rxt(k,441)*y(k,166)
         mat(k,941) = .110_r8*rxt(k,518)*y(k,166)
         mat(k,1382) = .330_r8*rxt(k,471)*y(k,166)
         mat(k,1023) = .050_r8*rxt(k,576)*y(k,166)
         mat(k,1485) = .120_r8*rxt(k,485)*y(k,166)
         mat(k,2527) = rxt(k,403)*y(k,238)
         mat(k,1981) = .050_r8*rxt(k,573)*y(k,8) + .370_r8*rxt(k,412)*y(k,30) &
                      + .120_r8*rxt(k,441)*y(k,35) + .110_r8*rxt(k,518)*y(k,129) &
                      + .330_r8*rxt(k,471)*y(k,136) + .050_r8*rxt(k,576)*y(k,141) &
                      + .120_r8*rxt(k,485)*y(k,142)
         mat(k,2374) = rxt(k,401)*y(k,238)
         mat(k,490) = rxt(k,403)*y(k,155) + rxt(k,401)*y(k,237)
         mat(k,2144) = mat(k,2144) + .350_r8*rxt(k,410)*y(k,29)
         mat(k,1634) = rxt(k,346)*y(k,91)
         mat(k,980) = rxt(k,346)*y(k,66) + rxt(k,347)*y(k,95) + rxt(k,349)*y(k,108) &
                      + rxt(k,348)*y(k,272)
         mat(k,1603) = rxt(k,347)*y(k,91)
         mat(k,2582) = rxt(k,349)*y(k,91)
         mat(k,2792) = rxt(k,348)*y(k,91)
         mat(k,1329) = -(rxt(k,307)*y(k,157) + rxt(k,335)*y(k,259) + (rxt(k,626) &
                      + rxt(k,686) + rxt(k,694) + rxt(k,703)) * y(k,111) + (rxt(k,627) &
                      + rxt(k,684) + rxt(k,697) + rxt(k,706)) * y(k,110) + (rxt(k,631) &
                      + rxt(k,713) + rxt(k,717) + rxt(k,721)) * y(k,112))
         mat(k,2751) = -rxt(k,307)*y(k,107)
         mat(k,2168) = -rxt(k,335)*y(k,107)
         mat(k,1790) = -(rxt(k,626) + rxt(k,686) + rxt(k,694) + rxt(k,703)) * y(k,107)
         mat(k,1680) = -(rxt(k,627) + rxt(k,684) + rxt(k,697) + rxt(k,706)) * y(k,107)
         mat(k,1657) = -(rxt(k,631) + rxt(k,713) + rxt(k,717) + rxt(k,721)) * y(k,107)
         mat(k,2214) = rxt(k,313)*y(k,237)
         mat(k,2389) = rxt(k,313)*y(k,117)
         mat(k,2602) = -(rxt(k,241)*y(k,259) + rxt(k,349)*y(k,91))
         mat(k,2203) = -rxt(k,241)*y(k,108)
         mat(k,987) = -rxt(k,349)*y(k,108)
         mat(k,2483) = rxt(k,389)*y(k,157)
         mat(k,1209) = rxt(k,421)*y(k,157)
         mat(k,1401) = rxt(k,447)*y(k,157)
         mat(k,1121) = (rxt(k,691)+rxt(k,700)+rxt(k,709))*y(k,103)
         mat(k,1134) = rxt(k,599)*y(k,157)
         mat(k,1828) = (rxt(k,691)+rxt(k,700)+rxt(k,709))*y(k,77) + rxt(k,640) &
                      *y(k,145)
         mat(k,1339) = rxt(k,307)*y(k,157)
         mat(k,1672) = rxt(k,337)*y(k,157)
         mat(k,397) = rxt(k,640)*y(k,103)
         mat(k,1958) = rxt(k,240)*y(k,259)
         mat(k,2784) = rxt(k,389)*y(k,53) + rxt(k,421)*y(k,56) + rxt(k,447)*y(k,60) &
                      + rxt(k,599)*y(k,85) + rxt(k,307)*y(k,107) + rxt(k,337)*y(k,112)
         mat(k,2203) = mat(k,2203) + rxt(k,240)*y(k,156)
         mat(k,532) = -(rxt(k,217)*y(k,259))
         mat(k,2101) = -rxt(k,217)*y(k,109)
         mat(k,1915) = rxt(k,238)*y(k,237)
         mat(k,2348) = rxt(k,238)*y(k,156)
         mat(k,1684) = -(rxt(k,301)*y(k,164) + (rxt(k,623) + rxt(k,683) + rxt(k,696) &
                      + rxt(k,705)) * y(k,99) + (rxt(k,625) + rxt(k,682) + rxt(k,695) &
                      + rxt(k,704)) * y(k,103) + (rxt(k,627) + rxt(k,684) + rxt(k,697) &
                      + rxt(k,706)) * y(k,107))
         mat(k,2665) = -rxt(k,301)*y(k,110)
         mat(k,1580) = -(rxt(k,623) + rxt(k,683) + rxt(k,696) + rxt(k,705)) * y(k,110)
         mat(k,1817) = -(rxt(k,625) + rxt(k,682) + rxt(k,695) + rxt(k,704)) * y(k,110)
         mat(k,1332) = -(rxt(k,627) + rxt(k,684) + rxt(k,697) + rxt(k,706)) * y(k,110)
         mat(k,522) = rxt(k,282)*y(k,259)
         mat(k,2436) = rxt(k,291)*y(k,237)
         mat(k,2406) = rxt(k,291)*y(k,23)
         mat(k,2186) = rxt(k,282)*y(k,20)
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
         mat(k,1795) = -(rxt(k,268)*y(k,72) + rxt(k,269)*y(k,164) + rxt(k,270) &
                      *y(k,259) + (rxt(k,622) + rxt(k,685) + rxt(k,693) + rxt(k,702) &
                      ) * y(k,99) + (rxt(k,624) + rxt(k,681) + rxt(k,692) + rxt(k,701) &
                      ) * y(k,103) + (rxt(k,626) + rxt(k,686) + rxt(k,694) + rxt(k,703) &
                      ) * y(k,107))
         mat(k,2276) = -rxt(k,268)*y(k,111)
         mat(k,2668) = -rxt(k,269)*y(k,111)
         mat(k,2189) = -rxt(k,270)*y(k,111)
         mat(k,1581) = -(rxt(k,622) + rxt(k,685) + rxt(k,693) + rxt(k,702)) * y(k,111)
         mat(k,1818) = -(rxt(k,624) + rxt(k,681) + rxt(k,692) + rxt(k,701)) * y(k,111)
         mat(k,1333) = -(rxt(k,626) + rxt(k,686) + rxt(k,694) + rxt(k,703)) * y(k,111)
         mat(k,1173) = rxt(k,375)*y(k,237)
         mat(k,579) = rxt(k,251)*y(k,259)
         mat(k,2698) = rxt(k,257)*y(k,237)
         mat(k,1116) = rxt(k,262)*y(k,259)
         mat(k,2409) = rxt(k,375)*y(k,70) + rxt(k,257)*y(k,76)
         mat(k,2189) = mat(k,2189) + rxt(k,251)*y(k,75) + rxt(k,262)*y(k,77)
         mat(k,1660) = -(rxt(k,308)*y(k,259) + rxt(k,337)*y(k,157) + (rxt(k,629) &
                      + rxt(k,711) + rxt(k,715) + rxt(k,719)) * y(k,103) + (rxt(k,630) &
                      + rxt(k,712) + rxt(k,716) + rxt(k,720)) * y(k,99) + (rxt(k,631) &
                      + rxt(k,713) + rxt(k,717) + rxt(k,721)) * y(k,107))
         mat(k,2185) = -rxt(k,308)*y(k,112)
         mat(k,2766) = -rxt(k,337)*y(k,112)
         mat(k,1816) = -(rxt(k,629) + rxt(k,711) + rxt(k,715) + rxt(k,719)) * y(k,112)
         mat(k,1579) = -(rxt(k,630) + rxt(k,712) + rxt(k,716) + rxt(k,720)) * y(k,112)
         mat(k,1331) = -(rxt(k,631) + rxt(k,713) + rxt(k,717) + rxt(k,721)) * y(k,112)
         mat(k,1593) = rxt(k,311)*y(k,259)
         mat(k,2618) = rxt(k,328)*y(k,237)
         mat(k,2405) = rxt(k,328)*y(k,127)
         mat(k,2185) = mat(k,2185) + rxt(k,311)*y(k,118)
         mat(k,1242) = -(rxt(k,464)*y(k,259))
         mat(k,2161) = -rxt(k,464)*y(k,113)
         mat(k,644) = .300_r8*rxt(k,509)*y(k,259)
         mat(k,613) = .500_r8*rxt(k,510)*y(k,259)
         mat(k,2538) = rxt(k,463)*y(k,234) + rxt(k,470)*y(k,244)
         mat(k,638) = rxt(k,463)*y(k,155)
         mat(k,1466) = rxt(k,470)*y(k,155)
         mat(k,2161) = mat(k,2161) + .300_r8*rxt(k,509)*y(k,130) + .500_r8*rxt(k,510) &
                      *y(k,131)
         mat(k,259) = -(rxt(k,495)*y(k,259))
         mat(k,2063) = -rxt(k,495)*y(k,114)
         mat(k,1256) = -(rxt(k,449)*y(k,259))
         mat(k,2162) = -rxt(k,449)*y(k,115)
         mat(k,645) = .700_r8*rxt(k,509)*y(k,259)
         mat(k,614) = .500_r8*rxt(k,510)*y(k,259)
         mat(k,664) = .500_r8*rxt(k,484)*y(k,259)
         mat(k,2539) = .050_r8*rxt(k,507)*y(k,240) + .220_r8*rxt(k,469)*y(k,244) &
                      + .250_r8*rxt(k,526)*y(k,268)
         mat(k,2745) = .050_r8*rxt(k,508)*y(k,240) + .220_r8*rxt(k,468)*y(k,244) &
                      + .250_r8*rxt(k,527)*y(k,268)
         mat(k,622) = .500_r8*rxt(k,453)*y(k,259)
         mat(k,1532) = .220_r8*rxt(k,465)*y(k,244) + .250_r8*rxt(k,523)*y(k,268)
         mat(k,1719) = .230_r8*rxt(k,466)*y(k,244) + .200_r8*rxt(k,454)*y(k,263) &
                      + .100_r8*rxt(k,524)*y(k,268)
         mat(k,1441) = .050_r8*rxt(k,507)*y(k,155) + .050_r8*rxt(k,508)*y(k,157)
         mat(k,1467) = .220_r8*rxt(k,469)*y(k,155) + .220_r8*rxt(k,468)*y(k,157) &
                      + .220_r8*rxt(k,465)*y(k,230) + .230_r8*rxt(k,466)*y(k,231)
         mat(k,2162) = mat(k,2162) + .700_r8*rxt(k,509)*y(k,130) + .500_r8*rxt(k,510) &
                      *y(k,131) + .500_r8*rxt(k,484)*y(k,140) + .500_r8*rxt(k,453) &
                      *y(k,179)
         mat(k,1316) = .200_r8*rxt(k,454)*y(k,231)
         mat(k,1345) = .250_r8*rxt(k,526)*y(k,155) + .250_r8*rxt(k,527)*y(k,157) &
                      + .250_r8*rxt(k,523)*y(k,230) + .100_r8*rxt(k,524)*y(k,231)
         mat(k,381) = -(rxt(k,496)*y(k,259))
         mat(k,2082) = -rxt(k,496)*y(k,116)
         mat(k,2494) = .870_r8*rxt(k,507)*y(k,240)
         mat(k,2720) = .950_r8*rxt(k,508)*y(k,240)
         mat(k,1526) = rxt(k,503)*y(k,240)
         mat(k,1702) = .750_r8*rxt(k,504)*y(k,240)
         mat(k,1432) = .870_r8*rxt(k,507)*y(k,155) + .950_r8*rxt(k,508)*y(k,157) &
                      + rxt(k,503)*y(k,230) + .750_r8*rxt(k,504)*y(k,231)
         mat(k,2226) = -(rxt(k,312)*y(k,23) + rxt(k,313)*y(k,237) + rxt(k,314) &
                      *y(k,128) + rxt(k,316)*y(k,156) + rxt(k,318)*y(k,157) + rxt(k,320) &
                      *y(k,155) + rxt(k,321)*y(k,166))
         mat(k,2445) = -rxt(k,312)*y(k,117)
         mat(k,2416) = -rxt(k,313)*y(k,117)
         mat(k,974) = -rxt(k,314)*y(k,117)
         mat(k,1951) = -rxt(k,316)*y(k,117)
         mat(k,2777) = -rxt(k,318)*y(k,117)
         mat(k,2569) = -rxt(k,320)*y(k,117)
         mat(k,2020) = -rxt(k,321)*y(k,117)
         mat(k,2313) = rxt(k,322)*y(k,127)
         mat(k,2445) = mat(k,2445) + rxt(k,323)*y(k,127)
         mat(k,388) = rxt(k,366)*y(k,72) + rxt(k,367)*y(k,259)
         mat(k,2283) = rxt(k,366)*y(k,63)
         mat(k,2705) = (rxt(k,325)+rxt(k,326))*y(k,127)
         mat(k,1129) = rxt(k,598)*y(k,127)
         mat(k,1336) = rxt(k,307)*y(k,157) + rxt(k,335)*y(k,259)
         mat(k,1596) = rxt(k,309)*y(k,157) + rxt(k,310)*y(k,164) + rxt(k,311)*y(k,259)
         mat(k,2628) = rxt(k,322)*y(k,19) + rxt(k,323)*y(k,23) + (rxt(k,325) &
                       +rxt(k,326))*y(k,76) + rxt(k,598)*y(k,85) + 2.000_r8*rxt(k,341) &
                      *y(k,127) + rxt(k,329)*y(k,155) + rxt(k,332)*y(k,164) &
                      + rxt(k,334)*y(k,259)
         mat(k,2569) = mat(k,2569) + rxt(k,329)*y(k,127)
         mat(k,2777) = mat(k,2777) + rxt(k,307)*y(k,107) + rxt(k,309)*y(k,118)
         mat(k,2675) = rxt(k,310)*y(k,118) + rxt(k,332)*y(k,127)
         mat(k,2196) = rxt(k,367)*y(k,63) + rxt(k,335)*y(k,107) + rxt(k,311)*y(k,118) &
                      + rxt(k,334)*y(k,127)
         mat(k,1592) = -(rxt(k,309)*y(k,157) + rxt(k,310)*y(k,164) + rxt(k,311) &
                      *y(k,259))
         mat(k,2763) = -rxt(k,309)*y(k,118)
         mat(k,2660) = -rxt(k,310)*y(k,118)
         mat(k,2181) = -rxt(k,311)*y(k,118)
         mat(k,1330) = (rxt(k,631)+rxt(k,713)+rxt(k,717)+rxt(k,721))*y(k,112)
         mat(k,1659) = (rxt(k,631)+rxt(k,713)+rxt(k,717)+rxt(k,721))*y(k,107)
         mat(k,2215) = rxt(k,314)*y(k,128)
         mat(k,192) = 2.000_r8*rxt(k,319)*y(k,125)
         mat(k,301) = 2.000_r8*rxt(k,315)*y(k,126)
         mat(k,971) = rxt(k,314)*y(k,117)
         mat(k,2608) = 2.000_r8*rxt(k,342)*y(k,127)
         mat(k,2609) = rxt(k,344)*y(k,170)
         mat(k,689) = rxt(k,344)*y(k,127)
         mat(k,688) = 2.000_r8*rxt(k,345)*y(k,170)
         mat(k,1575) = (rxt(k,630)+rxt(k,712)+rxt(k,716)+rxt(k,720))*y(k,112)
         mat(k,1327) = (rxt(k,627)+rxt(k,684)+rxt(k,697)+rxt(k,706))*y(k,110)
         mat(k,1677) = (rxt(k,627)+rxt(k,684)+rxt(k,697)+rxt(k,706))*y(k,107)
         mat(k,1655) = (rxt(k,630)+rxt(k,712)+rxt(k,716)+rxt(k,720))*y(k,99)
         mat(k,2691) = rxt(k,327)*y(k,127)
         mat(k,1812) = (rxt(k,629)+rxt(k,711)+rxt(k,715)+rxt(k,719))*y(k,112)
         mat(k,1328) = (rxt(k,626)+rxt(k,686)+rxt(k,694)+rxt(k,703))*y(k,111)
         mat(k,1788) = (rxt(k,626)+rxt(k,686)+rxt(k,694)+rxt(k,703))*y(k,107)
         mat(k,1656) = (rxt(k,629)+rxt(k,711)+rxt(k,715)+rxt(k,719))*y(k,103)
         mat(k,2611) = rxt(k,327)*y(k,76)
         mat(k,134) = -(rxt(k,497)*y(k,259))
         mat(k,2047) = -rxt(k,497)*y(k,124)
         mat(k,779) = .600_r8*rxt(k,520)*y(k,259)
         mat(k,2047) = mat(k,2047) + .600_r8*rxt(k,520)*y(k,133)
         mat(k,191) = -(4._r8*rxt(k,319)*y(k,125))
         mat(k,2209) = rxt(k,320)*y(k,155)
         mat(k,2489) = rxt(k,320)*y(k,117)
         mat(k,298) = -(4._r8*rxt(k,315)*y(k,126))
         mat(k,2210) = rxt(k,316)*y(k,156)
         mat(k,1908) = rxt(k,316)*y(k,117)
         mat(k,2636) = -(rxt(k,322)*y(k,19) + (rxt(k,323) + rxt(k,324)) * y(k,23) &
                      + (rxt(k,325) + rxt(k,326) + rxt(k,327)) * y(k,76) + rxt(k,328) &
                      *y(k,237) + rxt(k,329)*y(k,155) + rxt(k,330)*y(k,156) + rxt(k,331) &
                      *y(k,157) + rxt(k,332)*y(k,164) + rxt(k,333)*y(k,166) + rxt(k,334) &
                      *y(k,259) + (4._r8*rxt(k,341) + 4._r8*rxt(k,342)) * y(k,127) &
                      + rxt(k,344)*y(k,170) + rxt(k,598)*y(k,85))
         mat(k,2321) = -rxt(k,322)*y(k,127)
         mat(k,2453) = -(rxt(k,323) + rxt(k,324)) * y(k,127)
         mat(k,2713) = -(rxt(k,325) + rxt(k,326) + rxt(k,327)) * y(k,127)
         mat(k,2424) = -rxt(k,328)*y(k,127)
         mat(k,2577) = -rxt(k,329)*y(k,127)
         mat(k,1959) = -rxt(k,330)*y(k,127)
         mat(k,2785) = -rxt(k,331)*y(k,127)
         mat(k,2683) = -rxt(k,332)*y(k,127)
         mat(k,2028) = -rxt(k,333)*y(k,127)
         mat(k,2204) = -rxt(k,334)*y(k,127)
         mat(k,695) = -rxt(k,344)*y(k,127)
         mat(k,1135) = -rxt(k,598)*y(k,127)
         mat(k,2453) = mat(k,2453) + rxt(k,312)*y(k,117)
         mat(k,1673) = rxt(k,337)*y(k,157) + rxt(k,308)*y(k,259)
         mat(k,2234) = rxt(k,312)*y(k,23) + rxt(k,318)*y(k,157) + rxt(k,321)*y(k,166)
         mat(k,1600) = rxt(k,310)*y(k,164)
         mat(k,2577) = mat(k,2577) + rxt(k,336)*y(k,170)
         mat(k,2785) = mat(k,2785) + rxt(k,337)*y(k,112) + rxt(k,318)*y(k,117)
         mat(k,2683) = mat(k,2683) + rxt(k,310)*y(k,118)
         mat(k,2028) = mat(k,2028) + rxt(k,321)*y(k,117)
         mat(k,695) = mat(k,695) + rxt(k,336)*y(k,155)
         mat(k,2204) = mat(k,2204) + rxt(k,308)*y(k,112)
         mat(k,970) = -(rxt(k,314)*y(k,117))
         mat(k,2213) = -rxt(k,314)*y(k,128)
         mat(k,1591) = rxt(k,309)*y(k,157)
         mat(k,2613) = rxt(k,330)*y(k,156)
         mat(k,1925) = rxt(k,330)*y(k,127)
         mat(k,2728) = rxt(k,309)*y(k,118)
         mat(k,940) = -(rxt(k,511)*y(k,157) + rxt(k,518)*y(k,166) + rxt(k,519) &
                      *y(k,259))
         mat(k,2727) = -rxt(k,511)*y(k,129)
         mat(k,1980) = -rxt(k,518)*y(k,129)
         mat(k,2140) = -rxt(k,519)*y(k,129)
         mat(k,643) = -(rxt(k,509)*y(k,259))
         mat(k,2114) = -rxt(k,509)*y(k,130)
         mat(k,2507) = .080_r8*rxt(k,501)*y(k,239)
         mat(k,1404) = .080_r8*rxt(k,501)*y(k,155)
         mat(k,611) = -(rxt(k,510)*y(k,259))
         mat(k,2110) = -rxt(k,510)*y(k,131)
         mat(k,2505) = .080_r8*rxt(k,507)*y(k,240)
         mat(k,1433) = .080_r8*rxt(k,507)*y(k,155)
         mat(k,502) = -(rxt(k,517)*y(k,259))
         mat(k,2097) = -rxt(k,517)*y(k,132)
         mat(k,2344) = rxt(k,514)*y(k,241)
         mat(k,1360) = rxt(k,514)*y(k,237)
         mat(k,780) = -(rxt(k,520)*y(k,259))
         mat(k,2128) = -rxt(k,520)*y(k,133)
         mat(k,2362) = rxt(k,500)*y(k,239) + rxt(k,505)*y(k,240)
         mat(k,1405) = rxt(k,500)*y(k,237)
         mat(k,1435) = rxt(k,505)*y(k,237)
         mat(k,56) = -(rxt(k,672)*y(k,259))
         mat(k,2038) = -rxt(k,672)*y(k,134)
         mat(k,1384) = -(rxt(k,471)*y(k,166) + rxt(k,472)*y(k,259))
         mat(k,2000) = -rxt(k,471)*y(k,136)
         mat(k,2171) = -rxt(k,472)*y(k,136)
         mat(k,945) = .300_r8*rxt(k,518)*y(k,166)
         mat(k,2547) = .360_r8*rxt(k,501)*y(k,239)
         mat(k,2754) = .400_r8*rxt(k,502)*y(k,239)
         mat(k,2000) = mat(k,2000) + .300_r8*rxt(k,518)*y(k,129)
         mat(k,1537) = .390_r8*rxt(k,498)*y(k,239)
         mat(k,1727) = .310_r8*rxt(k,499)*y(k,239)
         mat(k,1413) = .360_r8*rxt(k,501)*y(k,155) + .400_r8*rxt(k,502)*y(k,157) &
                      + .390_r8*rxt(k,498)*y(k,230) + .310_r8*rxt(k,499)*y(k,231)
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
         mat(k,332) = -(rxt(k,473)*y(k,259))
         mat(k,2075) = -rxt(k,473)*y(k,137)
         mat(k,2329) = rxt(k,467)*y(k,244)
         mat(k,1464) = rxt(k,467)*y(k,237)
         mat(k,583) = -(rxt(k,482)*y(k,259))
         mat(k,2107) = -rxt(k,482)*y(k,138)
         mat(k,2503) = .800_r8*rxt(k,491)*y(k,222)
         mat(k,1070) = .800_r8*rxt(k,491)*y(k,155)
         mat(k,337) = -(rxt(k,483)*y(k,259))
         mat(k,2076) = -rxt(k,483)*y(k,139)
         mat(k,2330) = .800_r8*rxt(k,480)*y(k,248)
         mat(k,755) = .800_r8*rxt(k,480)*y(k,237)
         mat(k,663) = -(rxt(k,484)*y(k,259))
         mat(k,2117) = -rxt(k,484)*y(k,140)
         mat(k,1920) = rxt(k,487)*y(k,246)
         mat(k,1509) = rxt(k,487)*y(k,156)
         mat(k,1024) = -(rxt(k,575)*y(k,157) + rxt(k,576)*y(k,166) + rxt(k,577) &
                      *y(k,259))
         mat(k,2730) = -rxt(k,575)*y(k,141)
         mat(k,1982) = -rxt(k,576)*y(k,141)
         mat(k,2145) = -rxt(k,577)*y(k,141)
         mat(k,1492) = -(rxt(k,485)*y(k,166) + rxt(k,486)*y(k,259))
         mat(k,2005) = -rxt(k,485)*y(k,142)
         mat(k,2176) = -rxt(k,486)*y(k,142)
         mat(k,948) = .200_r8*rxt(k,518)*y(k,166)
         mat(k,2552) = .560_r8*rxt(k,501)*y(k,239)
         mat(k,2759) = .600_r8*rxt(k,502)*y(k,239)
         mat(k,2005) = mat(k,2005) + .200_r8*rxt(k,518)*y(k,129)
         mat(k,1542) = .610_r8*rxt(k,498)*y(k,239)
         mat(k,1732) = .440_r8*rxt(k,499)*y(k,239)
         mat(k,1417) = .560_r8*rxt(k,501)*y(k,155) + .600_r8*rxt(k,502)*y(k,157) &
                      + .610_r8*rxt(k,498)*y(k,230) + .440_r8*rxt(k,499)*y(k,231)
         mat(k,1104) = -(rxt(k,220)*y(k,155) + (rxt(k,221) + rxt(k,222) + rxt(k,223) &
                      ) * y(k,156) + rxt(k,224)*y(k,165) + rxt(k,232)*y(k,259) &
                      + rxt(k,731)*y(k,258))
         mat(k,2531) = -rxt(k,220)*y(k,143)
         mat(k,1928) = -(rxt(k,221) + rxt(k,222) + rxt(k,223)) * y(k,143)
         mat(k,1766) = -rxt(k,224)*y(k,143)
         mat(k,2150) = -rxt(k,232)*y(k,143)
         mat(k,911) = -rxt(k,731)*y(k,143)
         mat(k,2654) = rxt(k,218)*y(k,250) + rxt(k,728)*y(k,253)
         mat(k,1766) = mat(k,1766) + rxt(k,729)*y(k,253)
         mat(k,922) = 1.100_r8*rxt(k,724)*y(k,251) + .200_r8*rxt(k,722)*y(k,252)
         mat(k,599) = rxt(k,218)*y(k,164)
         mat(k,769) = 1.100_r8*rxt(k,724)*y(k,233)
         mat(k,903) = .200_r8*rxt(k,722)*y(k,233)
         mat(k,566) = rxt(k,728)*y(k,164) + rxt(k,729)*y(k,165)
         mat(k,278) = -((rxt(k,236) + rxt(k,237)) * y(k,255))
         mat(k,1849) = -(rxt(k,236) + rxt(k,237)) * y(k,144)
         mat(k,1098) = rxt(k,221)*y(k,156)
         mat(k,1907) = rxt(k,221)*y(k,143)
         mat(k,1910) = rxt(k,239)*y(k,157)
         mat(k,2721) = rxt(k,239)*y(k,156)
         mat(k,427) = -(rxt(k,521)*y(k,259))
         mat(k,2087) = -rxt(k,521)*y(k,146)
         mat(k,1703) = .200_r8*rxt(k,513)*y(k,241)
         mat(k,1359) = .200_r8*rxt(k,513)*y(k,231)
         mat(k,1156) = -(rxt(k,522)*y(k,259))
         mat(k,2155) = -rxt(k,522)*y(k,147)
         mat(k,2533) = rxt(k,515)*y(k,241)
         mat(k,2738) = rxt(k,516)*y(k,241)
         mat(k,1531) = rxt(k,512)*y(k,241)
         mat(k,1714) = .800_r8*rxt(k,513)*y(k,241)
         mat(k,1364) = rxt(k,515)*y(k,155) + rxt(k,516)*y(k,157) + rxt(k,512)*y(k,230) &
                      + .800_r8*rxt(k,513)*y(k,231)
         mat(k,79) = -(rxt(k,633)*y(k,259))
         mat(k,2041) = -rxt(k,633)*y(k,151)
         mat(k,2575) = -(rxt(k,220)*y(k,143) + rxt(k,229)*y(k,157) + rxt(k,233) &
                      *y(k,237) + rxt(k,234)*y(k,166) + rxt(k,235)*y(k,164) + rxt(k,258) &
                      *y(k,76) + rxt(k,292)*y(k,23) + rxt(k,320)*y(k,117) + rxt(k,329) &
                      *y(k,127) + rxt(k,336)*y(k,170) + rxt(k,376)*y(k,70) + rxt(k,395) &
                      *y(k,231) + rxt(k,403)*y(k,238) + rxt(k,416)*y(k,227) + rxt(k,427) &
                      *y(k,230) + rxt(k,431)*y(k,236) + rxt(k,444)*y(k,228) + rxt(k,452) &
                      *y(k,262) + rxt(k,456)*y(k,263) + (rxt(k,462) + rxt(k,463) &
                      ) * y(k,234) + (rxt(k,469) + rxt(k,470)) * y(k,244) + rxt(k,478) &
                      *y(k,246) + rxt(k,481)*y(k,248) + (rxt(k,491) + rxt(k,492) &
                      ) * y(k,222) + rxt(k,501)*y(k,239) + rxt(k,507)*y(k,240) &
                      + rxt(k,515)*y(k,241) + rxt(k,526)*y(k,268) + rxt(k,530) &
                      *y(k,221) + rxt(k,533)*y(k,224) + rxt(k,538)*y(k,226) + rxt(k,540) &
                      *y(k,229) + rxt(k,544)*y(k,232) + rxt(k,547)*y(k,245) + rxt(k,550) &
                      *y(k,247) + rxt(k,553)*y(k,261) + rxt(k,560)*y(k,266) + rxt(k,566) &
                      *y(k,269) + rxt(k,569)*y(k,271) + rxt(k,580)*y(k,254) + rxt(k,585) &
                      *y(k,264) + rxt(k,590)*y(k,265) + rxt(k,733)*y(k,258))
         mat(k,1112) = -rxt(k,220)*y(k,155)
         mat(k,2783) = -rxt(k,229)*y(k,155)
         mat(k,2422) = -rxt(k,233)*y(k,155)
         mat(k,2026) = -rxt(k,234)*y(k,155)
         mat(k,2681) = -rxt(k,235)*y(k,155)
         mat(k,2711) = -rxt(k,258)*y(k,155)
         mat(k,2451) = -rxt(k,292)*y(k,155)
         mat(k,2232) = -rxt(k,320)*y(k,155)
         mat(k,2634) = -rxt(k,329)*y(k,155)
         mat(k,694) = -rxt(k,336)*y(k,155)
         mat(k,1178) = -rxt(k,376)*y(k,155)
         mat(k,1749) = -rxt(k,395)*y(k,155)
         mat(k,494) = -rxt(k,403)*y(k,155)
         mat(k,1001) = -rxt(k,416)*y(k,155)
         mat(k,1554) = -rxt(k,427)*y(k,155)
         mat(k,864) = -rxt(k,431)*y(k,155)
         mat(k,898) = -rxt(k,444)*y(k,155)
         mat(k,886) = -rxt(k,452)*y(k,155)
         mat(k,1325) = -rxt(k,456)*y(k,155)
         mat(k,642) = -(rxt(k,462) + rxt(k,463)) * y(k,155)
         mat(k,1481) = -(rxt(k,469) + rxt(k,470)) * y(k,155)
         mat(k,1522) = -rxt(k,478)*y(k,155)
         mat(k,762) = -rxt(k,481)*y(k,155)
         mat(k,1085) = -(rxt(k,491) + rxt(k,492)) * y(k,155)
         mat(k,1427) = -rxt(k,501)*y(k,155)
         mat(k,1460) = -rxt(k,507)*y(k,155)
         mat(k,1379) = -rxt(k,515)*y(k,155)
         mat(k,1357) = -rxt(k,526)*y(k,155)
         mat(k,597) = -rxt(k,530)*y(k,155)
         mat(k,561) = -rxt(k,533)*y(k,155)
         mat(k,488) = -rxt(k,538)*y(k,155)
         mat(k,709) = -rxt(k,540)*y(k,155)
         mat(k,854) = -rxt(k,544)*y(k,155)
         mat(k,807) = -rxt(k,547)*y(k,155)
         mat(k,1010) = -rxt(k,550)*y(k,155)
         mat(k,501) = -rxt(k,553)*y(k,155)
         mat(k,822) = -rxt(k,560)*y(k,155)
         mat(k,846) = -rxt(k,566)*y(k,155)
         mat(k,576) = -rxt(k,569)*y(k,155)
         mat(k,1225) = -rxt(k,580)*y(k,155)
         mat(k,1289) = -rxt(k,585)*y(k,155)
         mat(k,1310) = -rxt(k,590)*y(k,155)
         mat(k,914) = -rxt(k,733)*y(k,155)
         mat(k,194) = 4.000_r8*rxt(k,319)*y(k,125)
         mat(k,1112) = mat(k,1112) + 2.000_r8*rxt(k,222)*y(k,156) + rxt(k,224) &
                      *y(k,165) + rxt(k,232)*y(k,259)
         mat(k,281) = 2.000_r8*rxt(k,236)*y(k,255)
         mat(k,1957) = 2.000_r8*rxt(k,222)*y(k,143) + rxt(k,225)*y(k,164) + rxt(k,609) &
                      *y(k,183)
         mat(k,2681) = mat(k,2681) + rxt(k,225)*y(k,156)
         mat(k,1783) = rxt(k,224)*y(k,143) + rxt(k,219)*y(k,250)
         mat(k,1630) = rxt(k,609)*y(k,156)
         mat(k,602) = rxt(k,219)*y(k,165)
         mat(k,1875) = 2.000_r8*rxt(k,236)*y(k,144)
         mat(k,2202) = rxt(k,232)*y(k,143)
         mat(k,1948) = -((rxt(k,221) + rxt(k,222) + rxt(k,223)) * y(k,143) + (rxt(k,225) &
                      + rxt(k,227)) * y(k,164) + rxt(k,226)*y(k,166) + rxt(k,238) &
                      *y(k,237) + rxt(k,239)*y(k,157) + rxt(k,240)*y(k,259) + rxt(k,250) &
                      *y(k,72) + rxt(k,260)*y(k,76) + rxt(k,285)*y(k,19) + rxt(k,295) &
                      *y(k,23) + rxt(k,316)*y(k,117) + rxt(k,330)*y(k,127) + rxt(k,438) &
                      *y(k,230) + rxt(k,487)*y(k,246) + rxt(k,545)*y(k,232) + rxt(k,548) &
                      *y(k,245) + rxt(k,551)*y(k,247) + rxt(k,555)*y(k,174) + rxt(k,558) &
                      *y(k,221) + rxt(k,609)*y(k,183))
         mat(k,1110) = -(rxt(k,221) + rxt(k,222) + rxt(k,223)) * y(k,156)
         mat(k,2672) = -(rxt(k,225) + rxt(k,227)) * y(k,156)
         mat(k,2017) = -rxt(k,226)*y(k,156)
         mat(k,2413) = -rxt(k,238)*y(k,156)
         mat(k,2774) = -rxt(k,239)*y(k,156)
         mat(k,2193) = -rxt(k,240)*y(k,156)
         mat(k,2280) = -rxt(k,250)*y(k,156)
         mat(k,2702) = -rxt(k,260)*y(k,156)
         mat(k,2310) = -rxt(k,285)*y(k,156)
         mat(k,2442) = -rxt(k,295)*y(k,156)
         mat(k,2223) = -rxt(k,316)*y(k,156)
         mat(k,2625) = -rxt(k,330)*y(k,156)
         mat(k,1549) = -rxt(k,438)*y(k,156)
         mat(k,1517) = -rxt(k,487)*y(k,156)
         mat(k,851) = -rxt(k,545)*y(k,156)
         mat(k,805) = -rxt(k,548)*y(k,156)
         mat(k,1007) = -rxt(k,551)*y(k,156)
         mat(k,527) = -rxt(k,555)*y(k,156)
         mat(k,594) = -rxt(k,558)*y(k,156)
         mat(k,1623) = -rxt(k,609)*y(k,156)
         mat(k,717) = rxt(k,489)*y(k,259)
         mat(k,404) = rxt(k,460)*y(k,157)
         mat(k,2442) = mat(k,2442) + rxt(k,292)*y(k,155)
         mat(k,1174) = rxt(k,376)*y(k,155) + rxt(k,377)*y(k,157)
         mat(k,580) = rxt(k,251)*y(k,259)
         mat(k,2702) = mat(k,2702) + rxt(k,258)*y(k,155)
         mat(k,534) = rxt(k,217)*y(k,259)
         mat(k,2223) = mat(k,2223) + rxt(k,318)*y(k,157)
         mat(k,302) = 4.000_r8*rxt(k,315)*y(k,126)
         mat(k,2625) = mat(k,2625) + rxt(k,329)*y(k,155) + rxt(k,331)*y(k,157)
         mat(k,647) = .700_r8*rxt(k,509)*y(k,259)
         mat(k,2566) = rxt(k,292)*y(k,23) + rxt(k,376)*y(k,70) + rxt(k,258)*y(k,76) &
                      + rxt(k,329)*y(k,127) + 2.000_r8*rxt(k,229)*y(k,157) &
                      + rxt(k,235)*y(k,164) + rxt(k,234)*y(k,166) + rxt(k,336) &
                      *y(k,170) + rxt(k,530)*y(k,221) + rxt(k,491)*y(k,222) &
                      + rxt(k,533)*y(k,224) + rxt(k,538)*y(k,226) + rxt(k,416) &
                      *y(k,227) + rxt(k,444)*y(k,228) + rxt(k,540)*y(k,229) &
                      + rxt(k,427)*y(k,230) + rxt(k,395)*y(k,231) + rxt(k,544) &
                      *y(k,232) + rxt(k,462)*y(k,234) + rxt(k,431)*y(k,236) &
                      + rxt(k,233)*y(k,237) + rxt(k,403)*y(k,238) + .920_r8*rxt(k,501) &
                      *y(k,239) + .920_r8*rxt(k,507)*y(k,240) + rxt(k,515)*y(k,241) &
                      + rxt(k,469)*y(k,244) + rxt(k,547)*y(k,245) + rxt(k,478) &
                      *y(k,246) + rxt(k,550)*y(k,247) + rxt(k,481)*y(k,248) &
                      + 1.600_r8*rxt(k,580)*y(k,254) + rxt(k,553)*y(k,261) &
                      + rxt(k,452)*y(k,262) + rxt(k,456)*y(k,263) + .900_r8*rxt(k,585) &
                      *y(k,264) + .800_r8*rxt(k,590)*y(k,265) + rxt(k,560)*y(k,266) &
                      + rxt(k,526)*y(k,268) + rxt(k,566)*y(k,269) + rxt(k,569) &
                      *y(k,271)
         mat(k,2774) = mat(k,2774) + rxt(k,460)*y(k,18) + rxt(k,377)*y(k,70) &
                      + rxt(k,318)*y(k,117) + rxt(k,331)*y(k,127) &
                      + 2.000_r8*rxt(k,229)*y(k,155) + rxt(k,230)*y(k,164) &
                      + rxt(k,228)*y(k,237) + rxt(k,502)*y(k,239) + rxt(k,508) &
                      *y(k,240) + rxt(k,516)*y(k,241) + rxt(k,468)*y(k,244) &
                      + rxt(k,479)*y(k,246) + 2.000_r8*rxt(k,581)*y(k,254) &
                      + rxt(k,231)*y(k,259) + rxt(k,527)*y(k,268)
         mat(k,932) = rxt(k,450)*y(k,259)
         mat(k,2672) = mat(k,2672) + rxt(k,235)*y(k,155) + rxt(k,230)*y(k,157)
         mat(k,2017) = mat(k,2017) + rxt(k,234)*y(k,155)
         mat(k,692) = rxt(k,336)*y(k,155)
         mat(k,698) = rxt(k,587)*y(k,259)
         mat(k,594) = mat(k,594) + rxt(k,530)*y(k,155)
         mat(k,1081) = rxt(k,491)*y(k,155)
         mat(k,558) = rxt(k,533)*y(k,155)
         mat(k,485) = rxt(k,538)*y(k,155)
         mat(k,997) = rxt(k,416)*y(k,155)
         mat(k,894) = rxt(k,444)*y(k,155)
         mat(k,705) = rxt(k,540)*y(k,155)
         mat(k,1549) = mat(k,1549) + rxt(k,427)*y(k,155)
         mat(k,1743) = rxt(k,395)*y(k,155) + .500_r8*rxt(k,578)*y(k,254)
         mat(k,851) = mat(k,851) + rxt(k,544)*y(k,155)
         mat(k,639) = rxt(k,462)*y(k,155)
         mat(k,860) = rxt(k,431)*y(k,155)
         mat(k,2413) = mat(k,2413) + rxt(k,233)*y(k,155) + rxt(k,228)*y(k,157)
         mat(k,491) = rxt(k,403)*y(k,155)
         mat(k,1422) = .920_r8*rxt(k,501)*y(k,155) + rxt(k,502)*y(k,157)
         mat(k,1455) = .920_r8*rxt(k,507)*y(k,155) + rxt(k,508)*y(k,157)
         mat(k,1375) = rxt(k,515)*y(k,155) + rxt(k,516)*y(k,157)
         mat(k,1477) = rxt(k,469)*y(k,155) + rxt(k,468)*y(k,157)
         mat(k,805) = mat(k,805) + rxt(k,547)*y(k,155)
         mat(k,1517) = mat(k,1517) + rxt(k,478)*y(k,155) + rxt(k,479)*y(k,157)
         mat(k,1007) = mat(k,1007) + rxt(k,550)*y(k,155)
         mat(k,759) = rxt(k,481)*y(k,155)
         mat(k,1221) = 1.600_r8*rxt(k,580)*y(k,155) + 2.000_r8*rxt(k,581)*y(k,157) &
                      + .500_r8*rxt(k,578)*y(k,231)
         mat(k,2193) = mat(k,2193) + rxt(k,489)*y(k,1) + rxt(k,251)*y(k,75) &
                      + rxt(k,217)*y(k,109) + .700_r8*rxt(k,509)*y(k,130) + rxt(k,231) &
                      *y(k,157) + rxt(k,450)*y(k,158) + rxt(k,587)*y(k,208)
         mat(k,498) = rxt(k,553)*y(k,155)
         mat(k,882) = rxt(k,452)*y(k,155)
         mat(k,1321) = rxt(k,456)*y(k,155)
         mat(k,1285) = .900_r8*rxt(k,585)*y(k,155)
         mat(k,1306) = .800_r8*rxt(k,590)*y(k,155)
         mat(k,819) = rxt(k,560)*y(k,155)
         mat(k,1353) = rxt(k,526)*y(k,155) + rxt(k,527)*y(k,157)
         mat(k,843) = rxt(k,566)*y(k,155)
         mat(k,573) = rxt(k,569)*y(k,155)
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
         mat(k,2788) = -(rxt(k,228)*y(k,237) + rxt(k,229)*y(k,155) + rxt(k,230) &
                      *y(k,164) + rxt(k,231)*y(k,259) + rxt(k,239)*y(k,156) + rxt(k,307) &
                      *y(k,107) + rxt(k,309)*y(k,118) + rxt(k,318)*y(k,117) + rxt(k,331) &
                      *y(k,127) + rxt(k,337)*y(k,112) + rxt(k,377)*y(k,70) + rxt(k,389) &
                      *y(k,53) + rxt(k,421)*y(k,56) + rxt(k,440)*y(k,35) + rxt(k,447) &
                      *y(k,60) + rxt(k,460)*y(k,18) + rxt(k,468)*y(k,244) + rxt(k,479) &
                      *y(k,246) + rxt(k,502)*y(k,239) + rxt(k,508)*y(k,240) + rxt(k,511) &
                      *y(k,129) + rxt(k,516)*y(k,241) + rxt(k,527)*y(k,268) + rxt(k,572) &
                      *y(k,8) + rxt(k,575)*y(k,141) + rxt(k,581)*y(k,254) + rxt(k,592) &
                      *y(k,210) + rxt(k,599)*y(k,85))
         mat(k,2427) = -rxt(k,228)*y(k,157)
         mat(k,2580) = -rxt(k,229)*y(k,157)
         mat(k,2686) = -rxt(k,230)*y(k,157)
         mat(k,2207) = -rxt(k,231)*y(k,157)
         mat(k,1962) = -rxt(k,239)*y(k,157)
         mat(k,1340) = -rxt(k,307)*y(k,157)
         mat(k,1602) = -rxt(k,309)*y(k,157)
         mat(k,2237) = -rxt(k,318)*y(k,157)
         mat(k,2639) = -rxt(k,331)*y(k,157)
         mat(k,1675) = -rxt(k,337)*y(k,157)
         mat(k,1179) = -rxt(k,377)*y(k,157)
         mat(k,2487) = -rxt(k,389)*y(k,157)
         mat(k,1210) = -rxt(k,421)*y(k,157)
         mat(k,1201) = -rxt(k,440)*y(k,157)
         mat(k,1402) = -rxt(k,447)*y(k,157)
         mat(k,407) = -rxt(k,460)*y(k,157)
         mat(k,1483) = -rxt(k,468)*y(k,157)
         mat(k,1524) = -rxt(k,479)*y(k,157)
         mat(k,1429) = -rxt(k,502)*y(k,157)
         mat(k,1462) = -rxt(k,508)*y(k,157)
         mat(k,955) = -rxt(k,511)*y(k,157)
         mat(k,1381) = -rxt(k,516)*y(k,157)
         mat(k,1358) = -rxt(k,527)*y(k,157)
         mat(k,1068) = -rxt(k,572)*y(k,157)
         mat(k,1041) = -rxt(k,575)*y(k,157)
         mat(k,1227) = -rxt(k,581)*y(k,157)
         mat(k,1144) = -rxt(k,592)*y(k,157)
         mat(k,1136) = -rxt(k,599)*y(k,157)
         mat(k,2324) = rxt(k,293)*y(k,24)
         mat(k,967) = rxt(k,293)*y(k,19) + rxt(k,294)*y(k,72) + rxt(k,296)*y(k,164)
         mat(k,2294) = rxt(k,294)*y(k,24) + rxt(k,259)*y(k,77)
         mat(k,1124) = rxt(k,259)*y(k,72) + rxt(k,261)*y(k,164) + rxt(k,262)*y(k,259)
         mat(k,988) = rxt(k,349)*y(k,108)
         mat(k,2606) = rxt(k,349)*y(k,91) + rxt(k,241)*y(k,259)
         mat(k,2237) = mat(k,2237) + rxt(k,314)*y(k,128)
         mat(k,979) = rxt(k,314)*y(k,117)
         mat(k,671) = .500_r8*rxt(k,484)*y(k,259)
         mat(k,1962) = mat(k,1962) + rxt(k,227)*y(k,164) + rxt(k,226)*y(k,166)
         mat(k,2686) = mat(k,2686) + rxt(k,296)*y(k,24) + rxt(k,261)*y(k,77) &
                      + rxt(k,227)*y(k,156)
         mat(k,2031) = rxt(k,226)*y(k,156)
         mat(k,610) = rxt(k,436)*y(k,259)
         mat(k,2207) = mat(k,2207) + rxt(k,262)*y(k,77) + rxt(k,241)*y(k,108) &
                      + .500_r8*rxt(k,484)*y(k,140) + rxt(k,436)*y(k,172)
         mat(k,929) = -(rxt(k,450)*y(k,259))
         mat(k,2139) = -rxt(k,450)*y(k,158)
         mat(k,1183) = rxt(k,440)*y(k,157)
         mat(k,612) = .500_r8*rxt(k,510)*y(k,259)
         mat(k,504) = rxt(k,517)*y(k,259)
         mat(k,428) = rxt(k,521)*y(k,259)
         mat(k,1153) = rxt(k,522)*y(k,259)
         mat(k,2726) = rxt(k,440)*y(k,35)
         mat(k,2139) = mat(k,2139) + .500_r8*rxt(k,510)*y(k,131) + rxt(k,517)*y(k,132) &
                      + rxt(k,521)*y(k,146) + rxt(k,522)*y(k,147)
         mat(k,433) = -(rxt(k,582)*y(k,259))
         mat(k,2088) = -rxt(k,582)*y(k,159)
         mat(k,2335) = rxt(k,579)*y(k,254)
         mat(k,1212) = rxt(k,579)*y(k,237)
         mat(k,2684) = -(rxt(k,197)*y(k,166) + 4._r8*rxt(k,198)*y(k,164) + rxt(k,199) &
                      *y(k,165) + rxt(k,200)*y(k,95) + rxt(k,201)*y(k,97) + rxt(k,206) &
                      *y(k,237) + rxt(k,212)*y(k,259) + (rxt(k,225) + rxt(k,227) &
                      ) * y(k,156) + rxt(k,230)*y(k,157) + rxt(k,235)*y(k,155) &
                      + rxt(k,261)*y(k,77) + rxt(k,263)*y(k,76) + rxt(k,266)*y(k,103) &
                      + rxt(k,269)*y(k,111) + rxt(k,296)*y(k,24) + rxt(k,297)*y(k,23) &
                      + rxt(k,299)*y(k,99) + rxt(k,301)*y(k,110) + rxt(k,310)*y(k,118) &
                      + rxt(k,332)*y(k,127) + rxt(k,390)*y(k,53) + rxt(k,601)*y(k,169) &
                      + (rxt(k,726) + rxt(k,727)) * y(k,251) + rxt(k,728)*y(k,253))
         mat(k,2029) = -rxt(k,197)*y(k,164)
         mat(k,1784) = -rxt(k,199)*y(k,164)
         mat(k,1614) = -rxt(k,200)*y(k,164)
         mat(k,677) = -rxt(k,201)*y(k,164)
         mat(k,2425) = -rxt(k,206)*y(k,164)
         mat(k,2205) = -rxt(k,212)*y(k,164)
         mat(k,1960) = -(rxt(k,225) + rxt(k,227)) * y(k,164)
         mat(k,2786) = -rxt(k,230)*y(k,164)
         mat(k,2578) = -rxt(k,235)*y(k,164)
         mat(k,1122) = -rxt(k,261)*y(k,164)
         mat(k,2714) = -rxt(k,263)*y(k,164)
         mat(k,1830) = -rxt(k,266)*y(k,164)
         mat(k,1807) = -rxt(k,269)*y(k,164)
         mat(k,966) = -rxt(k,296)*y(k,164)
         mat(k,2454) = -rxt(k,297)*y(k,164)
         mat(k,1589) = -rxt(k,299)*y(k,164)
         mat(k,1697) = -rxt(k,301)*y(k,164)
         mat(k,1601) = -rxt(k,310)*y(k,164)
         mat(k,2637) = -rxt(k,332)*y(k,164)
         mat(k,2485) = -rxt(k,390)*y(k,164)
         mat(k,415) = -rxt(k,601)*y(k,164)
         mat(k,773) = -(rxt(k,726) + rxt(k,727)) * y(k,164)
         mat(k,568) = -rxt(k,728)*y(k,164)
         mat(k,1902) = rxt(k,204)*y(k,237)
         mat(k,1113) = rxt(k,220)*y(k,155) + rxt(k,221)*y(k,156) + rxt(k,224)*y(k,165) &
                      + rxt(k,731)*y(k,258)
         mat(k,2578) = mat(k,2578) + rxt(k,220)*y(k,143)
         mat(k,1960) = mat(k,1960) + rxt(k,221)*y(k,143)
         mat(k,1784) = mat(k,1784) + rxt(k,224)*y(k,143) + rxt(k,603)*y(k,181) &
                      + rxt(k,610)*y(k,183) + rxt(k,730)*y(k,253) + (rxt(k,186) &
                       +rxt(k,187))*y(k,255) + rxt(k,736)*y(k,260)
         mat(k,829) = rxt(k,603)*y(k,165)
         mat(k,1631) = rxt(k,610)*y(k,165)
         mat(k,928) = rxt(k,722)*y(k,252) + 1.150_r8*rxt(k,723)*y(k,258)
         mat(k,2425) = mat(k,2425) + rxt(k,204)*y(k,94)
         mat(k,907) = rxt(k,722)*y(k,233)
         mat(k,568) = mat(k,568) + rxt(k,730)*y(k,165)
         mat(k,1878) = (rxt(k,186)+rxt(k,187))*y(k,165)
         mat(k,915) = rxt(k,731)*y(k,143) + 1.150_r8*rxt(k,723)*y(k,233)
         mat(k,2205) = mat(k,2205) + 2.000_r8*rxt(k,214)*y(k,259)
         mat(k,872) = rxt(k,736)*y(k,165)
         mat(k,1772) = -(rxt(k,186)*y(k,255) + rxt(k,191)*y(k,256) + rxt(k,199) &
                      *y(k,164) + rxt(k,205)*y(k,94) + rxt(k,219)*y(k,250) + rxt(k,224) &
                      *y(k,143) + rxt(k,433)*y(k,235) + rxt(k,603)*y(k,181) + rxt(k,610) &
                      *y(k,183) + rxt(k,725)*y(k,251) + (rxt(k,729) + rxt(k,730) &
                      ) * y(k,253) + rxt(k,736)*y(k,260))
         mat(k,1861) = -rxt(k,186)*y(k,165)
         mat(k,181) = -rxt(k,191)*y(k,165)
         mat(k,2667) = -rxt(k,199)*y(k,165)
         mat(k,1885) = -rxt(k,205)*y(k,165)
         mat(k,600) = -rxt(k,219)*y(k,165)
         mat(k,1107) = -rxt(k,224)*y(k,165)
         mat(k,518) = -rxt(k,433)*y(k,165)
         mat(k,825) = -rxt(k,603)*y(k,165)
         mat(k,1621) = -rxt(k,610)*y(k,165)
         mat(k,770) = -rxt(k,725)*y(k,165)
         mat(k,567) = -(rxt(k,729) + rxt(k,730)) * y(k,165)
         mat(k,871) = -rxt(k,736)*y(k,165)
         mat(k,2305) = rxt(k,286)*y(k,166) + rxt(k,284)*y(k,237)
         mat(k,2437) = 2.000_r8*rxt(k,287)*y(k,23) + (rxt(k,289)+rxt(k,290))*y(k,76) &
                      + rxt(k,323)*y(k,127) + rxt(k,297)*y(k,164) + rxt(k,291) &
                      *y(k,237)
         mat(k,1172) = rxt(k,374)*y(k,237)
         mat(k,2275) = rxt(k,252)*y(k,166) + rxt(k,248)*y(k,237)
         mat(k,2697) = (rxt(k,289)+rxt(k,290))*y(k,23) + (2.000_r8*rxt(k,254) &
                       +2.000_r8*rxt(k,255))*y(k,76) + (rxt(k,326)+rxt(k,327)) &
                      *y(k,127) + rxt(k,263)*y(k,164) + rxt(k,257)*y(k,237) &
                      + rxt(k,265)*y(k,259)
         mat(k,1885) = mat(k,1885) + rxt(k,208)*y(k,166) + rxt(k,202)*y(k,237)
         mat(k,533) = rxt(k,217)*y(k,259)
         mat(k,2218) = rxt(k,321)*y(k,166) + rxt(k,313)*y(k,237)
         mat(k,2620) = rxt(k,323)*y(k,23) + (rxt(k,326)+rxt(k,327))*y(k,76) &
                      + rxt(k,332)*y(k,164) + rxt(k,333)*y(k,166) + rxt(k,328) &
                      *y(k,237)
         mat(k,1107) = mat(k,1107) + rxt(k,223)*y(k,156)
         mat(k,279) = rxt(k,237)*y(k,255)
         mat(k,2561) = rxt(k,234)*y(k,166) + rxt(k,733)*y(k,258)
         mat(k,1943) = rxt(k,223)*y(k,143) + rxt(k,225)*y(k,164) + rxt(k,226)*y(k,166)
         mat(k,2769) = rxt(k,230)*y(k,164) + rxt(k,228)*y(k,237)
         mat(k,2667) = mat(k,2667) + rxt(k,297)*y(k,23) + rxt(k,263)*y(k,76) &
                      + rxt(k,332)*y(k,127) + rxt(k,225)*y(k,156) + rxt(k,230) &
                      *y(k,157) + 2.000_r8*rxt(k,198)*y(k,164) + 2.000_r8*rxt(k,197) &
                      *y(k,166) + rxt(k,206)*y(k,237) + rxt(k,190)*y(k,256) &
                      + rxt(k,212)*y(k,259)
         mat(k,1772) = mat(k,1772) + 2.000_r8*rxt(k,191)*y(k,256)
         mat(k,2012) = rxt(k,286)*y(k,19) + rxt(k,252)*y(k,72) + rxt(k,208)*y(k,94) &
                      + rxt(k,321)*y(k,117) + rxt(k,333)*y(k,127) + rxt(k,234) &
                      *y(k,155) + rxt(k,226)*y(k,156) + 2.000_r8*rxt(k,197)*y(k,164) &
                      + rxt(k,605)*y(k,181) + rxt(k,611)*y(k,183) &
                      + 2.000_r8*rxt(k,207)*y(k,237) + 2.000_r8*rxt(k,188)*y(k,255) &
                      + rxt(k,213)*y(k,259)
         mat(k,825) = mat(k,825) + rxt(k,605)*y(k,166)
         mat(k,1621) = mat(k,1621) + rxt(k,611)*y(k,166)
         mat(k,996) = rxt(k,415)*y(k,237)
         mat(k,893) = rxt(k,443)*y(k,237)
         mat(k,1738) = rxt(k,394)*y(k,237)
         mat(k,2408) = rxt(k,284)*y(k,19) + rxt(k,291)*y(k,23) + rxt(k,374)*y(k,70) &
                      + rxt(k,248)*y(k,72) + rxt(k,257)*y(k,76) + rxt(k,202)*y(k,94) &
                      + rxt(k,313)*y(k,117) + rxt(k,328)*y(k,127) + rxt(k,228) &
                      *y(k,157) + rxt(k,206)*y(k,164) + 2.000_r8*rxt(k,207)*y(k,166) &
                      + rxt(k,415)*y(k,227) + rxt(k,443)*y(k,228) + rxt(k,394) &
                      *y(k,231) + 2.000_r8*rxt(k,216)*y(k,237) + rxt(k,211)*y(k,259) &
                      + rxt(k,451)*y(k,262)
         mat(k,1861) = mat(k,1861) + rxt(k,237)*y(k,144) + 2.000_r8*rxt(k,188) &
                      *y(k,166)
         mat(k,181) = mat(k,181) + rxt(k,190)*y(k,164) + 2.000_r8*rxt(k,191)*y(k,165)
         mat(k,912) = rxt(k,733)*y(k,155)
         mat(k,2188) = rxt(k,265)*y(k,76) + rxt(k,217)*y(k,109) + rxt(k,212)*y(k,164) &
                      + rxt(k,213)*y(k,166) + rxt(k,211)*y(k,237)
         mat(k,881) = rxt(k,451)*y(k,237)
         mat(k,2018) = -(rxt(k,188)*y(k,255) + rxt(k,197)*y(k,164) + rxt(k,207) &
                      *y(k,237) + rxt(k,208)*y(k,94) + rxt(k,213)*y(k,259) + rxt(k,226) &
                      *y(k,156) + rxt(k,234)*y(k,155) + rxt(k,252)*y(k,72) + rxt(k,286) &
                      *y(k,19) + rxt(k,321)*y(k,117) + rxt(k,333)*y(k,127) + rxt(k,412) &
                      *y(k,30) + rxt(k,441)*y(k,35) + rxt(k,471)*y(k,136) + rxt(k,485) &
                      *y(k,142) + rxt(k,518)*y(k,129) + rxt(k,556)*y(k,174) + rxt(k,573) &
                      *y(k,8) + rxt(k,576)*y(k,141) + rxt(k,605)*y(k,181) + rxt(k,611) &
                      *y(k,183))
         mat(k,1867) = -rxt(k,188)*y(k,166)
         mat(k,2673) = -rxt(k,197)*y(k,166)
         mat(k,2414) = -rxt(k,207)*y(k,166)
         mat(k,1891) = -rxt(k,208)*y(k,166)
         mat(k,2194) = -rxt(k,213)*y(k,166)
         mat(k,1949) = -rxt(k,226)*y(k,166)
         mat(k,2567) = -rxt(k,234)*y(k,166)
         mat(k,2281) = -rxt(k,252)*y(k,166)
         mat(k,2311) = -rxt(k,286)*y(k,166)
         mat(k,2224) = -rxt(k,321)*y(k,166)
         mat(k,2626) = -rxt(k,333)*y(k,166)
         mat(k,631) = -rxt(k,412)*y(k,166)
         mat(k,1196) = -rxt(k,441)*y(k,166)
         mat(k,1390) = -rxt(k,471)*y(k,166)
         mat(k,1500) = -rxt(k,485)*y(k,166)
         mat(k,951) = -rxt(k,518)*y(k,166)
         mat(k,528) = -rxt(k,556)*y(k,166)
         mat(k,1064) = -rxt(k,573)*y(k,166)
         mat(k,1037) = -rxt(k,576)*y(k,166)
         mat(k,827) = -rxt(k,605)*y(k,166)
         mat(k,1624) = -rxt(k,611)*y(k,166)
         mat(k,2673) = mat(k,2673) + rxt(k,199)*y(k,165)
         mat(k,1776) = rxt(k,199)*y(k,164)
         mat(k,1550) = .150_r8*rxt(k,426)*y(k,237)
         mat(k,2414) = mat(k,2414) + .150_r8*rxt(k,426)*y(k,230) + .150_r8*rxt(k,476) &
                      *y(k,246)
         mat(k,1518) = .150_r8*rxt(k,476)*y(k,237)
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
         mat(k,508) = -(rxt(k,612)*y(k,183))
         mat(k,1617) = -rxt(k,612)*y(k,168)
         mat(k,2429) = rxt(k,288)*y(k,76)
         mat(k,2690) = rxt(k,288)*y(k,23) + 2.000_r8*rxt(k,256)*y(k,76) + rxt(k,325) &
                      *y(k,127)
         mat(k,2610) = rxt(k,325)*y(k,76)
         mat(k,408) = -(rxt(k,601)*y(k,164) + rxt(k,602)*y(k,259))
         mat(k,2643) = -rxt(k,601)*y(k,169)
         mat(k,2085) = -rxt(k,602)*y(k,169)
         mat(k,690) = -(rxt(k,336)*y(k,155) + rxt(k,344)*y(k,127) + 4._r8*rxt(k,345) &
                      *y(k,170))
         mat(k,2509) = -rxt(k,336)*y(k,170)
         mat(k,2612) = -rxt(k,344)*y(k,170)
         mat(k,2431) = rxt(k,324)*y(k,127)
         mat(k,2612) = mat(k,2612) + rxt(k,324)*y(k,23) + 2.000_r8*rxt(k,341)*y(k,127) &
                      + rxt(k,331)*y(k,157) + rxt(k,333)*y(k,166)
         mat(k,2725) = rxt(k,331)*y(k,127)
         mat(k,1975) = rxt(k,333)*y(k,127)
         mat(k,1239) = rxt(k,464)*y(k,259)
         mat(k,2491) = .100_r8*rxt(k,585)*y(k,264)
         mat(k,2066) = rxt(k,464)*y(k,113)
         mat(k,1273) = .100_r8*rxt(k,585)*y(k,155)
         mat(k,603) = -(rxt(k,436)*y(k,259))
         mat(k,2109) = -rxt(k,436)*y(k,172)
         mat(k,1919) = rxt(k,438)*y(k,230)
         mat(k,1527) = rxt(k,438)*y(k,156)
         mat(k,1906) = rxt(k,558)*y(k,221)
         mat(k,591) = rxt(k,558)*y(k,156)
         mat(k,525) = -(rxt(k,555)*y(k,156) + rxt(k,556)*y(k,166))
         mat(k,1913) = -rxt(k,555)*y(k,174)
         mat(k,1973) = -rxt(k,556)*y(k,174)
         mat(k,213) = .070_r8*rxt(k,542)*y(k,259)
         mat(k,2500) = rxt(k,540)*y(k,229)
         mat(k,177) = .060_r8*rxt(k,554)*y(k,259)
         mat(k,238) = .070_r8*rxt(k,570)*y(k,259)
         mat(k,703) = rxt(k,540)*y(k,155)
         mat(k,2100) = .070_r8*rxt(k,542)*y(k,84) + .060_r8*rxt(k,554)*y(k,175) &
                      + .070_r8*rxt(k,570)*y(k,217)
         mat(k,175) = -(rxt(k,554)*y(k,259))
         mat(k,2050) = -rxt(k,554)*y(k,175)
         mat(k,167) = .530_r8*rxt(k,531)*y(k,259)
         mat(k,2050) = mat(k,2050) + .530_r8*rxt(k,531)*y(k,9)
         mat(k,354) = -(rxt(k,557)*y(k,259))
         mat(k,2077) = -rxt(k,557)*y(k,176)
         mat(k,2331) = rxt(k,552)*y(k,261)
         mat(k,495) = rxt(k,552)*y(k,237)
         mat(k,619) = -(rxt(k,453)*y(k,259))
         mat(k,2111) = -rxt(k,453)*y(k,179)
         mat(k,2353) = rxt(k,451)*y(k,262)
         mat(k,877) = rxt(k,451)*y(k,237)
         mat(k,445) = -(rxt(k,457)*y(k,259))
         mat(k,2089) = -rxt(k,457)*y(k,180)
         mat(k,2336) = .850_r8*rxt(k,455)*y(k,263)
         mat(k,1314) = .850_r8*rxt(k,455)*y(k,237)
         mat(k,823) = -(rxt(k,603)*y(k,165) + rxt(k,605)*y(k,166) + rxt(k,608) &
                      *y(k,259))
         mat(k,1761) = -rxt(k,603)*y(k,181)
         mat(k,1978) = -rxt(k,605)*y(k,181)
         mat(k,2132) = -rxt(k,608)*y(k,181)
         mat(k,1620) = -(rxt(k,606)*y(k,23) + rxt(k,607)*y(k,76) + rxt(k,609)*y(k,156) &
                      + rxt(k,610)*y(k,165) + rxt(k,611)*y(k,166) + rxt(k,612) &
                      *y(k,168) + rxt(k,613)*y(k,259))
         mat(k,2435) = -rxt(k,606)*y(k,183)
         mat(k,2695) = -rxt(k,607)*y(k,183)
         mat(k,1938) = -rxt(k,609)*y(k,183)
         mat(k,1771) = -rxt(k,610)*y(k,183)
         mat(k,2009) = -rxt(k,611)*y(k,183)
         mat(k,510) = -rxt(k,612)*y(k,183)
         mat(k,2183) = -rxt(k,613)*y(k,183)
         mat(k,2662) = rxt(k,601)*y(k,169)
         mat(k,1771) = mat(k,1771) + rxt(k,603)*y(k,181)
         mat(k,2009) = mat(k,2009) + rxt(k,605)*y(k,181)
         mat(k,412) = rxt(k,601)*y(k,164)
         mat(k,824) = rxt(k,603)*y(k,165) + rxt(k,605)*y(k,166) + rxt(k,608)*y(k,259)
         mat(k,2183) = mat(k,2183) + rxt(k,608)*y(k,181)
         mat(k,1146) = -(rxt(k,604)*y(k,259))
         mat(k,2154) = -rxt(k,604)*y(k,184)
         mat(k,2434) = rxt(k,595)*y(k,85) + rxt(k,606)*y(k,183)
         mat(k,2263) = rxt(k,597)*y(k,85)
         mat(k,2694) = rxt(k,607)*y(k,183)
         mat(k,1126) = rxt(k,595)*y(k,23) + rxt(k,597)*y(k,72) + rxt(k,598)*y(k,127) &
                      + rxt(k,599)*y(k,157) + (rxt(k,600)+.500_r8*rxt(k,614))*y(k,259)
         mat(k,2615) = rxt(k,598)*y(k,85)
         mat(k,1930) = rxt(k,609)*y(k,183)
         mat(k,2737) = rxt(k,599)*y(k,85)
         mat(k,1767) = rxt(k,610)*y(k,183)
         mat(k,1988) = rxt(k,611)*y(k,183)
         mat(k,509) = rxt(k,612)*y(k,183)
         mat(k,410) = rxt(k,602)*y(k,259)
         mat(k,1619) = rxt(k,606)*y(k,23) + rxt(k,607)*y(k,76) + rxt(k,609)*y(k,156) &
                      + rxt(k,610)*y(k,165) + rxt(k,611)*y(k,166) + rxt(k,612) &
                      *y(k,168) + rxt(k,613)*y(k,259)
         mat(k,2154) = mat(k,2154) + (rxt(k,600)+.500_r8*rxt(k,614))*y(k,85) &
                      + rxt(k,602)*y(k,169) + rxt(k,613)*y(k,183)
         mat(k,283) = -(rxt(k,615)*y(k,272))
         mat(k,2791) = -rxt(k,615)*y(k,185)
         mat(k,1145) = rxt(k,604)*y(k,259)
         mat(k,2068) = rxt(k,604)*y(k,184)
         mat(k,1043) = .2202005_r8*rxt(k,666)*y(k,166) + .2202005_r8*rxt(k,667) &
                      *y(k,259)
         mat(k,160) = .0023005_r8*rxt(k,668)*y(k,259)
         mat(k,935) = .0031005_r8*rxt(k,671)*y(k,259)
         mat(k,51) = .2381005_r8*rxt(k,672)*y(k,259)
         mat(k,1016) = .0508005_r8*rxt(k,674)*y(k,166) + .0508005_r8*rxt(k,675) &
                      *y(k,259)
         mat(k,1964) = .2202005_r8*rxt(k,666)*y(k,8) + .0508005_r8*rxt(k,674)*y(k,141)
         mat(k,58) = .5931005_r8*rxt(k,676)*y(k,259)
         mat(k,199) = .1364005_r8*rxt(k,678)*y(k,259)
         mat(k,223) = .1677005_r8*rxt(k,679)*y(k,259)
         mat(k,2033) = .2202005_r8*rxt(k,667)*y(k,8) + .0023005_r8*rxt(k,668)*y(k,9) &
                      + .0031005_r8*rxt(k,671)*y(k,129) + .2381005_r8*rxt(k,672) &
                      *y(k,134) + .0508005_r8*rxt(k,675)*y(k,141) &
                      + .5931005_r8*rxt(k,676)*y(k,205) + .1364005_r8*rxt(k,678) &
                      *y(k,213) + .1677005_r8*rxt(k,679)*y(k,215)
         mat(k,1044) = .2067005_r8*rxt(k,666)*y(k,166) + .2067005_r8*rxt(k,667) &
                      *y(k,259)
         mat(k,161) = .0008005_r8*rxt(k,668)*y(k,259)
         mat(k,936) = .0035005_r8*rxt(k,671)*y(k,259)
         mat(k,52) = .1308005_r8*rxt(k,672)*y(k,259)
         mat(k,1017) = .1149005_r8*rxt(k,674)*y(k,166) + .1149005_r8*rxt(k,675) &
                      *y(k,259)
         mat(k,1965) = .2067005_r8*rxt(k,666)*y(k,8) + .1149005_r8*rxt(k,674)*y(k,141)
         mat(k,59) = .1534005_r8*rxt(k,676)*y(k,259)
         mat(k,200) = .0101005_r8*rxt(k,678)*y(k,259)
         mat(k,224) = .0174005_r8*rxt(k,679)*y(k,259)
         mat(k,2034) = .2067005_r8*rxt(k,667)*y(k,8) + .0008005_r8*rxt(k,668)*y(k,9) &
                      + .0035005_r8*rxt(k,671)*y(k,129) + .1308005_r8*rxt(k,672) &
                      *y(k,134) + .1149005_r8*rxt(k,675)*y(k,141) &
                      + .1534005_r8*rxt(k,676)*y(k,205) + .0101005_r8*rxt(k,678) &
                      *y(k,213) + .0174005_r8*rxt(k,679)*y(k,215)
         mat(k,1045) = .0653005_r8*rxt(k,666)*y(k,166) + .0653005_r8*rxt(k,667) &
                      *y(k,259)
         mat(k,162) = .0843005_r8*rxt(k,668)*y(k,259)
         mat(k,937) = .0003005_r8*rxt(k,671)*y(k,259)
         mat(k,53) = .0348005_r8*rxt(k,672)*y(k,259)
         mat(k,1018) = .0348005_r8*rxt(k,674)*y(k,166) + .0348005_r8*rxt(k,675) &
                      *y(k,259)
         mat(k,1966) = .0653005_r8*rxt(k,666)*y(k,8) + .0348005_r8*rxt(k,674)*y(k,141)
         mat(k,60) = .0459005_r8*rxt(k,676)*y(k,259)
         mat(k,201) = .0763005_r8*rxt(k,678)*y(k,259)
         mat(k,225) = .086_r8*rxt(k,679)*y(k,259)
         mat(k,2035) = .0653005_r8*rxt(k,667)*y(k,8) + .0843005_r8*rxt(k,668)*y(k,9) &
                      + .0003005_r8*rxt(k,671)*y(k,129) + .0348005_r8*rxt(k,672) &
                      *y(k,134) + .0348005_r8*rxt(k,675)*y(k,141) &
                      + .0459005_r8*rxt(k,676)*y(k,205) + .0763005_r8*rxt(k,678) &
                      *y(k,213) + .086_r8*rxt(k,679)*y(k,215)
         mat(k,1046) = .1749305_r8*rxt(k,665)*y(k,157) + .1284005_r8*rxt(k,666) &
                      *y(k,166) + .1284005_r8*rxt(k,667)*y(k,259)
         mat(k,163) = .0443005_r8*rxt(k,668)*y(k,259)
         mat(k,938) = .0590245_r8*rxt(k,669)*y(k,157) + .0033005_r8*rxt(k,670) &
                      *y(k,166) + .0271005_r8*rxt(k,671)*y(k,259)
         mat(k,54) = .0076005_r8*rxt(k,672)*y(k,259)
         mat(k,1019) = .1749305_r8*rxt(k,673)*y(k,157) + .0554005_r8*rxt(k,674) &
                      *y(k,166) + .0554005_r8*rxt(k,675)*y(k,259)
         mat(k,2718) = .1749305_r8*rxt(k,665)*y(k,8) + .0590245_r8*rxt(k,669)*y(k,129) &
                      + .1749305_r8*rxt(k,673)*y(k,141)
         mat(k,1967) = .1284005_r8*rxt(k,666)*y(k,8) + .0033005_r8*rxt(k,670)*y(k,129) &
                      + .0554005_r8*rxt(k,674)*y(k,141)
         mat(k,61) = .0085005_r8*rxt(k,676)*y(k,259)
         mat(k,202) = .2157005_r8*rxt(k,678)*y(k,259)
         mat(k,226) = .0512005_r8*rxt(k,679)*y(k,259)
         mat(k,2036) = .1284005_r8*rxt(k,667)*y(k,8) + .0443005_r8*rxt(k,668)*y(k,9) &
                      + .0271005_r8*rxt(k,671)*y(k,129) + .0076005_r8*rxt(k,672) &
                      *y(k,134) + .0554005_r8*rxt(k,675)*y(k,141) &
                      + .0085005_r8*rxt(k,676)*y(k,205) + .2157005_r8*rxt(k,678) &
                      *y(k,213) + .0512005_r8*rxt(k,679)*y(k,215)
         mat(k,1047) = .5901905_r8*rxt(k,665)*y(k,157) + .114_r8*rxt(k,666)*y(k,166) &
                      + .114_r8*rxt(k,667)*y(k,259)
         mat(k,164) = .1621005_r8*rxt(k,668)*y(k,259)
         mat(k,939) = .0250245_r8*rxt(k,669)*y(k,157) + .0474005_r8*rxt(k,671) &
                      *y(k,259)
         mat(k,55) = .0113005_r8*rxt(k,672)*y(k,259)
         mat(k,1020) = .5901905_r8*rxt(k,673)*y(k,157) + .1278005_r8*rxt(k,674) &
                      *y(k,166) + .1278005_r8*rxt(k,675)*y(k,259)
         mat(k,2719) = .5901905_r8*rxt(k,665)*y(k,8) + .0250245_r8*rxt(k,669)*y(k,129) &
                      + .5901905_r8*rxt(k,673)*y(k,141)
         mat(k,1968) = .114_r8*rxt(k,666)*y(k,8) + .1278005_r8*rxt(k,674)*y(k,141)
         mat(k,62) = .0128005_r8*rxt(k,676)*y(k,259)
         mat(k,203) = .0232005_r8*rxt(k,678)*y(k,259)
         mat(k,227) = .1598005_r8*rxt(k,679)*y(k,259)
         mat(k,2037) = .114_r8*rxt(k,667)*y(k,8) + .1621005_r8*rxt(k,668)*y(k,9) &
                      + .0474005_r8*rxt(k,671)*y(k,129) + .0113005_r8*rxt(k,672) &
                      *y(k,134) + .1278005_r8*rxt(k,675)*y(k,141) &
                      + .0128005_r8*rxt(k,676)*y(k,205) + .0232005_r8*rxt(k,678) &
                      *y(k,213) + .1598005_r8*rxt(k,679)*y(k,215)
         mat(k,63) = -(rxt(k,676)*y(k,259))
         mat(k,2039) = -rxt(k,676)*y(k,205)
         mat(k,206) = .100_r8*rxt(k,562)*y(k,259)
         mat(k,228) = .230_r8*rxt(k,564)*y(k,259)
         mat(k,2055) = .100_r8*rxt(k,562)*y(k,213) + .230_r8*rxt(k,564)*y(k,215)
         mat(k,721) = -(rxt(k,586)*y(k,259))
         mat(k,2123) = -rxt(k,586)*y(k,207)
         mat(k,2357) = rxt(k,584)*y(k,264)
         mat(k,1274) = rxt(k,584)*y(k,237)
         mat(k,696) = -(rxt(k,587)*y(k,259))
         mat(k,2120) = -rxt(k,587)*y(k,208)
         mat(k,2510) = .200_r8*rxt(k,580)*y(k,254) + .200_r8*rxt(k,590)*y(k,265)
         mat(k,1705) = .500_r8*rxt(k,578)*y(k,254)
         mat(k,1213) = .200_r8*rxt(k,580)*y(k,155) + .500_r8*rxt(k,578)*y(k,231)
         mat(k,1293) = .200_r8*rxt(k,590)*y(k,155)
         mat(k,546) = -(rxt(k,591)*y(k,259))
         mat(k,2103) = -rxt(k,591)*y(k,209)
         mat(k,2349) = rxt(k,589)*y(k,265)
         mat(k,1292) = rxt(k,589)*y(k,237)
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
         mat(k,1137) = -(rxt(k,592)*y(k,157) + rxt(k,593)*y(k,259))
         mat(k,2736) = -rxt(k,592)*y(k,210)
         mat(k,2153) = -rxt(k,593)*y(k,210)
         mat(k,1055) = .330_r8*rxt(k,573)*y(k,166)
         mat(k,1028) = .330_r8*rxt(k,576)*y(k,166)
         mat(k,2532) = .800_r8*rxt(k,580)*y(k,254) + .800_r8*rxt(k,590)*y(k,265)
         mat(k,2736) = mat(k,2736) + rxt(k,581)*y(k,254)
         mat(k,1987) = .330_r8*rxt(k,573)*y(k,8) + .330_r8*rxt(k,576)*y(k,141)
         mat(k,697) = rxt(k,587)*y(k,259)
         mat(k,1713) = .500_r8*rxt(k,578)*y(k,254) + rxt(k,588)*y(k,265)
         mat(k,1215) = .800_r8*rxt(k,580)*y(k,155) + rxt(k,581)*y(k,157) &
                      + .500_r8*rxt(k,578)*y(k,231)
         mat(k,2153) = mat(k,2153) + rxt(k,587)*y(k,208)
         mat(k,1296) = .800_r8*rxt(k,590)*y(k,155) + rxt(k,588)*y(k,231)
         mat(k,1229) = -(rxt(k,594)*y(k,259))
         mat(k,2160) = -rxt(k,594)*y(k,211)
         mat(k,1057) = .300_r8*rxt(k,573)*y(k,166)
         mat(k,1030) = .300_r8*rxt(k,576)*y(k,166)
         mat(k,2537) = .900_r8*rxt(k,585)*y(k,264)
         mat(k,1992) = .300_r8*rxt(k,573)*y(k,8) + .300_r8*rxt(k,576)*y(k,141)
         mat(k,1718) = rxt(k,583)*y(k,264)
         mat(k,1277) = .900_r8*rxt(k,585)*y(k,155) + rxt(k,583)*y(k,231)
         mat(k,734) = -(rxt(k,561)*y(k,259))
         mat(k,2124) = -rxt(k,561)*y(k,212)
         mat(k,2358) = rxt(k,559)*y(k,266)
         mat(k,811) = rxt(k,559)*y(k,237)
         mat(k,204) = -(rxt(k,562)*y(k,259))
         mat(k,2053) = -rxt(k,562)*y(k,213)
         mat(k,220) = -(rxt(k,528)*y(k,259))
         mat(k,2056) = -rxt(k,528)*y(k,214)
         mat(k,2327) = rxt(k,525)*y(k,268)
         mat(k,1342) = rxt(k,525)*y(k,237)
         mat(k,229) = -(rxt(k,564)*y(k,259))
         mat(k,2057) = -rxt(k,564)*y(k,215)
         mat(k,791) = -(rxt(k,567)*y(k,259))
         mat(k,2129) = -rxt(k,567)*y(k,216)
         mat(k,2363) = rxt(k,565)*y(k,269)
         mat(k,834) = rxt(k,565)*y(k,237)
         mat(k,237) = -(rxt(k,570)*y(k,259))
         mat(k,2058) = -rxt(k,570)*y(k,217)
         mat(k,230) = .150_r8*rxt(k,564)*y(k,259)
         mat(k,2058) = mat(k,2058) + .150_r8*rxt(k,564)*y(k,215)
         mat(k,475) = -(rxt(k,571)*y(k,259))
         mat(k,2094) = -rxt(k,571)*y(k,218)
         mat(k,2340) = rxt(k,568)*y(k,271)
         mat(k,569) = rxt(k,568)*y(k,237)
         mat(k,592) = -(rxt(k,529)*y(k,237) + rxt(k,530)*y(k,155) + rxt(k,558) &
                      *y(k,156))
         mat(k,2352) = -rxt(k,529)*y(k,221)
         mat(k,2504) = -rxt(k,530)*y(k,221)
         mat(k,1918) = -rxt(k,558)*y(k,221)
         mat(k,268) = rxt(k,535)*y(k,259)
         mat(k,2108) = rxt(k,535)*y(k,26)
         mat(k,1075) = -(rxt(k,490)*y(k,237) + (rxt(k,491) + rxt(k,492)) * y(k,155))
         mat(k,2375) = -rxt(k,490)*y(k,222)
         mat(k,2528) = -(rxt(k,491) + rxt(k,492)) * y(k,222)
         mat(k,748) = rxt(k,493)*y(k,259)
         mat(k,265) = rxt(k,494)*y(k,259)
         mat(k,2147) = rxt(k,493)*y(k,2) + rxt(k,494)*y(k,17)
         mat(k,555) = -(rxt(k,532)*y(k,237) + rxt(k,533)*y(k,155))
         mat(k,2350) = -rxt(k,532)*y(k,224)
         mat(k,2501) = -rxt(k,533)*y(k,224)
         mat(k,168) = .350_r8*rxt(k,531)*y(k,259)
         mat(k,465) = rxt(k,534)*y(k,259)
         mat(k,2104) = .350_r8*rxt(k,531)*y(k,9) + rxt(k,534)*y(k,10)
         mat(k,483) = -(rxt(k,536)*y(k,237) + rxt(k,538)*y(k,155))
         mat(k,2341) = -rxt(k,536)*y(k,226)
         mat(k,2495) = -rxt(k,538)*y(k,226)
         mat(k,361) = rxt(k,537)*y(k,259)
         mat(k,207) = .070_r8*rxt(k,562)*y(k,259)
         mat(k,231) = .060_r8*rxt(k,564)*y(k,259)
         mat(k,2095) = rxt(k,537)*y(k,27) + .070_r8*rxt(k,562)*y(k,213) &
                      + .060_r8*rxt(k,564)*y(k,215)
         mat(k,993) = -(4._r8*rxt(k,413)*y(k,227) + rxt(k,414)*y(k,231) + rxt(k,415) &
                      *y(k,237) + rxt(k,416)*y(k,155))
         mat(k,1710) = -rxt(k,414)*y(k,227)
         mat(k,2372) = -rxt(k,415)*y(k,227)
         mat(k,2525) = -rxt(k,416)*y(k,227)
         mat(k,366) = .500_r8*rxt(k,418)*y(k,259)
         mat(k,321) = rxt(k,419)*y(k,72) + rxt(k,420)*y(k,259)
         mat(k,2258) = rxt(k,419)*y(k,34)
         mat(k,2142) = .500_r8*rxt(k,418)*y(k,33) + rxt(k,420)*y(k,34)
         mat(k,889) = -(rxt(k,442)*y(k,231) + rxt(k,443)*y(k,237) + rxt(k,444) &
                      *y(k,155))
         mat(k,1708) = -rxt(k,442)*y(k,228)
         mat(k,2370) = -rxt(k,443)*y(k,228)
         mat(k,2520) = -rxt(k,444)*y(k,228)
         mat(k,452) = rxt(k,445)*y(k,259)
         mat(k,106) = rxt(k,446)*y(k,259)
         mat(k,2138) = rxt(k,445)*y(k,36) + rxt(k,446)*y(k,37)
         mat(k,704) = -(rxt(k,539)*y(k,237) + rxt(k,540)*y(k,155))
         mat(k,2356) = -rxt(k,539)*y(k,229)
         mat(k,2511) = -rxt(k,540)*y(k,229)
         mat(k,293) = rxt(k,541)*y(k,259)
         mat(k,2511) = mat(k,2511) + rxt(k,530)*y(k,221)
         mat(k,1976) = rxt(k,556)*y(k,174)
         mat(k,526) = rxt(k,556)*y(k,166)
         mat(k,593) = rxt(k,530)*y(k,155) + .400_r8*rxt(k,529)*y(k,237)
         mat(k,2356) = mat(k,2356) + .400_r8*rxt(k,529)*y(k,221)
         mat(k,2121) = rxt(k,541)*y(k,38)
         mat(k,1544) = -(4._r8*rxt(k,424)*y(k,230) + rxt(k,425)*y(k,231) + rxt(k,426) &
                      *y(k,237) + rxt(k,427)*y(k,155) + rxt(k,438)*y(k,156) + rxt(k,465) &
                      *y(k,244) + rxt(k,498)*y(k,239) + rxt(k,503)*y(k,240) + rxt(k,512) &
                      *y(k,241) + rxt(k,523)*y(k,268))
         mat(k,1734) = -rxt(k,425)*y(k,230)
         mat(k,2399) = -rxt(k,426)*y(k,230)
         mat(k,2554) = -rxt(k,427)*y(k,230)
         mat(k,1935) = -rxt(k,438)*y(k,230)
         mat(k,1474) = -rxt(k,465)*y(k,230)
         mat(k,1419) = -rxt(k,498)*y(k,230)
         mat(k,1452) = -rxt(k,503)*y(k,230)
         mat(k,1372) = -rxt(k,512)*y(k,230)
         mat(k,1350) = -rxt(k,523)*y(k,230)
         mat(k,1062) = .060_r8*rxt(k,573)*y(k,166)
         mat(k,1205) = rxt(k,421)*y(k,157) + rxt(k,422)*y(k,259)
         mat(k,1397) = rxt(k,447)*y(k,157) + rxt(k,448)*y(k,259)
         mat(k,657) = .500_r8*rxt(k,429)*y(k,259)
         mat(k,949) = .080_r8*rxt(k,518)*y(k,166)
         mat(k,1388) = .100_r8*rxt(k,471)*y(k,166)
         mat(k,1035) = .060_r8*rxt(k,576)*y(k,166)
         mat(k,1494) = .280_r8*rxt(k,485)*y(k,166)
         mat(k,2554) = mat(k,2554) + .530_r8*rxt(k,469)*y(k,244) + rxt(k,478)*y(k,246) &
                      + rxt(k,481)*y(k,248) + rxt(k,456)*y(k,263)
         mat(k,2761) = rxt(k,421)*y(k,56) + rxt(k,447)*y(k,60) + .530_r8*rxt(k,468) &
                      *y(k,244) + rxt(k,479)*y(k,246)
         mat(k,2007) = .060_r8*rxt(k,573)*y(k,8) + .080_r8*rxt(k,518)*y(k,129) &
                      + .100_r8*rxt(k,471)*y(k,136) + .060_r8*rxt(k,576)*y(k,141) &
                      + .280_r8*rxt(k,485)*y(k,142)
         mat(k,1232) = .650_r8*rxt(k,594)*y(k,259)
         mat(k,1544) = mat(k,1544) + .530_r8*rxt(k,465)*y(k,244)
         mat(k,1734) = mat(k,1734) + .260_r8*rxt(k,466)*y(k,244) + rxt(k,475)*y(k,246) &
                      + .300_r8*rxt(k,454)*y(k,263)
         mat(k,2399) = mat(k,2399) + .450_r8*rxt(k,476)*y(k,246) + .200_r8*rxt(k,480) &
                      *y(k,248) + .150_r8*rxt(k,455)*y(k,263)
         mat(k,1474) = mat(k,1474) + .530_r8*rxt(k,469)*y(k,155) + .530_r8*rxt(k,468) &
                      *y(k,157) + .530_r8*rxt(k,465)*y(k,230) + .260_r8*rxt(k,466) &
                      *y(k,231)
         mat(k,1514) = rxt(k,478)*y(k,155) + rxt(k,479)*y(k,157) + rxt(k,475)*y(k,231) &
                      + .450_r8*rxt(k,476)*y(k,237) + 4.000_r8*rxt(k,477)*y(k,246)
         mat(k,758) = rxt(k,481)*y(k,155) + .200_r8*rxt(k,480)*y(k,237)
         mat(k,2178) = rxt(k,422)*y(k,56) + rxt(k,448)*y(k,60) + .500_r8*rxt(k,429) &
                      *y(k,62) + .650_r8*rxt(k,594)*y(k,211)
         mat(k,1319) = rxt(k,456)*y(k,155) + .300_r8*rxt(k,454)*y(k,231) &
                      + .150_r8*rxt(k,455)*y(k,237)
         mat(k,1737) = -(rxt(k,253)*y(k,76) + (rxt(k,372) + rxt(k,373)) * y(k,70) &
                      + (4._r8*rxt(k,392) + 4._r8*rxt(k,393)) * y(k,231) + rxt(k,394) &
                      *y(k,237) + rxt(k,395)*y(k,155) + rxt(k,414)*y(k,227) + rxt(k,425) &
                      *y(k,230) + rxt(k,442)*y(k,228) + rxt(k,454)*y(k,263) + rxt(k,466) &
                      *y(k,244) + rxt(k,475)*y(k,246) + rxt(k,499)*y(k,239) + rxt(k,504) &
                      *y(k,240) + rxt(k,513)*y(k,241) + rxt(k,524)*y(k,268) + rxt(k,578) &
                      *y(k,254) + rxt(k,583)*y(k,264) + rxt(k,588)*y(k,265))
         mat(k,2696) = -rxt(k,253)*y(k,231)
         mat(k,1171) = -(rxt(k,372) + rxt(k,373)) * y(k,231)
         mat(k,2407) = -rxt(k,394)*y(k,231)
         mat(k,2560) = -rxt(k,395)*y(k,231)
         mat(k,995) = -rxt(k,414)*y(k,231)
         mat(k,1547) = -rxt(k,425)*y(k,231)
         mat(k,892) = -rxt(k,442)*y(k,231)
         mat(k,1320) = -rxt(k,454)*y(k,231)
         mat(k,1476) = -rxt(k,466)*y(k,231)
         mat(k,1516) = -rxt(k,475)*y(k,231)
         mat(k,1421) = -rxt(k,499)*y(k,231)
         mat(k,1454) = -rxt(k,504)*y(k,231)
         mat(k,1374) = -rxt(k,513)*y(k,231)
         mat(k,1352) = -rxt(k,524)*y(k,231)
         mat(k,1220) = -rxt(k,578)*y(k,231)
         mat(k,1284) = -rxt(k,583)*y(k,231)
         mat(k,1305) = -rxt(k,588)*y(k,231)
         mat(k,1193) = .280_r8*rxt(k,441)*y(k,166)
         mat(k,776) = rxt(k,428)*y(k,259)
         mat(k,470) = .700_r8*rxt(k,397)*y(k,259)
         mat(k,1640) = rxt(k,245)*y(k,72) + rxt(k,346)*y(k,91) + rxt(k,404)*y(k,255) &
                      + rxt(k,398)*y(k,259)
         mat(k,2274) = rxt(k,245)*y(k,66)
         mat(k,984) = rxt(k,346)*y(k,66)
         mat(k,950) = .050_r8*rxt(k,518)*y(k,166)
         mat(k,2560) = mat(k,2560) + rxt(k,427)*y(k,230) + .830_r8*rxt(k,544)*y(k,232) &
                      + .170_r8*rxt(k,550)*y(k,247)
         mat(k,2011) = .280_r8*rxt(k,441)*y(k,35) + .050_r8*rxt(k,518)*y(k,129)
         mat(k,1547) = mat(k,1547) + rxt(k,427)*y(k,155) + 4.000_r8*rxt(k,424) &
                      *y(k,230) + .900_r8*rxt(k,425)*y(k,231) + .450_r8*rxt(k,426) &
                      *y(k,237) + rxt(k,498)*y(k,239) + rxt(k,503)*y(k,240) &
                      + rxt(k,512)*y(k,241) + rxt(k,465)*y(k,244) + rxt(k,474) &
                      *y(k,246) + rxt(k,523)*y(k,268)
         mat(k,1737) = mat(k,1737) + .900_r8*rxt(k,425)*y(k,230)
         mat(k,850) = .830_r8*rxt(k,544)*y(k,155) + .330_r8*rxt(k,543)*y(k,237)
         mat(k,2407) = mat(k,2407) + .450_r8*rxt(k,426)*y(k,230) + .330_r8*rxt(k,543) &
                      *y(k,232) + .070_r8*rxt(k,549)*y(k,247)
         mat(k,1421) = mat(k,1421) + rxt(k,498)*y(k,230)
         mat(k,1454) = mat(k,1454) + rxt(k,503)*y(k,230)
         mat(k,1374) = mat(k,1374) + rxt(k,512)*y(k,230)
         mat(k,1476) = mat(k,1476) + rxt(k,465)*y(k,230)
         mat(k,1516) = mat(k,1516) + rxt(k,474)*y(k,230)
         mat(k,1006) = .170_r8*rxt(k,550)*y(k,155) + .070_r8*rxt(k,549)*y(k,237)
         mat(k,1860) = rxt(k,404)*y(k,66)
         mat(k,2187) = rxt(k,428)*y(k,61) + .700_r8*rxt(k,397)*y(k,65) + rxt(k,398) &
                      *y(k,66)
         mat(k,1352) = mat(k,1352) + rxt(k,523)*y(k,230)
         mat(k,847) = -(rxt(k,543)*y(k,237) + rxt(k,544)*y(k,155) + rxt(k,545) &
                      *y(k,156))
         mat(k,2367) = -rxt(k,543)*y(k,232)
         mat(k,2517) = -rxt(k,544)*y(k,232)
         mat(k,1923) = -rxt(k,545)*y(k,232)
         mat(k,921) = -(rxt(k,722)*y(k,252) + rxt(k,723)*y(k,258) + rxt(k,724) &
                      *y(k,251))
         mat(k,902) = -rxt(k,722)*y(k,233)
         mat(k,910) = -rxt(k,723)*y(k,233)
         mat(k,768) = -rxt(k,724)*y(k,233)
         mat(k,635) = -((rxt(k,462) + rxt(k,463)) * y(k,155))
         mat(k,2506) = -(rxt(k,462) + rxt(k,463)) * y(k,234)
         mat(k,401) = rxt(k,461)*y(k,259)
         mat(k,2113) = rxt(k,461)*y(k,18)
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
         mat(k,516) = -(rxt(k,433)*y(k,165))
         mat(k,1757) = -rxt(k,433)*y(k,235)
         mat(k,2499) = .750_r8*rxt(k,431)*y(k,236)
         mat(k,856) = .750_r8*rxt(k,431)*y(k,155)
         mat(k,857) = -(rxt(k,430)*y(k,237) + rxt(k,431)*y(k,155))
         mat(k,2368) = -rxt(k,430)*y(k,236)
         mat(k,2518) = -rxt(k,431)*y(k,236)
         mat(k,628) = rxt(k,437)*y(k,259)
         mat(k,2135) = rxt(k,437)*y(k,30)
         mat(k,2419) = -((rxt(k,202) + rxt(k,203) + rxt(k,204)) * y(k,94) + rxt(k,206) &
                      *y(k,164) + rxt(k,207)*y(k,166) + rxt(k,211)*y(k,259) &
                      + 4._r8*rxt(k,216)*y(k,237) + rxt(k,228)*y(k,157) + rxt(k,233) &
                      *y(k,155) + rxt(k,238)*y(k,156) + (rxt(k,248) + rxt(k,249) &
                      ) * y(k,72) + rxt(k,257)*y(k,76) + rxt(k,284)*y(k,19) + rxt(k,291) &
                      *y(k,23) + rxt(k,313)*y(k,117) + rxt(k,328)*y(k,127) + rxt(k,374) &
                      *y(k,70) + rxt(k,388)*y(k,53) + rxt(k,394)*y(k,231) + rxt(k,401) &
                      *y(k,238) + rxt(k,415)*y(k,227) + rxt(k,426)*y(k,230) + rxt(k,430) &
                      *y(k,236) + rxt(k,443)*y(k,228) + rxt(k,451)*y(k,262) + rxt(k,455) &
                      *y(k,263) + rxt(k,467)*y(k,244) + rxt(k,476)*y(k,246) + rxt(k,480) &
                      *y(k,248) + rxt(k,490)*y(k,222) + rxt(k,500)*y(k,239) + rxt(k,505) &
                      *y(k,240) + rxt(k,514)*y(k,241) + rxt(k,525)*y(k,268) + rxt(k,529) &
                      *y(k,221) + rxt(k,532)*y(k,224) + rxt(k,536)*y(k,226) + rxt(k,539) &
                      *y(k,229) + rxt(k,543)*y(k,232) + rxt(k,546)*y(k,245) + rxt(k,549) &
                      *y(k,247) + rxt(k,552)*y(k,261) + rxt(k,559)*y(k,266) + rxt(k,565) &
                      *y(k,269) + rxt(k,568)*y(k,271) + rxt(k,579)*y(k,254) + rxt(k,584) &
                      *y(k,264) + rxt(k,589)*y(k,265))
         mat(k,1896) = -(rxt(k,202) + rxt(k,203) + rxt(k,204)) * y(k,237)
         mat(k,2678) = -rxt(k,206)*y(k,237)
         mat(k,2023) = -rxt(k,207)*y(k,237)
         mat(k,2199) = -rxt(k,211)*y(k,237)
         mat(k,2780) = -rxt(k,228)*y(k,237)
         mat(k,2572) = -rxt(k,233)*y(k,237)
         mat(k,1954) = -rxt(k,238)*y(k,237)
         mat(k,2286) = -(rxt(k,248) + rxt(k,249)) * y(k,237)
         mat(k,2708) = -rxt(k,257)*y(k,237)
         mat(k,2316) = -rxt(k,284)*y(k,237)
         mat(k,2448) = -rxt(k,291)*y(k,237)
         mat(k,2229) = -rxt(k,313)*y(k,237)
         mat(k,2631) = -rxt(k,328)*y(k,237)
         mat(k,1176) = -rxt(k,374)*y(k,237)
         mat(k,2479) = -rxt(k,388)*y(k,237)
         mat(k,1747) = -rxt(k,394)*y(k,237)
         mat(k,492) = -rxt(k,401)*y(k,237)
         mat(k,999) = -rxt(k,415)*y(k,237)
         mat(k,1552) = -rxt(k,426)*y(k,237)
         mat(k,862) = -rxt(k,430)*y(k,237)
         mat(k,896) = -rxt(k,443)*y(k,237)
         mat(k,884) = -rxt(k,451)*y(k,237)
         mat(k,1323) = -rxt(k,455)*y(k,237)
         mat(k,1479) = -rxt(k,467)*y(k,237)
         mat(k,1520) = -rxt(k,476)*y(k,237)
         mat(k,761) = -rxt(k,480)*y(k,237)
         mat(k,1083) = -rxt(k,490)*y(k,237)
         mat(k,1425) = -rxt(k,500)*y(k,237)
         mat(k,1458) = -rxt(k,505)*y(k,237)
         mat(k,1377) = -rxt(k,514)*y(k,237)
         mat(k,1355) = -rxt(k,525)*y(k,237)
         mat(k,596) = -rxt(k,529)*y(k,237)
         mat(k,560) = -rxt(k,532)*y(k,237)
         mat(k,487) = -rxt(k,536)*y(k,237)
         mat(k,708) = -rxt(k,539)*y(k,237)
         mat(k,853) = -rxt(k,543)*y(k,237)
         mat(k,806) = -rxt(k,546)*y(k,237)
         mat(k,1009) = -rxt(k,549)*y(k,237)
         mat(k,500) = -rxt(k,552)*y(k,237)
         mat(k,821) = -rxt(k,559)*y(k,237)
         mat(k,845) = -rxt(k,565)*y(k,237)
         mat(k,575) = -rxt(k,568)*y(k,237)
         mat(k,1223) = -rxt(k,579)*y(k,237)
         mat(k,1287) = -rxt(k,584)*y(k,237)
         mat(k,1308) = -rxt(k,589)*y(k,237)
         mat(k,1066) = .570_r8*rxt(k,573)*y(k,166)
         mat(k,170) = .650_r8*rxt(k,531)*y(k,259)
         mat(k,2316) = mat(k,2316) + rxt(k,283)*y(k,53)
         mat(k,2448) = mat(k,2448) + rxt(k,298)*y(k,259)
         mat(k,319) = .350_r8*rxt(k,410)*y(k,259)
         mat(k,633) = .130_r8*rxt(k,412)*y(k,166)
         mat(k,290) = rxt(k,417)*y(k,259)
         mat(k,1198) = .280_r8*rxt(k,441)*y(k,166)
         mat(k,2479) = mat(k,2479) + rxt(k,283)*y(k,19) + rxt(k,244)*y(k,72) &
                      + rxt(k,389)*y(k,157) + rxt(k,390)*y(k,164)
         mat(k,686) = rxt(k,361)*y(k,72) + rxt(k,362)*y(k,259)
         mat(k,425) = rxt(k,364)*y(k,72) + rxt(k,365)*y(k,259)
         mat(k,78) = rxt(k,423)*y(k,259)
         mat(k,390) = rxt(k,366)*y(k,72) + rxt(k,367)*y(k,259)
         mat(k,875) = rxt(k,396)*y(k,259)
         mat(k,1648) = rxt(k,405)*y(k,255)
         mat(k,1176) = mat(k,1176) + rxt(k,376)*y(k,155) + rxt(k,377)*y(k,157) + ( &
                      + 2.000_r8*rxt(k,372)+rxt(k,373))*y(k,231)
         mat(k,2286) = mat(k,2286) + rxt(k,244)*y(k,53) + rxt(k,361)*y(k,54) &
                      + rxt(k,364)*y(k,57) + rxt(k,366)*y(k,63) + rxt(k,247)*y(k,97)
         mat(k,2708) = mat(k,2708) + rxt(k,253)*y(k,231) + rxt(k,264)*y(k,259)
         mat(k,1272) = rxt(k,408)*y(k,259)
         mat(k,215) = .730_r8*rxt(k,542)*y(k,259)
         mat(k,1132) = .500_r8*rxt(k,614)*y(k,259)
         mat(k,1267) = rxt(k,434)*y(k,259)
         mat(k,1097) = rxt(k,435)*y(k,259)
         mat(k,1896) = mat(k,1896) + rxt(k,205)*y(k,165)
         mat(k,676) = rxt(k,247)*y(k,72) + rxt(k,201)*y(k,164) + rxt(k,210)*y(k,259)
         mat(k,190) = rxt(k,399)*y(k,259)
         mat(k,1014) = rxt(k,400)*y(k,259)
         mat(k,1251) = rxt(k,464)*y(k,259)
         mat(k,1260) = rxt(k,449)*y(k,259)
         mat(k,2631) = mat(k,2631) + rxt(k,334)*y(k,259)
         mat(k,953) = .370_r8*rxt(k,518)*y(k,166)
         mat(k,649) = .300_r8*rxt(k,509)*y(k,259)
         mat(k,617) = rxt(k,510)*y(k,259)
         mat(k,507) = rxt(k,517)*y(k,259)
         mat(k,1392) = .140_r8*rxt(k,471)*y(k,166)
         mat(k,336) = .200_r8*rxt(k,473)*y(k,259)
         mat(k,669) = .500_r8*rxt(k,484)*y(k,259)
         mat(k,1039) = .570_r8*rxt(k,576)*y(k,166)
         mat(k,1502) = .280_r8*rxt(k,485)*y(k,166)
         mat(k,431) = rxt(k,521)*y(k,259)
         mat(k,1164) = rxt(k,522)*y(k,259)
         mat(k,2572) = mat(k,2572) + rxt(k,376)*y(k,70) + rxt(k,491)*y(k,222) &
                      + rxt(k,533)*y(k,224) + rxt(k,538)*y(k,226) + rxt(k,416) &
                      *y(k,227) + rxt(k,444)*y(k,228) + rxt(k,395)*y(k,231) &
                      + .170_r8*rxt(k,544)*y(k,232) + rxt(k,462)*y(k,234) &
                      + .250_r8*rxt(k,431)*y(k,236) + rxt(k,403)*y(k,238) &
                      + .920_r8*rxt(k,501)*y(k,239) + .920_r8*rxt(k,507)*y(k,240) &
                      + rxt(k,515)*y(k,241) + .470_r8*rxt(k,469)*y(k,244) &
                      + .400_r8*rxt(k,547)*y(k,245) + .830_r8*rxt(k,550)*y(k,247) &
                      + rxt(k,553)*y(k,261) + rxt(k,452)*y(k,262) + .900_r8*rxt(k,585) &
                      *y(k,264) + .800_r8*rxt(k,590)*y(k,265) + rxt(k,560)*y(k,266) &
                      + rxt(k,526)*y(k,268) + rxt(k,566)*y(k,269) + rxt(k,569) &
                      *y(k,271)
         mat(k,2780) = mat(k,2780) + rxt(k,389)*y(k,53) + rxt(k,377)*y(k,70) &
                      + rxt(k,502)*y(k,239) + rxt(k,508)*y(k,240) + rxt(k,516) &
                      *y(k,241) + .470_r8*rxt(k,468)*y(k,244) + rxt(k,231)*y(k,259) &
                      + rxt(k,527)*y(k,268)
         mat(k,2678) = mat(k,2678) + rxt(k,390)*y(k,53) + rxt(k,201)*y(k,97)
         mat(k,1780) = rxt(k,205)*y(k,94) + rxt(k,433)*y(k,235)
         mat(k,2023) = mat(k,2023) + .570_r8*rxt(k,573)*y(k,8) + .130_r8*rxt(k,412) &
                      *y(k,30) + .280_r8*rxt(k,441)*y(k,35) + .370_r8*rxt(k,518) &
                      *y(k,129) + .140_r8*rxt(k,471)*y(k,136) + .570_r8*rxt(k,576) &
                      *y(k,141) + .280_r8*rxt(k,485)*y(k,142) + rxt(k,213)*y(k,259)
         mat(k,179) = .800_r8*rxt(k,554)*y(k,259)
         mat(k,1149) = rxt(k,604)*y(k,259)
         mat(k,1236) = .200_r8*rxt(k,594)*y(k,259)
         mat(k,210) = .280_r8*rxt(k,562)*y(k,259)
         mat(k,236) = .380_r8*rxt(k,564)*y(k,259)
         mat(k,241) = .630_r8*rxt(k,570)*y(k,259)
         mat(k,1083) = mat(k,1083) + rxt(k,491)*y(k,155)
         mat(k,560) = mat(k,560) + rxt(k,533)*y(k,155)
         mat(k,487) = mat(k,487) + rxt(k,538)*y(k,155)
         mat(k,999) = mat(k,999) + rxt(k,416)*y(k,155) + 2.400_r8*rxt(k,413)*y(k,227) &
                      + rxt(k,414)*y(k,231)
         mat(k,896) = mat(k,896) + rxt(k,444)*y(k,155) + rxt(k,442)*y(k,231)
         mat(k,1552) = mat(k,1552) + .900_r8*rxt(k,425)*y(k,231) + rxt(k,498)*y(k,239) &
                      + rxt(k,503)*y(k,240) + rxt(k,512)*y(k,241) + .470_r8*rxt(k,465) &
                      *y(k,244) + rxt(k,523)*y(k,268)
         mat(k,1747) = mat(k,1747) + (2.000_r8*rxt(k,372)+rxt(k,373))*y(k,70) &
                      + rxt(k,253)*y(k,76) + rxt(k,395)*y(k,155) + rxt(k,414)*y(k,227) &
                      + rxt(k,442)*y(k,228) + .900_r8*rxt(k,425)*y(k,230) &
                      + 4.000_r8*rxt(k,392)*y(k,231) + rxt(k,499)*y(k,239) &
                      + rxt(k,504)*y(k,240) + 1.200_r8*rxt(k,513)*y(k,241) &
                      + .730_r8*rxt(k,466)*y(k,244) + rxt(k,475)*y(k,246) &
                      + .500_r8*rxt(k,578)*y(k,254) + .300_r8*rxt(k,454)*y(k,263) &
                      + rxt(k,583)*y(k,264) + rxt(k,588)*y(k,265) + .800_r8*rxt(k,524) &
                      *y(k,268)
         mat(k,853) = mat(k,853) + .170_r8*rxt(k,544)*y(k,155) + .070_r8*rxt(k,543) &
                      *y(k,237)
         mat(k,640) = rxt(k,462)*y(k,155)
         mat(k,519) = rxt(k,433)*y(k,165)
         mat(k,862) = mat(k,862) + .250_r8*rxt(k,431)*y(k,155)
         mat(k,2419) = mat(k,2419) + .070_r8*rxt(k,543)*y(k,232) + .160_r8*rxt(k,546) &
                      *y(k,245) + .330_r8*rxt(k,549)*y(k,247)
         mat(k,492) = mat(k,492) + rxt(k,403)*y(k,155)
         mat(k,1425) = mat(k,1425) + .920_r8*rxt(k,501)*y(k,155) + rxt(k,502)*y(k,157) &
                      + rxt(k,498)*y(k,230) + rxt(k,499)*y(k,231)
         mat(k,1458) = mat(k,1458) + .920_r8*rxt(k,507)*y(k,155) + rxt(k,508)*y(k,157) &
                      + rxt(k,503)*y(k,230) + rxt(k,504)*y(k,231)
         mat(k,1377) = mat(k,1377) + rxt(k,515)*y(k,155) + rxt(k,516)*y(k,157) &
                      + rxt(k,512)*y(k,230) + 1.200_r8*rxt(k,513)*y(k,231)
         mat(k,1479) = mat(k,1479) + .470_r8*rxt(k,469)*y(k,155) + .470_r8*rxt(k,468) &
                      *y(k,157) + .470_r8*rxt(k,465)*y(k,230) + .730_r8*rxt(k,466) &
                      *y(k,231)
         mat(k,806) = mat(k,806) + .400_r8*rxt(k,547)*y(k,155) + .160_r8*rxt(k,546) &
                      *y(k,237)
         mat(k,1520) = mat(k,1520) + rxt(k,475)*y(k,231)
         mat(k,1009) = mat(k,1009) + .830_r8*rxt(k,550)*y(k,155) + .330_r8*rxt(k,549) &
                      *y(k,237)
         mat(k,1223) = mat(k,1223) + .500_r8*rxt(k,578)*y(k,231)
         mat(k,1872) = rxt(k,405)*y(k,66)
         mat(k,2199) = mat(k,2199) + .650_r8*rxt(k,531)*y(k,9) + rxt(k,298)*y(k,23) &
                      + .350_r8*rxt(k,410)*y(k,29) + rxt(k,417)*y(k,32) + rxt(k,362) &
                      *y(k,54) + rxt(k,365)*y(k,57) + rxt(k,423)*y(k,58) + rxt(k,367) &
                      *y(k,63) + rxt(k,396)*y(k,64) + rxt(k,264)*y(k,76) + rxt(k,408) &
                      *y(k,79) + .730_r8*rxt(k,542)*y(k,84) + .500_r8*rxt(k,614) &
                      *y(k,85) + rxt(k,434)*y(k,92) + rxt(k,435)*y(k,93) + rxt(k,210) &
                      *y(k,97) + rxt(k,399)*y(k,104) + rxt(k,400)*y(k,105) &
                      + rxt(k,464)*y(k,113) + rxt(k,449)*y(k,115) + rxt(k,334) &
                      *y(k,127) + .300_r8*rxt(k,509)*y(k,130) + rxt(k,510)*y(k,131) &
                      + rxt(k,517)*y(k,132) + .200_r8*rxt(k,473)*y(k,137) &
                      + .500_r8*rxt(k,484)*y(k,140) + rxt(k,521)*y(k,146) + rxt(k,522) &
                      *y(k,147) + rxt(k,231)*y(k,157) + rxt(k,213)*y(k,166) &
                      + .800_r8*rxt(k,554)*y(k,175) + rxt(k,604)*y(k,184) &
                      + .200_r8*rxt(k,594)*y(k,211) + .280_r8*rxt(k,562)*y(k,213) &
                      + .380_r8*rxt(k,564)*y(k,215) + .630_r8*rxt(k,570)*y(k,217)
         mat(k,500) = mat(k,500) + rxt(k,553)*y(k,155)
         mat(k,884) = mat(k,884) + rxt(k,452)*y(k,155)
         mat(k,1323) = mat(k,1323) + .300_r8*rxt(k,454)*y(k,231)
         mat(k,1287) = mat(k,1287) + .900_r8*rxt(k,585)*y(k,155) + rxt(k,583)*y(k,231)
         mat(k,1308) = mat(k,1308) + .800_r8*rxt(k,590)*y(k,155) + rxt(k,588)*y(k,231)
         mat(k,821) = mat(k,821) + rxt(k,560)*y(k,155)
         mat(k,1355) = mat(k,1355) + rxt(k,526)*y(k,155) + rxt(k,527)*y(k,157) &
                      + rxt(k,523)*y(k,230) + .800_r8*rxt(k,524)*y(k,231)
         mat(k,845) = mat(k,845) + rxt(k,566)*y(k,155)
         mat(k,575) = mat(k,575) + rxt(k,569)*y(k,155)
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
         mat(k,489) = -(rxt(k,401)*y(k,237) + rxt(k,403)*y(k,155))
         mat(k,2342) = -rxt(k,401)*y(k,238)
         mat(k,2496) = -rxt(k,403)*y(k,238)
         mat(k,2458) = rxt(k,388)*y(k,237)
         mat(k,2342) = mat(k,2342) + rxt(k,388)*y(k,53)
         mat(k,1415) = -(rxt(k,498)*y(k,230) + rxt(k,499)*y(k,231) + rxt(k,500) &
                      *y(k,237) + rxt(k,501)*y(k,155) + rxt(k,502)*y(k,157))
         mat(k,1539) = -rxt(k,498)*y(k,239)
         mat(k,1729) = -rxt(k,499)*y(k,239)
         mat(k,2394) = -rxt(k,500)*y(k,239)
         mat(k,2549) = -rxt(k,501)*y(k,239)
         mat(k,2756) = -rxt(k,502)*y(k,239)
         mat(k,946) = .600_r8*rxt(k,519)*y(k,259)
         mat(k,2173) = .600_r8*rxt(k,519)*y(k,129)
         mat(k,1448) = -(rxt(k,503)*y(k,230) + rxt(k,504)*y(k,231) + rxt(k,505) &
                      *y(k,237) + rxt(k,507)*y(k,155) + rxt(k,508)*y(k,157))
         mat(k,1540) = -rxt(k,503)*y(k,240)
         mat(k,1730) = -rxt(k,504)*y(k,240)
         mat(k,2395) = -rxt(k,505)*y(k,240)
         mat(k,2550) = -rxt(k,507)*y(k,240)
         mat(k,2757) = -rxt(k,508)*y(k,240)
         mat(k,947) = .400_r8*rxt(k,519)*y(k,259)
         mat(k,2174) = .400_r8*rxt(k,519)*y(k,129)
         mat(k,1368) = -(rxt(k,512)*y(k,230) + rxt(k,513)*y(k,231) + rxt(k,514) &
                      *y(k,237) + rxt(k,515)*y(k,155) + rxt(k,516)*y(k,157))
         mat(k,1536) = -rxt(k,512)*y(k,241)
         mat(k,1726) = -rxt(k,513)*y(k,241)
         mat(k,2391) = -rxt(k,514)*y(k,241)
         mat(k,2546) = -rxt(k,515)*y(k,241)
         mat(k,2753) = -rxt(k,516)*y(k,241)
         mat(k,944) = rxt(k,511)*y(k,157)
         mat(k,2753) = mat(k,2753) + rxt(k,511)*y(k,129)
         mat(k,1472) = -(rxt(k,465)*y(k,230) + rxt(k,466)*y(k,231) + rxt(k,467) &
                      *y(k,237) + rxt(k,468)*y(k,157) + (rxt(k,469) + rxt(k,470) &
                      ) * y(k,155))
         mat(k,1541) = -rxt(k,465)*y(k,244)
         mat(k,1731) = -rxt(k,466)*y(k,244)
         mat(k,2396) = -rxt(k,467)*y(k,244)
         mat(k,2758) = -rxt(k,468)*y(k,244)
         mat(k,2551) = -(rxt(k,469) + rxt(k,470)) * y(k,244)
         mat(k,1386) = .500_r8*rxt(k,472)*y(k,259)
         mat(k,333) = .200_r8*rxt(k,473)*y(k,259)
         mat(k,1491) = rxt(k,486)*y(k,259)
         mat(k,2175) = .500_r8*rxt(k,472)*y(k,136) + .200_r8*rxt(k,473)*y(k,137) &
                      + rxt(k,486)*y(k,142)
         mat(k,802) = -(rxt(k,546)*y(k,237) + rxt(k,547)*y(k,155) + rxt(k,548) &
                      *y(k,156))
         mat(k,2364) = -rxt(k,546)*y(k,245)
         mat(k,2514) = -rxt(k,547)*y(k,245)
         mat(k,1922) = -rxt(k,548)*y(k,245)
         mat(k,1513) = -(rxt(k,474)*y(k,230) + rxt(k,475)*y(k,231) + rxt(k,476) &
                      *y(k,237) + 4._r8*rxt(k,477)*y(k,246) + rxt(k,478)*y(k,155) &
                      + rxt(k,479)*y(k,157) + rxt(k,487)*y(k,156))
         mat(k,1543) = -rxt(k,474)*y(k,246)
         mat(k,1733) = -rxt(k,475)*y(k,246)
         mat(k,2398) = -rxt(k,476)*y(k,246)
         mat(k,2553) = -rxt(k,478)*y(k,246)
         mat(k,2760) = -rxt(k,479)*y(k,246)
         mat(k,1934) = -rxt(k,487)*y(k,246)
         mat(k,1387) = .500_r8*rxt(k,472)*y(k,259)
         mat(k,334) = .500_r8*rxt(k,473)*y(k,259)
         mat(k,2177) = .500_r8*rxt(k,472)*y(k,136) + .500_r8*rxt(k,473)*y(k,137)
         mat(k,1002) = -(rxt(k,549)*y(k,237) + rxt(k,550)*y(k,155) + rxt(k,551) &
                      *y(k,156))
         mat(k,2373) = -rxt(k,549)*y(k,247)
         mat(k,2526) = -rxt(k,550)*y(k,247)
         mat(k,1926) = -rxt(k,551)*y(k,247)
         mat(k,756) = -(rxt(k,480)*y(k,237) + rxt(k,481)*y(k,155))
         mat(k,2360) = -rxt(k,480)*y(k,248)
         mat(k,2513) = -rxt(k,481)*y(k,248)
         mat(k,584) = rxt(k,482)*y(k,259)
         mat(k,338) = rxt(k,483)*y(k,259)
         mat(k,2126) = rxt(k,482)*y(k,138) + rxt(k,483)*y(k,139)
         mat(k,598) = -(rxt(k,218)*y(k,164) + rxt(k,219)*y(k,165))
         mat(k,2645) = -rxt(k,218)*y(k,250)
         mat(k,1759) = -rxt(k,219)*y(k,250)
         mat(k,2645) = mat(k,2645) + rxt(k,726)*y(k,251)
         mat(k,916) = .900_r8*rxt(k,724)*y(k,251) + .800_r8*rxt(k,722)*y(k,252)
         mat(k,763) = rxt(k,726)*y(k,164) + .900_r8*rxt(k,724)*y(k,233)
         mat(k,900) = .800_r8*rxt(k,722)*y(k,233)
         mat(k,764) = -(rxt(k,724)*y(k,233) + rxt(k,725)*y(k,165) + (rxt(k,726) &
                      + rxt(k,727)) * y(k,164))
         mat(k,917) = -rxt(k,724)*y(k,251)
         mat(k,1760) = -rxt(k,725)*y(k,251)
         mat(k,2647) = -(rxt(k,726) + rxt(k,727)) * y(k,251)
         mat(k,901) = -(rxt(k,722)*y(k,233))
         mat(k,919) = -rxt(k,722)*y(k,252)
         mat(k,1101) = rxt(k,731)*y(k,258)
         mat(k,2521) = rxt(k,733)*y(k,258)
         mat(k,2650) = rxt(k,726)*y(k,251)
         mat(k,1763) = rxt(k,730)*y(k,253)
         mat(k,766) = rxt(k,726)*y(k,164)
         mat(k,564) = rxt(k,730)*y(k,165)
         mat(k,908) = rxt(k,731)*y(k,143) + rxt(k,733)*y(k,155)
         mat(k,562) = -(rxt(k,728)*y(k,164) + (rxt(k,729) + rxt(k,730)) * y(k,165))
         mat(k,2644) = -rxt(k,728)*y(k,253)
         mat(k,1758) = -(rxt(k,729) + rxt(k,730)) * y(k,253)
         mat(k,1216) = -(rxt(k,578)*y(k,231) + rxt(k,579)*y(k,237) + rxt(k,580) &
                      *y(k,155) + rxt(k,581)*y(k,157))
         mat(k,1717) = -rxt(k,578)*y(k,254)
         mat(k,2381) = -rxt(k,579)*y(k,254)
         mat(k,2536) = -rxt(k,580)*y(k,254)
         mat(k,2742) = -rxt(k,581)*y(k,254)
         mat(k,1056) = rxt(k,572)*y(k,157)
         mat(k,1029) = rxt(k,575)*y(k,157)
         mat(k,2742) = mat(k,2742) + rxt(k,572)*y(k,8) + rxt(k,575)*y(k,141) &
                      + .500_r8*rxt(k,592)*y(k,210)
         mat(k,435) = rxt(k,582)*y(k,259)
         mat(k,1138) = .500_r8*rxt(k,592)*y(k,157)
         mat(k,2159) = rxt(k,582)*y(k,159)
         mat(k,1864) = -(rxt(k,183)*y(k,95) + rxt(k,184)*y(k,272) + (rxt(k,186) &
                      + rxt(k,187)) * y(k,165) + rxt(k,188)*y(k,166) + (rxt(k,236) &
                      + rxt(k,237)) * y(k,144) + rxt(k,271)*y(k,39) + rxt(k,272) &
                      *y(k,40) + rxt(k,273)*y(k,42) + rxt(k,274)*y(k,43) + rxt(k,275) &
                      *y(k,44) + rxt(k,276)*y(k,45) + rxt(k,277)*y(k,46) + (rxt(k,278) &
                      + rxt(k,279)) * y(k,103) + rxt(k,302)*y(k,41) + rxt(k,303) &
                      *y(k,68) + rxt(k,304)*y(k,96) + (rxt(k,305) + rxt(k,306) &
                      ) * y(k,99) + rxt(k,350)*y(k,82) + rxt(k,351)*y(k,83) + rxt(k,383) &
                      *y(k,47) + rxt(k,384)*y(k,54) + rxt(k,385)*y(k,100) + rxt(k,386) &
                      *y(k,101) + rxt(k,387)*y(k,102) + (rxt(k,404) + rxt(k,405) &
                      + rxt(k,406)) * y(k,66) + rxt(k,407)*y(k,104))
         mat(k,1609) = -rxt(k,183)*y(k,255)
         mat(k,2802) = -rxt(k,184)*y(k,255)
         mat(k,1773) = -(rxt(k,186) + rxt(k,187)) * y(k,255)
         mat(k,2015) = -rxt(k,188)*y(k,255)
         mat(k,280) = -(rxt(k,236) + rxt(k,237)) * y(k,255)
         mat(k,83) = -rxt(k,271)*y(k,255)
         mat(k,142) = -rxt(k,272)*y(k,255)
         mat(k,111) = -rxt(k,273)*y(k,255)
         mat(k,153) = -rxt(k,274)*y(k,255)
         mat(k,115) = -rxt(k,275)*y(k,255)
         mat(k,158) = -rxt(k,276)*y(k,255)
         mat(k,119) = -rxt(k,277)*y(k,255)
         mat(k,1820) = -(rxt(k,278) + rxt(k,279)) * y(k,255)
         mat(k,148) = -rxt(k,302)*y(k,255)
         mat(k,459) = -rxt(k,303)*y(k,255)
         mat(k,95) = -rxt(k,304)*y(k,255)
         mat(k,1582) = -(rxt(k,305) + rxt(k,306)) * y(k,255)
         mat(k,258) = -rxt(k,350)*y(k,255)
         mat(k,249) = -rxt(k,351)*y(k,255)
         mat(k,541) = -rxt(k,383)*y(k,255)
         mat(k,682) = -rxt(k,384)*y(k,255)
         mat(k,244) = -rxt(k,385)*y(k,255)
         mat(k,253) = -rxt(k,386)*y(k,255)
         mat(k,310) = -rxt(k,387)*y(k,255)
         mat(k,1643) = -(rxt(k,404) + rxt(k,405) + rxt(k,406)) * y(k,255)
         mat(k,188) = -rxt(k,407)*y(k,255)
         mat(k,1773) = mat(k,1773) + rxt(k,219)*y(k,250)
         mat(k,926) = .850_r8*rxt(k,723)*y(k,258)
         mat(k,601) = rxt(k,219)*y(k,165)
         mat(k,913) = .850_r8*rxt(k,723)*y(k,233)
         mat(k,180) = -(rxt(k,190)*y(k,164) + rxt(k,191)*y(k,165))
         mat(k,2641) = -rxt(k,190)*y(k,256)
         mat(k,1755) = -rxt(k,191)*y(k,256)
         mat(k,1558) = rxt(k,192)*y(k,257)
         mat(k,2641) = mat(k,2641) + rxt(k,194)*y(k,257)
         mat(k,1755) = mat(k,1755) + rxt(k,195)*y(k,257)
         mat(k,1969) = rxt(k,196)*y(k,257)
         mat(k,182) = rxt(k,192)*y(k,80) + rxt(k,194)*y(k,164) + rxt(k,195)*y(k,165) &
                      + rxt(k,196)*y(k,166)
         mat(k,183) = -(rxt(k,192)*y(k,80) + rxt(k,194)*y(k,164) + rxt(k,195)*y(k,165) &
                      + rxt(k,196)*y(k,166))
         mat(k,1559) = -rxt(k,192)*y(k,257)
         mat(k,2642) = -rxt(k,194)*y(k,257)
         mat(k,1756) = -rxt(k,195)*y(k,257)
         mat(k,1970) = -rxt(k,196)*y(k,257)
         mat(k,1756) = mat(k,1756) + rxt(k,186)*y(k,255)
         mat(k,1843) = rxt(k,186)*y(k,165)
         mat(k,909) = -(rxt(k,723)*y(k,233) + rxt(k,731)*y(k,143) + rxt(k,733) &
                      *y(k,155))
         mat(k,920) = -rxt(k,723)*y(k,258)
         mat(k,1102) = -rxt(k,731)*y(k,258)
         mat(k,2522) = -rxt(k,733)*y(k,258)
         mat(k,1562) = rxt(k,734)*y(k,260)
         mat(k,1764) = rxt(k,725)*y(k,251) + rxt(k,729)*y(k,253) + rxt(k,736)*y(k,260)
         mat(k,767) = rxt(k,725)*y(k,165)
         mat(k,565) = rxt(k,729)*y(k,165)
         mat(k,867) = rxt(k,734)*y(k,80) + rxt(k,736)*y(k,165)
         mat(k,2195) = -(rxt(k,209)*y(k,95) + rxt(k,210)*y(k,97) + rxt(k,211)*y(k,237) &
                      + rxt(k,212)*y(k,164) + rxt(k,213)*y(k,166) + (4._r8*rxt(k,214) &
                      + 4._r8*rxt(k,215)) * y(k,259) + rxt(k,217)*y(k,109) + rxt(k,231) &
                      *y(k,157) + rxt(k,232)*y(k,143) + rxt(k,240)*y(k,156) + rxt(k,241) &
                      *y(k,108) + rxt(k,251)*y(k,75) + rxt(k,262)*y(k,77) + (rxt(k,264) &
                      + rxt(k,265)) * y(k,76) + rxt(k,267)*y(k,103) + rxt(k,270) &
                      *y(k,111) + rxt(k,282)*y(k,20) + rxt(k,298)*y(k,23) + rxt(k,300) &
                      *y(k,99) + rxt(k,308)*y(k,112) + rxt(k,311)*y(k,118) + rxt(k,334) &
                      *y(k,127) + rxt(k,335)*y(k,107) + rxt(k,353)*y(k,28) + rxt(k,355) &
                      *y(k,31) + rxt(k,357)*y(k,47) + rxt(k,358)*y(k,48) + rxt(k,360) &
                      *y(k,49) + rxt(k,362)*y(k,54) + rxt(k,363)*y(k,55) + rxt(k,365) &
                      *y(k,57) + rxt(k,367)*y(k,63) + rxt(k,368)*y(k,67) + rxt(k,370) &
                      *y(k,68) + rxt(k,371)*y(k,69) + rxt(k,379)*y(k,71) + rxt(k,380) &
                      *y(k,100) + rxt(k,381)*y(k,101) + rxt(k,382)*y(k,102) + rxt(k,391) &
                      *y(k,53) + rxt(k,396)*y(k,64) + rxt(k,397)*y(k,65) + rxt(k,398) &
                      *y(k,66) + rxt(k,399)*y(k,104) + rxt(k,400)*y(k,105) + rxt(k,408) &
                      *y(k,79) + rxt(k,410)*y(k,29) + rxt(k,417)*y(k,32) + rxt(k,418) &
                      *y(k,33) + rxt(k,420)*y(k,34) + rxt(k,422)*y(k,56) + rxt(k,423) &
                      *y(k,58) + rxt(k,428)*y(k,61) + rxt(k,429)*y(k,62) + rxt(k,434) &
                      *y(k,92) + rxt(k,435)*y(k,93) + rxt(k,436)*y(k,172) + rxt(k,437) &
                      *y(k,30) + rxt(k,445)*y(k,36) + rxt(k,446)*y(k,37) + rxt(k,448) &
                      *y(k,60) + rxt(k,449)*y(k,115) + rxt(k,450)*y(k,158) + rxt(k,453) &
                      *y(k,179) + rxt(k,457)*y(k,180) + rxt(k,458)*y(k,35) + rxt(k,459) &
                      *y(k,59) + rxt(k,461)*y(k,18) + rxt(k,464)*y(k,113) + rxt(k,472) &
                      *y(k,136) + rxt(k,473)*y(k,137) + rxt(k,482)*y(k,138) + rxt(k,483) &
                      *y(k,139) + rxt(k,484)*y(k,140) + rxt(k,486)*y(k,142) + rxt(k,489) &
                      *y(k,1) + rxt(k,493)*y(k,2) + rxt(k,494)*y(k,17) + rxt(k,495) &
                      *y(k,114) + rxt(k,496)*y(k,116) + rxt(k,497)*y(k,124) + rxt(k,509) &
                      *y(k,130) + rxt(k,510)*y(k,131) + rxt(k,517)*y(k,132) + rxt(k,519) &
                      *y(k,129) + rxt(k,520)*y(k,133) + rxt(k,521)*y(k,146) + rxt(k,522) &
                      *y(k,147) + rxt(k,528)*y(k,214) + rxt(k,531)*y(k,9) + rxt(k,534) &
                      *y(k,10) + rxt(k,535)*y(k,26) + rxt(k,537)*y(k,27) + rxt(k,541) &
                      *y(k,38) + rxt(k,542)*y(k,84) + rxt(k,554)*y(k,175) + rxt(k,557) &
                      *y(k,176) + rxt(k,561)*y(k,212) + rxt(k,562)*y(k,213) + rxt(k,564) &
                      *y(k,215) + rxt(k,567)*y(k,216) + rxt(k,570)*y(k,217) + rxt(k,571) &
                      *y(k,218) + rxt(k,574)*y(k,8) + rxt(k,577)*y(k,141) + rxt(k,582) &
                      *y(k,159) + rxt(k,586)*y(k,207) + rxt(k,587)*y(k,208) + rxt(k,591) &
                      *y(k,209) + rxt(k,593)*y(k,210) + rxt(k,594)*y(k,211) + (rxt(k,600) &
                      + rxt(k,614)) * y(k,85) + rxt(k,602)*y(k,169) + rxt(k,604) &
                      *y(k,184) + rxt(k,608)*y(k,181) + rxt(k,613)*y(k,183) + rxt(k,633) &
                      *y(k,151))
         mat(k,1611) = -rxt(k,209)*y(k,259)
         mat(k,674) = -rxt(k,210)*y(k,259)
         mat(k,2415) = -rxt(k,211)*y(k,259)
         mat(k,2674) = -rxt(k,212)*y(k,259)
         mat(k,2019) = -rxt(k,213)*y(k,259)
         mat(k,535) = -rxt(k,217)*y(k,259)
         mat(k,2776) = -rxt(k,231)*y(k,259)
         mat(k,1111) = -rxt(k,232)*y(k,259)
         mat(k,1950) = -rxt(k,240)*y(k,259)
         mat(k,2594) = -rxt(k,241)*y(k,259)
         mat(k,581) = -rxt(k,251)*y(k,259)
         mat(k,1119) = -rxt(k,262)*y(k,259)
         mat(k,2704) = -(rxt(k,264) + rxt(k,265)) * y(k,259)
         mat(k,1823) = -rxt(k,267)*y(k,259)
         mat(k,1800) = -rxt(k,270)*y(k,259)
         mat(k,523) = -rxt(k,282)*y(k,259)
         mat(k,2444) = -rxt(k,298)*y(k,259)
         mat(k,1584) = -rxt(k,300)*y(k,259)
         mat(k,1667) = -rxt(k,308)*y(k,259)
         mat(k,1595) = -rxt(k,311)*y(k,259)
         mat(k,2627) = -rxt(k,334)*y(k,259)
         mat(k,1335) = -rxt(k,335)*y(k,259)
         mat(k,197) = -rxt(k,353)*y(k,259)
         mat(k,275) = -rxt(k,355)*y(k,259)
         mat(k,542) = -rxt(k,357)*y(k,259)
         mat(k,102) = -rxt(k,358)*y(k,259)
         mat(k,329) = -rxt(k,360)*y(k,259)
         mat(k,683) = -rxt(k,362)*y(k,259)
         mat(k,122) = -rxt(k,363)*y(k,259)
         mat(k,423) = -rxt(k,365)*y(k,259)
         mat(k,387) = -rxt(k,367)*y(k,259)
         mat(k,86) = -rxt(k,368)*y(k,259)
         mat(k,460) = -rxt(k,370)*y(k,259)
         mat(k,90) = -rxt(k,371)*y(k,259)
         mat(k,378) = -rxt(k,379)*y(k,259)
         mat(k,245) = -rxt(k,380)*y(k,259)
         mat(k,254) = -rxt(k,381)*y(k,259)
         mat(k,311) = -rxt(k,382)*y(k,259)
         mat(k,2475) = -rxt(k,391)*y(k,259)
         mat(k,874) = -rxt(k,396)*y(k,259)
         mat(k,472) = -rxt(k,397)*y(k,259)
         mat(k,1646) = -rxt(k,398)*y(k,259)
         mat(k,189) = -rxt(k,399)*y(k,259)
         mat(k,1013) = -rxt(k,400)*y(k,259)
         mat(k,1271) = -rxt(k,408)*y(k,259)
         mat(k,318) = -rxt(k,410)*y(k,259)
         mat(k,289) = -rxt(k,417)*y(k,259)
         mat(k,368) = -rxt(k,418)*y(k,259)
         mat(k,323) = -rxt(k,420)*y(k,259)
         mat(k,1207) = -rxt(k,422)*y(k,259)
         mat(k,77) = -rxt(k,423)*y(k,259)
         mat(k,777) = -rxt(k,428)*y(k,259)
         mat(k,660) = -rxt(k,429)*y(k,259)
         mat(k,1266) = -rxt(k,434)*y(k,259)
         mat(k,1096) = -rxt(k,435)*y(k,259)
         mat(k,608) = -rxt(k,436)*y(k,259)
         mat(k,632) = -rxt(k,437)*y(k,259)
         mat(k,454) = -rxt(k,445)*y(k,259)
         mat(k,107) = -rxt(k,446)*y(k,259)
         mat(k,1399) = -rxt(k,448)*y(k,259)
         mat(k,1259) = -rxt(k,449)*y(k,259)
         mat(k,933) = -rxt(k,450)*y(k,259)
         mat(k,623) = -rxt(k,453)*y(k,259)
         mat(k,448) = -rxt(k,457)*y(k,259)
         mat(k,1197) = -rxt(k,458)*y(k,259)
         mat(k,1090) = -rxt(k,459)*y(k,259)
         mat(k,405) = -rxt(k,461)*y(k,259)
         mat(k,1250) = -rxt(k,464)*y(k,259)
         mat(k,1391) = -rxt(k,472)*y(k,259)
         mat(k,335) = -rxt(k,473)*y(k,259)
         mat(k,587) = -rxt(k,482)*y(k,259)
         mat(k,341) = -rxt(k,483)*y(k,259)
         mat(k,668) = -rxt(k,484)*y(k,259)
         mat(k,1501) = -rxt(k,486)*y(k,259)
         mat(k,718) = -rxt(k,489)*y(k,259)
         mat(k,752) = -rxt(k,493)*y(k,259)
         mat(k,266) = -rxt(k,494)*y(k,259)
         mat(k,262) = -rxt(k,495)*y(k,259)
         mat(k,383) = -rxt(k,496)*y(k,259)
         mat(k,136) = -rxt(k,497)*y(k,259)
         mat(k,648) = -rxt(k,509)*y(k,259)
         mat(k,616) = -rxt(k,510)*y(k,259)
         mat(k,506) = -rxt(k,517)*y(k,259)
         mat(k,952) = -rxt(k,519)*y(k,259)
         mat(k,784) = -rxt(k,520)*y(k,259)
         mat(k,430) = -rxt(k,521)*y(k,259)
         mat(k,1163) = -rxt(k,522)*y(k,259)
         mat(k,222) = -rxt(k,528)*y(k,259)
         mat(k,169) = -rxt(k,531)*y(k,259)
         mat(k,467) = -rxt(k,534)*y(k,259)
         mat(k,269) = -rxt(k,535)*y(k,259)
         mat(k,363) = -rxt(k,537)*y(k,259)
         mat(k,294) = -rxt(k,541)*y(k,259)
         mat(k,214) = -rxt(k,542)*y(k,259)
         mat(k,178) = -rxt(k,554)*y(k,259)
         mat(k,357) = -rxt(k,557)*y(k,259)
         mat(k,742) = -rxt(k,561)*y(k,259)
         mat(k,209) = -rxt(k,562)*y(k,259)
         mat(k,235) = -rxt(k,564)*y(k,259)
         mat(k,800) = -rxt(k,567)*y(k,259)
         mat(k,240) = -rxt(k,570)*y(k,259)
         mat(k,479) = -rxt(k,571)*y(k,259)
         mat(k,1065) = -rxt(k,574)*y(k,259)
         mat(k,1038) = -rxt(k,577)*y(k,259)
         mat(k,437) = -rxt(k,582)*y(k,259)
         mat(k,728) = -rxt(k,586)*y(k,259)
         mat(k,699) = -rxt(k,587)*y(k,259)
         mat(k,550) = -rxt(k,591)*y(k,259)
         mat(k,1142) = -rxt(k,593)*y(k,259)
         mat(k,1235) = -rxt(k,594)*y(k,259)
         mat(k,1128) = -(rxt(k,600) + rxt(k,614)) * y(k,259)
         mat(k,414) = -rxt(k,602)*y(k,259)
         mat(k,1148) = -rxt(k,604)*y(k,259)
         mat(k,828) = -rxt(k,608)*y(k,259)
         mat(k,1625) = -rxt(k,613)*y(k,259)
         mat(k,80) = -rxt(k,633)*y(k,259)
         mat(k,1065) = mat(k,1065) + .630_r8*rxt(k,573)*y(k,166)
         mat(k,318) = mat(k,318) + .650_r8*rxt(k,410)*y(k,259)
         mat(k,632) = mat(k,632) + .130_r8*rxt(k,412)*y(k,166)
         mat(k,368) = mat(k,368) + .500_r8*rxt(k,418)*y(k,259)
         mat(k,1197) = mat(k,1197) + .360_r8*rxt(k,441)*y(k,166)
         mat(k,2475) = mat(k,2475) + rxt(k,390)*y(k,164)
         mat(k,472) = mat(k,472) + .300_r8*rxt(k,397)*y(k,259)
         mat(k,1646) = mat(k,1646) + rxt(k,404)*y(k,255)
         mat(k,2282) = rxt(k,249)*y(k,237)
         mat(k,986) = rxt(k,348)*y(k,272)
         mat(k,1892) = rxt(k,208)*y(k,166) + 2.000_r8*rxt(k,203)*y(k,237)
         mat(k,1611) = mat(k,1611) + rxt(k,200)*y(k,164) + rxt(k,183)*y(k,255)
         mat(k,674) = mat(k,674) + rxt(k,201)*y(k,164)
         mat(k,1584) = mat(k,1584) + rxt(k,299)*y(k,164) + rxt(k,305)*y(k,255)
         mat(k,1823) = mat(k,1823) + rxt(k,266)*y(k,164) + rxt(k,278)*y(k,255)
         mat(k,189) = mat(k,189) + rxt(k,407)*y(k,255)
         mat(k,1690) = rxt(k,301)*y(k,164)
         mat(k,1800) = mat(k,1800) + rxt(k,269)*y(k,164)
         mat(k,952) = mat(k,952) + .320_r8*rxt(k,518)*y(k,166)
         mat(k,784) = mat(k,784) + .600_r8*rxt(k,520)*y(k,259)
         mat(k,1391) = mat(k,1391) + .240_r8*rxt(k,471)*y(k,166)
         mat(k,335) = mat(k,335) + .100_r8*rxt(k,473)*y(k,259)
         mat(k,1038) = mat(k,1038) + .630_r8*rxt(k,576)*y(k,166)
         mat(k,1501) = mat(k,1501) + .360_r8*rxt(k,485)*y(k,166)
         mat(k,2568) = rxt(k,233)*y(k,237)
         mat(k,2776) = mat(k,2776) + rxt(k,228)*y(k,237)
         mat(k,2674) = mat(k,2674) + rxt(k,390)*y(k,53) + rxt(k,200)*y(k,95) &
                      + rxt(k,201)*y(k,97) + rxt(k,299)*y(k,99) + rxt(k,266)*y(k,103) &
                      + rxt(k,301)*y(k,110) + rxt(k,269)*y(k,111) + rxt(k,206) &
                      *y(k,237)
         mat(k,2019) = mat(k,2019) + .630_r8*rxt(k,573)*y(k,8) + .130_r8*rxt(k,412) &
                      *y(k,30) + .360_r8*rxt(k,441)*y(k,35) + rxt(k,208)*y(k,94) &
                      + .320_r8*rxt(k,518)*y(k,129) + .240_r8*rxt(k,471)*y(k,136) &
                      + .630_r8*rxt(k,576)*y(k,141) + .360_r8*rxt(k,485)*y(k,142) &
                      + rxt(k,207)*y(k,237)
         mat(k,623) = mat(k,623) + .500_r8*rxt(k,453)*y(k,259)
         mat(k,222) = mat(k,222) + .500_r8*rxt(k,528)*y(k,259)
         mat(k,595) = .400_r8*rxt(k,529)*y(k,237)
         mat(k,1551) = .450_r8*rxt(k,426)*y(k,237)
         mat(k,852) = .400_r8*rxt(k,543)*y(k,237)
         mat(k,2415) = mat(k,2415) + rxt(k,249)*y(k,72) + 2.000_r8*rxt(k,203)*y(k,94) &
                      + rxt(k,233)*y(k,155) + rxt(k,228)*y(k,157) + rxt(k,206) &
                      *y(k,164) + rxt(k,207)*y(k,166) + .400_r8*rxt(k,529)*y(k,221) &
                      + .450_r8*rxt(k,426)*y(k,230) + .400_r8*rxt(k,543)*y(k,232) &
                      + .450_r8*rxt(k,476)*y(k,246) + .400_r8*rxt(k,549)*y(k,247) &
                      + .200_r8*rxt(k,480)*y(k,248) + .150_r8*rxt(k,455)*y(k,263)
         mat(k,1519) = .450_r8*rxt(k,476)*y(k,237)
         mat(k,1008) = .400_r8*rxt(k,549)*y(k,237)
         mat(k,760) = .200_r8*rxt(k,480)*y(k,237)
         mat(k,1868) = rxt(k,404)*y(k,66) + rxt(k,183)*y(k,95) + rxt(k,305)*y(k,99) &
                      + rxt(k,278)*y(k,103) + rxt(k,407)*y(k,104) &
                      + 2.000_r8*rxt(k,184)*y(k,272)
         mat(k,2195) = mat(k,2195) + .650_r8*rxt(k,410)*y(k,29) + .500_r8*rxt(k,418) &
                      *y(k,33) + .300_r8*rxt(k,397)*y(k,65) + .600_r8*rxt(k,520) &
                      *y(k,133) + .100_r8*rxt(k,473)*y(k,137) + .500_r8*rxt(k,453) &
                      *y(k,179) + .500_r8*rxt(k,528)*y(k,214)
         mat(k,1322) = .150_r8*rxt(k,455)*y(k,237)
         mat(k,2806) = rxt(k,348)*y(k,91) + 2.000_r8*rxt(k,184)*y(k,255)
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
         mat(k,865) = -(rxt(k,734)*y(k,80) + rxt(k,736)*y(k,165))
         mat(k,1560) = -rxt(k,734)*y(k,260)
         mat(k,1762) = -rxt(k,736)*y(k,260)
         mat(k,2649) = rxt(k,727)*y(k,251) + rxt(k,728)*y(k,253)
         mat(k,765) = rxt(k,727)*y(k,164)
         mat(k,563) = rxt(k,728)*y(k,164)
         mat(k,496) = -(rxt(k,552)*y(k,237) + rxt(k,553)*y(k,155))
         mat(k,2343) = -rxt(k,552)*y(k,261)
         mat(k,2497) = -rxt(k,553)*y(k,261)
         mat(k,212) = .200_r8*rxt(k,542)*y(k,259)
         mat(k,176) = .140_r8*rxt(k,554)*y(k,259)
         mat(k,355) = rxt(k,557)*y(k,259)
         mat(k,2096) = .200_r8*rxt(k,542)*y(k,84) + .140_r8*rxt(k,554)*y(k,175) &
                      + rxt(k,557)*y(k,176)
         mat(k,878) = -(rxt(k,451)*y(k,237) + rxt(k,452)*y(k,155))
         mat(k,2369) = -rxt(k,451)*y(k,262)
         mat(k,2519) = -rxt(k,452)*y(k,262)
         mat(k,1182) = rxt(k,458)*y(k,259)
         mat(k,620) = .500_r8*rxt(k,453)*y(k,259)
         mat(k,2137) = rxt(k,458)*y(k,35) + .500_r8*rxt(k,453)*y(k,179)
         mat(k,1317) = -(rxt(k,454)*y(k,231) + rxt(k,455)*y(k,237) + rxt(k,456) &
                      *y(k,155))
         mat(k,1724) = -rxt(k,454)*y(k,263)
         mat(k,2388) = -rxt(k,455)*y(k,263)
         mat(k,2544) = -rxt(k,456)*y(k,263)
         mat(k,1060) = .060_r8*rxt(k,573)*y(k,166)
         mat(k,1087) = rxt(k,459)*y(k,259)
         mat(k,1033) = .060_r8*rxt(k,576)*y(k,166)
         mat(k,1998) = .060_r8*rxt(k,573)*y(k,8) + .060_r8*rxt(k,576)*y(k,141)
         mat(k,446) = rxt(k,457)*y(k,259)
         mat(k,1231) = .150_r8*rxt(k,594)*y(k,259)
         mat(k,2167) = rxt(k,459)*y(k,59) + rxt(k,457)*y(k,180) + .150_r8*rxt(k,594) &
                      *y(k,211)
         mat(k,1280) = -(rxt(k,583)*y(k,231) + rxt(k,584)*y(k,237) + rxt(k,585) &
                      *y(k,155))
         mat(k,1722) = -rxt(k,583)*y(k,264)
         mat(k,2386) = -rxt(k,584)*y(k,264)
         mat(k,2542) = -rxt(k,585)*y(k,264)
         mat(k,2748) = .500_r8*rxt(k,592)*y(k,210)
         mat(k,726) = rxt(k,586)*y(k,259)
         mat(k,1141) = .500_r8*rxt(k,592)*y(k,157) + rxt(k,593)*y(k,259)
         mat(k,2165) = rxt(k,586)*y(k,207) + rxt(k,593)*y(k,210)
         mat(k,1301) = -(rxt(k,588)*y(k,231) + rxt(k,589)*y(k,237) + rxt(k,590) &
                      *y(k,155))
         mat(k,1723) = -rxt(k,588)*y(k,265)
         mat(k,2387) = -rxt(k,589)*y(k,265)
         mat(k,2543) = -rxt(k,590)*y(k,265)
         mat(k,1059) = rxt(k,574)*y(k,259)
         mat(k,1032) = rxt(k,577)*y(k,259)
         mat(k,549) = rxt(k,591)*y(k,259)
         mat(k,2166) = rxt(k,574)*y(k,8) + rxt(k,577)*y(k,141) + rxt(k,591)*y(k,209)
         mat(k,813) = -(rxt(k,559)*y(k,237) + rxt(k,560)*y(k,155))
         mat(k,2365) = -rxt(k,559)*y(k,266)
         mat(k,2515) = -rxt(k,560)*y(k,266)
         mat(k,736) = rxt(k,561)*y(k,259)
         mat(k,208) = .650_r8*rxt(k,562)*y(k,259)
         mat(k,2131) = rxt(k,561)*y(k,212) + .650_r8*rxt(k,562)*y(k,213)
         mat(k,1348) = -(rxt(k,523)*y(k,230) + rxt(k,524)*y(k,231) + rxt(k,525) &
                      *y(k,237) + rxt(k,526)*y(k,155) + rxt(k,527)*y(k,157))
         mat(k,1535) = -rxt(k,523)*y(k,268)
         mat(k,1725) = -rxt(k,524)*y(k,268)
         mat(k,2390) = -rxt(k,525)*y(k,268)
         mat(k,2545) = -rxt(k,526)*y(k,268)
         mat(k,2752) = -rxt(k,527)*y(k,268)
         mat(k,261) = rxt(k,495)*y(k,259)
         mat(k,382) = rxt(k,496)*y(k,259)
         mat(k,135) = rxt(k,497)*y(k,259)
         mat(k,781) = .400_r8*rxt(k,520)*y(k,259)
         mat(k,221) = .500_r8*rxt(k,528)*y(k,259)
         mat(k,2169) = rxt(k,495)*y(k,114) + rxt(k,496)*y(k,116) + rxt(k,497)*y(k,124) &
                      + .400_r8*rxt(k,520)*y(k,133) + .500_r8*rxt(k,528)*y(k,214)
         mat(k,836) = -(rxt(k,565)*y(k,237) + rxt(k,566)*y(k,155))
         mat(k,2366) = -rxt(k,565)*y(k,269)
         mat(k,2516) = -rxt(k,566)*y(k,269)
         mat(k,232) = .560_r8*rxt(k,564)*y(k,259)
         mat(k,793) = rxt(k,567)*y(k,259)
         mat(k,2133) = .560_r8*rxt(k,564)*y(k,215) + rxt(k,567)*y(k,216)
         mat(k,570) = -(rxt(k,568)*y(k,237) + rxt(k,569)*y(k,155))
         mat(k,2351) = -rxt(k,568)*y(k,271)
         mat(k,2502) = -rxt(k,569)*y(k,271)
         mat(k,239) = .300_r8*rxt(k,570)*y(k,259)
         mat(k,476) = rxt(k,571)*y(k,259)
         mat(k,2105) = .300_r8*rxt(k,570)*y(k,217) + rxt(k,571)*y(k,218)
         mat(k,2819) = -(rxt(k,184)*y(k,255) + rxt(k,348)*y(k,91) + rxt(k,615) &
                      *y(k,185))
         mat(k,1881) = -rxt(k,184)*y(k,272)
         mat(k,989) = -rxt(k,348)*y(k,272)
         mat(k,286) = -rxt(k,615)*y(k,272)
         mat(k,277) = rxt(k,355)*y(k,259)
         mat(k,325) = rxt(k,420)*y(k,259)
         mat(k,456) = rxt(k,445)*y(k,259)
         mat(k,108) = rxt(k,446)*y(k,259)
         mat(k,545) = rxt(k,357)*y(k,259)
         mat(k,331) = rxt(k,360)*y(k,259)
         mat(k,2488) = rxt(k,391)*y(k,259)
         mat(k,687) = rxt(k,362)*y(k,259)
         mat(k,124) = rxt(k,363)*y(k,259)
         mat(k,1211) = rxt(k,422)*y(k,259)
         mat(k,426) = rxt(k,365)*y(k,259)
         mat(k,1091) = rxt(k,459)*y(k,259)
         mat(k,1403) = rxt(k,448)*y(k,259)
         mat(k,778) = rxt(k,428)*y(k,259)
         mat(k,662) = rxt(k,429)*y(k,259)
         mat(k,391) = rxt(k,367)*y(k,259)
         mat(k,474) = rxt(k,397)*y(k,259)
         mat(k,1654) = rxt(k,398)*y(k,259)
         mat(k,1180) = rxt(k,374)*y(k,237)
         mat(k,380) = rxt(k,379)*y(k,259)
         mat(k,1905) = rxt(k,204)*y(k,237)
         mat(k,1616) = rxt(k,209)*y(k,259)
         mat(k,678) = rxt(k,210)*y(k,259)
         mat(k,1590) = (rxt(k,623)+rxt(k,683)+rxt(k,696)+rxt(k,705))*y(k,110) + ( &
                      + rxt(k,622)+rxt(k,685)+rxt(k,693)+rxt(k,702))*y(k,111) + ( &
                      + rxt(k,630)+rxt(k,712)+rxt(k,716)+rxt(k,720))*y(k,112) &
                      + rxt(k,300)*y(k,259)
         mat(k,313) = rxt(k,382)*y(k,259)
         mat(k,1833) = (rxt(k,625)+rxt(k,682)+rxt(k,695)+rxt(k,704))*y(k,110) + ( &
                      + rxt(k,624)+rxt(k,681)+rxt(k,692)+rxt(k,701))*y(k,111) + ( &
                      + rxt(k,629)+rxt(k,711)+rxt(k,715)+rxt(k,719))*y(k,112) &
                      + rxt(k,267)*y(k,259)
         mat(k,1015) = rxt(k,400)*y(k,259)
         mat(k,1341) = (rxt(k,627)+rxt(k,684)+rxt(k,697)+rxt(k,706))*y(k,110) + ( &
                      + rxt(k,626)+rxt(k,686)+rxt(k,694)+rxt(k,703))*y(k,111) + ( &
                      + rxt(k,631)+rxt(k,713)+rxt(k,717)+rxt(k,721))*y(k,112) &
                      + rxt(k,335)*y(k,259)
         mat(k,2607) = rxt(k,241)*y(k,259)
         mat(k,538) = rxt(k,217)*y(k,259)
         mat(k,1699) = (rxt(k,623)+rxt(k,683)+rxt(k,696)+rxt(k,705))*y(k,99) + ( &
                      + rxt(k,625)+rxt(k,682)+rxt(k,695)+rxt(k,704))*y(k,103) + ( &
                      + rxt(k,627)+rxt(k,684)+rxt(k,697)+rxt(k,706))*y(k,107)
         mat(k,1810) = (rxt(k,622)+rxt(k,685)+rxt(k,693)+rxt(k,702))*y(k,99) + ( &
                      + rxt(k,624)+rxt(k,681)+rxt(k,692)+rxt(k,701))*y(k,103) + ( &
                      + rxt(k,626)+rxt(k,686)+rxt(k,694)+rxt(k,703))*y(k,107) &
                      + rxt(k,270)*y(k,259)
         mat(k,1676) = (rxt(k,630)+rxt(k,712)+rxt(k,716)+rxt(k,720))*y(k,99) + ( &
                      + rxt(k,629)+rxt(k,711)+rxt(k,715)+rxt(k,719))*y(k,103) + ( &
                      + rxt(k,631)+rxt(k,713)+rxt(k,717)+rxt(k,721))*y(k,107) &
                      + rxt(k,308)*y(k,259)
         mat(k,1394) = .500_r8*rxt(k,472)*y(k,259)
         mat(k,81) = rxt(k,633)*y(k,259)
         mat(k,626) = rxt(k,453)*y(k,259)
         mat(k,450) = rxt(k,457)*y(k,259)
         mat(k,2428) = rxt(k,374)*y(k,70) + rxt(k,204)*y(k,94) + rxt(k,211)*y(k,259)
         mat(k,2208) = rxt(k,355)*y(k,31) + rxt(k,420)*y(k,34) + rxt(k,445)*y(k,36) &
                      + rxt(k,446)*y(k,37) + rxt(k,357)*y(k,47) + rxt(k,360)*y(k,49) &
                      + rxt(k,391)*y(k,53) + rxt(k,362)*y(k,54) + rxt(k,363)*y(k,55) &
                      + rxt(k,422)*y(k,56) + rxt(k,365)*y(k,57) + rxt(k,459)*y(k,59) &
                      + rxt(k,448)*y(k,60) + rxt(k,428)*y(k,61) + rxt(k,429)*y(k,62) &
                      + rxt(k,367)*y(k,63) + rxt(k,397)*y(k,65) + rxt(k,398)*y(k,66) &
                      + rxt(k,379)*y(k,71) + rxt(k,209)*y(k,95) + rxt(k,210)*y(k,97) &
                      + rxt(k,300)*y(k,99) + rxt(k,382)*y(k,102) + rxt(k,267)*y(k,103) &
                      + rxt(k,400)*y(k,105) + rxt(k,335)*y(k,107) + rxt(k,241) &
                      *y(k,108) + rxt(k,217)*y(k,109) + rxt(k,270)*y(k,111) &
                      + rxt(k,308)*y(k,112) + .500_r8*rxt(k,472)*y(k,136) + rxt(k,633) &
                      *y(k,151) + rxt(k,453)*y(k,179) + rxt(k,457)*y(k,180) &
                      + rxt(k,211)*y(k,237) + 2.000_r8*rxt(k,214)*y(k,259)
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
         mat(k, 56) = mat(k, 56) + lmat(k, 56)
         mat(k, 57) = lmat(k, 57)
         mat(k, 63) = mat(k, 63) + lmat(k, 63)
         mat(k, 64) = lmat(k, 64)
         mat(k, 65) = lmat(k, 65)
         mat(k, 66) = lmat(k, 66)
         mat(k, 67) = lmat(k, 67)
         mat(k, 68) = lmat(k, 68)
         mat(k, 69) = lmat(k, 69)
         mat(k, 70) = lmat(k, 70)
         mat(k, 71) = lmat(k, 71)
         mat(k, 72) = lmat(k, 72)
         mat(k, 73) = lmat(k, 73)
         mat(k, 74) = lmat(k, 74)
         mat(k, 75) = lmat(k, 75)
         mat(k, 76) = mat(k, 76) + lmat(k, 76)
         mat(k, 79) = mat(k, 79) + lmat(k, 79)
         mat(k, 82) = mat(k, 82) + lmat(k, 82)
         mat(k, 84) = mat(k, 84) + lmat(k, 84)
         mat(k, 85) = mat(k, 85) + lmat(k, 85)
         mat(k, 87) = mat(k, 87) + lmat(k, 87)
         mat(k, 88) = mat(k, 88) + lmat(k, 88)
         mat(k, 89) = mat(k, 89) + lmat(k, 89)
         mat(k, 91) = mat(k, 91) + lmat(k, 91)
         mat(k, 92) = mat(k, 92) + lmat(k, 92)
         mat(k, 93) = mat(k, 93) + lmat(k, 93)
         mat(k, 94) = mat(k, 94) + lmat(k, 94)
         mat(k, 96) = mat(k, 96) + lmat(k, 96)
         mat(k, 97) = lmat(k, 97)
         mat(k, 98) = lmat(k, 98)
         mat(k, 99) = lmat(k, 99)
         mat(k, 100) = lmat(k, 100)
         mat(k, 101) = mat(k, 101) + lmat(k, 101)
         mat(k, 103) = mat(k, 103) + lmat(k, 103)
         mat(k, 104) = mat(k, 104) + lmat(k, 104)
         mat(k, 105) = mat(k, 105) + lmat(k, 105)
         mat(k, 109) = mat(k, 109) + lmat(k, 109)
         mat(k, 110) = mat(k, 110) + lmat(k, 110)
         mat(k, 112) = mat(k, 112) + lmat(k, 112)
         mat(k, 113) = mat(k, 113) + lmat(k, 113)
         mat(k, 114) = mat(k, 114) + lmat(k, 114)
         mat(k, 116) = mat(k, 116) + lmat(k, 116)
         mat(k, 117) = mat(k, 117) + lmat(k, 117)
         mat(k, 118) = mat(k, 118) + lmat(k, 118)
         mat(k, 120) = mat(k, 120) + lmat(k, 120)
         mat(k, 121) = mat(k, 121) + lmat(k, 121)
         mat(k, 123) = mat(k, 123) + lmat(k, 123)
         mat(k, 125) = lmat(k, 125)
         mat(k, 126) = lmat(k, 126)
         mat(k, 127) = lmat(k, 127)
         mat(k, 128) = lmat(k, 128)
         mat(k, 129) = lmat(k, 129)
         mat(k, 130) = lmat(k, 130)
         mat(k, 131) = lmat(k, 131)
         mat(k, 132) = lmat(k, 132)
         mat(k, 133) = lmat(k, 133)
         mat(k, 134) = mat(k, 134) + lmat(k, 134)
         mat(k, 137) = lmat(k, 137)
         mat(k, 138) = lmat(k, 138)
         mat(k, 139) = lmat(k, 139)
         mat(k, 140) = mat(k, 140) + lmat(k, 140)
         mat(k, 141) = mat(k, 141) + lmat(k, 141)
         mat(k, 143) = mat(k, 143) + lmat(k, 143)
         mat(k, 144) = mat(k, 144) + lmat(k, 144)
         mat(k, 145) = mat(k, 145) + lmat(k, 145)
         mat(k, 146) = mat(k, 146) + lmat(k, 146)
         mat(k, 147) = mat(k, 147) + lmat(k, 147)
         mat(k, 149) = mat(k, 149) + lmat(k, 149)
         mat(k, 150) = mat(k, 150) + lmat(k, 150)
         mat(k, 151) = mat(k, 151) + lmat(k, 151)
         mat(k, 152) = mat(k, 152) + lmat(k, 152)
         mat(k, 154) = mat(k, 154) + lmat(k, 154)
         mat(k, 155) = mat(k, 155) + lmat(k, 155)
         mat(k, 156) = mat(k, 156) + lmat(k, 156)
         mat(k, 157) = mat(k, 157) + lmat(k, 157)
         mat(k, 159) = mat(k, 159) + lmat(k, 159)
         mat(k, 165) = mat(k, 165) + lmat(k, 165)
         mat(k, 171) = lmat(k, 171)
         mat(k, 172) = lmat(k, 172)
         mat(k, 173) = lmat(k, 173)
         mat(k, 174) = lmat(k, 174)
         mat(k, 175) = mat(k, 175) + lmat(k, 175)
         mat(k, 180) = mat(k, 180) + lmat(k, 180)
         mat(k, 181) = mat(k, 181) + lmat(k, 181)
         mat(k, 182) = mat(k, 182) + lmat(k, 182)
         mat(k, 183) = mat(k, 183) + lmat(k, 183)
         mat(k, 184) = lmat(k, 184)
         mat(k, 185) = lmat(k, 185)
         mat(k, 186) = lmat(k, 186)
         mat(k, 187) = mat(k, 187) + lmat(k, 187)
         mat(k, 191) = mat(k, 191) + lmat(k, 191)
         mat(k, 193) = lmat(k, 193)
         mat(k, 194) = mat(k, 194) + lmat(k, 194)
         mat(k, 195) = mat(k, 195) + lmat(k, 195)
         mat(k, 198) = mat(k, 198) + lmat(k, 198)
         mat(k, 204) = mat(k, 204) + lmat(k, 204)
         mat(k, 211) = mat(k, 211) + lmat(k, 211)
         mat(k, 216) = lmat(k, 216)
         mat(k, 217) = lmat(k, 217)
         mat(k, 218) = lmat(k, 218)
         mat(k, 219) = lmat(k, 219)
         mat(k, 220) = mat(k, 220) + lmat(k, 220)
         mat(k, 222) = mat(k, 222) + lmat(k, 222)
         mat(k, 229) = mat(k, 229) + lmat(k, 229)
         mat(k, 237) = mat(k, 237) + lmat(k, 237)
         mat(k, 242) = mat(k, 242) + lmat(k, 242)
         mat(k, 243) = mat(k, 243) + lmat(k, 243)
         mat(k, 246) = mat(k, 246) + lmat(k, 246)
         mat(k, 247) = mat(k, 247) + lmat(k, 247)
         mat(k, 248) = mat(k, 248) + lmat(k, 248)
         mat(k, 250) = mat(k, 250) + lmat(k, 250)
         mat(k, 251) = mat(k, 251) + lmat(k, 251)
         mat(k, 252) = mat(k, 252) + lmat(k, 252)
         mat(k, 255) = mat(k, 255) + lmat(k, 255)
         mat(k, 256) = mat(k, 256) + lmat(k, 256)
         mat(k, 257) = mat(k, 257) + lmat(k, 257)
         mat(k, 259) = mat(k, 259) + lmat(k, 259)
         mat(k, 260) = lmat(k, 260)
         mat(k, 262) = mat(k, 262) + lmat(k, 262)
         mat(k, 263) = lmat(k, 263)
         mat(k, 264) = mat(k, 264) + lmat(k, 264)
         mat(k, 267) = mat(k, 267) + lmat(k, 267)
         mat(k, 270) = lmat(k, 270)
         mat(k, 271) = lmat(k, 271)
         mat(k, 272) = lmat(k, 272)
         mat(k, 273) = mat(k, 273) + lmat(k, 273)
         mat(k, 276) = mat(k, 276) + lmat(k, 276)
         mat(k, 278) = mat(k, 278) + lmat(k, 278)
         mat(k, 280) = mat(k, 280) + lmat(k, 280)
         mat(k, 283) = mat(k, 283) + lmat(k, 283)
         mat(k, 284) = lmat(k, 284)
         mat(k, 285) = lmat(k, 285)
         mat(k, 287) = mat(k, 287) + lmat(k, 287)
         mat(k, 291) = mat(k, 291) + lmat(k, 291)
         mat(k, 292) = lmat(k, 292)
         mat(k, 294) = mat(k, 294) + lmat(k, 294)
         mat(k, 295) = lmat(k, 295)
         mat(k, 296) = lmat(k, 296)
         mat(k, 297) = lmat(k, 297)
         mat(k, 298) = mat(k, 298) + lmat(k, 298)
         mat(k, 299) = lmat(k, 299)
         mat(k, 300) = lmat(k, 300)
         mat(k, 302) = mat(k, 302) + lmat(k, 302)
         mat(k, 303) = lmat(k, 303)
         mat(k, 304) = lmat(k, 304)
         mat(k, 305) = lmat(k, 305)
         mat(k, 306) = lmat(k, 306)
         mat(k, 307) = mat(k, 307) + lmat(k, 307)
         mat(k, 308) = mat(k, 308) + lmat(k, 308)
         mat(k, 312) = mat(k, 312) + lmat(k, 312)
         mat(k, 314) = mat(k, 314) + lmat(k, 314)
         mat(k, 320) = mat(k, 320) + lmat(k, 320)
         mat(k, 326) = mat(k, 326) + lmat(k, 326)
         mat(k, 330) = mat(k, 330) + lmat(k, 330)
         mat(k, 332) = mat(k, 332) + lmat(k, 332)
         mat(k, 337) = mat(k, 337) + lmat(k, 337)
         mat(k, 339) = lmat(k, 339)
         mat(k, 340) = lmat(k, 340)
         mat(k, 341) = mat(k, 341) + lmat(k, 341)
         mat(k, 342) = lmat(k, 342)
         mat(k, 343) = lmat(k, 343)
         mat(k, 344) = lmat(k, 344)
         mat(k, 345) = lmat(k, 345)
         mat(k, 346) = lmat(k, 346)
         mat(k, 347) = lmat(k, 347)
         mat(k, 348) = lmat(k, 348)
         mat(k, 349) = lmat(k, 349)
         mat(k, 350) = lmat(k, 350)
         mat(k, 351) = lmat(k, 351)
         mat(k, 352) = lmat(k, 352)
         mat(k, 353) = lmat(k, 353)
         mat(k, 354) = mat(k, 354) + lmat(k, 354)
         mat(k, 356) = lmat(k, 356)
         mat(k, 357) = mat(k, 357) + lmat(k, 357)
         mat(k, 358) = lmat(k, 358)
         mat(k, 359) = lmat(k, 359)
         mat(k, 360) = mat(k, 360) + lmat(k, 360)
         mat(k, 363) = mat(k, 363) + lmat(k, 363)
         mat(k, 364) = lmat(k, 364)
         mat(k, 365) = mat(k, 365) + lmat(k, 365)
         mat(k, 367) = mat(k, 367) + lmat(k, 367)
         mat(k, 368) = mat(k, 368) + lmat(k, 368)
         mat(k, 369) = lmat(k, 369)
         mat(k, 370) = lmat(k, 370)
         mat(k, 371) = lmat(k, 371)
         mat(k, 372) = lmat(k, 372)
         mat(k, 374) = mat(k, 374) + lmat(k, 374)
         mat(k, 375) = lmat(k, 375)
         mat(k, 379) = mat(k, 379) + lmat(k, 379)
         mat(k, 381) = mat(k, 381) + lmat(k, 381)
         mat(k, 384) = mat(k, 384) + lmat(k, 384)
         mat(k, 385) = lmat(k, 385)
         mat(k, 388) = mat(k, 388) + lmat(k, 388)
         mat(k, 392) = lmat(k, 392)
         mat(k, 393) = mat(k, 393) + lmat(k, 393)
         mat(k, 395) = lmat(k, 395)
         mat(k, 396) = lmat(k, 396)
         mat(k, 397) = mat(k, 397) + lmat(k, 397)
         mat(k, 398) = lmat(k, 398)
         mat(k, 399) = lmat(k, 399)
         mat(k, 400) = mat(k, 400) + lmat(k, 400)
         mat(k, 408) = mat(k, 408) + lmat(k, 408)
         mat(k, 409) = lmat(k, 409)
         mat(k, 411) = mat(k, 411) + lmat(k, 411)
         mat(k, 416) = lmat(k, 416)
         mat(k, 417) = lmat(k, 417)
         mat(k, 418) = lmat(k, 418)
         mat(k, 419) = mat(k, 419) + lmat(k, 419)
         mat(k, 421) = lmat(k, 421)
         mat(k, 424) = mat(k, 424) + lmat(k, 424)
         mat(k, 427) = mat(k, 427) + lmat(k, 427)
         mat(k, 432) = lmat(k, 432)
         mat(k, 433) = mat(k, 433) + lmat(k, 433)
         mat(k, 434) = lmat(k, 434)
         mat(k, 436) = lmat(k, 436)
         mat(k, 437) = mat(k, 437) + lmat(k, 437)
         mat(k, 438) = lmat(k, 438)
         mat(k, 439) = lmat(k, 439)
         mat(k, 440) = lmat(k, 440)
         mat(k, 441) = lmat(k, 441)
         mat(k, 442) = lmat(k, 442)
         mat(k, 443) = lmat(k, 443)
         mat(k, 444) = lmat(k, 444)
         mat(k, 445) = mat(k, 445) + lmat(k, 445)
         mat(k, 447) = lmat(k, 447)
         mat(k, 448) = mat(k, 448) + lmat(k, 448)
         mat(k, 449) = lmat(k, 449)
         mat(k, 451) = mat(k, 451) + lmat(k, 451)
         mat(k, 453) = lmat(k, 453)
         mat(k, 454) = mat(k, 454) + lmat(k, 454)
         mat(k, 455) = lmat(k, 455)
         mat(k, 457) = mat(k, 457) + lmat(k, 457)
         mat(k, 462) = mat(k, 462) + lmat(k, 462)
         mat(k, 463) = mat(k, 463) + lmat(k, 463)
         mat(k, 464) = lmat(k, 464)
         mat(k, 466) = lmat(k, 466)
         mat(k, 467) = mat(k, 467) + lmat(k, 467)
         mat(k, 468) = lmat(k, 468)
         mat(k, 469) = mat(k, 469) + lmat(k, 469)
         mat(k, 471) = lmat(k, 471)
         mat(k, 472) = mat(k, 472) + lmat(k, 472)
         mat(k, 473) = mat(k, 473) + lmat(k, 473)
         mat(k, 475) = mat(k, 475) + lmat(k, 475)
         mat(k, 477) = lmat(k, 477)
         mat(k, 478) = lmat(k, 478)
         mat(k, 479) = mat(k, 479) + lmat(k, 479)
         mat(k, 480) = lmat(k, 480)
         mat(k, 483) = mat(k, 483) + lmat(k, 483)
         mat(k, 489) = mat(k, 489) + lmat(k, 489)
         mat(k, 492) = mat(k, 492) + lmat(k, 492)
         mat(k, 493) = lmat(k, 493)
         mat(k, 496) = mat(k, 496) + lmat(k, 496)
         mat(k, 502) = mat(k, 502) + lmat(k, 502)
         mat(k, 503) = lmat(k, 503)
         mat(k, 505) = lmat(k, 505)
         mat(k, 507) = mat(k, 507) + lmat(k, 507)
         mat(k, 508) = mat(k, 508) + lmat(k, 508)
         mat(k, 511) = lmat(k, 511)
         mat(k, 512) = mat(k, 512) + lmat(k, 512)
         mat(k, 513) = lmat(k, 513)
         mat(k, 514) = lmat(k, 514)
         mat(k, 515) = lmat(k, 515)
         mat(k, 516) = mat(k, 516) + lmat(k, 516)
         mat(k, 519) = mat(k, 519) + lmat(k, 519)
         mat(k, 520) = lmat(k, 520)
         mat(k, 521) = mat(k, 521) + lmat(k, 521)
         mat(k, 524) = mat(k, 524) + lmat(k, 524)
         mat(k, 525) = mat(k, 525) + lmat(k, 525)
         mat(k, 529) = lmat(k, 529)
         mat(k, 530) = lmat(k, 530)
         mat(k, 531) = lmat(k, 531)
         mat(k, 532) = mat(k, 532) + lmat(k, 532)
         mat(k, 534) = mat(k, 534) + lmat(k, 534)
         mat(k, 535) = mat(k, 535) + lmat(k, 535)
         mat(k, 536) = lmat(k, 536)
         mat(k, 537) = lmat(k, 537)
         mat(k, 539) = mat(k, 539) + lmat(k, 539)
         mat(k, 544) = mat(k, 544) + lmat(k, 544)
         mat(k, 546) = mat(k, 546) + lmat(k, 546)
         mat(k, 547) = lmat(k, 547)
         mat(k, 548) = lmat(k, 548)
         mat(k, 550) = mat(k, 550) + lmat(k, 550)
         mat(k, 551) = lmat(k, 551)
         mat(k, 552) = lmat(k, 552)
         mat(k, 555) = mat(k, 555) + lmat(k, 555)
         mat(k, 562) = mat(k, 562) + lmat(k, 562)
         mat(k, 570) = mat(k, 570) + lmat(k, 570)
         mat(k, 577) = lmat(k, 577)
         mat(k, 578) = mat(k, 578) + lmat(k, 578)
         mat(k, 580) = mat(k, 580) + lmat(k, 580)
         mat(k, 582) = lmat(k, 582)
         mat(k, 583) = mat(k, 583) + lmat(k, 583)
         mat(k, 585) = lmat(k, 585)
         mat(k, 586) = lmat(k, 586)
         mat(k, 588) = lmat(k, 588)
         mat(k, 589) = lmat(k, 589)
         mat(k, 590) = lmat(k, 590)
         mat(k, 592) = mat(k, 592) + lmat(k, 592)
         mat(k, 598) = mat(k, 598) + lmat(k, 598)
         mat(k, 603) = mat(k, 603) + lmat(k, 603)
         mat(k, 604) = lmat(k, 604)
         mat(k, 605) = lmat(k, 605)
         mat(k, 606) = lmat(k, 606)
         mat(k, 607) = lmat(k, 607)
         mat(k, 610) = mat(k, 610) + lmat(k, 610)
         mat(k, 611) = mat(k, 611) + lmat(k, 611)
         mat(k, 618) = lmat(k, 618)
         mat(k, 619) = mat(k, 619) + lmat(k, 619)
         mat(k, 621) = lmat(k, 621)
         mat(k, 623) = mat(k, 623) + lmat(k, 623)
         mat(k, 624) = lmat(k, 624)
         mat(k, 625) = lmat(k, 625)
         mat(k, 627) = mat(k, 627) + lmat(k, 627)
         mat(k, 635) = mat(k, 635) + lmat(k, 635)
         mat(k, 643) = mat(k, 643) + lmat(k, 643)
         mat(k, 651) = lmat(k, 651)
         mat(k, 652) = lmat(k, 652)
         mat(k, 653) = lmat(k, 653)
         mat(k, 654) = lmat(k, 654)
         mat(k, 655) = lmat(k, 655)
         mat(k, 656) = mat(k, 656) + lmat(k, 656)
         mat(k, 658) = mat(k, 658) + lmat(k, 658)
         mat(k, 659) = lmat(k, 659)
         mat(k, 660) = mat(k, 660) + lmat(k, 660)
         mat(k, 663) = mat(k, 663) + lmat(k, 663)
         mat(k, 665) = lmat(k, 665)
         mat(k, 667) = lmat(k, 667)
         mat(k, 672) = mat(k, 672) + lmat(k, 672)
         mat(k, 674) = mat(k, 674) + lmat(k, 674)
         mat(k, 679) = mat(k, 679) + lmat(k, 679)
         mat(k, 680) = lmat(k, 680)
         mat(k, 685) = mat(k, 685) + lmat(k, 685)
         mat(k, 690) = mat(k, 690) + lmat(k, 690)
         mat(k, 691) = lmat(k, 691)
         mat(k, 693) = lmat(k, 693)
         mat(k, 696) = mat(k, 696) + lmat(k, 696)
         mat(k, 697) = mat(k, 697) + lmat(k, 697)
         mat(k, 698) = mat(k, 698) + lmat(k, 698)
         mat(k, 700) = lmat(k, 700)
         mat(k, 701) = lmat(k, 701)
         mat(k, 704) = mat(k, 704) + lmat(k, 704)
         mat(k, 710) = lmat(k, 710)
         mat(k, 711) = mat(k, 711) + lmat(k, 711)
         mat(k, 714) = mat(k, 714) + lmat(k, 714)
         mat(k, 715) = mat(k, 715) + lmat(k, 715)
         mat(k, 717) = mat(k, 717) + lmat(k, 717)
         mat(k, 719) = lmat(k, 719)
         mat(k, 720) = mat(k, 720) + lmat(k, 720)
         mat(k, 721) = mat(k, 721) + lmat(k, 721)
         mat(k, 722) = lmat(k, 722)
         mat(k, 723) = lmat(k, 723)
         mat(k, 724) = lmat(k, 724)
         mat(k, 725) = lmat(k, 725)
         mat(k, 727) = lmat(k, 727)
         mat(k, 728) = mat(k, 728) + lmat(k, 728)
         mat(k, 729) = lmat(k, 729)
         mat(k, 730) = lmat(k, 730)
         mat(k, 731) = lmat(k, 731)
         mat(k, 732) = lmat(k, 732)
         mat(k, 733) = lmat(k, 733)
         mat(k, 734) = mat(k, 734) + lmat(k, 734)
         mat(k, 739) = lmat(k, 739)
         mat(k, 741) = lmat(k, 741)
         mat(k, 742) = mat(k, 742) + lmat(k, 742)
         mat(k, 743) = lmat(k, 743)
         mat(k, 744) = lmat(k, 744)
         mat(k, 745) = mat(k, 745) + lmat(k, 745)
         mat(k, 749) = lmat(k, 749)
         mat(k, 750) = lmat(k, 750)
         mat(k, 752) = mat(k, 752) + lmat(k, 752)
         mat(k, 753) = lmat(k, 753)
         mat(k, 754) = lmat(k, 754)
         mat(k, 756) = mat(k, 756) + lmat(k, 756)
         mat(k, 764) = mat(k, 764) + lmat(k, 764)
         mat(k, 774) = mat(k, 774) + lmat(k, 774)
         mat(k, 780) = mat(k, 780) + lmat(k, 780)
         mat(k, 782) = lmat(k, 782)
         mat(k, 783) = lmat(k, 783)
         mat(k, 784) = mat(k, 784) + lmat(k, 784)
         mat(k, 785) = lmat(k, 785)
         mat(k, 786) = lmat(k, 786)
         mat(k, 787) = lmat(k, 787)
         mat(k, 788) = lmat(k, 788)
         mat(k, 789) = lmat(k, 789)
         mat(k, 790) = lmat(k, 790)
         mat(k, 791) = mat(k, 791) + lmat(k, 791)
         mat(k, 796) = lmat(k, 796)
         mat(k, 798) = lmat(k, 798)
         mat(k, 800) = mat(k, 800) + lmat(k, 800)
         mat(k, 801) = lmat(k, 801)
         mat(k, 802) = mat(k, 802) + lmat(k, 802)
         mat(k, 813) = mat(k, 813) + lmat(k, 813)
         mat(k, 823) = mat(k, 823) + lmat(k, 823)
         mat(k, 836) = mat(k, 836) + lmat(k, 836)
         mat(k, 847) = mat(k, 847) + lmat(k, 847)
         mat(k, 857) = mat(k, 857) + lmat(k, 857)
         mat(k, 865) = mat(k, 865) + lmat(k, 865)
         mat(k, 866) = lmat(k, 866)
         mat(k, 868) = lmat(k, 868)
         mat(k, 873) = mat(k, 873) + lmat(k, 873)
         mat(k, 878) = mat(k, 878) + lmat(k, 878)
         mat(k, 889) = mat(k, 889) + lmat(k, 889)
         mat(k, 901) = mat(k, 901) + lmat(k, 901)
         mat(k, 908) = mat(k, 908) + lmat(k, 908)
         mat(k, 909) = mat(k, 909) + lmat(k, 909)
         mat(k, 914) = mat(k, 914) + lmat(k, 914)
         mat(k, 921) = mat(k, 921) + lmat(k, 921)
         mat(k, 929) = mat(k, 929) + lmat(k, 929)
         mat(k, 931) = lmat(k, 931)
         mat(k, 932) = mat(k, 932) + lmat(k, 932)
         mat(k, 934) = lmat(k, 934)
         mat(k, 940) = mat(k, 940) + lmat(k, 940)
         mat(k, 956) = mat(k, 956) + lmat(k, 956)
         mat(k, 957) = mat(k, 957) + lmat(k, 957)
         mat(k, 958) = mat(k, 958) + lmat(k, 958)
         mat(k, 959) = lmat(k, 959)
         mat(k, 960) = lmat(k, 960)
         mat(k, 963) = mat(k, 963) + lmat(k, 963)
         mat(k, 964) = mat(k, 964) + lmat(k, 964)
         mat(k, 965) = lmat(k, 965)
         mat(k, 967) = mat(k, 967) + lmat(k, 967)
         mat(k, 968) = lmat(k, 968)
         mat(k, 969) = lmat(k, 969)
         mat(k, 970) = mat(k, 970) + lmat(k, 970)
         mat(k, 972) = lmat(k, 972)
         mat(k, 973) = lmat(k, 973)
         mat(k, 974) = mat(k, 974) + lmat(k, 974)
         mat(k, 977) = lmat(k, 977)
         mat(k, 978) = lmat(k, 978)
         mat(k, 979) = mat(k, 979) + lmat(k, 979)
         mat(k, 981) = mat(k, 981) + lmat(k, 981)
         mat(k, 993) = mat(k, 993) + lmat(k, 993)
         mat(k,1002) = mat(k,1002) + lmat(k,1002)
         mat(k,1011) = mat(k,1011) + lmat(k,1011)
         mat(k,1024) = mat(k,1024) + lmat(k,1024)
         mat(k,1051) = mat(k,1051) + lmat(k,1051)
         mat(k,1075) = mat(k,1075) + lmat(k,1075)
         mat(k,1086) = mat(k,1086) + lmat(k,1086)
         mat(k,1088) = lmat(k,1088)
         mat(k,1089) = lmat(k,1089)
         mat(k,1092) = lmat(k,1092)
         mat(k,1093) = mat(k,1093) + lmat(k,1093)
         mat(k,1094) = mat(k,1094) + lmat(k,1094)
         mat(k,1097) = mat(k,1097) + lmat(k,1097)
         mat(k,1099) = lmat(k,1099)
         mat(k,1103) = lmat(k,1103)
         mat(k,1104) = mat(k,1104) + lmat(k,1104)
         mat(k,1114) = mat(k,1114) + lmat(k,1114)
         mat(k,1115) = mat(k,1115) + lmat(k,1115)
         mat(k,1116) = mat(k,1116) + lmat(k,1116)
         mat(k,1118) = lmat(k,1118)
         mat(k,1120) = mat(k,1120) + lmat(k,1120)
         mat(k,1121) = mat(k,1121) + lmat(k,1121)
         mat(k,1123) = mat(k,1123) + lmat(k,1123)
         mat(k,1124) = mat(k,1124) + lmat(k,1124)
         mat(k,1125) = mat(k,1125) + lmat(k,1125)
         mat(k,1137) = mat(k,1137) + lmat(k,1137)
         mat(k,1139) = lmat(k,1139)
         mat(k,1140) = lmat(k,1140)
         mat(k,1143) = lmat(k,1143)
         mat(k,1146) = mat(k,1146) + lmat(k,1146)
         mat(k,1147) = lmat(k,1147)
         mat(k,1150) = lmat(k,1150)
         mat(k,1152) = lmat(k,1152)
         mat(k,1156) = mat(k,1156) + lmat(k,1156)
         mat(k,1162) = lmat(k,1162)
         mat(k,1164) = mat(k,1164) + lmat(k,1164)
         mat(k,1167) = lmat(k,1167)
         mat(k,1169) = mat(k,1169) + lmat(k,1169)
         mat(k,1185) = mat(k,1185) + lmat(k,1185)
         mat(k,1203) = mat(k,1203) + lmat(k,1203)
         mat(k,1204) = lmat(k,1204)
         mat(k,1206) = lmat(k,1206)
         mat(k,1208) = lmat(k,1208)
         mat(k,1216) = mat(k,1216) + lmat(k,1216)
         mat(k,1228) = mat(k,1228) + lmat(k,1228)
         mat(k,1229) = mat(k,1229) + lmat(k,1229)
         mat(k,1230) = mat(k,1230) + lmat(k,1230)
         mat(k,1231) = mat(k,1231) + lmat(k,1231)
         mat(k,1232) = mat(k,1232) + lmat(k,1232)
         mat(k,1233) = mat(k,1233) + lmat(k,1233)
         mat(k,1236) = mat(k,1236) + lmat(k,1236)
         mat(k,1237) = mat(k,1237) + lmat(k,1237)
         mat(k,1240) = lmat(k,1240)
         mat(k,1241) = lmat(k,1241)
         mat(k,1242) = mat(k,1242) + lmat(k,1242)
         mat(k,1243) = lmat(k,1243)
         mat(k,1244) = lmat(k,1244)
         mat(k,1245) = lmat(k,1245)
         mat(k,1247) = lmat(k,1247)
         mat(k,1249) = lmat(k,1249)
         mat(k,1251) = mat(k,1251) + lmat(k,1251)
         mat(k,1252) = lmat(k,1252)
         mat(k,1253) = lmat(k,1253)
         mat(k,1256) = mat(k,1256) + lmat(k,1256)
         mat(k,1258) = lmat(k,1258)
         mat(k,1260) = mat(k,1260) + lmat(k,1260)
         mat(k,1261) = lmat(k,1261)
         mat(k,1263) = mat(k,1263) + lmat(k,1263)
         mat(k,1264) = lmat(k,1264)
         mat(k,1267) = mat(k,1267) + lmat(k,1267)
         mat(k,1268) = mat(k,1268) + lmat(k,1268)
         mat(k,1269) = mat(k,1269) + lmat(k,1269)
         mat(k,1280) = mat(k,1280) + lmat(k,1280)
         mat(k,1301) = mat(k,1301) + lmat(k,1301)
         mat(k,1317) = mat(k,1317) + lmat(k,1317)
         mat(k,1329) = mat(k,1329) + lmat(k,1329)
         mat(k,1334) = lmat(k,1334)
         mat(k,1336) = mat(k,1336) + lmat(k,1336)
         mat(k,1348) = mat(k,1348) + lmat(k,1348)
         mat(k,1368) = mat(k,1368) + lmat(k,1368)
         mat(k,1383) = mat(k,1383) + lmat(k,1383)
         mat(k,1384) = mat(k,1384) + lmat(k,1384)
         mat(k,1387) = mat(k,1387) + lmat(k,1387)
         mat(k,1388) = mat(k,1388) + lmat(k,1388)
         mat(k,1392) = mat(k,1392) + lmat(k,1392)
         mat(k,1393) = mat(k,1393) + lmat(k,1393)
         mat(k,1395) = mat(k,1395) + lmat(k,1395)
         mat(k,1396) = mat(k,1396) + lmat(k,1396)
         mat(k,1397) = mat(k,1397) + lmat(k,1397)
         mat(k,1400) = lmat(k,1400)
         mat(k,1415) = mat(k,1415) + lmat(k,1415)
         mat(k,1431) = lmat(k,1431)
         mat(k,1448) = mat(k,1448) + lmat(k,1448)
         mat(k,1458) = mat(k,1458) + lmat(k,1458)
         mat(k,1472) = mat(k,1472) + lmat(k,1472)
         mat(k,1486) = lmat(k,1486)
         mat(k,1489) = mat(k,1489) + lmat(k,1489)
         mat(k,1492) = mat(k,1492) + lmat(k,1492)
         mat(k,1494) = mat(k,1494) + lmat(k,1494)
         mat(k,1497) = lmat(k,1497)
         mat(k,1513) = mat(k,1513) + lmat(k,1513)
         mat(k,1544) = mat(k,1544) + lmat(k,1544)
         mat(k,1565) = mat(k,1565) + lmat(k,1565)
         mat(k,1566) = mat(k,1566) + lmat(k,1566)
         mat(k,1574) = lmat(k,1574)
         mat(k,1578) = mat(k,1578) + lmat(k,1578)
         mat(k,1583) = mat(k,1583) + lmat(k,1583)
         mat(k,1587) = mat(k,1587) + lmat(k,1587)
         mat(k,1592) = mat(k,1592) + lmat(k,1592)
         mat(k,1596) = mat(k,1596) + lmat(k,1596)
         mat(k,1605) = mat(k,1605) + lmat(k,1605)
         mat(k,1618) = lmat(k,1618)
         mat(k,1620) = mat(k,1620) + lmat(k,1620)
         mat(k,1631) = mat(k,1631) + lmat(k,1631)
         mat(k,1636) = lmat(k,1636)
         mat(k,1637) = lmat(k,1637)
         mat(k,1638) = mat(k,1638) + lmat(k,1638)
         mat(k,1639) = mat(k,1639) + lmat(k,1639)
         mat(k,1640) = mat(k,1640) + lmat(k,1640)
         mat(k,1644) = mat(k,1644) + lmat(k,1644)
         mat(k,1646) = mat(k,1646) + lmat(k,1646)
         mat(k,1649) = mat(k,1649) + lmat(k,1649)
         mat(k,1652) = lmat(k,1652)
         mat(k,1654) = mat(k,1654) + lmat(k,1654)
         mat(k,1655) = mat(k,1655) + lmat(k,1655)
         mat(k,1656) = mat(k,1656) + lmat(k,1656)
         mat(k,1660) = mat(k,1660) + lmat(k,1660)
         mat(k,1667) = mat(k,1667) + lmat(k,1667)
         mat(k,1668) = lmat(k,1668)
         mat(k,1678) = mat(k,1678) + lmat(k,1678)
         mat(k,1679) = mat(k,1679) + lmat(k,1679)
         mat(k,1684) = mat(k,1684) + lmat(k,1684)
         mat(k,1690) = mat(k,1690) + lmat(k,1690)
         mat(k,1693) = lmat(k,1693)
         mat(k,1737) = mat(k,1737) + lmat(k,1737)
         mat(k,1762) = mat(k,1762) + lmat(k,1762)
         mat(k,1764) = mat(k,1764) + lmat(k,1764)
         mat(k,1765) = lmat(k,1765)
         mat(k,1772) = mat(k,1772) + lmat(k,1772)
         mat(k,1773) = mat(k,1773) + lmat(k,1773)
         mat(k,1784) = mat(k,1784) + lmat(k,1784)
         mat(k,1787) = mat(k,1787) + lmat(k,1787)
         mat(k,1795) = mat(k,1795) + lmat(k,1795)
         mat(k,1800) = mat(k,1800) + lmat(k,1800)
         mat(k,1802) = mat(k,1802) + lmat(k,1802)
         mat(k,1819) = mat(k,1819) + lmat(k,1819)
         mat(k,1821) = mat(k,1821) + lmat(k,1821)
         mat(k,1825) = mat(k,1825) + lmat(k,1825)
         mat(k,1864) = mat(k,1864) + lmat(k,1864)
         mat(k,1878) = mat(k,1878) + lmat(k,1878)
         mat(k,1889) = mat(k,1889) + lmat(k,1889)
         mat(k,1948) = mat(k,1948) + lmat(k,1948)
         mat(k,1950) = mat(k,1950) + lmat(k,1950)
         mat(k,1957) = mat(k,1957) + lmat(k,1957)
         mat(k,1958) = mat(k,1958) + lmat(k,1958)
         mat(k,1960) = mat(k,1960) + lmat(k,1960)
         mat(k,1969) = mat(k,1969) + lmat(k,1969)
         mat(k,2012) = mat(k,2012) + lmat(k,2012)
         mat(k,2015) = mat(k,2015) + lmat(k,2015)
         mat(k,2018) = mat(k,2018) + lmat(k,2018)
         mat(k,2029) = mat(k,2029) + lmat(k,2029)
         mat(k,2195) = mat(k,2195) + lmat(k,2195)
         mat(k,2226) = mat(k,2226) + lmat(k,2226)
         mat(k,2284) = mat(k,2284) + lmat(k,2284)
         mat(k,2315) = mat(k,2315) + lmat(k,2315)
         mat(k,2419) = mat(k,2419) + lmat(k,2419)
         mat(k,2428) = mat(k,2428) + lmat(k,2428)
         mat(k,2447) = mat(k,2447) + lmat(k,2447)
         mat(k,2449) = mat(k,2449) + lmat(k,2449)
         mat(k,2454) = mat(k,2454) + lmat(k,2454)
         mat(k,2460) = mat(k,2460) + lmat(k,2460)
         mat(k,2463) = lmat(k,2463)
         mat(k,2472) = mat(k,2472) + lmat(k,2472)
         mat(k,2481) = mat(k,2481) + lmat(k,2481)
         mat(k,2521) = mat(k,2521) + lmat(k,2521)
         mat(k,2523) = lmat(k,2523)
         mat(k,2531) = mat(k,2531) + lmat(k,2531)
         mat(k,2575) = mat(k,2575) + lmat(k,2575)
         mat(k,2578) = mat(k,2578) + lmat(k,2578)
         mat(k,2589) = lmat(k,2589)
         mat(k,2592) = lmat(k,2592)
         mat(k,2594) = mat(k,2594) + lmat(k,2594)
         mat(k,2602) = mat(k,2602) + lmat(k,2602)
         mat(k,2628) = mat(k,2628) + lmat(k,2628)
         mat(k,2636) = mat(k,2636) + lmat(k,2636)
         mat(k,2637) = mat(k,2637) + lmat(k,2637)
         mat(k,2649) = mat(k,2649) + lmat(k,2649)
         mat(k,2652) = lmat(k,2652)
         mat(k,2684) = mat(k,2684) + lmat(k,2684)
         mat(k,2706) = mat(k,2706) + lmat(k,2706)
         mat(k,2714) = mat(k,2714) + lmat(k,2714)
         mat(k,2715) = mat(k,2715) + lmat(k,2715)
         mat(k,2769) = mat(k,2769) + lmat(k,2769)
         mat(k,2774) = mat(k,2774) + lmat(k,2774)
         mat(k,2783) = mat(k,2783) + lmat(k,2783)
         mat(k,2784) = mat(k,2784) + lmat(k,2784)
         mat(k,2786) = mat(k,2786) + lmat(k,2786)
         mat(k,2788) = mat(k,2788) + lmat(k,2788)
         mat(k,2795) = lmat(k,2795)
         mat(k,2802) = mat(k,2802) + lmat(k,2802)
         mat(k,2803) = lmat(k,2803)
         mat(k,2806) = mat(k,2806) + lmat(k,2806)
         mat(k,2816) = lmat(k,2816)
         mat(k,2819) = mat(k,2819) + lmat(k,2819)
         mat(k, 233) = 0._r8
         mat(k, 234) = 0._r8
         mat(k, 309) = 0._r8
         mat(k, 362) = 0._r8
         mat(k, 376) = 0._r8
         mat(k, 484) = 0._r8
         mat(k, 486) = 0._r8
         mat(k, 499) = 0._r8
         mat(k, 556) = 0._r8
         mat(k, 559) = 0._r8
         mat(k, 574) = 0._r8
         mat(k, 706) = 0._r8
         mat(k, 707) = 0._r8
         mat(k, 712) = 0._r8
         mat(k, 713) = 0._r8
         mat(k, 716) = 0._r8
         mat(k, 735) = 0._r8
         mat(k, 737) = 0._r8
         mat(k, 738) = 0._r8
         mat(k, 740) = 0._r8
         mat(k, 746) = 0._r8
         mat(k, 747) = 0._r8
         mat(k, 751) = 0._r8
         mat(k, 771) = 0._r8
         mat(k, 772) = 0._r8
         mat(k, 792) = 0._r8
         mat(k, 794) = 0._r8
         mat(k, 795) = 0._r8
         mat(k, 797) = 0._r8
         mat(k, 799) = 0._r8
         mat(k, 812) = 0._r8
         mat(k, 814) = 0._r8
         mat(k, 815) = 0._r8
         mat(k, 817) = 0._r8
         mat(k, 820) = 0._r8
         mat(k, 835) = 0._r8
         mat(k, 837) = 0._r8
         mat(k, 838) = 0._r8
         mat(k, 840) = 0._r8
         mat(k, 842) = 0._r8
         mat(k, 844) = 0._r8
         mat(k, 858) = 0._r8
         mat(k, 859) = 0._r8
         mat(k, 861) = 0._r8
         mat(k, 880) = 0._r8
         mat(k, 883) = 0._r8
         mat(k, 887) = 0._r8
         mat(k, 895) = 0._r8
         mat(k, 899) = 0._r8
         mat(k, 904) = 0._r8
         mat(k, 905) = 0._r8
         mat(k, 906) = 0._r8
         mat(k, 918) = 0._r8
         mat(k, 923) = 0._r8
         mat(k, 924) = 0._r8
         mat(k, 925) = 0._r8
         mat(k, 927) = 0._r8
         mat(k, 961) = 0._r8
         mat(k, 975) = 0._r8
         mat(k, 976) = 0._r8
         mat(k, 998) = 0._r8
         mat(k,1025) = 0._r8
         mat(k,1027) = 0._r8
         mat(k,1034) = 0._r8
         mat(k,1042) = 0._r8
         mat(k,1052) = 0._r8
         mat(k,1054) = 0._r8
         mat(k,1061) = 0._r8
         mat(k,1069) = 0._r8
         mat(k,1073) = 0._r8
         mat(k,1074) = 0._r8
         mat(k,1078) = 0._r8
         mat(k,1079) = 0._r8
         mat(k,1080) = 0._r8
         mat(k,1082) = 0._r8
         mat(k,1100) = 0._r8
         mat(k,1105) = 0._r8
         mat(k,1106) = 0._r8
         mat(k,1108) = 0._r8
         mat(k,1151) = 0._r8
         mat(k,1154) = 0._r8
         mat(k,1157) = 0._r8
         mat(k,1158) = 0._r8
         mat(k,1159) = 0._r8
         mat(k,1160) = 0._r8
         mat(k,1161) = 0._r8
         mat(k,1165) = 0._r8
         mat(k,1166) = 0._r8
         mat(k,1187) = 0._r8
         mat(k,1189) = 0._r8
         mat(k,1190) = 0._r8
         mat(k,1194) = 0._r8
         mat(k,1195) = 0._r8
         mat(k,1200) = 0._r8
         mat(k,1202) = 0._r8
         mat(k,1217) = 0._r8
         mat(k,1218) = 0._r8
         mat(k,1219) = 0._r8
         mat(k,1222) = 0._r8
         mat(k,1226) = 0._r8
         mat(k,1234) = 0._r8
         mat(k,1238) = 0._r8
         mat(k,1246) = 0._r8
         mat(k,1248) = 0._r8
         mat(k,1254) = 0._r8
         mat(k,1255) = 0._r8
         mat(k,1281) = 0._r8
         mat(k,1282) = 0._r8
         mat(k,1286) = 0._r8
         mat(k,1290) = 0._r8
         mat(k,1291) = 0._r8
         mat(k,1297) = 0._r8
         mat(k,1298) = 0._r8
         mat(k,1299) = 0._r8
         mat(k,1300) = 0._r8
         mat(k,1302) = 0._r8
         mat(k,1303) = 0._r8
         mat(k,1304) = 0._r8
         mat(k,1307) = 0._r8
         mat(k,1311) = 0._r8
         mat(k,1312) = 0._r8
         mat(k,1313) = 0._r8
         mat(k,1326) = 0._r8
         mat(k,1337) = 0._r8
         mat(k,1338) = 0._r8
         mat(k,1354) = 0._r8
         mat(k,1361) = 0._r8
         mat(k,1363) = 0._r8
         mat(k,1365) = 0._r8
         mat(k,1366) = 0._r8
         mat(k,1367) = 0._r8
         mat(k,1369) = 0._r8
         mat(k,1370) = 0._r8
         mat(k,1371) = 0._r8
         mat(k,1373) = 0._r8
         mat(k,1376) = 0._r8
         mat(k,1380) = 0._r8
         mat(k,1389) = 0._r8
         mat(k,1398) = 0._r8
         mat(k,1407) = 0._r8
         mat(k,1408) = 0._r8
         mat(k,1409) = 0._r8
         mat(k,1410) = 0._r8
         mat(k,1411) = 0._r8
         mat(k,1412) = 0._r8
         mat(k,1414) = 0._r8
         mat(k,1416) = 0._r8
         mat(k,1418) = 0._r8
         mat(k,1423) = 0._r8
         mat(k,1424) = 0._r8
         mat(k,1428) = 0._r8
         mat(k,1430) = 0._r8
         mat(k,1434) = 0._r8
         mat(k,1437) = 0._r8
         mat(k,1438) = 0._r8
         mat(k,1440) = 0._r8
         mat(k,1443) = 0._r8
         mat(k,1444) = 0._r8
         mat(k,1445) = 0._r8
         mat(k,1446) = 0._r8
         mat(k,1449) = 0._r8
         mat(k,1450) = 0._r8
         mat(k,1451) = 0._r8
         mat(k,1453) = 0._r8
         mat(k,1456) = 0._r8
         mat(k,1457) = 0._r8
         mat(k,1461) = 0._r8
         mat(k,1463) = 0._r8
         mat(k,1470) = 0._r8
         mat(k,1473) = 0._r8
         mat(k,1475) = 0._r8
         mat(k,1478) = 0._r8
         mat(k,1482) = 0._r8
         mat(k,1484) = 0._r8
         mat(k,1488) = 0._r8
         mat(k,1493) = 0._r8
         mat(k,1496) = 0._r8
         mat(k,1498) = 0._r8
         mat(k,1499) = 0._r8
         mat(k,1504) = 0._r8
         mat(k,1505) = 0._r8
         mat(k,1506) = 0._r8
         mat(k,1507) = 0._r8
         mat(k,1511) = 0._r8
         mat(k,1512) = 0._r8
         mat(k,1523) = 0._r8
         mat(k,1525) = 0._r8
         mat(k,1546) = 0._r8
         mat(k,1548) = 0._r8
         mat(k,1555) = 0._r8
         mat(k,1556) = 0._r8
         mat(k,1557) = 0._r8
         mat(k,1561) = 0._r8
         mat(k,1563) = 0._r8
         mat(k,1564) = 0._r8
         mat(k,1567) = 0._r8
         mat(k,1568) = 0._r8
         mat(k,1569) = 0._r8
         mat(k,1570) = 0._r8
         mat(k,1571) = 0._r8
         mat(k,1572) = 0._r8
         mat(k,1573) = 0._r8
         mat(k,1585) = 0._r8
         mat(k,1586) = 0._r8
         mat(k,1594) = 0._r8
         mat(k,1597) = 0._r8
         mat(k,1598) = 0._r8
         mat(k,1599) = 0._r8
         mat(k,1606) = 0._r8
         mat(k,1607) = 0._r8
         mat(k,1613) = 0._r8
         mat(k,1615) = 0._r8
         mat(k,1628) = 0._r8
         mat(k,1633) = 0._r8
         mat(k,1641) = 0._r8
         mat(k,1645) = 0._r8
         mat(k,1650) = 0._r8
         mat(k,1651) = 0._r8
         mat(k,1653) = 0._r8
         mat(k,1661) = 0._r8
         mat(k,1662) = 0._r8
         mat(k,1664) = 0._r8
         mat(k,1665) = 0._r8
         mat(k,1666) = 0._r8
         mat(k,1669) = 0._r8
         mat(k,1670) = 0._r8
         mat(k,1671) = 0._r8
         mat(k,1674) = 0._r8
         mat(k,1682) = 0._r8
         mat(k,1683) = 0._r8
         mat(k,1685) = 0._r8
         mat(k,1687) = 0._r8
         mat(k,1688) = 0._r8
         mat(k,1689) = 0._r8
         mat(k,1691) = 0._r8
         mat(k,1692) = 0._r8
         mat(k,1695) = 0._r8
         mat(k,1696) = 0._r8
         mat(k,1698) = 0._r8
         mat(k,1709) = 0._r8
         mat(k,1736) = 0._r8
         mat(k,1739) = 0._r8
         mat(k,1740) = 0._r8
         mat(k,1741) = 0._r8
         mat(k,1742) = 0._r8
         mat(k,1744) = 0._r8
         mat(k,1745) = 0._r8
         mat(k,1750) = 0._r8
         mat(k,1751) = 0._r8
         mat(k,1753) = 0._r8
         mat(k,1754) = 0._r8
         mat(k,1769) = 0._r8
         mat(k,1770) = 0._r8
         mat(k,1775) = 0._r8
         mat(k,1777) = 0._r8
         mat(k,1778) = 0._r8
         mat(k,1779) = 0._r8
         mat(k,1781) = 0._r8
         mat(k,1782) = 0._r8
         mat(k,1785) = 0._r8
         mat(k,1786) = 0._r8
         mat(k,1792) = 0._r8
         mat(k,1793) = 0._r8
         mat(k,1794) = 0._r8
         mat(k,1797) = 0._r8
         mat(k,1798) = 0._r8
         mat(k,1799) = 0._r8
         mat(k,1801) = 0._r8
         mat(k,1803) = 0._r8
         mat(k,1804) = 0._r8
         mat(k,1805) = 0._r8
         mat(k,1806) = 0._r8
         mat(k,1809) = 0._r8
         mat(k,1822) = 0._r8
         mat(k,1824) = 0._r8
         mat(k,1826) = 0._r8
         mat(k,1827) = 0._r8
         mat(k,1829) = 0._r8
         mat(k,1832) = 0._r8
         mat(k,1858) = 0._r8
         mat(k,1859) = 0._r8
         mat(k,1862) = 0._r8
         mat(k,1866) = 0._r8
         mat(k,1869) = 0._r8
         mat(k,1876) = 0._r8
         mat(k,1877) = 0._r8
         mat(k,1880) = 0._r8
         mat(k,1883) = 0._r8
         mat(k,1884) = 0._r8
         mat(k,1886) = 0._r8
         mat(k,1887) = 0._r8
         mat(k,1888) = 0._r8
         mat(k,1890) = 0._r8
         mat(k,1893) = 0._r8
         mat(k,1894) = 0._r8
         mat(k,1895) = 0._r8
         mat(k,1897) = 0._r8
         mat(k,1898) = 0._r8
         mat(k,1899) = 0._r8
         mat(k,1900) = 0._r8
         mat(k,1901) = 0._r8
         mat(k,1903) = 0._r8
         mat(k,1904) = 0._r8
         mat(k,1911) = 0._r8
         mat(k,1912) = 0._r8
         mat(k,1914) = 0._r8
         mat(k,1917) = 0._r8
         mat(k,1921) = 0._r8
         mat(k,1927) = 0._r8
         mat(k,1931) = 0._r8
         mat(k,1932) = 0._r8
         mat(k,1933) = 0._r8
         mat(k,1936) = 0._r8
         mat(k,1937) = 0._r8
         mat(k,1939) = 0._r8
         mat(k,1940) = 0._r8
         mat(k,1941) = 0._r8
         mat(k,1942) = 0._r8
         mat(k,1944) = 0._r8
         mat(k,1945) = 0._r8
         mat(k,1946) = 0._r8
         mat(k,1947) = 0._r8
         mat(k,1956) = 0._r8
         mat(k,1963) = 0._r8
         mat(k,1979) = 0._r8
         mat(k,1984) = 0._r8
         mat(k,1986) = 0._r8
         mat(k,1991) = 0._r8
         mat(k,1993) = 0._r8
         mat(k,1994) = 0._r8
         mat(k,1996) = 0._r8
         mat(k,1997) = 0._r8
         mat(k,1999) = 0._r8
         mat(k,2002) = 0._r8
         mat(k,2003) = 0._r8
         mat(k,2004) = 0._r8
         mat(k,2006) = 0._r8
         mat(k,2013) = 0._r8
         mat(k,2014) = 0._r8
         mat(k,2027) = 0._r8
         mat(k,2032) = 0._r8
         mat(k,2098) = 0._r8
         mat(k,2115) = 0._r8
         mat(k,2130) = 0._r8
         mat(k,2134) = 0._r8
         mat(k,2141) = 0._r8
         mat(k,2143) = 0._r8
         mat(k,2170) = 0._r8
         mat(k,2191) = 0._r8
         mat(k,2211) = 0._r8
         mat(k,2212) = 0._r8
         mat(k,2216) = 0._r8
         mat(k,2217) = 0._r8
         mat(k,2219) = 0._r8
         mat(k,2220) = 0._r8
         mat(k,2221) = 0._r8
         mat(k,2222) = 0._r8
         mat(k,2225) = 0._r8
         mat(k,2227) = 0._r8
         mat(k,2231) = 0._r8
         mat(k,2233) = 0._r8
         mat(k,2235) = 0._r8
         mat(k,2236) = 0._r8
         mat(k,2238) = 0._r8
         mat(k,2256) = 0._r8
         mat(k,2259) = 0._r8
         mat(k,2260) = 0._r8
         mat(k,2265) = 0._r8
         mat(k,2266) = 0._r8
         mat(k,2268) = 0._r8
         mat(k,2269) = 0._r8
         mat(k,2271) = 0._r8
         mat(k,2273) = 0._r8
         mat(k,2278) = 0._r8
         mat(k,2287) = 0._r8
         mat(k,2289) = 0._r8
         mat(k,2290) = 0._r8
         mat(k,2291) = 0._r8
         mat(k,2292) = 0._r8
         mat(k,2295) = 0._r8
         mat(k,2298) = 0._r8
         mat(k,2301) = 0._r8
         mat(k,2303) = 0._r8
         mat(k,2304) = 0._r8
         mat(k,2306) = 0._r8
         mat(k,2307) = 0._r8
         mat(k,2308) = 0._r8
         mat(k,2309) = 0._r8
         mat(k,2312) = 0._r8
         mat(k,2314) = 0._r8
         mat(k,2319) = 0._r8
         mat(k,2320) = 0._r8
         mat(k,2322) = 0._r8
         mat(k,2323) = 0._r8
         mat(k,2325) = 0._r8
         mat(k,2345) = 0._r8
         mat(k,2346) = 0._r8
         mat(k,2347) = 0._r8
         mat(k,2371) = 0._r8
         mat(k,2376) = 0._r8
         mat(k,2378) = 0._r8
         mat(k,2382) = 0._r8
         mat(k,2383) = 0._r8
         mat(k,2384) = 0._r8
         mat(k,2392) = 0._r8
         mat(k,2397) = 0._r8
         mat(k,2402) = 0._r8
         mat(k,2404) = 0._r8
         mat(k,2411) = 0._r8
         mat(k,2423) = 0._r8
         mat(k,2438) = 0._r8
         mat(k,2439) = 0._r8
         mat(k,2440) = 0._r8
         mat(k,2441) = 0._r8
         mat(k,2443) = 0._r8
         mat(k,2450) = 0._r8
         mat(k,2452) = 0._r8
         mat(k,2456) = 0._r8
         mat(k,2457) = 0._r8
         mat(k,2459) = 0._r8
         mat(k,2461) = 0._r8
         mat(k,2464) = 0._r8
         mat(k,2465) = 0._r8
         mat(k,2466) = 0._r8
         mat(k,2467) = 0._r8
         mat(k,2468) = 0._r8
         mat(k,2469) = 0._r8
         mat(k,2471) = 0._r8
         mat(k,2473) = 0._r8
         mat(k,2474) = 0._r8
         mat(k,2476) = 0._r8
         mat(k,2480) = 0._r8
         mat(k,2482) = 0._r8
         mat(k,2484) = 0._r8
         mat(k,2486) = 0._r8
         mat(k,2524) = 0._r8
         mat(k,2556) = 0._r8
         mat(k,2557) = 0._r8
         mat(k,2558) = 0._r8
         mat(k,2559) = 0._r8
         mat(k,2562) = 0._r8
         mat(k,2563) = 0._r8
         mat(k,2564) = 0._r8
         mat(k,2565) = 0._r8
         mat(k,2576) = 0._r8
         mat(k,2581) = 0._r8
         mat(k,2584) = 0._r8
         mat(k,2585) = 0._r8
         mat(k,2586) = 0._r8
         mat(k,2587) = 0._r8
         mat(k,2588) = 0._r8
         mat(k,2590) = 0._r8
         mat(k,2591) = 0._r8
         mat(k,2593) = 0._r8
         mat(k,2595) = 0._r8
         mat(k,2596) = 0._r8
         mat(k,2597) = 0._r8
         mat(k,2598) = 0._r8
         mat(k,2599) = 0._r8
         mat(k,2600) = 0._r8
         mat(k,2601) = 0._r8
         mat(k,2603) = 0._r8
         mat(k,2604) = 0._r8
         mat(k,2605) = 0._r8
         mat(k,2616) = 0._r8
         mat(k,2617) = 0._r8
         mat(k,2619) = 0._r8
         mat(k,2621) = 0._r8
         mat(k,2622) = 0._r8
         mat(k,2623) = 0._r8
         mat(k,2624) = 0._r8
         mat(k,2633) = 0._r8
         mat(k,2635) = 0._r8
         mat(k,2640) = 0._r8
         mat(k,2648) = 0._r8
         mat(k,2651) = 0._r8
         mat(k,2656) = 0._r8
         mat(k,2658) = 0._r8
         mat(k,2663) = 0._r8
         mat(k,2664) = 0._r8
         mat(k,2666) = 0._r8
         mat(k,2670) = 0._r8
         mat(k,2682) = 0._r8
         mat(k,2687) = 0._r8
         mat(k,2700) = 0._r8
         mat(k,2701) = 0._r8
         mat(k,2703) = 0._r8
         mat(k,2712) = 0._r8
         mat(k,2716) = 0._r8
         mat(k,2717) = 0._r8
         mat(k,2723) = 0._r8
         mat(k,2724) = 0._r8
         mat(k,2729) = 0._r8
         mat(k,2732) = 0._r8
         mat(k,2743) = 0._r8
         mat(k,2744) = 0._r8
         mat(k,2749) = 0._r8
         mat(k,2750) = 0._r8
         mat(k,2762) = 0._r8
         mat(k,2764) = 0._r8
         mat(k,2765) = 0._r8
         mat(k,2767) = 0._r8
         mat(k,2768) = 0._r8
         mat(k,2770) = 0._r8
         mat(k,2771) = 0._r8
         mat(k,2772) = 0._r8
         mat(k,2773) = 0._r8
         mat(k,2775) = 0._r8
         mat(k,2779) = 0._r8
         mat(k,2781) = 0._r8
         mat(k,2787) = 0._r8
         mat(k,2789) = 0._r8
         mat(k,2794) = 0._r8
         mat(k,2796) = 0._r8
         mat(k,2797) = 0._r8
         mat(k,2798) = 0._r8
         mat(k,2799) = 0._r8
         mat(k,2800) = 0._r8
         mat(k,2801) = 0._r8
         mat(k,2804) = 0._r8
         mat(k,2805) = 0._r8
         mat(k,2807) = 0._r8
         mat(k,2808) = 0._r8
         mat(k,2809) = 0._r8
         mat(k,2810) = 0._r8
         mat(k,2811) = 0._r8
         mat(k,2812) = 0._r8
         mat(k,2813) = 0._r8
         mat(k,2814) = 0._r8
         mat(k,2815) = 0._r8
         mat(k,2817) = 0._r8
         mat(k,2818) = 0._r8
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
         mat(k, 56) = mat(k, 56) - dti(k)
         mat(k, 57) = mat(k, 57) - dti(k)
         mat(k, 63) = mat(k, 63) - dti(k)
         mat(k, 64) = mat(k, 64) - dti(k)
         mat(k, 65) = mat(k, 65) - dti(k)
         mat(k, 66) = mat(k, 66) - dti(k)
         mat(k, 67) = mat(k, 67) - dti(k)
         mat(k, 68) = mat(k, 68) - dti(k)
         mat(k, 69) = mat(k, 69) - dti(k)
         mat(k, 70) = mat(k, 70) - dti(k)
         mat(k, 71) = mat(k, 71) - dti(k)
         mat(k, 73) = mat(k, 73) - dti(k)
         mat(k, 76) = mat(k, 76) - dti(k)
         mat(k, 79) = mat(k, 79) - dti(k)
         mat(k, 82) = mat(k, 82) - dti(k)
         mat(k, 85) = mat(k, 85) - dti(k)
         mat(k, 89) = mat(k, 89) - dti(k)
         mat(k, 93) = mat(k, 93) - dti(k)
         mat(k, 97) = mat(k, 97) - dti(k)
         mat(k, 101) = mat(k, 101) - dti(k)
         mat(k, 105) = mat(k, 105) - dti(k)
         mat(k, 109) = mat(k, 109) - dti(k)
         mat(k, 113) = mat(k, 113) - dti(k)
         mat(k, 117) = mat(k, 117) - dti(k)
         mat(k, 121) = mat(k, 121) - dti(k)
         mat(k, 125) = mat(k, 125) - dti(k)
         mat(k, 128) = mat(k, 128) - dti(k)
         mat(k, 131) = mat(k, 131) - dti(k)
         mat(k, 134) = mat(k, 134) - dti(k)
         mat(k, 137) = mat(k, 137) - dti(k)
         mat(k, 140) = mat(k, 140) - dti(k)
         mat(k, 145) = mat(k, 145) - dti(k)
         mat(k, 150) = mat(k, 150) - dti(k)
         mat(k, 155) = mat(k, 155) - dti(k)
         mat(k, 165) = mat(k, 165) - dti(k)
         mat(k, 171) = mat(k, 171) - dti(k)
         mat(k, 175) = mat(k, 175) - dti(k)
         mat(k, 180) = mat(k, 180) - dti(k)
         mat(k, 183) = mat(k, 183) - dti(k)
         mat(k, 185) = mat(k, 185) - dti(k)
         mat(k, 187) = mat(k, 187) - dti(k)
         mat(k, 191) = mat(k, 191) - dti(k)
         mat(k, 195) = mat(k, 195) - dti(k)
         mat(k, 204) = mat(k, 204) - dti(k)
         mat(k, 211) = mat(k, 211) - dti(k)
         mat(k, 216) = mat(k, 216) - dti(k)
         mat(k, 220) = mat(k, 220) - dti(k)
         mat(k, 229) = mat(k, 229) - dti(k)
         mat(k, 237) = mat(k, 237) - dti(k)
         mat(k, 242) = mat(k, 242) - dti(k)
         mat(k, 247) = mat(k, 247) - dti(k)
         mat(k, 251) = mat(k, 251) - dti(k)
         mat(k, 256) = mat(k, 256) - dti(k)
         mat(k, 259) = mat(k, 259) - dti(k)
         mat(k, 264) = mat(k, 264) - dti(k)
         mat(k, 267) = mat(k, 267) - dti(k)
         mat(k, 270) = mat(k, 270) - dti(k)
         mat(k, 273) = mat(k, 273) - dti(k)
         mat(k, 278) = mat(k, 278) - dti(k)
         mat(k, 283) = mat(k, 283) - dti(k)
         mat(k, 287) = mat(k, 287) - dti(k)
         mat(k, 291) = mat(k, 291) - dti(k)
         mat(k, 295) = mat(k, 295) - dti(k)
         mat(k, 298) = mat(k, 298) - dti(k)
         mat(k, 304) = mat(k, 304) - dti(k)
         mat(k, 308) = mat(k, 308) - dti(k)
         mat(k, 314) = mat(k, 314) - dti(k)
         mat(k, 320) = mat(k, 320) - dti(k)
         mat(k, 326) = mat(k, 326) - dti(k)
         mat(k, 332) = mat(k, 332) - dti(k)
         mat(k, 337) = mat(k, 337) - dti(k)
         mat(k, 342) = mat(k, 342) - dti(k)
         mat(k, 347) = mat(k, 347) - dti(k)
         mat(k, 354) = mat(k, 354) - dti(k)
         mat(k, 360) = mat(k, 360) - dti(k)
         mat(k, 365) = mat(k, 365) - dti(k)
         mat(k, 370) = mat(k, 370) - dti(k)
         mat(k, 374) = mat(k, 374) - dti(k)
         mat(k, 381) = mat(k, 381) - dti(k)
         mat(k, 384) = mat(k, 384) - dti(k)
         mat(k, 392) = mat(k, 392) - dti(k)
         mat(k, 400) = mat(k, 400) - dti(k)
         mat(k, 408) = mat(k, 408) - dti(k)
         mat(k, 416) = mat(k, 416) - dti(k)
         mat(k, 419) = mat(k, 419) - dti(k)
         mat(k, 427) = mat(k, 427) - dti(k)
         mat(k, 433) = mat(k, 433) - dti(k)
         mat(k, 439) = mat(k, 439) - dti(k)
         mat(k, 445) = mat(k, 445) - dti(k)
         mat(k, 451) = mat(k, 451) - dti(k)
         mat(k, 457) = mat(k, 457) - dti(k)
         mat(k, 463) = mat(k, 463) - dti(k)
         mat(k, 469) = mat(k, 469) - dti(k)
         mat(k, 475) = mat(k, 475) - dti(k)
         mat(k, 483) = mat(k, 483) - dti(k)
         mat(k, 489) = mat(k, 489) - dti(k)
         mat(k, 496) = mat(k, 496) - dti(k)
         mat(k, 502) = mat(k, 502) - dti(k)
         mat(k, 508) = mat(k, 508) - dti(k)
         mat(k, 513) = mat(k, 513) - dti(k)
         mat(k, 516) = mat(k, 516) - dti(k)
         mat(k, 521) = mat(k, 521) - dti(k)
         mat(k, 525) = mat(k, 525) - dti(k)
         mat(k, 529) = mat(k, 529) - dti(k)
         mat(k, 532) = mat(k, 532) - dti(k)
         mat(k, 539) = mat(k, 539) - dti(k)
         mat(k, 546) = mat(k, 546) - dti(k)
         mat(k, 555) = mat(k, 555) - dti(k)
         mat(k, 562) = mat(k, 562) - dti(k)
         mat(k, 570) = mat(k, 570) - dti(k)
         mat(k, 578) = mat(k, 578) - dti(k)
         mat(k, 583) = mat(k, 583) - dti(k)
         mat(k, 588) = mat(k, 588) - dti(k)
         mat(k, 592) = mat(k, 592) - dti(k)
         mat(k, 598) = mat(k, 598) - dti(k)
         mat(k, 603) = mat(k, 603) - dti(k)
         mat(k, 611) = mat(k, 611) - dti(k)
         mat(k, 619) = mat(k, 619) - dti(k)
         mat(k, 627) = mat(k, 627) - dti(k)
         mat(k, 635) = mat(k, 635) - dti(k)
         mat(k, 643) = mat(k, 643) - dti(k)
         mat(k, 652) = mat(k, 652) - dti(k)
         mat(k, 656) = mat(k, 656) - dti(k)
         mat(k, 663) = mat(k, 663) - dti(k)
         mat(k, 672) = mat(k, 672) - dti(k)
         mat(k, 679) = mat(k, 679) - dti(k)
         mat(k, 690) = mat(k, 690) - dti(k)
         mat(k, 696) = mat(k, 696) - dti(k)
         mat(k, 704) = mat(k, 704) - dti(k)
         mat(k, 711) = mat(k, 711) - dti(k)
         mat(k, 721) = mat(k, 721) - dti(k)
         mat(k, 734) = mat(k, 734) - dti(k)
         mat(k, 745) = mat(k, 745) - dti(k)
         mat(k, 756) = mat(k, 756) - dti(k)
         mat(k, 764) = mat(k, 764) - dti(k)
         mat(k, 774) = mat(k, 774) - dti(k)
         mat(k, 780) = mat(k, 780) - dti(k)
         mat(k, 791) = mat(k, 791) - dti(k)
         mat(k, 802) = mat(k, 802) - dti(k)
         mat(k, 813) = mat(k, 813) - dti(k)
         mat(k, 823) = mat(k, 823) - dti(k)
         mat(k, 836) = mat(k, 836) - dti(k)
         mat(k, 847) = mat(k, 847) - dti(k)
         mat(k, 857) = mat(k, 857) - dti(k)
         mat(k, 865) = mat(k, 865) - dti(k)
         mat(k, 873) = mat(k, 873) - dti(k)
         mat(k, 878) = mat(k, 878) - dti(k)
         mat(k, 889) = mat(k, 889) - dti(k)
         mat(k, 901) = mat(k, 901) - dti(k)
         mat(k, 909) = mat(k, 909) - dti(k)
         mat(k, 921) = mat(k, 921) - dti(k)
         mat(k, 929) = mat(k, 929) - dti(k)
         mat(k, 940) = mat(k, 940) - dti(k)
         mat(k, 958) = mat(k, 958) - dti(k)
         mat(k, 970) = mat(k, 970) - dti(k)
         mat(k, 981) = mat(k, 981) - dti(k)
         mat(k, 993) = mat(k, 993) - dti(k)
         mat(k,1002) = mat(k,1002) - dti(k)
         mat(k,1011) = mat(k,1011) - dti(k)
         mat(k,1024) = mat(k,1024) - dti(k)
         mat(k,1051) = mat(k,1051) - dti(k)
         mat(k,1075) = mat(k,1075) - dti(k)
         mat(k,1086) = mat(k,1086) - dti(k)
         mat(k,1093) = mat(k,1093) - dti(k)
         mat(k,1104) = mat(k,1104) - dti(k)
         mat(k,1115) = mat(k,1115) - dti(k)
         mat(k,1125) = mat(k,1125) - dti(k)
         mat(k,1137) = mat(k,1137) - dti(k)
         mat(k,1146) = mat(k,1146) - dti(k)
         mat(k,1156) = mat(k,1156) - dti(k)
         mat(k,1169) = mat(k,1169) - dti(k)
         mat(k,1185) = mat(k,1185) - dti(k)
         mat(k,1203) = mat(k,1203) - dti(k)
         mat(k,1216) = mat(k,1216) - dti(k)
         mat(k,1229) = mat(k,1229) - dti(k)
         mat(k,1242) = mat(k,1242) - dti(k)
         mat(k,1256) = mat(k,1256) - dti(k)
         mat(k,1263) = mat(k,1263) - dti(k)
         mat(k,1269) = mat(k,1269) - dti(k)
         mat(k,1280) = mat(k,1280) - dti(k)
         mat(k,1301) = mat(k,1301) - dti(k)
         mat(k,1317) = mat(k,1317) - dti(k)
         mat(k,1329) = mat(k,1329) - dti(k)
         mat(k,1348) = mat(k,1348) - dti(k)
         mat(k,1368) = mat(k,1368) - dti(k)
         mat(k,1384) = mat(k,1384) - dti(k)
         mat(k,1396) = mat(k,1396) - dti(k)
         mat(k,1415) = mat(k,1415) - dti(k)
         mat(k,1448) = mat(k,1448) - dti(k)
         mat(k,1472) = mat(k,1472) - dti(k)
         mat(k,1492) = mat(k,1492) - dti(k)
         mat(k,1513) = mat(k,1513) - dti(k)
         mat(k,1544) = mat(k,1544) - dti(k)
         mat(k,1566) = mat(k,1566) - dti(k)
         mat(k,1578) = mat(k,1578) - dti(k)
         mat(k,1592) = mat(k,1592) - dti(k)
         mat(k,1605) = mat(k,1605) - dti(k)
         mat(k,1620) = mat(k,1620) - dti(k)
         mat(k,1639) = mat(k,1639) - dti(k)
         mat(k,1660) = mat(k,1660) - dti(k)
         mat(k,1684) = mat(k,1684) - dti(k)
         mat(k,1737) = mat(k,1737) - dti(k)
         mat(k,1772) = mat(k,1772) - dti(k)
         mat(k,1795) = mat(k,1795) - dti(k)
         mat(k,1819) = mat(k,1819) - dti(k)
         mat(k,1864) = mat(k,1864) - dti(k)
         mat(k,1889) = mat(k,1889) - dti(k)
         mat(k,1948) = mat(k,1948) - dti(k)
         mat(k,2018) = mat(k,2018) - dti(k)
         mat(k,2195) = mat(k,2195) - dti(k)
         mat(k,2226) = mat(k,2226) - dti(k)
         mat(k,2284) = mat(k,2284) - dti(k)
         mat(k,2315) = mat(k,2315) - dti(k)
         mat(k,2419) = mat(k,2419) - dti(k)
         mat(k,2449) = mat(k,2449) - dti(k)
         mat(k,2481) = mat(k,2481) - dti(k)
         mat(k,2575) = mat(k,2575) - dti(k)
         mat(k,2602) = mat(k,2602) - dti(k)
         mat(k,2636) = mat(k,2636) - dti(k)
         mat(k,2684) = mat(k,2684) - dti(k)
         mat(k,2715) = mat(k,2715) - dti(k)
         mat(k,2788) = mat(k,2788) - dti(k)
         mat(k,2819) = mat(k,2819) - dti(k)
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
