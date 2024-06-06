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
         mat(k,764) = -(rxt(k,489)*y(k,259))
         mat(k,2123) = -rxt(k,489)*y(k,1)
         mat(k,2550) = rxt(k,492)*y(k,222)
         mat(k,1091) = rxt(k,492)*y(k,155)
         mat(k,753) = -(rxt(k,493)*y(k,259))
         mat(k,2122) = -rxt(k,493)*y(k,2)
         mat(k,1090) = rxt(k,490)*y(k,237)
         mat(k,2369) = rxt(k,490)*y(k,222)
         mat(k,1070) = -(rxt(k,572)*y(k,157) + rxt(k,573)*y(k,166) + rxt(k,574) &
                      *y(k,259))
         mat(k,2781) = -rxt(k,572)*y(k,8)
         mat(k,1944) = -rxt(k,573)*y(k,8)
         mat(k,2144) = -rxt(k,574)*y(k,8)
         mat(k,192) = -(rxt(k,531)*y(k,259))
         mat(k,2046) = -rxt(k,531)*y(k,9)
         mat(k,476) = -(rxt(k,534)*y(k,259))
         mat(k,2089) = -rxt(k,534)*y(k,10)
         mat(k,572) = rxt(k,532)*y(k,237)
         mat(k,2348) = rxt(k,532)*y(k,224)
         mat(k,193) = .120_r8*rxt(k,531)*y(k,259)
         mat(k,2047) = .120_r8*rxt(k,531)*y(k,9)
         mat(k,1068) = .100_r8*rxt(k,573)*y(k,166)
         mat(k,1040) = .100_r8*rxt(k,576)*y(k,166)
         mat(k,1933) = .100_r8*rxt(k,573)*y(k,8) + .100_r8*rxt(k,576)*y(k,141)
         mat(k,2536) = .500_r8*rxt(k,533)*y(k,224) + .200_r8*rxt(k,560)*y(k,266) &
                      + .060_r8*rxt(k,566)*y(k,269)
         mat(k,573) = .500_r8*rxt(k,533)*y(k,155)
         mat(k,828) = .200_r8*rxt(k,560)*y(k,155)
         mat(k,851) = .060_r8*rxt(k,566)*y(k,155)
         mat(k,2530) = .200_r8*rxt(k,560)*y(k,266) + .200_r8*rxt(k,566)*y(k,269)
         mat(k,827) = .200_r8*rxt(k,560)*y(k,155)
         mat(k,849) = .200_r8*rxt(k,566)*y(k,155)
         mat(k,2546) = .200_r8*rxt(k,560)*y(k,266) + .150_r8*rxt(k,566)*y(k,269)
         mat(k,829) = .200_r8*rxt(k,560)*y(k,155)
         mat(k,852) = .150_r8*rxt(k,566)*y(k,155)
         mat(k,2531) = .210_r8*rxt(k,566)*y(k,269)
         mat(k,850) = .210_r8*rxt(k,566)*y(k,155)
         mat(k,283) = -(rxt(k,494)*y(k,259))
         mat(k,2062) = -rxt(k,494)*y(k,17)
         mat(k,1067) = .050_r8*rxt(k,573)*y(k,166)
         mat(k,1039) = .050_r8*rxt(k,576)*y(k,166)
         mat(k,1932) = .050_r8*rxt(k,573)*y(k,8) + .050_r8*rxt(k,576)*y(k,141)
         mat(k,411) = -(rxt(k,460)*y(k,157) + rxt(k,461)*y(k,259))
         mat(k,2772) = -rxt(k,460)*y(k,18)
         mat(k,2081) = -rxt(k,461)*y(k,18)
         mat(k,2313) = -(rxt(k,283)*y(k,53) + rxt(k,284)*y(k,237) + rxt(k,285) &
                      *y(k,156) + rxt(k,286)*y(k,166) + rxt(k,293)*y(k,24) + rxt(k,322) &
                      *y(k,127))
         mat(k,2640) = -rxt(k,283)*y(k,19)
         mat(k,2428) = -rxt(k,284)*y(k,19)
         mat(k,2698) = -rxt(k,285)*y(k,19)
         mat(k,1983) = -rxt(k,286)*y(k,19)
         mat(k,991) = -rxt(k,293)*y(k,19)
         mat(k,2757) = -rxt(k,322)*y(k,19)
         mat(k,543) = rxt(k,282)*y(k,259)
         mat(k,2457) = 4.000_r8*rxt(k,287)*y(k,23) + (rxt(k,288)+rxt(k,289))*y(k,76) &
                      + rxt(k,595)*y(k,85) + rxt(k,312)*y(k,117) + (rxt(k,323) &
                       +rxt(k,324))*y(k,127) + rxt(k,292)*y(k,155) + rxt(k,297) &
                      *y(k,164) + rxt(k,606)*y(k,183) + rxt(k,298)*y(k,259)
         mat(k,175) = rxt(k,272)*y(k,255)
         mat(k,180) = rxt(k,302)*y(k,255)
         mat(k,563) = 2.000_r8*rxt(k,356)*y(k,72) + 2.000_r8*rxt(k,383)*y(k,255) &
                      + 2.000_r8*rxt(k,357)*y(k,259)
         mat(k,151) = rxt(k,358)*y(k,259)
         mat(k,688) = rxt(k,361)*y(k,72) + rxt(k,384)*y(k,255) + rxt(k,362)*y(k,259)
         mat(k,119) = 2.000_r8*rxt(k,368)*y(k,259)
         mat(k,487) = 3.000_r8*rxt(k,369)*y(k,72) + 3.000_r8*rxt(k,303)*y(k,255) &
                      + 3.000_r8*rxt(k,370)*y(k,259)
         mat(k,123) = rxt(k,371)*y(k,259)
         mat(k,2283) = 2.000_r8*rxt(k,356)*y(k,47) + rxt(k,361)*y(k,54) &
                      + 3.000_r8*rxt(k,369)*y(k,68)
         mat(k,2013) = (rxt(k,288)+rxt(k,289))*y(k,23)
         mat(k,1150) = rxt(k,595)*y(k,23)
         mat(k,127) = 2.000_r8*rxt(k,304)*y(k,255)
         mat(k,1606) = rxt(k,299)*y(k,164) + rxt(k,305)*y(k,255) + rxt(k,300)*y(k,259)
         mat(k,2226) = rxt(k,312)*y(k,23)
         mat(k,2757) = mat(k,2757) + (rxt(k,323)+rxt(k,324))*y(k,23)
         mat(k,2609) = rxt(k,292)*y(k,23)
         mat(k,2504) = rxt(k,297)*y(k,23) + rxt(k,299)*y(k,99)
         mat(k,1646) = rxt(k,606)*y(k,23)
         mat(k,1890) = rxt(k,272)*y(k,40) + rxt(k,302)*y(k,41) + 2.000_r8*rxt(k,383) &
                      *y(k,47) + rxt(k,384)*y(k,54) + 3.000_r8*rxt(k,303)*y(k,68) &
                      + 2.000_r8*rxt(k,304)*y(k,96) + rxt(k,305)*y(k,99)
         mat(k,2196) = rxt(k,282)*y(k,20) + rxt(k,298)*y(k,23) + 2.000_r8*rxt(k,357) &
                      *y(k,47) + rxt(k,358)*y(k,48) + rxt(k,362)*y(k,54) &
                      + 2.000_r8*rxt(k,368)*y(k,67) + 3.000_r8*rxt(k,370)*y(k,68) &
                      + rxt(k,371)*y(k,69) + rxt(k,300)*y(k,99)
         mat(k,540) = -(rxt(k,282)*y(k,259))
         mat(k,2097) = -rxt(k,282)*y(k,20)
         mat(k,2295) = rxt(k,293)*y(k,24)
         mat(k,985) = rxt(k,293)*y(k,19)
         mat(k,1595) = (rxt(k,623)+rxt(k,697)+rxt(k,710)+rxt(k,719))*y(k,110)
         mat(k,1697) = (rxt(k,623)+rxt(k,697)+rxt(k,710)+rxt(k,719))*y(k,99)
         mat(k,2440) = rxt(k,290)*y(k,76)
         mat(k,986) = rxt(k,294)*y(k,72)
         mat(k,2250) = rxt(k,294)*y(k,24)
         mat(k,1998) = rxt(k,290)*y(k,23)
         mat(k,1596) = (rxt(k,622)+rxt(k,699)+rxt(k,707)+rxt(k,716))*y(k,111)
         mat(k,1809) = (rxt(k,625)+rxt(k,696)+rxt(k,709)+rxt(k,718))*y(k,110)
         mat(k,1698) = (rxt(k,625)+rxt(k,696)+rxt(k,709)+rxt(k,718))*y(k,103)
         mat(k,1831) = (rxt(k,622)+rxt(k,699)+rxt(k,707)+rxt(k,716))*y(k,99)
         mat(k,2294) = rxt(k,285)*y(k,156)
         mat(k,2654) = rxt(k,285)*y(k,19)
         mat(k,2459) = -(4._r8*rxt(k,287)*y(k,23) + (rxt(k,288) + rxt(k,289) + rxt(k,290) &
                      ) * y(k,76) + rxt(k,291)*y(k,237) + rxt(k,292)*y(k,155) &
                      + rxt(k,295)*y(k,156) + rxt(k,297)*y(k,164) + rxt(k,298) &
                      *y(k,259) + rxt(k,312)*y(k,117) + (rxt(k,323) + rxt(k,324) &
                      ) * y(k,127) + rxt(k,595)*y(k,85) + rxt(k,606)*y(k,183))
         mat(k,2015) = -(rxt(k,288) + rxt(k,289) + rxt(k,290)) * y(k,23)
         mat(k,2430) = -rxt(k,291)*y(k,23)
         mat(k,2611) = -rxt(k,292)*y(k,23)
         mat(k,2700) = -rxt(k,295)*y(k,23)
         mat(k,2506) = -rxt(k,297)*y(k,23)
         mat(k,2198) = -rxt(k,298)*y(k,23)
         mat(k,2228) = -rxt(k,312)*y(k,23)
         mat(k,2759) = -(rxt(k,323) + rxt(k,324)) * y(k,23)
         mat(k,1152) = -rxt(k,595)*y(k,23)
         mat(k,1648) = -rxt(k,606)*y(k,23)
         mat(k,2315) = rxt(k,322)*y(k,127) + rxt(k,286)*y(k,166)
         mat(k,992) = rxt(k,296)*y(k,164)
         mat(k,1607) = rxt(k,306)*y(k,255)
         mat(k,1712) = rxt(k,301)*y(k,164)
         mat(k,2759) = mat(k,2759) + rxt(k,322)*y(k,19)
         mat(k,2506) = mat(k,2506) + rxt(k,296)*y(k,24) + rxt(k,301)*y(k,110)
         mat(k,1985) = rxt(k,286)*y(k,19)
         mat(k,1892) = rxt(k,306)*y(k,99)
         mat(k,987) = -(rxt(k,293)*y(k,19) + rxt(k,294)*y(k,72) + rxt(k,296)*y(k,164))
         mat(k,2297) = -rxt(k,293)*y(k,24)
         mat(k,2255) = -rxt(k,294)*y(k,24)
         mat(k,2480) = -rxt(k,296)*y(k,24)
         mat(k,2442) = rxt(k,295)*y(k,156)
         mat(k,2670) = rxt(k,295)*y(k,23)
         mat(k,286) = -(rxt(k,535)*y(k,259))
         mat(k,2063) = -rxt(k,535)*y(k,26)
         mat(k,2528) = rxt(k,538)*y(k,226)
         mat(k,500) = rxt(k,538)*y(k,155)
         mat(k,379) = -(rxt(k,537)*y(k,259))
         mat(k,2076) = -rxt(k,537)*y(k,27)
         mat(k,501) = rxt(k,536)*y(k,237)
         mat(k,2342) = rxt(k,536)*y(k,226)
         mat(k,222) = -(rxt(k,352)*y(k,72) + rxt(k,353)*y(k,259))
         mat(k,2238) = -rxt(k,352)*y(k,28)
         mat(k,2050) = -rxt(k,353)*y(k,28)
         mat(k,333) = -(rxt(k,409)*y(k,72) + rxt(k,410)*y(k,259))
         mat(k,2240) = -rxt(k,409)*y(k,29)
         mat(k,2070) = -rxt(k,410)*y(k,29)
         mat(k,638) = -(rxt(k,411)*y(k,72) + rxt(k,412)*y(k,166) + rxt(k,437)*y(k,259))
         mat(k,2251) = -rxt(k,411)*y(k,30)
         mat(k,1935) = -rxt(k,412)*y(k,30)
         mat(k,2109) = -rxt(k,437)*y(k,30)
         mat(k,292) = -(rxt(k,354)*y(k,72) + rxt(k,355)*y(k,259))
         mat(k,2239) = -rxt(k,354)*y(k,31)
         mat(k,2065) = -rxt(k,355)*y(k,31)
         mat(k,306) = -(rxt(k,417)*y(k,259))
         mat(k,2067) = -rxt(k,417)*y(k,32)
         mat(k,1007) = .800_r8*rxt(k,413)*y(k,227) + .200_r8*rxt(k,414)*y(k,231)
         mat(k,1719) = .200_r8*rxt(k,414)*y(k,227)
         mat(k,384) = -(rxt(k,418)*y(k,259))
         mat(k,2077) = -rxt(k,418)*y(k,33)
         mat(k,1008) = rxt(k,415)*y(k,237)
         mat(k,2343) = rxt(k,415)*y(k,227)
         mat(k,339) = -(rxt(k,419)*y(k,72) + rxt(k,420)*y(k,259))
         mat(k,2241) = -rxt(k,419)*y(k,34)
         mat(k,2071) = -rxt(k,420)*y(k,34)
         mat(k,1204) = -(rxt(k,440)*y(k,157) + rxt(k,441)*y(k,166) + rxt(k,458) &
                      *y(k,259))
         mat(k,2790) = -rxt(k,440)*y(k,35)
         mat(k,1950) = -rxt(k,441)*y(k,35)
         mat(k,2155) = -rxt(k,458)*y(k,35)
         mat(k,959) = .130_r8*rxt(k,518)*y(k,166)
         mat(k,1950) = mat(k,1950) + .130_r8*rxt(k,518)*y(k,129)
         mat(k,470) = -(rxt(k,445)*y(k,259))
         mat(k,2088) = -rxt(k,445)*y(k,36)
         mat(k,907) = rxt(k,443)*y(k,237)
         mat(k,2347) = rxt(k,443)*y(k,228)
         mat(k,132) = -(rxt(k,446)*y(k,259))
         mat(k,2042) = -rxt(k,446)*y(k,37)
         mat(k,310) = -(rxt(k,541)*y(k,259))
         mat(k,2068) = -rxt(k,541)*y(k,38)
         mat(k,721) = rxt(k,539)*y(k,237)
         mat(k,2338) = rxt(k,539)*y(k,229)
         mat(k,113) = -(rxt(k,271)*y(k,255))
         mat(k,1853) = -rxt(k,271)*y(k,39)
         mat(k,171) = -(rxt(k,272)*y(k,255))
         mat(k,1858) = -rxt(k,272)*y(k,40)
         mat(k,176) = -(rxt(k,302)*y(k,255))
         mat(k,1859) = -rxt(k,302)*y(k,41)
         mat(k,136) = -(rxt(k,273)*y(k,255))
         mat(k,1855) = -rxt(k,273)*y(k,42)
         mat(k,181) = -(rxt(k,274)*y(k,255))
         mat(k,1860) = -rxt(k,274)*y(k,43)
         mat(k,140) = -(rxt(k,275)*y(k,255))
         mat(k,1856) = -rxt(k,275)*y(k,44)
         mat(k,186) = -(rxt(k,276)*y(k,255))
         mat(k,1861) = -rxt(k,276)*y(k,45)
         mat(k,144) = -(rxt(k,277)*y(k,255))
         mat(k,1857) = -rxt(k,277)*y(k,46)
         mat(k,558) = -(rxt(k,356)*y(k,72) + rxt(k,357)*y(k,259) + rxt(k,383)*y(k,255))
         mat(k,2248) = -rxt(k,356)*y(k,47)
         mat(k,2100) = -rxt(k,357)*y(k,47)
         mat(k,1871) = -rxt(k,383)*y(k,47)
         mat(k,148) = -(rxt(k,358)*y(k,259))
         mat(k,2043) = -rxt(k,358)*y(k,48)
         mat(k,345) = -(rxt(k,359)*y(k,72) + rxt(k,360)*y(k,259))
         mat(k,2242) = -rxt(k,359)*y(k,49)
         mat(k,2072) = -rxt(k,360)*y(k,49)
         mat(k,2645) = -(rxt(k,244)*y(k,72) + rxt(k,283)*y(k,19) + rxt(k,388)*y(k,237) &
                      + rxt(k,389)*y(k,157) + rxt(k,390)*y(k,164) + rxt(k,391) &
                      *y(k,259))
         mat(k,2288) = -rxt(k,244)*y(k,53)
         mat(k,2318) = -rxt(k,283)*y(k,53)
         mat(k,2433) = -rxt(k,388)*y(k,53)
         mat(k,2834) = -rxt(k,389)*y(k,53)
         mat(k,2509) = -rxt(k,390)*y(k,53)
         mat(k,2201) = -rxt(k,391)*y(k,53)
         mat(k,772) = .400_r8*rxt(k,489)*y(k,259)
         mat(k,1086) = .340_r8*rxt(k,573)*y(k,166)
         mat(k,416) = .500_r8*rxt(k,460)*y(k,157)
         mat(k,645) = rxt(k,412)*y(k,166)
         mat(k,1218) = .500_r8*rxt(k,441)*y(k,166)
         mat(k,680) = .500_r8*rxt(k,429)*y(k,259)
         mat(k,895) = rxt(k,396)*y(k,259)
         mat(k,492) = .300_r8*rxt(k,397)*y(k,259)
         mat(k,1669) = (rxt(k,405)+rxt(k,406))*y(k,255)
         mat(k,1180) = rxt(k,372)*y(k,231)
         mat(k,2018) = rxt(k,253)*y(k,231)
         mat(k,1287) = .800_r8*rxt(k,434)*y(k,259)
         mat(k,971) = .910_r8*rxt(k,518)*y(k,166)
         mat(k,668) = .300_r8*rxt(k,509)*y(k,259)
         mat(k,1412) = .120_r8*rxt(k,471)*y(k,166)
         mat(k,697) = .500_r8*rxt(k,484)*y(k,259)
         mat(k,1058) = .340_r8*rxt(k,576)*y(k,166)
         mat(k,1522) = .600_r8*rxt(k,485)*y(k,166)
         mat(k,2614) = .100_r8*rxt(k,491)*y(k,222) + rxt(k,395)*y(k,231) &
                      + .500_r8*rxt(k,462)*y(k,234) + .500_r8*rxt(k,431)*y(k,236) &
                      + .920_r8*rxt(k,501)*y(k,239) + .250_r8*rxt(k,469)*y(k,244) &
                      + rxt(k,478)*y(k,246) + rxt(k,452)*y(k,262) + rxt(k,456) &
                      *y(k,263) + .340_r8*rxt(k,585)*y(k,264) + .320_r8*rxt(k,590) &
                      *y(k,265) + .250_r8*rxt(k,526)*y(k,268)
         mat(k,2834) = mat(k,2834) + .500_r8*rxt(k,460)*y(k,18) + rxt(k,502)*y(k,239) &
                      + .250_r8*rxt(k,468)*y(k,244) + rxt(k,479)*y(k,246)
         mat(k,1988) = .340_r8*rxt(k,573)*y(k,8) + rxt(k,412)*y(k,30) &
                      + .500_r8*rxt(k,441)*y(k,35) + .910_r8*rxt(k,518)*y(k,129) &
                      + .120_r8*rxt(k,471)*y(k,136) + .340_r8*rxt(k,576)*y(k,141) &
                      + .600_r8*rxt(k,485)*y(k,142)
         mat(k,627) = rxt(k,436)*y(k,259)
         mat(k,1256) = .680_r8*rxt(k,594)*y(k,259)
         mat(k,1103) = .100_r8*rxt(k,491)*y(k,155)
         mat(k,1017) = .700_r8*rxt(k,414)*y(k,231)
         mat(k,916) = rxt(k,442)*y(k,231)
         mat(k,1572) = rxt(k,425)*y(k,231) + rxt(k,498)*y(k,239) + .250_r8*rxt(k,465) &
                      *y(k,244) + rxt(k,474)*y(k,246) + .250_r8*rxt(k,523)*y(k,268)
         mat(k,1769) = rxt(k,372)*y(k,70) + rxt(k,253)*y(k,76) + rxt(k,395)*y(k,155) &
                      + .700_r8*rxt(k,414)*y(k,227) + rxt(k,442)*y(k,228) + rxt(k,425) &
                      *y(k,230) + (4.000_r8*rxt(k,392)+2.000_r8*rxt(k,393))*y(k,231) &
                      + 1.500_r8*rxt(k,499)*y(k,239) + .750_r8*rxt(k,504)*y(k,240) &
                      + .800_r8*rxt(k,513)*y(k,241) + .880_r8*rxt(k,466)*y(k,244) &
                      + 2.000_r8*rxt(k,475)*y(k,246) + .750_r8*rxt(k,578)*y(k,254) &
                      + .800_r8*rxt(k,454)*y(k,263) + .930_r8*rxt(k,583)*y(k,264) &
                      + .950_r8*rxt(k,588)*y(k,265) + .800_r8*rxt(k,524)*y(k,268)
         mat(k,660) = .500_r8*rxt(k,462)*y(k,155)
         mat(k,882) = .500_r8*rxt(k,431)*y(k,155)
         mat(k,2433) = mat(k,2433) + .450_r8*rxt(k,476)*y(k,246) + .150_r8*rxt(k,455) &
                      *y(k,263)
         mat(k,1445) = .920_r8*rxt(k,501)*y(k,155) + rxt(k,502)*y(k,157) + rxt(k,498) &
                      *y(k,230) + 1.500_r8*rxt(k,499)*y(k,231)
         mat(k,1478) = .750_r8*rxt(k,504)*y(k,231)
         mat(k,1397) = .800_r8*rxt(k,513)*y(k,231)
         mat(k,1499) = .250_r8*rxt(k,469)*y(k,155) + .250_r8*rxt(k,468)*y(k,157) &
                      + .250_r8*rxt(k,465)*y(k,230) + .880_r8*rxt(k,466)*y(k,231)
         mat(k,1540) = rxt(k,478)*y(k,155) + rxt(k,479)*y(k,157) + rxt(k,474)*y(k,230) &
                      + 2.000_r8*rxt(k,475)*y(k,231) + .450_r8*rxt(k,476)*y(k,237) &
                      + 4.000_r8*rxt(k,477)*y(k,246)
         mat(k,1243) = .750_r8*rxt(k,578)*y(k,231)
         mat(k,1895) = (rxt(k,405)+rxt(k,406))*y(k,66)
         mat(k,2201) = mat(k,2201) + .400_r8*rxt(k,489)*y(k,1) + .500_r8*rxt(k,429) &
                      *y(k,62) + rxt(k,396)*y(k,64) + .300_r8*rxt(k,397)*y(k,65) &
                      + .800_r8*rxt(k,434)*y(k,92) + .300_r8*rxt(k,509)*y(k,130) &
                      + .500_r8*rxt(k,484)*y(k,140) + rxt(k,436)*y(k,172) &
                      + .680_r8*rxt(k,594)*y(k,211)
         mat(k,904) = rxt(k,452)*y(k,155)
         mat(k,1343) = rxt(k,456)*y(k,155) + .800_r8*rxt(k,454)*y(k,231) &
                      + .150_r8*rxt(k,455)*y(k,237)
         mat(k,1307) = .340_r8*rxt(k,585)*y(k,155) + .930_r8*rxt(k,583)*y(k,231)
         mat(k,1328) = .320_r8*rxt(k,590)*y(k,155) + .950_r8*rxt(k,588)*y(k,231)
         mat(k,1375) = .250_r8*rxt(k,526)*y(k,155) + .250_r8*rxt(k,523)*y(k,230) &
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
         mat(k,682) = -(rxt(k,361)*y(k,72) + rxt(k,362)*y(k,259) + rxt(k,384)*y(k,255))
         mat(k,2252) = -rxt(k,361)*y(k,54)
         mat(k,2115) = -rxt(k,362)*y(k,54)
         mat(k,1872) = -rxt(k,384)*y(k,54)
         mat(k,152) = -(rxt(k,363)*y(k,259))
         mat(k,2044) = -rxt(k,363)*y(k,55)
         mat(k,1222) = -(rxt(k,421)*y(k,157) + rxt(k,422)*y(k,259))
         mat(k,2791) = -rxt(k,421)*y(k,56)
         mat(k,2156) = -rxt(k,422)*y(k,56)
         mat(k,768) = .800_r8*rxt(k,489)*y(k,259)
         mat(k,414) = rxt(k,460)*y(k,157)
         mat(k,307) = rxt(k,417)*y(k,259)
         mat(k,386) = .500_r8*rxt(k,418)*y(k,259)
         mat(k,1205) = .500_r8*rxt(k,441)*y(k,166)
         mat(k,1506) = .100_r8*rxt(k,485)*y(k,166)
         mat(k,2573) = .400_r8*rxt(k,491)*y(k,222) + rxt(k,416)*y(k,227) &
                      + .270_r8*rxt(k,444)*y(k,228) + rxt(k,462)*y(k,234) + rxt(k,481) &
                      *y(k,248) + rxt(k,452)*y(k,262)
         mat(k,2791) = mat(k,2791) + rxt(k,460)*y(k,18)
         mat(k,1951) = .500_r8*rxt(k,441)*y(k,35) + .100_r8*rxt(k,485)*y(k,142)
         mat(k,1096) = .400_r8*rxt(k,491)*y(k,155)
         mat(k,1011) = rxt(k,416)*y(k,155) + 3.200_r8*rxt(k,413)*y(k,227) &
                      + .800_r8*rxt(k,414)*y(k,231)
         mat(k,910) = .270_r8*rxt(k,444)*y(k,155)
         mat(k,1735) = .800_r8*rxt(k,414)*y(k,227)
         mat(k,656) = rxt(k,462)*y(k,155)
         mat(k,2390) = .200_r8*rxt(k,480)*y(k,248)
         mat(k,776) = rxt(k,481)*y(k,155) + .200_r8*rxt(k,480)*y(k,237)
         mat(k,2156) = mat(k,2156) + .800_r8*rxt(k,489)*y(k,1) + rxt(k,417)*y(k,32) &
                      + .500_r8*rxt(k,418)*y(k,33)
         mat(k,898) = rxt(k,452)*y(k,155)
         mat(k,438) = -(rxt(k,364)*y(k,72) + rxt(k,365)*y(k,259))
         mat(k,2246) = -rxt(k,364)*y(k,57)
         mat(k,2084) = -rxt(k,365)*y(k,57)
         mat(k,107) = -(rxt(k,423)*y(k,259))
         mat(k,2038) = -rxt(k,423)*y(k,58)
         mat(k,1105) = -(rxt(k,459)*y(k,259))
         mat(k,2146) = -rxt(k,459)*y(k,59)
         mat(k,767) = .800_r8*rxt(k,489)*y(k,259)
         mat(k,1072) = .520_r8*rxt(k,573)*y(k,166)
         mat(k,413) = .500_r8*rxt(k,460)*y(k,157)
         mat(k,1044) = .520_r8*rxt(k,576)*y(k,166)
         mat(k,2567) = .250_r8*rxt(k,491)*y(k,222) + .820_r8*rxt(k,444)*y(k,228) &
                      + .500_r8*rxt(k,462)*y(k,234) + .270_r8*rxt(k,585)*y(k,264) &
                      + .040_r8*rxt(k,590)*y(k,265)
         mat(k,2783) = .500_r8*rxt(k,460)*y(k,18)
         mat(k,1946) = .520_r8*rxt(k,573)*y(k,8) + .520_r8*rxt(k,576)*y(k,141)
         mat(k,1247) = .500_r8*rxt(k,594)*y(k,259)
         mat(k,1095) = .250_r8*rxt(k,491)*y(k,155)
         mat(k,909) = .820_r8*rxt(k,444)*y(k,155) + .820_r8*rxt(k,442)*y(k,231)
         mat(k,1730) = .820_r8*rxt(k,442)*y(k,228) + .150_r8*rxt(k,583)*y(k,264) &
                      + .025_r8*rxt(k,588)*y(k,265)
         mat(k,655) = .500_r8*rxt(k,462)*y(k,155)
         mat(k,2146) = mat(k,2146) + .800_r8*rxt(k,489)*y(k,1) + .500_r8*rxt(k,594) &
                      *y(k,211)
         mat(k,1295) = .270_r8*rxt(k,585)*y(k,155) + .150_r8*rxt(k,583)*y(k,231)
         mat(k,1314) = .040_r8*rxt(k,590)*y(k,155) + .025_r8*rxt(k,588)*y(k,231)
         mat(k,1415) = -(rxt(k,447)*y(k,157) + rxt(k,448)*y(k,259))
         mat(k,2805) = -rxt(k,447)*y(k,60)
         mat(k,2170) = -rxt(k,448)*y(k,60)
         mat(k,1276) = rxt(k,449)*y(k,259)
         mat(k,1404) = .880_r8*rxt(k,471)*y(k,166)
         mat(k,1509) = .500_r8*rxt(k,485)*y(k,166)
         mat(k,2586) = .170_r8*rxt(k,544)*y(k,232) + .050_r8*rxt(k,507)*y(k,240) &
                      + .250_r8*rxt(k,469)*y(k,244) + .170_r8*rxt(k,550)*y(k,247) &
                      + .400_r8*rxt(k,560)*y(k,266) + .250_r8*rxt(k,526)*y(k,268) &
                      + .540_r8*rxt(k,566)*y(k,269) + .510_r8*rxt(k,569)*y(k,271)
         mat(k,2805) = mat(k,2805) + .050_r8*rxt(k,508)*y(k,240) + .250_r8*rxt(k,468) &
                      *y(k,244) + .250_r8*rxt(k,527)*y(k,268)
         mat(k,949) = rxt(k,450)*y(k,259)
         mat(k,1962) = .880_r8*rxt(k,471)*y(k,136) + .500_r8*rxt(k,485)*y(k,142)
         mat(k,1557) = .250_r8*rxt(k,465)*y(k,244) + .250_r8*rxt(k,523)*y(k,268)
         mat(k,1747) = .240_r8*rxt(k,466)*y(k,244) + .500_r8*rxt(k,454)*y(k,263) &
                      + .100_r8*rxt(k,524)*y(k,268)
         mat(k,868) = .170_r8*rxt(k,544)*y(k,155) + .070_r8*rxt(k,543)*y(k,237)
         mat(k,2403) = .070_r8*rxt(k,543)*y(k,232) + .070_r8*rxt(k,549)*y(k,247)
         mat(k,1466) = .050_r8*rxt(k,507)*y(k,155) + .050_r8*rxt(k,508)*y(k,157)
         mat(k,1490) = .250_r8*rxt(k,469)*y(k,155) + .250_r8*rxt(k,468)*y(k,157) &
                      + .250_r8*rxt(k,465)*y(k,230) + .240_r8*rxt(k,466)*y(k,231)
         mat(k,1022) = .170_r8*rxt(k,550)*y(k,155) + .070_r8*rxt(k,549)*y(k,237)
         mat(k,2170) = mat(k,2170) + rxt(k,449)*y(k,115) + rxt(k,450)*y(k,158)
         mat(k,1337) = .500_r8*rxt(k,454)*y(k,231)
         mat(k,837) = .400_r8*rxt(k,560)*y(k,155)
         mat(k,1368) = .250_r8*rxt(k,526)*y(k,155) + .250_r8*rxt(k,527)*y(k,157) &
                      + .250_r8*rxt(k,523)*y(k,230) + .100_r8*rxt(k,524)*y(k,231)
         mat(k,860) = .540_r8*rxt(k,566)*y(k,155)
         mat(k,591) = .510_r8*rxt(k,569)*y(k,155)
         mat(k,793) = -(rxt(k,428)*y(k,259))
         mat(k,2125) = -rxt(k,428)*y(k,61)
         mat(k,1200) = .120_r8*rxt(k,441)*y(k,166)
         mat(k,1938) = .120_r8*rxt(k,441)*y(k,35)
         mat(k,1548) = .100_r8*rxt(k,425)*y(k,231) + .150_r8*rxt(k,426)*y(k,237)
         mat(k,1725) = .100_r8*rxt(k,425)*y(k,230)
         mat(k,2371) = .150_r8*rxt(k,426)*y(k,230) + .150_r8*rxt(k,476)*y(k,246)
         mat(k,1529) = .150_r8*rxt(k,476)*y(k,237)
         mat(k,675) = -(rxt(k,429)*y(k,259))
         mat(k,2114) = -rxt(k,429)*y(k,62)
         mat(k,1547) = .400_r8*rxt(k,426)*y(k,237)
         mat(k,2364) = .400_r8*rxt(k,426)*y(k,230) + .400_r8*rxt(k,476)*y(k,246)
         mat(k,1527) = .400_r8*rxt(k,476)*y(k,237)
         mat(k,430) = -(rxt(k,366)*y(k,72) + rxt(k,367)*y(k,259))
         mat(k,2245) = -rxt(k,366)*y(k,63)
         mat(k,2083) = -rxt(k,367)*y(k,63)
         mat(k,892) = -(rxt(k,396)*y(k,259))
         mat(k,2134) = -rxt(k,396)*y(k,64)
         mat(k,1009) = .300_r8*rxt(k,414)*y(k,231)
         mat(k,1726) = .300_r8*rxt(k,414)*y(k,227) + 2.000_r8*rxt(k,393)*y(k,231) &
                      + .250_r8*rxt(k,499)*y(k,239) + .250_r8*rxt(k,504)*y(k,240) &
                      + .200_r8*rxt(k,513)*y(k,241) + .250_r8*rxt(k,466)*y(k,244) &
                      + .250_r8*rxt(k,578)*y(k,254) + .500_r8*rxt(k,454)*y(k,263) &
                      + .250_r8*rxt(k,583)*y(k,264) + .250_r8*rxt(k,588)*y(k,265) &
                      + .300_r8*rxt(k,524)*y(k,268)
         mat(k,1425) = .250_r8*rxt(k,499)*y(k,231)
         mat(k,1455) = .250_r8*rxt(k,504)*y(k,231)
         mat(k,1381) = .200_r8*rxt(k,513)*y(k,231)
         mat(k,1484) = .250_r8*rxt(k,466)*y(k,231)
         mat(k,1233) = .250_r8*rxt(k,578)*y(k,231)
         mat(k,1334) = .500_r8*rxt(k,454)*y(k,231)
         mat(k,1294) = .250_r8*rxt(k,583)*y(k,231)
         mat(k,1313) = .250_r8*rxt(k,588)*y(k,231)
         mat(k,1362) = .300_r8*rxt(k,524)*y(k,231)
         mat(k,488) = -(rxt(k,397)*y(k,259))
         mat(k,2091) = -rxt(k,397)*y(k,65)
         mat(k,1723) = rxt(k,394)*y(k,237)
         mat(k,2349) = rxt(k,394)*y(k,231)
         mat(k,1658) = -(rxt(k,245)*y(k,72) + rxt(k,346)*y(k,91) + rxt(k,398)*y(k,259) &
                      + (rxt(k,404) + rxt(k,405) + rxt(k,406)) * y(k,255))
         mat(k,2270) = -rxt(k,245)*y(k,66)
         mat(k,1000) = -rxt(k,346)*y(k,66)
         mat(k,2182) = -rxt(k,398)*y(k,66)
         mat(k,1876) = -(rxt(k,404) + rxt(k,405) + rxt(k,406)) * y(k,66)
         mat(k,1211) = .100_r8*rxt(k,441)*y(k,166)
         mat(k,1971) = .100_r8*rxt(k,441)*y(k,35)
         mat(k,116) = -(rxt(k,368)*y(k,259))
         mat(k,2040) = -rxt(k,368)*y(k,67)
         mat(k,482) = -(rxt(k,303)*y(k,255) + rxt(k,369)*y(k,72) + rxt(k,370)*y(k,259))
         mat(k,1870) = -rxt(k,303)*y(k,68)
         mat(k,2247) = -rxt(k,369)*y(k,68)
         mat(k,2090) = -rxt(k,370)*y(k,68)
         mat(k,120) = -(rxt(k,371)*y(k,259))
         mat(k,2041) = -rxt(k,371)*y(k,69)
         mat(k,1172) = -((rxt(k,372) + rxt(k,373)) * y(k,231) + (rxt(k,374) + rxt(k,375) &
                      ) * y(k,237) + rxt(k,376)*y(k,155) + rxt(k,377)*y(k,157))
         mat(k,1733) = -(rxt(k,372) + rxt(k,373)) * y(k,70)
         mat(k,2389) = -(rxt(k,374) + rxt(k,375)) * y(k,70)
         mat(k,2571) = -rxt(k,376)*y(k,70)
         mat(k,2788) = -rxt(k,377)*y(k,70)
         mat(k,346) = rxt(k,359)*y(k,72) + rxt(k,360)*y(k,259)
         mat(k,2262) = rxt(k,359)*y(k,49)
         mat(k,2153) = rxt(k,360)*y(k,49)
         mat(k,393) = -(rxt(k,378)*y(k,72) + rxt(k,379)*y(k,259))
         mat(k,2244) = -rxt(k,378)*y(k,71)
         mat(k,2079) = -rxt(k,379)*y(k,71)
         mat(k,2282) = -(rxt(k,244)*y(k,53) + rxt(k,245)*y(k,66) + rxt(k,246)*y(k,95) &
                      + rxt(k,247)*y(k,97) + (rxt(k,248) + rxt(k,249)) * y(k,237) &
                      + rxt(k,250)*y(k,156) + rxt(k,252)*y(k,166) + rxt(k,259)*y(k,77) &
                      + rxt(k,268)*y(k,111) + rxt(k,294)*y(k,24) + rxt(k,352)*y(k,28) &
                      + rxt(k,354)*y(k,31) + rxt(k,356)*y(k,47) + rxt(k,359)*y(k,49) &
                      + rxt(k,361)*y(k,54) + rxt(k,364)*y(k,57) + rxt(k,366)*y(k,63) &
                      + rxt(k,369)*y(k,68) + rxt(k,419)*y(k,34) + (rxt(k,596) &
                      + rxt(k,597)) * y(k,85))
         mat(k,2639) = -rxt(k,244)*y(k,72)
         mat(k,1665) = -rxt(k,245)*y(k,72)
         mat(k,1619) = -rxt(k,246)*y(k,72)
         mat(k,703) = -rxt(k,247)*y(k,72)
         mat(k,2427) = -(rxt(k,248) + rxt(k,249)) * y(k,72)
         mat(k,2697) = -rxt(k,250)*y(k,72)
         mat(k,1982) = -rxt(k,252)*y(k,72)
         mat(k,1139) = -rxt(k,259)*y(k,72)
         mat(k,1844) = -rxt(k,268)*y(k,72)
         mat(k,990) = -rxt(k,294)*y(k,72)
         mat(k,225) = -rxt(k,352)*y(k,72)
         mat(k,295) = -rxt(k,354)*y(k,72)
         mat(k,562) = -rxt(k,356)*y(k,72)
         mat(k,349) = -rxt(k,359)*y(k,72)
         mat(k,687) = -rxt(k,361)*y(k,72)
         mat(k,443) = -rxt(k,364)*y(k,72)
         mat(k,435) = -rxt(k,366)*y(k,72)
         mat(k,486) = -rxt(k,369)*y(k,72)
         mat(k,343) = -rxt(k,419)*y(k,72)
         mat(k,1149) = -(rxt(k,596) + rxt(k,597)) * y(k,72)
         mat(k,2456) = rxt(k,289)*y(k,76)
         mat(k,225) = mat(k,225) + 5.000_r8*rxt(k,352)*y(k,72) + 3.060_r8*rxt(k,353) &
                      *y(k,259)
         mat(k,295) = mat(k,295) + 2.000_r8*rxt(k,354)*y(k,72) + 2.000_r8*rxt(k,355) &
                      *y(k,259)
         mat(k,115) = 4.000_r8*rxt(k,271)*y(k,255)
         mat(k,174) = rxt(k,272)*y(k,255)
         mat(k,139) = 2.000_r8*rxt(k,273)*y(k,255)
         mat(k,185) = 2.000_r8*rxt(k,274)*y(k,255)
         mat(k,143) = 2.000_r8*rxt(k,275)*y(k,255)
         mat(k,190) = rxt(k,276)*y(k,255)
         mat(k,147) = 2.000_r8*rxt(k,277)*y(k,255)
         mat(k,150) = rxt(k,358)*y(k,259)
         mat(k,154) = 3.000_r8*rxt(k,363)*y(k,259)
         mat(k,443) = mat(k,443) + rxt(k,365)*y(k,259)
         mat(k,118) = rxt(k,368)*y(k,259)
         mat(k,122) = 2.000_r8*rxt(k,371)*y(k,259)
         mat(k,1177) = 2.000_r8*rxt(k,376)*y(k,155) + 2.000_r8*rxt(k,377)*y(k,157) &
                      + 2.000_r8*rxt(k,372)*y(k,231) + rxt(k,375)*y(k,237)
         mat(k,398) = rxt(k,379)*y(k,259)
         mat(k,2282) = mat(k,2282) + 5.000_r8*rxt(k,352)*y(k,28) + 2.000_r8*rxt(k,354) &
                      *y(k,31)
         mat(k,2012) = rxt(k,289)*y(k,23) + (4.000_r8*rxt(k,254)+2.000_r8*rxt(k,256)) &
                      *y(k,76) + rxt(k,326)*y(k,127) + rxt(k,258)*y(k,155) &
                      + rxt(k,263)*y(k,164) + rxt(k,607)*y(k,183) + rxt(k,253) &
                      *y(k,231) + rxt(k,264)*y(k,259)
         mat(k,269) = rxt(k,351)*y(k,255)
         mat(k,265) = rxt(k,385)*y(k,255) + rxt(k,380)*y(k,259)
         mat(k,274) = rxt(k,386)*y(k,255) + rxt(k,381)*y(k,259)
         mat(k,331) = rxt(k,387)*y(k,255) + rxt(k,382)*y(k,259)
         mat(k,1820) = rxt(k,266)*y(k,164) + rxt(k,278)*y(k,255) + rxt(k,267)*y(k,259)
         mat(k,2756) = rxt(k,326)*y(k,76)
         mat(k,2608) = 2.000_r8*rxt(k,376)*y(k,70) + rxt(k,258)*y(k,76)
         mat(k,2828) = 2.000_r8*rxt(k,377)*y(k,70)
         mat(k,2503) = rxt(k,263)*y(k,76) + rxt(k,266)*y(k,103)
         mat(k,1645) = rxt(k,607)*y(k,76)
         mat(k,1765) = 2.000_r8*rxt(k,372)*y(k,70) + rxt(k,253)*y(k,76)
         mat(k,2427) = mat(k,2427) + rxt(k,375)*y(k,70)
         mat(k,1889) = 4.000_r8*rxt(k,271)*y(k,39) + rxt(k,272)*y(k,40) &
                      + 2.000_r8*rxt(k,273)*y(k,42) + 2.000_r8*rxt(k,274)*y(k,43) &
                      + 2.000_r8*rxt(k,275)*y(k,44) + rxt(k,276)*y(k,45) &
                      + 2.000_r8*rxt(k,277)*y(k,46) + rxt(k,351)*y(k,83) + rxt(k,385) &
                      *y(k,100) + rxt(k,386)*y(k,101) + rxt(k,387)*y(k,102) &
                      + rxt(k,278)*y(k,103)
         mat(k,2195) = 3.060_r8*rxt(k,353)*y(k,28) + 2.000_r8*rxt(k,355)*y(k,31) &
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
         mat(k,2237) = rxt(k,259)*y(k,77)
         mat(k,1995) = 2.000_r8*rxt(k,255)*y(k,76)
         mat(k,1133) = rxt(k,259)*y(k,72) + (rxt(k,705)+rxt(k,714)+rxt(k,723)) &
                      *y(k,103)
         mat(k,1806) = (rxt(k,705)+rxt(k,714)+rxt(k,723))*y(k,77) + (rxt(k,624) &
                       +rxt(k,695)+rxt(k,706)+rxt(k,715))*y(k,111)
         mat(k,1829) = (rxt(k,624)+rxt(k,695)+rxt(k,706)+rxt(k,715))*y(k,103)
         mat(k,1994) = 2.000_r8*rxt(k,280)*y(k,76)
         mat(k,597) = -(rxt(k,251)*y(k,259))
         mat(k,2104) = -rxt(k,251)*y(k,75)
         mat(k,2249) = rxt(k,250)*y(k,156)
         mat(k,1808) = rxt(k,640)*y(k,145)
         mat(k,404) = rxt(k,640)*y(k,103)
         mat(k,2661) = rxt(k,250)*y(k,72)
         mat(k,2009) = -(rxt(k,253)*y(k,231) + (4._r8*rxt(k,254) + 4._r8*rxt(k,255) &
                      + 4._r8*rxt(k,256) + 4._r8*rxt(k,280)) * y(k,76) + rxt(k,257) &
                      *y(k,237) + rxt(k,258)*y(k,155) + rxt(k,260)*y(k,156) + rxt(k,263) &
                      *y(k,164) + (rxt(k,264) + rxt(k,265)) * y(k,259) + (rxt(k,288) &
                      + rxt(k,289) + rxt(k,290)) * y(k,23) + (rxt(k,325) + rxt(k,326) &
                      + rxt(k,327)) * y(k,127) + rxt(k,607)*y(k,183))
         mat(k,1763) = -rxt(k,253)*y(k,76)
         mat(k,2424) = -rxt(k,257)*y(k,76)
         mat(k,2605) = -rxt(k,258)*y(k,76)
         mat(k,2694) = -rxt(k,260)*y(k,76)
         mat(k,2500) = -rxt(k,263)*y(k,76)
         mat(k,2192) = -(rxt(k,264) + rxt(k,265)) * y(k,76)
         mat(k,2453) = -(rxt(k,288) + rxt(k,289) + rxt(k,290)) * y(k,76)
         mat(k,2753) = -(rxt(k,325) + rxt(k,326) + rxt(k,327)) * y(k,76)
         mat(k,1643) = -rxt(k,607)*y(k,76)
         mat(k,2279) = rxt(k,268)*y(k,111) + rxt(k,252)*y(k,166) + rxt(k,249)*y(k,237)
         mat(k,1137) = rxt(k,261)*y(k,164)
         mat(k,1817) = rxt(k,279)*y(k,255)
         mat(k,1841) = rxt(k,268)*y(k,72) + rxt(k,269)*y(k,164) + rxt(k,270)*y(k,259)
         mat(k,2500) = mat(k,2500) + rxt(k,261)*y(k,77) + rxt(k,269)*y(k,111)
         mat(k,1979) = rxt(k,252)*y(k,72)
         mat(k,530) = rxt(k,612)*y(k,183)
         mat(k,1643) = mat(k,1643) + rxt(k,612)*y(k,168)
         mat(k,2424) = mat(k,2424) + rxt(k,249)*y(k,72)
         mat(k,1886) = rxt(k,279)*y(k,103)
         mat(k,2192) = mat(k,2192) + rxt(k,270)*y(k,111)
         mat(k,1134) = -(rxt(k,259)*y(k,72) + rxt(k,261)*y(k,164) + rxt(k,262) &
                      *y(k,259) + (rxt(k,705) + rxt(k,714) + rxt(k,723)) * y(k,103))
         mat(k,2259) = -rxt(k,259)*y(k,77)
         mat(k,2482) = -rxt(k,261)*y(k,77)
         mat(k,2149) = -rxt(k,262)*y(k,77)
         mat(k,1810) = -(rxt(k,705) + rxt(k,714) + rxt(k,723)) * y(k,77)
         mat(k,1999) = rxt(k,260)*y(k,156)
         mat(k,2674) = rxt(k,260)*y(k,76)
         mat(k,1288) = -(rxt(k,408)*y(k,259))
         mat(k,2162) = -rxt(k,408)*y(k,79)
         mat(k,1077) = .230_r8*rxt(k,573)*y(k,166)
         mat(k,2298) = rxt(k,283)*y(k,53)
         mat(k,336) = .350_r8*rxt(k,410)*y(k,259)
         mat(k,641) = .630_r8*rxt(k,412)*y(k,166)
         mat(k,1207) = .560_r8*rxt(k,441)*y(k,166)
         mat(k,2622) = rxt(k,283)*y(k,19) + rxt(k,244)*y(k,72) + rxt(k,389)*y(k,157) &
                      + rxt(k,390)*y(k,164) + rxt(k,391)*y(k,259)
         mat(k,439) = rxt(k,364)*y(k,72)
         mat(k,1414) = rxt(k,447)*y(k,157) + rxt(k,448)*y(k,259)
         mat(k,1173) = rxt(k,376)*y(k,155) + rxt(k,377)*y(k,157) + (rxt(k,372) &
                       +rxt(k,373))*y(k,231) + rxt(k,375)*y(k,237)
         mat(k,2265) = rxt(k,244)*y(k,53) + rxt(k,364)*y(k,57)
         mat(k,1584) = rxt(k,748)*y(k,260)
         mat(k,1113) = rxt(k,435)*y(k,259)
         mat(k,960) = .620_r8*rxt(k,518)*y(k,166)
         mat(k,1402) = .650_r8*rxt(k,471)*y(k,166)
         mat(k,1049) = .230_r8*rxt(k,576)*y(k,166)
         mat(k,1508) = .560_r8*rxt(k,485)*y(k,166)
         mat(k,2579) = rxt(k,376)*y(k,70) + .170_r8*rxt(k,544)*y(k,232) &
                      + .220_r8*rxt(k,469)*y(k,244) + .400_r8*rxt(k,547)*y(k,245) &
                      + .350_r8*rxt(k,550)*y(k,247) + .225_r8*rxt(k,585)*y(k,264) &
                      + .250_r8*rxt(k,526)*y(k,268)
         mat(k,2797) = rxt(k,389)*y(k,53) + rxt(k,447)*y(k,60) + rxt(k,377)*y(k,70) &
                      + .220_r8*rxt(k,468)*y(k,244) + .500_r8*rxt(k,527)*y(k,268)
         mat(k,2484) = rxt(k,390)*y(k,53) + rxt(k,601)*y(k,169)
         mat(k,1956) = .230_r8*rxt(k,573)*y(k,8) + .630_r8*rxt(k,412)*y(k,30) &
                      + .560_r8*rxt(k,441)*y(k,35) + .620_r8*rxt(k,518)*y(k,129) &
                      + .650_r8*rxt(k,471)*y(k,136) + .230_r8*rxt(k,576)*y(k,141) &
                      + .560_r8*rxt(k,485)*y(k,142)
         mat(k,422) = rxt(k,601)*y(k,164) + rxt(k,602)*y(k,259)
         mat(k,1249) = .700_r8*rxt(k,594)*y(k,259)
         mat(k,1553) = .220_r8*rxt(k,465)*y(k,244) + .250_r8*rxt(k,523)*y(k,268)
         mat(k,1740) = (rxt(k,372)+rxt(k,373))*y(k,70) + .110_r8*rxt(k,466)*y(k,244) &
                      + .125_r8*rxt(k,583)*y(k,264) + .200_r8*rxt(k,524)*y(k,268)
         mat(k,867) = .170_r8*rxt(k,544)*y(k,155) + .070_r8*rxt(k,543)*y(k,237)
         mat(k,2395) = rxt(k,375)*y(k,70) + .070_r8*rxt(k,543)*y(k,232) &
                      + .160_r8*rxt(k,546)*y(k,245) + .140_r8*rxt(k,549)*y(k,247)
         mat(k,1488) = .220_r8*rxt(k,469)*y(k,155) + .220_r8*rxt(k,468)*y(k,157) &
                      + .220_r8*rxt(k,465)*y(k,230) + .110_r8*rxt(k,466)*y(k,231)
         mat(k,823) = .400_r8*rxt(k,547)*y(k,155) + .160_r8*rxt(k,546)*y(k,237)
         mat(k,1021) = .350_r8*rxt(k,550)*y(k,155) + .140_r8*rxt(k,549)*y(k,237)
         mat(k,2162) = mat(k,2162) + .350_r8*rxt(k,410)*y(k,29) + rxt(k,391)*y(k,53) &
                      + rxt(k,448)*y(k,60) + rxt(k,435)*y(k,93) + rxt(k,602)*y(k,169) &
                      + .700_r8*rxt(k,594)*y(k,211)
         mat(k,888) = rxt(k,748)*y(k,80)
         mat(k,1298) = .225_r8*rxt(k,585)*y(k,155) + .125_r8*rxt(k,583)*y(k,231)
         mat(k,1366) = .250_r8*rxt(k,526)*y(k,155) + .500_r8*rxt(k,527)*y(k,157) &
                      + .250_r8*rxt(k,523)*y(k,230) + .200_r8*rxt(k,524)*y(k,231)
         mat(k,1585) = -(rxt(k,748)*y(k,260))
         mat(k,889) = -rxt(k,748)*y(k,80)
         mat(k,1082) = .270_r8*rxt(k,573)*y(k,166)
         mat(k,1210) = .200_r8*rxt(k,441)*y(k,166)
         mat(k,794) = rxt(k,428)*y(k,259)
         mat(k,677) = .500_r8*rxt(k,429)*y(k,259)
         mat(k,1289) = rxt(k,408)*y(k,259)
         mat(k,1284) = .800_r8*rxt(k,434)*y(k,259)
         mat(k,1114) = rxt(k,435)*y(k,259)
         mat(k,1029) = rxt(k,400)*y(k,259)
         mat(k,694) = .500_r8*rxt(k,484)*y(k,259)
         mat(k,1054) = .270_r8*rxt(k,576)*y(k,166)
         mat(k,1514) = .100_r8*rxt(k,485)*y(k,166)
         mat(k,2593) = rxt(k,427)*y(k,230) + .900_r8*rxt(k,585)*y(k,264)
         mat(k,1969) = .270_r8*rxt(k,573)*y(k,8) + .200_r8*rxt(k,441)*y(k,35) &
                      + .270_r8*rxt(k,576)*y(k,141) + .100_r8*rxt(k,485)*y(k,142)
         mat(k,1252) = 1.800_r8*rxt(k,594)*y(k,259)
         mat(k,1564) = rxt(k,427)*y(k,155) + 4.000_r8*rxt(k,424)*y(k,230) &
                      + .900_r8*rxt(k,425)*y(k,231) + rxt(k,498)*y(k,239) &
                      + 2.000_r8*rxt(k,474)*y(k,246) + rxt(k,523)*y(k,268)
         mat(k,1754) = .900_r8*rxt(k,425)*y(k,230) + rxt(k,475)*y(k,246) &
                      + .500_r8*rxt(k,583)*y(k,264)
         mat(k,2410) = .450_r8*rxt(k,476)*y(k,246)
         mat(k,1439) = rxt(k,498)*y(k,230)
         mat(k,1534) = 2.000_r8*rxt(k,474)*y(k,230) + rxt(k,475)*y(k,231) &
                      + .450_r8*rxt(k,476)*y(k,237) + 4.000_r8*rxt(k,477)*y(k,246)
         mat(k,2177) = rxt(k,428)*y(k,61) + .500_r8*rxt(k,429)*y(k,62) + rxt(k,408) &
                      *y(k,79) + .800_r8*rxt(k,434)*y(k,92) + rxt(k,435)*y(k,93) &
                      + rxt(k,400)*y(k,105) + .500_r8*rxt(k,484)*y(k,140) &
                      + 1.800_r8*rxt(k,594)*y(k,211)
         mat(k,1302) = .900_r8*rxt(k,585)*y(k,155) + .500_r8*rxt(k,583)*y(k,231)
         mat(k,1370) = rxt(k,523)*y(k,230)
         mat(k,223) = .470_r8*rxt(k,353)*y(k,259)
         mat(k,1171) = rxt(k,373)*y(k,231) + rxt(k,374)*y(k,237)
         mat(k,392) = rxt(k,378)*y(k,72) + rxt(k,379)*y(k,259)
         mat(k,2243) = rxt(k,378)*y(k,71)
         mat(k,1720) = rxt(k,373)*y(k,70)
         mat(k,2344) = rxt(k,374)*y(k,70)
         mat(k,2078) = .470_r8*rxt(k,353)*y(k,28) + rxt(k,379)*y(k,71)
         mat(k,275) = -(rxt(k,350)*y(k,255))
         mat(k,1867) = -rxt(k,350)*y(k,82)
         mat(k,172) = rxt(k,272)*y(k,255)
         mat(k,177) = rxt(k,302)*y(k,255)
         mat(k,183) = rxt(k,274)*y(k,255)
         mat(k,141) = 2.000_r8*rxt(k,275)*y(k,255)
         mat(k,187) = 2.000_r8*rxt(k,276)*y(k,255)
         mat(k,145) = rxt(k,277)*y(k,255)
         mat(k,125) = 2.000_r8*rxt(k,304)*y(k,255)
         mat(k,271) = rxt(k,386)*y(k,255) + rxt(k,381)*y(k,259)
         mat(k,326) = rxt(k,387)*y(k,255) + rxt(k,382)*y(k,259)
         mat(k,1867) = mat(k,1867) + rxt(k,272)*y(k,40) + rxt(k,302)*y(k,41) &
                      + rxt(k,274)*y(k,43) + 2.000_r8*rxt(k,275)*y(k,44) &
                      + 2.000_r8*rxt(k,276)*y(k,45) + rxt(k,277)*y(k,46) &
                      + 2.000_r8*rxt(k,304)*y(k,96) + rxt(k,386)*y(k,101) + rxt(k,387) &
                      *y(k,102)
         mat(k,2060) = rxt(k,381)*y(k,101) + rxt(k,382)*y(k,102)
         mat(k,266) = -(rxt(k,351)*y(k,255))
         mat(k,1865) = -rxt(k,351)*y(k,83)
         mat(k,137) = rxt(k,273)*y(k,255)
         mat(k,182) = rxt(k,274)*y(k,255)
         mat(k,262) = rxt(k,385)*y(k,255) + rxt(k,380)*y(k,259)
         mat(k,1865) = mat(k,1865) + rxt(k,273)*y(k,42) + rxt(k,274)*y(k,43) &
                      + rxt(k,385)*y(k,100)
         mat(k,2058) = rxt(k,380)*y(k,100)
         mat(k,234) = -(rxt(k,542)*y(k,259))
         mat(k,2052) = -rxt(k,542)*y(k,84)
         mat(k,228) = .180_r8*rxt(k,562)*y(k,259)
         mat(k,2052) = mat(k,2052) + .180_r8*rxt(k,562)*y(k,213)
         mat(k,1144) = -(rxt(k,595)*y(k,23) + (rxt(k,596) + rxt(k,597)) * y(k,72) &
                      + rxt(k,598)*y(k,127) + rxt(k,599)*y(k,157) + (rxt(k,600) &
                      + rxt(k,614)) * y(k,259))
         mat(k,2443) = -rxt(k,595)*y(k,85)
         mat(k,2260) = -(rxt(k,596) + rxt(k,597)) * y(k,85)
         mat(k,2741) = -rxt(k,598)*y(k,85)
         mat(k,2785) = -rxt(k,599)*y(k,85)
         mat(k,2150) = -(rxt(k,600) + rxt(k,614)) * y(k,85)
         mat(k,874) = rxt(k,430)*y(k,237)
         mat(k,2336) = rxt(k,430)*y(k,236)
         mat(k,998) = -(rxt(k,346)*y(k,66) + rxt(k,347)*y(k,95) + rxt(k,348)*y(k,272) &
                      + rxt(k,349)*y(k,108))
         mat(k,1654) = -rxt(k,346)*y(k,91)
         mat(k,1611) = -rxt(k,347)*y(k,91)
         mat(k,2843) = -rxt(k,348)*y(k,91)
         mat(k,2710) = -rxt(k,349)*y(k,91)
         mat(k,178) = rxt(k,302)*y(k,255)
         mat(k,188) = rxt(k,276)*y(k,255)
         mat(k,276) = 2.000_r8*rxt(k,350)*y(k,255)
         mat(k,267) = rxt(k,351)*y(k,255)
         mat(k,1873) = rxt(k,302)*y(k,41) + rxt(k,276)*y(k,45) + 2.000_r8*rxt(k,350) &
                      *y(k,82) + rxt(k,351)*y(k,83)
         mat(k,1282) = -(rxt(k,434)*y(k,259))
         mat(k,2161) = -rxt(k,434)*y(k,92)
         mat(k,665) = .700_r8*rxt(k,509)*y(k,259)
         mat(k,650) = .500_r8*rxt(k,510)*y(k,259)
         mat(k,448) = rxt(k,521)*y(k,259)
         mat(k,2578) = .050_r8*rxt(k,507)*y(k,240) + .530_r8*rxt(k,469)*y(k,244) &
                      + .225_r8*rxt(k,585)*y(k,264) + .250_r8*rxt(k,526)*y(k,268)
         mat(k,2796) = .050_r8*rxt(k,508)*y(k,240) + .530_r8*rxt(k,468)*y(k,244) &
                      + .250_r8*rxt(k,527)*y(k,268)
         mat(k,1787) = rxt(k,433)*y(k,235)
         mat(k,1552) = .530_r8*rxt(k,465)*y(k,244) + .250_r8*rxt(k,523)*y(k,268)
         mat(k,1739) = .260_r8*rxt(k,466)*y(k,244) + .125_r8*rxt(k,583)*y(k,264) &
                      + .100_r8*rxt(k,524)*y(k,268)
         mat(k,536) = rxt(k,433)*y(k,165)
         mat(k,1461) = .050_r8*rxt(k,507)*y(k,155) + .050_r8*rxt(k,508)*y(k,157)
         mat(k,1487) = .530_r8*rxt(k,469)*y(k,155) + .530_r8*rxt(k,468)*y(k,157) &
                      + .530_r8*rxt(k,465)*y(k,230) + .260_r8*rxt(k,466)*y(k,231)
         mat(k,2161) = mat(k,2161) + .700_r8*rxt(k,509)*y(k,130) + .500_r8*rxt(k,510) &
                      *y(k,131) + rxt(k,521)*y(k,146)
         mat(k,1297) = .225_r8*rxt(k,585)*y(k,155) + .125_r8*rxt(k,583)*y(k,231)
         mat(k,1365) = .250_r8*rxt(k,526)*y(k,155) + .250_r8*rxt(k,527)*y(k,157) &
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
         mat(k,1112) = -(rxt(k,435)*y(k,259))
         mat(k,2147) = -rxt(k,435)*y(k,93)
         mat(k,335) = .650_r8*rxt(k,410)*y(k,259)
         mat(k,1281) = .200_r8*rxt(k,434)*y(k,259)
         mat(k,1187) = rxt(k,522)*y(k,259)
         mat(k,2568) = rxt(k,533)*y(k,224) + .050_r8*rxt(k,507)*y(k,240) &
                      + .400_r8*rxt(k,547)*y(k,245) + .170_r8*rxt(k,550)*y(k,247) &
                      + .700_r8*rxt(k,553)*y(k,261) + .600_r8*rxt(k,560)*y(k,266) &
                      + .250_r8*rxt(k,526)*y(k,268) + .340_r8*rxt(k,566)*y(k,269) &
                      + .170_r8*rxt(k,569)*y(k,271)
         mat(k,2784) = .050_r8*rxt(k,508)*y(k,240) + .250_r8*rxt(k,527)*y(k,268)
         mat(k,576) = rxt(k,533)*y(k,155)
         mat(k,1549) = .250_r8*rxt(k,523)*y(k,268)
         mat(k,1731) = .100_r8*rxt(k,524)*y(k,268)
         mat(k,2387) = .160_r8*rxt(k,546)*y(k,245) + .070_r8*rxt(k,549)*y(k,247)
         mat(k,1458) = .050_r8*rxt(k,507)*y(k,155) + .050_r8*rxt(k,508)*y(k,157)
         mat(k,822) = .400_r8*rxt(k,547)*y(k,155) + .160_r8*rxt(k,546)*y(k,237)
         mat(k,1020) = .170_r8*rxt(k,550)*y(k,155) + .070_r8*rxt(k,549)*y(k,237)
         mat(k,2147) = mat(k,2147) + .650_r8*rxt(k,410)*y(k,29) + .200_r8*rxt(k,434) &
                      *y(k,92) + rxt(k,522)*y(k,147)
         mat(k,516) = .700_r8*rxt(k,553)*y(k,155)
         mat(k,835) = .600_r8*rxt(k,560)*y(k,155)
         mat(k,1363) = .250_r8*rxt(k,526)*y(k,155) + .250_r8*rxt(k,527)*y(k,157) &
                      + .250_r8*rxt(k,523)*y(k,230) + .100_r8*rxt(k,524)*y(k,231)
         mat(k,858) = .340_r8*rxt(k,566)*y(k,155)
         mat(k,590) = .170_r8*rxt(k,569)*y(k,155)
         mat(k,1908) = -((rxt(k,202) + rxt(k,203) + rxt(k,204)) * y(k,237) + rxt(k,205) &
                      *y(k,165) + rxt(k,208)*y(k,166))
         mat(k,2422) = -(rxt(k,202) + rxt(k,203) + rxt(k,204)) * y(k,94)
         mat(k,1793) = -rxt(k,205)*y(k,94)
         mat(k,1977) = -rxt(k,208)*y(k,94)
         mat(k,2634) = rxt(k,391)*y(k,259)
         mat(k,1663) = rxt(k,405)*y(k,255)
         mat(k,2277) = rxt(k,246)*y(k,95)
         mat(k,1002) = rxt(k,347)*y(k,95)
         mat(k,1617) = rxt(k,246)*y(k,72) + rxt(k,347)*y(k,91) + rxt(k,200)*y(k,164) &
                      + rxt(k,183)*y(k,255) + rxt(k,209)*y(k,259)
         mat(k,1602) = rxt(k,306)*y(k,255)
         mat(k,1816) = rxt(k,279)*y(k,255)
         mat(k,1128) = rxt(k,232)*y(k,259)
         mat(k,2498) = rxt(k,200)*y(k,95) + rxt(k,212)*y(k,259)
         mat(k,424) = rxt(k,602)*y(k,259)
         mat(k,845) = rxt(k,608)*y(k,259)
         mat(k,1641) = rxt(k,613)*y(k,259)
         mat(k,1884) = rxt(k,405)*y(k,66) + rxt(k,183)*y(k,95) + rxt(k,306)*y(k,99) &
                      + rxt(k,279)*y(k,103)
         mat(k,2190) = rxt(k,391)*y(k,53) + rxt(k,209)*y(k,95) + rxt(k,232)*y(k,143) &
                      + rxt(k,212)*y(k,164) + rxt(k,602)*y(k,169) + rxt(k,608) &
                      *y(k,181) + rxt(k,613)*y(k,183)
         mat(k,1612) = -(rxt(k,183)*y(k,255) + rxt(k,200)*y(k,164) + rxt(k,209) &
                      *y(k,259) + rxt(k,246)*y(k,72) + rxt(k,347)*y(k,91))
         mat(k,1875) = -rxt(k,183)*y(k,95)
         mat(k,2487) = -rxt(k,200)*y(k,95)
         mat(k,2179) = -rxt(k,209)*y(k,95)
         mat(k,2268) = -rxt(k,246)*y(k,95)
         mat(k,999) = -rxt(k,347)*y(k,95)
         mat(k,1657) = rxt(k,406)*y(k,255)
         mat(k,1901) = rxt(k,202)*y(k,237)
         mat(k,2412) = rxt(k,202)*y(k,94)
         mat(k,1875) = mat(k,1875) + rxt(k,406)*y(k,66)
         mat(k,124) = -(rxt(k,304)*y(k,255))
         mat(k,1854) = -rxt(k,304)*y(k,96)
         mat(k,700) = -(rxt(k,201)*y(k,164) + rxt(k,210)*y(k,259) + rxt(k,247)*y(k,72))
         mat(k,2473) = -rxt(k,201)*y(k,97)
         mat(k,2117) = -rxt(k,210)*y(k,97)
         mat(k,2253) = -rxt(k,247)*y(k,97)
         mat(k,2365) = 2.000_r8*rxt(k,216)*y(k,237)
         mat(k,2117) = mat(k,2117) + 2.000_r8*rxt(k,215)*y(k,259)
         mat(k,301) = rxt(k,615)*y(k,272)
         mat(k,2840) = rxt(k,615)*y(k,185)
         mat(k,1597) = -(rxt(k,299)*y(k,164) + rxt(k,300)*y(k,259) + (rxt(k,305) &
                      + rxt(k,306)) * y(k,255) + (rxt(k,622) + rxt(k,699) + rxt(k,707) &
                      + rxt(k,716)) * y(k,111) + (rxt(k,623) + rxt(k,697) + rxt(k,710) &
                      + rxt(k,719)) * y(k,110) + (rxt(k,630) + rxt(k,726) + rxt(k,730) &
                      + rxt(k,734)) * y(k,112))
         mat(k,2486) = -rxt(k,299)*y(k,99)
         mat(k,2178) = -rxt(k,300)*y(k,99)
         mat(k,1874) = -(rxt(k,305) + rxt(k,306)) * y(k,99)
         mat(k,1833) = -(rxt(k,622) + rxt(k,699) + rxt(k,707) + rxt(k,716)) * y(k,99)
         mat(k,1700) = -(rxt(k,623) + rxt(k,697) + rxt(k,710) + rxt(k,719)) * y(k,99)
         mat(k,1677) = -(rxt(k,630) + rxt(k,726) + rxt(k,730) + rxt(k,734)) * y(k,99)
         mat(k,2300) = rxt(k,283)*y(k,53) + rxt(k,284)*y(k,237)
         mat(k,2624) = rxt(k,283)*y(k,19)
         mat(k,2411) = rxt(k,284)*y(k,19)
         mat(k,261) = -(rxt(k,380)*y(k,259) + rxt(k,385)*y(k,255))
         mat(k,2057) = -rxt(k,380)*y(k,100)
         mat(k,1864) = -rxt(k,385)*y(k,100)
         mat(k,270) = -(rxt(k,381)*y(k,259) + rxt(k,386)*y(k,255))
         mat(k,2059) = -rxt(k,381)*y(k,101)
         mat(k,1866) = -rxt(k,386)*y(k,101)
         mat(k,327) = -(rxt(k,382)*y(k,259) + rxt(k,387)*y(k,255))
         mat(k,2069) = -rxt(k,382)*y(k,102)
         mat(k,1869) = -rxt(k,387)*y(k,102)
         mat(k,1813) = -(rxt(k,266)*y(k,164) + rxt(k,267)*y(k,259) + (rxt(k,278) &
                      + rxt(k,279)) * y(k,255) + (rxt(k,624) + rxt(k,695) + rxt(k,706) &
                      + rxt(k,715)) * y(k,111) + (rxt(k,625) + rxt(k,696) + rxt(k,709) &
                      + rxt(k,718)) * y(k,110) + (rxt(k,629) + rxt(k,725) + rxt(k,729) &
                      + rxt(k,733)) * y(k,112) + rxt(k,640)*y(k,145) + (rxt(k,705) &
                      + rxt(k,714) + rxt(k,723)) * y(k,77))
         mat(k,2495) = -rxt(k,266)*y(k,103)
         mat(k,2187) = -rxt(k,267)*y(k,103)
         mat(k,1881) = -(rxt(k,278) + rxt(k,279)) * y(k,103)
         mat(k,1837) = -(rxt(k,624) + rxt(k,695) + rxt(k,706) + rxt(k,715)) * y(k,103)
         mat(k,1704) = -(rxt(k,625) + rxt(k,696) + rxt(k,709) + rxt(k,718)) * y(k,103)
         mat(k,1681) = -(rxt(k,629) + rxt(k,725) + rxt(k,729) + rxt(k,733)) * y(k,103)
         mat(k,405) = -rxt(k,640)*y(k,103)
         mat(k,1135) = -(rxt(k,705) + rxt(k,714) + rxt(k,723)) * y(k,103)
         mat(k,293) = rxt(k,354)*y(k,72)
         mat(k,341) = rxt(k,419)*y(k,72)
         mat(k,559) = rxt(k,356)*y(k,72)
         mat(k,347) = rxt(k,359)*y(k,72)
         mat(k,2631) = rxt(k,244)*y(k,72)
         mat(k,684) = rxt(k,361)*y(k,72)
         mat(k,441) = 2.000_r8*rxt(k,364)*y(k,72)
         mat(k,432) = rxt(k,366)*y(k,72)
         mat(k,1661) = rxt(k,245)*y(k,72)
         mat(k,483) = rxt(k,369)*y(k,72)
         mat(k,396) = rxt(k,378)*y(k,72)
         mat(k,2274) = rxt(k,354)*y(k,31) + rxt(k,419)*y(k,34) + rxt(k,356)*y(k,47) &
                      + rxt(k,359)*y(k,49) + rxt(k,244)*y(k,53) + rxt(k,361)*y(k,54) &
                      + 2.000_r8*rxt(k,364)*y(k,57) + rxt(k,366)*y(k,63) + rxt(k,245) &
                      *y(k,66) + rxt(k,369)*y(k,68) + rxt(k,378)*y(k,71) + rxt(k,597) &
                      *y(k,85) + rxt(k,246)*y(k,95) + rxt(k,247)*y(k,97) + rxt(k,268) &
                      *y(k,111) + rxt(k,248)*y(k,237)
         mat(k,2004) = rxt(k,265)*y(k,259)
         mat(k,1146) = rxt(k,597)*y(k,72)
         mat(k,1615) = rxt(k,246)*y(k,72)
         mat(k,701) = rxt(k,247)*y(k,72)
         mat(k,1837) = mat(k,1837) + rxt(k,268)*y(k,72)
         mat(k,2419) = rxt(k,248)*y(k,72)
         mat(k,2187) = mat(k,2187) + rxt(k,265)*y(k,76)
         mat(k,214) = -(rxt(k,399)*y(k,259) + rxt(k,407)*y(k,255))
         mat(k,2049) = -rxt(k,399)*y(k,104)
         mat(k,1863) = -rxt(k,407)*y(k,104)
         mat(k,1028) = -(rxt(k,400)*y(k,259))
         mat(k,2142) = -rxt(k,400)*y(k,105)
         mat(k,1069) = .050_r8*rxt(k,573)*y(k,166)
         mat(k,334) = .350_r8*rxt(k,410)*y(k,259)
         mat(k,640) = .370_r8*rxt(k,412)*y(k,166)
         mat(k,1203) = .120_r8*rxt(k,441)*y(k,166)
         mat(k,958) = .110_r8*rxt(k,518)*y(k,166)
         mat(k,1401) = .330_r8*rxt(k,471)*y(k,166)
         mat(k,1041) = .050_r8*rxt(k,576)*y(k,166)
         mat(k,1504) = .120_r8*rxt(k,485)*y(k,166)
         mat(k,2565) = rxt(k,403)*y(k,238)
         mat(k,1942) = .050_r8*rxt(k,573)*y(k,8) + .370_r8*rxt(k,412)*y(k,30) &
                      + .120_r8*rxt(k,441)*y(k,35) + .110_r8*rxt(k,518)*y(k,129) &
                      + .330_r8*rxt(k,471)*y(k,136) + .050_r8*rxt(k,576)*y(k,141) &
                      + .120_r8*rxt(k,485)*y(k,142)
         mat(k,2384) = rxt(k,401)*y(k,238)
         mat(k,509) = rxt(k,403)*y(k,155) + rxt(k,401)*y(k,237)
         mat(k,2142) = mat(k,2142) + .350_r8*rxt(k,410)*y(k,29)
         mat(k,1653) = rxt(k,346)*y(k,91)
         mat(k,997) = rxt(k,346)*y(k,66) + rxt(k,347)*y(k,95) + rxt(k,349)*y(k,108) &
                      + rxt(k,348)*y(k,272)
         mat(k,1610) = rxt(k,347)*y(k,91)
         mat(k,2709) = rxt(k,349)*y(k,91)
         mat(k,2842) = rxt(k,348)*y(k,91)
         mat(k,1348) = -(rxt(k,307)*y(k,157) + rxt(k,335)*y(k,259) + (rxt(k,626) &
                      + rxt(k,700) + rxt(k,708) + rxt(k,717)) * y(k,111) + (rxt(k,627) &
                      + rxt(k,698) + rxt(k,711) + rxt(k,720)) * y(k,110) + (rxt(k,631) &
                      + rxt(k,727) + rxt(k,731) + rxt(k,735)) * y(k,112))
         mat(k,2801) = -rxt(k,307)*y(k,107)
         mat(k,2166) = -rxt(k,335)*y(k,107)
         mat(k,1832) = -(rxt(k,626) + rxt(k,700) + rxt(k,708) + rxt(k,717)) * y(k,107)
         mat(k,1699) = -(rxt(k,627) + rxt(k,698) + rxt(k,711) + rxt(k,720)) * y(k,107)
         mat(k,1676) = -(rxt(k,631) + rxt(k,727) + rxt(k,731) + rxt(k,735)) * y(k,107)
         mat(k,2212) = rxt(k,313)*y(k,237)
         mat(k,2399) = rxt(k,313)*y(k,117)
         mat(k,2731) = -(rxt(k,241)*y(k,259) + rxt(k,349)*y(k,91))
         mat(k,2203) = -rxt(k,241)*y(k,108)
         mat(k,1004) = -rxt(k,349)*y(k,108)
         mat(k,2647) = rxt(k,389)*y(k,157)
         mat(k,1228) = rxt(k,421)*y(k,157)
         mat(k,1420) = rxt(k,447)*y(k,157)
         mat(k,1142) = (rxt(k,705)+rxt(k,714)+rxt(k,723))*y(k,103)
         mat(k,1153) = rxt(k,599)*y(k,157)
         mat(k,1825) = (rxt(k,705)+rxt(k,714)+rxt(k,723))*y(k,77) + rxt(k,640) &
                      *y(k,145)
         mat(k,1358) = rxt(k,307)*y(k,157)
         mat(k,1692) = rxt(k,337)*y(k,157)
         mat(k,409) = rxt(k,640)*y(k,103)
         mat(k,2705) = rxt(k,240)*y(k,259)
         mat(k,2836) = rxt(k,389)*y(k,53) + rxt(k,421)*y(k,56) + rxt(k,447)*y(k,60) &
                      + rxt(k,599)*y(k,85) + rxt(k,307)*y(k,107) + rxt(k,337)*y(k,112)
         mat(k,2203) = mat(k,2203) + rxt(k,240)*y(k,156)
         mat(k,551) = -(rxt(k,217)*y(k,259))
         mat(k,2099) = -rxt(k,217)*y(k,109)
         mat(k,2660) = rxt(k,238)*y(k,237)
         mat(k,2358) = rxt(k,238)*y(k,156)
         mat(k,1703) = -(rxt(k,301)*y(k,164) + (rxt(k,623) + rxt(k,697) + rxt(k,710) &
                      + rxt(k,719)) * y(k,99) + (rxt(k,625) + rxt(k,696) + rxt(k,709) &
                      + rxt(k,718)) * y(k,103) + (rxt(k,627) + rxt(k,698) + rxt(k,711) &
                      + rxt(k,720)) * y(k,107))
         mat(k,2492) = -rxt(k,301)*y(k,110)
         mat(k,1599) = -(rxt(k,623) + rxt(k,697) + rxt(k,710) + rxt(k,719)) * y(k,110)
         mat(k,1812) = -(rxt(k,625) + rxt(k,696) + rxt(k,709) + rxt(k,718)) * y(k,110)
         mat(k,1351) = -(rxt(k,627) + rxt(k,698) + rxt(k,711) + rxt(k,720)) * y(k,110)
         mat(k,541) = rxt(k,282)*y(k,259)
         mat(k,2446) = rxt(k,291)*y(k,237)
         mat(k,2416) = rxt(k,291)*y(k,23)
         mat(k,2184) = rxt(k,282)*y(k,20)
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
         mat(k,1838) = -(rxt(k,268)*y(k,72) + rxt(k,269)*y(k,164) + rxt(k,270) &
                      *y(k,259) + (rxt(k,622) + rxt(k,699) + rxt(k,707) + rxt(k,716) &
                      ) * y(k,99) + (rxt(k,624) + rxt(k,695) + rxt(k,706) + rxt(k,715) &
                      ) * y(k,103) + (rxt(k,626) + rxt(k,700) + rxt(k,708) + rxt(k,717) &
                      ) * y(k,107))
         mat(k,2275) = -rxt(k,268)*y(k,111)
         mat(k,2496) = -rxt(k,269)*y(k,111)
         mat(k,2188) = -rxt(k,270)*y(k,111)
         mat(k,1600) = -(rxt(k,622) + rxt(k,699) + rxt(k,707) + rxt(k,716)) * y(k,111)
         mat(k,1814) = -(rxt(k,624) + rxt(k,695) + rxt(k,706) + rxt(k,715)) * y(k,111)
         mat(k,1352) = -(rxt(k,626) + rxt(k,700) + rxt(k,708) + rxt(k,717)) * y(k,111)
         mat(k,1176) = rxt(k,375)*y(k,237)
         mat(k,598) = rxt(k,251)*y(k,259)
         mat(k,2005) = rxt(k,257)*y(k,237)
         mat(k,1136) = rxt(k,262)*y(k,259)
         mat(k,2420) = rxt(k,375)*y(k,70) + rxt(k,257)*y(k,76)
         mat(k,2188) = mat(k,2188) + rxt(k,251)*y(k,75) + rxt(k,262)*y(k,77)
         mat(k,1679) = -(rxt(k,308)*y(k,259) + rxt(k,337)*y(k,157) + (rxt(k,629) &
                      + rxt(k,725) + rxt(k,729) + rxt(k,733)) * y(k,103) + (rxt(k,630) &
                      + rxt(k,726) + rxt(k,730) + rxt(k,734)) * y(k,99) + (rxt(k,631) &
                      + rxt(k,727) + rxt(k,731) + rxt(k,735)) * y(k,107))
         mat(k,2183) = -rxt(k,308)*y(k,112)
         mat(k,2816) = -rxt(k,337)*y(k,112)
         mat(k,1811) = -(rxt(k,629) + rxt(k,725) + rxt(k,729) + rxt(k,733)) * y(k,112)
         mat(k,1598) = -(rxt(k,630) + rxt(k,726) + rxt(k,730) + rxt(k,734)) * y(k,112)
         mat(k,1350) = -(rxt(k,631) + rxt(k,727) + rxt(k,731) + rxt(k,735)) * y(k,112)
         mat(k,1626) = rxt(k,311)*y(k,259)
         mat(k,2745) = rxt(k,328)*y(k,237)
         mat(k,2415) = rxt(k,328)*y(k,127)
         mat(k,2183) = mat(k,2183) + rxt(k,311)*y(k,118)
         mat(k,1261) = -(rxt(k,464)*y(k,259))
         mat(k,2159) = -rxt(k,464)*y(k,113)
         mat(k,663) = .300_r8*rxt(k,509)*y(k,259)
         mat(k,648) = .500_r8*rxt(k,510)*y(k,259)
         mat(k,2576) = rxt(k,463)*y(k,234) + rxt(k,470)*y(k,244)
         mat(k,657) = rxt(k,463)*y(k,155)
         mat(k,1485) = rxt(k,470)*y(k,155)
         mat(k,2159) = mat(k,2159) + .300_r8*rxt(k,509)*y(k,130) + .500_r8*rxt(k,510) &
                      *y(k,131)
         mat(k,278) = -(rxt(k,495)*y(k,259))
         mat(k,2061) = -rxt(k,495)*y(k,114)
         mat(k,1275) = -(rxt(k,449)*y(k,259))
         mat(k,2160) = -rxt(k,449)*y(k,115)
         mat(k,664) = .700_r8*rxt(k,509)*y(k,259)
         mat(k,649) = .500_r8*rxt(k,510)*y(k,259)
         mat(k,692) = .500_r8*rxt(k,484)*y(k,259)
         mat(k,2577) = .050_r8*rxt(k,507)*y(k,240) + .220_r8*rxt(k,469)*y(k,244) &
                      + .250_r8*rxt(k,526)*y(k,268)
         mat(k,2795) = .050_r8*rxt(k,508)*y(k,240) + .220_r8*rxt(k,468)*y(k,244) &
                      + .250_r8*rxt(k,527)*y(k,268)
         mat(k,633) = .500_r8*rxt(k,453)*y(k,259)
         mat(k,1551) = .220_r8*rxt(k,465)*y(k,244) + .250_r8*rxt(k,523)*y(k,268)
         mat(k,1738) = .230_r8*rxt(k,466)*y(k,244) + .200_r8*rxt(k,454)*y(k,263) &
                      + .100_r8*rxt(k,524)*y(k,268)
         mat(k,1460) = .050_r8*rxt(k,507)*y(k,155) + .050_r8*rxt(k,508)*y(k,157)
         mat(k,1486) = .220_r8*rxt(k,469)*y(k,155) + .220_r8*rxt(k,468)*y(k,157) &
                      + .220_r8*rxt(k,465)*y(k,230) + .230_r8*rxt(k,466)*y(k,231)
         mat(k,2160) = mat(k,2160) + .700_r8*rxt(k,509)*y(k,130) + .500_r8*rxt(k,510) &
                      *y(k,131) + .500_r8*rxt(k,484)*y(k,140) + .500_r8*rxt(k,453) &
                      *y(k,179)
         mat(k,1335) = .200_r8*rxt(k,454)*y(k,231)
         mat(k,1364) = .250_r8*rxt(k,526)*y(k,155) + .250_r8*rxt(k,527)*y(k,157) &
                      + .250_r8*rxt(k,523)*y(k,230) + .100_r8*rxt(k,524)*y(k,231)
         mat(k,400) = -(rxt(k,496)*y(k,259))
         mat(k,2080) = -rxt(k,496)*y(k,116)
         mat(k,2532) = .870_r8*rxt(k,507)*y(k,240)
         mat(k,2770) = .950_r8*rxt(k,508)*y(k,240)
         mat(k,1545) = rxt(k,503)*y(k,240)
         mat(k,1721) = .750_r8*rxt(k,504)*y(k,240)
         mat(k,1451) = .870_r8*rxt(k,507)*y(k,155) + .950_r8*rxt(k,508)*y(k,157) &
                      + rxt(k,503)*y(k,230) + .750_r8*rxt(k,504)*y(k,231)
         mat(k,2224) = -(rxt(k,312)*y(k,23) + rxt(k,313)*y(k,237) + rxt(k,314) &
                      *y(k,128) + rxt(k,316)*y(k,156) + rxt(k,318)*y(k,157) + rxt(k,320) &
                      *y(k,155) + rxt(k,321)*y(k,166))
         mat(k,2455) = -rxt(k,312)*y(k,117)
         mat(k,2426) = -rxt(k,313)*y(k,117)
         mat(k,978) = -rxt(k,314)*y(k,117)
         mat(k,2696) = -rxt(k,316)*y(k,117)
         mat(k,2827) = -rxt(k,318)*y(k,117)
         mat(k,2607) = -rxt(k,320)*y(k,117)
         mat(k,1981) = -rxt(k,321)*y(k,117)
         mat(k,2311) = rxt(k,322)*y(k,127)
         mat(k,2455) = mat(k,2455) + rxt(k,323)*y(k,127)
         mat(k,434) = rxt(k,366)*y(k,72) + rxt(k,367)*y(k,259)
         mat(k,2281) = rxt(k,366)*y(k,63)
         mat(k,2011) = (rxt(k,325)+rxt(k,326))*y(k,127)
         mat(k,1148) = rxt(k,598)*y(k,127)
         mat(k,1355) = rxt(k,307)*y(k,157) + rxt(k,335)*y(k,259)
         mat(k,1628) = rxt(k,309)*y(k,157) + rxt(k,310)*y(k,164) + rxt(k,311)*y(k,259)
         mat(k,2755) = rxt(k,322)*y(k,19) + rxt(k,323)*y(k,23) + (rxt(k,325) &
                       +rxt(k,326))*y(k,76) + rxt(k,598)*y(k,85) + 2.000_r8*rxt(k,341) &
                      *y(k,127) + rxt(k,329)*y(k,155) + rxt(k,332)*y(k,164) &
                      + rxt(k,334)*y(k,259)
         mat(k,2607) = mat(k,2607) + rxt(k,329)*y(k,127)
         mat(k,2827) = mat(k,2827) + rxt(k,307)*y(k,107) + rxt(k,309)*y(k,118)
         mat(k,2502) = rxt(k,310)*y(k,118) + rxt(k,332)*y(k,127)
         mat(k,2194) = rxt(k,367)*y(k,63) + rxt(k,335)*y(k,107) + rxt(k,311)*y(k,118) &
                      + rxt(k,334)*y(k,127)
         mat(k,1625) = -(rxt(k,309)*y(k,157) + rxt(k,310)*y(k,164) + rxt(k,311) &
                      *y(k,259))
         mat(k,2813) = -rxt(k,309)*y(k,118)
         mat(k,2488) = -rxt(k,310)*y(k,118)
         mat(k,2180) = -rxt(k,311)*y(k,118)
         mat(k,1349) = (rxt(k,631)+rxt(k,727)+rxt(k,731)+rxt(k,735))*y(k,112)
         mat(k,1678) = (rxt(k,631)+rxt(k,727)+rxt(k,731)+rxt(k,735))*y(k,107)
         mat(k,2213) = rxt(k,314)*y(k,128)
         mat(k,219) = 2.000_r8*rxt(k,319)*y(k,125)
         mat(k,320) = 2.000_r8*rxt(k,315)*y(k,126)
         mat(k,976) = rxt(k,314)*y(k,117)
         mat(k,2735) = 2.000_r8*rxt(k,342)*y(k,127)
         mat(k,2736) = rxt(k,344)*y(k,170)
         mat(k,708) = rxt(k,344)*y(k,127)
         mat(k,707) = 2.000_r8*rxt(k,345)*y(k,170)
         mat(k,1594) = (rxt(k,630)+rxt(k,726)+rxt(k,730)+rxt(k,734))*y(k,112)
         mat(k,1346) = (rxt(k,627)+rxt(k,698)+rxt(k,711)+rxt(k,720))*y(k,110)
         mat(k,1696) = (rxt(k,627)+rxt(k,698)+rxt(k,711)+rxt(k,720))*y(k,107)
         mat(k,1674) = (rxt(k,630)+rxt(k,726)+rxt(k,730)+rxt(k,734))*y(k,99)
         mat(k,1997) = rxt(k,327)*y(k,127)
         mat(k,1807) = (rxt(k,629)+rxt(k,725)+rxt(k,729)+rxt(k,733))*y(k,112)
         mat(k,1347) = (rxt(k,626)+rxt(k,700)+rxt(k,708)+rxt(k,717))*y(k,111)
         mat(k,1830) = (rxt(k,626)+rxt(k,700)+rxt(k,708)+rxt(k,717))*y(k,107)
         mat(k,1675) = (rxt(k,629)+rxt(k,725)+rxt(k,729)+rxt(k,733))*y(k,103)
         mat(k,2738) = rxt(k,327)*y(k,76)
         mat(k,165) = -(rxt(k,497)*y(k,259))
         mat(k,2045) = -rxt(k,497)*y(k,124)
         mat(k,798) = .600_r8*rxt(k,520)*y(k,259)
         mat(k,2045) = mat(k,2045) + .600_r8*rxt(k,520)*y(k,133)
         mat(k,218) = -(4._r8*rxt(k,319)*y(k,125))
         mat(k,2207) = rxt(k,320)*y(k,155)
         mat(k,2527) = rxt(k,320)*y(k,117)
         mat(k,317) = -(4._r8*rxt(k,315)*y(k,126))
         mat(k,2208) = rxt(k,316)*y(k,156)
         mat(k,2653) = rxt(k,316)*y(k,117)
         mat(k,2765) = -(rxt(k,322)*y(k,19) + (rxt(k,323) + rxt(k,324)) * y(k,23) &
                      + (rxt(k,325) + rxt(k,326) + rxt(k,327)) * y(k,76) + rxt(k,328) &
                      *y(k,237) + rxt(k,329)*y(k,155) + rxt(k,330)*y(k,156) + rxt(k,331) &
                      *y(k,157) + rxt(k,332)*y(k,164) + rxt(k,333)*y(k,166) + rxt(k,334) &
                      *y(k,259) + (4._r8*rxt(k,341) + 4._r8*rxt(k,342)) * y(k,127) &
                      + rxt(k,344)*y(k,170) + rxt(k,598)*y(k,85))
         mat(k,2321) = -rxt(k,322)*y(k,127)
         mat(k,2465) = -(rxt(k,323) + rxt(k,324)) * y(k,127)
         mat(k,2021) = -(rxt(k,325) + rxt(k,326) + rxt(k,327)) * y(k,127)
         mat(k,2436) = -rxt(k,328)*y(k,127)
         mat(k,2617) = -rxt(k,329)*y(k,127)
         mat(k,2706) = -rxt(k,330)*y(k,127)
         mat(k,2837) = -rxt(k,331)*y(k,127)
         mat(k,2512) = -rxt(k,332)*y(k,127)
         mat(k,1991) = -rxt(k,333)*y(k,127)
         mat(k,2204) = -rxt(k,334)*y(k,127)
         mat(k,714) = -rxt(k,344)*y(k,127)
         mat(k,1154) = -rxt(k,598)*y(k,127)
         mat(k,2465) = mat(k,2465) + rxt(k,312)*y(k,117)
         mat(k,1693) = rxt(k,337)*y(k,157) + rxt(k,308)*y(k,259)
         mat(k,2234) = rxt(k,312)*y(k,23) + rxt(k,318)*y(k,157) + rxt(k,321)*y(k,166)
         mat(k,1634) = rxt(k,310)*y(k,164)
         mat(k,2617) = mat(k,2617) + rxt(k,336)*y(k,170)
         mat(k,2837) = mat(k,2837) + rxt(k,337)*y(k,112) + rxt(k,318)*y(k,117)
         mat(k,2512) = mat(k,2512) + rxt(k,310)*y(k,118)
         mat(k,1991) = mat(k,1991) + rxt(k,321)*y(k,117)
         mat(k,714) = mat(k,714) + rxt(k,336)*y(k,155)
         mat(k,2204) = mat(k,2204) + rxt(k,308)*y(k,112)
         mat(k,975) = -(rxt(k,314)*y(k,117))
         mat(k,2211) = -rxt(k,314)*y(k,128)
         mat(k,1624) = rxt(k,309)*y(k,157)
         mat(k,2740) = rxt(k,330)*y(k,156)
         mat(k,2669) = rxt(k,330)*y(k,127)
         mat(k,2778) = rxt(k,309)*y(k,118)
         mat(k,957) = -(rxt(k,511)*y(k,157) + rxt(k,518)*y(k,166) + rxt(k,519) &
                      *y(k,259))
         mat(k,2777) = -rxt(k,511)*y(k,129)
         mat(k,1941) = -rxt(k,518)*y(k,129)
         mat(k,2138) = -rxt(k,519)*y(k,129)
         mat(k,662) = -(rxt(k,509)*y(k,259))
         mat(k,2112) = -rxt(k,509)*y(k,130)
         mat(k,2545) = .080_r8*rxt(k,501)*y(k,239)
         mat(k,1423) = .080_r8*rxt(k,501)*y(k,155)
         mat(k,646) = -(rxt(k,510)*y(k,259))
         mat(k,2110) = -rxt(k,510)*y(k,131)
         mat(k,2543) = .080_r8*rxt(k,507)*y(k,240)
         mat(k,1452) = .080_r8*rxt(k,507)*y(k,155)
         mat(k,521) = -(rxt(k,517)*y(k,259))
         mat(k,2095) = -rxt(k,517)*y(k,132)
         mat(k,2354) = rxt(k,514)*y(k,241)
         mat(k,1379) = rxt(k,514)*y(k,237)
         mat(k,799) = -(rxt(k,520)*y(k,259))
         mat(k,2126) = -rxt(k,520)*y(k,133)
         mat(k,2372) = rxt(k,500)*y(k,239) + rxt(k,505)*y(k,240)
         mat(k,1424) = rxt(k,500)*y(k,237)
         mat(k,1454) = rxt(k,505)*y(k,237)
         mat(k,83) = -(rxt(k,680)*y(k,259))
         mat(k,2034) = -rxt(k,680)*y(k,134)
         mat(k,1403) = -(rxt(k,471)*y(k,166) + rxt(k,472)*y(k,259))
         mat(k,1961) = -rxt(k,471)*y(k,136)
         mat(k,2169) = -rxt(k,472)*y(k,136)
         mat(k,962) = .300_r8*rxt(k,518)*y(k,166)
         mat(k,2585) = .360_r8*rxt(k,501)*y(k,239)
         mat(k,2804) = .400_r8*rxt(k,502)*y(k,239)
         mat(k,1961) = mat(k,1961) + .300_r8*rxt(k,518)*y(k,129)
         mat(k,1556) = .390_r8*rxt(k,498)*y(k,239)
         mat(k,1746) = .310_r8*rxt(k,499)*y(k,239)
         mat(k,1432) = .360_r8*rxt(k,501)*y(k,155) + .400_r8*rxt(k,502)*y(k,157) &
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
         mat(k,351) = -(rxt(k,473)*y(k,259))
         mat(k,2073) = -rxt(k,473)*y(k,137)
         mat(k,2339) = rxt(k,467)*y(k,244)
         mat(k,1483) = rxt(k,467)*y(k,237)
         mat(k,602) = -(rxt(k,482)*y(k,259))
         mat(k,2105) = -rxt(k,482)*y(k,138)
         mat(k,2541) = .800_r8*rxt(k,491)*y(k,222)
         mat(k,1089) = .800_r8*rxt(k,491)*y(k,155)
         mat(k,356) = -(rxt(k,483)*y(k,259))
         mat(k,2074) = -rxt(k,483)*y(k,139)
         mat(k,2340) = .800_r8*rxt(k,480)*y(k,248)
         mat(k,774) = .800_r8*rxt(k,480)*y(k,237)
         mat(k,691) = -(rxt(k,484)*y(k,259))
         mat(k,2116) = -rxt(k,484)*y(k,140)
         mat(k,2665) = rxt(k,487)*y(k,246)
         mat(k,1528) = rxt(k,487)*y(k,156)
         mat(k,1042) = -(rxt(k,575)*y(k,157) + rxt(k,576)*y(k,166) + rxt(k,577) &
                      *y(k,259))
         mat(k,2780) = -rxt(k,575)*y(k,141)
         mat(k,1943) = -rxt(k,576)*y(k,141)
         mat(k,2143) = -rxt(k,577)*y(k,141)
         mat(k,1511) = -(rxt(k,485)*y(k,166) + rxt(k,486)*y(k,259))
         mat(k,1966) = -rxt(k,485)*y(k,142)
         mat(k,2174) = -rxt(k,486)*y(k,142)
         mat(k,965) = .200_r8*rxt(k,518)*y(k,166)
         mat(k,2590) = .560_r8*rxt(k,501)*y(k,239)
         mat(k,2809) = .600_r8*rxt(k,502)*y(k,239)
         mat(k,1966) = mat(k,1966) + .200_r8*rxt(k,518)*y(k,129)
         mat(k,1561) = .610_r8*rxt(k,498)*y(k,239)
         mat(k,1751) = .440_r8*rxt(k,499)*y(k,239)
         mat(k,1436) = .560_r8*rxt(k,501)*y(k,155) + .600_r8*rxt(k,502)*y(k,157) &
                      + .610_r8*rxt(k,498)*y(k,230) + .440_r8*rxt(k,499)*y(k,231)
         mat(k,1123) = -(rxt(k,220)*y(k,155) + (rxt(k,221) + rxt(k,222) + rxt(k,223) &
                      ) * y(k,156) + rxt(k,224)*y(k,165) + rxt(k,232)*y(k,259) &
                      + rxt(k,745)*y(k,258))
         mat(k,2569) = -rxt(k,220)*y(k,143)
         mat(k,2673) = -(rxt(k,221) + rxt(k,222) + rxt(k,223)) * y(k,143)
         mat(k,1785) = -rxt(k,224)*y(k,143)
         mat(k,2148) = -rxt(k,232)*y(k,143)
         mat(k,930) = -rxt(k,745)*y(k,143)
         mat(k,2481) = rxt(k,218)*y(k,250) + rxt(k,742)*y(k,253)
         mat(k,1785) = mat(k,1785) + rxt(k,743)*y(k,253)
         mat(k,941) = 1.100_r8*rxt(k,738)*y(k,251) + .200_r8*rxt(k,736)*y(k,252)
         mat(k,618) = rxt(k,218)*y(k,164)
         mat(k,788) = 1.100_r8*rxt(k,738)*y(k,233)
         mat(k,922) = .200_r8*rxt(k,736)*y(k,233)
         mat(k,585) = rxt(k,742)*y(k,164) + rxt(k,743)*y(k,165)
         mat(k,297) = -((rxt(k,236) + rxt(k,237)) * y(k,255))
         mat(k,1868) = -(rxt(k,236) + rxt(k,237)) * y(k,144)
         mat(k,1117) = rxt(k,221)*y(k,156)
         mat(k,2652) = rxt(k,221)*y(k,143)
         mat(k,2655) = rxt(k,239)*y(k,157)
         mat(k,2771) = rxt(k,239)*y(k,156)
         mat(k,446) = -(rxt(k,521)*y(k,259))
         mat(k,2085) = -rxt(k,521)*y(k,146)
         mat(k,1722) = .200_r8*rxt(k,513)*y(k,241)
         mat(k,1378) = .200_r8*rxt(k,513)*y(k,231)
         mat(k,1188) = -(rxt(k,522)*y(k,259))
         mat(k,2154) = -rxt(k,522)*y(k,147)
         mat(k,2572) = rxt(k,515)*y(k,241)
         mat(k,2789) = rxt(k,516)*y(k,241)
         mat(k,1550) = rxt(k,512)*y(k,241)
         mat(k,1734) = .800_r8*rxt(k,513)*y(k,241)
         mat(k,1383) = rxt(k,515)*y(k,155) + rxt(k,516)*y(k,157) + rxt(k,512)*y(k,230) &
                      + .800_r8*rxt(k,513)*y(k,231)
         mat(k,110) = -(rxt(k,633)*y(k,259))
         mat(k,2039) = -rxt(k,633)*y(k,151)
         mat(k,2613) = -(rxt(k,220)*y(k,143) + rxt(k,229)*y(k,157) + rxt(k,233) &
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
                      *y(k,264) + rxt(k,590)*y(k,265) + rxt(k,747)*y(k,258))
         mat(k,1131) = -rxt(k,220)*y(k,155)
         mat(k,2833) = -rxt(k,229)*y(k,155)
         mat(k,2432) = -rxt(k,233)*y(k,155)
         mat(k,1987) = -rxt(k,234)*y(k,155)
         mat(k,2508) = -rxt(k,235)*y(k,155)
         mat(k,2017) = -rxt(k,258)*y(k,155)
         mat(k,2461) = -rxt(k,292)*y(k,155)
         mat(k,2230) = -rxt(k,320)*y(k,155)
         mat(k,2761) = -rxt(k,329)*y(k,155)
         mat(k,712) = -rxt(k,336)*y(k,155)
         mat(k,1179) = -rxt(k,376)*y(k,155)
         mat(k,1768) = -rxt(k,395)*y(k,155)
         mat(k,511) = -rxt(k,403)*y(k,155)
         mat(k,1016) = -rxt(k,416)*y(k,155)
         mat(k,1571) = -rxt(k,427)*y(k,155)
         mat(k,881) = -rxt(k,431)*y(k,155)
         mat(k,915) = -rxt(k,444)*y(k,155)
         mat(k,903) = -rxt(k,452)*y(k,155)
         mat(k,1342) = -rxt(k,456)*y(k,155)
         mat(k,659) = -(rxt(k,462) + rxt(k,463)) * y(k,155)
         mat(k,1498) = -(rxt(k,469) + rxt(k,470)) * y(k,155)
         mat(k,1539) = -rxt(k,478)*y(k,155)
         mat(k,780) = -rxt(k,481)*y(k,155)
         mat(k,1102) = -(rxt(k,491) + rxt(k,492)) * y(k,155)
         mat(k,1444) = -rxt(k,501)*y(k,155)
         mat(k,1477) = -rxt(k,507)*y(k,155)
         mat(k,1396) = -rxt(k,515)*y(k,155)
         mat(k,1374) = -rxt(k,526)*y(k,155)
         mat(k,615) = -rxt(k,530)*y(k,155)
         mat(k,579) = -rxt(k,533)*y(k,155)
         mat(k,506) = -rxt(k,538)*y(k,155)
         mat(k,727) = -rxt(k,540)*y(k,155)
         mat(k,872) = -rxt(k,544)*y(k,155)
         mat(k,825) = -rxt(k,547)*y(k,155)
         mat(k,1026) = -rxt(k,550)*y(k,155)
         mat(k,519) = -rxt(k,553)*y(k,155)
         mat(k,840) = -rxt(k,560)*y(k,155)
         mat(k,864) = -rxt(k,566)*y(k,155)
         mat(k,594) = -rxt(k,569)*y(k,155)
         mat(k,1242) = -rxt(k,580)*y(k,155)
         mat(k,1306) = -rxt(k,585)*y(k,155)
         mat(k,1327) = -rxt(k,590)*y(k,155)
         mat(k,934) = -rxt(k,747)*y(k,155)
         mat(k,221) = 4.000_r8*rxt(k,319)*y(k,125)
         mat(k,1131) = mat(k,1131) + 2.000_r8*rxt(k,222)*y(k,156) + rxt(k,224) &
                      *y(k,165) + rxt(k,232)*y(k,259)
         mat(k,300) = 2.000_r8*rxt(k,236)*y(k,255)
         mat(k,2702) = 2.000_r8*rxt(k,222)*y(k,143) + rxt(k,225)*y(k,164) + rxt(k,609) &
                      *y(k,183)
         mat(k,2508) = mat(k,2508) + rxt(k,225)*y(k,156)
         mat(k,1802) = rxt(k,224)*y(k,143) + rxt(k,219)*y(k,250)
         mat(k,1650) = rxt(k,609)*y(k,156)
         mat(k,621) = rxt(k,219)*y(k,165)
         mat(k,1894) = 2.000_r8*rxt(k,236)*y(k,144)
         mat(k,2200) = rxt(k,232)*y(k,143)
         mat(k,2704) = -((rxt(k,221) + rxt(k,222) + rxt(k,223)) * y(k,143) + (rxt(k,225) &
                      + rxt(k,227)) * y(k,164) + rxt(k,226)*y(k,166) + rxt(k,238) &
                      *y(k,237) + rxt(k,239)*y(k,157) + rxt(k,240)*y(k,259) + rxt(k,250) &
                      *y(k,72) + rxt(k,260)*y(k,76) + rxt(k,285)*y(k,19) + rxt(k,295) &
                      *y(k,23) + rxt(k,316)*y(k,117) + rxt(k,330)*y(k,127) + rxt(k,438) &
                      *y(k,230) + rxt(k,487)*y(k,246) + rxt(k,545)*y(k,232) + rxt(k,548) &
                      *y(k,245) + rxt(k,551)*y(k,247) + rxt(k,555)*y(k,174) + rxt(k,558) &
                      *y(k,221) + rxt(k,609)*y(k,183))
         mat(k,1132) = -(rxt(k,221) + rxt(k,222) + rxt(k,223)) * y(k,156)
         mat(k,2510) = -(rxt(k,225) + rxt(k,227)) * y(k,156)
         mat(k,1989) = -rxt(k,226)*y(k,156)
         mat(k,2434) = -rxt(k,238)*y(k,156)
         mat(k,2835) = -rxt(k,239)*y(k,156)
         mat(k,2202) = -rxt(k,240)*y(k,156)
         mat(k,2289) = -rxt(k,250)*y(k,156)
         mat(k,2019) = -rxt(k,260)*y(k,156)
         mat(k,2319) = -rxt(k,285)*y(k,156)
         mat(k,2463) = -rxt(k,295)*y(k,156)
         mat(k,2232) = -rxt(k,316)*y(k,156)
         mat(k,2763) = -rxt(k,330)*y(k,156)
         mat(k,1573) = -rxt(k,438)*y(k,156)
         mat(k,1541) = -rxt(k,487)*y(k,156)
         mat(k,873) = -rxt(k,545)*y(k,156)
         mat(k,826) = -rxt(k,548)*y(k,156)
         mat(k,1027) = -rxt(k,551)*y(k,156)
         mat(k,547) = -rxt(k,555)*y(k,156)
         mat(k,616) = -rxt(k,558)*y(k,156)
         mat(k,1651) = -rxt(k,609)*y(k,156)
         mat(k,773) = rxt(k,489)*y(k,259)
         mat(k,417) = rxt(k,460)*y(k,157)
         mat(k,2463) = mat(k,2463) + rxt(k,292)*y(k,155)
         mat(k,1181) = rxt(k,376)*y(k,155) + rxt(k,377)*y(k,157)
         mat(k,601) = rxt(k,251)*y(k,259)
         mat(k,2019) = mat(k,2019) + rxt(k,258)*y(k,155)
         mat(k,555) = rxt(k,217)*y(k,259)
         mat(k,2232) = mat(k,2232) + rxt(k,318)*y(k,157)
         mat(k,322) = 4.000_r8*rxt(k,315)*y(k,126)
         mat(k,2763) = mat(k,2763) + rxt(k,329)*y(k,155) + rxt(k,331)*y(k,157)
         mat(k,669) = .700_r8*rxt(k,509)*y(k,259)
         mat(k,2615) = rxt(k,292)*y(k,23) + rxt(k,376)*y(k,70) + rxt(k,258)*y(k,76) &
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
         mat(k,2835) = mat(k,2835) + rxt(k,460)*y(k,18) + rxt(k,377)*y(k,70) &
                      + rxt(k,318)*y(k,117) + rxt(k,331)*y(k,127) &
                      + 2.000_r8*rxt(k,229)*y(k,155) + rxt(k,230)*y(k,164) &
                      + rxt(k,228)*y(k,237) + rxt(k,502)*y(k,239) + rxt(k,508) &
                      *y(k,240) + rxt(k,516)*y(k,241) + rxt(k,468)*y(k,244) &
                      + rxt(k,479)*y(k,246) + 2.000_r8*rxt(k,581)*y(k,254) &
                      + rxt(k,231)*y(k,259) + rxt(k,527)*y(k,268)
         mat(k,953) = rxt(k,450)*y(k,259)
         mat(k,2510) = mat(k,2510) + rxt(k,235)*y(k,155) + rxt(k,230)*y(k,157)
         mat(k,1989) = mat(k,1989) + rxt(k,234)*y(k,155)
         mat(k,713) = rxt(k,336)*y(k,155)
         mat(k,719) = rxt(k,587)*y(k,259)
         mat(k,616) = mat(k,616) + rxt(k,530)*y(k,155)
         mat(k,1104) = rxt(k,491)*y(k,155)
         mat(k,580) = rxt(k,533)*y(k,155)
         mat(k,507) = rxt(k,538)*y(k,155)
         mat(k,1018) = rxt(k,416)*y(k,155)
         mat(k,917) = rxt(k,444)*y(k,155)
         mat(k,728) = rxt(k,540)*y(k,155)
         mat(k,1573) = mat(k,1573) + rxt(k,427)*y(k,155)
         mat(k,1770) = rxt(k,395)*y(k,155) + .500_r8*rxt(k,578)*y(k,254)
         mat(k,873) = mat(k,873) + rxt(k,544)*y(k,155)
         mat(k,661) = rxt(k,462)*y(k,155)
         mat(k,883) = rxt(k,431)*y(k,155)
         mat(k,2434) = mat(k,2434) + rxt(k,233)*y(k,155) + rxt(k,228)*y(k,157)
         mat(k,513) = rxt(k,403)*y(k,155)
         mat(k,1446) = .920_r8*rxt(k,501)*y(k,155) + rxt(k,502)*y(k,157)
         mat(k,1479) = .920_r8*rxt(k,507)*y(k,155) + rxt(k,508)*y(k,157)
         mat(k,1398) = rxt(k,515)*y(k,155) + rxt(k,516)*y(k,157)
         mat(k,1500) = rxt(k,469)*y(k,155) + rxt(k,468)*y(k,157)
         mat(k,826) = mat(k,826) + rxt(k,547)*y(k,155)
         mat(k,1541) = mat(k,1541) + rxt(k,478)*y(k,155) + rxt(k,479)*y(k,157)
         mat(k,1027) = mat(k,1027) + rxt(k,550)*y(k,155)
         mat(k,781) = rxt(k,481)*y(k,155)
         mat(k,1244) = 1.600_r8*rxt(k,580)*y(k,155) + 2.000_r8*rxt(k,581)*y(k,157) &
                      + .500_r8*rxt(k,578)*y(k,231)
         mat(k,2202) = mat(k,2202) + rxt(k,489)*y(k,1) + rxt(k,251)*y(k,75) &
                      + rxt(k,217)*y(k,109) + .700_r8*rxt(k,509)*y(k,130) + rxt(k,231) &
                      *y(k,157) + rxt(k,450)*y(k,158) + rxt(k,587)*y(k,208)
         mat(k,520) = rxt(k,553)*y(k,155)
         mat(k,905) = rxt(k,452)*y(k,155)
         mat(k,1344) = rxt(k,456)*y(k,155)
         mat(k,1308) = .900_r8*rxt(k,585)*y(k,155)
         mat(k,1329) = .800_r8*rxt(k,590)*y(k,155)
         mat(k,841) = rxt(k,560)*y(k,155)
         mat(k,1376) = rxt(k,526)*y(k,155) + rxt(k,527)*y(k,157)
         mat(k,865) = rxt(k,566)*y(k,155)
         mat(k,595) = rxt(k,569)*y(k,155)
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
         mat(k,2838) = -(rxt(k,228)*y(k,237) + rxt(k,229)*y(k,155) + rxt(k,230) &
                      *y(k,164) + rxt(k,231)*y(k,259) + rxt(k,239)*y(k,156) + rxt(k,307) &
                      *y(k,107) + rxt(k,309)*y(k,118) + rxt(k,318)*y(k,117) + rxt(k,331) &
                      *y(k,127) + rxt(k,337)*y(k,112) + rxt(k,377)*y(k,70) + rxt(k,389) &
                      *y(k,53) + rxt(k,421)*y(k,56) + rxt(k,440)*y(k,35) + rxt(k,447) &
                      *y(k,60) + rxt(k,460)*y(k,18) + rxt(k,468)*y(k,244) + rxt(k,479) &
                      *y(k,246) + rxt(k,502)*y(k,239) + rxt(k,508)*y(k,240) + rxt(k,511) &
                      *y(k,129) + rxt(k,516)*y(k,241) + rxt(k,527)*y(k,268) + rxt(k,572) &
                      *y(k,8) + rxt(k,575)*y(k,141) + rxt(k,581)*y(k,254) + rxt(k,592) &
                      *y(k,210) + rxt(k,599)*y(k,85))
         mat(k,2437) = -rxt(k,228)*y(k,157)
         mat(k,2618) = -rxt(k,229)*y(k,157)
         mat(k,2513) = -rxt(k,230)*y(k,157)
         mat(k,2205) = -rxt(k,231)*y(k,157)
         mat(k,2707) = -rxt(k,239)*y(k,157)
         mat(k,1359) = -rxt(k,307)*y(k,157)
         mat(k,1635) = -rxt(k,309)*y(k,157)
         mat(k,2235) = -rxt(k,318)*y(k,157)
         mat(k,2766) = -rxt(k,331)*y(k,157)
         mat(k,1694) = -rxt(k,337)*y(k,157)
         mat(k,1182) = -rxt(k,377)*y(k,157)
         mat(k,2649) = -rxt(k,389)*y(k,157)
         mat(k,1229) = -rxt(k,421)*y(k,157)
         mat(k,1220) = -rxt(k,440)*y(k,157)
         mat(k,1421) = -rxt(k,447)*y(k,157)
         mat(k,418) = -rxt(k,460)*y(k,157)
         mat(k,1502) = -rxt(k,468)*y(k,157)
         mat(k,1543) = -rxt(k,479)*y(k,157)
         mat(k,1448) = -rxt(k,502)*y(k,157)
         mat(k,1481) = -rxt(k,508)*y(k,157)
         mat(k,972) = -rxt(k,511)*y(k,157)
         mat(k,1400) = -rxt(k,516)*y(k,157)
         mat(k,1377) = -rxt(k,527)*y(k,157)
         mat(k,1087) = -rxt(k,572)*y(k,157)
         mat(k,1059) = -rxt(k,575)*y(k,157)
         mat(k,1246) = -rxt(k,581)*y(k,157)
         mat(k,1163) = -rxt(k,592)*y(k,157)
         mat(k,1155) = -rxt(k,599)*y(k,157)
         mat(k,2322) = rxt(k,293)*y(k,24)
         mat(k,996) = rxt(k,293)*y(k,19) + rxt(k,294)*y(k,72) + rxt(k,296)*y(k,164)
         mat(k,2292) = rxt(k,294)*y(k,24) + rxt(k,259)*y(k,77)
         mat(k,1143) = rxt(k,259)*y(k,72) + rxt(k,261)*y(k,164) + rxt(k,262)*y(k,259)
         mat(k,1005) = rxt(k,349)*y(k,108)
         mat(k,2733) = rxt(k,349)*y(k,91) + rxt(k,241)*y(k,259)
         mat(k,2235) = mat(k,2235) + rxt(k,314)*y(k,128)
         mat(k,984) = rxt(k,314)*y(k,117)
         mat(k,699) = .500_r8*rxt(k,484)*y(k,259)
         mat(k,2707) = mat(k,2707) + rxt(k,227)*y(k,164) + rxt(k,226)*y(k,166)
         mat(k,2513) = mat(k,2513) + rxt(k,296)*y(k,24) + rxt(k,261)*y(k,77) &
                      + rxt(k,227)*y(k,156)
         mat(k,1992) = rxt(k,226)*y(k,156)
         mat(k,629) = rxt(k,436)*y(k,259)
         mat(k,2205) = mat(k,2205) + rxt(k,262)*y(k,77) + rxt(k,241)*y(k,108) &
                      + .500_r8*rxt(k,484)*y(k,140) + rxt(k,436)*y(k,172)
         mat(k,948) = -(rxt(k,450)*y(k,259))
         mat(k,2137) = -rxt(k,450)*y(k,158)
         mat(k,1202) = rxt(k,440)*y(k,157)
         mat(k,647) = .500_r8*rxt(k,510)*y(k,259)
         mat(k,523) = rxt(k,517)*y(k,259)
         mat(k,447) = rxt(k,521)*y(k,259)
         mat(k,1185) = rxt(k,522)*y(k,259)
         mat(k,2776) = rxt(k,440)*y(k,35)
         mat(k,2137) = mat(k,2137) + .500_r8*rxt(k,510)*y(k,131) + rxt(k,517)*y(k,132) &
                      + rxt(k,521)*y(k,146) + rxt(k,522)*y(k,147)
         mat(k,452) = -(rxt(k,582)*y(k,259))
         mat(k,2086) = -rxt(k,582)*y(k,159)
         mat(k,2345) = rxt(k,579)*y(k,254)
         mat(k,1231) = rxt(k,579)*y(k,237)
         mat(k,2507) = -(rxt(k,197)*y(k,166) + 4._r8*rxt(k,198)*y(k,164) + rxt(k,199) &
                      *y(k,165) + rxt(k,200)*y(k,95) + rxt(k,201)*y(k,97) + rxt(k,206) &
                      *y(k,237) + rxt(k,212)*y(k,259) + (rxt(k,225) + rxt(k,227) &
                      ) * y(k,156) + rxt(k,230)*y(k,157) + rxt(k,235)*y(k,155) &
                      + rxt(k,261)*y(k,77) + rxt(k,263)*y(k,76) + rxt(k,266)*y(k,103) &
                      + rxt(k,269)*y(k,111) + rxt(k,296)*y(k,24) + rxt(k,297)*y(k,23) &
                      + rxt(k,299)*y(k,99) + rxt(k,301)*y(k,110) + rxt(k,310)*y(k,118) &
                      + rxt(k,332)*y(k,127) + rxt(k,390)*y(k,53) + rxt(k,601)*y(k,169) &
                      + (rxt(k,740) + rxt(k,741)) * y(k,251) + rxt(k,742)*y(k,253))
         mat(k,1986) = -rxt(k,197)*y(k,164)
         mat(k,1801) = -rxt(k,199)*y(k,164)
         mat(k,1620) = -rxt(k,200)*y(k,164)
         mat(k,705) = -rxt(k,201)*y(k,164)
         mat(k,2431) = -rxt(k,206)*y(k,164)
         mat(k,2199) = -rxt(k,212)*y(k,164)
         mat(k,2701) = -(rxt(k,225) + rxt(k,227)) * y(k,164)
         mat(k,2832) = -rxt(k,230)*y(k,164)
         mat(k,2612) = -rxt(k,235)*y(k,164)
         mat(k,1140) = -rxt(k,261)*y(k,164)
         mat(k,2016) = -rxt(k,263)*y(k,164)
         mat(k,1823) = -rxt(k,266)*y(k,164)
         mat(k,1847) = -rxt(k,269)*y(k,164)
         mat(k,993) = -rxt(k,296)*y(k,164)
         mat(k,2460) = -rxt(k,297)*y(k,164)
         mat(k,1608) = -rxt(k,299)*y(k,164)
         mat(k,1713) = -rxt(k,301)*y(k,164)
         mat(k,1631) = -rxt(k,310)*y(k,164)
         mat(k,2760) = -rxt(k,332)*y(k,164)
         mat(k,2643) = -rxt(k,390)*y(k,164)
         mat(k,426) = -rxt(k,601)*y(k,164)
         mat(k,791) = -(rxt(k,740) + rxt(k,741)) * y(k,164)
         mat(k,587) = -rxt(k,742)*y(k,164)
         mat(k,1917) = rxt(k,204)*y(k,237)
         mat(k,1130) = rxt(k,220)*y(k,155) + rxt(k,221)*y(k,156) + rxt(k,224)*y(k,165) &
                      + rxt(k,745)*y(k,258)
         mat(k,2612) = mat(k,2612) + rxt(k,220)*y(k,143)
         mat(k,2701) = mat(k,2701) + rxt(k,221)*y(k,143)
         mat(k,1801) = mat(k,1801) + rxt(k,224)*y(k,143) + rxt(k,603)*y(k,181) &
                      + rxt(k,610)*y(k,183) + rxt(k,744)*y(k,253) + (rxt(k,186) &
                       +rxt(k,187))*y(k,255) + rxt(k,750)*y(k,260)
         mat(k,848) = rxt(k,603)*y(k,165)
         mat(k,1649) = rxt(k,610)*y(k,165)
         mat(k,946) = rxt(k,736)*y(k,252) + 1.150_r8*rxt(k,737)*y(k,258)
         mat(k,2431) = mat(k,2431) + rxt(k,204)*y(k,94)
         mat(k,925) = rxt(k,736)*y(k,233)
         mat(k,587) = mat(k,587) + rxt(k,744)*y(k,165)
         mat(k,1893) = (rxt(k,186)+rxt(k,187))*y(k,165)
         mat(k,933) = rxt(k,745)*y(k,143) + 1.150_r8*rxt(k,737)*y(k,233)
         mat(k,2199) = mat(k,2199) + 2.000_r8*rxt(k,214)*y(k,259)
         mat(k,891) = rxt(k,750)*y(k,165)
         mat(k,1791) = -(rxt(k,186)*y(k,255) + rxt(k,191)*y(k,256) + rxt(k,199) &
                      *y(k,164) + rxt(k,205)*y(k,94) + rxt(k,219)*y(k,250) + rxt(k,224) &
                      *y(k,143) + rxt(k,433)*y(k,235) + rxt(k,603)*y(k,181) + rxt(k,610) &
                      *y(k,183) + rxt(k,739)*y(k,251) + (rxt(k,743) + rxt(k,744) &
                      ) * y(k,253) + rxt(k,750)*y(k,260))
         mat(k,1880) = -rxt(k,186)*y(k,165)
         mat(k,208) = -rxt(k,191)*y(k,165)
         mat(k,2494) = -rxt(k,199)*y(k,165)
         mat(k,1904) = -rxt(k,205)*y(k,165)
         mat(k,619) = -rxt(k,219)*y(k,165)
         mat(k,1126) = -rxt(k,224)*y(k,165)
         mat(k,537) = -rxt(k,433)*y(k,165)
         mat(k,844) = -rxt(k,603)*y(k,165)
         mat(k,1640) = -rxt(k,610)*y(k,165)
         mat(k,789) = -rxt(k,739)*y(k,165)
         mat(k,586) = -(rxt(k,743) + rxt(k,744)) * y(k,165)
         mat(k,890) = -rxt(k,750)*y(k,165)
         mat(k,2303) = rxt(k,286)*y(k,166) + rxt(k,284)*y(k,237)
         mat(k,2447) = 2.000_r8*rxt(k,287)*y(k,23) + (rxt(k,289)+rxt(k,290))*y(k,76) &
                      + rxt(k,323)*y(k,127) + rxt(k,297)*y(k,164) + rxt(k,291) &
                      *y(k,237)
         mat(k,1175) = rxt(k,374)*y(k,237)
         mat(k,2273) = rxt(k,252)*y(k,166) + rxt(k,248)*y(k,237)
         mat(k,2003) = (rxt(k,289)+rxt(k,290))*y(k,23) + (2.000_r8*rxt(k,254) &
                       +2.000_r8*rxt(k,255))*y(k,76) + (rxt(k,326)+rxt(k,327)) &
                      *y(k,127) + rxt(k,263)*y(k,164) + rxt(k,257)*y(k,237) &
                      + rxt(k,265)*y(k,259)
         mat(k,1904) = mat(k,1904) + rxt(k,208)*y(k,166) + rxt(k,202)*y(k,237)
         mat(k,552) = rxt(k,217)*y(k,259)
         mat(k,2216) = rxt(k,321)*y(k,166) + rxt(k,313)*y(k,237)
         mat(k,2747) = rxt(k,323)*y(k,23) + (rxt(k,326)+rxt(k,327))*y(k,76) &
                      + rxt(k,332)*y(k,164) + rxt(k,333)*y(k,166) + rxt(k,328) &
                      *y(k,237)
         mat(k,1126) = mat(k,1126) + rxt(k,223)*y(k,156)
         mat(k,298) = rxt(k,237)*y(k,255)
         mat(k,2599) = rxt(k,234)*y(k,166) + rxt(k,747)*y(k,258)
         mat(k,2688) = rxt(k,223)*y(k,143) + rxt(k,225)*y(k,164) + rxt(k,226)*y(k,166)
         mat(k,2819) = rxt(k,230)*y(k,164) + rxt(k,228)*y(k,237)
         mat(k,2494) = mat(k,2494) + rxt(k,297)*y(k,23) + rxt(k,263)*y(k,76) &
                      + rxt(k,332)*y(k,127) + rxt(k,225)*y(k,156) + rxt(k,230) &
                      *y(k,157) + 2.000_r8*rxt(k,198)*y(k,164) + 2.000_r8*rxt(k,197) &
                      *y(k,166) + rxt(k,206)*y(k,237) + rxt(k,190)*y(k,256) &
                      + rxt(k,212)*y(k,259)
         mat(k,1791) = mat(k,1791) + 2.000_r8*rxt(k,191)*y(k,256)
         mat(k,1973) = rxt(k,286)*y(k,19) + rxt(k,252)*y(k,72) + rxt(k,208)*y(k,94) &
                      + rxt(k,321)*y(k,117) + rxt(k,333)*y(k,127) + rxt(k,234) &
                      *y(k,155) + rxt(k,226)*y(k,156) + 2.000_r8*rxt(k,197)*y(k,164) &
                      + rxt(k,605)*y(k,181) + rxt(k,611)*y(k,183) &
                      + 2.000_r8*rxt(k,207)*y(k,237) + 2.000_r8*rxt(k,188)*y(k,255) &
                      + rxt(k,213)*y(k,259)
         mat(k,844) = mat(k,844) + rxt(k,605)*y(k,166)
         mat(k,1640) = mat(k,1640) + rxt(k,611)*y(k,166)
         mat(k,1013) = rxt(k,415)*y(k,237)
         mat(k,912) = rxt(k,443)*y(k,237)
         mat(k,1757) = rxt(k,394)*y(k,237)
         mat(k,2418) = rxt(k,284)*y(k,19) + rxt(k,291)*y(k,23) + rxt(k,374)*y(k,70) &
                      + rxt(k,248)*y(k,72) + rxt(k,257)*y(k,76) + rxt(k,202)*y(k,94) &
                      + rxt(k,313)*y(k,117) + rxt(k,328)*y(k,127) + rxt(k,228) &
                      *y(k,157) + rxt(k,206)*y(k,164) + 2.000_r8*rxt(k,207)*y(k,166) &
                      + rxt(k,415)*y(k,227) + rxt(k,443)*y(k,228) + rxt(k,394) &
                      *y(k,231) + 2.000_r8*rxt(k,216)*y(k,237) + rxt(k,211)*y(k,259) &
                      + rxt(k,451)*y(k,262)
         mat(k,1880) = mat(k,1880) + rxt(k,237)*y(k,144) + 2.000_r8*rxt(k,188) &
                      *y(k,166)
         mat(k,208) = mat(k,208) + rxt(k,190)*y(k,164) + 2.000_r8*rxt(k,191)*y(k,165)
         mat(k,931) = rxt(k,747)*y(k,155)
         mat(k,2186) = rxt(k,265)*y(k,76) + rxt(k,217)*y(k,109) + rxt(k,212)*y(k,164) &
                      + rxt(k,213)*y(k,166) + rxt(k,211)*y(k,237)
         mat(k,900) = rxt(k,451)*y(k,237)
         mat(k,1978) = -(rxt(k,188)*y(k,255) + rxt(k,197)*y(k,164) + rxt(k,207) &
                      *y(k,237) + rxt(k,208)*y(k,94) + rxt(k,213)*y(k,259) + rxt(k,226) &
                      *y(k,156) + rxt(k,234)*y(k,155) + rxt(k,252)*y(k,72) + rxt(k,286) &
                      *y(k,19) + rxt(k,321)*y(k,117) + rxt(k,333)*y(k,127) + rxt(k,412) &
                      *y(k,30) + rxt(k,441)*y(k,35) + rxt(k,471)*y(k,136) + rxt(k,485) &
                      *y(k,142) + rxt(k,518)*y(k,129) + rxt(k,556)*y(k,174) + rxt(k,573) &
                      *y(k,8) + rxt(k,576)*y(k,141) + rxt(k,605)*y(k,181) + rxt(k,611) &
                      *y(k,183))
         mat(k,1885) = -rxt(k,188)*y(k,166)
         mat(k,2499) = -rxt(k,197)*y(k,166)
         mat(k,2423) = -rxt(k,207)*y(k,166)
         mat(k,1909) = -rxt(k,208)*y(k,166)
         mat(k,2191) = -rxt(k,213)*y(k,166)
         mat(k,2693) = -rxt(k,226)*y(k,166)
         mat(k,2604) = -rxt(k,234)*y(k,166)
         mat(k,2278) = -rxt(k,252)*y(k,166)
         mat(k,2308) = -rxt(k,286)*y(k,166)
         mat(k,2221) = -rxt(k,321)*y(k,166)
         mat(k,2752) = -rxt(k,333)*y(k,166)
         mat(k,642) = -rxt(k,412)*y(k,166)
         mat(k,1214) = -rxt(k,441)*y(k,166)
         mat(k,1409) = -rxt(k,471)*y(k,166)
         mat(k,1518) = -rxt(k,485)*y(k,166)
         mat(k,968) = -rxt(k,518)*y(k,166)
         mat(k,546) = -rxt(k,556)*y(k,166)
         mat(k,1083) = -rxt(k,573)*y(k,166)
         mat(k,1055) = -rxt(k,576)*y(k,166)
         mat(k,846) = -rxt(k,605)*y(k,166)
         mat(k,1642) = -rxt(k,611)*y(k,166)
         mat(k,2499) = mat(k,2499) + rxt(k,199)*y(k,165)
         mat(k,1794) = rxt(k,199)*y(k,164)
         mat(k,1568) = .150_r8*rxt(k,426)*y(k,237)
         mat(k,2423) = mat(k,2423) + .150_r8*rxt(k,426)*y(k,230) + .150_r8*rxt(k,476) &
                      *y(k,246)
         mat(k,1536) = .150_r8*rxt(k,476)*y(k,237)
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
         mat(k,527) = -(rxt(k,612)*y(k,183))
         mat(k,1636) = -rxt(k,612)*y(k,168)
         mat(k,2439) = rxt(k,288)*y(k,76)
         mat(k,1996) = rxt(k,288)*y(k,23) + 2.000_r8*rxt(k,256)*y(k,76) + rxt(k,325) &
                      *y(k,127)
         mat(k,2737) = rxt(k,325)*y(k,76)
         mat(k,419) = -(rxt(k,601)*y(k,164) + rxt(k,602)*y(k,259))
         mat(k,2470) = -rxt(k,601)*y(k,169)
         mat(k,2082) = -rxt(k,602)*y(k,169)
         mat(k,709) = -(rxt(k,336)*y(k,155) + rxt(k,344)*y(k,127) + 4._r8*rxt(k,345) &
                      *y(k,170))
         mat(k,2547) = -rxt(k,336)*y(k,170)
         mat(k,2739) = -rxt(k,344)*y(k,170)
         mat(k,2441) = rxt(k,324)*y(k,127)
         mat(k,2739) = mat(k,2739) + rxt(k,324)*y(k,23) + 2.000_r8*rxt(k,341)*y(k,127) &
                      + rxt(k,331)*y(k,157) + rxt(k,333)*y(k,166)
         mat(k,2775) = rxt(k,331)*y(k,127)
         mat(k,1936) = rxt(k,333)*y(k,127)
         mat(k,1258) = rxt(k,464)*y(k,259)
         mat(k,2529) = .100_r8*rxt(k,585)*y(k,264)
         mat(k,2064) = rxt(k,464)*y(k,113)
         mat(k,1292) = .100_r8*rxt(k,585)*y(k,155)
         mat(k,622) = -(rxt(k,436)*y(k,259))
         mat(k,2107) = -rxt(k,436)*y(k,172)
         mat(k,2664) = rxt(k,438)*y(k,230)
         mat(k,1546) = rxt(k,438)*y(k,156)
         mat(k,2651) = rxt(k,558)*y(k,221)
         mat(k,610) = rxt(k,558)*y(k,156)
         mat(k,544) = -(rxt(k,555)*y(k,156) + rxt(k,556)*y(k,166))
         mat(k,2658) = -rxt(k,555)*y(k,174)
         mat(k,1934) = -rxt(k,556)*y(k,174)
         mat(k,236) = .070_r8*rxt(k,542)*y(k,259)
         mat(k,2538) = rxt(k,540)*y(k,229)
         mat(k,204) = .060_r8*rxt(k,554)*y(k,259)
         mat(k,257) = .070_r8*rxt(k,570)*y(k,259)
         mat(k,722) = rxt(k,540)*y(k,155)
         mat(k,2098) = .070_r8*rxt(k,542)*y(k,84) + .060_r8*rxt(k,554)*y(k,175) &
                      + .070_r8*rxt(k,570)*y(k,217)
         mat(k,202) = -(rxt(k,554)*y(k,259))
         mat(k,2048) = -rxt(k,554)*y(k,175)
         mat(k,194) = .530_r8*rxt(k,531)*y(k,259)
         mat(k,2048) = mat(k,2048) + .530_r8*rxt(k,531)*y(k,9)
         mat(k,373) = -(rxt(k,557)*y(k,259))
         mat(k,2075) = -rxt(k,557)*y(k,176)
         mat(k,2341) = rxt(k,552)*y(k,261)
         mat(k,514) = rxt(k,552)*y(k,237)
         mat(k,630) = -(rxt(k,453)*y(k,259))
         mat(k,2108) = -rxt(k,453)*y(k,179)
         mat(k,2363) = rxt(k,451)*y(k,262)
         mat(k,896) = rxt(k,451)*y(k,237)
         mat(k,464) = -(rxt(k,457)*y(k,259))
         mat(k,2087) = -rxt(k,457)*y(k,180)
         mat(k,2346) = .850_r8*rxt(k,455)*y(k,263)
         mat(k,1333) = .850_r8*rxt(k,455)*y(k,237)
         mat(k,842) = -(rxt(k,603)*y(k,165) + rxt(k,605)*y(k,166) + rxt(k,608) &
                      *y(k,259))
         mat(k,1780) = -rxt(k,603)*y(k,181)
         mat(k,1939) = -rxt(k,605)*y(k,181)
         mat(k,2130) = -rxt(k,608)*y(k,181)
         mat(k,1639) = -(rxt(k,606)*y(k,23) + rxt(k,607)*y(k,76) + rxt(k,609)*y(k,156) &
                      + rxt(k,610)*y(k,165) + rxt(k,611)*y(k,166) + rxt(k,612) &
                      *y(k,168) + rxt(k,613)*y(k,259))
         mat(k,2445) = -rxt(k,606)*y(k,183)
         mat(k,2001) = -rxt(k,607)*y(k,183)
         mat(k,2683) = -rxt(k,609)*y(k,183)
         mat(k,1790) = -rxt(k,610)*y(k,183)
         mat(k,1970) = -rxt(k,611)*y(k,183)
         mat(k,529) = -rxt(k,612)*y(k,183)
         mat(k,2181) = -rxt(k,613)*y(k,183)
         mat(k,2489) = rxt(k,601)*y(k,169)
         mat(k,1790) = mat(k,1790) + rxt(k,603)*y(k,181)
         mat(k,1970) = mat(k,1970) + rxt(k,605)*y(k,181)
         mat(k,423) = rxt(k,601)*y(k,164)
         mat(k,843) = rxt(k,603)*y(k,165) + rxt(k,605)*y(k,166) + rxt(k,608)*y(k,259)
         mat(k,2181) = mat(k,2181) + rxt(k,608)*y(k,181)
         mat(k,1165) = -(rxt(k,604)*y(k,259))
         mat(k,2152) = -rxt(k,604)*y(k,184)
         mat(k,2444) = rxt(k,595)*y(k,85) + rxt(k,606)*y(k,183)
         mat(k,2261) = rxt(k,597)*y(k,85)
         mat(k,2000) = rxt(k,607)*y(k,183)
         mat(k,1145) = rxt(k,595)*y(k,23) + rxt(k,597)*y(k,72) + rxt(k,598)*y(k,127) &
                      + rxt(k,599)*y(k,157) + (rxt(k,600)+.500_r8*rxt(k,614))*y(k,259)
         mat(k,2742) = rxt(k,598)*y(k,85)
         mat(k,2675) = rxt(k,609)*y(k,183)
         mat(k,2787) = rxt(k,599)*y(k,85)
         mat(k,1786) = rxt(k,610)*y(k,183)
         mat(k,1949) = rxt(k,611)*y(k,183)
         mat(k,528) = rxt(k,612)*y(k,183)
         mat(k,421) = rxt(k,602)*y(k,259)
         mat(k,1638) = rxt(k,606)*y(k,23) + rxt(k,607)*y(k,76) + rxt(k,609)*y(k,156) &
                      + rxt(k,610)*y(k,165) + rxt(k,611)*y(k,166) + rxt(k,612) &
                      *y(k,168) + rxt(k,613)*y(k,259)
         mat(k,2152) = mat(k,2152) + (rxt(k,600)+.500_r8*rxt(k,614))*y(k,85) &
                      + rxt(k,602)*y(k,169) + rxt(k,613)*y(k,183)
         mat(k,302) = -(rxt(k,615)*y(k,272))
         mat(k,2841) = -rxt(k,615)*y(k,185)
         mat(k,1164) = rxt(k,604)*y(k,259)
         mat(k,2066) = rxt(k,604)*y(k,184)
         mat(k,1061) = .2202005_r8*rxt(k,668)*y(k,166)
         mat(k,1033) = .0508005_r8*rxt(k,684)*y(k,166)
         mat(k,2515) = .1279005_r8*rxt(k,667)*y(k,223) + .0097005_r8*rxt(k,672) &
                      *y(k,225) + .0003005_r8*rxt(k,675)*y(k,242) &
                      + .1056005_r8*rxt(k,679)*y(k,243) + .0245005_r8*rxt(k,683) &
                      *y(k,249) + .0154005_r8*rxt(k,689)*y(k,267) &
                      + .0063005_r8*rxt(k,693)*y(k,270)
         mat(k,1925) = .2202005_r8*rxt(k,668)*y(k,8) + .0508005_r8*rxt(k,684)*y(k,141)
         mat(k,52) = .5931005_r8*rxt(k,686)*y(k,259)
         mat(k,58) = .1279005_r8*rxt(k,667)*y(k,155) + .2202005_r8*rxt(k,666)*y(k,237)
         mat(k,64) = .0097005_r8*rxt(k,672)*y(k,155) + .0023005_r8*rxt(k,671)*y(k,237)
         mat(k,2324) = .2202005_r8*rxt(k,666)*y(k,223) + .0023005_r8*rxt(k,671) &
                      *y(k,225) + .0031005_r8*rxt(k,674)*y(k,242) &
                      + .2381005_r8*rxt(k,678)*y(k,243) + .0508005_r8*rxt(k,682) &
                      *y(k,249) + .1364005_r8*rxt(k,688)*y(k,267) &
                      + .1677005_r8*rxt(k,692)*y(k,270)
         mat(k,70) = .0003005_r8*rxt(k,675)*y(k,155) + .0031005_r8*rxt(k,674)*y(k,237)
         mat(k,76) = .1056005_r8*rxt(k,679)*y(k,155) + .2381005_r8*rxt(k,678)*y(k,237)
         mat(k,84) = .0245005_r8*rxt(k,683)*y(k,155) + .0508005_r8*rxt(k,682)*y(k,237)
         mat(k,2024) = .5931005_r8*rxt(k,686)*y(k,205)
         mat(k,90) = .0154005_r8*rxt(k,689)*y(k,155) + .1364005_r8*rxt(k,688)*y(k,237)
         mat(k,96) = .0063005_r8*rxt(k,693)*y(k,155) + .1677005_r8*rxt(k,692)*y(k,237)
         mat(k,1062) = .2067005_r8*rxt(k,668)*y(k,166)
         mat(k,1034) = .1149005_r8*rxt(k,684)*y(k,166)
         mat(k,2516) = .1792005_r8*rxt(k,667)*y(k,223) + .0034005_r8*rxt(k,672) &
                      *y(k,225) + .0003005_r8*rxt(k,675)*y(k,242) &
                      + .1026005_r8*rxt(k,679)*y(k,243) + .0082005_r8*rxt(k,683) &
                      *y(k,249) + .0452005_r8*rxt(k,689)*y(k,267) &
                      + .0237005_r8*rxt(k,693)*y(k,270)
         mat(k,1926) = .2067005_r8*rxt(k,668)*y(k,8) + .1149005_r8*rxt(k,684)*y(k,141)
         mat(k,53) = .1534005_r8*rxt(k,686)*y(k,259)
         mat(k,59) = .1792005_r8*rxt(k,667)*y(k,155) + .2067005_r8*rxt(k,666)*y(k,237)
         mat(k,65) = .0034005_r8*rxt(k,672)*y(k,155) + .0008005_r8*rxt(k,671)*y(k,237)
         mat(k,2325) = .2067005_r8*rxt(k,666)*y(k,223) + .0008005_r8*rxt(k,671) &
                      *y(k,225) + .0035005_r8*rxt(k,674)*y(k,242) &
                      + .1308005_r8*rxt(k,678)*y(k,243) + .1149005_r8*rxt(k,682) &
                      *y(k,249) + .0101005_r8*rxt(k,688)*y(k,267) &
                      + .0174005_r8*rxt(k,692)*y(k,270)
         mat(k,71) = .0003005_r8*rxt(k,675)*y(k,155) + .0035005_r8*rxt(k,674)*y(k,237)
         mat(k,77) = .1026005_r8*rxt(k,679)*y(k,155) + .1308005_r8*rxt(k,678)*y(k,237)
         mat(k,85) = .0082005_r8*rxt(k,683)*y(k,155) + .1149005_r8*rxt(k,682)*y(k,237)
         mat(k,2025) = .1534005_r8*rxt(k,686)*y(k,205)
         mat(k,91) = .0452005_r8*rxt(k,689)*y(k,155) + .0101005_r8*rxt(k,688)*y(k,237)
         mat(k,97) = .0237005_r8*rxt(k,693)*y(k,155) + .0174005_r8*rxt(k,692)*y(k,237)
         mat(k,1063) = .0653005_r8*rxt(k,668)*y(k,166)
         mat(k,1035) = .0348005_r8*rxt(k,684)*y(k,166)
         mat(k,2517) = .0676005_r8*rxt(k,667)*y(k,223) + .1579005_r8*rxt(k,672) &
                      *y(k,225) + .0073005_r8*rxt(k,675)*y(k,242) &
                      + .0521005_r8*rxt(k,679)*y(k,243) + .0772005_r8*rxt(k,683) &
                      *y(k,249) + .0966005_r8*rxt(k,689)*y(k,267) &
                      + .0025005_r8*rxt(k,693)*y(k,270)
         mat(k,1927) = .0653005_r8*rxt(k,668)*y(k,8) + .0348005_r8*rxt(k,684)*y(k,141)
         mat(k,54) = .0459005_r8*rxt(k,686)*y(k,259)
         mat(k,60) = .0676005_r8*rxt(k,667)*y(k,155) + .0653005_r8*rxt(k,666)*y(k,237)
         mat(k,66) = .1579005_r8*rxt(k,672)*y(k,155) + .0843005_r8*rxt(k,671)*y(k,237)
         mat(k,2326) = .0653005_r8*rxt(k,666)*y(k,223) + .0843005_r8*rxt(k,671) &
                      *y(k,225) + .0003005_r8*rxt(k,674)*y(k,242) &
                      + .0348005_r8*rxt(k,678)*y(k,243) + .0348005_r8*rxt(k,682) &
                      *y(k,249) + .0763005_r8*rxt(k,688)*y(k,267) + .086_r8*rxt(k,692) &
                      *y(k,270)
         mat(k,72) = .0073005_r8*rxt(k,675)*y(k,155) + .0003005_r8*rxt(k,674)*y(k,237)
         mat(k,78) = .0521005_r8*rxt(k,679)*y(k,155) + .0348005_r8*rxt(k,678)*y(k,237)
         mat(k,86) = .0772005_r8*rxt(k,683)*y(k,155) + .0348005_r8*rxt(k,682)*y(k,237)
         mat(k,2026) = .0459005_r8*rxt(k,686)*y(k,205)
         mat(k,92) = .0966005_r8*rxt(k,689)*y(k,155) + .0763005_r8*rxt(k,688)*y(k,237)
         mat(k,98) = .0025005_r8*rxt(k,693)*y(k,155) + .086_r8*rxt(k,692)*y(k,237)
         mat(k,1064) = .1749305_r8*rxt(k,665)*y(k,157) + .1284005_r8*rxt(k,668) &
                      *y(k,166)
         mat(k,954) = .0590245_r8*rxt(k,673)*y(k,157) + .0033005_r8*rxt(k,676) &
                      *y(k,166)
         mat(k,1036) = .1749305_r8*rxt(k,681)*y(k,157) + .0554005_r8*rxt(k,684) &
                      *y(k,166)
         mat(k,2518) = .079_r8*rxt(k,667)*y(k,223) + .0059005_r8*rxt(k,672)*y(k,225) &
                      + .0057005_r8*rxt(k,675)*y(k,242) + .0143005_r8*rxt(k,679) &
                      *y(k,243) + .0332005_r8*rxt(k,683)*y(k,249) &
                      + .0073005_r8*rxt(k,689)*y(k,267) + .011_r8*rxt(k,693)*y(k,270)
         mat(k,2768) = .1749305_r8*rxt(k,665)*y(k,8) + .0590245_r8*rxt(k,673)*y(k,129) &
                      + .1749305_r8*rxt(k,681)*y(k,141)
         mat(k,1928) = .1284005_r8*rxt(k,668)*y(k,8) + .0033005_r8*rxt(k,676)*y(k,129) &
                      + .0554005_r8*rxt(k,684)*y(k,141)
         mat(k,55) = .0085005_r8*rxt(k,686)*y(k,259)
         mat(k,61) = .079_r8*rxt(k,667)*y(k,155) + .1284005_r8*rxt(k,666)*y(k,237)
         mat(k,67) = .0059005_r8*rxt(k,672)*y(k,155) + .0443005_r8*rxt(k,671)*y(k,237)
         mat(k,2327) = .1284005_r8*rxt(k,666)*y(k,223) + .0443005_r8*rxt(k,671) &
                      *y(k,225) + .0271005_r8*rxt(k,674)*y(k,242) &
                      + .0076005_r8*rxt(k,678)*y(k,243) + .0554005_r8*rxt(k,682) &
                      *y(k,249) + .2157005_r8*rxt(k,688)*y(k,267) &
                      + .0512005_r8*rxt(k,692)*y(k,270)
         mat(k,73) = .0057005_r8*rxt(k,675)*y(k,155) + .0271005_r8*rxt(k,674)*y(k,237)
         mat(k,79) = .0143005_r8*rxt(k,679)*y(k,155) + .0076005_r8*rxt(k,678)*y(k,237)
         mat(k,87) = .0332005_r8*rxt(k,683)*y(k,155) + .0554005_r8*rxt(k,682)*y(k,237)
         mat(k,2027) = .0085005_r8*rxt(k,686)*y(k,205)
         mat(k,93) = .0073005_r8*rxt(k,689)*y(k,155) + .2157005_r8*rxt(k,688)*y(k,237)
         mat(k,99) = .011_r8*rxt(k,693)*y(k,155) + .0512005_r8*rxt(k,692)*y(k,237)
         mat(k,1065) = .5901905_r8*rxt(k,665)*y(k,157) + .114_r8*rxt(k,668)*y(k,166)
         mat(k,955) = .0250245_r8*rxt(k,673)*y(k,157)
         mat(k,1037) = .5901905_r8*rxt(k,681)*y(k,157) + .1278005_r8*rxt(k,684) &
                      *y(k,166)
         mat(k,2519) = .1254005_r8*rxt(k,667)*y(k,223) + .0536005_r8*rxt(k,672) &
                      *y(k,225) + .0623005_r8*rxt(k,675)*y(k,242) &
                      + .0166005_r8*rxt(k,679)*y(k,243) + .130_r8*rxt(k,683)*y(k,249) &
                      + .238_r8*rxt(k,689)*y(k,267) + .1185005_r8*rxt(k,693)*y(k,270)
         mat(k,2769) = .5901905_r8*rxt(k,665)*y(k,8) + .0250245_r8*rxt(k,673)*y(k,129) &
                      + .5901905_r8*rxt(k,681)*y(k,141)
         mat(k,1929) = .114_r8*rxt(k,668)*y(k,8) + .1278005_r8*rxt(k,684)*y(k,141)
         mat(k,56) = .0128005_r8*rxt(k,686)*y(k,259)
         mat(k,62) = .1254005_r8*rxt(k,667)*y(k,155) + .114_r8*rxt(k,666)*y(k,237)
         mat(k,68) = .0536005_r8*rxt(k,672)*y(k,155) + .1621005_r8*rxt(k,671)*y(k,237)
         mat(k,2328) = .114_r8*rxt(k,666)*y(k,223) + .1621005_r8*rxt(k,671)*y(k,225) &
                      + .0474005_r8*rxt(k,674)*y(k,242) + .0113005_r8*rxt(k,678) &
                      *y(k,243) + .1278005_r8*rxt(k,682)*y(k,249) &
                      + .0738005_r8*rxt(k,688)*y(k,267) + .1598005_r8*rxt(k,692) &
                      *y(k,270)
         mat(k,74) = .0623005_r8*rxt(k,675)*y(k,155) + .0474005_r8*rxt(k,674)*y(k,237)
         mat(k,80) = .0166005_r8*rxt(k,679)*y(k,155) + .0113005_r8*rxt(k,678)*y(k,237)
         mat(k,88) = .130_r8*rxt(k,683)*y(k,155) + .1278005_r8*rxt(k,682)*y(k,237)
         mat(k,2028) = .0128005_r8*rxt(k,686)*y(k,205)
         mat(k,94) = .238_r8*rxt(k,689)*y(k,155) + .0738005_r8*rxt(k,688)*y(k,237)
         mat(k,100) = .1185005_r8*rxt(k,693)*y(k,155) + .1598005_r8*rxt(k,692) &
                      *y(k,237)
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
         mat(k,57) = -(rxt(k,686)*y(k,259))
         mat(k,2029) = -rxt(k,686)*y(k,205)
         mat(k,229) = .100_r8*rxt(k,562)*y(k,259)
         mat(k,247) = .230_r8*rxt(k,564)*y(k,259)
         mat(k,2053) = .100_r8*rxt(k,562)*y(k,213) + .230_r8*rxt(k,564)*y(k,215)
         mat(k,729) = -(rxt(k,586)*y(k,259))
         mat(k,2120) = -rxt(k,586)*y(k,207)
         mat(k,2367) = rxt(k,584)*y(k,264)
         mat(k,1293) = rxt(k,584)*y(k,237)
         mat(k,715) = -(rxt(k,587)*y(k,259))
         mat(k,2118) = -rxt(k,587)*y(k,208)
         mat(k,2548) = .200_r8*rxt(k,580)*y(k,254) + .200_r8*rxt(k,590)*y(k,265)
         mat(k,1724) = .500_r8*rxt(k,578)*y(k,254)
         mat(k,1232) = .200_r8*rxt(k,580)*y(k,155) + .500_r8*rxt(k,578)*y(k,231)
         mat(k,1312) = .200_r8*rxt(k,590)*y(k,155)
         mat(k,565) = -(rxt(k,591)*y(k,259))
         mat(k,2101) = -rxt(k,591)*y(k,209)
         mat(k,2359) = rxt(k,589)*y(k,265)
         mat(k,1311) = rxt(k,589)*y(k,237)
         mat(k,1156) = -(rxt(k,592)*y(k,157) + rxt(k,593)*y(k,259))
         mat(k,2786) = -rxt(k,592)*y(k,210)
         mat(k,2151) = -rxt(k,593)*y(k,210)
         mat(k,1074) = .330_r8*rxt(k,573)*y(k,166)
         mat(k,1046) = .330_r8*rxt(k,576)*y(k,166)
         mat(k,2570) = .800_r8*rxt(k,580)*y(k,254) + .800_r8*rxt(k,590)*y(k,265)
         mat(k,2786) = mat(k,2786) + rxt(k,581)*y(k,254)
         mat(k,1948) = .330_r8*rxt(k,573)*y(k,8) + .330_r8*rxt(k,576)*y(k,141)
         mat(k,716) = rxt(k,587)*y(k,259)
         mat(k,1732) = .500_r8*rxt(k,578)*y(k,254) + rxt(k,588)*y(k,265)
         mat(k,1234) = .800_r8*rxt(k,580)*y(k,155) + rxt(k,581)*y(k,157) &
                      + .500_r8*rxt(k,578)*y(k,231)
         mat(k,2151) = mat(k,2151) + rxt(k,587)*y(k,208)
         mat(k,1315) = .800_r8*rxt(k,590)*y(k,155) + rxt(k,588)*y(k,231)
         mat(k,1248) = -(rxt(k,594)*y(k,259))
         mat(k,2158) = -rxt(k,594)*y(k,211)
         mat(k,1076) = .300_r8*rxt(k,573)*y(k,166)
         mat(k,1048) = .300_r8*rxt(k,576)*y(k,166)
         mat(k,2575) = .900_r8*rxt(k,585)*y(k,264)
         mat(k,1953) = .300_r8*rxt(k,573)*y(k,8) + .300_r8*rxt(k,576)*y(k,141)
         mat(k,1737) = rxt(k,583)*y(k,264)
         mat(k,1296) = .900_r8*rxt(k,585)*y(k,155) + rxt(k,583)*y(k,231)
         mat(k,742) = -(rxt(k,561)*y(k,259))
         mat(k,2121) = -rxt(k,561)*y(k,212)
         mat(k,2368) = rxt(k,559)*y(k,266)
         mat(k,830) = rxt(k,559)*y(k,237)
         mat(k,227) = -(rxt(k,562)*y(k,259))
         mat(k,2051) = -rxt(k,562)*y(k,213)
         mat(k,243) = -(rxt(k,528)*y(k,259))
         mat(k,2054) = -rxt(k,528)*y(k,214)
         mat(k,2337) = rxt(k,525)*y(k,268)
         mat(k,1361) = rxt(k,525)*y(k,237)
         mat(k,248) = -(rxt(k,564)*y(k,259))
         mat(k,2055) = -rxt(k,564)*y(k,215)
         mat(k,810) = -(rxt(k,567)*y(k,259))
         mat(k,2127) = -rxt(k,567)*y(k,216)
         mat(k,2373) = rxt(k,565)*y(k,269)
         mat(k,853) = rxt(k,565)*y(k,237)
         mat(k,256) = -(rxt(k,570)*y(k,259))
         mat(k,2056) = -rxt(k,570)*y(k,217)
         mat(k,249) = .150_r8*rxt(k,564)*y(k,259)
         mat(k,2056) = mat(k,2056) + .150_r8*rxt(k,564)*y(k,215)
         mat(k,494) = -(rxt(k,571)*y(k,259))
         mat(k,2092) = -rxt(k,571)*y(k,218)
         mat(k,2350) = rxt(k,568)*y(k,271)
         mat(k,588) = rxt(k,568)*y(k,237)
         mat(k,611) = -(rxt(k,529)*y(k,237) + rxt(k,530)*y(k,155) + rxt(k,558) &
                      *y(k,156))
         mat(k,2362) = -rxt(k,529)*y(k,221)
         mat(k,2542) = -rxt(k,530)*y(k,221)
         mat(k,2663) = -rxt(k,558)*y(k,221)
         mat(k,287) = rxt(k,535)*y(k,259)
         mat(k,2106) = rxt(k,535)*y(k,26)
         mat(k,1094) = -(rxt(k,490)*y(k,237) + (rxt(k,491) + rxt(k,492)) * y(k,155))
         mat(k,2385) = -rxt(k,490)*y(k,222)
         mat(k,2566) = -(rxt(k,491) + rxt(k,492)) * y(k,222)
         mat(k,756) = rxt(k,493)*y(k,259)
         mat(k,284) = rxt(k,494)*y(k,259)
         mat(k,2145) = rxt(k,493)*y(k,2) + rxt(k,494)*y(k,17)
         mat(k,63) = -(rxt(k,666)*y(k,237) + rxt(k,667)*y(k,155))
         mat(k,2329) = -rxt(k,666)*y(k,223)
         mat(k,2520) = -rxt(k,667)*y(k,223)
         mat(k,1066) = rxt(k,669)*y(k,259)
         mat(k,2030) = rxt(k,669)*y(k,8)
         mat(k,574) = -(rxt(k,532)*y(k,237) + rxt(k,533)*y(k,155))
         mat(k,2360) = -rxt(k,532)*y(k,224)
         mat(k,2539) = -rxt(k,533)*y(k,224)
         mat(k,195) = .350_r8*rxt(k,531)*y(k,259)
         mat(k,478) = rxt(k,534)*y(k,259)
         mat(k,2102) = .350_r8*rxt(k,531)*y(k,9) + rxt(k,534)*y(k,10)
         mat(k,69) = -(rxt(k,671)*y(k,237) + rxt(k,672)*y(k,155))
         mat(k,2330) = -rxt(k,671)*y(k,225)
         mat(k,2521) = -rxt(k,672)*y(k,225)
         mat(k,191) = rxt(k,670)*y(k,259)
         mat(k,2031) = rxt(k,670)*y(k,9)
         mat(k,502) = -(rxt(k,536)*y(k,237) + rxt(k,538)*y(k,155))
         mat(k,2351) = -rxt(k,536)*y(k,226)
         mat(k,2533) = -rxt(k,538)*y(k,226)
         mat(k,380) = rxt(k,537)*y(k,259)
         mat(k,230) = .070_r8*rxt(k,562)*y(k,259)
         mat(k,250) = .060_r8*rxt(k,564)*y(k,259)
         mat(k,2093) = rxt(k,537)*y(k,27) + .070_r8*rxt(k,562)*y(k,213) &
                      + .060_r8*rxt(k,564)*y(k,215)
         mat(k,1010) = -(4._r8*rxt(k,413)*y(k,227) + rxt(k,414)*y(k,231) + rxt(k,415) &
                      *y(k,237) + rxt(k,416)*y(k,155))
         mat(k,1729) = -rxt(k,414)*y(k,227)
         mat(k,2382) = -rxt(k,415)*y(k,227)
         mat(k,2563) = -rxt(k,416)*y(k,227)
         mat(k,385) = .500_r8*rxt(k,418)*y(k,259)
         mat(k,340) = rxt(k,419)*y(k,72) + rxt(k,420)*y(k,259)
         mat(k,2256) = rxt(k,419)*y(k,34)
         mat(k,2140) = .500_r8*rxt(k,418)*y(k,33) + rxt(k,420)*y(k,34)
         mat(k,908) = -(rxt(k,442)*y(k,231) + rxt(k,443)*y(k,237) + rxt(k,444) &
                      *y(k,155))
         mat(k,1727) = -rxt(k,442)*y(k,228)
         mat(k,2380) = -rxt(k,443)*y(k,228)
         mat(k,2558) = -rxt(k,444)*y(k,228)
         mat(k,471) = rxt(k,445)*y(k,259)
         mat(k,133) = rxt(k,446)*y(k,259)
         mat(k,2136) = rxt(k,445)*y(k,36) + rxt(k,446)*y(k,37)
         mat(k,723) = -(rxt(k,539)*y(k,237) + rxt(k,540)*y(k,155))
         mat(k,2366) = -rxt(k,539)*y(k,229)
         mat(k,2549) = -rxt(k,540)*y(k,229)
         mat(k,312) = rxt(k,541)*y(k,259)
         mat(k,2549) = mat(k,2549) + rxt(k,530)*y(k,221)
         mat(k,1937) = rxt(k,556)*y(k,174)
         mat(k,545) = rxt(k,556)*y(k,166)
         mat(k,612) = rxt(k,530)*y(k,155) + .400_r8*rxt(k,529)*y(k,237)
         mat(k,2366) = mat(k,2366) + .400_r8*rxt(k,529)*y(k,221)
         mat(k,2119) = rxt(k,541)*y(k,38)
         mat(k,1563) = -(4._r8*rxt(k,424)*y(k,230) + rxt(k,425)*y(k,231) + rxt(k,426) &
                      *y(k,237) + rxt(k,427)*y(k,155) + rxt(k,438)*y(k,156) + rxt(k,465) &
                      *y(k,244) + rxt(k,498)*y(k,239) + rxt(k,503)*y(k,240) + rxt(k,512) &
                      *y(k,241) + rxt(k,523)*y(k,268))
         mat(k,1753) = -rxt(k,425)*y(k,230)
         mat(k,2409) = -rxt(k,426)*y(k,230)
         mat(k,2592) = -rxt(k,427)*y(k,230)
         mat(k,2680) = -rxt(k,438)*y(k,230)
         mat(k,1493) = -rxt(k,465)*y(k,230)
         mat(k,1438) = -rxt(k,498)*y(k,230)
         mat(k,1471) = -rxt(k,503)*y(k,230)
         mat(k,1391) = -rxt(k,512)*y(k,230)
         mat(k,1369) = -rxt(k,523)*y(k,230)
         mat(k,1081) = .060_r8*rxt(k,573)*y(k,166)
         mat(k,1224) = rxt(k,421)*y(k,157) + rxt(k,422)*y(k,259)
         mat(k,1416) = rxt(k,447)*y(k,157) + rxt(k,448)*y(k,259)
         mat(k,676) = .500_r8*rxt(k,429)*y(k,259)
         mat(k,966) = .080_r8*rxt(k,518)*y(k,166)
         mat(k,1407) = .100_r8*rxt(k,471)*y(k,166)
         mat(k,1053) = .060_r8*rxt(k,576)*y(k,166)
         mat(k,1513) = .280_r8*rxt(k,485)*y(k,166)
         mat(k,2592) = mat(k,2592) + .530_r8*rxt(k,469)*y(k,244) + rxt(k,478)*y(k,246) &
                      + rxt(k,481)*y(k,248) + rxt(k,456)*y(k,263)
         mat(k,2811) = rxt(k,421)*y(k,56) + rxt(k,447)*y(k,60) + .530_r8*rxt(k,468) &
                      *y(k,244) + rxt(k,479)*y(k,246)
         mat(k,1968) = .060_r8*rxt(k,573)*y(k,8) + .080_r8*rxt(k,518)*y(k,129) &
                      + .100_r8*rxt(k,471)*y(k,136) + .060_r8*rxt(k,576)*y(k,141) &
                      + .280_r8*rxt(k,485)*y(k,142)
         mat(k,1251) = .650_r8*rxt(k,594)*y(k,259)
         mat(k,1563) = mat(k,1563) + .530_r8*rxt(k,465)*y(k,244)
         mat(k,1753) = mat(k,1753) + .260_r8*rxt(k,466)*y(k,244) + rxt(k,475)*y(k,246) &
                      + .300_r8*rxt(k,454)*y(k,263)
         mat(k,2409) = mat(k,2409) + .450_r8*rxt(k,476)*y(k,246) + .200_r8*rxt(k,480) &
                      *y(k,248) + .150_r8*rxt(k,455)*y(k,263)
         mat(k,1493) = mat(k,1493) + .530_r8*rxt(k,469)*y(k,155) + .530_r8*rxt(k,468) &
                      *y(k,157) + .530_r8*rxt(k,465)*y(k,230) + .260_r8*rxt(k,466) &
                      *y(k,231)
         mat(k,1533) = rxt(k,478)*y(k,155) + rxt(k,479)*y(k,157) + rxt(k,475)*y(k,231) &
                      + .450_r8*rxt(k,476)*y(k,237) + 4.000_r8*rxt(k,477)*y(k,246)
         mat(k,777) = rxt(k,481)*y(k,155) + .200_r8*rxt(k,480)*y(k,237)
         mat(k,2176) = rxt(k,422)*y(k,56) + rxt(k,448)*y(k,60) + .500_r8*rxt(k,429) &
                      *y(k,62) + .650_r8*rxt(k,594)*y(k,211)
         mat(k,1338) = rxt(k,456)*y(k,155) + .300_r8*rxt(k,454)*y(k,231) &
                      + .150_r8*rxt(k,455)*y(k,237)
         mat(k,1756) = -(rxt(k,253)*y(k,76) + (rxt(k,372) + rxt(k,373)) * y(k,70) &
                      + (4._r8*rxt(k,392) + 4._r8*rxt(k,393)) * y(k,231) + rxt(k,394) &
                      *y(k,237) + rxt(k,395)*y(k,155) + rxt(k,414)*y(k,227) + rxt(k,425) &
                      *y(k,230) + rxt(k,442)*y(k,228) + rxt(k,454)*y(k,263) + rxt(k,466) &
                      *y(k,244) + rxt(k,475)*y(k,246) + rxt(k,499)*y(k,239) + rxt(k,504) &
                      *y(k,240) + rxt(k,513)*y(k,241) + rxt(k,524)*y(k,268) + rxt(k,578) &
                      *y(k,254) + rxt(k,583)*y(k,264) + rxt(k,588)*y(k,265))
         mat(k,2002) = -rxt(k,253)*y(k,231)
         mat(k,1174) = -(rxt(k,372) + rxt(k,373)) * y(k,231)
         mat(k,2417) = -rxt(k,394)*y(k,231)
         mat(k,2598) = -rxt(k,395)*y(k,231)
         mat(k,1012) = -rxt(k,414)*y(k,231)
         mat(k,1566) = -rxt(k,425)*y(k,231)
         mat(k,911) = -rxt(k,442)*y(k,231)
         mat(k,1339) = -rxt(k,454)*y(k,231)
         mat(k,1495) = -rxt(k,466)*y(k,231)
         mat(k,1535) = -rxt(k,475)*y(k,231)
         mat(k,1440) = -rxt(k,499)*y(k,231)
         mat(k,1473) = -rxt(k,504)*y(k,231)
         mat(k,1393) = -rxt(k,513)*y(k,231)
         mat(k,1371) = -rxt(k,524)*y(k,231)
         mat(k,1239) = -rxt(k,578)*y(k,231)
         mat(k,1303) = -rxt(k,583)*y(k,231)
         mat(k,1324) = -rxt(k,588)*y(k,231)
         mat(k,1212) = .280_r8*rxt(k,441)*y(k,166)
         mat(k,795) = rxt(k,428)*y(k,259)
         mat(k,489) = .700_r8*rxt(k,397)*y(k,259)
         mat(k,1659) = rxt(k,245)*y(k,72) + rxt(k,346)*y(k,91) + rxt(k,404)*y(k,255) &
                      + rxt(k,398)*y(k,259)
         mat(k,2272) = rxt(k,245)*y(k,66)
         mat(k,1001) = rxt(k,346)*y(k,66)
         mat(k,967) = .050_r8*rxt(k,518)*y(k,166)
         mat(k,2598) = mat(k,2598) + rxt(k,427)*y(k,230) + .830_r8*rxt(k,544)*y(k,232) &
                      + .170_r8*rxt(k,550)*y(k,247)
         mat(k,1972) = .280_r8*rxt(k,441)*y(k,35) + .050_r8*rxt(k,518)*y(k,129)
         mat(k,1566) = mat(k,1566) + rxt(k,427)*y(k,155) + 4.000_r8*rxt(k,424) &
                      *y(k,230) + .900_r8*rxt(k,425)*y(k,231) + .450_r8*rxt(k,426) &
                      *y(k,237) + rxt(k,498)*y(k,239) + rxt(k,503)*y(k,240) &
                      + rxt(k,512)*y(k,241) + rxt(k,465)*y(k,244) + rxt(k,474) &
                      *y(k,246) + rxt(k,523)*y(k,268)
         mat(k,1756) = mat(k,1756) + .900_r8*rxt(k,425)*y(k,230)
         mat(k,869) = .830_r8*rxt(k,544)*y(k,155) + .330_r8*rxt(k,543)*y(k,237)
         mat(k,2417) = mat(k,2417) + .450_r8*rxt(k,426)*y(k,230) + .330_r8*rxt(k,543) &
                      *y(k,232) + .070_r8*rxt(k,549)*y(k,247)
         mat(k,1440) = mat(k,1440) + rxt(k,498)*y(k,230)
         mat(k,1473) = mat(k,1473) + rxt(k,503)*y(k,230)
         mat(k,1393) = mat(k,1393) + rxt(k,512)*y(k,230)
         mat(k,1495) = mat(k,1495) + rxt(k,465)*y(k,230)
         mat(k,1535) = mat(k,1535) + rxt(k,474)*y(k,230)
         mat(k,1023) = .170_r8*rxt(k,550)*y(k,155) + .070_r8*rxt(k,549)*y(k,237)
         mat(k,1879) = rxt(k,404)*y(k,66)
         mat(k,2185) = rxt(k,428)*y(k,61) + .700_r8*rxt(k,397)*y(k,65) + rxt(k,398) &
                      *y(k,66)
         mat(k,1371) = mat(k,1371) + rxt(k,523)*y(k,230)
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
         mat(k,866) = -(rxt(k,543)*y(k,237) + rxt(k,544)*y(k,155) + rxt(k,545) &
                      *y(k,156))
         mat(k,2377) = -rxt(k,543)*y(k,232)
         mat(k,2555) = -rxt(k,544)*y(k,232)
         mat(k,2668) = -rxt(k,545)*y(k,232)
         mat(k,940) = -(rxt(k,736)*y(k,252) + rxt(k,737)*y(k,258) + rxt(k,738) &
                      *y(k,251))
         mat(k,921) = -rxt(k,736)*y(k,233)
         mat(k,929) = -rxt(k,737)*y(k,233)
         mat(k,787) = -rxt(k,738)*y(k,233)
         mat(k,654) = -((rxt(k,462) + rxt(k,463)) * y(k,155))
         mat(k,2544) = -(rxt(k,462) + rxt(k,463)) * y(k,234)
         mat(k,412) = rxt(k,461)*y(k,259)
         mat(k,2111) = rxt(k,461)*y(k,18)
         mat(k,535) = -(rxt(k,433)*y(k,165))
         mat(k,1776) = -rxt(k,433)*y(k,235)
         mat(k,2537) = .750_r8*rxt(k,431)*y(k,236)
         mat(k,875) = .750_r8*rxt(k,431)*y(k,155)
         mat(k,876) = -(rxt(k,430)*y(k,237) + rxt(k,431)*y(k,155))
         mat(k,2378) = -rxt(k,430)*y(k,236)
         mat(k,2556) = -rxt(k,431)*y(k,236)
         mat(k,639) = rxt(k,437)*y(k,259)
         mat(k,2133) = rxt(k,437)*y(k,30)
         mat(k,2429) = -((rxt(k,202) + rxt(k,203) + rxt(k,204)) * y(k,94) + rxt(k,206) &
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
         mat(k,1915) = -(rxt(k,202) + rxt(k,203) + rxt(k,204)) * y(k,237)
         mat(k,2505) = -rxt(k,206)*y(k,237)
         mat(k,1984) = -rxt(k,207)*y(k,237)
         mat(k,2197) = -rxt(k,211)*y(k,237)
         mat(k,2830) = -rxt(k,228)*y(k,237)
         mat(k,2610) = -rxt(k,233)*y(k,237)
         mat(k,2699) = -rxt(k,238)*y(k,237)
         mat(k,2284) = -(rxt(k,248) + rxt(k,249)) * y(k,237)
         mat(k,2014) = -rxt(k,257)*y(k,237)
         mat(k,2314) = -rxt(k,284)*y(k,237)
         mat(k,2458) = -rxt(k,291)*y(k,237)
         mat(k,2227) = -rxt(k,313)*y(k,237)
         mat(k,2758) = -rxt(k,328)*y(k,237)
         mat(k,1178) = -rxt(k,374)*y(k,237)
         mat(k,2641) = -rxt(k,388)*y(k,237)
         mat(k,1766) = -rxt(k,394)*y(k,237)
         mat(k,510) = -rxt(k,401)*y(k,237)
         mat(k,1015) = -rxt(k,415)*y(k,237)
         mat(k,1570) = -rxt(k,426)*y(k,237)
         mat(k,880) = -rxt(k,430)*y(k,237)
         mat(k,914) = -rxt(k,443)*y(k,237)
         mat(k,902) = -rxt(k,451)*y(k,237)
         mat(k,1341) = -rxt(k,455)*y(k,237)
         mat(k,1497) = -rxt(k,467)*y(k,237)
         mat(k,1538) = -rxt(k,476)*y(k,237)
         mat(k,779) = -rxt(k,480)*y(k,237)
         mat(k,1101) = -rxt(k,490)*y(k,237)
         mat(k,1443) = -rxt(k,500)*y(k,237)
         mat(k,1476) = -rxt(k,505)*y(k,237)
         mat(k,1395) = -rxt(k,514)*y(k,237)
         mat(k,1373) = -rxt(k,525)*y(k,237)
         mat(k,614) = -rxt(k,529)*y(k,237)
         mat(k,578) = -rxt(k,532)*y(k,237)
         mat(k,505) = -rxt(k,536)*y(k,237)
         mat(k,726) = -rxt(k,539)*y(k,237)
         mat(k,871) = -rxt(k,543)*y(k,237)
         mat(k,824) = -rxt(k,546)*y(k,237)
         mat(k,1025) = -rxt(k,549)*y(k,237)
         mat(k,518) = -rxt(k,552)*y(k,237)
         mat(k,839) = -rxt(k,559)*y(k,237)
         mat(k,863) = -rxt(k,565)*y(k,237)
         mat(k,593) = -rxt(k,568)*y(k,237)
         mat(k,1241) = -rxt(k,579)*y(k,237)
         mat(k,1305) = -rxt(k,584)*y(k,237)
         mat(k,1326) = -rxt(k,589)*y(k,237)
         mat(k,1085) = .570_r8*rxt(k,573)*y(k,166)
         mat(k,197) = .650_r8*rxt(k,531)*y(k,259)
         mat(k,2314) = mat(k,2314) + rxt(k,283)*y(k,53)
         mat(k,2458) = mat(k,2458) + rxt(k,298)*y(k,259)
         mat(k,338) = .350_r8*rxt(k,410)*y(k,259)
         mat(k,644) = .130_r8*rxt(k,412)*y(k,166)
         mat(k,309) = rxt(k,417)*y(k,259)
         mat(k,1216) = .280_r8*rxt(k,441)*y(k,166)
         mat(k,2641) = mat(k,2641) + rxt(k,283)*y(k,19) + rxt(k,244)*y(k,72) &
                      + rxt(k,389)*y(k,157) + rxt(k,390)*y(k,164)
         mat(k,689) = rxt(k,361)*y(k,72) + rxt(k,362)*y(k,259)
         mat(k,444) = rxt(k,364)*y(k,72) + rxt(k,365)*y(k,259)
         mat(k,109) = rxt(k,423)*y(k,259)
         mat(k,436) = rxt(k,366)*y(k,72) + rxt(k,367)*y(k,259)
         mat(k,894) = rxt(k,396)*y(k,259)
         mat(k,1666) = rxt(k,405)*y(k,255)
         mat(k,1178) = mat(k,1178) + rxt(k,376)*y(k,155) + rxt(k,377)*y(k,157) + ( &
                      + 2.000_r8*rxt(k,372)+rxt(k,373))*y(k,231)
         mat(k,2284) = mat(k,2284) + rxt(k,244)*y(k,53) + rxt(k,361)*y(k,54) &
                      + rxt(k,364)*y(k,57) + rxt(k,366)*y(k,63) + rxt(k,247)*y(k,97)
         mat(k,2014) = mat(k,2014) + rxt(k,253)*y(k,231) + rxt(k,264)*y(k,259)
         mat(k,1291) = rxt(k,408)*y(k,259)
         mat(k,238) = .730_r8*rxt(k,542)*y(k,259)
         mat(k,1151) = .500_r8*rxt(k,614)*y(k,259)
         mat(k,1286) = rxt(k,434)*y(k,259)
         mat(k,1116) = rxt(k,435)*y(k,259)
         mat(k,1915) = mat(k,1915) + rxt(k,205)*y(k,165)
         mat(k,704) = rxt(k,247)*y(k,72) + rxt(k,201)*y(k,164) + rxt(k,210)*y(k,259)
         mat(k,217) = rxt(k,399)*y(k,259)
         mat(k,1031) = rxt(k,400)*y(k,259)
         mat(k,1269) = rxt(k,464)*y(k,259)
         mat(k,1279) = rxt(k,449)*y(k,259)
         mat(k,2758) = mat(k,2758) + rxt(k,334)*y(k,259)
         mat(k,970) = .370_r8*rxt(k,518)*y(k,166)
         mat(k,667) = .300_r8*rxt(k,509)*y(k,259)
         mat(k,652) = rxt(k,510)*y(k,259)
         mat(k,525) = rxt(k,517)*y(k,259)
         mat(k,1411) = .140_r8*rxt(k,471)*y(k,166)
         mat(k,355) = .200_r8*rxt(k,473)*y(k,259)
         mat(k,696) = .500_r8*rxt(k,484)*y(k,259)
         mat(k,1057) = .570_r8*rxt(k,576)*y(k,166)
         mat(k,1520) = .280_r8*rxt(k,485)*y(k,166)
         mat(k,450) = rxt(k,521)*y(k,259)
         mat(k,1195) = rxt(k,522)*y(k,259)
         mat(k,2610) = mat(k,2610) + rxt(k,376)*y(k,70) + rxt(k,491)*y(k,222) &
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
         mat(k,2830) = mat(k,2830) + rxt(k,389)*y(k,53) + rxt(k,377)*y(k,70) &
                      + rxt(k,502)*y(k,239) + rxt(k,508)*y(k,240) + rxt(k,516) &
                      *y(k,241) + .470_r8*rxt(k,468)*y(k,244) + rxt(k,231)*y(k,259) &
                      + rxt(k,527)*y(k,268)
         mat(k,2505) = mat(k,2505) + rxt(k,390)*y(k,53) + rxt(k,201)*y(k,97)
         mat(k,1799) = rxt(k,205)*y(k,94) + rxt(k,433)*y(k,235)
         mat(k,1984) = mat(k,1984) + .570_r8*rxt(k,573)*y(k,8) + .130_r8*rxt(k,412) &
                      *y(k,30) + .280_r8*rxt(k,441)*y(k,35) + .370_r8*rxt(k,518) &
                      *y(k,129) + .140_r8*rxt(k,471)*y(k,136) + .570_r8*rxt(k,576) &
                      *y(k,141) + .280_r8*rxt(k,485)*y(k,142) + rxt(k,213)*y(k,259)
         mat(k,206) = .800_r8*rxt(k,554)*y(k,259)
         mat(k,1168) = rxt(k,604)*y(k,259)
         mat(k,1255) = .200_r8*rxt(k,594)*y(k,259)
         mat(k,233) = .280_r8*rxt(k,562)*y(k,259)
         mat(k,255) = .380_r8*rxt(k,564)*y(k,259)
         mat(k,260) = .630_r8*rxt(k,570)*y(k,259)
         mat(k,1101) = mat(k,1101) + rxt(k,491)*y(k,155)
         mat(k,578) = mat(k,578) + rxt(k,533)*y(k,155)
         mat(k,505) = mat(k,505) + rxt(k,538)*y(k,155)
         mat(k,1015) = mat(k,1015) + rxt(k,416)*y(k,155) + 2.400_r8*rxt(k,413) &
                      *y(k,227) + rxt(k,414)*y(k,231)
         mat(k,914) = mat(k,914) + rxt(k,444)*y(k,155) + rxt(k,442)*y(k,231)
         mat(k,1570) = mat(k,1570) + .900_r8*rxt(k,425)*y(k,231) + rxt(k,498)*y(k,239) &
                      + rxt(k,503)*y(k,240) + rxt(k,512)*y(k,241) + .470_r8*rxt(k,465) &
                      *y(k,244) + rxt(k,523)*y(k,268)
         mat(k,1766) = mat(k,1766) + (2.000_r8*rxt(k,372)+rxt(k,373))*y(k,70) &
                      + rxt(k,253)*y(k,76) + rxt(k,395)*y(k,155) + rxt(k,414)*y(k,227) &
                      + rxt(k,442)*y(k,228) + .900_r8*rxt(k,425)*y(k,230) &
                      + 4.000_r8*rxt(k,392)*y(k,231) + rxt(k,499)*y(k,239) &
                      + rxt(k,504)*y(k,240) + 1.200_r8*rxt(k,513)*y(k,241) &
                      + .730_r8*rxt(k,466)*y(k,244) + rxt(k,475)*y(k,246) &
                      + .500_r8*rxt(k,578)*y(k,254) + .300_r8*rxt(k,454)*y(k,263) &
                      + rxt(k,583)*y(k,264) + rxt(k,588)*y(k,265) + .800_r8*rxt(k,524) &
                      *y(k,268)
         mat(k,871) = mat(k,871) + .170_r8*rxt(k,544)*y(k,155) + .070_r8*rxt(k,543) &
                      *y(k,237)
         mat(k,658) = rxt(k,462)*y(k,155)
         mat(k,538) = rxt(k,433)*y(k,165)
         mat(k,880) = mat(k,880) + .250_r8*rxt(k,431)*y(k,155)
         mat(k,2429) = mat(k,2429) + .070_r8*rxt(k,543)*y(k,232) + .160_r8*rxt(k,546) &
                      *y(k,245) + .330_r8*rxt(k,549)*y(k,247)
         mat(k,510) = mat(k,510) + rxt(k,403)*y(k,155)
         mat(k,1443) = mat(k,1443) + .920_r8*rxt(k,501)*y(k,155) + rxt(k,502)*y(k,157) &
                      + rxt(k,498)*y(k,230) + rxt(k,499)*y(k,231)
         mat(k,1476) = mat(k,1476) + .920_r8*rxt(k,507)*y(k,155) + rxt(k,508)*y(k,157) &
                      + rxt(k,503)*y(k,230) + rxt(k,504)*y(k,231)
         mat(k,1395) = mat(k,1395) + rxt(k,515)*y(k,155) + rxt(k,516)*y(k,157) &
                      + rxt(k,512)*y(k,230) + 1.200_r8*rxt(k,513)*y(k,231)
         mat(k,1497) = mat(k,1497) + .470_r8*rxt(k,469)*y(k,155) + .470_r8*rxt(k,468) &
                      *y(k,157) + .470_r8*rxt(k,465)*y(k,230) + .730_r8*rxt(k,466) &
                      *y(k,231)
         mat(k,824) = mat(k,824) + .400_r8*rxt(k,547)*y(k,155) + .160_r8*rxt(k,546) &
                      *y(k,237)
         mat(k,1538) = mat(k,1538) + rxt(k,475)*y(k,231)
         mat(k,1025) = mat(k,1025) + .830_r8*rxt(k,550)*y(k,155) + .330_r8*rxt(k,549) &
                      *y(k,237)
         mat(k,1241) = mat(k,1241) + .500_r8*rxt(k,578)*y(k,231)
         mat(k,1891) = rxt(k,405)*y(k,66)
         mat(k,2197) = mat(k,2197) + .650_r8*rxt(k,531)*y(k,9) + rxt(k,298)*y(k,23) &
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
         mat(k,518) = mat(k,518) + rxt(k,553)*y(k,155)
         mat(k,902) = mat(k,902) + rxt(k,452)*y(k,155)
         mat(k,1341) = mat(k,1341) + .300_r8*rxt(k,454)*y(k,231)
         mat(k,1305) = mat(k,1305) + .900_r8*rxt(k,585)*y(k,155) + rxt(k,583)*y(k,231)
         mat(k,1326) = mat(k,1326) + .800_r8*rxt(k,590)*y(k,155) + rxt(k,588)*y(k,231)
         mat(k,839) = mat(k,839) + rxt(k,560)*y(k,155)
         mat(k,1373) = mat(k,1373) + rxt(k,526)*y(k,155) + rxt(k,527)*y(k,157) &
                      + rxt(k,523)*y(k,230) + .800_r8*rxt(k,524)*y(k,231)
         mat(k,863) = mat(k,863) + rxt(k,566)*y(k,155)
         mat(k,593) = mat(k,593) + rxt(k,569)*y(k,155)
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
         mat(k,508) = -(rxt(k,401)*y(k,237) + rxt(k,403)*y(k,155))
         mat(k,2352) = -rxt(k,401)*y(k,238)
         mat(k,2534) = -rxt(k,403)*y(k,238)
         mat(k,2620) = rxt(k,388)*y(k,237)
         mat(k,2352) = mat(k,2352) + rxt(k,388)*y(k,53)
         mat(k,1434) = -(rxt(k,498)*y(k,230) + rxt(k,499)*y(k,231) + rxt(k,500) &
                      *y(k,237) + rxt(k,501)*y(k,155) + rxt(k,502)*y(k,157))
         mat(k,1558) = -rxt(k,498)*y(k,239)
         mat(k,1748) = -rxt(k,499)*y(k,239)
         mat(k,2404) = -rxt(k,500)*y(k,239)
         mat(k,2587) = -rxt(k,501)*y(k,239)
         mat(k,2806) = -rxt(k,502)*y(k,239)
         mat(k,963) = .600_r8*rxt(k,519)*y(k,259)
         mat(k,2171) = .600_r8*rxt(k,519)*y(k,129)
         mat(k,1467) = -(rxt(k,503)*y(k,230) + rxt(k,504)*y(k,231) + rxt(k,505) &
                      *y(k,237) + rxt(k,507)*y(k,155) + rxt(k,508)*y(k,157))
         mat(k,1559) = -rxt(k,503)*y(k,240)
         mat(k,1749) = -rxt(k,504)*y(k,240)
         mat(k,2405) = -rxt(k,505)*y(k,240)
         mat(k,2588) = -rxt(k,507)*y(k,240)
         mat(k,2807) = -rxt(k,508)*y(k,240)
         mat(k,964) = .400_r8*rxt(k,519)*y(k,259)
         mat(k,2172) = .400_r8*rxt(k,519)*y(k,129)
         mat(k,1387) = -(rxt(k,512)*y(k,230) + rxt(k,513)*y(k,231) + rxt(k,514) &
                      *y(k,237) + rxt(k,515)*y(k,155) + rxt(k,516)*y(k,157))
         mat(k,1555) = -rxt(k,512)*y(k,241)
         mat(k,1745) = -rxt(k,513)*y(k,241)
         mat(k,2401) = -rxt(k,514)*y(k,241)
         mat(k,2584) = -rxt(k,515)*y(k,241)
         mat(k,2803) = -rxt(k,516)*y(k,241)
         mat(k,961) = rxt(k,511)*y(k,157)
         mat(k,2803) = mat(k,2803) + rxt(k,511)*y(k,129)
         mat(k,75) = -(rxt(k,674)*y(k,237) + rxt(k,675)*y(k,155))
         mat(k,2331) = -rxt(k,674)*y(k,242)
         mat(k,2522) = -rxt(k,675)*y(k,242)
         mat(k,956) = rxt(k,677)*y(k,259)
         mat(k,2032) = rxt(k,677)*y(k,129)
         mat(k,81) = -(rxt(k,678)*y(k,237) + rxt(k,679)*y(k,155))
         mat(k,2332) = -rxt(k,678)*y(k,243)
         mat(k,2523) = -rxt(k,679)*y(k,243)
         mat(k,82) = rxt(k,680)*y(k,259)
         mat(k,2033) = rxt(k,680)*y(k,134)
         mat(k,1491) = -(rxt(k,465)*y(k,230) + rxt(k,466)*y(k,231) + rxt(k,467) &
                      *y(k,237) + rxt(k,468)*y(k,157) + (rxt(k,469) + rxt(k,470) &
                      ) * y(k,155))
         mat(k,1560) = -rxt(k,465)*y(k,244)
         mat(k,1750) = -rxt(k,466)*y(k,244)
         mat(k,2406) = -rxt(k,467)*y(k,244)
         mat(k,2808) = -rxt(k,468)*y(k,244)
         mat(k,2589) = -(rxt(k,469) + rxt(k,470)) * y(k,244)
         mat(k,1405) = .500_r8*rxt(k,472)*y(k,259)
         mat(k,352) = .200_r8*rxt(k,473)*y(k,259)
         mat(k,1510) = rxt(k,486)*y(k,259)
         mat(k,2173) = .500_r8*rxt(k,472)*y(k,136) + .200_r8*rxt(k,473)*y(k,137) &
                      + rxt(k,486)*y(k,142)
         mat(k,821) = -(rxt(k,546)*y(k,237) + rxt(k,547)*y(k,155) + rxt(k,548) &
                      *y(k,156))
         mat(k,2374) = -rxt(k,546)*y(k,245)
         mat(k,2552) = -rxt(k,547)*y(k,245)
         mat(k,2667) = -rxt(k,548)*y(k,245)
         mat(k,1532) = -(rxt(k,474)*y(k,230) + rxt(k,475)*y(k,231) + rxt(k,476) &
                      *y(k,237) + 4._r8*rxt(k,477)*y(k,246) + rxt(k,478)*y(k,155) &
                      + rxt(k,479)*y(k,157) + rxt(k,487)*y(k,156))
         mat(k,1562) = -rxt(k,474)*y(k,246)
         mat(k,1752) = -rxt(k,475)*y(k,246)
         mat(k,2408) = -rxt(k,476)*y(k,246)
         mat(k,2591) = -rxt(k,478)*y(k,246)
         mat(k,2810) = -rxt(k,479)*y(k,246)
         mat(k,2679) = -rxt(k,487)*y(k,246)
         mat(k,1406) = .500_r8*rxt(k,472)*y(k,259)
         mat(k,353) = .500_r8*rxt(k,473)*y(k,259)
         mat(k,2175) = .500_r8*rxt(k,472)*y(k,136) + .500_r8*rxt(k,473)*y(k,137)
         mat(k,1019) = -(rxt(k,549)*y(k,237) + rxt(k,550)*y(k,155) + rxt(k,551) &
                      *y(k,156))
         mat(k,2383) = -rxt(k,549)*y(k,247)
         mat(k,2564) = -rxt(k,550)*y(k,247)
         mat(k,2671) = -rxt(k,551)*y(k,247)
         mat(k,775) = -(rxt(k,480)*y(k,237) + rxt(k,481)*y(k,155))
         mat(k,2370) = -rxt(k,480)*y(k,248)
         mat(k,2551) = -rxt(k,481)*y(k,248)
         mat(k,603) = rxt(k,482)*y(k,259)
         mat(k,357) = rxt(k,483)*y(k,259)
         mat(k,2124) = rxt(k,482)*y(k,138) + rxt(k,483)*y(k,139)
         mat(k,89) = -(rxt(k,682)*y(k,237) + rxt(k,683)*y(k,155))
         mat(k,2333) = -rxt(k,682)*y(k,249)
         mat(k,2524) = -rxt(k,683)*y(k,249)
         mat(k,1038) = rxt(k,685)*y(k,259)
         mat(k,2035) = rxt(k,685)*y(k,141)
         mat(k,617) = -(rxt(k,218)*y(k,164) + rxt(k,219)*y(k,165))
         mat(k,2472) = -rxt(k,218)*y(k,250)
         mat(k,1778) = -rxt(k,219)*y(k,250)
         mat(k,2472) = mat(k,2472) + rxt(k,740)*y(k,251)
         mat(k,935) = .900_r8*rxt(k,738)*y(k,251) + .800_r8*rxt(k,736)*y(k,252)
         mat(k,782) = rxt(k,740)*y(k,164) + .900_r8*rxt(k,738)*y(k,233)
         mat(k,919) = .800_r8*rxt(k,736)*y(k,233)
         mat(k,783) = -(rxt(k,738)*y(k,233) + rxt(k,739)*y(k,165) + (rxt(k,740) &
                      + rxt(k,741)) * y(k,164))
         mat(k,936) = -rxt(k,738)*y(k,251)
         mat(k,1779) = -rxt(k,739)*y(k,251)
         mat(k,2474) = -(rxt(k,740) + rxt(k,741)) * y(k,251)
         mat(k,920) = -(rxt(k,736)*y(k,233))
         mat(k,938) = -rxt(k,736)*y(k,252)
         mat(k,1120) = rxt(k,745)*y(k,258)
         mat(k,2559) = rxt(k,747)*y(k,258)
         mat(k,2477) = rxt(k,740)*y(k,251)
         mat(k,1782) = rxt(k,744)*y(k,253)
         mat(k,785) = rxt(k,740)*y(k,164)
         mat(k,583) = rxt(k,744)*y(k,165)
         mat(k,927) = rxt(k,745)*y(k,143) + rxt(k,747)*y(k,155)
         mat(k,581) = -(rxt(k,742)*y(k,164) + (rxt(k,743) + rxt(k,744)) * y(k,165))
         mat(k,2471) = -rxt(k,742)*y(k,253)
         mat(k,1777) = -(rxt(k,743) + rxt(k,744)) * y(k,253)
         mat(k,1235) = -(rxt(k,578)*y(k,231) + rxt(k,579)*y(k,237) + rxt(k,580) &
                      *y(k,155) + rxt(k,581)*y(k,157))
         mat(k,1736) = -rxt(k,578)*y(k,254)
         mat(k,2391) = -rxt(k,579)*y(k,254)
         mat(k,2574) = -rxt(k,580)*y(k,254)
         mat(k,2792) = -rxt(k,581)*y(k,254)
         mat(k,1075) = rxt(k,572)*y(k,157)
         mat(k,1047) = rxt(k,575)*y(k,157)
         mat(k,2792) = mat(k,2792) + rxt(k,572)*y(k,8) + rxt(k,575)*y(k,141) &
                      + .500_r8*rxt(k,592)*y(k,210)
         mat(k,454) = rxt(k,582)*y(k,259)
         mat(k,1157) = .500_r8*rxt(k,592)*y(k,157)
         mat(k,2157) = rxt(k,582)*y(k,159)
         mat(k,1883) = -(rxt(k,183)*y(k,95) + rxt(k,184)*y(k,272) + (rxt(k,186) &
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
         mat(k,1616) = -rxt(k,183)*y(k,255)
         mat(k,2852) = -rxt(k,184)*y(k,255)
         mat(k,1792) = -(rxt(k,186) + rxt(k,187)) * y(k,255)
         mat(k,1976) = -rxt(k,188)*y(k,255)
         mat(k,299) = -(rxt(k,236) + rxt(k,237)) * y(k,255)
         mat(k,114) = -rxt(k,271)*y(k,255)
         mat(k,173) = -rxt(k,272)*y(k,255)
         mat(k,138) = -rxt(k,273)*y(k,255)
         mat(k,184) = -rxt(k,274)*y(k,255)
         mat(k,142) = -rxt(k,275)*y(k,255)
         mat(k,189) = -rxt(k,276)*y(k,255)
         mat(k,146) = -rxt(k,277)*y(k,255)
         mat(k,1815) = -(rxt(k,278) + rxt(k,279)) * y(k,255)
         mat(k,179) = -rxt(k,302)*y(k,255)
         mat(k,484) = -rxt(k,303)*y(k,255)
         mat(k,126) = -rxt(k,304)*y(k,255)
         mat(k,1601) = -(rxt(k,305) + rxt(k,306)) * y(k,255)
         mat(k,277) = -rxt(k,350)*y(k,255)
         mat(k,268) = -rxt(k,351)*y(k,255)
         mat(k,560) = -rxt(k,383)*y(k,255)
         mat(k,685) = -rxt(k,384)*y(k,255)
         mat(k,263) = -rxt(k,385)*y(k,255)
         mat(k,272) = -rxt(k,386)*y(k,255)
         mat(k,329) = -rxt(k,387)*y(k,255)
         mat(k,1662) = -(rxt(k,404) + rxt(k,405) + rxt(k,406)) * y(k,255)
         mat(k,215) = -rxt(k,407)*y(k,255)
         mat(k,1792) = mat(k,1792) + rxt(k,219)*y(k,250)
         mat(k,945) = .850_r8*rxt(k,737)*y(k,258)
         mat(k,620) = rxt(k,219)*y(k,165)
         mat(k,932) = .850_r8*rxt(k,737)*y(k,233)
         mat(k,207) = -(rxt(k,190)*y(k,164) + rxt(k,191)*y(k,165))
         mat(k,2468) = -rxt(k,190)*y(k,256)
         mat(k,1774) = -rxt(k,191)*y(k,256)
         mat(k,1577) = rxt(k,192)*y(k,257)
         mat(k,2468) = mat(k,2468) + rxt(k,194)*y(k,257)
         mat(k,1774) = mat(k,1774) + rxt(k,195)*y(k,257)
         mat(k,1930) = rxt(k,196)*y(k,257)
         mat(k,209) = rxt(k,192)*y(k,80) + rxt(k,194)*y(k,164) + rxt(k,195)*y(k,165) &
                      + rxt(k,196)*y(k,166)
         mat(k,210) = -(rxt(k,192)*y(k,80) + rxt(k,194)*y(k,164) + rxt(k,195)*y(k,165) &
                      + rxt(k,196)*y(k,166))
         mat(k,1578) = -rxt(k,192)*y(k,257)
         mat(k,2469) = -rxt(k,194)*y(k,257)
         mat(k,1775) = -rxt(k,195)*y(k,257)
         mat(k,1931) = -rxt(k,196)*y(k,257)
         mat(k,1775) = mat(k,1775) + rxt(k,186)*y(k,255)
         mat(k,1862) = rxt(k,186)*y(k,165)
         mat(k,928) = -(rxt(k,737)*y(k,233) + rxt(k,745)*y(k,143) + rxt(k,747) &
                      *y(k,155))
         mat(k,939) = -rxt(k,737)*y(k,258)
         mat(k,1121) = -rxt(k,745)*y(k,258)
         mat(k,2560) = -rxt(k,747)*y(k,258)
         mat(k,1581) = rxt(k,748)*y(k,260)
         mat(k,1783) = rxt(k,739)*y(k,251) + rxt(k,743)*y(k,253) + rxt(k,750)*y(k,260)
         mat(k,786) = rxt(k,739)*y(k,165)
         mat(k,584) = rxt(k,743)*y(k,165)
         mat(k,886) = rxt(k,748)*y(k,80) + rxt(k,750)*y(k,165)
         mat(k,2193) = -(rxt(k,209)*y(k,95) + rxt(k,210)*y(k,97) + rxt(k,211)*y(k,237) &
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
         mat(k,1618) = -rxt(k,209)*y(k,259)
         mat(k,702) = -rxt(k,210)*y(k,259)
         mat(k,2425) = -rxt(k,211)*y(k,259)
         mat(k,2501) = -rxt(k,212)*y(k,259)
         mat(k,1980) = -rxt(k,213)*y(k,259)
         mat(k,553) = -rxt(k,217)*y(k,259)
         mat(k,2826) = -rxt(k,231)*y(k,259)
         mat(k,1129) = -rxt(k,232)*y(k,259)
         mat(k,2695) = -rxt(k,240)*y(k,259)
         mat(k,2721) = -rxt(k,241)*y(k,259)
         mat(k,599) = -rxt(k,251)*y(k,259)
         mat(k,1138) = -rxt(k,262)*y(k,259)
         mat(k,2010) = -(rxt(k,264) + rxt(k,265)) * y(k,259)
         mat(k,1818) = -rxt(k,267)*y(k,259)
         mat(k,1842) = -rxt(k,270)*y(k,259)
         mat(k,542) = -rxt(k,282)*y(k,259)
         mat(k,2454) = -rxt(k,298)*y(k,259)
         mat(k,1603) = -rxt(k,300)*y(k,259)
         mat(k,1685) = -rxt(k,308)*y(k,259)
         mat(k,1627) = -rxt(k,311)*y(k,259)
         mat(k,2754) = -rxt(k,334)*y(k,259)
         mat(k,1354) = -rxt(k,335)*y(k,259)
         mat(k,224) = -rxt(k,353)*y(k,259)
         mat(k,294) = -rxt(k,355)*y(k,259)
         mat(k,561) = -rxt(k,357)*y(k,259)
         mat(k,149) = -rxt(k,358)*y(k,259)
         mat(k,348) = -rxt(k,360)*y(k,259)
         mat(k,686) = -rxt(k,362)*y(k,259)
         mat(k,153) = -rxt(k,363)*y(k,259)
         mat(k,442) = -rxt(k,365)*y(k,259)
         mat(k,433) = -rxt(k,367)*y(k,259)
         mat(k,117) = -rxt(k,368)*y(k,259)
         mat(k,485) = -rxt(k,370)*y(k,259)
         mat(k,121) = -rxt(k,371)*y(k,259)
         mat(k,397) = -rxt(k,379)*y(k,259)
         mat(k,264) = -rxt(k,380)*y(k,259)
         mat(k,273) = -rxt(k,381)*y(k,259)
         mat(k,330) = -rxt(k,382)*y(k,259)
         mat(k,2637) = -rxt(k,391)*y(k,259)
         mat(k,893) = -rxt(k,396)*y(k,259)
         mat(k,491) = -rxt(k,397)*y(k,259)
         mat(k,1664) = -rxt(k,398)*y(k,259)
         mat(k,216) = -rxt(k,399)*y(k,259)
         mat(k,1030) = -rxt(k,400)*y(k,259)
         mat(k,1290) = -rxt(k,408)*y(k,259)
         mat(k,337) = -rxt(k,410)*y(k,259)
         mat(k,308) = -rxt(k,417)*y(k,259)
         mat(k,387) = -rxt(k,418)*y(k,259)
         mat(k,342) = -rxt(k,420)*y(k,259)
         mat(k,1226) = -rxt(k,422)*y(k,259)
         mat(k,108) = -rxt(k,423)*y(k,259)
         mat(k,796) = -rxt(k,428)*y(k,259)
         mat(k,679) = -rxt(k,429)*y(k,259)
         mat(k,1285) = -rxt(k,434)*y(k,259)
         mat(k,1115) = -rxt(k,435)*y(k,259)
         mat(k,626) = -rxt(k,436)*y(k,259)
         mat(k,643) = -rxt(k,437)*y(k,259)
         mat(k,473) = -rxt(k,445)*y(k,259)
         mat(k,134) = -rxt(k,446)*y(k,259)
         mat(k,1418) = -rxt(k,448)*y(k,259)
         mat(k,1278) = -rxt(k,449)*y(k,259)
         mat(k,951) = -rxt(k,450)*y(k,259)
         mat(k,634) = -rxt(k,453)*y(k,259)
         mat(k,467) = -rxt(k,457)*y(k,259)
         mat(k,1215) = -rxt(k,458)*y(k,259)
         mat(k,1109) = -rxt(k,459)*y(k,259)
         mat(k,415) = -rxt(k,461)*y(k,259)
         mat(k,1268) = -rxt(k,464)*y(k,259)
         mat(k,1410) = -rxt(k,472)*y(k,259)
         mat(k,354) = -rxt(k,473)*y(k,259)
         mat(k,606) = -rxt(k,482)*y(k,259)
         mat(k,360) = -rxt(k,483)*y(k,259)
         mat(k,695) = -rxt(k,484)*y(k,259)
         mat(k,1519) = -rxt(k,486)*y(k,259)
         mat(k,770) = -rxt(k,489)*y(k,259)
         mat(k,760) = -rxt(k,493)*y(k,259)
         mat(k,285) = -rxt(k,494)*y(k,259)
         mat(k,281) = -rxt(k,495)*y(k,259)
         mat(k,402) = -rxt(k,496)*y(k,259)
         mat(k,167) = -rxt(k,497)*y(k,259)
         mat(k,666) = -rxt(k,509)*y(k,259)
         mat(k,651) = -rxt(k,510)*y(k,259)
         mat(k,524) = -rxt(k,517)*y(k,259)
         mat(k,969) = -rxt(k,519)*y(k,259)
         mat(k,803) = -rxt(k,520)*y(k,259)
         mat(k,449) = -rxt(k,521)*y(k,259)
         mat(k,1194) = -rxt(k,522)*y(k,259)
         mat(k,245) = -rxt(k,528)*y(k,259)
         mat(k,196) = -rxt(k,531)*y(k,259)
         mat(k,480) = -rxt(k,534)*y(k,259)
         mat(k,288) = -rxt(k,535)*y(k,259)
         mat(k,382) = -rxt(k,537)*y(k,259)
         mat(k,313) = -rxt(k,541)*y(k,259)
         mat(k,237) = -rxt(k,542)*y(k,259)
         mat(k,205) = -rxt(k,554)*y(k,259)
         mat(k,376) = -rxt(k,557)*y(k,259)
         mat(k,750) = -rxt(k,561)*y(k,259)
         mat(k,232) = -rxt(k,562)*y(k,259)
         mat(k,254) = -rxt(k,564)*y(k,259)
         mat(k,819) = -rxt(k,567)*y(k,259)
         mat(k,259) = -rxt(k,570)*y(k,259)
         mat(k,498) = -rxt(k,571)*y(k,259)
         mat(k,1084) = -rxt(k,574)*y(k,259)
         mat(k,1056) = -rxt(k,577)*y(k,259)
         mat(k,455) = -rxt(k,582)*y(k,259)
         mat(k,736) = -rxt(k,586)*y(k,259)
         mat(k,717) = -rxt(k,587)*y(k,259)
         mat(k,569) = -rxt(k,591)*y(k,259)
         mat(k,1161) = -rxt(k,593)*y(k,259)
         mat(k,1254) = -rxt(k,594)*y(k,259)
         mat(k,1147) = -(rxt(k,600) + rxt(k,614)) * y(k,259)
         mat(k,425) = -rxt(k,602)*y(k,259)
         mat(k,1167) = -rxt(k,604)*y(k,259)
         mat(k,847) = -rxt(k,608)*y(k,259)
         mat(k,1644) = -rxt(k,613)*y(k,259)
         mat(k,111) = -rxt(k,633)*y(k,259)
         mat(k,1084) = mat(k,1084) + .630_r8*rxt(k,573)*y(k,166)
         mat(k,337) = mat(k,337) + .650_r8*rxt(k,410)*y(k,259)
         mat(k,643) = mat(k,643) + .130_r8*rxt(k,412)*y(k,166)
         mat(k,387) = mat(k,387) + .500_r8*rxt(k,418)*y(k,259)
         mat(k,1215) = mat(k,1215) + .360_r8*rxt(k,441)*y(k,166)
         mat(k,2637) = mat(k,2637) + rxt(k,390)*y(k,164)
         mat(k,491) = mat(k,491) + .300_r8*rxt(k,397)*y(k,259)
         mat(k,1664) = mat(k,1664) + rxt(k,404)*y(k,255)
         mat(k,2280) = rxt(k,249)*y(k,237)
         mat(k,1003) = rxt(k,348)*y(k,272)
         mat(k,1911) = rxt(k,208)*y(k,166) + 2.000_r8*rxt(k,203)*y(k,237)
         mat(k,1618) = mat(k,1618) + rxt(k,200)*y(k,164) + rxt(k,183)*y(k,255)
         mat(k,702) = mat(k,702) + rxt(k,201)*y(k,164)
         mat(k,1603) = mat(k,1603) + rxt(k,299)*y(k,164) + rxt(k,305)*y(k,255)
         mat(k,1818) = mat(k,1818) + rxt(k,266)*y(k,164) + rxt(k,278)*y(k,255)
         mat(k,216) = mat(k,216) + rxt(k,407)*y(k,255)
         mat(k,1708) = rxt(k,301)*y(k,164)
         mat(k,1842) = mat(k,1842) + rxt(k,269)*y(k,164)
         mat(k,969) = mat(k,969) + .320_r8*rxt(k,518)*y(k,166)
         mat(k,803) = mat(k,803) + .600_r8*rxt(k,520)*y(k,259)
         mat(k,1410) = mat(k,1410) + .240_r8*rxt(k,471)*y(k,166)
         mat(k,354) = mat(k,354) + .100_r8*rxt(k,473)*y(k,259)
         mat(k,1056) = mat(k,1056) + .630_r8*rxt(k,576)*y(k,166)
         mat(k,1519) = mat(k,1519) + .360_r8*rxt(k,485)*y(k,166)
         mat(k,2606) = rxt(k,233)*y(k,237)
         mat(k,2826) = mat(k,2826) + rxt(k,228)*y(k,237)
         mat(k,2501) = mat(k,2501) + rxt(k,390)*y(k,53) + rxt(k,200)*y(k,95) &
                      + rxt(k,201)*y(k,97) + rxt(k,299)*y(k,99) + rxt(k,266)*y(k,103) &
                      + rxt(k,301)*y(k,110) + rxt(k,269)*y(k,111) + rxt(k,206) &
                      *y(k,237)
         mat(k,1980) = mat(k,1980) + .630_r8*rxt(k,573)*y(k,8) + .130_r8*rxt(k,412) &
                      *y(k,30) + .360_r8*rxt(k,441)*y(k,35) + rxt(k,208)*y(k,94) &
                      + .320_r8*rxt(k,518)*y(k,129) + .240_r8*rxt(k,471)*y(k,136) &
                      + .630_r8*rxt(k,576)*y(k,141) + .360_r8*rxt(k,485)*y(k,142) &
                      + rxt(k,207)*y(k,237)
         mat(k,634) = mat(k,634) + .500_r8*rxt(k,453)*y(k,259)
         mat(k,245) = mat(k,245) + .500_r8*rxt(k,528)*y(k,259)
         mat(k,613) = .400_r8*rxt(k,529)*y(k,237)
         mat(k,1569) = .450_r8*rxt(k,426)*y(k,237)
         mat(k,870) = .400_r8*rxt(k,543)*y(k,237)
         mat(k,2425) = mat(k,2425) + rxt(k,249)*y(k,72) + 2.000_r8*rxt(k,203)*y(k,94) &
                      + rxt(k,233)*y(k,155) + rxt(k,228)*y(k,157) + rxt(k,206) &
                      *y(k,164) + rxt(k,207)*y(k,166) + .400_r8*rxt(k,529)*y(k,221) &
                      + .450_r8*rxt(k,426)*y(k,230) + .400_r8*rxt(k,543)*y(k,232) &
                      + .450_r8*rxt(k,476)*y(k,246) + .400_r8*rxt(k,549)*y(k,247) &
                      + .200_r8*rxt(k,480)*y(k,248) + .150_r8*rxt(k,455)*y(k,263)
         mat(k,1537) = .450_r8*rxt(k,476)*y(k,237)
         mat(k,1024) = .400_r8*rxt(k,549)*y(k,237)
         mat(k,778) = .200_r8*rxt(k,480)*y(k,237)
         mat(k,1887) = rxt(k,404)*y(k,66) + rxt(k,183)*y(k,95) + rxt(k,305)*y(k,99) &
                      + rxt(k,278)*y(k,103) + rxt(k,407)*y(k,104) &
                      + 2.000_r8*rxt(k,184)*y(k,272)
         mat(k,2193) = mat(k,2193) + .650_r8*rxt(k,410)*y(k,29) + .500_r8*rxt(k,418) &
                      *y(k,33) + .300_r8*rxt(k,397)*y(k,65) + .600_r8*rxt(k,520) &
                      *y(k,133) + .100_r8*rxt(k,473)*y(k,137) + .500_r8*rxt(k,453) &
                      *y(k,179) + .500_r8*rxt(k,528)*y(k,214)
         mat(k,1340) = .150_r8*rxt(k,455)*y(k,237)
         mat(k,2856) = rxt(k,348)*y(k,91) + 2.000_r8*rxt(k,184)*y(k,255)
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
         mat(k,884) = -(rxt(k,748)*y(k,80) + rxt(k,750)*y(k,165))
         mat(k,1579) = -rxt(k,748)*y(k,260)
         mat(k,1781) = -rxt(k,750)*y(k,260)
         mat(k,2476) = rxt(k,741)*y(k,251) + rxt(k,742)*y(k,253)
         mat(k,784) = rxt(k,741)*y(k,164)
         mat(k,582) = rxt(k,742)*y(k,164)
         mat(k,515) = -(rxt(k,552)*y(k,237) + rxt(k,553)*y(k,155))
         mat(k,2353) = -rxt(k,552)*y(k,261)
         mat(k,2535) = -rxt(k,553)*y(k,261)
         mat(k,235) = .200_r8*rxt(k,542)*y(k,259)
         mat(k,203) = .140_r8*rxt(k,554)*y(k,259)
         mat(k,374) = rxt(k,557)*y(k,259)
         mat(k,2094) = .200_r8*rxt(k,542)*y(k,84) + .140_r8*rxt(k,554)*y(k,175) &
                      + rxt(k,557)*y(k,176)
         mat(k,897) = -(rxt(k,451)*y(k,237) + rxt(k,452)*y(k,155))
         mat(k,2379) = -rxt(k,451)*y(k,262)
         mat(k,2557) = -rxt(k,452)*y(k,262)
         mat(k,1201) = rxt(k,458)*y(k,259)
         mat(k,631) = .500_r8*rxt(k,453)*y(k,259)
         mat(k,2135) = rxt(k,458)*y(k,35) + .500_r8*rxt(k,453)*y(k,179)
         mat(k,1336) = -(rxt(k,454)*y(k,231) + rxt(k,455)*y(k,237) + rxt(k,456) &
                      *y(k,155))
         mat(k,1743) = -rxt(k,454)*y(k,263)
         mat(k,2398) = -rxt(k,455)*y(k,263)
         mat(k,2582) = -rxt(k,456)*y(k,263)
         mat(k,1079) = .060_r8*rxt(k,573)*y(k,166)
         mat(k,1106) = rxt(k,459)*y(k,259)
         mat(k,1051) = .060_r8*rxt(k,576)*y(k,166)
         mat(k,1959) = .060_r8*rxt(k,573)*y(k,8) + .060_r8*rxt(k,576)*y(k,141)
         mat(k,465) = rxt(k,457)*y(k,259)
         mat(k,1250) = .150_r8*rxt(k,594)*y(k,259)
         mat(k,2165) = rxt(k,459)*y(k,59) + rxt(k,457)*y(k,180) + .150_r8*rxt(k,594) &
                      *y(k,211)
         mat(k,1299) = -(rxt(k,583)*y(k,231) + rxt(k,584)*y(k,237) + rxt(k,585) &
                      *y(k,155))
         mat(k,1741) = -rxt(k,583)*y(k,264)
         mat(k,2396) = -rxt(k,584)*y(k,264)
         mat(k,2580) = -rxt(k,585)*y(k,264)
         mat(k,2798) = .500_r8*rxt(k,592)*y(k,210)
         mat(k,734) = rxt(k,586)*y(k,259)
         mat(k,1160) = .500_r8*rxt(k,592)*y(k,157) + rxt(k,593)*y(k,259)
         mat(k,2163) = rxt(k,586)*y(k,207) + rxt(k,593)*y(k,210)
         mat(k,1320) = -(rxt(k,588)*y(k,231) + rxt(k,589)*y(k,237) + rxt(k,590) &
                      *y(k,155))
         mat(k,1742) = -rxt(k,588)*y(k,265)
         mat(k,2397) = -rxt(k,589)*y(k,265)
         mat(k,2581) = -rxt(k,590)*y(k,265)
         mat(k,1078) = rxt(k,574)*y(k,259)
         mat(k,1050) = rxt(k,577)*y(k,259)
         mat(k,568) = rxt(k,591)*y(k,259)
         mat(k,2164) = rxt(k,574)*y(k,8) + rxt(k,577)*y(k,141) + rxt(k,591)*y(k,209)
         mat(k,832) = -(rxt(k,559)*y(k,237) + rxt(k,560)*y(k,155))
         mat(k,2375) = -rxt(k,559)*y(k,266)
         mat(k,2553) = -rxt(k,560)*y(k,266)
         mat(k,744) = rxt(k,561)*y(k,259)
         mat(k,231) = .650_r8*rxt(k,562)*y(k,259)
         mat(k,2129) = rxt(k,561)*y(k,212) + .650_r8*rxt(k,562)*y(k,213)
         mat(k,95) = -(rxt(k,688)*y(k,237) + rxt(k,689)*y(k,155))
         mat(k,2334) = -rxt(k,688)*y(k,267)
         mat(k,2525) = -rxt(k,689)*y(k,267)
         mat(k,226) = rxt(k,687)*y(k,259)
         mat(k,2036) = rxt(k,687)*y(k,213)
         mat(k,1367) = -(rxt(k,523)*y(k,230) + rxt(k,524)*y(k,231) + rxt(k,525) &
                      *y(k,237) + rxt(k,526)*y(k,155) + rxt(k,527)*y(k,157))
         mat(k,1554) = -rxt(k,523)*y(k,268)
         mat(k,1744) = -rxt(k,524)*y(k,268)
         mat(k,2400) = -rxt(k,525)*y(k,268)
         mat(k,2583) = -rxt(k,526)*y(k,268)
         mat(k,2802) = -rxt(k,527)*y(k,268)
         mat(k,280) = rxt(k,495)*y(k,259)
         mat(k,401) = rxt(k,496)*y(k,259)
         mat(k,166) = rxt(k,497)*y(k,259)
         mat(k,800) = .400_r8*rxt(k,520)*y(k,259)
         mat(k,244) = .500_r8*rxt(k,528)*y(k,259)
         mat(k,2167) = rxt(k,495)*y(k,114) + rxt(k,496)*y(k,116) + rxt(k,497)*y(k,124) &
                      + .400_r8*rxt(k,520)*y(k,133) + .500_r8*rxt(k,528)*y(k,214)
         mat(k,855) = -(rxt(k,565)*y(k,237) + rxt(k,566)*y(k,155))
         mat(k,2376) = -rxt(k,565)*y(k,269)
         mat(k,2554) = -rxt(k,566)*y(k,269)
         mat(k,251) = .560_r8*rxt(k,564)*y(k,259)
         mat(k,812) = rxt(k,567)*y(k,259)
         mat(k,2131) = .560_r8*rxt(k,564)*y(k,215) + rxt(k,567)*y(k,216)
         mat(k,101) = -(rxt(k,692)*y(k,237) + rxt(k,693)*y(k,155))
         mat(k,2335) = -rxt(k,692)*y(k,270)
         mat(k,2526) = -rxt(k,693)*y(k,270)
         mat(k,246) = rxt(k,691)*y(k,259)
         mat(k,2037) = rxt(k,691)*y(k,215)
         mat(k,589) = -(rxt(k,568)*y(k,237) + rxt(k,569)*y(k,155))
         mat(k,2361) = -rxt(k,568)*y(k,271)
         mat(k,2540) = -rxt(k,569)*y(k,271)
         mat(k,258) = .300_r8*rxt(k,570)*y(k,259)
         mat(k,495) = rxt(k,571)*y(k,259)
         mat(k,2103) = .300_r8*rxt(k,570)*y(k,217) + rxt(k,571)*y(k,218)
         mat(k,2869) = -(rxt(k,184)*y(k,255) + rxt(k,348)*y(k,91) + rxt(k,615) &
                      *y(k,185))
         mat(k,1900) = -rxt(k,184)*y(k,272)
         mat(k,1006) = -rxt(k,348)*y(k,272)
         mat(k,305) = -rxt(k,615)*y(k,272)
         mat(k,296) = rxt(k,355)*y(k,259)
         mat(k,344) = rxt(k,420)*y(k,259)
         mat(k,475) = rxt(k,445)*y(k,259)
         mat(k,135) = rxt(k,446)*y(k,259)
         mat(k,564) = rxt(k,357)*y(k,259)
         mat(k,350) = rxt(k,360)*y(k,259)
         mat(k,2650) = rxt(k,391)*y(k,259)
         mat(k,690) = rxt(k,362)*y(k,259)
         mat(k,155) = rxt(k,363)*y(k,259)
         mat(k,1230) = rxt(k,422)*y(k,259)
         mat(k,445) = rxt(k,365)*y(k,259)
         mat(k,1110) = rxt(k,459)*y(k,259)
         mat(k,1422) = rxt(k,448)*y(k,259)
         mat(k,797) = rxt(k,428)*y(k,259)
         mat(k,681) = rxt(k,429)*y(k,259)
         mat(k,437) = rxt(k,367)*y(k,259)
         mat(k,493) = rxt(k,397)*y(k,259)
         mat(k,1673) = rxt(k,398)*y(k,259)
         mat(k,1183) = rxt(k,374)*y(k,237)
         mat(k,399) = rxt(k,379)*y(k,259)
         mat(k,1924) = rxt(k,204)*y(k,237)
         mat(k,1623) = rxt(k,209)*y(k,259)
         mat(k,706) = rxt(k,210)*y(k,259)
         mat(k,1609) = (rxt(k,623)+rxt(k,697)+rxt(k,710)+rxt(k,719))*y(k,110) + ( &
                      + rxt(k,622)+rxt(k,699)+rxt(k,707)+rxt(k,716))*y(k,111) + ( &
                      + rxt(k,630)+rxt(k,726)+rxt(k,730)+rxt(k,734))*y(k,112) &
                      + rxt(k,300)*y(k,259)
         mat(k,332) = rxt(k,382)*y(k,259)
         mat(k,1828) = (rxt(k,625)+rxt(k,696)+rxt(k,709)+rxt(k,718))*y(k,110) + ( &
                      + rxt(k,624)+rxt(k,695)+rxt(k,706)+rxt(k,715))*y(k,111) + ( &
                      + rxt(k,629)+rxt(k,725)+rxt(k,729)+rxt(k,733))*y(k,112) &
                      + rxt(k,267)*y(k,259)
         mat(k,1032) = rxt(k,400)*y(k,259)
         mat(k,1360) = (rxt(k,627)+rxt(k,698)+rxt(k,711)+rxt(k,720))*y(k,110) + ( &
                      + rxt(k,626)+rxt(k,700)+rxt(k,708)+rxt(k,717))*y(k,111) + ( &
                      + rxt(k,631)+rxt(k,727)+rxt(k,731)+rxt(k,735))*y(k,112) &
                      + rxt(k,335)*y(k,259)
         mat(k,2734) = rxt(k,241)*y(k,259)
         mat(k,557) = rxt(k,217)*y(k,259)
         mat(k,1718) = (rxt(k,623)+rxt(k,697)+rxt(k,710)+rxt(k,719))*y(k,99) + ( &
                      + rxt(k,625)+rxt(k,696)+rxt(k,709)+rxt(k,718))*y(k,103) + ( &
                      + rxt(k,627)+rxt(k,698)+rxt(k,711)+rxt(k,720))*y(k,107)
         mat(k,1852) = (rxt(k,622)+rxt(k,699)+rxt(k,707)+rxt(k,716))*y(k,99) + ( &
                      + rxt(k,624)+rxt(k,695)+rxt(k,706)+rxt(k,715))*y(k,103) + ( &
                      + rxt(k,626)+rxt(k,700)+rxt(k,708)+rxt(k,717))*y(k,107) &
                      + rxt(k,270)*y(k,259)
         mat(k,1695) = (rxt(k,630)+rxt(k,726)+rxt(k,730)+rxt(k,734))*y(k,99) + ( &
                      + rxt(k,629)+rxt(k,725)+rxt(k,729)+rxt(k,733))*y(k,103) + ( &
                      + rxt(k,631)+rxt(k,727)+rxt(k,731)+rxt(k,735))*y(k,107) &
                      + rxt(k,308)*y(k,259)
         mat(k,1413) = .500_r8*rxt(k,472)*y(k,259)
         mat(k,112) = rxt(k,633)*y(k,259)
         mat(k,637) = rxt(k,453)*y(k,259)
         mat(k,469) = rxt(k,457)*y(k,259)
         mat(k,2438) = rxt(k,374)*y(k,70) + rxt(k,204)*y(k,94) + rxt(k,211)*y(k,259)
         mat(k,2206) = rxt(k,355)*y(k,31) + rxt(k,420)*y(k,34) + rxt(k,445)*y(k,36) &
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
         mat(k, 51) = lmat(k, 51)
         mat(k, 57) = mat(k, 57) + lmat(k, 57)
         mat(k, 63) = mat(k, 63) + lmat(k, 63)
         mat(k, 69) = mat(k, 69) + lmat(k, 69)
         mat(k, 75) = mat(k, 75) + lmat(k, 75)
         mat(k, 81) = mat(k, 81) + lmat(k, 81)
         mat(k, 83) = mat(k, 83) + lmat(k, 83)
         mat(k, 89) = mat(k, 89) + lmat(k, 89)
         mat(k, 95) = mat(k, 95) + lmat(k, 95)
         mat(k, 101) = mat(k, 101) + lmat(k, 101)
         mat(k, 102) = lmat(k, 102)
         mat(k, 103) = lmat(k, 103)
         mat(k, 104) = lmat(k, 104)
         mat(k, 105) = lmat(k, 105)
         mat(k, 106) = lmat(k, 106)
         mat(k, 107) = mat(k, 107) + lmat(k, 107)
         mat(k, 110) = mat(k, 110) + lmat(k, 110)
         mat(k, 113) = mat(k, 113) + lmat(k, 113)
         mat(k, 115) = mat(k, 115) + lmat(k, 115)
         mat(k, 116) = mat(k, 116) + lmat(k, 116)
         mat(k, 118) = mat(k, 118) + lmat(k, 118)
         mat(k, 119) = mat(k, 119) + lmat(k, 119)
         mat(k, 120) = mat(k, 120) + lmat(k, 120)
         mat(k, 122) = mat(k, 122) + lmat(k, 122)
         mat(k, 123) = mat(k, 123) + lmat(k, 123)
         mat(k, 124) = mat(k, 124) + lmat(k, 124)
         mat(k, 125) = mat(k, 125) + lmat(k, 125)
         mat(k, 127) = mat(k, 127) + lmat(k, 127)
         mat(k, 128) = lmat(k, 128)
         mat(k, 129) = lmat(k, 129)
         mat(k, 130) = lmat(k, 130)
         mat(k, 131) = lmat(k, 131)
         mat(k, 132) = mat(k, 132) + lmat(k, 132)
         mat(k, 136) = mat(k, 136) + lmat(k, 136)
         mat(k, 137) = mat(k, 137) + lmat(k, 137)
         mat(k, 139) = mat(k, 139) + lmat(k, 139)
         mat(k, 140) = mat(k, 140) + lmat(k, 140)
         mat(k, 141) = mat(k, 141) + lmat(k, 141)
         mat(k, 143) = mat(k, 143) + lmat(k, 143)
         mat(k, 144) = mat(k, 144) + lmat(k, 144)
         mat(k, 145) = mat(k, 145) + lmat(k, 145)
         mat(k, 147) = mat(k, 147) + lmat(k, 147)
         mat(k, 148) = mat(k, 148) + lmat(k, 148)
         mat(k, 150) = mat(k, 150) + lmat(k, 150)
         mat(k, 151) = mat(k, 151) + lmat(k, 151)
         mat(k, 152) = mat(k, 152) + lmat(k, 152)
         mat(k, 154) = mat(k, 154) + lmat(k, 154)
         mat(k, 156) = lmat(k, 156)
         mat(k, 157) = lmat(k, 157)
         mat(k, 158) = lmat(k, 158)
         mat(k, 159) = lmat(k, 159)
         mat(k, 160) = lmat(k, 160)
         mat(k, 161) = lmat(k, 161)
         mat(k, 162) = lmat(k, 162)
         mat(k, 163) = lmat(k, 163)
         mat(k, 164) = lmat(k, 164)
         mat(k, 165) = mat(k, 165) + lmat(k, 165)
         mat(k, 168) = lmat(k, 168)
         mat(k, 169) = lmat(k, 169)
         mat(k, 170) = lmat(k, 170)
         mat(k, 171) = mat(k, 171) + lmat(k, 171)
         mat(k, 172) = mat(k, 172) + lmat(k, 172)
         mat(k, 174) = mat(k, 174) + lmat(k, 174)
         mat(k, 175) = mat(k, 175) + lmat(k, 175)
         mat(k, 176) = mat(k, 176) + lmat(k, 176)
         mat(k, 177) = mat(k, 177) + lmat(k, 177)
         mat(k, 178) = mat(k, 178) + lmat(k, 178)
         mat(k, 180) = mat(k, 180) + lmat(k, 180)
         mat(k, 181) = mat(k, 181) + lmat(k, 181)
         mat(k, 182) = mat(k, 182) + lmat(k, 182)
         mat(k, 183) = mat(k, 183) + lmat(k, 183)
         mat(k, 185) = mat(k, 185) + lmat(k, 185)
         mat(k, 186) = mat(k, 186) + lmat(k, 186)
         mat(k, 187) = mat(k, 187) + lmat(k, 187)
         mat(k, 188) = mat(k, 188) + lmat(k, 188)
         mat(k, 190) = mat(k, 190) + lmat(k, 190)
         mat(k, 192) = mat(k, 192) + lmat(k, 192)
         mat(k, 198) = lmat(k, 198)
         mat(k, 199) = lmat(k, 199)
         mat(k, 200) = lmat(k, 200)
         mat(k, 201) = lmat(k, 201)
         mat(k, 202) = mat(k, 202) + lmat(k, 202)
         mat(k, 207) = mat(k, 207) + lmat(k, 207)
         mat(k, 208) = mat(k, 208) + lmat(k, 208)
         mat(k, 209) = mat(k, 209) + lmat(k, 209)
         mat(k, 210) = mat(k, 210) + lmat(k, 210)
         mat(k, 211) = lmat(k, 211)
         mat(k, 212) = lmat(k, 212)
         mat(k, 213) = lmat(k, 213)
         mat(k, 214) = mat(k, 214) + lmat(k, 214)
         mat(k, 218) = mat(k, 218) + lmat(k, 218)
         mat(k, 220) = lmat(k, 220)
         mat(k, 221) = mat(k, 221) + lmat(k, 221)
         mat(k, 222) = mat(k, 222) + lmat(k, 222)
         mat(k, 225) = mat(k, 225) + lmat(k, 225)
         mat(k, 227) = mat(k, 227) + lmat(k, 227)
         mat(k, 234) = mat(k, 234) + lmat(k, 234)
         mat(k, 239) = lmat(k, 239)
         mat(k, 240) = lmat(k, 240)
         mat(k, 241) = lmat(k, 241)
         mat(k, 242) = lmat(k, 242)
         mat(k, 243) = mat(k, 243) + lmat(k, 243)
         mat(k, 245) = mat(k, 245) + lmat(k, 245)
         mat(k, 248) = mat(k, 248) + lmat(k, 248)
         mat(k, 256) = mat(k, 256) + lmat(k, 256)
         mat(k, 261) = mat(k, 261) + lmat(k, 261)
         mat(k, 262) = mat(k, 262) + lmat(k, 262)
         mat(k, 265) = mat(k, 265) + lmat(k, 265)
         mat(k, 266) = mat(k, 266) + lmat(k, 266)
         mat(k, 267) = mat(k, 267) + lmat(k, 267)
         mat(k, 269) = mat(k, 269) + lmat(k, 269)
         mat(k, 270) = mat(k, 270) + lmat(k, 270)
         mat(k, 271) = mat(k, 271) + lmat(k, 271)
         mat(k, 274) = mat(k, 274) + lmat(k, 274)
         mat(k, 275) = mat(k, 275) + lmat(k, 275)
         mat(k, 276) = mat(k, 276) + lmat(k, 276)
         mat(k, 278) = mat(k, 278) + lmat(k, 278)
         mat(k, 279) = lmat(k, 279)
         mat(k, 281) = mat(k, 281) + lmat(k, 281)
         mat(k, 282) = lmat(k, 282)
         mat(k, 283) = mat(k, 283) + lmat(k, 283)
         mat(k, 286) = mat(k, 286) + lmat(k, 286)
         mat(k, 289) = lmat(k, 289)
         mat(k, 290) = lmat(k, 290)
         mat(k, 291) = lmat(k, 291)
         mat(k, 292) = mat(k, 292) + lmat(k, 292)
         mat(k, 295) = mat(k, 295) + lmat(k, 295)
         mat(k, 297) = mat(k, 297) + lmat(k, 297)
         mat(k, 299) = mat(k, 299) + lmat(k, 299)
         mat(k, 302) = mat(k, 302) + lmat(k, 302)
         mat(k, 303) = lmat(k, 303)
         mat(k, 304) = lmat(k, 304)
         mat(k, 306) = mat(k, 306) + lmat(k, 306)
         mat(k, 310) = mat(k, 310) + lmat(k, 310)
         mat(k, 311) = lmat(k, 311)
         mat(k, 313) = mat(k, 313) + lmat(k, 313)
         mat(k, 314) = lmat(k, 314)
         mat(k, 315) = lmat(k, 315)
         mat(k, 316) = lmat(k, 316)
         mat(k, 317) = mat(k, 317) + lmat(k, 317)
         mat(k, 318) = lmat(k, 318)
         mat(k, 319) = lmat(k, 319)
         mat(k, 321) = lmat(k, 321)
         mat(k, 322) = mat(k, 322) + lmat(k, 322)
         mat(k, 323) = lmat(k, 323)
         mat(k, 324) = lmat(k, 324)
         mat(k, 325) = lmat(k, 325)
         mat(k, 326) = mat(k, 326) + lmat(k, 326)
         mat(k, 327) = mat(k, 327) + lmat(k, 327)
         mat(k, 331) = mat(k, 331) + lmat(k, 331)
         mat(k, 333) = mat(k, 333) + lmat(k, 333)
         mat(k, 339) = mat(k, 339) + lmat(k, 339)
         mat(k, 345) = mat(k, 345) + lmat(k, 345)
         mat(k, 349) = mat(k, 349) + lmat(k, 349)
         mat(k, 351) = mat(k, 351) + lmat(k, 351)
         mat(k, 356) = mat(k, 356) + lmat(k, 356)
         mat(k, 358) = lmat(k, 358)
         mat(k, 359) = lmat(k, 359)
         mat(k, 360) = mat(k, 360) + lmat(k, 360)
         mat(k, 361) = lmat(k, 361)
         mat(k, 362) = lmat(k, 362)
         mat(k, 363) = lmat(k, 363)
         mat(k, 364) = lmat(k, 364)
         mat(k, 365) = lmat(k, 365)
         mat(k, 366) = lmat(k, 366)
         mat(k, 367) = lmat(k, 367)
         mat(k, 368) = lmat(k, 368)
         mat(k, 369) = lmat(k, 369)
         mat(k, 370) = lmat(k, 370)
         mat(k, 371) = lmat(k, 371)
         mat(k, 372) = lmat(k, 372)
         mat(k, 373) = mat(k, 373) + lmat(k, 373)
         mat(k, 375) = lmat(k, 375)
         mat(k, 376) = mat(k, 376) + lmat(k, 376)
         mat(k, 377) = lmat(k, 377)
         mat(k, 378) = lmat(k, 378)
         mat(k, 379) = mat(k, 379) + lmat(k, 379)
         mat(k, 382) = mat(k, 382) + lmat(k, 382)
         mat(k, 383) = lmat(k, 383)
         mat(k, 384) = mat(k, 384) + lmat(k, 384)
         mat(k, 386) = mat(k, 386) + lmat(k, 386)
         mat(k, 387) = mat(k, 387) + lmat(k, 387)
         mat(k, 388) = lmat(k, 388)
         mat(k, 389) = lmat(k, 389)
         mat(k, 390) = lmat(k, 390)
         mat(k, 391) = lmat(k, 391)
         mat(k, 393) = mat(k, 393) + lmat(k, 393)
         mat(k, 394) = lmat(k, 394)
         mat(k, 398) = mat(k, 398) + lmat(k, 398)
         mat(k, 400) = mat(k, 400) + lmat(k, 400)
         mat(k, 403) = lmat(k, 403)
         mat(k, 404) = mat(k, 404) + lmat(k, 404)
         mat(k, 406) = lmat(k, 406)
         mat(k, 407) = lmat(k, 407)
         mat(k, 408) = lmat(k, 408)
         mat(k, 409) = mat(k, 409) + lmat(k, 409)
         mat(k, 410) = lmat(k, 410)
         mat(k, 411) = mat(k, 411) + lmat(k, 411)
         mat(k, 419) = mat(k, 419) + lmat(k, 419)
         mat(k, 420) = lmat(k, 420)
         mat(k, 422) = mat(k, 422) + lmat(k, 422)
         mat(k, 427) = lmat(k, 427)
         mat(k, 428) = lmat(k, 428)
         mat(k, 429) = lmat(k, 429)
         mat(k, 430) = mat(k, 430) + lmat(k, 430)
         mat(k, 431) = lmat(k, 431)
         mat(k, 434) = mat(k, 434) + lmat(k, 434)
         mat(k, 438) = mat(k, 438) + lmat(k, 438)
         mat(k, 440) = lmat(k, 440)
         mat(k, 443) = mat(k, 443) + lmat(k, 443)
         mat(k, 446) = mat(k, 446) + lmat(k, 446)
         mat(k, 451) = lmat(k, 451)
         mat(k, 452) = mat(k, 452) + lmat(k, 452)
         mat(k, 453) = lmat(k, 453)
         mat(k, 455) = mat(k, 455) + lmat(k, 455)
         mat(k, 456) = lmat(k, 456)
         mat(k, 457) = lmat(k, 457)
         mat(k, 458) = lmat(k, 458)
         mat(k, 459) = lmat(k, 459)
         mat(k, 460) = lmat(k, 460)
         mat(k, 461) = lmat(k, 461)
         mat(k, 462) = lmat(k, 462)
         mat(k, 463) = lmat(k, 463)
         mat(k, 464) = mat(k, 464) + lmat(k, 464)
         mat(k, 466) = lmat(k, 466)
         mat(k, 467) = mat(k, 467) + lmat(k, 467)
         mat(k, 468) = lmat(k, 468)
         mat(k, 470) = mat(k, 470) + lmat(k, 470)
         mat(k, 472) = lmat(k, 472)
         mat(k, 473) = mat(k, 473) + lmat(k, 473)
         mat(k, 474) = lmat(k, 474)
         mat(k, 476) = mat(k, 476) + lmat(k, 476)
         mat(k, 477) = lmat(k, 477)
         mat(k, 479) = lmat(k, 479)
         mat(k, 480) = mat(k, 480) + lmat(k, 480)
         mat(k, 481) = lmat(k, 481)
         mat(k, 482) = mat(k, 482) + lmat(k, 482)
         mat(k, 487) = mat(k, 487) + lmat(k, 487)
         mat(k, 488) = mat(k, 488) + lmat(k, 488)
         mat(k, 490) = lmat(k, 490)
         mat(k, 491) = mat(k, 491) + lmat(k, 491)
         mat(k, 492) = mat(k, 492) + lmat(k, 492)
         mat(k, 494) = mat(k, 494) + lmat(k, 494)
         mat(k, 496) = lmat(k, 496)
         mat(k, 497) = lmat(k, 497)
         mat(k, 498) = mat(k, 498) + lmat(k, 498)
         mat(k, 499) = lmat(k, 499)
         mat(k, 502) = mat(k, 502) + lmat(k, 502)
         mat(k, 508) = mat(k, 508) + lmat(k, 508)
         mat(k, 510) = mat(k, 510) + lmat(k, 510)
         mat(k, 512) = lmat(k, 512)
         mat(k, 515) = mat(k, 515) + lmat(k, 515)
         mat(k, 521) = mat(k, 521) + lmat(k, 521)
         mat(k, 522) = lmat(k, 522)
         mat(k, 525) = mat(k, 525) + lmat(k, 525)
         mat(k, 526) = lmat(k, 526)
         mat(k, 527) = mat(k, 527) + lmat(k, 527)
         mat(k, 530) = mat(k, 530) + lmat(k, 530)
         mat(k, 531) = lmat(k, 531)
         mat(k, 532) = lmat(k, 532)
         mat(k, 533) = lmat(k, 533)
         mat(k, 534) = lmat(k, 534)
         mat(k, 535) = mat(k, 535) + lmat(k, 535)
         mat(k, 538) = mat(k, 538) + lmat(k, 538)
         mat(k, 539) = lmat(k, 539)
         mat(k, 540) = mat(k, 540) + lmat(k, 540)
         mat(k, 543) = mat(k, 543) + lmat(k, 543)
         mat(k, 544) = mat(k, 544) + lmat(k, 544)
         mat(k, 548) = lmat(k, 548)
         mat(k, 549) = lmat(k, 549)
         mat(k, 550) = lmat(k, 550)
         mat(k, 551) = mat(k, 551) + lmat(k, 551)
         mat(k, 553) = mat(k, 553) + lmat(k, 553)
         mat(k, 554) = lmat(k, 554)
         mat(k, 555) = mat(k, 555) + lmat(k, 555)
         mat(k, 556) = lmat(k, 556)
         mat(k, 558) = mat(k, 558) + lmat(k, 558)
         mat(k, 563) = mat(k, 563) + lmat(k, 563)
         mat(k, 565) = mat(k, 565) + lmat(k, 565)
         mat(k, 566) = lmat(k, 566)
         mat(k, 567) = lmat(k, 567)
         mat(k, 569) = mat(k, 569) + lmat(k, 569)
         mat(k, 570) = lmat(k, 570)
         mat(k, 571) = lmat(k, 571)
         mat(k, 574) = mat(k, 574) + lmat(k, 574)
         mat(k, 581) = mat(k, 581) + lmat(k, 581)
         mat(k, 589) = mat(k, 589) + lmat(k, 589)
         mat(k, 596) = lmat(k, 596)
         mat(k, 597) = mat(k, 597) + lmat(k, 597)
         mat(k, 600) = lmat(k, 600)
         mat(k, 601) = mat(k, 601) + lmat(k, 601)
         mat(k, 602) = mat(k, 602) + lmat(k, 602)
         mat(k, 604) = lmat(k, 604)
         mat(k, 605) = lmat(k, 605)
         mat(k, 607) = lmat(k, 607)
         mat(k, 608) = lmat(k, 608)
         mat(k, 609) = lmat(k, 609)
         mat(k, 611) = mat(k, 611) + lmat(k, 611)
         mat(k, 617) = mat(k, 617) + lmat(k, 617)
         mat(k, 622) = mat(k, 622) + lmat(k, 622)
         mat(k, 623) = lmat(k, 623)
         mat(k, 624) = lmat(k, 624)
         mat(k, 625) = lmat(k, 625)
         mat(k, 628) = lmat(k, 628)
         mat(k, 629) = mat(k, 629) + lmat(k, 629)
         mat(k, 630) = mat(k, 630) + lmat(k, 630)
         mat(k, 632) = lmat(k, 632)
         mat(k, 634) = mat(k, 634) + lmat(k, 634)
         mat(k, 635) = lmat(k, 635)
         mat(k, 636) = lmat(k, 636)
         mat(k, 638) = mat(k, 638) + lmat(k, 638)
         mat(k, 646) = mat(k, 646) + lmat(k, 646)
         mat(k, 653) = lmat(k, 653)
         mat(k, 654) = mat(k, 654) + lmat(k, 654)
         mat(k, 662) = mat(k, 662) + lmat(k, 662)
         mat(k, 670) = lmat(k, 670)
         mat(k, 671) = lmat(k, 671)
         mat(k, 672) = lmat(k, 672)
         mat(k, 673) = lmat(k, 673)
         mat(k, 674) = lmat(k, 674)
         mat(k, 675) = mat(k, 675) + lmat(k, 675)
         mat(k, 677) = mat(k, 677) + lmat(k, 677)
         mat(k, 678) = lmat(k, 678)
         mat(k, 679) = mat(k, 679) + lmat(k, 679)
         mat(k, 682) = mat(k, 682) + lmat(k, 682)
         mat(k, 683) = lmat(k, 683)
         mat(k, 688) = mat(k, 688) + lmat(k, 688)
         mat(k, 691) = mat(k, 691) + lmat(k, 691)
         mat(k, 693) = lmat(k, 693)
         mat(k, 698) = lmat(k, 698)
         mat(k, 700) = mat(k, 700) + lmat(k, 700)
         mat(k, 702) = mat(k, 702) + lmat(k, 702)
         mat(k, 709) = mat(k, 709) + lmat(k, 709)
         mat(k, 710) = lmat(k, 710)
         mat(k, 711) = lmat(k, 711)
         mat(k, 715) = mat(k, 715) + lmat(k, 715)
         mat(k, 716) = mat(k, 716) + lmat(k, 716)
         mat(k, 718) = lmat(k, 718)
         mat(k, 719) = mat(k, 719) + lmat(k, 719)
         mat(k, 720) = lmat(k, 720)
         mat(k, 723) = mat(k, 723) + lmat(k, 723)
         mat(k, 729) = mat(k, 729) + lmat(k, 729)
         mat(k, 730) = lmat(k, 730)
         mat(k, 731) = lmat(k, 731)
         mat(k, 732) = lmat(k, 732)
         mat(k, 733) = lmat(k, 733)
         mat(k, 735) = lmat(k, 735)
         mat(k, 736) = mat(k, 736) + lmat(k, 736)
         mat(k, 737) = lmat(k, 737)
         mat(k, 738) = lmat(k, 738)
         mat(k, 739) = lmat(k, 739)
         mat(k, 740) = lmat(k, 740)
         mat(k, 741) = lmat(k, 741)
         mat(k, 742) = mat(k, 742) + lmat(k, 742)
         mat(k, 747) = lmat(k, 747)
         mat(k, 749) = lmat(k, 749)
         mat(k, 750) = mat(k, 750) + lmat(k, 750)
         mat(k, 751) = lmat(k, 751)
         mat(k, 752) = lmat(k, 752)
         mat(k, 753) = mat(k, 753) + lmat(k, 753)
         mat(k, 757) = lmat(k, 757)
         mat(k, 758) = lmat(k, 758)
         mat(k, 760) = mat(k, 760) + lmat(k, 760)
         mat(k, 761) = lmat(k, 761)
         mat(k, 762) = lmat(k, 762)
         mat(k, 763) = lmat(k, 763)
         mat(k, 764) = mat(k, 764) + lmat(k, 764)
         mat(k, 767) = mat(k, 767) + lmat(k, 767)
         mat(k, 768) = mat(k, 768) + lmat(k, 768)
         mat(k, 771) = lmat(k, 771)
         mat(k, 772) = mat(k, 772) + lmat(k, 772)
         mat(k, 773) = mat(k, 773) + lmat(k, 773)
         mat(k, 775) = mat(k, 775) + lmat(k, 775)
         mat(k, 783) = mat(k, 783) + lmat(k, 783)
         mat(k, 793) = mat(k, 793) + lmat(k, 793)
         mat(k, 799) = mat(k, 799) + lmat(k, 799)
         mat(k, 801) = lmat(k, 801)
         mat(k, 802) = lmat(k, 802)
         mat(k, 803) = mat(k, 803) + lmat(k, 803)
         mat(k, 804) = lmat(k, 804)
         mat(k, 805) = lmat(k, 805)
         mat(k, 806) = lmat(k, 806)
         mat(k, 807) = lmat(k, 807)
         mat(k, 808) = lmat(k, 808)
         mat(k, 809) = lmat(k, 809)
         mat(k, 810) = mat(k, 810) + lmat(k, 810)
         mat(k, 815) = lmat(k, 815)
         mat(k, 817) = lmat(k, 817)
         mat(k, 819) = mat(k, 819) + lmat(k, 819)
         mat(k, 820) = lmat(k, 820)
         mat(k, 821) = mat(k, 821) + lmat(k, 821)
         mat(k, 832) = mat(k, 832) + lmat(k, 832)
         mat(k, 842) = mat(k, 842) + lmat(k, 842)
         mat(k, 855) = mat(k, 855) + lmat(k, 855)
         mat(k, 866) = mat(k, 866) + lmat(k, 866)
         mat(k, 876) = mat(k, 876) + lmat(k, 876)
         mat(k, 884) = mat(k, 884) + lmat(k, 884)
         mat(k, 885) = lmat(k, 885)
         mat(k, 887) = lmat(k, 887)
         mat(k, 892) = mat(k, 892) + lmat(k, 892)
         mat(k, 897) = mat(k, 897) + lmat(k, 897)
         mat(k, 908) = mat(k, 908) + lmat(k, 908)
         mat(k, 920) = mat(k, 920) + lmat(k, 920)
         mat(k, 927) = mat(k, 927) + lmat(k, 927)
         mat(k, 928) = mat(k, 928) + lmat(k, 928)
         mat(k, 934) = mat(k, 934) + lmat(k, 934)
         mat(k, 940) = mat(k, 940) + lmat(k, 940)
         mat(k, 948) = mat(k, 948) + lmat(k, 948)
         mat(k, 950) = lmat(k, 950)
         mat(k, 952) = lmat(k, 952)
         mat(k, 953) = mat(k, 953) + lmat(k, 953)
         mat(k, 957) = mat(k, 957) + lmat(k, 957)
         mat(k, 973) = lmat(k, 973)
         mat(k, 974) = lmat(k, 974)
         mat(k, 975) = mat(k, 975) + lmat(k, 975)
         mat(k, 977) = lmat(k, 977)
         mat(k, 978) = mat(k, 978) + lmat(k, 978)
         mat(k, 981) = lmat(k, 981)
         mat(k, 982) = lmat(k, 982)
         mat(k, 983) = lmat(k, 983)
         mat(k, 984) = mat(k, 984) + lmat(k, 984)
         mat(k, 985) = mat(k, 985) + lmat(k, 985)
         mat(k, 986) = mat(k, 986) + lmat(k, 986)
         mat(k, 987) = mat(k, 987) + lmat(k, 987)
         mat(k, 988) = lmat(k, 988)
         mat(k, 991) = mat(k, 991) + lmat(k, 991)
         mat(k, 992) = mat(k, 992) + lmat(k, 992)
         mat(k, 994) = lmat(k, 994)
         mat(k, 995) = lmat(k, 995)
         mat(k, 996) = mat(k, 996) + lmat(k, 996)
         mat(k, 998) = mat(k, 998) + lmat(k, 998)
         mat(k,1010) = mat(k,1010) + lmat(k,1010)
         mat(k,1019) = mat(k,1019) + lmat(k,1019)
         mat(k,1028) = mat(k,1028) + lmat(k,1028)
         mat(k,1042) = mat(k,1042) + lmat(k,1042)
         mat(k,1070) = mat(k,1070) + lmat(k,1070)
         mat(k,1094) = mat(k,1094) + lmat(k,1094)
         mat(k,1105) = mat(k,1105) + lmat(k,1105)
         mat(k,1107) = lmat(k,1107)
         mat(k,1108) = lmat(k,1108)
         mat(k,1111) = lmat(k,1111)
         mat(k,1112) = mat(k,1112) + lmat(k,1112)
         mat(k,1113) = mat(k,1113) + lmat(k,1113)
         mat(k,1116) = mat(k,1116) + lmat(k,1116)
         mat(k,1118) = lmat(k,1118)
         mat(k,1122) = lmat(k,1122)
         mat(k,1123) = mat(k,1123) + lmat(k,1123)
         mat(k,1133) = mat(k,1133) + lmat(k,1133)
         mat(k,1134) = mat(k,1134) + lmat(k,1134)
         mat(k,1136) = mat(k,1136) + lmat(k,1136)
         mat(k,1137) = mat(k,1137) + lmat(k,1137)
         mat(k,1139) = mat(k,1139) + lmat(k,1139)
         mat(k,1141) = lmat(k,1141)
         mat(k,1142) = mat(k,1142) + lmat(k,1142)
         mat(k,1143) = mat(k,1143) + lmat(k,1143)
         mat(k,1144) = mat(k,1144) + lmat(k,1144)
         mat(k,1156) = mat(k,1156) + lmat(k,1156)
         mat(k,1158) = lmat(k,1158)
         mat(k,1159) = lmat(k,1159)
         mat(k,1162) = lmat(k,1162)
         mat(k,1165) = mat(k,1165) + lmat(k,1165)
         mat(k,1166) = lmat(k,1166)
         mat(k,1169) = lmat(k,1169)
         mat(k,1172) = mat(k,1172) + lmat(k,1172)
         mat(k,1184) = lmat(k,1184)
         mat(k,1188) = mat(k,1188) + lmat(k,1188)
         mat(k,1195) = mat(k,1195) + lmat(k,1195)
         mat(k,1198) = lmat(k,1198)
         mat(k,1199) = lmat(k,1199)
         mat(k,1204) = mat(k,1204) + lmat(k,1204)
         mat(k,1222) = mat(k,1222) + lmat(k,1222)
         mat(k,1223) = lmat(k,1223)
         mat(k,1225) = lmat(k,1225)
         mat(k,1227) = lmat(k,1227)
         mat(k,1235) = mat(k,1235) + lmat(k,1235)
         mat(k,1247) = mat(k,1247) + lmat(k,1247)
         mat(k,1248) = mat(k,1248) + lmat(k,1248)
         mat(k,1249) = mat(k,1249) + lmat(k,1249)
         mat(k,1250) = mat(k,1250) + lmat(k,1250)
         mat(k,1251) = mat(k,1251) + lmat(k,1251)
         mat(k,1252) = mat(k,1252) + lmat(k,1252)
         mat(k,1255) = mat(k,1255) + lmat(k,1255)
         mat(k,1256) = mat(k,1256) + lmat(k,1256)
         mat(k,1259) = lmat(k,1259)
         mat(k,1260) = lmat(k,1260)
         mat(k,1261) = mat(k,1261) + lmat(k,1261)
         mat(k,1262) = lmat(k,1262)
         mat(k,1263) = lmat(k,1263)
         mat(k,1264) = lmat(k,1264)
         mat(k,1266) = lmat(k,1266)
         mat(k,1269) = mat(k,1269) + lmat(k,1269)
         mat(k,1270) = lmat(k,1270)
         mat(k,1271) = lmat(k,1271)
         mat(k,1272) = lmat(k,1272)
         mat(k,1275) = mat(k,1275) + lmat(k,1275)
         mat(k,1277) = lmat(k,1277)
         mat(k,1279) = mat(k,1279) + lmat(k,1279)
         mat(k,1280) = lmat(k,1280)
         mat(k,1282) = mat(k,1282) + lmat(k,1282)
         mat(k,1283) = lmat(k,1283)
         mat(k,1286) = mat(k,1286) + lmat(k,1286)
         mat(k,1287) = mat(k,1287) + lmat(k,1287)
         mat(k,1288) = mat(k,1288) + lmat(k,1288)
         mat(k,1299) = mat(k,1299) + lmat(k,1299)
         mat(k,1320) = mat(k,1320) + lmat(k,1320)
         mat(k,1336) = mat(k,1336) + lmat(k,1336)
         mat(k,1348) = mat(k,1348) + lmat(k,1348)
         mat(k,1353) = lmat(k,1353)
         mat(k,1355) = mat(k,1355) + lmat(k,1355)
         mat(k,1367) = mat(k,1367) + lmat(k,1367)
         mat(k,1387) = mat(k,1387) + lmat(k,1387)
         mat(k,1402) = mat(k,1402) + lmat(k,1402)
         mat(k,1403) = mat(k,1403) + lmat(k,1403)
         mat(k,1406) = mat(k,1406) + lmat(k,1406)
         mat(k,1407) = mat(k,1407) + lmat(k,1407)
         mat(k,1411) = mat(k,1411) + lmat(k,1411)
         mat(k,1412) = mat(k,1412) + lmat(k,1412)
         mat(k,1414) = mat(k,1414) + lmat(k,1414)
         mat(k,1415) = mat(k,1415) + lmat(k,1415)
         mat(k,1416) = mat(k,1416) + lmat(k,1416)
         mat(k,1419) = lmat(k,1419)
         mat(k,1434) = mat(k,1434) + lmat(k,1434)
         mat(k,1450) = lmat(k,1450)
         mat(k,1467) = mat(k,1467) + lmat(k,1467)
         mat(k,1476) = mat(k,1476) + lmat(k,1476)
         mat(k,1491) = mat(k,1491) + lmat(k,1491)
         mat(k,1505) = lmat(k,1505)
         mat(k,1508) = mat(k,1508) + lmat(k,1508)
         mat(k,1511) = mat(k,1511) + lmat(k,1511)
         mat(k,1513) = mat(k,1513) + lmat(k,1513)
         mat(k,1516) = lmat(k,1516)
         mat(k,1532) = mat(k,1532) + lmat(k,1532)
         mat(k,1563) = mat(k,1563) + lmat(k,1563)
         mat(k,1584) = mat(k,1584) + lmat(k,1584)
         mat(k,1585) = mat(k,1585) + lmat(k,1585)
         mat(k,1591) = lmat(k,1591)
         mat(k,1597) = mat(k,1597) + lmat(k,1597)
         mat(k,1602) = mat(k,1602) + lmat(k,1602)
         mat(k,1606) = mat(k,1606) + lmat(k,1606)
         mat(k,1612) = mat(k,1612) + lmat(k,1612)
         mat(k,1625) = mat(k,1625) + lmat(k,1625)
         mat(k,1628) = mat(k,1628) + lmat(k,1628)
         mat(k,1637) = lmat(k,1637)
         mat(k,1639) = mat(k,1639) + lmat(k,1639)
         mat(k,1649) = mat(k,1649) + lmat(k,1649)
         mat(k,1655) = lmat(k,1655)
         mat(k,1656) = lmat(k,1656)
         mat(k,1657) = mat(k,1657) + lmat(k,1657)
         mat(k,1658) = mat(k,1658) + lmat(k,1658)
         mat(k,1659) = mat(k,1659) + lmat(k,1659)
         mat(k,1663) = mat(k,1663) + lmat(k,1663)
         mat(k,1664) = mat(k,1664) + lmat(k,1664)
         mat(k,1667) = lmat(k,1667)
         mat(k,1669) = mat(k,1669) + lmat(k,1669)
         mat(k,1673) = mat(k,1673) + lmat(k,1673)
         mat(k,1674) = mat(k,1674) + lmat(k,1674)
         mat(k,1675) = mat(k,1675) + lmat(k,1675)
         mat(k,1679) = mat(k,1679) + lmat(k,1679)
         mat(k,1685) = mat(k,1685) + lmat(k,1685)
         mat(k,1686) = lmat(k,1686)
         mat(k,1697) = mat(k,1697) + lmat(k,1697)
         mat(k,1698) = mat(k,1698) + lmat(k,1698)
         mat(k,1703) = mat(k,1703) + lmat(k,1703)
         mat(k,1708) = mat(k,1708) + lmat(k,1708)
         mat(k,1711) = lmat(k,1711)
         mat(k,1756) = mat(k,1756) + lmat(k,1756)
         mat(k,1781) = mat(k,1781) + lmat(k,1781)
         mat(k,1783) = mat(k,1783) + lmat(k,1783)
         mat(k,1784) = lmat(k,1784)
         mat(k,1791) = mat(k,1791) + lmat(k,1791)
         mat(k,1792) = mat(k,1792) + lmat(k,1792)
         mat(k,1801) = mat(k,1801) + lmat(k,1801)
         mat(k,1813) = mat(k,1813) + lmat(k,1813)
         mat(k,1816) = mat(k,1816) + lmat(k,1816)
         mat(k,1820) = mat(k,1820) + lmat(k,1820)
         mat(k,1829) = mat(k,1829) + lmat(k,1829)
         mat(k,1838) = mat(k,1838) + lmat(k,1838)
         mat(k,1842) = mat(k,1842) + lmat(k,1842)
         mat(k,1844) = mat(k,1844) + lmat(k,1844)
         mat(k,1883) = mat(k,1883) + lmat(k,1883)
         mat(k,1893) = mat(k,1893) + lmat(k,1893)
         mat(k,1908) = mat(k,1908) + lmat(k,1908)
         mat(k,1930) = mat(k,1930) + lmat(k,1930)
         mat(k,1973) = mat(k,1973) + lmat(k,1973)
         mat(k,1976) = mat(k,1976) + lmat(k,1976)
         mat(k,1978) = mat(k,1978) + lmat(k,1978)
         mat(k,1986) = mat(k,1986) + lmat(k,1986)
         mat(k,2009) = mat(k,2009) + lmat(k,2009)
         mat(k,2012) = mat(k,2012) + lmat(k,2012)
         mat(k,2016) = mat(k,2016) + lmat(k,2016)
         mat(k,2193) = mat(k,2193) + lmat(k,2193)
         mat(k,2224) = mat(k,2224) + lmat(k,2224)
         mat(k,2282) = mat(k,2282) + lmat(k,2282)
         mat(k,2313) = mat(k,2313) + lmat(k,2313)
         mat(k,2429) = mat(k,2429) + lmat(k,2429)
         mat(k,2438) = mat(k,2438) + lmat(k,2438)
         mat(k,2457) = mat(k,2457) + lmat(k,2457)
         mat(k,2459) = mat(k,2459) + lmat(k,2459)
         mat(k,2460) = mat(k,2460) + lmat(k,2460)
         mat(k,2476) = mat(k,2476) + lmat(k,2476)
         mat(k,2479) = lmat(k,2479)
         mat(k,2507) = mat(k,2507) + lmat(k,2507)
         mat(k,2559) = mat(k,2559) + lmat(k,2559)
         mat(k,2561) = lmat(k,2561)
         mat(k,2569) = mat(k,2569) + lmat(k,2569)
         mat(k,2612) = mat(k,2612) + lmat(k,2612)
         mat(k,2613) = mat(k,2613) + lmat(k,2613)
         mat(k,2622) = mat(k,2622) + lmat(k,2622)
         mat(k,2625) = lmat(k,2625)
         mat(k,2634) = mat(k,2634) + lmat(k,2634)
         mat(k,2645) = mat(k,2645) + lmat(k,2645)
         mat(k,2695) = mat(k,2695) + lmat(k,2695)
         mat(k,2701) = mat(k,2701) + lmat(k,2701)
         mat(k,2702) = mat(k,2702) + lmat(k,2702)
         mat(k,2704) = mat(k,2704) + lmat(k,2704)
         mat(k,2705) = mat(k,2705) + lmat(k,2705)
         mat(k,2715) = lmat(k,2715)
         mat(k,2721) = mat(k,2721) + lmat(k,2721)
         mat(k,2730) = lmat(k,2730)
         mat(k,2731) = mat(k,2731) + lmat(k,2731)
         mat(k,2755) = mat(k,2755) + lmat(k,2755)
         mat(k,2760) = mat(k,2760) + lmat(k,2760)
         mat(k,2765) = mat(k,2765) + lmat(k,2765)
         mat(k,2819) = mat(k,2819) + lmat(k,2819)
         mat(k,2832) = mat(k,2832) + lmat(k,2832)
         mat(k,2833) = mat(k,2833) + lmat(k,2833)
         mat(k,2835) = mat(k,2835) + lmat(k,2835)
         mat(k,2836) = mat(k,2836) + lmat(k,2836)
         mat(k,2838) = mat(k,2838) + lmat(k,2838)
         mat(k,2845) = lmat(k,2845)
         mat(k,2852) = mat(k,2852) + lmat(k,2852)
         mat(k,2853) = lmat(k,2853)
         mat(k,2856) = mat(k,2856) + lmat(k,2856)
         mat(k,2862) = lmat(k,2862)
         mat(k,2869) = mat(k,2869) + lmat(k,2869)
         mat(k, 252) = 0._r8
         mat(k, 253) = 0._r8
         mat(k, 328) = 0._r8
         mat(k, 381) = 0._r8
         mat(k, 395) = 0._r8
         mat(k, 503) = 0._r8
         mat(k, 504) = 0._r8
         mat(k, 517) = 0._r8
         mat(k, 575) = 0._r8
         mat(k, 577) = 0._r8
         mat(k, 592) = 0._r8
         mat(k, 724) = 0._r8
         mat(k, 725) = 0._r8
         mat(k, 743) = 0._r8
         mat(k, 745) = 0._r8
         mat(k, 746) = 0._r8
         mat(k, 748) = 0._r8
         mat(k, 754) = 0._r8
         mat(k, 755) = 0._r8
         mat(k, 759) = 0._r8
         mat(k, 765) = 0._r8
         mat(k, 766) = 0._r8
         mat(k, 769) = 0._r8
         mat(k, 790) = 0._r8
         mat(k, 792) = 0._r8
         mat(k, 811) = 0._r8
         mat(k, 813) = 0._r8
         mat(k, 814) = 0._r8
         mat(k, 816) = 0._r8
         mat(k, 818) = 0._r8
         mat(k, 831) = 0._r8
         mat(k, 833) = 0._r8
         mat(k, 834) = 0._r8
         mat(k, 836) = 0._r8
         mat(k, 838) = 0._r8
         mat(k, 854) = 0._r8
         mat(k, 856) = 0._r8
         mat(k, 857) = 0._r8
         mat(k, 859) = 0._r8
         mat(k, 861) = 0._r8
         mat(k, 862) = 0._r8
         mat(k, 877) = 0._r8
         mat(k, 878) = 0._r8
         mat(k, 879) = 0._r8
         mat(k, 899) = 0._r8
         mat(k, 901) = 0._r8
         mat(k, 906) = 0._r8
         mat(k, 913) = 0._r8
         mat(k, 918) = 0._r8
         mat(k, 923) = 0._r8
         mat(k, 924) = 0._r8
         mat(k, 926) = 0._r8
         mat(k, 937) = 0._r8
         mat(k, 942) = 0._r8
         mat(k, 943) = 0._r8
         mat(k, 944) = 0._r8
         mat(k, 947) = 0._r8
         mat(k, 979) = 0._r8
         mat(k, 980) = 0._r8
         mat(k, 989) = 0._r8
         mat(k,1014) = 0._r8
         mat(k,1043) = 0._r8
         mat(k,1045) = 0._r8
         mat(k,1052) = 0._r8
         mat(k,1060) = 0._r8
         mat(k,1071) = 0._r8
         mat(k,1073) = 0._r8
         mat(k,1080) = 0._r8
         mat(k,1088) = 0._r8
         mat(k,1092) = 0._r8
         mat(k,1093) = 0._r8
         mat(k,1097) = 0._r8
         mat(k,1098) = 0._r8
         mat(k,1099) = 0._r8
         mat(k,1100) = 0._r8
         mat(k,1119) = 0._r8
         mat(k,1124) = 0._r8
         mat(k,1125) = 0._r8
         mat(k,1127) = 0._r8
         mat(k,1170) = 0._r8
         mat(k,1186) = 0._r8
         mat(k,1189) = 0._r8
         mat(k,1190) = 0._r8
         mat(k,1191) = 0._r8
         mat(k,1192) = 0._r8
         mat(k,1193) = 0._r8
         mat(k,1196) = 0._r8
         mat(k,1197) = 0._r8
         mat(k,1206) = 0._r8
         mat(k,1208) = 0._r8
         mat(k,1209) = 0._r8
         mat(k,1213) = 0._r8
         mat(k,1217) = 0._r8
         mat(k,1219) = 0._r8
         mat(k,1221) = 0._r8
         mat(k,1236) = 0._r8
         mat(k,1237) = 0._r8
         mat(k,1238) = 0._r8
         mat(k,1240) = 0._r8
         mat(k,1245) = 0._r8
         mat(k,1253) = 0._r8
         mat(k,1257) = 0._r8
         mat(k,1265) = 0._r8
         mat(k,1267) = 0._r8
         mat(k,1273) = 0._r8
         mat(k,1274) = 0._r8
         mat(k,1300) = 0._r8
         mat(k,1301) = 0._r8
         mat(k,1304) = 0._r8
         mat(k,1309) = 0._r8
         mat(k,1310) = 0._r8
         mat(k,1316) = 0._r8
         mat(k,1317) = 0._r8
         mat(k,1318) = 0._r8
         mat(k,1319) = 0._r8
         mat(k,1321) = 0._r8
         mat(k,1322) = 0._r8
         mat(k,1323) = 0._r8
         mat(k,1325) = 0._r8
         mat(k,1330) = 0._r8
         mat(k,1331) = 0._r8
         mat(k,1332) = 0._r8
         mat(k,1345) = 0._r8
         mat(k,1356) = 0._r8
         mat(k,1357) = 0._r8
         mat(k,1372) = 0._r8
         mat(k,1380) = 0._r8
         mat(k,1382) = 0._r8
         mat(k,1384) = 0._r8
         mat(k,1385) = 0._r8
         mat(k,1386) = 0._r8
         mat(k,1388) = 0._r8
         mat(k,1389) = 0._r8
         mat(k,1390) = 0._r8
         mat(k,1392) = 0._r8
         mat(k,1394) = 0._r8
         mat(k,1399) = 0._r8
         mat(k,1408) = 0._r8
         mat(k,1417) = 0._r8
         mat(k,1426) = 0._r8
         mat(k,1427) = 0._r8
         mat(k,1428) = 0._r8
         mat(k,1429) = 0._r8
         mat(k,1430) = 0._r8
         mat(k,1431) = 0._r8
         mat(k,1433) = 0._r8
         mat(k,1435) = 0._r8
         mat(k,1437) = 0._r8
         mat(k,1441) = 0._r8
         mat(k,1442) = 0._r8
         mat(k,1447) = 0._r8
         mat(k,1449) = 0._r8
         mat(k,1453) = 0._r8
         mat(k,1456) = 0._r8
         mat(k,1457) = 0._r8
         mat(k,1459) = 0._r8
         mat(k,1462) = 0._r8
         mat(k,1463) = 0._r8
         mat(k,1464) = 0._r8
         mat(k,1465) = 0._r8
         mat(k,1468) = 0._r8
         mat(k,1469) = 0._r8
         mat(k,1470) = 0._r8
         mat(k,1472) = 0._r8
         mat(k,1474) = 0._r8
         mat(k,1475) = 0._r8
         mat(k,1480) = 0._r8
         mat(k,1482) = 0._r8
         mat(k,1489) = 0._r8
         mat(k,1492) = 0._r8
         mat(k,1494) = 0._r8
         mat(k,1496) = 0._r8
         mat(k,1501) = 0._r8
         mat(k,1503) = 0._r8
         mat(k,1507) = 0._r8
         mat(k,1512) = 0._r8
         mat(k,1515) = 0._r8
         mat(k,1517) = 0._r8
         mat(k,1521) = 0._r8
         mat(k,1523) = 0._r8
         mat(k,1524) = 0._r8
         mat(k,1525) = 0._r8
         mat(k,1526) = 0._r8
         mat(k,1530) = 0._r8
         mat(k,1531) = 0._r8
         mat(k,1542) = 0._r8
         mat(k,1544) = 0._r8
         mat(k,1565) = 0._r8
         mat(k,1567) = 0._r8
         mat(k,1574) = 0._r8
         mat(k,1575) = 0._r8
         mat(k,1576) = 0._r8
         mat(k,1580) = 0._r8
         mat(k,1582) = 0._r8
         mat(k,1583) = 0._r8
         mat(k,1586) = 0._r8
         mat(k,1587) = 0._r8
         mat(k,1588) = 0._r8
         mat(k,1589) = 0._r8
         mat(k,1590) = 0._r8
         mat(k,1592) = 0._r8
         mat(k,1593) = 0._r8
         mat(k,1604) = 0._r8
         mat(k,1605) = 0._r8
         mat(k,1613) = 0._r8
         mat(k,1614) = 0._r8
         mat(k,1621) = 0._r8
         mat(k,1622) = 0._r8
         mat(k,1629) = 0._r8
         mat(k,1630) = 0._r8
         mat(k,1632) = 0._r8
         mat(k,1633) = 0._r8
         mat(k,1647) = 0._r8
         mat(k,1652) = 0._r8
         mat(k,1660) = 0._r8
         mat(k,1668) = 0._r8
         mat(k,1670) = 0._r8
         mat(k,1671) = 0._r8
         mat(k,1672) = 0._r8
         mat(k,1680) = 0._r8
         mat(k,1682) = 0._r8
         mat(k,1683) = 0._r8
         mat(k,1684) = 0._r8
         mat(k,1687) = 0._r8
         mat(k,1688) = 0._r8
         mat(k,1689) = 0._r8
         mat(k,1690) = 0._r8
         mat(k,1691) = 0._r8
         mat(k,1701) = 0._r8
         mat(k,1702) = 0._r8
         mat(k,1705) = 0._r8
         mat(k,1706) = 0._r8
         mat(k,1707) = 0._r8
         mat(k,1709) = 0._r8
         mat(k,1710) = 0._r8
         mat(k,1714) = 0._r8
         mat(k,1715) = 0._r8
         mat(k,1716) = 0._r8
         mat(k,1717) = 0._r8
         mat(k,1728) = 0._r8
         mat(k,1755) = 0._r8
         mat(k,1758) = 0._r8
         mat(k,1759) = 0._r8
         mat(k,1760) = 0._r8
         mat(k,1761) = 0._r8
         mat(k,1762) = 0._r8
         mat(k,1764) = 0._r8
         mat(k,1767) = 0._r8
         mat(k,1771) = 0._r8
         mat(k,1772) = 0._r8
         mat(k,1773) = 0._r8
         mat(k,1788) = 0._r8
         mat(k,1789) = 0._r8
         mat(k,1795) = 0._r8
         mat(k,1796) = 0._r8
         mat(k,1797) = 0._r8
         mat(k,1798) = 0._r8
         mat(k,1800) = 0._r8
         mat(k,1803) = 0._r8
         mat(k,1804) = 0._r8
         mat(k,1805) = 0._r8
         mat(k,1819) = 0._r8
         mat(k,1821) = 0._r8
         mat(k,1822) = 0._r8
         mat(k,1824) = 0._r8
         mat(k,1826) = 0._r8
         mat(k,1827) = 0._r8
         mat(k,1834) = 0._r8
         mat(k,1835) = 0._r8
         mat(k,1836) = 0._r8
         mat(k,1839) = 0._r8
         mat(k,1840) = 0._r8
         mat(k,1843) = 0._r8
         mat(k,1845) = 0._r8
         mat(k,1846) = 0._r8
         mat(k,1848) = 0._r8
         mat(k,1849) = 0._r8
         mat(k,1850) = 0._r8
         mat(k,1851) = 0._r8
         mat(k,1877) = 0._r8
         mat(k,1878) = 0._r8
         mat(k,1882) = 0._r8
         mat(k,1888) = 0._r8
         mat(k,1896) = 0._r8
         mat(k,1897) = 0._r8
         mat(k,1898) = 0._r8
         mat(k,1899) = 0._r8
         mat(k,1902) = 0._r8
         mat(k,1903) = 0._r8
         mat(k,1905) = 0._r8
         mat(k,1906) = 0._r8
         mat(k,1907) = 0._r8
         mat(k,1910) = 0._r8
         mat(k,1912) = 0._r8
         mat(k,1913) = 0._r8
         mat(k,1914) = 0._r8
         mat(k,1916) = 0._r8
         mat(k,1918) = 0._r8
         mat(k,1919) = 0._r8
         mat(k,1920) = 0._r8
         mat(k,1921) = 0._r8
         mat(k,1922) = 0._r8
         mat(k,1923) = 0._r8
         mat(k,1940) = 0._r8
         mat(k,1945) = 0._r8
         mat(k,1947) = 0._r8
         mat(k,1952) = 0._r8
         mat(k,1954) = 0._r8
         mat(k,1955) = 0._r8
         mat(k,1957) = 0._r8
         mat(k,1958) = 0._r8
         mat(k,1960) = 0._r8
         mat(k,1963) = 0._r8
         mat(k,1964) = 0._r8
         mat(k,1965) = 0._r8
         mat(k,1967) = 0._r8
         mat(k,1974) = 0._r8
         mat(k,1975) = 0._r8
         mat(k,1990) = 0._r8
         mat(k,1993) = 0._r8
         mat(k,2006) = 0._r8
         mat(k,2007) = 0._r8
         mat(k,2008) = 0._r8
         mat(k,2020) = 0._r8
         mat(k,2022) = 0._r8
         mat(k,2023) = 0._r8
         mat(k,2096) = 0._r8
         mat(k,2113) = 0._r8
         mat(k,2128) = 0._r8
         mat(k,2132) = 0._r8
         mat(k,2139) = 0._r8
         mat(k,2141) = 0._r8
         mat(k,2168) = 0._r8
         mat(k,2189) = 0._r8
         mat(k,2209) = 0._r8
         mat(k,2210) = 0._r8
         mat(k,2214) = 0._r8
         mat(k,2215) = 0._r8
         mat(k,2217) = 0._r8
         mat(k,2218) = 0._r8
         mat(k,2219) = 0._r8
         mat(k,2220) = 0._r8
         mat(k,2222) = 0._r8
         mat(k,2223) = 0._r8
         mat(k,2225) = 0._r8
         mat(k,2229) = 0._r8
         mat(k,2231) = 0._r8
         mat(k,2233) = 0._r8
         mat(k,2236) = 0._r8
         mat(k,2254) = 0._r8
         mat(k,2257) = 0._r8
         mat(k,2258) = 0._r8
         mat(k,2263) = 0._r8
         mat(k,2264) = 0._r8
         mat(k,2266) = 0._r8
         mat(k,2267) = 0._r8
         mat(k,2269) = 0._r8
         mat(k,2271) = 0._r8
         mat(k,2276) = 0._r8
         mat(k,2285) = 0._r8
         mat(k,2286) = 0._r8
         mat(k,2287) = 0._r8
         mat(k,2290) = 0._r8
         mat(k,2291) = 0._r8
         mat(k,2293) = 0._r8
         mat(k,2296) = 0._r8
         mat(k,2299) = 0._r8
         mat(k,2301) = 0._r8
         mat(k,2302) = 0._r8
         mat(k,2304) = 0._r8
         mat(k,2305) = 0._r8
         mat(k,2306) = 0._r8
         mat(k,2307) = 0._r8
         mat(k,2309) = 0._r8
         mat(k,2310) = 0._r8
         mat(k,2312) = 0._r8
         mat(k,2316) = 0._r8
         mat(k,2317) = 0._r8
         mat(k,2320) = 0._r8
         mat(k,2323) = 0._r8
         mat(k,2355) = 0._r8
         mat(k,2356) = 0._r8
         mat(k,2357) = 0._r8
         mat(k,2381) = 0._r8
         mat(k,2386) = 0._r8
         mat(k,2388) = 0._r8
         mat(k,2392) = 0._r8
         mat(k,2393) = 0._r8
         mat(k,2394) = 0._r8
         mat(k,2402) = 0._r8
         mat(k,2407) = 0._r8
         mat(k,2413) = 0._r8
         mat(k,2414) = 0._r8
         mat(k,2421) = 0._r8
         mat(k,2435) = 0._r8
         mat(k,2448) = 0._r8
         mat(k,2449) = 0._r8
         mat(k,2450) = 0._r8
         mat(k,2451) = 0._r8
         mat(k,2452) = 0._r8
         mat(k,2462) = 0._r8
         mat(k,2464) = 0._r8
         mat(k,2466) = 0._r8
         mat(k,2467) = 0._r8
         mat(k,2475) = 0._r8
         mat(k,2478) = 0._r8
         mat(k,2483) = 0._r8
         mat(k,2485) = 0._r8
         mat(k,2490) = 0._r8
         mat(k,2491) = 0._r8
         mat(k,2493) = 0._r8
         mat(k,2497) = 0._r8
         mat(k,2511) = 0._r8
         mat(k,2514) = 0._r8
         mat(k,2562) = 0._r8
         mat(k,2594) = 0._r8
         mat(k,2595) = 0._r8
         mat(k,2596) = 0._r8
         mat(k,2597) = 0._r8
         mat(k,2600) = 0._r8
         mat(k,2601) = 0._r8
         mat(k,2602) = 0._r8
         mat(k,2603) = 0._r8
         mat(k,2616) = 0._r8
         mat(k,2619) = 0._r8
         mat(k,2621) = 0._r8
         mat(k,2623) = 0._r8
         mat(k,2626) = 0._r8
         mat(k,2627) = 0._r8
         mat(k,2628) = 0._r8
         mat(k,2629) = 0._r8
         mat(k,2630) = 0._r8
         mat(k,2632) = 0._r8
         mat(k,2633) = 0._r8
         mat(k,2635) = 0._r8
         mat(k,2636) = 0._r8
         mat(k,2638) = 0._r8
         mat(k,2642) = 0._r8
         mat(k,2644) = 0._r8
         mat(k,2646) = 0._r8
         mat(k,2648) = 0._r8
         mat(k,2656) = 0._r8
         mat(k,2657) = 0._r8
         mat(k,2659) = 0._r8
         mat(k,2662) = 0._r8
         mat(k,2666) = 0._r8
         mat(k,2672) = 0._r8
         mat(k,2676) = 0._r8
         mat(k,2677) = 0._r8
         mat(k,2678) = 0._r8
         mat(k,2681) = 0._r8
         mat(k,2682) = 0._r8
         mat(k,2684) = 0._r8
         mat(k,2685) = 0._r8
         mat(k,2686) = 0._r8
         mat(k,2687) = 0._r8
         mat(k,2689) = 0._r8
         mat(k,2690) = 0._r8
         mat(k,2691) = 0._r8
         mat(k,2692) = 0._r8
         mat(k,2703) = 0._r8
         mat(k,2708) = 0._r8
         mat(k,2711) = 0._r8
         mat(k,2712) = 0._r8
         mat(k,2713) = 0._r8
         mat(k,2714) = 0._r8
         mat(k,2716) = 0._r8
         mat(k,2717) = 0._r8
         mat(k,2718) = 0._r8
         mat(k,2719) = 0._r8
         mat(k,2720) = 0._r8
         mat(k,2722) = 0._r8
         mat(k,2723) = 0._r8
         mat(k,2724) = 0._r8
         mat(k,2725) = 0._r8
         mat(k,2726) = 0._r8
         mat(k,2727) = 0._r8
         mat(k,2728) = 0._r8
         mat(k,2729) = 0._r8
         mat(k,2732) = 0._r8
         mat(k,2743) = 0._r8
         mat(k,2744) = 0._r8
         mat(k,2746) = 0._r8
         mat(k,2748) = 0._r8
         mat(k,2749) = 0._r8
         mat(k,2750) = 0._r8
         mat(k,2751) = 0._r8
         mat(k,2762) = 0._r8
         mat(k,2764) = 0._r8
         mat(k,2767) = 0._r8
         mat(k,2773) = 0._r8
         mat(k,2774) = 0._r8
         mat(k,2779) = 0._r8
         mat(k,2782) = 0._r8
         mat(k,2793) = 0._r8
         mat(k,2794) = 0._r8
         mat(k,2799) = 0._r8
         mat(k,2800) = 0._r8
         mat(k,2812) = 0._r8
         mat(k,2814) = 0._r8
         mat(k,2815) = 0._r8
         mat(k,2817) = 0._r8
         mat(k,2818) = 0._r8
         mat(k,2820) = 0._r8
         mat(k,2821) = 0._r8
         mat(k,2822) = 0._r8
         mat(k,2823) = 0._r8
         mat(k,2824) = 0._r8
         mat(k,2825) = 0._r8
         mat(k,2829) = 0._r8
         mat(k,2831) = 0._r8
         mat(k,2839) = 0._r8
         mat(k,2844) = 0._r8
         mat(k,2846) = 0._r8
         mat(k,2847) = 0._r8
         mat(k,2848) = 0._r8
         mat(k,2849) = 0._r8
         mat(k,2850) = 0._r8
         mat(k,2851) = 0._r8
         mat(k,2854) = 0._r8
         mat(k,2855) = 0._r8
         mat(k,2857) = 0._r8
         mat(k,2858) = 0._r8
         mat(k,2859) = 0._r8
         mat(k,2860) = 0._r8
         mat(k,2861) = 0._r8
         mat(k,2863) = 0._r8
         mat(k,2864) = 0._r8
         mat(k,2865) = 0._r8
         mat(k,2866) = 0._r8
         mat(k,2867) = 0._r8
         mat(k,2868) = 0._r8
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
         mat(k, 57) = mat(k, 57) - dti(k)
         mat(k, 63) = mat(k, 63) - dti(k)
         mat(k, 69) = mat(k, 69) - dti(k)
         mat(k, 75) = mat(k, 75) - dti(k)
         mat(k, 81) = mat(k, 81) - dti(k)
         mat(k, 83) = mat(k, 83) - dti(k)
         mat(k, 89) = mat(k, 89) - dti(k)
         mat(k, 95) = mat(k, 95) - dti(k)
         mat(k, 101) = mat(k, 101) - dti(k)
         mat(k, 102) = mat(k, 102) - dti(k)
         mat(k, 104) = mat(k, 104) - dti(k)
         mat(k, 107) = mat(k, 107) - dti(k)
         mat(k, 110) = mat(k, 110) - dti(k)
         mat(k, 113) = mat(k, 113) - dti(k)
         mat(k, 116) = mat(k, 116) - dti(k)
         mat(k, 120) = mat(k, 120) - dti(k)
         mat(k, 124) = mat(k, 124) - dti(k)
         mat(k, 128) = mat(k, 128) - dti(k)
         mat(k, 132) = mat(k, 132) - dti(k)
         mat(k, 136) = mat(k, 136) - dti(k)
         mat(k, 140) = mat(k, 140) - dti(k)
         mat(k, 144) = mat(k, 144) - dti(k)
         mat(k, 148) = mat(k, 148) - dti(k)
         mat(k, 152) = mat(k, 152) - dti(k)
         mat(k, 156) = mat(k, 156) - dti(k)
         mat(k, 159) = mat(k, 159) - dti(k)
         mat(k, 162) = mat(k, 162) - dti(k)
         mat(k, 165) = mat(k, 165) - dti(k)
         mat(k, 168) = mat(k, 168) - dti(k)
         mat(k, 171) = mat(k, 171) - dti(k)
         mat(k, 176) = mat(k, 176) - dti(k)
         mat(k, 181) = mat(k, 181) - dti(k)
         mat(k, 186) = mat(k, 186) - dti(k)
         mat(k, 192) = mat(k, 192) - dti(k)
         mat(k, 198) = mat(k, 198) - dti(k)
         mat(k, 202) = mat(k, 202) - dti(k)
         mat(k, 207) = mat(k, 207) - dti(k)
         mat(k, 210) = mat(k, 210) - dti(k)
         mat(k, 212) = mat(k, 212) - dti(k)
         mat(k, 214) = mat(k, 214) - dti(k)
         mat(k, 218) = mat(k, 218) - dti(k)
         mat(k, 222) = mat(k, 222) - dti(k)
         mat(k, 227) = mat(k, 227) - dti(k)
         mat(k, 234) = mat(k, 234) - dti(k)
         mat(k, 239) = mat(k, 239) - dti(k)
         mat(k, 243) = mat(k, 243) - dti(k)
         mat(k, 248) = mat(k, 248) - dti(k)
         mat(k, 256) = mat(k, 256) - dti(k)
         mat(k, 261) = mat(k, 261) - dti(k)
         mat(k, 266) = mat(k, 266) - dti(k)
         mat(k, 270) = mat(k, 270) - dti(k)
         mat(k, 275) = mat(k, 275) - dti(k)
         mat(k, 278) = mat(k, 278) - dti(k)
         mat(k, 283) = mat(k, 283) - dti(k)
         mat(k, 286) = mat(k, 286) - dti(k)
         mat(k, 289) = mat(k, 289) - dti(k)
         mat(k, 292) = mat(k, 292) - dti(k)
         mat(k, 297) = mat(k, 297) - dti(k)
         mat(k, 302) = mat(k, 302) - dti(k)
         mat(k, 306) = mat(k, 306) - dti(k)
         mat(k, 310) = mat(k, 310) - dti(k)
         mat(k, 314) = mat(k, 314) - dti(k)
         mat(k, 317) = mat(k, 317) - dti(k)
         mat(k, 323) = mat(k, 323) - dti(k)
         mat(k, 327) = mat(k, 327) - dti(k)
         mat(k, 333) = mat(k, 333) - dti(k)
         mat(k, 339) = mat(k, 339) - dti(k)
         mat(k, 345) = mat(k, 345) - dti(k)
         mat(k, 351) = mat(k, 351) - dti(k)
         mat(k, 356) = mat(k, 356) - dti(k)
         mat(k, 361) = mat(k, 361) - dti(k)
         mat(k, 366) = mat(k, 366) - dti(k)
         mat(k, 373) = mat(k, 373) - dti(k)
         mat(k, 379) = mat(k, 379) - dti(k)
         mat(k, 384) = mat(k, 384) - dti(k)
         mat(k, 389) = mat(k, 389) - dti(k)
         mat(k, 393) = mat(k, 393) - dti(k)
         mat(k, 400) = mat(k, 400) - dti(k)
         mat(k, 403) = mat(k, 403) - dti(k)
         mat(k, 411) = mat(k, 411) - dti(k)
         mat(k, 419) = mat(k, 419) - dti(k)
         mat(k, 427) = mat(k, 427) - dti(k)
         mat(k, 430) = mat(k, 430) - dti(k)
         mat(k, 438) = mat(k, 438) - dti(k)
         mat(k, 446) = mat(k, 446) - dti(k)
         mat(k, 452) = mat(k, 452) - dti(k)
         mat(k, 458) = mat(k, 458) - dti(k)
         mat(k, 464) = mat(k, 464) - dti(k)
         mat(k, 470) = mat(k, 470) - dti(k)
         mat(k, 476) = mat(k, 476) - dti(k)
         mat(k, 482) = mat(k, 482) - dti(k)
         mat(k, 488) = mat(k, 488) - dti(k)
         mat(k, 494) = mat(k, 494) - dti(k)
         mat(k, 502) = mat(k, 502) - dti(k)
         mat(k, 508) = mat(k, 508) - dti(k)
         mat(k, 515) = mat(k, 515) - dti(k)
         mat(k, 521) = mat(k, 521) - dti(k)
         mat(k, 527) = mat(k, 527) - dti(k)
         mat(k, 532) = mat(k, 532) - dti(k)
         mat(k, 535) = mat(k, 535) - dti(k)
         mat(k, 540) = mat(k, 540) - dti(k)
         mat(k, 544) = mat(k, 544) - dti(k)
         mat(k, 548) = mat(k, 548) - dti(k)
         mat(k, 551) = mat(k, 551) - dti(k)
         mat(k, 558) = mat(k, 558) - dti(k)
         mat(k, 565) = mat(k, 565) - dti(k)
         mat(k, 574) = mat(k, 574) - dti(k)
         mat(k, 581) = mat(k, 581) - dti(k)
         mat(k, 589) = mat(k, 589) - dti(k)
         mat(k, 597) = mat(k, 597) - dti(k)
         mat(k, 602) = mat(k, 602) - dti(k)
         mat(k, 607) = mat(k, 607) - dti(k)
         mat(k, 611) = mat(k, 611) - dti(k)
         mat(k, 617) = mat(k, 617) - dti(k)
         mat(k, 622) = mat(k, 622) - dti(k)
         mat(k, 630) = mat(k, 630) - dti(k)
         mat(k, 638) = mat(k, 638) - dti(k)
         mat(k, 646) = mat(k, 646) - dti(k)
         mat(k, 654) = mat(k, 654) - dti(k)
         mat(k, 662) = mat(k, 662) - dti(k)
         mat(k, 671) = mat(k, 671) - dti(k)
         mat(k, 675) = mat(k, 675) - dti(k)
         mat(k, 682) = mat(k, 682) - dti(k)
         mat(k, 691) = mat(k, 691) - dti(k)
         mat(k, 700) = mat(k, 700) - dti(k)
         mat(k, 709) = mat(k, 709) - dti(k)
         mat(k, 715) = mat(k, 715) - dti(k)
         mat(k, 723) = mat(k, 723) - dti(k)
         mat(k, 729) = mat(k, 729) - dti(k)
         mat(k, 742) = mat(k, 742) - dti(k)
         mat(k, 753) = mat(k, 753) - dti(k)
         mat(k, 764) = mat(k, 764) - dti(k)
         mat(k, 775) = mat(k, 775) - dti(k)
         mat(k, 783) = mat(k, 783) - dti(k)
         mat(k, 793) = mat(k, 793) - dti(k)
         mat(k, 799) = mat(k, 799) - dti(k)
         mat(k, 810) = mat(k, 810) - dti(k)
         mat(k, 821) = mat(k, 821) - dti(k)
         mat(k, 832) = mat(k, 832) - dti(k)
         mat(k, 842) = mat(k, 842) - dti(k)
         mat(k, 855) = mat(k, 855) - dti(k)
         mat(k, 866) = mat(k, 866) - dti(k)
         mat(k, 876) = mat(k, 876) - dti(k)
         mat(k, 884) = mat(k, 884) - dti(k)
         mat(k, 892) = mat(k, 892) - dti(k)
         mat(k, 897) = mat(k, 897) - dti(k)
         mat(k, 908) = mat(k, 908) - dti(k)
         mat(k, 920) = mat(k, 920) - dti(k)
         mat(k, 928) = mat(k, 928) - dti(k)
         mat(k, 940) = mat(k, 940) - dti(k)
         mat(k, 948) = mat(k, 948) - dti(k)
         mat(k, 957) = mat(k, 957) - dti(k)
         mat(k, 975) = mat(k, 975) - dti(k)
         mat(k, 987) = mat(k, 987) - dti(k)
         mat(k, 998) = mat(k, 998) - dti(k)
         mat(k,1010) = mat(k,1010) - dti(k)
         mat(k,1019) = mat(k,1019) - dti(k)
         mat(k,1028) = mat(k,1028) - dti(k)
         mat(k,1042) = mat(k,1042) - dti(k)
         mat(k,1070) = mat(k,1070) - dti(k)
         mat(k,1094) = mat(k,1094) - dti(k)
         mat(k,1105) = mat(k,1105) - dti(k)
         mat(k,1112) = mat(k,1112) - dti(k)
         mat(k,1123) = mat(k,1123) - dti(k)
         mat(k,1134) = mat(k,1134) - dti(k)
         mat(k,1144) = mat(k,1144) - dti(k)
         mat(k,1156) = mat(k,1156) - dti(k)
         mat(k,1165) = mat(k,1165) - dti(k)
         mat(k,1172) = mat(k,1172) - dti(k)
         mat(k,1188) = mat(k,1188) - dti(k)
         mat(k,1204) = mat(k,1204) - dti(k)
         mat(k,1222) = mat(k,1222) - dti(k)
         mat(k,1235) = mat(k,1235) - dti(k)
         mat(k,1248) = mat(k,1248) - dti(k)
         mat(k,1261) = mat(k,1261) - dti(k)
         mat(k,1275) = mat(k,1275) - dti(k)
         mat(k,1282) = mat(k,1282) - dti(k)
         mat(k,1288) = mat(k,1288) - dti(k)
         mat(k,1299) = mat(k,1299) - dti(k)
         mat(k,1320) = mat(k,1320) - dti(k)
         mat(k,1336) = mat(k,1336) - dti(k)
         mat(k,1348) = mat(k,1348) - dti(k)
         mat(k,1367) = mat(k,1367) - dti(k)
         mat(k,1387) = mat(k,1387) - dti(k)
         mat(k,1403) = mat(k,1403) - dti(k)
         mat(k,1415) = mat(k,1415) - dti(k)
         mat(k,1434) = mat(k,1434) - dti(k)
         mat(k,1467) = mat(k,1467) - dti(k)
         mat(k,1491) = mat(k,1491) - dti(k)
         mat(k,1511) = mat(k,1511) - dti(k)
         mat(k,1532) = mat(k,1532) - dti(k)
         mat(k,1563) = mat(k,1563) - dti(k)
         mat(k,1585) = mat(k,1585) - dti(k)
         mat(k,1597) = mat(k,1597) - dti(k)
         mat(k,1612) = mat(k,1612) - dti(k)
         mat(k,1625) = mat(k,1625) - dti(k)
         mat(k,1639) = mat(k,1639) - dti(k)
         mat(k,1658) = mat(k,1658) - dti(k)
         mat(k,1679) = mat(k,1679) - dti(k)
         mat(k,1703) = mat(k,1703) - dti(k)
         mat(k,1756) = mat(k,1756) - dti(k)
         mat(k,1791) = mat(k,1791) - dti(k)
         mat(k,1813) = mat(k,1813) - dti(k)
         mat(k,1838) = mat(k,1838) - dti(k)
         mat(k,1883) = mat(k,1883) - dti(k)
         mat(k,1908) = mat(k,1908) - dti(k)
         mat(k,1978) = mat(k,1978) - dti(k)
         mat(k,2009) = mat(k,2009) - dti(k)
         mat(k,2193) = mat(k,2193) - dti(k)
         mat(k,2224) = mat(k,2224) - dti(k)
         mat(k,2282) = mat(k,2282) - dti(k)
         mat(k,2313) = mat(k,2313) - dti(k)
         mat(k,2429) = mat(k,2429) - dti(k)
         mat(k,2459) = mat(k,2459) - dti(k)
         mat(k,2507) = mat(k,2507) - dti(k)
         mat(k,2613) = mat(k,2613) - dti(k)
         mat(k,2645) = mat(k,2645) - dti(k)
         mat(k,2704) = mat(k,2704) - dti(k)
         mat(k,2731) = mat(k,2731) - dti(k)
         mat(k,2765) = mat(k,2765) - dti(k)
         mat(k,2838) = mat(k,2838) - dti(k)
         mat(k,2869) = mat(k,2869) - dti(k)
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
