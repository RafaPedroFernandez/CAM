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
         mat(k,766) = -(rxt(k,493)*y(k,259))
         mat(k,2244) = -rxt(k,493)*y(k,1)
         mat(k,1960) = rxt(k,496)*y(k,223)
         mat(k,1108) = rxt(k,496)*y(k,154)
         mat(k,755) = -(rxt(k,497)*y(k,259))
         mat(k,2243) = -rxt(k,497)*y(k,2)
         mat(k,2075) = rxt(k,494)*y(k,223)
         mat(k,1107) = rxt(k,494)*y(k,107)
         mat(k,1087) = -(rxt(k,576)*y(k,156) + rxt(k,577)*y(k,166) + rxt(k,578) &
                      *y(k,259))
         mat(k,2342) = -rxt(k,576)*y(k,6)
         mat(k,2732) = -rxt(k,577)*y(k,6)
         mat(k,2267) = -rxt(k,578)*y(k,6)
         mat(k,190) = -(rxt(k,535)*y(k,259))
         mat(k,2167) = -rxt(k,535)*y(k,7)
         mat(k,468) = -(rxt(k,538)*y(k,259))
         mat(k,2210) = -rxt(k,538)*y(k,8)
         mat(k,2052) = rxt(k,536)*y(k,225)
         mat(k,576) = rxt(k,536)*y(k,107)
         mat(k,191) = .120_r8*rxt(k,535)*y(k,259)
         mat(k,2168) = .120_r8*rxt(k,535)*y(k,7)
         mat(k,1085) = .100_r8*rxt(k,577)*y(k,166)
         mat(k,1041) = .100_r8*rxt(k,580)*y(k,166)
         mat(k,2721) = .100_r8*rxt(k,577)*y(k,6) + .100_r8*rxt(k,580)*y(k,140)
         mat(k,1945) = .500_r8*rxt(k,537)*y(k,225) + .200_r8*rxt(k,564)*y(k,266) &
                      + .060_r8*rxt(k,570)*y(k,269)
         mat(k,577) = .500_r8*rxt(k,537)*y(k,154)
         mat(k,821) = .200_r8*rxt(k,564)*y(k,154)
         mat(k,852) = .060_r8*rxt(k,570)*y(k,154)
         mat(k,1939) = .200_r8*rxt(k,564)*y(k,266) + .200_r8*rxt(k,570)*y(k,269)
         mat(k,820) = .200_r8*rxt(k,564)*y(k,154)
         mat(k,850) = .200_r8*rxt(k,570)*y(k,154)
         mat(k,1955) = .200_r8*rxt(k,564)*y(k,266) + .150_r8*rxt(k,570)*y(k,269)
         mat(k,823) = .200_r8*rxt(k,564)*y(k,154)
         mat(k,853) = .150_r8*rxt(k,570)*y(k,154)
         mat(k,1940) = .210_r8*rxt(k,570)*y(k,269)
         mat(k,851) = .210_r8*rxt(k,570)*y(k,154)
         mat(k,281) = -(rxt(k,498)*y(k,259))
         mat(k,2183) = -rxt(k,498)*y(k,15)
         mat(k,1084) = .050_r8*rxt(k,577)*y(k,166)
         mat(k,1040) = .050_r8*rxt(k,580)*y(k,166)
         mat(k,2720) = .050_r8*rxt(k,577)*y(k,6) + .050_r8*rxt(k,580)*y(k,140)
         mat(k,415) = -(rxt(k,464)*y(k,156) + rxt(k,465)*y(k,259))
         mat(k,2333) = -rxt(k,464)*y(k,16)
         mat(k,2203) = -rxt(k,465)*y(k,16)
         mat(k,2571) = -(rxt(k,286)*y(k,51) + rxt(k,287)*y(k,107) + rxt(k,288) &
                      *y(k,155) + rxt(k,289)*y(k,166) + rxt(k,296)*y(k,22) + rxt(k,325) &
                      *y(k,126))
         mat(k,1916) = -rxt(k,286)*y(k,17)
         mat(k,2138) = -rxt(k,287)*y(k,17)
         mat(k,2705) = -rxt(k,288)*y(k,17)
         mat(k,2774) = -rxt(k,289)*y(k,17)
         mat(k,945) = -rxt(k,296)*y(k,17)
         mat(k,2831) = -rxt(k,325)*y(k,17)
         mat(k,547) = rxt(k,285)*y(k,259)
         mat(k,2600) = 4.000_r8*rxt(k,290)*y(k,21) + (rxt(k,291)+rxt(k,292))*y(k,74) &
                      + rxt(k,599)*y(k,83) + rxt(k,315)*y(k,116) + (rxt(k,326) &
                       +rxt(k,327))*y(k,126) + rxt(k,295)*y(k,154) + rxt(k,300) &
                      *y(k,164) + rxt(k,610)*y(k,183) + rxt(k,301)*y(k,259)
         mat(k,173) = rxt(k,275)*y(k,255)
         mat(k,178) = rxt(k,305)*y(k,255)
         mat(k,567) = 2.000_r8*rxt(k,359)*y(k,70) + 2.000_r8*rxt(k,386)*y(k,255) &
                      + 2.000_r8*rxt(k,360)*y(k,259)
         mat(k,149) = rxt(k,361)*y(k,259)
         mat(k,711) = rxt(k,364)*y(k,70) + rxt(k,387)*y(k,255) + rxt(k,365)*y(k,259)
         mat(k,121) = 2.000_r8*rxt(k,371)*y(k,259)
         mat(k,455) = 3.000_r8*rxt(k,372)*y(k,70) + 3.000_r8*rxt(k,306)*y(k,255) &
                      + 3.000_r8*rxt(k,373)*y(k,259)
         mat(k,125) = rxt(k,374)*y(k,259)
         mat(k,2485) = 2.000_r8*rxt(k,359)*y(k,45) + rxt(k,364)*y(k,52) &
                      + 3.000_r8*rxt(k,372)*y(k,66)
         mat(k,2423) = (rxt(k,291)+rxt(k,292))*y(k,21)
         mat(k,1175) = rxt(k,599)*y(k,21)
         mat(k,129) = 2.000_r8*rxt(k,307)*y(k,255)
         mat(k,1597) = rxt(k,302)*y(k,164) + rxt(k,308)*y(k,255) + rxt(k,303)*y(k,259)
         mat(k,2541) = rxt(k,315)*y(k,21)
         mat(k,2831) = mat(k,2831) + (rxt(k,326)+rxt(k,327))*y(k,21)
         mat(k,2022) = rxt(k,295)*y(k,21)
         mat(k,2647) = rxt(k,300)*y(k,21) + rxt(k,302)*y(k,97)
         mat(k,1638) = rxt(k,610)*y(k,21)
         mat(k,1885) = rxt(k,275)*y(k,38) + rxt(k,305)*y(k,39) + 2.000_r8*rxt(k,386) &
                      *y(k,45) + rxt(k,387)*y(k,52) + 3.000_r8*rxt(k,306)*y(k,66) &
                      + 2.000_r8*rxt(k,307)*y(k,94) + rxt(k,308)*y(k,97)
         mat(k,2321) = rxt(k,285)*y(k,18) + rxt(k,301)*y(k,21) + 2.000_r8*rxt(k,360) &
                      *y(k,45) + rxt(k,361)*y(k,46) + rxt(k,365)*y(k,52) &
                      + 2.000_r8*rxt(k,371)*y(k,65) + 3.000_r8*rxt(k,373)*y(k,66) &
                      + rxt(k,374)*y(k,67) + rxt(k,303)*y(k,97)
         mat(k,544) = -(rxt(k,285)*y(k,259))
         mat(k,2219) = -rxt(k,285)*y(k,18)
         mat(k,2550) = rxt(k,296)*y(k,22)
         mat(k,937) = rxt(k,296)*y(k,17)
         mat(k,1587) = (rxt(k,627)+rxt(k,701)+rxt(k,714)+rxt(k,723))*y(k,109)
         mat(k,1689) = (rxt(k,627)+rxt(k,701)+rxt(k,714)+rxt(k,723))*y(k,97)
         mat(k,2580) = rxt(k,293)*y(k,74)
         mat(k,938) = rxt(k,297)*y(k,70)
         mat(k,2445) = rxt(k,297)*y(k,22)
         mat(k,2405) = rxt(k,293)*y(k,21)
         mat(k,1588) = (rxt(k,626)+rxt(k,703)+rxt(k,711)+rxt(k,720))*y(k,110)
         mat(k,1825) = (rxt(k,629)+rxt(k,700)+rxt(k,713)+rxt(k,722))*y(k,109)
         mat(k,1690) = (rxt(k,629)+rxt(k,700)+rxt(k,713)+rxt(k,722))*y(k,101)
         mat(k,1800) = (rxt(k,626)+rxt(k,703)+rxt(k,711)+rxt(k,720))*y(k,97)
         mat(k,2549) = rxt(k,288)*y(k,155)
         mat(k,2658) = rxt(k,288)*y(k,17)
         mat(k,2601) = -(4._r8*rxt(k,290)*y(k,21) + (rxt(k,291) + rxt(k,292) + rxt(k,293) &
                      ) * y(k,74) + rxt(k,294)*y(k,107) + rxt(k,295)*y(k,154) &
                      + rxt(k,298)*y(k,155) + rxt(k,300)*y(k,164) + rxt(k,301) &
                      *y(k,259) + rxt(k,315)*y(k,116) + (rxt(k,326) + rxt(k,327) &
                      ) * y(k,126) + rxt(k,599)*y(k,83) + rxt(k,610)*y(k,183))
         mat(k,2424) = -(rxt(k,291) + rxt(k,292) + rxt(k,293)) * y(k,21)
         mat(k,2139) = -rxt(k,294)*y(k,21)
         mat(k,2023) = -rxt(k,295)*y(k,21)
         mat(k,2706) = -rxt(k,298)*y(k,21)
         mat(k,2648) = -rxt(k,300)*y(k,21)
         mat(k,2322) = -rxt(k,301)*y(k,21)
         mat(k,2542) = -rxt(k,315)*y(k,21)
         mat(k,2832) = -(rxt(k,326) + rxt(k,327)) * y(k,21)
         mat(k,1176) = -rxt(k,599)*y(k,21)
         mat(k,1639) = -rxt(k,610)*y(k,21)
         mat(k,2572) = rxt(k,325)*y(k,126) + rxt(k,289)*y(k,166)
         mat(k,946) = rxt(k,299)*y(k,164)
         mat(k,1598) = rxt(k,309)*y(k,255)
         mat(k,1705) = rxt(k,304)*y(k,164)
         mat(k,2832) = mat(k,2832) + rxt(k,325)*y(k,17)
         mat(k,2648) = mat(k,2648) + rxt(k,299)*y(k,22) + rxt(k,304)*y(k,109)
         mat(k,2775) = rxt(k,289)*y(k,17)
         mat(k,1886) = rxt(k,309)*y(k,97)
         mat(k,939) = -(rxt(k,296)*y(k,17) + rxt(k,297)*y(k,70) + rxt(k,299)*y(k,164))
         mat(k,2552) = -rxt(k,296)*y(k,22)
         mat(k,2450) = -rxt(k,297)*y(k,22)
         mat(k,2620) = -rxt(k,299)*y(k,22)
         mat(k,2582) = rxt(k,298)*y(k,155)
         mat(k,2673) = rxt(k,298)*y(k,21)
         mat(k,284) = -(rxt(k,539)*y(k,259))
         mat(k,2184) = -rxt(k,539)*y(k,24)
         mat(k,1937) = rxt(k,542)*y(k,227)
         mat(k,504) = rxt(k,542)*y(k,154)
         mat(k,378) = -(rxt(k,541)*y(k,259))
         mat(k,2197) = -rxt(k,541)*y(k,25)
         mat(k,2047) = rxt(k,540)*y(k,227)
         mat(k,505) = rxt(k,540)*y(k,107)
         mat(k,218) = -(rxt(k,355)*y(k,70) + rxt(k,356)*y(k,259))
         mat(k,2431) = -rxt(k,355)*y(k,26)
         mat(k,2171) = -rxt(k,356)*y(k,26)
         mat(k,328) = -(rxt(k,412)*y(k,70) + rxt(k,413)*y(k,259))
         mat(k,2434) = -rxt(k,412)*y(k,27)
         mat(k,2191) = -rxt(k,413)*y(k,27)
         mat(k,645) = -(rxt(k,414)*y(k,70) + rxt(k,415)*y(k,166) + rxt(k,440)*y(k,259))
         mat(k,2446) = -rxt(k,414)*y(k,28)
         mat(k,2723) = -rxt(k,415)*y(k,28)
         mat(k,2232) = -rxt(k,440)*y(k,28)
         mat(k,290) = -(rxt(k,357)*y(k,70) + rxt(k,358)*y(k,259))
         mat(k,2433) = -rxt(k,357)*y(k,29)
         mat(k,2186) = -rxt(k,358)*y(k,29)
         mat(k,304) = -(rxt(k,420)*y(k,259))
         mat(k,2188) = -rxt(k,420)*y(k,30)
         mat(k,1001) = .800_r8*rxt(k,416)*y(k,228) + .200_r8*rxt(k,417)*y(k,232)
         mat(k,1711) = .200_r8*rxt(k,417)*y(k,228)
         mat(k,388) = -(rxt(k,421)*y(k,259))
         mat(k,2199) = -rxt(k,421)*y(k,31)
         mat(k,2049) = rxt(k,418)*y(k,228)
         mat(k,1002) = rxt(k,418)*y(k,107)
         mat(k,337) = -(rxt(k,422)*y(k,70) + rxt(k,423)*y(k,259))
         mat(k,2435) = -rxt(k,422)*y(k,32)
         mat(k,2192) = -rxt(k,423)*y(k,32)
         mat(k,1245) = -(rxt(k,443)*y(k,156) + rxt(k,444)*y(k,166) + rxt(k,462) &
                      *y(k,259))
         mat(k,2354) = -rxt(k,443)*y(k,33)
         mat(k,2741) = -rxt(k,444)*y(k,33)
         mat(k,2280) = -rxt(k,462)*y(k,33)
         mat(k,966) = .130_r8*rxt(k,522)*y(k,166)
         mat(k,2741) = mat(k,2741) + .130_r8*rxt(k,522)*y(k,128)
         mat(k,480) = -(rxt(k,448)*y(k,259))
         mat(k,2212) = -rxt(k,448)*y(k,34)
         mat(k,2054) = rxt(k,446)*y(k,229)
         mat(k,1022) = rxt(k,446)*y(k,107)
         mat(k,343) = -(rxt(k,449)*y(k,259) + rxt(k,452)*y(k,70))
         mat(k,2193) = -rxt(k,449)*y(k,35)
         mat(k,2436) = -rxt(k,452)*y(k,35)
         mat(k,308) = -(rxt(k,545)*y(k,259))
         mat(k,2189) = -rxt(k,545)*y(k,36)
         mat(k,2044) = rxt(k,543)*y(k,230)
         mat(k,740) = rxt(k,543)*y(k,107)
         mat(k,112) = -(rxt(k,274)*y(k,255))
         mat(k,1845) = -rxt(k,274)*y(k,37)
         mat(k,169) = -(rxt(k,275)*y(k,255))
         mat(k,1850) = -rxt(k,275)*y(k,38)
         mat(k,174) = -(rxt(k,305)*y(k,255))
         mat(k,1851) = -rxt(k,305)*y(k,39)
         mat(k,134) = -(rxt(k,276)*y(k,255))
         mat(k,1847) = -rxt(k,276)*y(k,40)
         mat(k,179) = -(rxt(k,277)*y(k,255))
         mat(k,1852) = -rxt(k,277)*y(k,41)
         mat(k,138) = -(rxt(k,278)*y(k,255))
         mat(k,1848) = -rxt(k,278)*y(k,42)
         mat(k,184) = -(rxt(k,279)*y(k,255))
         mat(k,1853) = -rxt(k,279)*y(k,43)
         mat(k,142) = -(rxt(k,280)*y(k,255))
         mat(k,1849) = -rxt(k,280)*y(k,44)
         mat(k,562) = -(rxt(k,359)*y(k,70) + rxt(k,360)*y(k,259) + rxt(k,386)*y(k,255))
         mat(k,2443) = -rxt(k,359)*y(k,45)
         mat(k,2222) = -rxt(k,360)*y(k,45)
         mat(k,1863) = -rxt(k,386)*y(k,45)
         mat(k,146) = -(rxt(k,361)*y(k,259))
         mat(k,2164) = -rxt(k,361)*y(k,46)
         mat(k,349) = -(rxt(k,362)*y(k,70) + rxt(k,363)*y(k,259))
         mat(k,2437) = -rxt(k,362)*y(k,47)
         mat(k,2194) = -rxt(k,363)*y(k,47)
         mat(k,1907) = -(rxt(k,247)*y(k,70) + rxt(k,286)*y(k,17) + rxt(k,391)*y(k,107) &
                      + rxt(k,392)*y(k,156) + rxt(k,393)*y(k,164) + rxt(k,394) &
                      *y(k,259))
         mat(k,2476) = -rxt(k,247)*y(k,51)
         mat(k,2562) = -rxt(k,286)*y(k,51)
         mat(k,2129) = -rxt(k,391)*y(k,51)
         mat(k,2384) = -rxt(k,392)*y(k,51)
         mat(k,2638) = -rxt(k,393)*y(k,51)
         mat(k,2312) = -rxt(k,394)*y(k,51)
         mat(k,772) = .400_r8*rxt(k,493)*y(k,259)
         mat(k,1100) = .340_r8*rxt(k,577)*y(k,166)
         mat(k,419) = .500_r8*rxt(k,464)*y(k,156)
         mat(k,649) = rxt(k,415)*y(k,166)
         mat(k,1255) = .500_r8*rxt(k,444)*y(k,166)
         mat(k,674) = .500_r8*rxt(k,432)*y(k,259)
         mat(k,905) = rxt(k,399)*y(k,259)
         mat(k,494) = .300_r8*rxt(k,400)*y(k,259)
         mat(k,1655) = (rxt(k,408)+rxt(k,409))*y(k,255)
         mat(k,1234) = rxt(k,375)*y(k,232)
         mat(k,2414) = rxt(k,256)*y(k,232)
         mat(k,1280) = .800_r8*rxt(k,437)*y(k,259)
         mat(k,2129) = mat(k,2129) + .450_r8*rxt(k,480)*y(k,246) + .150_r8*rxt(k,459) &
                      *y(k,263)
         mat(k,975) = .910_r8*rxt(k,522)*y(k,166)
         mat(k,665) = .300_r8*rxt(k,513)*y(k,259)
         mat(k,1402) = .120_r8*rxt(k,475)*y(k,166)
         mat(k,681) = .500_r8*rxt(k,488)*y(k,259)
         mat(k,1056) = .340_r8*rxt(k,580)*y(k,166)
         mat(k,1510) = .600_r8*rxt(k,489)*y(k,166)
         mat(k,2013) = .100_r8*rxt(k,495)*y(k,223) + rxt(k,398)*y(k,232) &
                      + .500_r8*rxt(k,466)*y(k,235) + .500_r8*rxt(k,434)*y(k,237) &
                      + .920_r8*rxt(k,505)*y(k,239) + .250_r8*rxt(k,473)*y(k,244) &
                      + rxt(k,482)*y(k,246) + rxt(k,456)*y(k,262) + rxt(k,460) &
                      *y(k,263) + .340_r8*rxt(k,589)*y(k,264) + .320_r8*rxt(k,594) &
                      *y(k,265) + .250_r8*rxt(k,530)*y(k,268)
         mat(k,2384) = mat(k,2384) + .500_r8*rxt(k,464)*y(k,16) + rxt(k,506)*y(k,239) &
                      + .250_r8*rxt(k,472)*y(k,244) + rxt(k,483)*y(k,246)
         mat(k,2765) = .340_r8*rxt(k,577)*y(k,6) + rxt(k,415)*y(k,28) &
                      + .500_r8*rxt(k,444)*y(k,33) + .910_r8*rxt(k,522)*y(k,128) &
                      + .120_r8*rxt(k,475)*y(k,135) + .340_r8*rxt(k,580)*y(k,140) &
                      + .600_r8*rxt(k,489)*y(k,141)
         mat(k,633) = rxt(k,439)*y(k,259)
         mat(k,1224) = .680_r8*rxt(k,598)*y(k,259)
         mat(k,1117) = .100_r8*rxt(k,495)*y(k,154)
         mat(k,1008) = .700_r8*rxt(k,417)*y(k,232)
         mat(k,1028) = rxt(k,445)*y(k,232)
         mat(k,1560) = rxt(k,428)*y(k,232) + rxt(k,502)*y(k,239) + .250_r8*rxt(k,469) &
                      *y(k,244) + rxt(k,478)*y(k,246) + .250_r8*rxt(k,527)*y(k,268)
         mat(k,1753) = rxt(k,375)*y(k,68) + rxt(k,256)*y(k,74) + rxt(k,398)*y(k,154) &
                      + .700_r8*rxt(k,417)*y(k,228) + rxt(k,445)*y(k,229) + rxt(k,428) &
                      *y(k,231) + (4.000_r8*rxt(k,395)+2.000_r8*rxt(k,396))*y(k,232) &
                      + 1.500_r8*rxt(k,503)*y(k,239) + .750_r8*rxt(k,508)*y(k,240) &
                      + .800_r8*rxt(k,517)*y(k,241) + .880_r8*rxt(k,470)*y(k,244) &
                      + 2.000_r8*rxt(k,479)*y(k,246) + .750_r8*rxt(k,582)*y(k,254) &
                      + .800_r8*rxt(k,458)*y(k,263) + .930_r8*rxt(k,587)*y(k,264) &
                      + .950_r8*rxt(k,592)*y(k,265) + .800_r8*rxt(k,528)*y(k,268)
         mat(k,657) = .500_r8*rxt(k,466)*y(k,154)
         mat(k,880) = .500_r8*rxt(k,434)*y(k,154)
         mat(k,1433) = .920_r8*rxt(k,505)*y(k,154) + rxt(k,506)*y(k,156) + rxt(k,502) &
                      *y(k,231) + 1.500_r8*rxt(k,503)*y(k,232)
         mat(k,1466) = .750_r8*rxt(k,508)*y(k,232)
         mat(k,1387) = .800_r8*rxt(k,517)*y(k,232)
         mat(k,1488) = .250_r8*rxt(k,473)*y(k,154) + .250_r8*rxt(k,472)*y(k,156) &
                      + .250_r8*rxt(k,469)*y(k,231) + .880_r8*rxt(k,470)*y(k,232)
         mat(k,1528) = .450_r8*rxt(k,480)*y(k,107) + rxt(k,482)*y(k,154) + rxt(k,483) &
                      *y(k,156) + rxt(k,478)*y(k,231) + 2.000_r8*rxt(k,479)*y(k,232) &
                      + 4.000_r8*rxt(k,481)*y(k,246)
         mat(k,1210) = .750_r8*rxt(k,582)*y(k,232)
         mat(k,1876) = (rxt(k,408)+rxt(k,409))*y(k,64)
         mat(k,2312) = mat(k,2312) + .400_r8*rxt(k,493)*y(k,1) + .500_r8*rxt(k,432) &
                      *y(k,60) + rxt(k,399)*y(k,62) + .300_r8*rxt(k,400)*y(k,63) &
                      + .800_r8*rxt(k,437)*y(k,90) + .300_r8*rxt(k,513)*y(k,129) &
                      + .500_r8*rxt(k,488)*y(k,139) + rxt(k,439)*y(k,172) &
                      + .680_r8*rxt(k,598)*y(k,212)
         mat(k,898) = rxt(k,456)*y(k,154)
         mat(k,1348) = .150_r8*rxt(k,459)*y(k,107) + rxt(k,460)*y(k,154) &
                      + .800_r8*rxt(k,458)*y(k,232)
         mat(k,1295) = .340_r8*rxt(k,589)*y(k,154) + .930_r8*rxt(k,587)*y(k,232)
         mat(k,1146) = .320_r8*rxt(k,594)*y(k,154) + .950_r8*rxt(k,592)*y(k,232)
         mat(k,1365) = .250_r8*rxt(k,530)*y(k,154) + .250_r8*rxt(k,527)*y(k,231) &
                      + .800_r8*rxt(k,528)*y(k,232)
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
         mat(k,704) = -(rxt(k,364)*y(k,70) + rxt(k,365)*y(k,259) + rxt(k,387)*y(k,255))
         mat(k,2448) = -rxt(k,364)*y(k,52)
         mat(k,2239) = -rxt(k,365)*y(k,52)
         mat(k,1864) = -rxt(k,387)*y(k,52)
         mat(k,150) = -(rxt(k,366)*y(k,259))
         mat(k,2165) = -rxt(k,366)*y(k,53)
         mat(k,1263) = -(rxt(k,424)*y(k,156) + rxt(k,425)*y(k,259))
         mat(k,2355) = -rxt(k,424)*y(k,54)
         mat(k,2281) = -rxt(k,425)*y(k,54)
         mat(k,770) = .800_r8*rxt(k,493)*y(k,259)
         mat(k,418) = rxt(k,464)*y(k,156)
         mat(k,305) = rxt(k,420)*y(k,259)
         mat(k,390) = .500_r8*rxt(k,421)*y(k,259)
         mat(k,1246) = .500_r8*rxt(k,444)*y(k,166)
         mat(k,2100) = .200_r8*rxt(k,484)*y(k,248)
         mat(k,1498) = .100_r8*rxt(k,489)*y(k,166)
         mat(k,1986) = .400_r8*rxt(k,495)*y(k,223) + rxt(k,419)*y(k,228) &
                      + .270_r8*rxt(k,447)*y(k,229) + rxt(k,466)*y(k,235) + rxt(k,485) &
                      *y(k,248) + rxt(k,456)*y(k,262)
         mat(k,2355) = mat(k,2355) + rxt(k,464)*y(k,16)
         mat(k,2742) = .500_r8*rxt(k,444)*y(k,33) + .100_r8*rxt(k,489)*y(k,141)
         mat(k,1113) = .400_r8*rxt(k,495)*y(k,154)
         mat(k,1005) = rxt(k,419)*y(k,154) + 3.200_r8*rxt(k,416)*y(k,228) &
                      + .800_r8*rxt(k,417)*y(k,232)
         mat(k,1025) = .270_r8*rxt(k,447)*y(k,154)
         mat(k,1730) = .800_r8*rxt(k,417)*y(k,228)
         mat(k,655) = rxt(k,466)*y(k,154)
         mat(k,778) = .200_r8*rxt(k,484)*y(k,107) + rxt(k,485)*y(k,154)
         mat(k,2281) = mat(k,2281) + .800_r8*rxt(k,493)*y(k,1) + rxt(k,420)*y(k,30) &
                      + .500_r8*rxt(k,421)*y(k,31)
         mat(k,895) = rxt(k,456)*y(k,154)
         mat(k,442) = -(rxt(k,367)*y(k,70) + rxt(k,368)*y(k,259))
         mat(k,2441) = -rxt(k,367)*y(k,55)
         mat(k,2206) = -rxt(k,368)*y(k,55)
         mat(k,115) = -(rxt(k,426)*y(k,259))
         mat(k,2161) = -rxt(k,426)*y(k,56)
         mat(k,1160) = -(rxt(k,463)*y(k,259))
         mat(k,2273) = -rxt(k,463)*y(k,57)
         mat(k,769) = .800_r8*rxt(k,493)*y(k,259)
         mat(k,1092) = .520_r8*rxt(k,577)*y(k,166)
         mat(k,417) = .500_r8*rxt(k,464)*y(k,156)
         mat(k,1048) = .520_r8*rxt(k,580)*y(k,166)
         mat(k,1981) = .250_r8*rxt(k,495)*y(k,223) + .820_r8*rxt(k,447)*y(k,229) &
                      + .500_r8*rxt(k,466)*y(k,235) + .270_r8*rxt(k,589)*y(k,264) &
                      + .040_r8*rxt(k,594)*y(k,265)
         mat(k,2347) = .500_r8*rxt(k,464)*y(k,16)
         mat(k,2737) = .520_r8*rxt(k,577)*y(k,6) + .520_r8*rxt(k,580)*y(k,140)
         mat(k,1217) = .500_r8*rxt(k,598)*y(k,259)
         mat(k,1112) = .250_r8*rxt(k,495)*y(k,154)
         mat(k,1024) = .820_r8*rxt(k,447)*y(k,154) + .820_r8*rxt(k,445)*y(k,232)
         mat(k,1725) = .820_r8*rxt(k,445)*y(k,229) + .150_r8*rxt(k,587)*y(k,264) &
                      + .025_r8*rxt(k,592)*y(k,265)
         mat(k,654) = .500_r8*rxt(k,466)*y(k,154)
         mat(k,2273) = mat(k,2273) + .800_r8*rxt(k,493)*y(k,1) + .500_r8*rxt(k,598) &
                      *y(k,212)
         mat(k,1286) = .270_r8*rxt(k,589)*y(k,154) + .150_r8*rxt(k,587)*y(k,232)
         mat(k,1144) = .040_r8*rxt(k,594)*y(k,154) + .025_r8*rxt(k,592)*y(k,232)
         mat(k,1408) = -(rxt(k,450)*y(k,156) + rxt(k,451)*y(k,259))
         mat(k,2366) = -rxt(k,450)*y(k,58)
         mat(k,2292) = -rxt(k,451)*y(k,58)
         mat(k,2110) = .070_r8*rxt(k,547)*y(k,233) + .070_r8*rxt(k,553)*y(k,247)
         mat(k,1321) = rxt(k,453)*y(k,259)
         mat(k,1397) = .880_r8*rxt(k,475)*y(k,166)
         mat(k,1501) = .500_r8*rxt(k,489)*y(k,166)
         mat(k,1996) = .170_r8*rxt(k,548)*y(k,233) + .050_r8*rxt(k,511)*y(k,240) &
                      + .250_r8*rxt(k,473)*y(k,244) + .170_r8*rxt(k,554)*y(k,247) &
                      + .400_r8*rxt(k,564)*y(k,266) + .250_r8*rxt(k,530)*y(k,268) &
                      + .540_r8*rxt(k,570)*y(k,269) + .510_r8*rxt(k,573)*y(k,271)
         mat(k,2366) = mat(k,2366) + .050_r8*rxt(k,512)*y(k,240) + .250_r8*rxt(k,472) &
                      *y(k,244) + .250_r8*rxt(k,531)*y(k,268)
         mat(k,981) = rxt(k,454)*y(k,259)
         mat(k,2750) = .880_r8*rxt(k,475)*y(k,135) + .500_r8*rxt(k,489)*y(k,141)
         mat(k,1549) = .250_r8*rxt(k,469)*y(k,244) + .250_r8*rxt(k,527)*y(k,268)
         mat(k,1739) = .240_r8*rxt(k,470)*y(k,244) + .500_r8*rxt(k,458)*y(k,263) &
                      + .100_r8*rxt(k,528)*y(k,268)
         mat(k,869) = .070_r8*rxt(k,547)*y(k,107) + .170_r8*rxt(k,548)*y(k,154)
         mat(k,1458) = .050_r8*rxt(k,511)*y(k,154) + .050_r8*rxt(k,512)*y(k,156)
         mat(k,1482) = .250_r8*rxt(k,473)*y(k,154) + .250_r8*rxt(k,472)*y(k,156) &
                      + .250_r8*rxt(k,469)*y(k,231) + .240_r8*rxt(k,470)*y(k,232)
         mat(k,1016) = .070_r8*rxt(k,553)*y(k,107) + .170_r8*rxt(k,554)*y(k,154)
         mat(k,2292) = mat(k,2292) + rxt(k,453)*y(k,114) + rxt(k,454)*y(k,157)
         mat(k,1345) = .500_r8*rxt(k,458)*y(k,232)
         mat(k,830) = .400_r8*rxt(k,564)*y(k,154)
         mat(k,1361) = .250_r8*rxt(k,530)*y(k,154) + .250_r8*rxt(k,531)*y(k,156) &
                      + .250_r8*rxt(k,527)*y(k,231) + .100_r8*rxt(k,528)*y(k,232)
         mat(k,861) = .540_r8*rxt(k,570)*y(k,154)
         mat(k,595) = .510_r8*rxt(k,573)*y(k,154)
         mat(k,794) = -(rxt(k,431)*y(k,259))
         mat(k,2247) = -rxt(k,431)*y(k,59)
         mat(k,1241) = .120_r8*rxt(k,444)*y(k,166)
         mat(k,2078) = .150_r8*rxt(k,429)*y(k,231) + .150_r8*rxt(k,480)*y(k,246)
         mat(k,2726) = .120_r8*rxt(k,444)*y(k,33)
         mat(k,1540) = .150_r8*rxt(k,429)*y(k,107) + .100_r8*rxt(k,428)*y(k,232)
         mat(k,1717) = .100_r8*rxt(k,428)*y(k,231)
         mat(k,1521) = .150_r8*rxt(k,480)*y(k,107)
         mat(k,670) = -(rxt(k,432)*y(k,259))
         mat(k,2235) = -rxt(k,432)*y(k,60)
         mat(k,2070) = .360_r8*rxt(k,429)*y(k,231) + .400_r8*rxt(k,480)*y(k,246)
         mat(k,1539) = .360_r8*rxt(k,429)*y(k,107)
         mat(k,1519) = .400_r8*rxt(k,480)*y(k,107)
         mat(k,434) = -(rxt(k,369)*y(k,70) + rxt(k,370)*y(k,259))
         mat(k,2440) = -rxt(k,369)*y(k,61)
         mat(k,2205) = -rxt(k,370)*y(k,61)
         mat(k,904) = -(rxt(k,399)*y(k,259))
         mat(k,2257) = -rxt(k,399)*y(k,62)
         mat(k,1003) = .300_r8*rxt(k,417)*y(k,232)
         mat(k,1718) = .300_r8*rxt(k,417)*y(k,228) + 2.000_r8*rxt(k,396)*y(k,232) &
                      + .250_r8*rxt(k,503)*y(k,239) + .250_r8*rxt(k,508)*y(k,240) &
                      + .200_r8*rxt(k,517)*y(k,241) + .250_r8*rxt(k,470)*y(k,244) &
                      + .250_r8*rxt(k,582)*y(k,254) + .500_r8*rxt(k,458)*y(k,263) &
                      + .250_r8*rxt(k,587)*y(k,264) + .250_r8*rxt(k,592)*y(k,265) &
                      + .300_r8*rxt(k,528)*y(k,268)
         mat(k,1418) = .250_r8*rxt(k,503)*y(k,232)
         mat(k,1447) = .250_r8*rxt(k,508)*y(k,232)
         mat(k,1374) = .200_r8*rxt(k,517)*y(k,232)
         mat(k,1476) = .250_r8*rxt(k,470)*y(k,232)
         mat(k,1203) = .250_r8*rxt(k,582)*y(k,232)
         mat(k,1342) = .500_r8*rxt(k,458)*y(k,232)
         mat(k,1285) = .250_r8*rxt(k,587)*y(k,232)
         mat(k,1141) = .250_r8*rxt(k,592)*y(k,232)
         mat(k,1355) = .300_r8*rxt(k,528)*y(k,232)
         mat(k,492) = -(rxt(k,400)*y(k,259))
         mat(k,2214) = -rxt(k,400)*y(k,63)
         mat(k,2056) = rxt(k,397)*y(k,232)
         mat(k,1715) = rxt(k,397)*y(k,107)
         mat(k,1650) = -(rxt(k,248)*y(k,70) + rxt(k,349)*y(k,89) + rxt(k,401)*y(k,259) &
                      + (rxt(k,407) + rxt(k,408) + rxt(k,409)) * y(k,255))
         mat(k,2469) = -rxt(k,248)*y(k,64)
         mat(k,989) = -rxt(k,349)*y(k,64)
         mat(k,2304) = -rxt(k,401)*y(k,64)
         mat(k,1868) = -(rxt(k,407) + rxt(k,408) + rxt(k,409)) * y(k,64)
         mat(k,1252) = .100_r8*rxt(k,444)*y(k,166)
         mat(k,2759) = .100_r8*rxt(k,444)*y(k,33)
         mat(k,118) = -(rxt(k,371)*y(k,259))
         mat(k,2162) = -rxt(k,371)*y(k,65)
         mat(k,450) = -(rxt(k,306)*y(k,255) + rxt(k,372)*y(k,70) + rxt(k,373)*y(k,259))
         mat(k,1862) = -rxt(k,306)*y(k,66)
         mat(k,2442) = -rxt(k,372)*y(k,66)
         mat(k,2207) = -rxt(k,373)*y(k,66)
         mat(k,122) = -(rxt(k,374)*y(k,259))
         mat(k,2163) = -rxt(k,374)*y(k,67)
         mat(k,1229) = -((rxt(k,375) + rxt(k,376)) * y(k,232) + (rxt(k,377) + rxt(k,378) &
                      ) * y(k,107) + rxt(k,379)*y(k,154) + rxt(k,380)*y(k,156))
         mat(k,1729) = -(rxt(k,375) + rxt(k,376)) * y(k,68)
         mat(k,2099) = -(rxt(k,377) + rxt(k,378)) * y(k,68)
         mat(k,1985) = -rxt(k,379)*y(k,68)
         mat(k,2353) = -rxt(k,380)*y(k,68)
         mat(k,350) = rxt(k,362)*y(k,70) + rxt(k,363)*y(k,259)
         mat(k,2459) = rxt(k,362)*y(k,47)
         mat(k,2279) = rxt(k,363)*y(k,47)
         mat(k,400) = -(rxt(k,381)*y(k,70) + rxt(k,382)*y(k,259))
         mat(k,2439) = -rxt(k,381)*y(k,69)
         mat(k,2202) = -rxt(k,382)*y(k,69)
         mat(k,2482) = -(rxt(k,247)*y(k,51) + rxt(k,248)*y(k,64) + rxt(k,249)*y(k,93) &
                      + rxt(k,250)*y(k,95) + (rxt(k,251) + rxt(k,252)) * y(k,107) &
                      + rxt(k,253)*y(k,155) + rxt(k,255)*y(k,166) + rxt(k,262)*y(k,75) &
                      + rxt(k,271)*y(k,110) + rxt(k,297)*y(k,22) + rxt(k,355)*y(k,26) &
                      + rxt(k,357)*y(k,29) + rxt(k,359)*y(k,45) + rxt(k,362)*y(k,47) &
                      + rxt(k,364)*y(k,52) + rxt(k,367)*y(k,55) + rxt(k,369)*y(k,61) &
                      + rxt(k,372)*y(k,66) + rxt(k,422)*y(k,32) + rxt(k,452)*y(k,35) &
                      + (rxt(k,600) + rxt(k,601)) * y(k,83))
         mat(k,1913) = -rxt(k,247)*y(k,70)
         mat(k,1660) = -rxt(k,248)*y(k,70)
         mat(k,1623) = -rxt(k,249)*y(k,70)
         mat(k,701) = -rxt(k,250)*y(k,70)
         mat(k,2135) = -(rxt(k,251) + rxt(k,252)) * y(k,70)
         mat(k,2702) = -rxt(k,253)*y(k,70)
         mat(k,2771) = -rxt(k,255)*y(k,70)
         mat(k,1135) = -rxt(k,262)*y(k,70)
         mat(k,1812) = -rxt(k,271)*y(k,70)
         mat(k,943) = -rxt(k,297)*y(k,70)
         mat(k,221) = -rxt(k,355)*y(k,70)
         mat(k,293) = -rxt(k,357)*y(k,70)
         mat(k,566) = -rxt(k,359)*y(k,70)
         mat(k,353) = -rxt(k,362)*y(k,70)
         mat(k,710) = -rxt(k,364)*y(k,70)
         mat(k,448) = -rxt(k,367)*y(k,70)
         mat(k,439) = -rxt(k,369)*y(k,70)
         mat(k,454) = -rxt(k,372)*y(k,70)
         mat(k,341) = -rxt(k,422)*y(k,70)
         mat(k,347) = -rxt(k,452)*y(k,70)
         mat(k,1172) = -(rxt(k,600) + rxt(k,601)) * y(k,70)
         mat(k,2597) = rxt(k,292)*y(k,74)
         mat(k,221) = mat(k,221) + 5.000_r8*rxt(k,355)*y(k,70) + 3.060_r8*rxt(k,356) &
                      *y(k,259)
         mat(k,293) = mat(k,293) + 2.000_r8*rxt(k,357)*y(k,70) + 2.000_r8*rxt(k,358) &
                      *y(k,259)
         mat(k,114) = 4.000_r8*rxt(k,274)*y(k,255)
         mat(k,172) = rxt(k,275)*y(k,255)
         mat(k,137) = 2.000_r8*rxt(k,276)*y(k,255)
         mat(k,183) = 2.000_r8*rxt(k,277)*y(k,255)
         mat(k,141) = 2.000_r8*rxt(k,278)*y(k,255)
         mat(k,188) = rxt(k,279)*y(k,255)
         mat(k,145) = 2.000_r8*rxt(k,280)*y(k,255)
         mat(k,148) = rxt(k,361)*y(k,259)
         mat(k,152) = 3.000_r8*rxt(k,366)*y(k,259)
         mat(k,448) = mat(k,448) + rxt(k,368)*y(k,259)
         mat(k,120) = rxt(k,371)*y(k,259)
         mat(k,124) = 2.000_r8*rxt(k,374)*y(k,259)
         mat(k,1238) = rxt(k,378)*y(k,107) + 2.000_r8*rxt(k,379)*y(k,154) &
                      + 2.000_r8*rxt(k,380)*y(k,156) + 2.000_r8*rxt(k,375)*y(k,232)
         mat(k,405) = rxt(k,382)*y(k,259)
         mat(k,2482) = mat(k,2482) + 5.000_r8*rxt(k,355)*y(k,26) + 2.000_r8*rxt(k,357) &
                      *y(k,29)
         mat(k,2420) = rxt(k,292)*y(k,21) + (4.000_r8*rxt(k,257)+2.000_r8*rxt(k,259)) &
                      *y(k,74) + rxt(k,329)*y(k,126) + rxt(k,261)*y(k,154) &
                      + rxt(k,266)*y(k,164) + rxt(k,611)*y(k,183) + rxt(k,256) &
                      *y(k,232) + rxt(k,267)*y(k,259)
         mat(k,267) = rxt(k,354)*y(k,255)
         mat(k,263) = rxt(k,388)*y(k,255) + rxt(k,383)*y(k,259)
         mat(k,272) = rxt(k,389)*y(k,255) + rxt(k,384)*y(k,259)
         mat(k,317) = rxt(k,390)*y(k,255) + rxt(k,385)*y(k,259)
         mat(k,1835) = rxt(k,269)*y(k,164) + rxt(k,281)*y(k,255) + rxt(k,270)*y(k,259)
         mat(k,2135) = mat(k,2135) + rxt(k,378)*y(k,68)
         mat(k,2828) = rxt(k,329)*y(k,74)
         mat(k,2019) = 2.000_r8*rxt(k,379)*y(k,68) + rxt(k,261)*y(k,74)
         mat(k,2390) = 2.000_r8*rxt(k,380)*y(k,68)
         mat(k,2644) = rxt(k,266)*y(k,74) + rxt(k,269)*y(k,101)
         mat(k,1637) = rxt(k,611)*y(k,74)
         mat(k,1759) = 2.000_r8*rxt(k,375)*y(k,68) + rxt(k,256)*y(k,74)
         mat(k,1882) = 4.000_r8*rxt(k,274)*y(k,37) + rxt(k,275)*y(k,38) &
                      + 2.000_r8*rxt(k,276)*y(k,40) + 2.000_r8*rxt(k,277)*y(k,41) &
                      + 2.000_r8*rxt(k,278)*y(k,42) + rxt(k,279)*y(k,43) &
                      + 2.000_r8*rxt(k,280)*y(k,44) + rxt(k,354)*y(k,81) + rxt(k,388) &
                      *y(k,98) + rxt(k,389)*y(k,99) + rxt(k,390)*y(k,100) + rxt(k,281) &
                      *y(k,101)
         mat(k,2318) = 3.060_r8*rxt(k,356)*y(k,26) + 2.000_r8*rxt(k,358)*y(k,29) &
                      + rxt(k,361)*y(k,46) + 3.000_r8*rxt(k,366)*y(k,53) + rxt(k,368) &
                      *y(k,55) + rxt(k,371)*y(k,65) + 2.000_r8*rxt(k,374)*y(k,67) &
                      + rxt(k,382)*y(k,69) + rxt(k,267)*y(k,74) + rxt(k,383)*y(k,98) &
                      + rxt(k,384)*y(k,99) + rxt(k,385)*y(k,100) + rxt(k,270)*y(k,101)
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
         mat(k,2432) = rxt(k,262)*y(k,75)
         mat(k,2402) = 2.000_r8*rxt(k,258)*y(k,74)
         mat(k,1128) = rxt(k,262)*y(k,70) + (rxt(k,709)+rxt(k,718)+rxt(k,727)) &
                      *y(k,101)
         mat(k,1822) = (rxt(k,709)+rxt(k,718)+rxt(k,727))*y(k,75) + (rxt(k,628) &
                       +rxt(k,699)+rxt(k,710)+rxt(k,719))*y(k,110)
         mat(k,1798) = (rxt(k,628)+rxt(k,699)+rxt(k,710)+rxt(k,719))*y(k,101)
         mat(k,2401) = 2.000_r8*rxt(k,283)*y(k,74)
         mat(k,606) = -(rxt(k,254)*y(k,259))
         mat(k,2227) = -rxt(k,254)*y(k,73)
         mat(k,2444) = rxt(k,253)*y(k,155)
         mat(k,1824) = rxt(k,644)*y(k,144)
         mat(k,408) = rxt(k,644)*y(k,101)
         mat(k,2665) = rxt(k,253)*y(k,70)
         mat(k,2419) = -(rxt(k,256)*y(k,232) + (4._r8*rxt(k,257) + 4._r8*rxt(k,258) &
                      + 4._r8*rxt(k,259) + 4._r8*rxt(k,283)) * y(k,74) + rxt(k,260) &
                      *y(k,107) + rxt(k,261)*y(k,154) + rxt(k,263)*y(k,155) + rxt(k,266) &
                      *y(k,164) + (rxt(k,267) + rxt(k,268)) * y(k,259) + (rxt(k,291) &
                      + rxt(k,292) + rxt(k,293)) * y(k,21) + (rxt(k,328) + rxt(k,329) &
                      + rxt(k,330)) * y(k,126) + rxt(k,611)*y(k,183))
         mat(k,1758) = -rxt(k,256)*y(k,74)
         mat(k,2134) = -rxt(k,260)*y(k,74)
         mat(k,2018) = -rxt(k,261)*y(k,74)
         mat(k,2701) = -rxt(k,263)*y(k,74)
         mat(k,2643) = -rxt(k,266)*y(k,74)
         mat(k,2317) = -(rxt(k,267) + rxt(k,268)) * y(k,74)
         mat(k,2596) = -(rxt(k,291) + rxt(k,292) + rxt(k,293)) * y(k,74)
         mat(k,2827) = -(rxt(k,328) + rxt(k,329) + rxt(k,330)) * y(k,74)
         mat(k,1636) = -rxt(k,611)*y(k,74)
         mat(k,2481) = rxt(k,252)*y(k,107) + rxt(k,271)*y(k,110) + rxt(k,255)*y(k,166)
         mat(k,1134) = rxt(k,264)*y(k,164)
         mat(k,1834) = rxt(k,282)*y(k,255)
         mat(k,2134) = mat(k,2134) + rxt(k,252)*y(k,70)
         mat(k,1811) = rxt(k,271)*y(k,70) + rxt(k,272)*y(k,164) + rxt(k,273)*y(k,259)
         mat(k,2643) = mat(k,2643) + rxt(k,264)*y(k,75) + rxt(k,272)*y(k,110)
         mat(k,2770) = rxt(k,255)*y(k,70)
         mat(k,537) = rxt(k,616)*y(k,183)
         mat(k,1636) = mat(k,1636) + rxt(k,616)*y(k,168)
         mat(k,1881) = rxt(k,282)*y(k,101)
         mat(k,2317) = mat(k,2317) + rxt(k,273)*y(k,110)
         mat(k,1129) = -(rxt(k,262)*y(k,70) + rxt(k,264)*y(k,164) + rxt(k,265) &
                      *y(k,259) + (rxt(k,709) + rxt(k,718) + rxt(k,727)) * y(k,101))
         mat(k,2455) = -rxt(k,262)*y(k,75)
         mat(k,2622) = -rxt(k,264)*y(k,75)
         mat(k,2270) = -rxt(k,265)*y(k,75)
         mat(k,1826) = -(rxt(k,709) + rxt(k,718) + rxt(k,727)) * y(k,75)
         mat(k,2406) = rxt(k,263)*y(k,155)
         mat(k,2678) = rxt(k,263)*y(k,74)
         mat(k,1272) = -(rxt(k,411)*y(k,259))
         mat(k,2282) = -rxt(k,411)*y(k,77)
         mat(k,1095) = .230_r8*rxt(k,577)*y(k,166)
         mat(k,2553) = rxt(k,286)*y(k,51)
         mat(k,331) = .350_r8*rxt(k,413)*y(k,259)
         mat(k,648) = .630_r8*rxt(k,415)*y(k,166)
         mat(k,1247) = .560_r8*rxt(k,444)*y(k,166)
         mat(k,1895) = rxt(k,286)*y(k,17) + rxt(k,247)*y(k,70) + rxt(k,392)*y(k,156) &
                      + rxt(k,393)*y(k,164) + rxt(k,394)*y(k,259)
         mat(k,443) = rxt(k,367)*y(k,70)
         mat(k,1407) = rxt(k,450)*y(k,156) + rxt(k,451)*y(k,259)
         mat(k,1230) = rxt(k,378)*y(k,107) + rxt(k,379)*y(k,154) + rxt(k,380)*y(k,156) + ( &
                      + rxt(k,375)+rxt(k,376))*y(k,232)
         mat(k,2461) = rxt(k,247)*y(k,51) + rxt(k,367)*y(k,55)
         mat(k,1576) = rxt(k,752)*y(k,260)
         mat(k,1124) = rxt(k,438)*y(k,259)
         mat(k,2101) = rxt(k,378)*y(k,68) + .070_r8*rxt(k,547)*y(k,233) &
                      + .160_r8*rxt(k,550)*y(k,245) + .140_r8*rxt(k,553)*y(k,247)
         mat(k,967) = .620_r8*rxt(k,522)*y(k,166)
         mat(k,1395) = .650_r8*rxt(k,475)*y(k,166)
         mat(k,1051) = .230_r8*rxt(k,580)*y(k,166)
         mat(k,1499) = .560_r8*rxt(k,489)*y(k,166)
         mat(k,1987) = rxt(k,379)*y(k,68) + .170_r8*rxt(k,548)*y(k,233) &
                      + .220_r8*rxt(k,473)*y(k,244) + .400_r8*rxt(k,551)*y(k,245) &
                      + .350_r8*rxt(k,554)*y(k,247) + .225_r8*rxt(k,589)*y(k,264) &
                      + .250_r8*rxt(k,530)*y(k,268)
         mat(k,2356) = rxt(k,392)*y(k,51) + rxt(k,450)*y(k,58) + rxt(k,380)*y(k,68) &
                      + .220_r8*rxt(k,472)*y(k,244) + .500_r8*rxt(k,531)*y(k,268)
         mat(k,2624) = rxt(k,393)*y(k,51) + rxt(k,605)*y(k,169)
         mat(k,2743) = .230_r8*rxt(k,577)*y(k,6) + .630_r8*rxt(k,415)*y(k,28) &
                      + .560_r8*rxt(k,444)*y(k,33) + .620_r8*rxt(k,522)*y(k,128) &
                      + .650_r8*rxt(k,475)*y(k,135) + .230_r8*rxt(k,580)*y(k,140) &
                      + .560_r8*rxt(k,489)*y(k,141)
         mat(k,429) = rxt(k,605)*y(k,164) + rxt(k,606)*y(k,259)
         mat(k,1219) = .700_r8*rxt(k,598)*y(k,259)
         mat(k,1543) = .220_r8*rxt(k,469)*y(k,244) + .250_r8*rxt(k,527)*y(k,268)
         mat(k,1731) = (rxt(k,375)+rxt(k,376))*y(k,68) + .110_r8*rxt(k,470)*y(k,244) &
                      + .125_r8*rxt(k,587)*y(k,264) + .200_r8*rxt(k,528)*y(k,268)
         mat(k,868) = .070_r8*rxt(k,547)*y(k,107) + .170_r8*rxt(k,548)*y(k,154)
         mat(k,1477) = .220_r8*rxt(k,473)*y(k,154) + .220_r8*rxt(k,472)*y(k,156) &
                      + .220_r8*rxt(k,469)*y(k,231) + .110_r8*rxt(k,470)*y(k,232)
         mat(k,816) = .160_r8*rxt(k,550)*y(k,107) + .400_r8*rxt(k,551)*y(k,154)
         mat(k,1015) = .140_r8*rxt(k,553)*y(k,107) + .350_r8*rxt(k,554)*y(k,154)
         mat(k,2282) = mat(k,2282) + .350_r8*rxt(k,413)*y(k,27) + rxt(k,394)*y(k,51) &
                      + rxt(k,451)*y(k,58) + rxt(k,438)*y(k,91) + rxt(k,606)*y(k,169) &
                      + .700_r8*rxt(k,598)*y(k,212)
         mat(k,889) = rxt(k,752)*y(k,78)
         mat(k,1288) = .225_r8*rxt(k,589)*y(k,154) + .125_r8*rxt(k,587)*y(k,232)
         mat(k,1357) = .250_r8*rxt(k,530)*y(k,154) + .500_r8*rxt(k,531)*y(k,156) &
                      + .250_r8*rxt(k,527)*y(k,231) + .200_r8*rxt(k,528)*y(k,232)
         mat(k,1577) = -(rxt(k,752)*y(k,260))
         mat(k,890) = -rxt(k,752)*y(k,78)
         mat(k,1099) = .270_r8*rxt(k,577)*y(k,166)
         mat(k,1251) = .200_r8*rxt(k,444)*y(k,166)
         mat(k,795) = rxt(k,431)*y(k,259)
         mat(k,672) = .500_r8*rxt(k,432)*y(k,259)
         mat(k,1273) = rxt(k,411)*y(k,259)
         mat(k,1279) = .800_r8*rxt(k,437)*y(k,259)
         mat(k,1125) = rxt(k,438)*y(k,259)
         mat(k,997) = rxt(k,403)*y(k,259)
         mat(k,2117) = .490_r8*rxt(k,429)*y(k,231) + .450_r8*rxt(k,480)*y(k,246)
         mat(k,680) = .500_r8*rxt(k,488)*y(k,259)
         mat(k,1055) = .270_r8*rxt(k,580)*y(k,166)
         mat(k,1506) = .100_r8*rxt(k,489)*y(k,166)
         mat(k,2003) = rxt(k,430)*y(k,231) + .900_r8*rxt(k,589)*y(k,264)
         mat(k,2757) = .270_r8*rxt(k,577)*y(k,6) + .200_r8*rxt(k,444)*y(k,33) &
                      + .270_r8*rxt(k,580)*y(k,140) + .100_r8*rxt(k,489)*y(k,141)
         mat(k,1222) = 1.800_r8*rxt(k,598)*y(k,259)
         mat(k,1556) = .490_r8*rxt(k,429)*y(k,107) + rxt(k,430)*y(k,154) &
                      + 4.000_r8*rxt(k,427)*y(k,231) + .900_r8*rxt(k,428)*y(k,232) &
                      + rxt(k,502)*y(k,239) + 2.000_r8*rxt(k,478)*y(k,246) &
                      + rxt(k,527)*y(k,268)
         mat(k,1746) = .900_r8*rxt(k,428)*y(k,231) + rxt(k,479)*y(k,246) &
                      + .500_r8*rxt(k,587)*y(k,264)
         mat(k,1431) = rxt(k,502)*y(k,231)
         mat(k,1526) = .450_r8*rxt(k,480)*y(k,107) + 2.000_r8*rxt(k,478)*y(k,231) &
                      + rxt(k,479)*y(k,232) + 4.000_r8*rxt(k,481)*y(k,246)
         mat(k,2299) = rxt(k,431)*y(k,59) + .500_r8*rxt(k,432)*y(k,60) + rxt(k,411) &
                      *y(k,77) + .800_r8*rxt(k,437)*y(k,90) + rxt(k,438)*y(k,91) &
                      + rxt(k,403)*y(k,103) + .500_r8*rxt(k,488)*y(k,139) &
                      + 1.800_r8*rxt(k,598)*y(k,212)
         mat(k,1293) = .900_r8*rxt(k,589)*y(k,154) + .500_r8*rxt(k,587)*y(k,232)
         mat(k,1363) = rxt(k,527)*y(k,231)
         mat(k,219) = .470_r8*rxt(k,356)*y(k,259)
         mat(k,1228) = rxt(k,377)*y(k,107) + rxt(k,376)*y(k,232)
         mat(k,399) = rxt(k,381)*y(k,70) + rxt(k,382)*y(k,259)
         mat(k,2438) = rxt(k,381)*y(k,69)
         mat(k,2050) = rxt(k,377)*y(k,68)
         mat(k,1713) = rxt(k,376)*y(k,68)
         mat(k,2201) = .470_r8*rxt(k,356)*y(k,26) + rxt(k,382)*y(k,69)
         mat(k,273) = -(rxt(k,353)*y(k,255))
         mat(k,1859) = -rxt(k,353)*y(k,80)
         mat(k,170) = rxt(k,275)*y(k,255)
         mat(k,175) = rxt(k,305)*y(k,255)
         mat(k,181) = rxt(k,277)*y(k,255)
         mat(k,139) = 2.000_r8*rxt(k,278)*y(k,255)
         mat(k,185) = 2.000_r8*rxt(k,279)*y(k,255)
         mat(k,143) = rxt(k,280)*y(k,255)
         mat(k,127) = 2.000_r8*rxt(k,307)*y(k,255)
         mat(k,269) = rxt(k,389)*y(k,255) + rxt(k,384)*y(k,259)
         mat(k,312) = rxt(k,390)*y(k,255) + rxt(k,385)*y(k,259)
         mat(k,1859) = mat(k,1859) + rxt(k,275)*y(k,38) + rxt(k,305)*y(k,39) &
                      + rxt(k,277)*y(k,41) + 2.000_r8*rxt(k,278)*y(k,42) &
                      + 2.000_r8*rxt(k,279)*y(k,43) + rxt(k,280)*y(k,44) &
                      + 2.000_r8*rxt(k,307)*y(k,94) + rxt(k,389)*y(k,99) + rxt(k,390) &
                      *y(k,100)
         mat(k,2181) = rxt(k,384)*y(k,99) + rxt(k,385)*y(k,100)
         mat(k,264) = -(rxt(k,354)*y(k,255))
         mat(k,1857) = -rxt(k,354)*y(k,81)
         mat(k,135) = rxt(k,276)*y(k,255)
         mat(k,180) = rxt(k,277)*y(k,255)
         mat(k,260) = rxt(k,388)*y(k,255) + rxt(k,383)*y(k,259)
         mat(k,1857) = mat(k,1857) + rxt(k,276)*y(k,40) + rxt(k,277)*y(k,41) &
                      + rxt(k,388)*y(k,98)
         mat(k,2179) = rxt(k,383)*y(k,98)
         mat(k,230) = -(rxt(k,546)*y(k,259))
         mat(k,2173) = -rxt(k,546)*y(k,82)
         mat(k,224) = .180_r8*rxt(k,566)*y(k,259)
         mat(k,2173) = mat(k,2173) + .180_r8*rxt(k,566)*y(k,214)
         mat(k,1166) = -(rxt(k,599)*y(k,21) + (rxt(k,600) + rxt(k,601)) * y(k,70) &
                      + rxt(k,602)*y(k,126) + rxt(k,603)*y(k,156) + (rxt(k,604) &
                      + rxt(k,618)) * y(k,259))
         mat(k,2583) = -rxt(k,599)*y(k,83)
         mat(k,2457) = -(rxt(k,600) + rxt(k,601)) * y(k,83)
         mat(k,2812) = -rxt(k,602)*y(k,83)
         mat(k,2348) = -rxt(k,603)*y(k,83)
         mat(k,2274) = -(rxt(k,604) + rxt(k,618)) * y(k,83)
         mat(k,2042) = rxt(k,433)*y(k,237)
         mat(k,875) = rxt(k,433)*y(k,107)
         mat(k,987) = -(rxt(k,349)*y(k,64) + rxt(k,350)*y(k,93) + rxt(k,351)*y(k,272) &
                      + rxt(k,352)*y(k,106))
         mat(k,1646) = -rxt(k,349)*y(k,89)
         mat(k,1615) = -rxt(k,350)*y(k,89)
         mat(k,2842) = -rxt(k,351)*y(k,89)
         mat(k,2494) = -rxt(k,352)*y(k,89)
         mat(k,176) = rxt(k,305)*y(k,255)
         mat(k,186) = rxt(k,279)*y(k,255)
         mat(k,274) = 2.000_r8*rxt(k,353)*y(k,255)
         mat(k,265) = rxt(k,354)*y(k,255)
         mat(k,1865) = rxt(k,305)*y(k,39) + rxt(k,279)*y(k,43) + 2.000_r8*rxt(k,353) &
                      *y(k,80) + rxt(k,354)*y(k,81)
         mat(k,1278) = -(rxt(k,437)*y(k,259))
         mat(k,2283) = -rxt(k,437)*y(k,90)
         mat(k,662) = .700_r8*rxt(k,513)*y(k,259)
         mat(k,623) = .500_r8*rxt(k,514)*y(k,259)
         mat(k,458) = rxt(k,525)*y(k,259)
         mat(k,1988) = .050_r8*rxt(k,511)*y(k,240) + .530_r8*rxt(k,473)*y(k,244) &
                      + .225_r8*rxt(k,589)*y(k,264) + .250_r8*rxt(k,530)*y(k,268)
         mat(k,2357) = .050_r8*rxt(k,512)*y(k,240) + .530_r8*rxt(k,472)*y(k,244) &
                      + .250_r8*rxt(k,531)*y(k,268)
         mat(k,1780) = rxt(k,436)*y(k,236)
         mat(k,1544) = .530_r8*rxt(k,469)*y(k,244) + .250_r8*rxt(k,527)*y(k,268)
         mat(k,1732) = .260_r8*rxt(k,470)*y(k,244) + .125_r8*rxt(k,587)*y(k,264) &
                      + .100_r8*rxt(k,528)*y(k,268)
         mat(k,540) = rxt(k,436)*y(k,165)
         mat(k,1452) = .050_r8*rxt(k,511)*y(k,154) + .050_r8*rxt(k,512)*y(k,156)
         mat(k,1478) = .530_r8*rxt(k,473)*y(k,154) + .530_r8*rxt(k,472)*y(k,156) &
                      + .530_r8*rxt(k,469)*y(k,231) + .260_r8*rxt(k,470)*y(k,232)
         mat(k,2283) = mat(k,2283) + .700_r8*rxt(k,513)*y(k,129) + .500_r8*rxt(k,514) &
                      *y(k,130) + rxt(k,525)*y(k,145)
         mat(k,1289) = .225_r8*rxt(k,589)*y(k,154) + .125_r8*rxt(k,587)*y(k,232)
         mat(k,1358) = .250_r8*rxt(k,530)*y(k,154) + .250_r8*rxt(k,531)*y(k,156) &
                      + .250_r8*rxt(k,527)*y(k,231) + .100_r8*rxt(k,528)*y(k,232)
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
         mat(k,1123) = -(rxt(k,438)*y(k,259))
         mat(k,2269) = -rxt(k,438)*y(k,91)
         mat(k,330) = .650_r8*rxt(k,413)*y(k,259)
         mat(k,1276) = .200_r8*rxt(k,437)*y(k,259)
         mat(k,2093) = .160_r8*rxt(k,550)*y(k,245) + .070_r8*rxt(k,553)*y(k,247)
         mat(k,1188) = rxt(k,526)*y(k,259)
         mat(k,1978) = rxt(k,537)*y(k,225) + .050_r8*rxt(k,511)*y(k,240) &
                      + .400_r8*rxt(k,551)*y(k,245) + .170_r8*rxt(k,554)*y(k,247) &
                      + .700_r8*rxt(k,557)*y(k,261) + .600_r8*rxt(k,564)*y(k,266) &
                      + .250_r8*rxt(k,530)*y(k,268) + .340_r8*rxt(k,570)*y(k,269) &
                      + .170_r8*rxt(k,573)*y(k,271)
         mat(k,2344) = .050_r8*rxt(k,512)*y(k,240) + .250_r8*rxt(k,531)*y(k,268)
         mat(k,580) = rxt(k,537)*y(k,154)
         mat(k,1541) = .250_r8*rxt(k,527)*y(k,268)
         mat(k,1722) = .100_r8*rxt(k,528)*y(k,268)
         mat(k,1450) = .050_r8*rxt(k,511)*y(k,154) + .050_r8*rxt(k,512)*y(k,156)
         mat(k,815) = .160_r8*rxt(k,550)*y(k,107) + .400_r8*rxt(k,551)*y(k,154)
         mat(k,1014) = .070_r8*rxt(k,553)*y(k,107) + .170_r8*rxt(k,554)*y(k,154)
         mat(k,2269) = mat(k,2269) + .650_r8*rxt(k,413)*y(k,27) + .200_r8*rxt(k,437) &
                      *y(k,90) + rxt(k,526)*y(k,146)
         mat(k,526) = .700_r8*rxt(k,557)*y(k,154)
         mat(k,828) = .600_r8*rxt(k,564)*y(k,154)
         mat(k,1356) = .250_r8*rxt(k,530)*y(k,154) + .250_r8*rxt(k,531)*y(k,156) &
                      + .250_r8*rxt(k,527)*y(k,231) + .100_r8*rxt(k,528)*y(k,232)
         mat(k,859) = .340_r8*rxt(k,570)*y(k,154)
         mat(k,594) = .170_r8*rxt(k,573)*y(k,154)
         mat(k,2803) = -((rxt(k,202) + rxt(k,203) + rxt(k,204)) * y(k,107) + rxt(k,205) &
                      *y(k,165) + rxt(k,208)*y(k,166))
         mat(k,2143) = -(rxt(k,202) + rxt(k,203) + rxt(k,204)) * y(k,92)
         mat(k,1796) = -rxt(k,205)*y(k,92)
         mat(k,2779) = -rxt(k,208)*y(k,92)
         mat(k,1921) = rxt(k,394)*y(k,259)
         mat(k,1664) = rxt(k,408)*y(k,255)
         mat(k,2490) = rxt(k,249)*y(k,93)
         mat(k,994) = rxt(k,350)*y(k,93)
         mat(k,1626) = rxt(k,249)*y(k,70) + rxt(k,350)*y(k,89) + rxt(k,200)*y(k,164) &
                      + rxt(k,182)*y(k,255) + rxt(k,209)*y(k,259)
         mat(k,1600) = rxt(k,309)*y(k,255)
         mat(k,1842) = rxt(k,282)*y(k,255)
         mat(k,1077) = rxt(k,232)*y(k,259)
         mat(k,2652) = rxt(k,200)*y(k,93) + rxt(k,212)*y(k,259)
         mat(k,433) = rxt(k,606)*y(k,259)
         mat(k,841) = rxt(k,612)*y(k,259)
         mat(k,1643) = rxt(k,617)*y(k,259)
         mat(k,1890) = rxt(k,408)*y(k,64) + rxt(k,182)*y(k,93) + rxt(k,309)*y(k,97) &
                      + rxt(k,282)*y(k,101)
         mat(k,2326) = rxt(k,394)*y(k,51) + rxt(k,209)*y(k,93) + rxt(k,232)*y(k,142) &
                      + rxt(k,212)*y(k,164) + rxt(k,606)*y(k,169) + rxt(k,612) &
                      *y(k,181) + rxt(k,617)*y(k,183)
         mat(k,1616) = -(rxt(k,182)*y(k,255) + rxt(k,200)*y(k,164) + rxt(k,209) &
                      *y(k,259) + rxt(k,249)*y(k,70) + rxt(k,350)*y(k,89))
         mat(k,1867) = -rxt(k,182)*y(k,93)
         mat(k,2628) = -rxt(k,200)*y(k,93)
         mat(k,2302) = -rxt(k,209)*y(k,93)
         mat(k,2467) = -rxt(k,249)*y(k,93)
         mat(k,988) = -rxt(k,350)*y(k,93)
         mat(k,1649) = rxt(k,409)*y(k,255)
         mat(k,2782) = rxt(k,202)*y(k,107)
         mat(k,2120) = rxt(k,202)*y(k,92)
         mat(k,1867) = mat(k,1867) + rxt(k,409)*y(k,64)
         mat(k,126) = -(rxt(k,307)*y(k,255))
         mat(k,1846) = -rxt(k,307)*y(k,94)
         mat(k,697) = -(rxt(k,201)*y(k,164) + rxt(k,210)*y(k,259) + rxt(k,250)*y(k,70))
         mat(k,2612) = -rxt(k,201)*y(k,95)
         mat(k,2238) = -rxt(k,210)*y(k,95)
         mat(k,2447) = -rxt(k,250)*y(k,95)
         mat(k,2072) = 2.000_r8*rxt(k,216)*y(k,107)
         mat(k,2238) = mat(k,2238) + 2.000_r8*rxt(k,215)*y(k,259)
         mat(k,299) = rxt(k,619)*y(k,272)
         mat(k,2839) = rxt(k,619)*y(k,185)
         mat(k,1589) = -(rxt(k,302)*y(k,164) + rxt(k,303)*y(k,259) + (rxt(k,308) &
                      + rxt(k,309)) * y(k,255) + (rxt(k,626) + rxt(k,703) + rxt(k,711) &
                      + rxt(k,720)) * y(k,110) + (rxt(k,627) + rxt(k,701) + rxt(k,714) &
                      + rxt(k,723)) * y(k,109) + (rxt(k,634) + rxt(k,730) + rxt(k,734) &
                      + rxt(k,738)) * y(k,111))
         mat(k,2626) = -rxt(k,302)*y(k,97)
         mat(k,2300) = -rxt(k,303)*y(k,97)
         mat(k,1866) = -(rxt(k,308) + rxt(k,309)) * y(k,97)
         mat(k,1802) = -(rxt(k,626) + rxt(k,703) + rxt(k,711) + rxt(k,720)) * y(k,97)
         mat(k,1692) = -(rxt(k,627) + rxt(k,701) + rxt(k,714) + rxt(k,723)) * y(k,97)
         mat(k,1669) = -(rxt(k,634) + rxt(k,730) + rxt(k,734) + rxt(k,738)) * y(k,97)
         mat(k,2555) = rxt(k,286)*y(k,51) + rxt(k,287)*y(k,107)
         mat(k,1897) = rxt(k,286)*y(k,17)
         mat(k,2118) = rxt(k,287)*y(k,17)
         mat(k,259) = -(rxt(k,383)*y(k,259) + rxt(k,388)*y(k,255))
         mat(k,2178) = -rxt(k,383)*y(k,98)
         mat(k,1856) = -rxt(k,388)*y(k,98)
         mat(k,268) = -(rxt(k,384)*y(k,259) + rxt(k,389)*y(k,255))
         mat(k,2180) = -rxt(k,384)*y(k,99)
         mat(k,1858) = -rxt(k,389)*y(k,99)
         mat(k,313) = -(rxt(k,385)*y(k,259) + rxt(k,390)*y(k,255))
         mat(k,2190) = -rxt(k,385)*y(k,100)
         mat(k,1861) = -rxt(k,390)*y(k,100)
         mat(k,1830) = -(rxt(k,269)*y(k,164) + rxt(k,270)*y(k,259) + (rxt(k,281) &
                      + rxt(k,282)) * y(k,255) + (rxt(k,628) + rxt(k,699) + rxt(k,710) &
                      + rxt(k,719)) * y(k,110) + (rxt(k,629) + rxt(k,700) + rxt(k,713) &
                      + rxt(k,722)) * y(k,109) + (rxt(k,633) + rxt(k,729) + rxt(k,733) &
                      + rxt(k,737)) * y(k,111) + rxt(k,644)*y(k,144) + (rxt(k,709) &
                      + rxt(k,718) + rxt(k,727)) * y(k,75))
         mat(k,2636) = -rxt(k,269)*y(k,101)
         mat(k,2310) = -rxt(k,270)*y(k,101)
         mat(k,1874) = -(rxt(k,281) + rxt(k,282)) * y(k,101)
         mat(k,1807) = -(rxt(k,628) + rxt(k,699) + rxt(k,710) + rxt(k,719)) * y(k,101)
         mat(k,1697) = -(rxt(k,629) + rxt(k,700) + rxt(k,713) + rxt(k,722)) * y(k,101)
         mat(k,1674) = -(rxt(k,633) + rxt(k,729) + rxt(k,733) + rxt(k,737)) * y(k,101)
         mat(k,409) = -rxt(k,644)*y(k,101)
         mat(k,1131) = -(rxt(k,709) + rxt(k,718) + rxt(k,727)) * y(k,101)
         mat(k,291) = rxt(k,357)*y(k,70)
         mat(k,339) = rxt(k,422)*y(k,70)
         mat(k,345) = rxt(k,452)*y(k,70)
         mat(k,563) = rxt(k,359)*y(k,70)
         mat(k,351) = rxt(k,362)*y(k,70)
         mat(k,1905) = rxt(k,247)*y(k,70)
         mat(k,706) = rxt(k,364)*y(k,70)
         mat(k,445) = 2.000_r8*rxt(k,367)*y(k,70)
         mat(k,436) = rxt(k,369)*y(k,70)
         mat(k,1653) = rxt(k,248)*y(k,70)
         mat(k,451) = rxt(k,372)*y(k,70)
         mat(k,403) = rxt(k,381)*y(k,70)
         mat(k,2474) = rxt(k,357)*y(k,29) + rxt(k,422)*y(k,32) + rxt(k,452)*y(k,35) &
                      + rxt(k,359)*y(k,45) + rxt(k,362)*y(k,47) + rxt(k,247)*y(k,51) &
                      + rxt(k,364)*y(k,52) + 2.000_r8*rxt(k,367)*y(k,55) + rxt(k,369) &
                      *y(k,61) + rxt(k,248)*y(k,64) + rxt(k,372)*y(k,66) + rxt(k,381) &
                      *y(k,69) + rxt(k,601)*y(k,83) + rxt(k,249)*y(k,93) + rxt(k,250) &
                      *y(k,95) + rxt(k,251)*y(k,107) + rxt(k,271)*y(k,110)
         mat(k,2412) = rxt(k,268)*y(k,259)
         mat(k,1168) = rxt(k,601)*y(k,70)
         mat(k,1619) = rxt(k,249)*y(k,70)
         mat(k,698) = rxt(k,250)*y(k,70)
         mat(k,2127) = rxt(k,251)*y(k,70)
         mat(k,1807) = mat(k,1807) + rxt(k,271)*y(k,70)
         mat(k,2310) = mat(k,2310) + rxt(k,268)*y(k,74)
         mat(k,210) = -(rxt(k,402)*y(k,259) + rxt(k,410)*y(k,255))
         mat(k,2170) = -rxt(k,402)*y(k,102)
         mat(k,1855) = -rxt(k,410)*y(k,102)
         mat(k,996) = -(rxt(k,403)*y(k,259))
         mat(k,2261) = -rxt(k,403)*y(k,103)
         mat(k,1086) = .050_r8*rxt(k,577)*y(k,166)
         mat(k,329) = .350_r8*rxt(k,413)*y(k,259)
         mat(k,647) = .370_r8*rxt(k,415)*y(k,166)
         mat(k,1244) = .120_r8*rxt(k,444)*y(k,166)
         mat(k,2088) = rxt(k,404)*y(k,238)
         mat(k,965) = .110_r8*rxt(k,522)*y(k,166)
         mat(k,1394) = .330_r8*rxt(k,475)*y(k,166)
         mat(k,1042) = .050_r8*rxt(k,580)*y(k,166)
         mat(k,1496) = .120_r8*rxt(k,489)*y(k,166)
         mat(k,1972) = rxt(k,406)*y(k,238)
         mat(k,2730) = .050_r8*rxt(k,577)*y(k,6) + .370_r8*rxt(k,415)*y(k,28) &
                      + .120_r8*rxt(k,444)*y(k,33) + .110_r8*rxt(k,522)*y(k,128) &
                      + .330_r8*rxt(k,475)*y(k,135) + .050_r8*rxt(k,580)*y(k,140) &
                      + .120_r8*rxt(k,489)*y(k,141)
         mat(k,513) = rxt(k,404)*y(k,107) + rxt(k,406)*y(k,154)
         mat(k,2261) = mat(k,2261) + .350_r8*rxt(k,413)*y(k,27)
         mat(k,1645) = rxt(k,349)*y(k,89)
         mat(k,986) = rxt(k,349)*y(k,64) + rxt(k,350)*y(k,93) + rxt(k,352)*y(k,106) &
                      + rxt(k,351)*y(k,272)
         mat(k,1614) = rxt(k,350)*y(k,89)
         mat(k,2493) = rxt(k,352)*y(k,89)
         mat(k,2841) = rxt(k,351)*y(k,89)
         mat(k,1328) = -(rxt(k,310)*y(k,156) + rxt(k,338)*y(k,259) + (rxt(k,630) &
                      + rxt(k,704) + rxt(k,712) + rxt(k,721)) * y(k,110) + (rxt(k,631) &
                      + rxt(k,702) + rxt(k,715) + rxt(k,724)) * y(k,109) + (rxt(k,635) &
                      + rxt(k,731) + rxt(k,735) + rxt(k,739)) * y(k,111))
         mat(k,2361) = -rxt(k,310)*y(k,105)
         mat(k,2287) = -rxt(k,338)*y(k,105)
         mat(k,1801) = -(rxt(k,630) + rxt(k,704) + rxt(k,712) + rxt(k,721)) * y(k,105)
         mat(k,1691) = -(rxt(k,631) + rxt(k,702) + rxt(k,715) + rxt(k,724)) * y(k,105)
         mat(k,1668) = -(rxt(k,635) + rxt(k,731) + rxt(k,735) + rxt(k,739)) * y(k,105)
         mat(k,2105) = rxt(k,316)*y(k,116)
         mat(k,2524) = rxt(k,316)*y(k,107)
         mat(k,2509) = -(rxt(k,241)*y(k,259) + rxt(k,352)*y(k,89))
         mat(k,2319) = -rxt(k,241)*y(k,106)
         mat(k,993) = -rxt(k,352)*y(k,106)
         mat(k,1914) = rxt(k,392)*y(k,156)
         mat(k,1270) = rxt(k,424)*y(k,156)
         mat(k,1414) = rxt(k,450)*y(k,156)
         mat(k,1136) = (rxt(k,709)+rxt(k,718)+rxt(k,727))*y(k,101)
         mat(k,1173) = rxt(k,603)*y(k,156)
         mat(k,1836) = (rxt(k,709)+rxt(k,718)+rxt(k,727))*y(k,75) + rxt(k,644) &
                      *y(k,144)
         mat(k,1336) = rxt(k,310)*y(k,156)
         mat(k,1679) = rxt(k,340)*y(k,156)
         mat(k,412) = rxt(k,644)*y(k,101)
         mat(k,2703) = rxt(k,240)*y(k,259)
         mat(k,2391) = rxt(k,392)*y(k,51) + rxt(k,424)*y(k,54) + rxt(k,450)*y(k,58) &
                      + rxt(k,603)*y(k,83) + rxt(k,310)*y(k,105) + rxt(k,340)*y(k,111)
         mat(k,2319) = mat(k,2319) + rxt(k,240)*y(k,155)
         mat(k,2131) = -((rxt(k,202) + rxt(k,203) + rxt(k,204)) * y(k,92) + rxt(k,206) &
                      *y(k,164) + rxt(k,207)*y(k,166) + rxt(k,211)*y(k,259) &
                      + 4._r8*rxt(k,216)*y(k,107) + rxt(k,228)*y(k,156) + rxt(k,233) &
                      *y(k,154) + rxt(k,238)*y(k,155) + (rxt(k,251) + rxt(k,252) &
                      ) * y(k,70) + rxt(k,260)*y(k,74) + rxt(k,287)*y(k,17) + rxt(k,294) &
                      *y(k,21) + rxt(k,316)*y(k,116) + rxt(k,331)*y(k,126) + rxt(k,377) &
                      *y(k,68) + rxt(k,391)*y(k,51) + rxt(k,397)*y(k,232) + rxt(k,404) &
                      *y(k,238) + rxt(k,418)*y(k,228) + rxt(k,429)*y(k,231) + rxt(k,433) &
                      *y(k,237) + rxt(k,446)*y(k,229) + rxt(k,455)*y(k,262) + rxt(k,459) &
                      *y(k,263) + rxt(k,471)*y(k,244) + rxt(k,480)*y(k,246) + rxt(k,484) &
                      *y(k,248) + rxt(k,494)*y(k,223) + rxt(k,504)*y(k,239) + rxt(k,509) &
                      *y(k,240) + rxt(k,518)*y(k,241) + rxt(k,529)*y(k,268) + rxt(k,533) &
                      *y(k,222) + rxt(k,536)*y(k,225) + rxt(k,540)*y(k,227) + rxt(k,543) &
                      *y(k,230) + rxt(k,547)*y(k,233) + rxt(k,550)*y(k,245) + rxt(k,553) &
                      *y(k,247) + rxt(k,556)*y(k,261) + rxt(k,563)*y(k,266) + rxt(k,569) &
                      *y(k,269) + rxt(k,572)*y(k,271) + rxt(k,583)*y(k,254) + rxt(k,588) &
                      *y(k,264) + rxt(k,593)*y(k,265))
         mat(k,2791) = -(rxt(k,202) + rxt(k,203) + rxt(k,204)) * y(k,107)
         mat(k,2640) = -rxt(k,206)*y(k,107)
         mat(k,2767) = -rxt(k,207)*y(k,107)
         mat(k,2314) = -rxt(k,211)*y(k,107)
         mat(k,2386) = -rxt(k,228)*y(k,107)
         mat(k,2015) = -rxt(k,233)*y(k,107)
         mat(k,2698) = -rxt(k,238)*y(k,107)
         mat(k,2478) = -(rxt(k,251) + rxt(k,252)) * y(k,107)
         mat(k,2416) = -rxt(k,260)*y(k,107)
         mat(k,2564) = -rxt(k,287)*y(k,107)
         mat(k,2593) = -rxt(k,294)*y(k,107)
         mat(k,2534) = -rxt(k,316)*y(k,107)
         mat(k,2824) = -rxt(k,331)*y(k,107)
         mat(k,1236) = -rxt(k,377)*y(k,107)
         mat(k,1909) = -rxt(k,391)*y(k,107)
         mat(k,1755) = -rxt(k,397)*y(k,107)
         mat(k,516) = -rxt(k,404)*y(k,107)
         mat(k,1010) = -rxt(k,418)*y(k,107)
         mat(k,1562) = -rxt(k,429)*y(k,107)
         mat(k,882) = -rxt(k,433)*y(k,107)
         mat(k,1030) = -rxt(k,446)*y(k,107)
         mat(k,900) = -rxt(k,455)*y(k,107)
         mat(k,1350) = -rxt(k,459)*y(k,107)
         mat(k,1490) = -rxt(k,471)*y(k,107)
         mat(k,1530) = -rxt(k,480)*y(k,107)
         mat(k,781) = -rxt(k,484)*y(k,107)
         mat(k,1119) = -rxt(k,494)*y(k,107)
         mat(k,1435) = -rxt(k,504)*y(k,107)
         mat(k,1468) = -rxt(k,509)*y(k,107)
         mat(k,1389) = -rxt(k,518)*y(k,107)
         mat(k,1367) = -rxt(k,529)*y(k,107)
         mat(k,618) = -rxt(k,533)*y(k,107)
         mat(k,582) = -rxt(k,536)*y(k,107)
         mat(k,509) = -rxt(k,540)*y(k,107)
         mat(k,744) = -rxt(k,543)*y(k,107)
         mat(k,872) = -rxt(k,547)*y(k,107)
         mat(k,818) = -rxt(k,550)*y(k,107)
         mat(k,1019) = -rxt(k,553)*y(k,107)
         mat(k,528) = -rxt(k,556)*y(k,107)
         mat(k,832) = -rxt(k,563)*y(k,107)
         mat(k,864) = -rxt(k,569)*y(k,107)
         mat(k,597) = -rxt(k,572)*y(k,107)
         mat(k,1212) = -rxt(k,583)*y(k,107)
         mat(k,1297) = -rxt(k,588)*y(k,107)
         mat(k,1148) = -rxt(k,593)*y(k,107)
         mat(k,1101) = .570_r8*rxt(k,577)*y(k,166)
         mat(k,194) = .650_r8*rxt(k,535)*y(k,259)
         mat(k,2564) = mat(k,2564) + rxt(k,286)*y(k,51)
         mat(k,2593) = mat(k,2593) + rxt(k,301)*y(k,259)
         mat(k,332) = .350_r8*rxt(k,413)*y(k,259)
         mat(k,650) = .130_r8*rxt(k,415)*y(k,166)
         mat(k,306) = rxt(k,420)*y(k,259)
         mat(k,1257) = .280_r8*rxt(k,444)*y(k,166)
         mat(k,1909) = mat(k,1909) + rxt(k,286)*y(k,17) + rxt(k,247)*y(k,70) &
                      + rxt(k,392)*y(k,156) + rxt(k,393)*y(k,164)
         mat(k,708) = rxt(k,364)*y(k,70) + rxt(k,365)*y(k,259)
         mat(k,446) = rxt(k,367)*y(k,70) + rxt(k,368)*y(k,259)
         mat(k,116) = rxt(k,426)*y(k,259)
         mat(k,437) = rxt(k,369)*y(k,70) + rxt(k,370)*y(k,259)
         mat(k,906) = rxt(k,399)*y(k,259)
         mat(k,1657) = rxt(k,408)*y(k,255)
         mat(k,1236) = mat(k,1236) + rxt(k,379)*y(k,154) + rxt(k,380)*y(k,156) + ( &
                      + 2.000_r8*rxt(k,375)+rxt(k,376))*y(k,232)
         mat(k,2478) = mat(k,2478) + rxt(k,247)*y(k,51) + rxt(k,364)*y(k,52) &
                      + rxt(k,367)*y(k,55) + rxt(k,369)*y(k,61) + rxt(k,250)*y(k,95)
         mat(k,2416) = mat(k,2416) + rxt(k,256)*y(k,232) + rxt(k,267)*y(k,259)
         mat(k,1274) = rxt(k,411)*y(k,259)
         mat(k,233) = .730_r8*rxt(k,546)*y(k,259)
         mat(k,1169) = .500_r8*rxt(k,618)*y(k,259)
         mat(k,1281) = rxt(k,437)*y(k,259)
         mat(k,1126) = rxt(k,438)*y(k,259)
         mat(k,2791) = mat(k,2791) + rxt(k,205)*y(k,165)
         mat(k,699) = rxt(k,250)*y(k,70) + rxt(k,201)*y(k,164) + rxt(k,210)*y(k,259)
         mat(k,212) = rxt(k,402)*y(k,259)
         mat(k,998) = rxt(k,403)*y(k,259)
         mat(k,2131) = mat(k,2131) + .070_r8*rxt(k,547)*y(k,233) + .160_r8*rxt(k,550) &
                      *y(k,245) + .330_r8*rxt(k,553)*y(k,247)
         mat(k,1314) = rxt(k,468)*y(k,259)
         mat(k,1324) = rxt(k,453)*y(k,259)
         mat(k,2824) = mat(k,2824) + rxt(k,337)*y(k,259)
         mat(k,976) = .370_r8*rxt(k,522)*y(k,166)
         mat(k,666) = .300_r8*rxt(k,513)*y(k,259)
         mat(k,626) = rxt(k,514)*y(k,259)
         mat(k,489) = rxt(k,521)*y(k,259)
         mat(k,1403) = .140_r8*rxt(k,475)*y(k,166)
         mat(k,358) = .200_r8*rxt(k,477)*y(k,259)
         mat(k,682) = .500_r8*rxt(k,488)*y(k,259)
         mat(k,1057) = .570_r8*rxt(k,580)*y(k,166)
         mat(k,1512) = .280_r8*rxt(k,489)*y(k,166)
         mat(k,459) = rxt(k,525)*y(k,259)
         mat(k,1197) = rxt(k,526)*y(k,259)
         mat(k,2015) = mat(k,2015) + rxt(k,379)*y(k,68) + rxt(k,495)*y(k,223) &
                      + rxt(k,537)*y(k,225) + rxt(k,542)*y(k,227) + rxt(k,419) &
                      *y(k,228) + rxt(k,447)*y(k,229) + rxt(k,398)*y(k,232) &
                      + .170_r8*rxt(k,548)*y(k,233) + rxt(k,466)*y(k,235) &
                      + .250_r8*rxt(k,434)*y(k,237) + rxt(k,406)*y(k,238) &
                      + .920_r8*rxt(k,505)*y(k,239) + .920_r8*rxt(k,511)*y(k,240) &
                      + rxt(k,519)*y(k,241) + .470_r8*rxt(k,473)*y(k,244) &
                      + .400_r8*rxt(k,551)*y(k,245) + .830_r8*rxt(k,554)*y(k,247) &
                      + rxt(k,557)*y(k,261) + rxt(k,456)*y(k,262) + .900_r8*rxt(k,589) &
                      *y(k,264) + .800_r8*rxt(k,594)*y(k,265) + rxt(k,564)*y(k,266) &
                      + rxt(k,530)*y(k,268) + rxt(k,570)*y(k,269) + rxt(k,573) &
                      *y(k,271)
         mat(k,2386) = mat(k,2386) + rxt(k,392)*y(k,51) + rxt(k,380)*y(k,68) &
                      + rxt(k,506)*y(k,239) + rxt(k,512)*y(k,240) + rxt(k,520) &
                      *y(k,241) + .470_r8*rxt(k,472)*y(k,244) + rxt(k,231)*y(k,259) &
                      + rxt(k,531)*y(k,268)
         mat(k,2640) = mat(k,2640) + rxt(k,393)*y(k,51) + rxt(k,201)*y(k,95)
         mat(k,1787) = rxt(k,205)*y(k,92) + rxt(k,436)*y(k,236)
         mat(k,2767) = mat(k,2767) + .570_r8*rxt(k,577)*y(k,6) + .130_r8*rxt(k,415) &
                      *y(k,28) + .280_r8*rxt(k,444)*y(k,33) + .370_r8*rxt(k,522) &
                      *y(k,128) + .140_r8*rxt(k,475)*y(k,135) + .570_r8*rxt(k,580) &
                      *y(k,140) + .280_r8*rxt(k,489)*y(k,141) + rxt(k,213)*y(k,259)
         mat(k,203) = .800_r8*rxt(k,558)*y(k,259)
         mat(k,1181) = rxt(k,608)*y(k,259)
         mat(k,1225) = .200_r8*rxt(k,598)*y(k,259)
         mat(k,228) = .280_r8*rxt(k,566)*y(k,259)
         mat(k,252) = .380_r8*rxt(k,568)*y(k,259)
         mat(k,257) = .630_r8*rxt(k,574)*y(k,259)
         mat(k,1119) = mat(k,1119) + rxt(k,495)*y(k,154)
         mat(k,582) = mat(k,582) + rxt(k,537)*y(k,154)
         mat(k,509) = mat(k,509) + rxt(k,542)*y(k,154)
         mat(k,1010) = mat(k,1010) + rxt(k,419)*y(k,154) + 2.400_r8*rxt(k,416) &
                      *y(k,228) + rxt(k,417)*y(k,232)
         mat(k,1030) = mat(k,1030) + rxt(k,447)*y(k,154) + rxt(k,445)*y(k,232)
         mat(k,1562) = mat(k,1562) + .900_r8*rxt(k,428)*y(k,232) + rxt(k,502)*y(k,239) &
                      + rxt(k,507)*y(k,240) + rxt(k,516)*y(k,241) + .470_r8*rxt(k,469) &
                      *y(k,244) + rxt(k,527)*y(k,268)
         mat(k,1755) = mat(k,1755) + (2.000_r8*rxt(k,375)+rxt(k,376))*y(k,68) &
                      + rxt(k,256)*y(k,74) + rxt(k,398)*y(k,154) + rxt(k,417)*y(k,228) &
                      + rxt(k,445)*y(k,229) + .900_r8*rxt(k,428)*y(k,231) &
                      + 4.000_r8*rxt(k,395)*y(k,232) + rxt(k,503)*y(k,239) &
                      + rxt(k,508)*y(k,240) + 1.200_r8*rxt(k,517)*y(k,241) &
                      + .730_r8*rxt(k,470)*y(k,244) + rxt(k,479)*y(k,246) &
                      + .500_r8*rxt(k,582)*y(k,254) + .300_r8*rxt(k,458)*y(k,263) &
                      + rxt(k,587)*y(k,264) + rxt(k,592)*y(k,265) + .800_r8*rxt(k,528) &
                      *y(k,268)
         mat(k,872) = mat(k,872) + .070_r8*rxt(k,547)*y(k,107) + .170_r8*rxt(k,548) &
                      *y(k,154)
         mat(k,659) = rxt(k,466)*y(k,154)
         mat(k,543) = rxt(k,436)*y(k,165)
         mat(k,882) = mat(k,882) + .250_r8*rxt(k,434)*y(k,154)
         mat(k,516) = mat(k,516) + rxt(k,406)*y(k,154)
         mat(k,1435) = mat(k,1435) + .920_r8*rxt(k,505)*y(k,154) + rxt(k,506)*y(k,156) &
                      + rxt(k,502)*y(k,231) + rxt(k,503)*y(k,232)
         mat(k,1468) = mat(k,1468) + .920_r8*rxt(k,511)*y(k,154) + rxt(k,512)*y(k,156) &
                      + rxt(k,507)*y(k,231) + rxt(k,508)*y(k,232)
         mat(k,1389) = mat(k,1389) + rxt(k,519)*y(k,154) + rxt(k,520)*y(k,156) &
                      + rxt(k,516)*y(k,231) + 1.200_r8*rxt(k,517)*y(k,232)
         mat(k,1490) = mat(k,1490) + .470_r8*rxt(k,473)*y(k,154) + .470_r8*rxt(k,472) &
                      *y(k,156) + .470_r8*rxt(k,469)*y(k,231) + .730_r8*rxt(k,470) &
                      *y(k,232)
         mat(k,818) = mat(k,818) + .160_r8*rxt(k,550)*y(k,107) + .400_r8*rxt(k,551) &
                      *y(k,154)
         mat(k,1530) = mat(k,1530) + rxt(k,479)*y(k,232)
         mat(k,1019) = mat(k,1019) + .330_r8*rxt(k,553)*y(k,107) + .830_r8*rxt(k,554) &
                      *y(k,154)
         mat(k,1212) = mat(k,1212) + .500_r8*rxt(k,582)*y(k,232)
         mat(k,1878) = rxt(k,408)*y(k,64)
         mat(k,2314) = mat(k,2314) + .650_r8*rxt(k,535)*y(k,7) + rxt(k,301)*y(k,21) &
                      + .350_r8*rxt(k,413)*y(k,27) + rxt(k,420)*y(k,30) + rxt(k,365) &
                      *y(k,52) + rxt(k,368)*y(k,55) + rxt(k,426)*y(k,56) + rxt(k,370) &
                      *y(k,61) + rxt(k,399)*y(k,62) + rxt(k,267)*y(k,74) + rxt(k,411) &
                      *y(k,77) + .730_r8*rxt(k,546)*y(k,82) + .500_r8*rxt(k,618) &
                      *y(k,83) + rxt(k,437)*y(k,90) + rxt(k,438)*y(k,91) + rxt(k,210) &
                      *y(k,95) + rxt(k,402)*y(k,102) + rxt(k,403)*y(k,103) &
                      + rxt(k,468)*y(k,112) + rxt(k,453)*y(k,114) + rxt(k,337) &
                      *y(k,126) + .300_r8*rxt(k,513)*y(k,129) + rxt(k,514)*y(k,130) &
                      + rxt(k,521)*y(k,131) + .200_r8*rxt(k,477)*y(k,136) &
                      + .500_r8*rxt(k,488)*y(k,139) + rxt(k,525)*y(k,145) + rxt(k,526) &
                      *y(k,146) + rxt(k,231)*y(k,156) + rxt(k,213)*y(k,166) &
                      + .800_r8*rxt(k,558)*y(k,175) + rxt(k,608)*y(k,184) &
                      + .200_r8*rxt(k,598)*y(k,212) + .280_r8*rxt(k,566)*y(k,214) &
                      + .380_r8*rxt(k,568)*y(k,216) + .630_r8*rxt(k,574)*y(k,218)
         mat(k,528) = mat(k,528) + rxt(k,557)*y(k,154)
         mat(k,900) = mat(k,900) + rxt(k,456)*y(k,154)
         mat(k,1350) = mat(k,1350) + .300_r8*rxt(k,458)*y(k,232)
         mat(k,1297) = mat(k,1297) + .900_r8*rxt(k,589)*y(k,154) + rxt(k,587)*y(k,232)
         mat(k,1148) = mat(k,1148) + .800_r8*rxt(k,594)*y(k,154) + rxt(k,592)*y(k,232)
         mat(k,832) = mat(k,832) + rxt(k,564)*y(k,154)
         mat(k,1367) = mat(k,1367) + rxt(k,530)*y(k,154) + rxt(k,531)*y(k,156) &
                      + rxt(k,527)*y(k,231) + .800_r8*rxt(k,528)*y(k,232)
         mat(k,864) = mat(k,864) + rxt(k,570)*y(k,154)
         mat(k,597) = mat(k,597) + rxt(k,573)*y(k,154)
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
         mat(k,555) = -(rxt(k,218)*y(k,259))
         mat(k,2221) = -rxt(k,218)*y(k,108)
         mat(k,2064) = rxt(k,238)*y(k,155)
         mat(k,2664) = rxt(k,238)*y(k,107)
         mat(k,1695) = -(rxt(k,304)*y(k,164) + (rxt(k,627) + rxt(k,701) + rxt(k,714) &
                      + rxt(k,723)) * y(k,97) + (rxt(k,629) + rxt(k,700) + rxt(k,713) &
                      + rxt(k,722)) * y(k,101) + (rxt(k,631) + rxt(k,702) + rxt(k,715) &
                      + rxt(k,724)) * y(k,105))
         mat(k,2632) = -rxt(k,304)*y(k,109)
         mat(k,1591) = -(rxt(k,627) + rxt(k,701) + rxt(k,714) + rxt(k,723)) * y(k,109)
         mat(k,1828) = -(rxt(k,629) + rxt(k,700) + rxt(k,713) + rxt(k,722)) * y(k,109)
         mat(k,1331) = -(rxt(k,631) + rxt(k,702) + rxt(k,715) + rxt(k,724)) * y(k,109)
         mat(k,545) = rxt(k,285)*y(k,259)
         mat(k,2586) = rxt(k,294)*y(k,107)
         mat(k,2123) = rxt(k,294)*y(k,21)
         mat(k,2306) = rxt(k,285)*y(k,18)
         mat(k,1806) = -(rxt(k,271)*y(k,70) + rxt(k,272)*y(k,164) + rxt(k,273) &
                      *y(k,259) + (rxt(k,626) + rxt(k,703) + rxt(k,711) + rxt(k,720) &
                      ) * y(k,97) + (rxt(k,628) + rxt(k,699) + rxt(k,710) + rxt(k,719) &
                      ) * y(k,101) + (rxt(k,630) + rxt(k,704) + rxt(k,712) + rxt(k,721) &
                      ) * y(k,105))
         mat(k,2473) = -rxt(k,271)*y(k,110)
         mat(k,2635) = -rxt(k,272)*y(k,110)
         mat(k,2309) = -rxt(k,273)*y(k,110)
         mat(k,1592) = -(rxt(k,626) + rxt(k,703) + rxt(k,711) + rxt(k,720)) * y(k,110)
         mat(k,1829) = -(rxt(k,628) + rxt(k,699) + rxt(k,710) + rxt(k,719)) * y(k,110)
         mat(k,1332) = -(rxt(k,630) + rxt(k,704) + rxt(k,712) + rxt(k,721)) * y(k,110)
         mat(k,1233) = rxt(k,378)*y(k,107)
         mat(k,607) = rxt(k,254)*y(k,259)
         mat(k,2411) = rxt(k,260)*y(k,107)
         mat(k,1130) = rxt(k,265)*y(k,259)
         mat(k,2126) = rxt(k,378)*y(k,68) + rxt(k,260)*y(k,74)
         mat(k,2309) = mat(k,2309) + rxt(k,254)*y(k,73) + rxt(k,265)*y(k,75)
         mat(k,1671) = -(rxt(k,311)*y(k,259) + rxt(k,340)*y(k,156) + (rxt(k,633) &
                      + rxt(k,729) + rxt(k,733) + rxt(k,737)) * y(k,101) + (rxt(k,634) &
                      + rxt(k,730) + rxt(k,734) + rxt(k,738)) * y(k,97) + (rxt(k,635) &
                      + rxt(k,731) + rxt(k,735) + rxt(k,739)) * y(k,105))
         mat(k,2305) = -rxt(k,311)*y(k,111)
         mat(k,2377) = -rxt(k,340)*y(k,111)
         mat(k,1827) = -(rxt(k,633) + rxt(k,729) + rxt(k,733) + rxt(k,737)) * y(k,111)
         mat(k,1590) = -(rxt(k,634) + rxt(k,730) + rxt(k,734) + rxt(k,738)) * y(k,111)
         mat(k,1330) = -(rxt(k,635) + rxt(k,731) + rxt(k,735) + rxt(k,739)) * y(k,111)
         mat(k,2122) = rxt(k,331)*y(k,126)
         mat(k,1604) = rxt(k,314)*y(k,259)
         mat(k,2816) = rxt(k,331)*y(k,107)
         mat(k,2305) = mat(k,2305) + rxt(k,314)*y(k,117)
         mat(k,1307) = -(rxt(k,468)*y(k,259))
         mat(k,2285) = -rxt(k,468)*y(k,112)
         mat(k,663) = .300_r8*rxt(k,513)*y(k,259)
         mat(k,624) = .500_r8*rxt(k,514)*y(k,259)
         mat(k,1990) = rxt(k,467)*y(k,235) + rxt(k,474)*y(k,244)
         mat(k,656) = rxt(k,467)*y(k,154)
         mat(k,1479) = rxt(k,474)*y(k,154)
         mat(k,2285) = mat(k,2285) + .300_r8*rxt(k,513)*y(k,129) + .500_r8*rxt(k,514) &
                      *y(k,130)
         mat(k,276) = -(rxt(k,499)*y(k,259))
         mat(k,2182) = -rxt(k,499)*y(k,113)
         mat(k,1320) = -(rxt(k,453)*y(k,259))
         mat(k,2286) = -rxt(k,453)*y(k,114)
         mat(k,664) = .700_r8*rxt(k,513)*y(k,259)
         mat(k,625) = .500_r8*rxt(k,514)*y(k,259)
         mat(k,678) = .500_r8*rxt(k,488)*y(k,259)
         mat(k,1991) = .050_r8*rxt(k,511)*y(k,240) + .220_r8*rxt(k,473)*y(k,244) &
                      + .250_r8*rxt(k,530)*y(k,268)
         mat(k,2360) = .050_r8*rxt(k,512)*y(k,240) + .220_r8*rxt(k,472)*y(k,244) &
                      + .250_r8*rxt(k,531)*y(k,268)
         mat(k,640) = .500_r8*rxt(k,457)*y(k,259)
         mat(k,1545) = .220_r8*rxt(k,469)*y(k,244) + .250_r8*rxt(k,527)*y(k,268)
         mat(k,1734) = .230_r8*rxt(k,470)*y(k,244) + .200_r8*rxt(k,458)*y(k,263) &
                      + .100_r8*rxt(k,528)*y(k,268)
         mat(k,1454) = .050_r8*rxt(k,511)*y(k,154) + .050_r8*rxt(k,512)*y(k,156)
         mat(k,1480) = .220_r8*rxt(k,473)*y(k,154) + .220_r8*rxt(k,472)*y(k,156) &
                      + .220_r8*rxt(k,469)*y(k,231) + .230_r8*rxt(k,470)*y(k,232)
         mat(k,2286) = mat(k,2286) + .700_r8*rxt(k,513)*y(k,129) + .500_r8*rxt(k,514) &
                      *y(k,130) + .500_r8*rxt(k,488)*y(k,139) + .500_r8*rxt(k,457) &
                      *y(k,179)
         mat(k,1343) = .200_r8*rxt(k,458)*y(k,232)
         mat(k,1359) = .250_r8*rxt(k,530)*y(k,154) + .250_r8*rxt(k,531)*y(k,156) &
                      + .250_r8*rxt(k,527)*y(k,231) + .100_r8*rxt(k,528)*y(k,232)
         mat(k,393) = -(rxt(k,500)*y(k,259))
         mat(k,2200) = -rxt(k,500)*y(k,115)
         mat(k,1941) = .870_r8*rxt(k,511)*y(k,240)
         mat(k,2331) = .950_r8*rxt(k,512)*y(k,240)
         mat(k,1537) = rxt(k,507)*y(k,240)
         mat(k,1712) = .750_r8*rxt(k,508)*y(k,240)
         mat(k,1443) = .870_r8*rxt(k,511)*y(k,154) + .950_r8*rxt(k,512)*y(k,156) &
                      + rxt(k,507)*y(k,231) + .750_r8*rxt(k,508)*y(k,232)
         mat(k,2540) = -(rxt(k,315)*y(k,21) + rxt(k,316)*y(k,107) + rxt(k,317) &
                      *y(k,127) + rxt(k,319)*y(k,155) + rxt(k,321)*y(k,156) + rxt(k,323) &
                      *y(k,154) + rxt(k,324)*y(k,166))
         mat(k,2599) = -rxt(k,315)*y(k,116)
         mat(k,2137) = -rxt(k,316)*y(k,116)
         mat(k,957) = -rxt(k,317)*y(k,116)
         mat(k,2704) = -rxt(k,319)*y(k,116)
         mat(k,2392) = -rxt(k,321)*y(k,116)
         mat(k,2021) = -rxt(k,323)*y(k,116)
         mat(k,2773) = -rxt(k,324)*y(k,116)
         mat(k,2570) = rxt(k,325)*y(k,126)
         mat(k,2599) = mat(k,2599) + rxt(k,326)*y(k,126)
         mat(k,440) = rxt(k,369)*y(k,70) + rxt(k,370)*y(k,259)
         mat(k,2484) = rxt(k,369)*y(k,61)
         mat(k,2422) = (rxt(k,328)+rxt(k,329))*y(k,126)
         mat(k,1174) = rxt(k,602)*y(k,126)
         mat(k,1337) = rxt(k,310)*y(k,156) + rxt(k,338)*y(k,259)
         mat(k,1609) = rxt(k,312)*y(k,156) + rxt(k,313)*y(k,164) + rxt(k,314)*y(k,259)
         mat(k,2830) = rxt(k,325)*y(k,17) + rxt(k,326)*y(k,21) + (rxt(k,328) &
                       +rxt(k,329))*y(k,74) + rxt(k,602)*y(k,83) + 2.000_r8*rxt(k,344) &
                      *y(k,126) + rxt(k,332)*y(k,154) + rxt(k,335)*y(k,164) &
                      + rxt(k,337)*y(k,259)
         mat(k,2021) = mat(k,2021) + rxt(k,332)*y(k,126)
         mat(k,2392) = mat(k,2392) + rxt(k,310)*y(k,105) + rxt(k,312)*y(k,117)
         mat(k,2646) = rxt(k,313)*y(k,117) + rxt(k,335)*y(k,126)
         mat(k,2320) = rxt(k,370)*y(k,61) + rxt(k,338)*y(k,105) + rxt(k,314)*y(k,117) &
                      + rxt(k,337)*y(k,126)
         mat(k,1603) = -(rxt(k,312)*y(k,156) + rxt(k,313)*y(k,164) + rxt(k,314) &
                      *y(k,259))
         mat(k,2374) = -rxt(k,312)*y(k,117)
         mat(k,2627) = -rxt(k,313)*y(k,117)
         mat(k,2301) = -rxt(k,314)*y(k,117)
         mat(k,1329) = (rxt(k,635)+rxt(k,731)+rxt(k,735)+rxt(k,739))*y(k,111)
         mat(k,1670) = (rxt(k,635)+rxt(k,731)+rxt(k,735)+rxt(k,739))*y(k,105)
         mat(k,2525) = rxt(k,317)*y(k,127)
         mat(k,215) = 2.000_r8*rxt(k,322)*y(k,124)
         mat(k,325) = 2.000_r8*rxt(k,318)*y(k,125)
         mat(k,952) = rxt(k,317)*y(k,116)
         mat(k,2806) = 2.000_r8*rxt(k,345)*y(k,126)
         mat(k,2807) = rxt(k,347)*y(k,170)
         mat(k,733) = rxt(k,347)*y(k,126)
         mat(k,732) = 2.000_r8*rxt(k,348)*y(k,170)
         mat(k,1586) = (rxt(k,634)+rxt(k,730)+rxt(k,734)+rxt(k,738))*y(k,111)
         mat(k,1326) = (rxt(k,631)+rxt(k,702)+rxt(k,715)+rxt(k,724))*y(k,109)
         mat(k,1688) = (rxt(k,631)+rxt(k,702)+rxt(k,715)+rxt(k,724))*y(k,105)
         mat(k,1666) = (rxt(k,634)+rxt(k,730)+rxt(k,734)+rxt(k,738))*y(k,97)
         mat(k,2404) = rxt(k,330)*y(k,126)
         mat(k,1823) = (rxt(k,633)+rxt(k,729)+rxt(k,733)+rxt(k,737))*y(k,111)
         mat(k,1327) = (rxt(k,630)+rxt(k,704)+rxt(k,712)+rxt(k,721))*y(k,110)
         mat(k,1799) = (rxt(k,630)+rxt(k,704)+rxt(k,712)+rxt(k,721))*y(k,105)
         mat(k,1667) = (rxt(k,633)+rxt(k,729)+rxt(k,733)+rxt(k,737))*y(k,101)
         mat(k,2809) = rxt(k,330)*y(k,74)
         mat(k,163) = -(rxt(k,501)*y(k,259))
         mat(k,2166) = -rxt(k,501)*y(k,123)
         mat(k,842) = .600_r8*rxt(k,524)*y(k,259)
         mat(k,2166) = mat(k,2166) + .600_r8*rxt(k,524)*y(k,132)
         mat(k,214) = -(4._r8*rxt(k,322)*y(k,124))
         mat(k,2519) = rxt(k,323)*y(k,154)
         mat(k,1936) = rxt(k,323)*y(k,116)
         mat(k,322) = -(4._r8*rxt(k,318)*y(k,125))
         mat(k,2520) = rxt(k,319)*y(k,155)
         mat(k,2657) = rxt(k,319)*y(k,116)
         mat(k,2837) = -(rxt(k,325)*y(k,17) + (rxt(k,326) + rxt(k,327)) * y(k,21) &
                      + (rxt(k,328) + rxt(k,329) + rxt(k,330)) * y(k,74) + rxt(k,331) &
                      *y(k,107) + rxt(k,332)*y(k,154) + rxt(k,333)*y(k,155) + rxt(k,334) &
                      *y(k,156) + rxt(k,335)*y(k,164) + rxt(k,336)*y(k,166) + rxt(k,337) &
                      *y(k,259) + (4._r8*rxt(k,344) + 4._r8*rxt(k,345)) * y(k,126) &
                      + rxt(k,347)*y(k,170) + rxt(k,602)*y(k,83))
         mat(k,2577) = -rxt(k,325)*y(k,126)
         mat(k,2606) = -(rxt(k,326) + rxt(k,327)) * y(k,126)
         mat(k,2429) = -(rxt(k,328) + rxt(k,329) + rxt(k,330)) * y(k,126)
         mat(k,2144) = -rxt(k,331)*y(k,126)
         mat(k,2028) = -rxt(k,332)*y(k,126)
         mat(k,2711) = -rxt(k,333)*y(k,126)
         mat(k,2399) = -rxt(k,334)*y(k,126)
         mat(k,2653) = -rxt(k,335)*y(k,126)
         mat(k,2780) = -rxt(k,336)*y(k,126)
         mat(k,2327) = -rxt(k,337)*y(k,126)
         mat(k,739) = -rxt(k,347)*y(k,126)
         mat(k,1177) = -rxt(k,602)*y(k,126)
         mat(k,2606) = mat(k,2606) + rxt(k,315)*y(k,116)
         mat(k,1686) = rxt(k,340)*y(k,156) + rxt(k,311)*y(k,259)
         mat(k,2547) = rxt(k,315)*y(k,21) + rxt(k,321)*y(k,156) + rxt(k,324)*y(k,166)
         mat(k,1613) = rxt(k,313)*y(k,164)
         mat(k,2028) = mat(k,2028) + rxt(k,339)*y(k,170)
         mat(k,2399) = mat(k,2399) + rxt(k,340)*y(k,111) + rxt(k,321)*y(k,116)
         mat(k,2653) = mat(k,2653) + rxt(k,313)*y(k,117)
         mat(k,2780) = mat(k,2780) + rxt(k,324)*y(k,116)
         mat(k,739) = mat(k,739) + rxt(k,339)*y(k,154)
         mat(k,2327) = mat(k,2327) + rxt(k,311)*y(k,111)
         mat(k,951) = -(rxt(k,317)*y(k,116))
         mat(k,2523) = -rxt(k,317)*y(k,127)
         mat(k,1602) = rxt(k,312)*y(k,156)
         mat(k,2811) = rxt(k,333)*y(k,155)
         mat(k,2674) = rxt(k,333)*y(k,126)
         mat(k,2337) = rxt(k,312)*y(k,117)
         mat(k,964) = -(rxt(k,515)*y(k,156) + rxt(k,522)*y(k,166) + rxt(k,523) &
                      *y(k,259))
         mat(k,2338) = -rxt(k,515)*y(k,128)
         mat(k,2729) = -rxt(k,522)*y(k,128)
         mat(k,2258) = -rxt(k,523)*y(k,128)
         mat(k,661) = -(rxt(k,513)*y(k,259))
         mat(k,2234) = -rxt(k,513)*y(k,129)
         mat(k,1954) = .080_r8*rxt(k,505)*y(k,239)
         mat(k,1416) = .080_r8*rxt(k,505)*y(k,154)
         mat(k,621) = -(rxt(k,514)*y(k,259))
         mat(k,2229) = -rxt(k,514)*y(k,130)
         mat(k,1952) = .080_r8*rxt(k,511)*y(k,240)
         mat(k,1444) = .080_r8*rxt(k,511)*y(k,154)
         mat(k,486) = -(rxt(k,521)*y(k,259))
         mat(k,2213) = -rxt(k,521)*y(k,131)
         mat(k,2055) = rxt(k,518)*y(k,241)
         mat(k,1372) = rxt(k,518)*y(k,107)
         mat(k,843) = -(rxt(k,524)*y(k,259))
         mat(k,2252) = -rxt(k,524)*y(k,132)
         mat(k,2082) = rxt(k,504)*y(k,239) + rxt(k,509)*y(k,240)
         mat(k,1417) = rxt(k,504)*y(k,107)
         mat(k,1446) = rxt(k,509)*y(k,107)
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
         mat(k,85) = -(rxt(k,684)*y(k,259))
         mat(k,2156) = -rxt(k,684)*y(k,133)
         mat(k,1396) = -(rxt(k,475)*y(k,166) + rxt(k,476)*y(k,259))
         mat(k,2749) = -rxt(k,475)*y(k,135)
         mat(k,2291) = -rxt(k,476)*y(k,135)
         mat(k,969) = .300_r8*rxt(k,522)*y(k,166)
         mat(k,1995) = .360_r8*rxt(k,505)*y(k,239)
         mat(k,2365) = .400_r8*rxt(k,506)*y(k,239)
         mat(k,2749) = mat(k,2749) + .300_r8*rxt(k,522)*y(k,128)
         mat(k,1548) = .390_r8*rxt(k,502)*y(k,239)
         mat(k,1738) = .310_r8*rxt(k,503)*y(k,239)
         mat(k,1424) = .360_r8*rxt(k,505)*y(k,154) + .400_r8*rxt(k,506)*y(k,156) &
                      + .390_r8*rxt(k,502)*y(k,231) + .310_r8*rxt(k,503)*y(k,232)
         mat(k,355) = -(rxt(k,477)*y(k,259))
         mat(k,2195) = -rxt(k,477)*y(k,136)
         mat(k,2045) = rxt(k,471)*y(k,244)
         mat(k,1475) = rxt(k,471)*y(k,107)
         mat(k,600) = -(rxt(k,486)*y(k,259))
         mat(k,2226) = -rxt(k,486)*y(k,137)
         mat(k,1950) = .800_r8*rxt(k,495)*y(k,223)
         mat(k,1106) = .800_r8*rxt(k,495)*y(k,154)
         mat(k,360) = -(rxt(k,487)*y(k,259))
         mat(k,2196) = -rxt(k,487)*y(k,138)
         mat(k,2046) = .800_r8*rxt(k,484)*y(k,248)
         mat(k,776) = .800_r8*rxt(k,484)*y(k,107)
         mat(k,677) = -(rxt(k,488)*y(k,259))
         mat(k,2236) = -rxt(k,488)*y(k,139)
         mat(k,2669) = rxt(k,491)*y(k,246)
         mat(k,1520) = rxt(k,491)*y(k,155)
         mat(k,1043) = -(rxt(k,579)*y(k,156) + rxt(k,580)*y(k,166) + rxt(k,581) &
                      *y(k,259))
         mat(k,2341) = -rxt(k,579)*y(k,140)
         mat(k,2731) = -rxt(k,580)*y(k,140)
         mat(k,2265) = -rxt(k,581)*y(k,140)
         mat(k,1503) = -(rxt(k,489)*y(k,166) + rxt(k,490)*y(k,259))
         mat(k,2754) = -rxt(k,489)*y(k,141)
         mat(k,2296) = -rxt(k,490)*y(k,141)
         mat(k,972) = .200_r8*rxt(k,522)*y(k,166)
         mat(k,2000) = .560_r8*rxt(k,505)*y(k,239)
         mat(k,2370) = .600_r8*rxt(k,506)*y(k,239)
         mat(k,2754) = mat(k,2754) + .200_r8*rxt(k,522)*y(k,128)
         mat(k,1553) = .610_r8*rxt(k,502)*y(k,239)
         mat(k,1743) = .440_r8*rxt(k,503)*y(k,239)
         mat(k,1428) = .560_r8*rxt(k,505)*y(k,154) + .600_r8*rxt(k,506)*y(k,156) &
                      + .610_r8*rxt(k,502)*y(k,231) + .440_r8*rxt(k,503)*y(k,232)
         mat(k,1068) = -(rxt(k,221)*y(k,154) + (rxt(k,222) + rxt(k,223) + rxt(k,224) &
                      ) * y(k,155) + rxt(k,232)*y(k,259) + rxt(k,246)*y(k,165) &
                      + rxt(k,749)*y(k,258))
         mat(k,1976) = -rxt(k,221)*y(k,142)
         mat(k,2676) = -(rxt(k,222) + rxt(k,223) + rxt(k,224)) * y(k,142)
         mat(k,2266) = -rxt(k,232)*y(k,142)
         mat(k,1777) = -rxt(k,246)*y(k,142)
         mat(k,919) = -rxt(k,749)*y(k,142)
         mat(k,2621) = rxt(k,220)*y(k,250) + rxt(k,746)*y(k,253)
         mat(k,1777) = mat(k,1777) + rxt(k,747)*y(k,253)
         mat(k,930) = rxt(k,243)*y(k,250) + 1.100_r8*rxt(k,742)*y(k,251) &
                      + .200_r8*rxt(k,740)*y(k,252)
         mat(k,749) = rxt(k,220)*y(k,164) + rxt(k,243)*y(k,234)
         mat(k,719) = 1.100_r8*rxt(k,742)*y(k,234)
         mat(k,911) = .200_r8*rxt(k,740)*y(k,234)
         mat(k,589) = rxt(k,746)*y(k,164) + rxt(k,747)*y(k,165)
         mat(k,295) = -((rxt(k,236) + rxt(k,237)) * y(k,255))
         mat(k,1860) = -(rxt(k,236) + rxt(k,237)) * y(k,143)
         mat(k,1062) = rxt(k,222)*y(k,155)
         mat(k,2656) = rxt(k,222)*y(k,142)
         mat(k,2659) = rxt(k,239)*y(k,156)
         mat(k,2332) = rxt(k,239)*y(k,155)
         mat(k,456) = -(rxt(k,525)*y(k,259))
         mat(k,2208) = -rxt(k,525)*y(k,145)
         mat(k,1714) = .200_r8*rxt(k,517)*y(k,241)
         mat(k,1371) = .200_r8*rxt(k,517)*y(k,232)
         mat(k,1189) = -(rxt(k,526)*y(k,259))
         mat(k,2276) = -rxt(k,526)*y(k,146)
         mat(k,1982) = rxt(k,519)*y(k,241)
         mat(k,2350) = rxt(k,520)*y(k,241)
         mat(k,1542) = rxt(k,516)*y(k,241)
         mat(k,1726) = .800_r8*rxt(k,517)*y(k,241)
         mat(k,1376) = rxt(k,519)*y(k,154) + rxt(k,520)*y(k,156) + rxt(k,516)*y(k,231) &
                      + .800_r8*rxt(k,517)*y(k,232)
         mat(k,109) = -(rxt(k,637)*y(k,259))
         mat(k,2160) = -rxt(k,637)*y(k,150)
         mat(k,2014) = -(rxt(k,219)*y(k,250) + rxt(k,221)*y(k,142) + rxt(k,229) &
                      *y(k,156) + rxt(k,233)*y(k,107) + rxt(k,234)*y(k,166) + rxt(k,235) &
                      *y(k,164) + rxt(k,261)*y(k,74) + rxt(k,295)*y(k,21) + rxt(k,323) &
                      *y(k,116) + rxt(k,332)*y(k,126) + rxt(k,339)*y(k,170) + rxt(k,379) &
                      *y(k,68) + rxt(k,398)*y(k,232) + rxt(k,406)*y(k,238) + rxt(k,419) &
                      *y(k,228) + rxt(k,430)*y(k,231) + rxt(k,434)*y(k,237) + rxt(k,447) &
                      *y(k,229) + rxt(k,456)*y(k,262) + rxt(k,460)*y(k,263) + (rxt(k,466) &
                      + rxt(k,467)) * y(k,235) + (rxt(k,473) + rxt(k,474)) * y(k,244) &
                      + rxt(k,482)*y(k,246) + rxt(k,485)*y(k,248) + (rxt(k,495) &
                      + rxt(k,496)) * y(k,223) + rxt(k,505)*y(k,239) + rxt(k,511) &
                      *y(k,240) + rxt(k,519)*y(k,241) + rxt(k,530)*y(k,268) + rxt(k,534) &
                      *y(k,222) + rxt(k,537)*y(k,225) + rxt(k,542)*y(k,227) + rxt(k,544) &
                      *y(k,230) + rxt(k,548)*y(k,233) + rxt(k,551)*y(k,245) + rxt(k,554) &
                      *y(k,247) + rxt(k,557)*y(k,261) + rxt(k,564)*y(k,266) + rxt(k,570) &
                      *y(k,269) + rxt(k,573)*y(k,271) + rxt(k,584)*y(k,254) + rxt(k,589) &
                      *y(k,264) + rxt(k,594)*y(k,265) + rxt(k,751)*y(k,258))
         mat(k,752) = -rxt(k,219)*y(k,154)
         mat(k,1073) = -rxt(k,221)*y(k,154)
         mat(k,2385) = -rxt(k,229)*y(k,154)
         mat(k,2130) = -rxt(k,233)*y(k,154)
         mat(k,2766) = -rxt(k,234)*y(k,154)
         mat(k,2639) = -rxt(k,235)*y(k,154)
         mat(k,2415) = -rxt(k,261)*y(k,154)
         mat(k,2592) = -rxt(k,295)*y(k,154)
         mat(k,2533) = -rxt(k,323)*y(k,154)
         mat(k,2823) = -rxt(k,332)*y(k,154)
         mat(k,736) = -rxt(k,339)*y(k,154)
         mat(k,1235) = -rxt(k,379)*y(k,154)
         mat(k,1754) = -rxt(k,398)*y(k,154)
         mat(k,515) = -rxt(k,406)*y(k,154)
         mat(k,1009) = -rxt(k,419)*y(k,154)
         mat(k,1561) = -rxt(k,430)*y(k,154)
         mat(k,881) = -rxt(k,434)*y(k,154)
         mat(k,1029) = -rxt(k,447)*y(k,154)
         mat(k,899) = -rxt(k,456)*y(k,154)
         mat(k,1349) = -rxt(k,460)*y(k,154)
         mat(k,658) = -(rxt(k,466) + rxt(k,467)) * y(k,154)
         mat(k,1489) = -(rxt(k,473) + rxt(k,474)) * y(k,154)
         mat(k,1529) = -rxt(k,482)*y(k,154)
         mat(k,780) = -rxt(k,485)*y(k,154)
         mat(k,1118) = -(rxt(k,495) + rxt(k,496)) * y(k,154)
         mat(k,1434) = -rxt(k,505)*y(k,154)
         mat(k,1467) = -rxt(k,511)*y(k,154)
         mat(k,1388) = -rxt(k,519)*y(k,154)
         mat(k,1366) = -rxt(k,530)*y(k,154)
         mat(k,617) = -rxt(k,534)*y(k,154)
         mat(k,581) = -rxt(k,537)*y(k,154)
         mat(k,508) = -rxt(k,542)*y(k,154)
         mat(k,743) = -rxt(k,544)*y(k,154)
         mat(k,871) = -rxt(k,548)*y(k,154)
         mat(k,817) = -rxt(k,551)*y(k,154)
         mat(k,1018) = -rxt(k,554)*y(k,154)
         mat(k,527) = -rxt(k,557)*y(k,154)
         mat(k,831) = -rxt(k,564)*y(k,154)
         mat(k,863) = -rxt(k,570)*y(k,154)
         mat(k,596) = -rxt(k,573)*y(k,154)
         mat(k,1211) = -rxt(k,584)*y(k,154)
         mat(k,1296) = -rxt(k,589)*y(k,154)
         mat(k,1147) = -rxt(k,594)*y(k,154)
         mat(k,922) = -rxt(k,751)*y(k,154)
         mat(k,216) = 4.000_r8*rxt(k,322)*y(k,124)
         mat(k,1073) = mat(k,1073) + 2.000_r8*rxt(k,223)*y(k,155) + rxt(k,246) &
                      *y(k,165) + rxt(k,232)*y(k,259)
         mat(k,298) = 2.000_r8*rxt(k,236)*y(k,255)
         mat(k,2697) = 2.000_r8*rxt(k,223)*y(k,142) + rxt(k,225)*y(k,164) + rxt(k,613) &
                      *y(k,183)
         mat(k,2639) = mat(k,2639) + rxt(k,225)*y(k,155)
         mat(k,1786) = rxt(k,246)*y(k,142) + rxt(k,244)*y(k,250)
         mat(k,1633) = rxt(k,613)*y(k,155)
         mat(k,752) = mat(k,752) + rxt(k,244)*y(k,165)
         mat(k,1877) = 2.000_r8*rxt(k,236)*y(k,143)
         mat(k,2313) = rxt(k,232)*y(k,142)
         mat(k,2708) = -((rxt(k,222) + rxt(k,223) + rxt(k,224)) * y(k,142) + (rxt(k,225) &
                      + rxt(k,227)) * y(k,164) + rxt(k,226)*y(k,166) + rxt(k,238) &
                      *y(k,107) + rxt(k,239)*y(k,156) + rxt(k,240)*y(k,259) + rxt(k,253) &
                      *y(k,70) + rxt(k,263)*y(k,74) + rxt(k,288)*y(k,17) + rxt(k,298) &
                      *y(k,21) + rxt(k,319)*y(k,116) + rxt(k,333)*y(k,126) + rxt(k,441) &
                      *y(k,231) + rxt(k,491)*y(k,246) + rxt(k,549)*y(k,233) + rxt(k,552) &
                      *y(k,245) + rxt(k,555)*y(k,247) + rxt(k,559)*y(k,174) + rxt(k,562) &
                      *y(k,222) + rxt(k,613)*y(k,183))
         mat(k,1076) = -(rxt(k,222) + rxt(k,223) + rxt(k,224)) * y(k,155)
         mat(k,2650) = -(rxt(k,225) + rxt(k,227)) * y(k,155)
         mat(k,2777) = -rxt(k,226)*y(k,155)
         mat(k,2141) = -rxt(k,238)*y(k,155)
         mat(k,2396) = -rxt(k,239)*y(k,155)
         mat(k,2324) = -rxt(k,240)*y(k,155)
         mat(k,2488) = -rxt(k,253)*y(k,155)
         mat(k,2426) = -rxt(k,263)*y(k,155)
         mat(k,2574) = -rxt(k,288)*y(k,155)
         mat(k,2603) = -rxt(k,298)*y(k,155)
         mat(k,2544) = -rxt(k,319)*y(k,155)
         mat(k,2834) = -rxt(k,333)*y(k,155)
         mat(k,1566) = -rxt(k,441)*y(k,155)
         mat(k,1534) = -rxt(k,491)*y(k,155)
         mat(k,874) = -rxt(k,549)*y(k,155)
         mat(k,819) = -rxt(k,552)*y(k,155)
         mat(k,1021) = -rxt(k,555)*y(k,155)
         mat(k,550) = -rxt(k,559)*y(k,155)
         mat(k,620) = -rxt(k,562)*y(k,155)
         mat(k,1641) = -rxt(k,613)*y(k,155)
         mat(k,775) = rxt(k,493)*y(k,259)
         mat(k,422) = rxt(k,464)*y(k,156)
         mat(k,2603) = mat(k,2603) + rxt(k,295)*y(k,154)
         mat(k,1239) = rxt(k,379)*y(k,154) + rxt(k,380)*y(k,156)
         mat(k,610) = rxt(k,254)*y(k,259)
         mat(k,2426) = mat(k,2426) + rxt(k,261)*y(k,154)
         mat(k,2141) = mat(k,2141) + rxt(k,233)*y(k,154) + rxt(k,228)*y(k,156)
         mat(k,560) = rxt(k,218)*y(k,259)
         mat(k,2544) = mat(k,2544) + rxt(k,321)*y(k,156)
         mat(k,327) = 4.000_r8*rxt(k,318)*y(k,125)
         mat(k,2834) = mat(k,2834) + rxt(k,332)*y(k,154) + rxt(k,334)*y(k,156)
         mat(k,669) = .700_r8*rxt(k,513)*y(k,259)
         mat(k,2025) = rxt(k,295)*y(k,21) + rxt(k,379)*y(k,68) + rxt(k,261)*y(k,74) &
                      + rxt(k,233)*y(k,107) + rxt(k,332)*y(k,126) &
                      + 2.000_r8*rxt(k,229)*y(k,156) + rxt(k,235)*y(k,164) &
                      + rxt(k,234)*y(k,166) + rxt(k,339)*y(k,170) + rxt(k,534) &
                      *y(k,222) + rxt(k,495)*y(k,223) + rxt(k,537)*y(k,225) &
                      + rxt(k,542)*y(k,227) + rxt(k,419)*y(k,228) + rxt(k,447) &
                      *y(k,229) + rxt(k,544)*y(k,230) + rxt(k,430)*y(k,231) &
                      + rxt(k,398)*y(k,232) + rxt(k,548)*y(k,233) + rxt(k,466) &
                      *y(k,235) + rxt(k,434)*y(k,237) + rxt(k,406)*y(k,238) &
                      + .920_r8*rxt(k,505)*y(k,239) + .920_r8*rxt(k,511)*y(k,240) &
                      + rxt(k,519)*y(k,241) + rxt(k,473)*y(k,244) + rxt(k,551) &
                      *y(k,245) + rxt(k,482)*y(k,246) + rxt(k,554)*y(k,247) &
                      + rxt(k,485)*y(k,248) + 1.600_r8*rxt(k,584)*y(k,254) &
                      + rxt(k,557)*y(k,261) + rxt(k,456)*y(k,262) + rxt(k,460) &
                      *y(k,263) + .900_r8*rxt(k,589)*y(k,264) + .800_r8*rxt(k,594) &
                      *y(k,265) + rxt(k,564)*y(k,266) + rxt(k,530)*y(k,268) &
                      + rxt(k,570)*y(k,269) + rxt(k,573)*y(k,271)
         mat(k,2396) = mat(k,2396) + rxt(k,464)*y(k,16) + rxt(k,380)*y(k,68) &
                      + rxt(k,228)*y(k,107) + rxt(k,321)*y(k,116) + rxt(k,334) &
                      *y(k,126) + 2.000_r8*rxt(k,229)*y(k,154) + rxt(k,230)*y(k,164) &
                      + rxt(k,506)*y(k,239) + rxt(k,512)*y(k,240) + rxt(k,520) &
                      *y(k,241) + rxt(k,472)*y(k,244) + rxt(k,483)*y(k,246) &
                      + 2.000_r8*rxt(k,585)*y(k,254) + rxt(k,231)*y(k,259) &
                      + rxt(k,531)*y(k,268)
         mat(k,985) = rxt(k,454)*y(k,259)
         mat(k,2650) = mat(k,2650) + rxt(k,235)*y(k,154) + rxt(k,230)*y(k,156)
         mat(k,2777) = mat(k,2777) + rxt(k,234)*y(k,154)
         mat(k,738) = rxt(k,339)*y(k,154)
         mat(k,731) = rxt(k,591)*y(k,259)
         mat(k,620) = mat(k,620) + rxt(k,534)*y(k,154)
         mat(k,1121) = rxt(k,495)*y(k,154)
         mat(k,584) = rxt(k,537)*y(k,154)
         mat(k,511) = rxt(k,542)*y(k,154)
         mat(k,1012) = rxt(k,419)*y(k,154)
         mat(k,1032) = rxt(k,447)*y(k,154)
         mat(k,746) = rxt(k,544)*y(k,154)
         mat(k,1566) = mat(k,1566) + rxt(k,430)*y(k,154)
         mat(k,1762) = rxt(k,398)*y(k,154) + .500_r8*rxt(k,582)*y(k,254)
         mat(k,874) = mat(k,874) + rxt(k,548)*y(k,154)
         mat(k,660) = rxt(k,466)*y(k,154)
         mat(k,884) = rxt(k,434)*y(k,154)
         mat(k,517) = rxt(k,406)*y(k,154)
         mat(k,1439) = .920_r8*rxt(k,505)*y(k,154) + rxt(k,506)*y(k,156)
         mat(k,1472) = .920_r8*rxt(k,511)*y(k,154) + rxt(k,512)*y(k,156)
         mat(k,1393) = rxt(k,519)*y(k,154) + rxt(k,520)*y(k,156)
         mat(k,1494) = rxt(k,473)*y(k,154) + rxt(k,472)*y(k,156)
         mat(k,819) = mat(k,819) + rxt(k,551)*y(k,154)
         mat(k,1534) = mat(k,1534) + rxt(k,482)*y(k,154) + rxt(k,483)*y(k,156)
         mat(k,1021) = mat(k,1021) + rxt(k,554)*y(k,154)
         mat(k,783) = rxt(k,485)*y(k,154)
         mat(k,1216) = 1.600_r8*rxt(k,584)*y(k,154) + 2.000_r8*rxt(k,585)*y(k,156) &
                      + .500_r8*rxt(k,582)*y(k,232)
         mat(k,2324) = mat(k,2324) + rxt(k,493)*y(k,1) + rxt(k,254)*y(k,73) &
                      + rxt(k,218)*y(k,108) + .700_r8*rxt(k,513)*y(k,129) + rxt(k,231) &
                      *y(k,156) + rxt(k,454)*y(k,157) + rxt(k,591)*y(k,209)
         mat(k,530) = rxt(k,557)*y(k,154)
         mat(k,902) = rxt(k,456)*y(k,154)
         mat(k,1352) = rxt(k,460)*y(k,154)
         mat(k,1300) = .900_r8*rxt(k,589)*y(k,154)
         mat(k,1151) = .800_r8*rxt(k,594)*y(k,154)
         mat(k,834) = rxt(k,564)*y(k,154)
         mat(k,1370) = rxt(k,530)*y(k,154) + rxt(k,531)*y(k,156)
         mat(k,866) = rxt(k,570)*y(k,154)
         mat(k,599) = rxt(k,573)*y(k,154)
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
         mat(k,2388) = -(rxt(k,228)*y(k,107) + rxt(k,229)*y(k,154) + rxt(k,230) &
                      *y(k,164) + rxt(k,231)*y(k,259) + rxt(k,239)*y(k,155) + rxt(k,310) &
                      *y(k,105) + rxt(k,312)*y(k,117) + rxt(k,321)*y(k,116) + rxt(k,334) &
                      *y(k,126) + rxt(k,340)*y(k,111) + rxt(k,380)*y(k,68) + rxt(k,392) &
                      *y(k,51) + rxt(k,424)*y(k,54) + rxt(k,443)*y(k,33) + rxt(k,450) &
                      *y(k,58) + rxt(k,464)*y(k,16) + rxt(k,472)*y(k,244) + rxt(k,483) &
                      *y(k,246) + rxt(k,506)*y(k,239) + rxt(k,512)*y(k,240) + rxt(k,515) &
                      *y(k,128) + rxt(k,520)*y(k,241) + rxt(k,531)*y(k,268) + rxt(k,576) &
                      *y(k,6) + rxt(k,579)*y(k,140) + rxt(k,585)*y(k,254) + rxt(k,596) &
                      *y(k,211) + rxt(k,603)*y(k,83))
         mat(k,2133) = -rxt(k,228)*y(k,156)
         mat(k,2017) = -rxt(k,229)*y(k,156)
         mat(k,2642) = -rxt(k,230)*y(k,156)
         mat(k,2316) = -rxt(k,231)*y(k,156)
         mat(k,2700) = -rxt(k,239)*y(k,156)
         mat(k,1334) = -rxt(k,310)*y(k,156)
         mat(k,1606) = -rxt(k,312)*y(k,156)
         mat(k,2536) = -rxt(k,321)*y(k,156)
         mat(k,2826) = -rxt(k,334)*y(k,156)
         mat(k,1677) = -rxt(k,340)*y(k,156)
         mat(k,1237) = -rxt(k,380)*y(k,156)
         mat(k,1911) = -rxt(k,392)*y(k,156)
         mat(k,1269) = -rxt(k,424)*y(k,156)
         mat(k,1259) = -rxt(k,443)*y(k,156)
         mat(k,1413) = -rxt(k,450)*y(k,156)
         mat(k,421) = -rxt(k,464)*y(k,156)
         mat(k,1492) = -rxt(k,472)*y(k,156)
         mat(k,1532) = -rxt(k,483)*y(k,156)
         mat(k,1437) = -rxt(k,506)*y(k,156)
         mat(k,1470) = -rxt(k,512)*y(k,156)
         mat(k,978) = -rxt(k,515)*y(k,156)
         mat(k,1391) = -rxt(k,520)*y(k,156)
         mat(k,1369) = -rxt(k,531)*y(k,156)
         mat(k,1103) = -rxt(k,576)*y(k,156)
         mat(k,1059) = -rxt(k,579)*y(k,156)
         mat(k,1214) = -rxt(k,585)*y(k,156)
         mat(k,1159) = -rxt(k,596)*y(k,156)
         mat(k,1171) = -rxt(k,603)*y(k,156)
         mat(k,2566) = rxt(k,296)*y(k,22)
         mat(k,942) = rxt(k,296)*y(k,17) + rxt(k,297)*y(k,70) + rxt(k,299)*y(k,164)
         mat(k,2480) = rxt(k,297)*y(k,22) + rxt(k,262)*y(k,75)
         mat(k,1133) = rxt(k,262)*y(k,70) + rxt(k,264)*y(k,164) + rxt(k,265)*y(k,259)
         mat(k,992) = rxt(k,352)*y(k,106)
         mat(k,2506) = rxt(k,352)*y(k,89) + rxt(k,241)*y(k,259)
         mat(k,2536) = mat(k,2536) + rxt(k,317)*y(k,127)
         mat(k,954) = rxt(k,317)*y(k,116)
         mat(k,684) = .500_r8*rxt(k,488)*y(k,259)
         mat(k,2700) = mat(k,2700) + rxt(k,227)*y(k,164) + rxt(k,226)*y(k,166)
         mat(k,2642) = mat(k,2642) + rxt(k,299)*y(k,22) + rxt(k,264)*y(k,75) &
                      + rxt(k,227)*y(k,155)
         mat(k,2769) = rxt(k,226)*y(k,155)
         mat(k,635) = rxt(k,439)*y(k,259)
         mat(k,2316) = mat(k,2316) + rxt(k,265)*y(k,75) + rxt(k,241)*y(k,106) &
                      + .500_r8*rxt(k,488)*y(k,139) + rxt(k,439)*y(k,172)
         mat(k,980) = -(rxt(k,454)*y(k,259))
         mat(k,2259) = -rxt(k,454)*y(k,157)
         mat(k,1243) = rxt(k,443)*y(k,156)
         mat(k,622) = .500_r8*rxt(k,514)*y(k,259)
         mat(k,488) = rxt(k,521)*y(k,259)
         mat(k,457) = rxt(k,525)*y(k,259)
         mat(k,1186) = rxt(k,526)*y(k,259)
         mat(k,2339) = rxt(k,443)*y(k,33)
         mat(k,2259) = mat(k,2259) + .500_r8*rxt(k,514)*y(k,130) + rxt(k,521)*y(k,131) &
                      + rxt(k,525)*y(k,145) + rxt(k,526)*y(k,146)
         mat(k,462) = -(rxt(k,586)*y(k,259))
         mat(k,2209) = -rxt(k,586)*y(k,158)
         mat(k,2051) = rxt(k,583)*y(k,254)
         mat(k,1201) = rxt(k,583)*y(k,107)
         mat(k,2649) = -(rxt(k,197)*y(k,166) + 4._r8*rxt(k,198)*y(k,164) + rxt(k,199) &
                      *y(k,165) + rxt(k,200)*y(k,93) + rxt(k,201)*y(k,95) + rxt(k,206) &
                      *y(k,107) + rxt(k,212)*y(k,259) + (rxt(k,225) + rxt(k,227) &
                      ) * y(k,155) + rxt(k,230)*y(k,156) + rxt(k,235)*y(k,154) &
                      + rxt(k,264)*y(k,75) + rxt(k,266)*y(k,74) + rxt(k,269)*y(k,101) &
                      + rxt(k,272)*y(k,110) + rxt(k,299)*y(k,22) + rxt(k,300)*y(k,21) &
                      + rxt(k,302)*y(k,97) + rxt(k,304)*y(k,109) + rxt(k,313)*y(k,117) &
                      + rxt(k,335)*y(k,126) + rxt(k,393)*y(k,51) + rxt(k,605)*y(k,169) &
                      + (rxt(k,744) + rxt(k,745)) * y(k,251) + rxt(k,746)*y(k,253))
         mat(k,2776) = -rxt(k,197)*y(k,164)
         mat(k,1793) = -rxt(k,199)*y(k,164)
         mat(k,1625) = -rxt(k,200)*y(k,164)
         mat(k,702) = -rxt(k,201)*y(k,164)
         mat(k,2140) = -rxt(k,206)*y(k,164)
         mat(k,2323) = -rxt(k,212)*y(k,164)
         mat(k,2707) = -(rxt(k,225) + rxt(k,227)) * y(k,164)
         mat(k,2395) = -rxt(k,230)*y(k,164)
         mat(k,2024) = -rxt(k,235)*y(k,164)
         mat(k,1137) = -rxt(k,264)*y(k,164)
         mat(k,2425) = -rxt(k,266)*y(k,164)
         mat(k,1840) = -rxt(k,269)*y(k,164)
         mat(k,1817) = -rxt(k,272)*y(k,164)
         mat(k,947) = -rxt(k,299)*y(k,164)
         mat(k,2602) = -rxt(k,300)*y(k,164)
         mat(k,1599) = -rxt(k,302)*y(k,164)
         mat(k,1706) = -rxt(k,304)*y(k,164)
         mat(k,1611) = -rxt(k,313)*y(k,164)
         mat(k,2833) = -rxt(k,335)*y(k,164)
         mat(k,1918) = -rxt(k,393)*y(k,164)
         mat(k,432) = -rxt(k,605)*y(k,164)
         mat(k,721) = -(rxt(k,744) + rxt(k,745)) * y(k,164)
         mat(k,591) = -rxt(k,746)*y(k,164)
         mat(k,2800) = rxt(k,204)*y(k,107)
         mat(k,2140) = mat(k,2140) + rxt(k,204)*y(k,92)
         mat(k,1075) = rxt(k,221)*y(k,154) + rxt(k,222)*y(k,155) + rxt(k,246)*y(k,165) &
                      + rxt(k,749)*y(k,258)
         mat(k,2024) = mat(k,2024) + rxt(k,221)*y(k,142) + rxt(k,219)*y(k,250)
         mat(k,2707) = mat(k,2707) + rxt(k,222)*y(k,142)
         mat(k,1793) = mat(k,1793) + rxt(k,246)*y(k,142) + rxt(k,607)*y(k,181) &
                      + rxt(k,614)*y(k,183) + rxt(k,748)*y(k,253) + (rxt(k,185) &
                       +rxt(k,186))*y(k,255) + rxt(k,754)*y(k,260)
         mat(k,2776) = mat(k,2776) + 2.000_r8*rxt(k,188)*y(k,255)
         mat(k,839) = rxt(k,607)*y(k,165)
         mat(k,1640) = rxt(k,614)*y(k,165)
         mat(k,936) = rxt(k,740)*y(k,252) + 1.150_r8*rxt(k,741)*y(k,258)
         mat(k,753) = rxt(k,219)*y(k,154)
         mat(k,915) = rxt(k,740)*y(k,234)
         mat(k,591) = mat(k,591) + rxt(k,748)*y(k,165)
         mat(k,1887) = (rxt(k,185)+rxt(k,186))*y(k,165) + 2.000_r8*rxt(k,188)*y(k,166)
         mat(k,923) = rxt(k,749)*y(k,142) + 1.150_r8*rxt(k,741)*y(k,234)
         mat(k,2323) = mat(k,2323) + 2.000_r8*rxt(k,214)*y(k,259)
         mat(k,892) = rxt(k,754)*y(k,165)
         mat(k,1783) = -(rxt(k,185)*y(k,255) + rxt(k,191)*y(k,256) + rxt(k,199) &
                      *y(k,164) + rxt(k,205)*y(k,92) + rxt(k,244)*y(k,250) + rxt(k,246) &
                      *y(k,142) + rxt(k,436)*y(k,236) + rxt(k,607)*y(k,181) + rxt(k,614) &
                      *y(k,183) + rxt(k,743)*y(k,251) + (rxt(k,747) + rxt(k,748) &
                      ) * y(k,253) + rxt(k,754)*y(k,260))
         mat(k,1872) = -rxt(k,185)*y(k,165)
         mat(k,206) = -rxt(k,191)*y(k,165)
         mat(k,2634) = -rxt(k,199)*y(k,165)
         mat(k,2785) = -rxt(k,205)*y(k,165)
         mat(k,750) = -rxt(k,244)*y(k,165)
         mat(k,1071) = -rxt(k,246)*y(k,165)
         mat(k,541) = -rxt(k,436)*y(k,165)
         mat(k,837) = -rxt(k,607)*y(k,165)
         mat(k,1632) = -rxt(k,614)*y(k,165)
         mat(k,720) = -rxt(k,743)*y(k,165)
         mat(k,590) = -(rxt(k,747) + rxt(k,748)) * y(k,165)
         mat(k,891) = -rxt(k,754)*y(k,165)
         mat(k,2558) = rxt(k,287)*y(k,107) + rxt(k,289)*y(k,166)
         mat(k,2587) = 2.000_r8*rxt(k,290)*y(k,21) + (rxt(k,292)+rxt(k,293))*y(k,74) &
                      + rxt(k,294)*y(k,107) + rxt(k,326)*y(k,126) + rxt(k,300) &
                      *y(k,164)
         mat(k,1232) = rxt(k,377)*y(k,107)
         mat(k,2472) = rxt(k,251)*y(k,107) + rxt(k,255)*y(k,166)
         mat(k,2410) = (rxt(k,292)+rxt(k,293))*y(k,21) + (2.000_r8*rxt(k,257) &
                       +2.000_r8*rxt(k,258))*y(k,74) + rxt(k,260)*y(k,107) + ( &
                      + rxt(k,329)+rxt(k,330))*y(k,126) + rxt(k,266)*y(k,164) &
                      + rxt(k,268)*y(k,259)
         mat(k,2785) = mat(k,2785) + rxt(k,202)*y(k,107) + rxt(k,208)*y(k,166)
         mat(k,2125) = rxt(k,287)*y(k,17) + rxt(k,294)*y(k,21) + rxt(k,377)*y(k,68) &
                      + rxt(k,251)*y(k,70) + rxt(k,260)*y(k,74) + rxt(k,202)*y(k,92) &
                      + 2.000_r8*rxt(k,216)*y(k,107) + rxt(k,316)*y(k,116) &
                      + rxt(k,331)*y(k,126) + rxt(k,228)*y(k,156) + rxt(k,206) &
                      *y(k,164) + 2.000_r8*rxt(k,207)*y(k,166) + rxt(k,418)*y(k,228) &
                      + rxt(k,446)*y(k,229) + rxt(k,397)*y(k,232) + rxt(k,211) &
                      *y(k,259) + rxt(k,455)*y(k,262)
         mat(k,556) = rxt(k,218)*y(k,259)
         mat(k,2528) = rxt(k,316)*y(k,107) + rxt(k,324)*y(k,166)
         mat(k,2818) = rxt(k,326)*y(k,21) + (rxt(k,329)+rxt(k,330))*y(k,74) &
                      + rxt(k,331)*y(k,107) + rxt(k,335)*y(k,164) + rxt(k,336) &
                      *y(k,166)
         mat(k,1071) = mat(k,1071) + rxt(k,224)*y(k,155)
         mat(k,296) = rxt(k,237)*y(k,255)
         mat(k,2009) = rxt(k,234)*y(k,166) + rxt(k,751)*y(k,258)
         mat(k,2692) = rxt(k,224)*y(k,142) + rxt(k,225)*y(k,164) + rxt(k,226)*y(k,166)
         mat(k,2380) = rxt(k,228)*y(k,107) + rxt(k,230)*y(k,164)
         mat(k,2634) = mat(k,2634) + rxt(k,300)*y(k,21) + rxt(k,266)*y(k,74) &
                      + rxt(k,206)*y(k,107) + rxt(k,335)*y(k,126) + rxt(k,225) &
                      *y(k,155) + rxt(k,230)*y(k,156) + 2.000_r8*rxt(k,198)*y(k,164) &
                      + 2.000_r8*rxt(k,197)*y(k,166) + rxt(k,190)*y(k,256) &
                      + rxt(k,212)*y(k,259)
         mat(k,1783) = mat(k,1783) + 2.000_r8*rxt(k,191)*y(k,256)
         mat(k,2761) = rxt(k,289)*y(k,17) + rxt(k,255)*y(k,70) + rxt(k,208)*y(k,92) &
                      + 2.000_r8*rxt(k,207)*y(k,107) + rxt(k,324)*y(k,116) &
                      + rxt(k,336)*y(k,126) + rxt(k,234)*y(k,154) + rxt(k,226) &
                      *y(k,155) + 2.000_r8*rxt(k,197)*y(k,164) + rxt(k,609)*y(k,181) &
                      + rxt(k,615)*y(k,183) + (2.000_r8*rxt(k,187)+rxt(k,188)) &
                      *y(k,255) + rxt(k,213)*y(k,259)
         mat(k,837) = mat(k,837) + rxt(k,609)*y(k,166)
         mat(k,1632) = mat(k,1632) + rxt(k,615)*y(k,166)
         mat(k,1007) = rxt(k,418)*y(k,107)
         mat(k,1027) = rxt(k,446)*y(k,107)
         mat(k,1749) = rxt(k,397)*y(k,107)
         mat(k,1872) = mat(k,1872) + rxt(k,237)*y(k,143) + (2.000_r8*rxt(k,187) &
                       +rxt(k,188))*y(k,166)
         mat(k,206) = mat(k,206) + rxt(k,190)*y(k,164) + 2.000_r8*rxt(k,191)*y(k,165)
         mat(k,920) = rxt(k,751)*y(k,154)
         mat(k,2308) = rxt(k,268)*y(k,74) + rxt(k,211)*y(k,107) + rxt(k,218)*y(k,108) &
                      + rxt(k,212)*y(k,164) + rxt(k,213)*y(k,166)
         mat(k,897) = rxt(k,455)*y(k,107)
         mat(k,2778) = -((rxt(k,187) + rxt(k,188)) * y(k,255) + rxt(k,197)*y(k,164) &
                      + rxt(k,207)*y(k,107) + rxt(k,208)*y(k,92) + rxt(k,213)*y(k,259) &
                      + rxt(k,226)*y(k,155) + rxt(k,234)*y(k,154) + rxt(k,255)*y(k,70) &
                      + rxt(k,289)*y(k,17) + rxt(k,324)*y(k,116) + rxt(k,336)*y(k,126) &
                      + rxt(k,415)*y(k,28) + rxt(k,444)*y(k,33) + rxt(k,475)*y(k,135) &
                      + rxt(k,489)*y(k,141) + rxt(k,522)*y(k,128) + rxt(k,560) &
                      *y(k,174) + rxt(k,577)*y(k,6) + rxt(k,580)*y(k,140) + rxt(k,609) &
                      *y(k,181) + rxt(k,615)*y(k,183))
         mat(k,1889) = -(rxt(k,187) + rxt(k,188)) * y(k,166)
         mat(k,2651) = -rxt(k,197)*y(k,166)
         mat(k,2142) = -rxt(k,207)*y(k,166)
         mat(k,2802) = -rxt(k,208)*y(k,166)
         mat(k,2325) = -rxt(k,213)*y(k,166)
         mat(k,2709) = -rxt(k,226)*y(k,166)
         mat(k,2026) = -rxt(k,234)*y(k,166)
         mat(k,2489) = -rxt(k,255)*y(k,166)
         mat(k,2575) = -rxt(k,289)*y(k,166)
         mat(k,2545) = -rxt(k,324)*y(k,166)
         mat(k,2835) = -rxt(k,336)*y(k,166)
         mat(k,652) = -rxt(k,415)*y(k,166)
         mat(k,1261) = -rxt(k,444)*y(k,166)
         mat(k,1405) = -rxt(k,475)*y(k,166)
         mat(k,1517) = -rxt(k,489)*y(k,166)
         mat(k,979) = -rxt(k,522)*y(k,166)
         mat(k,551) = -rxt(k,560)*y(k,166)
         mat(k,1104) = -rxt(k,577)*y(k,166)
         mat(k,1060) = -rxt(k,580)*y(k,166)
         mat(k,840) = -rxt(k,609)*y(k,166)
         mat(k,1642) = -rxt(k,615)*y(k,166)
         mat(k,2142) = mat(k,2142) + .150_r8*rxt(k,429)*y(k,231) + .150_r8*rxt(k,480) &
                      *y(k,246)
         mat(k,2651) = mat(k,2651) + rxt(k,199)*y(k,165)
         mat(k,1795) = rxt(k,199)*y(k,164)
         mat(k,1567) = .150_r8*rxt(k,429)*y(k,107)
         mat(k,1535) = .150_r8*rxt(k,480)*y(k,107)
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
         mat(k,534) = -(rxt(k,616)*y(k,183))
         mat(k,1628) = -rxt(k,616)*y(k,168)
         mat(k,2579) = rxt(k,291)*y(k,74)
         mat(k,2403) = rxt(k,291)*y(k,21) + 2.000_r8*rxt(k,259)*y(k,74) + rxt(k,328) &
                      *y(k,126)
         mat(k,2808) = rxt(k,328)*y(k,74)
         mat(k,426) = -(rxt(k,605)*y(k,164) + rxt(k,606)*y(k,259))
         mat(k,2610) = -rxt(k,605)*y(k,169)
         mat(k,2204) = -rxt(k,606)*y(k,169)
         mat(k,734) = -(rxt(k,339)*y(k,154) + rxt(k,347)*y(k,126) + 4._r8*rxt(k,348) &
                      *y(k,170))
         mat(k,1957) = -rxt(k,339)*y(k,170)
         mat(k,2810) = -rxt(k,347)*y(k,170)
         mat(k,2581) = rxt(k,327)*y(k,126)
         mat(k,2810) = mat(k,2810) + rxt(k,327)*y(k,21) + 2.000_r8*rxt(k,344)*y(k,126) &
                      + rxt(k,334)*y(k,156) + rxt(k,336)*y(k,166)
         mat(k,2336) = rxt(k,334)*y(k,126)
         mat(k,2724) = rxt(k,336)*y(k,126)
         mat(k,1302) = rxt(k,468)*y(k,259)
         mat(k,1938) = .100_r8*rxt(k,589)*y(k,264)
         mat(k,2185) = rxt(k,468)*y(k,112)
         mat(k,1283) = .100_r8*rxt(k,589)*y(k,154)
         mat(k,629) = -(rxt(k,439)*y(k,259))
         mat(k,2230) = -rxt(k,439)*y(k,172)
         mat(k,2668) = rxt(k,441)*y(k,231)
         mat(k,1538) = rxt(k,441)*y(k,155)
         mat(k,2655) = rxt(k,562)*y(k,222)
         mat(k,614) = rxt(k,562)*y(k,155)
         mat(k,548) = -(rxt(k,559)*y(k,155) + rxt(k,560)*y(k,166))
         mat(k,2662) = -rxt(k,559)*y(k,174)
         mat(k,2722) = -rxt(k,560)*y(k,174)
         mat(k,232) = .070_r8*rxt(k,546)*y(k,259)
         mat(k,1947) = rxt(k,544)*y(k,230)
         mat(k,202) = .060_r8*rxt(k,558)*y(k,259)
         mat(k,255) = .070_r8*rxt(k,574)*y(k,259)
         mat(k,741) = rxt(k,544)*y(k,154)
         mat(k,2220) = .070_r8*rxt(k,546)*y(k,82) + .060_r8*rxt(k,558)*y(k,175) &
                      + .070_r8*rxt(k,574)*y(k,218)
         mat(k,200) = -(rxt(k,558)*y(k,259))
         mat(k,2169) = -rxt(k,558)*y(k,175)
         mat(k,192) = .530_r8*rxt(k,535)*y(k,259)
         mat(k,2169) = mat(k,2169) + .530_r8*rxt(k,535)*y(k,7)
         mat(k,383) = -(rxt(k,561)*y(k,259))
         mat(k,2198) = -rxt(k,561)*y(k,176)
         mat(k,2048) = rxt(k,556)*y(k,261)
         mat(k,524) = rxt(k,556)*y(k,107)
         mat(k,637) = -(rxt(k,457)*y(k,259))
         mat(k,2231) = -rxt(k,457)*y(k,179)
         mat(k,2069) = rxt(k,455)*y(k,262)
         mat(k,893) = rxt(k,455)*y(k,107)
         mat(k,474) = -(rxt(k,461)*y(k,259))
         mat(k,2211) = -rxt(k,461)*y(k,180)
         mat(k,2053) = .850_r8*rxt(k,459)*y(k,263)
         mat(k,1341) = .850_r8*rxt(k,459)*y(k,107)
         mat(k,835) = -(rxt(k,607)*y(k,165) + rxt(k,609)*y(k,166) + rxt(k,612) &
                      *y(k,259))
         mat(k,1772) = -rxt(k,607)*y(k,181)
         mat(k,2727) = -rxt(k,609)*y(k,181)
         mat(k,2251) = -rxt(k,612)*y(k,181)
         mat(k,1631) = -(rxt(k,610)*y(k,21) + rxt(k,611)*y(k,74) + rxt(k,613)*y(k,155) &
                      + rxt(k,614)*y(k,165) + rxt(k,615)*y(k,166) + rxt(k,616) &
                      *y(k,168) + rxt(k,617)*y(k,259))
         mat(k,2585) = -rxt(k,610)*y(k,183)
         mat(k,2408) = -rxt(k,611)*y(k,183)
         mat(k,2687) = -rxt(k,613)*y(k,183)
         mat(k,1782) = -rxt(k,614)*y(k,183)
         mat(k,2758) = -rxt(k,615)*y(k,183)
         mat(k,536) = -rxt(k,616)*y(k,183)
         mat(k,2303) = -rxt(k,617)*y(k,183)
         mat(k,2629) = rxt(k,605)*y(k,169)
         mat(k,1782) = mat(k,1782) + rxt(k,607)*y(k,181)
         mat(k,2758) = mat(k,2758) + rxt(k,609)*y(k,181)
         mat(k,430) = rxt(k,605)*y(k,164)
         mat(k,836) = rxt(k,607)*y(k,165) + rxt(k,609)*y(k,166) + rxt(k,612)*y(k,259)
         mat(k,2303) = mat(k,2303) + rxt(k,612)*y(k,181)
         mat(k,1179) = -(rxt(k,608)*y(k,259))
         mat(k,2275) = -rxt(k,608)*y(k,184)
         mat(k,2584) = rxt(k,599)*y(k,83) + rxt(k,610)*y(k,183)
         mat(k,2458) = rxt(k,601)*y(k,83)
         mat(k,2407) = rxt(k,611)*y(k,183)
         mat(k,1167) = rxt(k,599)*y(k,21) + rxt(k,601)*y(k,70) + rxt(k,602)*y(k,126) &
                      + rxt(k,603)*y(k,156) + (rxt(k,604)+.500_r8*rxt(k,618))*y(k,259)
         mat(k,2813) = rxt(k,602)*y(k,83)
         mat(k,2679) = rxt(k,613)*y(k,183)
         mat(k,2349) = rxt(k,603)*y(k,83)
         mat(k,1778) = rxt(k,614)*y(k,183)
         mat(k,2738) = rxt(k,615)*y(k,183)
         mat(k,535) = rxt(k,616)*y(k,183)
         mat(k,428) = rxt(k,606)*y(k,259)
         mat(k,1630) = rxt(k,610)*y(k,21) + rxt(k,611)*y(k,74) + rxt(k,613)*y(k,155) &
                      + rxt(k,614)*y(k,165) + rxt(k,615)*y(k,166) + rxt(k,616) &
                      *y(k,168) + rxt(k,617)*y(k,259)
         mat(k,2275) = mat(k,2275) + (rxt(k,604)+.500_r8*rxt(k,618))*y(k,83) &
                      + rxt(k,606)*y(k,169) + rxt(k,617)*y(k,183)
         mat(k,300) = -(rxt(k,619)*y(k,272))
         mat(k,2840) = -rxt(k,619)*y(k,185)
         mat(k,1178) = rxt(k,608)*y(k,259)
         mat(k,2187) = rxt(k,608)*y(k,184)
         mat(k,1078) = .2202005_r8*rxt(k,672)*y(k,166)
         mat(k,2030) = .2202005_r8*rxt(k,670)*y(k,224) + .0023005_r8*rxt(k,675) &
                      *y(k,226) + .0031005_r8*rxt(k,678)*y(k,242) &
                      + .2381005_r8*rxt(k,682)*y(k,243) + .0508005_r8*rxt(k,686) &
                      *y(k,249) + .1364005_r8*rxt(k,692)*y(k,267) &
                      + .1677005_r8*rxt(k,696)*y(k,270)
         mat(k,1034) = .0508005_r8*rxt(k,688)*y(k,166)
         mat(k,1924) = .1279005_r8*rxt(k,671)*y(k,224) + .0097005_r8*rxt(k,676) &
                      *y(k,226) + .0003005_r8*rxt(k,679)*y(k,242) &
                      + .1056005_r8*rxt(k,683)*y(k,243) + .0245005_r8*rxt(k,687) &
                      *y(k,249) + .0154005_r8*rxt(k,693)*y(k,267) &
                      + .0063005_r8*rxt(k,697)*y(k,270)
         mat(k,2713) = .2202005_r8*rxt(k,672)*y(k,6) + .0508005_r8*rxt(k,688)*y(k,140)
         mat(k,54) = .5931005_r8*rxt(k,690)*y(k,259)
         mat(k,60) = .2202005_r8*rxt(k,670)*y(k,107) + .1279005_r8*rxt(k,671)*y(k,154)
         mat(k,66) = .0023005_r8*rxt(k,675)*y(k,107) + .0097005_r8*rxt(k,676)*y(k,154)
         mat(k,72) = .0031005_r8*rxt(k,678)*y(k,107) + .0003005_r8*rxt(k,679)*y(k,154)
         mat(k,78) = .2381005_r8*rxt(k,682)*y(k,107) + .1056005_r8*rxt(k,683)*y(k,154)
         mat(k,86) = .0508005_r8*rxt(k,686)*y(k,107) + .0245005_r8*rxt(k,687)*y(k,154)
         mat(k,2146) = .5931005_r8*rxt(k,690)*y(k,206)
         mat(k,92) = .1364005_r8*rxt(k,692)*y(k,107) + .0154005_r8*rxt(k,693)*y(k,154)
         mat(k,98) = .1677005_r8*rxt(k,696)*y(k,107) + .0063005_r8*rxt(k,697)*y(k,154)
         mat(k,1079) = .2067005_r8*rxt(k,672)*y(k,166)
         mat(k,2031) = .2067005_r8*rxt(k,670)*y(k,224) + .0008005_r8*rxt(k,675) &
                      *y(k,226) + .0035005_r8*rxt(k,678)*y(k,242) &
                      + .1308005_r8*rxt(k,682)*y(k,243) + .1149005_r8*rxt(k,686) &
                      *y(k,249) + .0101005_r8*rxt(k,692)*y(k,267) &
                      + .0174005_r8*rxt(k,696)*y(k,270)
         mat(k,1035) = .1149005_r8*rxt(k,688)*y(k,166)
         mat(k,1925) = .1792005_r8*rxt(k,671)*y(k,224) + .0034005_r8*rxt(k,676) &
                      *y(k,226) + .0003005_r8*rxt(k,679)*y(k,242) &
                      + .1026005_r8*rxt(k,683)*y(k,243) + .0082005_r8*rxt(k,687) &
                      *y(k,249) + .0452005_r8*rxt(k,693)*y(k,267) &
                      + .0237005_r8*rxt(k,697)*y(k,270)
         mat(k,2714) = .2067005_r8*rxt(k,672)*y(k,6) + .1149005_r8*rxt(k,688)*y(k,140)
         mat(k,55) = .1534005_r8*rxt(k,690)*y(k,259)
         mat(k,61) = .2067005_r8*rxt(k,670)*y(k,107) + .1792005_r8*rxt(k,671)*y(k,154)
         mat(k,67) = .0008005_r8*rxt(k,675)*y(k,107) + .0034005_r8*rxt(k,676)*y(k,154)
         mat(k,73) = .0035005_r8*rxt(k,678)*y(k,107) + .0003005_r8*rxt(k,679)*y(k,154)
         mat(k,79) = .1308005_r8*rxt(k,682)*y(k,107) + .1026005_r8*rxt(k,683)*y(k,154)
         mat(k,87) = .1149005_r8*rxt(k,686)*y(k,107) + .0082005_r8*rxt(k,687)*y(k,154)
         mat(k,2147) = .1534005_r8*rxt(k,690)*y(k,206)
         mat(k,93) = .0101005_r8*rxt(k,692)*y(k,107) + .0452005_r8*rxt(k,693)*y(k,154)
         mat(k,99) = .0174005_r8*rxt(k,696)*y(k,107) + .0237005_r8*rxt(k,697)*y(k,154)
         mat(k,1080) = .0653005_r8*rxt(k,672)*y(k,166)
         mat(k,2032) = .0653005_r8*rxt(k,670)*y(k,224) + .0843005_r8*rxt(k,675) &
                      *y(k,226) + .0003005_r8*rxt(k,678)*y(k,242) &
                      + .0348005_r8*rxt(k,682)*y(k,243) + .0348005_r8*rxt(k,686) &
                      *y(k,249) + .0763005_r8*rxt(k,692)*y(k,267) + .086_r8*rxt(k,696) &
                      *y(k,270)
         mat(k,1036) = .0348005_r8*rxt(k,688)*y(k,166)
         mat(k,1926) = .0676005_r8*rxt(k,671)*y(k,224) + .1579005_r8*rxt(k,676) &
                      *y(k,226) + .0073005_r8*rxt(k,679)*y(k,242) &
                      + .0521005_r8*rxt(k,683)*y(k,243) + .0772005_r8*rxt(k,687) &
                      *y(k,249) + .0966005_r8*rxt(k,693)*y(k,267) &
                      + .0025005_r8*rxt(k,697)*y(k,270)
         mat(k,2715) = .0653005_r8*rxt(k,672)*y(k,6) + .0348005_r8*rxt(k,688)*y(k,140)
         mat(k,56) = .0459005_r8*rxt(k,690)*y(k,259)
         mat(k,62) = .0653005_r8*rxt(k,670)*y(k,107) + .0676005_r8*rxt(k,671)*y(k,154)
         mat(k,68) = .0843005_r8*rxt(k,675)*y(k,107) + .1579005_r8*rxt(k,676)*y(k,154)
         mat(k,74) = .0003005_r8*rxt(k,678)*y(k,107) + .0073005_r8*rxt(k,679)*y(k,154)
         mat(k,80) = .0348005_r8*rxt(k,682)*y(k,107) + .0521005_r8*rxt(k,683)*y(k,154)
         mat(k,88) = .0348005_r8*rxt(k,686)*y(k,107) + .0772005_r8*rxt(k,687)*y(k,154)
         mat(k,2148) = .0459005_r8*rxt(k,690)*y(k,206)
         mat(k,94) = .0763005_r8*rxt(k,692)*y(k,107) + .0966005_r8*rxt(k,693)*y(k,154)
         mat(k,100) = .086_r8*rxt(k,696)*y(k,107) + .0025005_r8*rxt(k,697)*y(k,154)
         mat(k,1081) = .1749305_r8*rxt(k,669)*y(k,156) + .1284005_r8*rxt(k,672) &
                      *y(k,166)
         mat(k,2033) = .1284005_r8*rxt(k,670)*y(k,224) + .0443005_r8*rxt(k,675) &
                      *y(k,226) + .0271005_r8*rxt(k,678)*y(k,242) &
                      + .0076005_r8*rxt(k,682)*y(k,243) + .0554005_r8*rxt(k,686) &
                      *y(k,249) + .2157005_r8*rxt(k,692)*y(k,267) &
                      + .0512005_r8*rxt(k,696)*y(k,270)
         mat(k,961) = .0590245_r8*rxt(k,677)*y(k,156) + .0033005_r8*rxt(k,680) &
                      *y(k,166)
         mat(k,1037) = .1749305_r8*rxt(k,685)*y(k,156) + .0554005_r8*rxt(k,688) &
                      *y(k,166)
         mat(k,1927) = .079_r8*rxt(k,671)*y(k,224) + .0059005_r8*rxt(k,676)*y(k,226) &
                      + .0057005_r8*rxt(k,679)*y(k,242) + .0143005_r8*rxt(k,683) &
                      *y(k,243) + .0332005_r8*rxt(k,687)*y(k,249) &
                      + .0073005_r8*rxt(k,693)*y(k,267) + .011_r8*rxt(k,697)*y(k,270)
         mat(k,2329) = .1749305_r8*rxt(k,669)*y(k,6) + .0590245_r8*rxt(k,677)*y(k,128) &
                      + .1749305_r8*rxt(k,685)*y(k,140)
         mat(k,2716) = .1284005_r8*rxt(k,672)*y(k,6) + .0033005_r8*rxt(k,680)*y(k,128) &
                      + .0554005_r8*rxt(k,688)*y(k,140)
         mat(k,57) = .0085005_r8*rxt(k,690)*y(k,259)
         mat(k,63) = .1284005_r8*rxt(k,670)*y(k,107) + .079_r8*rxt(k,671)*y(k,154)
         mat(k,69) = .0443005_r8*rxt(k,675)*y(k,107) + .0059005_r8*rxt(k,676)*y(k,154)
         mat(k,75) = .0271005_r8*rxt(k,678)*y(k,107) + .0057005_r8*rxt(k,679)*y(k,154)
         mat(k,81) = .0076005_r8*rxt(k,682)*y(k,107) + .0143005_r8*rxt(k,683)*y(k,154)
         mat(k,89) = .0554005_r8*rxt(k,686)*y(k,107) + .0332005_r8*rxt(k,687)*y(k,154)
         mat(k,2149) = .0085005_r8*rxt(k,690)*y(k,206)
         mat(k,95) = .2157005_r8*rxt(k,692)*y(k,107) + .0073005_r8*rxt(k,693)*y(k,154)
         mat(k,101) = .0512005_r8*rxt(k,696)*y(k,107) + .011_r8*rxt(k,697)*y(k,154)
         mat(k,1082) = .5901905_r8*rxt(k,669)*y(k,156) + .114_r8*rxt(k,672)*y(k,166)
         mat(k,2034) = .114_r8*rxt(k,670)*y(k,224) + .1621005_r8*rxt(k,675)*y(k,226) &
                      + .0474005_r8*rxt(k,678)*y(k,242) + .0113005_r8*rxt(k,682) &
                      *y(k,243) + .1278005_r8*rxt(k,686)*y(k,249) &
                      + .0738005_r8*rxt(k,692)*y(k,267) + .1598005_r8*rxt(k,696) &
                      *y(k,270)
         mat(k,962) = .0250245_r8*rxt(k,677)*y(k,156)
         mat(k,1038) = .5901905_r8*rxt(k,685)*y(k,156) + .1278005_r8*rxt(k,688) &
                      *y(k,166)
         mat(k,1928) = .1254005_r8*rxt(k,671)*y(k,224) + .0536005_r8*rxt(k,676) &
                      *y(k,226) + .0623005_r8*rxt(k,679)*y(k,242) &
                      + .0166005_r8*rxt(k,683)*y(k,243) + .130_r8*rxt(k,687)*y(k,249) &
                      + .238_r8*rxt(k,693)*y(k,267) + .1185005_r8*rxt(k,697)*y(k,270)
         mat(k,2330) = .5901905_r8*rxt(k,669)*y(k,6) + .0250245_r8*rxt(k,677)*y(k,128) &
                      + .5901905_r8*rxt(k,685)*y(k,140)
         mat(k,2717) = .114_r8*rxt(k,672)*y(k,6) + .1278005_r8*rxt(k,688)*y(k,140)
         mat(k,58) = .0128005_r8*rxt(k,690)*y(k,259)
         mat(k,64) = .114_r8*rxt(k,670)*y(k,107) + .1254005_r8*rxt(k,671)*y(k,154)
         mat(k,70) = .1621005_r8*rxt(k,675)*y(k,107) + .0536005_r8*rxt(k,676)*y(k,154)
         mat(k,76) = .0474005_r8*rxt(k,678)*y(k,107) + .0623005_r8*rxt(k,679)*y(k,154)
         mat(k,82) = .0113005_r8*rxt(k,682)*y(k,107) + .0166005_r8*rxt(k,683)*y(k,154)
         mat(k,90) = .1278005_r8*rxt(k,686)*y(k,107) + .130_r8*rxt(k,687)*y(k,154)
         mat(k,2150) = .0128005_r8*rxt(k,690)*y(k,206)
         mat(k,96) = .0738005_r8*rxt(k,692)*y(k,107) + .238_r8*rxt(k,693)*y(k,154)
         mat(k,102) = .1598005_r8*rxt(k,696)*y(k,107) + .1185005_r8*rxt(k,697) &
                      *y(k,154)
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
         mat(k,59) = -(rxt(k,690)*y(k,259))
         mat(k,2151) = -rxt(k,690)*y(k,206)
         mat(k,225) = .100_r8*rxt(k,566)*y(k,259)
         mat(k,245) = .230_r8*rxt(k,568)*y(k,259)
         mat(k,2174) = .100_r8*rxt(k,566)*y(k,214) + .230_r8*rxt(k,568)*y(k,216)
         mat(k,784) = -(rxt(k,590)*y(k,259))
         mat(k,2246) = -rxt(k,590)*y(k,208)
         mat(k,2077) = rxt(k,588)*y(k,264)
         mat(k,1284) = rxt(k,588)*y(k,107)
         mat(k,726) = -(rxt(k,591)*y(k,259))
         mat(k,2241) = -rxt(k,591)*y(k,209)
         mat(k,1956) = .200_r8*rxt(k,584)*y(k,254) + .200_r8*rxt(k,594)*y(k,265)
         mat(k,1716) = .500_r8*rxt(k,582)*y(k,254)
         mat(k,1202) = .200_r8*rxt(k,584)*y(k,154) + .500_r8*rxt(k,582)*y(k,232)
         mat(k,1140) = .200_r8*rxt(k,594)*y(k,154)
         mat(k,569) = -(rxt(k,595)*y(k,259))
         mat(k,2223) = -rxt(k,595)*y(k,210)
         mat(k,2065) = rxt(k,593)*y(k,265)
         mat(k,1139) = rxt(k,593)*y(k,107)
         mat(k,1152) = -(rxt(k,596)*y(k,156) + rxt(k,597)*y(k,259))
         mat(k,2346) = -rxt(k,596)*y(k,211)
         mat(k,2272) = -rxt(k,597)*y(k,211)
         mat(k,1091) = .330_r8*rxt(k,577)*y(k,166)
         mat(k,1047) = .330_r8*rxt(k,580)*y(k,166)
         mat(k,1980) = .800_r8*rxt(k,584)*y(k,254) + .800_r8*rxt(k,594)*y(k,265)
         mat(k,2346) = mat(k,2346) + rxt(k,585)*y(k,254)
         mat(k,2736) = .330_r8*rxt(k,577)*y(k,6) + .330_r8*rxt(k,580)*y(k,140)
         mat(k,727) = rxt(k,591)*y(k,259)
         mat(k,1724) = .500_r8*rxt(k,582)*y(k,254) + rxt(k,592)*y(k,265)
         mat(k,1204) = .800_r8*rxt(k,584)*y(k,154) + rxt(k,585)*y(k,156) &
                      + .500_r8*rxt(k,582)*y(k,232)
         mat(k,2272) = mat(k,2272) + rxt(k,591)*y(k,209)
         mat(k,1143) = .800_r8*rxt(k,594)*y(k,154) + rxt(k,592)*y(k,232)
         mat(k,1218) = -(rxt(k,598)*y(k,259))
         mat(k,2278) = -rxt(k,598)*y(k,212)
         mat(k,1094) = .300_r8*rxt(k,577)*y(k,166)
         mat(k,1050) = .300_r8*rxt(k,580)*y(k,166)
         mat(k,1984) = .900_r8*rxt(k,589)*y(k,264)
         mat(k,2740) = .300_r8*rxt(k,577)*y(k,6) + .300_r8*rxt(k,580)*y(k,140)
         mat(k,1728) = rxt(k,587)*y(k,264)
         mat(k,1287) = .900_r8*rxt(k,589)*y(k,154) + rxt(k,587)*y(k,232)
         mat(k,688) = -(rxt(k,565)*y(k,259))
         mat(k,2237) = -rxt(k,565)*y(k,213)
         mat(k,2071) = rxt(k,563)*y(k,266)
         mat(k,822) = rxt(k,563)*y(k,107)
         mat(k,223) = -(rxt(k,566)*y(k,259))
         mat(k,2172) = -rxt(k,566)*y(k,214)
         mat(k,239) = -(rxt(k,532)*y(k,259))
         mat(k,2175) = -rxt(k,532)*y(k,215)
         mat(k,2043) = rxt(k,529)*y(k,268)
         mat(k,1354) = rxt(k,529)*y(k,107)
         mat(k,246) = -(rxt(k,568)*y(k,259))
         mat(k,2176) = -rxt(k,568)*y(k,216)
         mat(k,803) = -(rxt(k,571)*y(k,259))
         mat(k,2248) = -rxt(k,571)*y(k,217)
         mat(k,2079) = rxt(k,569)*y(k,269)
         mat(k,854) = rxt(k,569)*y(k,107)
         mat(k,254) = -(rxt(k,574)*y(k,259))
         mat(k,2177) = -rxt(k,574)*y(k,218)
         mat(k,247) = .150_r8*rxt(k,568)*y(k,259)
         mat(k,2177) = mat(k,2177) + .150_r8*rxt(k,568)*y(k,216)
         mat(k,498) = -(rxt(k,575)*y(k,259))
         mat(k,2215) = -rxt(k,575)*y(k,219)
         mat(k,2057) = rxt(k,572)*y(k,271)
         mat(k,592) = rxt(k,572)*y(k,107)
         mat(k,615) = -(rxt(k,533)*y(k,107) + rxt(k,534)*y(k,154) + rxt(k,562) &
                      *y(k,155))
         mat(k,2068) = -rxt(k,533)*y(k,222)
         mat(k,1951) = -rxt(k,534)*y(k,222)
         mat(k,2667) = -rxt(k,562)*y(k,222)
         mat(k,285) = rxt(k,539)*y(k,259)
         mat(k,2228) = rxt(k,539)*y(k,24)
         mat(k,1111) = -(rxt(k,494)*y(k,107) + (rxt(k,495) + rxt(k,496)) * y(k,154))
         mat(k,2092) = -rxt(k,494)*y(k,223)
         mat(k,1977) = -(rxt(k,495) + rxt(k,496)) * y(k,223)
         mat(k,758) = rxt(k,497)*y(k,259)
         mat(k,282) = rxt(k,498)*y(k,259)
         mat(k,2268) = rxt(k,497)*y(k,2) + rxt(k,498)*y(k,15)
         mat(k,65) = -(rxt(k,670)*y(k,107) + rxt(k,671)*y(k,154))
         mat(k,2035) = -rxt(k,670)*y(k,224)
         mat(k,1929) = -rxt(k,671)*y(k,224)
         mat(k,1083) = rxt(k,673)*y(k,259)
         mat(k,2152) = rxt(k,673)*y(k,6)
         mat(k,578) = -(rxt(k,536)*y(k,107) + rxt(k,537)*y(k,154))
         mat(k,2066) = -rxt(k,536)*y(k,225)
         mat(k,1948) = -rxt(k,537)*y(k,225)
         mat(k,193) = .350_r8*rxt(k,535)*y(k,259)
         mat(k,470) = rxt(k,538)*y(k,259)
         mat(k,2224) = .350_r8*rxt(k,535)*y(k,7) + rxt(k,538)*y(k,8)
         mat(k,71) = -(rxt(k,675)*y(k,107) + rxt(k,676)*y(k,154))
         mat(k,2036) = -rxt(k,675)*y(k,226)
         mat(k,1930) = -rxt(k,676)*y(k,226)
         mat(k,189) = rxt(k,674)*y(k,259)
         mat(k,2153) = rxt(k,674)*y(k,7)
         mat(k,506) = -(rxt(k,540)*y(k,107) + rxt(k,542)*y(k,154))
         mat(k,2058) = -rxt(k,540)*y(k,227)
         mat(k,1942) = -rxt(k,542)*y(k,227)
         mat(k,379) = rxt(k,541)*y(k,259)
         mat(k,226) = .070_r8*rxt(k,566)*y(k,259)
         mat(k,248) = .060_r8*rxt(k,568)*y(k,259)
         mat(k,2216) = rxt(k,541)*y(k,25) + .070_r8*rxt(k,566)*y(k,214) &
                      + .060_r8*rxt(k,568)*y(k,216)
         mat(k,1004) = -(4._r8*rxt(k,416)*y(k,228) + rxt(k,417)*y(k,232) + rxt(k,418) &
                      *y(k,107) + rxt(k,419)*y(k,154))
         mat(k,1720) = -rxt(k,417)*y(k,228)
         mat(k,2089) = -rxt(k,418)*y(k,228)
         mat(k,1973) = -rxt(k,419)*y(k,228)
         mat(k,389) = .500_r8*rxt(k,421)*y(k,259)
         mat(k,338) = rxt(k,422)*y(k,70) + rxt(k,423)*y(k,259)
         mat(k,2452) = rxt(k,422)*y(k,32)
         mat(k,2262) = .500_r8*rxt(k,421)*y(k,31) + rxt(k,423)*y(k,32)
         mat(k,1023) = -(rxt(k,445)*y(k,232) + rxt(k,446)*y(k,107) + rxt(k,447) &
                      *y(k,154))
         mat(k,1721) = -rxt(k,445)*y(k,229)
         mat(k,2091) = -rxt(k,446)*y(k,229)
         mat(k,1975) = -rxt(k,447)*y(k,229)
         mat(k,481) = rxt(k,448)*y(k,259)
         mat(k,344) = rxt(k,452)*y(k,70) + rxt(k,449)*y(k,259)
         mat(k,2453) = rxt(k,452)*y(k,35)
         mat(k,2264) = rxt(k,448)*y(k,34) + rxt(k,449)*y(k,35)
         mat(k,742) = -(rxt(k,543)*y(k,107) + rxt(k,544)*y(k,154))
         mat(k,2074) = -rxt(k,543)*y(k,230)
         mat(k,1958) = -rxt(k,544)*y(k,230)
         mat(k,310) = rxt(k,545)*y(k,259)
         mat(k,2074) = mat(k,2074) + .400_r8*rxt(k,533)*y(k,222)
         mat(k,1958) = mat(k,1958) + rxt(k,534)*y(k,222)
         mat(k,2725) = rxt(k,560)*y(k,174)
         mat(k,549) = rxt(k,560)*y(k,166)
         mat(k,616) = .400_r8*rxt(k,533)*y(k,107) + rxt(k,534)*y(k,154)
         mat(k,2242) = rxt(k,545)*y(k,36)
         mat(k,1555) = -(4._r8*rxt(k,427)*y(k,231) + rxt(k,428)*y(k,232) + rxt(k,429) &
                      *y(k,107) + rxt(k,430)*y(k,154) + rxt(k,441)*y(k,155) + rxt(k,469) &
                      *y(k,244) + rxt(k,502)*y(k,239) + rxt(k,507)*y(k,240) + rxt(k,516) &
                      *y(k,241) + rxt(k,527)*y(k,268))
         mat(k,1745) = -rxt(k,428)*y(k,231)
         mat(k,2116) = -rxt(k,429)*y(k,231)
         mat(k,2002) = -rxt(k,430)*y(k,231)
         mat(k,2684) = -rxt(k,441)*y(k,231)
         mat(k,1485) = -rxt(k,469)*y(k,231)
         mat(k,1430) = -rxt(k,502)*y(k,231)
         mat(k,1463) = -rxt(k,507)*y(k,231)
         mat(k,1384) = -rxt(k,516)*y(k,231)
         mat(k,1362) = -rxt(k,527)*y(k,231)
         mat(k,1098) = .060_r8*rxt(k,577)*y(k,166)
         mat(k,1265) = rxt(k,424)*y(k,156) + rxt(k,425)*y(k,259)
         mat(k,1409) = rxt(k,450)*y(k,156) + rxt(k,451)*y(k,259)
         mat(k,671) = .500_r8*rxt(k,432)*y(k,259)
         mat(k,2116) = mat(k,2116) + .450_r8*rxt(k,480)*y(k,246) + .200_r8*rxt(k,484) &
                      *y(k,248) + .150_r8*rxt(k,459)*y(k,263)
         mat(k,973) = .080_r8*rxt(k,522)*y(k,166)
         mat(k,1400) = .100_r8*rxt(k,475)*y(k,166)
         mat(k,1054) = .060_r8*rxt(k,580)*y(k,166)
         mat(k,1505) = .280_r8*rxt(k,489)*y(k,166)
         mat(k,2002) = mat(k,2002) + .530_r8*rxt(k,473)*y(k,244) + rxt(k,482)*y(k,246) &
                      + rxt(k,485)*y(k,248) + rxt(k,460)*y(k,263)
         mat(k,2372) = rxt(k,424)*y(k,54) + rxt(k,450)*y(k,58) + .530_r8*rxt(k,472) &
                      *y(k,244) + rxt(k,483)*y(k,246)
         mat(k,2756) = .060_r8*rxt(k,577)*y(k,6) + .080_r8*rxt(k,522)*y(k,128) &
                      + .100_r8*rxt(k,475)*y(k,135) + .060_r8*rxt(k,580)*y(k,140) &
                      + .280_r8*rxt(k,489)*y(k,141)
         mat(k,1221) = .650_r8*rxt(k,598)*y(k,259)
         mat(k,1555) = mat(k,1555) + .530_r8*rxt(k,469)*y(k,244)
         mat(k,1745) = mat(k,1745) + .260_r8*rxt(k,470)*y(k,244) + rxt(k,479)*y(k,246) &
                      + .300_r8*rxt(k,458)*y(k,263)
         mat(k,1485) = mat(k,1485) + .530_r8*rxt(k,473)*y(k,154) + .530_r8*rxt(k,472) &
                      *y(k,156) + .530_r8*rxt(k,469)*y(k,231) + .260_r8*rxt(k,470) &
                      *y(k,232)
         mat(k,1525) = .450_r8*rxt(k,480)*y(k,107) + rxt(k,482)*y(k,154) + rxt(k,483) &
                      *y(k,156) + rxt(k,479)*y(k,232) + 4.000_r8*rxt(k,481)*y(k,246)
         mat(k,779) = .200_r8*rxt(k,484)*y(k,107) + rxt(k,485)*y(k,154)
         mat(k,2298) = rxt(k,425)*y(k,54) + rxt(k,451)*y(k,58) + .500_r8*rxt(k,432) &
                      *y(k,60) + .650_r8*rxt(k,598)*y(k,212)
         mat(k,1346) = .150_r8*rxt(k,459)*y(k,107) + rxt(k,460)*y(k,154) &
                      + .300_r8*rxt(k,458)*y(k,232)
         mat(k,1748) = -(rxt(k,256)*y(k,74) + (rxt(k,375) + rxt(k,376)) * y(k,68) &
                      + (4._r8*rxt(k,395) + 4._r8*rxt(k,396)) * y(k,232) + rxt(k,397) &
                      *y(k,107) + rxt(k,398)*y(k,154) + rxt(k,417)*y(k,228) + rxt(k,428) &
                      *y(k,231) + rxt(k,445)*y(k,229) + rxt(k,458)*y(k,263) + rxt(k,470) &
                      *y(k,244) + rxt(k,479)*y(k,246) + rxt(k,503)*y(k,239) + rxt(k,508) &
                      *y(k,240) + rxt(k,517)*y(k,241) + rxt(k,528)*y(k,268) + rxt(k,582) &
                      *y(k,254) + rxt(k,587)*y(k,264) + rxt(k,592)*y(k,265))
         mat(k,2409) = -rxt(k,256)*y(k,232)
         mat(k,1231) = -(rxt(k,375) + rxt(k,376)) * y(k,232)
         mat(k,2124) = -rxt(k,397)*y(k,232)
         mat(k,2008) = -rxt(k,398)*y(k,232)
         mat(k,1006) = -rxt(k,417)*y(k,232)
         mat(k,1558) = -rxt(k,428)*y(k,232)
         mat(k,1026) = -rxt(k,445)*y(k,232)
         mat(k,1347) = -rxt(k,458)*y(k,232)
         mat(k,1487) = -rxt(k,470)*y(k,232)
         mat(k,1527) = -rxt(k,479)*y(k,232)
         mat(k,1432) = -rxt(k,503)*y(k,232)
         mat(k,1465) = -rxt(k,508)*y(k,232)
         mat(k,1386) = -rxt(k,517)*y(k,232)
         mat(k,1364) = -rxt(k,528)*y(k,232)
         mat(k,1209) = -rxt(k,582)*y(k,232)
         mat(k,1294) = -rxt(k,587)*y(k,232)
         mat(k,1145) = -rxt(k,592)*y(k,232)
         mat(k,1253) = .280_r8*rxt(k,444)*y(k,166)
         mat(k,796) = rxt(k,431)*y(k,259)
         mat(k,493) = .700_r8*rxt(k,400)*y(k,259)
         mat(k,1651) = rxt(k,248)*y(k,70) + rxt(k,349)*y(k,89) + rxt(k,407)*y(k,255) &
                      + rxt(k,401)*y(k,259)
         mat(k,2471) = rxt(k,248)*y(k,64)
         mat(k,990) = rxt(k,349)*y(k,64)
         mat(k,2124) = mat(k,2124) + .490_r8*rxt(k,429)*y(k,231) + .330_r8*rxt(k,547) &
                      *y(k,233) + .070_r8*rxt(k,553)*y(k,247)
         mat(k,974) = .050_r8*rxt(k,522)*y(k,166)
         mat(k,2008) = mat(k,2008) + rxt(k,430)*y(k,231) + .830_r8*rxt(k,548)*y(k,233) &
                      + .170_r8*rxt(k,554)*y(k,247)
         mat(k,2760) = .280_r8*rxt(k,444)*y(k,33) + .050_r8*rxt(k,522)*y(k,128)
         mat(k,1558) = mat(k,1558) + .490_r8*rxt(k,429)*y(k,107) + rxt(k,430)*y(k,154) &
                      + 4.000_r8*rxt(k,427)*y(k,231) + .900_r8*rxt(k,428)*y(k,232) &
                      + rxt(k,502)*y(k,239) + rxt(k,507)*y(k,240) + rxt(k,516) &
                      *y(k,241) + rxt(k,469)*y(k,244) + rxt(k,478)*y(k,246) &
                      + rxt(k,527)*y(k,268)
         mat(k,1748) = mat(k,1748) + .900_r8*rxt(k,428)*y(k,231)
         mat(k,870) = .330_r8*rxt(k,547)*y(k,107) + .830_r8*rxt(k,548)*y(k,154)
         mat(k,1432) = mat(k,1432) + rxt(k,502)*y(k,231)
         mat(k,1465) = mat(k,1465) + rxt(k,507)*y(k,231)
         mat(k,1386) = mat(k,1386) + rxt(k,516)*y(k,231)
         mat(k,1487) = mat(k,1487) + rxt(k,469)*y(k,231)
         mat(k,1527) = mat(k,1527) + rxt(k,478)*y(k,231)
         mat(k,1017) = .070_r8*rxt(k,553)*y(k,107) + .170_r8*rxt(k,554)*y(k,154)
         mat(k,1871) = rxt(k,407)*y(k,64)
         mat(k,2307) = rxt(k,431)*y(k,59) + .700_r8*rxt(k,400)*y(k,63) + rxt(k,401) &
                      *y(k,64)
         mat(k,1364) = mat(k,1364) + rxt(k,527)*y(k,231)
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
         mat(k,867) = -(rxt(k,547)*y(k,107) + rxt(k,548)*y(k,154) + rxt(k,549) &
                      *y(k,155))
         mat(k,2084) = -rxt(k,547)*y(k,233)
         mat(k,1965) = -rxt(k,548)*y(k,233)
         mat(k,2672) = -rxt(k,549)*y(k,233)
         mat(k,929) = -(rxt(k,740)*y(k,252) + rxt(k,741)*y(k,258) + rxt(k,742) &
                      *y(k,251))
         mat(k,910) = -rxt(k,740)*y(k,234)
         mat(k,918) = -rxt(k,741)*y(k,234)
         mat(k,718) = -rxt(k,742)*y(k,234)
         mat(k,653) = -((rxt(k,466) + rxt(k,467)) * y(k,154))
         mat(k,1953) = -(rxt(k,466) + rxt(k,467)) * y(k,235)
         mat(k,416) = rxt(k,465)*y(k,259)
         mat(k,2233) = rxt(k,465)*y(k,16)
         mat(k,539) = -(rxt(k,436)*y(k,165))
         mat(k,1768) = -rxt(k,436)*y(k,236)
         mat(k,1946) = .750_r8*rxt(k,434)*y(k,237)
         mat(k,876) = .750_r8*rxt(k,434)*y(k,154)
         mat(k,877) = -(rxt(k,433)*y(k,107) + rxt(k,434)*y(k,154))
         mat(k,2085) = -rxt(k,433)*y(k,237)
         mat(k,1966) = -rxt(k,434)*y(k,237)
         mat(k,646) = rxt(k,440)*y(k,259)
         mat(k,2255) = rxt(k,440)*y(k,28)
         mat(k,512) = -(rxt(k,404)*y(k,107) + rxt(k,406)*y(k,154))
         mat(k,2059) = -rxt(k,404)*y(k,238)
         mat(k,1943) = -rxt(k,406)*y(k,238)
         mat(k,1893) = rxt(k,391)*y(k,107)
         mat(k,2059) = mat(k,2059) + rxt(k,391)*y(k,51)
         mat(k,1426) = -(rxt(k,502)*y(k,231) + rxt(k,503)*y(k,232) + rxt(k,504) &
                      *y(k,107) + rxt(k,505)*y(k,154) + rxt(k,506)*y(k,156))
         mat(k,1550) = -rxt(k,502)*y(k,239)
         mat(k,1740) = -rxt(k,503)*y(k,239)
         mat(k,2111) = -rxt(k,504)*y(k,239)
         mat(k,1997) = -rxt(k,505)*y(k,239)
         mat(k,2367) = -rxt(k,506)*y(k,239)
         mat(k,970) = .600_r8*rxt(k,523)*y(k,259)
         mat(k,2293) = .600_r8*rxt(k,523)*y(k,128)
         mat(k,1459) = -(rxt(k,507)*y(k,231) + rxt(k,508)*y(k,232) + rxt(k,509) &
                      *y(k,107) + rxt(k,511)*y(k,154) + rxt(k,512)*y(k,156))
         mat(k,1551) = -rxt(k,507)*y(k,240)
         mat(k,1741) = -rxt(k,508)*y(k,240)
         mat(k,2112) = -rxt(k,509)*y(k,240)
         mat(k,1998) = -rxt(k,511)*y(k,240)
         mat(k,2368) = -rxt(k,512)*y(k,240)
         mat(k,971) = .400_r8*rxt(k,523)*y(k,259)
         mat(k,2294) = .400_r8*rxt(k,523)*y(k,128)
         mat(k,1380) = -(rxt(k,516)*y(k,231) + rxt(k,517)*y(k,232) + rxt(k,518) &
                      *y(k,107) + rxt(k,519)*y(k,154) + rxt(k,520)*y(k,156))
         mat(k,1547) = -rxt(k,516)*y(k,241)
         mat(k,1737) = -rxt(k,517)*y(k,241)
         mat(k,2108) = -rxt(k,518)*y(k,241)
         mat(k,1994) = -rxt(k,519)*y(k,241)
         mat(k,2364) = -rxt(k,520)*y(k,241)
         mat(k,968) = rxt(k,515)*y(k,156)
         mat(k,2364) = mat(k,2364) + rxt(k,515)*y(k,128)
         mat(k,77) = -(rxt(k,678)*y(k,107) + rxt(k,679)*y(k,154))
         mat(k,2037) = -rxt(k,678)*y(k,242)
         mat(k,1931) = -rxt(k,679)*y(k,242)
         mat(k,963) = rxt(k,681)*y(k,259)
         mat(k,2154) = rxt(k,681)*y(k,128)
         mat(k,83) = -(rxt(k,682)*y(k,107) + rxt(k,683)*y(k,154))
         mat(k,2038) = -rxt(k,682)*y(k,243)
         mat(k,1932) = -rxt(k,683)*y(k,243)
         mat(k,84) = rxt(k,684)*y(k,259)
         mat(k,2155) = rxt(k,684)*y(k,133)
         mat(k,1483) = -(rxt(k,469)*y(k,231) + rxt(k,470)*y(k,232) + rxt(k,471) &
                      *y(k,107) + rxt(k,472)*y(k,156) + (rxt(k,473) + rxt(k,474) &
                      ) * y(k,154))
         mat(k,1552) = -rxt(k,469)*y(k,244)
         mat(k,1742) = -rxt(k,470)*y(k,244)
         mat(k,2113) = -rxt(k,471)*y(k,244)
         mat(k,2369) = -rxt(k,472)*y(k,244)
         mat(k,1999) = -(rxt(k,473) + rxt(k,474)) * y(k,244)
         mat(k,1398) = .500_r8*rxt(k,476)*y(k,259)
         mat(k,356) = .200_r8*rxt(k,477)*y(k,259)
         mat(k,1502) = rxt(k,490)*y(k,259)
         mat(k,2295) = .500_r8*rxt(k,476)*y(k,135) + .200_r8*rxt(k,477)*y(k,136) &
                      + rxt(k,490)*y(k,141)
         mat(k,814) = -(rxt(k,550)*y(k,107) + rxt(k,551)*y(k,154) + rxt(k,552) &
                      *y(k,155))
         mat(k,2080) = -rxt(k,550)*y(k,245)
         mat(k,1962) = -rxt(k,551)*y(k,245)
         mat(k,2671) = -rxt(k,552)*y(k,245)
         mat(k,1524) = -(rxt(k,478)*y(k,231) + rxt(k,479)*y(k,232) + rxt(k,480) &
                      *y(k,107) + 4._r8*rxt(k,481)*y(k,246) + rxt(k,482)*y(k,154) &
                      + rxt(k,483)*y(k,156) + rxt(k,491)*y(k,155))
         mat(k,1554) = -rxt(k,478)*y(k,246)
         mat(k,1744) = -rxt(k,479)*y(k,246)
         mat(k,2115) = -rxt(k,480)*y(k,246)
         mat(k,2001) = -rxt(k,482)*y(k,246)
         mat(k,2371) = -rxt(k,483)*y(k,246)
         mat(k,2683) = -rxt(k,491)*y(k,246)
         mat(k,1399) = .500_r8*rxt(k,476)*y(k,259)
         mat(k,357) = .500_r8*rxt(k,477)*y(k,259)
         mat(k,2297) = .500_r8*rxt(k,476)*y(k,135) + .500_r8*rxt(k,477)*y(k,136)
         mat(k,1013) = -(rxt(k,553)*y(k,107) + rxt(k,554)*y(k,154) + rxt(k,555) &
                      *y(k,155))
         mat(k,2090) = -rxt(k,553)*y(k,247)
         mat(k,1974) = -rxt(k,554)*y(k,247)
         mat(k,2675) = -rxt(k,555)*y(k,247)
         mat(k,777) = -(rxt(k,484)*y(k,107) + rxt(k,485)*y(k,154))
         mat(k,2076) = -rxt(k,484)*y(k,248)
         mat(k,1961) = -rxt(k,485)*y(k,248)
         mat(k,601) = rxt(k,486)*y(k,259)
         mat(k,361) = rxt(k,487)*y(k,259)
         mat(k,2245) = rxt(k,486)*y(k,137) + rxt(k,487)*y(k,138)
         mat(k,91) = -(rxt(k,686)*y(k,107) + rxt(k,687)*y(k,154))
         mat(k,2039) = -rxt(k,686)*y(k,249)
         mat(k,1933) = -rxt(k,687)*y(k,249)
         mat(k,1039) = rxt(k,689)*y(k,259)
         mat(k,2157) = rxt(k,689)*y(k,140)
         mat(k,748) = -(rxt(k,219)*y(k,154) + rxt(k,220)*y(k,164) + rxt(k,243) &
                      *y(k,234) + rxt(k,244)*y(k,165))
         mat(k,1959) = -rxt(k,219)*y(k,250)
         mat(k,2614) = -rxt(k,220)*y(k,250)
         mat(k,925) = -rxt(k,243)*y(k,250)
         mat(k,1771) = -rxt(k,244)*y(k,250)
         mat(k,2614) = mat(k,2614) + rxt(k,744)*y(k,251)
         mat(k,925) = mat(k,925) + .900_r8*rxt(k,742)*y(k,251) + .800_r8*rxt(k,740) &
                      *y(k,252)
         mat(k,714) = rxt(k,744)*y(k,164) + .900_r8*rxt(k,742)*y(k,234)
         mat(k,908) = .800_r8*rxt(k,740)*y(k,234)
         mat(k,713) = -(rxt(k,742)*y(k,234) + rxt(k,743)*y(k,165) + (rxt(k,744) &
                      + rxt(k,745)) * y(k,164))
         mat(k,924) = -rxt(k,742)*y(k,251)
         mat(k,1770) = -rxt(k,743)*y(k,251)
         mat(k,2613) = -(rxt(k,744) + rxt(k,745)) * y(k,251)
         mat(k,909) = -(rxt(k,740)*y(k,234))
         mat(k,927) = -rxt(k,740)*y(k,252)
         mat(k,1065) = rxt(k,749)*y(k,258)
         mat(k,1968) = rxt(k,751)*y(k,258)
         mat(k,2617) = rxt(k,744)*y(k,251)
         mat(k,1774) = rxt(k,748)*y(k,253)
         mat(k,716) = rxt(k,744)*y(k,164)
         mat(k,587) = rxt(k,748)*y(k,165)
         mat(k,916) = rxt(k,749)*y(k,142) + rxt(k,751)*y(k,154)
         mat(k,585) = -(rxt(k,746)*y(k,164) + (rxt(k,747) + rxt(k,748)) * y(k,165))
         mat(k,2611) = -rxt(k,746)*y(k,253)
         mat(k,1769) = -(rxt(k,747) + rxt(k,748)) * y(k,253)
         mat(k,1205) = -(rxt(k,582)*y(k,232) + rxt(k,583)*y(k,107) + rxt(k,584) &
                      *y(k,154) + rxt(k,585)*y(k,156))
         mat(k,1727) = -rxt(k,582)*y(k,254)
         mat(k,2097) = -rxt(k,583)*y(k,254)
         mat(k,1983) = -rxt(k,584)*y(k,254)
         mat(k,2351) = -rxt(k,585)*y(k,254)
         mat(k,1093) = rxt(k,576)*y(k,156)
         mat(k,1049) = rxt(k,579)*y(k,156)
         mat(k,2351) = mat(k,2351) + rxt(k,576)*y(k,6) + rxt(k,579)*y(k,140) &
                      + .500_r8*rxt(k,596)*y(k,211)
         mat(k,464) = rxt(k,586)*y(k,259)
         mat(k,1153) = .500_r8*rxt(k,596)*y(k,156)
         mat(k,2277) = rxt(k,586)*y(k,158)
         mat(k,1875) = -(rxt(k,182)*y(k,93) + rxt(k,183)*y(k,272) + (rxt(k,185) &
                      + rxt(k,186)) * y(k,165) + (rxt(k,187) + rxt(k,188)) * y(k,166) &
                      + (rxt(k,236) + rxt(k,237)) * y(k,143) + rxt(k,274)*y(k,37) &
                      + rxt(k,275)*y(k,38) + rxt(k,276)*y(k,40) + rxt(k,277)*y(k,41) &
                      + rxt(k,278)*y(k,42) + rxt(k,279)*y(k,43) + rxt(k,280)*y(k,44) &
                      + (rxt(k,281) + rxt(k,282)) * y(k,101) + rxt(k,305)*y(k,39) &
                      + rxt(k,306)*y(k,66) + rxt(k,307)*y(k,94) + (rxt(k,308) &
                      + rxt(k,309)) * y(k,97) + rxt(k,353)*y(k,80) + rxt(k,354) &
                      *y(k,81) + rxt(k,386)*y(k,45) + rxt(k,387)*y(k,52) + rxt(k,388) &
                      *y(k,98) + rxt(k,389)*y(k,99) + rxt(k,390)*y(k,100) + (rxt(k,407) &
                      + rxt(k,408) + rxt(k,409)) * y(k,64) + rxt(k,410)*y(k,102))
         mat(k,1620) = -rxt(k,182)*y(k,255)
         mat(k,2851) = -rxt(k,183)*y(k,255)
         mat(k,1784) = -(rxt(k,185) + rxt(k,186)) * y(k,255)
         mat(k,2764) = -(rxt(k,187) + rxt(k,188)) * y(k,255)
         mat(k,297) = -(rxt(k,236) + rxt(k,237)) * y(k,255)
         mat(k,113) = -rxt(k,274)*y(k,255)
         mat(k,171) = -rxt(k,275)*y(k,255)
         mat(k,136) = -rxt(k,276)*y(k,255)
         mat(k,182) = -rxt(k,277)*y(k,255)
         mat(k,140) = -rxt(k,278)*y(k,255)
         mat(k,187) = -rxt(k,279)*y(k,255)
         mat(k,144) = -rxt(k,280)*y(k,255)
         mat(k,1831) = -(rxt(k,281) + rxt(k,282)) * y(k,255)
         mat(k,177) = -rxt(k,305)*y(k,255)
         mat(k,452) = -rxt(k,306)*y(k,255)
         mat(k,128) = -rxt(k,307)*y(k,255)
         mat(k,1593) = -(rxt(k,308) + rxt(k,309)) * y(k,255)
         mat(k,275) = -rxt(k,353)*y(k,255)
         mat(k,266) = -rxt(k,354)*y(k,255)
         mat(k,564) = -rxt(k,386)*y(k,255)
         mat(k,707) = -rxt(k,387)*y(k,255)
         mat(k,261) = -rxt(k,388)*y(k,255)
         mat(k,270) = -rxt(k,389)*y(k,255)
         mat(k,315) = -rxt(k,390)*y(k,255)
         mat(k,1654) = -(rxt(k,407) + rxt(k,408) + rxt(k,409)) * y(k,255)
         mat(k,211) = -rxt(k,410)*y(k,255)
         mat(k,1784) = mat(k,1784) + rxt(k,244)*y(k,250)
         mat(k,934) = .850_r8*rxt(k,741)*y(k,258)
         mat(k,751) = rxt(k,244)*y(k,165)
         mat(k,921) = .850_r8*rxt(k,741)*y(k,234)
         mat(k,205) = -(rxt(k,190)*y(k,164) + rxt(k,191)*y(k,165))
         mat(k,2608) = -rxt(k,190)*y(k,256)
         mat(k,1766) = -rxt(k,191)*y(k,256)
         mat(k,1569) = rxt(k,192)*y(k,257)
         mat(k,2608) = mat(k,2608) + rxt(k,194)*y(k,257)
         mat(k,1766) = mat(k,1766) + rxt(k,195)*y(k,257)
         mat(k,2718) = rxt(k,196)*y(k,257)
         mat(k,207) = rxt(k,192)*y(k,78) + rxt(k,194)*y(k,164) + rxt(k,195)*y(k,165) &
                      + rxt(k,196)*y(k,166)
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
         mat(k,208) = -(rxt(k,192)*y(k,78) + rxt(k,194)*y(k,164) + rxt(k,195)*y(k,165) &
                      + rxt(k,196)*y(k,166))
         mat(k,1570) = -rxt(k,192)*y(k,257)
         mat(k,2609) = -rxt(k,194)*y(k,257)
         mat(k,1767) = -rxt(k,195)*y(k,257)
         mat(k,2719) = -rxt(k,196)*y(k,257)
         mat(k,1767) = mat(k,1767) + rxt(k,185)*y(k,255)
         mat(k,1854) = rxt(k,185)*y(k,165)
         mat(k,917) = -(rxt(k,741)*y(k,234) + rxt(k,749)*y(k,142) + rxt(k,751) &
                      *y(k,154))
         mat(k,928) = -rxt(k,741)*y(k,258)
         mat(k,1066) = -rxt(k,749)*y(k,258)
         mat(k,1969) = -rxt(k,751)*y(k,258)
         mat(k,1573) = rxt(k,752)*y(k,260)
         mat(k,1775) = rxt(k,743)*y(k,251) + rxt(k,747)*y(k,253) + rxt(k,754)*y(k,260)
         mat(k,717) = rxt(k,743)*y(k,165)
         mat(k,588) = rxt(k,747)*y(k,165)
         mat(k,887) = rxt(k,752)*y(k,78) + rxt(k,754)*y(k,165)
         mat(k,2315) = -(rxt(k,209)*y(k,93) + rxt(k,210)*y(k,95) + rxt(k,211)*y(k,107) &
                      + rxt(k,212)*y(k,164) + rxt(k,213)*y(k,166) + (4._r8*rxt(k,214) &
                      + 4._r8*rxt(k,215)) * y(k,259) + rxt(k,218)*y(k,108) + rxt(k,231) &
                      *y(k,156) + rxt(k,232)*y(k,142) + rxt(k,240)*y(k,155) + rxt(k,241) &
                      *y(k,106) + rxt(k,254)*y(k,73) + rxt(k,265)*y(k,75) + (rxt(k,267) &
                      + rxt(k,268)) * y(k,74) + rxt(k,270)*y(k,101) + rxt(k,273) &
                      *y(k,110) + rxt(k,285)*y(k,18) + rxt(k,301)*y(k,21) + rxt(k,303) &
                      *y(k,97) + rxt(k,311)*y(k,111) + rxt(k,314)*y(k,117) + rxt(k,337) &
                      *y(k,126) + rxt(k,338)*y(k,105) + rxt(k,356)*y(k,26) + rxt(k,358) &
                      *y(k,29) + rxt(k,360)*y(k,45) + rxt(k,361)*y(k,46) + rxt(k,363) &
                      *y(k,47) + rxt(k,365)*y(k,52) + rxt(k,366)*y(k,53) + rxt(k,368) &
                      *y(k,55) + rxt(k,370)*y(k,61) + rxt(k,371)*y(k,65) + rxt(k,373) &
                      *y(k,66) + rxt(k,374)*y(k,67) + rxt(k,382)*y(k,69) + rxt(k,383) &
                      *y(k,98) + rxt(k,384)*y(k,99) + rxt(k,385)*y(k,100) + rxt(k,394) &
                      *y(k,51) + rxt(k,399)*y(k,62) + rxt(k,400)*y(k,63) + rxt(k,401) &
                      *y(k,64) + rxt(k,402)*y(k,102) + rxt(k,403)*y(k,103) + rxt(k,411) &
                      *y(k,77) + rxt(k,413)*y(k,27) + rxt(k,420)*y(k,30) + rxt(k,421) &
                      *y(k,31) + rxt(k,423)*y(k,32) + rxt(k,425)*y(k,54) + rxt(k,426) &
                      *y(k,56) + rxt(k,431)*y(k,59) + rxt(k,432)*y(k,60) + rxt(k,437) &
                      *y(k,90) + rxt(k,438)*y(k,91) + rxt(k,439)*y(k,172) + rxt(k,440) &
                      *y(k,28) + rxt(k,448)*y(k,34) + rxt(k,449)*y(k,35) + rxt(k,451) &
                      *y(k,58) + rxt(k,453)*y(k,114) + rxt(k,454)*y(k,157) + rxt(k,457) &
                      *y(k,179) + rxt(k,461)*y(k,180) + rxt(k,462)*y(k,33) + rxt(k,463) &
                      *y(k,57) + rxt(k,465)*y(k,16) + rxt(k,468)*y(k,112) + rxt(k,476) &
                      *y(k,135) + rxt(k,477)*y(k,136) + rxt(k,486)*y(k,137) + rxt(k,487) &
                      *y(k,138) + rxt(k,488)*y(k,139) + rxt(k,490)*y(k,141) + rxt(k,493) &
                      *y(k,1) + rxt(k,497)*y(k,2) + rxt(k,498)*y(k,15) + rxt(k,499) &
                      *y(k,113) + rxt(k,500)*y(k,115) + rxt(k,501)*y(k,123) + rxt(k,513) &
                      *y(k,129) + rxt(k,514)*y(k,130) + rxt(k,521)*y(k,131) + rxt(k,523) &
                      *y(k,128) + rxt(k,524)*y(k,132) + rxt(k,525)*y(k,145) + rxt(k,526) &
                      *y(k,146) + rxt(k,532)*y(k,215) + rxt(k,535)*y(k,7) + rxt(k,538) &
                      *y(k,8) + rxt(k,539)*y(k,24) + rxt(k,541)*y(k,25) + rxt(k,545) &
                      *y(k,36) + rxt(k,546)*y(k,82) + rxt(k,558)*y(k,175) + rxt(k,561) &
                      *y(k,176) + rxt(k,565)*y(k,213) + rxt(k,566)*y(k,214) + rxt(k,568) &
                      *y(k,216) + rxt(k,571)*y(k,217) + rxt(k,574)*y(k,218) + rxt(k,575) &
                      *y(k,219) + rxt(k,578)*y(k,6) + rxt(k,581)*y(k,140) + rxt(k,586) &
                      *y(k,158) + rxt(k,590)*y(k,208) + rxt(k,591)*y(k,209) + rxt(k,595) &
                      *y(k,210) + rxt(k,597)*y(k,211) + rxt(k,598)*y(k,212) + (rxt(k,604) &
                      + rxt(k,618)) * y(k,83) + rxt(k,606)*y(k,169) + rxt(k,608) &
                      *y(k,184) + rxt(k,612)*y(k,181) + rxt(k,617)*y(k,183) + rxt(k,637) &
                      *y(k,150))
         mat(k,1621) = -rxt(k,209)*y(k,259)
         mat(k,700) = -rxt(k,210)*y(k,259)
         mat(k,2132) = -rxt(k,211)*y(k,259)
         mat(k,2641) = -rxt(k,212)*y(k,259)
         mat(k,2768) = -rxt(k,213)*y(k,259)
         mat(k,558) = -rxt(k,218)*y(k,259)
         mat(k,2387) = -rxt(k,231)*y(k,259)
         mat(k,1074) = -rxt(k,232)*y(k,259)
         mat(k,2699) = -rxt(k,240)*y(k,259)
         mat(k,2505) = -rxt(k,241)*y(k,259)
         mat(k,608) = -rxt(k,254)*y(k,259)
         mat(k,1132) = -rxt(k,265)*y(k,259)
         mat(k,2417) = -(rxt(k,267) + rxt(k,268)) * y(k,259)
         mat(k,1832) = -rxt(k,270)*y(k,259)
         mat(k,1809) = -rxt(k,273)*y(k,259)
         mat(k,546) = -rxt(k,285)*y(k,259)
         mat(k,2594) = -rxt(k,301)*y(k,259)
         mat(k,1594) = -rxt(k,303)*y(k,259)
         mat(k,1676) = -rxt(k,311)*y(k,259)
         mat(k,1605) = -rxt(k,314)*y(k,259)
         mat(k,2825) = -rxt(k,337)*y(k,259)
         mat(k,1333) = -rxt(k,338)*y(k,259)
         mat(k,220) = -rxt(k,356)*y(k,259)
         mat(k,292) = -rxt(k,358)*y(k,259)
         mat(k,565) = -rxt(k,360)*y(k,259)
         mat(k,147) = -rxt(k,361)*y(k,259)
         mat(k,352) = -rxt(k,363)*y(k,259)
         mat(k,709) = -rxt(k,365)*y(k,259)
         mat(k,151) = -rxt(k,366)*y(k,259)
         mat(k,447) = -rxt(k,368)*y(k,259)
         mat(k,438) = -rxt(k,370)*y(k,259)
         mat(k,119) = -rxt(k,371)*y(k,259)
         mat(k,453) = -rxt(k,373)*y(k,259)
         mat(k,123) = -rxt(k,374)*y(k,259)
         mat(k,404) = -rxt(k,382)*y(k,259)
         mat(k,262) = -rxt(k,383)*y(k,259)
         mat(k,271) = -rxt(k,384)*y(k,259)
         mat(k,316) = -rxt(k,385)*y(k,259)
         mat(k,1910) = -rxt(k,394)*y(k,259)
         mat(k,907) = -rxt(k,399)*y(k,259)
         mat(k,495) = -rxt(k,400)*y(k,259)
         mat(k,1658) = -rxt(k,401)*y(k,259)
         mat(k,213) = -rxt(k,402)*y(k,259)
         mat(k,999) = -rxt(k,403)*y(k,259)
         mat(k,1275) = -rxt(k,411)*y(k,259)
         mat(k,333) = -rxt(k,413)*y(k,259)
         mat(k,307) = -rxt(k,420)*y(k,259)
         mat(k,392) = -rxt(k,421)*y(k,259)
         mat(k,340) = -rxt(k,423)*y(k,259)
         mat(k,1268) = -rxt(k,425)*y(k,259)
         mat(k,117) = -rxt(k,426)*y(k,259)
         mat(k,797) = -rxt(k,431)*y(k,259)
         mat(k,675) = -rxt(k,432)*y(k,259)
         mat(k,1282) = -rxt(k,437)*y(k,259)
         mat(k,1127) = -rxt(k,438)*y(k,259)
         mat(k,634) = -rxt(k,439)*y(k,259)
         mat(k,651) = -rxt(k,440)*y(k,259)
         mat(k,484) = -rxt(k,448)*y(k,259)
         mat(k,346) = -rxt(k,449)*y(k,259)
         mat(k,1412) = -rxt(k,451)*y(k,259)
         mat(k,1325) = -rxt(k,453)*y(k,259)
         mat(k,984) = -rxt(k,454)*y(k,259)
         mat(k,643) = -rxt(k,457)*y(k,259)
         mat(k,478) = -rxt(k,461)*y(k,259)
         mat(k,1258) = -rxt(k,462)*y(k,259)
         mat(k,1164) = -rxt(k,463)*y(k,259)
         mat(k,420) = -rxt(k,465)*y(k,259)
         mat(k,1315) = -rxt(k,468)*y(k,259)
         mat(k,1404) = -rxt(k,476)*y(k,259)
         mat(k,359) = -rxt(k,477)*y(k,259)
         mat(k,604) = -rxt(k,486)*y(k,259)
         mat(k,364) = -rxt(k,487)*y(k,259)
         mat(k,683) = -rxt(k,488)*y(k,259)
         mat(k,1513) = -rxt(k,490)*y(k,259)
         mat(k,774) = -rxt(k,493)*y(k,259)
         mat(k,764) = -rxt(k,497)*y(k,259)
         mat(k,283) = -rxt(k,498)*y(k,259)
         mat(k,280) = -rxt(k,499)*y(k,259)
         mat(k,395) = -rxt(k,500)*y(k,259)
         mat(k,165) = -rxt(k,501)*y(k,259)
         mat(k,667) = -rxt(k,513)*y(k,259)
         mat(k,627) = -rxt(k,514)*y(k,259)
         mat(k,490) = -rxt(k,521)*y(k,259)
         mat(k,977) = -rxt(k,523)*y(k,259)
         mat(k,849) = -rxt(k,524)*y(k,259)
         mat(k,460) = -rxt(k,525)*y(k,259)
         mat(k,1198) = -rxt(k,526)*y(k,259)
         mat(k,241) = -rxt(k,532)*y(k,259)
         mat(k,195) = -rxt(k,535)*y(k,259)
         mat(k,473) = -rxt(k,538)*y(k,259)
         mat(k,286) = -rxt(k,539)*y(k,259)
         mat(k,382) = -rxt(k,541)*y(k,259)
         mat(k,311) = -rxt(k,545)*y(k,259)
         mat(k,234) = -rxt(k,546)*y(k,259)
         mat(k,204) = -rxt(k,558)*y(k,259)
         mat(k,387) = -rxt(k,561)*y(k,259)
         mat(k,696) = -rxt(k,565)*y(k,259)
         mat(k,229) = -rxt(k,566)*y(k,259)
         mat(k,253) = -rxt(k,568)*y(k,259)
         mat(k,813) = -rxt(k,571)*y(k,259)
         mat(k,258) = -rxt(k,574)*y(k,259)
         mat(k,503) = -rxt(k,575)*y(k,259)
         mat(k,1102) = -rxt(k,578)*y(k,259)
         mat(k,1058) = -rxt(k,581)*y(k,259)
         mat(k,465) = -rxt(k,586)*y(k,259)
         mat(k,793) = -rxt(k,590)*y(k,259)
         mat(k,729) = -rxt(k,591)*y(k,259)
         mat(k,575) = -rxt(k,595)*y(k,259)
         mat(k,1158) = -rxt(k,597)*y(k,259)
         mat(k,1226) = -rxt(k,598)*y(k,259)
         mat(k,1170) = -(rxt(k,604) + rxt(k,618)) * y(k,259)
         mat(k,431) = -rxt(k,606)*y(k,259)
         mat(k,1182) = -rxt(k,608)*y(k,259)
         mat(k,838) = -rxt(k,612)*y(k,259)
         mat(k,1635) = -rxt(k,617)*y(k,259)
         mat(k,110) = -rxt(k,637)*y(k,259)
         mat(k,1102) = mat(k,1102) + .630_r8*rxt(k,577)*y(k,166)
         mat(k,333) = mat(k,333) + .650_r8*rxt(k,413)*y(k,259)
         mat(k,651) = mat(k,651) + .130_r8*rxt(k,415)*y(k,166)
         mat(k,392) = mat(k,392) + .500_r8*rxt(k,421)*y(k,259)
         mat(k,1258) = mat(k,1258) + .360_r8*rxt(k,444)*y(k,166)
         mat(k,1910) = mat(k,1910) + rxt(k,393)*y(k,164)
         mat(k,495) = mat(k,495) + .300_r8*rxt(k,400)*y(k,259)
         mat(k,1658) = mat(k,1658) + rxt(k,407)*y(k,255)
         mat(k,2479) = rxt(k,252)*y(k,107)
         mat(k,991) = rxt(k,351)*y(k,272)
         mat(k,2792) = 2.000_r8*rxt(k,203)*y(k,107) + rxt(k,208)*y(k,166)
         mat(k,1621) = mat(k,1621) + rxt(k,200)*y(k,164) + rxt(k,182)*y(k,255)
         mat(k,700) = mat(k,700) + rxt(k,201)*y(k,164)
         mat(k,1594) = mat(k,1594) + rxt(k,302)*y(k,164) + rxt(k,308)*y(k,255)
         mat(k,1832) = mat(k,1832) + rxt(k,269)*y(k,164) + rxt(k,281)*y(k,255)
         mat(k,213) = mat(k,213) + rxt(k,410)*y(k,255)
         mat(k,2132) = mat(k,2132) + rxt(k,252)*y(k,70) + 2.000_r8*rxt(k,203)*y(k,92) &
                      + rxt(k,233)*y(k,154) + rxt(k,228)*y(k,156) + rxt(k,206) &
                      *y(k,164) + rxt(k,207)*y(k,166) + .400_r8*rxt(k,533)*y(k,222) &
                      + .490_r8*rxt(k,429)*y(k,231) + .400_r8*rxt(k,547)*y(k,233) &
                      + .450_r8*rxt(k,480)*y(k,246) + .400_r8*rxt(k,553)*y(k,247) &
                      + .200_r8*rxt(k,484)*y(k,248) + .150_r8*rxt(k,459)*y(k,263)
         mat(k,1699) = rxt(k,304)*y(k,164)
         mat(k,1809) = mat(k,1809) + rxt(k,272)*y(k,164)
         mat(k,977) = mat(k,977) + .320_r8*rxt(k,522)*y(k,166)
         mat(k,849) = mat(k,849) + .600_r8*rxt(k,524)*y(k,259)
         mat(k,1404) = mat(k,1404) + .240_r8*rxt(k,475)*y(k,166)
         mat(k,359) = mat(k,359) + .100_r8*rxt(k,477)*y(k,259)
         mat(k,1058) = mat(k,1058) + .630_r8*rxt(k,580)*y(k,166)
         mat(k,1513) = mat(k,1513) + .360_r8*rxt(k,489)*y(k,166)
         mat(k,2016) = rxt(k,233)*y(k,107)
         mat(k,2387) = mat(k,2387) + rxt(k,228)*y(k,107)
         mat(k,2641) = mat(k,2641) + rxt(k,393)*y(k,51) + rxt(k,200)*y(k,93) &
                      + rxt(k,201)*y(k,95) + rxt(k,302)*y(k,97) + rxt(k,269)*y(k,101) &
                      + rxt(k,206)*y(k,107) + rxt(k,304)*y(k,109) + rxt(k,272) &
                      *y(k,110)
         mat(k,2768) = mat(k,2768) + .630_r8*rxt(k,577)*y(k,6) + .130_r8*rxt(k,415) &
                      *y(k,28) + .360_r8*rxt(k,444)*y(k,33) + rxt(k,208)*y(k,92) &
                      + rxt(k,207)*y(k,107) + .320_r8*rxt(k,522)*y(k,128) &
                      + .240_r8*rxt(k,475)*y(k,135) + .630_r8*rxt(k,580)*y(k,140) &
                      + .360_r8*rxt(k,489)*y(k,141)
         mat(k,643) = mat(k,643) + .500_r8*rxt(k,457)*y(k,259)
         mat(k,241) = mat(k,241) + .500_r8*rxt(k,532)*y(k,259)
         mat(k,619) = .400_r8*rxt(k,533)*y(k,107)
         mat(k,1563) = .490_r8*rxt(k,429)*y(k,107)
         mat(k,873) = .400_r8*rxt(k,547)*y(k,107)
         mat(k,1531) = .450_r8*rxt(k,480)*y(k,107)
         mat(k,1020) = .400_r8*rxt(k,553)*y(k,107)
         mat(k,782) = .200_r8*rxt(k,484)*y(k,107)
         mat(k,1879) = rxt(k,407)*y(k,64) + rxt(k,182)*y(k,93) + rxt(k,308)*y(k,97) &
                      + rxt(k,281)*y(k,101) + rxt(k,410)*y(k,102) &
                      + 2.000_r8*rxt(k,183)*y(k,272)
         mat(k,2315) = mat(k,2315) + .650_r8*rxt(k,413)*y(k,27) + .500_r8*rxt(k,421) &
                      *y(k,31) + .300_r8*rxt(k,400)*y(k,63) + .600_r8*rxt(k,524) &
                      *y(k,132) + .100_r8*rxt(k,477)*y(k,136) + .500_r8*rxt(k,457) &
                      *y(k,179) + .500_r8*rxt(k,532)*y(k,215)
         mat(k,1351) = .150_r8*rxt(k,459)*y(k,107)
         mat(k,2855) = rxt(k,351)*y(k,89) + 2.000_r8*rxt(k,183)*y(k,255)
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
         mat(k,885) = -(rxt(k,752)*y(k,78) + rxt(k,754)*y(k,165))
         mat(k,1571) = -rxt(k,752)*y(k,260)
         mat(k,1773) = -rxt(k,754)*y(k,260)
         mat(k,2616) = rxt(k,745)*y(k,251) + rxt(k,746)*y(k,253)
         mat(k,715) = rxt(k,745)*y(k,164)
         mat(k,586) = rxt(k,746)*y(k,164)
         mat(k,525) = -(rxt(k,556)*y(k,107) + rxt(k,557)*y(k,154))
         mat(k,2060) = -rxt(k,556)*y(k,261)
         mat(k,1944) = -rxt(k,557)*y(k,261)
         mat(k,231) = .200_r8*rxt(k,546)*y(k,259)
         mat(k,201) = .140_r8*rxt(k,558)*y(k,259)
         mat(k,384) = rxt(k,561)*y(k,259)
         mat(k,2217) = .200_r8*rxt(k,546)*y(k,82) + .140_r8*rxt(k,558)*y(k,175) &
                      + rxt(k,561)*y(k,176)
         mat(k,894) = -(rxt(k,455)*y(k,107) + rxt(k,456)*y(k,154))
         mat(k,2086) = -rxt(k,455)*y(k,262)
         mat(k,1967) = -rxt(k,456)*y(k,262)
         mat(k,1242) = rxt(k,462)*y(k,259)
         mat(k,638) = .500_r8*rxt(k,457)*y(k,259)
         mat(k,2256) = rxt(k,462)*y(k,33) + .500_r8*rxt(k,457)*y(k,179)
         mat(k,1344) = -(rxt(k,458)*y(k,232) + rxt(k,459)*y(k,107) + rxt(k,460) &
                      *y(k,154))
         mat(k,1735) = -rxt(k,458)*y(k,263)
         mat(k,2106) = -rxt(k,459)*y(k,263)
         mat(k,1992) = -rxt(k,460)*y(k,263)
         mat(k,1096) = .060_r8*rxt(k,577)*y(k,166)
         mat(k,1161) = rxt(k,463)*y(k,259)
         mat(k,1052) = .060_r8*rxt(k,580)*y(k,166)
         mat(k,2747) = .060_r8*rxt(k,577)*y(k,6) + .060_r8*rxt(k,580)*y(k,140)
         mat(k,475) = rxt(k,461)*y(k,259)
         mat(k,1220) = .150_r8*rxt(k,598)*y(k,259)
         mat(k,2288) = rxt(k,463)*y(k,57) + rxt(k,461)*y(k,180) + .150_r8*rxt(k,598) &
                      *y(k,212)
         mat(k,1290) = -(rxt(k,587)*y(k,232) + rxt(k,588)*y(k,107) + rxt(k,589) &
                      *y(k,154))
         mat(k,1733) = -rxt(k,587)*y(k,264)
         mat(k,2103) = -rxt(k,588)*y(k,264)
         mat(k,1989) = -rxt(k,589)*y(k,264)
         mat(k,2358) = .500_r8*rxt(k,596)*y(k,211)
         mat(k,789) = rxt(k,590)*y(k,259)
         mat(k,1156) = .500_r8*rxt(k,596)*y(k,156) + rxt(k,597)*y(k,259)
         mat(k,2284) = rxt(k,590)*y(k,208) + rxt(k,597)*y(k,211)
         mat(k,1142) = -(rxt(k,592)*y(k,232) + rxt(k,593)*y(k,107) + rxt(k,594) &
                      *y(k,154))
         mat(k,1723) = -rxt(k,592)*y(k,265)
         mat(k,2094) = -rxt(k,593)*y(k,265)
         mat(k,1979) = -rxt(k,594)*y(k,265)
         mat(k,1090) = rxt(k,578)*y(k,259)
         mat(k,1046) = rxt(k,581)*y(k,259)
         mat(k,570) = rxt(k,595)*y(k,259)
         mat(k,2271) = rxt(k,578)*y(k,6) + rxt(k,581)*y(k,140) + rxt(k,595)*y(k,210)
         mat(k,825) = -(rxt(k,563)*y(k,107) + rxt(k,564)*y(k,154))
         mat(k,2081) = -rxt(k,563)*y(k,266)
         mat(k,1963) = -rxt(k,564)*y(k,266)
         mat(k,691) = rxt(k,565)*y(k,259)
         mat(k,227) = .650_r8*rxt(k,566)*y(k,259)
         mat(k,2250) = rxt(k,565)*y(k,213) + .650_r8*rxt(k,566)*y(k,214)
         mat(k,97) = -(rxt(k,692)*y(k,107) + rxt(k,693)*y(k,154))
         mat(k,2040) = -rxt(k,692)*y(k,267)
         mat(k,1934) = -rxt(k,693)*y(k,267)
         mat(k,222) = rxt(k,691)*y(k,259)
         mat(k,2158) = rxt(k,691)*y(k,214)
         mat(k,1360) = -(rxt(k,527)*y(k,231) + rxt(k,528)*y(k,232) + rxt(k,529) &
                      *y(k,107) + rxt(k,530)*y(k,154) + rxt(k,531)*y(k,156))
         mat(k,1546) = -rxt(k,527)*y(k,268)
         mat(k,1736) = -rxt(k,528)*y(k,268)
         mat(k,2107) = -rxt(k,529)*y(k,268)
         mat(k,1993) = -rxt(k,530)*y(k,268)
         mat(k,2363) = -rxt(k,531)*y(k,268)
         mat(k,278) = rxt(k,499)*y(k,259)
         mat(k,394) = rxt(k,500)*y(k,259)
         mat(k,164) = rxt(k,501)*y(k,259)
         mat(k,844) = .400_r8*rxt(k,524)*y(k,259)
         mat(k,240) = .500_r8*rxt(k,532)*y(k,259)
         mat(k,2289) = rxt(k,499)*y(k,113) + rxt(k,500)*y(k,115) + rxt(k,501)*y(k,123) &
                      + .400_r8*rxt(k,524)*y(k,132) + .500_r8*rxt(k,532)*y(k,215)
         mat(k,856) = -(rxt(k,569)*y(k,107) + rxt(k,570)*y(k,154))
         mat(k,2083) = -rxt(k,569)*y(k,269)
         mat(k,1964) = -rxt(k,570)*y(k,269)
         mat(k,249) = .560_r8*rxt(k,568)*y(k,259)
         mat(k,805) = rxt(k,571)*y(k,259)
         mat(k,2253) = .560_r8*rxt(k,568)*y(k,216) + rxt(k,571)*y(k,217)
         mat(k,103) = -(rxt(k,696)*y(k,107) + rxt(k,697)*y(k,154))
         mat(k,2041) = -rxt(k,696)*y(k,270)
         mat(k,1935) = -rxt(k,697)*y(k,270)
         mat(k,244) = rxt(k,695)*y(k,259)
         mat(k,2159) = rxt(k,695)*y(k,216)
         mat(k,593) = -(rxt(k,572)*y(k,107) + rxt(k,573)*y(k,154))
         mat(k,2067) = -rxt(k,572)*y(k,271)
         mat(k,1949) = -rxt(k,573)*y(k,271)
         mat(k,256) = .300_r8*rxt(k,574)*y(k,259)
         mat(k,499) = rxt(k,575)*y(k,259)
         mat(k,2225) = .300_r8*rxt(k,574)*y(k,218) + rxt(k,575)*y(k,219)
         mat(k,2868) = -(rxt(k,183)*y(k,255) + rxt(k,351)*y(k,89) + rxt(k,619) &
                      *y(k,185))
         mat(k,1892) = -rxt(k,183)*y(k,272)
         mat(k,995) = -rxt(k,351)*y(k,272)
         mat(k,303) = -rxt(k,619)*y(k,272)
         mat(k,294) = rxt(k,358)*y(k,259)
         mat(k,342) = rxt(k,423)*y(k,259)
         mat(k,485) = rxt(k,448)*y(k,259)
         mat(k,348) = rxt(k,449)*y(k,259)
         mat(k,568) = rxt(k,360)*y(k,259)
         mat(k,354) = rxt(k,363)*y(k,259)
         mat(k,1923) = rxt(k,394)*y(k,259)
         mat(k,712) = rxt(k,365)*y(k,259)
         mat(k,153) = rxt(k,366)*y(k,259)
         mat(k,1271) = rxt(k,425)*y(k,259)
         mat(k,449) = rxt(k,368)*y(k,259)
         mat(k,1165) = rxt(k,463)*y(k,259)
         mat(k,1415) = rxt(k,451)*y(k,259)
         mat(k,798) = rxt(k,431)*y(k,259)
         mat(k,676) = rxt(k,432)*y(k,259)
         mat(k,441) = rxt(k,370)*y(k,259)
         mat(k,497) = rxt(k,400)*y(k,259)
         mat(k,1665) = rxt(k,401)*y(k,259)
         mat(k,1240) = rxt(k,377)*y(k,107)
         mat(k,406) = rxt(k,382)*y(k,259)
         mat(k,2805) = rxt(k,204)*y(k,107)
         mat(k,1627) = rxt(k,209)*y(k,259)
         mat(k,703) = rxt(k,210)*y(k,259)
         mat(k,1601) = (rxt(k,627)+rxt(k,701)+rxt(k,714)+rxt(k,723))*y(k,109) + ( &
                      + rxt(k,626)+rxt(k,703)+rxt(k,711)+rxt(k,720))*y(k,110) + ( &
                      + rxt(k,634)+rxt(k,730)+rxt(k,734)+rxt(k,738))*y(k,111) &
                      + rxt(k,303)*y(k,259)
         mat(k,318) = rxt(k,385)*y(k,259)
         mat(k,1844) = (rxt(k,629)+rxt(k,700)+rxt(k,713)+rxt(k,722))*y(k,109) + ( &
                      + rxt(k,628)+rxt(k,699)+rxt(k,710)+rxt(k,719))*y(k,110) + ( &
                      + rxt(k,633)+rxt(k,729)+rxt(k,733)+rxt(k,737))*y(k,111) &
                      + rxt(k,270)*y(k,259)
         mat(k,1000) = rxt(k,403)*y(k,259)
         mat(k,1340) = (rxt(k,631)+rxt(k,702)+rxt(k,715)+rxt(k,724))*y(k,109) + ( &
                      + rxt(k,630)+rxt(k,704)+rxt(k,712)+rxt(k,721))*y(k,110) + ( &
                      + rxt(k,635)+rxt(k,731)+rxt(k,735)+rxt(k,739))*y(k,111) &
                      + rxt(k,338)*y(k,259)
         mat(k,2518) = rxt(k,241)*y(k,259)
         mat(k,2145) = rxt(k,377)*y(k,68) + rxt(k,204)*y(k,92) + rxt(k,211)*y(k,259)
         mat(k,561) = rxt(k,218)*y(k,259)
         mat(k,1710) = (rxt(k,627)+rxt(k,701)+rxt(k,714)+rxt(k,723))*y(k,97) + ( &
                      + rxt(k,629)+rxt(k,700)+rxt(k,713)+rxt(k,722))*y(k,101) + ( &
                      + rxt(k,631)+rxt(k,702)+rxt(k,715)+rxt(k,724))*y(k,105)
         mat(k,1821) = (rxt(k,626)+rxt(k,703)+rxt(k,711)+rxt(k,720))*y(k,97) + ( &
                      + rxt(k,628)+rxt(k,699)+rxt(k,710)+rxt(k,719))*y(k,101) + ( &
                      + rxt(k,630)+rxt(k,704)+rxt(k,712)+rxt(k,721))*y(k,105) &
                      + rxt(k,273)*y(k,259)
         mat(k,1687) = (rxt(k,634)+rxt(k,730)+rxt(k,734)+rxt(k,738))*y(k,97) + ( &
                      + rxt(k,633)+rxt(k,729)+rxt(k,733)+rxt(k,737))*y(k,101) + ( &
                      + rxt(k,635)+rxt(k,731)+rxt(k,735)+rxt(k,739))*y(k,105) &
                      + rxt(k,311)*y(k,259)
         mat(k,1406) = .500_r8*rxt(k,476)*y(k,259)
         mat(k,111) = rxt(k,637)*y(k,259)
         mat(k,644) = rxt(k,457)*y(k,259)
         mat(k,479) = rxt(k,461)*y(k,259)
         mat(k,2328) = rxt(k,358)*y(k,29) + rxt(k,423)*y(k,32) + rxt(k,448)*y(k,34) &
                      + rxt(k,449)*y(k,35) + rxt(k,360)*y(k,45) + rxt(k,363)*y(k,47) &
                      + rxt(k,394)*y(k,51) + rxt(k,365)*y(k,52) + rxt(k,366)*y(k,53) &
                      + rxt(k,425)*y(k,54) + rxt(k,368)*y(k,55) + rxt(k,463)*y(k,57) &
                      + rxt(k,451)*y(k,58) + rxt(k,431)*y(k,59) + rxt(k,432)*y(k,60) &
                      + rxt(k,370)*y(k,61) + rxt(k,400)*y(k,63) + rxt(k,401)*y(k,64) &
                      + rxt(k,382)*y(k,69) + rxt(k,209)*y(k,93) + rxt(k,210)*y(k,95) &
                      + rxt(k,303)*y(k,97) + rxt(k,385)*y(k,100) + rxt(k,270)*y(k,101) &
                      + rxt(k,403)*y(k,103) + rxt(k,338)*y(k,105) + rxt(k,241) &
                      *y(k,106) + rxt(k,211)*y(k,107) + rxt(k,218)*y(k,108) &
                      + rxt(k,273)*y(k,110) + rxt(k,311)*y(k,111) + .500_r8*rxt(k,476) &
                      *y(k,135) + rxt(k,637)*y(k,150) + rxt(k,457)*y(k,179) &
                      + rxt(k,461)*y(k,180) + 2.000_r8*rxt(k,214)*y(k,259)
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
         mat(k, 59) = mat(k, 59) + lmat(k, 59)
         mat(k, 65) = mat(k, 65) + lmat(k, 65)
         mat(k, 71) = mat(k, 71) + lmat(k, 71)
         mat(k, 77) = mat(k, 77) + lmat(k, 77)
         mat(k, 83) = mat(k, 83) + lmat(k, 83)
         mat(k, 85) = mat(k, 85) + lmat(k, 85)
         mat(k, 91) = mat(k, 91) + lmat(k, 91)
         mat(k, 97) = mat(k, 97) + lmat(k, 97)
         mat(k, 103) = mat(k, 103) + lmat(k, 103)
         mat(k, 104) = lmat(k, 104)
         mat(k, 105) = lmat(k, 105)
         mat(k, 106) = lmat(k, 106)
         mat(k, 107) = lmat(k, 107)
         mat(k, 108) = lmat(k, 108)
         mat(k, 109) = mat(k, 109) + lmat(k, 109)
         mat(k, 112) = mat(k, 112) + lmat(k, 112)
         mat(k, 114) = mat(k, 114) + lmat(k, 114)
         mat(k, 115) = mat(k, 115) + lmat(k, 115)
         mat(k, 118) = mat(k, 118) + lmat(k, 118)
         mat(k, 120) = mat(k, 120) + lmat(k, 120)
         mat(k, 121) = mat(k, 121) + lmat(k, 121)
         mat(k, 122) = mat(k, 122) + lmat(k, 122)
         mat(k, 124) = mat(k, 124) + lmat(k, 124)
         mat(k, 125) = mat(k, 125) + lmat(k, 125)
         mat(k, 126) = mat(k, 126) + lmat(k, 126)
         mat(k, 127) = mat(k, 127) + lmat(k, 127)
         mat(k, 129) = mat(k, 129) + lmat(k, 129)
         mat(k, 130) = lmat(k, 130)
         mat(k, 131) = lmat(k, 131)
         mat(k, 132) = lmat(k, 132)
         mat(k, 133) = lmat(k, 133)
         mat(k, 134) = mat(k, 134) + lmat(k, 134)
         mat(k, 135) = mat(k, 135) + lmat(k, 135)
         mat(k, 137) = mat(k, 137) + lmat(k, 137)
         mat(k, 138) = mat(k, 138) + lmat(k, 138)
         mat(k, 139) = mat(k, 139) + lmat(k, 139)
         mat(k, 141) = mat(k, 141) + lmat(k, 141)
         mat(k, 142) = mat(k, 142) + lmat(k, 142)
         mat(k, 143) = mat(k, 143) + lmat(k, 143)
         mat(k, 145) = mat(k, 145) + lmat(k, 145)
         mat(k, 146) = mat(k, 146) + lmat(k, 146)
         mat(k, 148) = mat(k, 148) + lmat(k, 148)
         mat(k, 149) = mat(k, 149) + lmat(k, 149)
         mat(k, 150) = mat(k, 150) + lmat(k, 150)
         mat(k, 152) = mat(k, 152) + lmat(k, 152)
         mat(k, 154) = lmat(k, 154)
         mat(k, 155) = lmat(k, 155)
         mat(k, 156) = lmat(k, 156)
         mat(k, 157) = lmat(k, 157)
         mat(k, 158) = lmat(k, 158)
         mat(k, 159) = lmat(k, 159)
         mat(k, 160) = lmat(k, 160)
         mat(k, 161) = lmat(k, 161)
         mat(k, 162) = lmat(k, 162)
         mat(k, 163) = mat(k, 163) + lmat(k, 163)
         mat(k, 166) = lmat(k, 166)
         mat(k, 167) = lmat(k, 167)
         mat(k, 168) = lmat(k, 168)
         mat(k, 169) = mat(k, 169) + lmat(k, 169)
         mat(k, 170) = mat(k, 170) + lmat(k, 170)
         mat(k, 172) = mat(k, 172) + lmat(k, 172)
         mat(k, 173) = mat(k, 173) + lmat(k, 173)
         mat(k, 174) = mat(k, 174) + lmat(k, 174)
         mat(k, 175) = mat(k, 175) + lmat(k, 175)
         mat(k, 176) = mat(k, 176) + lmat(k, 176)
         mat(k, 178) = mat(k, 178) + lmat(k, 178)
         mat(k, 179) = mat(k, 179) + lmat(k, 179)
         mat(k, 180) = mat(k, 180) + lmat(k, 180)
         mat(k, 181) = mat(k, 181) + lmat(k, 181)
         mat(k, 183) = mat(k, 183) + lmat(k, 183)
         mat(k, 184) = mat(k, 184) + lmat(k, 184)
         mat(k, 185) = mat(k, 185) + lmat(k, 185)
         mat(k, 186) = mat(k, 186) + lmat(k, 186)
         mat(k, 188) = mat(k, 188) + lmat(k, 188)
         mat(k, 190) = mat(k, 190) + lmat(k, 190)
         mat(k, 196) = lmat(k, 196)
         mat(k, 197) = lmat(k, 197)
         mat(k, 198) = lmat(k, 198)
         mat(k, 199) = lmat(k, 199)
         mat(k, 200) = mat(k, 200) + lmat(k, 200)
         mat(k, 205) = mat(k, 205) + lmat(k, 205)
         mat(k, 206) = mat(k, 206) + lmat(k, 206)
         mat(k, 207) = mat(k, 207) + lmat(k, 207)
         mat(k, 208) = mat(k, 208) + lmat(k, 208)
         mat(k, 209) = lmat(k, 209)
         mat(k, 210) = mat(k, 210) + lmat(k, 210)
         mat(k, 214) = mat(k, 214) + lmat(k, 214)
         mat(k, 216) = mat(k, 216) + lmat(k, 216)
         mat(k, 217) = lmat(k, 217)
         mat(k, 218) = mat(k, 218) + lmat(k, 218)
         mat(k, 221) = mat(k, 221) + lmat(k, 221)
         mat(k, 223) = mat(k, 223) + lmat(k, 223)
         mat(k, 230) = mat(k, 230) + lmat(k, 230)
         mat(k, 235) = lmat(k, 235)
         mat(k, 236) = lmat(k, 236)
         mat(k, 237) = lmat(k, 237)
         mat(k, 238) = lmat(k, 238)
         mat(k, 239) = mat(k, 239) + lmat(k, 239)
         mat(k, 241) = mat(k, 241) + lmat(k, 241)
         mat(k, 242) = lmat(k, 242)
         mat(k, 243) = lmat(k, 243)
         mat(k, 246) = mat(k, 246) + lmat(k, 246)
         mat(k, 254) = mat(k, 254) + lmat(k, 254)
         mat(k, 259) = mat(k, 259) + lmat(k, 259)
         mat(k, 260) = mat(k, 260) + lmat(k, 260)
         mat(k, 263) = mat(k, 263) + lmat(k, 263)
         mat(k, 264) = mat(k, 264) + lmat(k, 264)
         mat(k, 265) = mat(k, 265) + lmat(k, 265)
         mat(k, 267) = mat(k, 267) + lmat(k, 267)
         mat(k, 268) = mat(k, 268) + lmat(k, 268)
         mat(k, 269) = mat(k, 269) + lmat(k, 269)
         mat(k, 272) = mat(k, 272) + lmat(k, 272)
         mat(k, 273) = mat(k, 273) + lmat(k, 273)
         mat(k, 274) = mat(k, 274) + lmat(k, 274)
         mat(k, 276) = mat(k, 276) + lmat(k, 276)
         mat(k, 277) = lmat(k, 277)
         mat(k, 279) = lmat(k, 279)
         mat(k, 280) = mat(k, 280) + lmat(k, 280)
         mat(k, 281) = mat(k, 281) + lmat(k, 281)
         mat(k, 284) = mat(k, 284) + lmat(k, 284)
         mat(k, 287) = lmat(k, 287)
         mat(k, 288) = lmat(k, 288)
         mat(k, 289) = lmat(k, 289)
         mat(k, 290) = mat(k, 290) + lmat(k, 290)
         mat(k, 293) = mat(k, 293) + lmat(k, 293)
         mat(k, 295) = mat(k, 295) + lmat(k, 295)
         mat(k, 297) = mat(k, 297) + lmat(k, 297)
         mat(k, 300) = mat(k, 300) + lmat(k, 300)
         mat(k, 301) = lmat(k, 301)
         mat(k, 302) = lmat(k, 302)
         mat(k, 304) = mat(k, 304) + lmat(k, 304)
         mat(k, 308) = mat(k, 308) + lmat(k, 308)
         mat(k, 309) = lmat(k, 309)
         mat(k, 311) = mat(k, 311) + lmat(k, 311)
         mat(k, 312) = mat(k, 312) + lmat(k, 312)
         mat(k, 313) = mat(k, 313) + lmat(k, 313)
         mat(k, 317) = mat(k, 317) + lmat(k, 317)
         mat(k, 319) = lmat(k, 319)
         mat(k, 320) = lmat(k, 320)
         mat(k, 321) = lmat(k, 321)
         mat(k, 322) = mat(k, 322) + lmat(k, 322)
         mat(k, 323) = lmat(k, 323)
         mat(k, 324) = lmat(k, 324)
         mat(k, 326) = lmat(k, 326)
         mat(k, 327) = mat(k, 327) + lmat(k, 327)
         mat(k, 328) = mat(k, 328) + lmat(k, 328)
         mat(k, 334) = lmat(k, 334)
         mat(k, 335) = lmat(k, 335)
         mat(k, 336) = lmat(k, 336)
         mat(k, 337) = mat(k, 337) + lmat(k, 337)
         mat(k, 343) = mat(k, 343) + lmat(k, 343)
         mat(k, 349) = mat(k, 349) + lmat(k, 349)
         mat(k, 353) = mat(k, 353) + lmat(k, 353)
         mat(k, 355) = mat(k, 355) + lmat(k, 355)
         mat(k, 360) = mat(k, 360) + lmat(k, 360)
         mat(k, 362) = lmat(k, 362)
         mat(k, 363) = lmat(k, 363)
         mat(k, 364) = mat(k, 364) + lmat(k, 364)
         mat(k, 365) = lmat(k, 365)
         mat(k, 366) = lmat(k, 366)
         mat(k, 367) = lmat(k, 367)
         mat(k, 368) = lmat(k, 368)
         mat(k, 369) = lmat(k, 369)
         mat(k, 370) = lmat(k, 370)
         mat(k, 371) = lmat(k, 371)
         mat(k, 372) = lmat(k, 372)
         mat(k, 373) = lmat(k, 373)
         mat(k, 374) = lmat(k, 374)
         mat(k, 375) = lmat(k, 375)
         mat(k, 376) = lmat(k, 376)
         mat(k, 377) = lmat(k, 377)
         mat(k, 378) = mat(k, 378) + lmat(k, 378)
         mat(k, 381) = lmat(k, 381)
         mat(k, 382) = mat(k, 382) + lmat(k, 382)
         mat(k, 383) = mat(k, 383) + lmat(k, 383)
         mat(k, 385) = lmat(k, 385)
         mat(k, 386) = lmat(k, 386)
         mat(k, 387) = mat(k, 387) + lmat(k, 387)
         mat(k, 388) = mat(k, 388) + lmat(k, 388)
         mat(k, 390) = mat(k, 390) + lmat(k, 390)
         mat(k, 391) = lmat(k, 391)
         mat(k, 392) = mat(k, 392) + lmat(k, 392)
         mat(k, 393) = mat(k, 393) + lmat(k, 393)
         mat(k, 396) = lmat(k, 396)
         mat(k, 397) = lmat(k, 397)
         mat(k, 398) = lmat(k, 398)
         mat(k, 400) = mat(k, 400) + lmat(k, 400)
         mat(k, 401) = lmat(k, 401)
         mat(k, 405) = mat(k, 405) + lmat(k, 405)
         mat(k, 407) = lmat(k, 407)
         mat(k, 408) = mat(k, 408) + lmat(k, 408)
         mat(k, 410) = lmat(k, 410)
         mat(k, 411) = lmat(k, 411)
         mat(k, 412) = mat(k, 412) + lmat(k, 412)
         mat(k, 413) = lmat(k, 413)
         mat(k, 414) = lmat(k, 414)
         mat(k, 415) = mat(k, 415) + lmat(k, 415)
         mat(k, 423) = lmat(k, 423)
         mat(k, 424) = lmat(k, 424)
         mat(k, 425) = lmat(k, 425)
         mat(k, 426) = mat(k, 426) + lmat(k, 426)
         mat(k, 427) = lmat(k, 427)
         mat(k, 429) = mat(k, 429) + lmat(k, 429)
         mat(k, 434) = mat(k, 434) + lmat(k, 434)
         mat(k, 435) = lmat(k, 435)
         mat(k, 440) = mat(k, 440) + lmat(k, 440)
         mat(k, 442) = mat(k, 442) + lmat(k, 442)
         mat(k, 444) = lmat(k, 444)
         mat(k, 448) = mat(k, 448) + lmat(k, 448)
         mat(k, 450) = mat(k, 450) + lmat(k, 450)
         mat(k, 455) = mat(k, 455) + lmat(k, 455)
         mat(k, 456) = mat(k, 456) + lmat(k, 456)
         mat(k, 461) = lmat(k, 461)
         mat(k, 462) = mat(k, 462) + lmat(k, 462)
         mat(k, 463) = lmat(k, 463)
         mat(k, 465) = mat(k, 465) + lmat(k, 465)
         mat(k, 466) = lmat(k, 466)
         mat(k, 467) = lmat(k, 467)
         mat(k, 468) = mat(k, 468) + lmat(k, 468)
         mat(k, 469) = lmat(k, 469)
         mat(k, 471) = lmat(k, 471)
         mat(k, 472) = lmat(k, 472)
         mat(k, 473) = mat(k, 473) + lmat(k, 473)
         mat(k, 474) = mat(k, 474) + lmat(k, 474)
         mat(k, 476) = lmat(k, 476)
         mat(k, 477) = lmat(k, 477)
         mat(k, 478) = mat(k, 478) + lmat(k, 478)
         mat(k, 480) = mat(k, 480) + lmat(k, 480)
         mat(k, 482) = lmat(k, 482)
         mat(k, 483) = lmat(k, 483)
         mat(k, 484) = mat(k, 484) + lmat(k, 484)
         mat(k, 486) = mat(k, 486) + lmat(k, 486)
         mat(k, 487) = lmat(k, 487)
         mat(k, 489) = mat(k, 489) + lmat(k, 489)
         mat(k, 491) = lmat(k, 491)
         mat(k, 492) = mat(k, 492) + lmat(k, 492)
         mat(k, 494) = mat(k, 494) + lmat(k, 494)
         mat(k, 495) = mat(k, 495) + lmat(k, 495)
         mat(k, 496) = lmat(k, 496)
         mat(k, 498) = mat(k, 498) + lmat(k, 498)
         mat(k, 500) = lmat(k, 500)
         mat(k, 501) = lmat(k, 501)
         mat(k, 502) = lmat(k, 502)
         mat(k, 503) = mat(k, 503) + lmat(k, 503)
         mat(k, 506) = mat(k, 506) + lmat(k, 506)
         mat(k, 512) = mat(k, 512) + lmat(k, 512)
         mat(k, 514) = lmat(k, 514)
         mat(k, 516) = mat(k, 516) + lmat(k, 516)
         mat(k, 518) = lmat(k, 518)
         mat(k, 519) = lmat(k, 519)
         mat(k, 520) = lmat(k, 520)
         mat(k, 521) = lmat(k, 521)
         mat(k, 522) = lmat(k, 522)
         mat(k, 523) = lmat(k, 523)
         mat(k, 525) = mat(k, 525) + lmat(k, 525)
         mat(k, 531) = lmat(k, 531)
         mat(k, 532) = lmat(k, 532)
         mat(k, 533) = lmat(k, 533)
         mat(k, 534) = mat(k, 534) + lmat(k, 534)
         mat(k, 537) = mat(k, 537) + lmat(k, 537)
         mat(k, 538) = lmat(k, 538)
         mat(k, 539) = mat(k, 539) + lmat(k, 539)
         mat(k, 542) = lmat(k, 542)
         mat(k, 543) = mat(k, 543) + lmat(k, 543)
         mat(k, 544) = mat(k, 544) + lmat(k, 544)
         mat(k, 547) = mat(k, 547) + lmat(k, 547)
         mat(k, 548) = mat(k, 548) + lmat(k, 548)
         mat(k, 552) = lmat(k, 552)
         mat(k, 553) = lmat(k, 553)
         mat(k, 554) = lmat(k, 554)
         mat(k, 555) = mat(k, 555) + lmat(k, 555)
         mat(k, 557) = lmat(k, 557)
         mat(k, 558) = mat(k, 558) + lmat(k, 558)
         mat(k, 559) = lmat(k, 559)
         mat(k, 560) = mat(k, 560) + lmat(k, 560)
         mat(k, 562) = mat(k, 562) + lmat(k, 562)
         mat(k, 567) = mat(k, 567) + lmat(k, 567)
         mat(k, 569) = mat(k, 569) + lmat(k, 569)
         mat(k, 571) = lmat(k, 571)
         mat(k, 572) = lmat(k, 572)
         mat(k, 573) = lmat(k, 573)
         mat(k, 574) = lmat(k, 574)
         mat(k, 575) = mat(k, 575) + lmat(k, 575)
         mat(k, 578) = mat(k, 578) + lmat(k, 578)
         mat(k, 585) = mat(k, 585) + lmat(k, 585)
         mat(k, 593) = mat(k, 593) + lmat(k, 593)
         mat(k, 600) = mat(k, 600) + lmat(k, 600)
         mat(k, 602) = lmat(k, 602)
         mat(k, 603) = lmat(k, 603)
         mat(k, 605) = lmat(k, 605)
         mat(k, 606) = mat(k, 606) + lmat(k, 606)
         mat(k, 609) = lmat(k, 609)
         mat(k, 610) = mat(k, 610) + lmat(k, 610)
         mat(k, 611) = lmat(k, 611)
         mat(k, 612) = lmat(k, 612)
         mat(k, 613) = lmat(k, 613)
         mat(k, 615) = mat(k, 615) + lmat(k, 615)
         mat(k, 621) = mat(k, 621) + lmat(k, 621)
         mat(k, 628) = lmat(k, 628)
         mat(k, 629) = mat(k, 629) + lmat(k, 629)
         mat(k, 630) = lmat(k, 630)
         mat(k, 631) = lmat(k, 631)
         mat(k, 632) = lmat(k, 632)
         mat(k, 635) = mat(k, 635) + lmat(k, 635)
         mat(k, 636) = lmat(k, 636)
         mat(k, 637) = mat(k, 637) + lmat(k, 637)
         mat(k, 639) = lmat(k, 639)
         mat(k, 641) = lmat(k, 641)
         mat(k, 642) = lmat(k, 642)
         mat(k, 643) = mat(k, 643) + lmat(k, 643)
         mat(k, 645) = mat(k, 645) + lmat(k, 645)
         mat(k, 653) = mat(k, 653) + lmat(k, 653)
         mat(k, 661) = mat(k, 661) + lmat(k, 661)
         mat(k, 668) = lmat(k, 668)
         mat(k, 670) = mat(k, 670) + lmat(k, 670)
         mat(k, 672) = mat(k, 672) + lmat(k, 672)
         mat(k, 673) = lmat(k, 673)
         mat(k, 675) = mat(k, 675) + lmat(k, 675)
         mat(k, 677) = mat(k, 677) + lmat(k, 677)
         mat(k, 679) = lmat(k, 679)
         mat(k, 685) = lmat(k, 685)
         mat(k, 686) = lmat(k, 686)
         mat(k, 687) = lmat(k, 687)
         mat(k, 688) = mat(k, 688) + lmat(k, 688)
         mat(k, 689) = lmat(k, 689)
         mat(k, 693) = lmat(k, 693)
         mat(k, 694) = lmat(k, 694)
         mat(k, 695) = lmat(k, 695)
         mat(k, 696) = mat(k, 696) + lmat(k, 696)
         mat(k, 697) = mat(k, 697) + lmat(k, 697)
         mat(k, 700) = mat(k, 700) + lmat(k, 700)
         mat(k, 704) = mat(k, 704) + lmat(k, 704)
         mat(k, 705) = lmat(k, 705)
         mat(k, 711) = mat(k, 711) + lmat(k, 711)
         mat(k, 713) = mat(k, 713) + lmat(k, 713)
         mat(k, 722) = lmat(k, 722)
         mat(k, 723) = lmat(k, 723)
         mat(k, 724) = lmat(k, 724)
         mat(k, 725) = lmat(k, 725)
         mat(k, 726) = mat(k, 726) + lmat(k, 726)
         mat(k, 727) = mat(k, 727) + lmat(k, 727)
         mat(k, 728) = lmat(k, 728)
         mat(k, 730) = lmat(k, 730)
         mat(k, 731) = mat(k, 731) + lmat(k, 731)
         mat(k, 734) = mat(k, 734) + lmat(k, 734)
         mat(k, 735) = lmat(k, 735)
         mat(k, 737) = lmat(k, 737)
         mat(k, 742) = mat(k, 742) + lmat(k, 742)
         mat(k, 748) = mat(k, 748) + lmat(k, 748)
         mat(k, 749) = mat(k, 749) + lmat(k, 749)
         mat(k, 754) = lmat(k, 754)
         mat(k, 755) = mat(k, 755) + lmat(k, 755)
         mat(k, 759) = lmat(k, 759)
         mat(k, 760) = lmat(k, 760)
         mat(k, 762) = lmat(k, 762)
         mat(k, 763) = lmat(k, 763)
         mat(k, 764) = mat(k, 764) + lmat(k, 764)
         mat(k, 765) = lmat(k, 765)
         mat(k, 766) = mat(k, 766) + lmat(k, 766)
         mat(k, 769) = mat(k, 769) + lmat(k, 769)
         mat(k, 770) = mat(k, 770) + lmat(k, 770)
         mat(k, 772) = mat(k, 772) + lmat(k, 772)
         mat(k, 773) = lmat(k, 773)
         mat(k, 775) = mat(k, 775) + lmat(k, 775)
         mat(k, 777) = mat(k, 777) + lmat(k, 777)
         mat(k, 784) = mat(k, 784) + lmat(k, 784)
         mat(k, 785) = lmat(k, 785)
         mat(k, 786) = lmat(k, 786)
         mat(k, 787) = lmat(k, 787)
         mat(k, 788) = lmat(k, 788)
         mat(k, 790) = lmat(k, 790)
         mat(k, 791) = lmat(k, 791)
         mat(k, 792) = lmat(k, 792)
         mat(k, 793) = mat(k, 793) + lmat(k, 793)
         mat(k, 794) = mat(k, 794) + lmat(k, 794)
         mat(k, 799) = lmat(k, 799)
         mat(k, 800) = lmat(k, 800)
         mat(k, 801) = lmat(k, 801)
         mat(k, 802) = lmat(k, 802)
         mat(k, 803) = mat(k, 803) + lmat(k, 803)
         mat(k, 808) = lmat(k, 808)
         mat(k, 810) = lmat(k, 810)
         mat(k, 812) = lmat(k, 812)
         mat(k, 813) = mat(k, 813) + lmat(k, 813)
         mat(k, 814) = mat(k, 814) + lmat(k, 814)
         mat(k, 825) = mat(k, 825) + lmat(k, 825)
         mat(k, 835) = mat(k, 835) + lmat(k, 835)
         mat(k, 843) = mat(k, 843) + lmat(k, 843)
         mat(k, 845) = lmat(k, 845)
         mat(k, 846) = lmat(k, 846)
         mat(k, 847) = lmat(k, 847)
         mat(k, 848) = lmat(k, 848)
         mat(k, 849) = mat(k, 849) + lmat(k, 849)
         mat(k, 856) = mat(k, 856) + lmat(k, 856)
         mat(k, 867) = mat(k, 867) + lmat(k, 867)
         mat(k, 877) = mat(k, 877) + lmat(k, 877)
         mat(k, 885) = mat(k, 885) + lmat(k, 885)
         mat(k, 886) = lmat(k, 886)
         mat(k, 888) = lmat(k, 888)
         mat(k, 894) = mat(k, 894) + lmat(k, 894)
         mat(k, 904) = mat(k, 904) + lmat(k, 904)
         mat(k, 909) = mat(k, 909) + lmat(k, 909)
         mat(k, 916) = mat(k, 916) + lmat(k, 916)
         mat(k, 917) = mat(k, 917) + lmat(k, 917)
         mat(k, 922) = mat(k, 922) + lmat(k, 922)
         mat(k, 929) = mat(k, 929) + lmat(k, 929)
         mat(k, 937) = mat(k, 937) + lmat(k, 937)
         mat(k, 938) = mat(k, 938) + lmat(k, 938)
         mat(k, 939) = mat(k, 939) + lmat(k, 939)
         mat(k, 940) = lmat(k, 940)
         mat(k, 942) = mat(k, 942) + lmat(k, 942)
         mat(k, 944) = lmat(k, 944)
         mat(k, 945) = mat(k, 945) + lmat(k, 945)
         mat(k, 946) = mat(k, 946) + lmat(k, 946)
         mat(k, 948) = lmat(k, 948)
         mat(k, 949) = lmat(k, 949)
         mat(k, 950) = lmat(k, 950)
         mat(k, 951) = mat(k, 951) + lmat(k, 951)
         mat(k, 953) = lmat(k, 953)
         mat(k, 954) = mat(k, 954) + lmat(k, 954)
         mat(k, 956) = lmat(k, 956)
         mat(k, 957) = mat(k, 957) + lmat(k, 957)
         mat(k, 959) = lmat(k, 959)
         mat(k, 960) = lmat(k, 960)
         mat(k, 964) = mat(k, 964) + lmat(k, 964)
         mat(k, 980) = mat(k, 980) + lmat(k, 980)
         mat(k, 982) = lmat(k, 982)
         mat(k, 983) = lmat(k, 983)
         mat(k, 985) = mat(k, 985) + lmat(k, 985)
         mat(k, 987) = mat(k, 987) + lmat(k, 987)
         mat(k, 996) = mat(k, 996) + lmat(k, 996)
         mat(k,1004) = mat(k,1004) + lmat(k,1004)
         mat(k,1013) = mat(k,1013) + lmat(k,1013)
         mat(k,1023) = mat(k,1023) + lmat(k,1023)
         mat(k,1043) = mat(k,1043) + lmat(k,1043)
         mat(k,1063) = lmat(k,1063)
         mat(k,1067) = lmat(k,1067)
         mat(k,1068) = mat(k,1068) + lmat(k,1068)
         mat(k,1087) = mat(k,1087) + lmat(k,1087)
         mat(k,1111) = mat(k,1111) + lmat(k,1111)
         mat(k,1122) = lmat(k,1122)
         mat(k,1123) = mat(k,1123) + lmat(k,1123)
         mat(k,1124) = mat(k,1124) + lmat(k,1124)
         mat(k,1126) = mat(k,1126) + lmat(k,1126)
         mat(k,1128) = mat(k,1128) + lmat(k,1128)
         mat(k,1129) = mat(k,1129) + lmat(k,1129)
         mat(k,1130) = mat(k,1130) + lmat(k,1130)
         mat(k,1133) = mat(k,1133) + lmat(k,1133)
         mat(k,1134) = mat(k,1134) + lmat(k,1134)
         mat(k,1135) = mat(k,1135) + lmat(k,1135)
         mat(k,1136) = mat(k,1136) + lmat(k,1136)
         mat(k,1138) = lmat(k,1138)
         mat(k,1142) = mat(k,1142) + lmat(k,1142)
         mat(k,1152) = mat(k,1152) + lmat(k,1152)
         mat(k,1154) = lmat(k,1154)
         mat(k,1155) = lmat(k,1155)
         mat(k,1157) = lmat(k,1157)
         mat(k,1160) = mat(k,1160) + lmat(k,1160)
         mat(k,1162) = lmat(k,1162)
         mat(k,1163) = lmat(k,1163)
         mat(k,1166) = mat(k,1166) + lmat(k,1166)
         mat(k,1179) = mat(k,1179) + lmat(k,1179)
         mat(k,1180) = lmat(k,1180)
         mat(k,1183) = lmat(k,1183)
         mat(k,1185) = lmat(k,1185)
         mat(k,1189) = mat(k,1189) + lmat(k,1189)
         mat(k,1197) = mat(k,1197) + lmat(k,1197)
         mat(k,1199) = lmat(k,1199)
         mat(k,1200) = lmat(k,1200)
         mat(k,1205) = mat(k,1205) + lmat(k,1205)
         mat(k,1217) = mat(k,1217) + lmat(k,1217)
         mat(k,1218) = mat(k,1218) + lmat(k,1218)
         mat(k,1219) = mat(k,1219) + lmat(k,1219)
         mat(k,1220) = mat(k,1220) + lmat(k,1220)
         mat(k,1221) = mat(k,1221) + lmat(k,1221)
         mat(k,1222) = mat(k,1222) + lmat(k,1222)
         mat(k,1224) = mat(k,1224) + lmat(k,1224)
         mat(k,1225) = mat(k,1225) + lmat(k,1225)
         mat(k,1229) = mat(k,1229) + lmat(k,1229)
         mat(k,1245) = mat(k,1245) + lmat(k,1245)
         mat(k,1263) = mat(k,1263) + lmat(k,1263)
         mat(k,1264) = lmat(k,1264)
         mat(k,1266) = lmat(k,1266)
         mat(k,1267) = lmat(k,1267)
         mat(k,1272) = mat(k,1272) + lmat(k,1272)
         mat(k,1277) = lmat(k,1277)
         mat(k,1278) = mat(k,1278) + lmat(k,1278)
         mat(k,1280) = mat(k,1280) + lmat(k,1280)
         mat(k,1281) = mat(k,1281) + lmat(k,1281)
         mat(k,1290) = mat(k,1290) + lmat(k,1290)
         mat(k,1303) = lmat(k,1303)
         mat(k,1304) = lmat(k,1304)
         mat(k,1305) = lmat(k,1305)
         mat(k,1306) = lmat(k,1306)
         mat(k,1307) = mat(k,1307) + lmat(k,1307)
         mat(k,1308) = lmat(k,1308)
         mat(k,1310) = lmat(k,1310)
         mat(k,1313) = lmat(k,1313)
         mat(k,1314) = mat(k,1314) + lmat(k,1314)
         mat(k,1317) = lmat(k,1317)
         mat(k,1318) = lmat(k,1318)
         mat(k,1320) = mat(k,1320) + lmat(k,1320)
         mat(k,1322) = lmat(k,1322)
         mat(k,1323) = lmat(k,1323)
         mat(k,1324) = mat(k,1324) + lmat(k,1324)
         mat(k,1328) = mat(k,1328) + lmat(k,1328)
         mat(k,1337) = mat(k,1337) + lmat(k,1337)
         mat(k,1339) = lmat(k,1339)
         mat(k,1344) = mat(k,1344) + lmat(k,1344)
         mat(k,1360) = mat(k,1360) + lmat(k,1360)
         mat(k,1380) = mat(k,1380) + lmat(k,1380)
         mat(k,1395) = mat(k,1395) + lmat(k,1395)
         mat(k,1396) = mat(k,1396) + lmat(k,1396)
         mat(k,1399) = mat(k,1399) + lmat(k,1399)
         mat(k,1400) = mat(k,1400) + lmat(k,1400)
         mat(k,1402) = mat(k,1402) + lmat(k,1402)
         mat(k,1403) = mat(k,1403) + lmat(k,1403)
         mat(k,1407) = mat(k,1407) + lmat(k,1407)
         mat(k,1408) = mat(k,1408) + lmat(k,1408)
         mat(k,1409) = mat(k,1409) + lmat(k,1409)
         mat(k,1411) = lmat(k,1411)
         mat(k,1426) = mat(k,1426) + lmat(k,1426)
         mat(k,1442) = lmat(k,1442)
         mat(k,1459) = mat(k,1459) + lmat(k,1459)
         mat(k,1468) = mat(k,1468) + lmat(k,1468)
         mat(k,1483) = mat(k,1483) + lmat(k,1483)
         mat(k,1497) = lmat(k,1497)
         mat(k,1499) = mat(k,1499) + lmat(k,1499)
         mat(k,1503) = mat(k,1503) + lmat(k,1503)
         mat(k,1505) = mat(k,1505) + lmat(k,1505)
         mat(k,1508) = lmat(k,1508)
         mat(k,1524) = mat(k,1524) + lmat(k,1524)
         mat(k,1555) = mat(k,1555) + lmat(k,1555)
         mat(k,1576) = mat(k,1576) + lmat(k,1576)
         mat(k,1577) = mat(k,1577) + lmat(k,1577)
         mat(k,1583) = lmat(k,1583)
         mat(k,1589) = mat(k,1589) + lmat(k,1589)
         mat(k,1597) = mat(k,1597) + lmat(k,1597)
         mat(k,1600) = mat(k,1600) + lmat(k,1600)
         mat(k,1603) = mat(k,1603) + lmat(k,1603)
         mat(k,1609) = mat(k,1609) + lmat(k,1609)
         mat(k,1616) = mat(k,1616) + lmat(k,1616)
         mat(k,1629) = lmat(k,1629)
         mat(k,1631) = mat(k,1631) + lmat(k,1631)
         mat(k,1640) = mat(k,1640) + lmat(k,1640)
         mat(k,1647) = lmat(k,1647)
         mat(k,1648) = lmat(k,1648)
         mat(k,1649) = mat(k,1649) + lmat(k,1649)
         mat(k,1650) = mat(k,1650) + lmat(k,1650)
         mat(k,1651) = mat(k,1651) + lmat(k,1651)
         mat(k,1655) = mat(k,1655) + lmat(k,1655)
         mat(k,1658) = mat(k,1658) + lmat(k,1658)
         mat(k,1662) = lmat(k,1662)
         mat(k,1664) = mat(k,1664) + lmat(k,1664)
         mat(k,1665) = mat(k,1665) + lmat(k,1665)
         mat(k,1666) = mat(k,1666) + lmat(k,1666)
         mat(k,1667) = mat(k,1667) + lmat(k,1667)
         mat(k,1671) = mat(k,1671) + lmat(k,1671)
         mat(k,1676) = mat(k,1676) + lmat(k,1676)
         mat(k,1680) = lmat(k,1680)
         mat(k,1689) = mat(k,1689) + lmat(k,1689)
         mat(k,1690) = mat(k,1690) + lmat(k,1690)
         mat(k,1695) = mat(k,1695) + lmat(k,1695)
         mat(k,1699) = mat(k,1699) + lmat(k,1699)
         mat(k,1704) = lmat(k,1704)
         mat(k,1748) = mat(k,1748) + lmat(k,1748)
         mat(k,1773) = mat(k,1773) + lmat(k,1773)
         mat(k,1775) = mat(k,1775) + lmat(k,1775)
         mat(k,1776) = lmat(k,1776)
         mat(k,1783) = mat(k,1783) + lmat(k,1783)
         mat(k,1784) = mat(k,1784) + lmat(k,1784)
         mat(k,1793) = mat(k,1793) + lmat(k,1793)
         mat(k,1798) = mat(k,1798) + lmat(k,1798)
         mat(k,1806) = mat(k,1806) + lmat(k,1806)
         mat(k,1809) = mat(k,1809) + lmat(k,1809)
         mat(k,1812) = mat(k,1812) + lmat(k,1812)
         mat(k,1830) = mat(k,1830) + lmat(k,1830)
         mat(k,1835) = mat(k,1835) + lmat(k,1835)
         mat(k,1842) = mat(k,1842) + lmat(k,1842)
         mat(k,1875) = mat(k,1875) + lmat(k,1875)
         mat(k,1887) = mat(k,1887) + lmat(k,1887)
         mat(k,1895) = mat(k,1895) + lmat(k,1895)
         mat(k,1898) = lmat(k,1898)
         mat(k,1907) = mat(k,1907) + lmat(k,1907)
         mat(k,1921) = mat(k,1921) + lmat(k,1921)
         mat(k,1968) = mat(k,1968) + lmat(k,1968)
         mat(k,1970) = lmat(k,1970)
         mat(k,1976) = mat(k,1976) + lmat(k,1976)
         mat(k,2014) = mat(k,2014) + lmat(k,2014)
         mat(k,2024) = mat(k,2024) + lmat(k,2024)
         mat(k,2131) = mat(k,2131) + lmat(k,2131)
         mat(k,2145) = mat(k,2145) + lmat(k,2145)
         mat(k,2315) = mat(k,2315) + lmat(k,2315)
         mat(k,2380) = mat(k,2380) + lmat(k,2380)
         mat(k,2385) = mat(k,2385) + lmat(k,2385)
         mat(k,2388) = mat(k,2388) + lmat(k,2388)
         mat(k,2391) = mat(k,2391) + lmat(k,2391)
         mat(k,2395) = mat(k,2395) + lmat(k,2395)
         mat(k,2396) = mat(k,2396) + lmat(k,2396)
         mat(k,2419) = mat(k,2419) + lmat(k,2419)
         mat(k,2420) = mat(k,2420) + lmat(k,2420)
         mat(k,2425) = mat(k,2425) + lmat(k,2425)
         mat(k,2482) = mat(k,2482) + lmat(k,2482)
         mat(k,2500) = lmat(k,2500)
         mat(k,2505) = mat(k,2505) + lmat(k,2505)
         mat(k,2509) = mat(k,2509) + lmat(k,2509)
         mat(k,2514) = lmat(k,2514)
         mat(k,2540) = mat(k,2540) + lmat(k,2540)
         mat(k,2571) = mat(k,2571) + lmat(k,2571)
         mat(k,2600) = mat(k,2600) + lmat(k,2600)
         mat(k,2601) = mat(k,2601) + lmat(k,2601)
         mat(k,2602) = mat(k,2602) + lmat(k,2602)
         mat(k,2616) = mat(k,2616) + lmat(k,2616)
         mat(k,2619) = lmat(k,2619)
         mat(k,2649) = mat(k,2649) + lmat(k,2649)
         mat(k,2697) = mat(k,2697) + lmat(k,2697)
         mat(k,2699) = mat(k,2699) + lmat(k,2699)
         mat(k,2703) = mat(k,2703) + lmat(k,2703)
         mat(k,2707) = mat(k,2707) + lmat(k,2707)
         mat(k,2708) = mat(k,2708) + lmat(k,2708)
         mat(k,2718) = mat(k,2718) + lmat(k,2718)
         mat(k,2761) = mat(k,2761) + lmat(k,2761)
         mat(k,2764) = mat(k,2764) + lmat(k,2764)
         mat(k,2776) = mat(k,2776) + lmat(k,2776)
         mat(k,2778) = mat(k,2778) + lmat(k,2778)
         mat(k,2803) = mat(k,2803) + lmat(k,2803)
         mat(k,2830) = mat(k,2830) + lmat(k,2830)
         mat(k,2833) = mat(k,2833) + lmat(k,2833)
         mat(k,2837) = mat(k,2837) + lmat(k,2837)
         mat(k,2844) = lmat(k,2844)
         mat(k,2851) = mat(k,2851) + lmat(k,2851)
         mat(k,2855) = mat(k,2855) + lmat(k,2855)
         mat(k,2863) = lmat(k,2863)
         mat(k,2866) = lmat(k,2866)
         mat(k,2868) = mat(k,2868) + lmat(k,2868)
         mat(k, 250) = 0._r8
         mat(k, 251) = 0._r8
         mat(k, 314) = 0._r8
         mat(k, 380) = 0._r8
         mat(k, 402) = 0._r8
         mat(k, 507) = 0._r8
         mat(k, 510) = 0._r8
         mat(k, 529) = 0._r8
         mat(k, 579) = 0._r8
         mat(k, 583) = 0._r8
         mat(k, 598) = 0._r8
         mat(k, 690) = 0._r8
         mat(k, 692) = 0._r8
         mat(k, 745) = 0._r8
         mat(k, 747) = 0._r8
         mat(k, 756) = 0._r8
         mat(k, 757) = 0._r8
         mat(k, 761) = 0._r8
         mat(k, 767) = 0._r8
         mat(k, 768) = 0._r8
         mat(k, 771) = 0._r8
         mat(k, 804) = 0._r8
         mat(k, 806) = 0._r8
         mat(k, 807) = 0._r8
         mat(k, 809) = 0._r8
         mat(k, 811) = 0._r8
         mat(k, 824) = 0._r8
         mat(k, 826) = 0._r8
         mat(k, 827) = 0._r8
         mat(k, 829) = 0._r8
         mat(k, 833) = 0._r8
         mat(k, 855) = 0._r8
         mat(k, 857) = 0._r8
         mat(k, 858) = 0._r8
         mat(k, 860) = 0._r8
         mat(k, 862) = 0._r8
         mat(k, 865) = 0._r8
         mat(k, 878) = 0._r8
         mat(k, 879) = 0._r8
         mat(k, 883) = 0._r8
         mat(k, 896) = 0._r8
         mat(k, 901) = 0._r8
         mat(k, 903) = 0._r8
         mat(k, 912) = 0._r8
         mat(k, 913) = 0._r8
         mat(k, 914) = 0._r8
         mat(k, 926) = 0._r8
         mat(k, 931) = 0._r8
         mat(k, 932) = 0._r8
         mat(k, 933) = 0._r8
         mat(k, 935) = 0._r8
         mat(k, 941) = 0._r8
         mat(k, 955) = 0._r8
         mat(k, 958) = 0._r8
         mat(k,1011) = 0._r8
         mat(k,1031) = 0._r8
         mat(k,1033) = 0._r8
         mat(k,1044) = 0._r8
         mat(k,1045) = 0._r8
         mat(k,1053) = 0._r8
         mat(k,1061) = 0._r8
         mat(k,1064) = 0._r8
         mat(k,1069) = 0._r8
         mat(k,1070) = 0._r8
         mat(k,1072) = 0._r8
         mat(k,1088) = 0._r8
         mat(k,1089) = 0._r8
         mat(k,1097) = 0._r8
         mat(k,1105) = 0._r8
         mat(k,1109) = 0._r8
         mat(k,1110) = 0._r8
         mat(k,1114) = 0._r8
         mat(k,1115) = 0._r8
         mat(k,1116) = 0._r8
         mat(k,1120) = 0._r8
         mat(k,1149) = 0._r8
         mat(k,1150) = 0._r8
         mat(k,1184) = 0._r8
         mat(k,1187) = 0._r8
         mat(k,1190) = 0._r8
         mat(k,1191) = 0._r8
         mat(k,1192) = 0._r8
         mat(k,1193) = 0._r8
         mat(k,1194) = 0._r8
         mat(k,1195) = 0._r8
         mat(k,1196) = 0._r8
         mat(k,1206) = 0._r8
         mat(k,1207) = 0._r8
         mat(k,1208) = 0._r8
         mat(k,1213) = 0._r8
         mat(k,1215) = 0._r8
         mat(k,1223) = 0._r8
         mat(k,1227) = 0._r8
         mat(k,1248) = 0._r8
         mat(k,1249) = 0._r8
         mat(k,1250) = 0._r8
         mat(k,1254) = 0._r8
         mat(k,1256) = 0._r8
         mat(k,1260) = 0._r8
         mat(k,1262) = 0._r8
         mat(k,1291) = 0._r8
         mat(k,1292) = 0._r8
         mat(k,1298) = 0._r8
         mat(k,1299) = 0._r8
         mat(k,1301) = 0._r8
         mat(k,1309) = 0._r8
         mat(k,1311) = 0._r8
         mat(k,1312) = 0._r8
         mat(k,1316) = 0._r8
         mat(k,1319) = 0._r8
         mat(k,1335) = 0._r8
         mat(k,1338) = 0._r8
         mat(k,1353) = 0._r8
         mat(k,1368) = 0._r8
         mat(k,1373) = 0._r8
         mat(k,1375) = 0._r8
         mat(k,1377) = 0._r8
         mat(k,1378) = 0._r8
         mat(k,1379) = 0._r8
         mat(k,1381) = 0._r8
         mat(k,1382) = 0._r8
         mat(k,1383) = 0._r8
         mat(k,1385) = 0._r8
         mat(k,1390) = 0._r8
         mat(k,1392) = 0._r8
         mat(k,1401) = 0._r8
         mat(k,1410) = 0._r8
         mat(k,1419) = 0._r8
         mat(k,1420) = 0._r8
         mat(k,1421) = 0._r8
         mat(k,1422) = 0._r8
         mat(k,1423) = 0._r8
         mat(k,1425) = 0._r8
         mat(k,1427) = 0._r8
         mat(k,1429) = 0._r8
         mat(k,1436) = 0._r8
         mat(k,1438) = 0._r8
         mat(k,1440) = 0._r8
         mat(k,1441) = 0._r8
         mat(k,1445) = 0._r8
         mat(k,1448) = 0._r8
         mat(k,1449) = 0._r8
         mat(k,1451) = 0._r8
         mat(k,1453) = 0._r8
         mat(k,1455) = 0._r8
         mat(k,1456) = 0._r8
         mat(k,1457) = 0._r8
         mat(k,1460) = 0._r8
         mat(k,1461) = 0._r8
         mat(k,1462) = 0._r8
         mat(k,1464) = 0._r8
         mat(k,1469) = 0._r8
         mat(k,1471) = 0._r8
         mat(k,1473) = 0._r8
         mat(k,1474) = 0._r8
         mat(k,1481) = 0._r8
         mat(k,1484) = 0._r8
         mat(k,1486) = 0._r8
         mat(k,1491) = 0._r8
         mat(k,1493) = 0._r8
         mat(k,1495) = 0._r8
         mat(k,1500) = 0._r8
         mat(k,1504) = 0._r8
         mat(k,1507) = 0._r8
         mat(k,1509) = 0._r8
         mat(k,1511) = 0._r8
         mat(k,1514) = 0._r8
         mat(k,1515) = 0._r8
         mat(k,1516) = 0._r8
         mat(k,1518) = 0._r8
         mat(k,1522) = 0._r8
         mat(k,1523) = 0._r8
         mat(k,1533) = 0._r8
         mat(k,1536) = 0._r8
         mat(k,1557) = 0._r8
         mat(k,1559) = 0._r8
         mat(k,1564) = 0._r8
         mat(k,1565) = 0._r8
         mat(k,1568) = 0._r8
         mat(k,1572) = 0._r8
         mat(k,1574) = 0._r8
         mat(k,1575) = 0._r8
         mat(k,1578) = 0._r8
         mat(k,1579) = 0._r8
         mat(k,1580) = 0._r8
         mat(k,1581) = 0._r8
         mat(k,1582) = 0._r8
         mat(k,1584) = 0._r8
         mat(k,1585) = 0._r8
         mat(k,1595) = 0._r8
         mat(k,1596) = 0._r8
         mat(k,1607) = 0._r8
         mat(k,1608) = 0._r8
         mat(k,1610) = 0._r8
         mat(k,1612) = 0._r8
         mat(k,1617) = 0._r8
         mat(k,1618) = 0._r8
         mat(k,1622) = 0._r8
         mat(k,1624) = 0._r8
         mat(k,1634) = 0._r8
         mat(k,1644) = 0._r8
         mat(k,1652) = 0._r8
         mat(k,1656) = 0._r8
         mat(k,1659) = 0._r8
         mat(k,1661) = 0._r8
         mat(k,1663) = 0._r8
         mat(k,1672) = 0._r8
         mat(k,1673) = 0._r8
         mat(k,1675) = 0._r8
         mat(k,1678) = 0._r8
         mat(k,1681) = 0._r8
         mat(k,1682) = 0._r8
         mat(k,1683) = 0._r8
         mat(k,1684) = 0._r8
         mat(k,1685) = 0._r8
         mat(k,1693) = 0._r8
         mat(k,1694) = 0._r8
         mat(k,1696) = 0._r8
         mat(k,1698) = 0._r8
         mat(k,1700) = 0._r8
         mat(k,1701) = 0._r8
         mat(k,1702) = 0._r8
         mat(k,1703) = 0._r8
         mat(k,1707) = 0._r8
         mat(k,1708) = 0._r8
         mat(k,1709) = 0._r8
         mat(k,1719) = 0._r8
         mat(k,1747) = 0._r8
         mat(k,1750) = 0._r8
         mat(k,1751) = 0._r8
         mat(k,1752) = 0._r8
         mat(k,1756) = 0._r8
         mat(k,1757) = 0._r8
         mat(k,1760) = 0._r8
         mat(k,1761) = 0._r8
         mat(k,1763) = 0._r8
         mat(k,1764) = 0._r8
         mat(k,1765) = 0._r8
         mat(k,1779) = 0._r8
         mat(k,1781) = 0._r8
         mat(k,1785) = 0._r8
         mat(k,1788) = 0._r8
         mat(k,1789) = 0._r8
         mat(k,1790) = 0._r8
         mat(k,1791) = 0._r8
         mat(k,1792) = 0._r8
         mat(k,1794) = 0._r8
         mat(k,1797) = 0._r8
         mat(k,1803) = 0._r8
         mat(k,1804) = 0._r8
         mat(k,1805) = 0._r8
         mat(k,1808) = 0._r8
         mat(k,1810) = 0._r8
         mat(k,1813) = 0._r8
         mat(k,1814) = 0._r8
         mat(k,1815) = 0._r8
         mat(k,1816) = 0._r8
         mat(k,1818) = 0._r8
         mat(k,1819) = 0._r8
         mat(k,1820) = 0._r8
         mat(k,1833) = 0._r8
         mat(k,1837) = 0._r8
         mat(k,1838) = 0._r8
         mat(k,1839) = 0._r8
         mat(k,1841) = 0._r8
         mat(k,1843) = 0._r8
         mat(k,1869) = 0._r8
         mat(k,1870) = 0._r8
         mat(k,1873) = 0._r8
         mat(k,1880) = 0._r8
         mat(k,1883) = 0._r8
         mat(k,1884) = 0._r8
         mat(k,1888) = 0._r8
         mat(k,1891) = 0._r8
         mat(k,1894) = 0._r8
         mat(k,1896) = 0._r8
         mat(k,1899) = 0._r8
         mat(k,1900) = 0._r8
         mat(k,1901) = 0._r8
         mat(k,1902) = 0._r8
         mat(k,1903) = 0._r8
         mat(k,1904) = 0._r8
         mat(k,1906) = 0._r8
         mat(k,1908) = 0._r8
         mat(k,1912) = 0._r8
         mat(k,1915) = 0._r8
         mat(k,1917) = 0._r8
         mat(k,1919) = 0._r8
         mat(k,1920) = 0._r8
         mat(k,1922) = 0._r8
         mat(k,1971) = 0._r8
         mat(k,2004) = 0._r8
         mat(k,2005) = 0._r8
         mat(k,2006) = 0._r8
         mat(k,2007) = 0._r8
         mat(k,2010) = 0._r8
         mat(k,2011) = 0._r8
         mat(k,2012) = 0._r8
         mat(k,2020) = 0._r8
         mat(k,2027) = 0._r8
         mat(k,2029) = 0._r8
         mat(k,2061) = 0._r8
         mat(k,2062) = 0._r8
         mat(k,2063) = 0._r8
         mat(k,2073) = 0._r8
         mat(k,2087) = 0._r8
         mat(k,2095) = 0._r8
         mat(k,2096) = 0._r8
         mat(k,2098) = 0._r8
         mat(k,2102) = 0._r8
         mat(k,2104) = 0._r8
         mat(k,2109) = 0._r8
         mat(k,2114) = 0._r8
         mat(k,2119) = 0._r8
         mat(k,2121) = 0._r8
         mat(k,2128) = 0._r8
         mat(k,2136) = 0._r8
         mat(k,2218) = 0._r8
         mat(k,2240) = 0._r8
         mat(k,2249) = 0._r8
         mat(k,2254) = 0._r8
         mat(k,2260) = 0._r8
         mat(k,2263) = 0._r8
         mat(k,2290) = 0._r8
         mat(k,2311) = 0._r8
         mat(k,2334) = 0._r8
         mat(k,2335) = 0._r8
         mat(k,2340) = 0._r8
         mat(k,2343) = 0._r8
         mat(k,2345) = 0._r8
         mat(k,2352) = 0._r8
         mat(k,2359) = 0._r8
         mat(k,2362) = 0._r8
         mat(k,2373) = 0._r8
         mat(k,2375) = 0._r8
         mat(k,2376) = 0._r8
         mat(k,2378) = 0._r8
         mat(k,2379) = 0._r8
         mat(k,2381) = 0._r8
         mat(k,2382) = 0._r8
         mat(k,2383) = 0._r8
         mat(k,2389) = 0._r8
         mat(k,2393) = 0._r8
         mat(k,2394) = 0._r8
         mat(k,2397) = 0._r8
         mat(k,2398) = 0._r8
         mat(k,2400) = 0._r8
         mat(k,2413) = 0._r8
         mat(k,2418) = 0._r8
         mat(k,2421) = 0._r8
         mat(k,2427) = 0._r8
         mat(k,2428) = 0._r8
         mat(k,2430) = 0._r8
         mat(k,2449) = 0._r8
         mat(k,2451) = 0._r8
         mat(k,2454) = 0._r8
         mat(k,2456) = 0._r8
         mat(k,2460) = 0._r8
         mat(k,2462) = 0._r8
         mat(k,2463) = 0._r8
         mat(k,2464) = 0._r8
         mat(k,2465) = 0._r8
         mat(k,2466) = 0._r8
         mat(k,2468) = 0._r8
         mat(k,2470) = 0._r8
         mat(k,2475) = 0._r8
         mat(k,2477) = 0._r8
         mat(k,2483) = 0._r8
         mat(k,2486) = 0._r8
         mat(k,2487) = 0._r8
         mat(k,2491) = 0._r8
         mat(k,2492) = 0._r8
         mat(k,2495) = 0._r8
         mat(k,2496) = 0._r8
         mat(k,2497) = 0._r8
         mat(k,2498) = 0._r8
         mat(k,2499) = 0._r8
         mat(k,2501) = 0._r8
         mat(k,2502) = 0._r8
         mat(k,2503) = 0._r8
         mat(k,2504) = 0._r8
         mat(k,2507) = 0._r8
         mat(k,2508) = 0._r8
         mat(k,2510) = 0._r8
         mat(k,2511) = 0._r8
         mat(k,2512) = 0._r8
         mat(k,2513) = 0._r8
         mat(k,2515) = 0._r8
         mat(k,2516) = 0._r8
         mat(k,2517) = 0._r8
         mat(k,2521) = 0._r8
         mat(k,2522) = 0._r8
         mat(k,2526) = 0._r8
         mat(k,2527) = 0._r8
         mat(k,2529) = 0._r8
         mat(k,2530) = 0._r8
         mat(k,2531) = 0._r8
         mat(k,2532) = 0._r8
         mat(k,2535) = 0._r8
         mat(k,2537) = 0._r8
         mat(k,2538) = 0._r8
         mat(k,2539) = 0._r8
         mat(k,2543) = 0._r8
         mat(k,2546) = 0._r8
         mat(k,2548) = 0._r8
         mat(k,2551) = 0._r8
         mat(k,2554) = 0._r8
         mat(k,2556) = 0._r8
         mat(k,2557) = 0._r8
         mat(k,2559) = 0._r8
         mat(k,2560) = 0._r8
         mat(k,2561) = 0._r8
         mat(k,2563) = 0._r8
         mat(k,2565) = 0._r8
         mat(k,2567) = 0._r8
         mat(k,2568) = 0._r8
         mat(k,2569) = 0._r8
         mat(k,2573) = 0._r8
         mat(k,2576) = 0._r8
         mat(k,2578) = 0._r8
         mat(k,2588) = 0._r8
         mat(k,2589) = 0._r8
         mat(k,2590) = 0._r8
         mat(k,2591) = 0._r8
         mat(k,2595) = 0._r8
         mat(k,2598) = 0._r8
         mat(k,2604) = 0._r8
         mat(k,2605) = 0._r8
         mat(k,2607) = 0._r8
         mat(k,2615) = 0._r8
         mat(k,2618) = 0._r8
         mat(k,2623) = 0._r8
         mat(k,2625) = 0._r8
         mat(k,2630) = 0._r8
         mat(k,2631) = 0._r8
         mat(k,2633) = 0._r8
         mat(k,2637) = 0._r8
         mat(k,2645) = 0._r8
         mat(k,2654) = 0._r8
         mat(k,2660) = 0._r8
         mat(k,2661) = 0._r8
         mat(k,2663) = 0._r8
         mat(k,2666) = 0._r8
         mat(k,2670) = 0._r8
         mat(k,2677) = 0._r8
         mat(k,2680) = 0._r8
         mat(k,2681) = 0._r8
         mat(k,2682) = 0._r8
         mat(k,2685) = 0._r8
         mat(k,2686) = 0._r8
         mat(k,2688) = 0._r8
         mat(k,2689) = 0._r8
         mat(k,2690) = 0._r8
         mat(k,2691) = 0._r8
         mat(k,2693) = 0._r8
         mat(k,2694) = 0._r8
         mat(k,2695) = 0._r8
         mat(k,2696) = 0._r8
         mat(k,2710) = 0._r8
         mat(k,2712) = 0._r8
         mat(k,2728) = 0._r8
         mat(k,2733) = 0._r8
         mat(k,2734) = 0._r8
         mat(k,2735) = 0._r8
         mat(k,2739) = 0._r8
         mat(k,2744) = 0._r8
         mat(k,2745) = 0._r8
         mat(k,2746) = 0._r8
         mat(k,2748) = 0._r8
         mat(k,2751) = 0._r8
         mat(k,2752) = 0._r8
         mat(k,2753) = 0._r8
         mat(k,2755) = 0._r8
         mat(k,2762) = 0._r8
         mat(k,2763) = 0._r8
         mat(k,2772) = 0._r8
         mat(k,2781) = 0._r8
         mat(k,2783) = 0._r8
         mat(k,2784) = 0._r8
         mat(k,2786) = 0._r8
         mat(k,2787) = 0._r8
         mat(k,2788) = 0._r8
         mat(k,2789) = 0._r8
         mat(k,2790) = 0._r8
         mat(k,2793) = 0._r8
         mat(k,2794) = 0._r8
         mat(k,2795) = 0._r8
         mat(k,2796) = 0._r8
         mat(k,2797) = 0._r8
         mat(k,2798) = 0._r8
         mat(k,2799) = 0._r8
         mat(k,2801) = 0._r8
         mat(k,2804) = 0._r8
         mat(k,2814) = 0._r8
         mat(k,2815) = 0._r8
         mat(k,2817) = 0._r8
         mat(k,2819) = 0._r8
         mat(k,2820) = 0._r8
         mat(k,2821) = 0._r8
         mat(k,2822) = 0._r8
         mat(k,2829) = 0._r8
         mat(k,2836) = 0._r8
         mat(k,2838) = 0._r8
         mat(k,2843) = 0._r8
         mat(k,2845) = 0._r8
         mat(k,2846) = 0._r8
         mat(k,2847) = 0._r8
         mat(k,2848) = 0._r8
         mat(k,2849) = 0._r8
         mat(k,2850) = 0._r8
         mat(k,2852) = 0._r8
         mat(k,2853) = 0._r8
         mat(k,2854) = 0._r8
         mat(k,2856) = 0._r8
         mat(k,2857) = 0._r8
         mat(k,2858) = 0._r8
         mat(k,2859) = 0._r8
         mat(k,2860) = 0._r8
         mat(k,2861) = 0._r8
         mat(k,2862) = 0._r8
         mat(k,2864) = 0._r8
         mat(k,2865) = 0._r8
         mat(k,2867) = 0._r8
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
         mat(k, 59) = mat(k, 59) - dti(k)
         mat(k, 65) = mat(k, 65) - dti(k)
         mat(k, 71) = mat(k, 71) - dti(k)
         mat(k, 77) = mat(k, 77) - dti(k)
         mat(k, 83) = mat(k, 83) - dti(k)
         mat(k, 85) = mat(k, 85) - dti(k)
         mat(k, 91) = mat(k, 91) - dti(k)
         mat(k, 97) = mat(k, 97) - dti(k)
         mat(k, 103) = mat(k, 103) - dti(k)
         mat(k, 104) = mat(k, 104) - dti(k)
         mat(k, 106) = mat(k, 106) - dti(k)
         mat(k, 109) = mat(k, 109) - dti(k)
         mat(k, 112) = mat(k, 112) - dti(k)
         mat(k, 115) = mat(k, 115) - dti(k)
         mat(k, 118) = mat(k, 118) - dti(k)
         mat(k, 122) = mat(k, 122) - dti(k)
         mat(k, 126) = mat(k, 126) - dti(k)
         mat(k, 130) = mat(k, 130) - dti(k)
         mat(k, 134) = mat(k, 134) - dti(k)
         mat(k, 138) = mat(k, 138) - dti(k)
         mat(k, 142) = mat(k, 142) - dti(k)
         mat(k, 146) = mat(k, 146) - dti(k)
         mat(k, 150) = mat(k, 150) - dti(k)
         mat(k, 154) = mat(k, 154) - dti(k)
         mat(k, 157) = mat(k, 157) - dti(k)
         mat(k, 160) = mat(k, 160) - dti(k)
         mat(k, 163) = mat(k, 163) - dti(k)
         mat(k, 166) = mat(k, 166) - dti(k)
         mat(k, 169) = mat(k, 169) - dti(k)
         mat(k, 174) = mat(k, 174) - dti(k)
         mat(k, 179) = mat(k, 179) - dti(k)
         mat(k, 184) = mat(k, 184) - dti(k)
         mat(k, 190) = mat(k, 190) - dti(k)
         mat(k, 196) = mat(k, 196) - dti(k)
         mat(k, 200) = mat(k, 200) - dti(k)
         mat(k, 205) = mat(k, 205) - dti(k)
         mat(k, 208) = mat(k, 208) - dti(k)
         mat(k, 210) = mat(k, 210) - dti(k)
         mat(k, 214) = mat(k, 214) - dti(k)
         mat(k, 218) = mat(k, 218) - dti(k)
         mat(k, 223) = mat(k, 223) - dti(k)
         mat(k, 230) = mat(k, 230) - dti(k)
         mat(k, 235) = mat(k, 235) - dti(k)
         mat(k, 239) = mat(k, 239) - dti(k)
         mat(k, 242) = mat(k, 242) - dti(k)
         mat(k, 246) = mat(k, 246) - dti(k)
         mat(k, 254) = mat(k, 254) - dti(k)
         mat(k, 259) = mat(k, 259) - dti(k)
         mat(k, 264) = mat(k, 264) - dti(k)
         mat(k, 268) = mat(k, 268) - dti(k)
         mat(k, 273) = mat(k, 273) - dti(k)
         mat(k, 276) = mat(k, 276) - dti(k)
         mat(k, 281) = mat(k, 281) - dti(k)
         mat(k, 284) = mat(k, 284) - dti(k)
         mat(k, 287) = mat(k, 287) - dti(k)
         mat(k, 290) = mat(k, 290) - dti(k)
         mat(k, 295) = mat(k, 295) - dti(k)
         mat(k, 300) = mat(k, 300) - dti(k)
         mat(k, 304) = mat(k, 304) - dti(k)
         mat(k, 308) = mat(k, 308) - dti(k)
         mat(k, 313) = mat(k, 313) - dti(k)
         mat(k, 319) = mat(k, 319) - dti(k)
         mat(k, 322) = mat(k, 322) - dti(k)
         mat(k, 328) = mat(k, 328) - dti(k)
         mat(k, 334) = mat(k, 334) - dti(k)
         mat(k, 337) = mat(k, 337) - dti(k)
         mat(k, 343) = mat(k, 343) - dti(k)
         mat(k, 349) = mat(k, 349) - dti(k)
         mat(k, 355) = mat(k, 355) - dti(k)
         mat(k, 360) = mat(k, 360) - dti(k)
         mat(k, 365) = mat(k, 365) - dti(k)
         mat(k, 370) = mat(k, 370) - dti(k)
         mat(k, 378) = mat(k, 378) - dti(k)
         mat(k, 383) = mat(k, 383) - dti(k)
         mat(k, 388) = mat(k, 388) - dti(k)
         mat(k, 393) = mat(k, 393) - dti(k)
         mat(k, 396) = mat(k, 396) - dti(k)
         mat(k, 400) = mat(k, 400) - dti(k)
         mat(k, 407) = mat(k, 407) - dti(k)
         mat(k, 415) = mat(k, 415) - dti(k)
         mat(k, 423) = mat(k, 423) - dti(k)
         mat(k, 426) = mat(k, 426) - dti(k)
         mat(k, 434) = mat(k, 434) - dti(k)
         mat(k, 442) = mat(k, 442) - dti(k)
         mat(k, 450) = mat(k, 450) - dti(k)
         mat(k, 456) = mat(k, 456) - dti(k)
         mat(k, 462) = mat(k, 462) - dti(k)
         mat(k, 468) = mat(k, 468) - dti(k)
         mat(k, 474) = mat(k, 474) - dti(k)
         mat(k, 480) = mat(k, 480) - dti(k)
         mat(k, 486) = mat(k, 486) - dti(k)
         mat(k, 492) = mat(k, 492) - dti(k)
         mat(k, 498) = mat(k, 498) - dti(k)
         mat(k, 506) = mat(k, 506) - dti(k)
         mat(k, 512) = mat(k, 512) - dti(k)
         mat(k, 518) = mat(k, 518) - dti(k)
         mat(k, 525) = mat(k, 525) - dti(k)
         mat(k, 531) = mat(k, 531) - dti(k)
         mat(k, 534) = mat(k, 534) - dti(k)
         mat(k, 539) = mat(k, 539) - dti(k)
         mat(k, 544) = mat(k, 544) - dti(k)
         mat(k, 548) = mat(k, 548) - dti(k)
         mat(k, 552) = mat(k, 552) - dti(k)
         mat(k, 555) = mat(k, 555) - dti(k)
         mat(k, 562) = mat(k, 562) - dti(k)
         mat(k, 569) = mat(k, 569) - dti(k)
         mat(k, 578) = mat(k, 578) - dti(k)
         mat(k, 585) = mat(k, 585) - dti(k)
         mat(k, 593) = mat(k, 593) - dti(k)
         mat(k, 600) = mat(k, 600) - dti(k)
         mat(k, 606) = mat(k, 606) - dti(k)
         mat(k, 611) = mat(k, 611) - dti(k)
         mat(k, 615) = mat(k, 615) - dti(k)
         mat(k, 621) = mat(k, 621) - dti(k)
         mat(k, 629) = mat(k, 629) - dti(k)
         mat(k, 637) = mat(k, 637) - dti(k)
         mat(k, 645) = mat(k, 645) - dti(k)
         mat(k, 653) = mat(k, 653) - dti(k)
         mat(k, 661) = mat(k, 661) - dti(k)
         mat(k, 670) = mat(k, 670) - dti(k)
         mat(k, 677) = mat(k, 677) - dti(k)
         mat(k, 688) = mat(k, 688) - dti(k)
         mat(k, 697) = mat(k, 697) - dti(k)
         mat(k, 704) = mat(k, 704) - dti(k)
         mat(k, 713) = mat(k, 713) - dti(k)
         mat(k, 722) = mat(k, 722) - dti(k)
         mat(k, 726) = mat(k, 726) - dti(k)
         mat(k, 734) = mat(k, 734) - dti(k)
         mat(k, 742) = mat(k, 742) - dti(k)
         mat(k, 748) = mat(k, 748) - dti(k)
         mat(k, 755) = mat(k, 755) - dti(k)
         mat(k, 766) = mat(k, 766) - dti(k)
         mat(k, 777) = mat(k, 777) - dti(k)
         mat(k, 784) = mat(k, 784) - dti(k)
         mat(k, 794) = mat(k, 794) - dti(k)
         mat(k, 803) = mat(k, 803) - dti(k)
         mat(k, 814) = mat(k, 814) - dti(k)
         mat(k, 825) = mat(k, 825) - dti(k)
         mat(k, 835) = mat(k, 835) - dti(k)
         mat(k, 843) = mat(k, 843) - dti(k)
         mat(k, 856) = mat(k, 856) - dti(k)
         mat(k, 867) = mat(k, 867) - dti(k)
         mat(k, 877) = mat(k, 877) - dti(k)
         mat(k, 885) = mat(k, 885) - dti(k)
         mat(k, 894) = mat(k, 894) - dti(k)
         mat(k, 904) = mat(k, 904) - dti(k)
         mat(k, 909) = mat(k, 909) - dti(k)
         mat(k, 917) = mat(k, 917) - dti(k)
         mat(k, 929) = mat(k, 929) - dti(k)
         mat(k, 939) = mat(k, 939) - dti(k)
         mat(k, 951) = mat(k, 951) - dti(k)
         mat(k, 964) = mat(k, 964) - dti(k)
         mat(k, 980) = mat(k, 980) - dti(k)
         mat(k, 987) = mat(k, 987) - dti(k)
         mat(k, 996) = mat(k, 996) - dti(k)
         mat(k,1004) = mat(k,1004) - dti(k)
         mat(k,1013) = mat(k,1013) - dti(k)
         mat(k,1023) = mat(k,1023) - dti(k)
         mat(k,1043) = mat(k,1043) - dti(k)
         mat(k,1068) = mat(k,1068) - dti(k)
         mat(k,1087) = mat(k,1087) - dti(k)
         mat(k,1111) = mat(k,1111) - dti(k)
         mat(k,1123) = mat(k,1123) - dti(k)
         mat(k,1129) = mat(k,1129) - dti(k)
         mat(k,1142) = mat(k,1142) - dti(k)
         mat(k,1152) = mat(k,1152) - dti(k)
         mat(k,1160) = mat(k,1160) - dti(k)
         mat(k,1166) = mat(k,1166) - dti(k)
         mat(k,1179) = mat(k,1179) - dti(k)
         mat(k,1189) = mat(k,1189) - dti(k)
         mat(k,1205) = mat(k,1205) - dti(k)
         mat(k,1218) = mat(k,1218) - dti(k)
         mat(k,1229) = mat(k,1229) - dti(k)
         mat(k,1245) = mat(k,1245) - dti(k)
         mat(k,1263) = mat(k,1263) - dti(k)
         mat(k,1272) = mat(k,1272) - dti(k)
         mat(k,1278) = mat(k,1278) - dti(k)
         mat(k,1290) = mat(k,1290) - dti(k)
         mat(k,1307) = mat(k,1307) - dti(k)
         mat(k,1320) = mat(k,1320) - dti(k)
         mat(k,1328) = mat(k,1328) - dti(k)
         mat(k,1344) = mat(k,1344) - dti(k)
         mat(k,1360) = mat(k,1360) - dti(k)
         mat(k,1380) = mat(k,1380) - dti(k)
         mat(k,1396) = mat(k,1396) - dti(k)
         mat(k,1408) = mat(k,1408) - dti(k)
         mat(k,1426) = mat(k,1426) - dti(k)
         mat(k,1459) = mat(k,1459) - dti(k)
         mat(k,1483) = mat(k,1483) - dti(k)
         mat(k,1503) = mat(k,1503) - dti(k)
         mat(k,1524) = mat(k,1524) - dti(k)
         mat(k,1555) = mat(k,1555) - dti(k)
         mat(k,1577) = mat(k,1577) - dti(k)
         mat(k,1589) = mat(k,1589) - dti(k)
         mat(k,1603) = mat(k,1603) - dti(k)
         mat(k,1616) = mat(k,1616) - dti(k)
         mat(k,1631) = mat(k,1631) - dti(k)
         mat(k,1650) = mat(k,1650) - dti(k)
         mat(k,1671) = mat(k,1671) - dti(k)
         mat(k,1695) = mat(k,1695) - dti(k)
         mat(k,1748) = mat(k,1748) - dti(k)
         mat(k,1783) = mat(k,1783) - dti(k)
         mat(k,1806) = mat(k,1806) - dti(k)
         mat(k,1830) = mat(k,1830) - dti(k)
         mat(k,1875) = mat(k,1875) - dti(k)
         mat(k,1907) = mat(k,1907) - dti(k)
         mat(k,2014) = mat(k,2014) - dti(k)
         mat(k,2131) = mat(k,2131) - dti(k)
         mat(k,2315) = mat(k,2315) - dti(k)
         mat(k,2388) = mat(k,2388) - dti(k)
         mat(k,2419) = mat(k,2419) - dti(k)
         mat(k,2482) = mat(k,2482) - dti(k)
         mat(k,2509) = mat(k,2509) - dti(k)
         mat(k,2540) = mat(k,2540) - dti(k)
         mat(k,2571) = mat(k,2571) - dti(k)
         mat(k,2601) = mat(k,2601) - dti(k)
         mat(k,2649) = mat(k,2649) - dti(k)
         mat(k,2708) = mat(k,2708) - dti(k)
         mat(k,2778) = mat(k,2778) - dti(k)
         mat(k,2803) = mat(k,2803) - dti(k)
         mat(k,2837) = mat(k,2837) - dti(k)
         mat(k,2868) = mat(k,2868) - dti(k)
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
