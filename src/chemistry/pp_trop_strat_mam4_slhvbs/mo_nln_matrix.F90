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
         mat(k,757) = -(rxt(k,449)*y(k,248))
         mat(k,2103) = -rxt(k,449)*y(k,1)
         mat(k,2616) = rxt(k,452)*y(k,219)
         mat(k,995) = rxt(k,452)*y(k,153)
         mat(k,723) = -(rxt(k,453)*y(k,248))
         mat(k,2100) = -rxt(k,453)*y(k,2)
         mat(k,994) = rxt(k,450)*y(k,233)
         mat(k,1807) = rxt(k,450)*y(k,219)
         mat(k,1071) = -(rxt(k,532)*y(k,155) + rxt(k,533)*y(k,163) + rxt(k,534) &
                      *y(k,248))
         mat(k,2350) = -rxt(k,532)*y(k,6)
         mat(k,2536) = -rxt(k,533)*y(k,6)
         mat(k,2130) = -rxt(k,534)*y(k,6)
         mat(k,192) = -(rxt(k,491)*y(k,248))
         mat(k,2024) = -rxt(k,491)*y(k,7)
         mat(k,486) = -(rxt(k,494)*y(k,248))
         mat(k,2070) = -rxt(k,494)*y(k,8)
         mat(k,564) = rxt(k,492)*y(k,233)
         mat(k,1792) = rxt(k,492)*y(k,221)
         mat(k,193) = .120_r8*rxt(k,491)*y(k,248)
         mat(k,2025) = .120_r8*rxt(k,491)*y(k,7)
         mat(k,1065) = .100_r8*rxt(k,533)*y(k,163)
         mat(k,972) = .100_r8*rxt(k,536)*y(k,163)
         mat(k,2520) = .100_r8*rxt(k,533)*y(k,6) + .100_r8*rxt(k,536)*y(k,139)
         mat(k,2602) = .500_r8*rxt(k,493)*y(k,221) + .200_r8*rxt(k,520)*y(k,254) &
                      + .060_r8*rxt(k,526)*y(k,257)
         mat(k,565) = .500_r8*rxt(k,493)*y(k,153)
         mat(k,811) = .200_r8*rxt(k,520)*y(k,153)
         mat(k,835) = .060_r8*rxt(k,526)*y(k,153)
         mat(k,2596) = .200_r8*rxt(k,520)*y(k,254) + .200_r8*rxt(k,526)*y(k,257)
         mat(k,810) = .200_r8*rxt(k,520)*y(k,153)
         mat(k,833) = .200_r8*rxt(k,526)*y(k,153)
         mat(k,2612) = .200_r8*rxt(k,520)*y(k,254) + .150_r8*rxt(k,526)*y(k,257)
         mat(k,812) = .200_r8*rxt(k,520)*y(k,153)
         mat(k,836) = .150_r8*rxt(k,526)*y(k,153)
         mat(k,2598) = .210_r8*rxt(k,526)*y(k,257)
         mat(k,834) = .210_r8*rxt(k,526)*y(k,153)
         mat(k,281) = -(rxt(k,454)*y(k,248))
         mat(k,2040) = -rxt(k,454)*y(k,15)
         mat(k,1064) = .050_r8*rxt(k,533)*y(k,163)
         mat(k,971) = .050_r8*rxt(k,536)*y(k,163)
         mat(k,2519) = .050_r8*rxt(k,533)*y(k,6) + .050_r8*rxt(k,536)*y(k,139)
         mat(k,409) = -(rxt(k,420)*y(k,155) + rxt(k,421)*y(k,248))
         mat(k,2336) = -rxt(k,420)*y(k,16)
         mat(k,2059) = -rxt(k,421)*y(k,16)
         mat(k,2324) = -(rxt(k,243)*y(k,51) + rxt(k,244)*y(k,233) + rxt(k,245) &
                      *y(k,154) + rxt(k,246)*y(k,163) + rxt(k,253)*y(k,22) + rxt(k,282) &
                      *y(k,125))
         mat(k,1709) = -rxt(k,243)*y(k,17)
         mat(k,1870) = -rxt(k,244)*y(k,17)
         mat(k,2506) = -rxt(k,245)*y(k,17)
         mat(k,2572) = -rxt(k,246)*y(k,17)
         mat(k,905) = -rxt(k,253)*y(k,17)
         mat(k,2242) = -rxt(k,282)*y(k,17)
         mat(k,542) = rxt(k,242)*y(k,248)
         mat(k,2449) = 4.000_r8*rxt(k,247)*y(k,21) + (rxt(k,248)+rxt(k,249))*y(k,74) &
                      + rxt(k,555)*y(k,83) + rxt(k,272)*y(k,115) + (rxt(k,283) &
                       +rxt(k,284))*y(k,125) + rxt(k,252)*y(k,153) + rxt(k,257) &
                      *y(k,162) + rxt(k,566)*y(k,180) + rxt(k,258)*y(k,248)
         mat(k,175) = rxt(k,232)*y(k,247)
         mat(k,180) = rxt(k,262)*y(k,247)
         mat(k,555) = 2.000_r8*rxt(k,316)*y(k,70) + 2.000_r8*rxt(k,343)*y(k,247) &
                      + 2.000_r8*rxt(k,317)*y(k,248)
         mat(k,151) = rxt(k,318)*y(k,248)
         mat(k,670) = rxt(k,321)*y(k,70) + rxt(k,344)*y(k,247) + rxt(k,322)*y(k,248)
         mat(k,119) = 2.000_r8*rxt(k,328)*y(k,248)
         mat(k,497) = 3.000_r8*rxt(k,329)*y(k,70) + 3.000_r8*rxt(k,263)*y(k,247) &
                      + 3.000_r8*rxt(k,330)*y(k,248)
         mat(k,123) = rxt(k,331)*y(k,248)
         mat(k,1925) = 2.000_r8*rxt(k,316)*y(k,45) + rxt(k,321)*y(k,52) &
                      + 3.000_r8*rxt(k,329)*y(k,66)
         mat(k,2296) = (rxt(k,248)+rxt(k,249))*y(k,21)
         mat(k,1033) = rxt(k,555)*y(k,21)
         mat(k,127) = 2.000_r8*rxt(k,264)*y(k,247)
         mat(k,1525) = rxt(k,259)*y(k,162) + rxt(k,265)*y(k,247) + rxt(k,260)*y(k,248)
         mat(k,2422) = rxt(k,272)*y(k,21)
         mat(k,2242) = mat(k,2242) + (rxt(k,283)+rxt(k,284))*y(k,21)
         mat(k,2673) = rxt(k,252)*y(k,21)
         mat(k,2211) = rxt(k,257)*y(k,21) + rxt(k,259)*y(k,97)
         mat(k,1565) = rxt(k,566)*y(k,21)
         mat(k,1994) = rxt(k,232)*y(k,38) + rxt(k,262)*y(k,39) + 2.000_r8*rxt(k,343) &
                      *y(k,45) + rxt(k,344)*y(k,52) + 3.000_r8*rxt(k,263)*y(k,66) &
                      + 2.000_r8*rxt(k,264)*y(k,94) + rxt(k,265)*y(k,97)
         mat(k,2176) = rxt(k,242)*y(k,18) + rxt(k,258)*y(k,21) + 2.000_r8*rxt(k,317) &
                      *y(k,45) + rxt(k,318)*y(k,46) + rxt(k,322)*y(k,52) &
                      + 2.000_r8*rxt(k,328)*y(k,65) + 3.000_r8*rxt(k,330)*y(k,66) &
                      + rxt(k,331)*y(k,67) + rxt(k,260)*y(k,97)
         mat(k,539) = -(rxt(k,242)*y(k,248))
         mat(k,2076) = -rxt(k,242)*y(k,18)
         mat(k,2305) = rxt(k,253)*y(k,22)
         mat(k,897) = rxt(k,253)*y(k,17)
         mat(k,1514) = (rxt(k,583)+rxt(k,657)+rxt(k,670)+rxt(k,679))*y(k,108)
         mat(k,1666) = (rxt(k,583)+rxt(k,657)+rxt(k,670)+rxt(k,679))*y(k,97)
         mat(k,2432) = rxt(k,250)*y(k,74)
         mat(k,898) = rxt(k,254)*y(k,70)
         mat(k,1891) = rxt(k,254)*y(k,22)
         mat(k,2279) = rxt(k,250)*y(k,21)
         mat(k,1515) = (rxt(k,582)+rxt(k,659)+rxt(k,667)+rxt(k,676))*y(k,109)
         mat(k,1720) = (rxt(k,585)+rxt(k,656)+rxt(k,669)+rxt(k,678))*y(k,108)
         mat(k,1667) = (rxt(k,585)+rxt(k,656)+rxt(k,669)+rxt(k,678))*y(k,101)
         mat(k,1742) = (rxt(k,582)+rxt(k,659)+rxt(k,667)+rxt(k,676))*y(k,97)
         mat(k,2304) = rxt(k,245)*y(k,154)
         mat(k,2460) = rxt(k,245)*y(k,17)
         mat(k,2452) = -(4._r8*rxt(k,247)*y(k,21) + (rxt(k,248) + rxt(k,249) + rxt(k,250) &
                      ) * y(k,74) + rxt(k,251)*y(k,233) + rxt(k,252)*y(k,153) &
                      + rxt(k,255)*y(k,154) + rxt(k,257)*y(k,162) + rxt(k,258) &
                      *y(k,248) + rxt(k,272)*y(k,115) + (rxt(k,283) + rxt(k,284) &
                      ) * y(k,125) + rxt(k,555)*y(k,83) + rxt(k,566)*y(k,180))
         mat(k,2299) = -(rxt(k,248) + rxt(k,249) + rxt(k,250)) * y(k,21)
         mat(k,1873) = -rxt(k,251)*y(k,21)
         mat(k,2676) = -rxt(k,252)*y(k,21)
         mat(k,2509) = -rxt(k,255)*y(k,21)
         mat(k,2214) = -rxt(k,257)*y(k,21)
         mat(k,2179) = -rxt(k,258)*y(k,21)
         mat(k,2425) = -rxt(k,272)*y(k,21)
         mat(k,2245) = -(rxt(k,283) + rxt(k,284)) * y(k,21)
         mat(k,1036) = -rxt(k,555)*y(k,21)
         mat(k,1566) = -rxt(k,566)*y(k,21)
         mat(k,2327) = rxt(k,282)*y(k,125) + rxt(k,246)*y(k,163)
         mat(k,907) = rxt(k,256)*y(k,162)
         mat(k,1527) = rxt(k,266)*y(k,247)
         mat(k,1685) = rxt(k,261)*y(k,162)
         mat(k,2245) = mat(k,2245) + rxt(k,282)*y(k,17)
         mat(k,2214) = mat(k,2214) + rxt(k,256)*y(k,22) + rxt(k,261)*y(k,108)
         mat(k,2575) = rxt(k,246)*y(k,17)
         mat(k,1997) = rxt(k,266)*y(k,97)
         mat(k,899) = -(rxt(k,253)*y(k,17) + rxt(k,254)*y(k,70) + rxt(k,256)*y(k,162))
         mat(k,2307) = -rxt(k,253)*y(k,22)
         mat(k,1897) = -rxt(k,254)*y(k,22)
         mat(k,2187) = -rxt(k,256)*y(k,22)
         mat(k,2433) = rxt(k,255)*y(k,154)
         mat(k,2477) = rxt(k,255)*y(k,21)
         mat(k,284) = -(rxt(k,495)*y(k,248))
         mat(k,2041) = -rxt(k,495)*y(k,24)
         mat(k,2593) = rxt(k,498)*y(k,223)
         mat(k,510) = rxt(k,498)*y(k,153)
         mat(k,380) = -(rxt(k,497)*y(k,248))
         mat(k,2055) = -rxt(k,497)*y(k,25)
         mat(k,511) = rxt(k,496)*y(k,233)
         mat(k,1783) = rxt(k,496)*y(k,223)
         mat(k,220) = -(rxt(k,312)*y(k,70) + rxt(k,313)*y(k,248))
         mat(k,1879) = -rxt(k,312)*y(k,26)
         mat(k,2028) = -rxt(k,313)*y(k,26)
         mat(k,328) = -(rxt(k,369)*y(k,70) + rxt(k,370)*y(k,248))
         mat(k,1881) = -rxt(k,369)*y(k,27)
         mat(k,2048) = -rxt(k,370)*y(k,27)
         mat(k,639) = -(rxt(k,371)*y(k,70) + rxt(k,372)*y(k,163) + rxt(k,397)*y(k,248))
         mat(k,1892) = -rxt(k,371)*y(k,28)
         mat(k,2524) = -rxt(k,372)*y(k,28)
         mat(k,2089) = -rxt(k,397)*y(k,28)
         mat(k,290) = -(rxt(k,314)*y(k,70) + rxt(k,315)*y(k,248))
         mat(k,1880) = -rxt(k,314)*y(k,29)
         mat(k,2043) = -rxt(k,315)*y(k,29)
         mat(k,300) = -(rxt(k,377)*y(k,248))
         mat(k,2045) = -rxt(k,377)*y(k,30)
         mat(k,886) = .800_r8*rxt(k,373)*y(k,224) + .200_r8*rxt(k,374)*y(k,228)
         mat(k,1611) = .200_r8*rxt(k,374)*y(k,224)
         mat(k,385) = -(rxt(k,378)*y(k,248))
         mat(k,2056) = -rxt(k,378)*y(k,31)
         mat(k,887) = rxt(k,375)*y(k,233)
         mat(k,1784) = rxt(k,375)*y(k,224)
         mat(k,337) = -(rxt(k,379)*y(k,70) + rxt(k,380)*y(k,248))
         mat(k,1882) = -rxt(k,379)*y(k,32)
         mat(k,2049) = -rxt(k,380)*y(k,32)
         mat(k,1116) = -(rxt(k,400)*y(k,155) + rxt(k,401)*y(k,163) + rxt(k,418) &
                      *y(k,248))
         mat(k,2353) = -rxt(k,400)*y(k,33)
         mat(k,2538) = -rxt(k,401)*y(k,33)
         mat(k,2133) = -rxt(k,418)*y(k,33)
         mat(k,920) = .130_r8*rxt(k,478)*y(k,163)
         mat(k,2538) = mat(k,2538) + .130_r8*rxt(k,478)*y(k,127)
         mat(k,480) = -(rxt(k,405)*y(k,248))
         mat(k,2069) = -rxt(k,405)*y(k,34)
         mat(k,872) = rxt(k,403)*y(k,233)
         mat(k,1791) = rxt(k,403)*y(k,225)
         mat(k,132) = -(rxt(k,406)*y(k,248))
         mat(k,2020) = -rxt(k,406)*y(k,35)
         mat(k,304) = -(rxt(k,501)*y(k,248))
         mat(k,2046) = -rxt(k,501)*y(k,36)
         mat(k,714) = rxt(k,499)*y(k,233)
         mat(k,1778) = rxt(k,499)*y(k,226)
         mat(k,113) = -(rxt(k,231)*y(k,247))
         mat(k,1956) = -rxt(k,231)*y(k,37)
         mat(k,171) = -(rxt(k,232)*y(k,247))
         mat(k,1961) = -rxt(k,232)*y(k,38)
         mat(k,176) = -(rxt(k,262)*y(k,247))
         mat(k,1962) = -rxt(k,262)*y(k,39)
         mat(k,136) = -(rxt(k,233)*y(k,247))
         mat(k,1958) = -rxt(k,233)*y(k,40)
         mat(k,181) = -(rxt(k,234)*y(k,247))
         mat(k,1963) = -rxt(k,234)*y(k,41)
         mat(k,140) = -(rxt(k,235)*y(k,247))
         mat(k,1959) = -rxt(k,235)*y(k,42)
         mat(k,186) = -(rxt(k,236)*y(k,247))
         mat(k,1964) = -rxt(k,236)*y(k,43)
         mat(k,144) = -(rxt(k,237)*y(k,247))
         mat(k,1960) = -rxt(k,237)*y(k,44)
         mat(k,550) = -(rxt(k,316)*y(k,70) + rxt(k,317)*y(k,248) + rxt(k,343)*y(k,247))
         mat(k,1889) = -rxt(k,316)*y(k,45)
         mat(k,2078) = -rxt(k,317)*y(k,45)
         mat(k,1973) = -rxt(k,343)*y(k,45)
         mat(k,148) = -(rxt(k,318)*y(k,248))
         mat(k,2021) = -rxt(k,318)*y(k,46)
         mat(k,343) = -(rxt(k,319)*y(k,70) + rxt(k,320)*y(k,248))
         mat(k,1883) = -rxt(k,319)*y(k,47)
         mat(k,2050) = -rxt(k,320)*y(k,47)
         mat(k,1697) = -(rxt(k,204)*y(k,70) + rxt(k,243)*y(k,17) + rxt(k,348)*y(k,233) &
                      + rxt(k,349)*y(k,155) + rxt(k,350)*y(k,162) + rxt(k,351) &
                      *y(k,248))
         mat(k,1913) = -rxt(k,204)*y(k,51)
         mat(k,2312) = -rxt(k,243)*y(k,51)
         mat(k,1858) = -rxt(k,348)*y(k,51)
         mat(k,2382) = -rxt(k,349)*y(k,51)
         mat(k,2199) = -rxt(k,350)*y(k,51)
         mat(k,2164) = -rxt(k,351)*y(k,51)
         mat(k,763) = .400_r8*rxt(k,449)*y(k,248)
         mat(k,1082) = .340_r8*rxt(k,533)*y(k,163)
         mat(k,413) = .500_r8*rxt(k,420)*y(k,155)
         mat(k,643) = rxt(k,372)*y(k,163)
         mat(k,1124) = .500_r8*rxt(k,401)*y(k,163)
         mat(k,696) = .500_r8*rxt(k,389)*y(k,248)
         mat(k,869) = rxt(k,356)*y(k,248)
         mat(k,476) = .300_r8*rxt(k,357)*y(k,248)
         mat(k,1578) = (rxt(k,365)+rxt(k,366))*y(k,247)
         mat(k,1103) = rxt(k,332)*y(k,228)
         mat(k,2284) = rxt(k,213)*y(k,228)
         mat(k,1162) = .800_r8*rxt(k,394)*y(k,248)
         mat(k,929) = .910_r8*rxt(k,478)*y(k,163)
         mat(k,687) = .300_r8*rxt(k,469)*y(k,248)
         mat(k,1341) = .120_r8*rxt(k,431)*y(k,163)
         mat(k,703) = .500_r8*rxt(k,444)*y(k,248)
         mat(k,987) = .340_r8*rxt(k,536)*y(k,163)
         mat(k,1451) = .600_r8*rxt(k,445)*y(k,163)
         mat(k,2661) = .100_r8*rxt(k,451)*y(k,219) + rxt(k,355)*y(k,228) &
                      + .500_r8*rxt(k,422)*y(k,230) + .500_r8*rxt(k,391)*y(k,232) &
                      + .920_r8*rxt(k,461)*y(k,235) + .250_r8*rxt(k,429)*y(k,240) &
                      + rxt(k,438)*y(k,242) + rxt(k,412)*y(k,250) + rxt(k,416) &
                      *y(k,251) + .340_r8*rxt(k,545)*y(k,252) + .320_r8*rxt(k,550) &
                      *y(k,253) + .250_r8*rxt(k,486)*y(k,256)
         mat(k,2382) = mat(k,2382) + .500_r8*rxt(k,420)*y(k,16) + rxt(k,462)*y(k,235) &
                      + .250_r8*rxt(k,428)*y(k,240) + rxt(k,439)*y(k,242)
         mat(k,2560) = .340_r8*rxt(k,533)*y(k,6) + rxt(k,372)*y(k,28) &
                      + .500_r8*rxt(k,401)*y(k,33) + .910_r8*rxt(k,478)*y(k,127) &
                      + .120_r8*rxt(k,431)*y(k,134) + .340_r8*rxt(k,536)*y(k,139) &
                      + .600_r8*rxt(k,445)*y(k,140)
         mat(k,627) = rxt(k,396)*y(k,248)
         mat(k,1189) = .680_r8*rxt(k,554)*y(k,248)
         mat(k,1003) = .100_r8*rxt(k,451)*y(k,153)
         mat(k,892) = .700_r8*rxt(k,374)*y(k,228)
         mat(k,877) = rxt(k,402)*y(k,228)
         mat(k,1503) = rxt(k,385)*y(k,228) + rxt(k,458)*y(k,235) + .250_r8*rxt(k,425) &
                      *y(k,240) + rxt(k,434)*y(k,242) + .250_r8*rxt(k,483)*y(k,256)
         mat(k,1649) = rxt(k,332)*y(k,68) + rxt(k,213)*y(k,74) + rxt(k,355)*y(k,153) &
                      + .700_r8*rxt(k,374)*y(k,224) + rxt(k,402)*y(k,225) + rxt(k,385) &
                      *y(k,227) + (4.000_r8*rxt(k,352)+2.000_r8*rxt(k,353))*y(k,228) &
                      + 1.500_r8*rxt(k,459)*y(k,235) + .750_r8*rxt(k,464)*y(k,236) &
                      + .800_r8*rxt(k,473)*y(k,237) + .880_r8*rxt(k,426)*y(k,240) &
                      + 2.000_r8*rxt(k,435)*y(k,242) + .750_r8*rxt(k,538)*y(k,246) &
                      + .800_r8*rxt(k,414)*y(k,251) + .930_r8*rxt(k,543)*y(k,252) &
                      + .950_r8*rxt(k,548)*y(k,253) + .800_r8*rxt(k,484)*y(k,256)
         mat(k,651) = .500_r8*rxt(k,422)*y(k,153)
         mat(k,799) = .500_r8*rxt(k,391)*y(k,153)
         mat(k,1858) = mat(k,1858) + .450_r8*rxt(k,436)*y(k,242) + .150_r8*rxt(k,415) &
                      *y(k,251)
         mat(k,1374) = .920_r8*rxt(k,461)*y(k,153) + rxt(k,462)*y(k,155) + rxt(k,458) &
                      *y(k,227) + 1.500_r8*rxt(k,459)*y(k,228)
         mat(k,1407) = .750_r8*rxt(k,464)*y(k,228)
         mat(k,1326) = .800_r8*rxt(k,473)*y(k,228)
         mat(k,1429) = .250_r8*rxt(k,429)*y(k,153) + .250_r8*rxt(k,428)*y(k,155) &
                      + .250_r8*rxt(k,425)*y(k,227) + .880_r8*rxt(k,426)*y(k,228)
         mat(k,1471) = rxt(k,438)*y(k,153) + rxt(k,439)*y(k,155) + rxt(k,434)*y(k,227) &
                      + 2.000_r8*rxt(k,435)*y(k,228) + .450_r8*rxt(k,436)*y(k,233) &
                      + 4.000_r8*rxt(k,437)*y(k,242)
         mat(k,1175) = .750_r8*rxt(k,538)*y(k,228)
         mat(k,1982) = (rxt(k,365)+rxt(k,366))*y(k,64)
         mat(k,2164) = mat(k,2164) + .400_r8*rxt(k,449)*y(k,1) + .500_r8*rxt(k,389) &
                      *y(k,60) + rxt(k,356)*y(k,62) + .300_r8*rxt(k,357)*y(k,63) &
                      + .800_r8*rxt(k,394)*y(k,90) + .300_r8*rxt(k,469)*y(k,128) &
                      + .500_r8*rxt(k,444)*y(k,138) + rxt(k,396)*y(k,169) &
                      + .680_r8*rxt(k,554)*y(k,208)
         mat(k,862) = rxt(k,412)*y(k,153)
         mat(k,1272) = rxt(k,416)*y(k,153) + .800_r8*rxt(k,414)*y(k,228) &
                      + .150_r8*rxt(k,415)*y(k,233)
         mat(k,1235) = .340_r8*rxt(k,545)*y(k,153) + .930_r8*rxt(k,543)*y(k,228)
         mat(k,1256) = .320_r8*rxt(k,550)*y(k,153) + .950_r8*rxt(k,548)*y(k,228)
         mat(k,1304) = .250_r8*rxt(k,486)*y(k,153) + .250_r8*rxt(k,483)*y(k,227) &
                      + .800_r8*rxt(k,484)*y(k,228)
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
         mat(k,663) = -(rxt(k,321)*y(k,70) + rxt(k,322)*y(k,248) + rxt(k,344)*y(k,247))
         mat(k,1893) = -rxt(k,321)*y(k,52)
         mat(k,2092) = -rxt(k,322)*y(k,52)
         mat(k,1974) = -rxt(k,344)*y(k,52)
         mat(k,152) = -(rxt(k,323)*y(k,248))
         mat(k,2022) = -rxt(k,323)*y(k,53)
         mat(k,1149) = -(rxt(k,381)*y(k,155) + rxt(k,382)*y(k,248))
         mat(k,2355) = -rxt(k,381)*y(k,54)
         mat(k,2135) = -rxt(k,382)*y(k,54)
         mat(k,761) = .800_r8*rxt(k,449)*y(k,248)
         mat(k,412) = rxt(k,420)*y(k,155)
         mat(k,301) = rxt(k,377)*y(k,248)
         mat(k,387) = .500_r8*rxt(k,378)*y(k,248)
         mat(k,1117) = .500_r8*rxt(k,401)*y(k,163)
         mat(k,1441) = .100_r8*rxt(k,445)*y(k,163)
         mat(k,2636) = .400_r8*rxt(k,451)*y(k,219) + rxt(k,376)*y(k,224) &
                      + .270_r8*rxt(k,404)*y(k,225) + rxt(k,422)*y(k,230) + rxt(k,441) &
                      *y(k,244) + rxt(k,412)*y(k,250)
         mat(k,2355) = mat(k,2355) + rxt(k,420)*y(k,16)
         mat(k,2539) = .500_r8*rxt(k,401)*y(k,33) + .100_r8*rxt(k,445)*y(k,140)
         mat(k,1000) = .400_r8*rxt(k,451)*y(k,153)
         mat(k,890) = rxt(k,376)*y(k,153) + 3.200_r8*rxt(k,373)*y(k,224) &
                      + .800_r8*rxt(k,374)*y(k,228)
         mat(k,875) = .270_r8*rxt(k,404)*y(k,153)
         mat(k,1628) = .800_r8*rxt(k,374)*y(k,224)
         mat(k,649) = rxt(k,422)*y(k,153)
         mat(k,1831) = .200_r8*rxt(k,440)*y(k,244)
         mat(k,769) = rxt(k,441)*y(k,153) + .200_r8*rxt(k,440)*y(k,233)
         mat(k,2135) = mat(k,2135) + .800_r8*rxt(k,449)*y(k,1) + rxt(k,377)*y(k,30) &
                      + .500_r8*rxt(k,378)*y(k,31)
         mat(k,860) = rxt(k,412)*y(k,153)
         mat(k,436) = -(rxt(k,324)*y(k,70) + rxt(k,325)*y(k,248))
         mat(k,1887) = -rxt(k,324)*y(k,55)
         mat(k,2062) = -rxt(k,325)*y(k,55)
         mat(k,107) = -(rxt(k,383)*y(k,248))
         mat(k,2016) = -rxt(k,383)*y(k,56)
         mat(k,1019) = -(rxt(k,419)*y(k,248))
         mat(k,2125) = -rxt(k,419)*y(k,57)
         mat(k,760) = .800_r8*rxt(k,449)*y(k,248)
         mat(k,1068) = .520_r8*rxt(k,533)*y(k,163)
         mat(k,411) = .500_r8*rxt(k,420)*y(k,155)
         mat(k,976) = .520_r8*rxt(k,536)*y(k,163)
         mat(k,2630) = .250_r8*rxt(k,451)*y(k,219) + .820_r8*rxt(k,404)*y(k,225) &
                      + .500_r8*rxt(k,422)*y(k,230) + .270_r8*rxt(k,545)*y(k,252) &
                      + .040_r8*rxt(k,550)*y(k,253)
         mat(k,2345) = .500_r8*rxt(k,420)*y(k,16)
         mat(k,2532) = .520_r8*rxt(k,533)*y(k,6) + .520_r8*rxt(k,536)*y(k,139)
         mat(k,1183) = .500_r8*rxt(k,554)*y(k,248)
         mat(k,999) = .250_r8*rxt(k,451)*y(k,153)
         mat(k,874) = .820_r8*rxt(k,404)*y(k,153) + .820_r8*rxt(k,402)*y(k,228)
         mat(k,1623) = .820_r8*rxt(k,402)*y(k,225) + .150_r8*rxt(k,543)*y(k,252) &
                      + .025_r8*rxt(k,548)*y(k,253)
         mat(k,648) = .500_r8*rxt(k,422)*y(k,153)
         mat(k,2125) = mat(k,2125) + .800_r8*rxt(k,449)*y(k,1) + .500_r8*rxt(k,554) &
                      *y(k,208)
         mat(k,1227) = .270_r8*rxt(k,545)*y(k,153) + .150_r8*rxt(k,543)*y(k,228)
         mat(k,1246) = .040_r8*rxt(k,550)*y(k,153) + .025_r8*rxt(k,548)*y(k,228)
         mat(k,1348) = -(rxt(k,407)*y(k,155) + rxt(k,408)*y(k,248))
         mat(k,2369) = -rxt(k,407)*y(k,58)
         mat(k,2149) = -rxt(k,408)*y(k,58)
         mat(k,1218) = rxt(k,409)*y(k,248)
         mat(k,1337) = .880_r8*rxt(k,431)*y(k,163)
         mat(k,1444) = .500_r8*rxt(k,445)*y(k,163)
         mat(k,2649) = .170_r8*rxt(k,504)*y(k,229) + .050_r8*rxt(k,467)*y(k,236) &
                      + .250_r8*rxt(k,429)*y(k,240) + .170_r8*rxt(k,510)*y(k,243) &
                      + .400_r8*rxt(k,520)*y(k,254) + .250_r8*rxt(k,486)*y(k,256) &
                      + .540_r8*rxt(k,526)*y(k,257) + .510_r8*rxt(k,529)*y(k,259)
         mat(k,2369) = mat(k,2369) + .050_r8*rxt(k,468)*y(k,236) + .250_r8*rxt(k,428) &
                      *y(k,240) + .250_r8*rxt(k,487)*y(k,256)
         mat(k,910) = rxt(k,410)*y(k,248)
         mat(k,2550) = .880_r8*rxt(k,431)*y(k,134) + .500_r8*rxt(k,445)*y(k,140)
         mat(k,1494) = .250_r8*rxt(k,425)*y(k,240) + .250_r8*rxt(k,483)*y(k,256)
         mat(k,1640) = .240_r8*rxt(k,426)*y(k,240) + .500_r8*rxt(k,414)*y(k,251) &
                      + .100_r8*rxt(k,484)*y(k,256)
         mat(k,852) = .170_r8*rxt(k,504)*y(k,153) + .070_r8*rxt(k,503)*y(k,233)
         mat(k,1844) = .070_r8*rxt(k,503)*y(k,229) + .070_r8*rxt(k,509)*y(k,243)
         mat(k,1400) = .050_r8*rxt(k,467)*y(k,153) + .050_r8*rxt(k,468)*y(k,155)
         mat(k,1424) = .250_r8*rxt(k,429)*y(k,153) + .250_r8*rxt(k,428)*y(k,155) &
                      + .250_r8*rxt(k,425)*y(k,227) + .240_r8*rxt(k,426)*y(k,228)
         mat(k,959) = .170_r8*rxt(k,510)*y(k,153) + .070_r8*rxt(k,509)*y(k,233)
         mat(k,2149) = mat(k,2149) + rxt(k,409)*y(k,113) + rxt(k,410)*y(k,156)
         mat(k,1269) = .500_r8*rxt(k,414)*y(k,228)
         mat(k,820) = .400_r8*rxt(k,520)*y(k,153)
         mat(k,1301) = .250_r8*rxt(k,486)*y(k,153) + .250_r8*rxt(k,487)*y(k,155) &
                      + .250_r8*rxt(k,483)*y(k,227) + .100_r8*rxt(k,484)*y(k,228)
         mat(k,844) = .540_r8*rxt(k,526)*y(k,153)
         mat(k,584) = .510_r8*rxt(k,529)*y(k,153)
         mat(k,775) = -(rxt(k,388)*y(k,248))
         mat(k,2105) = -rxt(k,388)*y(k,59)
         mat(k,1111) = .120_r8*rxt(k,401)*y(k,163)
         mat(k,2526) = .120_r8*rxt(k,401)*y(k,33)
         mat(k,1484) = .100_r8*rxt(k,385)*y(k,228) + .150_r8*rxt(k,386)*y(k,233)
         mat(k,1617) = .100_r8*rxt(k,385)*y(k,227)
         mat(k,1811) = .150_r8*rxt(k,386)*y(k,227) + .150_r8*rxt(k,436)*y(k,242)
         mat(k,1463) = .150_r8*rxt(k,436)*y(k,233)
         mat(k,692) = -(rxt(k,389)*y(k,248))
         mat(k,2096) = -rxt(k,389)*y(k,60)
         mat(k,1483) = .400_r8*rxt(k,386)*y(k,233)
         mat(k,1805) = .400_r8*rxt(k,386)*y(k,227) + .400_r8*rxt(k,436)*y(k,242)
         mat(k,1461) = .400_r8*rxt(k,436)*y(k,233)
         mat(k,428) = -(rxt(k,326)*y(k,70) + rxt(k,327)*y(k,248))
         mat(k,1886) = -rxt(k,326)*y(k,61)
         mat(k,2061) = -rxt(k,327)*y(k,61)
         mat(k,868) = -(rxt(k,356)*y(k,248))
         mat(k,2114) = -rxt(k,356)*y(k,62)
         mat(k,888) = .300_r8*rxt(k,374)*y(k,228)
         mat(k,1618) = .300_r8*rxt(k,374)*y(k,224) + 2.000_r8*rxt(k,353)*y(k,228) &
                      + .250_r8*rxt(k,459)*y(k,235) + .250_r8*rxt(k,464)*y(k,236) &
                      + .200_r8*rxt(k,473)*y(k,237) + .250_r8*rxt(k,426)*y(k,240) &
                      + .250_r8*rxt(k,538)*y(k,246) + .500_r8*rxt(k,414)*y(k,251) &
                      + .250_r8*rxt(k,543)*y(k,252) + .250_r8*rxt(k,548)*y(k,253) &
                      + .300_r8*rxt(k,484)*y(k,256)
         mat(k,1358) = .250_r8*rxt(k,459)*y(k,228)
         mat(k,1389) = .250_r8*rxt(k,464)*y(k,228)
         mat(k,1314) = .200_r8*rxt(k,473)*y(k,228)
         mat(k,1418) = .250_r8*rxt(k,426)*y(k,228)
         mat(k,1168) = .250_r8*rxt(k,538)*y(k,228)
         mat(k,1266) = .500_r8*rxt(k,414)*y(k,228)
         mat(k,1225) = .250_r8*rxt(k,543)*y(k,228)
         mat(k,1245) = .250_r8*rxt(k,548)*y(k,228)
         mat(k,1294) = .300_r8*rxt(k,484)*y(k,228)
         mat(k,474) = -(rxt(k,357)*y(k,248))
         mat(k,2068) = -rxt(k,357)*y(k,63)
         mat(k,1615) = rxt(k,354)*y(k,233)
         mat(k,1790) = rxt(k,354)*y(k,228)
         mat(k,1576) = -(rxt(k,205)*y(k,70) + rxt(k,306)*y(k,89) + rxt(k,358)*y(k,248) &
                      + (rxt(k,364) + rxt(k,365) + rxt(k,366)) * y(k,247))
         mat(k,1910) = -rxt(k,205)*y(k,64)
         mat(k,949) = -rxt(k,306)*y(k,64)
         mat(k,2160) = -rxt(k,358)*y(k,64)
         mat(k,1978) = -(rxt(k,364) + rxt(k,365) + rxt(k,366)) * y(k,64)
         mat(k,1122) = .100_r8*rxt(k,401)*y(k,163)
         mat(k,2558) = .100_r8*rxt(k,401)*y(k,33)
         mat(k,116) = -(rxt(k,328)*y(k,248))
         mat(k,2018) = -rxt(k,328)*y(k,65)
         mat(k,492) = -(rxt(k,263)*y(k,247) + rxt(k,329)*y(k,70) + rxt(k,330)*y(k,248))
         mat(k,1972) = -rxt(k,263)*y(k,66)
         mat(k,1888) = -rxt(k,329)*y(k,66)
         mat(k,2071) = -rxt(k,330)*y(k,66)
         mat(k,120) = -(rxt(k,331)*y(k,248))
         mat(k,2019) = -rxt(k,331)*y(k,67)
         mat(k,1100) = -((rxt(k,332) + rxt(k,333)) * y(k,228) + (rxt(k,334) + rxt(k,335) &
                      ) * y(k,233) + rxt(k,336)*y(k,153) + rxt(k,337)*y(k,155))
         mat(k,1626) = -(rxt(k,332) + rxt(k,333)) * y(k,68)
         mat(k,1830) = -(rxt(k,334) + rxt(k,335)) * y(k,68)
         mat(k,2634) = -rxt(k,336)*y(k,68)
         mat(k,2352) = -rxt(k,337)*y(k,68)
         mat(k,344) = rxt(k,319)*y(k,70) + rxt(k,320)*y(k,248)
         mat(k,1903) = rxt(k,319)*y(k,47)
         mat(k,2132) = rxt(k,320)*y(k,47)
         mat(k,394) = -(rxt(k,338)*y(k,70) + rxt(k,339)*y(k,248))
         mat(k,1885) = -rxt(k,338)*y(k,69)
         mat(k,2058) = -rxt(k,339)*y(k,69)
         mat(k,1917) = -(rxt(k,204)*y(k,51) + rxt(k,205)*y(k,64) + rxt(k,206)*y(k,93) &
                      + rxt(k,207)*y(k,95) + (rxt(k,208) + rxt(k,209)) * y(k,233) &
                      + rxt(k,210)*y(k,154) + rxt(k,212)*y(k,163) + rxt(k,219)*y(k,75) &
                      + rxt(k,228)*y(k,109) + rxt(k,254)*y(k,22) + rxt(k,312)*y(k,26) &
                      + rxt(k,314)*y(k,29) + rxt(k,316)*y(k,45) + rxt(k,319)*y(k,47) &
                      + rxt(k,321)*y(k,52) + rxt(k,324)*y(k,55) + rxt(k,326)*y(k,61) &
                      + rxt(k,329)*y(k,66) + rxt(k,379)*y(k,32) + (rxt(k,556) &
                      + rxt(k,557)) * y(k,83))
         mat(k,1701) = -rxt(k,204)*y(k,70)
         mat(k,1581) = -rxt(k,205)*y(k,70)
         mat(k,1547) = -rxt(k,206)*y(k,70)
         mat(k,675) = -rxt(k,207)*y(k,70)
         mat(k,1862) = -(rxt(k,208) + rxt(k,209)) * y(k,70)
         mat(k,2498) = -rxt(k,210)*y(k,70)
         mat(k,2564) = -rxt(k,212)*y(k,70)
         mat(k,1012) = -rxt(k,219)*y(k,70)
         mat(k,1750) = -rxt(k,228)*y(k,70)
         mat(k,901) = -rxt(k,254)*y(k,70)
         mat(k,222) = -rxt(k,312)*y(k,70)
         mat(k,292) = -rxt(k,314)*y(k,70)
         mat(k,552) = -rxt(k,316)*y(k,70)
         mat(k,346) = -rxt(k,319)*y(k,70)
         mat(k,667) = -rxt(k,321)*y(k,70)
         mat(k,441) = -rxt(k,324)*y(k,70)
         mat(k,432) = -rxt(k,326)*y(k,70)
         mat(k,494) = -rxt(k,329)*y(k,70)
         mat(k,340) = -rxt(k,379)*y(k,70)
         mat(k,1029) = -(rxt(k,556) + rxt(k,557)) * y(k,70)
         mat(k,2441) = rxt(k,249)*y(k,74)
         mat(k,222) = mat(k,222) + 5.000_r8*rxt(k,312)*y(k,70) + 3.060_r8*rxt(k,313) &
                      *y(k,248)
         mat(k,292) = mat(k,292) + 2.000_r8*rxt(k,314)*y(k,70) + 2.000_r8*rxt(k,315) &
                      *y(k,248)
         mat(k,114) = 4.000_r8*rxt(k,231)*y(k,247)
         mat(k,173) = rxt(k,232)*y(k,247)
         mat(k,138) = 2.000_r8*rxt(k,233)*y(k,247)
         mat(k,184) = 2.000_r8*rxt(k,234)*y(k,247)
         mat(k,142) = 2.000_r8*rxt(k,235)*y(k,247)
         mat(k,189) = rxt(k,236)*y(k,247)
         mat(k,146) = 2.000_r8*rxt(k,237)*y(k,247)
         mat(k,149) = rxt(k,318)*y(k,248)
         mat(k,153) = 3.000_r8*rxt(k,323)*y(k,248)
         mat(k,441) = mat(k,441) + rxt(k,325)*y(k,248)
         mat(k,117) = rxt(k,328)*y(k,248)
         mat(k,121) = 2.000_r8*rxt(k,331)*y(k,248)
         mat(k,1106) = 2.000_r8*rxt(k,336)*y(k,153) + 2.000_r8*rxt(k,337)*y(k,155) &
                      + 2.000_r8*rxt(k,332)*y(k,228) + rxt(k,335)*y(k,233)
         mat(k,398) = rxt(k,339)*y(k,248)
         mat(k,1917) = mat(k,1917) + 5.000_r8*rxt(k,312)*y(k,26) + 2.000_r8*rxt(k,314) &
                      *y(k,29)
         mat(k,2288) = rxt(k,249)*y(k,21) + (4.000_r8*rxt(k,214)+2.000_r8*rxt(k,216)) &
                      *y(k,74) + rxt(k,286)*y(k,125) + rxt(k,218)*y(k,153) &
                      + rxt(k,223)*y(k,162) + rxt(k,567)*y(k,180) + rxt(k,213) &
                      *y(k,228) + rxt(k,224)*y(k,248)
         mat(k,266) = rxt(k,311)*y(k,247)
         mat(k,261) = rxt(k,345)*y(k,247) + rxt(k,340)*y(k,248)
         mat(k,270) = rxt(k,346)*y(k,247) + rxt(k,341)*y(k,248)
         mat(k,321) = rxt(k,347)*y(k,247) + rxt(k,342)*y(k,248)
         mat(k,1726) = rxt(k,226)*y(k,162) + rxt(k,238)*y(k,247) + rxt(k,227)*y(k,248)
         mat(k,2234) = rxt(k,286)*y(k,74)
         mat(k,2665) = 2.000_r8*rxt(k,336)*y(k,68) + rxt(k,218)*y(k,74)
         mat(k,2386) = 2.000_r8*rxt(k,337)*y(k,68)
         mat(k,2203) = rxt(k,223)*y(k,74) + rxt(k,226)*y(k,101)
         mat(k,1560) = rxt(k,567)*y(k,74)
         mat(k,1653) = 2.000_r8*rxt(k,332)*y(k,68) + rxt(k,213)*y(k,74)
         mat(k,1862) = mat(k,1862) + rxt(k,335)*y(k,68)
         mat(k,1986) = 4.000_r8*rxt(k,231)*y(k,37) + rxt(k,232)*y(k,38) &
                      + 2.000_r8*rxt(k,233)*y(k,40) + 2.000_r8*rxt(k,234)*y(k,41) &
                      + 2.000_r8*rxt(k,235)*y(k,42) + rxt(k,236)*y(k,43) &
                      + 2.000_r8*rxt(k,237)*y(k,44) + rxt(k,311)*y(k,81) + rxt(k,345) &
                      *y(k,98) + rxt(k,346)*y(k,99) + rxt(k,347)*y(k,100) + rxt(k,238) &
                      *y(k,101)
         mat(k,2168) = 3.060_r8*rxt(k,313)*y(k,26) + 2.000_r8*rxt(k,315)*y(k,29) &
                      + rxt(k,318)*y(k,46) + 3.000_r8*rxt(k,323)*y(k,53) + rxt(k,325) &
                      *y(k,55) + rxt(k,328)*y(k,65) + 2.000_r8*rxt(k,331)*y(k,67) &
                      + rxt(k,339)*y(k,69) + rxt(k,224)*y(k,74) + rxt(k,340)*y(k,98) &
                      + rxt(k,341)*y(k,99) + rxt(k,342)*y(k,100) + rxt(k,227)*y(k,101)
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
         mat(k,1878) = rxt(k,219)*y(k,75)
         mat(k,2276) = 2.000_r8*rxt(k,215)*y(k,74)
         mat(k,1008) = rxt(k,219)*y(k,70) + (rxt(k,665)+rxt(k,674)+rxt(k,683)) &
                      *y(k,101)
         mat(k,1717) = (rxt(k,665)+rxt(k,674)+rxt(k,683))*y(k,75) + (rxt(k,584) &
                       +rxt(k,655)+rxt(k,666)+rxt(k,675))*y(k,109)
         mat(k,1740) = (rxt(k,584)+rxt(k,655)+rxt(k,666)+rxt(k,675))*y(k,101)
         mat(k,2275) = 2.000_r8*rxt(k,240)*y(k,74)
         mat(k,590) = -(rxt(k,211)*y(k,248))
         mat(k,2083) = -rxt(k,211)*y(k,73)
         mat(k,1890) = rxt(k,210)*y(k,154)
         mat(k,1719) = rxt(k,600)*y(k,143)
         mat(k,402) = rxt(k,600)*y(k,101)
         mat(k,2468) = rxt(k,210)*y(k,70)
         mat(k,2295) = -(rxt(k,213)*y(k,228) + (4._r8*rxt(k,214) + 4._r8*rxt(k,215) &
                      + 4._r8*rxt(k,216) + 4._r8*rxt(k,240)) * y(k,74) + rxt(k,217) &
                      *y(k,233) + rxt(k,218)*y(k,153) + rxt(k,220)*y(k,154) + rxt(k,223) &
                      *y(k,162) + (rxt(k,224) + rxt(k,225)) * y(k,248) + (rxt(k,248) &
                      + rxt(k,249) + rxt(k,250)) * y(k,21) + (rxt(k,285) + rxt(k,286) &
                      + rxt(k,287)) * y(k,125) + rxt(k,567)*y(k,180))
         mat(k,1659) = -rxt(k,213)*y(k,74)
         mat(k,1869) = -rxt(k,217)*y(k,74)
         mat(k,2672) = -rxt(k,218)*y(k,74)
         mat(k,2505) = -rxt(k,220)*y(k,74)
         mat(k,2210) = -rxt(k,223)*y(k,74)
         mat(k,2175) = -(rxt(k,224) + rxt(k,225)) * y(k,74)
         mat(k,2448) = -(rxt(k,248) + rxt(k,249) + rxt(k,250)) * y(k,74)
         mat(k,2241) = -(rxt(k,285) + rxt(k,286) + rxt(k,287)) * y(k,74)
         mat(k,1564) = -rxt(k,567)*y(k,74)
         mat(k,1924) = rxt(k,228)*y(k,109) + rxt(k,212)*y(k,163) + rxt(k,209)*y(k,233)
         mat(k,1016) = rxt(k,221)*y(k,162)
         mat(k,1733) = rxt(k,239)*y(k,247)
         mat(k,1757) = rxt(k,228)*y(k,70) + rxt(k,229)*y(k,162) + rxt(k,230)*y(k,248)
         mat(k,2210) = mat(k,2210) + rxt(k,221)*y(k,75) + rxt(k,229)*y(k,109)
         mat(k,2571) = rxt(k,212)*y(k,70)
         mat(k,535) = rxt(k,572)*y(k,180)
         mat(k,1564) = mat(k,1564) + rxt(k,572)*y(k,165)
         mat(k,1869) = mat(k,1869) + rxt(k,209)*y(k,70)
         mat(k,1993) = rxt(k,239)*y(k,101)
         mat(k,2175) = mat(k,2175) + rxt(k,230)*y(k,109)
         mat(k,1009) = -(rxt(k,219)*y(k,70) + rxt(k,221)*y(k,162) + rxt(k,222) &
                      *y(k,248) + (rxt(k,665) + rxt(k,674) + rxt(k,683)) * y(k,101))
         mat(k,1898) = -rxt(k,219)*y(k,75)
         mat(k,2188) = -rxt(k,221)*y(k,75)
         mat(k,2124) = -rxt(k,222)*y(k,75)
         mat(k,1721) = -(rxt(k,665) + rxt(k,674) + rxt(k,683)) * y(k,75)
         mat(k,2280) = rxt(k,220)*y(k,154)
         mat(k,2480) = rxt(k,220)*y(k,74)
         mat(k,1195) = -(rxt(k,368)*y(k,248))
         mat(k,2139) = -rxt(k,368)*y(k,77)
         mat(k,1076) = .230_r8*rxt(k,533)*y(k,163)
         mat(k,2308) = rxt(k,243)*y(k,51)
         mat(k,331) = .350_r8*rxt(k,370)*y(k,248)
         mat(k,642) = .630_r8*rxt(k,372)*y(k,163)
         mat(k,1118) = .560_r8*rxt(k,401)*y(k,163)
         mat(k,1690) = rxt(k,243)*y(k,17) + rxt(k,204)*y(k,70) + rxt(k,349)*y(k,155) &
                      + rxt(k,350)*y(k,162) + rxt(k,351)*y(k,248)
         mat(k,437) = rxt(k,324)*y(k,70)
         mat(k,1347) = rxt(k,407)*y(k,155) + rxt(k,408)*y(k,248)
         mat(k,1101) = rxt(k,336)*y(k,153) + rxt(k,337)*y(k,155) + (rxt(k,332) &
                       +rxt(k,333))*y(k,228) + rxt(k,335)*y(k,233)
         mat(k,1906) = rxt(k,204)*y(k,51) + rxt(k,324)*y(k,55)
         mat(k,1054) = rxt(k,395)*y(k,248)
         mat(k,921) = .620_r8*rxt(k,478)*y(k,163)
         mat(k,1335) = .650_r8*rxt(k,431)*y(k,163)
         mat(k,982) = .230_r8*rxt(k,536)*y(k,163)
         mat(k,1442) = .560_r8*rxt(k,445)*y(k,163)
         mat(k,2640) = rxt(k,336)*y(k,68) + .170_r8*rxt(k,504)*y(k,229) &
                      + .220_r8*rxt(k,429)*y(k,240) + .400_r8*rxt(k,507)*y(k,241) &
                      + .350_r8*rxt(k,510)*y(k,243) + .225_r8*rxt(k,545)*y(k,252) &
                      + .250_r8*rxt(k,486)*y(k,256)
         mat(k,2359) = rxt(k,349)*y(k,51) + rxt(k,407)*y(k,58) + rxt(k,337)*y(k,68) &
                      + .220_r8*rxt(k,428)*y(k,240) + .500_r8*rxt(k,487)*y(k,256)
         mat(k,2190) = rxt(k,350)*y(k,51) + rxt(k,561)*y(k,166)
         mat(k,2543) = .230_r8*rxt(k,533)*y(k,6) + .630_r8*rxt(k,372)*y(k,28) &
                      + .560_r8*rxt(k,401)*y(k,33) + .620_r8*rxt(k,478)*y(k,127) &
                      + .650_r8*rxt(k,431)*y(k,134) + .230_r8*rxt(k,536)*y(k,139) &
                      + .560_r8*rxt(k,445)*y(k,140)
         mat(k,420) = rxt(k,561)*y(k,162) + rxt(k,562)*y(k,248)
         mat(k,1185) = .700_r8*rxt(k,554)*y(k,248)
         mat(k,1489) = .220_r8*rxt(k,425)*y(k,240) + .250_r8*rxt(k,483)*y(k,256)
         mat(k,1632) = (rxt(k,332)+rxt(k,333))*y(k,68) + .110_r8*rxt(k,426)*y(k,240) &
                      + .125_r8*rxt(k,543)*y(k,252) + .200_r8*rxt(k,484)*y(k,256)
         mat(k,851) = .170_r8*rxt(k,504)*y(k,153) + .070_r8*rxt(k,503)*y(k,233)
         mat(k,1835) = rxt(k,335)*y(k,68) + .070_r8*rxt(k,503)*y(k,229) &
                      + .160_r8*rxt(k,506)*y(k,241) + .140_r8*rxt(k,509)*y(k,243)
         mat(k,1420) = .220_r8*rxt(k,429)*y(k,153) + .220_r8*rxt(k,428)*y(k,155) &
                      + .220_r8*rxt(k,425)*y(k,227) + .110_r8*rxt(k,426)*y(k,228)
         mat(k,806) = .400_r8*rxt(k,507)*y(k,153) + .160_r8*rxt(k,506)*y(k,233)
         mat(k,958) = .350_r8*rxt(k,510)*y(k,153) + .140_r8*rxt(k,509)*y(k,233)
         mat(k,2139) = mat(k,2139) + .350_r8*rxt(k,370)*y(k,27) + rxt(k,351)*y(k,51) &
                      + rxt(k,408)*y(k,58) + rxt(k,395)*y(k,91) + rxt(k,562)*y(k,166) &
                      + .700_r8*rxt(k,554)*y(k,208)
         mat(k,1230) = .225_r8*rxt(k,545)*y(k,153) + .125_r8*rxt(k,543)*y(k,228)
         mat(k,1298) = .250_r8*rxt(k,486)*y(k,153) + .500_r8*rxt(k,487)*y(k,155) &
                      + .250_r8*rxt(k,483)*y(k,227) + .200_r8*rxt(k,484)*y(k,228)
         mat(k,1066) = .270_r8*rxt(k,533)*y(k,163)
         mat(k,1113) = .200_r8*rxt(k,401)*y(k,163)
         mat(k,776) = rxt(k,388)*y(k,248)
         mat(k,693) = .500_r8*rxt(k,389)*y(k,248)
         mat(k,1194) = rxt(k,368)*y(k,248)
         mat(k,1158) = .800_r8*rxt(k,394)*y(k,248)
         mat(k,1052) = rxt(k,395)*y(k,248)
         mat(k,1044) = rxt(k,360)*y(k,248)
         mat(k,700) = .500_r8*rxt(k,444)*y(k,248)
         mat(k,973) = .270_r8*rxt(k,536)*y(k,163)
         mat(k,1438) = .100_r8*rxt(k,445)*y(k,163)
         mat(k,2625) = rxt(k,387)*y(k,227) + .900_r8*rxt(k,545)*y(k,252)
         mat(k,2528) = .270_r8*rxt(k,533)*y(k,6) + .200_r8*rxt(k,401)*y(k,33) &
                      + .270_r8*rxt(k,536)*y(k,139) + .100_r8*rxt(k,445)*y(k,140)
         mat(k,1182) = 1.800_r8*rxt(k,554)*y(k,248)
         mat(k,1485) = rxt(k,387)*y(k,153) + 4.000_r8*rxt(k,384)*y(k,227) &
                      + .900_r8*rxt(k,385)*y(k,228) + rxt(k,458)*y(k,235) &
                      + 2.000_r8*rxt(k,434)*y(k,242) + rxt(k,483)*y(k,256)
         mat(k,1620) = .900_r8*rxt(k,385)*y(k,227) + rxt(k,435)*y(k,242) &
                      + .500_r8*rxt(k,543)*y(k,252)
         mat(k,1821) = .450_r8*rxt(k,436)*y(k,242)
         mat(k,1359) = rxt(k,458)*y(k,227)
         mat(k,1464) = 2.000_r8*rxt(k,434)*y(k,227) + rxt(k,435)*y(k,228) &
                      + .450_r8*rxt(k,436)*y(k,233) + 4.000_r8*rxt(k,437)*y(k,242)
         mat(k,2116) = rxt(k,388)*y(k,59) + .500_r8*rxt(k,389)*y(k,60) + rxt(k,368) &
                      *y(k,77) + .800_r8*rxt(k,394)*y(k,90) + rxt(k,395)*y(k,91) &
                      + rxt(k,360)*y(k,103) + .500_r8*rxt(k,444)*y(k,138) &
                      + 1.800_r8*rxt(k,554)*y(k,208)
         mat(k,1226) = .900_r8*rxt(k,545)*y(k,153) + .500_r8*rxt(k,543)*y(k,228)
         mat(k,1295) = rxt(k,483)*y(k,227)
         mat(k,221) = .470_r8*rxt(k,313)*y(k,248)
         mat(k,1099) = rxt(k,333)*y(k,228) + rxt(k,334)*y(k,233)
         mat(k,393) = rxt(k,338)*y(k,70) + rxt(k,339)*y(k,248)
         mat(k,1884) = rxt(k,338)*y(k,69)
         mat(k,1613) = rxt(k,333)*y(k,68)
         mat(k,1785) = rxt(k,334)*y(k,68)
         mat(k,2057) = .470_r8*rxt(k,313)*y(k,26) + rxt(k,339)*y(k,69)
         mat(k,273) = -(rxt(k,310)*y(k,247))
         mat(k,1970) = -rxt(k,310)*y(k,80)
         mat(k,172) = rxt(k,232)*y(k,247)
         mat(k,177) = rxt(k,262)*y(k,247)
         mat(k,183) = rxt(k,234)*y(k,247)
         mat(k,141) = 2.000_r8*rxt(k,235)*y(k,247)
         mat(k,187) = 2.000_r8*rxt(k,236)*y(k,247)
         mat(k,145) = rxt(k,237)*y(k,247)
         mat(k,125) = 2.000_r8*rxt(k,264)*y(k,247)
         mat(k,269) = rxt(k,346)*y(k,247) + rxt(k,341)*y(k,248)
         mat(k,318) = rxt(k,347)*y(k,247) + rxt(k,342)*y(k,248)
         mat(k,1970) = mat(k,1970) + rxt(k,232)*y(k,38) + rxt(k,262)*y(k,39) &
                      + rxt(k,234)*y(k,41) + 2.000_r8*rxt(k,235)*y(k,42) &
                      + 2.000_r8*rxt(k,236)*y(k,43) + rxt(k,237)*y(k,44) &
                      + 2.000_r8*rxt(k,264)*y(k,94) + rxt(k,346)*y(k,99) + rxt(k,347) &
                      *y(k,100)
         mat(k,2038) = rxt(k,341)*y(k,99) + rxt(k,342)*y(k,100)
         mat(k,264) = -(rxt(k,311)*y(k,247))
         mat(k,1968) = -rxt(k,311)*y(k,81)
         mat(k,137) = rxt(k,233)*y(k,247)
         mat(k,182) = rxt(k,234)*y(k,247)
         mat(k,260) = rxt(k,345)*y(k,247) + rxt(k,340)*y(k,248)
         mat(k,1968) = mat(k,1968) + rxt(k,233)*y(k,40) + rxt(k,234)*y(k,41) &
                      + rxt(k,345)*y(k,98)
         mat(k,2036) = rxt(k,340)*y(k,98)
         mat(k,232) = -(rxt(k,502)*y(k,248))
         mat(k,2030) = -rxt(k,502)*y(k,82)
         mat(k,226) = .180_r8*rxt(k,522)*y(k,248)
         mat(k,2030) = mat(k,2030) + .180_r8*rxt(k,522)*y(k,210)
         mat(k,1025) = -(rxt(k,555)*y(k,21) + (rxt(k,556) + rxt(k,557)) * y(k,70) &
                      + rxt(k,558)*y(k,125) + rxt(k,559)*y(k,155) + (rxt(k,560) &
                      + rxt(k,574)) * y(k,248))
         mat(k,2434) = -rxt(k,555)*y(k,83)
         mat(k,1899) = -(rxt(k,556) + rxt(k,557)) * y(k,83)
         mat(k,2225) = -rxt(k,558)*y(k,83)
         mat(k,2346) = -rxt(k,559)*y(k,83)
         mat(k,2126) = -(rxt(k,560) + rxt(k,574)) * y(k,83)
         mat(k,795) = rxt(k,390)*y(k,233)
         mat(k,1776) = rxt(k,390)*y(k,232)
         mat(k,947) = -(rxt(k,306)*y(k,64) + rxt(k,307)*y(k,93) + rxt(k,308)*y(k,260) &
                      + rxt(k,309)*y(k,106))
         mat(k,1573) = -rxt(k,306)*y(k,89)
         mat(k,1542) = -rxt(k,307)*y(k,89)
         mat(k,2684) = -rxt(k,308)*y(k,89)
         mat(k,2251) = -rxt(k,309)*y(k,89)
         mat(k,178) = rxt(k,262)*y(k,247)
         mat(k,188) = rxt(k,236)*y(k,247)
         mat(k,274) = 2.000_r8*rxt(k,310)*y(k,247)
         mat(k,265) = rxt(k,311)*y(k,247)
         mat(k,1975) = rxt(k,262)*y(k,39) + rxt(k,236)*y(k,43) + 2.000_r8*rxt(k,310) &
                      *y(k,80) + rxt(k,311)*y(k,81)
         mat(k,1160) = -(rxt(k,394)*y(k,248))
         mat(k,2136) = -rxt(k,394)*y(k,90)
         mat(k,684) = .700_r8*rxt(k,469)*y(k,248)
         mat(k,657) = .500_r8*rxt(k,470)*y(k,248)
         mat(k,458) = rxt(k,481)*y(k,248)
         mat(k,2637) = .050_r8*rxt(k,467)*y(k,236) + .530_r8*rxt(k,429)*y(k,240) &
                      + .225_r8*rxt(k,545)*y(k,252) + .250_r8*rxt(k,486)*y(k,256)
         mat(k,2356) = .050_r8*rxt(k,468)*y(k,236) + .530_r8*rxt(k,428)*y(k,240) &
                      + .250_r8*rxt(k,487)*y(k,256)
         mat(k,1488) = .530_r8*rxt(k,425)*y(k,240) + .250_r8*rxt(k,483)*y(k,256)
         mat(k,1629) = .260_r8*rxt(k,426)*y(k,240) + .125_r8*rxt(k,543)*y(k,252) &
                      + .100_r8*rxt(k,484)*y(k,256)
         mat(k,1393) = .050_r8*rxt(k,467)*y(k,153) + .050_r8*rxt(k,468)*y(k,155)
         mat(k,1419) = .530_r8*rxt(k,429)*y(k,153) + .530_r8*rxt(k,428)*y(k,155) &
                      + .530_r8*rxt(k,425)*y(k,227) + .260_r8*rxt(k,426)*y(k,228)
         mat(k,2136) = mat(k,2136) + .700_r8*rxt(k,469)*y(k,128) + .500_r8*rxt(k,470) &
                      *y(k,129) + rxt(k,481)*y(k,144)
         mat(k,1228) = .225_r8*rxt(k,545)*y(k,153) + .125_r8*rxt(k,543)*y(k,228)
         mat(k,1297) = .250_r8*rxt(k,486)*y(k,153) + .250_r8*rxt(k,487)*y(k,155) &
                      + .250_r8*rxt(k,483)*y(k,227) + .100_r8*rxt(k,484)*y(k,228)
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
         mat(k,1053) = -(rxt(k,395)*y(k,248))
         mat(k,2129) = -rxt(k,395)*y(k,91)
         mat(k,330) = .650_r8*rxt(k,370)*y(k,248)
         mat(k,1159) = .200_r8*rxt(k,394)*y(k,248)
         mat(k,1136) = rxt(k,482)*y(k,248)
         mat(k,2632) = rxt(k,493)*y(k,221) + .050_r8*rxt(k,467)*y(k,236) &
                      + .400_r8*rxt(k,507)*y(k,241) + .170_r8*rxt(k,510)*y(k,243) &
                      + .700_r8*rxt(k,513)*y(k,249) + .600_r8*rxt(k,520)*y(k,254) &
                      + .250_r8*rxt(k,486)*y(k,256) + .340_r8*rxt(k,526)*y(k,257) &
                      + .170_r8*rxt(k,529)*y(k,259)
         mat(k,2349) = .050_r8*rxt(k,468)*y(k,236) + .250_r8*rxt(k,487)*y(k,256)
         mat(k,568) = rxt(k,493)*y(k,153)
         mat(k,1486) = .250_r8*rxt(k,483)*y(k,256)
         mat(k,1624) = .100_r8*rxt(k,484)*y(k,256)
         mat(k,1828) = .160_r8*rxt(k,506)*y(k,241) + .070_r8*rxt(k,509)*y(k,243)
         mat(k,1392) = .050_r8*rxt(k,467)*y(k,153) + .050_r8*rxt(k,468)*y(k,155)
         mat(k,805) = .400_r8*rxt(k,507)*y(k,153) + .160_r8*rxt(k,506)*y(k,233)
         mat(k,957) = .170_r8*rxt(k,510)*y(k,153) + .070_r8*rxt(k,509)*y(k,233)
         mat(k,2129) = mat(k,2129) + .650_r8*rxt(k,370)*y(k,27) + .200_r8*rxt(k,394) &
                      *y(k,90) + rxt(k,482)*y(k,145)
         mat(k,526) = .700_r8*rxt(k,513)*y(k,153)
         mat(k,818) = .600_r8*rxt(k,520)*y(k,153)
         mat(k,1296) = .250_r8*rxt(k,486)*y(k,153) + .250_r8*rxt(k,487)*y(k,155) &
                      + .250_r8*rxt(k,483)*y(k,227) + .100_r8*rxt(k,484)*y(k,228)
         mat(k,842) = .340_r8*rxt(k,526)*y(k,153)
         mat(k,583) = .170_r8*rxt(k,529)*y(k,153)
         mat(k,1941) = -((rxt(k,164) + rxt(k,165) + rxt(k,166)) * y(k,233) + rxt(k,170) &
                      *y(k,163))
         mat(k,1863) = -(rxt(k,164) + rxt(k,165) + rxt(k,166)) * y(k,92)
         mat(k,2565) = -rxt(k,170)*y(k,92)
         mat(k,1702) = rxt(k,351)*y(k,248)
         mat(k,1582) = rxt(k,365)*y(k,247)
         mat(k,1918) = rxt(k,206)*y(k,93)
         mat(k,951) = rxt(k,307)*y(k,93)
         mat(k,1548) = rxt(k,206)*y(k,70) + rxt(k,307)*y(k,89) + rxt(k,162)*y(k,162) &
                      + rxt(k,153)*y(k,247) + rxt(k,171)*y(k,248)
         mat(k,1521) = rxt(k,266)*y(k,247)
         mat(k,1727) = rxt(k,239)*y(k,247)
         mat(k,575) = rxt(k,192)*y(k,248)
         mat(k,2204) = rxt(k,162)*y(k,93) + rxt(k,174)*y(k,248)
         mat(k,422) = rxt(k,562)*y(k,248)
         mat(k,604) = rxt(k,568)*y(k,248)
         mat(k,1561) = rxt(k,573)*y(k,248)
         mat(k,1987) = rxt(k,365)*y(k,64) + rxt(k,153)*y(k,93) + rxt(k,266)*y(k,97) &
                      + rxt(k,239)*y(k,101)
         mat(k,2169) = rxt(k,351)*y(k,51) + rxt(k,171)*y(k,93) + rxt(k,192)*y(k,141) &
                      + rxt(k,174)*y(k,162) + rxt(k,562)*y(k,166) + rxt(k,568) &
                      *y(k,178) + rxt(k,573)*y(k,180)
         mat(k,1543) = -(rxt(k,153)*y(k,247) + rxt(k,162)*y(k,162) + rxt(k,171) &
                      *y(k,248) + rxt(k,206)*y(k,70) + rxt(k,307)*y(k,89))
         mat(k,1977) = -rxt(k,153)*y(k,93)
         mat(k,2193) = -rxt(k,162)*y(k,93)
         mat(k,2158) = -rxt(k,171)*y(k,93)
         mat(k,1908) = -rxt(k,206)*y(k,93)
         mat(k,948) = -rxt(k,307)*y(k,93)
         mat(k,1575) = rxt(k,366)*y(k,247)
         mat(k,1933) = rxt(k,164)*y(k,233)
         mat(k,1853) = rxt(k,164)*y(k,92)
         mat(k,1977) = mat(k,1977) + rxt(k,366)*y(k,64)
         mat(k,124) = -(rxt(k,264)*y(k,247))
         mat(k,1957) = -rxt(k,264)*y(k,94)
         mat(k,672) = -(rxt(k,163)*y(k,162) + rxt(k,172)*y(k,248) + rxt(k,207)*y(k,70))
         mat(k,2186) = -rxt(k,163)*y(k,95)
         mat(k,2093) = -rxt(k,172)*y(k,95)
         mat(k,1894) = -rxt(k,207)*y(k,95)
         mat(k,1804) = 2.000_r8*rxt(k,178)*y(k,233)
         mat(k,2093) = mat(k,2093) + 2.000_r8*rxt(k,177)*y(k,248)
         mat(k,295) = rxt(k,575)*y(k,260)
         mat(k,2681) = rxt(k,575)*y(k,182)
         mat(k,1516) = -(rxt(k,259)*y(k,162) + rxt(k,260)*y(k,248) + (rxt(k,265) &
                      + rxt(k,266)) * y(k,247) + (rxt(k,582) + rxt(k,659) + rxt(k,667) &
                      + rxt(k,676)) * y(k,109) + (rxt(k,583) + rxt(k,657) + rxt(k,670) &
                      + rxt(k,679)) * y(k,108) + (rxt(k,590) + rxt(k,686) + rxt(k,690) &
                      + rxt(k,694)) * y(k,110))
         mat(k,2191) = -rxt(k,259)*y(k,97)
         mat(k,2156) = -rxt(k,260)*y(k,97)
         mat(k,1976) = -(rxt(k,265) + rxt(k,266)) * y(k,97)
         mat(k,1744) = -(rxt(k,582) + rxt(k,659) + rxt(k,667) + rxt(k,676)) * y(k,97)
         mat(k,1669) = -(rxt(k,583) + rxt(k,657) + rxt(k,670) + rxt(k,679)) * y(k,97)
         mat(k,1592) = -(rxt(k,590) + rxt(k,686) + rxt(k,690) + rxt(k,694)) * y(k,97)
         mat(k,2309) = rxt(k,243)*y(k,51) + rxt(k,244)*y(k,233)
         mat(k,1691) = rxt(k,243)*y(k,17)
         mat(k,1851) = rxt(k,244)*y(k,17)
         mat(k,259) = -(rxt(k,340)*y(k,248) + rxt(k,345)*y(k,247))
         mat(k,2035) = -rxt(k,340)*y(k,98)
         mat(k,1967) = -rxt(k,345)*y(k,98)
         mat(k,268) = -(rxt(k,341)*y(k,248) + rxt(k,346)*y(k,247))
         mat(k,2037) = -rxt(k,341)*y(k,99)
         mat(k,1969) = -rxt(k,346)*y(k,99)
         mat(k,319) = -(rxt(k,342)*y(k,248) + rxt(k,347)*y(k,247))
         mat(k,2047) = -rxt(k,342)*y(k,100)
         mat(k,1971) = -rxt(k,347)*y(k,100)
         mat(k,1724) = -(rxt(k,226)*y(k,162) + rxt(k,227)*y(k,248) + (rxt(k,238) &
                      + rxt(k,239)) * y(k,247) + (rxt(k,584) + rxt(k,655) + rxt(k,666) &
                      + rxt(k,675)) * y(k,109) + (rxt(k,585) + rxt(k,656) + rxt(k,669) &
                      + rxt(k,678)) * y(k,108) + (rxt(k,589) + rxt(k,685) + rxt(k,689) &
                      + rxt(k,693)) * y(k,110) + rxt(k,600)*y(k,143) + (rxt(k,665) &
                      + rxt(k,674) + rxt(k,683)) * y(k,75))
         mat(k,2200) = -rxt(k,226)*y(k,101)
         mat(k,2165) = -rxt(k,227)*y(k,101)
         mat(k,1983) = -(rxt(k,238) + rxt(k,239)) * y(k,101)
         mat(k,1748) = -(rxt(k,584) + rxt(k,655) + rxt(k,666) + rxt(k,675)) * y(k,101)
         mat(k,1673) = -(rxt(k,585) + rxt(k,656) + rxt(k,669) + rxt(k,678)) * y(k,101)
         mat(k,1596) = -(rxt(k,589) + rxt(k,685) + rxt(k,689) + rxt(k,693)) * y(k,101)
         mat(k,403) = -rxt(k,600)*y(k,101)
         mat(k,1010) = -(rxt(k,665) + rxt(k,674) + rxt(k,683)) * y(k,101)
         mat(k,291) = rxt(k,314)*y(k,70)
         mat(k,339) = rxt(k,379)*y(k,70)
         mat(k,551) = rxt(k,316)*y(k,70)
         mat(k,345) = rxt(k,319)*y(k,70)
         mat(k,1698) = rxt(k,204)*y(k,70)
         mat(k,665) = rxt(k,321)*y(k,70)
         mat(k,439) = 2.000_r8*rxt(k,324)*y(k,70)
         mat(k,430) = rxt(k,326)*y(k,70)
         mat(k,1579) = rxt(k,205)*y(k,70)
         mat(k,493) = rxt(k,329)*y(k,70)
         mat(k,397) = rxt(k,338)*y(k,70)
         mat(k,1914) = rxt(k,314)*y(k,29) + rxt(k,379)*y(k,32) + rxt(k,316)*y(k,45) &
                      + rxt(k,319)*y(k,47) + rxt(k,204)*y(k,51) + rxt(k,321)*y(k,52) &
                      + 2.000_r8*rxt(k,324)*y(k,55) + rxt(k,326)*y(k,61) + rxt(k,205) &
                      *y(k,64) + rxt(k,329)*y(k,66) + rxt(k,338)*y(k,69) + rxt(k,557) &
                      *y(k,83) + rxt(k,206)*y(k,93) + rxt(k,207)*y(k,95) + rxt(k,228) &
                      *y(k,109) + rxt(k,208)*y(k,233)
         mat(k,2285) = rxt(k,225)*y(k,248)
         mat(k,1027) = rxt(k,557)*y(k,70)
         mat(k,1546) = rxt(k,206)*y(k,70)
         mat(k,673) = rxt(k,207)*y(k,70)
         mat(k,1748) = mat(k,1748) + rxt(k,228)*y(k,70)
         mat(k,1859) = rxt(k,208)*y(k,70)
         mat(k,2165) = mat(k,2165) + rxt(k,225)*y(k,74)
         mat(k,209) = -(rxt(k,359)*y(k,248) + rxt(k,367)*y(k,247))
         mat(k,2027) = -rxt(k,359)*y(k,102)
         mat(k,1965) = -rxt(k,367)*y(k,102)
         mat(k,1045) = -(rxt(k,360)*y(k,248))
         mat(k,2128) = -rxt(k,360)*y(k,103)
         mat(k,1069) = .050_r8*rxt(k,533)*y(k,163)
         mat(k,329) = .350_r8*rxt(k,370)*y(k,248)
         mat(k,641) = .370_r8*rxt(k,372)*y(k,163)
         mat(k,1115) = .120_r8*rxt(k,401)*y(k,163)
         mat(k,919) = .110_r8*rxt(k,478)*y(k,163)
         mat(k,1334) = .330_r8*rxt(k,431)*y(k,163)
         mat(k,977) = .050_r8*rxt(k,536)*y(k,163)
         mat(k,1439) = .120_r8*rxt(k,445)*y(k,163)
         mat(k,2631) = rxt(k,363)*y(k,234)
         mat(k,2534) = .050_r8*rxt(k,533)*y(k,6) + .370_r8*rxt(k,372)*y(k,28) &
                      + .120_r8*rxt(k,401)*y(k,33) + .110_r8*rxt(k,478)*y(k,127) &
                      + .330_r8*rxt(k,431)*y(k,134) + .050_r8*rxt(k,536)*y(k,139) &
                      + .120_r8*rxt(k,445)*y(k,140)
         mat(k,1827) = rxt(k,361)*y(k,234)
         mat(k,519) = rxt(k,363)*y(k,153) + rxt(k,361)*y(k,233)
         mat(k,2128) = mat(k,2128) + .350_r8*rxt(k,370)*y(k,27)
         mat(k,1571) = rxt(k,306)*y(k,89)
         mat(k,946) = rxt(k,306)*y(k,64) + rxt(k,307)*y(k,93) + rxt(k,309)*y(k,106) &
                      + rxt(k,308)*y(k,260)
         mat(k,1541) = rxt(k,307)*y(k,89)
         mat(k,2250) = rxt(k,309)*y(k,89)
         mat(k,2683) = rxt(k,308)*y(k,89)
         mat(k,1280) = -(rxt(k,267)*y(k,155) + rxt(k,295)*y(k,248) + (rxt(k,586) &
                      + rxt(k,660) + rxt(k,668) + rxt(k,677)) * y(k,109) + (rxt(k,587) &
                      + rxt(k,658) + rxt(k,671) + rxt(k,680)) * y(k,108) + (rxt(k,591) &
                      + rxt(k,687) + rxt(k,691) + rxt(k,695)) * y(k,110))
         mat(k,2365) = -rxt(k,267)*y(k,105)
         mat(k,2145) = -rxt(k,295)*y(k,105)
         mat(k,1743) = -(rxt(k,586) + rxt(k,660) + rxt(k,668) + rxt(k,677)) * y(k,105)
         mat(k,1668) = -(rxt(k,587) + rxt(k,658) + rxt(k,671) + rxt(k,680)) * y(k,105)
         mat(k,1591) = -(rxt(k,591) + rxt(k,687) + rxt(k,691) + rxt(k,695)) * y(k,105)
         mat(k,2407) = rxt(k,273)*y(k,233)
         mat(k,1840) = rxt(k,273)*y(k,115)
         mat(k,2265) = -(rxt(k,201)*y(k,248) + rxt(k,309)*y(k,89))
         mat(k,2174) = -rxt(k,201)*y(k,106)
         mat(k,953) = -rxt(k,309)*y(k,106)
         mat(k,1707) = rxt(k,349)*y(k,155)
         mat(k,1155) = rxt(k,381)*y(k,155)
         mat(k,1353) = rxt(k,407)*y(k,155)
         mat(k,1015) = (rxt(k,665)+rxt(k,674)+rxt(k,683))*y(k,101)
         mat(k,1032) = rxt(k,559)*y(k,155)
         mat(k,1732) = (rxt(k,665)+rxt(k,674)+rxt(k,683))*y(k,75) + rxt(k,600) &
                      *y(k,143)
         mat(k,1288) = rxt(k,267)*y(k,155)
         mat(k,1604) = rxt(k,297)*y(k,155)
         mat(k,405) = rxt(k,600)*y(k,101)
         mat(k,2504) = rxt(k,200)*y(k,248)
         mat(k,2392) = rxt(k,349)*y(k,51) + rxt(k,381)*y(k,54) + rxt(k,407)*y(k,58) &
                      + rxt(k,559)*y(k,83) + rxt(k,267)*y(k,105) + rxt(k,297)*y(k,110)
         mat(k,2174) = mat(k,2174) + rxt(k,200)*y(k,154)
         mat(k,450) = -(rxt(k,179)*y(k,248))
         mat(k,2064) = -rxt(k,179)*y(k,107)
         mat(k,2463) = rxt(k,198)*y(k,233)
         mat(k,1787) = rxt(k,198)*y(k,154)
         mat(k,1672) = -(rxt(k,261)*y(k,162) + (rxt(k,583) + rxt(k,657) + rxt(k,670) &
                      + rxt(k,679)) * y(k,97) + (rxt(k,585) + rxt(k,656) + rxt(k,669) &
                      + rxt(k,678)) * y(k,101) + (rxt(k,587) + rxt(k,658) + rxt(k,671) &
                      + rxt(k,680)) * y(k,105))
         mat(k,2198) = -rxt(k,261)*y(k,108)
         mat(k,1518) = -(rxt(k,583) + rxt(k,657) + rxt(k,670) + rxt(k,679)) * y(k,108)
         mat(k,1723) = -(rxt(k,585) + rxt(k,656) + rxt(k,669) + rxt(k,678)) * y(k,108)
         mat(k,1283) = -(rxt(k,587) + rxt(k,658) + rxt(k,671) + rxt(k,680)) * y(k,108)
         mat(k,540) = rxt(k,242)*y(k,248)
         mat(k,2437) = rxt(k,251)*y(k,233)
         mat(k,1857) = rxt(k,251)*y(k,21)
         mat(k,2163) = rxt(k,242)*y(k,18)
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
         mat(k,1749) = -(rxt(k,228)*y(k,70) + rxt(k,229)*y(k,162) + rxt(k,230) &
                      *y(k,248) + (rxt(k,582) + rxt(k,659) + rxt(k,667) + rxt(k,676) &
                      ) * y(k,97) + (rxt(k,584) + rxt(k,655) + rxt(k,666) + rxt(k,675) &
                      ) * y(k,101) + (rxt(k,586) + rxt(k,660) + rxt(k,668) + rxt(k,677) &
                      ) * y(k,105))
         mat(k,1915) = -rxt(k,228)*y(k,109)
         mat(k,2201) = -rxt(k,229)*y(k,109)
         mat(k,2166) = -rxt(k,230)*y(k,109)
         mat(k,1519) = -(rxt(k,582) + rxt(k,659) + rxt(k,667) + rxt(k,676)) * y(k,109)
         mat(k,1725) = -(rxt(k,584) + rxt(k,655) + rxt(k,666) + rxt(k,675)) * y(k,109)
         mat(k,1284) = -(rxt(k,586) + rxt(k,660) + rxt(k,668) + rxt(k,677)) * y(k,109)
         mat(k,1104) = rxt(k,335)*y(k,233)
         mat(k,591) = rxt(k,211)*y(k,248)
         mat(k,2286) = rxt(k,217)*y(k,233)
         mat(k,1011) = rxt(k,222)*y(k,248)
         mat(k,1860) = rxt(k,335)*y(k,68) + rxt(k,217)*y(k,74)
         mat(k,2166) = mat(k,2166) + rxt(k,211)*y(k,73) + rxt(k,222)*y(k,75)
         mat(k,1594) = -(rxt(k,268)*y(k,248) + rxt(k,297)*y(k,155) + (rxt(k,589) &
                      + rxt(k,685) + rxt(k,689) + rxt(k,693)) * y(k,101) + (rxt(k,590) &
                      + rxt(k,686) + rxt(k,690) + rxt(k,694)) * y(k,97) + (rxt(k,591) &
                      + rxt(k,687) + rxt(k,691) + rxt(k,695)) * y(k,105))
         mat(k,2161) = -rxt(k,268)*y(k,110)
         mat(k,2379) = -rxt(k,297)*y(k,110)
         mat(k,1722) = -(rxt(k,589) + rxt(k,685) + rxt(k,689) + rxt(k,693)) * y(k,110)
         mat(k,1517) = -(rxt(k,590) + rxt(k,686) + rxt(k,690) + rxt(k,694)) * y(k,110)
         mat(k,1282) = -(rxt(k,591) + rxt(k,687) + rxt(k,691) + rxt(k,695)) * y(k,110)
         mat(k,1531) = rxt(k,271)*y(k,248)
         mat(k,2229) = rxt(k,288)*y(k,233)
         mat(k,1855) = rxt(k,288)*y(k,125)
         mat(k,2161) = mat(k,2161) + rxt(k,271)*y(k,116)
         mat(k,1204) = -(rxt(k,424)*y(k,248))
         mat(k,2140) = -rxt(k,424)*y(k,111)
         mat(k,685) = .300_r8*rxt(k,469)*y(k,248)
         mat(k,658) = .500_r8*rxt(k,470)*y(k,248)
         mat(k,2641) = rxt(k,423)*y(k,230) + rxt(k,430)*y(k,240)
         mat(k,650) = rxt(k,423)*y(k,153)
         mat(k,1421) = rxt(k,430)*y(k,153)
         mat(k,2140) = mat(k,2140) + .300_r8*rxt(k,469)*y(k,128) + .500_r8*rxt(k,470) &
                      *y(k,129)
         mat(k,276) = -(rxt(k,455)*y(k,248))
         mat(k,2039) = -rxt(k,455)*y(k,112)
         mat(k,1217) = -(rxt(k,409)*y(k,248))
         mat(k,2141) = -rxt(k,409)*y(k,113)
         mat(k,686) = .700_r8*rxt(k,469)*y(k,248)
         mat(k,659) = .500_r8*rxt(k,470)*y(k,248)
         mat(k,701) = .500_r8*rxt(k,444)*y(k,248)
         mat(k,2642) = .050_r8*rxt(k,467)*y(k,236) + .220_r8*rxt(k,429)*y(k,240) &
                      + .250_r8*rxt(k,486)*y(k,256)
         mat(k,2361) = .050_r8*rxt(k,468)*y(k,236) + .220_r8*rxt(k,428)*y(k,240) &
                      + .250_r8*rxt(k,487)*y(k,256)
         mat(k,634) = .500_r8*rxt(k,413)*y(k,248)
         mat(k,1490) = .220_r8*rxt(k,425)*y(k,240) + .250_r8*rxt(k,483)*y(k,256)
         mat(k,1633) = .230_r8*rxt(k,426)*y(k,240) + .200_r8*rxt(k,414)*y(k,251) &
                      + .100_r8*rxt(k,484)*y(k,256)
         mat(k,1396) = .050_r8*rxt(k,467)*y(k,153) + .050_r8*rxt(k,468)*y(k,155)
         mat(k,1422) = .220_r8*rxt(k,429)*y(k,153) + .220_r8*rxt(k,428)*y(k,155) &
                      + .220_r8*rxt(k,425)*y(k,227) + .230_r8*rxt(k,426)*y(k,228)
         mat(k,2141) = mat(k,2141) + .700_r8*rxt(k,469)*y(k,128) + .500_r8*rxt(k,470) &
                      *y(k,129) + .500_r8*rxt(k,444)*y(k,138) + .500_r8*rxt(k,413) &
                      *y(k,176)
         mat(k,1267) = .200_r8*rxt(k,414)*y(k,228)
         mat(k,1299) = .250_r8*rxt(k,486)*y(k,153) + .250_r8*rxt(k,487)*y(k,155) &
                      + .250_r8*rxt(k,483)*y(k,227) + .100_r8*rxt(k,484)*y(k,228)
         mat(k,349) = -(rxt(k,456)*y(k,248))
         mat(k,2051) = -rxt(k,456)*y(k,114)
         mat(k,2597) = .870_r8*rxt(k,467)*y(k,236)
         mat(k,2334) = .950_r8*rxt(k,468)*y(k,236)
         mat(k,1481) = rxt(k,463)*y(k,236)
         mat(k,1612) = .750_r8*rxt(k,464)*y(k,236)
         mat(k,1385) = .870_r8*rxt(k,467)*y(k,153) + .950_r8*rxt(k,468)*y(k,155) &
                      + rxt(k,463)*y(k,227) + .750_r8*rxt(k,464)*y(k,228)
         mat(k,2424) = -(rxt(k,272)*y(k,21) + rxt(k,273)*y(k,233) + rxt(k,274) &
                      *y(k,126) + rxt(k,276)*y(k,154) + rxt(k,278)*y(k,155) + rxt(k,280) &
                      *y(k,153) + rxt(k,281)*y(k,163))
         mat(k,2451) = -rxt(k,272)*y(k,115)
         mat(k,1872) = -rxt(k,273)*y(k,115)
         mat(k,944) = -rxt(k,274)*y(k,115)
         mat(k,2508) = -rxt(k,276)*y(k,115)
         mat(k,2396) = -rxt(k,278)*y(k,115)
         mat(k,2675) = -rxt(k,280)*y(k,115)
         mat(k,2574) = -rxt(k,281)*y(k,115)
         mat(k,2326) = rxt(k,282)*y(k,125)
         mat(k,2451) = mat(k,2451) + rxt(k,283)*y(k,125)
         mat(k,434) = rxt(k,326)*y(k,70) + rxt(k,327)*y(k,248)
         mat(k,1927) = rxt(k,326)*y(k,61)
         mat(k,2298) = (rxt(k,285)+rxt(k,286))*y(k,125)
         mat(k,1035) = rxt(k,558)*y(k,125)
         mat(k,1291) = rxt(k,267)*y(k,155) + rxt(k,295)*y(k,248)
         mat(k,1539) = rxt(k,269)*y(k,155) + rxt(k,270)*y(k,162) + rxt(k,271)*y(k,248)
         mat(k,2244) = rxt(k,282)*y(k,17) + rxt(k,283)*y(k,21) + (rxt(k,285) &
                       +rxt(k,286))*y(k,74) + rxt(k,558)*y(k,83) + 2.000_r8*rxt(k,301) &
                      *y(k,125) + rxt(k,289)*y(k,153) + rxt(k,292)*y(k,162) &
                      + rxt(k,294)*y(k,248)
         mat(k,2675) = mat(k,2675) + rxt(k,289)*y(k,125)
         mat(k,2396) = mat(k,2396) + rxt(k,267)*y(k,105) + rxt(k,269)*y(k,116)
         mat(k,2213) = rxt(k,270)*y(k,116) + rxt(k,292)*y(k,125)
         mat(k,2178) = rxt(k,327)*y(k,61) + rxt(k,295)*y(k,105) + rxt(k,271)*y(k,116) &
                      + rxt(k,294)*y(k,125)
         mat(k,1530) = -(rxt(k,269)*y(k,155) + rxt(k,270)*y(k,162) + rxt(k,271) &
                      *y(k,248))
         mat(k,2376) = -rxt(k,269)*y(k,116)
         mat(k,2192) = -rxt(k,270)*y(k,116)
         mat(k,2157) = -rxt(k,271)*y(k,116)
         mat(k,1281) = (rxt(k,591)+rxt(k,687)+rxt(k,691)+rxt(k,695))*y(k,110)
         mat(k,1593) = (rxt(k,591)+rxt(k,687)+rxt(k,691)+rxt(k,695))*y(k,105)
         mat(k,2408) = rxt(k,274)*y(k,126)
         mat(k,214) = 2.000_r8*rxt(k,279)*y(k,123)
         mat(k,315) = 2.000_r8*rxt(k,275)*y(k,124)
         mat(k,937) = rxt(k,274)*y(k,115)
         mat(k,2219) = 2.000_r8*rxt(k,302)*y(k,125)
         mat(k,2220) = rxt(k,304)*y(k,167)
         mat(k,596) = rxt(k,304)*y(k,125)
         mat(k,595) = 2.000_r8*rxt(k,305)*y(k,167)
         mat(k,1513) = (rxt(k,590)+rxt(k,686)+rxt(k,690)+rxt(k,694))*y(k,110)
         mat(k,1278) = (rxt(k,587)+rxt(k,658)+rxt(k,671)+rxt(k,680))*y(k,108)
         mat(k,1665) = (rxt(k,587)+rxt(k,658)+rxt(k,671)+rxt(k,680))*y(k,105)
         mat(k,1589) = (rxt(k,590)+rxt(k,686)+rxt(k,690)+rxt(k,694))*y(k,97)
         mat(k,2278) = rxt(k,287)*y(k,125)
         mat(k,1718) = (rxt(k,589)+rxt(k,685)+rxt(k,689)+rxt(k,693))*y(k,110)
         mat(k,1279) = (rxt(k,586)+rxt(k,660)+rxt(k,668)+rxt(k,677))*y(k,109)
         mat(k,1741) = (rxt(k,586)+rxt(k,660)+rxt(k,668)+rxt(k,677))*y(k,105)
         mat(k,1590) = (rxt(k,589)+rxt(k,685)+rxt(k,689)+rxt(k,693))*y(k,101)
         mat(k,2222) = rxt(k,287)*y(k,74)
         mat(k,165) = -(rxt(k,457)*y(k,248))
         mat(k,2023) = -rxt(k,457)*y(k,122)
         mat(k,825) = .600_r8*rxt(k,480)*y(k,248)
         mat(k,2023) = mat(k,2023) + .600_r8*rxt(k,480)*y(k,131)
         mat(k,213) = -(4._r8*rxt(k,279)*y(k,123))
         mat(k,2402) = rxt(k,280)*y(k,153)
         mat(k,2592) = rxt(k,280)*y(k,115)
         mat(k,312) = -(4._r8*rxt(k,275)*y(k,124))
         mat(k,2403) = rxt(k,276)*y(k,154)
         mat(k,2459) = rxt(k,276)*y(k,115)
         mat(k,2239) = -(rxt(k,282)*y(k,17) + (rxt(k,283) + rxt(k,284)) * y(k,21) &
                      + (rxt(k,285) + rxt(k,286) + rxt(k,287)) * y(k,74) + rxt(k,288) &
                      *y(k,233) + rxt(k,289)*y(k,153) + rxt(k,290)*y(k,154) + rxt(k,291) &
                      *y(k,155) + rxt(k,292)*y(k,162) + rxt(k,293)*y(k,163) + rxt(k,294) &
                      *y(k,248) + (4._r8*rxt(k,301) + 4._r8*rxt(k,302)) * y(k,125) &
                      + rxt(k,304)*y(k,167) + rxt(k,558)*y(k,83))
         mat(k,2321) = -rxt(k,282)*y(k,125)
         mat(k,2446) = -(rxt(k,283) + rxt(k,284)) * y(k,125)
         mat(k,2293) = -(rxt(k,285) + rxt(k,286) + rxt(k,287)) * y(k,125)
         mat(k,1867) = -rxt(k,288)*y(k,125)
         mat(k,2670) = -rxt(k,289)*y(k,125)
         mat(k,2503) = -rxt(k,290)*y(k,125)
         mat(k,2391) = -rxt(k,291)*y(k,125)
         mat(k,2208) = -rxt(k,292)*y(k,125)
         mat(k,2569) = -rxt(k,293)*y(k,125)
         mat(k,2173) = -rxt(k,294)*y(k,125)
         mat(k,598) = -rxt(k,304)*y(k,125)
         mat(k,1031) = -rxt(k,558)*y(k,125)
         mat(k,2446) = mat(k,2446) + rxt(k,272)*y(k,115)
         mat(k,1603) = rxt(k,297)*y(k,155) + rxt(k,268)*y(k,248)
         mat(k,2419) = rxt(k,272)*y(k,21) + rxt(k,278)*y(k,155) + rxt(k,281)*y(k,163)
         mat(k,1535) = rxt(k,270)*y(k,162)
         mat(k,2670) = mat(k,2670) + rxt(k,296)*y(k,167)
         mat(k,2391) = mat(k,2391) + rxt(k,297)*y(k,110) + rxt(k,278)*y(k,115)
         mat(k,2208) = mat(k,2208) + rxt(k,270)*y(k,116)
         mat(k,2569) = mat(k,2569) + rxt(k,281)*y(k,115)
         mat(k,598) = mat(k,598) + rxt(k,296)*y(k,153)
         mat(k,2173) = mat(k,2173) + rxt(k,268)*y(k,110)
         mat(k,936) = -(rxt(k,274)*y(k,115))
         mat(k,2406) = -rxt(k,274)*y(k,126)
         mat(k,1529) = rxt(k,269)*y(k,155)
         mat(k,2224) = rxt(k,290)*y(k,154)
         mat(k,2478) = rxt(k,290)*y(k,125)
         mat(k,2342) = rxt(k,269)*y(k,116)
         mat(k,918) = -(rxt(k,471)*y(k,155) + rxt(k,478)*y(k,163) + rxt(k,479) &
                      *y(k,248))
         mat(k,2341) = -rxt(k,471)*y(k,127)
         mat(k,2529) = -rxt(k,478)*y(k,127)
         mat(k,2119) = -rxt(k,479)*y(k,127)
         mat(k,683) = -(rxt(k,469)*y(k,248))
         mat(k,2095) = -rxt(k,469)*y(k,128)
         mat(k,2613) = .080_r8*rxt(k,461)*y(k,235)
         mat(k,1356) = .080_r8*rxt(k,461)*y(k,153)
         mat(k,655) = -(rxt(k,470)*y(k,248))
         mat(k,2091) = -rxt(k,470)*y(k,129)
         mat(k,2611) = .080_r8*rxt(k,467)*y(k,236)
         mat(k,1386) = .080_r8*rxt(k,467)*y(k,153)
         mat(k,444) = -(rxt(k,477)*y(k,248))
         mat(k,2063) = -rxt(k,477)*y(k,130)
         mat(k,1786) = rxt(k,474)*y(k,237)
         mat(k,1311) = rxt(k,474)*y(k,233)
         mat(k,826) = -(rxt(k,480)*y(k,248))
         mat(k,2110) = -rxt(k,480)*y(k,131)
         mat(k,1816) = rxt(k,460)*y(k,235) + rxt(k,465)*y(k,236)
         mat(k,1357) = rxt(k,460)*y(k,233)
         mat(k,1388) = rxt(k,465)*y(k,233)
         mat(k,83) = -(rxt(k,640)*y(k,248))
         mat(k,2012) = -rxt(k,640)*y(k,132)
         mat(k,1336) = -(rxt(k,431)*y(k,163) + rxt(k,432)*y(k,248))
         mat(k,2549) = -rxt(k,431)*y(k,134)
         mat(k,2148) = -rxt(k,432)*y(k,134)
         mat(k,923) = .300_r8*rxt(k,478)*y(k,163)
         mat(k,2648) = .360_r8*rxt(k,461)*y(k,235)
         mat(k,2368) = .400_r8*rxt(k,462)*y(k,235)
         mat(k,2549) = mat(k,2549) + .300_r8*rxt(k,478)*y(k,127)
         mat(k,1493) = .390_r8*rxt(k,458)*y(k,235)
         mat(k,1639) = .310_r8*rxt(k,459)*y(k,235)
         mat(k,1366) = .360_r8*rxt(k,461)*y(k,153) + .400_r8*rxt(k,462)*y(k,155) &
                      + .390_r8*rxt(k,458)*y(k,227) + .310_r8*rxt(k,459)*y(k,228)
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
         mat(k,352) = -(rxt(k,433)*y(k,248))
         mat(k,2052) = -rxt(k,433)*y(k,135)
         mat(k,1780) = rxt(k,427)*y(k,240)
         mat(k,1417) = rxt(k,427)*y(k,233)
         mat(k,608) = -(rxt(k,442)*y(k,248))
         mat(k,2085) = -rxt(k,442)*y(k,136)
         mat(k,2608) = .800_r8*rxt(k,451)*y(k,219)
         mat(k,993) = .800_r8*rxt(k,451)*y(k,153)
         mat(k,357) = -(rxt(k,443)*y(k,248))
         mat(k,2053) = -rxt(k,443)*y(k,137)
         mat(k,1781) = .800_r8*rxt(k,440)*y(k,244)
         mat(k,767) = .800_r8*rxt(k,440)*y(k,233)
         mat(k,699) = -(rxt(k,444)*y(k,248))
         mat(k,2097) = -rxt(k,444)*y(k,138)
         mat(k,2472) = rxt(k,447)*y(k,242)
         mat(k,1462) = rxt(k,447)*y(k,154)
         mat(k,974) = -(rxt(k,535)*y(k,155) + rxt(k,536)*y(k,163) + rxt(k,537) &
                      *y(k,248))
         mat(k,2343) = -rxt(k,535)*y(k,139)
         mat(k,2530) = -rxt(k,536)*y(k,139)
         mat(k,2122) = -rxt(k,537)*y(k,139)
         mat(k,1446) = -(rxt(k,445)*y(k,163) + rxt(k,446)*y(k,248))
         mat(k,2554) = -rxt(k,445)*y(k,140)
         mat(k,2153) = -rxt(k,446)*y(k,140)
         mat(k,926) = .200_r8*rxt(k,478)*y(k,163)
         mat(k,2653) = .560_r8*rxt(k,461)*y(k,235)
         mat(k,2373) = .600_r8*rxt(k,462)*y(k,235)
         mat(k,2554) = mat(k,2554) + .200_r8*rxt(k,478)*y(k,127)
         mat(k,1498) = .610_r8*rxt(k,458)*y(k,235)
         mat(k,1644) = .440_r8*rxt(k,459)*y(k,235)
         mat(k,1370) = .560_r8*rxt(k,461)*y(k,153) + .600_r8*rxt(k,462)*y(k,155) &
                      + .610_r8*rxt(k,458)*y(k,227) + .440_r8*rxt(k,459)*y(k,228)
         mat(k,574) = -(rxt(k,180)*y(k,153) + (rxt(k,181) + rxt(k,182) + rxt(k,183) &
                      ) * y(k,154) + rxt(k,192)*y(k,248))
         mat(k,2605) = -rxt(k,180)*y(k,141)
         mat(k,2467) = -(rxt(k,181) + rxt(k,182) + rxt(k,183)) * y(k,141)
         mat(k,2081) = -rxt(k,192)*y(k,141)
         mat(k,217) = -((rxt(k,196) + rxt(k,197)) * y(k,247))
         mat(k,1966) = -(rxt(k,196) + rxt(k,197)) * y(k,142)
         mat(k,573) = rxt(k,181)*y(k,154)
         mat(k,2458) = rxt(k,181)*y(k,141)
         mat(k,2461) = rxt(k,199)*y(k,155)
         mat(k,2335) = rxt(k,199)*y(k,154)
         mat(k,456) = -(rxt(k,481)*y(k,248))
         mat(k,2065) = -rxt(k,481)*y(k,144)
         mat(k,1614) = .200_r8*rxt(k,473)*y(k,237)
         mat(k,1312) = .200_r8*rxt(k,473)*y(k,228)
         mat(k,1137) = -(rxt(k,482)*y(k,248))
         mat(k,2134) = -rxt(k,482)*y(k,145)
         mat(k,2635) = rxt(k,475)*y(k,237)
         mat(k,2354) = rxt(k,476)*y(k,237)
         mat(k,1487) = rxt(k,472)*y(k,237)
         mat(k,1627) = .800_r8*rxt(k,473)*y(k,237)
         mat(k,1316) = rxt(k,475)*y(k,153) + rxt(k,476)*y(k,155) + rxt(k,472)*y(k,227) &
                      + .800_r8*rxt(k,473)*y(k,228)
         mat(k,110) = -(rxt(k,593)*y(k,248))
         mat(k,2017) = -rxt(k,593)*y(k,149)
         mat(k,2679) = -(rxt(k,180)*y(k,141) + rxt(k,189)*y(k,155) + rxt(k,193) &
                      *y(k,233) + rxt(k,194)*y(k,163) + rxt(k,195)*y(k,162) + rxt(k,218) &
                      *y(k,74) + rxt(k,252)*y(k,21) + rxt(k,280)*y(k,115) + rxt(k,289) &
                      *y(k,125) + rxt(k,296)*y(k,167) + rxt(k,336)*y(k,68) + rxt(k,355) &
                      *y(k,228) + rxt(k,363)*y(k,234) + rxt(k,376)*y(k,224) + rxt(k,387) &
                      *y(k,227) + rxt(k,391)*y(k,232) + rxt(k,404)*y(k,225) + rxt(k,412) &
                      *y(k,250) + rxt(k,416)*y(k,251) + (rxt(k,422) + rxt(k,423) &
                      ) * y(k,230) + (rxt(k,429) + rxt(k,430)) * y(k,240) + rxt(k,438) &
                      *y(k,242) + rxt(k,441)*y(k,244) + (rxt(k,451) + rxt(k,452) &
                      ) * y(k,219) + rxt(k,461)*y(k,235) + rxt(k,467)*y(k,236) &
                      + rxt(k,475)*y(k,237) + rxt(k,486)*y(k,256) + rxt(k,490) &
                      *y(k,218) + rxt(k,493)*y(k,221) + rxt(k,498)*y(k,223) + rxt(k,500) &
                      *y(k,226) + rxt(k,504)*y(k,229) + rxt(k,507)*y(k,241) + rxt(k,510) &
                      *y(k,243) + rxt(k,513)*y(k,249) + rxt(k,520)*y(k,254) + rxt(k,526) &
                      *y(k,257) + rxt(k,529)*y(k,259) + rxt(k,540)*y(k,246) + rxt(k,545) &
                      *y(k,252) + rxt(k,550)*y(k,253))
         mat(k,580) = -rxt(k,180)*y(k,153)
         mat(k,2400) = -rxt(k,189)*y(k,153)
         mat(k,1876) = -rxt(k,193)*y(k,153)
         mat(k,2578) = -rxt(k,194)*y(k,153)
         mat(k,2217) = -rxt(k,195)*y(k,153)
         mat(k,2302) = -rxt(k,218)*y(k,153)
         mat(k,2455) = -rxt(k,252)*y(k,153)
         mat(k,2428) = -rxt(k,280)*y(k,153)
         mat(k,2248) = -rxt(k,289)*y(k,153)
         mat(k,601) = -rxt(k,296)*y(k,153)
         mat(k,1109) = -rxt(k,336)*y(k,153)
         mat(k,1663) = -rxt(k,355)*y(k,153)
         mat(k,523) = -rxt(k,363)*y(k,153)
         mat(k,896) = -rxt(k,376)*y(k,153)
         mat(k,1511) = -rxt(k,387)*y(k,153)
         mat(k,803) = -rxt(k,391)*y(k,153)
         mat(k,881) = -rxt(k,404)*y(k,153)
         mat(k,866) = -rxt(k,412)*y(k,153)
         mat(k,1276) = -rxt(k,416)*y(k,153)
         mat(k,654) = -(rxt(k,422) + rxt(k,423)) * y(k,153)
         mat(k,1436) = -(rxt(k,429) + rxt(k,430)) * y(k,153)
         mat(k,1479) = -rxt(k,438)*y(k,153)
         mat(k,774) = -rxt(k,441)*y(k,153)
         mat(k,1007) = -(rxt(k,451) + rxt(k,452)) * y(k,153)
         mat(k,1382) = -rxt(k,461)*y(k,153)
         mat(k,1415) = -rxt(k,467)*y(k,153)
         mat(k,1333) = -rxt(k,475)*y(k,153)
         mat(k,1310) = -rxt(k,486)*y(k,153)
         mat(k,619) = -rxt(k,490)*y(k,153)
         mat(k,572) = -rxt(k,493)*y(k,153)
         mat(k,517) = -rxt(k,498)*y(k,153)
         mat(k,721) = -rxt(k,500)*y(k,153)
         mat(k,857) = -rxt(k,504)*y(k,153)
         mat(k,809) = -rxt(k,507)*y(k,153)
         mat(k,964) = -rxt(k,510)*y(k,153)
         mat(k,530) = -rxt(k,513)*y(k,153)
         mat(k,824) = -rxt(k,520)*y(k,153)
         mat(k,849) = -rxt(k,526)*y(k,153)
         mat(k,588) = -rxt(k,529)*y(k,153)
         mat(k,1181) = -rxt(k,540)*y(k,153)
         mat(k,1241) = -rxt(k,545)*y(k,153)
         mat(k,1263) = -rxt(k,550)*y(k,153)
         mat(k,216) = 4.000_r8*rxt(k,279)*y(k,123)
         mat(k,580) = mat(k,580) + 2.000_r8*rxt(k,182)*y(k,154) + rxt(k,192)*y(k,248)
         mat(k,219) = 2.000_r8*rxt(k,196)*y(k,247)
         mat(k,2512) = 2.000_r8*rxt(k,182)*y(k,141) + rxt(k,185)*y(k,162) + rxt(k,569) &
                      *y(k,180)
         mat(k,2217) = mat(k,2217) + rxt(k,185)*y(k,154)
         mat(k,1569) = rxt(k,569)*y(k,154)
         mat(k,2000) = 2.000_r8*rxt(k,196)*y(k,142)
         mat(k,2182) = rxt(k,192)*y(k,141)
         mat(k,2510) = -((rxt(k,181) + rxt(k,182) + rxt(k,183)) * y(k,141) + (rxt(k,185) &
                      + rxt(k,187)) * y(k,162) + rxt(k,186)*y(k,163) + rxt(k,198) &
                      *y(k,233) + rxt(k,199)*y(k,155) + rxt(k,200)*y(k,248) + rxt(k,210) &
                      *y(k,70) + rxt(k,220)*y(k,74) + rxt(k,245)*y(k,17) + rxt(k,255) &
                      *y(k,21) + rxt(k,276)*y(k,115) + rxt(k,290)*y(k,125) + rxt(k,398) &
                      *y(k,227) + rxt(k,447)*y(k,242) + rxt(k,505)*y(k,229) + rxt(k,508) &
                      *y(k,241) + rxt(k,511)*y(k,243) + rxt(k,515)*y(k,171) + rxt(k,518) &
                      *y(k,218) + rxt(k,569)*y(k,180))
         mat(k,579) = -(rxt(k,181) + rxt(k,182) + rxt(k,183)) * y(k,154)
         mat(k,2215) = -(rxt(k,185) + rxt(k,187)) * y(k,154)
         mat(k,2576) = -rxt(k,186)*y(k,154)
         mat(k,1874) = -rxt(k,198)*y(k,154)
         mat(k,2398) = -rxt(k,199)*y(k,154)
         mat(k,2180) = -rxt(k,200)*y(k,154)
         mat(k,1929) = -rxt(k,210)*y(k,154)
         mat(k,2300) = -rxt(k,220)*y(k,154)
         mat(k,2328) = -rxt(k,245)*y(k,154)
         mat(k,2453) = -rxt(k,255)*y(k,154)
         mat(k,2426) = -rxt(k,276)*y(k,154)
         mat(k,2246) = -rxt(k,290)*y(k,154)
         mat(k,1509) = -rxt(k,398)*y(k,154)
         mat(k,1477) = -rxt(k,447)*y(k,154)
         mat(k,856) = -rxt(k,505)*y(k,154)
         mat(k,808) = -rxt(k,508)*y(k,154)
         mat(k,963) = -rxt(k,511)*y(k,154)
         mat(k,545) = -rxt(k,515)*y(k,154)
         mat(k,618) = -rxt(k,518)*y(k,154)
         mat(k,1567) = -rxt(k,569)*y(k,154)
         mat(k,766) = rxt(k,449)*y(k,248)
         mat(k,416) = rxt(k,420)*y(k,155)
         mat(k,2453) = mat(k,2453) + rxt(k,252)*y(k,153)
         mat(k,1108) = rxt(k,336)*y(k,153) + rxt(k,337)*y(k,155)
         mat(k,594) = rxt(k,211)*y(k,248)
         mat(k,2300) = mat(k,2300) + rxt(k,218)*y(k,153)
         mat(k,454) = rxt(k,179)*y(k,248)
         mat(k,2426) = mat(k,2426) + rxt(k,278)*y(k,155)
         mat(k,317) = 4.000_r8*rxt(k,275)*y(k,124)
         mat(k,2246) = mat(k,2246) + rxt(k,289)*y(k,153) + rxt(k,291)*y(k,155)
         mat(k,691) = .700_r8*rxt(k,469)*y(k,248)
         mat(k,2677) = rxt(k,252)*y(k,21) + rxt(k,336)*y(k,68) + rxt(k,218)*y(k,74) &
                      + rxt(k,289)*y(k,125) + 2.000_r8*rxt(k,189)*y(k,155) &
                      + rxt(k,195)*y(k,162) + rxt(k,194)*y(k,163) + rxt(k,296) &
                      *y(k,167) + rxt(k,490)*y(k,218) + rxt(k,451)*y(k,219) &
                      + rxt(k,493)*y(k,221) + rxt(k,498)*y(k,223) + rxt(k,376) &
                      *y(k,224) + rxt(k,404)*y(k,225) + rxt(k,500)*y(k,226) &
                      + rxt(k,387)*y(k,227) + rxt(k,355)*y(k,228) + rxt(k,504) &
                      *y(k,229) + rxt(k,422)*y(k,230) + rxt(k,391)*y(k,232) &
                      + rxt(k,193)*y(k,233) + rxt(k,363)*y(k,234) + .920_r8*rxt(k,461) &
                      *y(k,235) + .920_r8*rxt(k,467)*y(k,236) + rxt(k,475)*y(k,237) &
                      + rxt(k,429)*y(k,240) + rxt(k,507)*y(k,241) + rxt(k,438) &
                      *y(k,242) + rxt(k,510)*y(k,243) + rxt(k,441)*y(k,244) &
                      + 1.600_r8*rxt(k,540)*y(k,246) + rxt(k,513)*y(k,249) &
                      + rxt(k,412)*y(k,250) + rxt(k,416)*y(k,251) + .900_r8*rxt(k,545) &
                      *y(k,252) + .800_r8*rxt(k,550)*y(k,253) + rxt(k,520)*y(k,254) &
                      + rxt(k,486)*y(k,256) + rxt(k,526)*y(k,257) + rxt(k,529) &
                      *y(k,259)
         mat(k,2398) = mat(k,2398) + rxt(k,420)*y(k,16) + rxt(k,337)*y(k,68) &
                      + rxt(k,278)*y(k,115) + rxt(k,291)*y(k,125) &
                      + 2.000_r8*rxt(k,189)*y(k,153) + rxt(k,190)*y(k,162) &
                      + rxt(k,188)*y(k,233) + rxt(k,462)*y(k,235) + rxt(k,468) &
                      *y(k,236) + rxt(k,476)*y(k,237) + rxt(k,428)*y(k,240) &
                      + rxt(k,439)*y(k,242) + 2.000_r8*rxt(k,541)*y(k,246) &
                      + rxt(k,191)*y(k,248) + rxt(k,487)*y(k,256)
         mat(k,914) = rxt(k,410)*y(k,248)
         mat(k,2215) = mat(k,2215) + rxt(k,195)*y(k,153) + rxt(k,190)*y(k,155)
         mat(k,2576) = mat(k,2576) + rxt(k,194)*y(k,153)
         mat(k,600) = rxt(k,296)*y(k,153)
         mat(k,713) = rxt(k,547)*y(k,248)
         mat(k,618) = mat(k,618) + rxt(k,490)*y(k,153)
         mat(k,1006) = rxt(k,451)*y(k,153)
         mat(k,571) = rxt(k,493)*y(k,153)
         mat(k,516) = rxt(k,498)*y(k,153)
         mat(k,895) = rxt(k,376)*y(k,153)
         mat(k,880) = rxt(k,404)*y(k,153)
         mat(k,719) = rxt(k,500)*y(k,153)
         mat(k,1509) = mat(k,1509) + rxt(k,387)*y(k,153)
         mat(k,1661) = rxt(k,355)*y(k,153) + .500_r8*rxt(k,538)*y(k,246)
         mat(k,856) = mat(k,856) + rxt(k,504)*y(k,153)
         mat(k,653) = rxt(k,422)*y(k,153)
         mat(k,802) = rxt(k,391)*y(k,153)
         mat(k,1874) = mat(k,1874) + rxt(k,193)*y(k,153) + rxt(k,188)*y(k,155)
         mat(k,522) = rxt(k,363)*y(k,153)
         mat(k,1380) = .920_r8*rxt(k,461)*y(k,153) + rxt(k,462)*y(k,155)
         mat(k,1413) = .920_r8*rxt(k,467)*y(k,153) + rxt(k,468)*y(k,155)
         mat(k,1332) = rxt(k,475)*y(k,153) + rxt(k,476)*y(k,155)
         mat(k,1435) = rxt(k,429)*y(k,153) + rxt(k,428)*y(k,155)
         mat(k,808) = mat(k,808) + rxt(k,507)*y(k,153)
         mat(k,1477) = mat(k,1477) + rxt(k,438)*y(k,153) + rxt(k,439)*y(k,155)
         mat(k,963) = mat(k,963) + rxt(k,510)*y(k,153)
         mat(k,773) = rxt(k,441)*y(k,153)
         mat(k,1180) = 1.600_r8*rxt(k,540)*y(k,153) + 2.000_r8*rxt(k,541)*y(k,155) &
                      + .500_r8*rxt(k,538)*y(k,228)
         mat(k,2180) = mat(k,2180) + rxt(k,449)*y(k,1) + rxt(k,211)*y(k,73) &
                      + rxt(k,179)*y(k,107) + .700_r8*rxt(k,469)*y(k,128) + rxt(k,191) &
                      *y(k,155) + rxt(k,410)*y(k,156) + rxt(k,547)*y(k,205)
         mat(k,529) = rxt(k,513)*y(k,153)
         mat(k,865) = rxt(k,412)*y(k,153)
         mat(k,1275) = rxt(k,416)*y(k,153)
         mat(k,1240) = .900_r8*rxt(k,545)*y(k,153)
         mat(k,1262) = .800_r8*rxt(k,550)*y(k,153)
         mat(k,823) = rxt(k,520)*y(k,153)
         mat(k,1309) = rxt(k,486)*y(k,153) + rxt(k,487)*y(k,155)
         mat(k,848) = rxt(k,526)*y(k,153)
         mat(k,587) = rxt(k,529)*y(k,153)
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
         mat(k,2395) = -(rxt(k,188)*y(k,233) + rxt(k,189)*y(k,153) + rxt(k,190) &
                      *y(k,162) + rxt(k,191)*y(k,248) + rxt(k,199)*y(k,154) + rxt(k,267) &
                      *y(k,105) + rxt(k,269)*y(k,116) + rxt(k,278)*y(k,115) + rxt(k,291) &
                      *y(k,125) + rxt(k,297)*y(k,110) + rxt(k,337)*y(k,68) + rxt(k,349) &
                      *y(k,51) + rxt(k,381)*y(k,54) + rxt(k,400)*y(k,33) + rxt(k,407) &
                      *y(k,58) + rxt(k,420)*y(k,16) + rxt(k,428)*y(k,240) + rxt(k,439) &
                      *y(k,242) + rxt(k,462)*y(k,235) + rxt(k,468)*y(k,236) + rxt(k,471) &
                      *y(k,127) + rxt(k,476)*y(k,237) + rxt(k,487)*y(k,256) + rxt(k,532) &
                      *y(k,6) + rxt(k,535)*y(k,139) + rxt(k,541)*y(k,246) + rxt(k,552) &
                      *y(k,207) + rxt(k,559)*y(k,83))
         mat(k,1871) = -rxt(k,188)*y(k,155)
         mat(k,2674) = -rxt(k,189)*y(k,155)
         mat(k,2212) = -rxt(k,190)*y(k,155)
         mat(k,2177) = -rxt(k,191)*y(k,155)
         mat(k,2507) = -rxt(k,199)*y(k,155)
         mat(k,1290) = -rxt(k,267)*y(k,155)
         mat(k,1538) = -rxt(k,269)*y(k,155)
         mat(k,2423) = -rxt(k,278)*y(k,155)
         mat(k,2243) = -rxt(k,291)*y(k,155)
         mat(k,1606) = -rxt(k,297)*y(k,155)
         mat(k,1107) = -rxt(k,337)*y(k,155)
         mat(k,1710) = -rxt(k,349)*y(k,155)
         mat(k,1156) = -rxt(k,381)*y(k,155)
         mat(k,1128) = -rxt(k,400)*y(k,155)
         mat(k,1354) = -rxt(k,407)*y(k,155)
         mat(k,415) = -rxt(k,420)*y(k,155)
         mat(k,1434) = -rxt(k,428)*y(k,155)
         mat(k,1476) = -rxt(k,439)*y(k,155)
         mat(k,1379) = -rxt(k,462)*y(k,155)
         mat(k,1412) = -rxt(k,468)*y(k,155)
         mat(k,932) = -rxt(k,471)*y(k,155)
         mat(k,1331) = -rxt(k,476)*y(k,155)
         mat(k,1308) = -rxt(k,487)*y(k,155)
         mat(k,1086) = -rxt(k,532)*y(k,155)
         mat(k,991) = -rxt(k,535)*y(k,155)
         mat(k,1179) = -rxt(k,541)*y(k,155)
         mat(k,1098) = -rxt(k,552)*y(k,155)
         mat(k,1034) = -rxt(k,559)*y(k,155)
         mat(k,2325) = rxt(k,253)*y(k,22)
         mat(k,906) = rxt(k,253)*y(k,17) + rxt(k,254)*y(k,70) + rxt(k,256)*y(k,162)
         mat(k,1926) = rxt(k,254)*y(k,22) + rxt(k,219)*y(k,75)
         mat(k,1017) = rxt(k,219)*y(k,70) + rxt(k,221)*y(k,162) + rxt(k,222)*y(k,248)
         mat(k,954) = rxt(k,309)*y(k,106)
         mat(k,2268) = rxt(k,309)*y(k,89) + rxt(k,201)*y(k,248)
         mat(k,2423) = mat(k,2423) + rxt(k,274)*y(k,126)
         mat(k,943) = rxt(k,274)*y(k,115)
         mat(k,706) = .500_r8*rxt(k,444)*y(k,248)
         mat(k,2507) = mat(k,2507) + rxt(k,187)*y(k,162) + rxt(k,186)*y(k,163)
         mat(k,2212) = mat(k,2212) + rxt(k,256)*y(k,22) + rxt(k,221)*y(k,75) &
                      + rxt(k,187)*y(k,154)
         mat(k,2573) = rxt(k,186)*y(k,154)
         mat(k,629) = rxt(k,396)*y(k,248)
         mat(k,2177) = mat(k,2177) + rxt(k,222)*y(k,75) + rxt(k,201)*y(k,106) &
                      + .500_r8*rxt(k,444)*y(k,138) + rxt(k,396)*y(k,169)
         mat(k,909) = -(rxt(k,410)*y(k,248))
         mat(k,2118) = -rxt(k,410)*y(k,156)
         mat(k,1114) = rxt(k,400)*y(k,155)
         mat(k,656) = .500_r8*rxt(k,470)*y(k,248)
         mat(k,446) = rxt(k,477)*y(k,248)
         mat(k,457) = rxt(k,481)*y(k,248)
         mat(k,1134) = rxt(k,482)*y(k,248)
         mat(k,2340) = rxt(k,400)*y(k,33)
         mat(k,2118) = mat(k,2118) + .500_r8*rxt(k,470)*y(k,129) + rxt(k,477)*y(k,130) &
                      + rxt(k,481)*y(k,144) + rxt(k,482)*y(k,145)
         mat(k,462) = -(rxt(k,542)*y(k,248))
         mat(k,2066) = -rxt(k,542)*y(k,157)
         mat(k,1788) = rxt(k,539)*y(k,246)
         mat(k,1166) = rxt(k,539)*y(k,233)
         mat(k,2207) = -(rxt(k,159)*y(k,163) + 4._r8*rxt(k,160)*y(k,162) + rxt(k,162) &
                      *y(k,93) + rxt(k,163)*y(k,95) + rxt(k,168)*y(k,233) + rxt(k,174) &
                      *y(k,248) + (rxt(k,185) + rxt(k,187)) * y(k,154) + rxt(k,190) &
                      *y(k,155) + rxt(k,195)*y(k,153) + rxt(k,221)*y(k,75) + rxt(k,223) &
                      *y(k,74) + rxt(k,226)*y(k,101) + rxt(k,229)*y(k,109) + rxt(k,256) &
                      *y(k,22) + rxt(k,257)*y(k,21) + rxt(k,259)*y(k,97) + rxt(k,261) &
                      *y(k,108) + rxt(k,270)*y(k,116) + rxt(k,292)*y(k,125) + rxt(k,350) &
                      *y(k,51) + rxt(k,561)*y(k,166))
         mat(k,2568) = -rxt(k,159)*y(k,162)
         mat(k,1551) = -rxt(k,162)*y(k,162)
         mat(k,677) = -rxt(k,163)*y(k,162)
         mat(k,1866) = -rxt(k,168)*y(k,162)
         mat(k,2172) = -rxt(k,174)*y(k,162)
         mat(k,2502) = -(rxt(k,185) + rxt(k,187)) * y(k,162)
         mat(k,2390) = -rxt(k,190)*y(k,162)
         mat(k,2669) = -rxt(k,195)*y(k,162)
         mat(k,1014) = -rxt(k,221)*y(k,162)
         mat(k,2292) = -rxt(k,223)*y(k,162)
         mat(k,1730) = -rxt(k,226)*y(k,162)
         mat(k,1754) = -rxt(k,229)*y(k,162)
         mat(k,903) = -rxt(k,256)*y(k,162)
         mat(k,2445) = -rxt(k,257)*y(k,162)
         mat(k,1524) = -rxt(k,259)*y(k,162)
         mat(k,1679) = -rxt(k,261)*y(k,162)
         mat(k,1534) = -rxt(k,270)*y(k,162)
         mat(k,2238) = -rxt(k,292)*y(k,162)
         mat(k,1705) = -rxt(k,350)*y(k,162)
         mat(k,424) = -rxt(k,561)*y(k,162)
         mat(k,1944) = rxt(k,166)*y(k,233)
         mat(k,578) = rxt(k,180)*y(k,153) + rxt(k,181)*y(k,154)
         mat(k,2669) = mat(k,2669) + rxt(k,180)*y(k,141)
         mat(k,2502) = mat(k,2502) + rxt(k,181)*y(k,141)
         mat(k,2568) = mat(k,2568) + 2.000_r8*rxt(k,158)*y(k,247)
         mat(k,1866) = mat(k,1866) + rxt(k,166)*y(k,92)
         mat(k,1990) = 2.000_r8*rxt(k,158)*y(k,163)
         mat(k,2172) = mat(k,2172) + 2.000_r8*rxt(k,176)*y(k,248)
         mat(k,2577) = -((rxt(k,157) + rxt(k,158)) * y(k,247) + rxt(k,159)*y(k,162) &
                      + rxt(k,169)*y(k,233) + rxt(k,170)*y(k,92) + rxt(k,175)*y(k,248) &
                      + rxt(k,186)*y(k,154) + rxt(k,194)*y(k,153) + rxt(k,212)*y(k,70) &
                      + rxt(k,246)*y(k,17) + rxt(k,281)*y(k,115) + rxt(k,293)*y(k,125) &
                      + rxt(k,372)*y(k,28) + rxt(k,401)*y(k,33) + rxt(k,431)*y(k,134) &
                      + rxt(k,445)*y(k,140) + rxt(k,478)*y(k,127) + rxt(k,516) &
                      *y(k,171) + rxt(k,533)*y(k,6) + rxt(k,536)*y(k,139) + rxt(k,565) &
                      *y(k,178) + rxt(k,571)*y(k,180))
         mat(k,1999) = -(rxt(k,157) + rxt(k,158)) * y(k,163)
         mat(k,2216) = -rxt(k,159)*y(k,163)
         mat(k,1875) = -rxt(k,169)*y(k,163)
         mat(k,1953) = -rxt(k,170)*y(k,163)
         mat(k,2181) = -rxt(k,175)*y(k,163)
         mat(k,2511) = -rxt(k,186)*y(k,163)
         mat(k,2678) = -rxt(k,194)*y(k,163)
         mat(k,1930) = -rxt(k,212)*y(k,163)
         mat(k,2329) = -rxt(k,246)*y(k,163)
         mat(k,2427) = -rxt(k,281)*y(k,163)
         mat(k,2247) = -rxt(k,293)*y(k,163)
         mat(k,646) = -rxt(k,372)*y(k,163)
         mat(k,1130) = -rxt(k,401)*y(k,163)
         mat(k,1345) = -rxt(k,431)*y(k,163)
         mat(k,1458) = -rxt(k,445)*y(k,163)
         mat(k,933) = -rxt(k,478)*y(k,163)
         mat(k,546) = -rxt(k,516)*y(k,163)
         mat(k,1088) = -rxt(k,533)*y(k,163)
         mat(k,992) = -rxt(k,536)*y(k,163)
         mat(k,607) = -rxt(k,565)*y(k,163)
         mat(k,1568) = -rxt(k,571)*y(k,163)
         mat(k,1510) = .150_r8*rxt(k,386)*y(k,233)
         mat(k,1875) = mat(k,1875) + .150_r8*rxt(k,386)*y(k,227) + .150_r8*rxt(k,436) &
                      *y(k,242)
         mat(k,1478) = .150_r8*rxt(k,436)*y(k,233)
         mat(k,531) = -(rxt(k,572)*y(k,180))
         mat(k,1555) = -rxt(k,572)*y(k,165)
         mat(k,2430) = rxt(k,248)*y(k,74)
         mat(k,2277) = rxt(k,248)*y(k,21) + 2.000_r8*rxt(k,216)*y(k,74) + rxt(k,285) &
                      *y(k,125)
         mat(k,2221) = rxt(k,285)*y(k,74)
         mat(k,417) = -(rxt(k,561)*y(k,162) + rxt(k,562)*y(k,248))
         mat(k,2184) = -rxt(k,561)*y(k,166)
         mat(k,2060) = -rxt(k,562)*y(k,166)
         mat(k,597) = -(rxt(k,296)*y(k,153) + rxt(k,304)*y(k,125) + 4._r8*rxt(k,305) &
                      *y(k,167))
         mat(k,2607) = -rxt(k,296)*y(k,167)
         mat(k,2223) = -rxt(k,304)*y(k,167)
         mat(k,2431) = rxt(k,284)*y(k,125)
         mat(k,2223) = mat(k,2223) + rxt(k,284)*y(k,21) + 2.000_r8*rxt(k,301)*y(k,125) &
                      + rxt(k,291)*y(k,155) + rxt(k,293)*y(k,163)
         mat(k,2338) = rxt(k,291)*y(k,125)
         mat(k,2522) = rxt(k,293)*y(k,125)
         mat(k,1199) = rxt(k,424)*y(k,248)
         mat(k,2594) = .100_r8*rxt(k,545)*y(k,252)
         mat(k,2042) = rxt(k,424)*y(k,111)
         mat(k,1223) = .100_r8*rxt(k,545)*y(k,153)
         mat(k,623) = -(rxt(k,396)*y(k,248))
         mat(k,2087) = -rxt(k,396)*y(k,169)
         mat(k,2471) = rxt(k,398)*y(k,227)
         mat(k,1482) = rxt(k,398)*y(k,154)
         mat(k,2457) = rxt(k,518)*y(k,218)
         mat(k,613) = rxt(k,518)*y(k,154)
         mat(k,543) = -(rxt(k,515)*y(k,154) + rxt(k,516)*y(k,163))
         mat(k,2465) = -rxt(k,515)*y(k,171)
         mat(k,2521) = -rxt(k,516)*y(k,171)
         mat(k,234) = .070_r8*rxt(k,502)*y(k,248)
         mat(k,2603) = rxt(k,500)*y(k,226)
         mat(k,204) = .060_r8*rxt(k,514)*y(k,248)
         mat(k,255) = .070_r8*rxt(k,530)*y(k,248)
         mat(k,715) = rxt(k,500)*y(k,153)
         mat(k,2077) = .070_r8*rxt(k,502)*y(k,82) + .060_r8*rxt(k,514)*y(k,172) &
                      + .070_r8*rxt(k,530)*y(k,214)
         mat(k,202) = -(rxt(k,514)*y(k,248))
         mat(k,2026) = -rxt(k,514)*y(k,172)
         mat(k,194) = .530_r8*rxt(k,491)*y(k,248)
         mat(k,2026) = mat(k,2026) + .530_r8*rxt(k,491)*y(k,7)
         mat(k,374) = -(rxt(k,517)*y(k,248))
         mat(k,2054) = -rxt(k,517)*y(k,173)
         mat(k,1782) = rxt(k,512)*y(k,249)
         mat(k,524) = rxt(k,512)*y(k,233)
         mat(k,631) = -(rxt(k,413)*y(k,248))
         mat(k,2088) = -rxt(k,413)*y(k,176)
         mat(k,1803) = rxt(k,411)*y(k,250)
         mat(k,858) = rxt(k,411)*y(k,233)
         mat(k,468) = -(rxt(k,417)*y(k,248))
         mat(k,2067) = -rxt(k,417)*y(k,177)
         mat(k,1789) = .850_r8*rxt(k,415)*y(k,251)
         mat(k,1265) = .850_r8*rxt(k,415)*y(k,233)
         mat(k,602) = -(rxt(k,565)*y(k,163) + rxt(k,568)*y(k,248))
         mat(k,2523) = -rxt(k,565)*y(k,178)
         mat(k,2084) = -rxt(k,568)*y(k,178)
         mat(k,1558) = -(rxt(k,566)*y(k,21) + rxt(k,567)*y(k,74) + rxt(k,569)*y(k,154) &
                      + rxt(k,571)*y(k,163) + rxt(k,572)*y(k,165) + rxt(k,573) &
                      *y(k,248))
         mat(k,2436) = -rxt(k,566)*y(k,180)
         mat(k,2282) = -rxt(k,567)*y(k,180)
         mat(k,2489) = -rxt(k,569)*y(k,180)
         mat(k,2557) = -rxt(k,571)*y(k,180)
         mat(k,533) = -rxt(k,572)*y(k,180)
         mat(k,2159) = -rxt(k,573)*y(k,180)
         mat(k,2194) = rxt(k,561)*y(k,166)
         mat(k,2557) = mat(k,2557) + rxt(k,565)*y(k,178)
         mat(k,421) = rxt(k,561)*y(k,162)
         mat(k,603) = rxt(k,565)*y(k,163) + rxt(k,568)*y(k,248)
         mat(k,2159) = mat(k,2159) + rxt(k,568)*y(k,178)
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
         mat(k,1038) = -(rxt(k,564)*y(k,248))
         mat(k,2127) = -rxt(k,564)*y(k,181)
         mat(k,2435) = rxt(k,555)*y(k,83) + rxt(k,566)*y(k,180)
         mat(k,1900) = rxt(k,557)*y(k,83)
         mat(k,2281) = rxt(k,567)*y(k,180)
         mat(k,1026) = rxt(k,555)*y(k,21) + rxt(k,557)*y(k,70) + rxt(k,558)*y(k,125) &
                      + rxt(k,559)*y(k,155) + (rxt(k,560)+.500_r8*rxt(k,574))*y(k,248)
         mat(k,2226) = rxt(k,558)*y(k,83)
         mat(k,2481) = rxt(k,569)*y(k,180)
         mat(k,2347) = rxt(k,559)*y(k,83)
         mat(k,2533) = rxt(k,571)*y(k,180)
         mat(k,532) = rxt(k,572)*y(k,180)
         mat(k,419) = rxt(k,562)*y(k,248)
         mat(k,1557) = rxt(k,566)*y(k,21) + rxt(k,567)*y(k,74) + rxt(k,569)*y(k,154) &
                      + rxt(k,571)*y(k,163) + rxt(k,572)*y(k,165) + rxt(k,573) &
                      *y(k,248)
         mat(k,2127) = mat(k,2127) + (rxt(k,560)+.500_r8*rxt(k,574))*y(k,83) &
                      + rxt(k,562)*y(k,166) + rxt(k,573)*y(k,180)
         mat(k,296) = -(rxt(k,575)*y(k,260))
         mat(k,2682) = -rxt(k,575)*y(k,182)
         mat(k,1037) = rxt(k,564)*y(k,248)
         mat(k,2044) = rxt(k,564)*y(k,181)
         mat(k,1058) = .2202005_r8*rxt(k,628)*y(k,163)
         mat(k,965) = .0508005_r8*rxt(k,644)*y(k,163)
         mat(k,2580) = .1279005_r8*rxt(k,627)*y(k,220) + .0097005_r8*rxt(k,632) &
                      *y(k,222) + .0003005_r8*rxt(k,635)*y(k,238) &
                      + .1056005_r8*rxt(k,639)*y(k,239) + .0245005_r8*rxt(k,643) &
                      *y(k,245) + .0154005_r8*rxt(k,649)*y(k,255) &
                      + .0063005_r8*rxt(k,653)*y(k,258)
         mat(k,2514) = .2202005_r8*rxt(k,628)*y(k,6) + .0508005_r8*rxt(k,644)*y(k,139)
         mat(k,52) = .5931005_r8*rxt(k,646)*y(k,248)
         mat(k,58) = .1279005_r8*rxt(k,627)*y(k,153) + .2202005_r8*rxt(k,626)*y(k,233)
         mat(k,64) = .0097005_r8*rxt(k,632)*y(k,153) + .0023005_r8*rxt(k,631)*y(k,233)
         mat(k,1764) = .2202005_r8*rxt(k,626)*y(k,220) + .0023005_r8*rxt(k,631) &
                      *y(k,222) + .0031005_r8*rxt(k,634)*y(k,238) &
                      + .2381005_r8*rxt(k,638)*y(k,239) + .0508005_r8*rxt(k,642) &
                      *y(k,245) + .1364005_r8*rxt(k,648)*y(k,255) &
                      + .1677005_r8*rxt(k,652)*y(k,258)
         mat(k,70) = .0003005_r8*rxt(k,635)*y(k,153) + .0031005_r8*rxt(k,634)*y(k,233)
         mat(k,76) = .1056005_r8*rxt(k,639)*y(k,153) + .2381005_r8*rxt(k,638)*y(k,233)
         mat(k,84) = .0245005_r8*rxt(k,643)*y(k,153) + .0508005_r8*rxt(k,642)*y(k,233)
         mat(k,2002) = .5931005_r8*rxt(k,646)*y(k,202)
         mat(k,90) = .0154005_r8*rxt(k,649)*y(k,153) + .1364005_r8*rxt(k,648)*y(k,233)
         mat(k,96) = .0063005_r8*rxt(k,653)*y(k,153) + .1677005_r8*rxt(k,652)*y(k,233)
         mat(k,1059) = .2067005_r8*rxt(k,628)*y(k,163)
         mat(k,966) = .1149005_r8*rxt(k,644)*y(k,163)
         mat(k,2581) = .1792005_r8*rxt(k,627)*y(k,220) + .0034005_r8*rxt(k,632) &
                      *y(k,222) + .0003005_r8*rxt(k,635)*y(k,238) &
                      + .1026005_r8*rxt(k,639)*y(k,239) + .0082005_r8*rxt(k,643) &
                      *y(k,245) + .0452005_r8*rxt(k,649)*y(k,255) &
                      + .0237005_r8*rxt(k,653)*y(k,258)
         mat(k,2515) = .2067005_r8*rxt(k,628)*y(k,6) + .1149005_r8*rxt(k,644)*y(k,139)
         mat(k,53) = .1534005_r8*rxt(k,646)*y(k,248)
         mat(k,59) = .1792005_r8*rxt(k,627)*y(k,153) + .2067005_r8*rxt(k,626)*y(k,233)
         mat(k,65) = .0034005_r8*rxt(k,632)*y(k,153) + .0008005_r8*rxt(k,631)*y(k,233)
         mat(k,1765) = .2067005_r8*rxt(k,626)*y(k,220) + .0008005_r8*rxt(k,631) &
                      *y(k,222) + .0035005_r8*rxt(k,634)*y(k,238) &
                      + .1308005_r8*rxt(k,638)*y(k,239) + .1149005_r8*rxt(k,642) &
                      *y(k,245) + .0101005_r8*rxt(k,648)*y(k,255) &
                      + .0174005_r8*rxt(k,652)*y(k,258)
         mat(k,71) = .0003005_r8*rxt(k,635)*y(k,153) + .0035005_r8*rxt(k,634)*y(k,233)
         mat(k,77) = .1026005_r8*rxt(k,639)*y(k,153) + .1308005_r8*rxt(k,638)*y(k,233)
         mat(k,85) = .0082005_r8*rxt(k,643)*y(k,153) + .1149005_r8*rxt(k,642)*y(k,233)
         mat(k,2003) = .1534005_r8*rxt(k,646)*y(k,202)
         mat(k,91) = .0452005_r8*rxt(k,649)*y(k,153) + .0101005_r8*rxt(k,648)*y(k,233)
         mat(k,97) = .0237005_r8*rxt(k,653)*y(k,153) + .0174005_r8*rxt(k,652)*y(k,233)
         mat(k,1060) = .0653005_r8*rxt(k,628)*y(k,163)
         mat(k,967) = .0348005_r8*rxt(k,644)*y(k,163)
         mat(k,2582) = .0676005_r8*rxt(k,627)*y(k,220) + .1579005_r8*rxt(k,632) &
                      *y(k,222) + .0073005_r8*rxt(k,635)*y(k,238) &
                      + .0521005_r8*rxt(k,639)*y(k,239) + .0772005_r8*rxt(k,643) &
                      *y(k,245) + .0966005_r8*rxt(k,649)*y(k,255) &
                      + .0025005_r8*rxt(k,653)*y(k,258)
         mat(k,2516) = .0653005_r8*rxt(k,628)*y(k,6) + .0348005_r8*rxt(k,644)*y(k,139)
         mat(k,54) = .0459005_r8*rxt(k,646)*y(k,248)
         mat(k,60) = .0676005_r8*rxt(k,627)*y(k,153) + .0653005_r8*rxt(k,626)*y(k,233)
         mat(k,66) = .1579005_r8*rxt(k,632)*y(k,153) + .0843005_r8*rxt(k,631)*y(k,233)
         mat(k,1766) = .0653005_r8*rxt(k,626)*y(k,220) + .0843005_r8*rxt(k,631) &
                      *y(k,222) + .0003005_r8*rxt(k,634)*y(k,238) &
                      + .0348005_r8*rxt(k,638)*y(k,239) + .0348005_r8*rxt(k,642) &
                      *y(k,245) + .0763005_r8*rxt(k,648)*y(k,255) + .086_r8*rxt(k,652) &
                      *y(k,258)
         mat(k,72) = .0073005_r8*rxt(k,635)*y(k,153) + .0003005_r8*rxt(k,634)*y(k,233)
         mat(k,78) = .0521005_r8*rxt(k,639)*y(k,153) + .0348005_r8*rxt(k,638)*y(k,233)
         mat(k,86) = .0772005_r8*rxt(k,643)*y(k,153) + .0348005_r8*rxt(k,642)*y(k,233)
         mat(k,2004) = .0459005_r8*rxt(k,646)*y(k,202)
         mat(k,92) = .0966005_r8*rxt(k,649)*y(k,153) + .0763005_r8*rxt(k,648)*y(k,233)
         mat(k,98) = .0025005_r8*rxt(k,653)*y(k,153) + .086_r8*rxt(k,652)*y(k,233)
         mat(k,1061) = .1749305_r8*rxt(k,625)*y(k,155) + .1284005_r8*rxt(k,628) &
                      *y(k,163)
         mat(k,915) = .0590245_r8*rxt(k,633)*y(k,155) + .0033005_r8*rxt(k,636) &
                      *y(k,163)
         mat(k,968) = .1749305_r8*rxt(k,641)*y(k,155) + .0554005_r8*rxt(k,644) &
                      *y(k,163)
         mat(k,2583) = .079_r8*rxt(k,627)*y(k,220) + .0059005_r8*rxt(k,632)*y(k,222) &
                      + .0057005_r8*rxt(k,635)*y(k,238) + .0143005_r8*rxt(k,639) &
                      *y(k,239) + .0332005_r8*rxt(k,643)*y(k,245) &
                      + .0073005_r8*rxt(k,649)*y(k,255) + .011_r8*rxt(k,653)*y(k,258)
         mat(k,2332) = .1749305_r8*rxt(k,625)*y(k,6) + .0590245_r8*rxt(k,633)*y(k,127) &
                      + .1749305_r8*rxt(k,641)*y(k,139)
         mat(k,2517) = .1284005_r8*rxt(k,628)*y(k,6) + .0033005_r8*rxt(k,636)*y(k,127) &
                      + .0554005_r8*rxt(k,644)*y(k,139)
         mat(k,55) = .0085005_r8*rxt(k,646)*y(k,248)
         mat(k,61) = .079_r8*rxt(k,627)*y(k,153) + .1284005_r8*rxt(k,626)*y(k,233)
         mat(k,67) = .0059005_r8*rxt(k,632)*y(k,153) + .0443005_r8*rxt(k,631)*y(k,233)
         mat(k,1767) = .1284005_r8*rxt(k,626)*y(k,220) + .0443005_r8*rxt(k,631) &
                      *y(k,222) + .0271005_r8*rxt(k,634)*y(k,238) &
                      + .0076005_r8*rxt(k,638)*y(k,239) + .0554005_r8*rxt(k,642) &
                      *y(k,245) + .2157005_r8*rxt(k,648)*y(k,255) &
                      + .0512005_r8*rxt(k,652)*y(k,258)
         mat(k,73) = .0057005_r8*rxt(k,635)*y(k,153) + .0271005_r8*rxt(k,634)*y(k,233)
         mat(k,79) = .0143005_r8*rxt(k,639)*y(k,153) + .0076005_r8*rxt(k,638)*y(k,233)
         mat(k,87) = .0332005_r8*rxt(k,643)*y(k,153) + .0554005_r8*rxt(k,642)*y(k,233)
         mat(k,2005) = .0085005_r8*rxt(k,646)*y(k,202)
         mat(k,93) = .0073005_r8*rxt(k,649)*y(k,153) + .2157005_r8*rxt(k,648)*y(k,233)
         mat(k,99) = .011_r8*rxt(k,653)*y(k,153) + .0512005_r8*rxt(k,652)*y(k,233)
         mat(k,1062) = .5901905_r8*rxt(k,625)*y(k,155) + .114_r8*rxt(k,628)*y(k,163)
         mat(k,916) = .0250245_r8*rxt(k,633)*y(k,155)
         mat(k,969) = .5901905_r8*rxt(k,641)*y(k,155) + .1278005_r8*rxt(k,644) &
                      *y(k,163)
         mat(k,2584) = .1254005_r8*rxt(k,627)*y(k,220) + .0536005_r8*rxt(k,632) &
                      *y(k,222) + .0623005_r8*rxt(k,635)*y(k,238) &
                      + .0166005_r8*rxt(k,639)*y(k,239) + .130_r8*rxt(k,643)*y(k,245) &
                      + .238_r8*rxt(k,649)*y(k,255) + .1185005_r8*rxt(k,653)*y(k,258)
         mat(k,2333) = .5901905_r8*rxt(k,625)*y(k,6) + .0250245_r8*rxt(k,633)*y(k,127) &
                      + .5901905_r8*rxt(k,641)*y(k,139)
         mat(k,2518) = .114_r8*rxt(k,628)*y(k,6) + .1278005_r8*rxt(k,644)*y(k,139)
         mat(k,56) = .0128005_r8*rxt(k,646)*y(k,248)
         mat(k,62) = .1254005_r8*rxt(k,627)*y(k,153) + .114_r8*rxt(k,626)*y(k,233)
         mat(k,68) = .0536005_r8*rxt(k,632)*y(k,153) + .1621005_r8*rxt(k,631)*y(k,233)
         mat(k,1768) = .114_r8*rxt(k,626)*y(k,220) + .1621005_r8*rxt(k,631)*y(k,222) &
                      + .0474005_r8*rxt(k,634)*y(k,238) + .0113005_r8*rxt(k,638) &
                      *y(k,239) + .1278005_r8*rxt(k,642)*y(k,245) &
                      + .0738005_r8*rxt(k,648)*y(k,255) + .1598005_r8*rxt(k,652) &
                      *y(k,258)
         mat(k,74) = .0623005_r8*rxt(k,635)*y(k,153) + .0474005_r8*rxt(k,634)*y(k,233)
         mat(k,80) = .0166005_r8*rxt(k,639)*y(k,153) + .0113005_r8*rxt(k,638)*y(k,233)
         mat(k,88) = .130_r8*rxt(k,643)*y(k,153) + .1278005_r8*rxt(k,642)*y(k,233)
         mat(k,2006) = .0128005_r8*rxt(k,646)*y(k,202)
         mat(k,94) = .238_r8*rxt(k,649)*y(k,153) + .0738005_r8*rxt(k,648)*y(k,233)
         mat(k,100) = .1185005_r8*rxt(k,653)*y(k,153) + .1598005_r8*rxt(k,652) &
                      *y(k,233)
         mat(k,57) = -(rxt(k,646)*y(k,248))
         mat(k,2007) = -rxt(k,646)*y(k,202)
         mat(k,227) = .100_r8*rxt(k,522)*y(k,248)
         mat(k,245) = .230_r8*rxt(k,524)*y(k,248)
         mat(k,2031) = .100_r8*rxt(k,522)*y(k,210) + .230_r8*rxt(k,524)*y(k,212)
         mat(k,733) = -(rxt(k,546)*y(k,248))
         mat(k,2101) = -rxt(k,546)*y(k,204)
         mat(k,1808) = rxt(k,544)*y(k,252)
         mat(k,1224) = rxt(k,544)*y(k,233)
         mat(k,708) = -(rxt(k,547)*y(k,248))
         mat(k,2098) = -rxt(k,547)*y(k,205)
         mat(k,2614) = .200_r8*rxt(k,540)*y(k,246) + .200_r8*rxt(k,550)*y(k,253)
         mat(k,1616) = .500_r8*rxt(k,538)*y(k,246)
         mat(k,1167) = .200_r8*rxt(k,540)*y(k,153) + .500_r8*rxt(k,538)*y(k,228)
         mat(k,1244) = .200_r8*rxt(k,550)*y(k,153)
         mat(k,557) = -(rxt(k,551)*y(k,248))
         mat(k,2079) = -rxt(k,551)*y(k,206)
         mat(k,1799) = rxt(k,549)*y(k,253)
         mat(k,1243) = rxt(k,549)*y(k,233)
         mat(k,1091) = -(rxt(k,552)*y(k,155) + rxt(k,553)*y(k,248))
         mat(k,2351) = -rxt(k,552)*y(k,207)
         mat(k,2131) = -rxt(k,553)*y(k,207)
         mat(k,1072) = .330_r8*rxt(k,533)*y(k,163)
         mat(k,979) = .330_r8*rxt(k,536)*y(k,163)
         mat(k,2633) = .800_r8*rxt(k,540)*y(k,246) + .800_r8*rxt(k,550)*y(k,253)
         mat(k,2351) = mat(k,2351) + rxt(k,541)*y(k,246)
         mat(k,2537) = .330_r8*rxt(k,533)*y(k,6) + .330_r8*rxt(k,536)*y(k,139)
         mat(k,709) = rxt(k,547)*y(k,248)
         mat(k,1625) = .500_r8*rxt(k,538)*y(k,246) + rxt(k,548)*y(k,253)
         mat(k,1169) = .800_r8*rxt(k,540)*y(k,153) + rxt(k,541)*y(k,155) &
                      + .500_r8*rxt(k,538)*y(k,228)
         mat(k,2131) = mat(k,2131) + rxt(k,547)*y(k,205)
         mat(k,1247) = .800_r8*rxt(k,550)*y(k,153) + rxt(k,548)*y(k,228)
         mat(k,1184) = -(rxt(k,554)*y(k,248))
         mat(k,2138) = -rxt(k,554)*y(k,208)
         mat(k,1075) = .300_r8*rxt(k,533)*y(k,163)
         mat(k,981) = .300_r8*rxt(k,536)*y(k,163)
         mat(k,2639) = .900_r8*rxt(k,545)*y(k,252)
         mat(k,2542) = .300_r8*rxt(k,533)*y(k,6) + .300_r8*rxt(k,536)*y(k,139)
         mat(k,1631) = rxt(k,543)*y(k,252)
         mat(k,1229) = .900_r8*rxt(k,545)*y(k,153) + rxt(k,543)*y(k,228)
         mat(k,746) = -(rxt(k,521)*y(k,248))
         mat(k,2102) = -rxt(k,521)*y(k,209)
         mat(k,1809) = rxt(k,519)*y(k,254)
         mat(k,813) = rxt(k,519)*y(k,233)
         mat(k,225) = -(rxt(k,522)*y(k,248))
         mat(k,2029) = -rxt(k,522)*y(k,210)
         mat(k,241) = -(rxt(k,488)*y(k,248))
         mat(k,2032) = -rxt(k,488)*y(k,211)
         mat(k,1777) = rxt(k,485)*y(k,256)
         mat(k,1293) = rxt(k,485)*y(k,233)
         mat(k,246) = -(rxt(k,524)*y(k,248))
         mat(k,2033) = -rxt(k,524)*y(k,212)
         mat(k,784) = -(rxt(k,527)*y(k,248))
         mat(k,2106) = -rxt(k,527)*y(k,213)
         mat(k,1812) = rxt(k,525)*y(k,257)
         mat(k,837) = rxt(k,525)*y(k,233)
         mat(k,254) = -(rxt(k,530)*y(k,248))
         mat(k,2034) = -rxt(k,530)*y(k,214)
         mat(k,247) = .150_r8*rxt(k,524)*y(k,248)
         mat(k,2034) = mat(k,2034) + .150_r8*rxt(k,524)*y(k,212)
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
         mat(k,504) = -(rxt(k,531)*y(k,248))
         mat(k,2072) = -rxt(k,531)*y(k,215)
         mat(k,1793) = rxt(k,528)*y(k,259)
         mat(k,581) = rxt(k,528)*y(k,233)
         mat(k,614) = -(rxt(k,489)*y(k,233) + rxt(k,490)*y(k,153) + rxt(k,518) &
                      *y(k,154))
         mat(k,1802) = -rxt(k,489)*y(k,218)
         mat(k,2609) = -rxt(k,490)*y(k,218)
         mat(k,2469) = -rxt(k,518)*y(k,218)
         mat(k,285) = rxt(k,495)*y(k,248)
         mat(k,2086) = rxt(k,495)*y(k,24)
         mat(k,998) = -(rxt(k,450)*y(k,233) + (rxt(k,451) + rxt(k,452)) * y(k,153))
         mat(k,1825) = -rxt(k,450)*y(k,219)
         mat(k,2629) = -(rxt(k,451) + rxt(k,452)) * y(k,219)
         mat(k,726) = rxt(k,453)*y(k,248)
         mat(k,282) = rxt(k,454)*y(k,248)
         mat(k,2123) = rxt(k,453)*y(k,2) + rxt(k,454)*y(k,15)
         mat(k,63) = -(rxt(k,626)*y(k,233) + rxt(k,627)*y(k,153))
         mat(k,1769) = -rxt(k,626)*y(k,220)
         mat(k,2585) = -rxt(k,627)*y(k,220)
         mat(k,1063) = rxt(k,629)*y(k,248)
         mat(k,2008) = rxt(k,629)*y(k,6)
         mat(k,566) = -(rxt(k,492)*y(k,233) + rxt(k,493)*y(k,153))
         mat(k,1800) = -rxt(k,492)*y(k,221)
         mat(k,2604) = -rxt(k,493)*y(k,221)
         mat(k,195) = .350_r8*rxt(k,491)*y(k,248)
         mat(k,488) = rxt(k,494)*y(k,248)
         mat(k,2080) = .350_r8*rxt(k,491)*y(k,7) + rxt(k,494)*y(k,8)
         mat(k,69) = -(rxt(k,631)*y(k,233) + rxt(k,632)*y(k,153))
         mat(k,1770) = -rxt(k,631)*y(k,222)
         mat(k,2586) = -rxt(k,632)*y(k,222)
         mat(k,191) = rxt(k,630)*y(k,248)
         mat(k,2009) = rxt(k,630)*y(k,7)
         mat(k,512) = -(rxt(k,496)*y(k,233) + rxt(k,498)*y(k,153))
         mat(k,1794) = -rxt(k,496)*y(k,223)
         mat(k,2599) = -rxt(k,498)*y(k,223)
         mat(k,381) = rxt(k,497)*y(k,248)
         mat(k,228) = .070_r8*rxt(k,522)*y(k,248)
         mat(k,248) = .060_r8*rxt(k,524)*y(k,248)
         mat(k,2073) = rxt(k,497)*y(k,25) + .070_r8*rxt(k,522)*y(k,210) &
                      + .060_r8*rxt(k,524)*y(k,212)
         mat(k,889) = -(4._r8*rxt(k,373)*y(k,224) + rxt(k,374)*y(k,228) + rxt(k,375) &
                      *y(k,233) + rxt(k,376)*y(k,153))
         mat(k,1621) = -rxt(k,374)*y(k,224)
         mat(k,1822) = -rxt(k,375)*y(k,224)
         mat(k,2626) = -rxt(k,376)*y(k,224)
         mat(k,386) = .500_r8*rxt(k,378)*y(k,248)
         mat(k,338) = rxt(k,379)*y(k,70) + rxt(k,380)*y(k,248)
         mat(k,1896) = rxt(k,379)*y(k,32)
         mat(k,2117) = .500_r8*rxt(k,378)*y(k,31) + rxt(k,380)*y(k,32)
         mat(k,873) = -(rxt(k,402)*y(k,228) + rxt(k,403)*y(k,233) + rxt(k,404) &
                      *y(k,153))
         mat(k,1619) = -rxt(k,402)*y(k,225)
         mat(k,1820) = -rxt(k,403)*y(k,225)
         mat(k,2624) = -rxt(k,404)*y(k,225)
         mat(k,481) = rxt(k,405)*y(k,248)
         mat(k,133) = rxt(k,406)*y(k,248)
         mat(k,2115) = rxt(k,405)*y(k,34) + rxt(k,406)*y(k,35)
         mat(k,716) = -(rxt(k,499)*y(k,233) + rxt(k,500)*y(k,153))
         mat(k,1806) = -rxt(k,499)*y(k,226)
         mat(k,2615) = -rxt(k,500)*y(k,226)
         mat(k,306) = rxt(k,501)*y(k,248)
         mat(k,2615) = mat(k,2615) + rxt(k,490)*y(k,218)
         mat(k,2525) = rxt(k,516)*y(k,171)
         mat(k,544) = rxt(k,516)*y(k,163)
         mat(k,615) = rxt(k,490)*y(k,153) + .400_r8*rxt(k,489)*y(k,233)
         mat(k,1806) = mat(k,1806) + .400_r8*rxt(k,489)*y(k,218)
         mat(k,2099) = rxt(k,501)*y(k,36)
         mat(k,1500) = -(4._r8*rxt(k,384)*y(k,227) + rxt(k,385)*y(k,228) + rxt(k,386) &
                      *y(k,233) + rxt(k,387)*y(k,153) + rxt(k,398)*y(k,154) + rxt(k,425) &
                      *y(k,240) + rxt(k,458)*y(k,235) + rxt(k,463)*y(k,236) + rxt(k,472) &
                      *y(k,237) + rxt(k,483)*y(k,256))
         mat(k,1646) = -rxt(k,385)*y(k,227)
         mat(k,1850) = -rxt(k,386)*y(k,227)
         mat(k,2655) = -rxt(k,387)*y(k,227)
         mat(k,2487) = -rxt(k,398)*y(k,227)
         mat(k,1427) = -rxt(k,425)*y(k,227)
         mat(k,1372) = -rxt(k,458)*y(k,227)
         mat(k,1405) = -rxt(k,463)*y(k,227)
         mat(k,1324) = -rxt(k,472)*y(k,227)
         mat(k,1302) = -rxt(k,483)*y(k,227)
         mat(k,1080) = .060_r8*rxt(k,533)*y(k,163)
         mat(k,1151) = rxt(k,381)*y(k,155) + rxt(k,382)*y(k,248)
         mat(k,1349) = rxt(k,407)*y(k,155) + rxt(k,408)*y(k,248)
         mat(k,694) = .500_r8*rxt(k,389)*y(k,248)
         mat(k,927) = .080_r8*rxt(k,478)*y(k,163)
         mat(k,1340) = .100_r8*rxt(k,431)*y(k,163)
         mat(k,986) = .060_r8*rxt(k,536)*y(k,163)
         mat(k,1448) = .280_r8*rxt(k,445)*y(k,163)
         mat(k,2655) = mat(k,2655) + .530_r8*rxt(k,429)*y(k,240) + rxt(k,438)*y(k,242) &
                      + rxt(k,441)*y(k,244) + rxt(k,416)*y(k,251)
         mat(k,2375) = rxt(k,381)*y(k,54) + rxt(k,407)*y(k,58) + .530_r8*rxt(k,428) &
                      *y(k,240) + rxt(k,439)*y(k,242)
         mat(k,2556) = .060_r8*rxt(k,533)*y(k,6) + .080_r8*rxt(k,478)*y(k,127) &
                      + .100_r8*rxt(k,431)*y(k,134) + .060_r8*rxt(k,536)*y(k,139) &
                      + .280_r8*rxt(k,445)*y(k,140)
         mat(k,1187) = .650_r8*rxt(k,554)*y(k,248)
         mat(k,1500) = mat(k,1500) + .530_r8*rxt(k,425)*y(k,240)
         mat(k,1646) = mat(k,1646) + .260_r8*rxt(k,426)*y(k,240) + rxt(k,435)*y(k,242) &
                      + .300_r8*rxt(k,414)*y(k,251)
         mat(k,1850) = mat(k,1850) + .450_r8*rxt(k,436)*y(k,242) + .200_r8*rxt(k,440) &
                      *y(k,244) + .150_r8*rxt(k,415)*y(k,251)
         mat(k,1427) = mat(k,1427) + .530_r8*rxt(k,429)*y(k,153) + .530_r8*rxt(k,428) &
                      *y(k,155) + .530_r8*rxt(k,425)*y(k,227) + .260_r8*rxt(k,426) &
                      *y(k,228)
         mat(k,1469) = rxt(k,438)*y(k,153) + rxt(k,439)*y(k,155) + rxt(k,435)*y(k,228) &
                      + .450_r8*rxt(k,436)*y(k,233) + 4.000_r8*rxt(k,437)*y(k,242)
         mat(k,770) = rxt(k,441)*y(k,153) + .200_r8*rxt(k,440)*y(k,233)
         mat(k,2155) = rxt(k,382)*y(k,54) + rxt(k,408)*y(k,58) + .500_r8*rxt(k,389) &
                      *y(k,60) + .650_r8*rxt(k,554)*y(k,208)
         mat(k,1270) = rxt(k,416)*y(k,153) + .300_r8*rxt(k,414)*y(k,228) &
                      + .150_r8*rxt(k,415)*y(k,233)
         mat(k,1648) = -(rxt(k,213)*y(k,74) + (rxt(k,332) + rxt(k,333)) * y(k,68) &
                      + (4._r8*rxt(k,352) + 4._r8*rxt(k,353)) * y(k,228) + rxt(k,354) &
                      *y(k,233) + rxt(k,355)*y(k,153) + rxt(k,374)*y(k,224) + rxt(k,385) &
                      *y(k,227) + rxt(k,402)*y(k,225) + rxt(k,414)*y(k,251) + rxt(k,426) &
                      *y(k,240) + rxt(k,435)*y(k,242) + rxt(k,459)*y(k,235) + rxt(k,464) &
                      *y(k,236) + rxt(k,473)*y(k,237) + rxt(k,484)*y(k,256) + rxt(k,538) &
                      *y(k,246) + rxt(k,543)*y(k,252) + rxt(k,548)*y(k,253))
         mat(k,2283) = -rxt(k,213)*y(k,228)
         mat(k,1102) = -(rxt(k,332) + rxt(k,333)) * y(k,228)
         mat(k,1856) = -rxt(k,354)*y(k,228)
         mat(k,2659) = -rxt(k,355)*y(k,228)
         mat(k,891) = -rxt(k,374)*y(k,228)
         mat(k,1502) = -rxt(k,385)*y(k,228)
         mat(k,876) = -rxt(k,402)*y(k,228)
         mat(k,1271) = -rxt(k,414)*y(k,228)
         mat(k,1428) = -rxt(k,426)*y(k,228)
         mat(k,1470) = -rxt(k,435)*y(k,228)
         mat(k,1373) = -rxt(k,459)*y(k,228)
         mat(k,1406) = -rxt(k,464)*y(k,228)
         mat(k,1325) = -rxt(k,473)*y(k,228)
         mat(k,1303) = -rxt(k,484)*y(k,228)
         mat(k,1174) = -rxt(k,538)*y(k,228)
         mat(k,1234) = -rxt(k,543)*y(k,228)
         mat(k,1255) = -rxt(k,548)*y(k,228)
         mat(k,1123) = .280_r8*rxt(k,401)*y(k,163)
         mat(k,777) = rxt(k,388)*y(k,248)
         mat(k,475) = .700_r8*rxt(k,357)*y(k,248)
         mat(k,1577) = rxt(k,205)*y(k,70) + rxt(k,306)*y(k,89) + rxt(k,364)*y(k,247) &
                      + rxt(k,358)*y(k,248)
         mat(k,1911) = rxt(k,205)*y(k,64)
         mat(k,950) = rxt(k,306)*y(k,64)
         mat(k,928) = .050_r8*rxt(k,478)*y(k,163)
         mat(k,2659) = mat(k,2659) + rxt(k,387)*y(k,227) + .830_r8*rxt(k,504)*y(k,229) &
                      + .170_r8*rxt(k,510)*y(k,243)
         mat(k,2559) = .280_r8*rxt(k,401)*y(k,33) + .050_r8*rxt(k,478)*y(k,127)
         mat(k,1502) = mat(k,1502) + rxt(k,387)*y(k,153) + 4.000_r8*rxt(k,384) &
                      *y(k,227) + .900_r8*rxt(k,385)*y(k,228) + .450_r8*rxt(k,386) &
                      *y(k,233) + rxt(k,458)*y(k,235) + rxt(k,463)*y(k,236) &
                      + rxt(k,472)*y(k,237) + rxt(k,425)*y(k,240) + rxt(k,434) &
                      *y(k,242) + rxt(k,483)*y(k,256)
         mat(k,1648) = mat(k,1648) + .900_r8*rxt(k,385)*y(k,227)
         mat(k,853) = .830_r8*rxt(k,504)*y(k,153) + .330_r8*rxt(k,503)*y(k,233)
         mat(k,1856) = mat(k,1856) + .450_r8*rxt(k,386)*y(k,227) + .330_r8*rxt(k,503) &
                      *y(k,229) + .070_r8*rxt(k,509)*y(k,243)
         mat(k,1373) = mat(k,1373) + rxt(k,458)*y(k,227)
         mat(k,1406) = mat(k,1406) + rxt(k,463)*y(k,227)
         mat(k,1325) = mat(k,1325) + rxt(k,472)*y(k,227)
         mat(k,1428) = mat(k,1428) + rxt(k,425)*y(k,227)
         mat(k,1470) = mat(k,1470) + rxt(k,434)*y(k,227)
         mat(k,960) = .170_r8*rxt(k,510)*y(k,153) + .070_r8*rxt(k,509)*y(k,233)
         mat(k,1980) = rxt(k,364)*y(k,64)
         mat(k,2162) = rxt(k,388)*y(k,59) + .700_r8*rxt(k,357)*y(k,63) + rxt(k,358) &
                      *y(k,64)
         mat(k,1303) = mat(k,1303) + rxt(k,483)*y(k,227)
         mat(k,850) = -(rxt(k,503)*y(k,233) + rxt(k,504)*y(k,153) + rxt(k,505) &
                      *y(k,154))
         mat(k,1818) = -rxt(k,503)*y(k,229)
         mat(k,2622) = -rxt(k,504)*y(k,229)
         mat(k,2475) = -rxt(k,505)*y(k,229)
         mat(k,647) = -((rxt(k,422) + rxt(k,423)) * y(k,153))
         mat(k,2610) = -(rxt(k,422) + rxt(k,423)) * y(k,230)
         mat(k,410) = rxt(k,421)*y(k,248)
         mat(k,2090) = rxt(k,421)*y(k,16)
         mat(k,2595) = .750_r8*rxt(k,391)*y(k,232)
         mat(k,796) = .750_r8*rxt(k,391)*y(k,153)
         mat(k,797) = -(rxt(k,390)*y(k,233) + rxt(k,391)*y(k,153))
         mat(k,1813) = -rxt(k,390)*y(k,232)
         mat(k,2618) = -rxt(k,391)*y(k,232)
         mat(k,640) = rxt(k,397)*y(k,248)
         mat(k,2107) = rxt(k,397)*y(k,28)
         mat(k,1861) = -((rxt(k,164) + rxt(k,165) + rxt(k,166)) * y(k,92) + rxt(k,168) &
                      *y(k,162) + rxt(k,169)*y(k,163) + rxt(k,173)*y(k,248) &
                      + 4._r8*rxt(k,178)*y(k,233) + rxt(k,188)*y(k,155) + rxt(k,193) &
                      *y(k,153) + rxt(k,198)*y(k,154) + (rxt(k,208) + rxt(k,209) &
                      ) * y(k,70) + rxt(k,217)*y(k,74) + rxt(k,244)*y(k,17) + rxt(k,251) &
                      *y(k,21) + rxt(k,273)*y(k,115) + rxt(k,288)*y(k,125) + rxt(k,334) &
                      *y(k,68) + rxt(k,348)*y(k,51) + rxt(k,354)*y(k,228) + rxt(k,361) &
                      *y(k,234) + rxt(k,375)*y(k,224) + rxt(k,386)*y(k,227) + rxt(k,390) &
                      *y(k,232) + rxt(k,403)*y(k,225) + rxt(k,411)*y(k,250) + rxt(k,415) &
                      *y(k,251) + rxt(k,427)*y(k,240) + rxt(k,436)*y(k,242) + rxt(k,440) &
                      *y(k,244) + rxt(k,450)*y(k,219) + rxt(k,460)*y(k,235) + rxt(k,465) &
                      *y(k,236) + rxt(k,474)*y(k,237) + rxt(k,485)*y(k,256) + rxt(k,489) &
                      *y(k,218) + rxt(k,492)*y(k,221) + rxt(k,496)*y(k,223) + rxt(k,499) &
                      *y(k,226) + rxt(k,503)*y(k,229) + rxt(k,506)*y(k,241) + rxt(k,509) &
                      *y(k,243) + rxt(k,512)*y(k,249) + rxt(k,519)*y(k,254) + rxt(k,525) &
                      *y(k,257) + rxt(k,528)*y(k,259) + rxt(k,539)*y(k,246) + rxt(k,544) &
                      *y(k,252) + rxt(k,549)*y(k,253))
         mat(k,1939) = -(rxt(k,164) + rxt(k,165) + rxt(k,166)) * y(k,233)
         mat(k,2202) = -rxt(k,168)*y(k,233)
         mat(k,2563) = -rxt(k,169)*y(k,233)
         mat(k,2167) = -rxt(k,173)*y(k,233)
         mat(k,2385) = -rxt(k,188)*y(k,233)
         mat(k,2664) = -rxt(k,193)*y(k,233)
         mat(k,2497) = -rxt(k,198)*y(k,233)
         mat(k,1916) = -(rxt(k,208) + rxt(k,209)) * y(k,233)
         mat(k,2287) = -rxt(k,217)*y(k,233)
         mat(k,2315) = -rxt(k,244)*y(k,233)
         mat(k,2440) = -rxt(k,251)*y(k,233)
         mat(k,2413) = -rxt(k,273)*y(k,233)
         mat(k,2233) = -rxt(k,288)*y(k,233)
         mat(k,1105) = -rxt(k,334)*y(k,233)
         mat(k,1700) = -rxt(k,348)*y(k,233)
         mat(k,1652) = -rxt(k,354)*y(k,233)
         mat(k,521) = -rxt(k,361)*y(k,233)
         mat(k,893) = -rxt(k,375)*y(k,233)
         mat(k,1504) = -rxt(k,386)*y(k,233)
         mat(k,800) = -rxt(k,390)*y(k,233)
         mat(k,878) = -rxt(k,403)*y(k,233)
         mat(k,863) = -rxt(k,411)*y(k,233)
         mat(k,1273) = -rxt(k,415)*y(k,233)
         mat(k,1430) = -rxt(k,427)*y(k,233)
         mat(k,1472) = -rxt(k,436)*y(k,233)
         mat(k,771) = -rxt(k,440)*y(k,233)
         mat(k,1004) = -rxt(k,450)*y(k,233)
         mat(k,1375) = -rxt(k,460)*y(k,233)
         mat(k,1408) = -rxt(k,465)*y(k,233)
         mat(k,1327) = -rxt(k,474)*y(k,233)
         mat(k,1305) = -rxt(k,485)*y(k,233)
         mat(k,616) = -rxt(k,489)*y(k,233)
         mat(k,569) = -rxt(k,492)*y(k,233)
         mat(k,514) = -rxt(k,496)*y(k,233)
         mat(k,717) = -rxt(k,499)*y(k,233)
         mat(k,854) = -rxt(k,503)*y(k,233)
         mat(k,807) = -rxt(k,506)*y(k,233)
         mat(k,961) = -rxt(k,509)*y(k,233)
         mat(k,527) = -rxt(k,512)*y(k,233)
         mat(k,821) = -rxt(k,519)*y(k,233)
         mat(k,846) = -rxt(k,525)*y(k,233)
         mat(k,585) = -rxt(k,528)*y(k,233)
         mat(k,1176) = -rxt(k,539)*y(k,233)
         mat(k,1236) = -rxt(k,544)*y(k,233)
         mat(k,1257) = -rxt(k,549)*y(k,233)
         mat(k,1083) = .570_r8*rxt(k,533)*y(k,163)
         mat(k,196) = .650_r8*rxt(k,491)*y(k,248)
         mat(k,2315) = mat(k,2315) + rxt(k,243)*y(k,51)
         mat(k,2440) = mat(k,2440) + rxt(k,258)*y(k,248)
         mat(k,332) = .350_r8*rxt(k,370)*y(k,248)
         mat(k,644) = .130_r8*rxt(k,372)*y(k,163)
         mat(k,302) = rxt(k,377)*y(k,248)
         mat(k,1125) = .280_r8*rxt(k,401)*y(k,163)
         mat(k,1700) = mat(k,1700) + rxt(k,243)*y(k,17) + rxt(k,204)*y(k,70) &
                      + rxt(k,349)*y(k,155) + rxt(k,350)*y(k,162)
         mat(k,666) = rxt(k,321)*y(k,70) + rxt(k,322)*y(k,248)
         mat(k,440) = rxt(k,324)*y(k,70) + rxt(k,325)*y(k,248)
         mat(k,108) = rxt(k,383)*y(k,248)
         mat(k,431) = rxt(k,326)*y(k,70) + rxt(k,327)*y(k,248)
         mat(k,870) = rxt(k,356)*y(k,248)
         mat(k,1580) = rxt(k,365)*y(k,247)
         mat(k,1105) = mat(k,1105) + rxt(k,336)*y(k,153) + rxt(k,337)*y(k,155) + ( &
                      + 2.000_r8*rxt(k,332)+rxt(k,333))*y(k,228)
         mat(k,1916) = mat(k,1916) + rxt(k,204)*y(k,51) + rxt(k,321)*y(k,52) &
                      + rxt(k,324)*y(k,55) + rxt(k,326)*y(k,61) + rxt(k,207)*y(k,95)
         mat(k,2287) = mat(k,2287) + rxt(k,213)*y(k,228) + rxt(k,224)*y(k,248)
         mat(k,1196) = rxt(k,368)*y(k,248)
         mat(k,235) = .730_r8*rxt(k,502)*y(k,248)
         mat(k,1028) = .500_r8*rxt(k,574)*y(k,248)
         mat(k,1163) = rxt(k,394)*y(k,248)
         mat(k,1055) = rxt(k,395)*y(k,248)
         mat(k,674) = rxt(k,207)*y(k,70) + rxt(k,163)*y(k,162) + rxt(k,172)*y(k,248)
         mat(k,210) = rxt(k,359)*y(k,248)
         mat(k,1047) = rxt(k,360)*y(k,248)
         mat(k,1210) = rxt(k,424)*y(k,248)
         mat(k,1221) = rxt(k,409)*y(k,248)
         mat(k,2233) = mat(k,2233) + rxt(k,294)*y(k,248)
         mat(k,930) = .370_r8*rxt(k,478)*y(k,163)
         mat(k,688) = .300_r8*rxt(k,469)*y(k,248)
         mat(k,660) = rxt(k,470)*y(k,248)
         mat(k,447) = rxt(k,477)*y(k,248)
         mat(k,1342) = .140_r8*rxt(k,431)*y(k,163)
         mat(k,355) = .200_r8*rxt(k,433)*y(k,248)
         mat(k,704) = .500_r8*rxt(k,444)*y(k,248)
         mat(k,988) = .570_r8*rxt(k,536)*y(k,163)
         mat(k,1452) = .280_r8*rxt(k,445)*y(k,163)
         mat(k,459) = rxt(k,481)*y(k,248)
         mat(k,1143) = rxt(k,482)*y(k,248)
         mat(k,2664) = mat(k,2664) + rxt(k,336)*y(k,68) + rxt(k,451)*y(k,219) &
                      + rxt(k,493)*y(k,221) + rxt(k,498)*y(k,223) + rxt(k,376) &
                      *y(k,224) + rxt(k,404)*y(k,225) + rxt(k,355)*y(k,228) &
                      + .170_r8*rxt(k,504)*y(k,229) + rxt(k,422)*y(k,230) &
                      + .250_r8*rxt(k,391)*y(k,232) + rxt(k,363)*y(k,234) &
                      + .920_r8*rxt(k,461)*y(k,235) + .920_r8*rxt(k,467)*y(k,236) &
                      + rxt(k,475)*y(k,237) + .470_r8*rxt(k,429)*y(k,240) &
                      + .400_r8*rxt(k,507)*y(k,241) + .830_r8*rxt(k,510)*y(k,243) &
                      + rxt(k,513)*y(k,249) + rxt(k,412)*y(k,250) + .900_r8*rxt(k,545) &
                      *y(k,252) + .800_r8*rxt(k,550)*y(k,253) + rxt(k,520)*y(k,254) &
                      + rxt(k,486)*y(k,256) + rxt(k,526)*y(k,257) + rxt(k,529) &
                      *y(k,259)
         mat(k,2385) = mat(k,2385) + rxt(k,349)*y(k,51) + rxt(k,337)*y(k,68) &
                      + rxt(k,462)*y(k,235) + rxt(k,468)*y(k,236) + rxt(k,476) &
                      *y(k,237) + .470_r8*rxt(k,428)*y(k,240) + rxt(k,191)*y(k,248) &
                      + rxt(k,487)*y(k,256)
         mat(k,2202) = mat(k,2202) + rxt(k,350)*y(k,51) + rxt(k,163)*y(k,95)
         mat(k,2563) = mat(k,2563) + .570_r8*rxt(k,533)*y(k,6) + .130_r8*rxt(k,372) &
                      *y(k,28) + .280_r8*rxt(k,401)*y(k,33) + .370_r8*rxt(k,478) &
                      *y(k,127) + .140_r8*rxt(k,431)*y(k,134) + .570_r8*rxt(k,536) &
                      *y(k,139) + .280_r8*rxt(k,445)*y(k,140) + rxt(k,175)*y(k,248)
         mat(k,205) = .800_r8*rxt(k,514)*y(k,248)
         mat(k,1040) = rxt(k,564)*y(k,248)
         mat(k,1190) = .200_r8*rxt(k,554)*y(k,248)
         mat(k,230) = .280_r8*rxt(k,522)*y(k,248)
         mat(k,252) = .380_r8*rxt(k,524)*y(k,248)
         mat(k,257) = .630_r8*rxt(k,530)*y(k,248)
         mat(k,1004) = mat(k,1004) + rxt(k,451)*y(k,153)
         mat(k,569) = mat(k,569) + rxt(k,493)*y(k,153)
         mat(k,514) = mat(k,514) + rxt(k,498)*y(k,153)
         mat(k,893) = mat(k,893) + rxt(k,376)*y(k,153) + 2.400_r8*rxt(k,373)*y(k,224) &
                      + rxt(k,374)*y(k,228)
         mat(k,878) = mat(k,878) + rxt(k,404)*y(k,153) + rxt(k,402)*y(k,228)
         mat(k,1504) = mat(k,1504) + .900_r8*rxt(k,385)*y(k,228) + rxt(k,458)*y(k,235) &
                      + rxt(k,463)*y(k,236) + rxt(k,472)*y(k,237) + .470_r8*rxt(k,425) &
                      *y(k,240) + rxt(k,483)*y(k,256)
         mat(k,1652) = mat(k,1652) + (2.000_r8*rxt(k,332)+rxt(k,333))*y(k,68) &
                      + rxt(k,213)*y(k,74) + rxt(k,355)*y(k,153) + rxt(k,374)*y(k,224) &
                      + rxt(k,402)*y(k,225) + .900_r8*rxt(k,385)*y(k,227) &
                      + 4.000_r8*rxt(k,352)*y(k,228) + rxt(k,459)*y(k,235) &
                      + rxt(k,464)*y(k,236) + 1.200_r8*rxt(k,473)*y(k,237) &
                      + .730_r8*rxt(k,426)*y(k,240) + rxt(k,435)*y(k,242) &
                      + .500_r8*rxt(k,538)*y(k,246) + .300_r8*rxt(k,414)*y(k,251) &
                      + rxt(k,543)*y(k,252) + rxt(k,548)*y(k,253) + .800_r8*rxt(k,484) &
                      *y(k,256)
         mat(k,854) = mat(k,854) + .170_r8*rxt(k,504)*y(k,153) + .070_r8*rxt(k,503) &
                      *y(k,233)
         mat(k,652) = rxt(k,422)*y(k,153)
         mat(k,800) = mat(k,800) + .250_r8*rxt(k,391)*y(k,153)
         mat(k,1861) = mat(k,1861) + .070_r8*rxt(k,503)*y(k,229) + .160_r8*rxt(k,506) &
                      *y(k,241) + .330_r8*rxt(k,509)*y(k,243)
         mat(k,521) = mat(k,521) + rxt(k,363)*y(k,153)
         mat(k,1375) = mat(k,1375) + .920_r8*rxt(k,461)*y(k,153) + rxt(k,462)*y(k,155) &
                      + rxt(k,458)*y(k,227) + rxt(k,459)*y(k,228)
         mat(k,1408) = mat(k,1408) + .920_r8*rxt(k,467)*y(k,153) + rxt(k,468)*y(k,155) &
                      + rxt(k,463)*y(k,227) + rxt(k,464)*y(k,228)
         mat(k,1327) = mat(k,1327) + rxt(k,475)*y(k,153) + rxt(k,476)*y(k,155) &
                      + rxt(k,472)*y(k,227) + 1.200_r8*rxt(k,473)*y(k,228)
         mat(k,1430) = mat(k,1430) + .470_r8*rxt(k,429)*y(k,153) + .470_r8*rxt(k,428) &
                      *y(k,155) + .470_r8*rxt(k,425)*y(k,227) + .730_r8*rxt(k,426) &
                      *y(k,228)
         mat(k,807) = mat(k,807) + .400_r8*rxt(k,507)*y(k,153) + .160_r8*rxt(k,506) &
                      *y(k,233)
         mat(k,1472) = mat(k,1472) + rxt(k,435)*y(k,228)
         mat(k,961) = mat(k,961) + .830_r8*rxt(k,510)*y(k,153) + .330_r8*rxt(k,509) &
                      *y(k,233)
         mat(k,1176) = mat(k,1176) + .500_r8*rxt(k,538)*y(k,228)
         mat(k,1985) = rxt(k,365)*y(k,64)
         mat(k,2167) = mat(k,2167) + .650_r8*rxt(k,491)*y(k,7) + rxt(k,258)*y(k,21) &
                      + .350_r8*rxt(k,370)*y(k,27) + rxt(k,377)*y(k,30) + rxt(k,322) &
                      *y(k,52) + rxt(k,325)*y(k,55) + rxt(k,383)*y(k,56) + rxt(k,327) &
                      *y(k,61) + rxt(k,356)*y(k,62) + rxt(k,224)*y(k,74) + rxt(k,368) &
                      *y(k,77) + .730_r8*rxt(k,502)*y(k,82) + .500_r8*rxt(k,574) &
                      *y(k,83) + rxt(k,394)*y(k,90) + rxt(k,395)*y(k,91) + rxt(k,172) &
                      *y(k,95) + rxt(k,359)*y(k,102) + rxt(k,360)*y(k,103) &
                      + rxt(k,424)*y(k,111) + rxt(k,409)*y(k,113) + rxt(k,294) &
                      *y(k,125) + .300_r8*rxt(k,469)*y(k,128) + rxt(k,470)*y(k,129) &
                      + rxt(k,477)*y(k,130) + .200_r8*rxt(k,433)*y(k,135) &
                      + .500_r8*rxt(k,444)*y(k,138) + rxt(k,481)*y(k,144) + rxt(k,482) &
                      *y(k,145) + rxt(k,191)*y(k,155) + rxt(k,175)*y(k,163) &
                      + .800_r8*rxt(k,514)*y(k,172) + rxt(k,564)*y(k,181) &
                      + .200_r8*rxt(k,554)*y(k,208) + .280_r8*rxt(k,522)*y(k,210) &
                      + .380_r8*rxt(k,524)*y(k,212) + .630_r8*rxt(k,530)*y(k,214)
         mat(k,527) = mat(k,527) + rxt(k,513)*y(k,153)
         mat(k,863) = mat(k,863) + rxt(k,412)*y(k,153)
         mat(k,1273) = mat(k,1273) + .300_r8*rxt(k,414)*y(k,228)
         mat(k,1236) = mat(k,1236) + .900_r8*rxt(k,545)*y(k,153) + rxt(k,543)*y(k,228)
         mat(k,1257) = mat(k,1257) + .800_r8*rxt(k,550)*y(k,153) + rxt(k,548)*y(k,228)
         mat(k,821) = mat(k,821) + rxt(k,520)*y(k,153)
         mat(k,1305) = mat(k,1305) + rxt(k,486)*y(k,153) + rxt(k,487)*y(k,155) &
                      + rxt(k,483)*y(k,227) + .800_r8*rxt(k,484)*y(k,228)
         mat(k,846) = mat(k,846) + rxt(k,526)*y(k,153)
         mat(k,585) = mat(k,585) + rxt(k,529)*y(k,153)
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
         mat(k,518) = -(rxt(k,361)*y(k,233) + rxt(k,363)*y(k,153))
         mat(k,1795) = -rxt(k,361)*y(k,234)
         mat(k,2600) = -rxt(k,363)*y(k,234)
         mat(k,1688) = rxt(k,348)*y(k,233)
         mat(k,1795) = mat(k,1795) + rxt(k,348)*y(k,51)
         mat(k,1368) = -(rxt(k,458)*y(k,227) + rxt(k,459)*y(k,228) + rxt(k,460) &
                      *y(k,233) + rxt(k,461)*y(k,153) + rxt(k,462)*y(k,155))
         mat(k,1495) = -rxt(k,458)*y(k,235)
         mat(k,1641) = -rxt(k,459)*y(k,235)
         mat(k,1845) = -rxt(k,460)*y(k,235)
         mat(k,2650) = -rxt(k,461)*y(k,235)
         mat(k,2370) = -rxt(k,462)*y(k,235)
         mat(k,924) = .600_r8*rxt(k,479)*y(k,248)
         mat(k,2150) = .600_r8*rxt(k,479)*y(k,127)
         mat(k,1401) = -(rxt(k,463)*y(k,227) + rxt(k,464)*y(k,228) + rxt(k,465) &
                      *y(k,233) + rxt(k,467)*y(k,153) + rxt(k,468)*y(k,155))
         mat(k,1496) = -rxt(k,463)*y(k,236)
         mat(k,1642) = -rxt(k,464)*y(k,236)
         mat(k,1846) = -rxt(k,465)*y(k,236)
         mat(k,2651) = -rxt(k,467)*y(k,236)
         mat(k,2371) = -rxt(k,468)*y(k,236)
         mat(k,925) = .400_r8*rxt(k,479)*y(k,248)
         mat(k,2151) = .400_r8*rxt(k,479)*y(k,127)
         mat(k,1320) = -(rxt(k,472)*y(k,227) + rxt(k,473)*y(k,228) + rxt(k,474) &
                      *y(k,233) + rxt(k,475)*y(k,153) + rxt(k,476)*y(k,155))
         mat(k,1492) = -rxt(k,472)*y(k,237)
         mat(k,1638) = -rxt(k,473)*y(k,237)
         mat(k,1842) = -rxt(k,474)*y(k,237)
         mat(k,2647) = -rxt(k,475)*y(k,237)
         mat(k,2367) = -rxt(k,476)*y(k,237)
         mat(k,922) = rxt(k,471)*y(k,155)
         mat(k,2367) = mat(k,2367) + rxt(k,471)*y(k,127)
         mat(k,75) = -(rxt(k,634)*y(k,233) + rxt(k,635)*y(k,153))
         mat(k,1771) = -rxt(k,634)*y(k,238)
         mat(k,2587) = -rxt(k,635)*y(k,238)
         mat(k,917) = rxt(k,637)*y(k,248)
         mat(k,2010) = rxt(k,637)*y(k,127)
         mat(k,81) = -(rxt(k,638)*y(k,233) + rxt(k,639)*y(k,153))
         mat(k,1772) = -rxt(k,638)*y(k,239)
         mat(k,2588) = -rxt(k,639)*y(k,239)
         mat(k,82) = rxt(k,640)*y(k,248)
         mat(k,2011) = rxt(k,640)*y(k,132)
         mat(k,1425) = -(rxt(k,425)*y(k,227) + rxt(k,426)*y(k,228) + rxt(k,427) &
                      *y(k,233) + rxt(k,428)*y(k,155) + (rxt(k,429) + rxt(k,430) &
                      ) * y(k,153))
         mat(k,1497) = -rxt(k,425)*y(k,240)
         mat(k,1643) = -rxt(k,426)*y(k,240)
         mat(k,1847) = -rxt(k,427)*y(k,240)
         mat(k,2372) = -rxt(k,428)*y(k,240)
         mat(k,2652) = -(rxt(k,429) + rxt(k,430)) * y(k,240)
         mat(k,1338) = .500_r8*rxt(k,432)*y(k,248)
         mat(k,353) = .200_r8*rxt(k,433)*y(k,248)
         mat(k,1445) = rxt(k,446)*y(k,248)
         mat(k,2152) = .500_r8*rxt(k,432)*y(k,134) + .200_r8*rxt(k,433)*y(k,135) &
                      + rxt(k,446)*y(k,140)
         mat(k,804) = -(rxt(k,506)*y(k,233) + rxt(k,507)*y(k,153) + rxt(k,508) &
                      *y(k,154))
         mat(k,1814) = -rxt(k,506)*y(k,241)
         mat(k,2619) = -rxt(k,507)*y(k,241)
         mat(k,2474) = -rxt(k,508)*y(k,241)
         mat(k,1468) = -(rxt(k,434)*y(k,227) + rxt(k,435)*y(k,228) + rxt(k,436) &
                      *y(k,233) + 4._r8*rxt(k,437)*y(k,242) + rxt(k,438)*y(k,153) &
                      + rxt(k,439)*y(k,155) + rxt(k,447)*y(k,154))
         mat(k,1499) = -rxt(k,434)*y(k,242)
         mat(k,1645) = -rxt(k,435)*y(k,242)
         mat(k,1849) = -rxt(k,436)*y(k,242)
         mat(k,2654) = -rxt(k,438)*y(k,242)
         mat(k,2374) = -rxt(k,439)*y(k,242)
         mat(k,2486) = -rxt(k,447)*y(k,242)
         mat(k,1339) = .500_r8*rxt(k,432)*y(k,248)
         mat(k,354) = .500_r8*rxt(k,433)*y(k,248)
         mat(k,2154) = .500_r8*rxt(k,432)*y(k,134) + .500_r8*rxt(k,433)*y(k,135)
         mat(k,956) = -(rxt(k,509)*y(k,233) + rxt(k,510)*y(k,153) + rxt(k,511) &
                      *y(k,154))
         mat(k,1824) = -rxt(k,509)*y(k,243)
         mat(k,2628) = -rxt(k,510)*y(k,243)
         mat(k,2479) = -rxt(k,511)*y(k,243)
         mat(k,768) = -(rxt(k,440)*y(k,233) + rxt(k,441)*y(k,153))
         mat(k,1810) = -rxt(k,440)*y(k,244)
         mat(k,2617) = -rxt(k,441)*y(k,244)
         mat(k,609) = rxt(k,442)*y(k,248)
         mat(k,358) = rxt(k,443)*y(k,248)
         mat(k,2104) = rxt(k,442)*y(k,136) + rxt(k,443)*y(k,137)
         mat(k,89) = -(rxt(k,642)*y(k,233) + rxt(k,643)*y(k,153))
         mat(k,1773) = -rxt(k,642)*y(k,245)
         mat(k,2589) = -rxt(k,643)*y(k,245)
         mat(k,970) = rxt(k,645)*y(k,248)
         mat(k,2013) = rxt(k,645)*y(k,139)
         mat(k,1170) = -(rxt(k,538)*y(k,228) + rxt(k,539)*y(k,233) + rxt(k,540) &
                      *y(k,153) + rxt(k,541)*y(k,155))
         mat(k,1630) = -rxt(k,538)*y(k,246)
         mat(k,1833) = -rxt(k,539)*y(k,246)
         mat(k,2638) = -rxt(k,540)*y(k,246)
         mat(k,2357) = -rxt(k,541)*y(k,246)
         mat(k,1074) = rxt(k,532)*y(k,155)
         mat(k,980) = rxt(k,535)*y(k,155)
         mat(k,2357) = mat(k,2357) + rxt(k,532)*y(k,6) + rxt(k,535)*y(k,139) &
                      + .500_r8*rxt(k,552)*y(k,207)
         mat(k,464) = rxt(k,542)*y(k,248)
         mat(k,1092) = .500_r8*rxt(k,552)*y(k,155)
         mat(k,2137) = rxt(k,542)*y(k,157)
         mat(k,1988) = -(rxt(k,153)*y(k,93) + rxt(k,154)*y(k,260) + (rxt(k,157) &
                      + rxt(k,158)) * y(k,163) + (rxt(k,196) + rxt(k,197)) * y(k,142) &
                      + rxt(k,231)*y(k,37) + rxt(k,232)*y(k,38) + rxt(k,233)*y(k,40) &
                      + rxt(k,234)*y(k,41) + rxt(k,235)*y(k,42) + rxt(k,236)*y(k,43) &
                      + rxt(k,237)*y(k,44) + (rxt(k,238) + rxt(k,239)) * y(k,101) &
                      + rxt(k,262)*y(k,39) + rxt(k,263)*y(k,66) + rxt(k,264)*y(k,94) &
                      + (rxt(k,265) + rxt(k,266)) * y(k,97) + rxt(k,310)*y(k,80) &
                      + rxt(k,311)*y(k,81) + rxt(k,343)*y(k,45) + rxt(k,344)*y(k,52) &
                      + rxt(k,345)*y(k,98) + rxt(k,346)*y(k,99) + rxt(k,347)*y(k,100) &
                      + (rxt(k,364) + rxt(k,365) + rxt(k,366)) * y(k,64) + rxt(k,367) &
                      *y(k,102))
         mat(k,1549) = -rxt(k,153)*y(k,247)
         mat(k,2696) = -rxt(k,154)*y(k,247)
         mat(k,2566) = -(rxt(k,157) + rxt(k,158)) * y(k,247)
         mat(k,218) = -(rxt(k,196) + rxt(k,197)) * y(k,247)
         mat(k,115) = -rxt(k,231)*y(k,247)
         mat(k,174) = -rxt(k,232)*y(k,247)
         mat(k,139) = -rxt(k,233)*y(k,247)
         mat(k,185) = -rxt(k,234)*y(k,247)
         mat(k,143) = -rxt(k,235)*y(k,247)
         mat(k,190) = -rxt(k,236)*y(k,247)
         mat(k,147) = -rxt(k,237)*y(k,247)
         mat(k,1728) = -(rxt(k,238) + rxt(k,239)) * y(k,247)
         mat(k,179) = -rxt(k,262)*y(k,247)
         mat(k,495) = -rxt(k,263)*y(k,247)
         mat(k,126) = -rxt(k,264)*y(k,247)
         mat(k,1522) = -(rxt(k,265) + rxt(k,266)) * y(k,247)
         mat(k,275) = -rxt(k,310)*y(k,247)
         mat(k,267) = -rxt(k,311)*y(k,247)
         mat(k,553) = -rxt(k,343)*y(k,247)
         mat(k,668) = -rxt(k,344)*y(k,247)
         mat(k,262) = -rxt(k,345)*y(k,247)
         mat(k,271) = -rxt(k,346)*y(k,247)
         mat(k,322) = -rxt(k,347)*y(k,247)
         mat(k,1583) = -(rxt(k,364) + rxt(k,365) + rxt(k,366)) * y(k,247)
         mat(k,211) = -rxt(k,367)*y(k,247)
         mat(k,2171) = -(rxt(k,171)*y(k,93) + rxt(k,172)*y(k,95) + rxt(k,173)*y(k,233) &
                      + rxt(k,174)*y(k,162) + rxt(k,175)*y(k,163) + (4._r8*rxt(k,176) &
                      + 4._r8*rxt(k,177)) * y(k,248) + rxt(k,179)*y(k,107) + rxt(k,191) &
                      *y(k,155) + rxt(k,192)*y(k,141) + rxt(k,200)*y(k,154) + rxt(k,201) &
                      *y(k,106) + rxt(k,211)*y(k,73) + rxt(k,222)*y(k,75) + (rxt(k,224) &
                      + rxt(k,225)) * y(k,74) + rxt(k,227)*y(k,101) + rxt(k,230) &
                      *y(k,109) + rxt(k,242)*y(k,18) + rxt(k,258)*y(k,21) + rxt(k,260) &
                      *y(k,97) + rxt(k,268)*y(k,110) + rxt(k,271)*y(k,116) + rxt(k,294) &
                      *y(k,125) + rxt(k,295)*y(k,105) + rxt(k,313)*y(k,26) + rxt(k,315) &
                      *y(k,29) + rxt(k,317)*y(k,45) + rxt(k,318)*y(k,46) + rxt(k,320) &
                      *y(k,47) + rxt(k,322)*y(k,52) + rxt(k,323)*y(k,53) + rxt(k,325) &
                      *y(k,55) + rxt(k,327)*y(k,61) + rxt(k,328)*y(k,65) + rxt(k,330) &
                      *y(k,66) + rxt(k,331)*y(k,67) + rxt(k,339)*y(k,69) + rxt(k,340) &
                      *y(k,98) + rxt(k,341)*y(k,99) + rxt(k,342)*y(k,100) + rxt(k,351) &
                      *y(k,51) + rxt(k,356)*y(k,62) + rxt(k,357)*y(k,63) + rxt(k,358) &
                      *y(k,64) + rxt(k,359)*y(k,102) + rxt(k,360)*y(k,103) + rxt(k,368) &
                      *y(k,77) + rxt(k,370)*y(k,27) + rxt(k,377)*y(k,30) + rxt(k,378) &
                      *y(k,31) + rxt(k,380)*y(k,32) + rxt(k,382)*y(k,54) + rxt(k,383) &
                      *y(k,56) + rxt(k,388)*y(k,59) + rxt(k,389)*y(k,60) + rxt(k,394) &
                      *y(k,90) + rxt(k,395)*y(k,91) + rxt(k,396)*y(k,169) + rxt(k,397) &
                      *y(k,28) + rxt(k,405)*y(k,34) + rxt(k,406)*y(k,35) + rxt(k,408) &
                      *y(k,58) + rxt(k,409)*y(k,113) + rxt(k,410)*y(k,156) + rxt(k,413) &
                      *y(k,176) + rxt(k,417)*y(k,177) + rxt(k,418)*y(k,33) + rxt(k,419) &
                      *y(k,57) + rxt(k,421)*y(k,16) + rxt(k,424)*y(k,111) + rxt(k,432) &
                      *y(k,134) + rxt(k,433)*y(k,135) + rxt(k,442)*y(k,136) + rxt(k,443) &
                      *y(k,137) + rxt(k,444)*y(k,138) + rxt(k,446)*y(k,140) + rxt(k,449) &
                      *y(k,1) + rxt(k,453)*y(k,2) + rxt(k,454)*y(k,15) + rxt(k,455) &
                      *y(k,112) + rxt(k,456)*y(k,114) + rxt(k,457)*y(k,122) + rxt(k,469) &
                      *y(k,128) + rxt(k,470)*y(k,129) + rxt(k,477)*y(k,130) + rxt(k,479) &
                      *y(k,127) + rxt(k,480)*y(k,131) + rxt(k,481)*y(k,144) + rxt(k,482) &
                      *y(k,145) + rxt(k,488)*y(k,211) + rxt(k,491)*y(k,7) + rxt(k,494) &
                      *y(k,8) + rxt(k,495)*y(k,24) + rxt(k,497)*y(k,25) + rxt(k,501) &
                      *y(k,36) + rxt(k,502)*y(k,82) + rxt(k,514)*y(k,172) + rxt(k,517) &
                      *y(k,173) + rxt(k,521)*y(k,209) + rxt(k,522)*y(k,210) + rxt(k,524) &
                      *y(k,212) + rxt(k,527)*y(k,213) + rxt(k,530)*y(k,214) + rxt(k,531) &
                      *y(k,215) + rxt(k,534)*y(k,6) + rxt(k,537)*y(k,139) + rxt(k,542) &
                      *y(k,157) + rxt(k,546)*y(k,204) + rxt(k,547)*y(k,205) + rxt(k,551) &
                      *y(k,206) + rxt(k,553)*y(k,207) + rxt(k,554)*y(k,208) + (rxt(k,560) &
                      + rxt(k,574)) * y(k,83) + rxt(k,562)*y(k,166) + rxt(k,564) &
                      *y(k,181) + rxt(k,568)*y(k,178) + rxt(k,573)*y(k,180) + rxt(k,593) &
                      *y(k,149))
         mat(k,1550) = -rxt(k,171)*y(k,248)
         mat(k,676) = -rxt(k,172)*y(k,248)
         mat(k,1865) = -rxt(k,173)*y(k,248)
         mat(k,2206) = -rxt(k,174)*y(k,248)
         mat(k,2567) = -rxt(k,175)*y(k,248)
         mat(k,452) = -rxt(k,179)*y(k,248)
         mat(k,2389) = -rxt(k,191)*y(k,248)
         mat(k,577) = -rxt(k,192)*y(k,248)
         mat(k,2501) = -rxt(k,200)*y(k,248)
         mat(k,2262) = -rxt(k,201)*y(k,248)
         mat(k,593) = -rxt(k,211)*y(k,248)
         mat(k,1013) = -rxt(k,222)*y(k,248)
         mat(k,2291) = -(rxt(k,224) + rxt(k,225)) * y(k,248)
         mat(k,1729) = -rxt(k,227)*y(k,248)
         mat(k,1753) = -rxt(k,230)*y(k,248)
         mat(k,541) = -rxt(k,242)*y(k,248)
         mat(k,2444) = -rxt(k,258)*y(k,248)
         mat(k,1523) = -rxt(k,260)*y(k,248)
         mat(k,1601) = -rxt(k,268)*y(k,248)
         mat(k,1533) = -rxt(k,271)*y(k,248)
         mat(k,2237) = -rxt(k,294)*y(k,248)
         mat(k,1287) = -rxt(k,295)*y(k,248)
         mat(k,223) = -rxt(k,313)*y(k,248)
         mat(k,293) = -rxt(k,315)*y(k,248)
         mat(k,554) = -rxt(k,317)*y(k,248)
         mat(k,150) = -rxt(k,318)*y(k,248)
         mat(k,347) = -rxt(k,320)*y(k,248)
         mat(k,669) = -rxt(k,322)*y(k,248)
         mat(k,154) = -rxt(k,323)*y(k,248)
         mat(k,442) = -rxt(k,325)*y(k,248)
         mat(k,433) = -rxt(k,327)*y(k,248)
         mat(k,118) = -rxt(k,328)*y(k,248)
         mat(k,496) = -rxt(k,330)*y(k,248)
         mat(k,122) = -rxt(k,331)*y(k,248)
         mat(k,399) = -rxt(k,339)*y(k,248)
         mat(k,263) = -rxt(k,340)*y(k,248)
         mat(k,272) = -rxt(k,341)*y(k,248)
         mat(k,323) = -rxt(k,342)*y(k,248)
         mat(k,1704) = -rxt(k,351)*y(k,248)
         mat(k,871) = -rxt(k,356)*y(k,248)
         mat(k,478) = -rxt(k,357)*y(k,248)
         mat(k,1584) = -rxt(k,358)*y(k,248)
         mat(k,212) = -rxt(k,359)*y(k,248)
         mat(k,1048) = -rxt(k,360)*y(k,248)
         mat(k,1197) = -rxt(k,368)*y(k,248)
         mat(k,333) = -rxt(k,370)*y(k,248)
         mat(k,303) = -rxt(k,377)*y(k,248)
         mat(k,389) = -rxt(k,378)*y(k,248)
         mat(k,341) = -rxt(k,380)*y(k,248)
         mat(k,1154) = -rxt(k,382)*y(k,248)
         mat(k,109) = -rxt(k,383)*y(k,248)
         mat(k,778) = -rxt(k,388)*y(k,248)
         mat(k,697) = -rxt(k,389)*y(k,248)
         mat(k,1164) = -rxt(k,394)*y(k,248)
         mat(k,1056) = -rxt(k,395)*y(k,248)
         mat(k,628) = -rxt(k,396)*y(k,248)
         mat(k,645) = -rxt(k,397)*y(k,248)
         mat(k,484) = -rxt(k,405)*y(k,248)
         mat(k,134) = -rxt(k,406)*y(k,248)
         mat(k,1351) = -rxt(k,408)*y(k,248)
         mat(k,1222) = -rxt(k,409)*y(k,248)
         mat(k,913) = -rxt(k,410)*y(k,248)
         mat(k,637) = -rxt(k,413)*y(k,248)
         mat(k,472) = -rxt(k,417)*y(k,248)
         mat(k,1126) = -rxt(k,418)*y(k,248)
         mat(k,1023) = -rxt(k,419)*y(k,248)
         mat(k,414) = -rxt(k,421)*y(k,248)
         mat(k,1211) = -rxt(k,424)*y(k,248)
         mat(k,1343) = -rxt(k,432)*y(k,248)
         mat(k,356) = -rxt(k,433)*y(k,248)
         mat(k,612) = -rxt(k,442)*y(k,248)
         mat(k,361) = -rxt(k,443)*y(k,248)
         mat(k,705) = -rxt(k,444)*y(k,248)
         mat(k,1453) = -rxt(k,446)*y(k,248)
         mat(k,765) = -rxt(k,449)*y(k,248)
         mat(k,732) = -rxt(k,453)*y(k,248)
         mat(k,283) = -rxt(k,454)*y(k,248)
         mat(k,280) = -rxt(k,455)*y(k,248)
         mat(k,351) = -rxt(k,456)*y(k,248)
         mat(k,167) = -rxt(k,457)*y(k,248)
         mat(k,689) = -rxt(k,469)*y(k,248)
         mat(k,661) = -rxt(k,470)*y(k,248)
         mat(k,448) = -rxt(k,477)*y(k,248)
         mat(k,931) = -rxt(k,479)*y(k,248)
         mat(k,832) = -rxt(k,480)*y(k,248)
         mat(k,460) = -rxt(k,481)*y(k,248)
         mat(k,1144) = -rxt(k,482)*y(k,248)
         mat(k,243) = -rxt(k,488)*y(k,248)
         mat(k,197) = -rxt(k,491)*y(k,248)
         mat(k,491) = -rxt(k,494)*y(k,248)
         mat(k,286) = -rxt(k,495)*y(k,248)
         mat(k,384) = -rxt(k,497)*y(k,248)
         mat(k,307) = -rxt(k,501)*y(k,248)
         mat(k,236) = -rxt(k,502)*y(k,248)
         mat(k,206) = -rxt(k,514)*y(k,248)
         mat(k,378) = -rxt(k,517)*y(k,248)
         mat(k,755) = -rxt(k,521)*y(k,248)
         mat(k,231) = -rxt(k,522)*y(k,248)
         mat(k,253) = -rxt(k,524)*y(k,248)
         mat(k,794) = -rxt(k,527)*y(k,248)
         mat(k,258) = -rxt(k,530)*y(k,248)
         mat(k,509) = -rxt(k,531)*y(k,248)
         mat(k,1084) = -rxt(k,534)*y(k,248)
         mat(k,989) = -rxt(k,537)*y(k,248)
         mat(k,465) = -rxt(k,542)*y(k,248)
         mat(k,742) = -rxt(k,546)*y(k,248)
         mat(k,711) = -rxt(k,547)*y(k,248)
         mat(k,563) = -rxt(k,551)*y(k,248)
         mat(k,1097) = -rxt(k,553)*y(k,248)
         mat(k,1191) = -rxt(k,554)*y(k,248)
         mat(k,1030) = -(rxt(k,560) + rxt(k,574)) * y(k,248)
         mat(k,423) = -rxt(k,562)*y(k,248)
         mat(k,1041) = -rxt(k,564)*y(k,248)
         mat(k,605) = -rxt(k,568)*y(k,248)
         mat(k,1562) = -rxt(k,573)*y(k,248)
         mat(k,111) = -rxt(k,593)*y(k,248)
         mat(k,1084) = mat(k,1084) + .630_r8*rxt(k,533)*y(k,163)
         mat(k,333) = mat(k,333) + .650_r8*rxt(k,370)*y(k,248)
         mat(k,645) = mat(k,645) + .130_r8*rxt(k,372)*y(k,163)
         mat(k,389) = mat(k,389) + .500_r8*rxt(k,378)*y(k,248)
         mat(k,1126) = mat(k,1126) + .360_r8*rxt(k,401)*y(k,163)
         mat(k,1704) = mat(k,1704) + rxt(k,350)*y(k,162)
         mat(k,478) = mat(k,478) + .300_r8*rxt(k,357)*y(k,248)
         mat(k,1584) = mat(k,1584) + rxt(k,364)*y(k,247)
         mat(k,1920) = rxt(k,209)*y(k,233)
         mat(k,952) = rxt(k,308)*y(k,260)
         mat(k,1943) = rxt(k,170)*y(k,163) + 2.000_r8*rxt(k,165)*y(k,233)
         mat(k,1550) = mat(k,1550) + rxt(k,162)*y(k,162) + rxt(k,153)*y(k,247)
         mat(k,676) = mat(k,676) + rxt(k,163)*y(k,162)
         mat(k,1523) = mat(k,1523) + rxt(k,259)*y(k,162) + rxt(k,265)*y(k,247)
         mat(k,1729) = mat(k,1729) + rxt(k,226)*y(k,162) + rxt(k,238)*y(k,247)
         mat(k,212) = mat(k,212) + rxt(k,367)*y(k,247)
         mat(k,1678) = rxt(k,261)*y(k,162)
         mat(k,1753) = mat(k,1753) + rxt(k,229)*y(k,162)
         mat(k,931) = mat(k,931) + .320_r8*rxt(k,478)*y(k,163)
         mat(k,832) = mat(k,832) + .600_r8*rxt(k,480)*y(k,248)
         mat(k,1343) = mat(k,1343) + .240_r8*rxt(k,431)*y(k,163)
         mat(k,356) = mat(k,356) + .100_r8*rxt(k,433)*y(k,248)
         mat(k,989) = mat(k,989) + .630_r8*rxt(k,536)*y(k,163)
         mat(k,1453) = mat(k,1453) + .360_r8*rxt(k,445)*y(k,163)
         mat(k,2668) = rxt(k,193)*y(k,233)
         mat(k,2389) = mat(k,2389) + rxt(k,188)*y(k,233)
         mat(k,2206) = mat(k,2206) + rxt(k,350)*y(k,51) + rxt(k,162)*y(k,93) &
                      + rxt(k,163)*y(k,95) + rxt(k,259)*y(k,97) + rxt(k,226)*y(k,101) &
                      + rxt(k,261)*y(k,108) + rxt(k,229)*y(k,109) + rxt(k,168) &
                      *y(k,233)
         mat(k,2567) = mat(k,2567) + .630_r8*rxt(k,533)*y(k,6) + .130_r8*rxt(k,372) &
                      *y(k,28) + .360_r8*rxt(k,401)*y(k,33) + rxt(k,170)*y(k,92) &
                      + .320_r8*rxt(k,478)*y(k,127) + .240_r8*rxt(k,431)*y(k,134) &
                      + .630_r8*rxt(k,536)*y(k,139) + .360_r8*rxt(k,445)*y(k,140) &
                      + rxt(k,169)*y(k,233)
         mat(k,637) = mat(k,637) + .500_r8*rxt(k,413)*y(k,248)
         mat(k,243) = mat(k,243) + .500_r8*rxt(k,488)*y(k,248)
         mat(k,617) = .400_r8*rxt(k,489)*y(k,233)
         mat(k,1505) = .450_r8*rxt(k,386)*y(k,233)
         mat(k,855) = .400_r8*rxt(k,503)*y(k,233)
         mat(k,1865) = mat(k,1865) + rxt(k,209)*y(k,70) + 2.000_r8*rxt(k,165)*y(k,92) &
                      + rxt(k,193)*y(k,153) + rxt(k,188)*y(k,155) + rxt(k,168) &
                      *y(k,162) + rxt(k,169)*y(k,163) + .400_r8*rxt(k,489)*y(k,218) &
                      + .450_r8*rxt(k,386)*y(k,227) + .400_r8*rxt(k,503)*y(k,229) &
                      + .450_r8*rxt(k,436)*y(k,242) + .400_r8*rxt(k,509)*y(k,243) &
                      + .200_r8*rxt(k,440)*y(k,244) + .150_r8*rxt(k,415)*y(k,251)
         mat(k,1473) = .450_r8*rxt(k,436)*y(k,233)
         mat(k,962) = .400_r8*rxt(k,509)*y(k,233)
         mat(k,772) = .200_r8*rxt(k,440)*y(k,233)
         mat(k,1989) = rxt(k,364)*y(k,64) + rxt(k,153)*y(k,93) + rxt(k,265)*y(k,97) &
                      + rxt(k,238)*y(k,101) + rxt(k,367)*y(k,102) &
                      + 2.000_r8*rxt(k,154)*y(k,260)
         mat(k,2171) = mat(k,2171) + .650_r8*rxt(k,370)*y(k,27) + .500_r8*rxt(k,378) &
                      *y(k,31) + .300_r8*rxt(k,357)*y(k,63) + .600_r8*rxt(k,480) &
                      *y(k,131) + .100_r8*rxt(k,433)*y(k,135) + .500_r8*rxt(k,413) &
                      *y(k,176) + .500_r8*rxt(k,488)*y(k,211)
         mat(k,1274) = .150_r8*rxt(k,415)*y(k,233)
         mat(k,2697) = rxt(k,308)*y(k,89) + 2.000_r8*rxt(k,154)*y(k,247)
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
         mat(k,525) = -(rxt(k,512)*y(k,233) + rxt(k,513)*y(k,153))
         mat(k,1796) = -rxt(k,512)*y(k,249)
         mat(k,2601) = -rxt(k,513)*y(k,249)
         mat(k,233) = .200_r8*rxt(k,502)*y(k,248)
         mat(k,203) = .140_r8*rxt(k,514)*y(k,248)
         mat(k,375) = rxt(k,517)*y(k,248)
         mat(k,2074) = .200_r8*rxt(k,502)*y(k,82) + .140_r8*rxt(k,514)*y(k,172) &
                      + rxt(k,517)*y(k,173)
         mat(k,859) = -(rxt(k,411)*y(k,233) + rxt(k,412)*y(k,153))
         mat(k,1819) = -rxt(k,411)*y(k,250)
         mat(k,2623) = -rxt(k,412)*y(k,250)
         mat(k,1112) = rxt(k,418)*y(k,248)
         mat(k,632) = .500_r8*rxt(k,413)*y(k,248)
         mat(k,2113) = rxt(k,418)*y(k,33) + .500_r8*rxt(k,413)*y(k,176)
         mat(k,1268) = -(rxt(k,414)*y(k,228) + rxt(k,415)*y(k,233) + rxt(k,416) &
                      *y(k,153))
         mat(k,1636) = -rxt(k,414)*y(k,251)
         mat(k,1839) = -rxt(k,415)*y(k,251)
         mat(k,2645) = -rxt(k,416)*y(k,251)
         mat(k,1078) = .060_r8*rxt(k,533)*y(k,163)
         mat(k,1020) = rxt(k,419)*y(k,248)
         mat(k,984) = .060_r8*rxt(k,536)*y(k,163)
         mat(k,2547) = .060_r8*rxt(k,533)*y(k,6) + .060_r8*rxt(k,536)*y(k,139)
         mat(k,469) = rxt(k,417)*y(k,248)
         mat(k,1186) = .150_r8*rxt(k,554)*y(k,248)
         mat(k,2144) = rxt(k,419)*y(k,57) + rxt(k,417)*y(k,177) + .150_r8*rxt(k,554) &
                      *y(k,208)
         mat(k,1231) = -(rxt(k,543)*y(k,228) + rxt(k,544)*y(k,233) + rxt(k,545) &
                      *y(k,153))
         mat(k,1634) = -rxt(k,543)*y(k,252)
         mat(k,1837) = -rxt(k,544)*y(k,252)
         mat(k,2643) = -rxt(k,545)*y(k,252)
         mat(k,2362) = .500_r8*rxt(k,552)*y(k,207)
         mat(k,739) = rxt(k,546)*y(k,248)
         mat(k,1095) = .500_r8*rxt(k,552)*y(k,155) + rxt(k,553)*y(k,248)
         mat(k,2142) = rxt(k,546)*y(k,204) + rxt(k,553)*y(k,207)
         mat(k,1252) = -(rxt(k,548)*y(k,228) + rxt(k,549)*y(k,233) + rxt(k,550) &
                      *y(k,153))
         mat(k,1635) = -rxt(k,548)*y(k,253)
         mat(k,1838) = -rxt(k,549)*y(k,253)
         mat(k,2644) = -rxt(k,550)*y(k,253)
         mat(k,1077) = rxt(k,534)*y(k,248)
         mat(k,983) = rxt(k,537)*y(k,248)
         mat(k,560) = rxt(k,551)*y(k,248)
         mat(k,2143) = rxt(k,534)*y(k,6) + rxt(k,537)*y(k,139) + rxt(k,551)*y(k,206)
         mat(k,815) = -(rxt(k,519)*y(k,233) + rxt(k,520)*y(k,153))
         mat(k,1815) = -rxt(k,519)*y(k,254)
         mat(k,2620) = -rxt(k,520)*y(k,254)
         mat(k,748) = rxt(k,521)*y(k,248)
         mat(k,229) = .650_r8*rxt(k,522)*y(k,248)
         mat(k,2109) = rxt(k,521)*y(k,209) + .650_r8*rxt(k,522)*y(k,210)
         mat(k,95) = -(rxt(k,648)*y(k,233) + rxt(k,649)*y(k,153))
         mat(k,1774) = -rxt(k,648)*y(k,255)
         mat(k,2590) = -rxt(k,649)*y(k,255)
         mat(k,224) = rxt(k,647)*y(k,248)
         mat(k,2014) = rxt(k,647)*y(k,210)
         mat(k,1300) = -(rxt(k,483)*y(k,227) + rxt(k,484)*y(k,228) + rxt(k,485) &
                      *y(k,233) + rxt(k,486)*y(k,153) + rxt(k,487)*y(k,155))
         mat(k,1491) = -rxt(k,483)*y(k,256)
         mat(k,1637) = -rxt(k,484)*y(k,256)
         mat(k,1841) = -rxt(k,485)*y(k,256)
         mat(k,2646) = -rxt(k,486)*y(k,256)
         mat(k,2366) = -rxt(k,487)*y(k,256)
         mat(k,278) = rxt(k,455)*y(k,248)
         mat(k,350) = rxt(k,456)*y(k,248)
         mat(k,166) = rxt(k,457)*y(k,248)
         mat(k,827) = .400_r8*rxt(k,480)*y(k,248)
         mat(k,242) = .500_r8*rxt(k,488)*y(k,248)
         mat(k,2146) = rxt(k,455)*y(k,112) + rxt(k,456)*y(k,114) + rxt(k,457)*y(k,122) &
                      + .400_r8*rxt(k,480)*y(k,131) + .500_r8*rxt(k,488)*y(k,211)
         mat(k,839) = -(rxt(k,525)*y(k,233) + rxt(k,526)*y(k,153))
         mat(k,1817) = -rxt(k,525)*y(k,257)
         mat(k,2621) = -rxt(k,526)*y(k,257)
         mat(k,249) = .560_r8*rxt(k,524)*y(k,248)
         mat(k,786) = rxt(k,527)*y(k,248)
         mat(k,2111) = .560_r8*rxt(k,524)*y(k,212) + rxt(k,527)*y(k,213)
         mat(k,101) = -(rxt(k,652)*y(k,233) + rxt(k,653)*y(k,153))
         mat(k,1775) = -rxt(k,652)*y(k,258)
         mat(k,2591) = -rxt(k,653)*y(k,258)
         mat(k,244) = rxt(k,651)*y(k,248)
         mat(k,2015) = rxt(k,651)*y(k,212)
         mat(k,582) = -(rxt(k,528)*y(k,233) + rxt(k,529)*y(k,153))
         mat(k,1801) = -rxt(k,528)*y(k,259)
         mat(k,2606) = -rxt(k,529)*y(k,259)
         mat(k,256) = .300_r8*rxt(k,530)*y(k,248)
         mat(k,505) = rxt(k,531)*y(k,248)
         mat(k,2082) = .300_r8*rxt(k,530)*y(k,214) + rxt(k,531)*y(k,215)
         mat(k,2709) = -(rxt(k,154)*y(k,247) + rxt(k,308)*y(k,89) + rxt(k,575) &
                      *y(k,182))
         mat(k,2001) = -rxt(k,154)*y(k,260)
         mat(k,955) = -rxt(k,308)*y(k,260)
         mat(k,299) = -rxt(k,575)*y(k,260)
         mat(k,294) = rxt(k,315)*y(k,248)
         mat(k,342) = rxt(k,380)*y(k,248)
         mat(k,485) = rxt(k,405)*y(k,248)
         mat(k,135) = rxt(k,406)*y(k,248)
         mat(k,556) = rxt(k,317)*y(k,248)
         mat(k,348) = rxt(k,320)*y(k,248)
         mat(k,1716) = rxt(k,351)*y(k,248)
         mat(k,671) = rxt(k,322)*y(k,248)
         mat(k,155) = rxt(k,323)*y(k,248)
         mat(k,1157) = rxt(k,382)*y(k,248)
         mat(k,443) = rxt(k,325)*y(k,248)
         mat(k,1024) = rxt(k,419)*y(k,248)
         mat(k,1355) = rxt(k,408)*y(k,248)
         mat(k,779) = rxt(k,388)*y(k,248)
         mat(k,698) = rxt(k,389)*y(k,248)
         mat(k,435) = rxt(k,327)*y(k,248)
         mat(k,479) = rxt(k,357)*y(k,248)
         mat(k,1588) = rxt(k,358)*y(k,248)
         mat(k,1110) = rxt(k,334)*y(k,233)
         mat(k,400) = rxt(k,339)*y(k,248)
         mat(k,1955) = rxt(k,166)*y(k,233)
         mat(k,1554) = rxt(k,171)*y(k,248)
         mat(k,678) = rxt(k,172)*y(k,248)
         mat(k,1528) = (rxt(k,583)+rxt(k,657)+rxt(k,670)+rxt(k,679))*y(k,108) + ( &
                      + rxt(k,582)+rxt(k,659)+rxt(k,667)+rxt(k,676))*y(k,109) + ( &
                      + rxt(k,590)+rxt(k,686)+rxt(k,690)+rxt(k,694))*y(k,110) &
                      + rxt(k,260)*y(k,248)
         mat(k,324) = rxt(k,342)*y(k,248)
         mat(k,1739) = (rxt(k,585)+rxt(k,656)+rxt(k,669)+rxt(k,678))*y(k,108) + ( &
                      + rxt(k,584)+rxt(k,655)+rxt(k,666)+rxt(k,675))*y(k,109) + ( &
                      + rxt(k,589)+rxt(k,685)+rxt(k,689)+rxt(k,693))*y(k,110) &
                      + rxt(k,227)*y(k,248)
         mat(k,1050) = rxt(k,360)*y(k,248)
         mat(k,1292) = (rxt(k,587)+rxt(k,658)+rxt(k,671)+rxt(k,680))*y(k,108) + ( &
                      + rxt(k,586)+rxt(k,660)+rxt(k,668)+rxt(k,677))*y(k,109) + ( &
                      + rxt(k,591)+rxt(k,687)+rxt(k,691)+rxt(k,695))*y(k,110) &
                      + rxt(k,295)*y(k,248)
         mat(k,2274) = rxt(k,201)*y(k,248)
         mat(k,455) = rxt(k,179)*y(k,248)
         mat(k,1687) = (rxt(k,583)+rxt(k,657)+rxt(k,670)+rxt(k,679))*y(k,97) + ( &
                      + rxt(k,585)+rxt(k,656)+rxt(k,669)+rxt(k,678))*y(k,101) + ( &
                      + rxt(k,587)+rxt(k,658)+rxt(k,671)+rxt(k,680))*y(k,105)
         mat(k,1763) = (rxt(k,582)+rxt(k,659)+rxt(k,667)+rxt(k,676))*y(k,97) + ( &
                      + rxt(k,584)+rxt(k,655)+rxt(k,666)+rxt(k,675))*y(k,101) + ( &
                      + rxt(k,586)+rxt(k,660)+rxt(k,668)+rxt(k,677))*y(k,105) &
                      + rxt(k,230)*y(k,248)
         mat(k,1610) = (rxt(k,590)+rxt(k,686)+rxt(k,690)+rxt(k,694))*y(k,97) + ( &
                      + rxt(k,589)+rxt(k,685)+rxt(k,689)+rxt(k,693))*y(k,101) + ( &
                      + rxt(k,591)+rxt(k,687)+rxt(k,691)+rxt(k,695))*y(k,105) &
                      + rxt(k,268)*y(k,248)
         mat(k,1346) = .500_r8*rxt(k,432)*y(k,248)
         mat(k,112) = rxt(k,593)*y(k,248)
         mat(k,638) = rxt(k,413)*y(k,248)
         mat(k,473) = rxt(k,417)*y(k,248)
         mat(k,1877) = rxt(k,334)*y(k,68) + rxt(k,166)*y(k,92) + rxt(k,173)*y(k,248)
         mat(k,2183) = rxt(k,315)*y(k,29) + rxt(k,380)*y(k,32) + rxt(k,405)*y(k,34) &
                      + rxt(k,406)*y(k,35) + rxt(k,317)*y(k,45) + rxt(k,320)*y(k,47) &
                      + rxt(k,351)*y(k,51) + rxt(k,322)*y(k,52) + rxt(k,323)*y(k,53) &
                      + rxt(k,382)*y(k,54) + rxt(k,325)*y(k,55) + rxt(k,419)*y(k,57) &
                      + rxt(k,408)*y(k,58) + rxt(k,388)*y(k,59) + rxt(k,389)*y(k,60) &
                      + rxt(k,327)*y(k,61) + rxt(k,357)*y(k,63) + rxt(k,358)*y(k,64) &
                      + rxt(k,339)*y(k,69) + rxt(k,171)*y(k,93) + rxt(k,172)*y(k,95) &
                      + rxt(k,260)*y(k,97) + rxt(k,342)*y(k,100) + rxt(k,227)*y(k,101) &
                      + rxt(k,360)*y(k,103) + rxt(k,295)*y(k,105) + rxt(k,201) &
                      *y(k,106) + rxt(k,179)*y(k,107) + rxt(k,230)*y(k,109) &
                      + rxt(k,268)*y(k,110) + .500_r8*rxt(k,432)*y(k,134) + rxt(k,593) &
                      *y(k,149) + rxt(k,413)*y(k,176) + rxt(k,417)*y(k,177) &
                      + rxt(k,173)*y(k,233) + 2.000_r8*rxt(k,176)*y(k,248)
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
         mat(k, 114) = mat(k, 114) + lmat(k, 114)
         mat(k, 116) = mat(k, 116) + lmat(k, 116)
         mat(k, 117) = mat(k, 117) + lmat(k, 117)
         mat(k, 119) = mat(k, 119) + lmat(k, 119)
         mat(k, 120) = mat(k, 120) + lmat(k, 120)
         mat(k, 121) = mat(k, 121) + lmat(k, 121)
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
         mat(k, 138) = mat(k, 138) + lmat(k, 138)
         mat(k, 140) = mat(k, 140) + lmat(k, 140)
         mat(k, 141) = mat(k, 141) + lmat(k, 141)
         mat(k, 142) = mat(k, 142) + lmat(k, 142)
         mat(k, 144) = mat(k, 144) + lmat(k, 144)
         mat(k, 145) = mat(k, 145) + lmat(k, 145)
         mat(k, 146) = mat(k, 146) + lmat(k, 146)
         mat(k, 148) = mat(k, 148) + lmat(k, 148)
         mat(k, 149) = mat(k, 149) + lmat(k, 149)
         mat(k, 151) = mat(k, 151) + lmat(k, 151)
         mat(k, 152) = mat(k, 152) + lmat(k, 152)
         mat(k, 153) = mat(k, 153) + lmat(k, 153)
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
         mat(k, 173) = mat(k, 173) + lmat(k, 173)
         mat(k, 175) = mat(k, 175) + lmat(k, 175)
         mat(k, 176) = mat(k, 176) + lmat(k, 176)
         mat(k, 177) = mat(k, 177) + lmat(k, 177)
         mat(k, 178) = mat(k, 178) + lmat(k, 178)
         mat(k, 180) = mat(k, 180) + lmat(k, 180)
         mat(k, 181) = mat(k, 181) + lmat(k, 181)
         mat(k, 182) = mat(k, 182) + lmat(k, 182)
         mat(k, 183) = mat(k, 183) + lmat(k, 183)
         mat(k, 184) = mat(k, 184) + lmat(k, 184)
         mat(k, 186) = mat(k, 186) + lmat(k, 186)
         mat(k, 187) = mat(k, 187) + lmat(k, 187)
         mat(k, 188) = mat(k, 188) + lmat(k, 188)
         mat(k, 189) = mat(k, 189) + lmat(k, 189)
         mat(k, 192) = mat(k, 192) + lmat(k, 192)
         mat(k, 198) = lmat(k, 198)
         mat(k, 199) = lmat(k, 199)
         mat(k, 200) = lmat(k, 200)
         mat(k, 201) = lmat(k, 201)
         mat(k, 202) = mat(k, 202) + lmat(k, 202)
         mat(k, 207) = lmat(k, 207)
         mat(k, 208) = lmat(k, 208)
         mat(k, 209) = mat(k, 209) + lmat(k, 209)
         mat(k, 213) = mat(k, 213) + lmat(k, 213)
         mat(k, 215) = lmat(k, 215)
         mat(k, 216) = mat(k, 216) + lmat(k, 216)
         mat(k, 217) = mat(k, 217) + lmat(k, 217)
         mat(k, 218) = mat(k, 218) + lmat(k, 218)
         mat(k, 220) = mat(k, 220) + lmat(k, 220)
         mat(k, 222) = mat(k, 222) + lmat(k, 222)
         mat(k, 225) = mat(k, 225) + lmat(k, 225)
         mat(k, 232) = mat(k, 232) + lmat(k, 232)
         mat(k, 237) = lmat(k, 237)
         mat(k, 238) = lmat(k, 238)
         mat(k, 239) = lmat(k, 239)
         mat(k, 240) = lmat(k, 240)
         mat(k, 241) = mat(k, 241) + lmat(k, 241)
         mat(k, 243) = mat(k, 243) + lmat(k, 243)
         mat(k, 246) = mat(k, 246) + lmat(k, 246)
         mat(k, 254) = mat(k, 254) + lmat(k, 254)
         mat(k, 259) = mat(k, 259) + lmat(k, 259)
         mat(k, 260) = mat(k, 260) + lmat(k, 260)
         mat(k, 261) = mat(k, 261) + lmat(k, 261)
         mat(k, 264) = mat(k, 264) + lmat(k, 264)
         mat(k, 265) = mat(k, 265) + lmat(k, 265)
         mat(k, 266) = mat(k, 266) + lmat(k, 266)
         mat(k, 268) = mat(k, 268) + lmat(k, 268)
         mat(k, 269) = mat(k, 269) + lmat(k, 269)
         mat(k, 270) = mat(k, 270) + lmat(k, 270)
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
         mat(k, 292) = mat(k, 292) + lmat(k, 292)
         mat(k, 296) = mat(k, 296) + lmat(k, 296)
         mat(k, 297) = lmat(k, 297)
         mat(k, 298) = lmat(k, 298)
         mat(k, 300) = mat(k, 300) + lmat(k, 300)
         mat(k, 304) = mat(k, 304) + lmat(k, 304)
         mat(k, 305) = lmat(k, 305)
         mat(k, 307) = mat(k, 307) + lmat(k, 307)
         mat(k, 308) = lmat(k, 308)
         mat(k, 309) = lmat(k, 309)
         mat(k, 310) = lmat(k, 310)
         mat(k, 311) = lmat(k, 311)
         mat(k, 312) = mat(k, 312) + lmat(k, 312)
         mat(k, 313) = lmat(k, 313)
         mat(k, 314) = lmat(k, 314)
         mat(k, 316) = lmat(k, 316)
         mat(k, 317) = mat(k, 317) + lmat(k, 317)
         mat(k, 318) = mat(k, 318) + lmat(k, 318)
         mat(k, 319) = mat(k, 319) + lmat(k, 319)
         mat(k, 321) = mat(k, 321) + lmat(k, 321)
         mat(k, 325) = lmat(k, 325)
         mat(k, 326) = lmat(k, 326)
         mat(k, 327) = lmat(k, 327)
         mat(k, 328) = mat(k, 328) + lmat(k, 328)
         mat(k, 334) = lmat(k, 334)
         mat(k, 335) = lmat(k, 335)
         mat(k, 336) = lmat(k, 336)
         mat(k, 337) = mat(k, 337) + lmat(k, 337)
         mat(k, 343) = mat(k, 343) + lmat(k, 343)
         mat(k, 346) = mat(k, 346) + lmat(k, 346)
         mat(k, 349) = mat(k, 349) + lmat(k, 349)
         mat(k, 352) = mat(k, 352) + lmat(k, 352)
         mat(k, 357) = mat(k, 357) + lmat(k, 357)
         mat(k, 359) = lmat(k, 359)
         mat(k, 360) = lmat(k, 360)
         mat(k, 361) = mat(k, 361) + lmat(k, 361)
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
         mat(k, 373) = lmat(k, 373)
         mat(k, 374) = mat(k, 374) + lmat(k, 374)
         mat(k, 376) = lmat(k, 376)
         mat(k, 377) = lmat(k, 377)
         mat(k, 378) = mat(k, 378) + lmat(k, 378)
         mat(k, 379) = lmat(k, 379)
         mat(k, 380) = mat(k, 380) + lmat(k, 380)
         mat(k, 383) = lmat(k, 383)
         mat(k, 384) = mat(k, 384) + lmat(k, 384)
         mat(k, 385) = mat(k, 385) + lmat(k, 385)
         mat(k, 387) = mat(k, 387) + lmat(k, 387)
         mat(k, 388) = lmat(k, 388)
         mat(k, 389) = mat(k, 389) + lmat(k, 389)
         mat(k, 390) = lmat(k, 390)
         mat(k, 391) = lmat(k, 391)
         mat(k, 392) = lmat(k, 392)
         mat(k, 394) = mat(k, 394) + lmat(k, 394)
         mat(k, 395) = lmat(k, 395)
         mat(k, 398) = mat(k, 398) + lmat(k, 398)
         mat(k, 401) = lmat(k, 401)
         mat(k, 402) = mat(k, 402) + lmat(k, 402)
         mat(k, 404) = lmat(k, 404)
         mat(k, 405) = mat(k, 405) + lmat(k, 405)
         mat(k, 406) = lmat(k, 406)
         mat(k, 407) = lmat(k, 407)
         mat(k, 408) = lmat(k, 408)
         mat(k, 409) = mat(k, 409) + lmat(k, 409)
         mat(k, 417) = mat(k, 417) + lmat(k, 417)
         mat(k, 418) = lmat(k, 418)
         mat(k, 420) = mat(k, 420) + lmat(k, 420)
         mat(k, 425) = lmat(k, 425)
         mat(k, 426) = lmat(k, 426)
         mat(k, 427) = lmat(k, 427)
         mat(k, 428) = mat(k, 428) + lmat(k, 428)
         mat(k, 429) = lmat(k, 429)
         mat(k, 434) = mat(k, 434) + lmat(k, 434)
         mat(k, 436) = mat(k, 436) + lmat(k, 436)
         mat(k, 438) = lmat(k, 438)
         mat(k, 441) = mat(k, 441) + lmat(k, 441)
         mat(k, 444) = mat(k, 444) + lmat(k, 444)
         mat(k, 445) = lmat(k, 445)
         mat(k, 447) = mat(k, 447) + lmat(k, 447)
         mat(k, 449) = lmat(k, 449)
         mat(k, 450) = mat(k, 450) + lmat(k, 450)
         mat(k, 451) = lmat(k, 451)
         mat(k, 452) = mat(k, 452) + lmat(k, 452)
         mat(k, 453) = lmat(k, 453)
         mat(k, 454) = mat(k, 454) + lmat(k, 454)
         mat(k, 456) = mat(k, 456) + lmat(k, 456)
         mat(k, 461) = lmat(k, 461)
         mat(k, 462) = mat(k, 462) + lmat(k, 462)
         mat(k, 463) = lmat(k, 463)
         mat(k, 465) = mat(k, 465) + lmat(k, 465)
         mat(k, 466) = lmat(k, 466)
         mat(k, 467) = lmat(k, 467)
         mat(k, 468) = mat(k, 468) + lmat(k, 468)
         mat(k, 470) = lmat(k, 470)
         mat(k, 471) = lmat(k, 471)
         mat(k, 472) = mat(k, 472) + lmat(k, 472)
         mat(k, 474) = mat(k, 474) + lmat(k, 474)
         mat(k, 476) = mat(k, 476) + lmat(k, 476)
         mat(k, 477) = lmat(k, 477)
         mat(k, 478) = mat(k, 478) + lmat(k, 478)
         mat(k, 480) = mat(k, 480) + lmat(k, 480)
         mat(k, 482) = lmat(k, 482)
         mat(k, 483) = lmat(k, 483)
         mat(k, 484) = mat(k, 484) + lmat(k, 484)
         mat(k, 486) = mat(k, 486) + lmat(k, 486)
         mat(k, 487) = lmat(k, 487)
         mat(k, 489) = lmat(k, 489)
         mat(k, 490) = lmat(k, 490)
         mat(k, 491) = mat(k, 491) + lmat(k, 491)
         mat(k, 492) = mat(k, 492) + lmat(k, 492)
         mat(k, 497) = mat(k, 497) + lmat(k, 497)
         mat(k, 498) = lmat(k, 498)
         mat(k, 499) = lmat(k, 499)
         mat(k, 500) = lmat(k, 500)
         mat(k, 501) = lmat(k, 501)
         mat(k, 502) = lmat(k, 502)
         mat(k, 503) = lmat(k, 503)
         mat(k, 504) = mat(k, 504) + lmat(k, 504)
         mat(k, 506) = lmat(k, 506)
         mat(k, 507) = lmat(k, 507)
         mat(k, 508) = lmat(k, 508)
         mat(k, 509) = mat(k, 509) + lmat(k, 509)
         mat(k, 512) = mat(k, 512) + lmat(k, 512)
         mat(k, 518) = mat(k, 518) + lmat(k, 518)
         mat(k, 520) = lmat(k, 520)
         mat(k, 521) = mat(k, 521) + lmat(k, 521)
         mat(k, 525) = mat(k, 525) + lmat(k, 525)
         mat(k, 531) = mat(k, 531) + lmat(k, 531)
         mat(k, 534) = lmat(k, 534)
         mat(k, 535) = mat(k, 535) + lmat(k, 535)
         mat(k, 536) = lmat(k, 536)
         mat(k, 537) = lmat(k, 537)
         mat(k, 538) = lmat(k, 538)
         mat(k, 539) = mat(k, 539) + lmat(k, 539)
         mat(k, 542) = mat(k, 542) + lmat(k, 542)
         mat(k, 543) = mat(k, 543) + lmat(k, 543)
         mat(k, 547) = lmat(k, 547)
         mat(k, 548) = lmat(k, 548)
         mat(k, 549) = lmat(k, 549)
         mat(k, 550) = mat(k, 550) + lmat(k, 550)
         mat(k, 555) = mat(k, 555) + lmat(k, 555)
         mat(k, 557) = mat(k, 557) + lmat(k, 557)
         mat(k, 558) = lmat(k, 558)
         mat(k, 559) = lmat(k, 559)
         mat(k, 561) = lmat(k, 561)
         mat(k, 562) = lmat(k, 562)
         mat(k, 563) = mat(k, 563) + lmat(k, 563)
         mat(k, 566) = mat(k, 566) + lmat(k, 566)
         mat(k, 574) = mat(k, 574) + lmat(k, 574)
         mat(k, 578) = mat(k, 578) + lmat(k, 578)
         mat(k, 580) = mat(k, 580) + lmat(k, 580)
         mat(k, 582) = mat(k, 582) + lmat(k, 582)
         mat(k, 589) = lmat(k, 589)
         mat(k, 590) = mat(k, 590) + lmat(k, 590)
         mat(k, 592) = lmat(k, 592)
         mat(k, 594) = mat(k, 594) + lmat(k, 594)
         mat(k, 597) = mat(k, 597) + lmat(k, 597)
         mat(k, 599) = lmat(k, 599)
         mat(k, 602) = mat(k, 602) + lmat(k, 602)
         mat(k, 603) = mat(k, 603) + lmat(k, 603)
         mat(k, 606) = lmat(k, 606)
         mat(k, 608) = mat(k, 608) + lmat(k, 608)
         mat(k, 610) = lmat(k, 610)
         mat(k, 611) = lmat(k, 611)
         mat(k, 614) = mat(k, 614) + lmat(k, 614)
         mat(k, 620) = lmat(k, 620)
         mat(k, 621) = lmat(k, 621)
         mat(k, 622) = lmat(k, 622)
         mat(k, 623) = mat(k, 623) + lmat(k, 623)
         mat(k, 624) = lmat(k, 624)
         mat(k, 625) = lmat(k, 625)
         mat(k, 626) = lmat(k, 626)
         mat(k, 629) = mat(k, 629) + lmat(k, 629)
         mat(k, 630) = lmat(k, 630)
         mat(k, 631) = mat(k, 631) + lmat(k, 631)
         mat(k, 633) = lmat(k, 633)
         mat(k, 635) = lmat(k, 635)
         mat(k, 636) = lmat(k, 636)
         mat(k, 637) = mat(k, 637) + lmat(k, 637)
         mat(k, 639) = mat(k, 639) + lmat(k, 639)
         mat(k, 647) = mat(k, 647) + lmat(k, 647)
         mat(k, 655) = mat(k, 655) + lmat(k, 655)
         mat(k, 662) = lmat(k, 662)
         mat(k, 663) = mat(k, 663) + lmat(k, 663)
         mat(k, 664) = lmat(k, 664)
         mat(k, 670) = mat(k, 670) + lmat(k, 670)
         mat(k, 672) = mat(k, 672) + lmat(k, 672)
         mat(k, 676) = mat(k, 676) + lmat(k, 676)
         mat(k, 679) = lmat(k, 679)
         mat(k, 680) = lmat(k, 680)
         mat(k, 681) = lmat(k, 681)
         mat(k, 682) = lmat(k, 682)
         mat(k, 683) = mat(k, 683) + lmat(k, 683)
         mat(k, 690) = lmat(k, 690)
         mat(k, 692) = mat(k, 692) + lmat(k, 692)
         mat(k, 693) = mat(k, 693) + lmat(k, 693)
         mat(k, 695) = lmat(k, 695)
         mat(k, 697) = mat(k, 697) + lmat(k, 697)
         mat(k, 699) = mat(k, 699) + lmat(k, 699)
         mat(k, 702) = lmat(k, 702)
         mat(k, 707) = lmat(k, 707)
         mat(k, 708) = mat(k, 708) + lmat(k, 708)
         mat(k, 709) = mat(k, 709) + lmat(k, 709)
         mat(k, 710) = lmat(k, 710)
         mat(k, 712) = lmat(k, 712)
         mat(k, 713) = mat(k, 713) + lmat(k, 713)
         mat(k, 716) = mat(k, 716) + lmat(k, 716)
         mat(k, 722) = lmat(k, 722)
         mat(k, 723) = mat(k, 723) + lmat(k, 723)
         mat(k, 727) = lmat(k, 727)
         mat(k, 728) = lmat(k, 728)
         mat(k, 730) = lmat(k, 730)
         mat(k, 731) = lmat(k, 731)
         mat(k, 732) = mat(k, 732) + lmat(k, 732)
         mat(k, 733) = mat(k, 733) + lmat(k, 733)
         mat(k, 734) = lmat(k, 734)
         mat(k, 735) = lmat(k, 735)
         mat(k, 736) = lmat(k, 736)
         mat(k, 737) = lmat(k, 737)
         mat(k, 738) = lmat(k, 738)
         mat(k, 740) = lmat(k, 740)
         mat(k, 741) = lmat(k, 741)
         mat(k, 742) = mat(k, 742) + lmat(k, 742)
         mat(k, 743) = lmat(k, 743)
         mat(k, 744) = lmat(k, 744)
         mat(k, 745) = lmat(k, 745)
         mat(k, 746) = mat(k, 746) + lmat(k, 746)
         mat(k, 751) = lmat(k, 751)
         mat(k, 753) = lmat(k, 753)
         mat(k, 754) = lmat(k, 754)
         mat(k, 755) = mat(k, 755) + lmat(k, 755)
         mat(k, 756) = lmat(k, 756)
         mat(k, 757) = mat(k, 757) + lmat(k, 757)
         mat(k, 760) = mat(k, 760) + lmat(k, 760)
         mat(k, 761) = mat(k, 761) + lmat(k, 761)
         mat(k, 763) = mat(k, 763) + lmat(k, 763)
         mat(k, 764) = lmat(k, 764)
         mat(k, 766) = mat(k, 766) + lmat(k, 766)
         mat(k, 768) = mat(k, 768) + lmat(k, 768)
         mat(k, 775) = mat(k, 775) + lmat(k, 775)
         mat(k, 780) = lmat(k, 780)
         mat(k, 781) = lmat(k, 781)
         mat(k, 782) = lmat(k, 782)
         mat(k, 783) = lmat(k, 783)
         mat(k, 784) = mat(k, 784) + lmat(k, 784)
         mat(k, 789) = lmat(k, 789)
         mat(k, 791) = lmat(k, 791)
         mat(k, 793) = lmat(k, 793)
         mat(k, 794) = mat(k, 794) + lmat(k, 794)
         mat(k, 797) = mat(k, 797) + lmat(k, 797)
         mat(k, 804) = mat(k, 804) + lmat(k, 804)
         mat(k, 815) = mat(k, 815) + lmat(k, 815)
         mat(k, 826) = mat(k, 826) + lmat(k, 826)
         mat(k, 828) = lmat(k, 828)
         mat(k, 829) = lmat(k, 829)
         mat(k, 830) = lmat(k, 830)
         mat(k, 831) = lmat(k, 831)
         mat(k, 832) = mat(k, 832) + lmat(k, 832)
         mat(k, 839) = mat(k, 839) + lmat(k, 839)
         mat(k, 850) = mat(k, 850) + lmat(k, 850)
         mat(k, 859) = mat(k, 859) + lmat(k, 859)
         mat(k, 868) = mat(k, 868) + lmat(k, 868)
         mat(k, 873) = mat(k, 873) + lmat(k, 873)
         mat(k, 883) = lmat(k, 883)
         mat(k, 884) = lmat(k, 884)
         mat(k, 885) = lmat(k, 885)
         mat(k, 889) = mat(k, 889) + lmat(k, 889)
         mat(k, 897) = mat(k, 897) + lmat(k, 897)
         mat(k, 898) = mat(k, 898) + lmat(k, 898)
         mat(k, 899) = mat(k, 899) + lmat(k, 899)
         mat(k, 900) = lmat(k, 900)
         mat(k, 904) = lmat(k, 904)
         mat(k, 905) = mat(k, 905) + lmat(k, 905)
         mat(k, 906) = mat(k, 906) + lmat(k, 906)
         mat(k, 907) = mat(k, 907) + lmat(k, 907)
         mat(k, 908) = lmat(k, 908)
         mat(k, 909) = mat(k, 909) + lmat(k, 909)
         mat(k, 911) = lmat(k, 911)
         mat(k, 912) = lmat(k, 912)
         mat(k, 914) = mat(k, 914) + lmat(k, 914)
         mat(k, 918) = mat(k, 918) + lmat(k, 918)
         mat(k, 934) = lmat(k, 934)
         mat(k, 935) = lmat(k, 935)
         mat(k, 936) = mat(k, 936) + lmat(k, 936)
         mat(k, 938) = lmat(k, 938)
         mat(k, 940) = lmat(k, 940)
         mat(k, 941) = lmat(k, 941)
         mat(k, 943) = mat(k, 943) + lmat(k, 943)
         mat(k, 944) = mat(k, 944) + lmat(k, 944)
         mat(k, 945) = lmat(k, 945)
         mat(k, 947) = mat(k, 947) + lmat(k, 947)
         mat(k, 956) = mat(k, 956) + lmat(k, 956)
         mat(k, 974) = mat(k, 974) + lmat(k, 974)
         mat(k, 998) = mat(k, 998) + lmat(k, 998)
         mat(k,1008) = mat(k,1008) + lmat(k,1008)
         mat(k,1009) = mat(k,1009) + lmat(k,1009)
         mat(k,1011) = mat(k,1011) + lmat(k,1011)
         mat(k,1012) = mat(k,1012) + lmat(k,1012)
         mat(k,1015) = mat(k,1015) + lmat(k,1015)
         mat(k,1016) = mat(k,1016) + lmat(k,1016)
         mat(k,1017) = mat(k,1017) + lmat(k,1017)
         mat(k,1018) = lmat(k,1018)
         mat(k,1019) = mat(k,1019) + lmat(k,1019)
         mat(k,1021) = lmat(k,1021)
         mat(k,1022) = lmat(k,1022)
         mat(k,1025) = mat(k,1025) + lmat(k,1025)
         mat(k,1038) = mat(k,1038) + lmat(k,1038)
         mat(k,1039) = lmat(k,1039)
         mat(k,1042) = lmat(k,1042)
         mat(k,1045) = mat(k,1045) + lmat(k,1045)
         mat(k,1051) = lmat(k,1051)
         mat(k,1053) = mat(k,1053) + lmat(k,1053)
         mat(k,1054) = mat(k,1054) + lmat(k,1054)
         mat(k,1055) = mat(k,1055) + lmat(k,1055)
         mat(k,1071) = mat(k,1071) + lmat(k,1071)
         mat(k,1091) = mat(k,1091) + lmat(k,1091)
         mat(k,1093) = lmat(k,1093)
         mat(k,1094) = lmat(k,1094)
         mat(k,1096) = lmat(k,1096)
         mat(k,1100) = mat(k,1100) + lmat(k,1100)
         mat(k,1116) = mat(k,1116) + lmat(k,1116)
         mat(k,1133) = lmat(k,1133)
         mat(k,1137) = mat(k,1137) + lmat(k,1137)
         mat(k,1143) = mat(k,1143) + lmat(k,1143)
         mat(k,1146) = lmat(k,1146)
         mat(k,1147) = lmat(k,1147)
         mat(k,1149) = mat(k,1149) + lmat(k,1149)
         mat(k,1150) = lmat(k,1150)
         mat(k,1152) = lmat(k,1152)
         mat(k,1153) = lmat(k,1153)
         mat(k,1160) = mat(k,1160) + lmat(k,1160)
         mat(k,1161) = lmat(k,1161)
         mat(k,1162) = mat(k,1162) + lmat(k,1162)
         mat(k,1163) = mat(k,1163) + lmat(k,1163)
         mat(k,1170) = mat(k,1170) + lmat(k,1170)
         mat(k,1182) = mat(k,1182) + lmat(k,1182)
         mat(k,1183) = mat(k,1183) + lmat(k,1183)
         mat(k,1184) = mat(k,1184) + lmat(k,1184)
         mat(k,1185) = mat(k,1185) + lmat(k,1185)
         mat(k,1186) = mat(k,1186) + lmat(k,1186)
         mat(k,1187) = mat(k,1187) + lmat(k,1187)
         mat(k,1189) = mat(k,1189) + lmat(k,1189)
         mat(k,1190) = mat(k,1190) + lmat(k,1190)
         mat(k,1195) = mat(k,1195) + lmat(k,1195)
         mat(k,1200) = lmat(k,1200)
         mat(k,1201) = lmat(k,1201)
         mat(k,1202) = lmat(k,1202)
         mat(k,1203) = lmat(k,1203)
         mat(k,1204) = mat(k,1204) + lmat(k,1204)
         mat(k,1205) = lmat(k,1205)
         mat(k,1207) = lmat(k,1207)
         mat(k,1209) = lmat(k,1209)
         mat(k,1210) = mat(k,1210) + lmat(k,1210)
         mat(k,1213) = lmat(k,1213)
         mat(k,1215) = lmat(k,1215)
         mat(k,1217) = mat(k,1217) + lmat(k,1217)
         mat(k,1219) = lmat(k,1219)
         mat(k,1220) = lmat(k,1220)
         mat(k,1221) = mat(k,1221) + lmat(k,1221)
         mat(k,1231) = mat(k,1231) + lmat(k,1231)
         mat(k,1252) = mat(k,1252) + lmat(k,1252)
         mat(k,1268) = mat(k,1268) + lmat(k,1268)
         mat(k,1280) = mat(k,1280) + lmat(k,1280)
         mat(k,1286) = lmat(k,1286)
         mat(k,1291) = mat(k,1291) + lmat(k,1291)
         mat(k,1300) = mat(k,1300) + lmat(k,1300)
         mat(k,1320) = mat(k,1320) + lmat(k,1320)
         mat(k,1335) = mat(k,1335) + lmat(k,1335)
         mat(k,1336) = mat(k,1336) + lmat(k,1336)
         mat(k,1339) = mat(k,1339) + lmat(k,1339)
         mat(k,1340) = mat(k,1340) + lmat(k,1340)
         mat(k,1341) = mat(k,1341) + lmat(k,1341)
         mat(k,1342) = mat(k,1342) + lmat(k,1342)
         mat(k,1347) = mat(k,1347) + lmat(k,1347)
         mat(k,1348) = mat(k,1348) + lmat(k,1348)
         mat(k,1349) = mat(k,1349) + lmat(k,1349)
         mat(k,1350) = lmat(k,1350)
         mat(k,1368) = mat(k,1368) + lmat(k,1368)
         mat(k,1384) = lmat(k,1384)
         mat(k,1401) = mat(k,1401) + lmat(k,1401)
         mat(k,1408) = mat(k,1408) + lmat(k,1408)
         mat(k,1425) = mat(k,1425) + lmat(k,1425)
         mat(k,1440) = lmat(k,1440)
         mat(k,1442) = mat(k,1442) + lmat(k,1442)
         mat(k,1446) = mat(k,1446) + lmat(k,1446)
         mat(k,1448) = mat(k,1448) + lmat(k,1448)
         mat(k,1450) = lmat(k,1450)
         mat(k,1468) = mat(k,1468) + lmat(k,1468)
         mat(k,1500) = mat(k,1500) + lmat(k,1500)
         mat(k,1516) = mat(k,1516) + lmat(k,1516)
         mat(k,1521) = mat(k,1521) + lmat(k,1521)
         mat(k,1525) = mat(k,1525) + lmat(k,1525)
         mat(k,1530) = mat(k,1530) + lmat(k,1530)
         mat(k,1539) = mat(k,1539) + lmat(k,1539)
         mat(k,1543) = mat(k,1543) + lmat(k,1543)
         mat(k,1556) = lmat(k,1556)
         mat(k,1557) = mat(k,1557) + lmat(k,1557)
         mat(k,1558) = mat(k,1558) + lmat(k,1558)
         mat(k,1563) = lmat(k,1563)
         mat(k,1572) = lmat(k,1572)
         mat(k,1574) = lmat(k,1574)
         mat(k,1575) = mat(k,1575) + lmat(k,1575)
         mat(k,1576) = mat(k,1576) + lmat(k,1576)
         mat(k,1577) = mat(k,1577) + lmat(k,1577)
         mat(k,1578) = mat(k,1578) + lmat(k,1578)
         mat(k,1582) = mat(k,1582) + lmat(k,1582)
         mat(k,1584) = mat(k,1584) + lmat(k,1584)
         mat(k,1585) = lmat(k,1585)
         mat(k,1588) = mat(k,1588) + lmat(k,1588)
         mat(k,1589) = mat(k,1589) + lmat(k,1589)
         mat(k,1590) = mat(k,1590) + lmat(k,1590)
         mat(k,1594) = mat(k,1594) + lmat(k,1594)
         mat(k,1601) = mat(k,1601) + lmat(k,1601)
         mat(k,1607) = lmat(k,1607)
         mat(k,1648) = mat(k,1648) + lmat(k,1648)
         mat(k,1666) = mat(k,1666) + lmat(k,1666)
         mat(k,1667) = mat(k,1667) + lmat(k,1667)
         mat(k,1672) = mat(k,1672) + lmat(k,1672)
         mat(k,1678) = mat(k,1678) + lmat(k,1678)
         mat(k,1682) = lmat(k,1682)
         mat(k,1690) = mat(k,1690) + lmat(k,1690)
         mat(k,1692) = lmat(k,1692)
         mat(k,1697) = mat(k,1697) + lmat(k,1697)
         mat(k,1702) = mat(k,1702) + lmat(k,1702)
         mat(k,1724) = mat(k,1724) + lmat(k,1724)
         mat(k,1726) = mat(k,1726) + lmat(k,1726)
         mat(k,1727) = mat(k,1727) + lmat(k,1727)
         mat(k,1740) = mat(k,1740) + lmat(k,1740)
         mat(k,1749) = mat(k,1749) + lmat(k,1749)
         mat(k,1750) = mat(k,1750) + lmat(k,1750)
         mat(k,1753) = mat(k,1753) + lmat(k,1753)
         mat(k,1861) = mat(k,1861) + lmat(k,1861)
         mat(k,1877) = mat(k,1877) + lmat(k,1877)
         mat(k,1917) = mat(k,1917) + lmat(k,1917)
         mat(k,1939) = mat(k,1939) + lmat(k,1939)
         mat(k,1941) = mat(k,1941) + lmat(k,1941)
         mat(k,1988) = mat(k,1988) + lmat(k,1988)
         mat(k,1990) = mat(k,1990) + lmat(k,1990)
         mat(k,2171) = mat(k,2171) + lmat(k,2171)
         mat(k,2207) = mat(k,2207) + lmat(k,2207)
         mat(k,2216) = mat(k,2216) + lmat(k,2216)
         mat(k,2238) = mat(k,2238) + lmat(k,2238)
         mat(k,2239) = mat(k,2239) + lmat(k,2239)
         mat(k,2244) = mat(k,2244) + lmat(k,2244)
         mat(k,2256) = lmat(k,2256)
         mat(k,2262) = mat(k,2262) + lmat(k,2262)
         mat(k,2265) = mat(k,2265) + lmat(k,2265)
         mat(k,2271) = lmat(k,2271)
         mat(k,2288) = mat(k,2288) + lmat(k,2288)
         mat(k,2292) = mat(k,2292) + lmat(k,2292)
         mat(k,2295) = mat(k,2295) + lmat(k,2295)
         mat(k,2324) = mat(k,2324) + lmat(k,2324)
         mat(k,2390) = mat(k,2390) + lmat(k,2390)
         mat(k,2392) = mat(k,2392) + lmat(k,2392)
         mat(k,2395) = mat(k,2395) + lmat(k,2395)
         mat(k,2398) = mat(k,2398) + lmat(k,2398)
         mat(k,2400) = mat(k,2400) + lmat(k,2400)
         mat(k,2424) = mat(k,2424) + lmat(k,2424)
         mat(k,2445) = mat(k,2445) + lmat(k,2445)
         mat(k,2449) = mat(k,2449) + lmat(k,2449)
         mat(k,2452) = mat(k,2452) + lmat(k,2452)
         mat(k,2501) = mat(k,2501) + lmat(k,2501)
         mat(k,2502) = mat(k,2502) + lmat(k,2502)
         mat(k,2504) = mat(k,2504) + lmat(k,2504)
         mat(k,2510) = mat(k,2510) + lmat(k,2510)
         mat(k,2512) = mat(k,2512) + lmat(k,2512)
         mat(k,2566) = mat(k,2566) + lmat(k,2566)
         mat(k,2568) = mat(k,2568) + lmat(k,2568)
         mat(k,2577) = mat(k,2577) + lmat(k,2577)
         mat(k,2605) = mat(k,2605) + lmat(k,2605)
         mat(k,2669) = mat(k,2669) + lmat(k,2669)
         mat(k,2679) = mat(k,2679) + lmat(k,2679)
         mat(k,2686) = lmat(k,2686)
         mat(k,2695) = lmat(k,2695)
         mat(k,2696) = mat(k,2696) + lmat(k,2696)
         mat(k,2697) = mat(k,2697) + lmat(k,2697)
         mat(k,2698) = lmat(k,2698)
         mat(k,2709) = mat(k,2709) + lmat(k,2709)
         mat(k, 250) = 0._r8
         mat(k, 251) = 0._r8
         mat(k, 320) = 0._r8
         mat(k, 382) = 0._r8
         mat(k, 396) = 0._r8
         mat(k, 513) = 0._r8
         mat(k, 515) = 0._r8
         mat(k, 528) = 0._r8
         mat(k, 567) = 0._r8
         mat(k, 570) = 0._r8
         mat(k, 576) = 0._r8
         mat(k, 586) = 0._r8
         mat(k, 718) = 0._r8
         mat(k, 720) = 0._r8
         mat(k, 724) = 0._r8
         mat(k, 725) = 0._r8
         mat(k, 729) = 0._r8
         mat(k, 747) = 0._r8
         mat(k, 749) = 0._r8
         mat(k, 750) = 0._r8
         mat(k, 752) = 0._r8
         mat(k, 758) = 0._r8
         mat(k, 759) = 0._r8
         mat(k, 762) = 0._r8
         mat(k, 785) = 0._r8
         mat(k, 787) = 0._r8
         mat(k, 788) = 0._r8
         mat(k, 790) = 0._r8
         mat(k, 792) = 0._r8
         mat(k, 798) = 0._r8
         mat(k, 801) = 0._r8
         mat(k, 814) = 0._r8
         mat(k, 816) = 0._r8
         mat(k, 817) = 0._r8
         mat(k, 819) = 0._r8
         mat(k, 822) = 0._r8
         mat(k, 838) = 0._r8
         mat(k, 840) = 0._r8
         mat(k, 841) = 0._r8
         mat(k, 843) = 0._r8
         mat(k, 845) = 0._r8
         mat(k, 847) = 0._r8
         mat(k, 861) = 0._r8
         mat(k, 864) = 0._r8
         mat(k, 867) = 0._r8
         mat(k, 879) = 0._r8
         mat(k, 882) = 0._r8
         mat(k, 894) = 0._r8
         mat(k, 902) = 0._r8
         mat(k, 939) = 0._r8
         mat(k, 942) = 0._r8
         mat(k, 975) = 0._r8
         mat(k, 978) = 0._r8
         mat(k, 985) = 0._r8
         mat(k, 990) = 0._r8
         mat(k, 996) = 0._r8
         mat(k, 997) = 0._r8
         mat(k,1001) = 0._r8
         mat(k,1002) = 0._r8
         mat(k,1005) = 0._r8
         mat(k,1043) = 0._r8
         mat(k,1046) = 0._r8
         mat(k,1049) = 0._r8
         mat(k,1057) = 0._r8
         mat(k,1067) = 0._r8
         mat(k,1070) = 0._r8
         mat(k,1073) = 0._r8
         mat(k,1079) = 0._r8
         mat(k,1081) = 0._r8
         mat(k,1085) = 0._r8
         mat(k,1087) = 0._r8
         mat(k,1089) = 0._r8
         mat(k,1090) = 0._r8
         mat(k,1119) = 0._r8
         mat(k,1120) = 0._r8
         mat(k,1121) = 0._r8
         mat(k,1127) = 0._r8
         mat(k,1129) = 0._r8
         mat(k,1131) = 0._r8
         mat(k,1132) = 0._r8
         mat(k,1135) = 0._r8
         mat(k,1138) = 0._r8
         mat(k,1139) = 0._r8
         mat(k,1140) = 0._r8
         mat(k,1141) = 0._r8
         mat(k,1142) = 0._r8
         mat(k,1145) = 0._r8
         mat(k,1148) = 0._r8
         mat(k,1165) = 0._r8
         mat(k,1171) = 0._r8
         mat(k,1172) = 0._r8
         mat(k,1173) = 0._r8
         mat(k,1177) = 0._r8
         mat(k,1178) = 0._r8
         mat(k,1188) = 0._r8
         mat(k,1192) = 0._r8
         mat(k,1193) = 0._r8
         mat(k,1198) = 0._r8
         mat(k,1206) = 0._r8
         mat(k,1208) = 0._r8
         mat(k,1212) = 0._r8
         mat(k,1214) = 0._r8
         mat(k,1216) = 0._r8
         mat(k,1232) = 0._r8
         mat(k,1233) = 0._r8
         mat(k,1237) = 0._r8
         mat(k,1238) = 0._r8
         mat(k,1239) = 0._r8
         mat(k,1242) = 0._r8
         mat(k,1248) = 0._r8
         mat(k,1249) = 0._r8
         mat(k,1250) = 0._r8
         mat(k,1251) = 0._r8
         mat(k,1253) = 0._r8
         mat(k,1254) = 0._r8
         mat(k,1258) = 0._r8
         mat(k,1259) = 0._r8
         mat(k,1260) = 0._r8
         mat(k,1261) = 0._r8
         mat(k,1264) = 0._r8
         mat(k,1277) = 0._r8
         mat(k,1285) = 0._r8
         mat(k,1289) = 0._r8
         mat(k,1306) = 0._r8
         mat(k,1307) = 0._r8
         mat(k,1313) = 0._r8
         mat(k,1315) = 0._r8
         mat(k,1317) = 0._r8
         mat(k,1318) = 0._r8
         mat(k,1319) = 0._r8
         mat(k,1321) = 0._r8
         mat(k,1322) = 0._r8
         mat(k,1323) = 0._r8
         mat(k,1328) = 0._r8
         mat(k,1329) = 0._r8
         mat(k,1330) = 0._r8
         mat(k,1344) = 0._r8
         mat(k,1352) = 0._r8
         mat(k,1360) = 0._r8
         mat(k,1361) = 0._r8
         mat(k,1362) = 0._r8
         mat(k,1363) = 0._r8
         mat(k,1364) = 0._r8
         mat(k,1365) = 0._r8
         mat(k,1367) = 0._r8
         mat(k,1369) = 0._r8
         mat(k,1371) = 0._r8
         mat(k,1376) = 0._r8
         mat(k,1377) = 0._r8
         mat(k,1378) = 0._r8
         mat(k,1381) = 0._r8
         mat(k,1383) = 0._r8
         mat(k,1387) = 0._r8
         mat(k,1390) = 0._r8
         mat(k,1391) = 0._r8
         mat(k,1394) = 0._r8
         mat(k,1395) = 0._r8
         mat(k,1397) = 0._r8
         mat(k,1398) = 0._r8
         mat(k,1399) = 0._r8
         mat(k,1402) = 0._r8
         mat(k,1403) = 0._r8
         mat(k,1404) = 0._r8
         mat(k,1409) = 0._r8
         mat(k,1410) = 0._r8
         mat(k,1411) = 0._r8
         mat(k,1414) = 0._r8
         mat(k,1416) = 0._r8
         mat(k,1423) = 0._r8
         mat(k,1426) = 0._r8
         mat(k,1431) = 0._r8
         mat(k,1432) = 0._r8
         mat(k,1433) = 0._r8
         mat(k,1437) = 0._r8
         mat(k,1443) = 0._r8
         mat(k,1447) = 0._r8
         mat(k,1449) = 0._r8
         mat(k,1454) = 0._r8
         mat(k,1455) = 0._r8
         mat(k,1456) = 0._r8
         mat(k,1457) = 0._r8
         mat(k,1459) = 0._r8
         mat(k,1460) = 0._r8
         mat(k,1465) = 0._r8
         mat(k,1466) = 0._r8
         mat(k,1467) = 0._r8
         mat(k,1474) = 0._r8
         mat(k,1475) = 0._r8
         mat(k,1480) = 0._r8
         mat(k,1501) = 0._r8
         mat(k,1506) = 0._r8
         mat(k,1507) = 0._r8
         mat(k,1508) = 0._r8
         mat(k,1512) = 0._r8
         mat(k,1520) = 0._r8
         mat(k,1526) = 0._r8
         mat(k,1532) = 0._r8
         mat(k,1536) = 0._r8
         mat(k,1537) = 0._r8
         mat(k,1540) = 0._r8
         mat(k,1544) = 0._r8
         mat(k,1545) = 0._r8
         mat(k,1552) = 0._r8
         mat(k,1553) = 0._r8
         mat(k,1559) = 0._r8
         mat(k,1570) = 0._r8
         mat(k,1586) = 0._r8
         mat(k,1587) = 0._r8
         mat(k,1595) = 0._r8
         mat(k,1597) = 0._r8
         mat(k,1598) = 0._r8
         mat(k,1599) = 0._r8
         mat(k,1600) = 0._r8
         mat(k,1602) = 0._r8
         mat(k,1605) = 0._r8
         mat(k,1608) = 0._r8
         mat(k,1609) = 0._r8
         mat(k,1622) = 0._r8
         mat(k,1647) = 0._r8
         mat(k,1650) = 0._r8
         mat(k,1651) = 0._r8
         mat(k,1654) = 0._r8
         mat(k,1655) = 0._r8
         mat(k,1656) = 0._r8
         mat(k,1657) = 0._r8
         mat(k,1658) = 0._r8
         mat(k,1660) = 0._r8
         mat(k,1662) = 0._r8
         mat(k,1664) = 0._r8
         mat(k,1670) = 0._r8
         mat(k,1671) = 0._r8
         mat(k,1674) = 0._r8
         mat(k,1675) = 0._r8
         mat(k,1676) = 0._r8
         mat(k,1677) = 0._r8
         mat(k,1680) = 0._r8
         mat(k,1681) = 0._r8
         mat(k,1683) = 0._r8
         mat(k,1684) = 0._r8
         mat(k,1686) = 0._r8
         mat(k,1689) = 0._r8
         mat(k,1693) = 0._r8
         mat(k,1694) = 0._r8
         mat(k,1695) = 0._r8
         mat(k,1696) = 0._r8
         mat(k,1699) = 0._r8
         mat(k,1703) = 0._r8
         mat(k,1706) = 0._r8
         mat(k,1708) = 0._r8
         mat(k,1711) = 0._r8
         mat(k,1712) = 0._r8
         mat(k,1713) = 0._r8
         mat(k,1714) = 0._r8
         mat(k,1715) = 0._r8
         mat(k,1731) = 0._r8
         mat(k,1734) = 0._r8
         mat(k,1735) = 0._r8
         mat(k,1736) = 0._r8
         mat(k,1737) = 0._r8
         mat(k,1738) = 0._r8
         mat(k,1745) = 0._r8
         mat(k,1746) = 0._r8
         mat(k,1747) = 0._r8
         mat(k,1751) = 0._r8
         mat(k,1752) = 0._r8
         mat(k,1755) = 0._r8
         mat(k,1756) = 0._r8
         mat(k,1758) = 0._r8
         mat(k,1759) = 0._r8
         mat(k,1760) = 0._r8
         mat(k,1761) = 0._r8
         mat(k,1762) = 0._r8
         mat(k,1779) = 0._r8
         mat(k,1797) = 0._r8
         mat(k,1798) = 0._r8
         mat(k,1823) = 0._r8
         mat(k,1826) = 0._r8
         mat(k,1829) = 0._r8
         mat(k,1832) = 0._r8
         mat(k,1834) = 0._r8
         mat(k,1836) = 0._r8
         mat(k,1843) = 0._r8
         mat(k,1848) = 0._r8
         mat(k,1852) = 0._r8
         mat(k,1854) = 0._r8
         mat(k,1864) = 0._r8
         mat(k,1868) = 0._r8
         mat(k,1895) = 0._r8
         mat(k,1901) = 0._r8
         mat(k,1902) = 0._r8
         mat(k,1904) = 0._r8
         mat(k,1905) = 0._r8
         mat(k,1907) = 0._r8
         mat(k,1909) = 0._r8
         mat(k,1912) = 0._r8
         mat(k,1919) = 0._r8
         mat(k,1921) = 0._r8
         mat(k,1922) = 0._r8
         mat(k,1923) = 0._r8
         mat(k,1928) = 0._r8
         mat(k,1931) = 0._r8
         mat(k,1932) = 0._r8
         mat(k,1934) = 0._r8
         mat(k,1935) = 0._r8
         mat(k,1936) = 0._r8
         mat(k,1937) = 0._r8
         mat(k,1938) = 0._r8
         mat(k,1940) = 0._r8
         mat(k,1942) = 0._r8
         mat(k,1945) = 0._r8
         mat(k,1946) = 0._r8
         mat(k,1947) = 0._r8
         mat(k,1948) = 0._r8
         mat(k,1949) = 0._r8
         mat(k,1950) = 0._r8
         mat(k,1951) = 0._r8
         mat(k,1952) = 0._r8
         mat(k,1954) = 0._r8
         mat(k,1979) = 0._r8
         mat(k,1981) = 0._r8
         mat(k,1984) = 0._r8
         mat(k,1991) = 0._r8
         mat(k,1992) = 0._r8
         mat(k,1995) = 0._r8
         mat(k,1996) = 0._r8
         mat(k,1998) = 0._r8
         mat(k,2075) = 0._r8
         mat(k,2094) = 0._r8
         mat(k,2108) = 0._r8
         mat(k,2112) = 0._r8
         mat(k,2120) = 0._r8
         mat(k,2121) = 0._r8
         mat(k,2147) = 0._r8
         mat(k,2170) = 0._r8
         mat(k,2185) = 0._r8
         mat(k,2189) = 0._r8
         mat(k,2195) = 0._r8
         mat(k,2196) = 0._r8
         mat(k,2197) = 0._r8
         mat(k,2205) = 0._r8
         mat(k,2209) = 0._r8
         mat(k,2218) = 0._r8
         mat(k,2227) = 0._r8
         mat(k,2228) = 0._r8
         mat(k,2230) = 0._r8
         mat(k,2231) = 0._r8
         mat(k,2232) = 0._r8
         mat(k,2235) = 0._r8
         mat(k,2236) = 0._r8
         mat(k,2240) = 0._r8
         mat(k,2249) = 0._r8
         mat(k,2252) = 0._r8
         mat(k,2253) = 0._r8
         mat(k,2254) = 0._r8
         mat(k,2255) = 0._r8
         mat(k,2257) = 0._r8
         mat(k,2258) = 0._r8
         mat(k,2259) = 0._r8
         mat(k,2260) = 0._r8
         mat(k,2261) = 0._r8
         mat(k,2263) = 0._r8
         mat(k,2264) = 0._r8
         mat(k,2266) = 0._r8
         mat(k,2267) = 0._r8
         mat(k,2269) = 0._r8
         mat(k,2270) = 0._r8
         mat(k,2272) = 0._r8
         mat(k,2273) = 0._r8
         mat(k,2289) = 0._r8
         mat(k,2290) = 0._r8
         mat(k,2294) = 0._r8
         mat(k,2297) = 0._r8
         mat(k,2301) = 0._r8
         mat(k,2303) = 0._r8
         mat(k,2306) = 0._r8
         mat(k,2310) = 0._r8
         mat(k,2311) = 0._r8
         mat(k,2313) = 0._r8
         mat(k,2314) = 0._r8
         mat(k,2316) = 0._r8
         mat(k,2317) = 0._r8
         mat(k,2318) = 0._r8
         mat(k,2319) = 0._r8
         mat(k,2320) = 0._r8
         mat(k,2322) = 0._r8
         mat(k,2323) = 0._r8
         mat(k,2330) = 0._r8
         mat(k,2331) = 0._r8
         mat(k,2337) = 0._r8
         mat(k,2339) = 0._r8
         mat(k,2344) = 0._r8
         mat(k,2348) = 0._r8
         mat(k,2358) = 0._r8
         mat(k,2360) = 0._r8
         mat(k,2363) = 0._r8
         mat(k,2364) = 0._r8
         mat(k,2377) = 0._r8
         mat(k,2378) = 0._r8
         mat(k,2380) = 0._r8
         mat(k,2381) = 0._r8
         mat(k,2383) = 0._r8
         mat(k,2384) = 0._r8
         mat(k,2387) = 0._r8
         mat(k,2388) = 0._r8
         mat(k,2393) = 0._r8
         mat(k,2394) = 0._r8
         mat(k,2397) = 0._r8
         mat(k,2399) = 0._r8
         mat(k,2401) = 0._r8
         mat(k,2404) = 0._r8
         mat(k,2405) = 0._r8
         mat(k,2409) = 0._r8
         mat(k,2410) = 0._r8
         mat(k,2411) = 0._r8
         mat(k,2412) = 0._r8
         mat(k,2414) = 0._r8
         mat(k,2415) = 0._r8
         mat(k,2416) = 0._r8
         mat(k,2417) = 0._r8
         mat(k,2418) = 0._r8
         mat(k,2420) = 0._r8
         mat(k,2421) = 0._r8
         mat(k,2429) = 0._r8
         mat(k,2438) = 0._r8
         mat(k,2439) = 0._r8
         mat(k,2442) = 0._r8
         mat(k,2443) = 0._r8
         mat(k,2447) = 0._r8
         mat(k,2450) = 0._r8
         mat(k,2454) = 0._r8
         mat(k,2456) = 0._r8
         mat(k,2462) = 0._r8
         mat(k,2464) = 0._r8
         mat(k,2466) = 0._r8
         mat(k,2470) = 0._r8
         mat(k,2473) = 0._r8
         mat(k,2476) = 0._r8
         mat(k,2482) = 0._r8
         mat(k,2483) = 0._r8
         mat(k,2484) = 0._r8
         mat(k,2485) = 0._r8
         mat(k,2488) = 0._r8
         mat(k,2490) = 0._r8
         mat(k,2491) = 0._r8
         mat(k,2492) = 0._r8
         mat(k,2493) = 0._r8
         mat(k,2494) = 0._r8
         mat(k,2495) = 0._r8
         mat(k,2496) = 0._r8
         mat(k,2499) = 0._r8
         mat(k,2500) = 0._r8
         mat(k,2513) = 0._r8
         mat(k,2527) = 0._r8
         mat(k,2531) = 0._r8
         mat(k,2535) = 0._r8
         mat(k,2540) = 0._r8
         mat(k,2541) = 0._r8
         mat(k,2544) = 0._r8
         mat(k,2545) = 0._r8
         mat(k,2546) = 0._r8
         mat(k,2548) = 0._r8
         mat(k,2551) = 0._r8
         mat(k,2552) = 0._r8
         mat(k,2553) = 0._r8
         mat(k,2555) = 0._r8
         mat(k,2561) = 0._r8
         mat(k,2562) = 0._r8
         mat(k,2570) = 0._r8
         mat(k,2579) = 0._r8
         mat(k,2627) = 0._r8
         mat(k,2656) = 0._r8
         mat(k,2657) = 0._r8
         mat(k,2658) = 0._r8
         mat(k,2660) = 0._r8
         mat(k,2662) = 0._r8
         mat(k,2663) = 0._r8
         mat(k,2666) = 0._r8
         mat(k,2667) = 0._r8
         mat(k,2671) = 0._r8
         mat(k,2680) = 0._r8
         mat(k,2685) = 0._r8
         mat(k,2687) = 0._r8
         mat(k,2688) = 0._r8
         mat(k,2689) = 0._r8
         mat(k,2690) = 0._r8
         mat(k,2691) = 0._r8
         mat(k,2692) = 0._r8
         mat(k,2693) = 0._r8
         mat(k,2694) = 0._r8
         mat(k,2699) = 0._r8
         mat(k,2700) = 0._r8
         mat(k,2701) = 0._r8
         mat(k,2702) = 0._r8
         mat(k,2703) = 0._r8
         mat(k,2704) = 0._r8
         mat(k,2705) = 0._r8
         mat(k,2706) = 0._r8
         mat(k,2707) = 0._r8
         mat(k,2708) = 0._r8
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
         mat(k, 209) = mat(k, 209) - dti(k)
         mat(k, 213) = mat(k, 213) - dti(k)
         mat(k, 217) = mat(k, 217) - dti(k)
         mat(k, 220) = mat(k, 220) - dti(k)
         mat(k, 225) = mat(k, 225) - dti(k)
         mat(k, 232) = mat(k, 232) - dti(k)
         mat(k, 237) = mat(k, 237) - dti(k)
         mat(k, 241) = mat(k, 241) - dti(k)
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
         mat(k, 296) = mat(k, 296) - dti(k)
         mat(k, 300) = mat(k, 300) - dti(k)
         mat(k, 304) = mat(k, 304) - dti(k)
         mat(k, 308) = mat(k, 308) - dti(k)
         mat(k, 312) = mat(k, 312) - dti(k)
         mat(k, 319) = mat(k, 319) - dti(k)
         mat(k, 325) = mat(k, 325) - dti(k)
         mat(k, 328) = mat(k, 328) - dti(k)
         mat(k, 334) = mat(k, 334) - dti(k)
         mat(k, 337) = mat(k, 337) - dti(k)
         mat(k, 343) = mat(k, 343) - dti(k)
         mat(k, 349) = mat(k, 349) - dti(k)
         mat(k, 352) = mat(k, 352) - dti(k)
         mat(k, 357) = mat(k, 357) - dti(k)
         mat(k, 362) = mat(k, 362) - dti(k)
         mat(k, 367) = mat(k, 367) - dti(k)
         mat(k, 374) = mat(k, 374) - dti(k)
         mat(k, 380) = mat(k, 380) - dti(k)
         mat(k, 385) = mat(k, 385) - dti(k)
         mat(k, 390) = mat(k, 390) - dti(k)
         mat(k, 394) = mat(k, 394) - dti(k)
         mat(k, 401) = mat(k, 401) - dti(k)
         mat(k, 409) = mat(k, 409) - dti(k)
         mat(k, 417) = mat(k, 417) - dti(k)
         mat(k, 425) = mat(k, 425) - dti(k)
         mat(k, 428) = mat(k, 428) - dti(k)
         mat(k, 436) = mat(k, 436) - dti(k)
         mat(k, 444) = mat(k, 444) - dti(k)
         mat(k, 450) = mat(k, 450) - dti(k)
         mat(k, 456) = mat(k, 456) - dti(k)
         mat(k, 462) = mat(k, 462) - dti(k)
         mat(k, 468) = mat(k, 468) - dti(k)
         mat(k, 474) = mat(k, 474) - dti(k)
         mat(k, 480) = mat(k, 480) - dti(k)
         mat(k, 486) = mat(k, 486) - dti(k)
         mat(k, 492) = mat(k, 492) - dti(k)
         mat(k, 498) = mat(k, 498) - dti(k)
         mat(k, 504) = mat(k, 504) - dti(k)
         mat(k, 512) = mat(k, 512) - dti(k)
         mat(k, 518) = mat(k, 518) - dti(k)
         mat(k, 525) = mat(k, 525) - dti(k)
         mat(k, 531) = mat(k, 531) - dti(k)
         mat(k, 536) = mat(k, 536) - dti(k)
         mat(k, 539) = mat(k, 539) - dti(k)
         mat(k, 543) = mat(k, 543) - dti(k)
         mat(k, 547) = mat(k, 547) - dti(k)
         mat(k, 550) = mat(k, 550) - dti(k)
         mat(k, 557) = mat(k, 557) - dti(k)
         mat(k, 566) = mat(k, 566) - dti(k)
         mat(k, 574) = mat(k, 574) - dti(k)
         mat(k, 582) = mat(k, 582) - dti(k)
         mat(k, 590) = mat(k, 590) - dti(k)
         mat(k, 597) = mat(k, 597) - dti(k)
         mat(k, 602) = mat(k, 602) - dti(k)
         mat(k, 608) = mat(k, 608) - dti(k)
         mat(k, 614) = mat(k, 614) - dti(k)
         mat(k, 620) = mat(k, 620) - dti(k)
         mat(k, 623) = mat(k, 623) - dti(k)
         mat(k, 631) = mat(k, 631) - dti(k)
         mat(k, 639) = mat(k, 639) - dti(k)
         mat(k, 647) = mat(k, 647) - dti(k)
         mat(k, 655) = mat(k, 655) - dti(k)
         mat(k, 663) = mat(k, 663) - dti(k)
         mat(k, 672) = mat(k, 672) - dti(k)
         mat(k, 679) = mat(k, 679) - dti(k)
         mat(k, 683) = mat(k, 683) - dti(k)
         mat(k, 692) = mat(k, 692) - dti(k)
         mat(k, 699) = mat(k, 699) - dti(k)
         mat(k, 708) = mat(k, 708) - dti(k)
         mat(k, 716) = mat(k, 716) - dti(k)
         mat(k, 723) = mat(k, 723) - dti(k)
         mat(k, 733) = mat(k, 733) - dti(k)
         mat(k, 746) = mat(k, 746) - dti(k)
         mat(k, 757) = mat(k, 757) - dti(k)
         mat(k, 768) = mat(k, 768) - dti(k)
         mat(k, 775) = mat(k, 775) - dti(k)
         mat(k, 784) = mat(k, 784) - dti(k)
         mat(k, 797) = mat(k, 797) - dti(k)
         mat(k, 804) = mat(k, 804) - dti(k)
         mat(k, 815) = mat(k, 815) - dti(k)
         mat(k, 826) = mat(k, 826) - dti(k)
         mat(k, 839) = mat(k, 839) - dti(k)
         mat(k, 850) = mat(k, 850) - dti(k)
         mat(k, 859) = mat(k, 859) - dti(k)
         mat(k, 868) = mat(k, 868) - dti(k)
         mat(k, 873) = mat(k, 873) - dti(k)
         mat(k, 883) = mat(k, 883) - dti(k)
         mat(k, 889) = mat(k, 889) - dti(k)
         mat(k, 899) = mat(k, 899) - dti(k)
         mat(k, 909) = mat(k, 909) - dti(k)
         mat(k, 918) = mat(k, 918) - dti(k)
         mat(k, 936) = mat(k, 936) - dti(k)
         mat(k, 947) = mat(k, 947) - dti(k)
         mat(k, 956) = mat(k, 956) - dti(k)
         mat(k, 974) = mat(k, 974) - dti(k)
         mat(k, 998) = mat(k, 998) - dti(k)
         mat(k,1009) = mat(k,1009) - dti(k)
         mat(k,1019) = mat(k,1019) - dti(k)
         mat(k,1025) = mat(k,1025) - dti(k)
         mat(k,1038) = mat(k,1038) - dti(k)
         mat(k,1045) = mat(k,1045) - dti(k)
         mat(k,1053) = mat(k,1053) - dti(k)
         mat(k,1071) = mat(k,1071) - dti(k)
         mat(k,1091) = mat(k,1091) - dti(k)
         mat(k,1100) = mat(k,1100) - dti(k)
         mat(k,1116) = mat(k,1116) - dti(k)
         mat(k,1137) = mat(k,1137) - dti(k)
         mat(k,1149) = mat(k,1149) - dti(k)
         mat(k,1160) = mat(k,1160) - dti(k)
         mat(k,1170) = mat(k,1170) - dti(k)
         mat(k,1184) = mat(k,1184) - dti(k)
         mat(k,1195) = mat(k,1195) - dti(k)
         mat(k,1204) = mat(k,1204) - dti(k)
         mat(k,1217) = mat(k,1217) - dti(k)
         mat(k,1231) = mat(k,1231) - dti(k)
         mat(k,1252) = mat(k,1252) - dti(k)
         mat(k,1268) = mat(k,1268) - dti(k)
         mat(k,1280) = mat(k,1280) - dti(k)
         mat(k,1300) = mat(k,1300) - dti(k)
         mat(k,1320) = mat(k,1320) - dti(k)
         mat(k,1336) = mat(k,1336) - dti(k)
         mat(k,1348) = mat(k,1348) - dti(k)
         mat(k,1368) = mat(k,1368) - dti(k)
         mat(k,1401) = mat(k,1401) - dti(k)
         mat(k,1425) = mat(k,1425) - dti(k)
         mat(k,1446) = mat(k,1446) - dti(k)
         mat(k,1468) = mat(k,1468) - dti(k)
         mat(k,1500) = mat(k,1500) - dti(k)
         mat(k,1516) = mat(k,1516) - dti(k)
         mat(k,1530) = mat(k,1530) - dti(k)
         mat(k,1543) = mat(k,1543) - dti(k)
         mat(k,1558) = mat(k,1558) - dti(k)
         mat(k,1576) = mat(k,1576) - dti(k)
         mat(k,1594) = mat(k,1594) - dti(k)
         mat(k,1648) = mat(k,1648) - dti(k)
         mat(k,1672) = mat(k,1672) - dti(k)
         mat(k,1697) = mat(k,1697) - dti(k)
         mat(k,1724) = mat(k,1724) - dti(k)
         mat(k,1749) = mat(k,1749) - dti(k)
         mat(k,1861) = mat(k,1861) - dti(k)
         mat(k,1917) = mat(k,1917) - dti(k)
         mat(k,1941) = mat(k,1941) - dti(k)
         mat(k,1988) = mat(k,1988) - dti(k)
         mat(k,2171) = mat(k,2171) - dti(k)
         mat(k,2207) = mat(k,2207) - dti(k)
         mat(k,2239) = mat(k,2239) - dti(k)
         mat(k,2265) = mat(k,2265) - dti(k)
         mat(k,2295) = mat(k,2295) - dti(k)
         mat(k,2324) = mat(k,2324) - dti(k)
         mat(k,2395) = mat(k,2395) - dti(k)
         mat(k,2424) = mat(k,2424) - dti(k)
         mat(k,2452) = mat(k,2452) - dti(k)
         mat(k,2510) = mat(k,2510) - dti(k)
         mat(k,2577) = mat(k,2577) - dti(k)
         mat(k,2679) = mat(k,2679) - dti(k)
         mat(k,2709) = mat(k,2709) - dti(k)
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
