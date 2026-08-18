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
         mat(k,752) = -(rxt(k,453)*y(k,254))
         mat(k,2100) = -rxt(k,453)*y(k,1)
         mat(k,2510) = rxt(k,456)*y(k,225)
         mat(k,1020) = rxt(k,456)*y(k,157)
         mat(k,741) = -(rxt(k,457)*y(k,254))
         mat(k,2099) = -rxt(k,457)*y(k,2)
         mat(k,1019) = rxt(k,454)*y(k,239)
         mat(k,1858) = rxt(k,454)*y(k,225)
         mat(k,988) = -(rxt(k,536)*y(k,159) + rxt(k,537)*y(k,168) + rxt(k,538) &
                      *y(k,254))
         mat(k,2647) = -rxt(k,536)*y(k,6)
         mat(k,2282) = -rxt(k,537)*y(k,6)
         mat(k,2121) = -rxt(k,538)*y(k,6)
         mat(k,189) = -(rxt(k,495)*y(k,254))
         mat(k,2021) = -rxt(k,495)*y(k,7)
         mat(k,506) = -(rxt(k,498)*y(k,254))
         mat(k,2068) = -rxt(k,498)*y(k,8)
         mat(k,586) = rxt(k,496)*y(k,239)
         mat(k,1841) = rxt(k,496)*y(k,227)
         mat(k,190) = .120_r8*rxt(k,495)*y(k,254)
         mat(k,2022) = .120_r8*rxt(k,495)*y(k,7)
         mat(k,986) = .100_r8*rxt(k,537)*y(k,168)
         mat(k,1091) = .100_r8*rxt(k,540)*y(k,168)
         mat(k,2272) = .100_r8*rxt(k,537)*y(k,6) + .100_r8*rxt(k,540)*y(k,143)
         mat(k,2497) = .500_r8*rxt(k,497)*y(k,227) + .200_r8*rxt(k,524)*y(k,260) &
                      + .060_r8*rxt(k,530)*y(k,263)
         mat(k,587) = .500_r8*rxt(k,497)*y(k,157)
         mat(k,833) = .200_r8*rxt(k,524)*y(k,157)
         mat(k,849) = .060_r8*rxt(k,530)*y(k,157)
         mat(k,2491) = .200_r8*rxt(k,524)*y(k,260) + .200_r8*rxt(k,530)*y(k,263)
         mat(k,832) = .200_r8*rxt(k,524)*y(k,157)
         mat(k,847) = .200_r8*rxt(k,530)*y(k,157)
         mat(k,2506) = .200_r8*rxt(k,524)*y(k,260) + .150_r8*rxt(k,530)*y(k,263)
         mat(k,835) = .200_r8*rxt(k,524)*y(k,157)
         mat(k,850) = .150_r8*rxt(k,530)*y(k,157)
         mat(k,2492) = .210_r8*rxt(k,530)*y(k,263)
         mat(k,848) = .210_r8*rxt(k,530)*y(k,157)
         mat(k,282) = -(rxt(k,458)*y(k,254))
         mat(k,2037) = -rxt(k,458)*y(k,15)
         mat(k,985) = .050_r8*rxt(k,537)*y(k,168)
         mat(k,1090) = .050_r8*rxt(k,540)*y(k,168)
         mat(k,2270) = .050_r8*rxt(k,537)*y(k,6) + .050_r8*rxt(k,540)*y(k,143)
         mat(k,423) = -(rxt(k,424)*y(k,159) + rxt(k,425)*y(k,254))
         mat(k,2639) = -rxt(k,424)*y(k,16)
         mat(k,2058) = -rxt(k,425)*y(k,16)
         mat(k,2230) = -(rxt(k,243)*y(k,51) + rxt(k,244)*y(k,239) + rxt(k,245) &
                      *y(k,158) + rxt(k,246)*y(k,168) + rxt(k,253)*y(k,22) + rxt(k,282) &
                      *y(k,127))
         mat(k,1720) = -rxt(k,243)*y(k,17)
         mat(k,1917) = -rxt(k,244)*y(k,17)
         mat(k,2436) = -rxt(k,245)*y(k,17)
         mat(k,2322) = -rxt(k,246)*y(k,17)
         mat(k,926) = -rxt(k,253)*y(k,17)
         mat(k,2202) = -rxt(k,282)*y(k,17)
         mat(k,563) = rxt(k,242)*y(k,254)
         mat(k,2464) = 4.000_r8*rxt(k,247)*y(k,21) + (rxt(k,248)+rxt(k,249))*y(k,74) &
                      + rxt(k,559)*y(k,83) + rxt(k,272)*y(k,116) + (rxt(k,283) &
                       +rxt(k,284))*y(k,127) + rxt(k,252)*y(k,157) + rxt(k,257) &
                      *y(k,167) + rxt(k,570)*y(k,185) + rxt(k,258)*y(k,254)
         mat(k,171) = rxt(k,232)*y(k,253)
         mat(k,177) = rxt(k,262)*y(k,253)
         mat(k,568) = 2.000_r8*rxt(k,319)*y(k,70) + 2.000_r8*rxt(k,346)*y(k,253) &
                      + 2.000_r8*rxt(k,320)*y(k,254)
         mat(k,143) = rxt(k,321)*y(k,254)
         mat(k,685) = rxt(k,324)*y(k,70) + rxt(k,347)*y(k,253) + rxt(k,325)*y(k,254)
         mat(k,151) = 2.000_r8*rxt(k,331)*y(k,254)
         mat(k,516) = 3.000_r8*rxt(k,332)*y(k,70) + 3.000_r8*rxt(k,263)*y(k,253) &
                      + 3.000_r8*rxt(k,333)*y(k,254)
         mat(k,123) = rxt(k,334)*y(k,254)
         mat(k,2625) = 2.000_r8*rxt(k,319)*y(k,45) + rxt(k,324)*y(k,52) &
                      + 3.000_r8*rxt(k,332)*y(k,66)
         mat(k,2351) = (rxt(k,248)+rxt(k,249))*y(k,21)
         mat(k,1040) = rxt(k,559)*y(k,21)
         mat(k,128) = 2.000_r8*rxt(k,264)*y(k,253)
         mat(k,1537) = rxt(k,259)*y(k,167) + rxt(k,265)*y(k,253) + rxt(k,260)*y(k,254)
         mat(k,2379) = rxt(k,272)*y(k,21)
         mat(k,2202) = mat(k,2202) + (rxt(k,283)+rxt(k,284))*y(k,21)
         mat(k,2564) = rxt(k,252)*y(k,21)
         mat(k,1802) = rxt(k,257)*y(k,21) + rxt(k,259)*y(k,98)
         mat(k,1576) = rxt(k,570)*y(k,21)
         mat(k,1988) = rxt(k,232)*y(k,38) + rxt(k,262)*y(k,39) + 2.000_r8*rxt(k,346) &
                      *y(k,45) + rxt(k,347)*y(k,52) + 3.000_r8*rxt(k,263)*y(k,66) &
                      + 2.000_r8*rxt(k,264)*y(k,94) + rxt(k,265)*y(k,98)
         mat(k,2172) = rxt(k,242)*y(k,18) + rxt(k,258)*y(k,21) + 2.000_r8*rxt(k,320) &
                      *y(k,45) + rxt(k,321)*y(k,46) + rxt(k,325)*y(k,52) &
                      + 2.000_r8*rxt(k,331)*y(k,65) + 3.000_r8*rxt(k,333)*y(k,66) &
                      + rxt(k,334)*y(k,67) + rxt(k,260)*y(k,98)
         mat(k,560) = -(rxt(k,242)*y(k,254))
         mat(k,2075) = -rxt(k,242)*y(k,18)
         mat(k,2214) = rxt(k,253)*y(k,22)
         mat(k,919) = rxt(k,253)*y(k,17)
         mat(k,1528) = (rxt(k,587)+rxt(k,661)+rxt(k,674)+rxt(k,683))*y(k,109)
         mat(k,1604) = (rxt(k,587)+rxt(k,661)+rxt(k,674)+rxt(k,683))*y(k,98)
         mat(k,2448) = rxt(k,250)*y(k,74)
         mat(k,920) = rxt(k,254)*y(k,70)
         mat(k,2588) = rxt(k,254)*y(k,22)
         mat(k,2337) = rxt(k,250)*y(k,21)
         mat(k,1529) = (rxt(k,586)+rxt(k,663)+rxt(k,671)+rxt(k,680))*y(k,110)
         mat(k,1757) = (rxt(k,589)+rxt(k,660)+rxt(k,673)+rxt(k,682))*y(k,109)
         mat(k,1605) = (rxt(k,589)+rxt(k,660)+rxt(k,673)+rxt(k,682))*y(k,102)
         mat(k,1733) = (rxt(k,586)+rxt(k,663)+rxt(k,671)+rxt(k,680))*y(k,98)
         mat(k,2213) = rxt(k,245)*y(k,158)
         mat(k,2393) = rxt(k,245)*y(k,17)
         mat(k,2470) = -(4._r8*rxt(k,247)*y(k,21) + (rxt(k,248) + rxt(k,249) + rxt(k,250) &
                      ) * y(k,74) + rxt(k,251)*y(k,239) + rxt(k,252)*y(k,157) &
                      + rxt(k,255)*y(k,158) + rxt(k,257)*y(k,167) + rxt(k,258) &
                      *y(k,254) + rxt(k,272)*y(k,116) + (rxt(k,283) + rxt(k,284) &
                      ) * y(k,127) + rxt(k,559)*y(k,83) + rxt(k,570)*y(k,185))
         mat(k,2357) = -(rxt(k,248) + rxt(k,249) + rxt(k,250)) * y(k,21)
         mat(k,1923) = -rxt(k,251)*y(k,21)
         mat(k,2570) = -rxt(k,252)*y(k,21)
         mat(k,2442) = -rxt(k,255)*y(k,21)
         mat(k,1808) = -rxt(k,257)*y(k,21)
         mat(k,2178) = -rxt(k,258)*y(k,21)
         mat(k,2385) = -rxt(k,272)*y(k,21)
         mat(k,2208) = -(rxt(k,283) + rxt(k,284)) * y(k,21)
         mat(k,1042) = -rxt(k,559)*y(k,21)
         mat(k,1581) = -rxt(k,570)*y(k,21)
         mat(k,2236) = rxt(k,282)*y(k,127) + rxt(k,246)*y(k,168)
         mat(k,928) = rxt(k,256)*y(k,167)
         mat(k,1540) = rxt(k,266)*y(k,253)
         mat(k,1622) = rxt(k,261)*y(k,167)
         mat(k,2208) = mat(k,2208) + rxt(k,282)*y(k,17)
         mat(k,1808) = mat(k,1808) + rxt(k,256)*y(k,22) + rxt(k,261)*y(k,109)
         mat(k,2328) = rxt(k,246)*y(k,17)
         mat(k,1994) = rxt(k,266)*y(k,98)
         mat(k,921) = -(rxt(k,253)*y(k,17) + rxt(k,254)*y(k,70) + rxt(k,256)*y(k,167))
         mat(k,2216) = -rxt(k,253)*y(k,22)
         mat(k,2595) = -rxt(k,254)*y(k,22)
         mat(k,1781) = -rxt(k,256)*y(k,22)
         mat(k,2450) = rxt(k,255)*y(k,158)
         mat(k,2410) = rxt(k,255)*y(k,21)
         mat(k,285) = -(rxt(k,499)*y(k,254))
         mat(k,2038) = -rxt(k,499)*y(k,24)
         mat(k,2488) = rxt(k,502)*y(k,229)
         mat(k,524) = rxt(k,502)*y(k,157)
         mat(k,386) = -(rxt(k,501)*y(k,254))
         mat(k,2051) = -rxt(k,501)*y(k,25)
         mat(k,525) = rxt(k,500)*y(k,239)
         mat(k,1831) = rxt(k,500)*y(k,229)
         mat(k,219) = -(rxt(k,315)*y(k,70) + rxt(k,316)*y(k,254))
         mat(k,2575) = -rxt(k,315)*y(k,26)
         mat(k,2025) = -rxt(k,316)*y(k,26)
         mat(k,329) = -(rxt(k,372)*y(k,70) + rxt(k,373)*y(k,254))
         mat(k,2578) = -rxt(k,372)*y(k,27)
         mat(k,2044) = -rxt(k,373)*y(k,27)
         mat(k,630) = -(rxt(k,374)*y(k,70) + rxt(k,375)*y(k,168) + rxt(k,400)*y(k,254))
         mat(k,2590) = -rxt(k,374)*y(k,28)
         mat(k,2275) = -rxt(k,375)*y(k,28)
         mat(k,2085) = -rxt(k,400)*y(k,28)
         mat(k,291) = -(rxt(k,317)*y(k,70) + rxt(k,318)*y(k,254))
         mat(k,2577) = -rxt(k,317)*y(k,29)
         mat(k,2040) = -rxt(k,318)*y(k,29)
         mat(k,296) = -(rxt(k,380)*y(k,254))
         mat(k,2041) = -rxt(k,380)*y(k,30)
         mat(k,886) = .800_r8*rxt(k,376)*y(k,230) + .200_r8*rxt(k,377)*y(k,234)
         mat(k,1626) = .200_r8*rxt(k,377)*y(k,230)
         mat(k,399) = -(rxt(k,381)*y(k,254))
         mat(k,2054) = -rxt(k,381)*y(k,31)
         mat(k,887) = rxt(k,378)*y(k,239)
         mat(k,1833) = rxt(k,378)*y(k,230)
         mat(k,338) = -(rxt(k,382)*y(k,70) + rxt(k,383)*y(k,254))
         mat(k,2579) = -rxt(k,382)*y(k,32)
         mat(k,2045) = -rxt(k,383)*y(k,32)
         mat(k,1130) = -(rxt(k,403)*y(k,159) + rxt(k,404)*y(k,168) + rxt(k,422) &
                      *y(k,254))
         mat(k,2657) = -rxt(k,403)*y(k,33)
         mat(k,2290) = -rxt(k,404)*y(k,33)
         mat(k,2132) = -rxt(k,422)*y(k,33)
         mat(k,905) = .130_r8*rxt(k,482)*y(k,168)
         mat(k,2290) = mat(k,2290) + .130_r8*rxt(k,482)*y(k,129)
         mat(k,500) = -(rxt(k,408)*y(k,254))
         mat(k,2067) = -rxt(k,408)*y(k,34)
         mat(k,949) = rxt(k,406)*y(k,239)
         mat(k,1840) = rxt(k,406)*y(k,231)
         mat(k,344) = -(rxt(k,409)*y(k,254) + rxt(k,412)*y(k,70))
         mat(k,2046) = -rxt(k,409)*y(k,35)
         mat(k,2580) = -rxt(k,412)*y(k,35)
         mat(k,305) = -(rxt(k,505)*y(k,254))
         mat(k,2043) = -rxt(k,505)*y(k,36)
         mat(k,732) = rxt(k,503)*y(k,239)
         mat(k,1827) = rxt(k,503)*y(k,232)
         mat(k,115) = -(rxt(k,231)*y(k,253))
         mat(k,1953) = -rxt(k,231)*y(k,37)
         mat(k,168) = -(rxt(k,232)*y(k,253))
         mat(k,1958) = -rxt(k,232)*y(k,38)
         mat(k,173) = -(rxt(k,262)*y(k,253))
         mat(k,1959) = -rxt(k,262)*y(k,39)
         mat(k,129) = -(rxt(k,233)*y(k,253))
         mat(k,1955) = -rxt(k,233)*y(k,40)
         mat(k,178) = -(rxt(k,234)*y(k,253))
         mat(k,1960) = -rxt(k,234)*y(k,41)
         mat(k,133) = -(rxt(k,235)*y(k,253))
         mat(k,1956) = -rxt(k,235)*y(k,42)
         mat(k,183) = -(rxt(k,236)*y(k,253))
         mat(k,1961) = -rxt(k,236)*y(k,43)
         mat(k,137) = -(rxt(k,237)*y(k,253))
         mat(k,1957) = -rxt(k,237)*y(k,44)
         mat(k,564) = -(rxt(k,319)*y(k,70) + rxt(k,320)*y(k,254) + rxt(k,346)*y(k,253))
         mat(k,2587) = -rxt(k,319)*y(k,45)
         mat(k,2076) = -rxt(k,320)*y(k,45)
         mat(k,1970) = -rxt(k,346)*y(k,45)
         mat(k,141) = -(rxt(k,321)*y(k,254))
         mat(k,2017) = -rxt(k,321)*y(k,46)
         mat(k,357) = -(rxt(k,322)*y(k,70) + rxt(k,323)*y(k,254))
         mat(k,2581) = -rxt(k,322)*y(k,47)
         mat(k,2048) = -rxt(k,323)*y(k,47)
         mat(k,1711) = -(rxt(k,204)*y(k,70) + rxt(k,243)*y(k,17) + rxt(k,351)*y(k,239) &
                      + rxt(k,352)*y(k,159) + rxt(k,353)*y(k,167) + rxt(k,354) &
                      *y(k,254))
         mat(k,2616) = -rxt(k,204)*y(k,51)
         mat(k,2221) = -rxt(k,243)*y(k,51)
         mat(k,1908) = -rxt(k,351)*y(k,51)
         mat(k,2686) = -rxt(k,352)*y(k,51)
         mat(k,1793) = -rxt(k,353)*y(k,51)
         mat(k,2163) = -rxt(k,354)*y(k,51)
         mat(k,758) = .400_r8*rxt(k,453)*y(k,254)
         mat(k,1001) = .340_r8*rxt(k,537)*y(k,168)
         mat(k,427) = .500_r8*rxt(k,424)*y(k,159)
         mat(k,634) = rxt(k,375)*y(k,168)
         mat(k,1138) = .500_r8*rxt(k,404)*y(k,168)
         mat(k,723) = .500_r8*rxt(k,392)*y(k,254)
         mat(k,883) = rxt(k,359)*y(k,254)
         mat(k,472) = .300_r8*rxt(k,360)*y(k,254)
         mat(k,1592) = (rxt(k,368)+rxt(k,369))*y(k,253)
         mat(k,1070) = rxt(k,335)*y(k,234)
         mat(k,2342) = rxt(k,213)*y(k,234)
         mat(k,1176) = .800_r8*rxt(k,397)*y(k,254)
         mat(k,914) = .910_r8*rxt(k,482)*y(k,168)
         mat(k,714) = .300_r8*rxt(k,473)*y(k,254)
         mat(k,1355) = .120_r8*rxt(k,435)*y(k,168)
         mat(k,674) = .500_r8*rxt(k,448)*y(k,254)
         mat(k,1108) = .340_r8*rxt(k,540)*y(k,168)
         mat(k,1465) = .600_r8*rxt(k,449)*y(k,168)
         mat(k,2555) = .100_r8*rxt(k,455)*y(k,225) + rxt(k,358)*y(k,234) &
                      + .500_r8*rxt(k,426)*y(k,236) + .500_r8*rxt(k,394)*y(k,238) &
                      + .920_r8*rxt(k,465)*y(k,241) + .250_r8*rxt(k,433)*y(k,246) &
                      + rxt(k,442)*y(k,248) + rxt(k,416)*y(k,256) + rxt(k,420) &
                      *y(k,257) + .340_r8*rxt(k,549)*y(k,258) + .320_r8*rxt(k,554) &
                      *y(k,259) + .250_r8*rxt(k,490)*y(k,262)
         mat(k,2686) = mat(k,2686) + .500_r8*rxt(k,424)*y(k,16) + rxt(k,466)*y(k,241) &
                      + .250_r8*rxt(k,432)*y(k,246) + rxt(k,443)*y(k,248)
         mat(k,2313) = .340_r8*rxt(k,537)*y(k,6) + rxt(k,375)*y(k,28) &
                      + .500_r8*rxt(k,404)*y(k,33) + .910_r8*rxt(k,482)*y(k,129) &
                      + .120_r8*rxt(k,435)*y(k,138) + .340_r8*rxt(k,540)*y(k,143) &
                      + .600_r8*rxt(k,449)*y(k,144)
         mat(k,642) = rxt(k,399)*y(k,254)
         mat(k,1203) = .680_r8*rxt(k,558)*y(k,254)
         mat(k,1028) = .100_r8*rxt(k,455)*y(k,157)
         mat(k,892) = .700_r8*rxt(k,377)*y(k,234)
         mat(k,954) = rxt(k,405)*y(k,234)
         mat(k,1517) = rxt(k,388)*y(k,234) + rxt(k,462)*y(k,241) + .250_r8*rxt(k,429) &
                      *y(k,246) + rxt(k,438)*y(k,248) + .250_r8*rxt(k,487)*y(k,262)
         mat(k,1664) = rxt(k,335)*y(k,68) + rxt(k,213)*y(k,74) + rxt(k,358)*y(k,157) &
                      + .700_r8*rxt(k,377)*y(k,230) + rxt(k,405)*y(k,231) + rxt(k,388) &
                      *y(k,233) + (4.000_r8*rxt(k,355)+2.000_r8*rxt(k,356))*y(k,234) &
                      + 1.500_r8*rxt(k,463)*y(k,241) + .750_r8*rxt(k,468)*y(k,242) &
                      + .800_r8*rxt(k,477)*y(k,243) + .880_r8*rxt(k,430)*y(k,246) &
                      + 2.000_r8*rxt(k,439)*y(k,248) + .750_r8*rxt(k,542)*y(k,252) &
                      + .800_r8*rxt(k,418)*y(k,257) + .930_r8*rxt(k,547)*y(k,258) &
                      + .950_r8*rxt(k,552)*y(k,259) + .800_r8*rxt(k,488)*y(k,262)
         mat(k,658) = .500_r8*rxt(k,426)*y(k,157)
         mat(k,821) = .500_r8*rxt(k,394)*y(k,157)
         mat(k,1908) = mat(k,1908) + .450_r8*rxt(k,440)*y(k,248) + .150_r8*rxt(k,419) &
                      *y(k,257)
         mat(k,1388) = .920_r8*rxt(k,465)*y(k,157) + rxt(k,466)*y(k,159) + rxt(k,462) &
                      *y(k,233) + 1.500_r8*rxt(k,463)*y(k,234)
         mat(k,1421) = .750_r8*rxt(k,468)*y(k,234)
         mat(k,1340) = .800_r8*rxt(k,477)*y(k,234)
         mat(k,1443) = .250_r8*rxt(k,433)*y(k,157) + .250_r8*rxt(k,432)*y(k,159) &
                      + .250_r8*rxt(k,429)*y(k,233) + .880_r8*rxt(k,430)*y(k,234)
         mat(k,1485) = rxt(k,442)*y(k,157) + rxt(k,443)*y(k,159) + rxt(k,438)*y(k,233) &
                      + 2.000_r8*rxt(k,439)*y(k,234) + .450_r8*rxt(k,440)*y(k,239) &
                      + 4.000_r8*rxt(k,441)*y(k,248)
         mat(k,1189) = .750_r8*rxt(k,542)*y(k,234)
         mat(k,1979) = (rxt(k,368)+rxt(k,369))*y(k,64)
         mat(k,2163) = mat(k,2163) + .400_r8*rxt(k,453)*y(k,1) + .500_r8*rxt(k,392) &
                      *y(k,60) + rxt(k,359)*y(k,62) + .300_r8*rxt(k,360)*y(k,63) &
                      + .800_r8*rxt(k,397)*y(k,90) + .300_r8*rxt(k,473)*y(k,130) &
                      + .500_r8*rxt(k,448)*y(k,142) + rxt(k,399)*y(k,174) &
                      + .680_r8*rxt(k,558)*y(k,214)
         mat(k,876) = rxt(k,416)*y(k,157)
         mat(k,1301) = rxt(k,420)*y(k,157) + .800_r8*rxt(k,418)*y(k,234) &
                      + .150_r8*rxt(k,419)*y(k,239)
         mat(k,1249) = .340_r8*rxt(k,549)*y(k,157) + .930_r8*rxt(k,547)*y(k,234)
         mat(k,1270) = .320_r8*rxt(k,554)*y(k,157) + .950_r8*rxt(k,552)*y(k,234)
         mat(k,1318) = .250_r8*rxt(k,490)*y(k,157) + .250_r8*rxt(k,487)*y(k,233) &
                      + .800_r8*rxt(k,488)*y(k,234)
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
         mat(k,679) = -(rxt(k,324)*y(k,70) + rxt(k,325)*y(k,254) + rxt(k,347)*y(k,253))
         mat(k,2591) = -rxt(k,324)*y(k,52)
         mat(k,2091) = -rxt(k,325)*y(k,52)
         mat(k,1971) = -rxt(k,347)*y(k,52)
         mat(k,145) = -(rxt(k,326)*y(k,254))
         mat(k,2018) = -rxt(k,326)*y(k,53)
         mat(k,1163) = -(rxt(k,384)*y(k,159) + rxt(k,385)*y(k,254))
         mat(k,2659) = -rxt(k,384)*y(k,54)
         mat(k,2134) = -rxt(k,385)*y(k,54)
         mat(k,756) = .800_r8*rxt(k,453)*y(k,254)
         mat(k,426) = rxt(k,424)*y(k,159)
         mat(k,297) = rxt(k,380)*y(k,254)
         mat(k,401) = .500_r8*rxt(k,381)*y(k,254)
         mat(k,1131) = .500_r8*rxt(k,404)*y(k,168)
         mat(k,1455) = .100_r8*rxt(k,449)*y(k,168)
         mat(k,2531) = .400_r8*rxt(k,455)*y(k,225) + rxt(k,379)*y(k,230) &
                      + .270_r8*rxt(k,407)*y(k,231) + rxt(k,426)*y(k,236) + rxt(k,445) &
                      *y(k,250) + rxt(k,416)*y(k,256)
         mat(k,2659) = mat(k,2659) + rxt(k,424)*y(k,16)
         mat(k,2291) = .500_r8*rxt(k,404)*y(k,33) + .100_r8*rxt(k,449)*y(k,144)
         mat(k,1025) = .400_r8*rxt(k,455)*y(k,157)
         mat(k,890) = rxt(k,379)*y(k,157) + 3.200_r8*rxt(k,376)*y(k,230) &
                      + .800_r8*rxt(k,377)*y(k,234)
         mat(k,952) = .270_r8*rxt(k,407)*y(k,157)
         mat(k,1643) = .800_r8*rxt(k,377)*y(k,230)
         mat(k,656) = rxt(k,426)*y(k,157)
         mat(k,1881) = .200_r8*rxt(k,444)*y(k,250)
         mat(k,764) = rxt(k,445)*y(k,157) + .200_r8*rxt(k,444)*y(k,239)
         mat(k,2134) = mat(k,2134) + .800_r8*rxt(k,453)*y(k,1) + rxt(k,380)*y(k,30) &
                      + .500_r8*rxt(k,381)*y(k,31)
         mat(k,874) = rxt(k,416)*y(k,157)
         mat(k,450) = -(rxt(k,327)*y(k,70) + rxt(k,328)*y(k,254))
         mat(k,2585) = -rxt(k,327)*y(k,55)
         mat(k,2060) = -rxt(k,328)*y(k,55)
         mat(k,118) = -(rxt(k,386)*y(k,254))
         mat(k,2015) = -rxt(k,386)*y(k,56)
         mat(k,1078) = -(rxt(k,423)*y(k,254))
         mat(k,2129) = -rxt(k,423)*y(k,57)
         mat(k,755) = .800_r8*rxt(k,453)*y(k,254)
         mat(k,992) = .520_r8*rxt(k,537)*y(k,168)
         mat(k,425) = .500_r8*rxt(k,424)*y(k,159)
         mat(k,1096) = .520_r8*rxt(k,540)*y(k,168)
         mat(k,2528) = .250_r8*rxt(k,455)*y(k,225) + .820_r8*rxt(k,407)*y(k,231) &
                      + .500_r8*rxt(k,426)*y(k,236) + .270_r8*rxt(k,549)*y(k,258) &
                      + .040_r8*rxt(k,554)*y(k,259)
         mat(k,2654) = .500_r8*rxt(k,424)*y(k,16)
         mat(k,2287) = .520_r8*rxt(k,537)*y(k,6) + .520_r8*rxt(k,540)*y(k,143)
         mat(k,1197) = .500_r8*rxt(k,558)*y(k,254)
         mat(k,1024) = .250_r8*rxt(k,455)*y(k,157)
         mat(k,951) = .820_r8*rxt(k,407)*y(k,157) + .820_r8*rxt(k,405)*y(k,234)
         mat(k,1640) = .820_r8*rxt(k,405)*y(k,231) + .150_r8*rxt(k,547)*y(k,258) &
                      + .025_r8*rxt(k,552)*y(k,259)
         mat(k,655) = .500_r8*rxt(k,426)*y(k,157)
         mat(k,2129) = mat(k,2129) + .800_r8*rxt(k,453)*y(k,1) + .500_r8*rxt(k,558) &
                      *y(k,214)
         mat(k,1241) = .270_r8*rxt(k,549)*y(k,157) + .150_r8*rxt(k,547)*y(k,234)
         mat(k,1260) = .040_r8*rxt(k,554)*y(k,157) + .025_r8*rxt(k,552)*y(k,234)
         mat(k,1362) = -(rxt(k,410)*y(k,159) + rxt(k,411)*y(k,254))
         mat(k,2673) = -rxt(k,410)*y(k,58)
         mat(k,2148) = -rxt(k,411)*y(k,58)
         mat(k,1232) = rxt(k,413)*y(k,254)
         mat(k,1351) = .880_r8*rxt(k,435)*y(k,168)
         mat(k,1458) = .500_r8*rxt(k,449)*y(k,168)
         mat(k,2544) = .170_r8*rxt(k,508)*y(k,235) + .050_r8*rxt(k,471)*y(k,242) &
                      + .250_r8*rxt(k,433)*y(k,246) + .170_r8*rxt(k,514)*y(k,249) &
                      + .400_r8*rxt(k,524)*y(k,260) + .250_r8*rxt(k,490)*y(k,262) &
                      + .540_r8*rxt(k,530)*y(k,263) + .510_r8*rxt(k,533)*y(k,265)
         mat(k,2673) = mat(k,2673) + .050_r8*rxt(k,472)*y(k,242) + .250_r8*rxt(k,432) &
                      *y(k,246) + .250_r8*rxt(k,491)*y(k,262)
         mat(k,932) = rxt(k,414)*y(k,254)
         mat(k,2302) = .880_r8*rxt(k,435)*y(k,138) + .500_r8*rxt(k,449)*y(k,144)
         mat(k,1508) = .250_r8*rxt(k,429)*y(k,246) + .250_r8*rxt(k,487)*y(k,262)
         mat(k,1655) = .240_r8*rxt(k,430)*y(k,246) + .500_r8*rxt(k,418)*y(k,257) &
                      + .100_r8*rxt(k,488)*y(k,262)
         mat(k,866) = .170_r8*rxt(k,508)*y(k,157) + .070_r8*rxt(k,507)*y(k,239)
         mat(k,1894) = .070_r8*rxt(k,507)*y(k,235) + .070_r8*rxt(k,513)*y(k,249)
         mat(k,1414) = .050_r8*rxt(k,471)*y(k,157) + .050_r8*rxt(k,472)*y(k,159)
         mat(k,1438) = .250_r8*rxt(k,433)*y(k,157) + .250_r8*rxt(k,432)*y(k,159) &
                      + .250_r8*rxt(k,429)*y(k,233) + .240_r8*rxt(k,430)*y(k,234)
         mat(k,973) = .170_r8*rxt(k,514)*y(k,157) + .070_r8*rxt(k,513)*y(k,239)
         mat(k,2148) = mat(k,2148) + rxt(k,413)*y(k,114) + rxt(k,414)*y(k,160)
         mat(k,1298) = .500_r8*rxt(k,418)*y(k,234)
         mat(k,842) = .400_r8*rxt(k,524)*y(k,157)
         mat(k,1315) = .250_r8*rxt(k,490)*y(k,157) + .250_r8*rxt(k,491)*y(k,159) &
                      + .250_r8*rxt(k,487)*y(k,233) + .100_r8*rxt(k,488)*y(k,234)
         mat(k,858) = .540_r8*rxt(k,530)*y(k,157)
         mat(k,598) = .510_r8*rxt(k,533)*y(k,157)
         mat(k,780) = -(rxt(k,391)*y(k,254))
         mat(k,2103) = -rxt(k,391)*y(k,59)
         mat(k,1125) = .120_r8*rxt(k,404)*y(k,168)
         mat(k,2277) = .120_r8*rxt(k,404)*y(k,33)
         mat(k,1498) = .100_r8*rxt(k,388)*y(k,234) + .150_r8*rxt(k,389)*y(k,239)
         mat(k,1632) = .100_r8*rxt(k,388)*y(k,233)
         mat(k,1861) = .150_r8*rxt(k,389)*y(k,233) + .150_r8*rxt(k,440)*y(k,248)
         mat(k,1477) = .150_r8*rxt(k,440)*y(k,239)
         mat(k,719) = -(rxt(k,392)*y(k,254))
         mat(k,2096) = -rxt(k,392)*y(k,60)
         mat(k,1497) = .400_r8*rxt(k,389)*y(k,239)
         mat(k,1856) = .400_r8*rxt(k,389)*y(k,233) + .400_r8*rxt(k,440)*y(k,248)
         mat(k,1476) = .400_r8*rxt(k,440)*y(k,239)
         mat(k,415) = -(rxt(k,329)*y(k,70) + rxt(k,330)*y(k,254))
         mat(k,2584) = -rxt(k,329)*y(k,61)
         mat(k,2057) = -rxt(k,330)*y(k,61)
         mat(k,882) = -(rxt(k,359)*y(k,254))
         mat(k,2113) = -rxt(k,359)*y(k,62)
         mat(k,888) = .300_r8*rxt(k,377)*y(k,234)
         mat(k,1633) = .300_r8*rxt(k,377)*y(k,230) + 2.000_r8*rxt(k,356)*y(k,234) &
                      + .250_r8*rxt(k,463)*y(k,241) + .250_r8*rxt(k,468)*y(k,242) &
                      + .200_r8*rxt(k,477)*y(k,243) + .250_r8*rxt(k,430)*y(k,246) &
                      + .250_r8*rxt(k,542)*y(k,252) + .500_r8*rxt(k,418)*y(k,257) &
                      + .250_r8*rxt(k,547)*y(k,258) + .250_r8*rxt(k,552)*y(k,259) &
                      + .300_r8*rxt(k,488)*y(k,262)
         mat(k,1372) = .250_r8*rxt(k,463)*y(k,234)
         mat(k,1403) = .250_r8*rxt(k,468)*y(k,234)
         mat(k,1328) = .200_r8*rxt(k,477)*y(k,234)
         mat(k,1432) = .250_r8*rxt(k,430)*y(k,234)
         mat(k,1182) = .250_r8*rxt(k,542)*y(k,234)
         mat(k,1295) = .500_r8*rxt(k,418)*y(k,234)
         mat(k,1239) = .250_r8*rxt(k,547)*y(k,234)
         mat(k,1259) = .250_r8*rxt(k,552)*y(k,234)
         mat(k,1308) = .300_r8*rxt(k,488)*y(k,234)
         mat(k,470) = -(rxt(k,360)*y(k,254))
         mat(k,2063) = -rxt(k,360)*y(k,63)
         mat(k,1630) = rxt(k,357)*y(k,239)
         mat(k,1836) = rxt(k,357)*y(k,234)
         mat(k,1590) = -(rxt(k,205)*y(k,70) + rxt(k,309)*y(k,89) + rxt(k,361)*y(k,254) &
                      + (rxt(k,367) + rxt(k,368) + rxt(k,369)) * y(k,253))
         mat(k,2612) = -rxt(k,205)*y(k,64)
         mat(k,963) = -rxt(k,309)*y(k,64)
         mat(k,2159) = -rxt(k,361)*y(k,64)
         mat(k,1975) = -(rxt(k,367) + rxt(k,368) + rxt(k,369)) * y(k,64)
         mat(k,1136) = .100_r8*rxt(k,404)*y(k,168)
         mat(k,2310) = .100_r8*rxt(k,404)*y(k,33)
         mat(k,149) = -(rxt(k,331)*y(k,254))
         mat(k,2019) = -rxt(k,331)*y(k,65)
         mat(k,512) = -(rxt(k,263)*y(k,253) + rxt(k,332)*y(k,70) + rxt(k,333)*y(k,254))
         mat(k,1969) = -rxt(k,263)*y(k,66)
         mat(k,2586) = -rxt(k,332)*y(k,66)
         mat(k,2069) = -rxt(k,333)*y(k,66)
         mat(k,121) = -(rxt(k,334)*y(k,254))
         mat(k,2016) = -rxt(k,334)*y(k,67)
         mat(k,1067) = -((rxt(k,335) + rxt(k,336)) * y(k,234) + (rxt(k,337) + rxt(k,338) &
                      ) * y(k,239) + rxt(k,339)*y(k,157) + rxt(k,340)*y(k,159))
         mat(k,1639) = -(rxt(k,335) + rxt(k,336)) * y(k,68)
         mat(k,1878) = -(rxt(k,337) + rxt(k,338)) * y(k,68)
         mat(k,2527) = -rxt(k,339)*y(k,68)
         mat(k,2653) = -rxt(k,340)*y(k,68)
         mat(k,358) = rxt(k,322)*y(k,70) + rxt(k,323)*y(k,254)
         mat(k,2602) = rxt(k,322)*y(k,47)
         mat(k,2128) = rxt(k,323)*y(k,47)
         mat(k,408) = -(rxt(k,341)*y(k,70) + rxt(k,342)*y(k,254))
         mat(k,2583) = -rxt(k,341)*y(k,69)
         mat(k,2056) = -rxt(k,342)*y(k,69)
         mat(k,2633) = -(rxt(k,204)*y(k,51) + rxt(k,205)*y(k,64) + rxt(k,206)*y(k,93) &
                      + rxt(k,207)*y(k,95) + (rxt(k,208) + rxt(k,209)) * y(k,239) &
                      + rxt(k,210)*y(k,158) + rxt(k,212)*y(k,168) + rxt(k,219)*y(k,75) &
                      + rxt(k,228)*y(k,110) + rxt(k,254)*y(k,22) + rxt(k,315)*y(k,26) &
                      + rxt(k,317)*y(k,29) + rxt(k,319)*y(k,45) + rxt(k,322)*y(k,47) &
                      + rxt(k,324)*y(k,52) + rxt(k,327)*y(k,55) + rxt(k,329)*y(k,61) &
                      + rxt(k,332)*y(k,66) + rxt(k,382)*y(k,32) + rxt(k,412)*y(k,35) &
                      + (rxt(k,560) + rxt(k,561)) * y(k,83))
         mat(k,1728) = -rxt(k,204)*y(k,70)
         mat(k,1600) = -rxt(k,205)*y(k,70)
         mat(k,1554) = -rxt(k,206)*y(k,70)
         mat(k,704) = -rxt(k,207)*y(k,70)
         mat(k,1925) = -(rxt(k,208) + rxt(k,209)) * y(k,70)
         mat(k,2444) = -rxt(k,210)*y(k,70)
         mat(k,2330) = -rxt(k,212)*y(k,70)
         mat(k,1016) = -rxt(k,219)*y(k,70)
         mat(k,1752) = -rxt(k,228)*y(k,70)
         mat(k,929) = -rxt(k,254)*y(k,70)
         mat(k,222) = -rxt(k,315)*y(k,70)
         mat(k,294) = -rxt(k,317)*y(k,70)
         mat(k,569) = -rxt(k,319)*y(k,70)
         mat(k,361) = -rxt(k,322)*y(k,70)
         mat(k,686) = -rxt(k,324)*y(k,70)
         mat(k,456) = -rxt(k,327)*y(k,70)
         mat(k,421) = -rxt(k,329)*y(k,70)
         mat(k,517) = -rxt(k,332)*y(k,70)
         mat(k,342) = -rxt(k,382)*y(k,70)
         mat(k,348) = -rxt(k,412)*y(k,70)
         mat(k,1043) = -(rxt(k,560) + rxt(k,561)) * y(k,70)
         mat(k,2472) = rxt(k,249)*y(k,74)
         mat(k,222) = mat(k,222) + 5.000_r8*rxt(k,315)*y(k,70) + 3.060_r8*rxt(k,316) &
                      *y(k,254)
         mat(k,294) = mat(k,294) + 2.000_r8*rxt(k,317)*y(k,70) + 2.000_r8*rxt(k,318) &
                      *y(k,254)
         mat(k,117) = 4.000_r8*rxt(k,231)*y(k,253)
         mat(k,172) = rxt(k,232)*y(k,253)
         mat(k,132) = 2.000_r8*rxt(k,233)*y(k,253)
         mat(k,182) = 2.000_r8*rxt(k,234)*y(k,253)
         mat(k,136) = 2.000_r8*rxt(k,235)*y(k,253)
         mat(k,187) = rxt(k,236)*y(k,253)
         mat(k,140) = 2.000_r8*rxt(k,237)*y(k,253)
         mat(k,144) = rxt(k,321)*y(k,254)
         mat(k,147) = 3.000_r8*rxt(k,326)*y(k,254)
         mat(k,456) = mat(k,456) + rxt(k,328)*y(k,254)
         mat(k,152) = rxt(k,331)*y(k,254)
         mat(k,124) = 2.000_r8*rxt(k,334)*y(k,254)
         mat(k,1075) = 2.000_r8*rxt(k,339)*y(k,157) + 2.000_r8*rxt(k,340)*y(k,159) &
                      + 2.000_r8*rxt(k,335)*y(k,234) + rxt(k,338)*y(k,239)
         mat(k,413) = rxt(k,342)*y(k,254)
         mat(k,2633) = mat(k,2633) + 5.000_r8*rxt(k,315)*y(k,26) + 2.000_r8*rxt(k,317) &
                      *y(k,29)
         mat(k,2359) = rxt(k,249)*y(k,21) + (4.000_r8*rxt(k,214)+2.000_r8*rxt(k,216)) &
                      *y(k,74) + rxt(k,286)*y(k,127) + rxt(k,218)*y(k,157) &
                      + rxt(k,223)*y(k,167) + rxt(k,571)*y(k,185) + rxt(k,213) &
                      *y(k,234) + rxt(k,224)*y(k,254)
         mat(k,268) = rxt(k,314)*y(k,253)
         mat(k,264) = rxt(k,348)*y(k,253) + rxt(k,343)*y(k,254)
         mat(k,273) = rxt(k,349)*y(k,253) + rxt(k,344)*y(k,254)
         mat(k,355) = rxt(k,350)*y(k,253) + rxt(k,345)*y(k,254)
         mat(k,1775) = rxt(k,226)*y(k,167) + rxt(k,238)*y(k,253) + rxt(k,227)*y(k,254)
         mat(k,2210) = rxt(k,286)*y(k,74)
         mat(k,2572) = 2.000_r8*rxt(k,339)*y(k,68) + rxt(k,218)*y(k,74)
         mat(k,2703) = 2.000_r8*rxt(k,340)*y(k,68)
         mat(k,1810) = rxt(k,223)*y(k,74) + rxt(k,226)*y(k,102)
         mat(k,1583) = rxt(k,571)*y(k,74)
         mat(k,1677) = 2.000_r8*rxt(k,335)*y(k,68) + rxt(k,213)*y(k,74)
         mat(k,1925) = mat(k,1925) + rxt(k,338)*y(k,68)
         mat(k,1996) = 4.000_r8*rxt(k,231)*y(k,37) + rxt(k,232)*y(k,38) &
                      + 2.000_r8*rxt(k,233)*y(k,40) + 2.000_r8*rxt(k,234)*y(k,41) &
                      + 2.000_r8*rxt(k,235)*y(k,42) + rxt(k,236)*y(k,43) &
                      + 2.000_r8*rxt(k,237)*y(k,44) + rxt(k,314)*y(k,81) + rxt(k,348) &
                      *y(k,99) + rxt(k,349)*y(k,100) + rxt(k,350)*y(k,101) &
                      + rxt(k,238)*y(k,102)
         mat(k,2180) = 3.060_r8*rxt(k,316)*y(k,26) + 2.000_r8*rxt(k,318)*y(k,29) &
                      + rxt(k,321)*y(k,46) + 3.000_r8*rxt(k,326)*y(k,53) + rxt(k,328) &
                      *y(k,55) + rxt(k,331)*y(k,65) + 2.000_r8*rxt(k,334)*y(k,67) &
                      + rxt(k,342)*y(k,69) + rxt(k,224)*y(k,74) + rxt(k,343)*y(k,99) &
                      + rxt(k,344)*y(k,100) + rxt(k,345)*y(k,101) + rxt(k,227) &
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
         mat(k,2576) = rxt(k,219)*y(k,75)
         mat(k,2334) = 2.000_r8*rxt(k,215)*y(k,74)
         mat(k,1007) = rxt(k,219)*y(k,70) + (rxt(k,669)+rxt(k,678)+rxt(k,687)) &
                      *y(k,102)
         mat(k,1755) = (rxt(k,669)+rxt(k,678)+rxt(k,687))*y(k,75) + (rxt(k,588) &
                       +rxt(k,659)+rxt(k,670)+rxt(k,679))*y(k,110)
         mat(k,1731) = (rxt(k,588)+rxt(k,659)+rxt(k,670)+rxt(k,679))*y(k,102)
         mat(k,2333) = 2.000_r8*rxt(k,240)*y(k,74)
         mat(k,625) = -(rxt(k,211)*y(k,254))
         mat(k,2084) = -rxt(k,211)*y(k,73)
         mat(k,2589) = rxt(k,210)*y(k,158)
         mat(k,1758) = rxt(k,604)*y(k,147)
         mat(k,435) = rxt(k,604)*y(k,102)
         mat(k,2403) = rxt(k,210)*y(k,70)
         mat(k,2354) = -(rxt(k,213)*y(k,234) + (4._r8*rxt(k,214) + 4._r8*rxt(k,215) &
                      + 4._r8*rxt(k,216) + 4._r8*rxt(k,240)) * y(k,74) + rxt(k,217) &
                      *y(k,239) + rxt(k,218)*y(k,157) + rxt(k,220)*y(k,158) + rxt(k,223) &
                      *y(k,167) + (rxt(k,224) + rxt(k,225)) * y(k,254) + (rxt(k,248) &
                      + rxt(k,249) + rxt(k,250)) * y(k,21) + (rxt(k,285) + rxt(k,286) &
                      + rxt(k,287)) * y(k,127) + rxt(k,571)*y(k,185))
         mat(k,1674) = -rxt(k,213)*y(k,74)
         mat(k,1920) = -rxt(k,217)*y(k,74)
         mat(k,2567) = -rxt(k,218)*y(k,74)
         mat(k,2439) = -rxt(k,220)*y(k,74)
         mat(k,1805) = -rxt(k,223)*y(k,74)
         mat(k,2175) = -(rxt(k,224) + rxt(k,225)) * y(k,74)
         mat(k,2467) = -(rxt(k,248) + rxt(k,249) + rxt(k,250)) * y(k,74)
         mat(k,2205) = -(rxt(k,285) + rxt(k,286) + rxt(k,287)) * y(k,74)
         mat(k,1579) = -rxt(k,571)*y(k,74)
         mat(k,2628) = rxt(k,228)*y(k,110) + rxt(k,212)*y(k,168) + rxt(k,209)*y(k,239)
         mat(k,1014) = rxt(k,221)*y(k,167)
         mat(k,1771) = rxt(k,239)*y(k,253)
         mat(k,1748) = rxt(k,228)*y(k,70) + rxt(k,229)*y(k,167) + rxt(k,230)*y(k,254)
         mat(k,1805) = mat(k,1805) + rxt(k,221)*y(k,75) + rxt(k,229)*y(k,110)
         mat(k,2325) = rxt(k,212)*y(k,70)
         mat(k,549) = rxt(k,576)*y(k,185)
         mat(k,1579) = mat(k,1579) + rxt(k,576)*y(k,170)
         mat(k,1920) = mat(k,1920) + rxt(k,209)*y(k,70)
         mat(k,1991) = rxt(k,239)*y(k,102)
         mat(k,2175) = mat(k,2175) + rxt(k,230)*y(k,110)
         mat(k,1008) = -(rxt(k,219)*y(k,70) + rxt(k,221)*y(k,167) + rxt(k,222) &
                      *y(k,254) + (rxt(k,669) + rxt(k,678) + rxt(k,687)) * y(k,102))
         mat(k,2597) = -rxt(k,219)*y(k,75)
         mat(k,1782) = -rxt(k,221)*y(k,75)
         mat(k,2122) = -rxt(k,222)*y(k,75)
         mat(k,1759) = -(rxt(k,669) + rxt(k,678) + rxt(k,687)) * y(k,75)
         mat(k,2338) = rxt(k,220)*y(k,158)
         mat(k,2413) = rxt(k,220)*y(k,74)
         mat(k,1209) = -(rxt(k,371)*y(k,254))
         mat(k,2138) = -rxt(k,371)*y(k,77)
         mat(k,996) = .230_r8*rxt(k,537)*y(k,168)
         mat(k,2217) = rxt(k,243)*y(k,51)
         mat(k,332) = .350_r8*rxt(k,373)*y(k,254)
         mat(k,633) = .630_r8*rxt(k,375)*y(k,168)
         mat(k,1132) = .560_r8*rxt(k,404)*y(k,168)
         mat(k,1704) = rxt(k,243)*y(k,17) + rxt(k,204)*y(k,70) + rxt(k,352)*y(k,159) &
                      + rxt(k,353)*y(k,167) + rxt(k,354)*y(k,254)
         mat(k,451) = rxt(k,327)*y(k,70)
         mat(k,1361) = rxt(k,410)*y(k,159) + rxt(k,411)*y(k,254)
         mat(k,1068) = rxt(k,339)*y(k,157) + rxt(k,340)*y(k,159) + (rxt(k,335) &
                       +rxt(k,336))*y(k,234) + rxt(k,338)*y(k,239)
         mat(k,2606) = rxt(k,204)*y(k,51) + rxt(k,327)*y(k,55)
         mat(k,1062) = rxt(k,398)*y(k,254)
         mat(k,906) = .620_r8*rxt(k,482)*y(k,168)
         mat(k,1349) = .650_r8*rxt(k,435)*y(k,168)
         mat(k,1102) = .230_r8*rxt(k,540)*y(k,168)
         mat(k,1456) = .560_r8*rxt(k,449)*y(k,168)
         mat(k,2535) = rxt(k,339)*y(k,68) + .170_r8*rxt(k,508)*y(k,235) &
                      + .220_r8*rxt(k,433)*y(k,246) + .400_r8*rxt(k,511)*y(k,247) &
                      + .350_r8*rxt(k,514)*y(k,249) + .225_r8*rxt(k,549)*y(k,258) &
                      + .250_r8*rxt(k,490)*y(k,262)
         mat(k,2663) = rxt(k,352)*y(k,51) + rxt(k,410)*y(k,58) + rxt(k,340)*y(k,68) &
                      + .220_r8*rxt(k,432)*y(k,246) + .500_r8*rxt(k,491)*y(k,262)
         mat(k,1784) = rxt(k,353)*y(k,51) + rxt(k,565)*y(k,171)
         mat(k,2295) = .230_r8*rxt(k,537)*y(k,6) + .630_r8*rxt(k,375)*y(k,28) &
                      + .560_r8*rxt(k,404)*y(k,33) + .620_r8*rxt(k,482)*y(k,129) &
                      + .650_r8*rxt(k,435)*y(k,138) + .230_r8*rxt(k,540)*y(k,143) &
                      + .560_r8*rxt(k,449)*y(k,144)
         mat(k,445) = rxt(k,565)*y(k,167) + rxt(k,566)*y(k,254)
         mat(k,1199) = .700_r8*rxt(k,558)*y(k,254)
         mat(k,1503) = .220_r8*rxt(k,429)*y(k,246) + .250_r8*rxt(k,487)*y(k,262)
         mat(k,1647) = (rxt(k,335)+rxt(k,336))*y(k,68) + .110_r8*rxt(k,430)*y(k,246) &
                      + .125_r8*rxt(k,547)*y(k,258) + .200_r8*rxt(k,488)*y(k,262)
         mat(k,865) = .170_r8*rxt(k,508)*y(k,157) + .070_r8*rxt(k,507)*y(k,239)
         mat(k,1885) = rxt(k,338)*y(k,68) + .070_r8*rxt(k,507)*y(k,235) &
                      + .160_r8*rxt(k,510)*y(k,247) + .140_r8*rxt(k,513)*y(k,249)
         mat(k,1434) = .220_r8*rxt(k,433)*y(k,157) + .220_r8*rxt(k,432)*y(k,159) &
                      + .220_r8*rxt(k,429)*y(k,233) + .110_r8*rxt(k,430)*y(k,234)
         mat(k,828) = .400_r8*rxt(k,511)*y(k,157) + .160_r8*rxt(k,510)*y(k,239)
         mat(k,972) = .350_r8*rxt(k,514)*y(k,157) + .140_r8*rxt(k,513)*y(k,239)
         mat(k,2138) = mat(k,2138) + .350_r8*rxt(k,373)*y(k,27) + rxt(k,354)*y(k,51) &
                      + rxt(k,411)*y(k,58) + rxt(k,398)*y(k,91) + rxt(k,566)*y(k,171) &
                      + .700_r8*rxt(k,558)*y(k,214)
         mat(k,1244) = .225_r8*rxt(k,549)*y(k,157) + .125_r8*rxt(k,547)*y(k,234)
         mat(k,1312) = .250_r8*rxt(k,490)*y(k,157) + .500_r8*rxt(k,491)*y(k,159) &
                      + .250_r8*rxt(k,487)*y(k,233) + .200_r8*rxt(k,488)*y(k,234)
         mat(k,987) = .270_r8*rxt(k,537)*y(k,168)
         mat(k,1127) = .200_r8*rxt(k,404)*y(k,168)
         mat(k,781) = rxt(k,391)*y(k,254)
         mat(k,720) = .500_r8*rxt(k,392)*y(k,254)
         mat(k,1208) = rxt(k,371)*y(k,254)
         mat(k,1172) = .800_r8*rxt(k,397)*y(k,254)
         mat(k,1060) = rxt(k,398)*y(k,254)
         mat(k,1045) = rxt(k,363)*y(k,254)
         mat(k,671) = .500_r8*rxt(k,448)*y(k,254)
         mat(k,1092) = .270_r8*rxt(k,540)*y(k,168)
         mat(k,1452) = .100_r8*rxt(k,449)*y(k,168)
         mat(k,2520) = rxt(k,390)*y(k,233) + .900_r8*rxt(k,549)*y(k,258)
         mat(k,2280) = .270_r8*rxt(k,537)*y(k,6) + .200_r8*rxt(k,404)*y(k,33) &
                      + .270_r8*rxt(k,540)*y(k,143) + .100_r8*rxt(k,449)*y(k,144)
         mat(k,1196) = 1.800_r8*rxt(k,558)*y(k,254)
         mat(k,1499) = rxt(k,390)*y(k,157) + 4.000_r8*rxt(k,387)*y(k,233) &
                      + .900_r8*rxt(k,388)*y(k,234) + rxt(k,462)*y(k,241) &
                      + 2.000_r8*rxt(k,438)*y(k,248) + rxt(k,487)*y(k,262)
         mat(k,1635) = .900_r8*rxt(k,388)*y(k,233) + rxt(k,439)*y(k,248) &
                      + .500_r8*rxt(k,547)*y(k,258)
         mat(k,1871) = .450_r8*rxt(k,440)*y(k,248)
         mat(k,1373) = rxt(k,462)*y(k,233)
         mat(k,1478) = 2.000_r8*rxt(k,438)*y(k,233) + rxt(k,439)*y(k,234) &
                      + .450_r8*rxt(k,440)*y(k,239) + 4.000_r8*rxt(k,441)*y(k,248)
         mat(k,2115) = rxt(k,391)*y(k,59) + .500_r8*rxt(k,392)*y(k,60) + rxt(k,371) &
                      *y(k,77) + .800_r8*rxt(k,397)*y(k,90) + rxt(k,398)*y(k,91) &
                      + rxt(k,363)*y(k,104) + .500_r8*rxt(k,448)*y(k,142) &
                      + 1.800_r8*rxt(k,558)*y(k,214)
         mat(k,1240) = .900_r8*rxt(k,549)*y(k,157) + .500_r8*rxt(k,547)*y(k,234)
         mat(k,1309) = rxt(k,487)*y(k,233)
         mat(k,220) = .470_r8*rxt(k,316)*y(k,254)
         mat(k,1066) = rxt(k,336)*y(k,234) + rxt(k,337)*y(k,239)
         mat(k,407) = rxt(k,341)*y(k,70) + rxt(k,342)*y(k,254)
         mat(k,2582) = rxt(k,341)*y(k,69)
         mat(k,1628) = rxt(k,336)*y(k,68)
         mat(k,1834) = rxt(k,337)*y(k,68)
         mat(k,2055) = .470_r8*rxt(k,316)*y(k,26) + rxt(k,342)*y(k,69)
         mat(k,274) = -(rxt(k,313)*y(k,253))
         mat(k,1967) = -rxt(k,313)*y(k,80)
         mat(k,169) = rxt(k,232)*y(k,253)
         mat(k,174) = rxt(k,262)*y(k,253)
         mat(k,180) = rxt(k,234)*y(k,253)
         mat(k,134) = 2.000_r8*rxt(k,235)*y(k,253)
         mat(k,184) = 2.000_r8*rxt(k,236)*y(k,253)
         mat(k,138) = rxt(k,237)*y(k,253)
         mat(k,126) = 2.000_r8*rxt(k,264)*y(k,253)
         mat(k,270) = rxt(k,349)*y(k,253) + rxt(k,344)*y(k,254)
         mat(k,350) = rxt(k,350)*y(k,253) + rxt(k,345)*y(k,254)
         mat(k,1967) = mat(k,1967) + rxt(k,232)*y(k,38) + rxt(k,262)*y(k,39) &
                      + rxt(k,234)*y(k,41) + 2.000_r8*rxt(k,235)*y(k,42) &
                      + 2.000_r8*rxt(k,236)*y(k,43) + rxt(k,237)*y(k,44) &
                      + 2.000_r8*rxt(k,264)*y(k,94) + rxt(k,349)*y(k,100) + rxt(k,350) &
                      *y(k,101)
         mat(k,2035) = rxt(k,344)*y(k,100) + rxt(k,345)*y(k,101)
         mat(k,265) = -(rxt(k,314)*y(k,253))
         mat(k,1965) = -rxt(k,314)*y(k,81)
         mat(k,130) = rxt(k,233)*y(k,253)
         mat(k,179) = rxt(k,234)*y(k,253)
         mat(k,261) = rxt(k,348)*y(k,253) + rxt(k,343)*y(k,254)
         mat(k,1965) = mat(k,1965) + rxt(k,233)*y(k,40) + rxt(k,234)*y(k,41) &
                      + rxt(k,348)*y(k,99)
         mat(k,2033) = rxt(k,343)*y(k,99)
         mat(k,233) = -(rxt(k,506)*y(k,254))
         mat(k,2027) = -rxt(k,506)*y(k,82)
         mat(k,227) = .180_r8*rxt(k,526)*y(k,254)
         mat(k,2027) = mat(k,2027) + .180_r8*rxt(k,526)*y(k,216)
         mat(k,1033) = -(rxt(k,559)*y(k,21) + (rxt(k,560) + rxt(k,561)) * y(k,70) &
                      + rxt(k,562)*y(k,127) + rxt(k,563)*y(k,159) + (rxt(k,564) &
                      + rxt(k,578)) * y(k,254))
         mat(k,2451) = -rxt(k,559)*y(k,83)
         mat(k,2598) = -(rxt(k,560) + rxt(k,561)) * y(k,83)
         mat(k,2189) = -rxt(k,562)*y(k,83)
         mat(k,2649) = -rxt(k,563)*y(k,83)
         mat(k,2124) = -(rxt(k,564) + rxt(k,578)) * y(k,83)
         mat(k,817) = rxt(k,393)*y(k,239)
         mat(k,1825) = rxt(k,393)*y(k,238)
         mat(k,961) = -(rxt(k,309)*y(k,64) + rxt(k,310)*y(k,93) + rxt(k,311)*y(k,266) &
                      + rxt(k,312)*y(k,107))
         mat(k,1587) = -rxt(k,309)*y(k,89)
         mat(k,1544) = -rxt(k,310)*y(k,89)
         mat(k,2711) = -rxt(k,311)*y(k,89)
         mat(k,1929) = -rxt(k,312)*y(k,89)
         mat(k,175) = rxt(k,262)*y(k,253)
         mat(k,185) = rxt(k,236)*y(k,253)
         mat(k,275) = 2.000_r8*rxt(k,313)*y(k,253)
         mat(k,266) = rxt(k,314)*y(k,253)
         mat(k,1972) = rxt(k,262)*y(k,39) + rxt(k,236)*y(k,43) + 2.000_r8*rxt(k,313) &
                      *y(k,80) + rxt(k,314)*y(k,81)
         mat(k,1174) = -(rxt(k,397)*y(k,254))
         mat(k,2135) = -rxt(k,397)*y(k,90)
         mat(k,711) = .700_r8*rxt(k,473)*y(k,254)
         mat(k,664) = .500_r8*rxt(k,474)*y(k,254)
         mat(k,466) = rxt(k,485)*y(k,254)
         mat(k,2532) = .050_r8*rxt(k,471)*y(k,242) + .530_r8*rxt(k,433)*y(k,246) &
                      + .225_r8*rxt(k,549)*y(k,258) + .250_r8*rxt(k,490)*y(k,262)
         mat(k,2660) = .050_r8*rxt(k,472)*y(k,242) + .530_r8*rxt(k,432)*y(k,246) &
                      + .250_r8*rxt(k,491)*y(k,262)
         mat(k,1502) = .530_r8*rxt(k,429)*y(k,246) + .250_r8*rxt(k,487)*y(k,262)
         mat(k,1644) = .260_r8*rxt(k,430)*y(k,246) + .125_r8*rxt(k,547)*y(k,258) &
                      + .100_r8*rxt(k,488)*y(k,262)
         mat(k,1407) = .050_r8*rxt(k,471)*y(k,157) + .050_r8*rxt(k,472)*y(k,159)
         mat(k,1433) = .530_r8*rxt(k,433)*y(k,157) + .530_r8*rxt(k,432)*y(k,159) &
                      + .530_r8*rxt(k,429)*y(k,233) + .260_r8*rxt(k,430)*y(k,234)
         mat(k,2135) = mat(k,2135) + .700_r8*rxt(k,473)*y(k,130) + .500_r8*rxt(k,474) &
                      *y(k,131) + rxt(k,485)*y(k,148)
         mat(k,1242) = .225_r8*rxt(k,549)*y(k,157) + .125_r8*rxt(k,547)*y(k,234)
         mat(k,1311) = .250_r8*rxt(k,490)*y(k,157) + .250_r8*rxt(k,491)*y(k,159) &
                      + .250_r8*rxt(k,487)*y(k,233) + .100_r8*rxt(k,488)*y(k,234)
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
         mat(k,1061) = -(rxt(k,398)*y(k,254))
         mat(k,2127) = -rxt(k,398)*y(k,91)
         mat(k,331) = .650_r8*rxt(k,373)*y(k,254)
         mat(k,1173) = .200_r8*rxt(k,397)*y(k,254)
         mat(k,1150) = rxt(k,486)*y(k,254)
         mat(k,2526) = rxt(k,497)*y(k,227) + .050_r8*rxt(k,471)*y(k,242) &
                      + .400_r8*rxt(k,511)*y(k,247) + .170_r8*rxt(k,514)*y(k,249) &
                      + .700_r8*rxt(k,517)*y(k,255) + .600_r8*rxt(k,524)*y(k,260) &
                      + .250_r8*rxt(k,490)*y(k,262) + .340_r8*rxt(k,530)*y(k,263) &
                      + .170_r8*rxt(k,533)*y(k,265)
         mat(k,2652) = .050_r8*rxt(k,472)*y(k,242) + .250_r8*rxt(k,491)*y(k,262)
         mat(k,590) = rxt(k,497)*y(k,157)
         mat(k,1500) = .250_r8*rxt(k,487)*y(k,262)
         mat(k,1638) = .100_r8*rxt(k,488)*y(k,262)
         mat(k,1877) = .160_r8*rxt(k,510)*y(k,247) + .070_r8*rxt(k,513)*y(k,249)
         mat(k,1406) = .050_r8*rxt(k,471)*y(k,157) + .050_r8*rxt(k,472)*y(k,159)
         mat(k,827) = .400_r8*rxt(k,511)*y(k,157) + .160_r8*rxt(k,510)*y(k,239)
         mat(k,971) = .170_r8*rxt(k,514)*y(k,157) + .070_r8*rxt(k,513)*y(k,239)
         mat(k,2127) = mat(k,2127) + .650_r8*rxt(k,373)*y(k,27) + .200_r8*rxt(k,397) &
                      *y(k,90) + rxt(k,486)*y(k,149)
         mat(k,540) = .700_r8*rxt(k,517)*y(k,157)
         mat(k,840) = .600_r8*rxt(k,524)*y(k,157)
         mat(k,1310) = .250_r8*rxt(k,490)*y(k,157) + .250_r8*rxt(k,491)*y(k,159) &
                      + .250_r8*rxt(k,487)*y(k,233) + .100_r8*rxt(k,488)*y(k,234)
         mat(k,856) = .340_r8*rxt(k,530)*y(k,157)
         mat(k,597) = .170_r8*rxt(k,533)*y(k,157)
         mat(k,2254) = -((rxt(k,164) + rxt(k,165) + rxt(k,166)) * y(k,239) + rxt(k,170) &
                      *y(k,168))
         mat(k,1918) = -(rxt(k,164) + rxt(k,165) + rxt(k,166)) * y(k,92)
         mat(k,2323) = -rxt(k,170)*y(k,92)
         mat(k,1721) = rxt(k,354)*y(k,254)
         mat(k,1599) = rxt(k,368)*y(k,253)
         mat(k,2626) = rxt(k,206)*y(k,93)
         mat(k,967) = rxt(k,310)*y(k,93)
         mat(k,1553) = rxt(k,206)*y(k,70) + rxt(k,310)*y(k,89) + rxt(k,162)*y(k,167) &
                      + rxt(k,153)*y(k,253) + rxt(k,171)*y(k,254)
         mat(k,1538) = rxt(k,266)*y(k,253)
         mat(k,1770) = rxt(k,239)*y(k,253)
         mat(k,576) = rxt(k,192)*y(k,254)
         mat(k,1803) = rxt(k,162)*y(k,93) + rxt(k,174)*y(k,254)
         mat(k,449) = rxt(k,566)*y(k,254)
         mat(k,615) = rxt(k,572)*y(k,254)
         mat(k,1577) = rxt(k,577)*y(k,254)
         mat(k,1989) = rxt(k,368)*y(k,64) + rxt(k,153)*y(k,93) + rxt(k,266)*y(k,98) &
                      + rxt(k,239)*y(k,102)
         mat(k,2173) = rxt(k,354)*y(k,51) + rxt(k,171)*y(k,93) + rxt(k,192)*y(k,145) &
                      + rxt(k,174)*y(k,167) + rxt(k,566)*y(k,171) + rxt(k,572) &
                      *y(k,183) + rxt(k,577)*y(k,185)
         mat(k,1545) = -(rxt(k,153)*y(k,253) + rxt(k,162)*y(k,167) + rxt(k,171) &
                      *y(k,254) + rxt(k,206)*y(k,70) + rxt(k,310)*y(k,89))
         mat(k,1974) = -rxt(k,153)*y(k,93)
         mat(k,1786) = -rxt(k,162)*y(k,93)
         mat(k,2156) = -rxt(k,171)*y(k,93)
         mat(k,2610) = -rxt(k,206)*y(k,93)
         mat(k,962) = -rxt(k,310)*y(k,93)
         mat(k,1589) = rxt(k,369)*y(k,253)
         mat(k,2241) = rxt(k,164)*y(k,239)
         mat(k,1902) = rxt(k,164)*y(k,92)
         mat(k,1974) = mat(k,1974) + rxt(k,369)*y(k,64)
         mat(k,125) = -(rxt(k,264)*y(k,253))
         mat(k,1954) = -rxt(k,264)*y(k,94)
         mat(k,699) = -(rxt(k,163)*y(k,167) + rxt(k,172)*y(k,254) + rxt(k,207)*y(k,70))
         mat(k,1780) = -rxt(k,163)*y(k,95)
         mat(k,2093) = -rxt(k,172)*y(k,95)
         mat(k,2592) = -rxt(k,207)*y(k,95)
         mat(k,1854) = 2.000_r8*rxt(k,178)*y(k,239)
         mat(k,2093) = mat(k,2093) + 2.000_r8*rxt(k,177)*y(k,254)
         mat(k,300) = rxt(k,579)*y(k,266)
         mat(k,2707) = rxt(k,579)*y(k,187)
         mat(k,208) = rxt(k,298)*y(k,266)
         mat(k,785) = rxt(k,299)*y(k,254)
         mat(k,1999) = rxt(k,299)*y(k,172)
         mat(k,2706) = rxt(k,298)*y(k,121)
         mat(k,1530) = -(rxt(k,259)*y(k,167) + rxt(k,260)*y(k,254) + (rxt(k,265) &
                      + rxt(k,266)) * y(k,253) + (rxt(k,586) + rxt(k,663) + rxt(k,671) &
                      + rxt(k,680)) * y(k,110) + (rxt(k,587) + rxt(k,661) + rxt(k,674) &
                      + rxt(k,683)) * y(k,109) + (rxt(k,594) + rxt(k,690) + rxt(k,694) &
                      + rxt(k,698)) * y(k,111))
         mat(k,1785) = -rxt(k,259)*y(k,98)
         mat(k,2155) = -rxt(k,260)*y(k,98)
         mat(k,1973) = -(rxt(k,265) + rxt(k,266)) * y(k,98)
         mat(k,1735) = -(rxt(k,586) + rxt(k,663) + rxt(k,671) + rxt(k,680)) * y(k,98)
         mat(k,1607) = -(rxt(k,587) + rxt(k,661) + rxt(k,674) + rxt(k,683)) * y(k,98)
         mat(k,1683) = -(rxt(k,594) + rxt(k,690) + rxt(k,694) + rxt(k,698)) * y(k,98)
         mat(k,2218) = rxt(k,243)*y(k,51) + rxt(k,244)*y(k,239)
         mat(k,1705) = rxt(k,243)*y(k,17)
         mat(k,1901) = rxt(k,244)*y(k,17)
         mat(k,260) = -(rxt(k,343)*y(k,254) + rxt(k,348)*y(k,253))
         mat(k,2032) = -rxt(k,343)*y(k,99)
         mat(k,1964) = -rxt(k,348)*y(k,99)
         mat(k,269) = -(rxt(k,344)*y(k,254) + rxt(k,349)*y(k,253))
         mat(k,2034) = -rxt(k,344)*y(k,100)
         mat(k,1966) = -rxt(k,349)*y(k,100)
         mat(k,351) = -(rxt(k,345)*y(k,254) + rxt(k,350)*y(k,253))
         mat(k,2047) = -rxt(k,345)*y(k,101)
         mat(k,1968) = -rxt(k,350)*y(k,101)
         mat(k,1763) = -(rxt(k,226)*y(k,167) + rxt(k,227)*y(k,254) + (rxt(k,238) &
                      + rxt(k,239)) * y(k,253) + (rxt(k,588) + rxt(k,659) + rxt(k,670) &
                      + rxt(k,679)) * y(k,110) + (rxt(k,589) + rxt(k,660) + rxt(k,673) &
                      + rxt(k,682)) * y(k,109) + (rxt(k,593) + rxt(k,689) + rxt(k,693) &
                      + rxt(k,697)) * y(k,111) + rxt(k,604)*y(k,147) + (rxt(k,669) &
                      + rxt(k,678) + rxt(k,687)) * y(k,75))
         mat(k,1795) = -rxt(k,226)*y(k,102)
         mat(k,2165) = -rxt(k,227)*y(k,102)
         mat(k,1981) = -(rxt(k,238) + rxt(k,239)) * y(k,102)
         mat(k,1740) = -(rxt(k,588) + rxt(k,659) + rxt(k,670) + rxt(k,679)) * y(k,102)
         mat(k,1612) = -(rxt(k,589) + rxt(k,660) + rxt(k,673) + rxt(k,682)) * y(k,102)
         mat(k,1688) = -(rxt(k,593) + rxt(k,689) + rxt(k,693) + rxt(k,697)) * y(k,102)
         mat(k,436) = -rxt(k,604)*y(k,102)
         mat(k,1010) = -(rxt(k,669) + rxt(k,678) + rxt(k,687)) * y(k,102)
         mat(k,292) = rxt(k,317)*y(k,70)
         mat(k,340) = rxt(k,382)*y(k,70)
         mat(k,346) = rxt(k,412)*y(k,70)
         mat(k,565) = rxt(k,319)*y(k,70)
         mat(k,359) = rxt(k,322)*y(k,70)
         mat(k,1713) = rxt(k,204)*y(k,70)
         mat(k,681) = rxt(k,324)*y(k,70)
         mat(k,453) = 2.000_r8*rxt(k,327)*y(k,70)
         mat(k,417) = rxt(k,329)*y(k,70)
         mat(k,1593) = rxt(k,205)*y(k,70)
         mat(k,513) = rxt(k,332)*y(k,70)
         mat(k,411) = rxt(k,341)*y(k,70)
         mat(k,2618) = rxt(k,317)*y(k,29) + rxt(k,382)*y(k,32) + rxt(k,412)*y(k,35) &
                      + rxt(k,319)*y(k,45) + rxt(k,322)*y(k,47) + rxt(k,204)*y(k,51) &
                      + rxt(k,324)*y(k,52) + 2.000_r8*rxt(k,327)*y(k,55) + rxt(k,329) &
                      *y(k,61) + rxt(k,205)*y(k,64) + rxt(k,332)*y(k,66) + rxt(k,341) &
                      *y(k,69) + rxt(k,561)*y(k,83) + rxt(k,206)*y(k,93) + rxt(k,207) &
                      *y(k,95) + rxt(k,228)*y(k,110) + rxt(k,208)*y(k,239)
         mat(k,2344) = rxt(k,225)*y(k,254)
         mat(k,1035) = rxt(k,561)*y(k,70)
         mat(k,1548) = rxt(k,206)*y(k,70)
         mat(k,700) = rxt(k,207)*y(k,70)
         mat(k,1740) = mat(k,1740) + rxt(k,228)*y(k,70)
         mat(k,1910) = rxt(k,208)*y(k,70)
         mat(k,2165) = mat(k,2165) + rxt(k,225)*y(k,74)
         mat(k,204) = -(rxt(k,362)*y(k,254) + rxt(k,370)*y(k,253))
         mat(k,2024) = -rxt(k,362)*y(k,103)
         mat(k,1962) = -rxt(k,370)*y(k,103)
         mat(k,1046) = -(rxt(k,363)*y(k,254))
         mat(k,2125) = -rxt(k,363)*y(k,104)
         mat(k,990) = .050_r8*rxt(k,537)*y(k,168)
         mat(k,330) = .350_r8*rxt(k,373)*y(k,254)
         mat(k,632) = .370_r8*rxt(k,375)*y(k,168)
         mat(k,1129) = .120_r8*rxt(k,404)*y(k,168)
         mat(k,904) = .110_r8*rxt(k,482)*y(k,168)
         mat(k,1348) = .330_r8*rxt(k,435)*y(k,168)
         mat(k,1094) = .050_r8*rxt(k,540)*y(k,168)
         mat(k,1453) = .120_r8*rxt(k,449)*y(k,168)
         mat(k,2525) = rxt(k,366)*y(k,240)
         mat(k,2284) = .050_r8*rxt(k,537)*y(k,6) + .370_r8*rxt(k,375)*y(k,28) &
                      + .120_r8*rxt(k,404)*y(k,33) + .110_r8*rxt(k,482)*y(k,129) &
                      + .330_r8*rxt(k,435)*y(k,138) + .050_r8*rxt(k,540)*y(k,143) &
                      + .120_r8*rxt(k,449)*y(k,144)
         mat(k,1876) = rxt(k,364)*y(k,240)
         mat(k,533) = rxt(k,366)*y(k,157) + rxt(k,364)*y(k,239)
         mat(k,2125) = mat(k,2125) + .350_r8*rxt(k,373)*y(k,27)
         mat(k,1585) = rxt(k,309)*y(k,89)
         mat(k,960) = rxt(k,309)*y(k,64) + rxt(k,310)*y(k,93) + rxt(k,312)*y(k,107) &
                      + rxt(k,311)*y(k,266)
         mat(k,1543) = rxt(k,310)*y(k,89)
         mat(k,1928) = rxt(k,312)*y(k,89)
         mat(k,2710) = rxt(k,311)*y(k,89)
         mat(k,1281) = -(rxt(k,267)*y(k,159) + rxt(k,295)*y(k,254) + (rxt(k,590) &
                      + rxt(k,664) + rxt(k,672) + rxt(k,681)) * y(k,110) + (rxt(k,591) &
                      + rxt(k,662) + rxt(k,675) + rxt(k,684)) * y(k,109) + (rxt(k,595) &
                      + rxt(k,691) + rxt(k,695) + rxt(k,699)) * y(k,111))
         mat(k,2668) = -rxt(k,267)*y(k,106)
         mat(k,2143) = -rxt(k,295)*y(k,106)
         mat(k,1734) = -(rxt(k,590) + rxt(k,664) + rxt(k,672) + rxt(k,681)) * y(k,106)
         mat(k,1606) = -(rxt(k,591) + rxt(k,662) + rxt(k,675) + rxt(k,684)) * y(k,106)
         mat(k,1682) = -(rxt(k,595) + rxt(k,691) + rxt(k,695) + rxt(k,699)) * y(k,106)
         mat(k,2367) = rxt(k,273)*y(k,239)
         mat(k,1889) = rxt(k,273)*y(k,116)
         mat(k,1938) = -(rxt(k,201)*y(k,254) + rxt(k,312)*y(k,89))
         mat(k,2168) = -rxt(k,201)*y(k,107)
         mat(k,965) = -rxt(k,312)*y(k,107)
         mat(k,1716) = rxt(k,352)*y(k,159)
         mat(k,1168) = rxt(k,384)*y(k,159)
         mat(k,1366) = rxt(k,410)*y(k,159)
         mat(k,1012) = (rxt(k,669)+rxt(k,678)+rxt(k,687))*y(k,102)
         mat(k,1037) = rxt(k,563)*y(k,159)
         mat(k,1765) = (rxt(k,669)+rxt(k,678)+rxt(k,687))*y(k,75) + rxt(k,604) &
                      *y(k,147)
         mat(k,1286) = rxt(k,267)*y(k,159)
         mat(k,1690) = rxt(k,300)*y(k,159)
         mat(k,438) = rxt(k,604)*y(k,102)
         mat(k,2432) = rxt(k,200)*y(k,254)
         mat(k,2691) = rxt(k,352)*y(k,51) + rxt(k,384)*y(k,54) + rxt(k,410)*y(k,58) &
                      + rxt(k,563)*y(k,83) + rxt(k,267)*y(k,106) + rxt(k,300)*y(k,111)
         mat(k,2168) = mat(k,2168) + rxt(k,200)*y(k,158)
         mat(k,494) = -(rxt(k,179)*y(k,254))
         mat(k,2066) = -rxt(k,179)*y(k,108)
         mat(k,2396) = rxt(k,198)*y(k,239)
         mat(k,1839) = rxt(k,198)*y(k,158)
         mat(k,1609) = -(rxt(k,261)*y(k,167) + (rxt(k,587) + rxt(k,661) + rxt(k,674) &
                      + rxt(k,683)) * y(k,98) + (rxt(k,589) + rxt(k,660) + rxt(k,673) &
                      + rxt(k,682)) * y(k,102) + (rxt(k,591) + rxt(k,662) + rxt(k,675) &
                      + rxt(k,684)) * y(k,106))
         mat(k,1790) = -rxt(k,261)*y(k,109)
         mat(k,1531) = -(rxt(k,587) + rxt(k,661) + rxt(k,674) + rxt(k,683)) * y(k,109)
         mat(k,1760) = -(rxt(k,589) + rxt(k,660) + rxt(k,673) + rxt(k,682)) * y(k,109)
         mat(k,1283) = -(rxt(k,591) + rxt(k,662) + rxt(k,675) + rxt(k,684)) * y(k,109)
         mat(k,561) = rxt(k,242)*y(k,254)
         mat(k,2454) = rxt(k,251)*y(k,239)
         mat(k,1905) = rxt(k,251)*y(k,21)
         mat(k,2160) = rxt(k,242)*y(k,18)
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
         mat(k,1739) = -(rxt(k,228)*y(k,70) + rxt(k,229)*y(k,167) + rxt(k,230) &
                      *y(k,254) + (rxt(k,586) + rxt(k,663) + rxt(k,671) + rxt(k,680) &
                      ) * y(k,98) + (rxt(k,588) + rxt(k,659) + rxt(k,670) + rxt(k,679) &
                      ) * y(k,102) + (rxt(k,590) + rxt(k,664) + rxt(k,672) + rxt(k,681) &
                      ) * y(k,106))
         mat(k,2617) = -rxt(k,228)*y(k,110)
         mat(k,1794) = -rxt(k,229)*y(k,110)
         mat(k,2164) = -rxt(k,230)*y(k,110)
         mat(k,1533) = -(rxt(k,586) + rxt(k,663) + rxt(k,671) + rxt(k,680)) * y(k,110)
         mat(k,1762) = -(rxt(k,588) + rxt(k,659) + rxt(k,670) + rxt(k,679)) * y(k,110)
         mat(k,1285) = -(rxt(k,590) + rxt(k,664) + rxt(k,672) + rxt(k,681)) * y(k,110)
         mat(k,1071) = rxt(k,338)*y(k,239)
         mat(k,626) = rxt(k,211)*y(k,254)
         mat(k,2343) = rxt(k,217)*y(k,239)
         mat(k,1009) = rxt(k,222)*y(k,254)
         mat(k,1909) = rxt(k,338)*y(k,68) + rxt(k,217)*y(k,74)
         mat(k,2164) = mat(k,2164) + rxt(k,211)*y(k,73) + rxt(k,222)*y(k,75)
         mat(k,1686) = -(rxt(k,268)*y(k,254) + rxt(k,300)*y(k,159) + (rxt(k,593) &
                      + rxt(k,689) + rxt(k,693) + rxt(k,697)) * y(k,102) + (rxt(k,594) &
                      + rxt(k,690) + rxt(k,694) + rxt(k,698)) * y(k,98) + (rxt(k,595) &
                      + rxt(k,691) + rxt(k,695) + rxt(k,699)) * y(k,106))
         mat(k,2162) = -rxt(k,268)*y(k,111)
         mat(k,2685) = -rxt(k,300)*y(k,111)
         mat(k,1761) = -(rxt(k,593) + rxt(k,689) + rxt(k,693) + rxt(k,697)) * y(k,111)
         mat(k,1532) = -(rxt(k,594) + rxt(k,690) + rxt(k,694) + rxt(k,698)) * y(k,111)
         mat(k,1284) = -(rxt(k,595) + rxt(k,691) + rxt(k,695) + rxt(k,699)) * y(k,111)
         mat(k,1559) = rxt(k,271)*y(k,254)
         mat(k,210) = rxt(k,298)*y(k,266)
         mat(k,2193) = rxt(k,288)*y(k,239)
         mat(k,1907) = rxt(k,288)*y(k,127)
         mat(k,2162) = mat(k,2162) + rxt(k,271)*y(k,117)
         mat(k,2717) = rxt(k,298)*y(k,121)
         mat(k,1218) = -(rxt(k,428)*y(k,254))
         mat(k,2139) = -rxt(k,428)*y(k,112)
         mat(k,712) = .300_r8*rxt(k,473)*y(k,254)
         mat(k,665) = .500_r8*rxt(k,474)*y(k,254)
         mat(k,2536) = rxt(k,427)*y(k,236) + rxt(k,434)*y(k,246)
         mat(k,657) = rxt(k,427)*y(k,157)
         mat(k,1435) = rxt(k,434)*y(k,157)
         mat(k,2139) = mat(k,2139) + .300_r8*rxt(k,473)*y(k,130) + .500_r8*rxt(k,474) &
                      *y(k,131)
         mat(k,277) = -(rxt(k,459)*y(k,254))
         mat(k,2036) = -rxt(k,459)*y(k,113)
         mat(k,1231) = -(rxt(k,413)*y(k,254))
         mat(k,2140) = -rxt(k,413)*y(k,114)
         mat(k,713) = .700_r8*rxt(k,473)*y(k,254)
         mat(k,666) = .500_r8*rxt(k,474)*y(k,254)
         mat(k,672) = .500_r8*rxt(k,448)*y(k,254)
         mat(k,2537) = .050_r8*rxt(k,471)*y(k,242) + .220_r8*rxt(k,433)*y(k,246) &
                      + .250_r8*rxt(k,490)*y(k,262)
         mat(k,2665) = .050_r8*rxt(k,472)*y(k,242) + .220_r8*rxt(k,432)*y(k,246) &
                      + .250_r8*rxt(k,491)*y(k,262)
         mat(k,649) = .500_r8*rxt(k,417)*y(k,254)
         mat(k,1504) = .220_r8*rxt(k,429)*y(k,246) + .250_r8*rxt(k,487)*y(k,262)
         mat(k,1648) = .230_r8*rxt(k,430)*y(k,246) + .200_r8*rxt(k,418)*y(k,257) &
                      + .100_r8*rxt(k,488)*y(k,262)
         mat(k,1410) = .050_r8*rxt(k,471)*y(k,157) + .050_r8*rxt(k,472)*y(k,159)
         mat(k,1436) = .220_r8*rxt(k,433)*y(k,157) + .220_r8*rxt(k,432)*y(k,159) &
                      + .220_r8*rxt(k,429)*y(k,233) + .230_r8*rxt(k,430)*y(k,234)
         mat(k,2140) = mat(k,2140) + .700_r8*rxt(k,473)*y(k,130) + .500_r8*rxt(k,474) &
                      *y(k,131) + .500_r8*rxt(k,448)*y(k,142) + .500_r8*rxt(k,417) &
                      *y(k,181)
         mat(k,1296) = .200_r8*rxt(k,418)*y(k,234)
         mat(k,1313) = .250_r8*rxt(k,490)*y(k,157) + .250_r8*rxt(k,491)*y(k,159) &
                      + .250_r8*rxt(k,487)*y(k,233) + .100_r8*rxt(k,488)*y(k,234)
         mat(k,391) = -(rxt(k,460)*y(k,254))
         mat(k,2052) = -rxt(k,460)*y(k,115)
         mat(k,2493) = .870_r8*rxt(k,471)*y(k,242)
         mat(k,2638) = .950_r8*rxt(k,472)*y(k,242)
         mat(k,1495) = rxt(k,467)*y(k,242)
         mat(k,1627) = .750_r8*rxt(k,468)*y(k,242)
         mat(k,1399) = .870_r8*rxt(k,471)*y(k,157) + .950_r8*rxt(k,472)*y(k,159) &
                      + rxt(k,467)*y(k,233) + .750_r8*rxt(k,468)*y(k,234)
         mat(k,2383) = -(rxt(k,272)*y(k,21) + rxt(k,273)*y(k,239) + rxt(k,274) &
                      *y(k,128) + rxt(k,276)*y(k,158) + rxt(k,278)*y(k,159) + rxt(k,280) &
                      *y(k,157) + rxt(k,281)*y(k,168))
         mat(k,2468) = -rxt(k,272)*y(k,116)
         mat(k,1921) = -rxt(k,273)*y(k,116)
         mat(k,945) = -rxt(k,274)*y(k,116)
         mat(k,2440) = -rxt(k,276)*y(k,116)
         mat(k,2699) = -rxt(k,278)*y(k,116)
         mat(k,2568) = -rxt(k,280)*y(k,116)
         mat(k,2326) = -rxt(k,281)*y(k,116)
         mat(k,2234) = rxt(k,282)*y(k,127)
         mat(k,2468) = mat(k,2468) + rxt(k,283)*y(k,127)
         mat(k,420) = rxt(k,329)*y(k,70) + rxt(k,330)*y(k,254)
         mat(k,2629) = rxt(k,329)*y(k,61)
         mat(k,2355) = (rxt(k,285)+rxt(k,286))*y(k,127)
         mat(k,1041) = rxt(k,562)*y(k,127)
         mat(k,1290) = rxt(k,267)*y(k,159) + rxt(k,295)*y(k,254)
         mat(k,1565) = rxt(k,269)*y(k,159) + rxt(k,270)*y(k,167) + rxt(k,271)*y(k,254)
         mat(k,2206) = rxt(k,282)*y(k,17) + rxt(k,283)*y(k,21) + (rxt(k,285) &
                       +rxt(k,286))*y(k,74) + rxt(k,562)*y(k,83) + 2.000_r8*rxt(k,304) &
                      *y(k,127) + rxt(k,289)*y(k,157) + rxt(k,292)*y(k,167) &
                      + rxt(k,294)*y(k,254)
         mat(k,2568) = mat(k,2568) + rxt(k,289)*y(k,127)
         mat(k,2699) = mat(k,2699) + rxt(k,267)*y(k,106) + rxt(k,269)*y(k,117)
         mat(k,1806) = rxt(k,270)*y(k,117) + rxt(k,292)*y(k,127)
         mat(k,2176) = rxt(k,330)*y(k,61) + rxt(k,295)*y(k,106) + rxt(k,271)*y(k,117) &
                      + rxt(k,294)*y(k,127)
         mat(k,1558) = -(rxt(k,269)*y(k,159) + rxt(k,270)*y(k,167) + rxt(k,271) &
                      *y(k,254))
         mat(k,2680) = -rxt(k,269)*y(k,117)
         mat(k,1787) = -rxt(k,270)*y(k,117)
         mat(k,2157) = -rxt(k,271)*y(k,117)
         mat(k,1282) = (rxt(k,595)+rxt(k,691)+rxt(k,695)+rxt(k,699))*y(k,111)
         mat(k,1684) = (rxt(k,595)+rxt(k,691)+rxt(k,695)+rxt(k,699))*y(k,106)
         mat(k,2368) = rxt(k,274)*y(k,128)
         mat(k,213) = 2.000_r8*rxt(k,279)*y(k,125)
         mat(k,326) = 2.000_r8*rxt(k,275)*y(k,126)
         mat(k,940) = rxt(k,274)*y(k,116)
         mat(k,314) = -(rxt(k,297)*y(k,168))
         mat(k,2271) = -rxt(k,297)*y(k,118)
         mat(k,2184) = 2.000_r8*rxt(k,305)*y(k,127)
         mat(k,2183) = rxt(k,307)*y(k,172)
         mat(k,787) = rxt(k,307)*y(k,127)
         mat(k,786) = 2.000_r8*rxt(k,308)*y(k,172)
         mat(k,209) = -(rxt(k,298)*y(k,266))
         mat(k,2708) = -rxt(k,298)*y(k,121)
         mat(k,313) = rxt(k,297)*y(k,168)
         mat(k,2269) = rxt(k,297)*y(k,118)
         mat(k,1527) = (rxt(k,594)+rxt(k,690)+rxt(k,694)+rxt(k,698))*y(k,111)
         mat(k,1279) = (rxt(k,591)+rxt(k,662)+rxt(k,675)+rxt(k,684))*y(k,109)
         mat(k,1603) = (rxt(k,591)+rxt(k,662)+rxt(k,675)+rxt(k,684))*y(k,106)
         mat(k,1680) = (rxt(k,594)+rxt(k,690)+rxt(k,694)+rxt(k,698))*y(k,98)
         mat(k,2336) = rxt(k,287)*y(k,127)
         mat(k,1756) = (rxt(k,593)+rxt(k,689)+rxt(k,693)+rxt(k,697))*y(k,111)
         mat(k,1280) = (rxt(k,590)+rxt(k,664)+rxt(k,672)+rxt(k,681))*y(k,110)
         mat(k,1732) = (rxt(k,590)+rxt(k,664)+rxt(k,672)+rxt(k,681))*y(k,106)
         mat(k,1681) = (rxt(k,593)+rxt(k,689)+rxt(k,693)+rxt(k,697))*y(k,102)
         mat(k,2186) = rxt(k,287)*y(k,74)
         mat(k,162) = -(rxt(k,461)*y(k,254))
         mat(k,2020) = -rxt(k,461)*y(k,124)
         mat(k,809) = .600_r8*rxt(k,484)*y(k,254)
         mat(k,2020) = mat(k,2020) + .600_r8*rxt(k,484)*y(k,133)
         mat(k,212) = -(4._r8*rxt(k,279)*y(k,125))
         mat(k,2362) = rxt(k,280)*y(k,157)
         mat(k,2487) = rxt(k,280)*y(k,116)
         mat(k,323) = -(4._r8*rxt(k,275)*y(k,126))
         mat(k,2363) = rxt(k,276)*y(k,158)
         mat(k,2392) = rxt(k,276)*y(k,116)
         mat(k,2201) = -(rxt(k,282)*y(k,17) + (rxt(k,283) + rxt(k,284)) * y(k,21) &
                      + (rxt(k,285) + rxt(k,286) + rxt(k,287)) * y(k,74) + rxt(k,288) &
                      *y(k,239) + rxt(k,289)*y(k,157) + rxt(k,290)*y(k,158) + rxt(k,291) &
                      *y(k,159) + rxt(k,292)*y(k,167) + rxt(k,293)*y(k,168) + rxt(k,294) &
                      *y(k,254) + (4._r8*rxt(k,304) + 4._r8*rxt(k,305)) * y(k,127) &
                      + rxt(k,307)*y(k,172) + rxt(k,562)*y(k,83))
         mat(k,2229) = -rxt(k,282)*y(k,127)
         mat(k,2463) = -(rxt(k,283) + rxt(k,284)) * y(k,127)
         mat(k,2350) = -(rxt(k,285) + rxt(k,286) + rxt(k,287)) * y(k,127)
         mat(k,1916) = -rxt(k,288)*y(k,127)
         mat(k,2563) = -rxt(k,289)*y(k,127)
         mat(k,2435) = -rxt(k,290)*y(k,127)
         mat(k,2694) = -rxt(k,291)*y(k,127)
         mat(k,1801) = -rxt(k,292)*y(k,127)
         mat(k,2321) = -rxt(k,293)*y(k,127)
         mat(k,2171) = -rxt(k,294)*y(k,127)
         mat(k,790) = -rxt(k,307)*y(k,127)
         mat(k,1039) = -rxt(k,562)*y(k,127)
         mat(k,2463) = mat(k,2463) + rxt(k,272)*y(k,116)
         mat(k,1693) = rxt(k,300)*y(k,159) + rxt(k,268)*y(k,254)
         mat(k,2378) = rxt(k,272)*y(k,21) + rxt(k,278)*y(k,159) + rxt(k,281)*y(k,168)
         mat(k,1563) = rxt(k,270)*y(k,167)
         mat(k,2563) = mat(k,2563) + rxt(k,296)*y(k,172)
         mat(k,2694) = mat(k,2694) + rxt(k,300)*y(k,111) + rxt(k,278)*y(k,116)
         mat(k,1801) = mat(k,1801) + rxt(k,270)*y(k,117)
         mat(k,2321) = mat(k,2321) + rxt(k,281)*y(k,116)
         mat(k,790) = mat(k,790) + rxt(k,296)*y(k,157)
         mat(k,2171) = mat(k,2171) + rxt(k,268)*y(k,111)
         mat(k,939) = -(rxt(k,274)*y(k,116))
         mat(k,2366) = -rxt(k,274)*y(k,128)
         mat(k,1557) = rxt(k,269)*y(k,159)
         mat(k,2188) = rxt(k,290)*y(k,158)
         mat(k,2411) = rxt(k,290)*y(k,127)
         mat(k,2646) = rxt(k,269)*y(k,117)
         mat(k,903) = -(rxt(k,475)*y(k,159) + rxt(k,482)*y(k,168) + rxt(k,483) &
                      *y(k,254))
         mat(k,2644) = -rxt(k,475)*y(k,129)
         mat(k,2281) = -rxt(k,482)*y(k,129)
         mat(k,2116) = -rxt(k,483)*y(k,129)
         mat(k,710) = -(rxt(k,473)*y(k,254))
         mat(k,2095) = -rxt(k,473)*y(k,130)
         mat(k,2507) = .080_r8*rxt(k,465)*y(k,241)
         mat(k,1370) = .080_r8*rxt(k,465)*y(k,157)
         mat(k,662) = -(rxt(k,474)*y(k,254))
         mat(k,2089) = -rxt(k,474)*y(k,131)
         mat(k,2505) = .080_r8*rxt(k,471)*y(k,242)
         mat(k,1400) = .080_r8*rxt(k,471)*y(k,157)
         mat(k,458) = -(rxt(k,481)*y(k,254))
         mat(k,2061) = -rxt(k,481)*y(k,132)
         mat(k,1835) = rxt(k,478)*y(k,243)
         mat(k,1325) = rxt(k,478)*y(k,239)
         mat(k,810) = -(rxt(k,484)*y(k,254))
         mat(k,2106) = -rxt(k,484)*y(k,133)
         mat(k,1863) = rxt(k,464)*y(k,241) + rxt(k,469)*y(k,242)
         mat(k,1371) = rxt(k,464)*y(k,239)
         mat(k,1402) = rxt(k,469)*y(k,239)
         mat(k,88) = -(rxt(k,644)*y(k,254))
         mat(k,2010) = -rxt(k,644)*y(k,134)
         mat(k,1350) = -(rxt(k,435)*y(k,168) + rxt(k,436)*y(k,254))
         mat(k,2301) = -rxt(k,435)*y(k,138)
         mat(k,2147) = -rxt(k,436)*y(k,138)
         mat(k,908) = .300_r8*rxt(k,482)*y(k,168)
         mat(k,2543) = .360_r8*rxt(k,465)*y(k,241)
         mat(k,2672) = .400_r8*rxt(k,466)*y(k,241)
         mat(k,2301) = mat(k,2301) + .300_r8*rxt(k,482)*y(k,129)
         mat(k,1507) = .390_r8*rxt(k,462)*y(k,241)
         mat(k,1654) = .310_r8*rxt(k,463)*y(k,241)
         mat(k,1380) = .360_r8*rxt(k,465)*y(k,157) + .400_r8*rxt(k,466)*y(k,159) &
                      + .390_r8*rxt(k,462)*y(k,233) + .310_r8*rxt(k,463)*y(k,234)
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
         mat(k,368) = -(rxt(k,437)*y(k,254))
         mat(k,2049) = -rxt(k,437)*y(k,139)
         mat(k,1829) = rxt(k,431)*y(k,246)
         mat(k,1431) = rxt(k,431)*y(k,239)
         mat(k,603) = -(rxt(k,446)*y(k,254))
         mat(k,2081) = -rxt(k,446)*y(k,140)
         mat(k,2502) = .800_r8*rxt(k,455)*y(k,225)
         mat(k,1018) = .800_r8*rxt(k,455)*y(k,157)
         mat(k,373) = -(rxt(k,447)*y(k,254))
         mat(k,2050) = -rxt(k,447)*y(k,141)
         mat(k,1830) = .800_r8*rxt(k,444)*y(k,250)
         mat(k,762) = .800_r8*rxt(k,444)*y(k,239)
         mat(k,670) = -(rxt(k,448)*y(k,254))
         mat(k,2090) = -rxt(k,448)*y(k,142)
         mat(k,2405) = rxt(k,451)*y(k,248)
         mat(k,1475) = rxt(k,451)*y(k,158)
         mat(k,1097) = -(rxt(k,539)*y(k,159) + rxt(k,540)*y(k,168) + rxt(k,541) &
                      *y(k,254))
         mat(k,2655) = -rxt(k,539)*y(k,143)
         mat(k,2288) = -rxt(k,540)*y(k,143)
         mat(k,2130) = -rxt(k,541)*y(k,143)
         mat(k,1460) = -(rxt(k,449)*y(k,168) + rxt(k,450)*y(k,254))
         mat(k,2306) = -rxt(k,449)*y(k,144)
         mat(k,2152) = -rxt(k,450)*y(k,144)
         mat(k,911) = .200_r8*rxt(k,482)*y(k,168)
         mat(k,2548) = .560_r8*rxt(k,465)*y(k,241)
         mat(k,2677) = .600_r8*rxt(k,466)*y(k,241)
         mat(k,2306) = mat(k,2306) + .200_r8*rxt(k,482)*y(k,129)
         mat(k,1512) = .610_r8*rxt(k,462)*y(k,241)
         mat(k,1659) = .440_r8*rxt(k,463)*y(k,241)
         mat(k,1384) = .560_r8*rxt(k,465)*y(k,157) + .600_r8*rxt(k,466)*y(k,159) &
                      + .610_r8*rxt(k,462)*y(k,233) + .440_r8*rxt(k,463)*y(k,234)
         mat(k,572) = -(rxt(k,180)*y(k,157) + (rxt(k,181) + rxt(k,182) + rxt(k,183) &
                      ) * y(k,158) + rxt(k,192)*y(k,254))
         mat(k,2499) = -rxt(k,180)*y(k,145)
         mat(k,2400) = -(rxt(k,181) + rxt(k,182) + rxt(k,183)) * y(k,145)
         mat(k,2077) = -rxt(k,192)*y(k,145)
         mat(k,216) = -((rxt(k,196) + rxt(k,197)) * y(k,253))
         mat(k,1963) = -(rxt(k,196) + rxt(k,197)) * y(k,146)
         mat(k,571) = rxt(k,181)*y(k,158)
         mat(k,2391) = rxt(k,181)*y(k,145)
         mat(k,2395) = rxt(k,199)*y(k,159)
         mat(k,2640) = rxt(k,199)*y(k,158)
         mat(k,464) = -(rxt(k,485)*y(k,254))
         mat(k,2062) = -rxt(k,485)*y(k,148)
         mat(k,1629) = .200_r8*rxt(k,477)*y(k,243)
         mat(k,1326) = .200_r8*rxt(k,477)*y(k,234)
         mat(k,1151) = -(rxt(k,486)*y(k,254))
         mat(k,2133) = -rxt(k,486)*y(k,149)
         mat(k,2530) = rxt(k,479)*y(k,243)
         mat(k,2658) = rxt(k,480)*y(k,243)
         mat(k,1501) = rxt(k,476)*y(k,243)
         mat(k,1642) = .800_r8*rxt(k,477)*y(k,243)
         mat(k,1330) = rxt(k,479)*y(k,157) + rxt(k,480)*y(k,159) + rxt(k,476)*y(k,233) &
                      + .800_r8*rxt(k,477)*y(k,234)
         mat(k,112) = -(rxt(k,597)*y(k,254))
         mat(k,2014) = -rxt(k,597)*y(k,153)
         mat(k,2571) = -(rxt(k,180)*y(k,145) + rxt(k,189)*y(k,159) + rxt(k,193) &
                      *y(k,239) + rxt(k,194)*y(k,168) + rxt(k,195)*y(k,167) + rxt(k,218) &
                      *y(k,74) + rxt(k,252)*y(k,21) + rxt(k,280)*y(k,116) + rxt(k,289) &
                      *y(k,127) + rxt(k,296)*y(k,172) + rxt(k,339)*y(k,68) + rxt(k,358) &
                      *y(k,234) + rxt(k,366)*y(k,240) + rxt(k,379)*y(k,230) + rxt(k,390) &
                      *y(k,233) + rxt(k,394)*y(k,238) + rxt(k,407)*y(k,231) + rxt(k,416) &
                      *y(k,256) + rxt(k,420)*y(k,257) + (rxt(k,426) + rxt(k,427) &
                      ) * y(k,236) + (rxt(k,433) + rxt(k,434)) * y(k,246) + rxt(k,442) &
                      *y(k,248) + rxt(k,445)*y(k,250) + (rxt(k,455) + rxt(k,456) &
                      ) * y(k,225) + rxt(k,465)*y(k,241) + rxt(k,471)*y(k,242) &
                      + rxt(k,479)*y(k,243) + rxt(k,490)*y(k,262) + rxt(k,494) &
                      *y(k,224) + rxt(k,497)*y(k,227) + rxt(k,502)*y(k,229) + rxt(k,504) &
                      *y(k,232) + rxt(k,508)*y(k,235) + rxt(k,511)*y(k,247) + rxt(k,514) &
                      *y(k,249) + rxt(k,517)*y(k,255) + rxt(k,524)*y(k,260) + rxt(k,530) &
                      *y(k,263) + rxt(k,533)*y(k,265) + rxt(k,544)*y(k,252) + rxt(k,549) &
                      *y(k,258) + rxt(k,554)*y(k,259))
         mat(k,578) = -rxt(k,180)*y(k,157)
         mat(k,2702) = -rxt(k,189)*y(k,157)
         mat(k,1924) = -rxt(k,193)*y(k,157)
         mat(k,2329) = -rxt(k,194)*y(k,157)
         mat(k,1809) = -rxt(k,195)*y(k,157)
         mat(k,2358) = -rxt(k,218)*y(k,157)
         mat(k,2471) = -rxt(k,252)*y(k,157)
         mat(k,2386) = -rxt(k,280)*y(k,157)
         mat(k,2209) = -rxt(k,289)*y(k,157)
         mat(k,793) = -rxt(k,296)*y(k,157)
         mat(k,1074) = -rxt(k,339)*y(k,157)
         mat(k,1676) = -rxt(k,358)*y(k,157)
         mat(k,537) = -rxt(k,366)*y(k,157)
         mat(k,896) = -rxt(k,379)*y(k,157)
         mat(k,1524) = -rxt(k,390)*y(k,157)
         mat(k,825) = -rxt(k,394)*y(k,157)
         mat(k,958) = -rxt(k,407)*y(k,157)
         mat(k,880) = -rxt(k,416)*y(k,157)
         mat(k,1305) = -rxt(k,420)*y(k,157)
         mat(k,661) = -(rxt(k,426) + rxt(k,427)) * y(k,157)
         mat(k,1449) = -(rxt(k,433) + rxt(k,434)) * y(k,157)
         mat(k,1492) = -rxt(k,442)*y(k,157)
         mat(k,769) = -rxt(k,445)*y(k,157)
         mat(k,1032) = -(rxt(k,455) + rxt(k,456)) * y(k,157)
         mat(k,1395) = -rxt(k,465)*y(k,157)
         mat(k,1428) = -rxt(k,471)*y(k,157)
         mat(k,1346) = -rxt(k,479)*y(k,157)
         mat(k,1323) = -rxt(k,490)*y(k,157)
         mat(k,623) = -rxt(k,494)*y(k,157)
         mat(k,594) = -rxt(k,497)*y(k,157)
         mat(k,531) = -rxt(k,502)*y(k,157)
         mat(k,739) = -rxt(k,504)*y(k,157)
         mat(k,871) = -rxt(k,508)*y(k,157)
         mat(k,831) = -rxt(k,511)*y(k,157)
         mat(k,978) = -rxt(k,514)*y(k,157)
         mat(k,544) = -rxt(k,517)*y(k,157)
         mat(k,846) = -rxt(k,524)*y(k,157)
         mat(k,863) = -rxt(k,530)*y(k,157)
         mat(k,602) = -rxt(k,533)*y(k,157)
         mat(k,1194) = -rxt(k,544)*y(k,157)
         mat(k,1255) = -rxt(k,549)*y(k,157)
         mat(k,1276) = -rxt(k,554)*y(k,157)
         mat(k,215) = 4.000_r8*rxt(k,279)*y(k,125)
         mat(k,578) = mat(k,578) + 2.000_r8*rxt(k,182)*y(k,158) + rxt(k,192)*y(k,254)
         mat(k,218) = 2.000_r8*rxt(k,196)*y(k,253)
         mat(k,2443) = 2.000_r8*rxt(k,182)*y(k,145) + rxt(k,185)*y(k,167) + rxt(k,573) &
                      *y(k,185)
         mat(k,1809) = mat(k,1809) + rxt(k,185)*y(k,158)
         mat(k,1582) = rxt(k,573)*y(k,158)
         mat(k,1995) = 2.000_r8*rxt(k,196)*y(k,146)
         mat(k,2179) = rxt(k,192)*y(k,145)
         mat(k,2441) = -((rxt(k,181) + rxt(k,182) + rxt(k,183)) * y(k,145) + (rxt(k,185) &
                      + rxt(k,187)) * y(k,167) + rxt(k,186)*y(k,168) + rxt(k,198) &
                      *y(k,239) + rxt(k,199)*y(k,159) + rxt(k,200)*y(k,254) + rxt(k,210) &
                      *y(k,70) + rxt(k,220)*y(k,74) + rxt(k,245)*y(k,17) + rxt(k,255) &
                      *y(k,21) + rxt(k,276)*y(k,116) + rxt(k,290)*y(k,127) + rxt(k,401) &
                      *y(k,233) + rxt(k,451)*y(k,248) + rxt(k,509)*y(k,235) + rxt(k,512) &
                      *y(k,247) + rxt(k,515)*y(k,249) + rxt(k,519)*y(k,176) + rxt(k,522) &
                      *y(k,224) + rxt(k,573)*y(k,185))
         mat(k,577) = -(rxt(k,181) + rxt(k,182) + rxt(k,183)) * y(k,158)
         mat(k,1807) = -(rxt(k,185) + rxt(k,187)) * y(k,158)
         mat(k,2327) = -rxt(k,186)*y(k,158)
         mat(k,1922) = -rxt(k,198)*y(k,158)
         mat(k,2700) = -rxt(k,199)*y(k,158)
         mat(k,2177) = -rxt(k,200)*y(k,158)
         mat(k,2630) = -rxt(k,210)*y(k,158)
         mat(k,2356) = -rxt(k,220)*y(k,158)
         mat(k,2235) = -rxt(k,245)*y(k,158)
         mat(k,2469) = -rxt(k,255)*y(k,158)
         mat(k,2384) = -rxt(k,276)*y(k,158)
         mat(k,2207) = -rxt(k,290)*y(k,158)
         mat(k,1523) = -rxt(k,401)*y(k,158)
         mat(k,1491) = -rxt(k,451)*y(k,158)
         mat(k,870) = -rxt(k,509)*y(k,158)
         mat(k,830) = -rxt(k,512)*y(k,158)
         mat(k,977) = -rxt(k,515)*y(k,158)
         mat(k,559) = -rxt(k,519)*y(k,158)
         mat(k,622) = -rxt(k,522)*y(k,158)
         mat(k,1580) = -rxt(k,573)*y(k,158)
         mat(k,761) = rxt(k,453)*y(k,254)
         mat(k,429) = rxt(k,424)*y(k,159)
         mat(k,2469) = mat(k,2469) + rxt(k,252)*y(k,157)
         mat(k,1073) = rxt(k,339)*y(k,157) + rxt(k,340)*y(k,159)
         mat(k,628) = rxt(k,211)*y(k,254)
         mat(k,2356) = mat(k,2356) + rxt(k,218)*y(k,157)
         mat(k,497) = rxt(k,179)*y(k,254)
         mat(k,2384) = mat(k,2384) + rxt(k,278)*y(k,159)
         mat(k,328) = 4.000_r8*rxt(k,275)*y(k,126)
         mat(k,2207) = mat(k,2207) + rxt(k,289)*y(k,157) + rxt(k,291)*y(k,159)
         mat(k,718) = .700_r8*rxt(k,473)*y(k,254)
         mat(k,2569) = rxt(k,252)*y(k,21) + rxt(k,339)*y(k,68) + rxt(k,218)*y(k,74) &
                      + rxt(k,289)*y(k,127) + 2.000_r8*rxt(k,189)*y(k,159) &
                      + rxt(k,195)*y(k,167) + rxt(k,194)*y(k,168) + rxt(k,296) &
                      *y(k,172) + rxt(k,494)*y(k,224) + rxt(k,455)*y(k,225) &
                      + rxt(k,497)*y(k,227) + rxt(k,502)*y(k,229) + rxt(k,379) &
                      *y(k,230) + rxt(k,407)*y(k,231) + rxt(k,504)*y(k,232) &
                      + rxt(k,390)*y(k,233) + rxt(k,358)*y(k,234) + rxt(k,508) &
                      *y(k,235) + rxt(k,426)*y(k,236) + rxt(k,394)*y(k,238) &
                      + rxt(k,193)*y(k,239) + rxt(k,366)*y(k,240) + .920_r8*rxt(k,465) &
                      *y(k,241) + .920_r8*rxt(k,471)*y(k,242) + rxt(k,479)*y(k,243) &
                      + rxt(k,433)*y(k,246) + rxt(k,511)*y(k,247) + rxt(k,442) &
                      *y(k,248) + rxt(k,514)*y(k,249) + rxt(k,445)*y(k,250) &
                      + 1.600_r8*rxt(k,544)*y(k,252) + rxt(k,517)*y(k,255) &
                      + rxt(k,416)*y(k,256) + rxt(k,420)*y(k,257) + .900_r8*rxt(k,549) &
                      *y(k,258) + .800_r8*rxt(k,554)*y(k,259) + rxt(k,524)*y(k,260) &
                      + rxt(k,490)*y(k,262) + rxt(k,530)*y(k,263) + rxt(k,533) &
                      *y(k,265)
         mat(k,2700) = mat(k,2700) + rxt(k,424)*y(k,16) + rxt(k,340)*y(k,68) &
                      + rxt(k,278)*y(k,116) + rxt(k,291)*y(k,127) &
                      + 2.000_r8*rxt(k,189)*y(k,157) + rxt(k,190)*y(k,167) &
                      + rxt(k,188)*y(k,239) + rxt(k,466)*y(k,241) + rxt(k,472) &
                      *y(k,242) + rxt(k,480)*y(k,243) + rxt(k,432)*y(k,246) &
                      + rxt(k,443)*y(k,248) + 2.000_r8*rxt(k,545)*y(k,252) &
                      + rxt(k,191)*y(k,254) + rxt(k,491)*y(k,262)
         mat(k,936) = rxt(k,414)*y(k,254)
         mat(k,1807) = mat(k,1807) + rxt(k,195)*y(k,157) + rxt(k,190)*y(k,159)
         mat(k,2327) = mat(k,2327) + rxt(k,194)*y(k,157)
         mat(k,792) = rxt(k,296)*y(k,157)
         mat(k,731) = rxt(k,551)*y(k,254)
         mat(k,622) = mat(k,622) + rxt(k,494)*y(k,157)
         mat(k,1031) = rxt(k,455)*y(k,157)
         mat(k,593) = rxt(k,497)*y(k,157)
         mat(k,530) = rxt(k,502)*y(k,157)
         mat(k,895) = rxt(k,379)*y(k,157)
         mat(k,957) = rxt(k,407)*y(k,157)
         mat(k,738) = rxt(k,504)*y(k,157)
         mat(k,1523) = mat(k,1523) + rxt(k,390)*y(k,157)
         mat(k,1675) = rxt(k,358)*y(k,157) + .500_r8*rxt(k,542)*y(k,252)
         mat(k,870) = mat(k,870) + rxt(k,508)*y(k,157)
         mat(k,660) = rxt(k,426)*y(k,157)
         mat(k,824) = rxt(k,394)*y(k,157)
         mat(k,1922) = mat(k,1922) + rxt(k,193)*y(k,157) + rxt(k,188)*y(k,159)
         mat(k,536) = rxt(k,366)*y(k,157)
         mat(k,1394) = .920_r8*rxt(k,465)*y(k,157) + rxt(k,466)*y(k,159)
         mat(k,1427) = .920_r8*rxt(k,471)*y(k,157) + rxt(k,472)*y(k,159)
         mat(k,1345) = rxt(k,479)*y(k,157) + rxt(k,480)*y(k,159)
         mat(k,1448) = rxt(k,433)*y(k,157) + rxt(k,432)*y(k,159)
         mat(k,830) = mat(k,830) + rxt(k,511)*y(k,157)
         mat(k,1491) = mat(k,1491) + rxt(k,442)*y(k,157) + rxt(k,443)*y(k,159)
         mat(k,977) = mat(k,977) + rxt(k,514)*y(k,157)
         mat(k,768) = rxt(k,445)*y(k,157)
         mat(k,1193) = 1.600_r8*rxt(k,544)*y(k,157) + 2.000_r8*rxt(k,545)*y(k,159) &
                      + .500_r8*rxt(k,542)*y(k,234)
         mat(k,2177) = mat(k,2177) + rxt(k,453)*y(k,1) + rxt(k,211)*y(k,73) &
                      + rxt(k,179)*y(k,108) + .700_r8*rxt(k,473)*y(k,130) + rxt(k,191) &
                      *y(k,159) + rxt(k,414)*y(k,160) + rxt(k,551)*y(k,211)
         mat(k,543) = rxt(k,517)*y(k,157)
         mat(k,879) = rxt(k,416)*y(k,157)
         mat(k,1304) = rxt(k,420)*y(k,157)
         mat(k,1254) = .900_r8*rxt(k,549)*y(k,157)
         mat(k,1275) = .800_r8*rxt(k,554)*y(k,157)
         mat(k,845) = rxt(k,524)*y(k,157)
         mat(k,1322) = rxt(k,490)*y(k,157) + rxt(k,491)*y(k,159)
         mat(k,862) = rxt(k,530)*y(k,157)
         mat(k,601) = rxt(k,533)*y(k,157)
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
         mat(k,2704) = -(rxt(k,188)*y(k,239) + rxt(k,189)*y(k,157) + rxt(k,190) &
                      *y(k,167) + rxt(k,191)*y(k,254) + rxt(k,199)*y(k,158) + rxt(k,267) &
                      *y(k,106) + rxt(k,269)*y(k,117) + rxt(k,278)*y(k,116) + rxt(k,291) &
                      *y(k,127) + rxt(k,300)*y(k,111) + rxt(k,340)*y(k,68) + rxt(k,352) &
                      *y(k,51) + rxt(k,384)*y(k,54) + rxt(k,403)*y(k,33) + rxt(k,410) &
                      *y(k,58) + rxt(k,424)*y(k,16) + rxt(k,432)*y(k,246) + rxt(k,443) &
                      *y(k,248) + rxt(k,466)*y(k,241) + rxt(k,472)*y(k,242) + rxt(k,475) &
                      *y(k,129) + rxt(k,480)*y(k,243) + rxt(k,491)*y(k,262) + rxt(k,536) &
                      *y(k,6) + rxt(k,539)*y(k,143) + rxt(k,545)*y(k,252) + rxt(k,556) &
                      *y(k,213) + rxt(k,563)*y(k,83))
         mat(k,1926) = -rxt(k,188)*y(k,159)
         mat(k,2573) = -rxt(k,189)*y(k,159)
         mat(k,1811) = -rxt(k,190)*y(k,159)
         mat(k,2181) = -rxt(k,191)*y(k,159)
         mat(k,2445) = -rxt(k,199)*y(k,159)
         mat(k,1292) = -rxt(k,267)*y(k,159)
         mat(k,1568) = -rxt(k,269)*y(k,159)
         mat(k,2388) = -rxt(k,278)*y(k,159)
         mat(k,2211) = -rxt(k,291)*y(k,159)
         mat(k,1700) = -rxt(k,300)*y(k,159)
         mat(k,1076) = -rxt(k,340)*y(k,159)
         mat(k,1729) = -rxt(k,352)*y(k,159)
         mat(k,1170) = -rxt(k,384)*y(k,159)
         mat(k,1145) = -rxt(k,403)*y(k,159)
         mat(k,1368) = -rxt(k,410)*y(k,159)
         mat(k,430) = -rxt(k,424)*y(k,159)
         mat(k,1450) = -rxt(k,432)*y(k,159)
         mat(k,1493) = -rxt(k,443)*y(k,159)
         mat(k,1396) = -rxt(k,466)*y(k,159)
         mat(k,1429) = -rxt(k,472)*y(k,159)
         mat(k,918) = -rxt(k,475)*y(k,159)
         mat(k,1347) = -rxt(k,480)*y(k,159)
         mat(k,1324) = -rxt(k,491)*y(k,159)
         mat(k,1006) = -rxt(k,536)*y(k,159)
         mat(k,1115) = -rxt(k,539)*y(k,159)
         mat(k,1195) = -rxt(k,545)*y(k,159)
         mat(k,1124) = -rxt(k,556)*y(k,159)
         mat(k,1044) = -rxt(k,563)*y(k,159)
         mat(k,2239) = rxt(k,253)*y(k,22)
         mat(k,930) = rxt(k,253)*y(k,17) + rxt(k,254)*y(k,70) + rxt(k,256)*y(k,167)
         mat(k,2634) = rxt(k,254)*y(k,22) + rxt(k,219)*y(k,75)
         mat(k,1017) = rxt(k,219)*y(k,70) + rxt(k,221)*y(k,167) + rxt(k,222)*y(k,254)
         mat(k,968) = rxt(k,312)*y(k,107)
         mat(k,1951) = rxt(k,312)*y(k,89) + rxt(k,201)*y(k,254)
         mat(k,2388) = mat(k,2388) + rxt(k,274)*y(k,128)
         mat(k,948) = rxt(k,274)*y(k,116)
         mat(k,678) = .500_r8*rxt(k,448)*y(k,254)
         mat(k,2445) = mat(k,2445) + rxt(k,187)*y(k,167) + rxt(k,186)*y(k,168)
         mat(k,1811) = mat(k,1811) + rxt(k,256)*y(k,22) + rxt(k,221)*y(k,75) &
                      + rxt(k,187)*y(k,158)
         mat(k,2331) = rxt(k,186)*y(k,158)
         mat(k,645) = rxt(k,399)*y(k,254)
         mat(k,2181) = mat(k,2181) + rxt(k,222)*y(k,75) + rxt(k,201)*y(k,107) &
                      + .500_r8*rxt(k,448)*y(k,142) + rxt(k,399)*y(k,174)
         mat(k,931) = -(rxt(k,414)*y(k,254))
         mat(k,2117) = -rxt(k,414)*y(k,160)
         mat(k,1128) = rxt(k,403)*y(k,159)
         mat(k,663) = .500_r8*rxt(k,474)*y(k,254)
         mat(k,460) = rxt(k,481)*y(k,254)
         mat(k,465) = rxt(k,485)*y(k,254)
         mat(k,1148) = rxt(k,486)*y(k,254)
         mat(k,2645) = rxt(k,403)*y(k,33)
         mat(k,2117) = mat(k,2117) + .500_r8*rxt(k,474)*y(k,131) + rxt(k,481)*y(k,132) &
                      + rxt(k,485)*y(k,148) + rxt(k,486)*y(k,149)
         mat(k,476) = -(rxt(k,546)*y(k,254))
         mat(k,2064) = -rxt(k,546)*y(k,161)
         mat(k,1837) = rxt(k,543)*y(k,252)
         mat(k,1180) = rxt(k,543)*y(k,239)
         mat(k,1796) = -(rxt(k,159)*y(k,168) + 4._r8*rxt(k,160)*y(k,167) + rxt(k,162) &
                      *y(k,93) + rxt(k,163)*y(k,95) + rxt(k,168)*y(k,239) + rxt(k,174) &
                      *y(k,254) + (rxt(k,185) + rxt(k,187)) * y(k,158) + rxt(k,190) &
                      *y(k,159) + rxt(k,195)*y(k,157) + rxt(k,221)*y(k,75) + rxt(k,223) &
                      *y(k,74) + rxt(k,226)*y(k,102) + rxt(k,229)*y(k,110) + rxt(k,256) &
                      *y(k,22) + rxt(k,257)*y(k,21) + rxt(k,259)*y(k,98) + rxt(k,261) &
                      *y(k,109) + rxt(k,270)*y(k,117) + rxt(k,292)*y(k,127) + rxt(k,353) &
                      *y(k,51) + rxt(k,565)*y(k,171))
         mat(k,2316) = -rxt(k,159)*y(k,167)
         mat(k,1549) = -rxt(k,162)*y(k,167)
         mat(k,701) = -rxt(k,163)*y(k,167)
         mat(k,1911) = -rxt(k,168)*y(k,167)
         mat(k,2166) = -rxt(k,174)*y(k,167)
         mat(k,2430) = -(rxt(k,185) + rxt(k,187)) * y(k,167)
         mat(k,2689) = -rxt(k,190)*y(k,167)
         mat(k,2558) = -rxt(k,195)*y(k,167)
         mat(k,1011) = -rxt(k,221)*y(k,167)
         mat(k,2345) = -rxt(k,223)*y(k,167)
         mat(k,1764) = -rxt(k,226)*y(k,167)
         mat(k,1741) = -rxt(k,229)*y(k,167)
         mat(k,923) = -rxt(k,256)*y(k,167)
         mat(k,2458) = -rxt(k,257)*y(k,167)
         mat(k,1534) = -rxt(k,259)*y(k,167)
         mat(k,1613) = -rxt(k,261)*y(k,167)
         mat(k,1560) = -rxt(k,270)*y(k,167)
         mat(k,2196) = -rxt(k,292)*y(k,167)
         mat(k,1714) = -rxt(k,353)*y(k,167)
         mat(k,447) = -rxt(k,565)*y(k,167)
         mat(k,2247) = rxt(k,166)*y(k,239)
         mat(k,573) = rxt(k,180)*y(k,157) + rxt(k,181)*y(k,158)
         mat(k,2558) = mat(k,2558) + rxt(k,180)*y(k,145)
         mat(k,2430) = mat(k,2430) + rxt(k,181)*y(k,145)
         mat(k,2316) = mat(k,2316) + 2.000_r8*rxt(k,158)*y(k,253)
         mat(k,1911) = mat(k,1911) + rxt(k,166)*y(k,92)
         mat(k,1982) = 2.000_r8*rxt(k,158)*y(k,168)
         mat(k,2166) = mat(k,2166) + 2.000_r8*rxt(k,176)*y(k,254)
         mat(k,2324) = -((rxt(k,157) + rxt(k,158)) * y(k,253) + rxt(k,159)*y(k,167) &
                      + rxt(k,169)*y(k,239) + rxt(k,170)*y(k,92) + rxt(k,175)*y(k,254) &
                      + rxt(k,186)*y(k,158) + rxt(k,194)*y(k,157) + rxt(k,212)*y(k,70) &
                      + rxt(k,246)*y(k,17) + rxt(k,281)*y(k,116) + rxt(k,293)*y(k,127) &
                      + rxt(k,375)*y(k,28) + rxt(k,404)*y(k,33) + rxt(k,435)*y(k,138) &
                      + rxt(k,449)*y(k,144) + rxt(k,482)*y(k,129) + rxt(k,520) &
                      *y(k,176) + rxt(k,537)*y(k,6) + rxt(k,540)*y(k,143) + rxt(k,569) &
                      *y(k,183) + rxt(k,575)*y(k,185))
         mat(k,1990) = -(rxt(k,157) + rxt(k,158)) * y(k,168)
         mat(k,1804) = -rxt(k,159)*y(k,168)
         mat(k,1919) = -rxt(k,169)*y(k,168)
         mat(k,2255) = -rxt(k,170)*y(k,168)
         mat(k,2174) = -rxt(k,175)*y(k,168)
         mat(k,2438) = -rxt(k,186)*y(k,168)
         mat(k,2566) = -rxt(k,194)*y(k,168)
         mat(k,2627) = -rxt(k,212)*y(k,168)
         mat(k,2232) = -rxt(k,246)*y(k,168)
         mat(k,2381) = -rxt(k,281)*y(k,168)
         mat(k,2204) = -rxt(k,293)*y(k,168)
         mat(k,637) = -rxt(k,375)*y(k,168)
         mat(k,1142) = -rxt(k,404)*y(k,168)
         mat(k,1359) = -rxt(k,435)*y(k,168)
         mat(k,1470) = -rxt(k,449)*y(k,168)
         mat(k,917) = -rxt(k,482)*y(k,168)
         mat(k,558) = -rxt(k,520)*y(k,168)
         mat(k,1005) = -rxt(k,537)*y(k,168)
         mat(k,1112) = -rxt(k,540)*y(k,168)
         mat(k,616) = -rxt(k,569)*y(k,168)
         mat(k,1578) = -rxt(k,575)*y(k,168)
         mat(k,1522) = .150_r8*rxt(k,389)*y(k,239)
         mat(k,1919) = mat(k,1919) + .150_r8*rxt(k,389)*y(k,233) + .150_r8*rxt(k,440) &
                      *y(k,248)
         mat(k,1490) = .150_r8*rxt(k,440)*y(k,239)
         mat(k,545) = -(rxt(k,576)*y(k,185))
         mat(k,1569) = -rxt(k,576)*y(k,170)
         mat(k,2447) = rxt(k,248)*y(k,74)
         mat(k,2335) = rxt(k,248)*y(k,21) + 2.000_r8*rxt(k,216)*y(k,74) + rxt(k,285) &
                      *y(k,127)
         mat(k,2185) = rxt(k,285)*y(k,74)
         mat(k,442) = -(rxt(k,565)*y(k,167) + rxt(k,566)*y(k,254))
         mat(k,1778) = -rxt(k,565)*y(k,171)
         mat(k,2059) = -rxt(k,566)*y(k,171)
         mat(k,788) = -(rxt(k,296)*y(k,157) + rxt(k,299)*y(k,254) + rxt(k,307) &
                      *y(k,127) + 4._r8*rxt(k,308)*y(k,172))
         mat(k,2512) = -rxt(k,296)*y(k,172)
         mat(k,2104) = -rxt(k,299)*y(k,172)
         mat(k,2187) = -rxt(k,307)*y(k,172)
         mat(k,2449) = rxt(k,284)*y(k,127)
         mat(k,2187) = mat(k,2187) + rxt(k,284)*y(k,21) + 2.000_r8*rxt(k,304)*y(k,127) &
                      + rxt(k,291)*y(k,159) + rxt(k,293)*y(k,168)
         mat(k,2643) = rxt(k,291)*y(k,127)
         mat(k,2278) = rxt(k,293)*y(k,127)
         mat(k,1213) = rxt(k,428)*y(k,254)
         mat(k,2489) = .100_r8*rxt(k,549)*y(k,258)
         mat(k,2039) = rxt(k,428)*y(k,112)
         mat(k,1237) = .100_r8*rxt(k,549)*y(k,157)
         mat(k,638) = -(rxt(k,399)*y(k,254))
         mat(k,2086) = -rxt(k,399)*y(k,174)
         mat(k,2404) = rxt(k,401)*y(k,233)
         mat(k,1496) = rxt(k,401)*y(k,158)
         mat(k,2390) = rxt(k,522)*y(k,224)
         mat(k,617) = rxt(k,522)*y(k,158)
         mat(k,556) = -(rxt(k,519)*y(k,158) + rxt(k,520)*y(k,168))
         mat(k,2398) = -rxt(k,519)*y(k,176)
         mat(k,2273) = -rxt(k,520)*y(k,176)
         mat(k,235) = .070_r8*rxt(k,506)*y(k,254)
         mat(k,2498) = rxt(k,504)*y(k,232)
         mat(k,201) = .060_r8*rxt(k,518)*y(k,254)
         mat(k,256) = .070_r8*rxt(k,534)*y(k,254)
         mat(k,733) = rxt(k,504)*y(k,157)
         mat(k,2074) = .070_r8*rxt(k,506)*y(k,82) + .060_r8*rxt(k,518)*y(k,177) &
                      + .070_r8*rxt(k,534)*y(k,220)
         mat(k,199) = -(rxt(k,518)*y(k,254))
         mat(k,2023) = -rxt(k,518)*y(k,177)
         mat(k,191) = .530_r8*rxt(k,495)*y(k,254)
         mat(k,2023) = mat(k,2023) + .530_r8*rxt(k,495)*y(k,7)
         mat(k,394) = -(rxt(k,521)*y(k,254))
         mat(k,2053) = -rxt(k,521)*y(k,178)
         mat(k,1832) = rxt(k,516)*y(k,255)
         mat(k,538) = rxt(k,516)*y(k,239)
         mat(k,646) = -(rxt(k,417)*y(k,254))
         mat(k,2087) = -rxt(k,417)*y(k,181)
         mat(k,1852) = rxt(k,415)*y(k,256)
         mat(k,872) = rxt(k,415)*y(k,239)
         mat(k,488) = -(rxt(k,421)*y(k,254))
         mat(k,2065) = -rxt(k,421)*y(k,182)
         mat(k,1838) = .850_r8*rxt(k,419)*y(k,257)
         mat(k,1294) = .850_r8*rxt(k,419)*y(k,239)
         mat(k,611) = -(rxt(k,569)*y(k,168) + rxt(k,572)*y(k,254))
         mat(k,2274) = -rxt(k,569)*y(k,183)
         mat(k,2082) = -rxt(k,572)*y(k,183)
         mat(k,1572) = -(rxt(k,570)*y(k,21) + rxt(k,571)*y(k,74) + rxt(k,573)*y(k,158) &
                      + rxt(k,575)*y(k,168) + rxt(k,576)*y(k,170) + rxt(k,577) &
                      *y(k,254))
         mat(k,2453) = -rxt(k,570)*y(k,185)
         mat(k,2340) = -rxt(k,571)*y(k,185)
         mat(k,2422) = -rxt(k,573)*y(k,185)
         mat(k,2309) = -rxt(k,575)*y(k,185)
         mat(k,547) = -rxt(k,576)*y(k,185)
         mat(k,2158) = -rxt(k,577)*y(k,185)
         mat(k,1788) = rxt(k,565)*y(k,171)
         mat(k,2309) = mat(k,2309) + rxt(k,569)*y(k,183)
         mat(k,446) = rxt(k,565)*y(k,167)
         mat(k,612) = rxt(k,569)*y(k,168) + rxt(k,572)*y(k,254)
         mat(k,2158) = mat(k,2158) + rxt(k,572)*y(k,183)
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
         mat(k,1053) = -(rxt(k,568)*y(k,254))
         mat(k,2126) = -rxt(k,568)*y(k,186)
         mat(k,2452) = rxt(k,559)*y(k,83) + rxt(k,570)*y(k,185)
         mat(k,2600) = rxt(k,561)*y(k,83)
         mat(k,2339) = rxt(k,571)*y(k,185)
         mat(k,1034) = rxt(k,559)*y(k,21) + rxt(k,561)*y(k,70) + rxt(k,562)*y(k,127) &
                      + rxt(k,563)*y(k,159) + (rxt(k,564)+.500_r8*rxt(k,578))*y(k,254)
         mat(k,2190) = rxt(k,562)*y(k,83)
         mat(k,2414) = rxt(k,573)*y(k,185)
         mat(k,2651) = rxt(k,563)*y(k,83)
         mat(k,2285) = rxt(k,575)*y(k,185)
         mat(k,546) = rxt(k,576)*y(k,185)
         mat(k,444) = rxt(k,566)*y(k,254)
         mat(k,1571) = rxt(k,570)*y(k,21) + rxt(k,571)*y(k,74) + rxt(k,573)*y(k,158) &
                      + rxt(k,575)*y(k,168) + rxt(k,576)*y(k,170) + rxt(k,577) &
                      *y(k,254)
         mat(k,2126) = mat(k,2126) + (rxt(k,564)+.500_r8*rxt(k,578))*y(k,83) &
                      + rxt(k,566)*y(k,171) + rxt(k,577)*y(k,185)
         mat(k,301) = -(rxt(k,579)*y(k,266))
         mat(k,2709) = -rxt(k,579)*y(k,187)
         mat(k,1052) = rxt(k,568)*y(k,254)
         mat(k,2042) = rxt(k,568)*y(k,186)
         mat(k,979) = .2202005_r8*rxt(k,632)*y(k,168)
         mat(k,1084) = .0508005_r8*rxt(k,648)*y(k,168)
         mat(k,2475) = .1279005_r8*rxt(k,631)*y(k,226) + .0097005_r8*rxt(k,636) &
                      *y(k,228) + .0003005_r8*rxt(k,639)*y(k,244) &
                      + .1056005_r8*rxt(k,643)*y(k,245) + .0245005_r8*rxt(k,647) &
                      *y(k,251) + .0154005_r8*rxt(k,653)*y(k,261) &
                      + .0063005_r8*rxt(k,657)*y(k,264)
         mat(k,2264) = .2202005_r8*rxt(k,632)*y(k,6) + .0508005_r8*rxt(k,648)*y(k,143)
         mat(k,57) = .5931005_r8*rxt(k,650)*y(k,254)
         mat(k,63) = .1279005_r8*rxt(k,631)*y(k,157) + .2202005_r8*rxt(k,630)*y(k,239)
         mat(k,69) = .0097005_r8*rxt(k,636)*y(k,157) + .0023005_r8*rxt(k,635)*y(k,239)
         mat(k,1813) = .2202005_r8*rxt(k,630)*y(k,226) + .0023005_r8*rxt(k,635) &
                      *y(k,228) + .0031005_r8*rxt(k,638)*y(k,244) &
                      + .2381005_r8*rxt(k,642)*y(k,245) + .0508005_r8*rxt(k,646) &
                      *y(k,251) + .1364005_r8*rxt(k,652)*y(k,261) &
                      + .1677005_r8*rxt(k,656)*y(k,264)
         mat(k,75) = .0003005_r8*rxt(k,639)*y(k,157) + .0031005_r8*rxt(k,638)*y(k,239)
         mat(k,81) = .1056005_r8*rxt(k,643)*y(k,157) + .2381005_r8*rxt(k,642)*y(k,239)
         mat(k,89) = .0245005_r8*rxt(k,647)*y(k,157) + .0508005_r8*rxt(k,646)*y(k,239)
         mat(k,2000) = .5931005_r8*rxt(k,650)*y(k,208)
         mat(k,95) = .0154005_r8*rxt(k,653)*y(k,157) + .1364005_r8*rxt(k,652)*y(k,239)
         mat(k,101) = .0063005_r8*rxt(k,657)*y(k,157) + .1677005_r8*rxt(k,656) &
                      *y(k,239)
         mat(k,980) = .2067005_r8*rxt(k,632)*y(k,168)
         mat(k,1085) = .1149005_r8*rxt(k,648)*y(k,168)
         mat(k,2476) = .1792005_r8*rxt(k,631)*y(k,226) + .0034005_r8*rxt(k,636) &
                      *y(k,228) + .0003005_r8*rxt(k,639)*y(k,244) &
                      + .1026005_r8*rxt(k,643)*y(k,245) + .0082005_r8*rxt(k,647) &
                      *y(k,251) + .0452005_r8*rxt(k,653)*y(k,261) &
                      + .0237005_r8*rxt(k,657)*y(k,264)
         mat(k,2265) = .2067005_r8*rxt(k,632)*y(k,6) + .1149005_r8*rxt(k,648)*y(k,143)
         mat(k,58) = .1534005_r8*rxt(k,650)*y(k,254)
         mat(k,64) = .1792005_r8*rxt(k,631)*y(k,157) + .2067005_r8*rxt(k,630)*y(k,239)
         mat(k,70) = .0034005_r8*rxt(k,636)*y(k,157) + .0008005_r8*rxt(k,635)*y(k,239)
         mat(k,1814) = .2067005_r8*rxt(k,630)*y(k,226) + .0008005_r8*rxt(k,635) &
                      *y(k,228) + .0035005_r8*rxt(k,638)*y(k,244) &
                      + .1308005_r8*rxt(k,642)*y(k,245) + .1149005_r8*rxt(k,646) &
                      *y(k,251) + .0101005_r8*rxt(k,652)*y(k,261) &
                      + .0174005_r8*rxt(k,656)*y(k,264)
         mat(k,76) = .0003005_r8*rxt(k,639)*y(k,157) + .0035005_r8*rxt(k,638)*y(k,239)
         mat(k,82) = .1026005_r8*rxt(k,643)*y(k,157) + .1308005_r8*rxt(k,642)*y(k,239)
         mat(k,90) = .0082005_r8*rxt(k,647)*y(k,157) + .1149005_r8*rxt(k,646)*y(k,239)
         mat(k,2001) = .1534005_r8*rxt(k,650)*y(k,208)
         mat(k,96) = .0452005_r8*rxt(k,653)*y(k,157) + .0101005_r8*rxt(k,652)*y(k,239)
         mat(k,102) = .0237005_r8*rxt(k,657)*y(k,157) + .0174005_r8*rxt(k,656) &
                      *y(k,239)
         mat(k,981) = .0653005_r8*rxt(k,632)*y(k,168)
         mat(k,1086) = .0348005_r8*rxt(k,648)*y(k,168)
         mat(k,2477) = .0676005_r8*rxt(k,631)*y(k,226) + .1579005_r8*rxt(k,636) &
                      *y(k,228) + .0073005_r8*rxt(k,639)*y(k,244) &
                      + .0521005_r8*rxt(k,643)*y(k,245) + .0772005_r8*rxt(k,647) &
                      *y(k,251) + .0966005_r8*rxt(k,653)*y(k,261) &
                      + .0025005_r8*rxt(k,657)*y(k,264)
         mat(k,2266) = .0653005_r8*rxt(k,632)*y(k,6) + .0348005_r8*rxt(k,648)*y(k,143)
         mat(k,59) = .0459005_r8*rxt(k,650)*y(k,254)
         mat(k,65) = .0676005_r8*rxt(k,631)*y(k,157) + .0653005_r8*rxt(k,630)*y(k,239)
         mat(k,71) = .1579005_r8*rxt(k,636)*y(k,157) + .0843005_r8*rxt(k,635)*y(k,239)
         mat(k,1815) = .0653005_r8*rxt(k,630)*y(k,226) + .0843005_r8*rxt(k,635) &
                      *y(k,228) + .0003005_r8*rxt(k,638)*y(k,244) &
                      + .0348005_r8*rxt(k,642)*y(k,245) + .0348005_r8*rxt(k,646) &
                      *y(k,251) + .0763005_r8*rxt(k,652)*y(k,261) + .086_r8*rxt(k,656) &
                      *y(k,264)
         mat(k,77) = .0073005_r8*rxt(k,639)*y(k,157) + .0003005_r8*rxt(k,638)*y(k,239)
         mat(k,83) = .0521005_r8*rxt(k,643)*y(k,157) + .0348005_r8*rxt(k,642)*y(k,239)
         mat(k,91) = .0772005_r8*rxt(k,647)*y(k,157) + .0348005_r8*rxt(k,646)*y(k,239)
         mat(k,2002) = .0459005_r8*rxt(k,650)*y(k,208)
         mat(k,97) = .0966005_r8*rxt(k,653)*y(k,157) + .0763005_r8*rxt(k,652)*y(k,239)
         mat(k,103) = .0025005_r8*rxt(k,657)*y(k,157) + .086_r8*rxt(k,656)*y(k,239)
         mat(k,982) = .1749305_r8*rxt(k,629)*y(k,159) + .1284005_r8*rxt(k,632) &
                      *y(k,168)
         mat(k,900) = .0590245_r8*rxt(k,637)*y(k,159) + .0033005_r8*rxt(k,640) &
                      *y(k,168)
         mat(k,1087) = .1749305_r8*rxt(k,645)*y(k,159) + .0554005_r8*rxt(k,648) &
                      *y(k,168)
         mat(k,2478) = .079_r8*rxt(k,631)*y(k,226) + .0059005_r8*rxt(k,636)*y(k,228) &
                      + .0057005_r8*rxt(k,639)*y(k,244) + .0143005_r8*rxt(k,643) &
                      *y(k,245) + .0332005_r8*rxt(k,647)*y(k,251) &
                      + .0073005_r8*rxt(k,653)*y(k,261) + .011_r8*rxt(k,657)*y(k,264)
         mat(k,2636) = .1749305_r8*rxt(k,629)*y(k,6) + .0590245_r8*rxt(k,637)*y(k,129) &
                      + .1749305_r8*rxt(k,645)*y(k,143)
         mat(k,2267) = .1284005_r8*rxt(k,632)*y(k,6) + .0033005_r8*rxt(k,640)*y(k,129) &
                      + .0554005_r8*rxt(k,648)*y(k,143)
         mat(k,60) = .0085005_r8*rxt(k,650)*y(k,254)
         mat(k,66) = .079_r8*rxt(k,631)*y(k,157) + .1284005_r8*rxt(k,630)*y(k,239)
         mat(k,72) = .0059005_r8*rxt(k,636)*y(k,157) + .0443005_r8*rxt(k,635)*y(k,239)
         mat(k,1816) = .1284005_r8*rxt(k,630)*y(k,226) + .0443005_r8*rxt(k,635) &
                      *y(k,228) + .0271005_r8*rxt(k,638)*y(k,244) &
                      + .0076005_r8*rxt(k,642)*y(k,245) + .0554005_r8*rxt(k,646) &
                      *y(k,251) + .2157005_r8*rxt(k,652)*y(k,261) &
                      + .0512005_r8*rxt(k,656)*y(k,264)
         mat(k,78) = .0057005_r8*rxt(k,639)*y(k,157) + .0271005_r8*rxt(k,638)*y(k,239)
         mat(k,84) = .0143005_r8*rxt(k,643)*y(k,157) + .0076005_r8*rxt(k,642)*y(k,239)
         mat(k,92) = .0332005_r8*rxt(k,647)*y(k,157) + .0554005_r8*rxt(k,646)*y(k,239)
         mat(k,2003) = .0085005_r8*rxt(k,650)*y(k,208)
         mat(k,98) = .0073005_r8*rxt(k,653)*y(k,157) + .2157005_r8*rxt(k,652)*y(k,239)
         mat(k,104) = .011_r8*rxt(k,657)*y(k,157) + .0512005_r8*rxt(k,656)*y(k,239)
         mat(k,983) = .5901905_r8*rxt(k,629)*y(k,159) + .114_r8*rxt(k,632)*y(k,168)
         mat(k,901) = .0250245_r8*rxt(k,637)*y(k,159)
         mat(k,1088) = .5901905_r8*rxt(k,645)*y(k,159) + .1278005_r8*rxt(k,648) &
                      *y(k,168)
         mat(k,2479) = .1254005_r8*rxt(k,631)*y(k,226) + .0536005_r8*rxt(k,636) &
                      *y(k,228) + .0623005_r8*rxt(k,639)*y(k,244) &
                      + .0166005_r8*rxt(k,643)*y(k,245) + .130_r8*rxt(k,647)*y(k,251) &
                      + .238_r8*rxt(k,653)*y(k,261) + .1185005_r8*rxt(k,657)*y(k,264)
         mat(k,2637) = .5901905_r8*rxt(k,629)*y(k,6) + .0250245_r8*rxt(k,637)*y(k,129) &
                      + .5901905_r8*rxt(k,645)*y(k,143)
         mat(k,2268) = .114_r8*rxt(k,632)*y(k,6) + .1278005_r8*rxt(k,648)*y(k,143)
         mat(k,61) = .0128005_r8*rxt(k,650)*y(k,254)
         mat(k,67) = .1254005_r8*rxt(k,631)*y(k,157) + .114_r8*rxt(k,630)*y(k,239)
         mat(k,73) = .0536005_r8*rxt(k,636)*y(k,157) + .1621005_r8*rxt(k,635)*y(k,239)
         mat(k,1817) = .114_r8*rxt(k,630)*y(k,226) + .1621005_r8*rxt(k,635)*y(k,228) &
                      + .0474005_r8*rxt(k,638)*y(k,244) + .0113005_r8*rxt(k,642) &
                      *y(k,245) + .1278005_r8*rxt(k,646)*y(k,251) &
                      + .0738005_r8*rxt(k,652)*y(k,261) + .1598005_r8*rxt(k,656) &
                      *y(k,264)
         mat(k,79) = .0623005_r8*rxt(k,639)*y(k,157) + .0474005_r8*rxt(k,638)*y(k,239)
         mat(k,85) = .0166005_r8*rxt(k,643)*y(k,157) + .0113005_r8*rxt(k,642)*y(k,239)
         mat(k,93) = .130_r8*rxt(k,647)*y(k,157) + .1278005_r8*rxt(k,646)*y(k,239)
         mat(k,2004) = .0128005_r8*rxt(k,650)*y(k,208)
         mat(k,99) = .238_r8*rxt(k,653)*y(k,157) + .0738005_r8*rxt(k,652)*y(k,239)
         mat(k,105) = .1185005_r8*rxt(k,657)*y(k,157) + .1598005_r8*rxt(k,656) &
                      *y(k,239)
         mat(k,62) = -(rxt(k,650)*y(k,254))
         mat(k,2005) = -rxt(k,650)*y(k,208)
         mat(k,228) = .100_r8*rxt(k,526)*y(k,254)
         mat(k,246) = .230_r8*rxt(k,528)*y(k,254)
         mat(k,2028) = .100_r8*rxt(k,526)*y(k,216) + .230_r8*rxt(k,528)*y(k,218)
         mat(k,770) = -(rxt(k,550)*y(k,254))
         mat(k,2102) = -rxt(k,550)*y(k,210)
         mat(k,1860) = rxt(k,548)*y(k,258)
         mat(k,1238) = rxt(k,548)*y(k,239)
         mat(k,726) = -(rxt(k,551)*y(k,254))
         mat(k,2097) = -rxt(k,551)*y(k,211)
         mat(k,2508) = .200_r8*rxt(k,544)*y(k,252) + .200_r8*rxt(k,554)*y(k,259)
         mat(k,1631) = .500_r8*rxt(k,542)*y(k,252)
         mat(k,1181) = .200_r8*rxt(k,544)*y(k,157) + .500_r8*rxt(k,542)*y(k,234)
         mat(k,1258) = .200_r8*rxt(k,554)*y(k,157)
         mat(k,579) = -(rxt(k,555)*y(k,254))
         mat(k,2078) = -rxt(k,555)*y(k,212)
         mat(k,1848) = rxt(k,553)*y(k,259)
         mat(k,1257) = rxt(k,553)*y(k,239)
         mat(k,1117) = -(rxt(k,556)*y(k,159) + rxt(k,557)*y(k,254))
         mat(k,2656) = -rxt(k,556)*y(k,213)
         mat(k,2131) = -rxt(k,557)*y(k,213)
         mat(k,993) = .330_r8*rxt(k,537)*y(k,168)
         mat(k,1098) = .330_r8*rxt(k,540)*y(k,168)
         mat(k,2529) = .800_r8*rxt(k,544)*y(k,252) + .800_r8*rxt(k,554)*y(k,259)
         mat(k,2656) = mat(k,2656) + rxt(k,545)*y(k,252)
         mat(k,2289) = .330_r8*rxt(k,537)*y(k,6) + .330_r8*rxt(k,540)*y(k,143)
         mat(k,727) = rxt(k,551)*y(k,254)
         mat(k,1641) = .500_r8*rxt(k,542)*y(k,252) + rxt(k,552)*y(k,259)
         mat(k,1183) = .800_r8*rxt(k,544)*y(k,157) + rxt(k,545)*y(k,159) &
                      + .500_r8*rxt(k,542)*y(k,234)
         mat(k,2131) = mat(k,2131) + rxt(k,551)*y(k,211)
         mat(k,1261) = .800_r8*rxt(k,554)*y(k,157) + rxt(k,552)*y(k,234)
         mat(k,1198) = -(rxt(k,558)*y(k,254))
         mat(k,2137) = -rxt(k,558)*y(k,214)
         mat(k,995) = .300_r8*rxt(k,537)*y(k,168)
         mat(k,1101) = .300_r8*rxt(k,540)*y(k,168)
         mat(k,2534) = .900_r8*rxt(k,549)*y(k,258)
         mat(k,2294) = .300_r8*rxt(k,537)*y(k,6) + .300_r8*rxt(k,540)*y(k,143)
         mat(k,1646) = rxt(k,547)*y(k,258)
         mat(k,1243) = .900_r8*rxt(k,549)*y(k,157) + rxt(k,547)*y(k,234)
         mat(k,690) = -(rxt(k,525)*y(k,254))
         mat(k,2092) = -rxt(k,525)*y(k,215)
         mat(k,1853) = rxt(k,523)*y(k,260)
         mat(k,834) = rxt(k,523)*y(k,239)
         mat(k,226) = -(rxt(k,526)*y(k,254))
         mat(k,2026) = -rxt(k,526)*y(k,216)
         mat(k,242) = -(rxt(k,492)*y(k,254))
         mat(k,2029) = -rxt(k,492)*y(k,217)
         mat(k,1826) = rxt(k,489)*y(k,262)
         mat(k,1307) = rxt(k,489)*y(k,239)
         mat(k,247) = -(rxt(k,528)*y(k,254))
         mat(k,2030) = -rxt(k,528)*y(k,218)
         mat(k,798) = -(rxt(k,531)*y(k,254))
         mat(k,2105) = -rxt(k,531)*y(k,219)
         mat(k,1862) = rxt(k,529)*y(k,263)
         mat(k,851) = rxt(k,529)*y(k,239)
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
         mat(k,255) = -(rxt(k,534)*y(k,254))
         mat(k,2031) = -rxt(k,534)*y(k,220)
         mat(k,248) = .150_r8*rxt(k,528)*y(k,254)
         mat(k,2031) = mat(k,2031) + .150_r8*rxt(k,528)*y(k,218)
         mat(k,518) = -(rxt(k,535)*y(k,254))
         mat(k,2070) = -rxt(k,535)*y(k,221)
         mat(k,1842) = rxt(k,532)*y(k,265)
         mat(k,595) = rxt(k,532)*y(k,239)
         mat(k,618) = -(rxt(k,493)*y(k,239) + rxt(k,494)*y(k,157) + rxt(k,522) &
                      *y(k,158))
         mat(k,1851) = -rxt(k,493)*y(k,224)
         mat(k,2503) = -rxt(k,494)*y(k,224)
         mat(k,2402) = -rxt(k,522)*y(k,224)
         mat(k,286) = rxt(k,499)*y(k,254)
         mat(k,2083) = rxt(k,499)*y(k,24)
         mat(k,1023) = -(rxt(k,454)*y(k,239) + (rxt(k,455) + rxt(k,456)) * y(k,157))
         mat(k,1875) = -rxt(k,454)*y(k,225)
         mat(k,2524) = -(rxt(k,455) + rxt(k,456)) * y(k,225)
         mat(k,744) = rxt(k,457)*y(k,254)
         mat(k,283) = rxt(k,458)*y(k,254)
         mat(k,2123) = rxt(k,457)*y(k,2) + rxt(k,458)*y(k,15)
         mat(k,68) = -(rxt(k,630)*y(k,239) + rxt(k,631)*y(k,157))
         mat(k,1818) = -rxt(k,630)*y(k,226)
         mat(k,2480) = -rxt(k,631)*y(k,226)
         mat(k,984) = rxt(k,633)*y(k,254)
         mat(k,2006) = rxt(k,633)*y(k,6)
         mat(k,588) = -(rxt(k,496)*y(k,239) + rxt(k,497)*y(k,157))
         mat(k,1849) = -rxt(k,496)*y(k,227)
         mat(k,2500) = -rxt(k,497)*y(k,227)
         mat(k,192) = .350_r8*rxt(k,495)*y(k,254)
         mat(k,508) = rxt(k,498)*y(k,254)
         mat(k,2079) = .350_r8*rxt(k,495)*y(k,7) + rxt(k,498)*y(k,8)
         mat(k,74) = -(rxt(k,635)*y(k,239) + rxt(k,636)*y(k,157))
         mat(k,1819) = -rxt(k,635)*y(k,228)
         mat(k,2481) = -rxt(k,636)*y(k,228)
         mat(k,188) = rxt(k,634)*y(k,254)
         mat(k,2007) = rxt(k,634)*y(k,7)
         mat(k,526) = -(rxt(k,500)*y(k,239) + rxt(k,502)*y(k,157))
         mat(k,1843) = -rxt(k,500)*y(k,229)
         mat(k,2494) = -rxt(k,502)*y(k,229)
         mat(k,387) = rxt(k,501)*y(k,254)
         mat(k,229) = .070_r8*rxt(k,526)*y(k,254)
         mat(k,249) = .060_r8*rxt(k,528)*y(k,254)
         mat(k,2071) = rxt(k,501)*y(k,25) + .070_r8*rxt(k,526)*y(k,216) &
                      + .060_r8*rxt(k,528)*y(k,218)
         mat(k,889) = -(4._r8*rxt(k,376)*y(k,230) + rxt(k,377)*y(k,234) + rxt(k,378) &
                      *y(k,239) + rxt(k,379)*y(k,157))
         mat(k,1634) = -rxt(k,377)*y(k,230)
         mat(k,1870) = -rxt(k,378)*y(k,230)
         mat(k,2519) = -rxt(k,379)*y(k,230)
         mat(k,400) = .500_r8*rxt(k,381)*y(k,254)
         mat(k,339) = rxt(k,382)*y(k,70) + rxt(k,383)*y(k,254)
         mat(k,2594) = rxt(k,382)*y(k,32)
         mat(k,2114) = .500_r8*rxt(k,381)*y(k,31) + rxt(k,383)*y(k,32)
         mat(k,950) = -(rxt(k,405)*y(k,234) + rxt(k,406)*y(k,239) + rxt(k,407) &
                      *y(k,157))
         mat(k,1637) = -rxt(k,405)*y(k,231)
         mat(k,1873) = -rxt(k,406)*y(k,231)
         mat(k,2522) = -rxt(k,407)*y(k,231)
         mat(k,501) = rxt(k,408)*y(k,254)
         mat(k,345) = rxt(k,412)*y(k,70) + rxt(k,409)*y(k,254)
         mat(k,2596) = rxt(k,412)*y(k,35)
         mat(k,2118) = rxt(k,408)*y(k,34) + rxt(k,409)*y(k,35)
         mat(k,734) = -(rxt(k,503)*y(k,239) + rxt(k,504)*y(k,157))
         mat(k,1857) = -rxt(k,503)*y(k,232)
         mat(k,2509) = -rxt(k,504)*y(k,232)
         mat(k,307) = rxt(k,505)*y(k,254)
         mat(k,2509) = mat(k,2509) + rxt(k,494)*y(k,224)
         mat(k,2276) = rxt(k,520)*y(k,176)
         mat(k,557) = rxt(k,520)*y(k,168)
         mat(k,619) = rxt(k,494)*y(k,157) + .400_r8*rxt(k,493)*y(k,239)
         mat(k,1857) = mat(k,1857) + .400_r8*rxt(k,493)*y(k,224)
         mat(k,2098) = rxt(k,505)*y(k,36)
         mat(k,1514) = -(4._r8*rxt(k,387)*y(k,233) + rxt(k,388)*y(k,234) + rxt(k,389) &
                      *y(k,239) + rxt(k,390)*y(k,157) + rxt(k,401)*y(k,158) + rxt(k,429) &
                      *y(k,246) + rxt(k,462)*y(k,241) + rxt(k,467)*y(k,242) + rxt(k,476) &
                      *y(k,243) + rxt(k,487)*y(k,262))
         mat(k,1661) = -rxt(k,388)*y(k,233)
         mat(k,1900) = -rxt(k,389)*y(k,233)
         mat(k,2550) = -rxt(k,390)*y(k,233)
         mat(k,2420) = -rxt(k,401)*y(k,233)
         mat(k,1441) = -rxt(k,429)*y(k,233)
         mat(k,1386) = -rxt(k,462)*y(k,233)
         mat(k,1419) = -rxt(k,467)*y(k,233)
         mat(k,1338) = -rxt(k,476)*y(k,233)
         mat(k,1316) = -rxt(k,487)*y(k,233)
         mat(k,1000) = .060_r8*rxt(k,537)*y(k,168)
         mat(k,1165) = rxt(k,384)*y(k,159) + rxt(k,385)*y(k,254)
         mat(k,1363) = rxt(k,410)*y(k,159) + rxt(k,411)*y(k,254)
         mat(k,721) = .500_r8*rxt(k,392)*y(k,254)
         mat(k,912) = .080_r8*rxt(k,482)*y(k,168)
         mat(k,1354) = .100_r8*rxt(k,435)*y(k,168)
         mat(k,1106) = .060_r8*rxt(k,540)*y(k,168)
         mat(k,1462) = .280_r8*rxt(k,449)*y(k,168)
         mat(k,2550) = mat(k,2550) + .530_r8*rxt(k,433)*y(k,246) + rxt(k,442)*y(k,248) &
                      + rxt(k,445)*y(k,250) + rxt(k,420)*y(k,257)
         mat(k,2679) = rxt(k,384)*y(k,54) + rxt(k,410)*y(k,58) + .530_r8*rxt(k,432) &
                      *y(k,246) + rxt(k,443)*y(k,248)
         mat(k,2308) = .060_r8*rxt(k,537)*y(k,6) + .080_r8*rxt(k,482)*y(k,129) &
                      + .100_r8*rxt(k,435)*y(k,138) + .060_r8*rxt(k,540)*y(k,143) &
                      + .280_r8*rxt(k,449)*y(k,144)
         mat(k,1201) = .650_r8*rxt(k,558)*y(k,254)
         mat(k,1514) = mat(k,1514) + .530_r8*rxt(k,429)*y(k,246)
         mat(k,1661) = mat(k,1661) + .260_r8*rxt(k,430)*y(k,246) + rxt(k,439)*y(k,248) &
                      + .300_r8*rxt(k,418)*y(k,257)
         mat(k,1900) = mat(k,1900) + .450_r8*rxt(k,440)*y(k,248) + .200_r8*rxt(k,444) &
                      *y(k,250) + .150_r8*rxt(k,419)*y(k,257)
         mat(k,1441) = mat(k,1441) + .530_r8*rxt(k,433)*y(k,157) + .530_r8*rxt(k,432) &
                      *y(k,159) + .530_r8*rxt(k,429)*y(k,233) + .260_r8*rxt(k,430) &
                      *y(k,234)
         mat(k,1483) = rxt(k,442)*y(k,157) + rxt(k,443)*y(k,159) + rxt(k,439)*y(k,234) &
                      + .450_r8*rxt(k,440)*y(k,239) + 4.000_r8*rxt(k,441)*y(k,248)
         mat(k,765) = rxt(k,445)*y(k,157) + .200_r8*rxt(k,444)*y(k,239)
         mat(k,2154) = rxt(k,385)*y(k,54) + rxt(k,411)*y(k,58) + .500_r8*rxt(k,392) &
                      *y(k,60) + .650_r8*rxt(k,558)*y(k,214)
         mat(k,1299) = rxt(k,420)*y(k,157) + .300_r8*rxt(k,418)*y(k,234) &
                      + .150_r8*rxt(k,419)*y(k,239)
         mat(k,1663) = -(rxt(k,213)*y(k,74) + (rxt(k,335) + rxt(k,336)) * y(k,68) &
                      + (4._r8*rxt(k,355) + 4._r8*rxt(k,356)) * y(k,234) + rxt(k,357) &
                      *y(k,239) + rxt(k,358)*y(k,157) + rxt(k,377)*y(k,230) + rxt(k,388) &
                      *y(k,233) + rxt(k,405)*y(k,231) + rxt(k,418)*y(k,257) + rxt(k,430) &
                      *y(k,246) + rxt(k,439)*y(k,248) + rxt(k,463)*y(k,241) + rxt(k,468) &
                      *y(k,242) + rxt(k,477)*y(k,243) + rxt(k,488)*y(k,262) + rxt(k,542) &
                      *y(k,252) + rxt(k,547)*y(k,258) + rxt(k,552)*y(k,259))
         mat(k,2341) = -rxt(k,213)*y(k,234)
         mat(k,1069) = -(rxt(k,335) + rxt(k,336)) * y(k,234)
         mat(k,1906) = -rxt(k,357)*y(k,234)
         mat(k,2553) = -rxt(k,358)*y(k,234)
         mat(k,891) = -rxt(k,377)*y(k,234)
         mat(k,1516) = -rxt(k,388)*y(k,234)
         mat(k,953) = -rxt(k,405)*y(k,234)
         mat(k,1300) = -rxt(k,418)*y(k,234)
         mat(k,1442) = -rxt(k,430)*y(k,234)
         mat(k,1484) = -rxt(k,439)*y(k,234)
         mat(k,1387) = -rxt(k,463)*y(k,234)
         mat(k,1420) = -rxt(k,468)*y(k,234)
         mat(k,1339) = -rxt(k,477)*y(k,234)
         mat(k,1317) = -rxt(k,488)*y(k,234)
         mat(k,1188) = -rxt(k,542)*y(k,234)
         mat(k,1248) = -rxt(k,547)*y(k,234)
         mat(k,1269) = -rxt(k,552)*y(k,234)
         mat(k,1137) = .280_r8*rxt(k,404)*y(k,168)
         mat(k,782) = rxt(k,391)*y(k,254)
         mat(k,471) = .700_r8*rxt(k,360)*y(k,254)
         mat(k,1591) = rxt(k,205)*y(k,70) + rxt(k,309)*y(k,89) + rxt(k,367)*y(k,253) &
                      + rxt(k,361)*y(k,254)
         mat(k,2614) = rxt(k,205)*y(k,64)
         mat(k,964) = rxt(k,309)*y(k,64)
         mat(k,913) = .050_r8*rxt(k,482)*y(k,168)
         mat(k,2553) = mat(k,2553) + rxt(k,390)*y(k,233) + .830_r8*rxt(k,508)*y(k,235) &
                      + .170_r8*rxt(k,514)*y(k,249)
         mat(k,2311) = .280_r8*rxt(k,404)*y(k,33) + .050_r8*rxt(k,482)*y(k,129)
         mat(k,1516) = mat(k,1516) + rxt(k,390)*y(k,157) + 4.000_r8*rxt(k,387) &
                      *y(k,233) + .900_r8*rxt(k,388)*y(k,234) + .450_r8*rxt(k,389) &
                      *y(k,239) + rxt(k,462)*y(k,241) + rxt(k,467)*y(k,242) &
                      + rxt(k,476)*y(k,243) + rxt(k,429)*y(k,246) + rxt(k,438) &
                      *y(k,248) + rxt(k,487)*y(k,262)
         mat(k,1663) = mat(k,1663) + .900_r8*rxt(k,388)*y(k,233)
         mat(k,867) = .830_r8*rxt(k,508)*y(k,157) + .330_r8*rxt(k,507)*y(k,239)
         mat(k,1906) = mat(k,1906) + .450_r8*rxt(k,389)*y(k,233) + .330_r8*rxt(k,507) &
                      *y(k,235) + .070_r8*rxt(k,513)*y(k,249)
         mat(k,1387) = mat(k,1387) + rxt(k,462)*y(k,233)
         mat(k,1420) = mat(k,1420) + rxt(k,467)*y(k,233)
         mat(k,1339) = mat(k,1339) + rxt(k,476)*y(k,233)
         mat(k,1442) = mat(k,1442) + rxt(k,429)*y(k,233)
         mat(k,1484) = mat(k,1484) + rxt(k,438)*y(k,233)
         mat(k,974) = .170_r8*rxt(k,514)*y(k,157) + .070_r8*rxt(k,513)*y(k,239)
         mat(k,1977) = rxt(k,367)*y(k,64)
         mat(k,2161) = rxt(k,391)*y(k,59) + .700_r8*rxt(k,360)*y(k,63) + rxt(k,361) &
                      *y(k,64)
         mat(k,1317) = mat(k,1317) + rxt(k,487)*y(k,233)
         mat(k,864) = -(rxt(k,507)*y(k,239) + rxt(k,508)*y(k,157) + rxt(k,509) &
                      *y(k,158))
         mat(k,1868) = -rxt(k,507)*y(k,235)
         mat(k,2517) = -rxt(k,508)*y(k,235)
         mat(k,2408) = -rxt(k,509)*y(k,235)
         mat(k,654) = -((rxt(k,426) + rxt(k,427)) * y(k,157))
         mat(k,2504) = -(rxt(k,426) + rxt(k,427)) * y(k,236)
         mat(k,424) = rxt(k,425)*y(k,254)
         mat(k,2088) = rxt(k,425)*y(k,16)
         mat(k,2490) = .750_r8*rxt(k,394)*y(k,238)
         mat(k,818) = .750_r8*rxt(k,394)*y(k,157)
         mat(k,819) = -(rxt(k,393)*y(k,239) + rxt(k,394)*y(k,157))
         mat(k,1864) = -rxt(k,393)*y(k,238)
         mat(k,2513) = -rxt(k,394)*y(k,238)
         mat(k,631) = rxt(k,400)*y(k,254)
         mat(k,2107) = rxt(k,400)*y(k,28)
         mat(k,1912) = -((rxt(k,164) + rxt(k,165) + rxt(k,166)) * y(k,92) + rxt(k,168) &
                      *y(k,167) + rxt(k,169)*y(k,168) + rxt(k,173)*y(k,254) &
                      + 4._r8*rxt(k,178)*y(k,239) + rxt(k,188)*y(k,159) + rxt(k,193) &
                      *y(k,157) + rxt(k,198)*y(k,158) + (rxt(k,208) + rxt(k,209) &
                      ) * y(k,70) + rxt(k,217)*y(k,74) + rxt(k,244)*y(k,17) + rxt(k,251) &
                      *y(k,21) + rxt(k,273)*y(k,116) + rxt(k,288)*y(k,127) + rxt(k,337) &
                      *y(k,68) + rxt(k,351)*y(k,51) + rxt(k,357)*y(k,234) + rxt(k,364) &
                      *y(k,240) + rxt(k,378)*y(k,230) + rxt(k,389)*y(k,233) + rxt(k,393) &
                      *y(k,238) + rxt(k,406)*y(k,231) + rxt(k,415)*y(k,256) + rxt(k,419) &
                      *y(k,257) + rxt(k,431)*y(k,246) + rxt(k,440)*y(k,248) + rxt(k,444) &
                      *y(k,250) + rxt(k,454)*y(k,225) + rxt(k,464)*y(k,241) + rxt(k,469) &
                      *y(k,242) + rxt(k,478)*y(k,243) + rxt(k,489)*y(k,262) + rxt(k,493) &
                      *y(k,224) + rxt(k,496)*y(k,227) + rxt(k,500)*y(k,229) + rxt(k,503) &
                      *y(k,232) + rxt(k,507)*y(k,235) + rxt(k,510)*y(k,247) + rxt(k,513) &
                      *y(k,249) + rxt(k,516)*y(k,255) + rxt(k,523)*y(k,260) + rxt(k,529) &
                      *y(k,263) + rxt(k,532)*y(k,265) + rxt(k,543)*y(k,252) + rxt(k,548) &
                      *y(k,258) + rxt(k,553)*y(k,259))
         mat(k,2248) = -(rxt(k,164) + rxt(k,165) + rxt(k,166)) * y(k,239)
         mat(k,1797) = -rxt(k,168)*y(k,239)
         mat(k,2317) = -rxt(k,169)*y(k,239)
         mat(k,2167) = -rxt(k,173)*y(k,239)
         mat(k,2690) = -rxt(k,188)*y(k,239)
         mat(k,2559) = -rxt(k,193)*y(k,239)
         mat(k,2431) = -rxt(k,198)*y(k,239)
         mat(k,2620) = -(rxt(k,208) + rxt(k,209)) * y(k,239)
         mat(k,2346) = -rxt(k,217)*y(k,239)
         mat(k,2225) = -rxt(k,244)*y(k,239)
         mat(k,2459) = -rxt(k,251)*y(k,239)
         mat(k,2374) = -rxt(k,273)*y(k,239)
         mat(k,2197) = -rxt(k,288)*y(k,239)
         mat(k,1072) = -rxt(k,337)*y(k,239)
         mat(k,1715) = -rxt(k,351)*y(k,239)
         mat(k,1668) = -rxt(k,357)*y(k,239)
         mat(k,535) = -rxt(k,364)*y(k,239)
         mat(k,893) = -rxt(k,378)*y(k,239)
         mat(k,1519) = -rxt(k,389)*y(k,239)
         mat(k,822) = -rxt(k,393)*y(k,239)
         mat(k,955) = -rxt(k,406)*y(k,239)
         mat(k,877) = -rxt(k,415)*y(k,239)
         mat(k,1302) = -rxt(k,419)*y(k,239)
         mat(k,1445) = -rxt(k,431)*y(k,239)
         mat(k,1487) = -rxt(k,440)*y(k,239)
         mat(k,766) = -rxt(k,444)*y(k,239)
         mat(k,1029) = -rxt(k,454)*y(k,239)
         mat(k,1390) = -rxt(k,464)*y(k,239)
         mat(k,1423) = -rxt(k,469)*y(k,239)
         mat(k,1342) = -rxt(k,478)*y(k,239)
         mat(k,1320) = -rxt(k,489)*y(k,239)
         mat(k,620) = -rxt(k,493)*y(k,239)
         mat(k,591) = -rxt(k,496)*y(k,239)
         mat(k,528) = -rxt(k,500)*y(k,239)
         mat(k,735) = -rxt(k,503)*y(k,239)
         mat(k,868) = -rxt(k,507)*y(k,239)
         mat(k,829) = -rxt(k,510)*y(k,239)
         mat(k,975) = -rxt(k,513)*y(k,239)
         mat(k,541) = -rxt(k,516)*y(k,239)
         mat(k,843) = -rxt(k,523)*y(k,239)
         mat(k,860) = -rxt(k,529)*y(k,239)
         mat(k,599) = -rxt(k,532)*y(k,239)
         mat(k,1190) = -rxt(k,543)*y(k,239)
         mat(k,1251) = -rxt(k,548)*y(k,239)
         mat(k,1272) = -rxt(k,553)*y(k,239)
         mat(k,1003) = .570_r8*rxt(k,537)*y(k,168)
         mat(k,193) = .650_r8*rxt(k,495)*y(k,254)
         mat(k,2225) = mat(k,2225) + rxt(k,243)*y(k,51)
         mat(k,2459) = mat(k,2459) + rxt(k,258)*y(k,254)
         mat(k,333) = .350_r8*rxt(k,373)*y(k,254)
         mat(k,635) = .130_r8*rxt(k,375)*y(k,168)
         mat(k,298) = rxt(k,380)*y(k,254)
         mat(k,1140) = .280_r8*rxt(k,404)*y(k,168)
         mat(k,1715) = mat(k,1715) + rxt(k,243)*y(k,17) + rxt(k,204)*y(k,70) &
                      + rxt(k,352)*y(k,159) + rxt(k,353)*y(k,167)
         mat(k,682) = rxt(k,324)*y(k,70) + rxt(k,325)*y(k,254)
         mat(k,454) = rxt(k,327)*y(k,70) + rxt(k,328)*y(k,254)
         mat(k,119) = rxt(k,386)*y(k,254)
         mat(k,418) = rxt(k,329)*y(k,70) + rxt(k,330)*y(k,254)
         mat(k,884) = rxt(k,359)*y(k,254)
         mat(k,1595) = rxt(k,368)*y(k,253)
         mat(k,1072) = mat(k,1072) + rxt(k,339)*y(k,157) + rxt(k,340)*y(k,159) + ( &
                      + 2.000_r8*rxt(k,335)+rxt(k,336))*y(k,234)
         mat(k,2620) = mat(k,2620) + rxt(k,204)*y(k,51) + rxt(k,324)*y(k,52) &
                      + rxt(k,327)*y(k,55) + rxt(k,329)*y(k,61) + rxt(k,207)*y(k,95)
         mat(k,2346) = mat(k,2346) + rxt(k,213)*y(k,234) + rxt(k,224)*y(k,254)
         mat(k,1211) = rxt(k,371)*y(k,254)
         mat(k,236) = .730_r8*rxt(k,506)*y(k,254)
         mat(k,1036) = .500_r8*rxt(k,578)*y(k,254)
         mat(k,1178) = rxt(k,397)*y(k,254)
         mat(k,1064) = rxt(k,398)*y(k,254)
         mat(k,702) = rxt(k,207)*y(k,70) + rxt(k,163)*y(k,167) + rxt(k,172)*y(k,254)
         mat(k,205) = rxt(k,362)*y(k,254)
         mat(k,1049) = rxt(k,363)*y(k,254)
         mat(k,1225) = rxt(k,428)*y(k,254)
         mat(k,1235) = rxt(k,413)*y(k,254)
         mat(k,2197) = mat(k,2197) + rxt(k,294)*y(k,254)
         mat(k,915) = .370_r8*rxt(k,482)*y(k,168)
         mat(k,715) = .300_r8*rxt(k,473)*y(k,254)
         mat(k,667) = rxt(k,474)*y(k,254)
         mat(k,461) = rxt(k,481)*y(k,254)
         mat(k,1357) = .140_r8*rxt(k,435)*y(k,168)
         mat(k,371) = .200_r8*rxt(k,437)*y(k,254)
         mat(k,675) = .500_r8*rxt(k,448)*y(k,254)
         mat(k,1110) = .570_r8*rxt(k,540)*y(k,168)
         mat(k,1467) = .280_r8*rxt(k,449)*y(k,168)
         mat(k,467) = rxt(k,485)*y(k,254)
         mat(k,1158) = rxt(k,486)*y(k,254)
         mat(k,2559) = mat(k,2559) + rxt(k,339)*y(k,68) + rxt(k,455)*y(k,225) &
                      + rxt(k,497)*y(k,227) + rxt(k,502)*y(k,229) + rxt(k,379) &
                      *y(k,230) + rxt(k,407)*y(k,231) + rxt(k,358)*y(k,234) &
                      + .170_r8*rxt(k,508)*y(k,235) + rxt(k,426)*y(k,236) &
                      + .250_r8*rxt(k,394)*y(k,238) + rxt(k,366)*y(k,240) &
                      + .920_r8*rxt(k,465)*y(k,241) + .920_r8*rxt(k,471)*y(k,242) &
                      + rxt(k,479)*y(k,243) + .470_r8*rxt(k,433)*y(k,246) &
                      + .400_r8*rxt(k,511)*y(k,247) + .830_r8*rxt(k,514)*y(k,249) &
                      + rxt(k,517)*y(k,255) + rxt(k,416)*y(k,256) + .900_r8*rxt(k,549) &
                      *y(k,258) + .800_r8*rxt(k,554)*y(k,259) + rxt(k,524)*y(k,260) &
                      + rxt(k,490)*y(k,262) + rxt(k,530)*y(k,263) + rxt(k,533) &
                      *y(k,265)
         mat(k,2690) = mat(k,2690) + rxt(k,352)*y(k,51) + rxt(k,340)*y(k,68) &
                      + rxt(k,466)*y(k,241) + rxt(k,472)*y(k,242) + rxt(k,480) &
                      *y(k,243) + .470_r8*rxt(k,432)*y(k,246) + rxt(k,191)*y(k,254) &
                      + rxt(k,491)*y(k,262)
         mat(k,1797) = mat(k,1797) + rxt(k,353)*y(k,51) + rxt(k,163)*y(k,95)
         mat(k,2317) = mat(k,2317) + .570_r8*rxt(k,537)*y(k,6) + .130_r8*rxt(k,375) &
                      *y(k,28) + .280_r8*rxt(k,404)*y(k,33) + .370_r8*rxt(k,482) &
                      *y(k,129) + .140_r8*rxt(k,435)*y(k,138) + .570_r8*rxt(k,540) &
                      *y(k,143) + .280_r8*rxt(k,449)*y(k,144) + rxt(k,175)*y(k,254)
         mat(k,202) = .800_r8*rxt(k,518)*y(k,254)
         mat(k,1056) = rxt(k,568)*y(k,254)
         mat(k,1205) = .200_r8*rxt(k,558)*y(k,254)
         mat(k,231) = .280_r8*rxt(k,526)*y(k,254)
         mat(k,253) = .380_r8*rxt(k,528)*y(k,254)
         mat(k,258) = .630_r8*rxt(k,534)*y(k,254)
         mat(k,1029) = mat(k,1029) + rxt(k,455)*y(k,157)
         mat(k,591) = mat(k,591) + rxt(k,497)*y(k,157)
         mat(k,528) = mat(k,528) + rxt(k,502)*y(k,157)
         mat(k,893) = mat(k,893) + rxt(k,379)*y(k,157) + 2.400_r8*rxt(k,376)*y(k,230) &
                      + rxt(k,377)*y(k,234)
         mat(k,955) = mat(k,955) + rxt(k,407)*y(k,157) + rxt(k,405)*y(k,234)
         mat(k,1519) = mat(k,1519) + .900_r8*rxt(k,388)*y(k,234) + rxt(k,462)*y(k,241) &
                      + rxt(k,467)*y(k,242) + rxt(k,476)*y(k,243) + .470_r8*rxt(k,429) &
                      *y(k,246) + rxt(k,487)*y(k,262)
         mat(k,1668) = mat(k,1668) + (2.000_r8*rxt(k,335)+rxt(k,336))*y(k,68) &
                      + rxt(k,213)*y(k,74) + rxt(k,358)*y(k,157) + rxt(k,377)*y(k,230) &
                      + rxt(k,405)*y(k,231) + .900_r8*rxt(k,388)*y(k,233) &
                      + 4.000_r8*rxt(k,355)*y(k,234) + rxt(k,463)*y(k,241) &
                      + rxt(k,468)*y(k,242) + 1.200_r8*rxt(k,477)*y(k,243) &
                      + .730_r8*rxt(k,430)*y(k,246) + rxt(k,439)*y(k,248) &
                      + .500_r8*rxt(k,542)*y(k,252) + .300_r8*rxt(k,418)*y(k,257) &
                      + rxt(k,547)*y(k,258) + rxt(k,552)*y(k,259) + .800_r8*rxt(k,488) &
                      *y(k,262)
         mat(k,868) = mat(k,868) + .170_r8*rxt(k,508)*y(k,157) + .070_r8*rxt(k,507) &
                      *y(k,239)
         mat(k,659) = rxt(k,426)*y(k,157)
         mat(k,822) = mat(k,822) + .250_r8*rxt(k,394)*y(k,157)
         mat(k,1912) = mat(k,1912) + .070_r8*rxt(k,507)*y(k,235) + .160_r8*rxt(k,510) &
                      *y(k,247) + .330_r8*rxt(k,513)*y(k,249)
         mat(k,535) = mat(k,535) + rxt(k,366)*y(k,157)
         mat(k,1390) = mat(k,1390) + .920_r8*rxt(k,465)*y(k,157) + rxt(k,466)*y(k,159) &
                      + rxt(k,462)*y(k,233) + rxt(k,463)*y(k,234)
         mat(k,1423) = mat(k,1423) + .920_r8*rxt(k,471)*y(k,157) + rxt(k,472)*y(k,159) &
                      + rxt(k,467)*y(k,233) + rxt(k,468)*y(k,234)
         mat(k,1342) = mat(k,1342) + rxt(k,479)*y(k,157) + rxt(k,480)*y(k,159) &
                      + rxt(k,476)*y(k,233) + 1.200_r8*rxt(k,477)*y(k,234)
         mat(k,1445) = mat(k,1445) + .470_r8*rxt(k,433)*y(k,157) + .470_r8*rxt(k,432) &
                      *y(k,159) + .470_r8*rxt(k,429)*y(k,233) + .730_r8*rxt(k,430) &
                      *y(k,234)
         mat(k,829) = mat(k,829) + .400_r8*rxt(k,511)*y(k,157) + .160_r8*rxt(k,510) &
                      *y(k,239)
         mat(k,1487) = mat(k,1487) + rxt(k,439)*y(k,234)
         mat(k,975) = mat(k,975) + .830_r8*rxt(k,514)*y(k,157) + .330_r8*rxt(k,513) &
                      *y(k,239)
         mat(k,1190) = mat(k,1190) + .500_r8*rxt(k,542)*y(k,234)
         mat(k,1983) = rxt(k,368)*y(k,64)
         mat(k,2167) = mat(k,2167) + .650_r8*rxt(k,495)*y(k,7) + rxt(k,258)*y(k,21) &
                      + .350_r8*rxt(k,373)*y(k,27) + rxt(k,380)*y(k,30) + rxt(k,325) &
                      *y(k,52) + rxt(k,328)*y(k,55) + rxt(k,386)*y(k,56) + rxt(k,330) &
                      *y(k,61) + rxt(k,359)*y(k,62) + rxt(k,224)*y(k,74) + rxt(k,371) &
                      *y(k,77) + .730_r8*rxt(k,506)*y(k,82) + .500_r8*rxt(k,578) &
                      *y(k,83) + rxt(k,397)*y(k,90) + rxt(k,398)*y(k,91) + rxt(k,172) &
                      *y(k,95) + rxt(k,362)*y(k,103) + rxt(k,363)*y(k,104) &
                      + rxt(k,428)*y(k,112) + rxt(k,413)*y(k,114) + rxt(k,294) &
                      *y(k,127) + .300_r8*rxt(k,473)*y(k,130) + rxt(k,474)*y(k,131) &
                      + rxt(k,481)*y(k,132) + .200_r8*rxt(k,437)*y(k,139) &
                      + .500_r8*rxt(k,448)*y(k,142) + rxt(k,485)*y(k,148) + rxt(k,486) &
                      *y(k,149) + rxt(k,191)*y(k,159) + rxt(k,175)*y(k,168) &
                      + .800_r8*rxt(k,518)*y(k,177) + rxt(k,568)*y(k,186) &
                      + .200_r8*rxt(k,558)*y(k,214) + .280_r8*rxt(k,526)*y(k,216) &
                      + .380_r8*rxt(k,528)*y(k,218) + .630_r8*rxt(k,534)*y(k,220)
         mat(k,541) = mat(k,541) + rxt(k,517)*y(k,157)
         mat(k,877) = mat(k,877) + rxt(k,416)*y(k,157)
         mat(k,1302) = mat(k,1302) + .300_r8*rxt(k,418)*y(k,234)
         mat(k,1251) = mat(k,1251) + .900_r8*rxt(k,549)*y(k,157) + rxt(k,547)*y(k,234)
         mat(k,1272) = mat(k,1272) + .800_r8*rxt(k,554)*y(k,157) + rxt(k,552)*y(k,234)
         mat(k,843) = mat(k,843) + rxt(k,524)*y(k,157)
         mat(k,1320) = mat(k,1320) + rxt(k,490)*y(k,157) + rxt(k,491)*y(k,159) &
                      + rxt(k,487)*y(k,233) + .800_r8*rxt(k,488)*y(k,234)
         mat(k,860) = mat(k,860) + rxt(k,530)*y(k,157)
         mat(k,599) = mat(k,599) + rxt(k,533)*y(k,157)
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
         mat(k,532) = -(rxt(k,364)*y(k,239) + rxt(k,366)*y(k,157))
         mat(k,1844) = -rxt(k,364)*y(k,240)
         mat(k,2495) = -rxt(k,366)*y(k,240)
         mat(k,1702) = rxt(k,351)*y(k,239)
         mat(k,1844) = mat(k,1844) + rxt(k,351)*y(k,51)
         mat(k,1382) = -(rxt(k,462)*y(k,233) + rxt(k,463)*y(k,234) + rxt(k,464) &
                      *y(k,239) + rxt(k,465)*y(k,157) + rxt(k,466)*y(k,159))
         mat(k,1509) = -rxt(k,462)*y(k,241)
         mat(k,1656) = -rxt(k,463)*y(k,241)
         mat(k,1895) = -rxt(k,464)*y(k,241)
         mat(k,2545) = -rxt(k,465)*y(k,241)
         mat(k,2674) = -rxt(k,466)*y(k,241)
         mat(k,909) = .600_r8*rxt(k,483)*y(k,254)
         mat(k,2149) = .600_r8*rxt(k,483)*y(k,129)
         mat(k,1415) = -(rxt(k,467)*y(k,233) + rxt(k,468)*y(k,234) + rxt(k,469) &
                      *y(k,239) + rxt(k,471)*y(k,157) + rxt(k,472)*y(k,159))
         mat(k,1510) = -rxt(k,467)*y(k,242)
         mat(k,1657) = -rxt(k,468)*y(k,242)
         mat(k,1896) = -rxt(k,469)*y(k,242)
         mat(k,2546) = -rxt(k,471)*y(k,242)
         mat(k,2675) = -rxt(k,472)*y(k,242)
         mat(k,910) = .400_r8*rxt(k,483)*y(k,254)
         mat(k,2150) = .400_r8*rxt(k,483)*y(k,129)
         mat(k,1334) = -(rxt(k,476)*y(k,233) + rxt(k,477)*y(k,234) + rxt(k,478) &
                      *y(k,239) + rxt(k,479)*y(k,157) + rxt(k,480)*y(k,159))
         mat(k,1506) = -rxt(k,476)*y(k,243)
         mat(k,1653) = -rxt(k,477)*y(k,243)
         mat(k,1892) = -rxt(k,478)*y(k,243)
         mat(k,2542) = -rxt(k,479)*y(k,243)
         mat(k,2671) = -rxt(k,480)*y(k,243)
         mat(k,907) = rxt(k,475)*y(k,159)
         mat(k,2671) = mat(k,2671) + rxt(k,475)*y(k,129)
         mat(k,80) = -(rxt(k,638)*y(k,239) + rxt(k,639)*y(k,157))
         mat(k,1820) = -rxt(k,638)*y(k,244)
         mat(k,2482) = -rxt(k,639)*y(k,244)
         mat(k,902) = rxt(k,641)*y(k,254)
         mat(k,2008) = rxt(k,641)*y(k,129)
         mat(k,86) = -(rxt(k,642)*y(k,239) + rxt(k,643)*y(k,157))
         mat(k,1821) = -rxt(k,642)*y(k,245)
         mat(k,2483) = -rxt(k,643)*y(k,245)
         mat(k,87) = rxt(k,644)*y(k,254)
         mat(k,2009) = rxt(k,644)*y(k,134)
         mat(k,1439) = -(rxt(k,429)*y(k,233) + rxt(k,430)*y(k,234) + rxt(k,431) &
                      *y(k,239) + rxt(k,432)*y(k,159) + (rxt(k,433) + rxt(k,434) &
                      ) * y(k,157))
         mat(k,1511) = -rxt(k,429)*y(k,246)
         mat(k,1658) = -rxt(k,430)*y(k,246)
         mat(k,1897) = -rxt(k,431)*y(k,246)
         mat(k,2676) = -rxt(k,432)*y(k,246)
         mat(k,2547) = -(rxt(k,433) + rxt(k,434)) * y(k,246)
         mat(k,1352) = .500_r8*rxt(k,436)*y(k,254)
         mat(k,369) = .200_r8*rxt(k,437)*y(k,254)
         mat(k,1459) = rxt(k,450)*y(k,254)
         mat(k,2151) = .500_r8*rxt(k,436)*y(k,138) + .200_r8*rxt(k,437)*y(k,139) &
                      + rxt(k,450)*y(k,144)
         mat(k,826) = -(rxt(k,510)*y(k,239) + rxt(k,511)*y(k,157) + rxt(k,512) &
                      *y(k,158))
         mat(k,1865) = -rxt(k,510)*y(k,247)
         mat(k,2514) = -rxt(k,511)*y(k,247)
         mat(k,2407) = -rxt(k,512)*y(k,247)
         mat(k,1482) = -(rxt(k,438)*y(k,233) + rxt(k,439)*y(k,234) + rxt(k,440) &
                      *y(k,239) + 4._r8*rxt(k,441)*y(k,248) + rxt(k,442)*y(k,157) &
                      + rxt(k,443)*y(k,159) + rxt(k,451)*y(k,158))
         mat(k,1513) = -rxt(k,438)*y(k,248)
         mat(k,1660) = -rxt(k,439)*y(k,248)
         mat(k,1899) = -rxt(k,440)*y(k,248)
         mat(k,2549) = -rxt(k,442)*y(k,248)
         mat(k,2678) = -rxt(k,443)*y(k,248)
         mat(k,2419) = -rxt(k,451)*y(k,248)
         mat(k,1353) = .500_r8*rxt(k,436)*y(k,254)
         mat(k,370) = .500_r8*rxt(k,437)*y(k,254)
         mat(k,2153) = .500_r8*rxt(k,436)*y(k,138) + .500_r8*rxt(k,437)*y(k,139)
         mat(k,970) = -(rxt(k,513)*y(k,239) + rxt(k,514)*y(k,157) + rxt(k,515) &
                      *y(k,158))
         mat(k,1874) = -rxt(k,513)*y(k,249)
         mat(k,2523) = -rxt(k,514)*y(k,249)
         mat(k,2412) = -rxt(k,515)*y(k,249)
         mat(k,763) = -(rxt(k,444)*y(k,239) + rxt(k,445)*y(k,157))
         mat(k,1859) = -rxt(k,444)*y(k,250)
         mat(k,2511) = -rxt(k,445)*y(k,250)
         mat(k,604) = rxt(k,446)*y(k,254)
         mat(k,374) = rxt(k,447)*y(k,254)
         mat(k,2101) = rxt(k,446)*y(k,140) + rxt(k,447)*y(k,141)
         mat(k,94) = -(rxt(k,646)*y(k,239) + rxt(k,647)*y(k,157))
         mat(k,1822) = -rxt(k,646)*y(k,251)
         mat(k,2484) = -rxt(k,647)*y(k,251)
         mat(k,1089) = rxt(k,649)*y(k,254)
         mat(k,2011) = rxt(k,649)*y(k,143)
         mat(k,1184) = -(rxt(k,542)*y(k,234) + rxt(k,543)*y(k,239) + rxt(k,544) &
                      *y(k,157) + rxt(k,545)*y(k,159))
         mat(k,1645) = -rxt(k,542)*y(k,252)
         mat(k,1883) = -rxt(k,543)*y(k,252)
         mat(k,2533) = -rxt(k,544)*y(k,252)
         mat(k,2661) = -rxt(k,545)*y(k,252)
         mat(k,994) = rxt(k,536)*y(k,159)
         mat(k,1100) = rxt(k,539)*y(k,159)
         mat(k,2661) = mat(k,2661) + rxt(k,536)*y(k,6) + rxt(k,539)*y(k,143) &
                      + .500_r8*rxt(k,556)*y(k,213)
         mat(k,478) = rxt(k,546)*y(k,254)
         mat(k,1118) = .500_r8*rxt(k,556)*y(k,159)
         mat(k,2136) = rxt(k,546)*y(k,161)
         mat(k,1985) = -(rxt(k,153)*y(k,93) + rxt(k,154)*y(k,266) + (rxt(k,157) &
                      + rxt(k,158)) * y(k,168) + (rxt(k,196) + rxt(k,197)) * y(k,146) &
                      + rxt(k,231)*y(k,37) + rxt(k,232)*y(k,38) + rxt(k,233)*y(k,40) &
                      + rxt(k,234)*y(k,41) + rxt(k,235)*y(k,42) + rxt(k,236)*y(k,43) &
                      + rxt(k,237)*y(k,44) + (rxt(k,238) + rxt(k,239)) * y(k,102) &
                      + rxt(k,262)*y(k,39) + rxt(k,263)*y(k,66) + rxt(k,264)*y(k,94) &
                      + (rxt(k,265) + rxt(k,266)) * y(k,98) + rxt(k,313)*y(k,80) &
                      + rxt(k,314)*y(k,81) + rxt(k,346)*y(k,45) + rxt(k,347)*y(k,52) &
                      + rxt(k,348)*y(k,99) + rxt(k,349)*y(k,100) + rxt(k,350)*y(k,101) &
                      + (rxt(k,367) + rxt(k,368) + rxt(k,369)) * y(k,64) + rxt(k,370) &
                      *y(k,103))
         mat(k,1551) = -rxt(k,153)*y(k,253)
         mat(k,2724) = -rxt(k,154)*y(k,253)
         mat(k,2319) = -(rxt(k,157) + rxt(k,158)) * y(k,253)
         mat(k,217) = -(rxt(k,196) + rxt(k,197)) * y(k,253)
         mat(k,116) = -rxt(k,231)*y(k,253)
         mat(k,170) = -rxt(k,232)*y(k,253)
         mat(k,131) = -rxt(k,233)*y(k,253)
         mat(k,181) = -rxt(k,234)*y(k,253)
         mat(k,135) = -rxt(k,235)*y(k,253)
         mat(k,186) = -rxt(k,236)*y(k,253)
         mat(k,139) = -rxt(k,237)*y(k,253)
         mat(k,1766) = -(rxt(k,238) + rxt(k,239)) * y(k,253)
         mat(k,176) = -rxt(k,262)*y(k,253)
         mat(k,514) = -rxt(k,263)*y(k,253)
         mat(k,127) = -rxt(k,264)*y(k,253)
         mat(k,1535) = -(rxt(k,265) + rxt(k,266)) * y(k,253)
         mat(k,276) = -rxt(k,313)*y(k,253)
         mat(k,267) = -rxt(k,314)*y(k,253)
         mat(k,566) = -rxt(k,346)*y(k,253)
         mat(k,683) = -rxt(k,347)*y(k,253)
         mat(k,262) = -rxt(k,348)*y(k,253)
         mat(k,271) = -rxt(k,349)*y(k,253)
         mat(k,353) = -rxt(k,350)*y(k,253)
         mat(k,1597) = -(rxt(k,367) + rxt(k,368) + rxt(k,369)) * y(k,253)
         mat(k,206) = -rxt(k,370)*y(k,253)
         mat(k,2170) = -(rxt(k,171)*y(k,93) + rxt(k,172)*y(k,95) + rxt(k,173)*y(k,239) &
                      + rxt(k,174)*y(k,167) + rxt(k,175)*y(k,168) + (4._r8*rxt(k,176) &
                      + 4._r8*rxt(k,177)) * y(k,254) + rxt(k,179)*y(k,108) + rxt(k,191) &
                      *y(k,159) + rxt(k,192)*y(k,145) + rxt(k,200)*y(k,158) + rxt(k,201) &
                      *y(k,107) + rxt(k,211)*y(k,73) + rxt(k,222)*y(k,75) + (rxt(k,224) &
                      + rxt(k,225)) * y(k,74) + rxt(k,227)*y(k,102) + rxt(k,230) &
                      *y(k,110) + rxt(k,242)*y(k,18) + rxt(k,258)*y(k,21) + rxt(k,260) &
                      *y(k,98) + rxt(k,268)*y(k,111) + rxt(k,271)*y(k,117) + rxt(k,294) &
                      *y(k,127) + rxt(k,295)*y(k,106) + rxt(k,299)*y(k,172) + rxt(k,316) &
                      *y(k,26) + rxt(k,318)*y(k,29) + rxt(k,320)*y(k,45) + rxt(k,321) &
                      *y(k,46) + rxt(k,323)*y(k,47) + rxt(k,325)*y(k,52) + rxt(k,326) &
                      *y(k,53) + rxt(k,328)*y(k,55) + rxt(k,330)*y(k,61) + rxt(k,331) &
                      *y(k,65) + rxt(k,333)*y(k,66) + rxt(k,334)*y(k,67) + rxt(k,342) &
                      *y(k,69) + rxt(k,343)*y(k,99) + rxt(k,344)*y(k,100) + rxt(k,345) &
                      *y(k,101) + rxt(k,354)*y(k,51) + rxt(k,359)*y(k,62) + rxt(k,360) &
                      *y(k,63) + rxt(k,361)*y(k,64) + rxt(k,362)*y(k,103) + rxt(k,363) &
                      *y(k,104) + rxt(k,371)*y(k,77) + rxt(k,373)*y(k,27) + rxt(k,380) &
                      *y(k,30) + rxt(k,381)*y(k,31) + rxt(k,383)*y(k,32) + rxt(k,385) &
                      *y(k,54) + rxt(k,386)*y(k,56) + rxt(k,391)*y(k,59) + rxt(k,392) &
                      *y(k,60) + rxt(k,397)*y(k,90) + rxt(k,398)*y(k,91) + rxt(k,399) &
                      *y(k,174) + rxt(k,400)*y(k,28) + rxt(k,408)*y(k,34) + rxt(k,409) &
                      *y(k,35) + rxt(k,411)*y(k,58) + rxt(k,413)*y(k,114) + rxt(k,414) &
                      *y(k,160) + rxt(k,417)*y(k,181) + rxt(k,421)*y(k,182) + rxt(k,422) &
                      *y(k,33) + rxt(k,423)*y(k,57) + rxt(k,425)*y(k,16) + rxt(k,428) &
                      *y(k,112) + rxt(k,436)*y(k,138) + rxt(k,437)*y(k,139) + rxt(k,446) &
                      *y(k,140) + rxt(k,447)*y(k,141) + rxt(k,448)*y(k,142) + rxt(k,450) &
                      *y(k,144) + rxt(k,453)*y(k,1) + rxt(k,457)*y(k,2) + rxt(k,458) &
                      *y(k,15) + rxt(k,459)*y(k,113) + rxt(k,460)*y(k,115) + rxt(k,461) &
                      *y(k,124) + rxt(k,473)*y(k,130) + rxt(k,474)*y(k,131) + rxt(k,481) &
                      *y(k,132) + rxt(k,483)*y(k,129) + rxt(k,484)*y(k,133) + rxt(k,485) &
                      *y(k,148) + rxt(k,486)*y(k,149) + rxt(k,492)*y(k,217) + rxt(k,495) &
                      *y(k,7) + rxt(k,498)*y(k,8) + rxt(k,499)*y(k,24) + rxt(k,501) &
                      *y(k,25) + rxt(k,505)*y(k,36) + rxt(k,506)*y(k,82) + rxt(k,518) &
                      *y(k,177) + rxt(k,521)*y(k,178) + rxt(k,525)*y(k,215) + rxt(k,526) &
                      *y(k,216) + rxt(k,528)*y(k,218) + rxt(k,531)*y(k,219) + rxt(k,534) &
                      *y(k,220) + rxt(k,535)*y(k,221) + rxt(k,538)*y(k,6) + rxt(k,541) &
                      *y(k,143) + rxt(k,546)*y(k,161) + rxt(k,550)*y(k,210) + rxt(k,551) &
                      *y(k,211) + rxt(k,555)*y(k,212) + rxt(k,557)*y(k,213) + rxt(k,558) &
                      *y(k,214) + (rxt(k,564) + rxt(k,578)) * y(k,83) + rxt(k,566) &
                      *y(k,171) + rxt(k,568)*y(k,186) + rxt(k,572)*y(k,183) + rxt(k,577) &
                      *y(k,185) + rxt(k,597)*y(k,153))
         mat(k,1552) = -rxt(k,171)*y(k,254)
         mat(k,703) = -rxt(k,172)*y(k,254)
         mat(k,1915) = -rxt(k,173)*y(k,254)
         mat(k,1800) = -rxt(k,174)*y(k,254)
         mat(k,2320) = -rxt(k,175)*y(k,254)
         mat(k,496) = -rxt(k,179)*y(k,254)
         mat(k,2693) = -rxt(k,191)*y(k,254)
         mat(k,575) = -rxt(k,192)*y(k,254)
         mat(k,2434) = -rxt(k,200)*y(k,254)
         mat(k,1940) = -rxt(k,201)*y(k,254)
         mat(k,627) = -rxt(k,211)*y(k,254)
         mat(k,1013) = -rxt(k,222)*y(k,254)
         mat(k,2349) = -(rxt(k,224) + rxt(k,225)) * y(k,254)
         mat(k,1767) = -rxt(k,227)*y(k,254)
         mat(k,1744) = -rxt(k,230)*y(k,254)
         mat(k,562) = -rxt(k,242)*y(k,254)
         mat(k,2462) = -rxt(k,258)*y(k,254)
         mat(k,1536) = -rxt(k,260)*y(k,254)
         mat(k,1692) = -rxt(k,268)*y(k,254)
         mat(k,1562) = -rxt(k,271)*y(k,254)
         mat(k,2200) = -rxt(k,294)*y(k,254)
         mat(k,1287) = -rxt(k,295)*y(k,254)
         mat(k,789) = -rxt(k,299)*y(k,254)
         mat(k,221) = -rxt(k,316)*y(k,254)
         mat(k,293) = -rxt(k,318)*y(k,254)
         mat(k,567) = -rxt(k,320)*y(k,254)
         mat(k,142) = -rxt(k,321)*y(k,254)
         mat(k,360) = -rxt(k,323)*y(k,254)
         mat(k,684) = -rxt(k,325)*y(k,254)
         mat(k,146) = -rxt(k,326)*y(k,254)
         mat(k,455) = -rxt(k,328)*y(k,254)
         mat(k,419) = -rxt(k,330)*y(k,254)
         mat(k,150) = -rxt(k,331)*y(k,254)
         mat(k,515) = -rxt(k,333)*y(k,254)
         mat(k,122) = -rxt(k,334)*y(k,254)
         mat(k,412) = -rxt(k,342)*y(k,254)
         mat(k,263) = -rxt(k,343)*y(k,254)
         mat(k,272) = -rxt(k,344)*y(k,254)
         mat(k,354) = -rxt(k,345)*y(k,254)
         mat(k,1718) = -rxt(k,354)*y(k,254)
         mat(k,885) = -rxt(k,359)*y(k,254)
         mat(k,473) = -rxt(k,360)*y(k,254)
         mat(k,1598) = -rxt(k,361)*y(k,254)
         mat(k,207) = -rxt(k,362)*y(k,254)
         mat(k,1050) = -rxt(k,363)*y(k,254)
         mat(k,1212) = -rxt(k,371)*y(k,254)
         mat(k,334) = -rxt(k,373)*y(k,254)
         mat(k,299) = -rxt(k,380)*y(k,254)
         mat(k,403) = -rxt(k,381)*y(k,254)
         mat(k,341) = -rxt(k,383)*y(k,254)
         mat(k,1169) = -rxt(k,385)*y(k,254)
         mat(k,120) = -rxt(k,386)*y(k,254)
         mat(k,783) = -rxt(k,391)*y(k,254)
         mat(k,724) = -rxt(k,392)*y(k,254)
         mat(k,1179) = -rxt(k,397)*y(k,254)
         mat(k,1065) = -rxt(k,398)*y(k,254)
         mat(k,643) = -rxt(k,399)*y(k,254)
         mat(k,636) = -rxt(k,400)*y(k,254)
         mat(k,504) = -rxt(k,408)*y(k,254)
         mat(k,347) = -rxt(k,409)*y(k,254)
         mat(k,1367) = -rxt(k,411)*y(k,254)
         mat(k,1236) = -rxt(k,413)*y(k,254)
         mat(k,935) = -rxt(k,414)*y(k,254)
         mat(k,652) = -rxt(k,417)*y(k,254)
         mat(k,492) = -rxt(k,421)*y(k,254)
         mat(k,1141) = -rxt(k,422)*y(k,254)
         mat(k,1082) = -rxt(k,423)*y(k,254)
         mat(k,428) = -rxt(k,425)*y(k,254)
         mat(k,1227) = -rxt(k,428)*y(k,254)
         mat(k,1358) = -rxt(k,436)*y(k,254)
         mat(k,372) = -rxt(k,437)*y(k,254)
         mat(k,607) = -rxt(k,446)*y(k,254)
         mat(k,377) = -rxt(k,447)*y(k,254)
         mat(k,676) = -rxt(k,448)*y(k,254)
         mat(k,1469) = -rxt(k,450)*y(k,254)
         mat(k,760) = -rxt(k,453)*y(k,254)
         mat(k,750) = -rxt(k,457)*y(k,254)
         mat(k,284) = -rxt(k,458)*y(k,254)
         mat(k,281) = -rxt(k,459)*y(k,254)
         mat(k,393) = -rxt(k,460)*y(k,254)
         mat(k,164) = -rxt(k,461)*y(k,254)
         mat(k,717) = -rxt(k,473)*y(k,254)
         mat(k,669) = -rxt(k,474)*y(k,254)
         mat(k,462) = -rxt(k,481)*y(k,254)
         mat(k,916) = -rxt(k,483)*y(k,254)
         mat(k,816) = -rxt(k,484)*y(k,254)
         mat(k,469) = -rxt(k,485)*y(k,254)
         mat(k,1160) = -rxt(k,486)*y(k,254)
         mat(k,244) = -rxt(k,492)*y(k,254)
         mat(k,194) = -rxt(k,495)*y(k,254)
         mat(k,511) = -rxt(k,498)*y(k,254)
         mat(k,287) = -rxt(k,499)*y(k,254)
         mat(k,390) = -rxt(k,501)*y(k,254)
         mat(k,308) = -rxt(k,505)*y(k,254)
         mat(k,237) = -rxt(k,506)*y(k,254)
         mat(k,203) = -rxt(k,518)*y(k,254)
         mat(k,398) = -rxt(k,521)*y(k,254)
         mat(k,698) = -rxt(k,525)*y(k,254)
         mat(k,232) = -rxt(k,526)*y(k,254)
         mat(k,254) = -rxt(k,528)*y(k,254)
         mat(k,808) = -rxt(k,531)*y(k,254)
         mat(k,259) = -rxt(k,534)*y(k,254)
         mat(k,523) = -rxt(k,535)*y(k,254)
         mat(k,1004) = -rxt(k,538)*y(k,254)
         mat(k,1111) = -rxt(k,541)*y(k,254)
         mat(k,480) = -rxt(k,546)*y(k,254)
         mat(k,779) = -rxt(k,550)*y(k,254)
         mat(k,730) = -rxt(k,551)*y(k,254)
         mat(k,585) = -rxt(k,555)*y(k,254)
         mat(k,1123) = -rxt(k,557)*y(k,254)
         mat(k,1206) = -rxt(k,558)*y(k,254)
         mat(k,1038) = -(rxt(k,564) + rxt(k,578)) * y(k,254)
         mat(k,448) = -rxt(k,566)*y(k,254)
         mat(k,1057) = -rxt(k,568)*y(k,254)
         mat(k,614) = -rxt(k,572)*y(k,254)
         mat(k,1575) = -rxt(k,577)*y(k,254)
         mat(k,113) = -rxt(k,597)*y(k,254)
         mat(k,1004) = mat(k,1004) + .630_r8*rxt(k,537)*y(k,168)
         mat(k,334) = mat(k,334) + .650_r8*rxt(k,373)*y(k,254)
         mat(k,636) = mat(k,636) + .130_r8*rxt(k,375)*y(k,168)
         mat(k,403) = mat(k,403) + .500_r8*rxt(k,381)*y(k,254)
         mat(k,1141) = mat(k,1141) + .360_r8*rxt(k,404)*y(k,168)
         mat(k,1718) = mat(k,1718) + rxt(k,353)*y(k,167)
         mat(k,473) = mat(k,473) + .300_r8*rxt(k,360)*y(k,254)
         mat(k,1598) = mat(k,1598) + rxt(k,367)*y(k,253)
         mat(k,2623) = rxt(k,209)*y(k,239)
         mat(k,966) = rxt(k,311)*y(k,266)
         mat(k,2251) = rxt(k,170)*y(k,168) + 2.000_r8*rxt(k,165)*y(k,239)
         mat(k,1552) = mat(k,1552) + rxt(k,162)*y(k,167) + rxt(k,153)*y(k,253)
         mat(k,703) = mat(k,703) + rxt(k,163)*y(k,167)
         mat(k,1536) = mat(k,1536) + rxt(k,259)*y(k,167) + rxt(k,265)*y(k,253)
         mat(k,1767) = mat(k,1767) + rxt(k,226)*y(k,167) + rxt(k,238)*y(k,253)
         mat(k,207) = mat(k,207) + rxt(k,370)*y(k,253)
         mat(k,1616) = rxt(k,261)*y(k,167)
         mat(k,1744) = mat(k,1744) + rxt(k,229)*y(k,167)
         mat(k,916) = mat(k,916) + .320_r8*rxt(k,482)*y(k,168)
         mat(k,816) = mat(k,816) + .600_r8*rxt(k,484)*y(k,254)
         mat(k,1358) = mat(k,1358) + .240_r8*rxt(k,435)*y(k,168)
         mat(k,372) = mat(k,372) + .100_r8*rxt(k,437)*y(k,254)
         mat(k,1111) = mat(k,1111) + .630_r8*rxt(k,540)*y(k,168)
         mat(k,1469) = mat(k,1469) + .360_r8*rxt(k,449)*y(k,168)
         mat(k,2562) = rxt(k,193)*y(k,239)
         mat(k,2693) = mat(k,2693) + rxt(k,188)*y(k,239)
         mat(k,1800) = mat(k,1800) + rxt(k,353)*y(k,51) + rxt(k,162)*y(k,93) &
                      + rxt(k,163)*y(k,95) + rxt(k,259)*y(k,98) + rxt(k,226)*y(k,102) &
                      + rxt(k,261)*y(k,109) + rxt(k,229)*y(k,110) + rxt(k,168) &
                      *y(k,239)
         mat(k,2320) = mat(k,2320) + .630_r8*rxt(k,537)*y(k,6) + .130_r8*rxt(k,375) &
                      *y(k,28) + .360_r8*rxt(k,404)*y(k,33) + rxt(k,170)*y(k,92) &
                      + .320_r8*rxt(k,482)*y(k,129) + .240_r8*rxt(k,435)*y(k,138) &
                      + .630_r8*rxt(k,540)*y(k,143) + .360_r8*rxt(k,449)*y(k,144) &
                      + rxt(k,169)*y(k,239)
         mat(k,652) = mat(k,652) + .500_r8*rxt(k,417)*y(k,254)
         mat(k,244) = mat(k,244) + .500_r8*rxt(k,492)*y(k,254)
         mat(k,621) = .400_r8*rxt(k,493)*y(k,239)
         mat(k,1521) = .450_r8*rxt(k,389)*y(k,239)
         mat(k,869) = .400_r8*rxt(k,507)*y(k,239)
         mat(k,1915) = mat(k,1915) + rxt(k,209)*y(k,70) + 2.000_r8*rxt(k,165)*y(k,92) &
                      + rxt(k,193)*y(k,157) + rxt(k,188)*y(k,159) + rxt(k,168) &
                      *y(k,167) + rxt(k,169)*y(k,168) + .400_r8*rxt(k,493)*y(k,224) &
                      + .450_r8*rxt(k,389)*y(k,233) + .400_r8*rxt(k,507)*y(k,235) &
                      + .450_r8*rxt(k,440)*y(k,248) + .400_r8*rxt(k,513)*y(k,249) &
                      + .200_r8*rxt(k,444)*y(k,250) + .150_r8*rxt(k,419)*y(k,257)
         mat(k,1489) = .450_r8*rxt(k,440)*y(k,239)
         mat(k,976) = .400_r8*rxt(k,513)*y(k,239)
         mat(k,767) = .200_r8*rxt(k,444)*y(k,239)
         mat(k,1986) = rxt(k,367)*y(k,64) + rxt(k,153)*y(k,93) + rxt(k,265)*y(k,98) &
                      + rxt(k,238)*y(k,102) + rxt(k,370)*y(k,103) &
                      + 2.000_r8*rxt(k,154)*y(k,266)
         mat(k,2170) = mat(k,2170) + .650_r8*rxt(k,373)*y(k,27) + .500_r8*rxt(k,381) &
                      *y(k,31) + .300_r8*rxt(k,360)*y(k,63) + .600_r8*rxt(k,484) &
                      *y(k,133) + .100_r8*rxt(k,437)*y(k,139) + .500_r8*rxt(k,417) &
                      *y(k,181) + .500_r8*rxt(k,492)*y(k,217)
         mat(k,1303) = .150_r8*rxt(k,419)*y(k,239)
         mat(k,2725) = rxt(k,311)*y(k,89) + 2.000_r8*rxt(k,154)*y(k,253)
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
         mat(k,539) = -(rxt(k,516)*y(k,239) + rxt(k,517)*y(k,157))
         mat(k,1845) = -rxt(k,516)*y(k,255)
         mat(k,2496) = -rxt(k,517)*y(k,255)
         mat(k,234) = .200_r8*rxt(k,506)*y(k,254)
         mat(k,200) = .140_r8*rxt(k,518)*y(k,254)
         mat(k,395) = rxt(k,521)*y(k,254)
         mat(k,2072) = .200_r8*rxt(k,506)*y(k,82) + .140_r8*rxt(k,518)*y(k,177) &
                      + rxt(k,521)*y(k,178)
         mat(k,873) = -(rxt(k,415)*y(k,239) + rxt(k,416)*y(k,157))
         mat(k,1869) = -rxt(k,415)*y(k,256)
         mat(k,2518) = -rxt(k,416)*y(k,256)
         mat(k,1126) = rxt(k,422)*y(k,254)
         mat(k,647) = .500_r8*rxt(k,417)*y(k,254)
         mat(k,2112) = rxt(k,422)*y(k,33) + .500_r8*rxt(k,417)*y(k,181)
         mat(k,1297) = -(rxt(k,418)*y(k,234) + rxt(k,419)*y(k,239) + rxt(k,420) &
                      *y(k,157))
         mat(k,1651) = -rxt(k,418)*y(k,257)
         mat(k,1890) = -rxt(k,419)*y(k,257)
         mat(k,2540) = -rxt(k,420)*y(k,257)
         mat(k,998) = .060_r8*rxt(k,537)*y(k,168)
         mat(k,1079) = rxt(k,423)*y(k,254)
         mat(k,1104) = .060_r8*rxt(k,540)*y(k,168)
         mat(k,2299) = .060_r8*rxt(k,537)*y(k,6) + .060_r8*rxt(k,540)*y(k,143)
         mat(k,489) = rxt(k,421)*y(k,254)
         mat(k,1200) = .150_r8*rxt(k,558)*y(k,254)
         mat(k,2144) = rxt(k,423)*y(k,57) + rxt(k,421)*y(k,182) + .150_r8*rxt(k,558) &
                      *y(k,214)
         mat(k,1245) = -(rxt(k,547)*y(k,234) + rxt(k,548)*y(k,239) + rxt(k,549) &
                      *y(k,157))
         mat(k,1649) = -rxt(k,547)*y(k,258)
         mat(k,1887) = -rxt(k,548)*y(k,258)
         mat(k,2538) = -rxt(k,549)*y(k,258)
         mat(k,2666) = .500_r8*rxt(k,556)*y(k,213)
         mat(k,776) = rxt(k,550)*y(k,254)
         mat(k,1121) = .500_r8*rxt(k,556)*y(k,159) + rxt(k,557)*y(k,254)
         mat(k,2141) = rxt(k,550)*y(k,210) + rxt(k,557)*y(k,213)
         mat(k,1266) = -(rxt(k,552)*y(k,234) + rxt(k,553)*y(k,239) + rxt(k,554) &
                      *y(k,157))
         mat(k,1650) = -rxt(k,552)*y(k,259)
         mat(k,1888) = -rxt(k,553)*y(k,259)
         mat(k,2539) = -rxt(k,554)*y(k,259)
         mat(k,997) = rxt(k,538)*y(k,254)
         mat(k,1103) = rxt(k,541)*y(k,254)
         mat(k,582) = rxt(k,555)*y(k,254)
         mat(k,2142) = rxt(k,538)*y(k,6) + rxt(k,541)*y(k,143) + rxt(k,555)*y(k,212)
         mat(k,837) = -(rxt(k,523)*y(k,239) + rxt(k,524)*y(k,157))
         mat(k,1866) = -rxt(k,523)*y(k,260)
         mat(k,2515) = -rxt(k,524)*y(k,260)
         mat(k,693) = rxt(k,525)*y(k,254)
         mat(k,230) = .650_r8*rxt(k,526)*y(k,254)
         mat(k,2109) = rxt(k,525)*y(k,215) + .650_r8*rxt(k,526)*y(k,216)
         mat(k,100) = -(rxt(k,652)*y(k,239) + rxt(k,653)*y(k,157))
         mat(k,1823) = -rxt(k,652)*y(k,261)
         mat(k,2485) = -rxt(k,653)*y(k,261)
         mat(k,225) = rxt(k,651)*y(k,254)
         mat(k,2012) = rxt(k,651)*y(k,216)
         mat(k,1314) = -(rxt(k,487)*y(k,233) + rxt(k,488)*y(k,234) + rxt(k,489) &
                      *y(k,239) + rxt(k,490)*y(k,157) + rxt(k,491)*y(k,159))
         mat(k,1505) = -rxt(k,487)*y(k,262)
         mat(k,1652) = -rxt(k,488)*y(k,262)
         mat(k,1891) = -rxt(k,489)*y(k,262)
         mat(k,2541) = -rxt(k,490)*y(k,262)
         mat(k,2670) = -rxt(k,491)*y(k,262)
         mat(k,279) = rxt(k,459)*y(k,254)
         mat(k,392) = rxt(k,460)*y(k,254)
         mat(k,163) = rxt(k,461)*y(k,254)
         mat(k,811) = .400_r8*rxt(k,484)*y(k,254)
         mat(k,243) = .500_r8*rxt(k,492)*y(k,254)
         mat(k,2145) = rxt(k,459)*y(k,113) + rxt(k,460)*y(k,115) + rxt(k,461)*y(k,124) &
                      + .400_r8*rxt(k,484)*y(k,133) + .500_r8*rxt(k,492)*y(k,217)
         mat(k,853) = -(rxt(k,529)*y(k,239) + rxt(k,530)*y(k,157))
         mat(k,1867) = -rxt(k,529)*y(k,263)
         mat(k,2516) = -rxt(k,530)*y(k,263)
         mat(k,250) = .560_r8*rxt(k,528)*y(k,254)
         mat(k,800) = rxt(k,531)*y(k,254)
         mat(k,2110) = .560_r8*rxt(k,528)*y(k,218) + rxt(k,531)*y(k,219)
         mat(k,106) = -(rxt(k,656)*y(k,239) + rxt(k,657)*y(k,157))
         mat(k,1824) = -rxt(k,656)*y(k,264)
         mat(k,2486) = -rxt(k,657)*y(k,264)
         mat(k,245) = rxt(k,655)*y(k,254)
         mat(k,2013) = rxt(k,655)*y(k,218)
         mat(k,596) = -(rxt(k,532)*y(k,239) + rxt(k,533)*y(k,157))
         mat(k,1850) = -rxt(k,532)*y(k,265)
         mat(k,2501) = -rxt(k,533)*y(k,265)
         mat(k,257) = .300_r8*rxt(k,534)*y(k,254)
         mat(k,519) = rxt(k,535)*y(k,254)
         mat(k,2080) = .300_r8*rxt(k,534)*y(k,220) + rxt(k,535)*y(k,221)
         mat(k,2737) = -(rxt(k,154)*y(k,253) + rxt(k,298)*y(k,121) + rxt(k,311) &
                      *y(k,89) + rxt(k,579)*y(k,187))
         mat(k,1998) = -rxt(k,154)*y(k,266)
         mat(k,211) = -rxt(k,298)*y(k,266)
         mat(k,969) = -rxt(k,311)*y(k,266)
         mat(k,304) = -rxt(k,579)*y(k,266)
         mat(k,295) = rxt(k,318)*y(k,254)
         mat(k,343) = rxt(k,383)*y(k,254)
         mat(k,505) = rxt(k,408)*y(k,254)
         mat(k,349) = rxt(k,409)*y(k,254)
         mat(k,570) = rxt(k,320)*y(k,254)
         mat(k,362) = rxt(k,323)*y(k,254)
         mat(k,1730) = rxt(k,354)*y(k,254)
         mat(k,687) = rxt(k,325)*y(k,254)
         mat(k,148) = rxt(k,326)*y(k,254)
         mat(k,1171) = rxt(k,385)*y(k,254)
         mat(k,457) = rxt(k,328)*y(k,254)
         mat(k,1083) = rxt(k,423)*y(k,254)
         mat(k,1369) = rxt(k,411)*y(k,254)
         mat(k,784) = rxt(k,391)*y(k,254)
         mat(k,725) = rxt(k,392)*y(k,254)
         mat(k,422) = rxt(k,330)*y(k,254)
         mat(k,475) = rxt(k,360)*y(k,254)
         mat(k,1602) = rxt(k,361)*y(k,254)
         mat(k,1077) = rxt(k,337)*y(k,239)
         mat(k,414) = rxt(k,342)*y(k,254)
         mat(k,2263) = rxt(k,166)*y(k,239)
         mat(k,1556) = rxt(k,171)*y(k,254)
         mat(k,705) = rxt(k,172)*y(k,254)
         mat(k,1542) = (rxt(k,587)+rxt(k,661)+rxt(k,674)+rxt(k,683))*y(k,109) + ( &
                      + rxt(k,586)+rxt(k,663)+rxt(k,671)+rxt(k,680))*y(k,110) + ( &
                      + rxt(k,594)+rxt(k,690)+rxt(k,694)+rxt(k,698))*y(k,111) &
                      + rxt(k,260)*y(k,254)
         mat(k,356) = rxt(k,345)*y(k,254)
         mat(k,1777) = (rxt(k,589)+rxt(k,660)+rxt(k,673)+rxt(k,682))*y(k,109) + ( &
                      + rxt(k,588)+rxt(k,659)+rxt(k,670)+rxt(k,679))*y(k,110) + ( &
                      + rxt(k,593)+rxt(k,689)+rxt(k,693)+rxt(k,697))*y(k,111) &
                      + rxt(k,227)*y(k,254)
         mat(k,1051) = rxt(k,363)*y(k,254)
         mat(k,1293) = (rxt(k,591)+rxt(k,662)+rxt(k,675)+rxt(k,684))*y(k,109) + ( &
                      + rxt(k,590)+rxt(k,664)+rxt(k,672)+rxt(k,681))*y(k,110) + ( &
                      + rxt(k,595)+rxt(k,691)+rxt(k,695)+rxt(k,699))*y(k,111) &
                      + rxt(k,295)*y(k,254)
         mat(k,1952) = rxt(k,201)*y(k,254)
         mat(k,499) = rxt(k,179)*y(k,254)
         mat(k,1625) = (rxt(k,587)+rxt(k,661)+rxt(k,674)+rxt(k,683))*y(k,98) + ( &
                      + rxt(k,589)+rxt(k,660)+rxt(k,673)+rxt(k,682))*y(k,102) + ( &
                      + rxt(k,591)+rxt(k,662)+rxt(k,675)+rxt(k,684))*y(k,106)
         mat(k,1754) = (rxt(k,586)+rxt(k,663)+rxt(k,671)+rxt(k,680))*y(k,98) + ( &
                      + rxt(k,588)+rxt(k,659)+rxt(k,670)+rxt(k,679))*y(k,102) + ( &
                      + rxt(k,590)+rxt(k,664)+rxt(k,672)+rxt(k,681))*y(k,106) &
                      + rxt(k,230)*y(k,254)
         mat(k,1701) = (rxt(k,594)+rxt(k,690)+rxt(k,694)+rxt(k,698))*y(k,98) + ( &
                      + rxt(k,593)+rxt(k,689)+rxt(k,693)+rxt(k,697))*y(k,102) + ( &
                      + rxt(k,595)+rxt(k,691)+rxt(k,695)+rxt(k,699))*y(k,106) &
                      + rxt(k,268)*y(k,254)
         mat(k,1360) = .500_r8*rxt(k,436)*y(k,254)
         mat(k,114) = rxt(k,597)*y(k,254)
         mat(k,653) = rxt(k,417)*y(k,254)
         mat(k,493) = rxt(k,421)*y(k,254)
         mat(k,1927) = rxt(k,337)*y(k,68) + rxt(k,166)*y(k,92) + rxt(k,173)*y(k,254)
         mat(k,2182) = rxt(k,318)*y(k,29) + rxt(k,383)*y(k,32) + rxt(k,408)*y(k,34) &
                      + rxt(k,409)*y(k,35) + rxt(k,320)*y(k,45) + rxt(k,323)*y(k,47) &
                      + rxt(k,354)*y(k,51) + rxt(k,325)*y(k,52) + rxt(k,326)*y(k,53) &
                      + rxt(k,385)*y(k,54) + rxt(k,328)*y(k,55) + rxt(k,423)*y(k,57) &
                      + rxt(k,411)*y(k,58) + rxt(k,391)*y(k,59) + rxt(k,392)*y(k,60) &
                      + rxt(k,330)*y(k,61) + rxt(k,360)*y(k,63) + rxt(k,361)*y(k,64) &
                      + rxt(k,342)*y(k,69) + rxt(k,171)*y(k,93) + rxt(k,172)*y(k,95) &
                      + rxt(k,260)*y(k,98) + rxt(k,345)*y(k,101) + rxt(k,227)*y(k,102) &
                      + rxt(k,363)*y(k,104) + rxt(k,295)*y(k,106) + rxt(k,201) &
                      *y(k,107) + rxt(k,179)*y(k,108) + rxt(k,230)*y(k,110) &
                      + rxt(k,268)*y(k,111) + .500_r8*rxt(k,436)*y(k,138) + rxt(k,597) &
                      *y(k,153) + rxt(k,417)*y(k,181) + rxt(k,421)*y(k,182) &
                      + rxt(k,173)*y(k,239) + 2.000_r8*rxt(k,176)*y(k,254)
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
         mat(k, 209) = mat(k, 209) + lmat(k, 209)
         mat(k, 212) = mat(k, 212) + lmat(k, 212)
         mat(k, 214) = lmat(k, 214)
         mat(k, 215) = mat(k, 215) + lmat(k, 215)
         mat(k, 216) = mat(k, 216) + lmat(k, 216)
         mat(k, 217) = mat(k, 217) + lmat(k, 217)
         mat(k, 219) = mat(k, 219) + lmat(k, 219)
         mat(k, 222) = mat(k, 222) + lmat(k, 222)
         mat(k, 223) = lmat(k, 223)
         mat(k, 224) = lmat(k, 224)
         mat(k, 226) = mat(k, 226) + lmat(k, 226)
         mat(k, 233) = mat(k, 233) + lmat(k, 233)
         mat(k, 238) = lmat(k, 238)
         mat(k, 239) = lmat(k, 239)
         mat(k, 240) = lmat(k, 240)
         mat(k, 241) = lmat(k, 241)
         mat(k, 242) = mat(k, 242) + lmat(k, 242)
         mat(k, 244) = mat(k, 244) + lmat(k, 244)
         mat(k, 247) = mat(k, 247) + lmat(k, 247)
         mat(k, 255) = mat(k, 255) + lmat(k, 255)
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
         mat(k, 275) = mat(k, 275) + lmat(k, 275)
         mat(k, 277) = mat(k, 277) + lmat(k, 277)
         mat(k, 278) = lmat(k, 278)
         mat(k, 280) = lmat(k, 280)
         mat(k, 281) = mat(k, 281) + lmat(k, 281)
         mat(k, 282) = mat(k, 282) + lmat(k, 282)
         mat(k, 285) = mat(k, 285) + lmat(k, 285)
         mat(k, 288) = lmat(k, 288)
         mat(k, 289) = lmat(k, 289)
         mat(k, 290) = lmat(k, 290)
         mat(k, 291) = mat(k, 291) + lmat(k, 291)
         mat(k, 294) = mat(k, 294) + lmat(k, 294)
         mat(k, 296) = mat(k, 296) + lmat(k, 296)
         mat(k, 301) = mat(k, 301) + lmat(k, 301)
         mat(k, 302) = lmat(k, 302)
         mat(k, 303) = lmat(k, 303)
         mat(k, 305) = mat(k, 305) + lmat(k, 305)
         mat(k, 306) = lmat(k, 306)
         mat(k, 308) = mat(k, 308) + lmat(k, 308)
         mat(k, 309) = lmat(k, 309)
         mat(k, 310) = lmat(k, 310)
         mat(k, 311) = lmat(k, 311)
         mat(k, 312) = lmat(k, 312)
         mat(k, 314) = mat(k, 314) + lmat(k, 314)
         mat(k, 315) = lmat(k, 315)
         mat(k, 317) = lmat(k, 317)
         mat(k, 318) = lmat(k, 318)
         mat(k, 320) = lmat(k, 320)
         mat(k, 321) = lmat(k, 321)
         mat(k, 322) = lmat(k, 322)
         mat(k, 323) = mat(k, 323) + lmat(k, 323)
         mat(k, 324) = lmat(k, 324)
         mat(k, 325) = lmat(k, 325)
         mat(k, 327) = lmat(k, 327)
         mat(k, 328) = mat(k, 328) + lmat(k, 328)
         mat(k, 329) = mat(k, 329) + lmat(k, 329)
         mat(k, 335) = lmat(k, 335)
         mat(k, 336) = lmat(k, 336)
         mat(k, 337) = lmat(k, 337)
         mat(k, 338) = mat(k, 338) + lmat(k, 338)
         mat(k, 344) = mat(k, 344) + lmat(k, 344)
         mat(k, 350) = mat(k, 350) + lmat(k, 350)
         mat(k, 351) = mat(k, 351) + lmat(k, 351)
         mat(k, 355) = mat(k, 355) + lmat(k, 355)
         mat(k, 357) = mat(k, 357) + lmat(k, 357)
         mat(k, 361) = mat(k, 361) + lmat(k, 361)
         mat(k, 363) = lmat(k, 363)
         mat(k, 364) = lmat(k, 364)
         mat(k, 365) = lmat(k, 365)
         mat(k, 366) = lmat(k, 366)
         mat(k, 367) = lmat(k, 367)
         mat(k, 368) = mat(k, 368) + lmat(k, 368)
         mat(k, 373) = mat(k, 373) + lmat(k, 373)
         mat(k, 375) = lmat(k, 375)
         mat(k, 376) = lmat(k, 376)
         mat(k, 377) = mat(k, 377) + lmat(k, 377)
         mat(k, 378) = lmat(k, 378)
         mat(k, 379) = lmat(k, 379)
         mat(k, 380) = lmat(k, 380)
         mat(k, 381) = lmat(k, 381)
         mat(k, 382) = lmat(k, 382)
         mat(k, 383) = lmat(k, 383)
         mat(k, 384) = lmat(k, 384)
         mat(k, 385) = lmat(k, 385)
         mat(k, 386) = mat(k, 386) + lmat(k, 386)
         mat(k, 389) = lmat(k, 389)
         mat(k, 390) = mat(k, 390) + lmat(k, 390)
         mat(k, 391) = mat(k, 391) + lmat(k, 391)
         mat(k, 394) = mat(k, 394) + lmat(k, 394)
         mat(k, 396) = lmat(k, 396)
         mat(k, 397) = lmat(k, 397)
         mat(k, 398) = mat(k, 398) + lmat(k, 398)
         mat(k, 399) = mat(k, 399) + lmat(k, 399)
         mat(k, 401) = mat(k, 401) + lmat(k, 401)
         mat(k, 402) = lmat(k, 402)
         mat(k, 403) = mat(k, 403) + lmat(k, 403)
         mat(k, 404) = lmat(k, 404)
         mat(k, 405) = lmat(k, 405)
         mat(k, 406) = lmat(k, 406)
         mat(k, 408) = mat(k, 408) + lmat(k, 408)
         mat(k, 409) = lmat(k, 409)
         mat(k, 413) = mat(k, 413) + lmat(k, 413)
         mat(k, 415) = mat(k, 415) + lmat(k, 415)
         mat(k, 416) = lmat(k, 416)
         mat(k, 420) = mat(k, 420) + lmat(k, 420)
         mat(k, 423) = mat(k, 423) + lmat(k, 423)
         mat(k, 431) = lmat(k, 431)
         mat(k, 432) = lmat(k, 432)
         mat(k, 433) = lmat(k, 433)
         mat(k, 434) = lmat(k, 434)
         mat(k, 435) = mat(k, 435) + lmat(k, 435)
         mat(k, 437) = lmat(k, 437)
         mat(k, 438) = mat(k, 438) + lmat(k, 438)
         mat(k, 439) = lmat(k, 439)
         mat(k, 440) = lmat(k, 440)
         mat(k, 441) = lmat(k, 441)
         mat(k, 442) = mat(k, 442) + lmat(k, 442)
         mat(k, 443) = lmat(k, 443)
         mat(k, 445) = mat(k, 445) + lmat(k, 445)
         mat(k, 450) = mat(k, 450) + lmat(k, 450)
         mat(k, 452) = lmat(k, 452)
         mat(k, 456) = mat(k, 456) + lmat(k, 456)
         mat(k, 458) = mat(k, 458) + lmat(k, 458)
         mat(k, 459) = lmat(k, 459)
         mat(k, 461) = mat(k, 461) + lmat(k, 461)
         mat(k, 463) = lmat(k, 463)
         mat(k, 464) = mat(k, 464) + lmat(k, 464)
         mat(k, 468) = lmat(k, 468)
         mat(k, 470) = mat(k, 470) + lmat(k, 470)
         mat(k, 472) = mat(k, 472) + lmat(k, 472)
         mat(k, 473) = mat(k, 473) + lmat(k, 473)
         mat(k, 474) = lmat(k, 474)
         mat(k, 476) = mat(k, 476) + lmat(k, 476)
         mat(k, 477) = lmat(k, 477)
         mat(k, 479) = lmat(k, 479)
         mat(k, 480) = mat(k, 480) + lmat(k, 480)
         mat(k, 481) = lmat(k, 481)
         mat(k, 482) = lmat(k, 482)
         mat(k, 483) = lmat(k, 483)
         mat(k, 484) = lmat(k, 484)
         mat(k, 485) = lmat(k, 485)
         mat(k, 486) = lmat(k, 486)
         mat(k, 487) = lmat(k, 487)
         mat(k, 488) = mat(k, 488) + lmat(k, 488)
         mat(k, 490) = lmat(k, 490)
         mat(k, 491) = lmat(k, 491)
         mat(k, 492) = mat(k, 492) + lmat(k, 492)
         mat(k, 494) = mat(k, 494) + lmat(k, 494)
         mat(k, 495) = lmat(k, 495)
         mat(k, 496) = mat(k, 496) + lmat(k, 496)
         mat(k, 497) = mat(k, 497) + lmat(k, 497)
         mat(k, 498) = lmat(k, 498)
         mat(k, 500) = mat(k, 500) + lmat(k, 500)
         mat(k, 502) = lmat(k, 502)
         mat(k, 503) = lmat(k, 503)
         mat(k, 504) = mat(k, 504) + lmat(k, 504)
         mat(k, 506) = mat(k, 506) + lmat(k, 506)
         mat(k, 507) = lmat(k, 507)
         mat(k, 509) = lmat(k, 509)
         mat(k, 510) = lmat(k, 510)
         mat(k, 511) = mat(k, 511) + lmat(k, 511)
         mat(k, 512) = mat(k, 512) + lmat(k, 512)
         mat(k, 516) = mat(k, 516) + lmat(k, 516)
         mat(k, 518) = mat(k, 518) + lmat(k, 518)
         mat(k, 520) = lmat(k, 520)
         mat(k, 521) = lmat(k, 521)
         mat(k, 522) = lmat(k, 522)
         mat(k, 523) = mat(k, 523) + lmat(k, 523)
         mat(k, 526) = mat(k, 526) + lmat(k, 526)
         mat(k, 532) = mat(k, 532) + lmat(k, 532)
         mat(k, 534) = lmat(k, 534)
         mat(k, 535) = mat(k, 535) + lmat(k, 535)
         mat(k, 539) = mat(k, 539) + lmat(k, 539)
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
         mat(k, 563) = mat(k, 563) + lmat(k, 563)
         mat(k, 564) = mat(k, 564) + lmat(k, 564)
         mat(k, 568) = mat(k, 568) + lmat(k, 568)
         mat(k, 572) = mat(k, 572) + lmat(k, 572)
         mat(k, 573) = mat(k, 573) + lmat(k, 573)
         mat(k, 578) = mat(k, 578) + lmat(k, 578)
         mat(k, 579) = mat(k, 579) + lmat(k, 579)
         mat(k, 580) = lmat(k, 580)
         mat(k, 581) = lmat(k, 581)
         mat(k, 583) = lmat(k, 583)
         mat(k, 584) = lmat(k, 584)
         mat(k, 585) = mat(k, 585) + lmat(k, 585)
         mat(k, 588) = mat(k, 588) + lmat(k, 588)
         mat(k, 596) = mat(k, 596) + lmat(k, 596)
         mat(k, 603) = mat(k, 603) + lmat(k, 603)
         mat(k, 605) = lmat(k, 605)
         mat(k, 606) = lmat(k, 606)
         mat(k, 608) = lmat(k, 608)
         mat(k, 609) = lmat(k, 609)
         mat(k, 610) = lmat(k, 610)
         mat(k, 611) = mat(k, 611) + lmat(k, 611)
         mat(k, 612) = mat(k, 612) + lmat(k, 612)
         mat(k, 613) = lmat(k, 613)
         mat(k, 618) = mat(k, 618) + lmat(k, 618)
         mat(k, 624) = lmat(k, 624)
         mat(k, 625) = mat(k, 625) + lmat(k, 625)
         mat(k, 628) = mat(k, 628) + lmat(k, 628)
         mat(k, 629) = lmat(k, 629)
         mat(k, 630) = mat(k, 630) + lmat(k, 630)
         mat(k, 638) = mat(k, 638) + lmat(k, 638)
         mat(k, 639) = lmat(k, 639)
         mat(k, 640) = lmat(k, 640)
         mat(k, 641) = lmat(k, 641)
         mat(k, 644) = lmat(k, 644)
         mat(k, 645) = mat(k, 645) + lmat(k, 645)
         mat(k, 646) = mat(k, 646) + lmat(k, 646)
         mat(k, 648) = lmat(k, 648)
         mat(k, 650) = lmat(k, 650)
         mat(k, 651) = lmat(k, 651)
         mat(k, 652) = mat(k, 652) + lmat(k, 652)
         mat(k, 654) = mat(k, 654) + lmat(k, 654)
         mat(k, 662) = mat(k, 662) + lmat(k, 662)
         mat(k, 668) = lmat(k, 668)
         mat(k, 670) = mat(k, 670) + lmat(k, 670)
         mat(k, 673) = lmat(k, 673)
         mat(k, 677) = lmat(k, 677)
         mat(k, 679) = mat(k, 679) + lmat(k, 679)
         mat(k, 680) = lmat(k, 680)
         mat(k, 685) = mat(k, 685) + lmat(k, 685)
         mat(k, 688) = lmat(k, 688)
         mat(k, 689) = lmat(k, 689)
         mat(k, 690) = mat(k, 690) + lmat(k, 690)
         mat(k, 691) = lmat(k, 691)
         mat(k, 695) = lmat(k, 695)
         mat(k, 696) = lmat(k, 696)
         mat(k, 697) = lmat(k, 697)
         mat(k, 698) = mat(k, 698) + lmat(k, 698)
         mat(k, 699) = mat(k, 699) + lmat(k, 699)
         mat(k, 703) = mat(k, 703) + lmat(k, 703)
         mat(k, 706) = lmat(k, 706)
         mat(k, 707) = lmat(k, 707)
         mat(k, 708) = lmat(k, 708)
         mat(k, 709) = lmat(k, 709)
         mat(k, 710) = mat(k, 710) + lmat(k, 710)
         mat(k, 716) = lmat(k, 716)
         mat(k, 719) = mat(k, 719) + lmat(k, 719)
         mat(k, 720) = mat(k, 720) + lmat(k, 720)
         mat(k, 722) = lmat(k, 722)
         mat(k, 724) = mat(k, 724) + lmat(k, 724)
         mat(k, 726) = mat(k, 726) + lmat(k, 726)
         mat(k, 727) = mat(k, 727) + lmat(k, 727)
         mat(k, 728) = lmat(k, 728)
         mat(k, 729) = lmat(k, 729)
         mat(k, 731) = mat(k, 731) + lmat(k, 731)
         mat(k, 734) = mat(k, 734) + lmat(k, 734)
         mat(k, 740) = lmat(k, 740)
         mat(k, 741) = mat(k, 741) + lmat(k, 741)
         mat(k, 745) = lmat(k, 745)
         mat(k, 746) = lmat(k, 746)
         mat(k, 748) = lmat(k, 748)
         mat(k, 749) = lmat(k, 749)
         mat(k, 750) = mat(k, 750) + lmat(k, 750)
         mat(k, 751) = lmat(k, 751)
         mat(k, 752) = mat(k, 752) + lmat(k, 752)
         mat(k, 755) = mat(k, 755) + lmat(k, 755)
         mat(k, 756) = mat(k, 756) + lmat(k, 756)
         mat(k, 758) = mat(k, 758) + lmat(k, 758)
         mat(k, 759) = lmat(k, 759)
         mat(k, 761) = mat(k, 761) + lmat(k, 761)
         mat(k, 763) = mat(k, 763) + lmat(k, 763)
         mat(k, 770) = mat(k, 770) + lmat(k, 770)
         mat(k, 771) = lmat(k, 771)
         mat(k, 772) = lmat(k, 772)
         mat(k, 773) = lmat(k, 773)
         mat(k, 774) = lmat(k, 774)
         mat(k, 775) = lmat(k, 775)
         mat(k, 777) = lmat(k, 777)
         mat(k, 778) = lmat(k, 778)
         mat(k, 779) = mat(k, 779) + lmat(k, 779)
         mat(k, 780) = mat(k, 780) + lmat(k, 780)
         mat(k, 788) = mat(k, 788) + lmat(k, 788)
         mat(k, 791) = lmat(k, 791)
         mat(k, 794) = lmat(k, 794)
         mat(k, 795) = lmat(k, 795)
         mat(k, 796) = lmat(k, 796)
         mat(k, 797) = lmat(k, 797)
         mat(k, 798) = mat(k, 798) + lmat(k, 798)
         mat(k, 803) = lmat(k, 803)
         mat(k, 805) = lmat(k, 805)
         mat(k, 807) = lmat(k, 807)
         mat(k, 808) = mat(k, 808) + lmat(k, 808)
         mat(k, 810) = mat(k, 810) + lmat(k, 810)
         mat(k, 812) = lmat(k, 812)
         mat(k, 813) = lmat(k, 813)
         mat(k, 814) = lmat(k, 814)
         mat(k, 815) = lmat(k, 815)
         mat(k, 816) = mat(k, 816) + lmat(k, 816)
         mat(k, 819) = mat(k, 819) + lmat(k, 819)
         mat(k, 826) = mat(k, 826) + lmat(k, 826)
         mat(k, 837) = mat(k, 837) + lmat(k, 837)
         mat(k, 853) = mat(k, 853) + lmat(k, 853)
         mat(k, 864) = mat(k, 864) + lmat(k, 864)
         mat(k, 873) = mat(k, 873) + lmat(k, 873)
         mat(k, 882) = mat(k, 882) + lmat(k, 882)
         mat(k, 889) = mat(k, 889) + lmat(k, 889)
         mat(k, 897) = lmat(k, 897)
         mat(k, 898) = lmat(k, 898)
         mat(k, 899) = lmat(k, 899)
         mat(k, 903) = mat(k, 903) + lmat(k, 903)
         mat(k, 919) = mat(k, 919) + lmat(k, 919)
         mat(k, 920) = mat(k, 920) + lmat(k, 920)
         mat(k, 921) = mat(k, 921) + lmat(k, 921)
         mat(k, 922) = lmat(k, 922)
         mat(k, 924) = lmat(k, 924)
         mat(k, 926) = mat(k, 926) + lmat(k, 926)
         mat(k, 927) = lmat(k, 927)
         mat(k, 928) = mat(k, 928) + lmat(k, 928)
         mat(k, 930) = mat(k, 930) + lmat(k, 930)
         mat(k, 931) = mat(k, 931) + lmat(k, 931)
         mat(k, 933) = lmat(k, 933)
         mat(k, 934) = lmat(k, 934)
         mat(k, 936) = mat(k, 936) + lmat(k, 936)
         mat(k, 937) = lmat(k, 937)
         mat(k, 938) = lmat(k, 938)
         mat(k, 939) = mat(k, 939) + lmat(k, 939)
         mat(k, 941) = lmat(k, 941)
         mat(k, 942) = lmat(k, 942)
         mat(k, 943) = lmat(k, 943)
         mat(k, 945) = mat(k, 945) + lmat(k, 945)
         mat(k, 946) = lmat(k, 946)
         mat(k, 948) = mat(k, 948) + lmat(k, 948)
         mat(k, 950) = mat(k, 950) + lmat(k, 950)
         mat(k, 961) = mat(k, 961) + lmat(k, 961)
         mat(k, 970) = mat(k, 970) + lmat(k, 970)
         mat(k, 988) = mat(k, 988) + lmat(k, 988)
         mat(k,1007) = mat(k,1007) + lmat(k,1007)
         mat(k,1008) = mat(k,1008) + lmat(k,1008)
         mat(k,1009) = mat(k,1009) + lmat(k,1009)
         mat(k,1012) = mat(k,1012) + lmat(k,1012)
         mat(k,1014) = mat(k,1014) + lmat(k,1014)
         mat(k,1015) = lmat(k,1015)
         mat(k,1016) = mat(k,1016) + lmat(k,1016)
         mat(k,1017) = mat(k,1017) + lmat(k,1017)
         mat(k,1023) = mat(k,1023) + lmat(k,1023)
         mat(k,1033) = mat(k,1033) + lmat(k,1033)
         mat(k,1046) = mat(k,1046) + lmat(k,1046)
         mat(k,1053) = mat(k,1053) + lmat(k,1053)
         mat(k,1054) = lmat(k,1054)
         mat(k,1055) = lmat(k,1055)
         mat(k,1059) = lmat(k,1059)
         mat(k,1061) = mat(k,1061) + lmat(k,1061)
         mat(k,1062) = mat(k,1062) + lmat(k,1062)
         mat(k,1064) = mat(k,1064) + lmat(k,1064)
         mat(k,1067) = mat(k,1067) + lmat(k,1067)
         mat(k,1078) = mat(k,1078) + lmat(k,1078)
         mat(k,1080) = lmat(k,1080)
         mat(k,1081) = lmat(k,1081)
         mat(k,1097) = mat(k,1097) + lmat(k,1097)
         mat(k,1117) = mat(k,1117) + lmat(k,1117)
         mat(k,1119) = lmat(k,1119)
         mat(k,1120) = lmat(k,1120)
         mat(k,1122) = lmat(k,1122)
         mat(k,1130) = mat(k,1130) + lmat(k,1130)
         mat(k,1147) = lmat(k,1147)
         mat(k,1151) = mat(k,1151) + lmat(k,1151)
         mat(k,1158) = mat(k,1158) + lmat(k,1158)
         mat(k,1159) = lmat(k,1159)
         mat(k,1161) = lmat(k,1161)
         mat(k,1163) = mat(k,1163) + lmat(k,1163)
         mat(k,1164) = lmat(k,1164)
         mat(k,1166) = lmat(k,1166)
         mat(k,1167) = lmat(k,1167)
         mat(k,1174) = mat(k,1174) + lmat(k,1174)
         mat(k,1175) = lmat(k,1175)
         mat(k,1176) = mat(k,1176) + lmat(k,1176)
         mat(k,1178) = mat(k,1178) + lmat(k,1178)
         mat(k,1184) = mat(k,1184) + lmat(k,1184)
         mat(k,1196) = mat(k,1196) + lmat(k,1196)
         mat(k,1197) = mat(k,1197) + lmat(k,1197)
         mat(k,1198) = mat(k,1198) + lmat(k,1198)
         mat(k,1199) = mat(k,1199) + lmat(k,1199)
         mat(k,1200) = mat(k,1200) + lmat(k,1200)
         mat(k,1201) = mat(k,1201) + lmat(k,1201)
         mat(k,1203) = mat(k,1203) + lmat(k,1203)
         mat(k,1205) = mat(k,1205) + lmat(k,1205)
         mat(k,1209) = mat(k,1209) + lmat(k,1209)
         mat(k,1214) = lmat(k,1214)
         mat(k,1215) = lmat(k,1215)
         mat(k,1216) = lmat(k,1216)
         mat(k,1217) = lmat(k,1217)
         mat(k,1218) = mat(k,1218) + lmat(k,1218)
         mat(k,1219) = lmat(k,1219)
         mat(k,1221) = lmat(k,1221)
         mat(k,1223) = lmat(k,1223)
         mat(k,1225) = mat(k,1225) + lmat(k,1225)
         mat(k,1226) = lmat(k,1226)
         mat(k,1228) = lmat(k,1228)
         mat(k,1231) = mat(k,1231) + lmat(k,1231)
         mat(k,1233) = lmat(k,1233)
         mat(k,1234) = lmat(k,1234)
         mat(k,1235) = mat(k,1235) + lmat(k,1235)
         mat(k,1245) = mat(k,1245) + lmat(k,1245)
         mat(k,1266) = mat(k,1266) + lmat(k,1266)
         mat(k,1281) = mat(k,1281) + lmat(k,1281)
         mat(k,1289) = lmat(k,1289)
         mat(k,1290) = mat(k,1290) + lmat(k,1290)
         mat(k,1297) = mat(k,1297) + lmat(k,1297)
         mat(k,1314) = mat(k,1314) + lmat(k,1314)
         mat(k,1334) = mat(k,1334) + lmat(k,1334)
         mat(k,1349) = mat(k,1349) + lmat(k,1349)
         mat(k,1350) = mat(k,1350) + lmat(k,1350)
         mat(k,1353) = mat(k,1353) + lmat(k,1353)
         mat(k,1354) = mat(k,1354) + lmat(k,1354)
         mat(k,1355) = mat(k,1355) + lmat(k,1355)
         mat(k,1357) = mat(k,1357) + lmat(k,1357)
         mat(k,1361) = mat(k,1361) + lmat(k,1361)
         mat(k,1362) = mat(k,1362) + lmat(k,1362)
         mat(k,1363) = mat(k,1363) + lmat(k,1363)
         mat(k,1365) = lmat(k,1365)
         mat(k,1382) = mat(k,1382) + lmat(k,1382)
         mat(k,1398) = lmat(k,1398)
         mat(k,1415) = mat(k,1415) + lmat(k,1415)
         mat(k,1423) = mat(k,1423) + lmat(k,1423)
         mat(k,1439) = mat(k,1439) + lmat(k,1439)
         mat(k,1454) = lmat(k,1454)
         mat(k,1456) = mat(k,1456) + lmat(k,1456)
         mat(k,1460) = mat(k,1460) + lmat(k,1460)
         mat(k,1462) = mat(k,1462) + lmat(k,1462)
         mat(k,1464) = lmat(k,1464)
         mat(k,1482) = mat(k,1482) + lmat(k,1482)
         mat(k,1514) = mat(k,1514) + lmat(k,1514)
         mat(k,1530) = mat(k,1530) + lmat(k,1530)
         mat(k,1537) = mat(k,1537) + lmat(k,1537)
         mat(k,1538) = mat(k,1538) + lmat(k,1538)
         mat(k,1545) = mat(k,1545) + lmat(k,1545)
         mat(k,1558) = mat(k,1558) + lmat(k,1558)
         mat(k,1565) = mat(k,1565) + lmat(k,1565)
         mat(k,1570) = lmat(k,1570)
         mat(k,1571) = mat(k,1571) + lmat(k,1571)
         mat(k,1572) = mat(k,1572) + lmat(k,1572)
         mat(k,1573) = lmat(k,1573)
         mat(k,1586) = lmat(k,1586)
         mat(k,1588) = lmat(k,1588)
         mat(k,1589) = mat(k,1589) + lmat(k,1589)
         mat(k,1590) = mat(k,1590) + lmat(k,1590)
         mat(k,1591) = mat(k,1591) + lmat(k,1591)
         mat(k,1592) = mat(k,1592) + lmat(k,1592)
         mat(k,1594) = lmat(k,1594)
         mat(k,1598) = mat(k,1598) + lmat(k,1598)
         mat(k,1599) = mat(k,1599) + lmat(k,1599)
         mat(k,1602) = mat(k,1602) + lmat(k,1602)
         mat(k,1604) = mat(k,1604) + lmat(k,1604)
         mat(k,1605) = mat(k,1605) + lmat(k,1605)
         mat(k,1609) = mat(k,1609) + lmat(k,1609)
         mat(k,1616) = mat(k,1616) + lmat(k,1616)
         mat(k,1618) = lmat(k,1618)
         mat(k,1663) = mat(k,1663) + lmat(k,1663)
         mat(k,1680) = mat(k,1680) + lmat(k,1680)
         mat(k,1681) = mat(k,1681) + lmat(k,1681)
         mat(k,1686) = mat(k,1686) + lmat(k,1686)
         mat(k,1692) = mat(k,1692) + lmat(k,1692)
         mat(k,1696) = lmat(k,1696)
         mat(k,1704) = mat(k,1704) + lmat(k,1704)
         mat(k,1706) = lmat(k,1706)
         mat(k,1711) = mat(k,1711) + lmat(k,1711)
         mat(k,1721) = mat(k,1721) + lmat(k,1721)
         mat(k,1731) = mat(k,1731) + lmat(k,1731)
         mat(k,1739) = mat(k,1739) + lmat(k,1739)
         mat(k,1744) = mat(k,1744) + lmat(k,1744)
         mat(k,1752) = mat(k,1752) + lmat(k,1752)
         mat(k,1763) = mat(k,1763) + lmat(k,1763)
         mat(k,1770) = mat(k,1770) + lmat(k,1770)
         mat(k,1775) = mat(k,1775) + lmat(k,1775)
         mat(k,1796) = mat(k,1796) + lmat(k,1796)
         mat(k,1804) = mat(k,1804) + lmat(k,1804)
         mat(k,1912) = mat(k,1912) + lmat(k,1912)
         mat(k,1927) = mat(k,1927) + lmat(k,1927)
         mat(k,1935) = lmat(k,1935)
         mat(k,1938) = mat(k,1938) + lmat(k,1938)
         mat(k,1940) = mat(k,1940) + lmat(k,1940)
         mat(k,1947) = lmat(k,1947)
         mat(k,1982) = mat(k,1982) + lmat(k,1982)
         mat(k,1985) = mat(k,1985) + lmat(k,1985)
         mat(k,2170) = mat(k,2170) + lmat(k,2170)
         mat(k,2196) = mat(k,2196) + lmat(k,2196)
         mat(k,2201) = mat(k,2201) + lmat(k,2201)
         mat(k,2206) = mat(k,2206) + lmat(k,2206)
         mat(k,2230) = mat(k,2230) + lmat(k,2230)
         mat(k,2248) = mat(k,2248) + lmat(k,2248)
         mat(k,2254) = mat(k,2254) + lmat(k,2254)
         mat(k,2316) = mat(k,2316) + lmat(k,2316)
         mat(k,2319) = mat(k,2319) + lmat(k,2319)
         mat(k,2324) = mat(k,2324) + lmat(k,2324)
         mat(k,2345) = mat(k,2345) + lmat(k,2345)
         mat(k,2354) = mat(k,2354) + lmat(k,2354)
         mat(k,2359) = mat(k,2359) + lmat(k,2359)
         mat(k,2383) = mat(k,2383) + lmat(k,2383)
         mat(k,2430) = mat(k,2430) + lmat(k,2430)
         mat(k,2432) = mat(k,2432) + lmat(k,2432)
         mat(k,2434) = mat(k,2434) + lmat(k,2434)
         mat(k,2441) = mat(k,2441) + lmat(k,2441)
         mat(k,2443) = mat(k,2443) + lmat(k,2443)
         mat(k,2458) = mat(k,2458) + lmat(k,2458)
         mat(k,2464) = mat(k,2464) + lmat(k,2464)
         mat(k,2470) = mat(k,2470) + lmat(k,2470)
         mat(k,2499) = mat(k,2499) + lmat(k,2499)
         mat(k,2558) = mat(k,2558) + lmat(k,2558)
         mat(k,2571) = mat(k,2571) + lmat(k,2571)
         mat(k,2633) = mat(k,2633) + lmat(k,2633)
         mat(k,2689) = mat(k,2689) + lmat(k,2689)
         mat(k,2691) = mat(k,2691) + lmat(k,2691)
         mat(k,2700) = mat(k,2700) + lmat(k,2700)
         mat(k,2702) = mat(k,2702) + lmat(k,2702)
         mat(k,2704) = mat(k,2704) + lmat(k,2704)
         mat(k,2713) = lmat(k,2713)
         mat(k,2721) = lmat(k,2721)
         mat(k,2724) = mat(k,2724) + lmat(k,2724)
         mat(k,2725) = mat(k,2725) + lmat(k,2725)
         mat(k,2728) = lmat(k,2728)
         mat(k,2737) = mat(k,2737) + lmat(k,2737)
         mat(k, 251) = 0._r8
         mat(k, 252) = 0._r8
         mat(k, 316) = 0._r8
         mat(k, 319) = 0._r8
         mat(k, 352) = 0._r8
         mat(k, 388) = 0._r8
         mat(k, 410) = 0._r8
         mat(k, 527) = 0._r8
         mat(k, 529) = 0._r8
         mat(k, 542) = 0._r8
         mat(k, 574) = 0._r8
         mat(k, 589) = 0._r8
         mat(k, 592) = 0._r8
         mat(k, 600) = 0._r8
         mat(k, 692) = 0._r8
         mat(k, 694) = 0._r8
         mat(k, 736) = 0._r8
         mat(k, 737) = 0._r8
         mat(k, 742) = 0._r8
         mat(k, 743) = 0._r8
         mat(k, 747) = 0._r8
         mat(k, 753) = 0._r8
         mat(k, 754) = 0._r8
         mat(k, 757) = 0._r8
         mat(k, 799) = 0._r8
         mat(k, 801) = 0._r8
         mat(k, 802) = 0._r8
         mat(k, 804) = 0._r8
         mat(k, 806) = 0._r8
         mat(k, 820) = 0._r8
         mat(k, 823) = 0._r8
         mat(k, 836) = 0._r8
         mat(k, 838) = 0._r8
         mat(k, 839) = 0._r8
         mat(k, 841) = 0._r8
         mat(k, 844) = 0._r8
         mat(k, 852) = 0._r8
         mat(k, 854) = 0._r8
         mat(k, 855) = 0._r8
         mat(k, 857) = 0._r8
         mat(k, 859) = 0._r8
         mat(k, 861) = 0._r8
         mat(k, 875) = 0._r8
         mat(k, 878) = 0._r8
         mat(k, 881) = 0._r8
         mat(k, 894) = 0._r8
         mat(k, 925) = 0._r8
         mat(k, 944) = 0._r8
         mat(k, 947) = 0._r8
         mat(k, 956) = 0._r8
         mat(k, 959) = 0._r8
         mat(k, 989) = 0._r8
         mat(k, 991) = 0._r8
         mat(k, 999) = 0._r8
         mat(k,1002) = 0._r8
         mat(k,1021) = 0._r8
         mat(k,1022) = 0._r8
         mat(k,1026) = 0._r8
         mat(k,1027) = 0._r8
         mat(k,1030) = 0._r8
         mat(k,1047) = 0._r8
         mat(k,1048) = 0._r8
         mat(k,1058) = 0._r8
         mat(k,1063) = 0._r8
         mat(k,1093) = 0._r8
         mat(k,1095) = 0._r8
         mat(k,1099) = 0._r8
         mat(k,1105) = 0._r8
         mat(k,1107) = 0._r8
         mat(k,1109) = 0._r8
         mat(k,1113) = 0._r8
         mat(k,1114) = 0._r8
         mat(k,1116) = 0._r8
         mat(k,1133) = 0._r8
         mat(k,1134) = 0._r8
         mat(k,1135) = 0._r8
         mat(k,1139) = 0._r8
         mat(k,1143) = 0._r8
         mat(k,1144) = 0._r8
         mat(k,1146) = 0._r8
         mat(k,1149) = 0._r8
         mat(k,1152) = 0._r8
         mat(k,1153) = 0._r8
         mat(k,1154) = 0._r8
         mat(k,1155) = 0._r8
         mat(k,1156) = 0._r8
         mat(k,1157) = 0._r8
         mat(k,1162) = 0._r8
         mat(k,1177) = 0._r8
         mat(k,1185) = 0._r8
         mat(k,1186) = 0._r8
         mat(k,1187) = 0._r8
         mat(k,1191) = 0._r8
         mat(k,1192) = 0._r8
         mat(k,1202) = 0._r8
         mat(k,1204) = 0._r8
         mat(k,1207) = 0._r8
         mat(k,1210) = 0._r8
         mat(k,1220) = 0._r8
         mat(k,1222) = 0._r8
         mat(k,1224) = 0._r8
         mat(k,1229) = 0._r8
         mat(k,1230) = 0._r8
         mat(k,1246) = 0._r8
         mat(k,1247) = 0._r8
         mat(k,1250) = 0._r8
         mat(k,1252) = 0._r8
         mat(k,1253) = 0._r8
         mat(k,1256) = 0._r8
         mat(k,1262) = 0._r8
         mat(k,1263) = 0._r8
         mat(k,1264) = 0._r8
         mat(k,1265) = 0._r8
         mat(k,1267) = 0._r8
         mat(k,1268) = 0._r8
         mat(k,1271) = 0._r8
         mat(k,1273) = 0._r8
         mat(k,1274) = 0._r8
         mat(k,1277) = 0._r8
         mat(k,1278) = 0._r8
         mat(k,1288) = 0._r8
         mat(k,1291) = 0._r8
         mat(k,1306) = 0._r8
         mat(k,1319) = 0._r8
         mat(k,1321) = 0._r8
         mat(k,1327) = 0._r8
         mat(k,1329) = 0._r8
         mat(k,1331) = 0._r8
         mat(k,1332) = 0._r8
         mat(k,1333) = 0._r8
         mat(k,1335) = 0._r8
         mat(k,1336) = 0._r8
         mat(k,1337) = 0._r8
         mat(k,1341) = 0._r8
         mat(k,1343) = 0._r8
         mat(k,1344) = 0._r8
         mat(k,1356) = 0._r8
         mat(k,1364) = 0._r8
         mat(k,1374) = 0._r8
         mat(k,1375) = 0._r8
         mat(k,1376) = 0._r8
         mat(k,1377) = 0._r8
         mat(k,1378) = 0._r8
         mat(k,1379) = 0._r8
         mat(k,1381) = 0._r8
         mat(k,1383) = 0._r8
         mat(k,1385) = 0._r8
         mat(k,1389) = 0._r8
         mat(k,1391) = 0._r8
         mat(k,1392) = 0._r8
         mat(k,1393) = 0._r8
         mat(k,1397) = 0._r8
         mat(k,1401) = 0._r8
         mat(k,1404) = 0._r8
         mat(k,1405) = 0._r8
         mat(k,1408) = 0._r8
         mat(k,1409) = 0._r8
         mat(k,1411) = 0._r8
         mat(k,1412) = 0._r8
         mat(k,1413) = 0._r8
         mat(k,1416) = 0._r8
         mat(k,1417) = 0._r8
         mat(k,1418) = 0._r8
         mat(k,1422) = 0._r8
         mat(k,1424) = 0._r8
         mat(k,1425) = 0._r8
         mat(k,1426) = 0._r8
         mat(k,1430) = 0._r8
         mat(k,1437) = 0._r8
         mat(k,1440) = 0._r8
         mat(k,1444) = 0._r8
         mat(k,1446) = 0._r8
         mat(k,1447) = 0._r8
         mat(k,1451) = 0._r8
         mat(k,1457) = 0._r8
         mat(k,1461) = 0._r8
         mat(k,1463) = 0._r8
         mat(k,1466) = 0._r8
         mat(k,1468) = 0._r8
         mat(k,1471) = 0._r8
         mat(k,1472) = 0._r8
         mat(k,1473) = 0._r8
         mat(k,1474) = 0._r8
         mat(k,1479) = 0._r8
         mat(k,1480) = 0._r8
         mat(k,1481) = 0._r8
         mat(k,1486) = 0._r8
         mat(k,1488) = 0._r8
         mat(k,1494) = 0._r8
         mat(k,1515) = 0._r8
         mat(k,1518) = 0._r8
         mat(k,1520) = 0._r8
         mat(k,1525) = 0._r8
         mat(k,1526) = 0._r8
         mat(k,1539) = 0._r8
         mat(k,1541) = 0._r8
         mat(k,1546) = 0._r8
         mat(k,1547) = 0._r8
         mat(k,1550) = 0._r8
         mat(k,1555) = 0._r8
         mat(k,1561) = 0._r8
         mat(k,1564) = 0._r8
         mat(k,1566) = 0._r8
         mat(k,1567) = 0._r8
         mat(k,1574) = 0._r8
         mat(k,1584) = 0._r8
         mat(k,1596) = 0._r8
         mat(k,1601) = 0._r8
         mat(k,1608) = 0._r8
         mat(k,1610) = 0._r8
         mat(k,1611) = 0._r8
         mat(k,1614) = 0._r8
         mat(k,1615) = 0._r8
         mat(k,1617) = 0._r8
         mat(k,1619) = 0._r8
         mat(k,1620) = 0._r8
         mat(k,1621) = 0._r8
         mat(k,1623) = 0._r8
         mat(k,1624) = 0._r8
         mat(k,1636) = 0._r8
         mat(k,1662) = 0._r8
         mat(k,1665) = 0._r8
         mat(k,1666) = 0._r8
         mat(k,1667) = 0._r8
         mat(k,1669) = 0._r8
         mat(k,1670) = 0._r8
         mat(k,1671) = 0._r8
         mat(k,1672) = 0._r8
         mat(k,1673) = 0._r8
         mat(k,1678) = 0._r8
         mat(k,1679) = 0._r8
         mat(k,1685) = 0._r8
         mat(k,1687) = 0._r8
         mat(k,1689) = 0._r8
         mat(k,1691) = 0._r8
         mat(k,1694) = 0._r8
         mat(k,1695) = 0._r8
         mat(k,1697) = 0._r8
         mat(k,1698) = 0._r8
         mat(k,1699) = 0._r8
         mat(k,1703) = 0._r8
         mat(k,1707) = 0._r8
         mat(k,1708) = 0._r8
         mat(k,1709) = 0._r8
         mat(k,1710) = 0._r8
         mat(k,1712) = 0._r8
         mat(k,1717) = 0._r8
         mat(k,1719) = 0._r8
         mat(k,1722) = 0._r8
         mat(k,1723) = 0._r8
         mat(k,1724) = 0._r8
         mat(k,1725) = 0._r8
         mat(k,1726) = 0._r8
         mat(k,1727) = 0._r8
         mat(k,1736) = 0._r8
         mat(k,1737) = 0._r8
         mat(k,1738) = 0._r8
         mat(k,1742) = 0._r8
         mat(k,1743) = 0._r8
         mat(k,1745) = 0._r8
         mat(k,1746) = 0._r8
         mat(k,1747) = 0._r8
         mat(k,1749) = 0._r8
         mat(k,1750) = 0._r8
         mat(k,1751) = 0._r8
         mat(k,1753) = 0._r8
         mat(k,1768) = 0._r8
         mat(k,1769) = 0._r8
         mat(k,1772) = 0._r8
         mat(k,1773) = 0._r8
         mat(k,1774) = 0._r8
         mat(k,1776) = 0._r8
         mat(k,1779) = 0._r8
         mat(k,1783) = 0._r8
         mat(k,1789) = 0._r8
         mat(k,1791) = 0._r8
         mat(k,1792) = 0._r8
         mat(k,1798) = 0._r8
         mat(k,1799) = 0._r8
         mat(k,1812) = 0._r8
         mat(k,1828) = 0._r8
         mat(k,1846) = 0._r8
         mat(k,1847) = 0._r8
         mat(k,1855) = 0._r8
         mat(k,1872) = 0._r8
         mat(k,1879) = 0._r8
         mat(k,1880) = 0._r8
         mat(k,1882) = 0._r8
         mat(k,1884) = 0._r8
         mat(k,1886) = 0._r8
         mat(k,1893) = 0._r8
         mat(k,1898) = 0._r8
         mat(k,1903) = 0._r8
         mat(k,1904) = 0._r8
         mat(k,1913) = 0._r8
         mat(k,1914) = 0._r8
         mat(k,1930) = 0._r8
         mat(k,1931) = 0._r8
         mat(k,1932) = 0._r8
         mat(k,1933) = 0._r8
         mat(k,1934) = 0._r8
         mat(k,1936) = 0._r8
         mat(k,1937) = 0._r8
         mat(k,1939) = 0._r8
         mat(k,1941) = 0._r8
         mat(k,1942) = 0._r8
         mat(k,1943) = 0._r8
         mat(k,1944) = 0._r8
         mat(k,1945) = 0._r8
         mat(k,1946) = 0._r8
         mat(k,1948) = 0._r8
         mat(k,1949) = 0._r8
         mat(k,1950) = 0._r8
         mat(k,1976) = 0._r8
         mat(k,1978) = 0._r8
         mat(k,1980) = 0._r8
         mat(k,1984) = 0._r8
         mat(k,1987) = 0._r8
         mat(k,1992) = 0._r8
         mat(k,1993) = 0._r8
         mat(k,1997) = 0._r8
         mat(k,2073) = 0._r8
         mat(k,2094) = 0._r8
         mat(k,2108) = 0._r8
         mat(k,2111) = 0._r8
         mat(k,2119) = 0._r8
         mat(k,2120) = 0._r8
         mat(k,2146) = 0._r8
         mat(k,2169) = 0._r8
         mat(k,2191) = 0._r8
         mat(k,2192) = 0._r8
         mat(k,2194) = 0._r8
         mat(k,2195) = 0._r8
         mat(k,2198) = 0._r8
         mat(k,2199) = 0._r8
         mat(k,2203) = 0._r8
         mat(k,2212) = 0._r8
         mat(k,2215) = 0._r8
         mat(k,2219) = 0._r8
         mat(k,2220) = 0._r8
         mat(k,2222) = 0._r8
         mat(k,2223) = 0._r8
         mat(k,2224) = 0._r8
         mat(k,2226) = 0._r8
         mat(k,2227) = 0._r8
         mat(k,2228) = 0._r8
         mat(k,2231) = 0._r8
         mat(k,2233) = 0._r8
         mat(k,2237) = 0._r8
         mat(k,2238) = 0._r8
         mat(k,2240) = 0._r8
         mat(k,2242) = 0._r8
         mat(k,2243) = 0._r8
         mat(k,2244) = 0._r8
         mat(k,2245) = 0._r8
         mat(k,2246) = 0._r8
         mat(k,2249) = 0._r8
         mat(k,2250) = 0._r8
         mat(k,2252) = 0._r8
         mat(k,2253) = 0._r8
         mat(k,2256) = 0._r8
         mat(k,2257) = 0._r8
         mat(k,2258) = 0._r8
         mat(k,2259) = 0._r8
         mat(k,2260) = 0._r8
         mat(k,2261) = 0._r8
         mat(k,2262) = 0._r8
         mat(k,2279) = 0._r8
         mat(k,2283) = 0._r8
         mat(k,2286) = 0._r8
         mat(k,2292) = 0._r8
         mat(k,2293) = 0._r8
         mat(k,2296) = 0._r8
         mat(k,2297) = 0._r8
         mat(k,2298) = 0._r8
         mat(k,2300) = 0._r8
         mat(k,2303) = 0._r8
         mat(k,2304) = 0._r8
         mat(k,2305) = 0._r8
         mat(k,2307) = 0._r8
         mat(k,2312) = 0._r8
         mat(k,2314) = 0._r8
         mat(k,2315) = 0._r8
         mat(k,2318) = 0._r8
         mat(k,2332) = 0._r8
         mat(k,2347) = 0._r8
         mat(k,2348) = 0._r8
         mat(k,2352) = 0._r8
         mat(k,2353) = 0._r8
         mat(k,2360) = 0._r8
         mat(k,2361) = 0._r8
         mat(k,2364) = 0._r8
         mat(k,2365) = 0._r8
         mat(k,2369) = 0._r8
         mat(k,2370) = 0._r8
         mat(k,2371) = 0._r8
         mat(k,2372) = 0._r8
         mat(k,2373) = 0._r8
         mat(k,2375) = 0._r8
         mat(k,2376) = 0._r8
         mat(k,2377) = 0._r8
         mat(k,2380) = 0._r8
         mat(k,2382) = 0._r8
         mat(k,2387) = 0._r8
         mat(k,2389) = 0._r8
         mat(k,2394) = 0._r8
         mat(k,2397) = 0._r8
         mat(k,2399) = 0._r8
         mat(k,2401) = 0._r8
         mat(k,2406) = 0._r8
         mat(k,2409) = 0._r8
         mat(k,2415) = 0._r8
         mat(k,2416) = 0._r8
         mat(k,2417) = 0._r8
         mat(k,2418) = 0._r8
         mat(k,2421) = 0._r8
         mat(k,2423) = 0._r8
         mat(k,2424) = 0._r8
         mat(k,2425) = 0._r8
         mat(k,2426) = 0._r8
         mat(k,2427) = 0._r8
         mat(k,2428) = 0._r8
         mat(k,2429) = 0._r8
         mat(k,2433) = 0._r8
         mat(k,2437) = 0._r8
         mat(k,2446) = 0._r8
         mat(k,2455) = 0._r8
         mat(k,2456) = 0._r8
         mat(k,2457) = 0._r8
         mat(k,2460) = 0._r8
         mat(k,2461) = 0._r8
         mat(k,2465) = 0._r8
         mat(k,2466) = 0._r8
         mat(k,2473) = 0._r8
         mat(k,2474) = 0._r8
         mat(k,2521) = 0._r8
         mat(k,2551) = 0._r8
         mat(k,2552) = 0._r8
         mat(k,2554) = 0._r8
         mat(k,2556) = 0._r8
         mat(k,2557) = 0._r8
         mat(k,2560) = 0._r8
         mat(k,2561) = 0._r8
         mat(k,2565) = 0._r8
         mat(k,2574) = 0._r8
         mat(k,2593) = 0._r8
         mat(k,2599) = 0._r8
         mat(k,2601) = 0._r8
         mat(k,2603) = 0._r8
         mat(k,2604) = 0._r8
         mat(k,2605) = 0._r8
         mat(k,2607) = 0._r8
         mat(k,2608) = 0._r8
         mat(k,2609) = 0._r8
         mat(k,2611) = 0._r8
         mat(k,2613) = 0._r8
         mat(k,2615) = 0._r8
         mat(k,2619) = 0._r8
         mat(k,2621) = 0._r8
         mat(k,2622) = 0._r8
         mat(k,2624) = 0._r8
         mat(k,2631) = 0._r8
         mat(k,2632) = 0._r8
         mat(k,2635) = 0._r8
         mat(k,2641) = 0._r8
         mat(k,2642) = 0._r8
         mat(k,2648) = 0._r8
         mat(k,2650) = 0._r8
         mat(k,2662) = 0._r8
         mat(k,2664) = 0._r8
         mat(k,2667) = 0._r8
         mat(k,2669) = 0._r8
         mat(k,2681) = 0._r8
         mat(k,2682) = 0._r8
         mat(k,2683) = 0._r8
         mat(k,2684) = 0._r8
         mat(k,2687) = 0._r8
         mat(k,2688) = 0._r8
         mat(k,2692) = 0._r8
         mat(k,2695) = 0._r8
         mat(k,2696) = 0._r8
         mat(k,2697) = 0._r8
         mat(k,2698) = 0._r8
         mat(k,2701) = 0._r8
         mat(k,2705) = 0._r8
         mat(k,2712) = 0._r8
         mat(k,2714) = 0._r8
         mat(k,2715) = 0._r8
         mat(k,2716) = 0._r8
         mat(k,2718) = 0._r8
         mat(k,2719) = 0._r8
         mat(k,2720) = 0._r8
         mat(k,2722) = 0._r8
         mat(k,2723) = 0._r8
         mat(k,2726) = 0._r8
         mat(k,2727) = 0._r8
         mat(k,2729) = 0._r8
         mat(k,2730) = 0._r8
         mat(k,2731) = 0._r8
         mat(k,2732) = 0._r8
         mat(k,2733) = 0._r8
         mat(k,2734) = 0._r8
         mat(k,2735) = 0._r8
         mat(k,2736) = 0._r8
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
         mat(k, 209) = mat(k, 209) - dti(k)
         mat(k, 212) = mat(k, 212) - dti(k)
         mat(k, 216) = mat(k, 216) - dti(k)
         mat(k, 219) = mat(k, 219) - dti(k)
         mat(k, 223) = mat(k, 223) - dti(k)
         mat(k, 226) = mat(k, 226) - dti(k)
         mat(k, 233) = mat(k, 233) - dti(k)
         mat(k, 238) = mat(k, 238) - dti(k)
         mat(k, 242) = mat(k, 242) - dti(k)
         mat(k, 247) = mat(k, 247) - dti(k)
         mat(k, 255) = mat(k, 255) - dti(k)
         mat(k, 260) = mat(k, 260) - dti(k)
         mat(k, 265) = mat(k, 265) - dti(k)
         mat(k, 269) = mat(k, 269) - dti(k)
         mat(k, 274) = mat(k, 274) - dti(k)
         mat(k, 277) = mat(k, 277) - dti(k)
         mat(k, 282) = mat(k, 282) - dti(k)
         mat(k, 285) = mat(k, 285) - dti(k)
         mat(k, 288) = mat(k, 288) - dti(k)
         mat(k, 291) = mat(k, 291) - dti(k)
         mat(k, 296) = mat(k, 296) - dti(k)
         mat(k, 301) = mat(k, 301) - dti(k)
         mat(k, 305) = mat(k, 305) - dti(k)
         mat(k, 309) = mat(k, 309) - dti(k)
         mat(k, 314) = mat(k, 314) - dti(k)
         mat(k, 320) = mat(k, 320) - dti(k)
         mat(k, 323) = mat(k, 323) - dti(k)
         mat(k, 329) = mat(k, 329) - dti(k)
         mat(k, 335) = mat(k, 335) - dti(k)
         mat(k, 338) = mat(k, 338) - dti(k)
         mat(k, 344) = mat(k, 344) - dti(k)
         mat(k, 351) = mat(k, 351) - dti(k)
         mat(k, 357) = mat(k, 357) - dti(k)
         mat(k, 363) = mat(k, 363) - dti(k)
         mat(k, 368) = mat(k, 368) - dti(k)
         mat(k, 373) = mat(k, 373) - dti(k)
         mat(k, 378) = mat(k, 378) - dti(k)
         mat(k, 386) = mat(k, 386) - dti(k)
         mat(k, 391) = mat(k, 391) - dti(k)
         mat(k, 394) = mat(k, 394) - dti(k)
         mat(k, 399) = mat(k, 399) - dti(k)
         mat(k, 404) = mat(k, 404) - dti(k)
         mat(k, 408) = mat(k, 408) - dti(k)
         mat(k, 415) = mat(k, 415) - dti(k)
         mat(k, 423) = mat(k, 423) - dti(k)
         mat(k, 431) = mat(k, 431) - dti(k)
         mat(k, 434) = mat(k, 434) - dti(k)
         mat(k, 442) = mat(k, 442) - dti(k)
         mat(k, 450) = mat(k, 450) - dti(k)
         mat(k, 458) = mat(k, 458) - dti(k)
         mat(k, 464) = mat(k, 464) - dti(k)
         mat(k, 470) = mat(k, 470) - dti(k)
         mat(k, 476) = mat(k, 476) - dti(k)
         mat(k, 482) = mat(k, 482) - dti(k)
         mat(k, 488) = mat(k, 488) - dti(k)
         mat(k, 494) = mat(k, 494) - dti(k)
         mat(k, 500) = mat(k, 500) - dti(k)
         mat(k, 506) = mat(k, 506) - dti(k)
         mat(k, 512) = mat(k, 512) - dti(k)
         mat(k, 518) = mat(k, 518) - dti(k)
         mat(k, 526) = mat(k, 526) - dti(k)
         mat(k, 532) = mat(k, 532) - dti(k)
         mat(k, 539) = mat(k, 539) - dti(k)
         mat(k, 545) = mat(k, 545) - dti(k)
         mat(k, 550) = mat(k, 550) - dti(k)
         mat(k, 553) = mat(k, 553) - dti(k)
         mat(k, 556) = mat(k, 556) - dti(k)
         mat(k, 560) = mat(k, 560) - dti(k)
         mat(k, 564) = mat(k, 564) - dti(k)
         mat(k, 572) = mat(k, 572) - dti(k)
         mat(k, 579) = mat(k, 579) - dti(k)
         mat(k, 588) = mat(k, 588) - dti(k)
         mat(k, 596) = mat(k, 596) - dti(k)
         mat(k, 603) = mat(k, 603) - dti(k)
         mat(k, 608) = mat(k, 608) - dti(k)
         mat(k, 611) = mat(k, 611) - dti(k)
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
         mat(k, 726) = mat(k, 726) - dti(k)
         mat(k, 734) = mat(k, 734) - dti(k)
         mat(k, 741) = mat(k, 741) - dti(k)
         mat(k, 752) = mat(k, 752) - dti(k)
         mat(k, 763) = mat(k, 763) - dti(k)
         mat(k, 770) = mat(k, 770) - dti(k)
         mat(k, 780) = mat(k, 780) - dti(k)
         mat(k, 788) = mat(k, 788) - dti(k)
         mat(k, 798) = mat(k, 798) - dti(k)
         mat(k, 810) = mat(k, 810) - dti(k)
         mat(k, 819) = mat(k, 819) - dti(k)
         mat(k, 826) = mat(k, 826) - dti(k)
         mat(k, 837) = mat(k, 837) - dti(k)
         mat(k, 853) = mat(k, 853) - dti(k)
         mat(k, 864) = mat(k, 864) - dti(k)
         mat(k, 873) = mat(k, 873) - dti(k)
         mat(k, 882) = mat(k, 882) - dti(k)
         mat(k, 889) = mat(k, 889) - dti(k)
         mat(k, 897) = mat(k, 897) - dti(k)
         mat(k, 903) = mat(k, 903) - dti(k)
         mat(k, 921) = mat(k, 921) - dti(k)
         mat(k, 931) = mat(k, 931) - dti(k)
         mat(k, 939) = mat(k, 939) - dti(k)
         mat(k, 950) = mat(k, 950) - dti(k)
         mat(k, 961) = mat(k, 961) - dti(k)
         mat(k, 970) = mat(k, 970) - dti(k)
         mat(k, 988) = mat(k, 988) - dti(k)
         mat(k,1008) = mat(k,1008) - dti(k)
         mat(k,1023) = mat(k,1023) - dti(k)
         mat(k,1033) = mat(k,1033) - dti(k)
         mat(k,1046) = mat(k,1046) - dti(k)
         mat(k,1053) = mat(k,1053) - dti(k)
         mat(k,1061) = mat(k,1061) - dti(k)
         mat(k,1067) = mat(k,1067) - dti(k)
         mat(k,1078) = mat(k,1078) - dti(k)
         mat(k,1097) = mat(k,1097) - dti(k)
         mat(k,1117) = mat(k,1117) - dti(k)
         mat(k,1130) = mat(k,1130) - dti(k)
         mat(k,1151) = mat(k,1151) - dti(k)
         mat(k,1163) = mat(k,1163) - dti(k)
         mat(k,1174) = mat(k,1174) - dti(k)
         mat(k,1184) = mat(k,1184) - dti(k)
         mat(k,1198) = mat(k,1198) - dti(k)
         mat(k,1209) = mat(k,1209) - dti(k)
         mat(k,1218) = mat(k,1218) - dti(k)
         mat(k,1231) = mat(k,1231) - dti(k)
         mat(k,1245) = mat(k,1245) - dti(k)
         mat(k,1266) = mat(k,1266) - dti(k)
         mat(k,1281) = mat(k,1281) - dti(k)
         mat(k,1297) = mat(k,1297) - dti(k)
         mat(k,1314) = mat(k,1314) - dti(k)
         mat(k,1334) = mat(k,1334) - dti(k)
         mat(k,1350) = mat(k,1350) - dti(k)
         mat(k,1362) = mat(k,1362) - dti(k)
         mat(k,1382) = mat(k,1382) - dti(k)
         mat(k,1415) = mat(k,1415) - dti(k)
         mat(k,1439) = mat(k,1439) - dti(k)
         mat(k,1460) = mat(k,1460) - dti(k)
         mat(k,1482) = mat(k,1482) - dti(k)
         mat(k,1514) = mat(k,1514) - dti(k)
         mat(k,1530) = mat(k,1530) - dti(k)
         mat(k,1545) = mat(k,1545) - dti(k)
         mat(k,1558) = mat(k,1558) - dti(k)
         mat(k,1572) = mat(k,1572) - dti(k)
         mat(k,1590) = mat(k,1590) - dti(k)
         mat(k,1609) = mat(k,1609) - dti(k)
         mat(k,1663) = mat(k,1663) - dti(k)
         mat(k,1686) = mat(k,1686) - dti(k)
         mat(k,1711) = mat(k,1711) - dti(k)
         mat(k,1739) = mat(k,1739) - dti(k)
         mat(k,1763) = mat(k,1763) - dti(k)
         mat(k,1796) = mat(k,1796) - dti(k)
         mat(k,1912) = mat(k,1912) - dti(k)
         mat(k,1938) = mat(k,1938) - dti(k)
         mat(k,1985) = mat(k,1985) - dti(k)
         mat(k,2170) = mat(k,2170) - dti(k)
         mat(k,2201) = mat(k,2201) - dti(k)
         mat(k,2230) = mat(k,2230) - dti(k)
         mat(k,2254) = mat(k,2254) - dti(k)
         mat(k,2324) = mat(k,2324) - dti(k)
         mat(k,2354) = mat(k,2354) - dti(k)
         mat(k,2383) = mat(k,2383) - dti(k)
         mat(k,2441) = mat(k,2441) - dti(k)
         mat(k,2470) = mat(k,2470) - dti(k)
         mat(k,2571) = mat(k,2571) - dti(k)
         mat(k,2633) = mat(k,2633) - dti(k)
         mat(k,2704) = mat(k,2704) - dti(k)
         mat(k,2737) = mat(k,2737) - dti(k)
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
