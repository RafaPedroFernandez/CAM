
      module mo_setrxt

      use shr_kind_mod, only : r8 => shr_kind_r8

      private
      public :: setrxt
      public :: setrxt_hrates

      contains

      subroutine setrxt( rate, temp, m, ncol )
 
      use ppgrid, only : pcols, pver


      use chem_mods, only : rxntot
      use mo_jpl,    only : jpl

      implicit none

!-------------------------------------------------------
!       ... dummy arguments
!-------------------------------------------------------
      integer, intent(in) :: ncol
      real(r8), intent(in)    :: temp(pcols,pver)
      real(r8), intent(in)    :: m(ncol*pver)
      real(r8), intent(inout) :: rate(ncol*pver,max(1,rxntot))

!-------------------------------------------------------
!       ... local variables
!-------------------------------------------------------
      integer   ::  n
      integer   ::  offset
      real(r8)  :: itemp(ncol*pver)
      real(r8)  :: exp_fac(ncol*pver)
      real(r8)  :: ko(ncol*pver)
      real(r8)  :: kinf(ncol*pver)

      rate(:,173) = 1.2e-10_r8
      rate(:,177) = 1.2e-10_r8
      rate(:,178) = 1.2e-10_r8
      rate(:,184) = 6.9e-12_r8
      rate(:,185) = 7.2e-11_r8
      rate(:,186) = 1.6e-12_r8
      rate(:,192) = 1.8e-12_r8
      rate(:,196) = 1.8e-12_r8
      rate(:,208) = 3.5e-12_r8
      rate(:,210) = 1.3e-11_r8
      rate(:,211) = 2.2e-11_r8
      rate(:,212) = 5e-11_r8
      rate(:,249) = 1.7e-13_r8
      rate(:,251) = 2.607e-10_r8
      rate(:,252) = 9.75e-11_r8
      rate(:,253) = 2.07e-10_r8
      rate(:,254) = 2.088e-10_r8
      rate(:,255) = 1.17e-10_r8
      rate(:,256) = 4.644e-11_r8
      rate(:,257) = 1.204e-10_r8
      rate(:,258) = 9.9e-11_r8
      rate(:,259) = 3.3e-12_r8
      rate(:,282) = 4.5e-11_r8
      rate(:,283) = 4.62e-10_r8
      rate(:,284) = 1.2e-10_r8
      rate(:,285) = 9e-11_r8
      rate(:,286) = 3e-11_r8
      rate(:,288) = 2e-13_r8
      rate(:,289) = 1.5e-12_r8
      rate(:,290) = 1.25e-10_r8
      rate(:,291) = 1.8e-10_r8
      rate(:,292) = 1.44e-11_r8
      rate(:,298) = 1e-10_r8
      rate(:,302) = 2.49e-11_r8
      rate(:,311) = 9e-12_r8
      rate(:,312) = 1.4e-10_r8
      rate(:,313) = 3.6e-16_r8
      rate(:,314) = 1e-10_r8
      rate(:,330) = 2.14e-11_r8
      rate(:,331) = 1.9e-10_r8
      rate(:,334) = 1.3e-12_r8
      rate(:,352) = 1.2e-12_r8
      rate(:,353) = 8e-13_r8
      rate(:,357) = 2.3e-12_r8
      rate(:,363) = 2.57e-10_r8
      rate(:,364) = 1.8e-10_r8
      rate(:,365) = 1.794e-10_r8
      rate(:,366) = 1.3e-10_r8
      rate(:,367) = 7.65e-11_r8
      rate(:,380) = 4e-13_r8
      rate(:,384) = 1.31e-10_r8
      rate(:,385) = 3.5e-11_r8
      rate(:,386) = 9e-12_r8
      rate(:,393) = 6.8e-14_r8
      rate(:,394) = 2e-13_r8
      rate(:,409) = 1e-12_r8
      rate(:,413) = 1e-14_r8
      rate(:,414) = 1e-11_r8
      rate(:,415) = 1.15e-11_r8
      rate(:,416) = 4e-14_r8
      rate(:,429) = 3e-12_r8
      rate(:,430) = 6.7e-13_r8
      rate(:,440) = 3.5e-13_r8
      rate(:,441) = 5.4e-11_r8
      rate(:,444) = 2e-12_r8
      rate(:,445) = 1.4e-11_r8
      rate(:,448) = 2.4e-12_r8
      rate(:,459) = 5e-12_r8
      rate(:,469) = 2.2e-12_r8
      rate(:,471) = 6.7e-12_r8
      rate(:,474) = 3.5e-12_r8
      rate(:,477) = 1.3e-11_r8
      rate(:,478) = 1.4e-11_r8
      rate(:,482) = 2.4e-12_r8
      rate(:,483) = 1.4e-11_r8
      rate(:,488) = 2.4e-12_r8
      rate(:,489) = 4e-11_r8
      rate(:,490) = 4e-11_r8
      rate(:,492) = 1.4e-11_r8
      rate(:,496) = 2.4e-12_r8
      rate(:,497) = 4e-11_r8
      rate(:,501) = 7e-11_r8
      rate(:,502) = 1e-10_r8
      rate(:,507) = 2.4e-12_r8
      rate(:,522) = 4.7e-11_r8
      rate(:,535) = 2.1e-12_r8
      rate(:,536) = 2.8e-13_r8
      rate(:,544) = 1.7e-11_r8
      rate(:,550) = 8.4e-11_r8
      rate(:,552) = 1.9e-11_r8
      rate(:,553) = 1.2e-14_r8
      rate(:,554) = 2e-10_r8
      rate(:,561) = 2.4e-12_r8
      rate(:,562) = 2e-11_r8
      rate(:,566) = 2.3e-11_r8
      rate(:,567) = 2e-11_r8
      rate(:,571) = 3.3e-11_r8
      rate(:,572) = 1e-12_r8
      rate(:,573) = 5.7e-11_r8
      rate(:,574) = 3.4e-11_r8
      rate(:,576) = 3.3e-10_r8
      rate(:,583) = 2.3e-12_r8
      rate(:,585) = 1.2e-11_r8
      rate(:,586) = 5.7e-11_r8
      rate(:,587) = 2.8e-11_r8
      rate(:,588) = 6.6e-11_r8
      rate(:,589) = 1.4e-11_r8
      rate(:,592) = 1.9e-12_r8
      rate(:,623) = 6.34e-08_r8
      rate(:,645) = 1.9e-11_r8
      rate(:,646) = 1.2e-14_r8
      rate(:,647) = 2e-10_r8
      rate(:,652) = 1.34e-11_r8
      rate(:,653) = 1.34e-11_r8
      rate(:,657) = 1.34e-11_r8
      rate(:,658) = 1.34e-11_r8
      rate(:,661) = 1.7e-11_r8
      rate(:,704) = 1.29e-07_r8
      rate(:,705) = 2.31e-07_r8
      rate(:,706) = 2.31e-06_r8
      rate(:,707) = 4.63e-07_r8
 
      do n = 1,pver
        offset = (n-1)*ncol
        itemp(offset+1:offset+ncol) = 1._r8 / temp(:ncol,n)
      end do

      rate(:,174) = 1.63e-10_r8 * exp( 60._r8 * itemp(:) )
      rate(:,175) = 2.15e-11_r8 * exp( 110._r8 * itemp(:) )
      rate(:,176) = 3.3e-11_r8 * exp( 55._r8 * itemp(:) )
      rate(:,179) = 8e-12_r8 * exp( -2060._r8 * itemp(:) )
      rate(:,182) = 1.6e-11_r8 * exp( -4570._r8 * itemp(:) )
      exp_fac(:) = exp( -2000._r8 * itemp(:) )
      rate(:,183) = 1.4e-12_r8 * exp_fac(:)
      rate(:,498) = 1.05e-14_r8 * exp_fac(:)
      rate(:,650) = 1.05e-14_r8 * exp_fac(:)
      exp_fac(:) = exp( 200._r8 * itemp(:) )
      rate(:,188) = 3e-11_r8 * exp_fac(:)
      rate(:,280) = 5.5e-12_r8 * exp_fac(:)
      rate(:,377) = 3.8e-12_r8 * exp_fac(:)
      rate(:,398) = 3.8e-12_r8 * exp_fac(:)
      rate(:,425) = 3.8e-12_r8 * exp_fac(:)
      rate(:,433) = 3.8e-12_r8 * exp_fac(:)
      rate(:,437) = 3.8e-12_r8 * exp_fac(:)
      rate(:,453) = 2.3e-11_r8 * exp_fac(:)
      rate(:,463) = 3.8e-12_r8 * exp_fac(:)
      rate(:,473) = 3.8e-12_r8 * exp_fac(:)
      rate(:,500) = 1.52e-11_r8 * exp_fac(:)
      rate(:,508) = 1.52e-12_r8 * exp_fac(:)
      rate(:,514) = 3.8e-12_r8 * exp_fac(:)
      rate(:,517) = 3.8e-12_r8 * exp_fac(:)
      rate(:,521) = 3.8e-12_r8 * exp_fac(:)
      rate(:,537) = 3.8e-12_r8 * exp_fac(:)
      rate(:,541) = 3.8e-12_r8 * exp_fac(:)
      rate(:,547) = 3.8e-12_r8 * exp_fac(:)
      rate(:,551) = 3.8e-12_r8 * exp_fac(:)
      rate(:,189) = 1e-14_r8 * exp( -490._r8 * itemp(:) )
      rate(:,190) = 1.4e-10_r8 * exp( -470._r8 * itemp(:) )
      rate(:,191) = 2.8e-12_r8 * exp( -1800._r8 * itemp(:) )
      exp_fac(:) = exp( 250._r8 * itemp(:) )
      rate(:,193) = 4.8e-11_r8 * exp_fac(:)
      rate(:,278) = 1.7e-11_r8 * exp_fac(:)
      exp_fac(:) = exp( 180._r8 * itemp(:) )
      rate(:,194) = 1.8e-11_r8 * exp_fac(:)
      rate(:,411) = 4.2e-12_r8 * exp_fac(:)
      rate(:,432) = 4.2e-12_r8 * exp_fac(:)
      rate(:,461) = 4.2e-12_r8 * exp_fac(:)
      rate(:,481) = 4.4e-12_r8 * exp_fac(:)
      rate(:,487) = 4.4e-12_r8 * exp_fac(:)
      rate(:,560) = 4.2e-12_r8 * exp_fac(:)
      rate(:,565) = 4.2e-12_r8 * exp_fac(:)
      rate(:,570) = 4.2e-12_r8 * exp_fac(:)
      rate(:,195) = 1.7e-12_r8 * exp( -940._r8 * itemp(:) )
      rate(:,199) = 4.5e-13_r8 * exp( 610._r8 * itemp(:) )
      rate(:,200) = 2.1e-11_r8 * exp( 100._r8 * itemp(:) )
      exp_fac(:) = exp( 220._r8 * itemp(:) )
      rate(:,201) = 2.9e-12_r8 * exp_fac(:)
      rate(:,202) = 1.45e-12_r8 * exp_fac(:)
      rate(:,203) = 1.45e-12_r8 * exp_fac(:)
      rate(:,204) = 3.3e-12_r8 * exp( -3150._r8 * itemp(:) )
      rate(:,205) = 5.1e-12_r8 * exp( 210._r8 * itemp(:) )
      exp_fac(:) = exp( -2450._r8 * itemp(:) )
      rate(:,206) = 1.2e-13_r8 * exp_fac(:)
      rate(:,234) = 3e-11_r8 * exp_fac(:)
      exp_fac(:) = exp( 125._r8 * itemp(:) )
      rate(:,209) = 1.7e-11_r8 * exp_fac(:)
      rate(:,371) = 5.5e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 260._r8 * itemp(:) )
      rate(:,213) = 3.44e-12_r8 * exp_fac(:)
      rate(:,269) = 2.3e-12_r8 * exp_fac(:)
      rate(:,272) = 8.8e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -1500._r8 * itemp(:) )
      rate(:,214) = 3e-12_r8 * exp_fac(:)
      rate(:,279) = 5.8e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 20._r8 * itemp(:) )
      rate(:,216) = 7.26e-11_r8 * exp_fac(:)
      rate(:,217) = 4.64e-11_r8 * exp_fac(:)
      rate(:,224) = 8.1e-11_r8 * exp( -30._r8 * itemp(:) )
      rate(:,225) = 7.1e-12_r8 * exp( -1270._r8 * itemp(:) )
      rate(:,226) = 3.05e-11_r8 * exp( -2270._r8 * itemp(:) )
      rate(:,227) = 1.1e-11_r8 * exp( -980._r8 * itemp(:) )
      exp_fac(:) = exp( 270._r8 * itemp(:) )
      rate(:,228) = 1.4e-11_r8 * exp_fac(:)
      rate(:,244) = 7.4e-12_r8 * exp_fac(:)
      rate(:,407) = 8.1e-12_r8 * exp_fac(:)
      rate(:,229) = 3.6e-11_r8 * exp( -375._r8 * itemp(:) )
      rate(:,231) = 2.4e-12_r8 * exp( -1250._r8 * itemp(:) )
      rate(:,232) = 2.3e-11_r8 * exp( -200._r8 * itemp(:) )
      rate(:,233) = 3.3e-12_r8 * exp( -115._r8 * itemp(:) )
      rate(:,235) = 1e-12_r8 * exp( -1590._r8 * itemp(:) )
      rate(:,236) = 3.5e-13_r8 * exp( -1370._r8 * itemp(:) )
      exp_fac(:) = exp( 290._r8 * itemp(:) )
      rate(:,237) = 2.6e-12_r8 * exp_fac(:)
      rate(:,238) = 6.4e-12_r8 * exp_fac(:)
      rate(:,270) = 4.1e-13_r8 * exp_fac(:)
      rate(:,510) = 7.5e-12_r8 * exp_fac(:)
      rate(:,524) = 7.5e-12_r8 * exp_fac(:)
      rate(:,527) = 7.5e-12_r8 * exp_fac(:)
      rate(:,530) = 7.5e-12_r8 * exp_fac(:)
      rate(:,239) = 6.5e-12_r8 * exp( 135._r8 * itemp(:) )
      exp_fac(:) = exp( -840._r8 * itemp(:) )
      rate(:,241) = 3.6e-12_r8 * exp_fac(:)
      rate(:,337) = 2e-12_r8 * exp_fac(:)
      rate(:,242) = 1.2e-12_r8 * exp( -330._r8 * itemp(:) )
      rate(:,243) = 2.8e-11_r8 * exp( 85._r8 * itemp(:) )
      exp_fac(:) = exp( 230._r8 * itemp(:) )
      rate(:,245) = 6e-13_r8 * exp_fac(:)
      rate(:,267) = 1.5e-12_r8 * exp_fac(:)
      rate(:,277) = 1.9e-11_r8 * exp_fac(:)
      rate(:,246) = 1e-11_r8 * exp( -3300._r8 * itemp(:) )
      rate(:,247) = 1.8e-12_r8 * exp( -250._r8 * itemp(:) )
      rate(:,248) = 3.4e-12_r8 * exp( -130._r8 * itemp(:) )
      exp_fac(:) = exp( -500._r8 * itemp(:) )
      rate(:,250) = 3e-12_r8 * exp_fac(:)
      rate(:,327) = 1.4e-10_r8 * exp_fac(:)
      rate(:,262) = 2.1e-11_r8 * exp( 240._r8 * itemp(:) )
      exp_fac(:) = exp( -800._r8 * itemp(:) )
      rate(:,263) = 1.7e-11_r8 * exp_fac(:)
      rate(:,336) = 6.3e-12_r8 * exp_fac(:)
      rate(:,264) = 4.8e-12_r8 * exp( -310._r8 * itemp(:) )
      rate(:,266) = 1.6e-11_r8 * exp( -780._r8 * itemp(:) )
      rate(:,268) = 9.5e-13_r8 * exp( 550._r8 * itemp(:) )
      rate(:,271) = 4.5e-12_r8 * exp( 460._r8 * itemp(:) )
      exp_fac(:) = exp( 365._r8 * itemp(:) )
      rate(:,273) = 1.78e-11_r8 * exp_fac(:)
      rate(:,396) = 2.6e-12_r8 * exp_fac(:)
      rate(:,513) = 2.6e-12_r8 * exp_fac(:)
      rate(:,518) = 2.6e-12_r8 * exp_fac(:)
      rate(:,520) = 2.6e-12_r8 * exp_fac(:)
      rate(:,533) = 2.6e-12_r8 * exp_fac(:)
      rate(:,540) = 2.6e-12_r8 * exp_fac(:)
      rate(:,546) = 2.6e-12_r8 * exp_fac(:)
      rate(:,549) = 2.6e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 215._r8 * itemp(:) )
      rate(:,274) = 6.28e-11_r8 * exp_fac(:)
      rate(:,276) = 1.9e-11_r8 * exp_fac(:)
      rate(:,281) = 1.2e-10_r8 * exp( -430._r8 * itemp(:) )
      rate(:,287) = 1.3e-12_r8 * exp( -1830._r8 * itemp(:) )
      rate(:,293) = 1.5e-11_r8 * exp( -1090._r8 * itemp(:) )
      rate(:,294) = 9.1e-11_r8 * exp( -146._r8 * itemp(:) )
      rate(:,295) = 4.7e-13_r8 * exp( -1670._r8 * itemp(:) )
      rate(:,297) = 1.008e+18_r8 * exp( -13670._r8 * itemp(:) )
      rate(:,299) = 8.4e-11_r8 * exp( -2620._r8 * itemp(:) )
      rate(:,301) = 2.1e-11_r8 * exp( -830._r8 * itemp(:) )
      exp_fac(:) = exp( 510._r8 * itemp(:) )
      rate(:,303) = 3e-12_r8 * exp_fac(:)
      rate(:,304) = 1.2e-11_r8 * exp_fac(:)
      exp_fac(:) = exp( 280._r8 * itemp(:) )
      rate(:,305) = 2.585e-12_r8 * exp_fac(:)
      rate(:,306) = 1.175e-12_r8 * exp_fac(:)
      rate(:,307) = 9.4e-13_r8 * exp_fac(:)
      rate(:,308) = 1.3e-11_r8 * exp( 570._r8 * itemp(:) )
      exp_fac(:) = exp( 300._r8 * itemp(:) )
      rate(:,309) = 7.15e-12_r8 * exp_fac(:)
      rate(:,375) = 2.8e-12_r8 * exp_fac(:)
      rate(:,436) = 2.9e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 440._r8 * itemp(:) )
      rate(:,315) = 1.6e-11_r8 * exp_fac(:)
      rate(:,557) = 1.2e-11_r8 * exp_fac(:)
      rate(:,656) = 1.2e-11_r8 * exp_fac(:)
      rate(:,316) = 1.1e-12_r8 * exp( 542._r8 * itemp(:) )
      rate(:,326) = 1.6e-10_r8 * exp( -260._r8 * itemp(:) )
      exp_fac(:) = exp( 0._r8 * itemp(:) )
      rate(:,328) = 1.4e-11_r8 * exp_fac(:)
      rate(:,330) = 2.14e-11_r8 * exp_fac(:)
      rate(:,331) = 1.9e-10_r8 * exp_fac(:)
      rate(:,334) = 1.3e-12_r8 * exp_fac(:)
      rate(:,352) = 1.2e-12_r8 * exp_fac(:)
      rate(:,353) = 8e-13_r8 * exp_fac(:)
      rate(:,357) = 2.3e-12_r8 * exp_fac(:)
      rate(:,363) = 2.57e-10_r8 * exp_fac(:)
      rate(:,364) = 1.8e-10_r8 * exp_fac(:)
      rate(:,365) = 1.794e-10_r8 * exp_fac(:)
      rate(:,366) = 1.3e-10_r8 * exp_fac(:)
      rate(:,367) = 7.65e-11_r8 * exp_fac(:)
      rate(:,380) = 4e-13_r8 * exp_fac(:)
      rate(:,384) = 1.31e-10_r8 * exp_fac(:)
      rate(:,385) = 3.5e-11_r8 * exp_fac(:)
      rate(:,386) = 9e-12_r8 * exp_fac(:)
      rate(:,393) = 6.8e-14_r8 * exp_fac(:)
      rate(:,394) = 2e-13_r8 * exp_fac(:)
      rate(:,409) = 1e-12_r8 * exp_fac(:)
      rate(:,413) = 1e-14_r8 * exp_fac(:)
      rate(:,414) = 1e-11_r8 * exp_fac(:)
      rate(:,415) = 1.15e-11_r8 * exp_fac(:)
      rate(:,416) = 4e-14_r8 * exp_fac(:)
      rate(:,429) = 3e-12_r8 * exp_fac(:)
      rate(:,430) = 6.7e-13_r8 * exp_fac(:)
      rate(:,440) = 3.5e-13_r8 * exp_fac(:)
      rate(:,441) = 5.4e-11_r8 * exp_fac(:)
      rate(:,444) = 2e-12_r8 * exp_fac(:)
      rate(:,445) = 1.4e-11_r8 * exp_fac(:)
      rate(:,448) = 2.4e-12_r8 * exp_fac(:)
      rate(:,459) = 5e-12_r8 * exp_fac(:)
      rate(:,469) = 2.2e-12_r8 * exp_fac(:)
      rate(:,471) = 6.7e-12_r8 * exp_fac(:)
      rate(:,474) = 3.5e-12_r8 * exp_fac(:)
      rate(:,477) = 1.3e-11_r8 * exp_fac(:)
      rate(:,478) = 1.4e-11_r8 * exp_fac(:)
      rate(:,482) = 2.4e-12_r8 * exp_fac(:)
      rate(:,483) = 1.4e-11_r8 * exp_fac(:)
      rate(:,488) = 2.4e-12_r8 * exp_fac(:)
      rate(:,489) = 4e-11_r8 * exp_fac(:)
      rate(:,490) = 4e-11_r8 * exp_fac(:)
      rate(:,492) = 1.4e-11_r8 * exp_fac(:)
      rate(:,496) = 2.4e-12_r8 * exp_fac(:)
      rate(:,497) = 4e-11_r8 * exp_fac(:)
      rate(:,501) = 7e-11_r8 * exp_fac(:)
      rate(:,502) = 1e-10_r8 * exp_fac(:)
      rate(:,507) = 2.4e-12_r8 * exp_fac(:)
      rate(:,522) = 4.7e-11_r8 * exp_fac(:)
      rate(:,535) = 2.1e-12_r8 * exp_fac(:)
      rate(:,536) = 2.8e-13_r8 * exp_fac(:)
      rate(:,544) = 1.7e-11_r8 * exp_fac(:)
      rate(:,550) = 8.4e-11_r8 * exp_fac(:)
      rate(:,552) = 1.9e-11_r8 * exp_fac(:)
      rate(:,553) = 1.2e-14_r8 * exp_fac(:)
      rate(:,554) = 2e-10_r8 * exp_fac(:)
      rate(:,561) = 2.4e-12_r8 * exp_fac(:)
      rate(:,562) = 2e-11_r8 * exp_fac(:)
      rate(:,566) = 2.3e-11_r8 * exp_fac(:)
      rate(:,567) = 2e-11_r8 * exp_fac(:)
      rate(:,571) = 3.3e-11_r8 * exp_fac(:)
      rate(:,572) = 1e-12_r8 * exp_fac(:)
      rate(:,573) = 5.7e-11_r8 * exp_fac(:)
      rate(:,574) = 3.4e-11_r8 * exp_fac(:)
      rate(:,576) = 3.3e-10_r8 * exp_fac(:)
      rate(:,583) = 2.3e-12_r8 * exp_fac(:)
      rate(:,585) = 1.2e-11_r8 * exp_fac(:)
      rate(:,586) = 5.7e-11_r8 * exp_fac(:)
      rate(:,587) = 2.8e-11_r8 * exp_fac(:)
      rate(:,588) = 6.6e-11_r8 * exp_fac(:)
      rate(:,589) = 1.4e-11_r8 * exp_fac(:)
      rate(:,592) = 1.9e-12_r8 * exp_fac(:)
      rate(:,623) = 6.34e-08_r8 * exp_fac(:)
      rate(:,645) = 1.9e-11_r8 * exp_fac(:)
      rate(:,646) = 1.2e-14_r8 * exp_fac(:)
      rate(:,647) = 2e-10_r8 * exp_fac(:)
      rate(:,652) = 1.34e-11_r8 * exp_fac(:)
      rate(:,653) = 1.34e-11_r8 * exp_fac(:)
      rate(:,657) = 1.34e-11_r8 * exp_fac(:)
      rate(:,658) = 1.34e-11_r8 * exp_fac(:)
      rate(:,661) = 1.7e-11_r8 * exp_fac(:)
      rate(:,704) = 1.29e-07_r8 * exp_fac(:)
      rate(:,705) = 2.31e-07_r8 * exp_fac(:)
      rate(:,706) = 2.31e-06_r8 * exp_fac(:)
      rate(:,707) = 4.63e-07_r8 * exp_fac(:)
      exp_fac(:) = exp( 400._r8 * itemp(:) )
      rate(:,329) = 6e-12_r8 * exp_fac(:)
      rate(:,446) = 5e-13_r8 * exp_fac(:)
      rate(:,479) = 5e-13_r8 * exp_fac(:)
      rate(:,484) = 5e-13_r8 * exp_fac(:)
      rate(:,493) = 5e-13_r8 * exp_fac(:)
      rate(:,504) = 5e-13_r8 * exp_fac(:)
      exp_fac(:) = exp( -990._r8 * itemp(:) )
      rate(:,333) = 4.7e-12_r8 * exp_fac(:)
      rate(:,358) = 3.3e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -1150._r8 * itemp(:) )
      rate(:,335) = 1.14e-11_r8 * exp_fac(:)
      rate(:,342) = 1.42e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -880._r8 * itemp(:) )
      rate(:,338) = 2.1e-12_r8 * exp_fac(:)
      rate(:,340) = 1.92e-12_r8 * exp_fac(:)
      rate(:,339) = 7.4e-12_r8 * exp( -910._r8 * itemp(:) )
      rate(:,341) = 1.46e-11_r8 * exp( -1040._r8 * itemp(:) )
      exp_fac(:) = exp( -1520._r8 * itemp(:) )
      rate(:,343) = 1.64e-12_r8 * exp_fac(:)
      rate(:,465) = 8.5e-16_r8 * exp_fac(:)
      rate(:,344) = 2.03e-11_r8 * exp( -1110._r8 * itemp(:) )
      rate(:,345) = 1.96e-12_r8 * exp( -1200._r8 * itemp(:) )
      rate(:,346) = 2.9e-11_r8 * exp( -1000._r8 * itemp(:) )
      exp_fac(:) = exp( -1100._r8 * itemp(:) )
      rate(:,347) = 2.9e-12_r8 * exp_fac(:)
      rate(:,591) = 3.4e-12_r8 * exp_fac(:)
      rate(:,348) = 9e-13_r8 * exp( -420._r8 * itemp(:) )
      rate(:,349) = 4.85e-12_r8 * exp( -850._r8 * itemp(:) )
      rate(:,350) = 9e-13_r8 * exp( -360._r8 * itemp(:) )
      rate(:,351) = 9.4e-13_r8 * exp( -510._r8 * itemp(:) )
      exp_fac(:) = exp( 700._r8 * itemp(:) )
      rate(:,354) = 3.92e-13_r8 * exp_fac(:)
      rate(:,355) = 1.68e-13_r8 * exp_fac(:)
      rate(:,381) = 7.5e-13_r8 * exp_fac(:)
      rate(:,395) = 7.5e-13_r8 * exp_fac(:)
      rate(:,410) = 7.5e-13_r8 * exp_fac(:)
      rate(:,431) = 7.5e-13_r8 * exp_fac(:)
      rate(:,435) = 8.6e-13_r8 * exp_fac(:)
      rate(:,447) = 8e-13_r8 * exp_fac(:)
      rate(:,460) = 7.5e-13_r8 * exp_fac(:)
      rate(:,470) = 7.5e-13_r8 * exp_fac(:)
      rate(:,480) = 8e-13_r8 * exp_fac(:)
      rate(:,485) = 8e-13_r8 * exp_fac(:)
      rate(:,494) = 8e-13_r8 * exp_fac(:)
      rate(:,505) = 8e-13_r8 * exp_fac(:)
      rate(:,512) = 7.5e-13_r8 * exp_fac(:)
      rate(:,516) = 7.5e-13_r8 * exp_fac(:)
      rate(:,519) = 7.5e-13_r8 * exp_fac(:)
      rate(:,532) = 7.5e-13_r8 * exp_fac(:)
      rate(:,539) = 7.5e-13_r8 * exp_fac(:)
      rate(:,545) = 7.5e-13_r8 * exp_fac(:)
      rate(:,548) = 7.5e-13_r8 * exp_fac(:)
      rate(:,559) = 7.5e-13_r8 * exp_fac(:)
      rate(:,564) = 7.5e-13_r8 * exp_fac(:)
      rate(:,569) = 7.5e-13_r8 * exp_fac(:)
      exp_fac(:) = exp( 360._r8 * itemp(:) )
      rate(:,356) = 4.05e-12_r8 * exp_fac(:)
      rate(:,424) = 2.7e-12_r8 * exp_fac(:)
      rate(:,449) = 2.7e-12_r8 * exp_fac(:)
      rate(:,450) = 1.3e-13_r8 * exp_fac(:)
      rate(:,452) = 9.6e-12_r8 * exp_fac(:)
      rate(:,458) = 5.3e-12_r8 * exp_fac(:)
      rate(:,495) = 2.7e-12_r8 * exp_fac(:)
      rate(:,506) = 2.7e-12_r8 * exp_fac(:)
      rate(:,359) = 2.2e-12_r8 * exp( -920._r8 * itemp(:) )
      exp_fac(:) = exp( -1600._r8 * itemp(:) )
      rate(:,360) = 1.25e-12_r8 * exp_fac(:)
      rate(:,370) = 3.4e-11_r8 * exp_fac(:)
      rate(:,361) = 1.3e-12_r8 * exp( -1770._r8 * itemp(:) )
      rate(:,362) = 9.2e-13_r8 * exp( -1560._r8 * itemp(:) )
      rate(:,368) = 9.7e-15_r8 * exp( 625._r8 * itemp(:) )
      rate(:,369) = 6e-13_r8 * exp( -2058._r8 * itemp(:) )
      rate(:,372) = 5e-13_r8 * exp( -424._r8 * itemp(:) )
      rate(:,373) = 1.9e-14_r8 * exp( 706._r8 * itemp(:) )
      rate(:,374) = 4.1e-13_r8 * exp( 750._r8 * itemp(:) )
      rate(:,376) = 2.9e-12_r8 * exp( -345._r8 * itemp(:) )
      rate(:,378) = 2.45e-12_r8 * exp( -1775._r8 * itemp(:) )
      rate(:,382) = 2.4e+12_r8 * exp( -7000._r8 * itemp(:) )
      rate(:,383) = 2.6e-12_r8 * exp( 265._r8 * itemp(:) )
      rate(:,387) = 1.08e-10_r8 * exp( 105._r8 * itemp(:) )
      rate(:,392) = 1.2e-14_r8 * exp( -2630._r8 * itemp(:) )
      rate(:,397) = 6.9e-12_r8 * exp( -230._r8 * itemp(:) )
      rate(:,399) = 7.2e-11_r8 * exp( -70._r8 * itemp(:) )
      rate(:,400) = 7.66e-12_r8 * exp( -1020._r8 * itemp(:) )
      exp_fac(:) = exp( -1900._r8 * itemp(:) )
      rate(:,401) = 1.4e-12_r8 * exp_fac(:)
      rate(:,421) = 6.5e-15_r8 * exp_fac(:)
      rate(:,402) = 4.63e-12_r8 * exp( 350._r8 * itemp(:) )
      rate(:,403) = 7.8e-13_r8 * exp( -1050._r8 * itemp(:) )
      exp_fac(:) = exp( 500._r8 * itemp(:) )
      rate(:,404) = 2.9e-12_r8 * exp_fac(:)
      rate(:,405) = 2e-12_r8 * exp_fac(:)
      rate(:,434) = 7.1e-13_r8 * exp_fac(:)
      rate(:,455) = 2e-12_r8 * exp_fac(:)
      rate(:,558) = 2e-12_r8 * exp_fac(:)
      rate(:,563) = 2e-12_r8 * exp_fac(:)
      rate(:,568) = 2e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 1040._r8 * itemp(:) )
      rate(:,406) = 4.3e-13_r8 * exp_fac(:)
      rate(:,456) = 4.3e-13_r8 * exp_fac(:)
      rate(:,509) = 4.3e-13_r8 * exp_fac(:)
      rate(:,523) = 4.3e-13_r8 * exp_fac(:)
      rate(:,526) = 4.3e-13_r8 * exp_fac(:)
      rate(:,529) = 4.3e-13_r8 * exp_fac(:)
      rate(:,408) = 3.15e-14_r8 * exp( 920._r8 * itemp(:) )
      rate(:,412) = 1.6e+11_r8 * exp( -4150._r8 * itemp(:) )
      rate(:,420) = 4.6e-13_r8 * exp( -1156._r8 * itemp(:) )
      rate(:,422) = 1e-13_r8 * exp( 557._r8 * itemp(:) )
      rate(:,423) = 1.41e-13_r8 * exp( 1300._r8 * itemp(:) )
      rate(:,426) = 9.19e-12_r8 * exp( -630._r8 * itemp(:) )
      rate(:,427) = 1.4e-12_r8 * exp( -1860._r8 * itemp(:) )
      rate(:,428) = 8.4e-13_r8 * exp( 830._r8 * itemp(:) )
      rate(:,442) = 4.8e-12_r8 * exp( 120._r8 * itemp(:) )
      rate(:,443) = 5.1e-14_r8 * exp( 693._r8 * itemp(:) )
      rate(:,451) = 1.5e-15_r8 * exp( -2100._r8 * itemp(:) )
      exp_fac(:) = exp( 530._r8 * itemp(:) )
      rate(:,454) = 4.6e-12_r8 * exp_fac(:)
      rate(:,457) = 2.3e-12_r8 * exp_fac(:)
      rate(:,462) = 2.3e-12_r8 * exp( -170._r8 * itemp(:) )
      rate(:,466) = 4.13e-12_r8 * exp( 452._r8 * itemp(:) )
      rate(:,472) = 5.4e-14_r8 * exp( 870._r8 * itemp(:) )
      exp_fac(:) = exp( 175._r8 * itemp(:) )
      rate(:,475) = 1.86e-11_r8 * exp_fac(:)
      rate(:,476) = 1.86e-11_r8 * exp_fac(:)
      rate(:,486) = 1.6e+09_r8 * exp( -8300._r8 * itemp(:) )
      exp_fac(:) = exp( -446._r8 * itemp(:) )
      rate(:,491) = 3.03e-12_r8 * exp_fac(:)
      rate(:,649) = 3.03e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 410._r8 * itemp(:) )
      rate(:,499) = 2.54e-11_r8 * exp_fac(:)
      rate(:,651) = 2.54e-11_r8 * exp_fac(:)
      rate(:,503) = 1.3e-12_r8 * exp( 640._r8 * itemp(:) )
      exp_fac(:) = exp( -193._r8 * itemp(:) )
      rate(:,511) = 2.3e-12_r8 * exp_fac(:)
      rate(:,648) = 2.3e-12_r8 * exp_fac(:)
      rate(:,515) = 5.9e-12_r8 * exp( 225._r8 * itemp(:) )
      rate(:,534) = 4.7e-13_r8 * exp( 1220._r8 * itemp(:) )
      exp_fac(:) = exp( 352._r8 * itemp(:) )
      rate(:,542) = 1.7e-12_r8 * exp_fac(:)
      rate(:,660) = 1.7e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 490._r8 * itemp(:) )
      rate(:,555) = 1.2e-12_r8 * exp_fac(:)
      rate(:,654) = 1.2e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -580._r8 * itemp(:) )
      rate(:,556) = 6.3e-16_r8 * exp_fac(:)
      rate(:,655) = 6.3e-16_r8 * exp_fac(:)
      rate(:,575) = 1e-14_r8 * exp( 950._r8 * itemp(:) )
      rate(:,577) = 9.4e-11_r8 * exp( 190._r8 * itemp(:) )
      rate(:,578) = 3.2e-13_r8 * exp( -925._r8 * itemp(:) )
      rate(:,579) = 1.9e-13_r8 * exp( 520._r8 * itemp(:) )
      rate(:,580) = 1.1e-11_r8 * exp( -280._r8 * itemp(:) )
      rate(:,581) = 2.1e-11_r8 * exp( -2200._r8 * itemp(:) )
      rate(:,582) = 7.2e-14_r8 * exp( -1070._r8 * itemp(:) )
      rate(:,590) = 1.6e-13_r8 * exp( -2280._r8 * itemp(:) )
      rate(:,593) = 2.6e-11_r8 * exp( 330._r8 * itemp(:) )
      rate(:,613) = 1.7e-12_r8 * exp( -710._r8 * itemp(:) )

      itemp(:) = 300._r8 * itemp(:)
 
      n = ncol*pver

      ko(:) = 5.3e-32_r8 * itemp(:)**1.8_r8
      kinf(:) = 9.5e-11_r8 * itemp(:)**(-0.4_r8)
      call jpl( rate(:,187), m, 0.6_r8, ko, kinf, n )

      ko(:) = 6.9e-31_r8 * itemp(:)**1._r8
      kinf(:) = 2.6e-11_r8
      call jpl( rate(:,197), m, 0.6_r8, ko, kinf, n )

      ko(:) = 2.5e-31_r8 * itemp(:)**1.8_r8
      kinf(:) = 2.2e-11_r8 * itemp(:)**0.7_r8
      call jpl( rate(:,207), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9e-32_r8 * itemp(:)**1.5_r8
      kinf(:) = 3e-11_r8
      call jpl( rate(:,215), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.9e-31_r8 * itemp(:)**3.4_r8
      kinf(:) = 4e-12_r8 * itemp(:)**0.3_r8
      call jpl( rate(:,218), m, 0.6_r8, ko, kinf, n )

      ko(:) = 2.4e-30_r8 * itemp(:)**3._r8
      kinf(:) = 1.6e-12_r8 * itemp(:)**(-0.1_r8)
      call jpl( rate(:,219), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.8e-30_r8 * itemp(:)**3._r8
      kinf(:) = 2.8e-11_r8
      call jpl( rate(:,220), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.8e-31_r8 * itemp(:)**2._r8
      kinf(:) = 1e-10_r8 * itemp(:)
      call jpl( rate(:,230), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.8e-31_r8 * itemp(:)**3.4_r8
      kinf(:) = 1.5e-11_r8 * itemp(:)**1.9_r8
      call jpl( rate(:,240), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.9e-32_r8 * itemp(:)**3.6_r8
      kinf(:) = 3.7e-12_r8 * itemp(:)**1.6_r8
      call jpl( rate(:,260), m, 0.6_r8, ko, kinf, n )

      ko(:) = 4.2e-31_r8 * itemp(:)**2.4_r8
      kinf(:) = 2.7e-11_r8
      call jpl( rate(:,265), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.2e-31_r8 * itemp(:)**3.2_r8
      kinf(:) = 6.9e-12_r8 * itemp(:)**2.9_r8
      call jpl( rate(:,275), m, 0.6_r8, ko, kinf, n )

      ko(:) = 3e-31_r8 * itemp(:)**1._r8
      kinf(:) = 6.6e-11_r8
      call jpl( rate(:,296), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.8e-32_r8 * itemp(:)**1._r8
      kinf(:) = 1.7e-11_r8
      call jpl( rate(:,300), m, 0.6_r8, ko, kinf, n )

      ko(:) = 6.5e-31_r8 * itemp(:)**3.5_r8
      kinf(:) = 7.6e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,310), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.4e-28_r8 * itemp(:)**8.5_r8
      kinf(:) = 4e-11_r8 * itemp(:)**1.2_r8
      call jpl( rate(:,332), m, 0.6_r8, ko, kinf, n )

      ko(:) = 6.1e-33_r8 * itemp(:)**1.5_r8
      kinf(:) = 9.8e-15_r8 * itemp(:)**(-4.6_r8)
      call jpl( rate(:,379), m, 0.8_r8, ko, kinf, n )

      ko(:) = 5.2e-30_r8 * itemp(:)**2.4_r8
      kinf(:) = 2.2e-10_r8 * itemp(:)**0.7_r8
      call jpl( rate(:,389), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.5e-30_r8
      kinf(:) = 8.3e-13_r8 * itemp(:)**(-2._r8)
      call jpl( rate(:,390), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.6e-29_r8 * itemp(:)**3.3_r8
      kinf(:) = 3.1e-10_r8 * itemp(:)
      call jpl( rate(:,391), m, 0.6_r8, ko, kinf, n )

      ko(:) = 8.6e-29_r8 * itemp(:)**3.1_r8
      kinf(:) = 9e-12_r8 * itemp(:)**0.85_r8
      call jpl( rate(:,417), m, 0.48_r8, ko, kinf, n )

      ko(:) = 7.3e-29_r8 * itemp(:)**4.1_r8
      kinf(:) = 9.5e-12_r8 * itemp(:)**1.6_r8
      call jpl( rate(:,418), m, 0.6_r8, ko, kinf, n )

      ko(:) = 8e-27_r8 * itemp(:)**3.5_r8
      kinf(:) = 3e-11_r8
      call jpl( rate(:,438), m, 0.5_r8, ko, kinf, n )

      ko(:) = 8e-27_r8 * itemp(:)**3.5_r8
      kinf(:) = 3e-11_r8
      call jpl( rate(:,464), m, 0.5_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,467), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,525), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,528), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,531), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,538), m, 0.6_r8, ko, kinf, n )

      ko(:) = 2.9e-31_r8 * itemp(:)**4.1_r8
      kinf(:) = 1.7e-12_r8 * itemp(:)**(-0.2_r8)
      call jpl( rate(:,584), m, 0.6_r8, ko, kinf, n )

      end subroutine setrxt


      subroutine setrxt_hrates( rate, temp, m, ncol, kbot )
 
      use ppgrid, only : pcols, pver


      use chem_mods, only : rxntot
      use mo_jpl,    only : jpl

      implicit none

!-------------------------------------------------------
!       ... dummy arguments
!-------------------------------------------------------
      integer, intent(in) :: ncol
      integer, intent(in) :: kbot
      real(r8), intent(in)    :: temp(pcols,pver)
      real(r8), intent(in)    :: m(ncol*pver)
      real(r8), intent(inout) :: rate(ncol*pver,max(1,rxntot))

!-------------------------------------------------------
!       ... local variables
!-------------------------------------------------------
      integer   ::  n
      integer   ::  offset
      integer   ::  k
      real(r8)  :: itemp(ncol*kbot)
      real(r8)  :: exp_fac(ncol*kbot)
      real(r8)  :: ko(ncol*kbot)
      real(r8)  :: kinf(ncol*kbot)
      real(r8)  :: wrk(ncol*kbot)
 
      n = ncol*kbot

      rate(:n,184) = 6.9e-12_r8
 
      do k = 1,kbot
        offset = (k-1)*ncol
        itemp(offset+1:offset+ncol) = 1._r8 / temp(:ncol,k)
      end do

      rate(:n,175) = 2.15e-11_r8 * exp( 110._r8 * itemp(:) )
      rate(:n,179) = 8e-12_r8 * exp( -2060._r8 * itemp(:) )
      rate(:n,188) = 3e-11_r8 * exp( 200._r8 * itemp(:) )
      rate(:n,189) = 1e-14_r8 * exp( -490._r8 * itemp(:) )
      rate(:n,190) = 1.4e-10_r8 * exp( -470._r8 * itemp(:) )
      rate(:n,193) = 4.8e-11_r8 * exp( 250._r8 * itemp(:) )
      rate(:n,194) = 1.8e-11_r8 * exp( 180._r8 * itemp(:) )
      rate(:n,195) = 1.7e-12_r8 * exp( -940._r8 * itemp(:) )
      rate(:n,200) = 2.1e-11_r8 * exp( 100._r8 * itemp(:) )
      rate(:n,204) = 3.3e-12_r8 * exp( -3150._r8 * itemp(:) )
      rate(:n,205) = 5.1e-12_r8 * exp( 210._r8 * itemp(:) )
      rate(:n,213) = 3.44e-12_r8 * exp( 260._r8 * itemp(:) )
      rate(:n,214) = 3e-12_r8 * exp( -1500._r8 * itemp(:) )

      itemp(:) = 300._r8 * itemp(:)

      ko(:) = 5.3e-32_r8 * itemp(:)**1.8_r8
      kinf(:) = 9.5e-11_r8 * itemp(:)**(-0.4_r8)
      call jpl( wrk, m, 0.6_r8, ko, kinf, n )
      rate(:n,187) = wrk(:)






























      end subroutine setrxt_hrates

      end module mo_setrxt
