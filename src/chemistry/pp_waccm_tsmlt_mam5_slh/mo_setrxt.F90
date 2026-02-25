
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

      rate(:,180) = 0.000258_r8
      rate(:,181) = 0.085_r8
      rate(:,182) = 1.2e-10_r8
      rate(:,187) = 1.2e-10_r8
      rate(:,188) = 1.2e-10_r8
      rate(:,189) = 1e-20_r8
      rate(:,190) = 1.3e-16_r8
      rate(:,192) = 4.2e-13_r8
      rate(:,194) = 8e-14_r8
      rate(:,195) = 3.9e-17_r8
      rate(:,202) = 6.9e-12_r8
      rate(:,203) = 7.2e-11_r8
      rate(:,204) = 1.6e-12_r8
      rate(:,210) = 1.8e-12_r8
      rate(:,214) = 1.8e-12_r8
      rate(:,217) = 1.06e-05_r8
      rate(:,219) = 7e-11_r8
      rate(:,220) = 7e-13_r8
      rate(:,228) = 3.5e-12_r8
      rate(:,230) = 1.3e-11_r8
      rate(:,231) = 2.2e-11_r8
      rate(:,232) = 5e-11_r8
      rate(:,272) = 1.7e-13_r8
      rate(:,274) = 2.607e-10_r8
      rate(:,275) = 9.75e-11_r8
      rate(:,276) = 2.07e-10_r8
      rate(:,277) = 2.088e-10_r8
      rate(:,278) = 1.17e-10_r8
      rate(:,279) = 4.644e-11_r8
      rate(:,280) = 1.204e-10_r8
      rate(:,281) = 9.9e-11_r8
      rate(:,282) = 3.3e-12_r8
      rate(:,305) = 4.5e-11_r8
      rate(:,306) = 4.62e-10_r8
      rate(:,307) = 1.2e-10_r8
      rate(:,308) = 9e-11_r8
      rate(:,309) = 3e-11_r8
      rate(:,311) = 2e-13_r8
      rate(:,312) = 1.5e-12_r8
      rate(:,313) = 1.25e-10_r8
      rate(:,314) = 1.8e-10_r8
      rate(:,315) = 1.44e-11_r8
      rate(:,321) = 1e-10_r8
      rate(:,325) = 2.49e-11_r8
      rate(:,334) = 9e-12_r8
      rate(:,335) = 1.4e-10_r8
      rate(:,336) = 3.6e-16_r8
      rate(:,337) = 1e-10_r8
      rate(:,353) = 2.14e-11_r8
      rate(:,354) = 1.9e-10_r8
      rate(:,357) = 1.3e-12_r8
      rate(:,375) = 1.2e-12_r8
      rate(:,376) = 8e-13_r8
      rate(:,380) = 2.3e-12_r8
      rate(:,386) = 2.57e-10_r8
      rate(:,387) = 1.8e-10_r8
      rate(:,388) = 1.794e-10_r8
      rate(:,389) = 1.3e-10_r8
      rate(:,390) = 7.65e-11_r8
      rate(:,403) = 4e-13_r8
      rate(:,407) = 1.31e-10_r8
      rate(:,408) = 3.5e-11_r8
      rate(:,409) = 9e-12_r8
      rate(:,416) = 6.8e-14_r8
      rate(:,417) = 2e-13_r8
      rate(:,432) = 1e-12_r8
      rate(:,436) = 1e-14_r8
      rate(:,437) = 1e-11_r8
      rate(:,438) = 1.15e-11_r8
      rate(:,439) = 4e-14_r8
      rate(:,452) = 1.45e-10_r8
      rate(:,453) = 3e-12_r8
      rate(:,454) = 6.7e-13_r8
      rate(:,464) = 3.5e-13_r8
      rate(:,465) = 5.4e-11_r8
      rate(:,468) = 2e-12_r8
      rate(:,469) = 1.4e-11_r8
      rate(:,472) = 2.4e-12_r8
      rate(:,483) = 5e-12_r8
      rate(:,493) = 2.2e-12_r8
      rate(:,495) = 6.7e-12_r8
      rate(:,498) = 3.5e-12_r8
      rate(:,501) = 1.3e-11_r8
      rate(:,502) = 1.4e-11_r8
      rate(:,506) = 2.4e-12_r8
      rate(:,507) = 1.4e-11_r8
      rate(:,512) = 2.4e-12_r8
      rate(:,513) = 4e-11_r8
      rate(:,514) = 4e-11_r8
      rate(:,516) = 1.4e-11_r8
      rate(:,520) = 2.4e-12_r8
      rate(:,521) = 4e-11_r8
      rate(:,525) = 7e-11_r8
      rate(:,526) = 1e-10_r8
      rate(:,531) = 2.4e-12_r8
      rate(:,546) = 4.7e-11_r8
      rate(:,559) = 2.1e-12_r8
      rate(:,560) = 2.8e-13_r8
      rate(:,568) = 1.7e-11_r8
      rate(:,574) = 8.4e-11_r8
      rate(:,576) = 1.9e-11_r8
      rate(:,577) = 1.2e-14_r8
      rate(:,578) = 2e-10_r8
      rate(:,585) = 2.4e-12_r8
      rate(:,586) = 2e-11_r8
      rate(:,590) = 2.3e-11_r8
      rate(:,591) = 2e-11_r8
      rate(:,595) = 3.3e-11_r8
      rate(:,596) = 1e-12_r8
      rate(:,597) = 5.7e-11_r8
      rate(:,598) = 3.4e-11_r8
      rate(:,600) = 3.3e-10_r8
      rate(:,607) = 2.3e-12_r8
      rate(:,609) = 1.2e-11_r8
      rate(:,610) = 5.7e-11_r8
      rate(:,611) = 2.8e-11_r8
      rate(:,612) = 6.6e-11_r8
      rate(:,613) = 1.4e-11_r8
      rate(:,616) = 1.9e-12_r8
      rate(:,647) = 6.34e-08_r8
      rate(:,669) = 1.9e-11_r8
      rate(:,672) = 1.2e-14_r8
      rate(:,673) = 2e-10_r8
      rate(:,684) = 1.34e-11_r8
      rate(:,690) = 1.34e-11_r8
      rate(:,695) = 1.7e-11_r8
      rate(:,743) = 6e-11_r8
      rate(:,746) = 1e-12_r8
      rate(:,747) = 4e-10_r8
      rate(:,748) = 2e-10_r8
      rate(:,749) = 1e-10_r8
      rate(:,750) = 5e-16_r8
      rate(:,751) = 4.4e-10_r8
      rate(:,752) = 9e-10_r8
      rate(:,755) = 1.29e-07_r8
      rate(:,756) = 2.31e-07_r8
      rate(:,757) = 2.31e-06_r8
      rate(:,758) = 4.63e-07_r8
 
      do n = 1,pver
        offset = (n-1)*ncol
        itemp(offset+1:offset+ncol) = 1._r8 / temp(:ncol,n)
      end do

      rate(:,183) = 1.63e-10_r8 * exp( 60._r8 * itemp(:) )
      rate(:,184) = 2.15e-11_r8 * exp( 110._r8 * itemp(:) )
      exp_fac(:) = exp( 55._r8 * itemp(:) )
      rate(:,185) = 2.64e-11_r8 * exp_fac(:)
      rate(:,186) = 6.6e-12_r8 * exp_fac(:)
      rate(:,191) = 3.6e-18_r8 * exp( -220._r8 * itemp(:) )
      rate(:,193) = 1.8e-15_r8 * exp( 45._r8 * itemp(:) )
      rate(:,196) = 3.5e-11_r8 * exp( -135._r8 * itemp(:) )
      rate(:,197) = 8e-12_r8 * exp( -2060._r8 * itemp(:) )
      rate(:,200) = 1.6e-11_r8 * exp( -4570._r8 * itemp(:) )
      exp_fac(:) = exp( -2000._r8 * itemp(:) )
      rate(:,201) = 1.4e-12_r8 * exp_fac(:)
      rate(:,522) = 1.05e-14_r8 * exp_fac(:)
      rate(:,680) = 1.05e-14_r8 * exp_fac(:)
      exp_fac(:) = exp( 200._r8 * itemp(:) )
      rate(:,206) = 3e-11_r8 * exp_fac(:)
      rate(:,303) = 5.5e-12_r8 * exp_fac(:)
      rate(:,400) = 3.8e-12_r8 * exp_fac(:)
      rate(:,421) = 3.8e-12_r8 * exp_fac(:)
      rate(:,448) = 3.8e-12_r8 * exp_fac(:)
      rate(:,457) = 3.8e-12_r8 * exp_fac(:)
      rate(:,461) = 3.8e-12_r8 * exp_fac(:)
      rate(:,477) = 2.3e-11_r8 * exp_fac(:)
      rate(:,487) = 3.8e-12_r8 * exp_fac(:)
      rate(:,497) = 3.8e-12_r8 * exp_fac(:)
      rate(:,524) = 1.52e-11_r8 * exp_fac(:)
      rate(:,532) = 1.52e-12_r8 * exp_fac(:)
      rate(:,538) = 3.8e-12_r8 * exp_fac(:)
      rate(:,541) = 3.8e-12_r8 * exp_fac(:)
      rate(:,545) = 3.8e-12_r8 * exp_fac(:)
      rate(:,561) = 3.8e-12_r8 * exp_fac(:)
      rate(:,565) = 3.8e-12_r8 * exp_fac(:)
      rate(:,571) = 3.8e-12_r8 * exp_fac(:)
      rate(:,575) = 3.8e-12_r8 * exp_fac(:)
      rate(:,207) = 1e-14_r8 * exp( -490._r8 * itemp(:) )
      rate(:,208) = 1.4e-10_r8 * exp( -470._r8 * itemp(:) )
      rate(:,209) = 2.8e-12_r8 * exp( -1800._r8 * itemp(:) )
      exp_fac(:) = exp( 250._r8 * itemp(:) )
      rate(:,211) = 4.8e-11_r8 * exp_fac(:)
      rate(:,301) = 1.7e-11_r8 * exp_fac(:)
      exp_fac(:) = exp( 180._r8 * itemp(:) )
      rate(:,212) = 1.8e-11_r8 * exp_fac(:)
      rate(:,434) = 4.2e-12_r8 * exp_fac(:)
      rate(:,456) = 4.2e-12_r8 * exp_fac(:)
      rate(:,485) = 4.2e-12_r8 * exp_fac(:)
      rate(:,505) = 4.4e-12_r8 * exp_fac(:)
      rate(:,511) = 4.4e-12_r8 * exp_fac(:)
      rate(:,584) = 4.2e-12_r8 * exp_fac(:)
      rate(:,589) = 4.2e-12_r8 * exp_fac(:)
      rate(:,594) = 4.2e-12_r8 * exp_fac(:)
      rate(:,213) = 1.7e-12_r8 * exp( -940._r8 * itemp(:) )
      rate(:,218) = 4.5e-13_r8 * exp( 610._r8 * itemp(:) )
      rate(:,221) = 2.1e-11_r8 * exp( 100._r8 * itemp(:) )
      exp_fac(:) = exp( 220._r8 * itemp(:) )
      rate(:,222) = 2.9e-12_r8 * exp_fac(:)
      rate(:,223) = 1.45e-12_r8 * exp_fac(:)
      rate(:,224) = 1.45e-12_r8 * exp_fac(:)
      rate(:,225) = 5.1e-12_r8 * exp( 210._r8 * itemp(:) )
      exp_fac(:) = exp( -2450._r8 * itemp(:) )
      rate(:,226) = 1.2e-13_r8 * exp_fac(:)
      rate(:,257) = 3e-11_r8 * exp_fac(:)
      exp_fac(:) = exp( 125._r8 * itemp(:) )
      rate(:,229) = 1.7e-11_r8 * exp_fac(:)
      rate(:,394) = 5.5e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 260._r8 * itemp(:) )
      rate(:,233) = 3.44e-12_r8 * exp_fac(:)
      rate(:,292) = 2.3e-12_r8 * exp_fac(:)
      rate(:,295) = 8.8e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -1500._r8 * itemp(:) )
      rate(:,234) = 3e-12_r8 * exp_fac(:)
      rate(:,302) = 5.8e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 20._r8 * itemp(:) )
      rate(:,236) = 7.26e-11_r8 * exp_fac(:)
      rate(:,237) = 4.64e-11_r8 * exp_fac(:)
      rate(:,247) = 8.1e-11_r8 * exp( -30._r8 * itemp(:) )
      rate(:,248) = 7.1e-12_r8 * exp( -1270._r8 * itemp(:) )
      rate(:,249) = 3.05e-11_r8 * exp( -2270._r8 * itemp(:) )
      rate(:,250) = 1.1e-11_r8 * exp( -980._r8 * itemp(:) )
      exp_fac(:) = exp( 270._r8 * itemp(:) )
      rate(:,251) = 1.4e-11_r8 * exp_fac(:)
      rate(:,267) = 7.4e-12_r8 * exp_fac(:)
      rate(:,430) = 8.1e-12_r8 * exp_fac(:)
      rate(:,252) = 3.6e-11_r8 * exp( -375._r8 * itemp(:) )
      rate(:,254) = 2.4e-12_r8 * exp( -1250._r8 * itemp(:) )
      rate(:,255) = 2.3e-11_r8 * exp( -200._r8 * itemp(:) )
      rate(:,256) = 3.3e-12_r8 * exp( -115._r8 * itemp(:) )
      rate(:,258) = 1e-12_r8 * exp( -1590._r8 * itemp(:) )
      rate(:,259) = 3.5e-13_r8 * exp( -1370._r8 * itemp(:) )
      exp_fac(:) = exp( 290._r8 * itemp(:) )
      rate(:,260) = 2.6e-12_r8 * exp_fac(:)
      rate(:,261) = 6.4e-12_r8 * exp_fac(:)
      rate(:,293) = 4.1e-13_r8 * exp_fac(:)
      rate(:,534) = 7.5e-12_r8 * exp_fac(:)
      rate(:,548) = 7.5e-12_r8 * exp_fac(:)
      rate(:,551) = 7.5e-12_r8 * exp_fac(:)
      rate(:,554) = 7.5e-12_r8 * exp_fac(:)
      rate(:,262) = 6.5e-12_r8 * exp( 135._r8 * itemp(:) )
      exp_fac(:) = exp( -840._r8 * itemp(:) )
      rate(:,264) = 3.6e-12_r8 * exp_fac(:)
      rate(:,360) = 2e-12_r8 * exp_fac(:)
      rate(:,265) = 1.2e-12_r8 * exp( -330._r8 * itemp(:) )
      rate(:,266) = 2.8e-11_r8 * exp( 85._r8 * itemp(:) )
      exp_fac(:) = exp( 230._r8 * itemp(:) )
      rate(:,268) = 6e-13_r8 * exp_fac(:)
      rate(:,290) = 1.5e-12_r8 * exp_fac(:)
      rate(:,300) = 1.9e-11_r8 * exp_fac(:)
      rate(:,269) = 1e-11_r8 * exp( -3300._r8 * itemp(:) )
      rate(:,270) = 1.8e-12_r8 * exp( -250._r8 * itemp(:) )
      rate(:,271) = 3.4e-12_r8 * exp( -130._r8 * itemp(:) )
      exp_fac(:) = exp( -500._r8 * itemp(:) )
      rate(:,273) = 3e-12_r8 * exp_fac(:)
      rate(:,350) = 1.4e-10_r8 * exp_fac(:)
      rate(:,285) = 2.1e-11_r8 * exp( 240._r8 * itemp(:) )
      exp_fac(:) = exp( -800._r8 * itemp(:) )
      rate(:,286) = 1.7e-11_r8 * exp_fac(:)
      rate(:,359) = 6.3e-12_r8 * exp_fac(:)
      rate(:,287) = 4.8e-12_r8 * exp( -310._r8 * itemp(:) )
      rate(:,289) = 1.6e-11_r8 * exp( -780._r8 * itemp(:) )
      rate(:,291) = 9.5e-13_r8 * exp( 550._r8 * itemp(:) )
      rate(:,294) = 4.5e-12_r8 * exp( 460._r8 * itemp(:) )
      exp_fac(:) = exp( 365._r8 * itemp(:) )
      rate(:,296) = 1.78e-11_r8 * exp_fac(:)
      rate(:,419) = 2.6e-12_r8 * exp_fac(:)
      rate(:,537) = 2.6e-12_r8 * exp_fac(:)
      rate(:,542) = 2.6e-12_r8 * exp_fac(:)
      rate(:,544) = 2.6e-12_r8 * exp_fac(:)
      rate(:,557) = 2.6e-12_r8 * exp_fac(:)
      rate(:,564) = 2.6e-12_r8 * exp_fac(:)
      rate(:,570) = 2.6e-12_r8 * exp_fac(:)
      rate(:,573) = 2.6e-12_r8 * exp_fac(:)
      rate(:,676) = 2.6e-12_r8 * exp_fac(:)
      rate(:,683) = 2.6e-12_r8 * exp_fac(:)
      rate(:,693) = 2.6e-12_r8 * exp_fac(:)
      rate(:,697) = 2.6e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 215._r8 * itemp(:) )
      rate(:,297) = 6.28e-11_r8 * exp_fac(:)
      rate(:,299) = 1.9e-11_r8 * exp_fac(:)
      rate(:,304) = 1.2e-10_r8 * exp( -430._r8 * itemp(:) )
      rate(:,310) = 1.3e-12_r8 * exp( -1830._r8 * itemp(:) )
      rate(:,316) = 1.5e-11_r8 * exp( -1090._r8 * itemp(:) )
      rate(:,317) = 9.1e-11_r8 * exp( -146._r8 * itemp(:) )
      rate(:,318) = 4.7e-13_r8 * exp( -1670._r8 * itemp(:) )
      rate(:,320) = 1.008e+18_r8 * exp( -13670._r8 * itemp(:) )
      rate(:,322) = 8.4e-11_r8 * exp( -2620._r8 * itemp(:) )
      rate(:,324) = 2.1e-11_r8 * exp( -830._r8 * itemp(:) )
      exp_fac(:) = exp( 510._r8 * itemp(:) )
      rate(:,326) = 3e-12_r8 * exp_fac(:)
      rate(:,327) = 1.2e-11_r8 * exp_fac(:)
      exp_fac(:) = exp( 280._r8 * itemp(:) )
      rate(:,328) = 2.585e-12_r8 * exp_fac(:)
      rate(:,329) = 1.175e-12_r8 * exp_fac(:)
      rate(:,330) = 9.4e-13_r8 * exp_fac(:)
      rate(:,331) = 1.3e-11_r8 * exp( 570._r8 * itemp(:) )
      exp_fac(:) = exp( 300._r8 * itemp(:) )
      rate(:,332) = 7.15e-12_r8 * exp_fac(:)
      rate(:,398) = 2.8e-12_r8 * exp_fac(:)
      rate(:,460) = 2.9e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 440._r8 * itemp(:) )
      rate(:,338) = 1.6e-11_r8 * exp_fac(:)
      rate(:,581) = 1.2e-11_r8 * exp_fac(:)
      rate(:,689) = 1.2e-11_r8 * exp_fac(:)
      rate(:,339) = 1.1e-12_r8 * exp( 542._r8 * itemp(:) )
      rate(:,349) = 1.6e-10_r8 * exp( -260._r8 * itemp(:) )
      exp_fac(:) = exp( 0._r8 * itemp(:) )
      rate(:,351) = 1.4e-11_r8 * exp_fac(:)
      rate(:,353) = 2.14e-11_r8 * exp_fac(:)
      rate(:,354) = 1.9e-10_r8 * exp_fac(:)
      rate(:,357) = 1.3e-12_r8 * exp_fac(:)
      rate(:,375) = 1.2e-12_r8 * exp_fac(:)
      rate(:,376) = 8e-13_r8 * exp_fac(:)
      rate(:,380) = 2.3e-12_r8 * exp_fac(:)
      rate(:,386) = 2.57e-10_r8 * exp_fac(:)
      rate(:,387) = 1.8e-10_r8 * exp_fac(:)
      rate(:,388) = 1.794e-10_r8 * exp_fac(:)
      rate(:,389) = 1.3e-10_r8 * exp_fac(:)
      rate(:,390) = 7.65e-11_r8 * exp_fac(:)
      rate(:,403) = 4e-13_r8 * exp_fac(:)
      rate(:,407) = 1.31e-10_r8 * exp_fac(:)
      rate(:,408) = 3.5e-11_r8 * exp_fac(:)
      rate(:,409) = 9e-12_r8 * exp_fac(:)
      rate(:,416) = 6.8e-14_r8 * exp_fac(:)
      rate(:,417) = 2e-13_r8 * exp_fac(:)
      rate(:,432) = 1e-12_r8 * exp_fac(:)
      rate(:,436) = 1e-14_r8 * exp_fac(:)
      rate(:,437) = 1e-11_r8 * exp_fac(:)
      rate(:,438) = 1.15e-11_r8 * exp_fac(:)
      rate(:,439) = 4e-14_r8 * exp_fac(:)
      rate(:,452) = 1.45e-10_r8 * exp_fac(:)
      rate(:,453) = 3e-12_r8 * exp_fac(:)
      rate(:,454) = 6.7e-13_r8 * exp_fac(:)
      rate(:,464) = 3.5e-13_r8 * exp_fac(:)
      rate(:,465) = 5.4e-11_r8 * exp_fac(:)
      rate(:,468) = 2e-12_r8 * exp_fac(:)
      rate(:,469) = 1.4e-11_r8 * exp_fac(:)
      rate(:,472) = 2.4e-12_r8 * exp_fac(:)
      rate(:,483) = 5e-12_r8 * exp_fac(:)
      rate(:,493) = 2.2e-12_r8 * exp_fac(:)
      rate(:,495) = 6.7e-12_r8 * exp_fac(:)
      rate(:,498) = 3.5e-12_r8 * exp_fac(:)
      rate(:,501) = 1.3e-11_r8 * exp_fac(:)
      rate(:,502) = 1.4e-11_r8 * exp_fac(:)
      rate(:,506) = 2.4e-12_r8 * exp_fac(:)
      rate(:,507) = 1.4e-11_r8 * exp_fac(:)
      rate(:,512) = 2.4e-12_r8 * exp_fac(:)
      rate(:,513) = 4e-11_r8 * exp_fac(:)
      rate(:,514) = 4e-11_r8 * exp_fac(:)
      rate(:,516) = 1.4e-11_r8 * exp_fac(:)
      rate(:,520) = 2.4e-12_r8 * exp_fac(:)
      rate(:,521) = 4e-11_r8 * exp_fac(:)
      rate(:,525) = 7e-11_r8 * exp_fac(:)
      rate(:,526) = 1e-10_r8 * exp_fac(:)
      rate(:,531) = 2.4e-12_r8 * exp_fac(:)
      rate(:,546) = 4.7e-11_r8 * exp_fac(:)
      rate(:,559) = 2.1e-12_r8 * exp_fac(:)
      rate(:,560) = 2.8e-13_r8 * exp_fac(:)
      rate(:,568) = 1.7e-11_r8 * exp_fac(:)
      rate(:,574) = 8.4e-11_r8 * exp_fac(:)
      rate(:,576) = 1.9e-11_r8 * exp_fac(:)
      rate(:,577) = 1.2e-14_r8 * exp_fac(:)
      rate(:,578) = 2e-10_r8 * exp_fac(:)
      rate(:,585) = 2.4e-12_r8 * exp_fac(:)
      rate(:,586) = 2e-11_r8 * exp_fac(:)
      rate(:,590) = 2.3e-11_r8 * exp_fac(:)
      rate(:,591) = 2e-11_r8 * exp_fac(:)
      rate(:,595) = 3.3e-11_r8 * exp_fac(:)
      rate(:,596) = 1e-12_r8 * exp_fac(:)
      rate(:,597) = 5.7e-11_r8 * exp_fac(:)
      rate(:,598) = 3.4e-11_r8 * exp_fac(:)
      rate(:,600) = 3.3e-10_r8 * exp_fac(:)
      rate(:,607) = 2.3e-12_r8 * exp_fac(:)
      rate(:,609) = 1.2e-11_r8 * exp_fac(:)
      rate(:,610) = 5.7e-11_r8 * exp_fac(:)
      rate(:,611) = 2.8e-11_r8 * exp_fac(:)
      rate(:,612) = 6.6e-11_r8 * exp_fac(:)
      rate(:,613) = 1.4e-11_r8 * exp_fac(:)
      rate(:,616) = 1.9e-12_r8 * exp_fac(:)
      rate(:,647) = 6.34e-08_r8 * exp_fac(:)
      rate(:,669) = 1.9e-11_r8 * exp_fac(:)
      rate(:,672) = 1.2e-14_r8 * exp_fac(:)
      rate(:,673) = 2e-10_r8 * exp_fac(:)
      rate(:,684) = 1.34e-11_r8 * exp_fac(:)
      rate(:,690) = 1.34e-11_r8 * exp_fac(:)
      rate(:,695) = 1.7e-11_r8 * exp_fac(:)
      rate(:,743) = 6e-11_r8 * exp_fac(:)
      rate(:,746) = 1e-12_r8 * exp_fac(:)
      rate(:,747) = 4e-10_r8 * exp_fac(:)
      rate(:,748) = 2e-10_r8 * exp_fac(:)
      rate(:,749) = 1e-10_r8 * exp_fac(:)
      rate(:,750) = 5e-16_r8 * exp_fac(:)
      rate(:,751) = 4.4e-10_r8 * exp_fac(:)
      rate(:,752) = 9e-10_r8 * exp_fac(:)
      rate(:,755) = 1.29e-07_r8 * exp_fac(:)
      rate(:,756) = 2.31e-07_r8 * exp_fac(:)
      rate(:,757) = 2.31e-06_r8 * exp_fac(:)
      rate(:,758) = 4.63e-07_r8 * exp_fac(:)
      exp_fac(:) = exp( 400._r8 * itemp(:) )
      rate(:,352) = 6e-12_r8 * exp_fac(:)
      rate(:,470) = 5e-13_r8 * exp_fac(:)
      rate(:,503) = 5e-13_r8 * exp_fac(:)
      rate(:,508) = 5e-13_r8 * exp_fac(:)
      rate(:,517) = 5e-13_r8 * exp_fac(:)
      rate(:,528) = 5e-13_r8 * exp_fac(:)
      exp_fac(:) = exp( -990._r8 * itemp(:) )
      rate(:,356) = 4.7e-12_r8 * exp_fac(:)
      rate(:,381) = 3.3e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -1150._r8 * itemp(:) )
      rate(:,358) = 1.14e-11_r8 * exp_fac(:)
      rate(:,365) = 1.42e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -880._r8 * itemp(:) )
      rate(:,361) = 2.1e-12_r8 * exp_fac(:)
      rate(:,363) = 1.92e-12_r8 * exp_fac(:)
      rate(:,362) = 7.4e-12_r8 * exp( -910._r8 * itemp(:) )
      rate(:,364) = 1.46e-11_r8 * exp( -1040._r8 * itemp(:) )
      exp_fac(:) = exp( -1520._r8 * itemp(:) )
      rate(:,366) = 1.64e-12_r8 * exp_fac(:)
      rate(:,489) = 8.5e-16_r8 * exp_fac(:)
      rate(:,367) = 2.03e-11_r8 * exp( -1110._r8 * itemp(:) )
      rate(:,368) = 1.96e-12_r8 * exp( -1200._r8 * itemp(:) )
      rate(:,369) = 2.9e-11_r8 * exp( -1000._r8 * itemp(:) )
      exp_fac(:) = exp( -1100._r8 * itemp(:) )
      rate(:,370) = 2.9e-12_r8 * exp_fac(:)
      rate(:,615) = 3.4e-12_r8 * exp_fac(:)
      rate(:,371) = 9e-13_r8 * exp( -420._r8 * itemp(:) )
      rate(:,372) = 4.85e-12_r8 * exp( -850._r8 * itemp(:) )
      rate(:,373) = 9e-13_r8 * exp( -360._r8 * itemp(:) )
      rate(:,374) = 9.4e-13_r8 * exp( -510._r8 * itemp(:) )
      exp_fac(:) = exp( 700._r8 * itemp(:) )
      rate(:,377) = 3.92e-13_r8 * exp_fac(:)
      rate(:,378) = 1.68e-13_r8 * exp_fac(:)
      rate(:,404) = 7.5e-13_r8 * exp_fac(:)
      rate(:,418) = 7.5e-13_r8 * exp_fac(:)
      rate(:,433) = 7.5e-13_r8 * exp_fac(:)
      rate(:,455) = 7.5e-13_r8 * exp_fac(:)
      rate(:,459) = 8.6e-13_r8 * exp_fac(:)
      rate(:,471) = 8e-13_r8 * exp_fac(:)
      rate(:,484) = 7.5e-13_r8 * exp_fac(:)
      rate(:,494) = 7.5e-13_r8 * exp_fac(:)
      rate(:,504) = 8e-13_r8 * exp_fac(:)
      rate(:,509) = 8e-13_r8 * exp_fac(:)
      rate(:,518) = 8e-13_r8 * exp_fac(:)
      rate(:,529) = 8e-13_r8 * exp_fac(:)
      rate(:,536) = 7.5e-13_r8 * exp_fac(:)
      rate(:,540) = 7.5e-13_r8 * exp_fac(:)
      rate(:,543) = 7.5e-13_r8 * exp_fac(:)
      rate(:,556) = 7.5e-13_r8 * exp_fac(:)
      rate(:,563) = 7.5e-13_r8 * exp_fac(:)
      rate(:,569) = 7.5e-13_r8 * exp_fac(:)
      rate(:,572) = 7.5e-13_r8 * exp_fac(:)
      rate(:,583) = 7.5e-13_r8 * exp_fac(:)
      rate(:,588) = 7.5e-13_r8 * exp_fac(:)
      rate(:,593) = 7.5e-13_r8 * exp_fac(:)
      rate(:,675) = 7.5e-13_r8 * exp_fac(:)
      rate(:,682) = 7.5e-13_r8 * exp_fac(:)
      rate(:,692) = 7.5e-13_r8 * exp_fac(:)
      rate(:,696) = 7.5e-13_r8 * exp_fac(:)
      exp_fac(:) = exp( 360._r8 * itemp(:) )
      rate(:,379) = 4.05e-12_r8 * exp_fac(:)
      rate(:,447) = 2.7e-12_r8 * exp_fac(:)
      rate(:,473) = 2.7e-12_r8 * exp_fac(:)
      rate(:,474) = 1.3e-13_r8 * exp_fac(:)
      rate(:,476) = 9.6e-12_r8 * exp_fac(:)
      rate(:,482) = 5.3e-12_r8 * exp_fac(:)
      rate(:,519) = 2.7e-12_r8 * exp_fac(:)
      rate(:,530) = 2.7e-12_r8 * exp_fac(:)
      rate(:,671) = 2.7e-12_r8 * exp_fac(:)
      rate(:,687) = 2.7e-12_r8 * exp_fac(:)
      rate(:,382) = 2.2e-12_r8 * exp( -920._r8 * itemp(:) )
      exp_fac(:) = exp( -1600._r8 * itemp(:) )
      rate(:,383) = 1.25e-12_r8 * exp_fac(:)
      rate(:,393) = 3.4e-11_r8 * exp_fac(:)
      rate(:,384) = 1.3e-12_r8 * exp( -1770._r8 * itemp(:) )
      rate(:,385) = 9.2e-13_r8 * exp( -1560._r8 * itemp(:) )
      rate(:,391) = 9.7e-15_r8 * exp( 625._r8 * itemp(:) )
      rate(:,392) = 6e-13_r8 * exp( -2058._r8 * itemp(:) )
      rate(:,395) = 5e-13_r8 * exp( -424._r8 * itemp(:) )
      rate(:,396) = 1.9e-14_r8 * exp( 706._r8 * itemp(:) )
      rate(:,397) = 4.1e-13_r8 * exp( 750._r8 * itemp(:) )
      rate(:,399) = 2.9e-12_r8 * exp( -345._r8 * itemp(:) )
      rate(:,401) = 2.45e-12_r8 * exp( -1775._r8 * itemp(:) )
      rate(:,405) = 2.4e+12_r8 * exp( -7000._r8 * itemp(:) )
      rate(:,406) = 2.6e-12_r8 * exp( 265._r8 * itemp(:) )
      rate(:,410) = 1.08e-10_r8 * exp( 105._r8 * itemp(:) )
      rate(:,415) = 1.2e-14_r8 * exp( -2630._r8 * itemp(:) )
      rate(:,420) = 6.9e-12_r8 * exp( -230._r8 * itemp(:) )
      rate(:,422) = 7.2e-11_r8 * exp( -70._r8 * itemp(:) )
      rate(:,423) = 7.66e-12_r8 * exp( -1020._r8 * itemp(:) )
      exp_fac(:) = exp( -1900._r8 * itemp(:) )
      rate(:,424) = 1.4e-12_r8 * exp_fac(:)
      rate(:,444) = 6.5e-15_r8 * exp_fac(:)
      exp_fac(:) = exp( 350._r8 * itemp(:) )
      rate(:,425) = 4.63e-12_r8 * exp_fac(:)
      rate(:,679) = 2.7e-12_r8 * exp_fac(:)
      rate(:,426) = 7.8e-13_r8 * exp( -1050._r8 * itemp(:) )
      exp_fac(:) = exp( 500._r8 * itemp(:) )
      rate(:,427) = 2.9e-12_r8 * exp_fac(:)
      rate(:,428) = 2e-12_r8 * exp_fac(:)
      rate(:,458) = 7.1e-13_r8 * exp_fac(:)
      rate(:,479) = 2e-12_r8 * exp_fac(:)
      rate(:,582) = 2e-12_r8 * exp_fac(:)
      rate(:,587) = 2e-12_r8 * exp_fac(:)
      rate(:,592) = 2e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 1040._r8 * itemp(:) )
      rate(:,429) = 4.3e-13_r8 * exp_fac(:)
      rate(:,480) = 4.3e-13_r8 * exp_fac(:)
      rate(:,533) = 4.3e-13_r8 * exp_fac(:)
      rate(:,547) = 4.3e-13_r8 * exp_fac(:)
      rate(:,550) = 4.3e-13_r8 * exp_fac(:)
      rate(:,553) = 4.3e-13_r8 * exp_fac(:)
      rate(:,431) = 3.15e-14_r8 * exp( 920._r8 * itemp(:) )
      rate(:,435) = 1.6e+11_r8 * exp( -4150._r8 * itemp(:) )
      rate(:,443) = 4.6e-13_r8 * exp( -1156._r8 * itemp(:) )
      rate(:,445) = 1e-13_r8 * exp( 557._r8 * itemp(:) )
      exp_fac(:) = exp( 1300._r8 * itemp(:) )
      rate(:,446) = 1.41e-13_r8 * exp_fac(:)
      rate(:,670) = 2.75e-13_r8 * exp_fac(:)
      rate(:,678) = 2.12e-13_r8 * exp_fac(:)
      rate(:,686) = 2.6e-13_r8 * exp_fac(:)
      rate(:,449) = 9.19e-12_r8 * exp( -630._r8 * itemp(:) )
      rate(:,450) = 1.4e-12_r8 * exp( -1860._r8 * itemp(:) )
      rate(:,451) = 8.4e-13_r8 * exp( 830._r8 * itemp(:) )
      rate(:,466) = 4.8e-12_r8 * exp( 120._r8 * itemp(:) )
      rate(:,467) = 5.1e-14_r8 * exp( 693._r8 * itemp(:) )
      rate(:,475) = 1.5e-15_r8 * exp( -2100._r8 * itemp(:) )
      exp_fac(:) = exp( 530._r8 * itemp(:) )
      rate(:,478) = 4.6e-12_r8 * exp_fac(:)
      rate(:,481) = 2.3e-12_r8 * exp_fac(:)
      rate(:,486) = 2.3e-12_r8 * exp( -170._r8 * itemp(:) )
      rate(:,490) = 4.13e-12_r8 * exp( 452._r8 * itemp(:) )
      rate(:,496) = 5.4e-14_r8 * exp( 870._r8 * itemp(:) )
      exp_fac(:) = exp( 175._r8 * itemp(:) )
      rate(:,499) = 1.86e-11_r8 * exp_fac(:)
      rate(:,500) = 1.86e-11_r8 * exp_fac(:)
      rate(:,510) = 1.6e+09_r8 * exp( -8300._r8 * itemp(:) )
      exp_fac(:) = exp( -446._r8 * itemp(:) )
      rate(:,515) = 3.03e-12_r8 * exp_fac(:)
      rate(:,677) = 3.03e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 410._r8 * itemp(:) )
      rate(:,523) = 2.54e-11_r8 * exp_fac(:)
      rate(:,681) = 2.54e-11_r8 * exp_fac(:)
      rate(:,527) = 1.3e-12_r8 * exp( 640._r8 * itemp(:) )
      exp_fac(:) = exp( -193._r8 * itemp(:) )
      rate(:,535) = 2.3e-12_r8 * exp_fac(:)
      rate(:,674) = 2.3e-12_r8 * exp_fac(:)
      rate(:,539) = 5.9e-12_r8 * exp( 225._r8 * itemp(:) )
      rate(:,558) = 4.7e-13_r8 * exp( 1220._r8 * itemp(:) )
      exp_fac(:) = exp( 352._r8 * itemp(:) )
      rate(:,566) = 1.7e-12_r8 * exp_fac(:)
      rate(:,691) = 1.7e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 490._r8 * itemp(:) )
      rate(:,579) = 1.2e-12_r8 * exp_fac(:)
      rate(:,685) = 1.2e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -580._r8 * itemp(:) )
      rate(:,580) = 6.3e-16_r8 * exp_fac(:)
      rate(:,688) = 6.3e-16_r8 * exp_fac(:)
      rate(:,599) = 1e-14_r8 * exp( 950._r8 * itemp(:) )
      rate(:,601) = 9.4e-11_r8 * exp( 190._r8 * itemp(:) )
      rate(:,602) = 3.2e-13_r8 * exp( -925._r8 * itemp(:) )
      rate(:,603) = 1.9e-13_r8 * exp( 520._r8 * itemp(:) )
      rate(:,604) = 1.1e-11_r8 * exp( -280._r8 * itemp(:) )
      rate(:,605) = 2.1e-11_r8 * exp( -2200._r8 * itemp(:) )
      rate(:,606) = 7.2e-14_r8 * exp( -1070._r8 * itemp(:) )
      rate(:,614) = 1.6e-13_r8 * exp( -2280._r8 * itemp(:) )
      rate(:,617) = 2.6e-11_r8 * exp( 330._r8 * itemp(:) )
      rate(:,637) = 1.7e-12_r8 * exp( -710._r8 * itemp(:) )

      itemp(:) = 300._r8 * itemp(:)
 
      n = ncol*pver

      ko(:) = 5.3e-32_r8 * itemp(:)**1.8_r8
      kinf(:) = 9.5e-11_r8 * itemp(:)**(-0.4_r8)
      call jpl( rate(:,205), m, 0.6_r8, ko, kinf, n )

      ko(:) = 6.9e-31_r8 * itemp(:)**1._r8
      kinf(:) = 2.6e-11_r8
      call jpl( rate(:,215), m, 0.6_r8, ko, kinf, n )

      ko(:) = 2.5e-31_r8 * itemp(:)**1.8_r8
      kinf(:) = 2.2e-11_r8 * itemp(:)**0.7_r8
      call jpl( rate(:,227), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9e-32_r8 * itemp(:)**1.5_r8
      kinf(:) = 3e-11_r8
      call jpl( rate(:,235), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.9e-31_r8 * itemp(:)**3.4_r8
      kinf(:) = 4e-12_r8 * itemp(:)**0.3_r8
      call jpl( rate(:,238), m, 0.6_r8, ko, kinf, n )

      ko(:) = 2.4e-30_r8 * itemp(:)**3._r8
      kinf(:) = 1.6e-12_r8 * itemp(:)**(-0.1_r8)
      call jpl( rate(:,239), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.8e-30_r8 * itemp(:)**3._r8
      kinf(:) = 2.8e-11_r8
      call jpl( rate(:,240), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.8e-31_r8 * itemp(:)**2._r8
      kinf(:) = 1e-10_r8 * itemp(:)
      call jpl( rate(:,253), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.8e-31_r8 * itemp(:)**3.4_r8
      kinf(:) = 1.5e-11_r8 * itemp(:)**1.9_r8
      call jpl( rate(:,263), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.9e-32_r8 * itemp(:)**3.6_r8
      kinf(:) = 3.7e-12_r8 * itemp(:)**1.6_r8
      call jpl( rate(:,283), m, 0.6_r8, ko, kinf, n )

      ko(:) = 4.2e-31_r8 * itemp(:)**2.4_r8
      kinf(:) = 2.7e-11_r8
      call jpl( rate(:,288), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.2e-31_r8 * itemp(:)**3.2_r8
      kinf(:) = 6.9e-12_r8 * itemp(:)**2.9_r8
      call jpl( rate(:,298), m, 0.6_r8, ko, kinf, n )

      ko(:) = 3e-31_r8 * itemp(:)**1._r8
      kinf(:) = 6.6e-11_r8
      call jpl( rate(:,319), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.8e-32_r8 * itemp(:)**1._r8
      kinf(:) = 1.7e-11_r8
      call jpl( rate(:,323), m, 0.6_r8, ko, kinf, n )

      ko(:) = 6.5e-31_r8 * itemp(:)**3.5_r8
      kinf(:) = 7.6e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,333), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.4e-28_r8 * itemp(:)**8.5_r8
      kinf(:) = 4e-11_r8 * itemp(:)**1.2_r8
      call jpl( rate(:,355), m, 0.6_r8, ko, kinf, n )

      ko(:) = 6.1e-33_r8 * itemp(:)**1.5_r8
      kinf(:) = 9.8e-15_r8 * itemp(:)**(-4.6_r8)
      call jpl( rate(:,402), m, 0.8_r8, ko, kinf, n )

      ko(:) = 5.2e-30_r8 * itemp(:)**2.4_r8
      kinf(:) = 2.2e-10_r8 * itemp(:)**0.7_r8
      call jpl( rate(:,412), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.5e-30_r8
      kinf(:) = 8.3e-13_r8 * itemp(:)**(-2._r8)
      call jpl( rate(:,413), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.6e-29_r8 * itemp(:)**3.3_r8
      kinf(:) = 3.1e-10_r8 * itemp(:)
      call jpl( rate(:,414), m, 0.6_r8, ko, kinf, n )

      ko(:) = 8.6e-29_r8 * itemp(:)**3.1_r8
      kinf(:) = 9e-12_r8 * itemp(:)**0.85_r8
      call jpl( rate(:,440), m, 0.48_r8, ko, kinf, n )

      ko(:) = 7.3e-29_r8 * itemp(:)**4.1_r8
      kinf(:) = 9.5e-12_r8 * itemp(:)**1.6_r8
      call jpl( rate(:,441), m, 0.6_r8, ko, kinf, n )

      ko(:) = 8e-27_r8 * itemp(:)**3.5_r8
      kinf(:) = 3e-11_r8
      call jpl( rate(:,462), m, 0.5_r8, ko, kinf, n )

      ko(:) = 8e-27_r8 * itemp(:)**3.5_r8
      kinf(:) = 3e-11_r8
      call jpl( rate(:,488), m, 0.5_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,491), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,549), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,552), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,555), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,562), m, 0.6_r8, ko, kinf, n )

      ko(:) = 2.9e-31_r8 * itemp(:)**4.1_r8
      kinf(:) = 1.7e-12_r8 * itemp(:)**(-0.2_r8)
      call jpl( rate(:,608), m, 0.6_r8, ko, kinf, n )

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

      rate(:n,189) = 1e-20_r8
      rate(:n,190) = 1.3e-16_r8
      rate(:n,194) = 8e-14_r8
      rate(:n,195) = 3.9e-17_r8
      rate(:n,202) = 6.9e-12_r8
      rate(:n,219) = 7e-11_r8
      rate(:n,220) = 7e-13_r8
      rate(:n,743) = 6e-11_r8
      rate(:n,746) = 1e-12_r8
      rate(:n,747) = 4e-10_r8
      rate(:n,748) = 2e-10_r8
      rate(:n,749) = 1e-10_r8
      rate(:n,751) = 4.4e-10_r8
 
      do k = 1,kbot
        offset = (k-1)*ncol
        itemp(offset+1:offset+ncol) = 1._r8 / temp(:ncol,k)
      end do

      rate(:n,184) = 2.15e-11_r8 * exp( 110._r8 * itemp(:) )
      exp_fac(:) = exp( 55._r8 * itemp(:) )
      rate(:n,185) = 2.64e-11_r8 * exp_fac(:)
      rate(:n,186) = 6.6e-12_r8 * exp_fac(:)
      rate(:n,191) = 3.6e-18_r8 * exp( -220._r8 * itemp(:) )
      rate(:n,193) = 1.8e-15_r8 * exp( 45._r8 * itemp(:) )
      rate(:n,196) = 3.5e-11_r8 * exp( -135._r8 * itemp(:) )
      rate(:n,197) = 8e-12_r8 * exp( -2060._r8 * itemp(:) )
      rate(:n,206) = 3e-11_r8 * exp( 200._r8 * itemp(:) )
      rate(:n,207) = 1e-14_r8 * exp( -490._r8 * itemp(:) )
      rate(:n,208) = 1.4e-10_r8 * exp( -470._r8 * itemp(:) )
      rate(:n,211) = 4.8e-11_r8 * exp( 250._r8 * itemp(:) )
      rate(:n,212) = 1.8e-11_r8 * exp( 180._r8 * itemp(:) )
      rate(:n,213) = 1.7e-12_r8 * exp( -940._r8 * itemp(:) )
      rate(:n,221) = 2.1e-11_r8 * exp( 100._r8 * itemp(:) )
      rate(:n,225) = 5.1e-12_r8 * exp( 210._r8 * itemp(:) )
      rate(:n,233) = 3.44e-12_r8 * exp( 260._r8 * itemp(:) )
      rate(:n,234) = 3e-12_r8 * exp( -1500._r8 * itemp(:) )

      itemp(:) = 300._r8 * itemp(:)

      ko(:) = 5.3e-32_r8 * itemp(:)**1.8_r8
      kinf(:) = 9.5e-11_r8 * itemp(:)**(-0.4_r8)
      call jpl( wrk, m, 0.6_r8, ko, kinf, n )
      rate(:n,205) = wrk(:)






























      end subroutine setrxt_hrates

      end module mo_setrxt
