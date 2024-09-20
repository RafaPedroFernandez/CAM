!-----------------------------------------------------------------------
! Reads namelist options for slh recycling efficiency using the Free-Regime Aprox. (FRA)
!
! Created by Rafa Fernandez -- 30 June 2023
!-----------------------------------------------------------------------
module mo_slh_routines

  use spmd_utils,       only : masterproc
  use cam_abortutils,   only : endrun
  use shr_kind_mod,     only : r8 => shr_kind_r8

  implicit none

!rpf_CESM2_SLH
  real(r8), protected         :: SSAdehal_ScalingFactor  = 1._r8
  real(r8), protected         :: SLHemiss_ScalingFactor  = 1._r8   ! SLHemiss_ScalingFactor = 1.0 always. (rpf)
  real(r8), protected         :: ICEfraprx_ScalingFactor = 1._r8   ! Not used anymore. Replaced by ICEfraprx_ScalingFactor_I (rpf)
  real(r8), protected         :: LIQfraprx_ScalingFactor = 1._r8   ! Not used anymore. Replaced by LIQfraprx_ScalingFactor_I (rpf)
  real(r8), protected         :: SSAhno3_ScalingFactor   = 1._r8
  real(r8), protected         :: SSAn2o5_ScalingFactor   = 1._r8
  real(r8), protected         :: LIQfraprx_ScalingFactor_I   = 1._r8
  real(r8), protected         :: ICEfraprx_ScalingFactor_I   = 1._r8
  real(r8), protected         :: ICEfraprx_ScalingFactor_Br  = 1._r8
!rpf_CESM2_SLH

contains

  !-----------------------------------------------------------------------
  !-----------------------------------------------------------------------

  subroutine slh_readnl(nlfile)

    use cam_abortutils,  only: endrun
    use namelist_utils,  only: find_group_name
    use units,           only: getunit, freeunit
#ifdef SPMD
!rpf_CESM2_SLH
    use mpishorthand,    only: mpichar, mpicom, mpir8
!rpf_CESM2_SLH
#endif

    implicit none

    character(len=*), intent(in) :: nlfile  ! filepath for file containing namelist input

    integer :: unitn, i, ierr

!rpf_CESM2_SLH
    namelist /slh_nl/ SSAdehal_ScalingFactor,                                                   &
                      SLHemiss_ScalingFactor, ICEfraprx_ScalingFactor, LIQfraprx_ScalingFactor, &
                      SSAhno3_ScalingFactor, SSAn2o5_ScalingFactor,                             &
                      LIQfraprx_ScalingFactor_I, ICEfraprx_ScalingFactor_I, ICEfraprx_ScalingFactor_Br
!rpf_CESM2_SLH

    if (masterproc) then
       unitn = getunit()
       open( unitn, file=trim(nlfile), status='old' )
       call find_group_name(unitn, 'slh_nl', status=ierr)
       if (ierr == 0) then
          read(unitn, slh_nl, iostat=ierr)
          if (ierr /= 0) then
             call endrun('mo_slh_routines->slh_readnl: ERROR reading slh_nl namelist')
          end if
       end if
       close(unitn)
       call freeunit(unitn)
    end if

!rpf_CESM2_SLH
    call mpibcast (SSAdehal_ScalingFactor,      1, mpir8,    0, mpicom)
!     call mpibcast (SLHemiss_ScalingFactor,      1, mpir8,    0, mpicom)
!     call mpibcast (ICEfraprx_ScalingFactor,     1, mpir8,    0, mpicom)
!     call mpibcast (LIQfraprx_ScalingFactor,     1, mpir8,    0, mpicom)
    call mpibcast (SSAhno3_ScalingFactor,       1, mpir8,    0, mpicom)
    call mpibcast (SSAn2o5_ScalingFactor,       1, mpir8,    0, mpicom)
    call mpibcast (LIQfraprx_ScalingFactor_I,   1, mpir8,    0, mpicom)
    call mpibcast (ICEfraprx_ScalingFactor_I,   1, mpir8,    0, mpicom)
    call mpibcast (ICEfraprx_ScalingFactor_Br,  1, mpir8,    0, mpicom)
!rpf_CESM2_SLH

  end subroutine slh_readnl

end module mo_slh_routines
