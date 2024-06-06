!rpf_CESM2_SLH
      module mo_sadtrop

      use shr_kind_mod,  only : r8 => shr_kind_r8
      use physconst,     only : pi
      use ppgrid,        only : pcols, pver
      use m_sad_data,    only : a, b
      use cam_logfile,   only : iulog
      

      implicit none

      private
      public  :: sadtrop_inti
      public  :: icesad_trop_calc
      public  :: trop_sad_top

      save

      real(r8), parameter :: four_thrd = 4._r8/3._r8
      real(r8), parameter :: one_thrd = 1._r8/3._r8
      real(r8), parameter :: two_thrd = 2._r8/3._r8
      real(r8), parameter :: four_pi  = 4._r8*pi

      integer :: trop_sad_top
      integer :: trop_sad_topp

    contains

      subroutine sadtrop_inti(pbuf2d)
!----------------------------------------------------------------------
!     ... initialize the sad module
!----------------------------------------------------------------------

      use time_manager, only : is_first_step
      use ref_pres,     only : pref_mid_norm
      use physics_buffer, only : physics_buffer_desc, pbuf_set_field

      implicit none

      type(physics_buffer_desc), pointer :: pbuf2d(:,:)

!---------------------------------------------------------------------- 
!	... Local variables
!---------------------------------------------------------------------- 
      integer  ::  k

!---------------------------------------------------------------------- 
!	... find level where etamids are all > 1 hPa
!---------------------------------------------------------------------- 
      trop_sad_top = 0
      do k = pver,1,-1
	 if( (pref_mid_norm(k)) < .001_r8 ) then
             trop_sad_top = k
             exit
         end if
      end do
      trop_sad_topp = trop_sad_top + 1
      write(iulog,*) 'sad_inti: sad capped at level ',trop_sad_top
      write(iulog,*) '          whose midpoint is ',pref_mid_norm(trop_sad_topp)*1.e3_r8,' hPa'

      end subroutine sadtrop_inti
!=============================================================================
!
!=============================================================================
      subroutine icesad_trop_calc( lchnk, m, press, temper, h2o_cond, sad_sage, &
                                   radius_trop, sad_trop, ncol, troplev, pbuf )

      use cam_history, only : outfld
      use physics_buffer, only : physics_buffer_desc

      implicit none

!-------------------------------------------------------------------------------
!	... dummy arguments
!-------------------------------------------------------------------------------
      integer,  intent(in)    ::  lchnk                      ! chnk id
      integer,  intent(in)    ::  ncol                       ! columns in chunk
      real(r8), intent(in)    ::  m           (ncol,pver)    ! Air density (molecules cm-3)
      real(r8), intent(in)    ::  sad_sage    (ncol,pver)    ! SAGEII surface area density (cm2 aer. cm-3 air)
      real(r8), intent(in)    ::  press       (ncol,pver)    ! Pressure, hPa
      real(r8), intent(in)    ::  temper      (pcols,pver)   ! Temperature (K)
      real(r8), intent(inout) ::  h2o_cond    (ncol,pver)    ! H2O condensed phase  (mole fraction)
      real(r8), intent(out)   ::  radius_trop (ncol,pver)    ! Radius of Sulfate, NAT, and ICE (cm)
      real(r8), intent(out)   ::  sad_trop    (ncol,pver)    ! Surface area density of Sulfate, NAT, ICE (cm2 cm-3)
      integer,  intent(in)    ::  troplev     (pcols)

!-------------------------------------------------------------------------------
!	... local variables
!-------------------------------------------------------------------------------
      real(r8), parameter :: temp_floor = 0._r8

      type(physics_buffer_desc), pointer :: pbuf(:)

!rpf_liqsad ==> Should keep in mind that the variable names *_ice are used for both liquid or ice fields
!rpf_liqsad --- Wether is ice or liq it depends on the array h2o_cond(ncol,pver) passed by the calling routine
!----------------------------------------------------------------------
!	... local variables
!----------------------------------------------------------------------
      logical  ::  mask_lbs        (ncol,pver)   ! LBS mask T: Free_Trop??
      logical  ::  mask_ice        (ncol,pver)   ! ICE mask T: Free_Trop??
      real(r8) ::  lcl_h2o_avail   (ncol,pver)   ! H2O temporary arrays
      real(r8) ::  lcl_sad_ice     (ncol,pver)   ! SAD of ICE aerosol        (cm2 cm-3)
      real(r8) ::  lcl_radius_ice  (ncol,pver)   ! Radius of ICE aerosol     (cm)
      integer  ::  i, k
      real(r8) ::  temp            (pcols,pver)     ! wrk temperature array
      integer  ::  kk_tropo

!----------------------------------------------------------------------
!     ... limit temperature
!----------------------------------------------------------------------
      do k = 1,pver
         temp(:ncol,k)    = max( temp_floor,temper(:ncol,k) )
      end do

!rpf --- from upper
         do k = 1,pver
            radius_trop     (:,k) = 0._r8
            sad_trop        (:,k) = 0._r8
            lcl_h2o_avail   (:,k) = 0._r8
            lcl_sad_ice     (:,k) = 0._r8
            lcl_radius_ice  (:,k) = 0._r8
            mask_ice        (:,k) = .false.
            mask_lbs        (:,k) = .false.
         end do
!rpf --- from upper
!======================================================================
!======================================================================
!     ... Logic for deriving ICE
!         Ice formation occurs here if condensed phase H2O exists.
!
!         mask_lbs  = false.... T > 200K or SAD_SULF < 1e-15 or 
!                               P >2hPa or P <300hPa
!         mask_ice  = true .... H2O_COND > 0.0
!======================================================================
!======================================================================
      do k = trop_sad_topp,pver
	 do i = 1,ncol
	    if( .not. mask_lbs(i,k) ) then
               mask_ice(i,k) = h2o_cond(i,k) > 0._r8
	    else
	       mask_ice(i,k) = .false.
	    end if
         end do
      end do

!rpf  Allow computation only in troposphere
      do i = 1,ncol
         kk_tropo = troplev(i)
         do k = pver, 1, -1
           if ( k < kk_tropo ) then
             mask_ice(i,k) = .false.
           endif
         enddo
      enddo

all_ice : &
      if( any( mask_ice(:,trop_sad_topp:pver) ) ) then
         do k = trop_sad_topp,pver
            where( mask_ice(:,k) )
	       lcl_h2o_avail(:,k) = h2o_cond(:,k)
            endwhere
         end do
!----------------------------------------------------------------------
!        ... ICE 
!----------------------------------------------------------------------
         call ice_sad_calc( ncol, press, temp, m, lcl_h2o_avail, &
			    lcl_sad_ice, lcl_radius_ice, mask_ice )

         do k = trop_sad_topp,pver
            where( mask_ice(:,k) )
               sad_trop   (:,k) = lcl_sad_ice       (:,k)
               radius_trop(:,k) = lcl_radius_ice    (:,k)
            endwhere
         end do
      end if all_ice


      end subroutine icesad_trop_calc

      subroutine ice_sad_calc( ncol, press, temp, m, h2o_avail, &
			       sad_ice, radius_ice, mask )

      implicit none

!----------------------------------------------------------------------
!	... dummy arguments
!----------------------------------------------------------------------
      integer, intent(in)   :: ncol
      real(r8), intent(in)  :: press     (ncol,pver)
      real(r8), intent(in)  :: temp      (pcols,pver)
      real(r8), intent(in)  :: m         (ncol,pver)
      real(r8), intent(in)  :: h2o_avail (ncol,pver)
      real(r8), intent(out) :: sad_ice   (ncol,pver)
      real(r8), intent(out) :: radius_ice(ncol,pver)
      logical, intent(in)   :: mask      (ncol,pver)

!----------------------------------------------------------------------
!	... local variables
!----------------------------------------------------------------------
      real(r8), parameter :: &
                 avo_num       = 6.02214e23_r8, &
                 aconst        = -2663.5_r8, &
                 bconst        = 12.537_r8, &
                 ice_mass_dens = 1._r8, &
                 ice_part_dens = 1.e-1_r8, &
                 mwh2o         = 18._r8, &
                 sigma_ice     = 1.6_r8, &
                 ice_dens_aer  = ice_mass_dens / (mwh2o/avo_num), &
                 ice_dens_aeri = 1._r8/ice_dens_aer

      integer  :: k
      real(r8) :: h2o_cond_ice(ncol)      ! Condensed phase H2O (from CAM)
      real(r8) :: voldens_ice (ncol)      ! Volume Density, um3 cm-3

      do k = trop_sad_topp,pver
	 where( mask(:,k) )
!----------------------------------------------------------------------
!     .... Convert condensed phase to molecules cm-3 units
!----------------------------------------------------------------------
	   h2o_cond_ice(:) = h2o_avail(:,k) * m(:,k)
!----------------------------------------------------------------------
!     .... ICE volume density .....
!----------------------------------------------------------------------
           voldens_ice(:) = h2o_cond_ice(:)*ice_dens_aeri
!----------------------------------------------------------------------
!     .... Calculate the SAD from log normal distribution .....
!----------------------------------------------------------------------
           sad_ice(:,k) = (four_pi*ice_part_dens)**one_thrd &
                         *(3._r8*voldens_ice(:))**two_thrd &
                         *exp( -(log( sigma_ice ))**2 )
!----------------------------------------------------------------------
!    .... Calculate the radius from log normal distribution .....
!----------------------------------------------------------------------
           radius_ice(:,k) = (3._r8*h2o_cond_ice(:) &
                              /(ice_dens_aer*four_pi*ice_part_dens))**one_thrd &
                             *exp( -1.5_r8*(log( sigma_ice ))**2 )
         endwhere
      end do

      end subroutine ice_sad_calc

     end module mo_sadtrop
!rpf_CESM2_SLH
