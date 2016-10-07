!IDEAL:MODEL_LAYER:INITIALIZATION
!

!  This MODULE holds the routines which are used to perform various initializations
!  for the individual domains.

!  This MODULE CONTAINS the following routines:

!  initialize_field_test - 1. Set different fields to different constant
!                             values.  This is only a test.  If the correct
!                             domain is not found (based upon the "id")
!                             then a fatal error is issued.

!-------------------------------------------------------------------------

MODULE module_initialize_ideal

	USE module_domain
	USE module_io_domain
	USE module_state_description
	USE module_model_constants
	USE module_bc
	USE module_timing
	USE module_configure
	USE module_init_utilities
#ifdef DM_PARALLEL
	USE module_dm
#endif

CONTAINS

!-------------------------------------------------------------------------
! this is a wrapper for the solver-specific init_domain routines.
! Also dereferences the grid variables and passes them down as arguments.
! This is crucial, since the lower level routines may do message passing
! and this will get fouled up on machines that insist on passing down
! copies of assumed-shape arrays (by passing down as arguments, the 
! data are treated as assumed-size -- ie. f77 -- arrays and the copying
! business is avoided).  Fie on the F90 designers.  Fie and a pox.

	SUBROUTINE init_domain ( grid )
		IMPLICIT NONE
		!  Input data.
		TYPE (domain), POINTER :: grid 
		!  Local data.
		INTEGER :: idum1, idum2
		CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )
		
		CALL init_domain_rk( grid &
!
#include <actual_new_args.inc>
!
                        )
	
	END SUBROUTINE init_domain
	
!-------------------------------------------------------------------------

#ifdef WRFLES
!=========================================================================
! Tak Yamaguchi 2011/05 Clean up code for WRFLES
	SUBROUTINE init_domain_rk ( grid &
!
# include <dummy_new_args.inc>
!
)
		
		! WRFLES
		USE module_wrfles, ONLY:wrfles_setup_in_idealexe, wrfles_setperturb, surface_type, &
				coriolis, latitude, longitude
		! END WRFLES
		
		IMPLICIT NONE
		
		! Input data.
		TYPE (domain), POINTER :: grid
		
# include <dummy_new_decl.inc>
		
		TYPE (grid_config_rec_type) :: config_flags
		
		! Local data
		INTEGER :: ids, ide, jds, jde, kds, kde, &
		           ims, ime, jms, jme, kms, kme, &
		           its, ite, jts, jte, kts, kte, &
		           i, j, k
		INTEGER, PARAMETER :: nl_max = 1000
		REAL, DIMENSION(nl_max) :: zk, p_in, theta, rho, u, v, qv, pd_in
		INTEGER :: nl_in
		
		INTEGER :: icm, jcm, error
		REAL :: p_surf, p_level, qvf, z_at_v, z_at_u
		REAL :: z_scale, cof1, cof2
		REAL :: qvf1, qvf2, pd_surf
		REAL :: thtmp, ptmp, temp(3)
		
		LOGICAL :: stretch_grid, dry_sounding
		
		LOGICAL, EXTERNAL :: wrf_dm_on_monitor
	
#ifdef DM_PARALLEL
#    include <data_calls.inc>
#endif

		SELECT CASE ( model_data_order )
		CASE ( DATA_ORDER_ZXY )
			kds = grid%sd31 ; kde = grid%ed31 ;
			ids = grid%sd32 ; ide = grid%ed32 ;
			jds = grid%sd33 ; jde = grid%ed33 ;
			kms = grid%sm31 ; kme = grid%em31 ;
			ims = grid%sm32 ; ime = grid%em32 ;
			jms = grid%sm33 ; jme = grid%em33 ;
			kts = grid%sp31 ; kte = grid%ep31 ;   ! note that tile is entire patch
			its = grid%sp32 ; ite = grid%ep32 ;   ! note that tile is entire patch
			jts = grid%sp33 ; jte = grid%ep33 ;   ! note that tile is entire patch
		CASE ( DATA_ORDER_XYZ )
			ids = grid%sd31 ; ide = grid%ed31 ;
			jds = grid%sd32 ; jde = grid%ed32 ;
			kds = grid%sd33 ; kde = grid%ed33 ;
			ims = grid%sm31 ; ime = grid%em31 ;
			jms = grid%sm32 ; jme = grid%em32 ;
			kms = grid%sm33 ; kme = grid%em33 ;
			its = grid%sp31 ; ite = grid%ep31 ;   ! note that tile is entire patch
			jts = grid%sp32 ; jte = grid%ep32 ;   ! note that tile is entire patch
			kts = grid%sp33 ; kte = grid%ep33 ;   ! note that tile is entire patch
		CASE ( DATA_ORDER_XZY )
			ids = grid%sd31 ; ide = grid%ed31 ;
			kds = grid%sd32 ; kde = grid%ed32 ;
			jds = grid%sd33 ; jde = grid%ed33 ;
			ims = grid%sm31 ; ime = grid%em31 ;
			kms = grid%sm32 ; kme = grid%em32 ;
			jms = grid%sm33 ; jme = grid%em33 ;
			its = grid%sp31 ; ite = grid%ep31 ;   ! note that tile is entire patch
			kts = grid%sp32 ; kte = grid%ep32 ;   ! note that tile is entire patch
			jts = grid%sp33 ; jte = grid%ep33 ;   ! note that tile is entire patch
		END SELECT
		
		! WRFLES
		CALL wrfles_setup_in_idealexe
		! END WRFLES
		
		CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
		
		! here we check to see if the boundary conditions are set properly
		CALL boundary_condition_check( config_flags, bdyzone, error, grid%id )
		
		grid%itimestep = 0
		
#ifdef DM_PARALLEL
		CALL wrf_dm_bcast_bytes( icm , IWORDSIZE )
		CALL wrf_dm_bcast_bytes( jcm , IWORDSIZE )
#endif
		
		CALL nl_set_mminlu(1, '    ')
		
		! WRFLES
		CALL nl_set_iswater(1,0)
		CALL nl_set_cen_lat(1,latitude)
		CALL nl_set_cen_lon(1,longitude)
		! END WRFLES
		
		CALL nl_set_truelat1(1,0.)
		CALL nl_set_truelat2(1,0.)
		CALL nl_set_moad_cen_lat(1,0.)
		CALL nl_set_stand_lon(1,0.)
		CALL nl_set_pole_lon(1,0.)
		CALL nl_set_pole_lat(1,90.)
		CALL nl_set_map_proj(1,0)
		
		! here we initialize data we currently is not initialized in the input data
		DO j = jts, jte
			DO i = its, ite
				grid%msftx(i,j)    = 1.
				grid%msfty(i,j)    = 1.
				grid%msfux(i,j)    = 1.
				grid%msfuy(i,j)    = 1.
				grid%msfvx(i,j)    = 1.
				grid%msfvx_inv(i,j)= 1.
				grid%msfvy(i,j)    = 1.
				grid%sina(i,j)     = 0.
				grid%cosa(i,j)     = 1.
				grid%e(i,j)        = 0.
				
				! WRFLES
				grid%f(i,j)     = coriolis
				grid%xlat(i,j)  = latitude
				grid%xlong(i,j) = longitude
				grid%xland(i,j) = surface_type
				! END WRFLES*
			ENDDO
		ENDDO
		DO j = jts, jte
			DO k = kts, kte
				DO i = its, ite
					grid%ww(i,k,j) = 0.
				ENDDO
			ENDDO
		ENDDO
		grid%step_number = 0
		
		
		
		!-----------------------------------------------------------------------------
		! set up the grid <===== Make a subroutine in module_wrfles.F for more frexibility for future
		! stretch_grid = .TRUE.
		stretch_grid = .FALSE.
		IF (stretch_grid) THEN ! exponential stretch for eta (nearly constant dz)
			z_scale = .40
			DO k = 1, kde
				grid%znw(k) = ( EXP( - (k-1) / FLOAT(kde-1) / z_scale ) - EXP( - 1. / z_scale ) ) &
				            / ( 1. - EXP( - 1. / z_scale ) )
			ENDDO
		ELSE
			DO k = 1, kde
				grid%znw(k) = 1. - FLOAT(k-1) / FLOAT(kde-1)
			ENDDO
		ENDIF
		
		DO k = 1, kde-1
			grid%dnw(k)  = grid%znw(k+1) - grid%znw(k)
			grid%rdnw(k) = 1. / grid%dnw(k)
			grid%znu(k)  = 0.5 * ( grid%znw(k+1) + grid%znw(k) )
		ENDDO
		DO k = 2, kde-1
			grid%dn(k)  = 0.5 * ( grid%dnw(k) + grid%dnw(k-1) )
			grid%rdn(k) = 1. / grid%dn(k)
			grid%fnp(k) = 0.5 * grid%dnw(k  ) / grid%dn(k)
			grid%fnm(k) = 0.5 * grid%dnw(k-1) / grid%dn(k)
		ENDDO
		
		cof1 = (2.*grid%dn(2)+grid%dn(3))/(grid%dn(2)+grid%dn(3))*grid%dnw(1)/grid%dn(2)
		cof2 =     grid%dn(2)        /(grid%dn(2)+grid%dn(3))*grid%dnw(1)/grid%dn(3)
		grid%cf1 = grid%fnp(2) + cof1
		grid%cf2 = grid%fnm(2) - cof1 - cof2
		grid%cf3 = cof2
		
		grid%cfn  = ( 0.5 * grid%dnw(kde-1) + grid%dn(kde-1) ) / grid%dn(kde-1)
		grid%cfn1 = - 0.5 * grid%dnw(kde-1) / grid%dn(kde-1)
		grid%rdx = 1. / config_flags%dx
		grid%rdy = 1. / config_flags%dy
		
		
		
		!-----------------------------------------------------------------------------
		! get the sounding from the ascii sounding file, first get dry sounding and
		! calculate base state
		dry_sounding = .TRUE.
		IF ( wrf_dm_on_monitor() ) THEN
			WRITE(6,*) ' getting dry sounding for base state '
			CALL get_sounding( zk, p_in, pd_in, theta, rho, u, v, qv, dry_sounding, nl_max, nl_in )
		ENDIF
		CALL wrf_dm_bcast_real( zk , nl_max )
		CALL wrf_dm_bcast_real( p_in , nl_max )
		CALL wrf_dm_bcast_real( pd_in , nl_max )
		CALL wrf_dm_bcast_real( theta , nl_max )
		CALL wrf_dm_bcast_real( rho , nl_max )
		CALL wrf_dm_bcast_real( u , nl_max )
		CALL wrf_dm_bcast_real( v , nl_max )
		CALL wrf_dm_bcast_real( qv , nl_max )
		CALL wrf_dm_bcast_integer( nl_in , 1 )
		WRITE(6,*) ' returned from reading sounding, nl_in is ',nl_in
		
		! find ptop for the desired ztop (ztop is input from the namelist),
		! and find surface pressure
		grid%p_top = interp_0( p_in, zk, config_flags%ztop, nl_in )
		
		DO j = jts, jte
			DO i = its, ite
				grid%ht(i,j) = 0.
			ENDDO
		ENDDO
		
		DO j = jts, jte
			DO i = its, ite
				grid%phb(i,1,j) = g * grid%ht(i,j)
				grid%ph0(i,1,j) = g * grid%ht(i,j)
			ENDDO
		ENDDO
		
		DO j = jts, jte
			DO i = its, ite
				p_surf = interp_0( p_in, zk, grid%phb(i,1,j)/g, nl_in )
				grid%mub(i,j) = p_surf - grid%p_top
				
				! this is dry hydrostatic sounding (base state), so given grid%p (coordinate),
				! interp theta (from interp) and compute 1/rho from eqn. of state
				DO k = 1, kte-1
					p_level = grid%znu(k)*( p_surf - grid%p_top) + grid%p_top
					grid%pb(i,k,j) = p_level
					grid%t_init(i,k,j) = interp_0( theta, p_in, p_level, nl_in ) - t0
					grid%alb(i,k,j) = (r_d/p1000mb) * (grid%t_init(i,k,j)+t0) &
					                * (grid%pb(i,k,j)/p1000mb) ** cvpm
				ENDDO
				
				! calc hydrostatic balance (alternatively we could interp the geopotential from the
				! sounding, but this assures that the base state is in exact hydrostatic balance with
				! respect to the model eqns.
				DO k = 2, kte
					grid%phb(i,k,j) = grid%phb(i,k-1,j)-grid%dnw(k-1)*grid%mub(i,j)*grid%alb(i,k-1,j)
				ENDDO
			ENDDO
		ENDDO
		
		IF ( wrf_dm_on_monitor() ) THEN
			WRITE(6,*) ' ptop is ', grid%p_top
			WRITE(6,*) ' base state grid%mub(1,1), p_surf is ', grid%mub(1,1), &
				grid%mub(1,1)+grid%p_top
		ENDIF
		
		
		! calculate full state for each column - this includes moisture.
		WRITE(6,*) ' getting moist sounding for full state '
		dry_sounding = .FALSE.
		CALL get_sounding( zk, p_in, pd_in, theta, rho, u, v, qv, dry_sounding, nl_max, nl_in )
		
		DO j = jts, MIN(jde-1,jte)
			DO i = its, MIN(ide-1,ite)
			
				! At this point grid%p_top is already set. find the DRY mass in the column
				! by interpolating the DRY pressure.
				pd_surf = interp_0( pd_in, zk, grid%phb(i,1,j)/g, nl_in )
				
				! compute the perturbation mass and the full mass
				grid%mu_1(i,j) = pd_surf-grid%p_top - grid%mub(i,j)
				grid%mu_2(i,j) = grid%mu_1(i,j)
				grid%mu0(i,j) = grid%mu_1(i,j) + grid%mub(i,j)
				
				! given the dry pressure and coordinate system, interp the potential
				! temperature and qv
				DO k = 1, kde-1
					p_level = grid%znu(k)*(pd_surf - grid%p_top) + grid%p_top
					moist(i,k,j,P_QV) = interp_0( qv, pd_in, p_level, nl_in )
					grid%t_1(i,k,j) = interp_0( theta, pd_in, p_level, nl_in ) - t0
					grid%t_2(i,k,j) = grid%t_1(i,k,j)
				ENDDO
				
				! integrate the hydrostatic equation (from the RHS of the bigstep
				! vertical momentum equation) down from the top to get grid%p.
				! first from the top of the model to the top pressure
				k = kte-1   ! top level
				qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k,j,P_QV))
				qvf2 = 1./(1.+qvf1)
				qvf1 = qvf1*qvf2
				grid%p(i,k,j) = - 0.5*(grid%mu_1(i,j)+qvf1*grid%mub(i,j))/grid%rdnw(k)/qvf2
				qvf = 1. + rvovrd*moist(i,k,j,P_QV)
				grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_1(i,k,j)+t0)*qvf &
				                * (((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
				grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
				
				! down the column
				DO k = kte-2, 1, -1
					qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k+1,j,P_QV))
					qvf2 = 1./(1.+qvf1)
					qvf1 = qvf1*qvf2
					grid%p(i,k,j) = grid%p(i,k+1,j) &
					              - (grid%mu_1(i,j)+qvf1*grid%mub(i,j))/qvf2/grid%rdn(k+1)
					qvf = 1. + rvovrd*moist(i,k,j,P_QV)
					grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_1(i,k,j)+t0)*qvf &
					                * (((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
					grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
				ENDDO
				
				! this is the hydrostatic equation used in the model after the
				! small timesteps.  In the model, grid%al (inverse density)
				! is computed from the geopotential.
				
				grid%ph_1(i,1,j) = 0.
				DO k = 2, kte
					grid%ph_1(i,k,j) = grid%ph_1(i,k-1,j) - (1./grid%rdnw(k-1)) &
					                 * ( (grid%mub(i,j)+grid%mu_1(i,j))*grid%al(i,k-1,j) &
					                    + grid%mu_1(i,j)*grid%alb(i,k-1,j) )
					grid%ph_2(i,k,j) = grid%ph_1(i,k,j)
					grid%ph0(i,k,j) = grid%ph_1(i,k,j) + grid%phb(i,k,j)
				ENDDO
				
				IF ( wrf_dm_on_monitor() ) THEN
					IF ((i==2) .AND. (j==2)) THEN
						WRITE(6,*) ' grid%ph_1 calc ',grid%ph_1(2,1,2), grid%ph_1(2,2,2), &
							grid%mu_1(2,2)+grid%mub(2,2), grid%mu_1(2,2), grid%alb(2,1,2), &
							grid%al(1,2,1), grid%rdnw(1)
					ENDIF
				ENDIF
			ENDDO
		ENDDO
		
		
		
		!-----------------------------------------------------------------------------
		! thermal perturbation to kick off convection
		WRITE(6,*) ' Setting initial perturbation.'
		WRITE(6,*) ' See wrfles_setperturb in ./frame/moule_wrfles.F'
		! End indices
		i = MIN(ide-1,ite)
		j = MIN(jde-1,jte)
		k = kte-1
		! Set up perturbations
		! Note tile is entire patch, i.e., subdomain
		CALL wrfles_setperturb( &
			grid%phb(its:i,1:k,jts:j), grid%ph_1(its:i,1:k,jts:j), &
			grid%t_1(its:i,1:k,jts:j), moist(its:i,1:k,jts:j,P_QV), grid%tke_1(its:i,1:k,jts:j), &
			its, i, jts, j, 1, k )
		! Match up t_2 and tke_2
		grid%t_2(:,:,:) = grid%t_1(:,:,:)
		grid%tke_2(:,:,:) = grid%tke_1(:,:,:)
		
		! Recompute density and geopotential
		DO j = jts, MIN(jde-1,jte)
			DO i = its, MIN(ide-1,ite)
				DO k = 1, kte-1
					qvf = 1. + rvovrd*moist(i,k,j,P_QV)
					grid%alt(i,k,j) = (r_d/p1000mb) * (grid%t_1(i,k,j)+t0) * qvf &
					                * ( ( (grid%p(i,k,j)+grid%pb(i,k,j)) / p1000mb ) ** cvpm )
					grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
				ENDDO
				DO k = 2, kte
					grid%ph_1(i,k,j) = grid%ph_1(i,k-1,j) - (1./grid%rdnw(k-1)) &
					                 * ( (grid%mub(i,j)+grid%mu_1(i,j)) * grid%al(i,k-1,j) &
					                 + grid%mu_1(i,j) * grid%alb(i,k-1,j) )
					grid%ph_2(i,k,j) = grid%ph_1(i,k,j)
					grid%ph0(i,k,j) = grid%ph_1(i,k,j) + grid%phb(i,k,j)
				ENDDO
			ENDDO
		ENDDO
		
		
		
		!-----------------------------------------------------------------------------
		IF ( wrf_dm_on_monitor() ) THEN
			WRITE(6,*) ' grid%mu_1 from comp ', grid%mu_1(1,1)
			WRITE(6,*) ' full state sounding from comp, ph, grid%p, grid%al, grid%t_1, qv '
			DO k = 1, kde-1
				WRITE(6,'(i3,1x,5(1x,1pe10.3))') k, grid%ph_1(1,k,1)+grid%phb(1,k,1), &
					grid%p(1,k,1)+grid%pb(1,k,1), grid%alt(1,k,1), &
					grid%t_1(1,k,1)+t0, moist(1,k,1,P_QV)
			ENDDO
			WRITE(6,*) ' pert state sounding from comp, grid%ph_1, pp, alp, grid%t_1, qv '
			DO k = 1, kde-1
				WRITE(6,'(i3,1x,5(1x,1pe10.3))') k, grid%ph_1(1,k,1), &
					grid%p(1,k,1), grid%al(1,k,1), grid%t_1(1,k,1), moist(1,k,1,P_QV)
			ENDDO
		ENDIF
		
		
		
		!-----------------------------------------------------------------------------
		! interp v
		DO j = jts, jte
			DO i = its, MIN(ide-1,ite)
				IF (j == jds) THEN
					z_at_v = grid%phb(i,1,j)/g
				ELSE IF (j == jde) THEN
					z_at_v = grid%phb(i,1,j-1)/g
				ELSE
					z_at_v = 0.5*(grid%phb(i,1,j)+grid%phb(i,1,j-1))/g
				ENDIF
				p_surf = interp_0( p_in, zk, z_at_v, nl_in )
				DO k = 1, kte-1
					p_level = grid%znu(k)*(p_surf - grid%p_top) + grid%p_top
					grid%v_1(i,k,j) = interp_0( v, p_in, p_level, nl_in )
					grid%v_2(i,k,j) = grid%v_1(i,k,j)
				ENDDO
			ENDDO
		ENDDO
		
		! interp u
		DO j = jts, MIN(jde-1,jte)
			DO i = its, ite
				IF (i == ids) THEN
					z_at_u = grid%phb(i,1,j)/g
				ELSE IF (i == ide) THEN
					z_at_u = grid%phb(i-1,1,j)/g
				ELSE
					z_at_u = 0.5*(grid%phb(i,1,j)+grid%phb(i-1,1,j))/g
				ENDIF
				p_surf = interp_0( p_in, zk, z_at_u, nl_in )
				DO k = 1, kte-1
					p_level = grid%znu(k)*(p_surf - grid%p_top) + grid%p_top
					grid%u_1(i,k,j) = interp_0( u, p_in, p_level, nl_in )
					grid%u_2(i,k,j) = grid%u_1(i,k,j)
				ENDDO
			ENDDO
		ENDDO
		
		! Set w
		DO j = jts, MIN(jde-1,jte)
			DO k = kts, kte
				DO I = its, MIN(ide-1,ite)
					grid%w_1(i,k,j) = 0.
					grid%w_2(i,k,j) = 0.
				ENDDO
			ENDDO
		ENDDO
		
		! Set a few more things
		DO j = jts, MIN(jde-1,jte)
			DO k = kts, kte-1
				DO i = its, MIN(ide-1,ite)
					grid%h_diabatic(i,k,j) = 0.
				ENDDO
			ENDDO
		ENDDO
		
		IF ( wrf_dm_on_monitor() ) THEN
			DO k=1,kte-1
				grid%t_base(k) = grid%t_1(1,k,1)
				grid%qv_base(k) = moist(1,k,1,P_QV)
				grid%u_base(k) = grid%u_1(1,k,1)
				grid%v_base(k) = grid%v_1(1,k,1)
				grid%z_base(k) = 0.5 * ( grid%phb(1,k,1) + grid%phb(1,k+1,1) &
				                       + grid%ph_1(1,k,1) + grid%ph_1(1,k+1,1) ) / g
			ENDDO
		ENDIF
		CALL wrf_dm_bcast_real( grid%t_base, kte )
		CALL wrf_dm_bcast_real( grid%qv_base, kte )
		CALL wrf_dm_bcast_real( grid%u_base, kte )
		CALL wrf_dm_bcast_real( grid%v_base, kte )
		CALL wrf_dm_bcast_real( grid%z_base, kte )
		
		DO j = jts, MIN(jde-1,jte)
			DO i = its, MIN(ide-1,ite)
				thtmp   = grid%t_2(i,1,j)+t0
				ptmp    = grid%p(i,1,j)+grid%pb(i,1,j)
				temp(1) = thtmp * (ptmp/p1000mb)**rcp
				thtmp   = grid%t_2(i,2,j)+t0
				ptmp    = grid%p(i,2,j)+grid%pb(i,2,j)
				temp(2) = thtmp * (ptmp/p1000mb)**rcp
				thtmp   = grid%t_2(i,3,j)+t0
				ptmp    = grid%p(i,3,j)+grid%pb(i,3,j)
				temp(3) = thtmp * (ptmp/p1000mb)**rcp
				grid%tsk(i,j) = grid%cf1*temp(1)+grid%cf2*temp(2)+grid%cf3*temp(3)
				grid%tmn(i,j) = grid%tsk(i,j) - 0.5
			ENDDO
		ENDDO
		
	END SUBROUTINE init_domain_rk

!=========================================================================
#else /*ORIGINAL init_domain_rk*/
! Tak Yamaguchi 2011/12 Clean up code
	SUBROUTINE init_domain_rk ( grid &
!
# include <dummy_new_args.inc>
!
)
		IMPLICIT NONE
		
		! Input data.
		TYPE (domain), POINTER :: grid
		
# include <dummy_new_decl.inc>
		TYPE (grid_config_rec_type) :: config_flags
		
		! Local data
		INTEGER :: ids, ide, jds, jde, kds, kde, &
		           ims, ime, jms, jme, kms, kme, &
		           its, ite, jts, jte, kts, kte, &
		           i, j, k
		INTEGER, PARAMETER :: nl_max = 1000
		REAL, DIMENSION(nl_max) :: zk, p_in, theta, rho, u, v, qv, pd_in
		INTEGER :: nl_in
		INTEGER :: icm, jcm, ii, im1, jj, jm1, loop, error, fid, nxc, nyc
		REAL :: u_mean, v_mean, f0, p_surf, p_level, qvf, z_at_v, z_at_u
		REAL :: z_scale, xrad, yrad, zrad, rad, delt, cof1, cof2
		REAL :: hm
		REAL :: pi
		
		! Stuff from original initialization that has been dropped from the Registry
		REAL :: vnu, xnu, xnus, dinit0, cbh, p0_temp, t0_temp, zd, zt
		REAL :: qvf1, qvf2, pd_surf
		INTEGER :: it
		REAL :: thtmp, ptmp, temp(3)
		
		LOGICAL :: moisture_init
		LOGICAL :: stretch_grid, dry_sounding
		
		INTEGER :: xs , xe , ys , ye
		REAL :: mtn_ht
		LOGICAL, EXTERNAL :: wrf_dm_on_monitor
		! For LES, add randx
		REAL :: randx
		
#ifdef DM_PARALLEL
#    include <data_calls.inc>
#endif
		
		SELECT CASE ( model_data_order )
		CASE ( DATA_ORDER_ZXY )
			kds = grid%sd31 ; kde = grid%ed31 ;
			ids = grid%sd32 ; ide = grid%ed32 ;
			jds = grid%sd33 ; jde = grid%ed33 ;
			kms = grid%sm31 ; kme = grid%em31 ;
			ims = grid%sm32 ; ime = grid%em32 ;
			jms = grid%sm33 ; jme = grid%em33 ;
			kts = grid%sp31 ; kte = grid%ep31 ;   ! note that tile is entire patch
			its = grid%sp32 ; ite = grid%ep32 ;   ! note that tile is entire patch
			jts = grid%sp33 ; jte = grid%ep33 ;   ! note that tile is entire patch
		CASE ( DATA_ORDER_XYZ )
			ids = grid%sd31 ; ide = grid%ed31 ;
			jds = grid%sd32 ; jde = grid%ed32 ;
			kds = grid%sd33 ; kde = grid%ed33 ;
			ims = grid%sm31 ; ime = grid%em31 ;
			jms = grid%sm32 ; jme = grid%em32 ;
			kms = grid%sm33 ; kme = grid%em33 ;
			its = grid%sp31 ; ite = grid%ep31 ;   ! note that tile is entire patch
			jts = grid%sp32 ; jte = grid%ep32 ;   ! note that tile is entire patch
			kts = grid%sp33 ; kte = grid%ep33 ;   ! note that tile is entire patch
		CASE ( DATA_ORDER_XZY )
			ids = grid%sd31 ; ide = grid%ed31 ;
			kds = grid%sd32 ; kde = grid%ed32 ;
			jds = grid%sd33 ; jde = grid%ed33 ;
			ims = grid%sm31 ; ime = grid%em31 ;
			kms = grid%sm32 ; kme = grid%em32 ;
			jms = grid%sm33 ; jme = grid%em33 ;
			its = grid%sp31 ; ite = grid%ep31 ;   ! note that tile is entire patch
			kts = grid%sp32 ; kte = grid%ep32 ;   ! note that tile is entire patch
			jts = grid%sp33 ; jte = grid%ep33 ;   ! note that tile is entire patch
		END SELECT
		
		! FOR LES, set stretch to false
		! stretch_grid = .true.
		stretch_grid = .false.
		delt = 3.
		
		! z_scale = .50
		z_scale = .40
		pi = 2.*asin(1.0)
		WRITE(6,*) ' pi is ', pi
		nxc = (ide-ids)/2
		nyc = (jde-jds)/2
		
		CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
		
		! Here we check to see if the boundary conditions are set properly
		CALL boundary_condition_check( config_flags, bdyzone, error, grid%id )
		
		moisture_init = .true.
		
		grid%itimestep=0
		
#ifdef DM_PARALLEL
		CALL wrf_dm_bcast_bytes( icm , IWORDSIZE )
		CALL wrf_dm_bcast_bytes( jcm , IWORDSIZE )
#endif
		CALL nl_set_mminlu(1, '    ')
		CALL nl_set_iswater(1,0)
		CALL nl_set_cen_lat(1,40.)
		CALL nl_set_cen_lon(1,-105.)
		CALL nl_set_truelat1(1,0.)
		CALL nl_set_truelat2(1,0.)
		CALL nl_set_moad_cen_lat (1,0.)
		CALL nl_set_stand_lon (1,0.)
		CALL nl_set_pole_lon (1,0.)
		CALL nl_set_pole_lat (1,90.)
		CALL nl_set_map_proj(1,0)
		
		! Here we initialize data we currently is not initialized in the input data
		DO j = jts, jte
			DO i = its, ite
				grid%msftx(i,j)    = 1.
				grid%msfty(i,j)    = 1.
				grid%msfux(i,j)    = 1.
				grid%msfuy(i,j)    = 1.
				grid%msfvx(i,j)    = 1.
				grid%msfvx_inv(i,j)= 1.
				grid%msfvy(i,j)    = 1.
				grid%sina(i,j)     = 0.
				grid%cosa(i,j)     = 1.
				grid%e(i,j)        = 0.
				grid%f(i,j)        = 1.e-4
			END DO
		END DO
		
		DO j = jts, jte
			DO k = kts, kte
				DO i = its, ite
					grid%ww(i,k,j) = 0.
				END DO
			END DO
		END DO
		
		grid%step_number = 0
		
		! Set up the grid
		IF (stretch_grid) THEN ! exponential stretch for eta (nearly constant dz)
			DO k=1, kde
				grid%znw(k) = (exp(-(k-1)/float(kde-1)/z_scale) - exp(-1./z_scale)) &
				            / (1.-exp(-1./z_scale))
			ENDDO
		ELSE
			DO k=1, kde
				grid%znw(k) = 1. - float(k-1)/float(kde-1)
			ENDDO
		ENDIF
		
		DO k=1, kde-1
			grid%dnw(k) = grid%znw(k+1) - grid%znw(k)
			grid%rdnw(k) = 1./grid%dnw(k)
			grid%znu(k) = 0.5*(grid%znw(k+1)+grid%znw(k))
		ENDDO
		DO k=2, kde-1
			grid%dn(k) = 0.5*(grid%dnw(k)+grid%dnw(k-1))
			grid%rdn(k) = 1./grid%dn(k)
			grid%fnp(k) = .5* grid%dnw(k  )/grid%dn(k)
			grid%fnm(k) = .5* grid%dnw(k-1)/grid%dn(k)
		ENDDO
		
		cof1 = (2.*grid%dn(2)+grid%dn(3))/(grid%dn(2)+grid%dn(3))*grid%dnw(1)/grid%dn(2)
		cof2 =     grid%dn(2)        /(grid%dn(2)+grid%dn(3))*grid%dnw(1)/grid%dn(3)
		grid%cf1  = grid%fnp(2) + cof1
		grid%cf2  = grid%fnm(2) - cof1 - cof2
		grid%cf3  = cof2
		
		grid%cfn  = (.5*grid%dnw(kde-1)+grid%dn(kde-1))/grid%dn(kde-1)
		grid%cfn1 = -.5*grid%dnw(kde-1)/grid%dn(kde-1)
		grid%rdx = 1./config_flags%dx
		grid%rdy = 1./config_flags%dy
		
		! Get the sounding from the ascii sounding file, first get dry sounding and
		! Calculate base state
		dry_sounding = .true.
		IF ( wrf_dm_on_monitor() ) THEN
			WRITE(6,*) ' getting dry sounding for base state '
			CALL get_sounding( zk, p_in, pd_in, theta, rho, u, v, qv, dry_sounding, nl_max, nl_in )
		ENDIF
		CALL wrf_dm_bcast_real( zk , nl_max )
		CALL wrf_dm_bcast_real( p_in , nl_max )
		CALL wrf_dm_bcast_real( pd_in , nl_max )
		CALL wrf_dm_bcast_real( theta , nl_max )
		CALL wrf_dm_bcast_real( rho , nl_max )
		CALL wrf_dm_bcast_real( u , nl_max )
		CALL wrf_dm_bcast_real( v , nl_max )
		CALL wrf_dm_bcast_real( qv , nl_max )
		CALL wrf_dm_bcast_integer ( nl_in , 1 )
		
		WRITE(6,*) ' returned from reading sounding, nl_in is ', nl_in
		
		! Find ptop for the desired ztop (ztop is input from the namelist),
		! and find surface pressure
		grid%p_top = interp_0( p_in, zk, config_flags%ztop, nl_in )
		
		DO j=jts, jte
			DO i=its, ite
				grid%ht(i,j) = 0.
			ENDDO
		ENDDO
		
		xs=ide/2 -3
		xs=ids   -3
		xe=xs + 6
		ys=jde/2 -3
		ye=ys + 6
		mtn_ht = 500
#ifdef MTN
		DO j=max(ys,jds),min(ye,jde-1)
			DO i=max(xs,ids),min(xe,ide-1)
				grid%ht(i,j) = mtn_ht * 0.25 &
				             * ( 1. + COS ( 2*pi/(xe-xs) * ( i-xs ) + pi ) ) &
				             * ( 1. + COS ( 2*pi/(ye-ys) * ( j-ys ) + pi ) )
			ENDDO
		ENDDO
#endif
#ifdef EW_RIDGE
		DO j=max(ys,jds),min(ye,jde-1)
			DO i=ids,ide
				grid%ht(i,j) = mtn_ht * 0.50 * ( 1. + COS ( 2*pi/(ye-ys) * ( j-ys ) + pi ) )
			ENDDO
		ENDDO
#endif
#ifdef NS_RIDGE
		DO j=jds,jde
			DO i=max(xs,ids),min(xe,ide-1)
				grid%ht(i,j) = mtn_ht * 0.50 * ( 1. + COS ( 2*pi/(xe-xs) * ( i-xs ) + pi ) )
			ENDDO
		ENDDO
#endif
		DO j=jts,jte
			DO i=its,ite
				grid%phb(i,1,j) = g * grid%ht(i,j)
				grid%ph0(i,1,j) = g * grid%ht(i,j)
			ENDDO
		ENDDO
		
		DO J = jts, jte
			DO I = its, ite
				p_surf = interp_0( p_in, zk, grid%phb(i,1,j)/g, nl_in )
				grid%mub(i,j) = p_surf-grid%p_top
				
				! This is dry hydrostatic sounding (base state), so given grid%p (coordinate),
				! interp theta (from interp) and compute 1/rho from eqn. of state
				DO K = 1, kte-1
					p_level = grid%znu(k)*(p_surf - grid%p_top) + grid%p_top
					grid%pb(i,k,j) = p_level
					grid%t_init(i,k,j) = interp_0( theta, p_in, p_level, nl_in ) - t0
				grid%alb(i,k,j) = (r_d/p1000mb)*(grid%t_init(i,k,j)+t0)*(grid%pb(i,k,j)/p1000mb)**cvpm
				ENDDO
				
				! calc hydrostatic balance (alternatively we could interp the geopotential from the
				! sounding, but this assures that the base state is in exact hydrostatic balance with
				! respect to the model eqns.
				DO k  = 2,kte
					grid%phb(i,k,j) = grid%phb(i,k-1,j) - grid%dnw(k-1)*grid%mub(i,j)*grid%alb(i,k-1,j)
				ENDDO
			ENDDO
		ENDDO
		
		IF ( wrf_dm_on_monitor() ) THEN
			WRITE(6,*) ' ptop is ',grid%p_top
			WRITE(6,*) ' base state grid%mub(1,1), p_surf is ',grid%mub(1,1),grid%mub(1,1)+grid%p_top
		ENDIF
		
		! calculate full state for each column - this includes moisture.
		WRITE(6,*) ' getting moist sounding for full state '
		dry_sounding = .false.
		CALL get_sounding( zk, p_in, pd_in, theta, rho, u, v, qv, dry_sounding, nl_max, nl_in )
		
		DO J = jts, min(jde-1,jte)
			DO I = its, min(ide-1,ite)
			
				! At this point grid%p_top is already set. find the DRY mass in the column
				! by interpolating the DRY pressure.
				pd_surf = interp_0( pd_in, zk, grid%phb(i,1,j)/g, nl_in )
				
				! compute the perturbation mass and the full mass
				grid%mu_1(i,j) = pd_surf-grid%p_top - grid%mub(i,j)
				grid%mu_2(i,j) = grid%mu_1(i,j)
				grid%mu0(i,j) = grid%mu_1(i,j) + grid%mub(i,j)
				
				! given the dry pressure and coordinate system, interp the potential
				! temperature and qv
				DO k=1,kde-1
					p_level = grid%znu(k)*(pd_surf - grid%p_top) + grid%p_top
					moist(i,k,j,P_QV) = interp_0( qv, pd_in, p_level, nl_in )
					grid%t_1(i,k,j)   = interp_0( theta, pd_in, p_level, nl_in ) - t0
					grid%t_2(i,k,j)   = grid%t_1(i,k,j)
				ENDDO
				
				! integrate the hydrostatic equation (from the RHS of the bigstep
				! vertical momentum equation) down from the top to get grid%p.
				! first from the top of the model to the top pressure
				k = kte-1  ! top level
				qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k,j,P_QV))
				qvf2 = 1./(1.+qvf1)
				qvf1 = qvf1*qvf2
				
			!	grid%p(i,k,j) = - 0.5*grid%mu_1(i,j)/grid%rdnw(k)
				grid%p(i,k,j) = - 0.5*(grid%mu_1(i,j)+qvf1*grid%mub(i,j))/grid%rdnw(k)/qvf2
				qvf = 1. + rvovrd*moist(i,k,j,P_QV)
				grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_1(i,k,j)+t0)*qvf &
				                * (((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
				grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
				
				! down the column
				DO k=kte-2, 1, -1
					qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k+1,j,P_QV))
					qvf2 = 1./(1.+qvf1)
					qvf1 = qvf1*qvf2
					grid%p(i,k,j) = grid%p(i,k+1,j) &
					              - (grid%mu_1(i,j) + qvf1*grid%mub(i,j))/qvf2/grid%rdn(k+1)
					qvf = 1. + rvovrd*moist(i,k,j,P_QV)
					grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_1(i,k,j)+t0)*qvf &
					                * (((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
					grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
				ENDDO
				
				! this is the hydrostatic equation used in the model after the
				! small timesteps.  In the model, grid%al (inverse density)
				! is computed from the geopotential.
				grid%ph_1(i,1,j) = 0.
				DO k  = 2,kte
					grid%ph_1(i,k,j) = grid%ph_1(i,k-1,j) - (1./grid%rdnw(k-1)) &
					                 * ( (grid%mub(i,j)+grid%mu_1(i,j))*grid%al(i,k-1,j) &
					                    + grid%mu_1(i,j)*grid%alb(i,k-1,j) )
					grid%ph_2(i,k,j) = grid%ph_1(i,k,j) 
					grid%ph0(i,k,j) = grid%ph_1(i,k,j) + grid%phb(i,k,j)
				ENDDO
				
				IF ( wrf_dm_on_monitor() ) THEN
					IF((i==2) .and. (j==2)) THEN
						WRITE(6,*) ' grid%ph_1 calc ',grid%ph_1(2,1,2), grid%ph_1(2,2,2), &
							grid%mu_1(2,2)+grid%mub(2,2), grid%mu_1(2,2), & 
							grid%alb(2,1,2), grid%al(1,2,1), grid%rdnw(1)
					ENDIF
				ENDIF
			ENDDO
		ENDDO
		
		! Thermal perturbation to kick off convection
		WRITE(6,*) ' nxc, nyc for perturbation ', nxc, nyc
		WRITE(6,*) ' delt for perturbation ', delt
		
		! For LES, change the initial random perturbations
		! For 2D test, call randx outside I-loop
		! For 3D runs, call randx inside both I-J loops
		
		DO J = jts, min(jde-1,jte)
		!	yrad = config_flags%dy*float(j-nyc)/10000.
			yrad = 0.
			DO I = its, min(ide-1,ite)
			!	xrad = config_flags%dx*float(i-nxc)/10000.
				xrad = 0.
				CALL random_number(randx)
				randx = randx - 0.5
			!	DO K = 1, kte-1
				DO K = 1, 4
					! No bubbles for LES!
					! put in preturbation theta (bubble) and recalc density.  note,
					! the mass in the column is not changing, so when theta changes,
					! we recompute density and geopotential
				!	zrad = 0.5*(grid%ph_1(i,k,j)+grid%ph_1(i,k+1,j) &
				!	     + grid%phb(i,k,j)+grid%phb(i,k+1,j))/g
				!	zrad = (zrad-1500.)/1500.
					zrad = 0.
					RAD=SQRT(xrad*xrad+yrad*yrad+zrad*zrad)
					IF (RAD <= 1.) THEN
						!	grid%t_1(i,k,j)=grid%t_1(i,k,j)+delt*COS(.5*PI*RAD)**2
						grid%t_1(i,k,j)=grid%t_1(i,k,j)+ 0.1 *randx
						grid%t_2(i,k,j)=grid%t_1(i,k,j)
						qvf = 1. + rvovrd*moist(i,k,j,P_QV)
						grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_1(i,k,j)+t0)*qvf &
						                * (((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
						grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
					ENDIF
				ENDDO
				
				! Rebalance hydrostatically
				DO k = 2, kte
					grid%ph_1(i,k,j) = grid%ph_1(i,k-1,j) - (1./grid%rdnw(k-1)) &
					                 * ( (grid%mub(i,j)+grid%mu_1(i,j))*grid%al(i,k-1,j) &
					                    + grid%mu_1(i,j)*grid%alb(i,k-1,j) )
					grid%ph_2(i,k,j) = grid%ph_1(i,k,j)
					grid%ph0(i,k,j) = grid%ph_1(i,k,j) + grid%phb(i,k,j)
				ENDDO
			ENDDO
		ENDDO
		
		IF ( wrf_dm_on_monitor() ) THEN
			WRITE(6,*) ' grid%mu_1 from comp ', grid%mu_1(1,1)
			WRITE(6,*) ' full state sounding from comp, ph, grid%p, grid%al, grid%t_1, qv'
			DO k=1, kde-1
				WRITE(6,'(i3,1x,5(1x,1pe10.3))') k, grid%ph_1(1,k,1)+grid%phb(1,k,1), &
					grid%p(1,k,1)+grid%pb(1,k,1), grid%alt(1,k,1), grid%t_1(1,k,1)+t0, moist(1,k,1,P_QV)
			ENDDO
			WRITE(6,*) ' pert state sounding from comp, grid%ph_1, pp, alp, grid%t_1, qv'
			DO k = 1, kde-1
				WRITE(6,'(i3,1x,5(1x,1pe10.3))') k, grid%ph_1(1,k,1), grid%p(1,k,1), grid%al(1,k,1), &
					grid%t_1(1,k,1), moist(1,k,1,P_QV)
			ENDDO
		ENDIF
		
		! Interp v
		DO J = jts, jte
			DO I = its, min(ide-1,ite)
				IF (j == jds) THEN
					z_at_v = grid%phb(i,1,j)/g
				ELSE IF (j == jde) THEN
					z_at_v = grid%phb(i,1,j-1)/g
				ELSE
					z_at_v = 0.5*(grid%phb(i,1,j)+grid%phb(i,1,j-1))/g
				END IF
				p_surf = interp_0( p_in, zk, z_at_v, nl_in )
				DO K = 1, kte-1
					p_level = grid%znu(k)*(p_surf - grid%p_top) + grid%p_top
					grid%v_1(i,k,j) = interp_0( v, p_in, p_level, nl_in )
					grid%v_2(i,k,j) = grid%v_1(i,k,j)
				ENDDO
			ENDDO
		ENDDO
		
		! Interp u
		DO J = jts, min(jde-1,jte)
			DO I = its, ite
				IF (i == ids) THEN
					z_at_u = grid%phb(i,1,j)/g
				ELSE IF (i == ide) THEN
					z_at_u = grid%phb(i-1,1,j)/g
				ELSE
					z_at_u = 0.5*(grid%phb(i,1,j)+grid%phb(i-1,1,j))/g
				END IF
				p_surf = interp_0( p_in, zk, z_at_u, nl_in )
				DO K = 1, kte-1
					p_level = grid%znu(k)*(p_surf - grid%p_top) + grid%p_top
					grid%u_1(i,k,j) = interp_0( u, p_in, p_level, nl_in )
					grid%u_2(i,k,j) = grid%u_1(i,k,j)
				ENDDO
			ENDDO
		ENDDO
		
		! Set w
		DO J = jts, min(jde-1,jte)
			DO K = kts, kte
				DO I = its, min(ide-1,ite)
					grid%w_1(i,k,j) = 0.
					grid%w_2(i,k,j) = 0.
				ENDDO
			ENDDO
		ENDDO
		
		! Set a few more things
		DO J = jts, min(jde-1,jte)
			DO K = kts, kte-1
				DO I = its, min(ide-1,ite)
					grid%h_diabatic(i,k,j) = 0.
				ENDDO
			ENDDO
		ENDDO
		
		IF ( wrf_dm_on_monitor() ) THEN
			DO k=1,kte-1
				grid%t_base(k) = grid%t_1(1,k,1)
				grid%qv_base(k) = moist(1,k,1,P_QV)
				grid%u_base(k) = grid%u_1(1,k,1)
				grid%v_base(k) = grid%v_1(1,k,1)
		grid%z_base(k) = 0.5*(grid%phb(1,k,1)+grid%phb(1,k+1,1)+grid%ph_1(1,k,1)+grid%ph_1(1,k+1,1))/g
			 ENDDO
		ENDIF
		CALL wrf_dm_bcast_real( grid%t_base , kte )
		CALL wrf_dm_bcast_real( grid%qv_base , kte )
		CALL wrf_dm_bcast_real( grid%u_base , kte )
		CALL wrf_dm_bcast_real( grid%v_base , kte )
		CALL wrf_dm_bcast_real( grid%z_base , kte )
		
		DO J = jts, min(jde-1,jte)
			DO I = its, min(ide-1,ite)
				thtmp   = grid%t_2(i,1,j)+t0
				ptmp    = grid%p(i,1,j)+grid%pb(i,1,j)
				temp(1) = thtmp * (ptmp/p1000mb)**rcp
				thtmp   = grid%t_2(i,2,j)+t0
				ptmp    = grid%p(i,2,j)+grid%pb(i,2,j)
				temp(2) = thtmp * (ptmp/p1000mb)**rcp
				thtmp   = grid%t_2(i,3,j)+t0
				ptmp    = grid%p(i,3,j)+grid%pb(i,3,j)
				temp(3) = thtmp * (ptmp/p1000mb)**rcp
				
				! For LES-CBL, add 5 degrees to the surface temperature!
			!	grid%tsk(I,J)=grid%cf1*temp(1)+grid%cf2*temp(2)+grid%cf3*temp(3)+5.
				! Other case
				grid%tsk(I,J)=grid%cf1*temp(1)+grid%cf2*temp(2)+grid%cf3*temp(3)
				grid%tmn(I,J)=grid%tsk(I,J)-0.5
			ENDDO
		ENDDO
		
	END SUBROUTINE init_domain_rk
	
#endif /*WRFLES or ORIGINAL init_domain_rk*/

	SUBROUTINE init_module_initialize
	END SUBROUTINE init_module_initialize

!------------------------------------------------------------------------------
!	test driver for get_sounding
!		implicit none
!		integer n
!		parameter(n = 1000)
!		real zk(n),p(n),theta(n),rho(n),u(n),v(n),qv(n),pd(n)
!		logical dry
!		integer nl,k
!		dry = .false.
!		dry = .true.
!		call get_sounding( zk, p, pd, theta, rho, u, v, qv, dry, n, nl )
!		write(6,*) ' input levels ',nl
!		write(6,*) ' sounding '
!		write(6,*) '  k  height(m)  press (Pa) pd(Pa) theta (K) den(kg/m^3)  u(m/s)     v(m/s)    qv(g/g) '
!		do k=1,nl
!			write(6,'(1x,i3,8(1x,1pe10.3))') k, zk(k), p(k), pd(k), theta(k), rho(k), u(k), v(k), qv(k)
!		enddo
!	end
!------------------------------------------------------------------------------

	SUBROUTINE get_sounding( zk, p, p_dry, theta, rho, u, v, qv, dry, nl_max, nl_in )
		IMPLICIT NONE
		INTEGER nl_max, nl_in
		REAL zk(nl_max), p(nl_max), theta(nl_max), rho(nl_max), &
		     u(nl_max), v(nl_max), qv(nl_max), p_dry(nl_max)
		LOGICAL dry
		INTEGER n
		PARAMETER(n=1000)
		LOGICAL debug
		PARAMETER( debug = .true.)
		
		! Input sounding data
		REAL p_surf, th_surf, qv_surf
		REAL pi_surf, pi(n)
		REAL h_input(n), th_input(n), qv_input(n), u_input(n), v_input(n)
		
		! Diagnostics
		REAL rho_surf, p_input(n), rho_input(n)
		REAL pm_input(n)  !  this are for full moist sounding
		
		! Local data
		REAL r
		PARAMETER(r = r_d)
		INTEGER k, it, nl
		REAL qvf, qvf1, dz
		
		! First, read the sounding
		CALL read_sounding( p_surf, th_surf, qv_surf, &
		                    h_input, th_input, qv_input, u_input, v_input,n, nl, debug )
		
		IF (dry) THEN
			DO k = 1, nl
				qv_input(k) = 0.
			ENDDO
		ENDIF
		
		IF (debug) WRITE(6,*) ' number of input levels = ', nl
		
		nl_in = nl
		IF (nl_in .gt. nl_max ) THEN
			WRITE(6,*) ' too many levels for input arrays ', nl_in,nl_max
			CALL wrf_error_fatal ( ' too many levels for input arrays ' )
		ENDIF
		
		! Compute diagnostics
		! First, convert qv(g/kg) to qv(g/g)
		DO k = 1, nl
			qv_input(k) = 0.001*qv_input(k)
		ENDDO
		p_surf = 100.*p_surf  ! convert to pascals
		qvf = 1. + rvovrd*qv_input(1)
		rho_surf = 1./((r/p1000mb)*th_surf*qvf*((p_surf/p1000mb)**cvpm))
		pi_surf = (p_surf/p1000mb)**(r/cp)
		
		IF (debug) THEN
			WRITE(6,*) ' surface density is ', rho_surf
			WRITE(6,*) ' surface pi is      ', pi_surf
		ENDIF
		
		! Integrate moist sounding hydrostatically, starting from the specified surface pressure
		! -> first, integrate from surface to lowest level
		qvf = 1. + rvovrd*qv_input(1)
		qvf1 = 1. + qv_input(1)
		rho_input(1) = rho_surf
		dz = h_input(1)
		DO it = 1, 10
			pm_input(1) = p_surf - 0.5*dz*(rho_surf+rho_input(1))*g*qvf1
			rho_input(1) = 1./((r/p1000mb)*th_input(1)*qvf*((pm_input(1)/p1000mb)**cvpm))
		ENDDO
		
		! Integrate up the column
		DO k = 2, nl
			rho_input(k) = rho_input(k-1)
			dz = h_input(k)-h_input(k-1)
			qvf1 = 0.5*(2.+(qv_input(k-1)+qv_input(k)))
			qvf = 1. + rvovrd*qv_input(k)   ! qv is in g/kg here
			DO it = 1, 10
				pm_input(k) = pm_input(k-1) - 0.5*dz*(rho_input(k)+rho_input(k-1))*g*qvf1
				rho_input(k) = 1./((r/p1000mb)*th_input(k)*qvf*((pm_input(k)/p1000mb)**cvpm))
			ENDDO
		ENDDO
		
		! We have the moist sounding
		! Next, compute the dry sounding using p at the highest level from the
		! moist sounding and integrating down.
		p_input(nl) = pm_input(nl)
		DO k = nl-1, 1, -1
			dz = h_input(k+1)-h_input(k)
			p_input(k) = p_input(k+1) + 0.5*dz*(rho_input(k)+rho_input(k+1))*g
		ENDDO
		DO k = 1, nl
			zk(k) = h_input(k)
			p(k) = pm_input(k)
			p_dry(k) = p_input(k)
			theta(k) = th_input(k)
			rho(k) = rho_input(k)
			u(k) = u_input(k)
			v(k) = v_input(k)
			qv(k) = qv_input(k)
		ENDDO
		
		IF (debug) THEN
			WRITE(6,*) ' sounding '
			WRITE(6,*) '  k  height(m)  press (Pa) pd(Pa) theta (K) den(kg/m^3)  u(m/s)     v(m/s)    qv(g/g) '
			DO k = 1, nl
				WRITE(6,'(1x,i3,8(1x,1pe10.3))') k, zk(k), p(k), p_dry(k), theta(k), rho(k), u(k), &
					v(k), qv(k)
			ENDDO
		ENDIF
		
	END SUBROUTINE get_sounding
	
!------------------------------------------------------------------------------

	SUBROUTINE read_sounding( ps,ts,qvs,h,th,qv,u,v,n,nl,debug )
		IMPLICIT NONE
		INTEGER n, nl
		REAL ps, ts, qvs, h(n), th(n), qv(n), u(n), v(n)
		LOGICAL end_of_file
		LOGICAL debug
		INTEGER k
		
		OPEN(UNIT=10, FILE='input_sounding', FORM='formatted', STATUS='old')
		REWIND(10)
		READ(10,*) ps, ts, qvs
		IF (debug) THEN
			WRITE(6,*) ' input sounding surface parameters '
			WRITE(6,*) ' surface pressure (mb) ', ps
			WRITE(6,*) ' surface pot. temp (K) ', ts
			WRITE(6,*) ' surface mixing ratio (g/kg) ', qvs
		ENDIF
		
		end_of_file = .FALSE.
		k = 0
		
		DO WHILE (.NOT.end_of_file)
			READ(10,*,END=100) h(k+1), th(k+1), qv(k+1), u(k+1), v(k+1)
			k = k + 1
			IF (debug) WRITE(6,'(1x,i3,5(1x,e10.3))') k, h(k), th(k), qv(k), u(k), v(k)
			GO TO 110
100      end_of_file = .TRUE.
110      CONTINUE
		ENDDO
		nl = k
		CLOSE(UNIT=10, STATUS='keep')
	END SUBROUTINE read_sounding
	
END MODULE module_initialize_ideal
