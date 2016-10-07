#ifdef WRFLES
MODULE module_wrfles

! Version 1.3.2.1
! Takanobu Yamaguchi
! tak.yamaguchi@noaa.gov
! Since 2011/02
! Implemented in WRF3.3.1 in 2012/03
! - This module sets up and updates various parameters required for LES mode.
! - Followd by SAM6.8.2
! - Nesting is not considered, so do not run WRF-LES with nesting.
! - Althoguth this module should work with OMP since there is no OMP call in this 
!   module and ohter source codes related to this module, do not compile with OMP for safety reason.
!
! What is changed from v1.3.2
! - Tw bugs were fixed for the RICO initialization (r01 to rpm1 for qv, kte=>kte-1)
! - Deleted local logical flags to avoid confusion and bugs. Deleted flags: dosfcforcing,
!   donudging_q, donudging_uvtq, dolsforcing, dolsforcings, dolsforcing_uv, dolsforcing_tq
! - Modified so that u0 & v0 are not computed if doupdategwnd=TRUE and donudging_uv=FALSE.
! - Modified so that wrfles_subsidence works when wlsf is positive (ascent).

	IMPLICIT NONE
	
!!	SAVE   ! BE CAREFUL WHEN REMOVE THIS LINE
	PRIVATE   ! BE CAREFUL WHEN REMOVE THIS LINE
	
	CHARACTER (LEN=50), SAVE, PUBLIC :: casename
	
	! Flags specified in prm
	LOGICAL, SAVE, PUBLIC :: dosgsdif       ! apply SGS diffusion
	LOGICAL, SAVE, PUBLIC :: dospecifysst   ! time varying or constant SST
	LOGICAL, SAVE, PUBLIC :: docompsfcflx   ! compute sfc shflx and lhflx
	LOGICAL, SAVE, PUBLIC :: docompsfcust   ! compute sfc ustar
	LOGICAL, SAVE, PUBLIC :: dosfcspecial   ! special surface fluxes specified by particular case
	LOGICAL, SAVE :: doupdategwnd   ! update large scale wind (u_base & v_base)
	LOGICAL, SAVE :: dosubsidence   ! apply subsidence
	LOGICAL, SAVE :: doadvforcing   ! apply large scale forcing
	LOGICAL, SAVE :: doradforcing   ! apply radiative forcing
	LOGICAL, SAVE :: donudging_uv   ! apply u & v nudging
	LOGICAL, SAVE :: donudging_tq   ! apply tp & qv nudging (input)
	LOGICAL, SAVE :: donudging_tp   ! apply theta nudging
	LOGICAL, SAVE :: donudging_qv   ! apply qv nudging
	LOGICAL, SAVE :: donudging_qt   ! apply nudging to qt instead of qv
	LOGICAL, SAVE :: dondglimiter   ! apply monotonic nudging for qv and qnt
	LOGICAL, SAVE :: doupperbound   ! maintain the vertical gradient of tp & qv at two highest level
	
	! Constant variables specified in prm
	INTEGER, SAVE :: perturb_type   ! initial perturbation
	INTEGER, SAVE :: sfcspecial_type   ! special surface flux type
	INTEGER, SAVE, PUBLIC :: surface_type   ! ocean or land
	REAL, SAVE, PUBLIC :: coriolis
	REAL, SAVE, PUBLIC :: latitude
	REAL, SAVE, PUBLIC :: longitude
	REAL, SAVE :: tau_ndg   ! nudging time scale (s) for tp, qv and u, v if tau_ndguv is not set
	REAL, SAVE :: tau_ndguv ! nudging time scale for u, v
	REAL, SAVE, PUBLIC :: ug, vg    ! domain translation velocity
!????? How to use ug and vg in WRF ????????????????????????????????????????????????????????????????
	
	! Special flags for the Feingold scheme specified in prm
	LOGICAL, SAVE :: donudging_qnt   ! apply nudging qnn+qnc to the initial CCN number concentration
	
	! Parameters for module_mp_feingold2m.F specified in prm
	INTEGER, SAVE, PUBLIC :: xindex_feingold2m   ! Index of x for activation
	REAL, SAVE, PUBLIC :: nccn_feingold2m        ! CCN number concentration (#/mg)
	REAL, SAVE, PUBLIC :: dg_ccn_feingold2m      ! Mean of CCN size distribution (cm)
	REAL, SAVE, PUBLIC :: sg_ccn_feingold2m      ! Sigma of CCN size distrituion
	LOGICAL, SAVE, PUBLIC :: bimodal_feingold2m  ! size distribution is bimodal
	REAL, SAVE, PUBLIC :: nccn2_feingold2m       ! CCN number concentration for 2nd distribution
	REAL, SAVE, PUBLIC :: dg2_ccn_feingold2m     ! Mean of CCN size distribution for 2nd distribution
	REAL, SAVE, PUBLIC :: sg2_ccn_feingold2m     ! Sigma of CCN size distrituion for 2nd distribution
	REAL, SAVE, PUBLIC :: sg_cloud_feingold2m    ! Sigma of cloud distribution
	REAL, SAVE, PUBLIC :: sg_rain_feingold2m     ! Sigma of rain distribution
	REAL, SAVE, PUBLIC :: drzst_feingold2m       ! # of seconds to initialte drizzle
	INTEGER, SAVE, PUBLIC :: drzflg_feingold2m   ! Sedimentation flag: 0-no, 1-bin, 2-bulk
	LOGICAL, SAVE, PUBLIC :: colflg_feingold2m   ! Collision & coalescence flag
	LOGICAL, SAVE, PUBLIC :: srcccn_add_feingold2m         ! Add CCN source
	INTEGER, SAVE, PUBLIC :: srcccn_startstep_feingold2m   ! Start time step for soruce of CCN
	REAL, SAVE, PUBLIC :: srcccn_starttime_feingold2m      ! Start actual time (second)
	REAL, SAVE, PUBLIC :: srcccn_feingold2m        ! Source of CCN (#/mg/hour)
	REAL, SAVE, PUBLIC :: srcccn_qvmin_feingold2m  ! CCN source is applied more than the value (g/kg)
	
	! Surface forcing
	INTEGER, SAVE :: nsfc
	REAL, SAVE, ALLOCATABLE, DIMENSION(:) :: daysfc   ! day
	REAL, SAVE, ALLOCATABLE, DIMENSION(:) :: sstsfc   ! SST
	REAL, SAVE, ALLOCATABLE, DIMENSION(:) :: ustsfc   ! u & v surface frictioin velocity (m/s)
	REAL, SAVE, ALLOCATABLE, DIMENSION(:) :: shfsfc   ! sensible heat flux (W/m2)
	REAL, SAVE, ALLOCATABLE, DIMENSION(:) :: lhfsfc   ! latent heat flux (W/m2)
	REAL, SAVE :: coef_sfc    ! linear time interpolation coefficient for large-scale forcing
	INTEGER, SAVE :: n1_sfc   ! indices for two time level for time interpolaiton
	INTEGER, SAVE :: n2_sfc   ! coef_sfc, n1_sfc & n2_sfc can be declared in wrfles_sfcforcing.
	
	! Large scale forcing
	INTEGER, SAVE :: nlsf
	INTEGER, SAVE :: nzlsf   ! number of vertical levels for large scale forcing
	REAL, SAVE :: itau_ndg   ! = 1./tau_ndg
	REAL, SAVE :: itau_ndguv ! = 1./tau_ndguv
	REAL, SAVE, ALLOCATABLE, DIMENSION(:)   :: daylsf   ! day
	REAL, SAVE, ALLOCATABLE, DIMENSION(:)   :: zlsf     ! vertical level
	REAL, SAVE, ALLOCATABLE, DIMENSION(:,:) :: ulsf     ! large scale u for nudgeing
	REAL, SAVE, ALLOCATABLE, DIMENSION(:,:) :: vlsf     ! large scale v for nudgeing
	REAL, SAVE, ALLOCATABLE, DIMENSION(:,:) :: wlsf     ! large scale subsidence
	REAL, SAVE, ALLOCATABLE, DIMENSION(:,:) :: tlsf     ! large scale theta for nudgeing
	REAL, SAVE, ALLOCATABLE, DIMENSION(:,:) :: qlsf     ! large scale qv for nudgeing
	REAL, SAVE, ALLOCATABLE, DIMENSION(:,:) :: dtlsf    ! large scale adv tendency for theta (K/s)
	REAL, SAVE, ALLOCATABLE, DIMENSION(:,:) :: dqlsf    ! large scale adv tendency for qv (kg/kg/s)
	REAL, SAVE, ALLOCATABLE, DIMENSION(:,:) :: rlsf     ! large scale radiative heating rate (K/s)
	REAL, SAVE :: coef_lsf    ! linear time interpolation coefficient for large-scale forcing
	INTEGER, SAVE :: n1_lsf   ! indices for two time level for time interpolaiton
	INTEGER, SAVE :: n2_lsf
	REAL, SAVE, ALLOCATABLE, DIMENSION(:,:) :: undg, vndg, tndg, qndg ! interp. ZLSF=>ZSH
	REAL, SAVE, ALLOCATABLE, DIMENSION(:) :: u0, v0, t0, qv0, qc0  ! mean profile at ZSH
	REAL, SAVE, ALLOCATABLE, DIMENSION(:) :: qv0min, qv0max   ! min & max profile for qv
	REAL, SAVE, ALLOCATABLE, DIMENSION(:) :: qc0min, qc0max   ! min & max profile for qc
#ifdef ESRL
	REAL, SAVE, ALLOCATABLE, DIMENSION(:) :: qnt0   ! mean profile for nudging
	REAL, SAVE, ALLOCATABLE, DIMENSION(:) :: qnn0min, qnn0max   ! min & max profile for qnn
	REAL, SAVE, ALLOCATABLE, DIMENSION(:) :: qnc0min, qnc0max   ! min & max profile for qnc
#endif /*ESRL*/
	REAL, SAVE, ALLOCATABLE, DIMENSION(:,:,:) :: zu_eta, zv_eta, zs_eta   ! u, v, s local height
	INTEGER, SAVE, ALLOCATABLE, DIMENSION(:,:,:) :: km1zlsf, k00zlsf, kp1zlsf, kp2zlsf! ZLSF=>ZS_ETA
	
	! WRF grid index information
	INTEGER, SAVE :: ids, ide, jds, jde, kds, kde   ! Index in terms of the whole domain
	INTEGER, SAVE :: ims, ime, jms, jme, kms, kme   ! Index including ghost points
	INTEGER, SAVE :: ips, ipe, jps, jpe, kps, kpe   ! Index inside of the subdomain
	
	! MPI stuff for horizontal mean calculation (for nudging)
	LOGICAL, SAVE :: dompi    ! logical switch to do mpi
	INTEGER, SAVE :: mpi_comm ! communicator
	INTEGER, SAVE :: rank     ! rank of the current subdomain task (default = 0)
	INTEGER, SAVE :: nsubdomains   ! number of subdomains/tiles/procesors
	INTEGER, SAVE :: nx    ! # of x direction grid, which does not contain ghost points for MPI
	INTEGER, SAVE :: ny    ! # of y direction grid, which does not contain ghost points for MPI
	INTEGER, SAVE :: nz    ! # of vertical levels for WRF: mass level at nz does not have value
	INTEGER, SAVE :: nzm   ! nz-1: scalar level
	INTEGER, SAVE :: nzsh  ! nzsh = (nzm-1)*hrlayers+1: number of levels for zsh
	INTEGER, SAVE :: nzmm  ! nzmm = (nzm-1)*hrlayers_zmm+1: used for min and max profiles
	INTEGER, SAVE :: nndg  ! # of vars to be nudged = number of mean profiles to be computed
	INTEGER, SAVE :: nmnmx ! # of vars to construct min & max profiles for monotonic nudging
	INTEGER, PARAMETER :: hrlayers = 10   ! # of layers+1 between zs(k) and zs(k+1) for zsh
	                                      ! zs=zsh if hrlayers=1
	INTEGER, PARAMETER :: hrlayers_zmm = 10  ! for zmm
	REAL, SAVE, ALLOCATABLE, DIMENSION(:) :: zs    ! height of the initial eta scalar level
	REAL, SAVE, ALLOCATABLE, DIMENSION(:) :: zsh   ! high resolution height level used for nudging.
	REAL, SAVE, ALLOCATABLE, DIMENSION(:) :: zmm
	REAL, SAVE :: factor_xy
	REAL(8), SAVE :: factor_n   ! for horizontal mean profile
	
	! PUBLIC functions and subroutines
	PUBLIC :: wrfles_setup_in_idealexe
	PUBLIC :: wrfles_setperturb
	PUBLIC :: wrfles_setup_in_wrfexe
	PUBLIC :: wrfles_sfcforcing
	PUBLIC :: wrfles_sfcforcing_special
	PUBLIC :: wrfles_prepforcing
	PUBLIC :: wrfles_couple2mass
	PUBLIC :: wrfles_radforcing
	PUBLIC :: wrfles_lsforcing
	PUBLIC :: wrfles_subsidence
	PUBLIC :: wrfles_upperbound
	
CONTAINS

	!=====================================================================================
	
	SUBROUTINE wrfles_setup_in_idealexe
	
	! This subroutine is called in module_initialize_les.F
	
		IMPLICIT NONE
		
		PRINT*,'========== WRF-LES @ IDEAL.EXE =========='
		
		! Get casename
		OPEN(66, FILE='./CaseName', STATUS='old', FORM='formatted')
		READ(66,*) casename
		CLOSE(66)
		PRINT*,'casename: ', casename
		
		! Obtain domain information for future use (MPI, etc.)
		CALL task_init
		PRINT*,'nsubdomains: ', nsubdomains
		
		! Set some parameter for ideal.exe
		CALL read_prm
		
		PRINT*,'============================='
		
	END SUBROUTINE wrfles_setup_in_idealexe
	
	!=====================================================================================
	
	SUBROUTINE wrfles_setperturb( &
		! IN
		phb, ph, &
		! INOUT
		theta, qv, tke, &
		! INDICES (IN)
		its, ite, jts, jte, kts, kte ) ! They are actually ips, ipe, jps, jpe, 1, nzm.
		
	! This subroutine is called in module_initialize_les.F
		
		USE module_model_constants, ONLY: g
		
		IMPLICIT NONE
		
		! Input & output
		INTEGER, INTENT(IN) :: its, ite, jts, jte, kts, kte
		REAL, DIMENSION(its:ite,kts:kte,jts:jte), INTENT(IN) :: phb
		REAL, DIMENSION(its:ite,kts:kte,jts:jte), INTENT(IN) :: ph
		REAL, DIMENSION(its:ite,kts:kte,jts:jte), INTENT(INOUT) :: theta
		REAL, DIMENSION(its:ite,kts:kte,jts:jte), INTENT(INOUT) :: qv
		REAL, DIMENSION(its:ite,kts:kte,jts:jte), INTENT(INOUT) :: tke
		
		! Local
		INTEGER :: i, j, k
		REAL :: r01, rpm1
		REAL :: delt, delq, zperturb, znow
		
	!	PRINT*,'rank: ', rank
		
		IF ( perturb_type /= 1 ) THEN   ! TEMPORAL
		! Setup randum number
		j = rank * 30000   ! 30000 probably large enough to avoid same random numbers between ranks
		DO i = 1, j
			CALL random_number(r01)   ! returns random number between 0 and 1
		ENDDO
		ENDIF
		
		! Set perturbation
		SELECT CASE (perturb_type)
		CASE(0)   ! SAM's standard perturbation
			DO j = jts, jte
				DO k = kts, kte
					DO i = its, ite
						CALL random_number(r01)
						rpm1 = 1. - 2. * r01   ! convert r01 to a number between -1 and 1
						IF ( k <= 5 ) theta(i,k,j) = theta(i,k,j) + 0.02 * rpm1 * ( 6 - k )
						IF ( k <= 4 ) tke(i,k,j) = 0.04 * ( 5 - k )
					ENDDO
				ENDDO
			ENDDO
		
		CASE(1)   ! Perturbation for VOCALS RF06 used by H. Wang
			delt = 0.2
			delq = 0.05e-3
			zperturb = 600.
			DO j = jts, jte
				DO i = its, ite
					DO k = 1, kte-1
						znow = 0.5 * ( ph(i,k,j) + ph(i,k+1,j) + phb(i,k,j) + phb(i,k+1,j) ) / g
						IF ( znow <= zperturb ) THEN
							CALL random_number(r01)   ! random number between 0 and 1
							theta(i,k,j) = theta(i,k,j) + delt * ( r01 - 0.5 ) * 2.
							CALL random_number(r01)
							qv(i,k,j)= qv(i,k,j) + delq * ( r01 - 0.5 ) * 2.
						ENDIF
					ENDDO
				ENDDO
			ENDDO
		
		CASE(2)   ! DYCOMS-II RF02 (Ackerman et al. 2009)
			DO j = jts, jte
				DO k = kts, kte
					DO i = its, ite
						IF ( qv(i,k,j) >= 8.E-3 ) THEN   ! qv = 8 (g/kg) at the PBL top
							CALL random_number(r01)
							rpm1 = 1. - 2. * r01   ! convert r01 to a number between -1 and 1
							theta(i,k,j) = theta(i,k,j) + 0.1 * rpm1
							tke(i,k,j) = 1.
						ENDIF
					ENDDO
				ENDDO
			ENDDO
		
		CASE(3)   ! RICO (vanZanten et al. 2011)
			DO j = jts, jte
				DO k = kts, kte-1
					DO i = its, ite
						CALL random_number(r01)
						rpm1 = 1. - 2. * r01   ! convert r01 to a number between -1 and 1
						theta(i,k,j) = theta(i,k,j) + 0.1 * rpm1
						CALL random_number(r01)
						rpm1 = 1. - 2. * r01
						qv(i,k,j)= qv(i,k,j) + 2.5E-5 * rpm1
						znow = 0.5 * ( ph(i,k,j) + ph(i,k+1,j) + phb(i,k,j) + phb(i,k+1,j) ) / g
						tke(i,k,j) = 1. - znow / 4000.
					ENDDO
				ENDDO
			ENDDO
		
		CASE(999)
			PRINT*,'No initial perturbation.'
		CASE DEFAULT
			PRINT*,'perturb_type is not defined. Abort!'
			CALL task_abort()
		END SELECT
	
	END SUBROUTINE wrfles_setperturb
	
	!=====================================================================================
	
	SUBROUTINE wrfles_setup_in_wrfexe( grid )
	
	! This subroutine is called in module_wrf_top.F
	
		USE module_dm, ONLY: ntasks_x, ntasks_y
		USE module_domain, ONLY: domain, get_ijk_from_grid
		USE module_model_constants, ONLY: g
		
		IMPLICIT NONE
		
		! Input
		TYPE(domain), INTENT(IN) :: grid
		
		! Misc
		REAL :: tmp, dzs, dzh
		INTEGER :: ios, ierr, k, n, k1, k2, km1, k00, kp1, kp2, khunt
		
		PRINT*,'========== WRF-LES @ WRF.EXE =========='
		
		! Get casename
		OPEN(66, FILE='./CaseName', STATUS='old', FORM='formatted')
		READ(66,*) casename
		CLOSE(66)
		PRINT*,'casename: ', casename
		
		! Obtain domain information for future use (MPI, etc.)
		CALL task_init
		PRINT*,'nsubdomains: ', nsubdomains
		
		! Read prm
		CALL read_prm
		
		! Get grid index information
		CALL get_ijk_from_grid( grid, &
				ids, ide, jds, jde, kds, kde, &   ! domain
				ims, ime, jms, jme, kms, kme, &   ! subdomain+ghorst points
				ips, ipe, jps, jpe, kps, kpe )    ! subdomain
		! Even number subdomain in x (y) direction ==> ide (jde) has to be odd number
		! See detailed information at module_statistics.F
		IF ( nsubdomains > 1 .AND. MOD((ide-1)/(ipe-ips+1),2) == 0 .AND. MOD(ide,2) == 0 ) THEN
			PRINT*,'Set e_we odd number. Abort!'
			CALL task_abort()
		ENDIF
		IF ( nsubdomains > 1 .AND. MOD((jde-1)/(jpe-jps+1),2) == 0 .AND. MOD(jde,2) == 0 ) THEN
			PRINT*,'Set e_sn odd number. Abort!'
			CALL task_abort()
		ENDIF
		! Check if number of grid points in each direction is divisible without remainder
		IF ( nsubdomains > 1 .AND. MOD(ide-1,ntasks_x) /= 0 ) THEN
			PRINT*,'Set e_we-1 divisible without remainder by ntaskx_x(=nproc_x). Abort!'
			CALL task_abort()
		ENDIF
		IF ( nsubdomains > 1 .AND. MOD(jde-1,ntasks_y) /= 0 ) THEN
			PRINT*,'Set e_sn-1 divisible without remainder by ntasks_y(=nproc_y). Abort!'
			CALL task_abort()
		ENDIF
		
		!==================== VERY IMPORTANT: GRID INDEX INFORMATION ====================
		! WRF allocates one extra grid at ipe=ide and jpe=jde. Adjust by subtracting 1
		IF ( ipe == ide ) ipe = ipe - 1
		IF ( jpe == jde ) jpe = jpe - 1
		! Now ipe-ips & jpe-jps for all processrs are same
		nx = ipe - ips + 1
		ny = jpe - jps + 1
		! WRF allocates one extra layer for the u, v, & scalar so subtract 1
		nz  = kpe
		nzm = kpe - 1
		PRINT*,'nx, ny, nz, nzm: ', nx, ny, nz, nzm
		!==================================================================================
		
		
		! Initial hieght levels for future use
		! See the subroutine phy_prep in /dyn_em/module_big_step_utilities_em.F
		ALLOCATE( zs(nz) )
		DO k = 1, nzm
			zs(k) = 0.5 * ( grid%phb(ips,k,jps) + grid%phb(ips,k+1,jps) ) / g
		ENDDO
	!	PRINT*, 'k, zs:'
	!	DO k = nzm, 1, -1
	!		WRITE(*,'(I5,4F11.3,2f8.3)'), k, zs(k)
	!	ENDDO
		
		! High resolution vertical level for mean profile for nudging
		IF ( doupdategwnd .OR. donudging_uv .OR. donudging_tp .OR. donudging_qv .OR. donudging_qt &
			.OR. donudging_qnt ) THEN
			! zsh(1) = zs(1)
			! ...
			! zsh(1+1*hrlayers) = zs(2)
			! ...
			! zsh(1+2*hrlayers) = zs(3)
			! zsh(nzsh) = zs(nzm)
			nzsh = ( nzm - 1 ) * hrlayers + 1
			ALLOCATE( zsh(nzsh) )
			k2 = 0
			DO k = 1, nzm-1
				dzs = zs(k+1) - zs(k)
				dzh = dzs / REAL(hrlayers)
				DO k1 = 1, hrlayers
					k2 = k2 + 1
					zsh(k2) = zs(k) + REAL(k1-1) * dzh
				ENDDO
			ENDDO
			zsh(nzsh) = zs(nzm)
		!	DO k = 1, nzsh
		!		k1 = 1 + (k-1) / hrlayers
		!		WRITE(*,'(I5,2F11.3,I5)'), k, zsh(k), zs(k1), k1
		!	ENDDO
		ENDIF
		
		! Read surface data
		IF ( dospecifysst .OR. (.NOT.docompsfcflx) .OR. (.NOT.docompsfcust) ) THEN
			OPEN(66, FILE='./'//TRIM(casename)//'/sfc', STATUS='old', FORM='formatted')
			READ(66,*)   ! header
			nsfc = 0
			ios = 0
			DO WHILE ( ios == 0 )
				READ(66, IOSTAT=ios, FMT=*) tmp, tmp, tmp, tmp, tmp
				IF (ios /= 0) EXIT
				nsfc = nsfc + 1
			ENDDO
			ALLOCATE( daysfc(nsfc), sstsfc(nsfc), shfsfc(nsfc), lhfsfc(nsfc), ustsfc(nsfc) )
			REWIND(66)
			READ(66,*)   ! header
			DO n = 1, nsfc
				READ(66,*) daysfc(n), sstsfc(n), shfsfc(n), lhfsfc(n), ustsfc(n)
			ENDDO
			CLOSE(66)
			PRINT*, 'Surface forcing interval (days): ', daysfc(1), daysfc(nsfc)
		!	DO n = 1, nsfc   ! test
		!		WRITE(*,'(5F11.4)') daysfc(n), sstsfc(n), shfsfc(n), lhfsfc(n), ustsfc(n)
		!	ENDDO
		ENDIF
		
		! Read large scale forcing data
		IF ( doupdategwnd .OR. donudging_uv .OR. donudging_tp .OR. donudging_qv .OR. donudging_qt &
			.OR. doadvforcing .OR. dosubsidence .OR. doradforcing ) THEN
			OPEN(66, FILE='./'//TRIM(casename)//'/lsf', STATUS='old', FORM='formatted')
			READ(66,*)   ! header
			READ(66,*) nzlsf
			nlsf = 0
			ios = 0
			DO WHILE ( ios == 0 )
				READ(66, IOSTAT=ios, FMT=*) tmp
				IF (ios /= 0) EXIT
				DO k = 1, nzlsf
					READ(66,*) tmp, tmp, tmp, tmp, tmp, tmp, tmp, tmp, tmp
				ENDDO
				nlsf = nlsf + 1
			ENDDO
			ALLOCATE( daylsf(nlsf), zlsf(nzlsf), ulsf(nzlsf,nlsf), vlsf(nzlsf,nlsf), &
			          wlsf(nzlsf,nlsf), tlsf(nzlsf,nlsf), qlsf(nzlsf,nlsf), dtlsf(nzlsf,nlsf), &
			          dqlsf(nzlsf,nlsf), rlsf(nzlsf,nlsf) )
			REWIND(66)
			READ(66,*)   ! header
			READ(66,*)   ! nzlsf
			DO n = 1, nlsf
				READ(66,*) daylsf(n)
				DO k = 1, nzlsf
					READ(66,*) zlsf(k), tlsf(k,n), qlsf(k,n), ulsf(k,n), vlsf(k,n), wlsf(k,n), &
					           dtlsf(k,n), dqlsf(k,n), rlsf(k,n)
				ENDDO
			ENDDO
			CLOSE(66)
			PRINT*, 'Large-scale forcing interval (days): ', daylsf(1), daylsf(nlsf)
		!	DO n = 1, nlsf   ! test
		!		PRINT*, daylsf(n)
		!	!	DO k = 1, nzlsf
		!!			k = 1
		!			WRITE(*,'(I5,5F11.3,3E11.4)') k, zlsf(k), ulsf(k,n), vlsf(k,n), wlsf(k,n), &
		!			                             tlsf(k,n), qlsf(k,n), dtlsf(k,n), dqlsf(k,n), rlsf(k,n)
		!	!	ENDDO
		!	ENDDO
			qlsf(:,:) = qlsf(:,:) * 1.E-3   ! g/kg => kg/kg
		ENDIF
		
		! Allocate 3D arrays for interpolation
		! Local heights
		! Maybe (ips:ips+nx-1,1:nzm,jps:jps+ny-1) is OK.
		IF ( donudging_uv .OR. dosubsidence ) THEN
			ALLOCATE( zu_eta(ips:ipe,nzm,jps:jpe), zv_eta(ips:ipe,nzm,jps:jpe) )
		ENDIF
		IF ( donudging_uv .OR. donudging_tp .OR. donudging_qv .OR. donudging_qt .OR. doadvforcing &
			.OR. dosubsidence .OR. donudging_qnt ) THEN
			ALLOCATE( zs_eta(ips-1:ipe,nzm,jps-1:jpe) )   ! ips-1 for RHO@U, jps-1 for RHO@V
		ENDIF
		IF ( doadvforcing .OR. dosubsidence ) THEN
			ALLOCATE( km1zlsf(ips:ipe,nzm,jps:jpe), k00zlsf(ips:ipe,nzm,jps:jpe), &
			          kp1zlsf(ips:ipe,nzm,jps:jpe), kp2zlsf(ips:ipe,nzm,jps:jpe) )
		ENDIF
		
		! Prepare for nudging
		! nzsh > nzlsf?
		IF ( ( doupdategwnd .OR. donudging_uv .OR. donudging_tp .OR. donudging_qv .OR. donudging_qt &
			.OR. donudging_qnt ) .AND. ( nzsh < nzlsf ) ) THEN
			PRINT*,'nzsh has to be greater than or equal to nzlsf. Abort.'
			PRINT*,'Increase hrlayers in module_wrfles.F or modify lsf.'
			PRINT*,'nzsh, nzlsf:', nzsh, nzlsf
			CALL task_abort()
		ENDIF
		! Count nndg and nmnmx
		nndg = 0   ! number of variables for computing mean profile
		nmnmx = 0  ! number of variables for constructing min & max profiles
		IF ( donudging_uv )  nndg = nndg + 2   ! u & v
		IF ( donudging_tp )  nndg = nndg + 1
		IF ( donudging_qv .OR. donudging_qt ) nndg = nndg + 1 ! qv
		IF ( donudging_qt )  nndg = nndg + 1   ! qc
		IF ( donudging_qnt ) nndg = nndg + 1
		IF ( dondglimiter ) THEN
			IF ( donudging_qv .OR. donudging_qt ) nmnmx = nmnmx + 1 ! qv
			IF ( donudging_qt )  nmnmx = nmnmx + 1   ! qc
			IF ( donudging_qnt ) nmnmx = nmnmx + 2   ! qnn, qnc
		ENDIF
		PRINT*,'number of mean profile computed for large scale forcing:', nndg
		! Allocate high resolution 1D arrays
		! Interpolate soundings to zsh
		! Store interpolated sounding into the 1D array
		! U & V
		IF ( doupdategwnd .OR. donudging_uv ) THEN
			ALLOCATE( undg(nzsh,nlsf), vndg(nzsh,nlsf) )
			IF ( donudging_uv ) ALLOCATE( u0(nzsh), v0(nzsh) )
			DO n = 1, nlsf
				khunt = 0
				DO k = 1, nzsh
					CALL find_indices( zsh(k), zlsf, nzlsf, km1, k00, kp1, kp2, khunt )
					undg(k,n) = mono_cubic_interp( zsh(k), km1, k00, kp1, kp2, &
							zlsf(km1),   zlsf(k00),   zlsf(kp1),   zlsf(kp2), &
							ulsf(km1,n), ulsf(k00,n), ulsf(kp1,n), ulsf(kp2,n) )
					vndg(k,n) = mono_cubic_interp( zsh(k), km1, k00, kp1, kp2, &
							zlsf(km1),   zlsf(k00),   zlsf(kp1),   zlsf(kp2), &
							vlsf(km1,n), vlsf(k00,n), vlsf(kp1,n), vlsf(kp2,n) )
				ENDDO
			ENDDO
		ENDIF
		! TP
		IF ( donudging_tp ) THEN
			ALLOCATE( tndg(nzsh,nlsf), t0(nzsh) )
			DO n = 1, nlsf
				khunt = 0
				DO k = 1, nzsh
					CALL find_indices( zsh(k), zlsf, nzlsf, km1, k00, kp1, kp2, khunt )
					tndg(k,n) = mono_cubic_interp( zsh(k), km1, k00, kp1, kp2, &
							zlsf(km1),   zlsf(k00),   zlsf(kp1),   zlsf(kp2), &
							tlsf(km1,n), tlsf(k00,n), tlsf(kp1,n), tlsf(kp2,n) )
				ENDDO
			ENDDO
		ENDIF
		! QV or QT
		IF ( donudging_qv .OR. donudging_qt ) THEN
			ALLOCATE( qndg(nzsh,nlsf), qv0(nzsh) )
			IF ( donudging_qt ) ALLOCATE( qc0(nzsh) )
			DO n = 1, nlsf
				khunt = 0
				DO k = 1, nzsh
					CALL find_indices( zsh(k), zlsf, nzlsf, km1, k00, kp1, kp2, khunt )
					qndg(k,n) = mono_cubic_interp( zsh(k), km1, k00, kp1, kp2, &
							zlsf(km1),   zlsf(k00),   zlsf(kp1),   zlsf(kp2), &
							qlsf(km1,n), qlsf(k00,n), qlsf(kp1,n), qlsf(kp2,n) )
				ENDDO
			ENDDO
		ENDIF
		! Monotonic nudging stuff
		IF ( dondglimiter ) THEN
			! zmm for min & max profiles
			nzmm = ( nzm - 1 ) * hrlayers_zmm + 1
			ALLOCATE( zmm(nzmm) )
			k2 = 0
			DO k = 1, nzm-1
				dzs = zs(k+1) - zs(k)
				dzh = dzs / REAL(hrlayers_zmm)
				DO k1 = 1, hrlayers_zmm
					k2 = k2 + 1
					zmm(k2) = zs(k) + REAL(k1-1) * dzh
				ENDDO
			ENDDO
			zmm(nzmm) = zs(nzm)
			ALLOCATE( qv0min(nzmm), qv0max(nzmm) )
			IF ( donudging_qt ) ALLOCATE( qc0min(nzmm), qc0max(nzmm) )
		ENDIF
#ifdef ESRL
		! QNT
		IF ( donudging_qnt ) THEN
			ALLOCATE( qnt0(nzsh) )
			IF ( dondglimiter ) ALLOCATE( qnn0min(nzmm), qnn0max(nzmm), qnc0min(nzmm), qnc0max(nzmm) )
		ENDIF
#endif /*ESRL*/
		
		! For horizontal average
		factor_xy = 1. / REAL( nx * ny )
		factor_n  = 1. / DBLE( nsubdomains )
		
		! One could deallocate arrays which will not be used, but this is not processed for future
		! version, which may use these arrays.
		
		PRINT*,'============================='
		
	END SUBROUTINE wrfles_setup_in_wrfexe
	
	!=====================================================================================
	
	SUBROUTINE read_prm
	
	! Reads namelist from ./casename/prm
	
		IMPLICIT NONE
		
		NAMELIST /LESPRM/ &
			! Default parameters
			! Flags
			! SGS diffusion
			dosgsdif, &
			! Surface forcing
			dospecifysst, docompsfcflx, docompsfcust, dosfcspecial, &
			! Large scale forcing
			doupdategwnd, dosubsidence, doadvforcing, doradforcing, donudging_uv, donudging_tq, &
			donudging_tp, donudging_qv, donudging_qt, dondglimiter, &
			! Other
			doupperbound, &
			! Misc.
			perturb_type, sfcspecial_type, surface_type, coriolis, latitude, longitude, &
			tau_ndg, tau_ndguv, ug, vg, &
			
			! Special flag for the Feingold scheme
			donudging_qnt, &
			
			! Parameters for module_mp_feingold2m.F
			xindex_feingold2m, nccn_feingold2m, dg_ccn_feingold2m, sg_ccn_feingold2m, &
			bimodal_feingold2m, nccn2_feingold2m, dg2_ccn_feingold2m, sg2_ccn_feingold2m, &
			sg_cloud_feingold2m, sg_rain_feingold2m, &
			drzst_feingold2m, drzflg_feingold2m, colflg_feingold2m, &
			srcccn_add_feingold2m, srcccn_startstep_feingold2m, srcccn_starttime_feingold2m, &
			srcccn_feingold2m, srcccn_qvmin_feingold2m
		
		dosgsdif     = .TRUE.
		dospecifysst = .FALSE.
		docompsfcflx = .FALSE.
		docompsfcust = .FALSE.
		dosfcspecial = .FALSE.
		doupdategwnd = .FALSE.
		dosubsidence = .FALSE.
		doadvforcing = .FALSE.
		doradforcing = .FALSE.
		donudging_uv = .FALSE.
		donudging_tq = .FALSE.
		donudging_tp = .FALSE.
		donudging_qv = .FALSE.
		donudging_qt = .FALSE.
		dondglimiter = .FALSE.
		doupperbound = .FALSE.
		perturb_type = 0
		sfcspecial_type = 0
		surface_type = 2   ! ocean
		coriolis  = 0.
		latitude  = 0.
		longitude = 0.
		tau_ndg   = -999.
		tau_ndguv = -999.
		ug = 0.
		vg = 0.
		
		! Special flag for the Feingold scheme
		donudging_qnt = .FALSE.
		
		! Parameters for module_mp_feingold2m.F
		xindex_feingold2m = 1
		nccn_feingold2m   = 150.
		dg_ccn_feingold2m = 0.2e-4
		sg_ccn_feingold2m = 1.5
		bimodal_feingold2m = .FALSE.
		nccn2_feingold2m   = 150.
		dg2_ccn_feingold2m = 0.2e-4
		sg2_ccn_feingold2m = 1.5
		sg_cloud_feingold2m = 1.2
		sg_rain_feingold2m  = 1.2
		drzst_feingold2m  = 0.
		drzflg_feingold2m = 1
		colflg_feingold2m = .TRUE.
		srcccn_add_feingold2m = .FALSE.
		srcccn_startstep_feingold2m = 0
		srcccn_starttime_feingold2m = 0
		srcccn_feingold2m = 0.
		srcccn_qvmin_feingold2m = 0.
		
		OPEN(66, FILE='./'//TRIM(casename)//'/prm', STATUS='old', FORM='formatted')
		READ(66, LESPRM)
		CLOSE(66)
		PRINT*,'dosgsdif: ', dosgsdif
		PRINT*,'dospecifysst: ', dospecifysst
		PRINT*,'docompsfcflx: ', docompsfcflx
		PRINT*,'docompsfcust: ', docompsfcust
		PRINT*,'dosfcspecial: ', dosfcspecial
		PRINT*,'doupdategwnd: ', doupdategwnd
		PRINT*,'dosubsidence: ', dosubsidence
		PRINT*,'doadvforcing: ', doadvforcing
		PRINT*,'doradforcing: ', doradforcing
		PRINT*,'donudging_uv: ', donudging_uv
		PRINT*,'donudging_tq: ', donudging_tq
		PRINT*,'donudging_tp: ', donudging_tp
		PRINT*,'donudging_qv: ', donudging_qv
		PRINT*,'donudging_qt: ', donudging_qt
		PRINT*,'dondglimiter: ', dondglimiter
		PRINT*,'doupperbound: ', doupperbound
		PRINT*,'perturb_type: ', perturb_type
		PRINT*,'sfcspecial_type: ', sfcspecial_type
		PRINT*,'surface_type: ', surface_type
		PRINT*,'coriolis: ', coriolis
		PRINT*,'latitude: ', latitude
		PRINT*,'longitude: ', longitude
		PRINT*,'tau_ndg: ', tau_ndg
		PRINT*,'tau_ndguv: ', tau_ndguv
		PRINT*,'ug: ', ug
		PRINT*,'vg: ', vg
#ifdef ESRL
		PRINT*,'Special flags for Feingold2m'
		PRINT*,'donudging_qnt: ', donudging_qnt
		PRINT*,'Feingold2m parameters'
		PRINT*,'xindex_feingold2m: ', xindex_feingold2m
		PRINT*,'nccn_feingold2m: ', nccn_feingold2m
		PRINT*,'dg_ccn_feingold2m: ', dg_ccn_feingold2m
		PRINT*,'sg_ccn_feingold2m: ', sg_ccn_feingold2m
		PRINT*,'bimodal_feingold2m: ', bimodal_feingold2m
		PRINT*,'nccn2_feingold2m: ', nccn2_feingold2m
		PRINT*,'dg2_ccn_feingold2m: ', dg2_ccn_feingold2m
		PRINT*,'sg2_ccn_feingold2m: ', sg2_ccn_feingold2m
		PRINT*,'sg_cloud_feingold2m: ', sg_cloud_feingold2m
		PRINT*,'sg_rain_feingold2m: ',  sg_rain_feingold2m
		PRINT*,'drzst_feingold2m: ',  drzst_feingold2m
		PRINT*,'drzflg_feingold2m: ', drzflg_feingold2m
		PRINT*,'colflg_feingold2m: ', colflg_feingold2m
		PRINT*,'srcccn_add_feingold2m: ', srcccn_add_feingold2m
		PRINT*,'srcccn_starttime_feingold2m: ', srcccn_starttime_feingold2m
	!!	PRINT*,'srcccn_startstep_feingold2m: ', srcccn_startstep_feingold2m
		PRINT*,'srcccn_feingold2m: ', srcccn_feingold2m
		PRINT*,'srcccn_qvmin_feingold2m: ', srcccn_qvmin_feingold2m
#endif /*ESRL*/
		
		! Adjust flag for nudging
		IF ( donudging_qv .AND. donudging_qt ) THEN
			PRINT*,'Set donudging_qv = .FALSE. or donudging_qt = .FALSE. Abort!'
			CALL task_abort()
		ENDIF
		IF ( donudging_tp .AND. ( donudging_qv .OR. donudging_qt ) ) donudging_tq = .TRUE.
		IF ( donudging_tq ) donudging_tp = .TRUE.
		IF ( donudging_tq .AND. (.NOT.donudging_qt) ) donudging_qv = .TRUE.
		IF ( donudging_tq .AND. (.NOT.donudging_qv) ) donudging_qt = .TRUE.
		
		! Check flag for monotonic nudging limiter
		IF ( (.NOT.donudging_qv) .AND. (.NOT.donudging_qt) .AND. (.NOT.donudging_qnt) ) &
			dondglimiter = .FALSE.
		
		! Adjust flag for updating geostrophic wind
		IF ( donudging_uv ) doupdategwnd = .TRUE.
		
	!	PRINT*,'-----Reset nudging flags based on input-----'
	!	PRINT*,'doupdategwnd: ', doupdategwnd
	!	PRINT*,'donudging_tq: ', donudging_tq
	!	PRINT*,'donudging_tp: ', donudging_tp
	!	PRINT*,'donudging_qv: ', donudging_qv
	!	PRINT*,'donudging_qt: ', donudging_qt
	!	PRINT*,'dondglimiter: ', dondglimiter
		
		! Inverse of nudging time scale
		IF ( donudging_uv .OR. donudging_tp .OR. donudging_qv .OR. donudging_qt &
			.OR. donudging_qnt ) THEN
			IF ( tau_ndg > 0. ) THEN
				itau_ndg = 1. / tau_ndg
			ELSE
				PRINT*,'Set tau_ndg > 0. Abort!'
				CALL task_abort()
			ENDIF
			IF ( tau_ndguv <= 0. ) tau_ndguv = tau_ndg
			itau_ndguv = 1. / tau_ndguv
		ENDIF
		
		! Convert the unit of srcccn_feingold2m from #/mg/hour to #/mg/second
		! Convert the unit of srcccn_qvmin_feingold2m from g/kg to kg/kg
		srcccn_feingold2m = srcccn_feingold2m / 3600.
		srcccn_qvmin_feingold2m = srcccn_qvmin_feingold2m * 1.E-3
		
	END SUBROUTINE read_prm
		
	!=====================================================================================
	
	SUBROUTINE wrfles_sfcforcing( dt, nstep, ust, shf, lhf, sst )
	
	! Returns ust, shf, lhf, SST by linear time interpolation
	! This subroutine is called in /path2wrf/phys/module_surface_driver.F
	
		IMPLICIT NONE
		
		! Input
		INTEGER, INTENT(IN) :: nstep
		REAL, INTENT(IN) :: dt
		
		! Output
		REAL, INTENT(OUT) :: ust
		REAL, INTENT(OUT) :: shf
		REAL, INTENT(OUT) :: lhf
		REAL, INTENT(OUT) :: sst
		
		! Local
		REAL :: day
		INTEGER :: n
		
		IF ( dospecifysst .OR. (.NOT.docompsfcflx) .OR. (.NOT.docompsfcust) ) THEN
			day = REAL(nstep) * dt / 86400.
			DO n = 2, nsfc
				IF ( day < daysfc(n) ) THEN   ! If day >= daysfc(nsfc), n1_sfc=nsfc-1 & n2_sfc=nsfc
					n1_sfc = n - 1
					n2_sfc = n
					EXIT
				ENDIF
			ENDDO
		!	PRINT*,'wrfles_sfcforcing: n1_sfc, n2_sfc = ', n1_sfc, n2_sfc
			coef_sfc = ( day - daysfc(n1_sfc) ) / ( daysfc(n2_sfc) - daysfc(n1_sfc) )
			IF ( dospecifysst ) THEN
				sst = ( 1. - coef_sfc ) * sstsfc(n1_sfc) + coef_sfc * sstsfc(n2_sfc)
			ENDIF
			IF ( .NOT.docompsfcust ) THEN
				ust = ( 1. - coef_sfc ) * ustsfc(n1_sfc) + coef_sfc * ustsfc(n2_sfc)
			ENDIF
			IF ( .NOT.docompsfcflx ) THEN
				shf = ( 1. - coef_sfc ) * shfsfc(n1_sfc) + coef_sfc * shfsfc(n2_sfc)
				lhf = ( 1. - coef_sfc ) * lhfsfc(n1_sfc) + coef_sfc * lhfsfc(n2_sfc)
			ENDIF
		ENDIF
		
	END SUBROUTINE wrfles_sfcforcing
	
	!=====================================================================================
	
	SUBROUTINE wrfles_sfcforcing_special( &
		! IN
		u, v, tp, qv, z, rho, exner, tsfc, psfc, cp, xlv, &
		! INOUT
		ust, ustm, hfx, qfx, lh, &
		! INDICES
		its, ite, jts, jte, kts, kte )
		
	! Special surface fluxes specified by the particular case, which requires modification of code.
	! Use this option if sufrace fluxes and ustar are not specified in the sfc file and/or not
	! computed with the WRF surface layer code (module_sf_sfclay.F).
		
		IMPLICIT NONE
		
		! Input
		INTEGER, INTENT(IN) :: its, ite, jts, jte, kts, kte
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: u, v   ! u and v wind (m/s)
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: tp ! potential temperature (K)
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: qv ! water vapor mixing ratio (kg/kg)
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: z  ! scalar level (m)
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: rho ! air density
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: exner ! exner function
		REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: tsfc   ! surface absolut temperature (K)
		REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: psfc   ! surface pressure (Pa)
		REAL, INTENT(IN) :: cp, xlv
		
		! Input & Output
		REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: ust, ustm, hfx, qfx, lh
		
		! Local
		INTEGER :: i, j
		REAL :: cm, ch, cq, z0, sf, ws   ! Used for RICO
		
		SELECT CASE (sfcspecial_type)
		CASE(0)
			PRINT*,'No surface forcing.'
			
		CASE(1)   ! RICO
			cm = 0.001229   ! momentum transfer coefficinets at 20 m
			ch = 0.001094   ! heat
			cq = 0.001133   ! moisture
			z0 = 0.00015    ! roughness height (m)
			DO j = jts, jte
				DO i = its, ite
					! compute scale factor for transfer coeffieients for the lowest scalar level
					! as described in the ATEX case (Stevens et al. 2001 JAS)
					sf = ( LOG(20./z0) / LOG(z(i,kts,j)/z0) ) ** 2
					
					! wind speed
					ws = MAX( SQRT( u(i,kts,j)**2 + v(i,kts,j)**2 ), 0.01 )
					
					! friction velocity (ustar)
					ust(i,j) = SQRT( cm * sf ) * ws
					ustm(i,j) = ust(i,j)   ! this is used for LES mode
					
					! sensible heat flux with theta (W/m2)
					! Like module_sf_sfclay.F, cpm is used to convert to W/m2, instead od cp. Here
					! cpm=cp*(1+0.8qv), which is cp for moist air, and this will be used to convert to
					! K/s in module_diffusion_em.F
					! NOTE: IF DOCOMPSFCFLX=FALSE, THE SPECIFIED HFX IS ASSUMED TO BE ALREADY CONSIDERED
					!       WITH CPM. HOWEVER ADJUSTMENT IS PROBABLY UNNECESSARY SINCE CP IS VERY CLOSE
					!       TO CPM (E.G., FOR QV=20 G/KG, 0.8*0.02=0.016, AND 0.016*CP << CP).
					hfx(i,j) = - ch * sf * ws * ( tp(i,kts,j) - 298.5 ) & !299.8 / exner(i,kts,j) ) &
					         * cp * ( 1. + 0.8 * qv(i,kts,j) ) * rho(i,kts,j)
					
					! latent heat flux (kg/m3 m/s for qfx, W/m2 for lh)
					qfx(i,j) = - cq * sf * ws * ( qv(i,kts,j) - 0.0214 ) * rho(i,kts,j)
					lh(i,j) = qfx(i,j) * xlv
				ENDDO
			ENDDO
			
		CASE DEFAULT
			PRINT*,'sfcspecial_type is not defined. Abort!'
			CALL task_abort()
		END SELECT
		
	END SUBROUTINE wrfles_sfcforcing_special
	
	!=====================================================================================
	
	SUBROUTINE wrfles_prepforcing( &
#ifdef ESRL
		! IN
		dt, nstep, n_moist, phb, ph, alb, al, moist, u, v, tp, qnn, qnc )
#else
		! IN
		dt, nstep, n_moist, phb, ph, alb, al, moist, u, v, tp )
#endif /*ESRL*/
	
	! Prepare forcing
	! 1: find coef_lsf, n1_lsf, n2_lsf for large-scale forcing and radiative forcing
	! 2: compute mean profile for nudging or surface momentum flux
	! This subroutine is called in /path2wrf/dyn_em/module_first_rk_step_part1.F
	!
	! OMP option not yet
	! This code does not work with nested domains, whose vertical level is different from parent
	! domain.
	!
	! Flow chart for the physics tendency calculation in solve_em.F
	! solve_em.F
	!	HALO_EM_A <- dyn_em 8: ru, rv, rw, ww, php, alt, al, p, muu, muv, mut
	!	PERIOD_BDY_EM_A <- dyn_em 2: ru, rv, rw, ww, php, alt, p, muu, muv, mut, ph_2, al
	!	IF (rk_step == 1)
	!		CALL first_rk_step_part1 (in dyn_em/module_firts_rk_step_part1.F)
	!			CALL init_zero_tendency (in dyn_em/module_em.F)
	!				<-- initialize u_tend=0, v_tend=0, t_tend=0, q_tend=0, s_tend=0
	!			HALO_EM_PHYS_A <- dyn_em 4: u_2, v_2
	!			CALL phy_prep (in dyn_em/module_big_step_utilities_em.F)
	!			CALL wrfles_prepforcing <-- current subroutine
	!			CALL wrfles_lsforcing <--add tendency to u_tend, v_tend, t_tend, q_tend.
	!			CALL wrfles_subsidence <--add tendency to u_tend, v_tend, t_tend, q_tend, s_tend.
	!			CALL wrfles_couple2mass <--multiply by the column mass (mu, muu, muv)
	!			CALL radiation_driver (in phys/module_radiation_driver.F) <-- compute radiative tend
	!			CALL surface_driver (in phys/module_surface_driver.F) <-- compute surface tend
	!			Calls other drivers, which are not used for LES
	!			Note: The driver subroutines do not use the tendency arrays (u_tend, etc.)
	!		CAL first_rk_step_part2.F (in dyn_em/module_first_rk_step_part2.F)
	!			CALL calculate_phy_tend (in dyn_em/module_em.F)
	!				<-- couples the physics tendencies from the driver subroutines to the column mass
	!			HALO_EM_TKE_C <- dyn_em 8: u_2, v_2, z, zx, zy, rdz, rdzw, ustm
	!			PERIOD_BDY_EM_A1 <- dyn_em 3: rdzw, rdz, z, zx, zy, ustm
	!			Calls subroutines, PERIODs, HALOs, which prepare diffusion calculation
	!			CALL update_phy_ten (in pnys/module_physics_addtendc.F)
	!				<-- add all tendencies from driver subroutines to u_tend, v_tend, t_tend, q_tend,
	!				    s_tend.
	!			Calls subroutines, which update tendency arrays with diffusion.
	!	ENDIF
	!	Advection for u, v, w, tp
	!	Acoustic loop
	!	Update u, v, w, tp, phi, mud
	!	Advection then update for moist, tke, scalar
	!	One time update for microphysics after the RK loop
	! END solve_em.F
	
		USE module_model_constants, ONLY: g
		USE module_state_description, ONLY: p_qv, p_qc
		
		IMPLICIT NONE
		
		! Input
		REAL, INTENT(IN) :: dt
		INTEGER, INTENT(IN) :: nstep
		INTEGER, INTENT(IN) :: n_moist
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: phb
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: ph
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: alb
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: al
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: u
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: v
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: tp
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme,n_moist), INTENT(IN) :: moist
#ifdef ESRL
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: qnn   ! CCN number concentration
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: qnc   ! droplet number concentration
#endif /*ESRL*/
		
		! Local
		LOGICAL :: doprepforcing
		REAL :: day
		INTEGER, DIMENSION(ips:ipe,nzsh,jps:jpe) :: km1zsh, k00zsh, kp1zsh, kp2zsh
		INTEGER, DIMENSION(ips:ipe,nzmm,jps:jpe) :: km1zmm, k00zmm, kp1zmm, kp2zmm
		REAL, DIMENSION(ips-1:ipe,nzsh,jps-1:jpe) :: rho   ! density at zsh
		REAL, DIMENSION(ips-1:ipe,nzm,jps-1:jpe) :: qm   ! summation of moist variables
		REAL, DIMENSION(ips-1:ipe,nzm) :: rho2d   ! rho at local point
		REAL, DIMENSION(nzsh) :: rho0    ! mean profile at zsh
		REAL, DIMENSION(ips:ipe,nzmm,jps:jpe) :: f   ! temporal array for min & max
#ifdef ESRL
		REAL, DIMENSION(ips:ipe,nzm) :: qnt   ! qnn+qnc
#endif /*ESRL*/
		REAL(8), DIMENSION(nzsh*(nndg+1)) :: buf1, buf2     ! MPI buffer for mean, +1 for rho
		REAL, DIMENSION(nzmm*nmnmx) :: buf1mn, buf2mn, buf1mx, buf2mx   ! MPI buffer for min & max
		INTEGER :: km1, k00, kp1, kp2, khunt, khunt1, khunt2
		INTEGER :: i, j, k, n
		INTEGER :: indg    ! counter for buf1 and buf2
		INTEGER :: imnmx   ! counter for buf1mn, buf2mn, buf1mx, and buf2mx
		
		doprepforcing = doupdategwnd .OR. donudging_uv .OR. donudging_tp .OR. donudging_qv &
		            .OR. donudging_qt .OR. doadvforcing .OR. dosubsidence .OR. doradforcing
		
		IF ( .NOT.doprepforcing ) THEN
			RETURN   ! Nothing to be done here.
		ELSE
			day = REAL(nstep) * dt / 86400.
			DO n = 2, nlsf
				IF ( day < daylsf(n) ) THEN   ! If day >= daylsf(nlsf), n1_lsf=nlsf-1 & n2_lsf=nlsf
					n1_lsf = n - 1
					n2_lsf = n
					EXIT
				ENDIF
			ENDDO
			coef_lsf = ( day - daylsf(n1_lsf) ) / ( daylsf(n2_lsf) - daylsf(n1_lsf) )
		!	PRINT*,'wrfles_prepforcing: n1_lsf, n2_lsf = ', n1_lsf, n2_lsf
		ENDIF
		
		! ZU_ETA & ZV_ETA
		IF ( donudging_uv .OR. dosubsidence ) THEN
			! Local height
			DO j = jps, jpe
				DO k = 1, nzm
					DO i = ips, ipe
						! U
						zu_eta(i,k,j) = 0.25 / g &
						              * ( phb(i-1,k+1,j) + ph(i-1,k+1,j) + phb(i,k+1,j) + ph(i,k+1,j) &
						                + phb(i-1,k,j) + ph(i-1,k,j) + phb(i,k,j) + ph(i,k,j) )
						! V
						zv_eta(i,k,j) = 0.25 / g &
						              * ( phb(i,k+1,j-1) + ph(i,k+1,j-1) + phb(i,k+1,j) + ph(i,k+1,j) &
						                + phb(i,k,j-1) + ph(i,k,j-1) + phb(i,k,j) + ph(i,k,j) )
					ENDDO
				ENDDO
			ENDDO
		ENDIF
		
		! ZS_ETA
		IF ( donudging_uv .OR. donudging_tp .OR. donudging_qv .OR. donudging_qt .OR. doadvforcing &
			.OR. dosubsidence .OR. donudging_qnt ) THEN
			DO j = jps-1, jpe   ! jps-1 for RHO@V
				DO k = 1, nzm
					DO i = ips-1, ipe   ! ips-1 for RHO@U
						zs_eta(i,k,j) = 0.5 / g * ( phb(i,k+1,j) + ph(i,k+1,j) + phb(i,k,j) + ph(i,k,j) )
					ENDDO
				ENDDO
			ENDDO
		ENDIF
		! Interpolation index from ZLSF to ZS_ETA
		IF ( doadvforcing .OR. dosubsidence ) THEN
			DO j = jps, jpe
				DO i = ips, ipe
					khunt = 0
					DO k = 1, nzm
						CALL find_indices( zs_eta(i,k,j), zlsf, nzlsf, &
								km1zlsf(i,k,j), k00zlsf(i,k,j), kp1zlsf(i,k,j), kp2zlsf(i,k,j), khunt )
					ENDDO
				ENDDO
			ENDDO
		ENDIF
		
		
		
		!========== Mean profiles for nudging and geostrophic wind update ==========
		! Reset counter for MPI
		indg = 0
		
		! Compute mean profiles on each processor
		! RHO for mass-weighted average
		! Interpolation indices to ZSH for TP, QV, QT, QNT
		IF ( donudging_uv .OR. donudging_tp .OR. donudging_qv .OR. donudging_qt &
			.OR. donudging_qnt ) THEN
			! Summation of moist variables for density calculation
			qm(:,:,:) = 0.
			DO n = 1, n_moist
				DO j = jps-1, jpe   ! jps-1 for RHO@V
					DO k = 1, nzm
						DO i = ips-1, ipe   ! ips-1 for RHO@U
							qm(i,k,j) = qm(i,k,j) + moist(i,k,j,n)
						ENDDO
					ENDDO
				ENDDO
			ENDDO
			! RHO and interpolation indices
			rho0(:) = 0.
			DO j = jps-1, jpe
				DO k = 1, nzm
					DO i = ips-1, ipe
						rho2d(i,k) = ( 1. + qm(i,k,j) ) / ( alb(i,k,j) + al(i,k,j) )
					ENDDO
				ENDDO
				DO i = ips-1, ipe
					khunt = 0
					DO k = 1, nzsh
						CALL find_indices( zsh(k), zs_eta(i,1:nzm,j), nzm, km1, k00, kp1, kp2, khunt )
						rho(i,k,j) = mono_cubic_interp( zsh(k), km1, k00, kp1, kp2, &
								zs_eta(i,km1,j), zs_eta(i,k00,j), zs_eta(i,kp1,j), zs_eta(i,kp2,j), &
								rho2d(i,km1),    rho2d(i,k00),    rho2d(i,kp1),    rho2d(i,kp2) )
						IF ( j >= jps .AND. i >= ips ) THEN
							km1zsh(i,k,j) = km1
							k00zsh(i,k,j) = k00
							kp1zsh(i,k,j) = kp1
							kp2zsh(i,k,j) = kp2
							rho0(k) = rho0(k) + rho(i,k,j)
						ENDIF
					ENDDO
				ENDDO
			ENDDO
			! Average, then store it in buf1 for MPI
			rho0(:) = rho0(:) * factor_xy
			IF ( dompi ) THEN
				buf1(1+indg*nzsh:(indg+1)*nzsh) = rho0(1:nzsh)
				indg = indg + 1
			ENDIF
		ENDIF
		
		! U & V
		IF ( donudging_uv ) THEN
			u0(:) = 0.
			v0(:) = 0.
			DO j = jps, jpe
				DO i = ips, ipe
					khunt1 = 0
					khunt2 = 0
					DO k = 1, nzsh
						! U
						CALL find_indices( zsh(k), zu_eta(i,1:nzm,j), nzm, km1, k00, kp1, kp2, khunt1 )
						u0(k) = u0(k) + mono_cubic_interp( zsh(k), km1, k00, kp1, kp2, &
								zu_eta(i,km1,j), zu_eta(i,k00,j), zu_eta(i,kp1,j), zu_eta(i,kp2,j), &
								u(i,km1,j),      u(i,k00,j),      u(i,kp1,j),      u(i,kp2,j) ) &
								* 0.5 * ( rho(i-1,k,j) + rho(i,k,j) )
						! V
						CALL find_indices( zsh(k), zv_eta(i,1:nzm,j), nzm, km1, k00, kp1, kp2, khunt2 )
						v0(k) = v0(k) + mono_cubic_interp( zsh(k), km1, k00, kp1, kp2, &
								zv_eta(i,km1,j), zv_eta(i,k00,j), zv_eta(i,kp1,j), zv_eta(i,kp2,j), &
								v(i,km1,j),      v(i,k00,j),      v(i,kp1,j),      v(i,kp2,j) ) &
								* 0.5 * ( rho(i,k,j-1) + rho(i,k,j) )
					ENDDO
				ENDDO
			ENDDO
			u0(:) = u0(:) * factor_xy
			v0(:) = v0(:) * factor_xy
			IF ( dompi ) THEN
				! Store summation for MPI run
				buf1( 1+indg*nzsh:(indg+1)*nzsh ) = u0(1:nzsh)
				indg = indg + 1
				buf1( 1+indg*nzsh:(indg+1)*nzsh ) = v0(1:nzsh)
				indg = indg + 1
			ENDIF
		ENDIF
		
		! TP
		IF ( donudging_tp ) THEN
			t0(:) = 0.
			DO j = jps, jpe
				DO k = 1, nzsh
					DO i = ips, ipe
						km1 = km1zsh(i,k,j)
						k00 = k00zsh(i,k,j)
						kp1 = kp1zsh(i,k,j)
						kp2 = kp2zsh(i,k,j)
						t0(k) = t0(k) + mono_cubic_interp( zsh(k), km1, k00, kp1, kp2, &
								zs_eta(i,km1,j), zs_eta(i,k00,j), zs_eta(i,kp1,j), zs_eta(i,kp2,j), &
								tp(i,km1,j),     tp(i,k00,j),     tp(i,kp1,j),     tp(i,kp2,j) ) &
								* rho(i,k,j)
					ENDDO
				ENDDO
			ENDDO
			t0(:) = t0(:) * factor_xy
			IF ( dompi ) THEN
				buf1( 1+indg*nzsh:(indg+1)*nzsh ) = t0(1:nzsh)
				indg = indg + 1
			ENDIF
		ENDIF
		
		! QV
	!	IF ( donudging_q ) THEN
		IF ( donudging_qv .OR. donudging_qt ) THEN
			qv0(:) = 0
			DO j = jps, jpe
				DO k = 1, nzsh
					DO i = ips, ipe
						km1 = km1zsh(i,k,j)
						k00 = k00zsh(i,k,j)
						kp1 = kp1zsh(i,k,j)
						kp2 = kp2zsh(i,k,j)
						qv0(k) = qv0(k) + mono_cubic_interp( zsh(k), km1, k00, kp1, kp2, &
								zs_eta(i,km1,j), zs_eta(i,k00,j), zs_eta(i,kp1,j), zs_eta(i,kp2,j), &
								moist(i,km1,j,p_qv), moist(i,k00,j,p_qv), &
								moist(i,kp1,j,p_qv), moist(i,kp2,j,p_qv) ) * rho(i,k,j)
					ENDDO
				ENDDO
			ENDDO
			qv0(:) = qv0(:) * factor_xy
			IF ( dompi ) THEN
				buf1( 1+indg*nzsh:(indg+1)*nzsh ) = qv0(1:nzsh)
				indg = indg + 1
			ENDIF
		ENDIF
		
		! QC
		IF ( donudging_qt ) THEN
			qc0(:) = 0
			DO j = jps, jpe
				DO k = 1, nzsh
					DO i = ips, ipe
						km1 = km1zsh(i,k,j)
						k00 = k00zsh(i,k,j)
						kp1 = kp1zsh(i,k,j)
						kp2 = kp2zsh(i,k,j)
						! Should qc be limited smaller than a threshold?
						qc0(k) = qc0(k) + mono_cubic_interp( zsh(k), km1, k00, kp1, kp2, &
								zs_eta(i,km1,j), zs_eta(i,k00,j), zs_eta(i,kp1,j), zs_eta(i,kp2,j), &
								moist(i,km1,j,p_qc), moist(i,k00,j,p_qc), &
								moist(i,kp1,j,p_qc), moist(i,kp2,j,p_qc) ) * rho(i,k,j)
					ENDDO
				ENDDO
			ENDDO
			qc0(:) = qc0(:) * factor_xy
			IF ( dompi ) THEN
				buf1( 1+indg*nzsh:(indg+1)*nzsh ) = qc0(1:nzsh)
				indg = indg + 1
			ENDIF
		ENDIF
		
#ifdef ESRL
		! QNT = total number concentration = CCN + droplet number
		! Feingold scheme has not initiated with nstep=1, so do not add any nudging tendency.
		IF ( donudging_qnt .AND. nstep > 1 ) THEN
			qnt0(:) = 0.
			DO j = jps, jpe
				DO k = 1, nzm
					DO i = ips, ipe
						qnt(i,k) = qnn(i,k,j) + qnc(i,k,j)
					ENDDO
				ENDDO
				DO k = 1, nzsh
					DO i = ips, ipe
						km1 = km1zsh(i,k,j)
						k00 = k00zsh(i,k,j)
						kp1 = kp1zsh(i,k,j)
						kp2 = kp2zsh(i,k,j)
						qnt0(k) = qnt0(k) + mono_cubic_interp( zsh(k), km1, k00, kp1, kp2, &
								zs_eta(i,km1,j), zs_eta(i,k00,j), zs_eta(i,kp1,j), zs_eta(i,kp2,j), &
								qnt(i,km1),      qnt(i,k00),      qnt(i,kp1),      qnt(i,kp2) ) &
								* rho(i,k,j)
					ENDDO
				ENDDO
			ENDDO
			qnt0(:) = qnt0(:) * factor_xy
			IF ( dompi ) THEN
				buf1( 1+indg*nzsh:(indg+1)*nzsh ) = qnt0(1:nzsh)
				indg = indg + 1
			ENDIF
		ENDIF
#endif /*ESRL*/
		
		! Compute mean over whole processors with MPI
		IF ( dompi .AND. nndg > 0 ) THEN
			CALL task_sum_real8( buf1, buf2, nzsh*(nndg+1) )   ! +1 for rho0
			buf2(:) = buf2(:) * factor_n
			indg = 0
			rho0(1:nzsh) = buf2( 1+indg*nzsh:(indg+1)*nzsh )
			indg = indg + 1
			IF ( donudging_uv ) THEN
				u0(1:nzsh) = buf2( 1+indg*nzsh:(indg+1)*nzsh )
				indg = indg + 1
				v0(1:nzsh) = buf2( 1+indg*nzsh:(indg+1)*nzsh )
				indg = indg + 1
			ENDIF
			IF ( donudging_tp ) THEN
				t0(1:nzsh) = buf2( 1+indg*nzsh:(indg+1)*nzsh )
				indg = indg + 1
			ENDIF
			IF ( donudging_qv .OR. donudging_qt ) THEN
				qv0(1:nzsh) = buf2( 1+indg*nzsh:(indg+1)*nzsh )
				indg = indg + 1
			ENDIF
			IF ( donudging_qt ) THEN
				qc0(1:nzsh) = buf2( 1+indg*nzsh:(indg+1)*nzsh )
				indg = indg + 1
			ENDIF
#ifdef ESRL
			IF ( donudging_qnt .AND. nstep > 1 ) THEN
				qnt0(1:nzsh) = buf2( 1+indg*nzsh:(indg+1)*nzsh )
				indg = indg + 1
			ENDIF
#endif /*ESRL*/
		ENDIF
		
		! Mass-weighted mean profile
		IF ( donudging_uv ) THEN
			u0(:) = u0(:) / rho0(:)
			v0(:) = v0(:) / rho0(:)
		ENDIF
		IF ( donudging_tp ) t0(:)  = t0(:)  / rho0(:)
		IF ( donudging_qv .OR. donudging_qt ) qv0(:) = qv0(:) / rho0(:)
		IF ( donudging_qt ) qc0(:) = qc0(:) / rho0(:)
#ifdef ESRL
		IF ( donudging_qnt .AND. nstep > 1 ) qnt0(:) = qnt0(:) / rho0(:)
#endif /*ESRL*/
		
		
		! ========== Max and min profiles for monotonic nudging limiter ==========
!!!!! Should the max and min profiles be constructed with mass weight? !!!!!
		IF ( dondglimiter ) THEN
			! Interpolation indices to ZMM for monotonic nudging limiter
			DO j = jps, jpe
				DO i = ips, ipe
					khunt = 0
					DO k = 1, nzmm
						CALL find_indices( zmm(k), zs_eta(i,1:nzm,j), nzm, &
								km1zmm(i,k,j), k00zmm(i,k,j), kp1zmm(i,k,j), kp2zmm(i,k,j), khunt )
					ENDDO
				ENDDO
			ENDDO
			
			! Reset counter for MPI
			imnmx = 0
			
			! Min and max profile
			! QV
			IF ( donudging_qv .OR. donudging_qt ) THEN
				DO j = jps, jpe
					DO k = 1, nzmm
						DO i = ips, ipe
							km1 = km1zmm(i,k,j)
							k00 = k00zmm(i,k,j)
							kp1 = kp1zmm(i,k,j)
							kp2 = kp2zmm(i,k,j)
							f(i,k,j) = mono_cubic_interp( zmm(k), km1, k00, kp1, kp2, &
									zs_eta(i,km1,j), zs_eta(i,k00,j), zs_eta(i,kp1,j), zs_eta(i,kp2,j), &
									moist(i,km1,j,p_qv), moist(i,k00,j,p_qv), &
									moist(i,kp1,j,p_qv), moist(i,kp2,j,p_qv) )
						ENDDO
					ENDDO
				ENDDO
				DO k = 1, nzmm
					qv0min(k) = MINVAL( f(ips:ips+nx-1,k,jps:jps+ny-1) )
					qv0max(k) = MAXVAL( f(ips:ips+nx-1,k,jps:jps+ny-1) )
				ENDDO
				IF ( dompi ) THEN
					buf1mn( 1+imnmx*nzmm:(imnmx+1)*nzmm ) = qv0min(1:nzmm)
					buf1mx( 1+imnmx*nzmm:(imnmx+1)*nzmm ) = qv0max(1:nzmm)
					imnmx = imnmx + 1
				ENDIF
			ENDIF
			
			! QC
			IF ( donudging_qt ) THEN
				DO j = jps, jpe
					DO k = 1, nzmm
						DO i = ips, ipe
							km1 = km1zmm(i,k,j)
							k00 = k00zmm(i,k,j)
							kp1 = kp1zmm(i,k,j)
							kp2 = kp2zmm(i,k,j)
							f(i,k,j) = mono_cubic_interp( zmm(k), km1, k00, kp1, kp2, &
									zs_eta(i,km1,j), zs_eta(i,k00,j), zs_eta(i,kp1,j), zs_eta(i,kp2,j), &
									moist(i,km1,j,p_qc), moist(i,k00,j,p_qc), &
									moist(i,kp1,j,p_qc), moist(i,kp2,j,p_qc) )
						ENDDO
					ENDDO
				ENDDO
				DO k = 1, nzmm
					qc0min(k) = MINVAL( f(ips:ips+nx-1,k,jps:jps+ny-1) )
					qc0max(k) = MAXVAL( f(ips:ips+nx-1,k,jps:jps+ny-1) )
				ENDDO
				IF ( dompi ) THEN
					buf1mn( 1+imnmx*nzmm:(imnmx+1)*nzmm ) = qv0min(1:nzmm)
					buf1mx( 1+imnmx*nzmm:(imnmx+1)*nzmm ) = qv0max(1:nzmm)
					imnmx = imnmx + 1
				ENDIF
			ENDIF
			
#ifdef ESRL
			IF ( donudging_qnt .AND. nstep > 1 ) THEN
				! QNN
				DO j = jps, jpe
					DO k = 1, nzmm
						DO i = ips, ipe
							km1 = km1zmm(i,k,j)
							k00 = k00zmm(i,k,j)
							kp1 = kp1zmm(i,k,j)
							kp2 = kp2zmm(i,k,j)
							f(i,k,j) = mono_cubic_interp( zmm(k), km1, k00, kp1, kp2, &
									zs_eta(i,km1,j), zs_eta(i,k00,j), zs_eta(i,kp1,j), zs_eta(i,kp2,j), &
									qnn(i,km1,j),    qnn(i,k00,j),    qnn(i,kp1,j),    qnn(i,kp2,j) )
						ENDDO
					ENDDO
				ENDDO
				DO k = 1, nzmm
					qnn0min(k) = MINVAL( f(ips:ips+nx-1,k,jps:jps+ny-1) )
					qnn0max(k) = MAXVAL( f(ips:ips+nx-1,k,jps:jps+ny-1) )
				ENDDO
				IF ( dompi ) THEN
					buf1mn( 1+imnmx*nzmm:(imnmx+1)*nzmm ) = qnn0min(1:nzmm)
					buf1mx( 1+imnmx*nzmm:(imnmx+1)*nzmm ) = qnn0max(1:nzmm)
					imnmx = imnmx + 1
				ENDIF
				
				! QNC
				DO j = jps, jpe
					DO k = 1, nzmm
						DO i = ips, ipe
							km1 = km1zmm(i,k,j)
							k00 = k00zmm(i,k,j)
							kp1 = kp1zmm(i,k,j)
							kp2 = kp2zmm(i,k,j)
							f(i,k,j) = mono_cubic_interp( zmm(k), km1, k00, kp1, kp2, &
									zs_eta(i,km1,j), zs_eta(i,k00,j), zs_eta(i,kp1,j), zs_eta(i,kp2,j), &
									qnc(i,km1,j),    qnc(i,k00,j),    qnc(i,kp1,j),    qnc(i,kp2,j) )
						ENDDO
					ENDDO
				ENDDO
				DO k = 1, nzmm
					qnc0min(k) = MINVAL( f(ips:ips+nx-1,k,jps:jps+ny-1) )
					qnc0max(k) = MAXVAL( f(ips:ips+nx-1,k,jps:jps+ny-1) )
				ENDDO
				IF ( dompi ) THEN
					buf1mn( 1+imnmx*nzmm:(imnmx+1)*nzmm ) = qnc0min(1:nzmm)
					buf1mx( 1+imnmx*nzmm:(imnmx+1)*nzmm ) = qnc0max(1:nzmm)
					imnmx = imnmx + 1
				ENDIF
			ENDIF   ! donudging_qnt
#endif /*ESRL*/
			
			! Min & max profiles over whole processors with MPI
			IF ( dompi ) THEN
				CALL task_min_real( buf1mn, buf2mn, nzmm*nmnmx )
				CALL task_max_real( buf1mx, buf2mx, nzmm*nmnmx )
				imnmx = 0
				IF ( donudging_qv .OR. donudging_qt ) THEN
					qv0min(1:nzmm) = buf2mn( 1+imnmx*nzmm:(imnmx+1)*nzmm )
					qv0max(1:nzmm) = buf2mx( 1+imnmx*nzmm:(imnmx+1)*nzmm )
					imnmx = imnmx + 1
				ENDIF
				IF ( donudging_qt ) THEN
					qc0min(1:nzmm) = buf2mn( 1+imnmx*nzmm:(imnmx+1)*nzmm )
					qc0max(1:nzmm) = buf2mx( 1+imnmx*nzmm:(imnmx+1)*nzmm )
					imnmx = imnmx + 1
				ENDIF
#ifdef ESRL
				IF ( donudging_qnt .AND. nstep > 1 ) THEN
					qnn0min(1:nzmm) = buf2mn( 1+imnmx*nzmm:(imnmx+1)*nzmm )
					qnn0max(1:nzmm) = buf2mx( 1+imnmx*nzmm:(imnmx+1)*nzmm )
					imnmx = imnmx + 1
					qnc0min(1:nzmm) = buf2mn( 1+imnmx*nzmm:(imnmx+1)*nzmm )
					qnc0max(1:nzmm) = buf2mx( 1+imnmx*nzmm:(imnmx+1)*nzmm )
					imnmx = imnmx + 1
				ENDIF
#endif
			ENDIF
		ENDIF   ! dondglimiter
		
	END SUBROUTINE wrfles_prepforcing
	
	!=====================================================================================
	
	SUBROUTINE wrfles_couple2mass( &
		! IN
		n_moist, n_scalar, mut, muu, muv, &
		! INOUT
		u_tend, v_tend, t_tend, m_tend, s_tend, &
		! INDICES
		its, ite, jts, jte, kts, kte )
		
	! Cauple tendency array to the column mass (mut, muu, muv) after large-scale forcing and
	! subsidence.
	! See the subroutine calculate_phy_tend in /path2wrf/dyn_em/module_em.F
		
#ifdef ESRL
		USE module_state_description, ONLY: p_qv, p_qc, p_qnn, p_qnc
#else
		USE module_state_description, ONLY: p_qv, p_qc
#endif /*ESRL*/
		
		IMPLICIT NONE
		
		! Input
		INTEGER, INTENT(IN) :: n_moist, n_scalar
		INTEGER, INTENT(IN) :: its, ite, jts, jte, kts, kte
		REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: mut
		REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: muu
		REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: muv
		
		! Input & Output
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: u_tend
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: v_tend
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: t_tend
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme,n_moist),  INTENT(INOUT) :: m_tend
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme,n_scalar), INTENT(INOUT) :: s_tend
		
		! Local
		INTEGER :: i, j, k, n
		
		! U & V
		IF ( donudging_uv .OR. dosubsidence ) THEN
			DO j = jts, jte
				DO k = 1, kte
					DO i = its, ite
						u_tend(i,k,j) = muu(i,j) * u_tend(i,k,j)
						v_tend(i,k,j) = muv(i,j) * v_tend(i,k,j)
					ENDDO
				ENDDO
			ENDDO
		ENDIF
		
		! T
		IF ( donudging_tp .OR. doadvforcing .OR. dosubsidence ) THEN
			DO j = jts, jte
				DO k = 1, kte
					DO i = its, ite
						t_tend(i,k,j) = mut(i,j) * t_tend(i,k,j)
					ENDDO
				ENDDO
			ENDDO
		ENDIF
		
		! M
		! QV
		IF ( donudging_qv .OR. doadvforcing .OR. dosubsidence ) THEN
			DO j = jts, jte
				DO k = 1, kte
					DO i = its, ite
						m_tend(i,k,j,p_qv) = mut(i,j) * m_tend(i,k,j,p_qv)
					ENDDO
				ENDDO
			ENDDO
		ENDIF
		! QC
		IF ( donudging_qt .OR. dosubsidence ) THEN
			DO j = jts, jte
				DO k = 1, kte
					DO i = its, ite
						m_tend(i,k,j,p_qc) = mut(i,j) * m_tend(i,k,j,p_qc)
					ENDDO
				ENDDO
			ENDDO
		ENDIF
		! Other M
		IF ( dosubsidence ) THEN
			DO n = 1, n_moist
				IF ( n /= p_qv .AND. n /= p_qc ) THEN
					DO j = jts, jte
						DO k = 1, kte
							DO i = its, ite
								m_tend(i,k,j,n) = mut(i,j) * m_tend(i,k,j,n)
							ENDDO
						ENDDO
					ENDDO
				ENDIF
			ENDDO
		ENDIF
		
		! S
		IF ( dosubsidence ) THEN
			DO n = 1, n_scalar
				DO j = jts, jte
					DO k = 1, kte
						DO i = its, ite
							s_tend(i,k,j,n) = mut(i,j) * s_tend(i,k,j,n)
						ENDDO
					ENDDO
				ENDDO
			ENDDO
		ENDIF
		
#ifdef ESRL
		! QNN and QNC nudging only
		IF ( (.NOT.dosubsidence) .AND. donudging_qnt ) THEN
			DO j = jts, jte
				DO k = 1, kte
					DO i = its, ite
						s_tend(i,k,j,p_qnn) = mut(i,j) * s_tend(i,k,j,p_qnn)
						s_tend(i,k,j,p_qnc) = mut(i,j) * s_tend(i,k,j,p_qnc)
					ENDDO
				ENDDO
			ENDDO
		ENDIF
#endif /*ESRL*/
		
	END SUBROUTINE wrfles_couple2mass
	
	!=====================================================================================
	
	SUBROUTINE wrfles_radforcing( &
		! IN
		z, exner, &
		! INOUT
		rthraten, &
		! INDICES (IN)
		its, ite, jts, jte, kts, kte )
	
	! Compute radiative forcing and store it in longwave heating rate array.
	! Returns zero forcing with doradforcing=.FALSE.
	! This subroutine is called in /path2wrf/phys/module_radiation_driver.F
		
		IMPLICIT NONE
		
		! Input
		INTEGER, INTENT(IN) :: its, ite, jts, jte, kts, kte
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: z, exner
		
		! Output
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: rthraten
		
		! Local
		REAL, DIMENSION(nzlsf) :: rad
		INTEGER :: km1, k00, kp1, kp2, khunt
		INTEGER :: i, j, k
		
		IF ( doradforcing ) THEN
		!	PRINT*,'wrfles_radforcing: n1_lsf, n2_lsf = ', n1_lsf, n2_lsf
			
			! Radiative forcing at current time
			rad(:) = ( 1. - coef_lsf ) * rlsf(:,n1_lsf) + coef_lsf * rlsf(:,n2_lsf)
			
			! Apply forcing
			DO j = jts, jte
				DO i = its, ite
					khunt = 0
					DO k = kts, kte
						CALL find_indices( z(i,k,j), zlsf, nzlsf, km1, k00, kp1, kp2, khunt )
						rthraten(i,k,j) = mono_cubic_interp( z(i,k,j), km1, k00, kp1, kp2, &
								zlsf(km1), zlsf(k00), zlsf(kp1), zlsf(kp2), &
								rad(km1),  rad(k00),  rad(kp1),  rad(kp2) ) / exner(i,k,j)
					ENDDO
				ENDDO
			ENDDO
		ELSE
			rthraten(its:ite,kts:kte,jts:jte) = 0.
		ENDIF
		
	END SUBROUTINE wrfles_radforcing
	
	!=====================================================================================
	
	SUBROUTINE wrfles_lsforcing( &
#ifdef ESRL
		! IN
		dt, zbase, qv, qc, nstep, qnn, qnc, &
		! INOUT
		ubase, vbase, u_tend, v_tend, t_tend, qv_tend, qc_tend, qnn_tend, qnc_tend )
#else
		! IN
		dt, zbase, qv, qc, &
		! INOUT
		ubase, vbase, u_tend, v_tend, t_tend, qv_tend, qc_tend )
#endif /*ESRL*/
		
	! Add large scale forcing (advective tendency and nudging) to tendency array.
	! Update geostrophic wind.
	! This subroutine is called in /path2wrf/dyn_em/module_first_rk_step_part1.F.
	
		USE module_model_constants, ONLY: g
#ifdef WRFSTAT
		USE module_statistics, ONLY: docollect_data, uobs, unudge, vobs, vnudge, tplshadv, tpobs, &
		                             tpnudge, qvlshadv, qvobs, qvnudge, qntnudge
#endif /*WRFSTAT*/
		
		IMPLICIT NONE
		
		! Input
		REAL, INTENT(IN) :: dt
		REAL, DIMENSION(kms:kme), INTENT(IN) :: zbase
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: qv
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: qc
#ifdef ESRL
		INTEGER, INTENT(IN) :: nstep
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: qnn   ! CCN number concentration
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: qnc   ! droplet number concentration
#endif /*ESRL*/
		
		! Output
		REAL, DIMENSION(kms:kme), INTENT(INOUT) :: ubase, vbase
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: u_tend
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: v_tend
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: t_tend
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: qv_tend
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: qc_tend
#ifdef ESRL
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: qnn_tend
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: qnc_tend
#endif /*ESRL*/
		
		! Local
		INTEGER :: i, j, k
		LOGICAL, SAVE :: dolsforcing = .FALSE.
		! Vertical interpolation
		INTEGER :: km1, k00, kp1, kp2, khunt, khunt1, khunt2, ka, kb
#ifdef WRFSTAT
		LOGICAL, SAVE :: k4zlsf2zs = .FALSE.
		INTEGER, SAVE, ALLOCATABLE, DIMENSION(:) :: km1zs, k00zs, kp1zs, kp2zs   ! ZLSF => ZS
#endif /*WRFSTAT*/
		LOGICAL, SAVE :: k4zsh2zbase = .FALSE.
		INTEGER, SAVE, ALLOCATABLE, DIMENSION(:) :: km1zb, k00zb, kp1zb, kp2zb    ! ZSH => ZBASE
		INTEGER, DIMENSION(ips:ipe,nzm,jps:jpe) :: km1zsh, k00zsh, kp1zsh, kp2zsh ! ZSH => ZS_ETA
		INTEGER, DIMENSION(ips:ipe,nzm,jps:jpe) :: km1zmm, k00zmm, kp1zmm, kp2zmm ! ZMM => ZS_ETA
		! Large scale advective tendency
		REAL, DIMENSION(nzlsf) :: dtg0, dqg0
		! Nudging
		REAL, DIMENSION(nzsh) :: ug0, vg0, tg0, qg0   ! observed profile
		REAL, DIMENSION(nzsh) :: dudt, dvdt, dtdt, dqdt   ! nudging tendency
		REAL :: dqtdt, dqvdt, dqcdt   ! nudging tendency
		REAL :: fraction   ! used to partition nudging tendency, e.g.,dqtdt => dqvdt + dqcdt
		REAL :: fmn, fmx, Lmn, Lmx   ! For monotonic limiter
#ifdef ESRL
		REAL, DIMENSION(nzsh) :: dqndt
		REAL :: dqntdt, dqnndt, dqncdt
		REAL :: nccn
#endif /*ESRL*/
		
		dolsforcing = doupdategwnd .OR. donudging_uv .OR. donudging_tp .OR. donudging_qv &
		           .OR. donudging_qt .OR. doadvforcing
		IF ( dolsforcing ) THEN
		
		!	PRINT*,'wrfles_lsforcing: n1_lsf, n2_lsf = ', n1_lsf, n2_lsf
			
			! Large scale advective tendency
			IF ( doadvforcing ) THEN
				dtg0(:) = ( 1. - coef_lsf ) * dtlsf(:,n1_lsf) + coef_lsf * dtlsf(:,n2_lsf)
				dqg0(:) = ( 1. - coef_lsf ) * dqlsf(:,n1_lsf) + coef_lsf * dqlsf(:,n2_lsf)
				DO j = jps, jpe
					DO k = 1, nzm
						DO i = ips, ipe
							km1 = km1zlsf(i,k,j)
							k00 = k00zlsf(i,k,j)
							kp1 = kp1zlsf(i,k,j)
							kp2 = kp2zlsf(i,k,j)
							t_tend(i,k,j) = t_tend(i,k,j) &
									+ mono_cubic_interp( zs_eta(i,k,j), km1, k00, kp1, kp2, &
									zlsf(km1), zlsf(k00), zlsf(kp1), zlsf(kp2), &
									dtg0(km1), dtg0(k00), dtg0(kp1), dtg0(kp2) )
							qv_tend(i,k,j) = qv_tend(i,k,j) &
									+ mono_cubic_interp( zs_eta(i,k,j), km1, k00, kp1, kp2, &
									zlsf(km1), zlsf(k00), zlsf(kp1), zlsf(kp2), &
									dqg0(km1), dqg0(k00), dqg0(kp1), dqg0(kp2) )
						ENDDO
					ENDDO
				ENDDO
#ifdef WRFSTAT
				! Collect output for module_statistics.F
				IF ( docollect_data ) THEN
					! TPLSADVH, QVLSADVH
					IF ( .NOT.k4zlsf2zs ) THEN
						ALLOCATE( km1zs(nzm), k00zs(nzm), kp1zs(nzm), kp2zs(nzm) )
						khunt = 0
						DO k = 1, nzm
							CALL find_indices(zs(k),zlsf,nzlsf,km1zs(k),k00zs(k),kp1zs(k),kp2zs(k),khunt)
						ENDDO
						k4zlsf2zs = .TRUE.
					ENDIF
					DO k = 1, nzm
						km1 = km1zs(k)
						k00 = k00zs(k)
						kp1 = kp1zs(k)
						kp2 = kp2zs(k)
						tplshadv(k) = mono_cubic_interp( zs(k), km1, k00, kp1, kp2, &
								zlsf(km1), zlsf(k00), zlsf(kp1), zlsf(kp2), &
								dtg0(km1), dtg0(k00), dtg0(kp1), dtg0(kp2) )
						qvlshadv(k) = mono_cubic_interp( zs(k), km1, k00, kp1, kp2, &
								zlsf(km1), zlsf(k00), zlsf(kp1), zlsf(kp2), &
								dqg0(km1), dqg0(k00), dqg0(kp1), dqg0(kp2) )
					ENDDO
				ENDIF
#endif /*WRFSTAT*/
			ENDIF
			
			! Update ubase and vbase = geostrophic/observed/specified wind
			IF ( doupdategwnd .OR. donudging_uv ) THEN
				ug0(:) = ( 1. - coef_lsf ) * undg(:,n1_lsf) + coef_lsf * undg(:,n2_lsf)
				vg0(:) = ( 1. - coef_lsf ) * vndg(:,n1_lsf) + coef_lsf * vndg(:,n2_lsf)
				! ????? zbase = zs ?????
				IF ( .NOT.k4zsh2zbase ) THEN
					ALLOCATE( km1zb(nzm), k00zb(nzm), kp1zb(nzm), kp2zb(nzm) )
					khunt = 0
					DO k = 1, nzm   ! = kpe-1
						CALL find_indices(zbase(k),zsh,nzsh,km1zb(k),k00zb(k),kp1zb(k),kp2zb(k),khunt)
					ENDDO
					k4zsh2zbase = .TRUE.
				ENDIF
				DO k = 1, nzm   ! = kpe-1
					km1 = km1zb(k)
					k00 = k00zb(k)
					kp1 = kp1zb(k)
					kp2 = kp2zb(k)
					ubase(k) = mono_cubic_interp( zbase(k), km1, k00, kp1, kp2, &
							zsh(km1), zsh(k00), zsh(kp1), zsh(kp2), &
							ug0(km1), ug0(k00), ug0(kp1), ug0(kp2) )
					vbase(k) = mono_cubic_interp( zbase(k), km1, k00, kp1, kp2, &
							zsh(km1), zsh(k00), zsh(kp1), zsh(kp2), &
							vg0(km1), vg0(k00), vg0(kp1), vg0(kp2) )
				ENDDO
#ifdef WRFSTAT
				! Collect output for module_statistics.F
				IF ( docollect_data ) THEN
					! UOBS & VOBS
					DO k = 1, nzm
						k00 = 1 + ( k - 1 ) * hrlayers   ! k00=index of zsh at zs
						uobs(k) = ug0(k00)
						vobs(k) = vg0(k00)
					ENDDO
				ENDIF
#endif /*WRFSTAT*/
			ENDIF
			
			! Nudging
			! U & V
			IF ( donudging_uv ) THEN
				! Nudging tendency defined at ZSH
				dudt(:) = ( ug0(:) - u0(:) ) * itau_ndguv
				dvdt(:) = ( vg0(:) - v0(:) ) * itau_ndguv
				DO j = jps, jpe
					DO i = ips, ipe
						khunt1 = 0
						khunt2 = 0
						DO k = 1, nzm
							! U
							CALL find_indices( zu_eta(i,k,j), zsh, nzsh, km1, k00, kp1, kp2, khunt1 )
							u_tend(i,k,j) = u_tend(i,k,j) &
									+ mono_cubic_interp( zu_eta(i,k,j), km1, k00, kp1, kp2, &
									zsh(km1),  zsh(k00),  zsh(kp1),  zsh(kp2), &
									dudt(km1), dudt(k00), dudt(kp1), dudt(kp2) )
							! V
							CALL find_indices( zv_eta(i,k,j), zsh, nzsh, km1, k00, kp1, kp2, khunt2 )
							v_tend(i,k,j) = v_tend(i,k,j) &
									+ mono_cubic_interp( zv_eta(i,k,j), km1, k00, kp1, kp2, &
									zsh(km1),  zsh(k00),  zsh(kp1),  zsh(kp2), &
									dvdt(km1), dvdt(k00), dvdt(kp1), dvdt(kp2) )
						ENDDO
					ENDDO
				ENDDO
#ifdef WRFSTAT
				! Collect output for module_statistics.F
				IF ( docollect_data ) THEN
					! UNUDGE & VNUDGE
					DO k = 1, nzm
						k00 = 1 + ( k - 1 ) * hrlayers   ! k00=index of zsh at zs
						unudge(k) = dudt(k00)
						vnudge(k) = dvdt(k00)
					ENDDO
				ENDIF
#endif /*WRFSTAT*/
			ENDIF
			
			
			! TP, QV, QT, QNT
			! Prepare interpolation indices from ZSH to ZS_ETA
			IF ( donudging_tp .OR. donudging_qv .OR. donudging_qt .OR. donudging_qnt ) THEN
				DO j = jps, jpe
					DO i = ips, ipe
						khunt = 0
						DO k = 1, nzm
							CALL find_indices( zs_eta(i,k,j), zsh, nzsh, &
									km1zsh(i,k,j), k00zsh(i,k,j), kp1zsh(i,k,j), kp2zsh(i,k,j), khunt )
						ENDDO
					ENDDO
				ENDDO
			ENDIF
			IF ( dondglimiter ) THEN
				DO j = jps, jpe
					DO i = ips, ipe
						khunt = 0
						DO k = 1, nzm
							CALL find_indices( zs_eta(i,k,j), zmm, nzmm, &
									km1zmm(i,k,j), k00zmm(i,k,j), kp1zmm(i,k,j), kp2zmm(i,k,j), khunt )
						ENDDO
					ENDDO
				ENDDO
			ENDIF
			
			! TP
			IF ( donudging_tp ) THEN
				tg0(:)  = ( 1. - coef_lsf ) * tndg(:,n1_lsf) + coef_lsf * tndg(:,n2_lsf)
				dtdt(:) = ( tg0(:) - t0(:) ) * itau_ndg
				DO j = jps, jpe
					DO k = 1, nzm
						DO i = ips, ipe
							km1 = km1zsh(i,k,j)
							k00 = k00zsh(i,k,j)
							kp1 = kp1zsh(i,k,j)
							kp2 = kp2zsh(i,k,j)
							t_tend(i,k,j) = t_tend(i,k,j) &
									+ mono_cubic_interp( zs_eta(i,k,j), km1, k00, kp1, kp2, &
									zsh(km1),  zsh(k00),  zsh(kp1),  zsh(kp2), &
									dtdt(km1), dtdt(k00), dtdt(kp1), dtdt(kp2) )
						ENDDO
					ENDDO
				ENDDO
#ifdef WRFSTAT
				! Collect output for module_statistics.F
				IF ( docollect_data ) THEN
					! TPOBS & TPNUDGE
					DO k = 1, nzm
						k00 = 1 + ( k - 1 ) * hrlayers
						tpobs(k) = tg0(k00)
						tpnudge(k) = dtdt(k00)
					ENDDO
				ENDIF
#endif /*WRFSTAT*/
			ENDIF
			
			! QV, QT
			IF ( donudging_qv .OR. donudging_qt ) &
				qg0(:) = ( 1. - coef_lsf ) * qndg(:,n1_lsf) + coef_lsf * qndg(:,n2_lsf)
			
			! QV
			IF ( donudging_qv ) THEN
				dqdt(:) = ( qg0(:) - qv0(:) ) * itau_ndg
				IF ( .NOT.dondglimiter ) THEN
					DO j = jps, jpe
						DO k = 1, nzm
							DO i = ips, ipe
								km1 = km1zsh(i,k,j)
								k00 = k00zsh(i,k,j)
								kp1 = kp1zsh(i,k,j)
								kp2 = kp2zsh(i,k,j)
								qv_tend(i,k,j) = qv_tend(i,k,j) &
										+ mono_cubic_interp( zs_eta(i,k,j), km1, k00, kp1, kp2, &
										zsh(km1),  zsh(k00),  zsh(kp1),  zsh(kp2), &
										dqdt(km1), dqdt(k00), dqdt(kp1), dqdt(kp2) )
							ENDDO
						ENDDO
					ENDDO
				ELSE
					! Limit nudging tendency
					DO j = jps, jpe
						DO k = 1, nzm
							DO i = ips, ipe
								! Nudging tendency
								km1 = km1zsh(i,k,j)
								k00 = k00zsh(i,k,j)
								kp1 = kp1zsh(i,k,j)
								kp2 = kp2zsh(i,k,j)
								dqvdt = mono_cubic_interp( zs_eta(i,k,j), km1, k00, kp1, kp2, &
										zsh(km1),  zsh(k00),  zsh(kp1),  zsh(kp2), &
										dqdt(km1), dqdt(k00), dqdt(kp1), dqdt(kp2) )
								! Min & max at ZS_ETA level
								km1 = km1zmm(i,k,j)
								k00 = k00zmm(i,k,j)
								kp1 = kp1zmm(i,k,j)
								kp2 = kp2zmm(i,k,j)
								fmn = mono_cubic_interp( zs_eta(i,k,j), km1, k00, kp1, kp2, &
										zmm(km1),    zmm(k00),    zmm(kp1),    zmm(kp2), &
										qv0min(km1), qv0min(k00), qv0min(kp1), qv0min(kp2) )
								fmx = mono_cubic_interp( zs_eta(i,k,j), km1, k00, kp1, kp2, &
										zmm(km1),    zmm(k00),    zmm(kp1),    zmm(kp2), &
										qv0max(km1), qv0max(k00), qv0max(kp1), qv0max(kp2) )
								! Local min & max bound
								kb = MAX( 1, k-1 )
								ka = MIN( nzm, k+1 )
								Lmn = MIN( qv(i,k,j),  qv(i-1,k,j), qv(i+1,k,j), qv(i,kb,j), &
								           qv(i,ka,j), qv(i,k,j-1), qv(i,k,j+1) )
								Lmx = MAX( qv(i,k,j),  qv(i-1,k,j), qv(i+1,k,j), qv(i,kb,j), &
								           qv(i,ka,j), qv(i,k,j-1), qv(i,k,j+1) )
								! Limit nudging tendency
								dqvdt = MAX( MIN( dqvdt, 0. ), MIN( (fmn-Lmn)/dt, 0. ) ) &   !sink
								      + MIN( MAX( dqvdt, 0. ), MAX( (fmx-Lmx)/dt, 0. ) )   !source
								! Add to tendency
								qv_tend(i,k,j) = qv_tend(i,k,j) + dqvdt
							ENDDO
						ENDDO
					ENDDO
				ENDIF   ! dondglimiter
			ENDIF   ! donudging_qv
			
			! QT
			IF ( donudging_qt ) THEN
				dqdt(:) = ( qg0(:) - qv0(:) - qc0(:) ) * itau_ndg
				IF ( .NOT.dondglimiter ) THEN
					DO j = jps, jpe
						DO k = 1, nzm
							DO i = ips, ipe
								! Nudging tendency for QT
								km1 = km1zsh(i,k,j)
								k00 = k00zsh(i,k,j)
								kp1 = kp1zsh(i,k,j)
								kp2 = kp2zsh(i,k,j)
								dqtdt = mono_cubic_interp( zs_eta(i,k,j), km1, k00, kp1, kp2, &
										zsh(km1),  zsh(k00),  zsh(kp1),  zsh(kp2), &
										dqdt(km1), dqdt(k00), dqdt(kp1), dqdt(kp2) )
								! Fraction of qv to qt
								fraction = qv(i,k,j) / ( qv(i,k,j) + qc(i,k,j) )
								! Add to tendency for QV & QC
								qv_tend(i,k,j) = qv_tend(i,k,j) + dqtdt * fraction
								qc_tend(i,k,j) = qc_tend(i,k,j) + dqtdt * ( 1. - fraction )
							ENDDO
						ENDDO
					ENDDO
				ELSE
					! Limit nudging tendency
					DO j = jps, jpe
						DO k = 1, nzm
							DO i = ips, ipe
								! Nudging tendency
								km1 = km1zsh(i,k,j)
								k00 = k00zsh(i,k,j)
								kp1 = kp1zsh(i,k,j)
								kp2 = kp2zsh(i,k,j)
								dqtdt = mono_cubic_interp( zs_eta(i,k,j), km1, k00, kp1, kp2, &
										zsh(km1),  zsh(k00),  zsh(kp1),  zsh(kp2), &
										dqdt(km1), dqdt(k00), dqdt(kp1), dqdt(kp2) )
								! Nudging tendency for QV & QC
								fraction = qv(i,k,j) / ( qv(i,k,j) + qc(i,k,j) )
								dqvdt = dqtdt * fraction
								dqcdt = dqtdt * ( 1. - fraction )
								
								! Limit nudging tendency for QV
								! Min & max at ZS_ETA level
								km1 = km1zmm(i,k,j)
								k00 = k00zmm(i,k,j)
								kp1 = kp1zmm(i,k,j)
								kp2 = kp2zmm(i,k,j)
								fmn = mono_cubic_interp( zs_eta(i,k,j), km1, k00, kp1, kp2, &
										zmm(km1),    zmm(k00),    zmm(kp1),    zmm(kp2), &
										qv0min(km1), qv0min(k00), qv0min(kp1), qv0min(kp2) )
								fmx = mono_cubic_interp( zs_eta(i,k,j), km1, k00, kp1, kp2, &
										zmm(km1),    zmm(k00),    zmm(kp1),    zmm(kp2), &
										qv0max(km1), qv0max(k00), qv0max(kp1), qv0max(kp2) )
								! Local min & max bound
								kb = MAX( 1, k-1 )
								ka = MIN( nzm, k+1 )
								Lmn = MIN( qv(i,k,j),  qv(i-1,k,j), qv(i+1,k,j), qv(i,kb,j), &
								           qv(i,ka,j), qv(i,k,j-1), qv(i,k,j+1) )
								Lmx = MAX( qv(i,k,j),  qv(i-1,k,j), qv(i+1,k,j), qv(i,kb,j), &
								           qv(i,ka,j), qv(i,k,j-1), qv(i,k,j+1) )
								! Limit nudging tendency
								dqvdt = MAX( MIN( dqvdt, 0. ), MIN( (fmn-Lmn)/dt, 0. ) ) &   !sink
								      + MIN( MAX( dqvdt, 0. ), MAX( (fmx-Lmx)/dt, 0. ) )   !source
								! Add to tendency to QV
								qv_tend(i,k,j) = qv_tend(i,k,j) + dqvdt
									
								! Limit nudging tendency for QC
								! CAUTION: ONLY WORK WITH MICROPHYSICS WHICH TAKES CARE OF NEGATIVE QC!
								! Min & max at z
								fmn = mono_cubic_interp( zs_eta(i,k,j), km1, k00, kp1, kp2, &
										zmm(km1),    zmm(k00),    zmm(kp1),    zmm(kp2), &
										qc0min(km1), qc0min(k00), qc0min(kp1), qc0min(kp2) )
								fmx = mono_cubic_interp( zs_eta(i,k,j), km1, k00, kp1, kp2, &
										zmm(km1),    zmm(k00),    zmm(kp1),    zmm(kp2), &
										qc0max(km1), qc0max(k00), qc0max(kp1), qc0max(kp2) )
								! Local min & max bound
								Lmn = MIN( qc(i,k,j),  qc(i-1,k,j), qc(i+1,k,j), qc(i,kb,j), &
								           qc(i,ka,j), qc(i,k,j-1), qc(i,k,j+1) )
								Lmx = MAX( qc(i,k,j),  qc(i-1,k,j), qc(i+1,k,j), qc(i,kb,j), &
								           qc(i,ka,j), qc(i,k,j-1), qc(i,k,j+1) )
								! Limit nudging tendency
								dqcdt = MAX( MIN( dqcdt, 0. ), MIN( (fmn-Lmn)/dt, 0. ) ) &   !sink
								      + MIN( MAX( dqcdt, 0. ), MAX( (fmx-Lmx)/dt, 0. ) )   !source
								! Add to tendency to QC
								qc_tend(i,k,j) = qc_tend(i,k,j) + dqcdt
							ENDDO
						ENDDO
					ENDDO
				ENDIF   ! dondglimiter
			ENDIF   ! donudging_qt
			
#ifdef WRFSTAT
			IF ( donudging_qv .OR. donudging_qt ) THEN
				! Collect output for module_statistics.F
				IF ( docollect_data ) THEN
					! QVOBS & QVNUDGE
					DO k = 1, nzm
						k00 = 1 + ( k - 1 ) * hrlayers
						qvobs(k) = qg0(k00)
						qvnudge(k) = dqdt(k00)
					ENDDO
				ENDIF
			ENDIF
#endif /*WRFSTAT*/
			
		ENDIF   ! dolsforcing
		
		
#ifdef ESRL
		! QNT nudging
		IF ( donudging_qnt .AND. nstep > 1 ) THEN
			! Total number of CCN
			nccn = nccn_feingold2m
			IF ( bimodal_feingold2m ) nccn = nccn + nccn2_feingold2m
			
			dqndt(:) = ( nccn - qnt0(:) ) * itau_ndg
			
			! Nudging
			IF ( .NOT.dondglimiter ) THEN
				DO j = jps, jpe
					DO k = 1, nzm
						DO i = ips, ipe
							km1 = km1zsh(i,k,j)
							k00 = k00zsh(i,k,j)
							kp1 = kp1zsh(i,k,j)
							kp2 = kp2zsh(i,k,j)
							dqntdt = mono_cubic_interp( zs_eta(i,k,j), km1, k00, kp1, kp2, &
									zsh(km1),   zsh(k00),   zsh(kp1),   zsh(kp2), &
									dqndt(km1), dqndt(k00), dqndt(kp1), dqndt(kp2) )
							! Fraction of qnn to qnn+qnc
							fraction = qnn(i,k,j) / ( qnn(i,k,j) + qnc(i,k,j) )
							! Add to tendency to QNN & QNC
							qnn_tend(i,k,j) = qnn_tend(i,k,j) + dqntdt * fraction
							qnc_tend(i,k,j) = qnc_tend(i,k,j) + dqntdt * ( 1. - fraction )
						ENDDO
					ENDDO
				ENDDO
			ELSE
				! Limit nudging tendency
				DO j = jps, jpe
					DO k = 1, nzm
						DO i = ips, ipe
							km1 = km1zsh(i,k,j)
							k00 = k00zsh(i,k,j)
							kp1 = kp1zsh(i,k,j)
							kp2 = kp2zsh(i,k,j)
							dqntdt = mono_cubic_interp( zs_eta(i,k,j), km1, k00, kp1, kp2, &
									zsh(km1),   zsh(k00),   zsh(kp1),   zsh(kp2), &
									dqndt(km1), dqndt(k00), dqndt(kp1), dqndt(kp2) )
							fraction = qnn(i,k,j) / ( qnn(i,k,j) + qnc(i,k,j) )
							dqnndt = dqntdt * fraction
							dqncdt = dqntdt * ( 1. - fraction )
							
							! Limit nudging tendency for QNN
							! Min and max at ZS_ETA level
							km1 = km1zmm(i,k,j)
							k00 = k00zmm(i,k,j)
							kp1 = kp1zmm(i,k,j)
							kp2 = kp2zmm(i,k,j)
							fmn = mono_cubic_interp( zs_eta(i,k,j), km1, k00, kp1, kp2, &
									zmm(km1),     zmm(k00),     zmm(kp1),     zmm(kp2), &
									qnn0min(km1), qnn0min(k00), qnn0min(kp1), qnn0min(kp2) )
							fmx = mono_cubic_interp( zs_eta(i,k,j), km1, k00, kp1, kp2, &
									zmm(km1),     zmm(k00),     zmm(kp1),     zmm(kp2), &
									qnn0max(km1), qnn0max(k00), qnn0max(kp1), qnn0max(kp2) )
							! Local min & max bound
							kb = MAX( 1, k-1 )
							ka = MIN( nzm, k+1 )
							Lmn = MIN( qnn(i,k,j),  qnn(i-1,k,j), qnn(i+1,k,j), qnn(i,kb,j), &
							           qnn(i,ka,j), qnn(i,k,j-1), qnn(i,k,j+1) )
							Lmx = MAX( qnn(i,k,j),  qnn(i-1,k,j), qnn(i+1,k,j), qnn(i,kb,j), &
							           qnn(i,ka,j), qnn(i,k,j-1), qnn(i,k,j+1) )
							! Limit nudging tendency
							dqnndt = MAX( MIN( dqnndt, 0. ), MIN( (fmn-Lmn)/dt, 0. ) ) &   !sink
							       + MIN( MAX( dqnndt, 0. ), MAX( (fmx-Lmx)/dt, 0. ) )   !source
							! Add to tendency to QNN
							qnn_tend(i,k,j) = qnn_tend(i,k,j) + dqnndt
							
							! Limit nudging tendency for QNC
							! Min & max at ZS_ETA level
							fmn = mono_cubic_interp( zs_eta(i,k,j), km1, k00, kp1, kp2, &
									zmm(km1),     zmm(k00),     zmm(kp1),     zmm(kp2), &
									qnc0min(km1), qnc0min(k00), qnc0min(kp1), qnc0min(kp2) )
							fmx = mono_cubic_interp( zs_eta(i,k,j), km1, k00, kp1, kp2, &
									zmm(km1),     zmm(k00),     zmm(kp1),     zmm(kp2), &
									qnc0max(km1), qnc0max(k00), qnc0max(kp1), qnc0max(kp2) )
							! Local min & max bound
							Lmn = MIN( qnc(i,k,j),  qnc(i-1,k,j), qnc(i+1,k,j), qnc(i,kb,j), &
							           qnc(i,ka,j), qnc(i,k,j-1), qnc(i,k,j+1) )
							Lmx = MAX( qnc(i,k,j),  qnc(i-1,k,j), qnc(i+1,k,j), qnc(i,kb,j), &
							           qnc(i,ka,j), qnc(i,k,j-1), qnc(i,k,j+1) )
							! Limit nudging tendency
							dqncdt = MAX( MIN( dqncdt, 0. ), MIN( (fmn-Lmn)/dt, 0. ) ) &   !sink
							       + MIN( MAX( dqncdt, 0. ), MAX( (fmx-Lmx)/dt, 0. ) )   !source
							! Add to tendency to QNC
							qnc_tend(i,k,j) = qnc_tend(i,k,j) + dqncdt
						ENDDO
					ENDDO
				ENDDO
			ENDIF   ! dondglimiter
#ifdef WRFSTAT
			! Collect output for module_statistics.F
			! qnn and qnc nudging tendencies have not been implemented.
			IF ( docollect_data ) THEN
				DO k = 1, nzm
					k00 = 1 + ( k - 1 ) * hrlayers
					qntnudge(k) = dqndt(k00)
				ENDDO
			ENDIF
#endif /*WRFSTAT*/
		ENDIF   ! donudging_qnt
#endif /*ESRL*/
		
	END SUBROUTINE wrfles_lsforcing
	
	!======================= Interpolation subroutine =======================
	
	SUBROUTINE find_indices( z_interp_height, z_local_height, nz_in, km1, k00, kp1, kp2, klo )
	
		IMPLICIT NONE
		! IN
		INTEGER, INTENT(IN) :: nz_in
		REAL, INTENT(IN) :: z_interp_height
		REAL, DIMENSION(nz_in), INTENT(IN) :: z_local_height
		! OUT
		INTEGER, INTENT(OUT) :: km1, k00, kp1, kp2
		! INOUT
		INTEGER, INTENT(INOUT) :: klo
		! Local
		INTEGER :: khi, km, inc
		
		! Use the subroutine hunt in Numerical recipe to find k00.
		! Hunt has been modified so that it only works when
		! - z_local_height(nz_in) is ascend
		! - z_interp_height is highre or equal to the preiously z_interp_heigh
		! Both are true for this module.
		! Input klo has to be zero as an initial guess for the first time.
		! This subroutine is the most efficient if it is used in vertical level loop.
		
		!==========
		! Hunt
		! Given an array z_local_height(1:nz_in), and given a value z_interp_height, returns a value
		! klo such that z_interp_height is between z_local_height(klo) and z_local_height(klo+1).
		! z_local_height must be monotonically increasing. klo = 0 or klo = nz_in is returned to
		! indicate that z_interp_height is out of range. klo on input is taken as the initial guess
		! for klo on output.
		IF ( klo <= 0 .OR. klo > nz_in ) THEN
			! Input guess not useful. Go immediately to bisection.
			klo = 0
			khi = nz_in + 1
		ELSE
			! Set the hunting increment.
			inc = 1
			! Hunt up since new jlo is either current jlo or larger
			DO
				khi = klo + inc
				IF ( khi > nz_in ) THEN   ! Done hunting, since off end of table.
					khi = nz_in + 1
					EXIT
				ELSE
					IF ( z_interp_height < z_local_height(khi) ) EXIT
					! Not done hunting, so double the increment
					klo = khi
					inc = inc + inc
				ENDIF
			ENDDO
		ENDIF
		! Hunt is done, so begin the final bisection phase:
		DO
			IF ( khi - klo <= 1 ) THEN
				IF ( z_interp_height == z_local_height(nz_in) ) klo = nz_in - 1
				IF ( z_interp_height == z_local_height(1) )     klo = 1
				EXIT
			ELSE
				km = ( khi + klo ) / 2
				IF ( z_interp_height >= z_local_height(km) ) THEN
					klo = km
				ELSE
					khi = km
				ENDIF
			ENDIF
		ENDDO
		!==========
		
		! Assign k00 = klo
		k00 = klo
		
		! Assign km1, k00, kp1, kp2 to comunicate with mono_cubic_interp
		! At this point, 
		! - k00 = 0 [z_interp_height < z_local_height(1)]
		! - k00 = nz_in [z_interp_height > z_local_height(nz_in)]
		! - 1 <= k00 <= nz_in-1
		IF ( k00 == 0 ) THEN
		! interpolation height is below the lowest local height, indicate extrapolation for
		! mono_cubic_interp by setting km1 = nz_in
			km1 = nz_in
			k00 = 1
			kp1 = 2
			kp2 = 3
		ELSE IF ( k00 == nz_in ) THEN
		! interpolation height is above the highest local height, indicate extrapolation for
		! mono_cubic_interp by setting km1 = nz_in
			km1 = nz_in
			k00 = nz_in - 1
			kp1 = nz_in
			kp2 = nz_in
		ELSE IF ( k00 == 1 ) THEN
			km1 = 1
			kp1 = 2
			kp2 = 3
		ELSE IF ( k00 == nz_in-1 ) THEN
			km1 = nz_in - 2
			kp1 = nz_in
			kp2 = nz_in
		ELSE
			km1 = k00 - 1
			kp1 = k00 + 1
			kp2 = k00 + 2
		ENDIF
	END SUBROUTINE find_indices
	
	!-------------------------------------------------------
	
	FUNCTION mono_cubic_interp( z_in, km1, k00, kp1, kp2, zm1, z00, zp1, zp2, fm1, f00, fp1, fp2 ) &
		RESULT( f_out )
	
	! Steffen's monotonic cubic interpolation method
	! Returns monotonic cubic interpolated value between x00 and xp1
	
		IMPLICIT NONE
		
		REAL, INTENT(IN) :: z_in
		INTEGER, INTENT(IN) :: km1, k00, kp1, kp2
		REAL, INTENT(IN) :: zm1, z00, zp1, zp2
		REAL, INTENT(IN) :: fm1, f00, fp1, fp2
		REAL :: f_out
		
		REAL :: hm1, h00, hp1
		REAL :: sm1, s00, sp1
		REAL :: p00, pp1
		REAL :: dfdx00, dfdxp1
		REAL :: c1, c2, c3, c4
		REAL :: w00, wp1
		
		IF ( km1 <= k00 ) THEN
			hm1 = z00 - zm1
			h00 = zp1 - z00
			hp1 = zp2 - zp1
			IF ( km1 == k00 ) THEN
				s00 = ( fp1 - f00 ) / ( zp1 - z00 )
				sp1 = ( fp2 - fp1 ) / ( zp2 - zp1 )
				dfdx00 = s00
				pp1 = ( s00 * hp1 + sp1 * h00 ) / ( h00 + hp1 )
				dfdxp1 = ( SIGN(1.0,s00) + SIGN(1.0,sp1) ) * MIN(ABS(s00),ABS(sp1),0.5*ABS(pp1))
			ELSE IF ( kp1 == kp2 ) THEN
				sm1 = ( f00 - fm1 ) / ( z00 - zm1 )
				s00 = ( fp1 - f00 ) / ( zp1 - z00 )
				p00 = ( sm1 * h00 + s00 * hm1 ) / ( hm1 + h00 )
				dfdx00 = ( SIGN(1.0,sm1) + SIGN(1.0,s00) ) * MIN(ABS(sm1),ABS(s00),0.5*ABS(p00))
				dfdxp1 = s00
			ELSE
				sm1 = ( f00 - fm1 ) / ( z00 - zm1 )
				s00 = ( fp1 - f00 ) / ( zp1 - z00 )
				sp1 = ( fp2 - fp1 ) / ( zp2 - zp1 )
				p00 = ( sm1 * h00 + s00 * hm1 ) / ( hm1 + h00 )
				pp1 = ( s00 * hp1 + sp1 * h00 ) / ( h00 + hp1 )
				dfdx00 = ( SIGN(1.0,sm1) + SIGN(1.0,s00) ) * MIN(ABS(sm1),ABS(s00),0.5*ABS(p00))
				dfdxp1 = ( SIGN(1.0,s00) + SIGN(1.0,sp1) ) * MIN(ABS(s00),ABS(sp1),0.5*ABS(pp1))
			ENDIF
			c1 = ( dfdx00 + dfdxp1 - 2. * s00 ) / ( h00 ** 2 )
			c2 = ( 3. * s00 - 2. * dfdx00 - dfdxp1 ) / h00
			c3 = dfdx00
			c4 = f00
			f_out = c1 * ( (z_in - z00) ** 3 ) + c2 * ( (z_in - z00) ** 2 ) + c3 * (z_in - z00) + c4
		ELSE
			! Linear extrapolation
			wp1 = ( z_in - z00 ) / ( zp1 - z00 )
			w00 = 1. - wp1
			f_out = wp1 * fp1 + w00 * f00
		ENDIF
	
	END FUNCTION mono_cubic_interp
	
	!=====================================================================================
	
	SUBROUTINE wrfles_subsidence( &
		! IN
		n_moist, n_scalar, phb, ph, u, v, theta, moist, scalar, &
		! INOUT
		u_tend, v_tend, t_tend, m_tend, s_tend )
		
	! This subroutine is called in /pth2wrf/dyn_em/module_first_rk_step_part1.F
	! Subsidence is applied between k = 2 and nzm-1 (=kpe-2).
		
		USE module_state_description, ONLY: p_qv
		USE module_model_constants, ONLY: g
#ifdef WRFSTAT
		USE module_statistics, ONLY: docollect_data, wobs, ulsvadv_eta, vlsvadv_eta, tplsvadv_eta, &
		                             qvlsvadv_eta
#endif /*WRFSTAT*/
		
		IMPLICIT NONE
		
		! Input
		INTEGER, INTENT(IN) :: n_moist
		INTEGER, INTENT(IN) :: n_scalar
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: ph    ! perturb geopotential
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: phb   ! base geopotential
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: u
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: v
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: theta   ! = t - t0
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme,n_moist),  INTENT(IN) :: moist 
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme,n_scalar), INTENT(IN) :: scalar
		
		! Input & Output
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: u_tend
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: v_tend
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: t_tend
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme,n_moist),  INTENT(INOUT) :: m_tend
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme,n_scalar), INTENT(INOUT) :: s_tend
		
		! Local
		REAL :: wsub_uv
		REAL, DIMENSION(nzlsf) :: wls
		REAL, DIMENSION(ips:ipe,nzm,jps:jpe) :: wsub   ! interpolated subsidence at scalar level
		REAL, DIMENSION(ips:ipe,nzm,jps:jpe) :: idz    ! inverse of dz at scalar level
		REAL, DIMENSION(ips:ipe,nzm,jps:jpe) :: uwls   ! output for module_statistics.F
		REAL, DIMENSION(ips:ipe,nzm,jps:jpe) :: vwls   ! output for module_statistics.F
		REAL, DIMENSION(ips:ipe,nzm,jps:jpe) :: twls   ! output for module_statistics.F
		REAL, DIMENSION(ips:ipe,nzm,jps:jpe) :: qwls   ! output for module_statistics.F
		INTEGER :: km1, k00, kp1, kp2, khunt, khunt1, khunt2, k1, k2
#ifdef WRFSTAT
		LOGICAL, SAVE :: k4zlsf2zs = .FALSE.
		INTEGER, SAVE, ALLOCATABLE, DIMENSION(:) :: km1zs, k00zs, kp1zs, kp2zs   ! ZLSF => ZS
#endif /*WRFSTAT*/
		INTEGER :: i, j, k, n
		
		IF ( dosubsidence ) THEN
		
		!	PRINT*,'wrfles_subsidence: n1_lsf, n2_lsf = ', n1_lsf, n2_lsf
			
			! Large-scale subsidence at current time
			wls(:) = ( 1. - coef_lsf ) * wlsf(:,n1_lsf) + coef_lsf * wlsf(:,n2_lsf)
			
#ifdef WRFSTAT
			! Prepare output for module_statistics.F
			IF ( docollect_data ) THEN
				! WOBS
				IF ( .NOT.k4zlsf2zs ) THEN
					ALLOCATE( km1zs(nzm), k00zs(nzm), kp1zs(nzm), kp2zs(nzm) )
					khunt = 0
					DO k = 1, nzm
						CALL find_indices(zs(k),zlsf,nzlsf,km1zs(k),k00zs(k),kp1zs(k),kp2zs(k),khunt)
					ENDDO
					k4zlsf2zs = .TRUE.
				ENDIF
				DO k = 1, nzm
					km1 = km1zs(k)
					k00 = k00zs(k)
					kp1 = kp1zs(k)
					kp2 = kp2zs(k)
					wobs(k) = mono_cubic_interp( zs(k), km1, k00, kp1, kp2, &
							zlsf(km1), zlsf(k00), zlsf(kp1), zlsf(kp2), &
							wls(km1),  wls(k00),  wls(kp1),  wls(kp2) )
				ENDDO
			ENDIF
#endif /*WRFSTAT*/
			
			! U & V
			DO j = jps, jpe
				DO i = ips, ipe
					khunt1 = 0
					khunt2 = 0
					DO k = 2, nzm-1
						! U
						CALL find_indices( zu_eta(i,k,j), zlsf, nzlsf, km1, k00, kp1, kp2, khunt1 )
						wsub_uv = mono_cubic_interp( zu_eta(i,k,j), km1, k00, kp1, kp2, &
								zlsf(km1), zlsf(k00), zlsf(kp1), zlsf(kp2), &
								wls(km1),  wls(k00),  wls(kp1),  wls(kp2) )
						IF ( wsub_uv >= 0.0 ) THEN
							k1 = k
							k2 = k-1
						ELSE
							k1 = k+1
							k2 = k
						ENDIF
						uwls(i,k,j) = wsub_uv * ( u(i,k1,j) - u(i,k2,j) ) &
						            / ( zu_eta(i,k1,j) - zu_eta(i,k2,j) )
						u_tend(i,k,j) = u_tend(i,k,j) - uwls(i,k,j)
						! V
						CALL find_indices( zv_eta(i,k,j), zlsf, nzlsf, km1, k00, kp1, kp2, khunt2 )
						wsub_uv = mono_cubic_interp( zv_eta(i,k,j), km1, k00, kp1, kp2, &
								zlsf(km1), zlsf(k00), zlsf(kp1), zlsf(kp2), &
								wls(km1),  wls(k00),  wls(kp1),  wls(kp2) )
						IF ( wsub_uv >= 0.0 ) THEN
							k1 = k
							k2 = k-1
						ELSE
							k1 = k+1
							k2 = k
						ENDIF
						vwls(i,k,j) = wsub_uv * ( v(i,k1,j) - v(i,k2,j) ) &
						            / ( zv_eta(i,k1,j) - zv_eta(i,k2,j) )
						v_tend(i,k,j) = v_tend(i,k,j) - vwls(i,k,j)
					ENDDO
				ENDDO
			ENDDO
			
			! T, M, & S
			DO j = jps, jpe
				DO k = 2, nzm-1
					DO i = ips, ipe
						km1 = km1zlsf(i,k,j)
						k00 = k00zlsf(i,k,j)
						kp1 = kp1zlsf(i,k,j)
						kp2 = kp2zlsf(i,k,j)
						wsub(i,k,j) = mono_cubic_interp( zs_eta(i,k,j), km1, k00, kp1, kp2, &
								zlsf(km1), zlsf(k00), zlsf(kp1), zlsf(kp2), &
								wls(km1),  wls(k00),  wls(kp1),  wls(kp2) )
						IF ( wsub(i,k,j) >= 0.0 ) THEN
							k1 = k
							k2 = k-1
						ELSE
							k1 = k+1
							k2 = k
						ENDIF
						idz(i,k,j) = 1.0 / ( zs_eta(i,k1,j) - zs_eta(i,k2,j) )
						! TP
						twls(i,k,j) = wsub(i,k,j) * ( theta(i,k1,j) - theta(i,k2,j) ) * idz(i,k,j)
						t_tend(i,k,j) = t_tend(i,k,j) - twls(i,k,j)
						! QV
						qwls(i,k,j) = wsub(i,k,j) * ( moist(i,k1,j,p_qv) - moist(i,k2,j,p_qv) )*idz(i,k,j)
						m_tend(i,k,j,p_qv) = m_tend(i,k,j,p_qv) - qwls(i,k,j)
					ENDDO
				ENDDO
			ENDDO
			! M
			DO n = 1, n_moist
				IF ( n /= p_qv ) THEN
					DO j = jps, jpe
						DO k = 2, nzm-1
							DO i = ips, ipe
								IF ( wsub(i,k,j) >= 0.0 ) THEN
									k1 = k
									k2 = k-1
								ELSE
									k1 = k+1
									k2 = k
								ENDIF
								m_tend(i,k,j,n) = m_tend(i,k,j,n) &
								         - wsub(i,k,j) * ( moist(i,k1,j,n) - moist(i,k2,j,n) ) * idz(i,k,j)
							ENDDO
						ENDDO
					ENDDO
				ENDIF
			ENDDO
			! S
			DO n = 1, n_scalar
				DO j = jps, jpe
					DO k = 2, nzm-1
						DO i = ips, ipe
							IF ( wsub(i,k,j) >= 0.0 ) THEN
								k1 = k
								k2 = k-1
							ELSE
								k1 = k+1
								k2 = k
							ENDIF
							s_tend(i,k,j,n) = s_tend(i,k,j,n) &
							          - wsub(i,k,j) * ( scalar(i,k1,j,n) - scalar(i,k2,j,n) ) * idz(i,k,j)
						ENDDO
					ENDDO
				ENDDO
			ENDDO
			
#ifdef WRFSTAT
			! Collect output for module_statistics.F
			IF ( docollect_data ) THEN
				DO j = jps, jpe
					DO k = 1, nzm-1
						DO i = ips, ipe
							ulsvadv_eta(i,k,j) = - uwls(i,k,j)    ! m/s2
							vlsvadv_eta(i,k,j) = - vwls(i,k,j)    ! m/s2
							tplsvadv_eta(i,k,j) = - twls(i,k,j)   ! K/s for TP
							qvlsvadv_eta(i,k,j) = - qwls(i,k,j)   ! kg/kg/s
						ENDDO
					ENDDO
				ENDDO
			ENDIF
#endif /*WRFSTAT*/
			
		ENDIF   ! dosubsidence
		
	END SUBROUTINE wrfles_subsidence
	
	!=====================================================================================
	
	SUBROUTINE wrfles_upperbound( &
		! IN
		ph, phb, &
		! INOUT
		tp, qv, &
		! INDICES (IN)
		its, ite, jts, jte, kts, kte )
	
	! This subroutine is called in /path2wrf/dyn_em/solve_em.F.
	!
	! This subroutine deals with the upper boundary for potential temperature and water vapor
	! mixing ratio in order to avoid problems that happens at the upper boundary, e.g., depletion
	! of water vapor due to subsidence.
	!
	! This subroutine maintains the potential temperature and water vapor mixing ratio at the
	! highest level by using the vertical gradient of one layer below
		
		IMPLICIT NONE
		
		! Input
		INTEGER, INTENT(IN) :: its, ite, jts, jte, kts, kte
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: ph    ! perturb geopotential
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: phb   ! base geopotential
		
		! Input & output
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: tp   ! potential temperature
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: qv   ! water vapor mixing ratio
		
		! Local
		INTEGER :: i, j, k
		REAL :: z00, zm1, zm2, dfdz
		
		IF ( doupperbound ) THEN
			! Maintain vertical gradient
			DO j = jts, jte
				DO i = its, ite
					! Note: z00, zm1, zm2 are actually geopotential (gz), and dfdz is 1/g*df/dz.
					!       dfdz will be multiplied by geopotential so g is canceled.
					k = kte
					z00 = 0.5 * ( ph(i,k+1,j) + phb(i,k+1,j) + ph(i,k,j) + phb(i,k,j) )
					zm1 = 0.5 * ( ph(i,k,j) + phb(i,k,j) + ph(i,k-1,j) + phb(i,k-1,j) )
					zm2 = 0.5 * ( ph(i,k-1,j) + phb(i,k-1,j) + ph(i,k-2,j) + phb(i,k-2,j) )
					dfdz = ( tp(i,k-1,j) - tp(i,k-2,j) ) / ( zm1 - zm2 )
					tp(i,k,j) = tp(i,k-1,j) + dfdz * ( z00 - zm1 )
					dfdz = ( qv(i,k-1,j) - qv(i,k-2,j) ) / ( zm1 - zm2 )
					qv(i,k,j) = qv(i,k-1,j) + dfdz * ( z00 - zm1 )
				ENDDO
			ENDDO
		ENDIF
		
	END SUBROUTINE wrfles_upperbound
	
	!============== MPI utility copied form SAM6.8.2/SRC/task_util_MPI.f90 ===============
	SUBROUTINE task_init
		USE module_dm, ONLY: local_communicator
		IMPLICIT NONE
		INCLUDE 'mpif.h'
		INTEGER :: ierr
#ifndef DM_PARALLEL
		dompi = .FALSE.
		rank = 0
		nsubdomains = 1
#else
		mpi_comm = local_communicator
		dompi = .TRUE.
		CALL MPI_COMM_RANK(mpi_comm, rank, ierr)
		CALL MPI_COMM_SIZE(mpi_comm, nsubdomains, ierr)
#endif
	END SUBROUTINE task_init
	!-------------------------------------------------------
	SUBROUTINE task_abort
		IMPLICIT NONE
		INCLUDE 'mpif.h'
		INTEGER :: ierr, rc
		IF (dompi) THEN
			CALL MPI_ABORT(mpi_comm, rc, ierr)
			CALL MPI_FINALIZE(ierr)
		ENDIF
		CALL EXIT(999)   ! to avolid resubmission when finished
	END SUBROUTINE task_abort
	!-------------------------------------------------------
	SUBROUTINE task_sum_real(buffer_in, buffer_out, length)
		IMPLICIT NONE
		INCLUDE 'mpif.h'
		INTEGER, INTENT(IN) :: length   ! buffers' length
		REAL, DIMENSION(length), INTENT(IN)  :: buffer_in
		REAL, DIMENSION(length), INTENT(OUT) :: buffer_out   ! buffer of data
		INTEGER :: ierr
		CALL MPI_ALLREDUCE(buffer_in, buffer_out, length, MPI_REAL, MPI_SUM, mpi_comm, ierr)
	END SUBROUTINE task_sum_real
	!-------------------------------------------------------
	SUBROUTINE task_sum_real8(buffer_in, buffer_out, length)
		IMPLICIT NONE
		INCLUDE 'mpif.h'
		INTEGER, INTENT(IN) :: length
		DOUBLE PRECISION, DIMENSION(length), INTENT(IN)  :: buffer_in
		DOUBLE PRECISION, DIMENSION(length), INTENT(OUT) :: buffer_out
		INTEGER :: ierr
		CALL MPI_ALLREDUCE(buffer_in,buffer_out,length,MPI_DOUBLE_PRECISION,MPI_SUM,mpi_comm,ierr)
	END SUBROUTINE task_sum_real8
	!-------------------------------------------------------
	SUBROUTINE task_max_real(buffer_in, buffer_out, length)
		IMPLICIT NONE
		INCLUDE 'mpif.h'
		INTEGER, INTENT(IN) :: length
		REAL, DIMENSION(length), INTENT(IN)  :: buffer_in
		REAL, DIMENSION(length), INTENT(OUT) :: buffer_out
		INTEGER :: ierr
		CALL MPI_ALLREDUCE(buffer_in, buffer_out, length, MPI_REAL, MPI_MAX, mpi_comm, ierr)
	END SUBROUTINE task_max_real
	!-------------------------------------------------------
	SUBROUTINE task_min_real(buffer_in, buffer_out, length)
		IMPLICIT NONE
		INCLUDE 'mpif.h'
		INTEGER, INTENT(IN) :: length
		REAL, DIMENSION(length), INTENT(IN) ::  buffer_in
		REAL, DIMENSION(length), INTENT(OUT) :: buffer_out
		INTEGER :: ierr
		CALL MPI_ALLREDUCE(buffer_in, buffer_out, length, MPI_REAL, MPI_MIN, mpi_comm, ierr)
	END SUBROUTINE task_min_real
	!=====================================================================================
	
END MODULE module_wrfles
#endif /*WRFLES*/
