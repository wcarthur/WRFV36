!WRF:MODEL_LAYER:PHYSICS
!
MODULE module_mp_example
CONTAINS
	SUBROUTINE mp_example( &
		! IN
		..., &
		! INOUT
		..., &
		! OUT
		..., &
		! INDICES
		ids, ide, jds, jde, kds, kde, &   ! domain dims
		ims, ime, jms, jme, kms, kme, &   ! memory dims
		its, ite, jts, jte, kts, kte )    ! tile dims
		
! Templete how to collect precipitation, evaporation, condensation rates and
! pass to module_statistics.F

! 1. Add the USE statement
#ifdef WRFSTAT
		! Tak Yamaguchi 2012/02
		USE module_statistics, ONLY: docollect_data, doavgprecflux, &
			precipl_eta, &   ! liquid precipitation rate (mm/s)
			precipi_eta, &   ! ice precipitation rate (mm/s)
			vfmi_eta, &      ! mass-weighted ice particle fall speed (m/s)
			cond_rate_eta, & ! condensation rate (kg/kg/s)
			evap_rate_eta, & ! evaporation rate (kg/kg/s) positively defined
			deps_rate_eta, & ! deposition rate (kg/kg/s)
			subl_rate_eta    ! sublimation rate (kg/kg/s) positively defined
#endif /*WRFSTAT*/

! 2. Allocate temporal 3D array to calculate output variables.
!    Set value to 0 every time the scheme is called.
#ifdef WRFSTAT
		! Tak Yamaguchi 2012/02 - Temporal array to hold output variables for module_statistics.F
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme) :: precl3d, preci3d, vfmi3d
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme) :: cond3d, evap3d, deps3d, subl3d
		! Initialize array
		precl3d(its:ite,kts:kte,jts:jte) = 0.
		preci3d(its:ite,kts:kte,jts:jte) = 0.
		vfmi3d(its:ite,kts:kte,jts:jte) = 0.
		cond3d(its:ite,kts:kte,jts:jte) = 0.
		evap3d(its:ite,kts:kte,jts:jte) = 0.
		deps3d(its:ite,kts:kte,jts:jte) = 0.
		subl3d(its:ite,kts:kte,jts:jte) = 0.
#endif /*WRFSTAT*/

! 3. Compute output variables and store them into the temporal arrays
		precl3d(i,k,j) =
		preci3d(i,k,j) =
		vfmi3d(i,k,j) =
		cond3d(i,k,j) =
		evap3d(i,k,j) =
		deps3d(i,k,j) =
		subl3d(i,k,j) =
		
! 4. Pass the output to module_statistics.F
#ifdef WRFSTAT
		! Tak Yamaguchi 2012/02 - Collect data for module_statistics.F
		IF ( doavgprecflux ) THEN
			DO j = jts, jte
				DO k = kts, kte
					DO i = its, ite
						precipl_eta(i,k,j) = precipl_eta(i,k,j) + precl3d(i,k,j)   ! mm/s
						precipi_eta(i,k,j) = precipi_eta(i,k,j) + preci3d(i,k,j)   ! mm/s
					ENDDO
				ENDDO
			ENDDO
		ENDIF
		IF ( docollect_data ) THEN
			DO i = its, ite
				DO j = jts, jte
					DO k = kts, kte
						vfmi_eta(i,k,j) = vfmi3d(i,k,j)   ! m/s
						cond_rate_eta(i,k,j) =   cond3d(i,k,j)   ! kg/kg/s
						evap_rate_eta(i,k,j) = - evap3d(i,k,j)   ! kg/kg/s, positively defined
						deps_rate_eta(i,k,j) =   deps3d(i,k,j)   ! kg/kg/s
						subl_rate_eta(i,k,j) = - subl3d(i,k,j)   ! kg/kg/s, positively defined
					ENDDO
				ENDDO
			ENDDO
			IF ( .NOT.doavgprecflux ) THEN
				DO i = its, ite
					DO j = jts, jte
						DO k = kts, kte
							precipl_eta(i,k,j) = precl3d(i,k,j)
							precipi_eta(i,k,j) = preci3d(i,k,j)
						ENDDO
					ENDDO
				ENDDO
			ENDIF
		ENDIF
#endif /*WRFSTAT*/

	END SUBROUTINE mp_example
END MODULE module_mp_example
