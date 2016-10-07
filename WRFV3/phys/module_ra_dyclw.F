!WRF:MODEL_LAYER:PHYSICS
!
#ifdef WRFLES
! Tak Yamaguchi 2012/03 WRFLES
MODULE module_ra_dyclw

! This code is originally developed by H. Wang.

	! Parameters
	REAL, PARAMETER :: fr0=70.
	REAL, PARAMETER :: fr1=22.
	REAL, PARAMETER :: xka=85.
	REAL, PARAMETER :: cpd=1004.
	
	! DYCOMS_RF01 values
	REAL, PARAMETER :: div=3.75e-6
	REAL, PARAMETER :: qiv=8.e-3

CONTAINS

	SUBROUTINE DYCLW ( RTHRATEN, qv, qc, qr, rho, z, dz8w, &
	                   itimestep, dt, t, pii, &
	                   ids, ide, jds, jde, kds, kde, &
	                   ims, ime, jms, jme, kms, kme, &
	                   its, ite, jts, jte, kts, kte )
#ifdef WRFSTAT
		! Tak Yamaguchi 2012/03
		USE module_statistics, ONLY: docollect_data, radlwup_eta
#endif /*WRFSTAT*/
		IMPLICIT NONE
		INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
		                       ims, ime, jms, jme, kms, kme, &
		                       its, ite, jts, jte, kts, kte
		INTEGER, INTENT(IN) :: itimestep
		REAL, INTENT(IN) :: dt
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: RTHRATEN
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: dz8w, z, qv, qc, qr, t, pii, rho
		REAL, DIMENSION(kts:kte) :: LWPZ, LWPT, dzt, fx, fx0, fx1, fx2, qt
		INTEGER :: i, j, k, kiv
		REAL :: zi, fact, delta, ql
		
		j_loop: DO J=jts,jte
			i_loop: DO I=its,ite
			
			!	DO K=1,kte-1
			!		DZT(K)=1./(Z(I,K+1,J)-Z(I,K,J))
			!	ENDDO
			!	DZT(kte)=1./(Z(I,kte,J)-Z(I,kte-1,J))
			
				LWPZ(1)=0.   ! LWP from surface to z=1
				! the second term (surface to z)
				DO K=2,kte
					ql=qc(i,k,j)+qr(i,k,j)
					LWPZ(k)=LWPZ(k-1) + max( 0.,rho(i,k,j)*ql*dz8w(i,k,j) )
					fx1(k)=fr1*exp( -1.*xka*LWPZ(k) )
					
					qt(k)=ql+qv(i,k,j)
					IF (qt(k).GE.qiv) kiv=k   ! the largest kiv is at the inversion height
				ENDDO
				qt(1)=qv(i,1,j)+qc(i,1,j)+qr(i,1,j)
				fx1(1)=fr1
				
				! zi for the third term
				delta=qt(kiv+1)-qt(kiv)
				IF (delta.NE.0.) THEN
					zi=z(i,kiv,j)+(z(i,kiv+1,j)-z(i,kiv,j))*(qiv-qt(kiv))/delta
				ELSE
					zi=z(i,kiv,j)
				ENDIF
				fact=rho(i,kiv,j)*cpd*div
				
				! the first term (z to ztop) and the third term z>zi
				DO K=1,kte
					LWPT(k)=LWPZ(kte)-LWPZ(k)
					fx0(k)=fr0*exp( -1.*xka*LWPT(k) )
					fx2(k)=0.
					IF (z(i,k,j).GT.zi .AND. fact.GT.0.) THEN
						fx2(k)=fact*( 0.25*(z(i,k,j)-zi)**(4./3.) + zi*(z(i,k,j)-zi)**(1./3.) )
					ENDIF
					fx(k)=fx0(k)+fx1(k)+fx2(k)   ! the net LW flux
				ENDDO
				
				! the LW heating rate
				DO K=2,kte-1
					rthraten(i,k,j)=rthraten(i,k,j)-(fx(k)-fx(k-1))/(cpd*rho(i,k,j)*dz8w(i,k,j)) &
					               / pii(i,k,j)
					! Tak Yamaguchi 2012/02 - Divide by inverse of the exner function
					! According to the DYCOMS-II RF01 intercomparison webpage, the exner function is
					! assumed to be 1, but this will cause inconsistency in module_statistics.F where
					! multiplies by the exner function for conversion from potential temperature to
					! absolute temperature. Also, this way is energetically consistent.
					
					! print out for verification
				!	IF ( (itimestep.EQ.100 .OR. itimestep.EQ.300) &
				!	   .AND. i.LE.(its+5) .AND. j.LE.(jts+5) ) THEN
				!		PRINT*,k,zi,LWPZ(k),LWPT(k),fx(k),rthraten(i,k,j)
				!	ENDIF
				ENDDO
				rthraten(i,kte,j)=rthraten(i,kte-1,j)
				rthraten(i,1,j)=rthraten(i,2,j)
				
#ifdef WRFSTAT
				! Tak Yamaguchi 2012/03 - Collect output for module_statistics.F
				IF ( docollect_data ) THEN
					DO k = 1, kte
						radlwup_eta(i,k,j) = fx(k)
					ENDDO
				ENDIF
#endif /*WRFSTAT*/
				
			ENDDO i_loop
		ENDDO j_loop
		
	END SUBROUTINE DYCLW
	
END MODULE module_ra_dyclw
#endif /*WRFLES*/
