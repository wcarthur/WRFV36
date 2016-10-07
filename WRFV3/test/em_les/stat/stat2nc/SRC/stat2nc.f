c-------------------------------------------------------
	implicit none
	include 'netcdf.inc'
	include 'hbuf.inc'
	
	integer flag(HBUF_MAX_LENGTH)
c-------------------------------------------------------	
	character caseid*40, filename*148, filename2*148
	character name*80
	integer nzm, ntime, npar
	real time(50000), z(5000), p(5000), dz(5000)
	real f(5000000), f1(5000000), parms(5000000)
	real tmp(5000), tmp1(5000), tmp2(5000), tmp3(5000)
	real rho(5000)
	real cape,cin

	integer i, j, k, l, m, nparms
	integer vdimids(2), ndimids
	integer ncid, err, zid, timeid, varid
	character(80) long_name
	character(15) abbr_name
	character(12) units

	integer iargc
	external iargc

	time=0.
	z=0.
	p=0.
	f=0.
c-------------------------------------------------------	
	m=iargc()
	if(m.eq.0.) then
	 print*,'you forgot to specify the name of the file.'
	 stop
	endif
	call getarg(1,filename)
	print *,'open file: ',filename
	open(2,file=filename,status='old',form='unformatted')

	do i=1,144
	 if(filename(i:i+4).eq.'.stat') then
	  filename2=filename(1:i-1)//'.nc'
	  print*,filename2
	 endif
	end do
c-------------------------------------------------------	
c Count how many time points in a file:
	call HBUF_info(2,ntime,time,nzm,z,p,caseid)
	call HBUF_read(2,nzm,'RHO',1,1,rho,m)
	print*,'.......',ntime
	print*,(time(k),k=1,ntime/3)
	print*
	print*,(z(k),k=1,nzm)
	print*
	print*,(p(k),k=1,nzm)
	print*
	print*,(rho(k),k=1,nzm)

	dz(1) = 0.5*(z(1)+z(2))
	do k=2,nzm-1
	 dz(k) = 0.5*(z(k+1)-z(k-1))
	end do
	dz(nzm) = dz(nzm-1)
	
	err = NF_CREATE(filename2, NF_CLOBBER, ncid)

	err = NF_REDEF(ncid)

!	err = NF_PUT_ATT_TEXT(ncid,NF_GLOBAL,'model',
!     &            19,'CSU CEM version 1.0')
	err = NF_PUT_ATT_TEXT(ncid,NF_GLOBAL,'caseid',
     &                                  len_trim(caseid),caseid)

	err = NF_DEF_DIM(ncid, 'z', nzm, zid)
	err = NF_DEF_DIM(ncid, 'time', ntime, timeid)

	err = NF_DEF_VAR(ncid, 'z', NF_FLOAT, 1, zid, varid)
	err = NF_PUT_ATT_TEXT(ncid,varid,'units',1,'m')
	err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',6,'height')

	err = NF_DEF_VAR(ncid, 'time', NF_FLOAT, 1, timeid, varid)
	err = NF_PUT_ATT_TEXT(ncid,varid,'units',3,'day')
	err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',4,'time')

	err = NF_DEF_VAR(ncid, 'p', NF_FLOAT, 1, zid, varid)
	err = NF_PUT_ATT_TEXT(ncid,varid,'units',2,'mb')
	err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',8,'pressure')

	err = NF_ENDDEF(ncid)

	err = NF_INQ_VARID(ncid,'time',varid)
	err = NF_PUT_VAR_REAL(ncid, varid, time)

	err = NF_INQ_VARID(ncid,'z',varid)
	err = NF_PUT_VAR_REAL(ncid, varid, z)

	err = NF_INQ_VARID(ncid,'p',varid)
	err = NF_PUT_VAR_REAL(ncid, varid, p)
!
!  1-D fields now:
!
	call HBUF_parms(2,parms,nparms) ! CHANGE nparams IN THIS SUBROUTINE IF THE LAST npar IS CHANGED!
	call HBUF_read(2,nzm,'RHO',1,1,rho,m)
c--------------------------------------------------------
        long_name = 'SST'
        abbr_name = 'SST'
        units = ' '
	npar = 1
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c--------------------------------------------------------
        long_name = 'Surface pressure'
        abbr_name = 'Ps'
        units = ' '
	npar = 2
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c--------------------------------------------------------
        long_name = 'Shaded cloud fraction'
        abbr_name = 'CLDSHD'
        units = ' '
	npar = 3
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c--------------------------------------------------------
        long_name = 'Low cloud fraction'
        abbr_name = 'CLDLOW'
        units = ' '
	npar = 4
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c--------------------------------------------------------
        long_name = 'Middle cloud fraction'
        abbr_name = 'CLDMID'
        units = ' '
	npar = 5
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c--------------------------------------------------------
        long_name = 'High cloud fraction'
        abbr_name = 'CLDHI'
        units = ' '
	npar = 6
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c--------------------------------------------------------
        long_name = 'Cloud fraction above 245K level'
        abbr_name = 'CLD245'
        units = ' '
	npar = 7
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c--------------------------------------------------------
        long_name = 'Surface precip. fraction'
        abbr_name = 'AREAPREC'
        units = ' '
	npar = 8
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c----------------------------------------------------------
        long_name = 'Surface precipitation'
        abbr_name = 'PREC'
        units = 'mm/day'
	npar = 9
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c----------------------------------------------------------
        long_name = 'Surface liquid precipitation'
        abbr_name = 'PRECL'
        units = 'mm/day'
	npar = 10
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c----------------------------------------------------------
        long_name = 'Surface ice precipitation'
        abbr_name = 'PRECI'
        units = 'mm/day'
	npar = 11
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
!c--------------------------------------------------------
!        long_name = 'Convective Precipitation Rate'
!        abbr_name = 'PRECC'
!        units = 'mm/day'
!	npar = 12
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
!c--------------------------------------------------------
!        long_name = 'Stratiform Precipitation Rate'
!        abbr_name = 'PRECS'
!        units = 'mm/day'
!	npar = 13
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
c----------------------------------------------------------
        long_name = 'Precipitable water'
        abbr_name = 'PW'
        units = 'mm'
	npar = 14
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c----------------------------------------------------------
!        long_name = 'Observed Precipitable Water'
!        abbr_name = 'PWOBS'
!        units = 'mm'
!        call HBUF_read(2,nzm,'QVOBS',1,ntime,f,m)
!        do i=1,ntime
!         tmp(i)=0.
!         do k=1,nzm-1
!          tmp(i)=tmp(i)+rho(k)*f(k+(i-1)*nzm)*dz(k)
!         end do
!         tmp(i)=tmp(i)*1.e-3
!        end do
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
c----------------------------------------------------------
        long_name = 'Cloud water path'
        abbr_name = 'CWP'
        units = 'g/m2'
	npar = 15
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c----------------------------------------------------------
        long_name = 'Cloud ice water path'
        abbr_name = 'IWP'
        units = 'g/m2'
	npar = 16
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c----------------------------------------------------------
        long_name = 'Rain water path'
        abbr_name = 'RWP'
        units = 'g/m2'
	npar = 17
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c----------------------------------------------------------
        long_name = 'Snow water path'
        abbr_name = 'SWP'
        units = 'g/m2'
	npar = 18
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c----------------------------------------------------------
        long_name = 'Grauple water path'
        abbr_name = 'GWP'
        units = 'g/m2'
	npar = 19
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c----------------------------------------------------------
        long_name = 'Variance of liquid water path (CWP+RWP)'
        abbr_name = 'LWP2'
        units = 'g2/m4'
	npar = 20
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c----------------------------------------------------------
        long_name = 'Variance of ice water path (IWP+SWP+GWP)'
        abbr_name = 'IWP2'
        units = 'g/m2'
	npar = 21
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c--------------------------------------------------------
        long_name = 'Maximum updraft velocity'
        abbr_name = 'WMAX'
        units = 'm/s'
	npar = 22
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c--------------------------------------------------------
        long_name = 'Maximum downdraft velocity'
        abbr_name = 'WMIN'
        units = 'm/s'
	npar = 23
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c--------------------------------------------------------
        long_name = 'Maximum X wind velocity'
        abbr_name = 'UMAX'
        units = 'm/s'
	npar = 24
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c--------------------------------------------------------
        long_name = 'Maximum negative X wind velocity'
        abbr_name = 'UMIN'
        units = 'm/s'
	npar = 25
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c--------------------------------------------------------
        long_name = 'Maximum Y wind velocity'
        abbr_name = 'VMAX'
        units = 'm/s'
	npar = 26
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c--------------------------------------------------------
        long_name = 'Maximum negative Y wind velocity'
        abbr_name = 'VMIN'
        units = 'm/s'
	npar = 27
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c----------------------------------------------------------
        long_name = 'Latent heat flux'
        abbr_name = 'LHF'
        units = 'W/m2'
	npar = 28
        !call HBUF_read(2,nzm,'QTFLUXS',1,ntime,f,m)
        !do i=1,ntime
        ! tmp(i)=f(1+(i-1)*nzm)
        !end do
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name
c----------------------------------------------------------
        long_name = 'Sensible heat flux'
        abbr_name = 'SHF'
        units = 'W/m2'
	npar = 29
        !call HBUF_read(2,nzm,'LWSEFLUXS',1,ntime,f,m)
        !do i=1,ntime
        ! tmp(i)=f(1+(i-1)*nzm)
        !end do
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,long_name



!c----------------------------------------------------------
!        long_name = 'CAPE'
!        abbr_name = 'CAPE'
!        units = 'J/kg'
!        call HBUF_read(2,nzm,'TABS',1,ntime,f,m)
!        call HBUF_read(2,nzm,'QV',1,ntime,f1,m)
!        do i=1,ntime
!         tmp(1:nzm)=f(1+(i-1)*nzm:nzm+(i-1)*nzm)
!         tmp1(1:nzm)=f1(1+(i-1)*nzm:nzm+(i-1)*nzm)
!         tmp2(i) =  cape(nzm,p(1:nzm),tmp(1:nzm),tmp1(1:nzm))
!         tmp3(i) =  cin(nzm,p(1:nzm),tmp(1:nzm),tmp1(1:nzm))
!          print*,'CAPE: CIN:',tmp2(i), tmp3(i)
!        end do
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp2)
!        print*,long_name
!c----------------------------------------------------------
!        long_name = 'CAPEOBS'
!        abbr_name = 'CAPEOBS'
!        units = 'J/kg'
!        call HBUF_read(2,nzm,'TABSOBS',1,ntime,f,m)
!        call HBUF_read(2,nzm,'QVOBS',1,ntime,f1,m)
!        do i=1,ntime
!         tmp(1:nzm)=f(1+(i-1)*nzm:nzm+(i-1)*nzm)
!         tmp1(1:nzm)=f1(1+(i-1)*nzm:nzm+(i-1)*nzm)
!         tmp2(i) =  cape(nzm,p(1:nzm),tmp(1:nzm),tmp1(1:nzm))
!         tmp3(i) =  cin(nzm,p(1:nzm),tmp(1:nzm),tmp1(1:nzm))
!	  print*,'CAPEOBS: CINOBS:',tmp2(i), tmp3(i)
!        end do
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp2)
!        print*,long_name
!c----------------------------------------------------------
!        long_name = 'CIN'
!        abbr_name = 'CIN'
!        units = 'J/kg'
!        call HBUF_read(2,nzm,'TABS',1,ntime,f,m)
!        call HBUF_read(2,nzm,'QV',1,ntime,f1,m)
!        do i=1,ntime
!         tmp(1:nzm)=f(1+(i-1)*nzm:nzm+(i-1)*nzm)
!         tmp1(1:nzm)=f1(1+(i-1)*nzm:nzm+(i-1)*nzm) 
!         tmp2(i) =  cin(nzm,p(1:nzm),tmp(1:nzm),tmp1(1:nzm))
!        end do
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp2)
!        print*,long_name
!c----------------------------------------------------------
!        long_name = 'CINOBS'
!        abbr_name = 'CINOBS'
!        units = 'J/kg'
!        call HBUF_read(2,nzm,'TABSOBS',1,ntime,f,m)
!        call HBUF_read(2,nzm,'QVOBS',1,ntime,f1,m)
!        do i=1,ntime
!         tmp(1:nzm)=f(1+(i-1)*nzm:nzm+(i-1)*nzm)
!         tmp1(1:nzm)=f1(1+(i-1)*nzm:nzm+(i-1)*nzm) 
!         tmp2(i) =  cin(nzm,p(1:nzm),tmp(1:nzm),tmp1(1:nzm))
!        end do
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp2)
!        print*,long_name
c--------------------------------------------------------
!!!!!	if(nparms.gt.16) then
!        long_name = 'Net LW flux at sfc'
!        abbr_name = 'LWNS'
!        units = 'W/m2'
!	npar = 30
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
!c--------------------------------------------------------
!        long_name = 'Net LW flux at TOA'
!        abbr_name = 'LWNT'
!        units = 'W/m2'
!	npar = 31
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
!c--------------------------------------------------------
!        long_name = 'Net LW flux at sfc (Clear Sky)'
!        abbr_name = 'LWNSC'
!        units = 'W/m2'
!	npar = 32
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
!c--------------------------------------------------------
!        long_name = 'Net LW flux at TOA (Clear Scy)'
!        abbr_name = 'LWNTC'
!        units = 'W/m2'
!	npar = 33
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
!c--------------------------------------------------------
!        long_name = 'Downward LW flux at sfc'
!        abbr_name = 'LWDS'
!        units = 'W/m2'
!	npar = 34
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
!c--------------------------------------------------------
!        long_name = 'Net SW flux at sfc'
!        abbr_name = 'SWNS'
!        units = 'W/m2'
!	npar = 35
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
!c--------------------------------------------------------
!        long_name = 'Net SW flux at TOA'
!        abbr_name = 'SWNT'
!        units = 'W/m2'
!	npar = 36
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
!c--------------------------------------------------------
!        long_name = 'Net SW flux at sfc (Clear Sky)'
!        abbr_name = 'SWNSC'
!        units = 'W/m2'
!	npar = 37
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
!c--------------------------------------------------------
!        long_name = 'Net SW flux at TOA (Clear Scy)'
!        abbr_name = 'SWNTC'
!        units = 'W/m2'
!	npar = 38
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
!c--------------------------------------------------------
!        long_name = 'Downward SW flux at sfc'
!        abbr_name = 'SWDS'
!        units = 'W/m2'
!	npar = 39
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
!c--------------------------------------------------------
!        long_name = 'Incoming SW flux at TOA'
!        abbr_name = 'SOLIN'
!        units = 'W/m2'
!	npar = 40
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
!!!!!	end if
c--------------------------------------------------------------
        ndimids=2
        vdimids(1) = zid
        vdimids(2) = timeid

        do k=1,hbuf_length

           write(6,'(a72)') deflist(k)
           call HBUF_read(2,nzm,namelist(k),1,ntime,f,m)

           err = NF_REDEF(ncid)
           name = namelist(k)
           l=len_trim(name)
           err = NF_DEF_VAR(ncid,name(1:l),NF_FLOAT,
     &                                  ndimids,vdimids,varid)
           err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                                len_trim(deflist(k)),deflist(k))
           err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                              len_trim(unitlist(k)),unitlist(k))
           err = NF_PUT_ATT_REAL(ncid,varid,'missing_value',NF_FLOAT,
     &                              1,-9999.)
           err = NF_ENDDEF(ncid)
           err = NF_PUT_VAR_REAL(ncid, varid, f)
           if(err.ne.0) print*,'error:',err

        end do

	err = NF_CLOSE(ncid)

	end
