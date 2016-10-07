c
c hbuf_info - return the number of time points, time point values,
c and some other useful information

	subroutine HBUF_info(unit,ntime,time,nzm,z,p,caseid)

	implicit none
	include 'hbuf.inc'

!  input:
	integer unit	! data file device unit-number
!  output:
        integer ntime	! number of time points	
	real time(*)	! time points (days)
	integer nzm	! number of vertical levels
	real z(*)	! vertical levels (m)
	real p(*)	! pressure levels(mb)
	character*(*) caseid
!  local
	integer n,k,nstep
	real f
	
        ntime = 0
	rewind(unit)
        do while(.true.)
         read(unit,end=111)  caseid
         ntime=ntime+1
         read (unit) time(ntime),f,nstep,n,n,n,nzm,f,f,f,
     &          (z(k),k=1,nzm),(z(k),k=1,nzm),f,(p(k),k=1,nzm)
         print*,'NSTEP=',nstep
         read (unit) hbuf_length
         do k = 1,hbuf_length
               read(unit) namelist(k)
               read(unit) deflist(k)
               read(unit) unitlist(k)
               read(unit)
         end do
        end do
 111    continue
	rewind(unit)
	print*,'HBUF_LENGTH=',hbuf_length
	return
	end
c
c hbuf_parms - return several parameters that are recorded in
c the header of the hbuffer

	subroutine HBUF_parms(unit,parms,nparms)
	implicit none
	include 'hbuf.inc'

!  input:
	integer unit	! data file device unit-number
!  output:
	real parms(*)	! array with some parameters
	integer nparms	! number of parameters in parms
!  local
	integer ntime,n,k,nzm
	real f
	real tmp(5000)	! time points (days)

c============================
	nparms = 40
c===========================

	ntime = 0
	rewind(unit)
	read (unit)
        read (unit) f,f,n,n,n,n,nzm
	print*,'nzm=',nzm
	rewind(unit)
        do while(.true.)
         read(unit,end=111)  
         ntime=ntime+1
         read (unit,err=555) f,f,n,n,n,n,nzm,f,f,f,
     &          (tmp(k),k=1,nzm),(tmp(k),k=1,nzm),f,(tmp(k),k=1,nzm),
     &		(parms(k+nparms*(ntime-1)),k=1,nparms)
	 goto 666
 555	 nparms=8  ! old dataset
	 backspace(unit)
	 read (unit,err=555) f,f,n,n,n,n,nzm,f,f,f,
     &          (tmp(k),k=1,nzm),(tmp(k),k=1,nzm),f,(tmp(k),k=1,nzm),
     &          (parms(k+nparms*(ntime-1)),k=1,nparms)
 666	 read (unit) hbuf_length
         do k = 1,hbuf_length
               read(unit) 
               read(unit) 
               read(unit) 
               read(unit)
         end do
        end do
 111    continue
	print*,'nparms=',nparms
	rewind(unit)
	return
	end


c
c Given a name of the parameter (from the namelist), 
c extract (ntimeend-ntimestart+1) vertical profiles
c starting from time-point ntimestart and ending at 
c time-point ntimeend.

	subroutine HBUF_read
     &            (unit,nzm,name,ntimestart,ntimeend,f,nread)
	implicit none
!   input:
	integer unit ! data file device unit-number
	integer nzm  ! number of vertical levels
	character *(*) name ! name of the parameter
	integer ntimestart ! starting time point number
	integer ntimeend   ! ending time point number
!   output:
	real f(*) ! data buffer
	integer nread ! number of time-points actually read
!   local:
	integer ntime,k,l,hbuf_length
	character *15 namelist

	rewind(unit)
	ntime=0
	nread=0
        do while(.true.)
         read(unit,end=111)
	 ntime=ntime+1  
         read (unit)
         read (unit) hbuf_length
         do l = 1,hbuf_length
               read(unit) namelist
               read(unit) 
               read(unit) 
          if((.not.(lgt(name,namelist).or.llt(name,namelist)))
     &	     .and.ntime.ge.ntimestart.and.ntime.le.ntimeend) then
	        nread=nread+1
     		read(unit) (f(k+(nread-1)*nzm),k=1,nzm) 
          else
                read(unit)
	  endif
         end do
        end do
 111    continue
	rewind(unit)
	if(nread.eq.0) then
	   print*,TRIM(name)//': no data read by HBUF_read.'
	   f(1:(ntimeend-ntimestart+1)*nzm) = 0.
	end if
	return
	end

