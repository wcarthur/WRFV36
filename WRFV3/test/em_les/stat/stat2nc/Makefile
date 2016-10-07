# Makefile to compile conversion utilities. You need to set include and library paths for NETCDF
#

PLATFORM := $(shell uname -s)

# uncomment line for your platform or add your own:

#---------------------------------------------------------
# AIX 

ifeq ($(PLATFORM),AIX)

LIB_NETCDF = /usr/local/xlf/lib
INC_NETCDF = /usr/local/xlf/include

FF = xlf90_r -qfixed -I$(INC_NETCDF) 
LDFLAGS = -L$(LIB_NETCDF) -lnetcdf

endif
#---------------------------------------------------------
# SGI IRIX64

ifeq ($(PLATFORM),IRIX64)

LIB_NETCDF = /usr/local/lib

FF = f90 -fixedform -extend_source -I$(INC_NETCDF) 
LDFLAGS = -L$(LIB_NETCDF) -lnetcdf

endif
#---------------------------------------------------------
# Mac OS X 

ifeq ($(PLATFORM),Darwin)

INC_NETCDF    := /usr/local/intel32/include
LIB_NETCDF    := /usr/local/intel32/lib

##FF = /usr/local/intel/bin/mpif90 -fixed -extend_source -I$(INC_NETCDF) -m32
FF = ifort -fixed -extend_source -I$(INC_NETCDF) -m32
LDFLAGS = -L$(LIB_NETCDF) -lnetcdf

#LIB_NETCDF = /usr/local/xlf/lib
#INC_NETCDF = /usr/local/xlf/include

#FF = xlf90 -qfixed -I$(INC_NETCDF) 
#LDFLAGS = -L$(LIB_NETCDF) -lnetcdf

#FF = f95 -qfixed -kind=byte -gline -I$(INC_NETCDF) 
#LDFLAGS = -L$(LIB_NETCDF) -lnetcdf

endif
#---------------------------------------------------------
#
ifeq ($(PLATFORM),Linux)

INC_NETCDF   := /usr/local/netCDF/netcdf-4.0-pgi/include
LIB_NETCDF   := /usr/local/netCDF/netcdf-4.0-pgi/lib

FF = pgf90 -Mfixed -Mextend -byteswapio -I${INC_NETCDF}
LDFLAGS = -L${LIB_NETCDF} -lnetcdf


#INC_NETCDF   := /apps/nco/netcdf-final/netcdf-3.6.2/include
#LIB_NETCDF   := /apps/nco/netcdf-final/netcdf-3.6.2/lib

#INC_NETCDF   := /usr/local/apps/V1R3/netcdf-3.6.0-p1/include
#LIB_NETCDF   := /usr/local/apps/V1R3/netcdf-3.6.0-p1/lib

#FF = xlf90 -qsuffix=f=f -qfixed=132 -I$(INC_NETCDF)
#DFLAGS = -L${LIB_NETCDF} -lnetcdf 
#LDFLAGS = -L${LIB_NETCDF} -L/usr/lib -lnetcdf -lpthread


#---------------------------------------------------------
#
# Linux Cluster (seawulf@sunysb)
#

#INC_NETCDF = /nfs/user04/marat/local/include
#LIB_NETCDF = /nfs/user04/marat/local/lib

#FF = ifort -O3 -fixed -extend_source -I${INC_NETCDF}
#LDFLAGS = -L${LIB_NETCDF} -lnetcdf

endif

#---------------------------------------------------------

VPATH = ./SRC

#all: bin2D2nc bin3D2nc 2Dbin2nc 2Dbin2nc_mean bin3D2nc_mean com3D2bin 2Dcom2nc 2Dcom2nc_mean com3D2nc com3D2nc_mean com2D2nc stat2evol  stat2nc stat2evolnc isccp2nc stat2nc_sp com3D2nc_sep 2Dbin2nc_sep 2Dcom_sep2one 2Dbin_sep2one com3D_sep2one bin3D_sep2one
#all: bin3D2nc com3D2bin com3D2nc stat2nc 2Dcom2nc
#all: stat2nc bin3D2nc com3D2bin com3D2nc
all: stat2nc

#stat2evol: stat2evol.f cape_analysis.f hbuf_lib.f
#	$(FF) -o $@ -I./SRC ./SRC/stat2evol.f ./SRC/cape_analysis.f ./SRC/hbuf_lib.f ./SRC/sat.f 

#.f:   
#	$(FF) -o $@ -I./SRC $< ./SRC/hbuf_lib.f ./SRC/cape.f ./SRC/cin.f $(LDFLAGS) 

.f:
	$(FF) -o $@ -I./SRC $< ./SRC/hbuf_lib.f $(LDFLAGS)

clean: 
	rm bin* com* stat* 2* isccp* *.o
