!
!    reads_input.f90
!
!  FUNCTIONS:
!
!    File_reading  - Reads Emission inventory and Mesh to interpolate
!       check      - in case of error prints error messages
!
!	Created by Agustin Garcia on 28/08/2012.
!
!****************************************************************************
!
!  PROGRAM: file_reading
!
!  PURPOSE:  Reads emissions inventory from new_emiss.csv and wrfinput_d01
!            
!   version 0.1  27 august 2012
!           1.0  12 august 2016
!           2.0  14 april  2018
!
!***************************************************************************
subroutine file_reading
    use vars_dat
    use netcdf
    implicit none

! This is the name of the data file to read
character (len = *), parameter :: FILE_NAME = "wrfchemin.nc" !Inventory
integer :: i,j,l,it,ikk
integer :: ncid, iunit=10
integer :: lat_varid,lon_varid,btDimID
integer :: latVarId, lonVarId
integer :: land_catID
integer :: dimlon,dimlat,dimtime
integer,dimension(NDIMS):: dim,id_dim
integer,dimension(radm+1):: id_var
real,ALLOCATABLE :: ea(:,:,:,:)
character(len=10) :: cdum
character (len=20 ) :: name
character (len = *), parameter :: LAT_NAME = "XLAT"
character (len = *), parameter :: LON_NAME = "XLONG"
character (len = *), parameter :: REC_NAME = "Times"

! Open the file.
    print *,"Reading file: ",FILE_NAME
    call check( nf90_open(FILE_NAME, nf90_nowrite, ncid) )
    call check( nf90_get_att(ncid, nf90_GLOBAL, "TITLE", TITLE))
    call check( nf90_get_att(ncid, NF90_GLOBAL, "START_DATE",iTime))
    call check( nf90_get_att(ncid, NF90_GLOBAL, "DAY",cday))
    call check( nf90_get_att(ncid, NF90_GLOBAL, "MECHANISM",mecha))
! Get the vars ID of the latitude and longitude coordinate variables.
    call check( nf90_inq_varid(ncid, LAT_NAME, lat_varid) )
    call check( nf90_inq_varid(ncid, LON_NAME, lon_varid) )
    call check( nf90_inq_varid(ncid, REC_NAME, id_var(radm+1)) )
!  Get dims ID and dimension values
    do i=1,NDIMS
        call check(nf90_inq_dimid(ncid, sdim(i), id_dim(i)))
        call check(nf90_inquire_dimension(ncid,id_dim(i),len=dim(i)))
    end do
    ntime=dim(1) ! No hours in wrfchemin file
    if(.not.ALLOCATED(XLON)) allocate (XLON(dim(3),dim(4),dim(1)))
    if(.not.ALLOCATED(XLAT)) allocate (XLAT(dim(3),dim(4),dim(1)))
    if(.not.ALLOCATED(elat)) allocate (elat(dim(3),dim(4)))
    if(.not.ALLOCATED(elon)) allocate (elon(dim(3),dim(4)))
    if(.not.ALLOCATED(ea)) allocate (ea(dim(3),dim(4),dim(6),dim(1)))
    if(.not.ALLOCATED(ed)) allocate(ed(dim(3),dim(4),dim(6),dim(1),radm))
!
!   Retrive initial Time
    call check(nf90_get_var(ncid, id_var(radm+1), Times,start = (/ 1, 1 /)))
    current_date(1:19)=Times(1,1)
    print *,current_date!,lat_varid,lon_varid

!
!   Get lat and lon values.
    print *,"* Get lat and lon values"
    call check(nf90_get_var(ncid, lat_varid, XLAT))
    call check(nf90_get_var(ncid, lon_varid, XLON,start = (/ 1, 1,1 /)))
!    print *,XLAT(1,1,1),XLAT(1,2,dim(1))
!    print *,XLON(1,1,1),XLON(2,1,1)
    do i=1,dim(3)
        do j=1,dim(4)
        elat(i,j)=XLAT(i,j,1)
        elon(i,j)=XLON(i,j,1)
        end do
    end do
print *,'  Reading Global Attribiutes'
call check( nf90_get_att(ncid, nf90_global, "DX", dx))
call check( nf90_get_att(ncid, nf90_global, "DY", dy))
call check( nf90_get_att(ncid, nf90_global, "CEN_LAT",cenlat))
call check( nf90_get_att(ncid, nf90_global, "CEN_LON",cenlon))
call check( nf90_get_att(ncid, nf90_global, "TRUELAT1",trulat1))
call check( nf90_get_att(ncid, nf90_global, "TRUELAT2",trulat2))
call check( nf90_get_att(ncid, nf90_global, "MOAD_CEN_LAT",moadcenlat))
call check( nf90_get_att(ncid, nf90_global, "STAND_LON",stdlon))
call check( nf90_get_att(ncid, nf90_global, "POLE_LAT",pollat))
call check( nf90_get_att(ncid, nf90_global, "POLE_LON",pollon))
call check( nf90_get_att(ncid, nf90_global, "GMT",gmt))
call check( nf90_get_att(ncid, nf90_global, "JULYR",julyr))
call check( nf90_get_att(ncid, nf90_global, "JULDAY",julday))
call check( nf90_get_att(ncid, nf90_global, "MAP_PROJ",mapproj))
call check( nf90_get_att(ncid, nf90_global, "MMINLU",mminlu))
!call check( nf90_get_att(ncid, nf90_global, "ISWATER",iswater))
!call check( nf90_get_att(ncid, nf90_global, "ISLAKE",islake))
!call check( nf90_get_att(ncid, nf90_global, "ISICE",isice))
!call check( nf90_get_att(ncid, nf90_global, "ISURBAN",isurban))
!call check( nf90_get_att(ncid, nf90_global,"ISOILWATER",isoilwater))
!call check( nf90_get_att(ncid, nf90_global,"GRID_ID",grid_id))
!print *,elat(1,1),elat(1,2)
!print *,elon(1,1),elon(2,1)

!
!     Get emissions values
    print *,"* Get emissions values"
    do ikk=1,radm
      if(nf90_inq_varid(ncid,ename(ikk), id_var(ikk)).eq.nf90_noerr)  then
       ifexist(ikk)=1
       !print *,ikk,ename(ikk),id_var(ikk),ifexist(ikk)
      call check(nf90_get_var(ncid, id_var(ikk),ea,start = (/1,1,1,1/)))
      do i=1, dim(3)
        do j=1, dim(4)
            do l=1,dim(6)
                do it=1,dim(1) !times in file
                ed(i,j,l,it,ikk)=0. !ea(i,j,l,it)
                end do
            end do
        end do
      end do
        end if
    end do
    ifexist(6)=1  ! Para Metano
    !print *,MAXVAL(ei),MAXVAL(ed)
    call check( nf90_get_att(ncid, nf90_global, "DX", dxe))
    call check( nf90_get_att(ncid, nf90_global, "DY", dye))
    eix= dim(3)
    ejx= dim(4)
    deallocate (ea)
    print * ,'** Done Reading Emissions file'
    call check( nf90_close(ncid) )

!
!  open new_emiss.csv
!
write(6,*)"   ----- lee new_emiss.csv"
open (iunit,file='new_emiss.csv',action='read',err=35)
read(iunit,*) nemiss,nemisv,cdum,cdum,cdum,cdum
print *,nemiss,nemisv,cdum
rewind(iunit)
ALLOCATE(pname(nemisv))
read(iunit,*) nemiss,nemisv,cdum,cdum,cdum,cdum,(pname(i),i=1,nemisv)
!print *,pname
ALLOCATE(icf(nemiss),jcf(nemiss),ihf(2,nemiss),zlef(nemiss))
ALLOCATE(f_lat(nemiss),f_lon(nemiss))
ALLOCATE(f_emis(nemisv,nemiss))
do i=1,nemiss
read(iunit,*)f_lat(i),f_lon(i),zlef(i),ihf(1,i),ihf(2,i),(f_emis(j,i),j=1,nemisv)
end do
i=nemiss
print *,ihf(1,i),ihf(2,i),(f_emis(j,i),j=1,nemisv)
close(iunit)
    print * ,'* Done reading new_emiss.csv file'
!    deallocate (XLAT,XLON)
return
35 print *, "Error opening file new_emiss.csv UNIT=",iunit
stop

end subroutine file_reading
!
!  CCCC  H   H  EEEEE   CCCC  K   K
! CC     H   H  E      CC     K K
! C      HHHHH  EEE   C       KK
! CC     H   H  E      CC     K K
!  CCCC  H   H  EEEEE   CCCC  K   K

subroutine check(status)
    USE netcdf
    integer, intent ( in) :: status
    if(status /= nf90_noerr) then
        print *, trim(nf90_strerror(status))
        stop 2
    end if
end subroutine check
