!
!    computations.f90
!
!	Created by Agustin Garcia on 28/08/2012.
!
!****************************************************************************
!
!  FUNCTIONS:
!
!    computations  -  Identifies the i,j cell grid position vy localiza subr
!    localization  -  using point lat,lon localization finds the i,j values
!                     of the specific cell in the emissions mesh
!
!
!****************************************************************************
!
!
!  PURPOSE:  Locates the cell grid where the emissions has to be added
!            
!
!****************************************************************************
subroutine computations
use vars_dat
implicit none
integer :: i,j,ii,jj,kl,l
integer :: ih
real*8   :: ylat1,ylat2,xlon1,xlon2
real*8   :: elat1,elat2,elon1,elon2
real*8   :: alat,alon,area,tot
real*8  ::  xmas,xemis
print *, "*****  Doing localization ****"

call localization(elat,elon,eix,ejx,f_lat,f_lon,icf,jcf,nemiss) ! new emiss

!
print '(A)','******   Done localization'
!deallocate(elat,elon)
print *,"      ++++++++++++"
!print *,icf,jcf

RETURN
end subroutine computations

Subroutine localization(xlat,xlon,mi,mj,clat,clon,ist,jst,nst)
implicit none
    integer :: mi,mj,nst,i,j,l
    integer,dimension(nst):: ist,jst
    real,dimension(mi,mj):: xlat,xlon
    real,dimension(nst):: clat,clon
    do l=1,nst
    ! Out of the region
        ist(l)=0
        jst(l)=0
        do i = 1,mi-1
            do j= 1,mj-1
            if(clon(l) .ge. xlon(i,j)  .and. clon(l) .le. xlon(i+1,j).and.&
            &clat(l) .ge. xlat(i,j)  .and. clat(l) .le. xlat(i,j+1))then
            ist(l)= i
            jst(l)= j
            end if
            end do
        end do
    end do
    RETURN
end Subroutine localization
