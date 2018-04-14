!
!    vars_dat_mod.f90
!
!	Created by Agustin Garcia on 28/08/2012.
!
!****************************************************************************
!
!  FUNCTIONS:
!	Set up variables used during the process.
!
!
!****************************************************************************

module vars_dat
! Emissions Inventories Variables
integer :: zlev       ! Layer of emission (1 to 8) 8 lower 1 upper
integer,parameter :: radm=44    ! number of RADM2 classes
integer,parameter :: nh =24 ! No hours
integer,parameter :: NDIMS= 6
!real,allocatable:: ei(:,:,:,:,:)  ! emissions by nx,ny,level,nh,radm
real,allocatable:: ed(:,:,:,:,:)  ! emissions by nx,ny,level,nh,radm from NEW DOMAIN
real,allocatable:: elat(:,:),elon(:,:)     ! by nx,ny from emissions domain
real,allocatable:: dlat(:,:),dlon(:,:)     ! by nx,ny from NEW DOMAIN
real,allocatable ::xlon(:,:,:),xlat(:,:,:)! by nx,ny,nh emissions
integer:: dix,djx,eix,ejx,grid_id  ! ntime No of hours in wrfchemin file
integer:: julyr,julday,mapproj,iswater,islake,isice,isurban,isoilwater,ntime
integer :: ifexist(radm)=0
integer :: nemiss  ! Number of lines emission sources in new_emiss.csv file
integer :: nemisv  ! Number of columns emission sources in new_emiss.csv file
real :: cenlat,cenlon, dx,dy,dxe,dye
real :: trulat1, trulat2,moadcenlat,stdlon,pollat,pollon
real :: gmt
! Localization of new sources
REAL, ALLOCATABLE :: f_lat(:),f_lon(:)! Localization new sources
REAL, ALLOCATABLE :: f_emis(:,:)      ! Emission dim 1 site , dim 2 :
!    1   2   3   4     5      6  7    8    9     10    11
!    SO2	CO	NOX HC3 PM2.5	PM10 CH4  CLS	ETH	CO2	ORA1
!    12            13    14     15      16      17       18     19
!	Formaldehido	ALD 	ORA2	Tolueno	Xyleno	OL2	HC5	HC8
integer, ALLOCATABLE:: zlef(:),icf(:),jcf(:),ihf(:,:)   ! i,j values from lat,lon fires
character(len=3) :: cday
character(len=19)::mminlu
character(len=19):: iTime
character(len=38):: Title
character(len=19),dimension(1,1)::Times
character (len=19),dimension(NDIMS) ::sdim=(/"Time               ",&
& "DateStrLen         ","west_east          ","south_north        ",&
&"bottom_top         ","emissions_zdim_stag"/)

character(len=11),dimension(radm):: ename=(/'E_CO   ','E_NH3  ','E_NO   ', &
'E_NO2  ','E_SO2  ','E_ALD  ','E_CH4  ','E_CSL  ','E_ETH  ','E_GLY ', &
'E_HC3  ','E_HC5  ','E_HC8  ','E_HCHO ','E_ISO  ','E_KET  ','E_MACR ', &
'E_MGLY ','E_MVK  ','E_OL2  ','E_OLI  ','E_OLT  ','E_ORA1 ','E_ORA2 ', &
'E_TOL  ','E_XYL  ','E_CO2  ','E_PM_10','E_PM25 ','E_SO4I ','E_NO3I ', &
'E_PM25I','E_ORGI ','E_ECI  ','E_SO4J ','E_NO3J ','E_PM25J','E_ORGJ ','E_ECJ  ',&
'EBIO_ISO','EBIO_C10H16','EBIO_NO','EBIO_NOx','EBIO_OTHER'/)
character(len= 16),dimension(radm):: cname=(/'Carbon Monoxide ','NH3             ','NO              ', &
'NO2  ','SO2  ','ALDEHYDES  ','METHANE','CRESOL','Ethane','Glyoxal', &
'HC3  ','HC5  ','HC8  ','HCHO ','ISOPRENE','Acetone','Acrolein', &
'MGLY ','Methyl Vinil Ketone  ','Alkenes','alkenes   ','Terminal Alkynes','Formic Acid','Acetic Acid ', &
'TOLUENE  ','XYLENE  ','Carbon Dioxide','PM_10','PM_25 ','Sulfates ','Nitrates ','PM25I',&
'Organic ','Elemental Carbon  ','SulfatesJ','NitratesJ','PM25J','Organic','Elemental Carbon',&
'biog isoprene','biog monoterpene','biog NO','biog NOx','biog other VOCs'/)
character (len=19) :: current_date,current_datem,mecha

character (len=11),ALLOCATABLE:: pname(:)

integer  LCO,LNO,LSO2,LALD,LHCHO,LORA2
DATA     LCO,LNO,LSO2,LALD,LHCHO,LORA2/ &
         1,3,5,6,14,24/
integer LFile(19)
! Domain Variables
common /domain/ ifexist,zlev, dix,djx,eix,ejx,dx,dy,dxe,dye,Title
common /date/ id_grid,ntime,current_date,cday,mecha,cname,Times
common /wrf/ julyr,julday,mapproj,iswater,islake,isice,isurban,isoilwater,&
            cenlat,cenlon,trulat1, trulat2,moadcenlat,stdlon,pollat,pollon,&
            gmt,mminlu
common /emis/LCO,LNO,LSO2,LALD,LHCHO,LORA2,nemiss,nemisv

end module vars_dat
