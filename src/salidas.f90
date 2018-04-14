!
!        salidas.f90
!
!  FUNCTIONS:
!
!    File_out  - Writes Emission inventory in a new mesh to interpolate
!
!    crea_attr - creates attributes for gas variables
!    crea_attr2 -creates attributes for aerosol variables
!
!   
!	Created by Agustin Garcia on 28/08/2012.
!
!****************************************************************************
!  Proposito:
!            Guarda los datos del inventario interpolado para el
!            mecanismo RADM2 en formato netcdf
!***************************************************************************

subroutine File_out
    use netcdf
    use vars_dat
    implicit none
    integer :: i,j,l
    integer :: ncid
    integer :: id_varlong,id_varlat,id_vartime
    integer :: periodo,it,ikk,id,iu,iit,eit
    integer,dimension(NDIMS):: dim,id_dim
    integer,dimension(radm):: id_var
    integer isp(32)
    integer :: dimids2(2),dimids3(3),dimids4(4)
    real,ALLOCATABLE :: ea(:,:,:,:)
    character (len=20) :: FILE_NAME
    character(8)  :: date
    character(10) :: time
    character(19) :: hoy
    character (len = *), parameter :: REC_NAME = "Times"
    DATA isp / 1, 3, 4, 8, 9, 23,15,19,20,21, &
    22,28,24,25,26, 27,38,33,14,41, 40, &
    39,44,45,46,47, 48,49,50,51,52, 53/
    print *,"Guarda Archivo"
    ! ******************************************************************
    call date_and_time(date,time)
    write(hoy,'(A8,x,A10)')date,time
    print *,hoy," ",SIZE(Times),Times
    IF(current_date(12:13).EQ.'00') THEN
        print *,'PERIODO 1'
        FILE_NAME='wrfchemi_00z_d01'         !******
        PERIODO=1
        iit= 0
        eit=ntime-1
    else
        Print *,'PERIODO 2'
        FILE_NAME='wrfchemi_12z_d01'         !******
        PERIODO=2
        iit=12
        eit=ntime+11
    end if
write(FILE_NAME(16:16),'(I1)')1
! Open NETCDF emissions file
    call check( nf90_create(FILE_NAME, nf90_clobber, ncid) )
!     Define dimensiones
    dim(1)=1
    dim(2)=19
    dim(3)=SIZE(ED,DIM=1)
    dim(4)=SIZE(ED,DIM=2)
    dim(5)=1!mkx
    dim(6)=SIZE(ED,DIM=3) ! VERTICAL DATA
    if(.not.ALLOCATED(ea)) allocate (ea(dim(3),dim(4),dim(6),dim(1)))
    call check( nf90_def_dim(ncid,sdim(1), NF90_UNLIMITED, id_dim(1)) )
    do i=2,NDIMS
        call check( nf90_def_dim(ncid, sdim(i), dim(i), id_dim(i)) )
    end do

    dimids2 = (/id_dim(2),id_dim(1)/)
    dimids3 = (/id_dim(3),id_dim(2),id_dim(1) /)
    dimids4 = (/id_dim(3),id_dim(4),id_dim(6),id_dim(1)/)

!Attributos Globales NF90_GLOBAL
    call check( nf90_put_att(ncid, NF90_GLOBAL, "TITLE",trim(TITLE)//" with emissions"))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "START_DATE",iTime))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "DAY ",cday))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "SIMULATION_START_DATE",iTime))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "WEST-EAST_GRID_DIMENSION",dim(3)))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "SOUTH-NORTH_GRID_DIMENSION",dim(4)))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "BOTTOM-TOP_GRID_DIMENSION",1))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "DX",dx))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "DY",dy))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "CEN_LAT",cenlat))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "CEN_LON",cenlon))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "TRUELAT1",trulat1))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "TRUELAT2",trulat2))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "MOAD_CEN_LAT",moadcenlat))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "STAND_LON",stdlon))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "POLE_LAT",pollat))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "POLE_LON",pollon))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "GMT",gmt))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "JULYR",julyr))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "JULDAY",julday))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "MAP_PROJ",mapproj))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "MMINLU",mminlu))
!    call check( nf90_put_att(ncid, nf90_global, "ISWATER",iswater))
!    call check( nf90_put_att(ncid, nf90_global, "ISLAKE",islake))
!    call check( nf90_put_att(ncid, nf90_global, "ISICE",isice))
!    call check( nf90_put_att(ncid, nf90_global, "ISURBAN",isurban))
!    call check( nf90_put_att(ncid, nf90_global,"ISOILWATER",isoilwater))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "MECHANISM",mecha))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "CREATION_DATE",hoy))
!  Define las variables
    call check( nf90_def_var(ncid,REC_NAME, NF90_CHAR, dimids2,id_vartime ) )
!  Attributos para cada variable
    call check( nf90_def_var(ncid, "XLONG", NF90_REAL,(/id_dim(3),id_dim(4),id_dim(1)/),id_varlong ) )
! Assign  attributes
    call check( nf90_put_att(ncid, id_varlong, "FieldType", 104 ) )
    call check( nf90_put_att(ncid, id_varlong, "MemoryOrder", "XYZ") )
    call check( nf90_put_att(ncid, id_varlong, "description", "LONGITUDE, WEST IS NEGATIVE") )
    call check( nf90_put_att(ncid, id_varlong, "units", "degree_east"))
    call check( nf90_put_att(ncid, id_varlong, "axis", "X") )
    call check( nf90_def_var(ncid, "XLAT", NF90_REAL,(/id_dim(3),id_dim(4),id_dim(1)/),id_varlat ) )
! Assign  attributes
    call check( nf90_put_att(ncid, id_varlat, "FieldType", 104 ) )
    call check( nf90_put_att(ncid, id_varlat, "MemoryOrder", "XYZ") )
    call check( nf90_put_att(ncid, id_varlat, "description", "LATITUDE, SOUTH IS NEGATIVE") )
    call check( nf90_put_att(ncid, id_varlat, "units", "degree_north"))
    call check( nf90_put_att(ncid, id_varlat, "axis", "Y") )
    do i=1,radm
        if(ifexist(i).ne.0) THEN
         if(i.lt.26 .or.i.gt.37) THEN
            call crea_attr(ncid,4,dimids4,ename(i),cname(i),id_var(i))
         else
            call crea_attr2(ncid,4,dimids4,ename(i),cname(i),id_var(i))
         end if
        end if !ifexist
    end do
!
!   Terminan definiciones
call check( nf90_enddef(ncid) )
!    Inicia loop de tiempo
tiempo: do it=iit,eit
    write(6,'(A,x,I3)')'TIEMPO: ', it
        gases: do ikk=1,radm
         if(ifexist(ikk).ne.0) THEN
            ea=0.0
            if(ikk.eq.1) then
                if (it.lt.10) then
                    write(current_date(13:13),'(A1)')char(it+48)
                else
                    id = int((it)/10)+48 !  Decenas
                    iu = it-10*int((it)/10)+48 ! unidades
                    write(current_date(12:13),'(A1,A1)')char(id),char(iu)
                end if
                write(current_date(1:4),'(I4)') julyr
                Times(1,1)=current_date(1:19)

                if (periodo.eq. 1) then
                    call check( nf90_put_var(ncid,id_vartime,REC_NAME,start=(/1,it+1/)) )
                    call check( nf90_put_var(ncid, id_varlong,elon,start=(/1,1,it+1/)) )
                   call check( nf90_put_var(ncid, id_varlat,elat,start=(/1,1,it+1/)) )

                else
                    call check( nf90_put_var(ncid,id_vartime,REC_NAME,start=(/1,it-11/)) )
                    call check( nf90_put_var(ncid, id_varlong,elon,start=(/1,1,it-11/)) )
                    call check( nf90_put_var(ncid, id_varlat,elat,start=(/1,1,it-11/)) )
                endif
             end if   ! for kk == 1
            do i=1, dim(3)
                do j=1, dim(4)
                    do l=1,dim(6)
                        if(periodo.eq.1) then
                        ea(i,j,l,1)=ed(i,j,l,it+1,ikk)
                        else
                        ea(i,j,l,1)=ed(i,j,l,it-11,ikk)
                        endif
                    end do
                end do
            end do
!         Emisiones
          call emisiones(ikk,it)

            if(periodo.eq.1) then
                call check( nf90_put_var(ncid, id_var(ikk),ea,start=(/1,1,1,it+1/)) )
            else
                call check( nf90_put_var(ncid, id_var(ikk),ea,start=(/1,1,1,it-11/)) )        !******
            endif
        else
         !print *,ikk,ename(ikk)
        endif !ifexist
        end do gases
end do tiempo
call check( nf90_close(ncid) )
deallocate(ea,ed,xlon,xlat)
return
contains
!  CCCC RRRR  EEEEE  AAA      AAA  TTTTT TTTTT RRRR
! CC    R  RR E     A   A    A   A   T     T   R  RR
! C     RRRR  EEEE  AAAAA    AAAAA   T     T   RRRR
! CC    R  R  E     A   A    A   A   T     T   R  R
!  CCCC R   R EEEEE A   A____A   A   T     T   R   R
subroutine crea_attr(ncid,idm,dimids,svar,cname,id_var)
    implicit none
    integer , INTENT(IN) ::ncid,idm
    integer, INTENT(out) :: id_var
    integer, INTENT(IN),dimension(idm):: dimids
    character(len=*), INTENT(IN)::svar,cname
    character(len=50) :: cvar
    cvar="Emissions rate of "//trim(cname)

    call check( nf90_def_var(ncid, svar, NF90_REAL, dimids,id_var ) )
    ! Assign  attributes
    call check( nf90_put_att(ncid, id_var, "FieldType", 104 ) )
    call check( nf90_put_att(ncid, id_var, "MemoryOrder", "XYZ") )
    call check( nf90_put_att(ncid, id_var, "description", Cvar) )
    call check( nf90_put_att(ncid, id_var, "units", "mol km^-2 hr^-1"))
    call check( nf90_put_att(ncid, id_var, "stagger", "Z") )
    call check( nf90_put_att(ncid, id_var, "coordinates", "XLONG XLAT") )
    ! print *,"Entro a Attributos de variable",dimids,id_var,idm
    return
end subroutine crea_attr
!  CCCC RRRR  EEEEE  AAA      AAA  TTTTT TTTTT RRRR   222
! CC    R  RR E     A   A    A   A   T     T   R  RR 2   2
! C     RRRR  EEEE  AAAAA    AAAAA   T     T   RRRR     2
! CC    R  R  E     A   A    A   A   T     T   R  R   2
!  CCCC R   R EEEEE A   A____A   A   T     T   R   R 22222
subroutine crea_attr2(ncid,idm,dimids,svar,cname,id_var)
    implicit none
    integer, INTENT(IN) ::ncid,idm
    integer, INTENT(out) :: id_var
    integer,INTENT(IN) ,dimension(idm):: dimids
    character(len=*),INTENT(IN) ::svar,cname
    character(len=50) :: cvar
    cvar="Emissions rate of "//trim(cname)
    call check( nf90_def_var(ncid, svar, NF90_REAL, dimids,id_var ) )
    ! Assign  attributes
    call check( nf90_put_att(ncid, id_var, "FieldType", 104 ) )
    call check( nf90_put_att(ncid, id_var, "MemoryOrder", "XYZ") )
    call check( nf90_put_att(ncid, id_var, "description",cvar) )
    call check( nf90_put_att(ncid, id_var, "units", "ug m-2 s-1"))
    call check( nf90_put_att(ncid, id_var, "stagger", "Z") )
    call check( nf90_put_att(ncid, id_var, "coordinates", "XLONG XLAT") )
    !print *,"Entro a Attributos de variable",dimids,idm,id_var
    return
    end subroutine crea_attr2

! EEEEE M     M IIIII  SSSS IIIII  OOO  N   N EEEEE  SSSS
! E     M M M M   I   S       I   O   O NN  N E     S
! EEE   M  M  M   I    SSS    I   O   O N N N EEE    SSS
! E     M     M   I       S   I   O   O N  NN E         S
! EEEEE M     M IIIII SSSS  IIIII  OOO  N   N EEEEE SSSS
      subroutine Emisiones(iksp,iiti)
      implicit none
      integer dim1,dim2,dim3,dim4
      real ::m2km=1e6
      integer :: iksp,iiti,nsp
      integer ::i,j,levl
!    INORGANIC
      do i=1,nemiss !from  vars_dat
        levl=zlef(i)
      if((icf(i).ne.0 .or.jcf(i).ne.0 ).and.(iiti.ge.ihf(1,i).and.iiti.le.ihf(2,i))) then
        do j =1,nemisv
        if(pname(j).eq.ename(iksp) )then
        if(f_emis(j,i).ne.0)print*,pname(j),iksp,ename(iksp),f_emis(j,i)
        ea(icf(i),jcf(i),levl,1)=ea(icf(i),jcf(i),levl,1)&
        + f_emis(j,i)!*m2km/(dx*dy)
        end if
        end do
      end if ! icf and jcf .ne. 0
      end do
      return
      end subroutine Emisiones
      end subroutine File_out
