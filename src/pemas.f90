!
!    pemas.f90
!
!  FUNCTIONS:
!	PEmAS      - Point Emissions Addition Software main program.
!
!    File_reading  - Reads Emission inventory File (WRF-chem), variables and mesh
!    conversion  - Add in the location the emissions into the emissions  WRF/chem file
!    File_out      - Write results
!
!	Created by Agustin Garcia on 28/08/2012.
!
!           version 1.0          12/08/2016
!****************************************************************************
!
!  PROGRAM: PEmAS
!
!  PURPOSE:  Main program calls subroutines for read, compute and write.
!
!****************************************************************************

program PEmAS

! Variables

! Body of Interpola

    Call file_reading

    Call computations

    Call File_out

end program PEmAS
