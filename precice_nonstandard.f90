module precice_nonstandard
implicit none

interface

    ! This is an _explicit_ interface block for a "Fortran" procedure
    ! implemented as an external subroutine (i.e. in the global namespace).
    !
    ! It obeys non-standard linkage conventions determined by 
    ! individually by the compiler vendors. It doesn't need to be implemented
    ! in Fortran, but could also be C, C++, or anything else, as long
    ! as it fulfills the linker expectations and the width and interpretation
    ! of the types match the implementation.
    !
    ! On Linux for example, this procedure would match the C declaration
    ! void precicef_create_(
    !       char *,
    !       char *,
    !       int *,
    !       int *, 
    !       size_t,
    !       size_t);
    !
    ! where the last two arguments are "hidden" arguments, added by the
    ! compiler that specify the string lengths. The current implementation
    ! of the Fortran preCICE library matches this convention (almost).
    !
    subroutine precicef_create(participantName, configFileName, &
      &                        solverProcessIndex, solverProcessSize)
      implicit none
      character(len=*) :: participantName
      character(len=*) :: configFileName
      integer :: solverProcessIndex
      integer :: solverProcessSize
    end subroutine

end interface

end module


