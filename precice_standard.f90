module precice_standard

use, intrinsic :: iso_c_binding, only: c_int, c_char, c_null_char
implicit none
private

public :: precicef_create

interface
    !
    ! This is an _explicit_ interface for the procedure in the precice C API.
    ! It expects null-terminated strings and the integers are received by value. 
    !
    ! If we don't want to expose the procedure, we keep it private to the 
    ! module. We could even place the interface block inside of the Fortran 
    ! wrapper.

! PRECICE_API void precicec_createParticipant(
!    const char *participantName,
!    const char *configFileName,
!    int         solverProcessIndex,
!    int         solverProcessSize);
!
    subroutine precicec_create(participantName, configFileName, &
      &                        solverProcessIndex, solverProcessSize) &
      & bind(c,name="precicec_createParticipant")
      use, intrinsic :: iso_c_binding, only: c_char, c_int
      implicit none
      character(kind=c_char), intent(in) :: participantName(*) ! null-terminated string
      character(kind=c_char), intent(in) :: configFileName(*) ! null-terminated string
      integer(c_int), value :: solverProcessIndex
      integer(c_int), value :: solverProcessSize
    end subroutine
end interface

contains

    ! To save Fortran programmers from the trouble of null-terminating their
    ! strings, we make a thin wrapper.
    !
    subroutine precicef_create(participantName, configFileName, &
      &                        solverProcessIndex, solverProcessSize)

      character(len=*), intent(in) :: participantName
      character(len=*), intent(in) :: configFileName
      integer, intent(in) :: solverProcessIndex
      integer, intent(in) :: solverProcessSize

      ! Here I've trimmed the whitespace, but in principle we could
      ! leave this decision to the user. I expect that users
      ! will typically use string literals or constants (parameters)
      ! and not dynamic strings that potentially have trailing whitespace.
      !
      ! The wrapper creates temporary copies of the input strings, but
      ! we can assume that this overhead is negligible. 
      ! 
      call precicec_create( &
            trim(participantName)//c_null_char, &
            trim(configFileName)//c_null_char, &
            solverProcessIndex, solverProcessSize)

    end subroutine

end module