module precice_tooling
use precice_participant_api, only: participant_type => participant
implicit none
private

public :: REFTYPE_XML, REFTYPE_DTD, REFTYPE_MD

enum, bind(c)
    enumerator :: REFTYPE_XML = 0
    enumerator :: REFTYPE_DTD = 1
    enumerator :: REFTYPE_MD  = 2
end enum

integer, parameter :: ConfigReferenceType = kind(REFTYPE_XML)

contains

    subroutine printConfigReference(unit,reftype)
        integer, intent(in) :: unit
        integer(ConfigReferenceType), intent(in) :: reftype
    end subroutine

    subroutine checkConfiguration(filename,participant,size)
        character(len=*), intent(in) :: filename
        type(participant_type), intent(in) :: participant
        integer, intent(in) :: size
    end subroutine

end module