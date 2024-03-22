

module precice_participant_api
use, intrinsic :: iso_c_binding
implicit none
private

public :: participant

type :: participant
    private
    type(c_ptr) :: ptr = c_null_ptr
contains

    ! Steering Methods
    procedure :: initialize
    procedure :: advance
    procedure :: finalize

    ! Implicit Coupling
    procedure :: requiresWritingCheckpoint
    procedure :: requiresReadingCheckpoint

    ! Status queries
    procedure :: getMeshDimensions
    procedure :: getDataDimensions
    procedure :: isCouplingOngoing
    procedure :: isTimeWindowComplete
    procedure :: getMaxTimeStepSize

    ! Mesh Access
    procedure :: requiresMeshConnectivityFor
    procedure :: setMeshVertex
    procedure :: getMeshVertexSize
    procedure :: setMeshVertices
    ! procedure :: setMeshEdge
    ! procedure :: setMeshEdges
    ! procedure :: setMeshTriangle
    ! procedure :: setMeshTriangles
    ! procedure :: setMeshQuad
    ! procedure :: setMeshQuads
    ! procedure :: setMeshTetrahedron
    ! procedure :: setMeshTetrahedra

    ! Data Access
    procedure :: requiresInitialData
    procedure :: writeData
    procedure :: readData

    ! Direct Access
    procedure :: setMeshAccessRegion
    procedure :: getMeshVertexIDsAndCoordinates

    ! Gradient Data (Experimental)
    procedure :: requiresGradientDataFor
    procedure :: writeGradientData

    ! Finalizer
    final :: destroy_participant

end type

interface participant
    module procedure :: default_participant
#if defined(WITH_MPI)
    module procedure :: mpi_participant
#endif
end interface

integer, parameter :: dp = c_double
integer, parameter :: VertexId = c_int

!
! custom C bindings (using F2018 features)
! 
interface
    function pfapi_default_participant( participantName, &
                                        configurationFileName, &
                                        solverProcessIndex, &
                                        solverProcessSize) bind(c)
        use iso_c_binding, only: c_ptr, c_int
        implicit none
        character(len=*), intent(in) :: participantName, configurationFileName
        integer(c_int), value :: solverProcessIndex, solverProcessSize
        type(c_ptr) :: pfapi_default_participant
    end function
#if defined(WITH_MPI)
    function pfapi_mpi_participant( participantName, &
                                    configurationFileName, &
                                    solverProcessIndex, &
                                    solverProcessSize, &
                                    comm) bind(c)
        use iso_c_binding, only: c_ptr, c_int
        implicit none
        character(len=*), intent(in) :: participantName, configurationFileName
        integer(c_int), value :: solverProcessIndex, solverProcessSize
        integer :: comm
        type(c_ptr) :: pfapi_mpi_participant
    end function
#endif
    subroutine pfapi_initialize(ph) bind(c)
        use iso_c_binding, only: c_ptr
        implicit none
        type(c_ptr), value :: ph
    end subroutine
    subroutine pfapi_advance(ph,computedTimeStepSize) bind(c)
        use iso_c_binding, only: c_ptr, c_double
        implicit none
        type(c_ptr), value :: ph
        real(c_double), value :: computedTimeStepSize
    end subroutine
    subroutine pfapi_finalize(ph) bind(c)
        use iso_c_binding, only: c_ptr
        implicit none
        type(c_ptr), value :: ph
    end subroutine
    pure function pfapi_getMeshDimensions(ph,meshName) bind(c)
        use iso_c_binding, only: c_ptr, c_int
        implicit none
        type(c_ptr), value :: ph
        character(len=*), intent(in) :: meshName
        integer(c_int) :: pfapi_getMeshDimensions 
    end function
    pure function pfapi_getDataDimensions(ph,meshName,dataName) bind(c)
        use iso_c_binding, only: c_ptr, c_int
        implicit none
        type(c_ptr), value :: ph
        character(len=*), intent(in) :: meshName, dataName
        integer(c_int) :: pfapi_getDataDimensions 
    end function
    pure function pfapi_isCouplingOngoing(ph) bind(c)
        use iso_c_binding, only: c_ptr, c_bool
        implicit none
        type(c_ptr), value :: ph
        logical(c_bool) :: pfapi_isCouplingOngoing
    end function
    pure function pfapi_isTimeWindowComplete(ph) bind(c)
        use iso_c_binding, only: c_ptr, c_bool
        implicit none
        type(c_ptr), value :: ph
        logical(c_bool) :: pfapi_isTimeWindowComplete
    end function
    pure function pfapi_getMaxTimeStepSize(ph) bind(c)
        use iso_c_binding, only: c_ptr, c_double
        implicit none
        type(c_ptr), value :: ph
        real(c_double) :: pfapi_getMaxTimeStepSize
    end function
    pure function pfapi_requiresInitialData(ph) bind(c)
        use iso_c_binding, only: c_ptr, c_bool
        implicit none
        type(c_ptr), value :: ph
        logical(c_bool) :: pfapi_requiresInitialData
    end function
    pure function pfapi_requiresWritingCheckPoint(ph) bind(c)
        use iso_c_binding, only: c_ptr, c_bool
        implicit none
        type(c_ptr), value :: ph
        logical(c_bool) :: pfapi_requiresWritingCheckPoint
    end function
    pure function pfapi_requiresReadingCheckPoint(ph) bind(c)
        use iso_c_binding, only: c_ptr, c_bool
        implicit none
        type(c_ptr), value :: ph
        logical(c_bool) :: pfapi_requiresReadingCheckPoint
    end function
    pure function pfapi_requiresMeshConnectivityFor(ph) bind(c)
        use iso_c_binding, only: c_ptr, c_bool
        implicit none
        type(c_ptr), value :: ph
        logical(c_bool) :: pfapi_requiresMeshConnectivityFor
    end function
    pure function pfapi_setMeshVertex(ph) bind(c)
        use iso_c_binding, only: c_ptr, c_int
        implicit none
        type(c_ptr), value :: ph
        integer(c_int) :: pfapi_setMeshVertex
    end function
end interface

contains

    function getVersionInformation()
        character(len=:), allocatable :: getVersionInformation
    end function

    function default_Participant(participantName, configurationFileName, &
        solverProcessIndex, solverProcessSize) result(this)
        character(len=*), intent(in) :: participantName, configurationFileName
        integer, intent(in) :: solverProcessIndex, solverProcessSize
        type(participant) :: this

        this%ptr = pfapi_default_participant(participantName, &
                configurationFileName, solverProcessSize, solverProcessIndex)

    end function

#if defined(WITH_MPI)
    function mpi_Participant(participantName, configurationFileName, &
        solverProcessIndex, solverProcessSize, comm) result(this)
        character(len=*), intent(in) :: participantName, configurationFileName
        integer, intent(in) :: solverProcessIndex, solverProcessSize
        integer, intent(in) :: comm
        type(participant) :: this

        this%ptr = pfapi_mpi_participant(participantName, &
                configurationFileName, solverProcessSize, solverProcessIndex)
        
    end function
#endif

    subroutine destroy_participant(self)
        type(Participant), intent(inout) :: self
        call pfapi_finalize(self%ptr)
        self%ptr = c_null_ptr
    end subroutine

    subroutine initialize(self)
        class(Participant), intent(inout) :: self
        call pfapi_initialize(self%ptr)
    end subroutine
    subroutine advance(self,computedTimeStepSize)
        class(Participant), intent(inout) :: self
        real(c_double), intent(in) :: computedTimeStepSize
        call pfapi_advance(self%ptr,computedTimeStepSize)
    end subroutine

    subroutine finalize(self)
        class(Participant), intent(inout) :: self
        call pfapi_finalize(self%ptr)
    end subroutine

    ! bool  requiresWritingCheckpoint ()
    logical function requiresWritingCheckpoint(self)
        class(Participant), intent(in) :: self
    end function

    ! bool  requiresReadingCheckpoint ()
    logical function requiresReadingCheckPoint(self)
        class(Participant), intent(in) :: self
    end function

!
! Status queries
!
    pure integer function getMeshDimensions(self,meshName)
        class(Participant), intent(in) :: self
        character(len=*), intent(in) :: meshName
        getMeshDimensions = pfapi_getMeshDimensions(self%ptr,meshName)
    end function

    pure integer function getDataDimensions(self,meshName,dataName)
        class(Participant), intent(in) :: self
        character(len=*), intent(in) :: meshName, dataName
        getDataDimensions = pfapi_getDataDimensions(self%ptr,meshName,dataName)
    end function

    pure logical function isCouplingOngoing(self)
        class(Participant), intent(in) :: self
        isCouplingOngoing = pfapi_isCouplingOngoing(self%ptr)
    end function

    pure logical function isTimeWindowComplete(self)
        class(Participant), intent(in) :: self
        isTimeWindowComplete = pfapi_isTimeWindowComplete(self%ptr)
    end function

    pure real(c_double) function getMaxTimeStepSize(self)
        class(Participant), intent(in) :: self
        getMaxTimeStepSize = pfapi_getMaxTimeStepSize(self%ptr)
    end function

    pure logical function requiresMeshConnectivityFor(self,meshName)
        class(Participant), intent(in) :: self
        character(len=*), intent(in) :: meshName
        error stop "NotImplementedError: mesh access not available"
    end function

    integer(VertexId) function setMeshVertex(self,meshName,position)
        class(Participant), intent(in) :: self
        character(len=*), intent(in) :: meshName
        real(c_double), intent(in), contiguous :: position(:)  
    end function

    pure integer function getMeshVertexSize(self,meshName)
        class(Participant), intent(in) :: self
        character(len=*), intent(in) :: meshName

    end function

    subroutine setMeshVertices(self,meshName,coordinates,ids)
        class(Participant), intent(in) :: self
        character(len=*), intent(in) :: meshName
        real(c_double), intent(in) :: coordinates(:)
        integer(VertexId), intent(out) :: ids(:)    
    end subroutine



    ! TODO: IS THIS method const?
    ! bool  requiresMeshConnectivityFor (::precice::string_view meshName) const
    logical function requiresInitialData(self)
        class(Participant), intent(in) :: self
    end function


    subroutine writeData(self,meshName,dataName,ids,values)
        class(Participant), intent(inout) :: self
        character(len=*), intent(in) :: meshName, dataName
        integer(VertexId), intent(in) :: ids(:)
        real(c_double), intent(in) :: values(:)
    end subroutine

    subroutine readData(self,meshName,dataName,ids,relativeReadTime,values)
        class(Participant), intent(in) :: self
        character(len=*), intent(in) :: meshName, dataName
        integer(VertexId), intent(in) :: ids(:)
        real(c_double), intent(in) :: relativeReadTime
        real(c_double), intent(inout) :: values(:)
    end subroutine


    subroutine setMeshAccessRegion(self,meshName,boundingBox)
        class(Participant), intent(in) :: self
        character(len=*), intent(in) :: meshName
        real(c_double), intent(in) :: boundingBox(:)
    end subroutine

    subroutine getMeshVertexIDsAndCoordinates(self,meshName,ids,coordinates)
        class(Participant), intent(in) :: self
        character(len=*), intent(in) :: meshName
        integer(VertexId), intent(in) :: ids(:)
        real(c_double), intent(inout) :: coordinates(:)
    end subroutine

    subroutine requiresGradientDataFor(self,meshName,dataName)
        class(Participant), intent(in) :: self
        character(len=*), intent(in) :: meshName, dataName
    end subroutine

    subroutine writeGradientData(self,meshName,dataName,ids,gradients)
        class(Participant), intent(inout) :: self
        character(len=*), intent(in) :: meshName, dataName
        integer, intent(in), contiguous :: ids(:)
        real(c_double), intent(in), contiguous :: gradients(:)
    end subroutine

end module

! module precice_tooling
! implicit none

! enum, bind(c)
!     enumerator :: XML = 0
!     enumerator :: DTD = 1
!     enumerator :: MD  = 2
! end enum

! integer, parameter :: ConfigReferenceType = kind(XML)

! contains

!     subroutine printConfigReference(unit,reftype)
!     end subroutine

!     subroutine checkConfiguration(filename,participant,size)
!     end subroutine

! end module
