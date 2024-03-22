

module precice_participant_api
use, intrinsic :: iso_c_binding
implicit none
private

public :: participant, dp 

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
include "pfapi.fi"

! MPI functions
#if defined(WITH_MPI)
interface
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
end interface
#endif


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

! 
! Finalizer
!
    subroutine destroy_participant(self)
        type(Participant), intent(inout) :: self
        call pfapi_finalize(self%ptr)
        self%ptr = c_null_ptr
    end subroutine

!
! Steering Methods
!
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

!
! Status Queries
!
    logical function requiresWritingCheckpoint(self)
        class(Participant), intent(in) :: self
        requiresWritingCheckpoint = pfapi_requiresWritingCheckpoint(self%ptr)
    end function
    logical function requiresReadingCheckPoint(self)
        class(Participant), intent(in) :: self
        requiresReadingCheckpoint = pfapi_requiresReadingCheckpoint(self%ptr)
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
        real(c_double), intent(in), contiguous :: coordinates(:,:)
        integer(VertexId), intent(in), contiguous :: ids(:)

        !call pfapi_setMeshVertices(self%ptr,meshName,d,sz,coordinates,ids)   
    end subroutine



    ! TODO: IS THIS method const?
    ! bool  requiresMeshConnectivityFor (::precice::string_view meshName) const
    logical function requiresInitialData(self)
        class(Participant), intent(in) :: self
    end function


    subroutine writeData(self,meshName,dataName,ids,values)
        class(Participant), intent(inout) :: self
        character(len=*), intent(in) :: meshName, dataName
        integer(VertexId), intent(in), contiguous :: ids(:)
        real(c_double), intent(in), contiguous :: values(:,:)
    end subroutine

    subroutine readData(self,meshName,dataName,ids,relativeReadTime,values)
        class(Participant), intent(in) :: self
        character(len=*), intent(in) :: meshName, dataName
        integer(VertexId), intent(in), contiguous :: ids(:)
        real(c_double), intent(in) :: relativeReadTime
        real(c_double), intent(inout), contiguous :: values(:,:)
    end subroutine

!
! Direct Access
!

    subroutine setMeshAccessRegion(self,meshName,boundingBox)
        class(Participant), intent(in) :: self
        character(len=*), intent(in) :: meshName
        real(c_double), intent(in) :: boundingBox(*)
        call pfapi_setMeshAccessRegion(self%ptr,meshName,boundingBox)
    end subroutine
    subroutine setMeshAccessRegion_xyz(self,meshName,xlim,ylim,zlim)
        class(Participant), intent(in) :: self
        character(len=*), intent(in) :: meshName
        real(c_double), intent(in) :: xlim(2), ylim(2), zlim(2)

        real(c_double) :: bbox(6)
        bbox = [xlim, ylim, zlim]

        call pfapi_setMeshAccessRegion(self%ptr,meshName,bbox)
    end subroutine

    subroutine getMeshVertexIDsAndCoordinates(self,meshName,ids,coordinates)
        class(Participant), intent(in) :: self
        character(len=*), intent(in) :: meshName
        integer(VertexId), intent(out), allocatable :: ids(:)
        real(c_double), intent(out), allocatable :: coordinates(:,:)

        integer :: sz, dim

        sz = pfapi_getMeshVertexSize(self%ptr, meshName)
        dim = pfapi_getMeshDimensions(self%ptr, meshName)

        allocate(ids(sz), coordinates(dim,sz))

        call pfapi_getMeshVertexIdsAndCoordinates(self%ptr, &
            meshname, &
            dim,sz,ids,coordinates)

    end subroutine

    logical function requiresGradientDataFor(self,meshName,dataName)
        class(Participant), intent(in) :: self
        character(len=*), intent(in) :: meshName, dataName
        requiresGradientDataFor = pfapi_requiresGradientDataFor(self%ptr,meshName,dataName)
    end function

    subroutine writeGradientData(self,meshName,dataName,ids,gradients)
        class(Participant), intent(inout) :: self
        character(len=*), intent(in) :: meshName, dataName
        integer, intent(in), contiguous :: ids(:)
        real(c_double), intent(in), contiguous :: gradients(:)
        call pfapi_writeGradientData(self%ptr,meshName,dataName,size(ids),ids,gradients)
    end subroutine

end module


