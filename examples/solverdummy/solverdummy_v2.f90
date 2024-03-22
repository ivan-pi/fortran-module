program main

use precice_participant_api, only: participant, dp
implicit none

! the participant type encapsulates the precice coupling interface
type(participant) :: mediator

type :: cmdargs
    character(len=64) :: config, participant
    character(len=64) :: meshname, readdata, writedata
end type

type(cmdargs) :: args

! >>>>   execution starts here   <<<<

write(*,'(A)') 'dummy: starting fortran solver dummy...'

args = default_args()

mediator = participant(
    participantName = args%participant_name, &
    participantConfig = args%config, &
    solverProcessIndex = 0, &
    solverProcessSize = 1)

call run_dummy_solver(nvert=3, &
    meshname=args%meshname,
    writedata=args%writedata,
    readdata=args%readdata)

    write (*,'(A)') 'dummy: closing fortran solver dummy...'

! >>>>   execution ends here   <<<<

contains

    function default_args()
        type(cmdargs) :: args

        call get_command_argument(1, args%config)
        call get_command_argument(2, args%participant)

        if (args%participant == 'solverone') then
            write(*,'(A)') 'solverone'
            args%writedata = 'data-one'
            args%readdata = 'data-two'
            args%meshname = 'solverone-mesh'
        else if (participant_name == 'solvertwo') then
            write(*,'(A)') 'solvertwo'
            args%writedata = 'data-two'
            args%readdata = 'data-one'
            args%meshname = 'solvertwo-mesh'
        else
            error stop 'error: unknown participant'
        end if

    end function

    subroutine run_dummy_solver(nvert,meshname,writedata,readdata)
        implicit none
        integer, intent(in) :: nvert
        character(len=*), intent(in) :: meshname, writedata, readdata

        real(dp), allocatable :: vertices(:,:)
        real(dp), allocatable :: wdata(:,:), rdata(:,:)
        integer, allocatable :: ids(:)

        integer :: d, i, j
        real(dp) :: dt

        d = mediator%getMeshDimensions(meshname)
        allocate(vertices(d,nvert), ids(nvert))
        allocate(rdata(d,nvert), wdata(d,nvert))

        !
        ! Prepare mock mesh and data
        !
        do i = 1, nvert
          do j = 1, d
            vertices(j,i) = i - 1 + j 
            rdata(j,i)  = i - 1
            wdata(j,i)  = i - 1
          end do
          ids(i) = i - 1
        end do

        call mediator%setMeshVertices(meshname, vertices, ids)

        if (mediator%requiresInitialData()) then
            write(*,'(A)') 'dummy: writing initial data'
        end if

        call mediator%initialize()

        !
        ! Time loop
        !
        do while (mediator%isCouplingOngoing())

            if (mediator%requiresWritingCheckpoint()) then
                write(*,'(A)') 'dummy: writing iteration checkpoint'
            end if

            dt = mediator%getMaxTimestepSize()
            call mediator%readData(meshname,readdata,ids,dt,rdata)
            write(*,'(A,*(G0))') 'readdata: ', rdata
            wdata = rdata + 1
            call mediator%writeData(meshname,writedata,ids,wdata)

            call mediator%advance(dt)

            if (mediator%requiresReadingCheckpoint()) then
                write(*,'(A)') 'dummy: reading iteration checkpoint'
            else
                write(*,'(A)') 'dummy: advancing in time'
            end if

        end do

        !
        ! Finalization
        !
        call mediator%finalize()

    end subroutine

end program
