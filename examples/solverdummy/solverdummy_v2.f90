program main

use precice_participant_api, only: participant, dp
implicit none

! This derived type encapsulates the precice coupling interface
type(participant) :: solver

character(len=64) :: config, participant_name
character(len=64) :: mesh_name, rdata_name, wdata_name

! >>>>   execution starts here   <<<<

    write(*,'(A)') 'dummy: starting fortran solver dummy...'
    call get_command_argument(1, config)
    call get_command_argument(2, participant_name)

    ! Echo arguments to the output unit
    write(*,'(A)') 'config: '//config
    write(*,'(A)') 'participant: '//participant_name

    if (participant_name == 'solverone') then
        write(*,'(A)') "solverone"
        wdata_name = 'data-one'
        rdata_name = 'data-two'
        mesh_name = 'solverone-mesh'
    else if (participant_name == 'solvertwo') then
        write(*,'(A)') "solvertwo"
        wdata_name = 'data-two'
        rdata_name = 'data-one'
        mesh_name = 'solvertwo-mesh'
    else
        error stop 'dummy: error - unknown participant'
    end if

    !
    ! Create the coupled solver
    !
    solver = participant(participant_name,config, &
        solverProcessIndex = 0, &
        solverProcessSize = 1)

    !
    ! Run the simulation
    !
    call run(nvert=3)

! >>>>   execution ends here   <<<<

contains

    subroutine run(nvert)
        implicit none
        integer, intent(in) :: nvert

        real(dp), allocatable :: vertices(:,:)
        real(dp), allocatable :: wdata(:,:), rdata(:,:)
        integer, allocatable :: ids(:)

        integer :: d, i, j
        real(dp) :: dt

        d = solver%getMeshDimensions(mesh_name)
        allocate(vertices(d,nvert), ids(nvert))
        allocate(rdata(d,nvert), wdata(d,nvert))

        !
        ! Prepare mock mesh
        !
        do i = 1, nvert
          do j = 1, d
            vertices(j,i) = i - 1 + j 
            rdata(j,i)  = i - 1
            wdata(j,i)  = i - 1
          end do
          ids(i) = i - 1
        end do

        call solver%setMeshVertices(mesh_name, vertices, ids)

        if (solver%requiresInitialData()) then
            write(*,'(A)') 'dummy: writing initial data'
        end if

        call solver%initialize()

        !
        ! Time loop
        !
        do while (solver%isCouplingOngoing())

            if (solver%requiresWritingCheckpoint()) then
                write(*,'(A)') 'dummy: writing iteration checkpoint'
            end if

            dt = solver%getMaxTimestepSize()

            call solver%readData(mesh_name,rdata_name,ids, dt, rdata)
            write(*,'(A,*(G0))') 'readdata: ', rdata
            wdata = rdata + 1
            call solver%writeData(mesh_name,wdata_name,ids,wdata)

            call solver%advance(dt)

            if (solver%requiresReadingCheckpoint()) then
                write(*,'(A)') 'dummy: reading iteration checkpoint'
            else
                write(*,'(A)') 'dummy: advancing in time'
            end if

        end do

        !
        ! Finalization
        !
        call solver%finalize()
        write (*,'(A)') 'dummy: closing fortran solver dummy...'

    end subroutine

end program
