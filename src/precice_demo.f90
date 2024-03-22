program precice_demo
use precice_participant_api, only: participant
implicit none
type(participant) :: actor
actor = participant("mesh","config.xmkl", 0, 1)

end program