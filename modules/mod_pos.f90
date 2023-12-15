module position
    implicit none
    !
    save
    ! 11/10/2023: Addition of identity_Label
    ! 17/11/2023: Addition of dimers_interact
    ! Variables
    integer,            allocatable,dimension(:)  :: Label          ! Vector of each type of sphere.
    integer,            allocatable,dimension(:)  :: identity_Label ! Vector of each INDIVIDUAL sphere.
    double precision,   allocatable,dimension(:,:):: coord          ! Matrix of coordinates (x,y,z).
    double precision,   allocatable,dimension(:,:):: dimers_interact! Matrix of epsilon and sigmas of each dimers type.
    ! Here epsilon is first and sigma second.
    double precision,   allocatable,dimension(:)  :: Energy_loop    ! Vector containing all the energies for all the steps.
    double precision,   allocatable,dimension(:)  :: Energy_average ! Vector of average energy.
end module position