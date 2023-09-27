module constant
    implicit none
    
    save

    ! Variables
    double precision    :: Temperature=295
    character(len=132), allocatable,dimension(:)    :: Name
    double precision,   allocatable,dimension(:)    :: sigma
    double precision,   allocatable,dimension(:)    :: epsilon_
    double precision    :: density=-1
    double precision    :: dr=0.5 ! In between 0 and 1
    integer             :: N_part=-1
    integer             :: Restart=0
    integer             :: simulation_time=1 ! Time in seconds
    integer             :: Freq_write=0      ! Frequency in fs
    integer, dimension(3):: Box_dimension=[-1,-1,-1]
    integer             :: Number_of_species=1


end module constant