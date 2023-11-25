module constant
    implicit none
    
    save

    ! Variables
    double precision    :: Temperature=295.0d0
    character(len=132), allocatable,dimension(:)    :: Name
    double precision,   allocatable,dimension(:)    :: sigma
    double precision,   allocatable,dimension(:)    :: epsilon_
    double precision,   allocatable,dimension(:)    :: Proportion
    double precision    :: density=-1.0d0
    double precision    :: dr=0.5 ! Value by default
    double precision    :: boltzmann = 1.380649E-23 ! J/K
    double precision    :: pi = 4.0d0*ATAN(1.0d0)
    double precision    :: kJ_mol_to_J = 1/(6.02214076E+20)
    integer             :: N_part=-1
    integer             :: Restart=0
    integer             :: simulation_time=1 ! Number of MC moves we try
    integer             :: Freq_write=0      ! Frequency in fs
    double precision, dimension(3):: Box_dimension=[-1.0d0,-1.0d0,-1.0d0]
    integer             :: Number_of_species=1
    double precision    :: displacement=-1.0d0
    ! The use of -1 is non-physical to amplify the fact it's a dummy variable.
    ! If Npart is equal to -1 we need to compute it with d and Bdim.
    ! If d is equal to -1 we need to compute it with Npart and Bdim.
    ! If Bdim have a -1 in the list, we need to compute it with Npart and d.



end module constant