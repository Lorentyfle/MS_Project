program main_main
    ! 11/10/2023 : Addition of the squeletton of the main of the program.
    ! Coded by T.Jamin.
    use sub, only : load_input, load_input_position, write_input_position, load_input_position_last
    use sub, only : coord_gen, random_atom, DNB, dr_verif, Box_good
    use sub, only : Debug_print, write_matrix
    use sub, only : sigma_epsilon_dimers, pick_dimers_data, PBC
    use sub, only : random_select, random_displace, minimum_image, Metropolis, energy
    use constant, only : Temperature, Name, sigma, epsilon_, density, Box_dimension, N_part, Proportion
    use constant, only : dr,Restart, simulation_time, Number_of_species, Freq_write, kJ_mol_to_J
    use position, only : Label, coord, identity_Label, dimers_interact, Energy_loop, Energy_average
    use mod_function, only : arithmetic_mean, geometric_mean, sort_increasing
    use mod_function, only : Lennard_Jones
    implicit none
    double precision:: tmp_numerical
    logical         :: searchB=.FALSE.
    integer         :: i,j
    ! values to test the outputs
    double precision, dimension(3)  :: atom_chosen, atom_displaced
    integer                         :: atom_index 
    double precision, dimension(2)  :: LJ_param_dimer
    ! values to run the simulation
    double precision :: energy_save, energy_new, energy_old, Delta_E, acceptance_ratio
    double precision, dimension(:), allocatable :: distances
    integer :: index, accepted_moves
    logical :: accept
    !
    ! ******************
    ! Reading the input
    ! ******************
    !
    call load_input()
    !
    ! *******************************
    ! Generation of starting position
    ! *******************************
    !
    do i = 1, 3
        if ( density /= -1 .and. N_part /= -1 .and. Box_dimension(i) /= -1) then
            allocate(coord(N_part,3), identity_Label(N_part))
            write(*,*) "We have everything, we will read the last position &
            of the i/o file and go directly to the simulation."
            call load_input_position_last(.false.)
            ! It could also be the first position.
            ! But we are not thinking that far yet.
            ! It will allow people to edit a file and give the coordinate they want
            ! To have personnalized starting conditions.
            write(*,*) "Identity label          x                       y                     z"
            do j = 1, size(identity_Label)
                write(*,*) identity_Label(j), coord(j,1),coord(j,2),coord(j,3)
            end do
            go to 1
        end if
    end do
    !
    ! Box_good(Box_dimension, searchB)
    ! searchB = OUTPUT T/F to say if we have the box as an unknown
    !
    ! Param_good(searchB,density,N_part)
    !
    ! If it's not the case we will compute the value missing and create and random box.
    !
    ! DNB() => Computation of the value missing
    ! ********DNB()**********
    ! Volume = Box(1)*Box(2)*Box(3)
    ! Density = N_part/Volume
    ! N_part = round(Volume*density)
    ! Box(1) = (N_part/density)**(1/3)
    ! We suppose the box to be a cube
    ! Box(2) = Box(1)
    ! Box(3) = Box(1)
    ! *********************
    do i = 1, 3
        if ( (density == -1 .or. N_part == -1) .and. Box_dimension(i) /= -1) then
            write(*,*) "We need at least two parameters"
        end if
    end do
    ! Verification of the box
    call Box_good(searchB)
    call DNB(searchB)
    ! Organizing the box from lower to higher dimension to be certain everything is regular.
    call sort_increasing(Box_dimension,Box_dimension)
    !
    ! Verification of dr
    !
    call dr_verif()
    !
    call Debug_print()
    !
    call sigma_epsilon_dimers()
    !
    ! Generation of the input
    call coord_gen() ! => Generation of the random coordinates/starting point.
    call random_atom()
    !
    !
    ! write_input_position() => Write the input position at the end of the file.
    ! It can also write the output position at the end of the file.
    call write_input_position()
    !
    ! The use of -1 is non-physical to amplify the fact it's a dummy variable.
    ! If Npart is equal to -1 we need to compute it with d and Bdim.
    ! If d is equal to -1 we need to compute it with Npart and Bdim.
    ! If Bdim have a -1 in the list, we need to compute it with Npart and d.
    !
    deallocate(coord,identity_Label)
    call load_input_position_last(.true.)
    !
    !do i = 1, size(coord,1)
    !        write(*,*) coord(i,:)
    !end do
    !write(*,*) identity_Label
    !
    !
    ! ***********************************
    ! Monte Carlo of Lennard Jones fluid
    ! ***********************************
    !
    1 if ( Number_of_species > 1 ) then
        write(*,*) "LJ of binary fluids"
        write(*,*) "Not available in this version"
        ! Test functions:
        write(*,*) "---TEST FUNCTION---"
        call arithmetic_mean(sigma,tmp_numerical)
        write(*,*) "Arithmetic mean"
        write(*,*) sigma
        write(*,*) tmp_numerical
        call geometric_mean(epsilon_,tmp_numerical)
        write(*,*) "Geometrical mean"
        write(*,*) epsilon_
        write(*,*) tmp_numerical
        call pick_dimers_data(1,1,LJ_param_dimer(1),LJ_param_dimer(2))
        write(*,*) LJ_param_dimer
        call pick_dimers_data(2,2,LJ_param_dimer(1),LJ_param_dimer(2))
        write(*,*) LJ_param_dimer
        write(*,*)
        do i = 1, size(dimers_interact,1)
            write(*,*) dimers_interact(i,:)
        end do
        stop
    else
        write(*,*) "MC of a LJ fluid."
    end if
    !call pick_dimers_data(1,1,LJ_param_dimer(1),LJ_param_dimer(2))
    !
    !call random_select(atom_chosen,atom_index)
    !call random_displace(atom_chosen,atom_index,atom_displaced)
    !write(*,*) atom_chosen
    !write(*,*) atom_index
    !write(*,*) atom_displaced
    !write(*,*) Label
    !write(*,*) Lennard_Jones(1.0d0,LJ_param_dimer)

    ! MC simulation starts

    allocate(distances(N_part))
    allocate(Energy_loop(simulation_time),Energy_average(simulation_time))
    accepted_moves = 0
    energy_save    = 0.0d0
    Energy_loop    = 0.0d0
    do i = 1, simulation_time
        call random_select(atom_chosen, atom_index)             ! pick an atom at random, and keep track of its position in coord()
        !write(*,*) atom_chosen,atom_index
        call minimum_image(atom_chosen, atom_index, distances)      ! calculate the distances with minimum image convention
        !write(*,*) "Distances:"
        !do j = 1, size(distances,1)
        !    write(*,*) distances(j)
        !end do
        call energy(atom_index, distances, energy_old)                  ! find the starting energy
        !write(*,*) "distance_old = ", distances
        !write(*,*) "Index =", atom_index
        call random_displace(atom_chosen, atom_index, atom_displaced)   ! perturb the position in random directions
        call minimum_image(atom_displaced, atom_index, distances)       ! recalculuate the distances
        call energy(atom_index, distances, energy_new)                  ! find the new energy
        !write(*,*) "distance_new = ", distances
        Delta_E = energy_new - energy_old
        !write(*,*) "DeltaE = ",Delta_E, "E_old = ", energy_old, "E_new =", energy_new
        call Metropolis(Delta_E*kJ_mol_to_J, accept)    ! use the Metropolis criterion to tell if we accept the new configuration
        ! We use the energy in Joules inside the Metropolis function
        ! To respect the units.
        if ( accept ) then
            do j = 1, 3
                coord(atom_index, j) = atom_displaced(j)
            end do
            !call PBC()
            !write(*,*) "We have a new potential energy:" 
            !write(*,*) energy_new, " kJ/mol"
            !write(*,*) "indexes = ", atom_index
            accepted_moves = accepted_moves + 1
            energy_save = energy_new
        else
            !write(*,*) "We keep the old potential energy:" 
            !write(*,*) energy_old, " kJ/mol"
            energy_save = energy_old
        end if
        Energy_loop(i) = energy_save    ! The saved energy is in kJ/mol
        Energy_average(i) = sum(Energy_loop)/dble(i)
        !acceptance_ratio = accepted_moves / i
        
        if ( MOD(i, Freq_write) == 0 ) then
            acceptance_ratio = dble(accepted_moves) / dble(i)
            !write(*,*) "Coordinates"
            !call write_matrix(coord)                        ! save the configuration
            write(*,*) "E = ",energy_save,"kJ/mol"          ! save the potential energy
            write(*,*) "E = ",energy_save*kJ_mol_to_J,"J"
            write(*,*) "accept_ratio = ",acceptance_ratio   ! and keep track of how many MC moves we accept/reject
            write(*,*) "Accept moves = ",accepted_moves
            write(*,*) "Simulation time =", i,"/",simulation_time
            write(*,"(A16,G14.6,A7)") "<Epot>_{atom} = ", Energy_average(i)," kJ/mol"
            ! Potential E of one atom => AVERAGE value of the potential E of one atom
            ! In kJ/mol
        end if
    end do
    call write_input_position()
end program main_main