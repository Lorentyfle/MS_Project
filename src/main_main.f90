program main_main
    ! 11/10/2023 : Addition of the squeletton of the main of the program.
    ! Coded by T.Jamin.
    use sub, only : load_input, load_input_position, write_input_position, load_input_position_last
    use sub, only : coord_gen, random_atom, DNB, dr_verif, Box_good, displacement_verif,verif_DNB
    use sub, only : Debug_print, write_matrix
    use sub, only : load_output_log_last,write_output_log,write_output_energy_last
    use sub, only : sigma_epsilon_dimers, pick_dimers_data, PBC
    use sub, only : random_select, random_displace, minimum_image, Metropolis, energy
    use constant, only : Temperature, Name, sigma, epsilon_, density, Box_dimension, N_part, Proportion
    use constant, only : dr,Restart, simulation_time, Number_of_species, Freq_write, kJ_mol_to_J
    use position, only : Label, coord, identity_Label, dimers_interact, Energy_loop, Energy_average
    use mod_function, only : arithmetic_mean, geometric_mean, sort_increasing
    use mod_function, only : Lennard_Jones,sum_KQ,stop_at_space
    use rdf, only : partial_rdf,write_g_of_r

    implicit none
    double precision:: tmp_numerical
    logical         :: searchB=.FALSE.
    integer         :: i,j
    ! values to test the outputs
    double precision, dimension(2)  :: LJ_param_dimer
    logical                         :: file_exists
    character(len=30)                       :: out_energy='./input_output/out_energy.txt'
    ! values to run the simulation
    double precision, dimension(3)  :: atom_chosen, atom_displaced
    integer                         :: atom_index
    double precision :: energy_save, energy_new, energy_old, Delta_E, acceptance_ratio
    double precision, dimension(:), allocatable :: distances
    integer :: index, accepted_moves
    logical :: accept,begining_sim=.true.,begining_sim2=.true.
    integer,dimension(:),allocatable            :: Step_restart     ! number of species already moved from the previous simulation
    double precision,dimension(:),allocatable   :: Energy_restart   ! value of the energy in function of the species from the previous simulation
    double precision,dimension(:),allocatable   :: Vector_output    ! Vector of the data to feed to the write_output_energy_last()
    double precision,dimension(:),allocatable   :: av_E_end         ! Average energy of the species at the end of the simulation
    double precision,dimension(:,:),allocatable :: av_E             ! Average energy of the species during the simulation.
    double precision,dimension(:,:),allocatable :: E_species        ! Energy of the species during the simulation
    integer,dimension(:),allocatable            :: cKQ              ! Counter of the Kind of species in Question
    integer                                     :: KQ               ! Kind of species in Question
    !
    ! stuff for the RDF
    integer :: histogram_boxes
    double precision, dimension(:,:), allocatable :: g_of_r
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
    ! The use of -1 is non-physical to amplify the fact it's a dummy variable.
    ! If Npart is equal to -1 we need to compute it with d and Bdim.
    ! If d is equal to -1 we need to compute it with Npart and Bdim.
    ! If Bdim have a -1 in the list, we need to compute it with Npart and d.
    !
    call verif_DNB()
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
    ! Verification of displacement
    !
    call displacement_verif()
    !
    call Debug_print()
    !
    call sigma_epsilon_dimers()
    !
    if ( Restart /= 1 ) then ! We do this actions only if we do not need to restart. If we don't restart, it's no use to compute this again.
        ! Generation of the input
        call coord_gen() ! => Generation of the random coordinates/starting point.
        call random_atom()
        ! write_input_position() => Write the input position at the end of the file.
        ! It can also write the output position at the end of the file.
        call write_input_position()
    end if
    !
    !
    if ( allocated(coord) .or. allocated(identity_Label) ) then
        deallocate(coord,identity_Label)
    end if
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
        write(*,*) "Normally not available in this version"
        ! Test functions:
        !write(*,*) "---TEST FUNCTION---"
        !call arithmetic_mean(sigma,tmp_numerical)
        !write(*,*) "Arithmetic mean"
        !write(*,*) sigma
        !write(*,*) tmp_numerical
        !call geometric_mean(epsilon_,tmp_numerical)
        !write(*,*) "Geometrical mean"
        !write(*,*) epsilon_
        !write(*,*) tmp_numerical
        !call pick_dimers_data(1,1,LJ_param_dimer(1),LJ_param_dimer(2))
        !write(*,*) LJ_param_dimer
        !call pick_dimers_data(2,2,LJ_param_dimer(1),LJ_param_dimer(2))
        !write(*,*) LJ_param_dimer
        !write(*,*)
        !do i = 1, size(dimers_interact,1)
        !    write(*,*) dimers_interact(i,:)
        !end do
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
    ! *********************
    ! MC simulation starts
    ! *********************
    ! Allocation variables
    allocate(distances(N_part))
    allocate(Energy_loop(simulation_time),Energy_average(simulation_time))
    allocate(Vector_output(4))  ! For now we put 3 because we have 3 outputs.
    allocate(Step_restart(Number_of_species),Energy_restart(Number_of_species))
    allocate(av_E(simulation_time,Number_of_species))
    allocate(av_E_end(Number_of_species))
    allocate(cKQ(Number_of_species))
    !
    if ( Restart == 1 ) then
        ! We restart
        allocate(E_species(simulation_time+1,Number_of_species))
        ! Here we add one more room for taking the previous calculations
        ! /!\ We need to not feed into Data_vector() Energy at simulation_time+1
    else
        ! We do not restart
        allocate(E_species(simulation_time,Number_of_species))
    end if
    ! *********************************
    ! Initialisation of the variables.
    ! *********************************
    accepted_moves = 0
    KQ             = 0
    cKQ            = 0
    av_E_end       = 0.0d0
    Energy_restart = 0.0d0
    Step_restart   = 0
    av_E           = 0.0d0
    E_species      = 0.0d0
    energy_save    = 0.0d0
    Energy_loop    = 0.0d0
    Vector_output  = 0.0d0
    !
    if ( Restart == 1 ) then
        call load_output_log_last(Energy_restart,Step_restart)
        ! We put the datas inside the specific arrays for the simulation rerun.
        cKQ = Step_restart
        E_species(simulation_time+1,:) = Energy_restart*Step_restart
        ! We need to multiply by Step_restart, because it's the Energy we will obtain at the end
        !
        ! Be careful to see if this way of writing can work.
        !write(*,*) cKQ
        !write(*,*)
        !call write_matrix(E_species)
    end if
    !
    !
    do i = 1, simulation_time
        ! MC() => energy_save, atom_index
        call random_select(atom_chosen, atom_index)             ! pick an atom at random, and keep track of its position in coord()
        call minimum_image(atom_chosen, atom_index, distances)      ! calculate the distances with minimum image convention
        call energy(atom_index, distances, energy_old)                  ! find the starting energy
        call random_displace(atom_chosen, atom_index, atom_displaced)   ! perturb the position in random directions
        call minimum_image(atom_displaced, atom_index, distances)       ! recalculuate the distances
        call energy(atom_index, distances, energy_new)                  ! find the new energy
        Delta_E = energy_new - energy_old
        call Metropolis(Delta_E*kJ_mol_to_J, accept)    ! use the Metropolis criterion to tell if we accept the new configuration
        ! We use the energy in Joules inside the Metropolis function
        ! To respect the units.
        if ( accept ) then
            do j = 1, 3
                coord(atom_index, j) = atom_displaced(j)
            end do
            accepted_moves = accepted_moves + 1
            energy_save = energy_new
        else
            energy_save = energy_old
        end if
        !
        ! => atom_index, energy_save (kJ/mol)
        !
        KQ = identity_Label(atom_index)
        cKQ(KQ) = cKQ(KQ) + 1
        E_species(i,KQ) = energy_save/2.0d0 ! We divide the energy by two because the interactions are counted twice!
        av_E(i,KQ) = sum_KQ(E_species,KQ)/cKQ(KQ)
        av_E_end(KQ) = av_E(i,KQ)
        ! Old energy calculation
        !Energy_loop(i) = energy_save    ! The saved energy is in kJ/mol
        !Energy_average(i) = sum(Energy_loop)/dble(i)
        !acceptance_ratio = accepted_moves / i
        if ( MOD(i, Freq_write) == 0 ) then
            acceptance_ratio = dble(accepted_moves) / dble(i)
            ! The acceptance will be taken into account in the restart for the next update.
            !write(*,*) "Coordinates"
            !call write_matrix(coord)               ! save the configuration
            ! ***********************************************************
            ! Initialisation of the Vector_output() for saving the datas.
            ! ***********************************************************
            Vector_output(1) = KQ                 ! Label_species
            Vector_output(2) = sum(cKQ)           ! Simulation step
            Vector_output(3) = av_E(i,KQ)         ! <E_i>_species
            Vector_output(4) = E_species(i,KQ)    ! E_i_species
            ! **************************
            call write_output_energy_last(Vector_output,begining_sim)
            if ( i == simulation_time ) then
                call write_output_log(cKQ,av_E_end,begining_sim2)
                begining_sim2 = .false.
            end if
            if ( begining_sim ) then
                begining_sim = .false.
            end if
        elseif (MOD(i,Freq_write*2) == 0) then
            write(*,*) "E = ",energy_save,"kJ/mol"          ! save the potential energy
            write(*,*) "E = ",energy_save*kJ_mol_to_J,"J"
            write(*,*) "accept_ratio = ",acceptance_ratio   ! and keep track of how many MC moves we accept/reject
            write(*,*) "Accept moves = ",accepted_moves
            write(*,*) "Simulation time =", i,"/",simulation_time
            write(*,"(A16,G14.6,A7)") "<Epot>_{atom} = ", Energy_average(i)," kJ/mol"
            ! Potential E of one atom => AVERAGE value of the potential E of one atom
            ! In kJ/mol
            ! - 5.6 kJ/mol per atom
        end if
    end do
    call write_input_position()
    ! calculate the radial distribution function
    histogram_boxes = 200
    do i = 1, Number_of_species
        do j = i, Number_of_species
            call partial_rdf(i,j,histogram_boxes,g_of_r)
            call write_g_of_r(i,j,g_of_r,stop_at_space(Name(Label(i)),3),stop_at_space(Name(Label(j)),3))
            !write(*,*) "RDF for particles of type ", i, " with particles of type ", j, " = "
        end do
    end do
    !call write_matrix(g_of_r)
end program main_main