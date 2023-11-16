program main_main
    ! 11/10/2023 : Addition of the squeletton of the main of the program.
    ! Coded by T.Jamin.
    use sub, only : load_input,load_input_position, write_input_position, load_input_position_last
    use sub, only : coord_gen, random_atom,DNB, dr_verif,Box_good
    use sub, only : Debug_print
    use sub, only : random_select,random_displace,minimum_image,Metropolis
    use constant, only : Temperature, Name, sigma, epsilon_, density, Box_dimension, N_part, Proportion
    use constant, only : dr,Restart, simulation_time, Number_of_species, Freq_write
    use position, only : Label, coord, identity_Label
    use mod_function, only : arithmetic_mean, geometric_mean,sort_increasing
    implicit none
    double precision:: tmp_numerical
    logical         :: searchB=.FALSE.
    integer         :: i,j
    ! values to test the outputs
    double precision, dimension(3):: atom_chosen, atom_displaced
    integer                       :: atom_index 
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
    !
    ! Verification of dr
    !
    call dr_verif()
    !
    call Debug_print()
    ! Generation of the input
    call coord_gen() ! => Generation of the random coordinates/starting point.
    call random_atom()
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
    
    !do i = 1, size(coord,1)
    !        write(*,*) coord(i,:)
    !end do
    !write(*,*) identity_Label
    
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
        stop
    else
        write(*,*) "MC of a LJ fluid."
    end if
    call random_select(atom_chosen,atom_index)
    call random_displace(atom_chosen,atom_index,atom_displaced)
    write(*,*) atom_chosen
    write(*,*) atom_index
    write(*,*) atom_displaced
    ! do i = 1, simulation_time
    !     call random_select( atom, index)                      ! pick an atom at random, and keep track of its position in coord()
        
    !     energy_old = energy(cutoff, coord, atom, index)             ! find the starting energy
        
    !     call random_displace(atom_identity, atom, move_atom)        ! perturb the position in random directions
        
    !     energy_new = energy(cutoff, coord, move_atom, index)        ! find the new energy
        
    !     Delta_E = energy_old - energy_new

    !     call Metropolis(Delta_E, T, accept)                         ! use the Metropolis criterion to tell if we accept the new configuration
    !     if ( accept ) then
    !         do j = 1, 3
    !             coord(index, j) = move_atom(j)
    !         end do
    !         accepted_moves = accepted_moves + 1
    !     end if

    !     acceptance_ratio = accepted_moves / i
        
    !     if ( MOD(i, Freq_write) == 0 ) then
    !         write_to_output(coord, energy_new, acceptance_ratio)    ! save the configuration, potential energy 
    !     end if                                                      ! and keep track of how many MC moves we accept/reject

    !     i = i + 1
    ! end do
    !
end program main_main