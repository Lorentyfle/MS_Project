program main_main
    ! 11/10/2023 : Addition of the squeletton of the main of the program.
    ! Coded by T.Jamin.
    use sub, only : load_input,load_input_position, write_input_position, load_input_position_last
    use constant, only : Temperature, Name, sigma, epsilon_, density, Box_dimension, N_part, Proportion
    use constant, only : dr,Restart, simulation_time, Number_of_species, Freq_write
    use position, only : Label, coord, identity_Label
    use mod_function, only : arithmetic_mean, geometric_mean
    implicit none
    double precision:: tmp_numerical
    integer         :: i,j
    !
    ! ******************
    ! Reading the input
    ! ******************
    !
    call load_input()
    ! Shows that the input are well taken:
    write(*,*) "T =", Temperature
    write(*,*) "dr =", dr, "Restart =", Restart," Freq_write =", Freq_write,"Simu_time =", simulation_time
    write(*,*) "Nbr_spec =", Number_of_species
    write(*,*) "Label     sigma       epsilon        Proportion      Name"
    do i = 1, Number_of_species
        write(*,*) Label(i), sigma(i), epsilon_(i), Proportion(i), Name(i)
    end do
    write(*,*) "Box dimension ="
    write(*,*) Box_dimension
    write(*,*) "d =", density
    write(*,*) "Npart =", N_part
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
            !allocate(coord(N_part,3), identity_Label(N_part))
            write(*,*) "We need at least two parameters"
        end if
    end do
    allocate(coord(N_part,3), identity_Label(N_part))

    ! coord_gen() => Generation of the random coordinates/starting point.

    ! **For now we read the begining of the file until the functions are done**
    call load_input_position(.false.)

    ! write_input_position() => Write the input position at the end of the file.
    ! It can also write the output position at the end of the file.
    call write_input_position()
    !
    do i = 1, size(identity_Label)
        write(*,*) identity_Label(i), coord(i,1),coord(i,2),coord(i,3)
    end do
    !
    ! The use of -1 is non-physical to amplify the fact it's a dummy variable.
    ! If Npart is equal to -1 we need to compute it with d and Bdim.
    ! If d is equal to -1 we need to compute it with Npart and Bdim.
    ! If Bdim have a -1 in the list, we need to compute it with Npart and d.
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

end program main_main