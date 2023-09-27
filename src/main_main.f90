program main_main
    use sub, only : load_input,load_input_position, write_input_position, load_input_position_last
    use constant, only : Temperature, Name, sigma, epsilon_, density, Box_dimension, N_part
    use constant, only : dr,Restart, simulation_time, Number_of_species, Freq_write
    use position, only : Label, coord
    implicit none

    integer         :: i
    
    write(*,*) "Test"
    call load_input()
    write(*,*) "Aurevoir"
    write(*,*) "T =", Temperature
    write(*,*) "dr =", dr, "Restart =", Restart," Freq_write =", Freq_write,"Simu_time =", simulation_time
    write(*,*) "Nbr_spec =", Number_of_species
    write(*,*) Name
    write(*,*) sigma
    write(*,*) epsilon_
    write(*,*) Box_dimension
    write(*,*) "d =", density
    write(*,*) "Npart =", N_part
    !
    call load_input_position()
    !
    do i = 1, size(Label)
        write(*,*) Label(i), coord(i,1),coord(i,2),coord(i,3)
    end do
    ! We can even write in a file OwO
    call write_input_position()
    call load_input_position_last(.false.)
    ! The use of -1 is non-physical to amplify the fact it's a dummy variable.
    ! If Npart is equal to -1 we need to compute it with d and Bdim.
    ! If d is equal to -1 we need to compute it with Npart and Bdim.
    ! If Bdim have a -1 in the list, we need to compute it with Npart and d.
    !
    ! *******************************
    ! Generation of starting position
    ! *******************************
    !
    
    !
    ! ***********************************
    ! Monte Carlo of Lennard Jones fluid
    ! ***********************************
    !
    if ( Number_of_species > 1 ) then
        write(*,*) "LJ of binary fluids"
        write(*,*) "Not available in this version"
        stop
    else
        write(*,*) "MC of a LJ fluid."
    end if

end program main_main