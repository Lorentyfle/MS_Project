module sub
    implicit none
    
contains
    ! 27/09/2023 : Addition of load_input(), load_input_position()
    ! load_input_position_last(), write_input_position(), create_file()
    ! All this subroutines were coded by T.Jamin.
    ! 11/10/2023 : Addition of the reading of proportions in load_input()
    ! Bug correction for load_position_last() to read the last position.
    ! Variable Label goes from string to integer. Edits necessary are added.
    ! Addition of identity_Label that will be the tag of each individual molecules and not the type of molecules.
    ! The program will now not stop if NdB are given in the input file.
    ! 13/11/2023 : Addition of coord_gen()
    ! The Box dimension can now take floats
    ! 14/11/2023 : Addition of random_atom(), DNB() and Box_good()
    ! Coded by T.Jamin.
    subroutine load_input()
        ! This subroutine will read the specific input file and put the variables into global variable 
        ! through mod_cst.f90
        ! ***********
        ! * Modules *
        ! ***********
        use constant, only : Temperature
        use constant, only : Name, sigma, epsilon_, Proportion
        use constant, only : N_part, density, Box_dimension
        use constant, only : dr, Restart, simulation_time, Freq_write
        use constant, only : Number_of_species
        use mod_function, only : StripSpaces
        use position, only : Label
        implicit none
        ! ***************
        ! * Declaration *
        ! ***************
        character(len=24)       :: input_fort       = './input_output/input.txt'
        character(len=1)        :: target_comment = "#"
        character(len=12)       :: targetList(12)
        character(len=132)      :: line, tmp
        double precision        :: numerical_tmp
        logical                 :: good_line, done, multi_data_verif
        integer                 :: i, j, stat,k, integer_tmp
        integer                 :: nlines, input_lines, dimension_lattice
        !
        ! Initialisation of the target list
        !
        targetList(1) = "T ="           ! Temperature
        targetList(2) = "Name ="        ! Have N possible entities
        targetList(3) = "sigma ="       ! Have N possible entities
        targetList(4) = "epsilon ="     ! Have N possible entities
        targetList(5) = "density ="     ! Verification with N_part and bdimension.
        targetList(6) = "bdimension ="  ! Verification with density and N_part.
        targetList(7) = "N_part ="      ! Verification with density and bdimension.
        targetList(8) = "simtime ="     ! Time the simulation take in MC steps.
        targetList(9) = "dr ="          ! Needs to be in between 0 and 1.
        targetList(10)= "Freq_write ="  ! A frequency.
        targetList(11)= "Restart ="     ! Either 0 or 1.
        targetList(12)= "P ="           ! Have N possible proportions.
        !
        dimension_lattice = 0
        !
        !
        ! **************
        ! Find the datas
        ! **************
        !
        open(1,file=input_fort,position="rewind")
        ! ***********************
        ! Find number of species
        ! **********************
        nlines = 0
        tmp = ''
        done = .false.
        k = 0
        do while (.NOT. done)
            k = k + 1
            !write(*,*) k
            read(1, fmt="(a)", iostat=stat) line
            if(stat<0)then
                done = .true.
                exit
            end if
            ! Test for comments or blank lines
            good_line = .false.
            do i=1, len(line)
                dimension_lattice = 0
                if(line(i:i) == ' ') cycle ! Remove reading blanks
                if(line(i:i) == target_comment) then  ! Comment line
                    exit
                else
                    good_line = .true.
                    tmp = ''
                    if( TRIM(line(i:7)) == TRIM("sigma =")) then
                        integer_tmp = 0
                        do j = 1, len(line)
                            if ( line(j:j) == ";" ) then
                                integer_tmp = integer_tmp + 1
                            end if
                        end do
                        Number_of_species = integer_tmp
                        if (Number_of_species == 0) then
                            Number_of_species = 1
                        end if
                        done = .true.
                    end if
                end if
            end do
        end do
        !
        ! Allocate the data with respect of the size of the matrix.
        !
        allocate(sigma(Number_of_species),Name(Number_of_species), epsilon_(Number_of_species), Proportion(Number_of_species))
        allocate(Label(Number_of_species))
        !
        ! We put the name of the "Label" which will take number from 1 to infinity which will designate the number linked to
        ! A specific species.
        do i = 1, Number_of_species
            Label(i) = i
        end do
        !
        rewind(1)
        ! ************
        ! Extract data
        ! ************
        done = .false.
        k = 0
        do while(.NOT. done)
            k = k + 1
            !write(*,*) k
            read(1, fmt="(a)", iostat=stat) line
            if(stat<0)then
                done = .true.
                exit
            end if
            ! Test for comments or blank lines
            good_line = .false.
            do i=1, len(line)
                dimension_lattice = 0
                if(line(i:i) == ' ') cycle ! Remove reading blanks
                if(line(i:i) == target_comment) then  ! Comment line
                    exit
                else
                    good_line = .true.
                    tmp = ''
                    if(line(i:i) == "=") then
                        multi_data_verif = .false.
                        dimension_lattice = 0
                        !if ( density /= -1 .and. N_part /= -1 .and. &
                        !Box_dimension(1) /= -1 .and. Box_dimension(2) /= -1 .and. Box_dimension(3) /= -1 ) then
                        !    write(*,*) "Data overflow."
                        !    write(*,*) "Please do not enter the dimension of the box, the density and &
                        !    the number of particules at the same time."
                        !    stop
                        !end if
                        if(TRIM(line(1:i)) == TRIM(targetList(1))) then
                            tmp = TRIM(line(i+1:len(line)))
                            read(tmp,*) numerical_tmp
                            Temperature = numerical_tmp
                            exit
                        elseif ( TRIM(line(1:i)) == TRIM(targetList(2)) ) then
                            do j = 1, len(line)
                                if ( line(j:j) == ";" ) then
                                    multi_data_verif = .true.
                                end if
                            end do
                            if (multi_data_verif) then
                                integer_tmp = i+1
                                do j = 1, len(line)
                                    if ( line(j:j) == ";" ) then
                                        dimension_lattice = dimension_lattice + 1
                                        tmp = TRIM(line(integer_tmp:j-1))
                                        integer_tmp = j+1
                                        Name(dimension_lattice) = tmp
                                    end if
                                end do
                                integer_tmp = 0
                            else
                                tmp = TRIM(line(i+1:len(line)))
                                call StripSpaces(tmp)
                                Name(1) = tmp
                            end if
                            exit
                        elseif ( TRIM(line(1:i)) == TRIM(targetList(3)) ) then
                            do j = 1, len(line)
                                if ( line(j:j) == ";" ) then
                                    multi_data_verif = .true.
                                end if
                            end do
                            if (multi_data_verif) then
                                integer_tmp = i+1
                                do j = 1, len(line)
                                    if ( line(j:j) == ";" ) then
                                        dimension_lattice = dimension_lattice + 1
                                        tmp = TRIM(line(integer_tmp:j-1))
                                        integer_tmp = j+1
                                        read(tmp,*) numerical_tmp
                                        sigma(dimension_lattice) = numerical_tmp
                                    end if
                                end do
                                integer_tmp = 0                            
                            else
                                tmp = TRIM(line(i+1:len(line)))
                                read(tmp,*) numerical_tmp
                                sigma(1) = numerical_tmp
                                exit
                            end if
                        elseif ( TRIM(line(1:i)) == TRIM(targetList(4)) ) then
                            do j = 1, len(line)
                                if ( line(j:j) == ";" ) then
                                    multi_data_verif = .true.
                                end if
                            end do
                            if (multi_data_verif) then
                                integer_tmp = i+1
                                do j = 1, len(line)
                                    if ( line(j:j) == ";" ) then
                                        dimension_lattice = dimension_lattice + 1
                                        tmp = TRIM(line(integer_tmp:j-1))
                                        integer_tmp = j+1
                                        read(tmp,*) numerical_tmp
                                        epsilon_(dimension_lattice) = numerical_tmp
                                    end if
                                end do
                                integer_tmp = 0
                            else
                                tmp = TRIM(line(i+1:len(line)))
                                read(tmp,*) numerical_tmp
                                epsilon_(1) = numerical_tmp
                                exit
                            end if
                        elseif ( TRIM(line(1:i)) == TRIM(targetList(5)) ) then
                            tmp = TRIM(line(i+1:len(line)))
                            read(tmp,*) numerical_tmp
                            density = numerical_tmp
                            exit
                        elseif ( TRIM(line(1:i)) == TRIM(targetList(6)) ) then
                            integer_tmp = i + 1
                            do j = 1, len(line)
                                if ( line(j:j) == ";" ) then
                                    dimension_lattice = dimension_lattice + 1
                                    tmp = TRIM(line(integer_tmp:j-1))
                                    integer_tmp = j+1
                                    read(tmp,*) numerical_tmp
                                    Box_dimension(dimension_lattice) = numerical_tmp
                                end if
                            end do
                            integer_tmp = 0
                            exit
                        elseif ( TRIM(line(1:i)) == TRIM(targetList(7)) ) then
                            tmp = TRIM(line(i+1:len(line)))
                            read(tmp,*) integer_tmp
                            N_part = integer_tmp
                            exit
                        elseif ( TRIM(line(1:i)) == TRIM(targetList(8)) ) then
                            tmp = TRIM(line(i+1:len(line)))
                            read(tmp,*) integer_tmp
                            simulation_time = integer_tmp
                            exit
                        elseif ( TRIM(line(1:i)) == TRIM(targetList(9)) ) then
                            tmp = TRIM(line(i+1:len(line)))
                            read(tmp,*) numerical_tmp
                            dr = numerical_tmp
                            if ( dr < 0 .or. dr > 1 ) then
                                write(*,*) "Value out of range."
                                write(*,*) "dr must be in between 0 and 1."
                                stop
                            end if
                            exit
                        elseif ( TRIM(line(1:i)) == TRIM(targetList(10)) ) then
                            tmp = TRIM(line(i+1:len(line)))
                            read(tmp,*) integer_tmp
                            Freq_write = integer_tmp
                            exit
                        elseif ( TRIM(line(1:i)) == TRIM(targetList(11))) then
                            tmp = TRIM(line(i+1:len(line)))
                            read(tmp,*) integer_tmp
                            Restart = integer_tmp
                            if ( Restart /= 0 .and. Restart /= 1 ) then
                                write(*,*) "Unexpected value."
                                write(*,*) "Restart should be either 0 or 1."
                                write(*,*) "0 => We do not have a restart."
                                write(*,*) "1 => We have a restart."
                                stop
                            end if
                            exit
                        elseif ( TRIM(line(1:i)) == TRIM(targetList(12)) ) then
                            do j = 1, len(line)
                                if ( line(j:j) == ";" ) then
                                    multi_data_verif = .true.
                                end if
                            end do
                            if (multi_data_verif) then
                                integer_tmp = i+1
                                do j = 1, len(line)
                                    if ( line(j:j) == ";" ) then
                                        dimension_lattice = dimension_lattice + 1
                                        tmp = TRIM(line(integer_tmp:j-1))
                                        integer_tmp = j+1
                                        read(tmp,*) numerical_tmp
                                        Proportion(dimension_lattice) = numerical_tmp
                                    end if
                                end do
                                integer_tmp = 0                            
                            else
                                tmp = TRIM(line(i+1:len(line)))
                                read(tmp,*) numerical_tmp
                                sigma(1) = numerical_tmp
                                exit
                            end if
                            ! We can add more loop if we need to search more datas/variables
                        end if
                        exit
                    end if
                end if
            end do
            if (good_line) input_lines = input_lines+1
        end do
        close(1)
    end subroutine load_input

    subroutine load_input_position(allocated_value)
        use position, only: identity_Label, coord
        use mod_function, only : StripSpaces
        implicit none
        !
        ! Here the input of the position will be made.
        ! We need to follow this format :
        ! LABEL    x     y     z
        ! We take the identity label with respect to the number of particles.
        ! x, y and z will be double precision
        ! This function will only read the FIRST position
        !
        logical, intent(in)     :: allocated_value ! Do we need to allocate the Label and coord?
        character(len=30)       :: input_fort       = './input_output/io_pos_opti.txt'
        character(len=1)        :: target_comment = "#"
        character(len=132)      :: line, tmp
        logical                 :: done, file_exists
        integer                 :: nlines, integer_tmp, integer_tmp2, stat
        integer                 :: i,j,k, rank_data, dim_coord
        double precision        :: numerical_tmp
        !
        ! ***********************
        ! Find number of species
        ! **********************
        !
        nlines = 0
        tmp = ''
        done = .false.
        k = 0
        inquire(file=input_fort, exist=file_exists)
        if ( .NOT. file_exists ) then
            write(*,*) "Undetected input file."
            call create_file(input_fort)
        end if

        open(1,file=input_fort,position="rewind")
        !
        nlines = 0
        do
            read(1,*,end=10) line
            nlines = nlines + 1
            do i=1, len(line)                
                if(line(i:1) == ' ') cycle ! Remove reading blanks
                if(line(i:1) == target_comment) then  ! Comment line
                    nlines = nlines - 1
                    exit
                elseif (line(i:6) == "-STOP-") then
                    nlines = nlines - 1
                    go to 10
                end if
            end do
        end do
        10  rewind(1)
        !
        ! We allocate the space for the position of each species
        !
        if ( allocated_value ) then
            allocate(identity_Label(nlines),coord(nlines,3))
        end if
        !
        ! We put the data of the position of each species.
        !
        ! ************
        ! Extract data
        ! ************
        !
        done = .false.
        k = 0
        rank_data = 0
        do while(.NOT. done)
            read(1, fmt="(a)", iostat=stat) line
            if(stat<0 .or. k == nlines)then
                done = .true.
                exit
            end if
            ! Test for comments or blank lines
            do i=1, len(line)
                if(line(i:i) == ' ') cycle ! Remove reading blanks
                if(line(i:i) == target_comment) then  ! Comment line
                    exit
                else
                    tmp = ''
                    if(line(i:i) == ";") then 
                        k = k+1
                        rank_data = rank_data + 1
                        tmp = TRIM(line(1:i-1))
                        call StripSpaces(tmp)
                        !write(*,*) tmp
                        read(tmp, *) integer_tmp2
                        identity_Label(rank_data) = integer_tmp2
                        ! Here we will extract the x,y,z positions
                        tmp = ""
                        integer_tmp = i+1
                        dim_coord = 0
                        do j = i+1, len(line)
                            if ( line(j:j) == ";" ) then
                                dim_coord = dim_coord + 1
                                tmp = TRIM(line(integer_tmp:j-1))
                                call StripSpaces(tmp)
                                read(tmp,*) numerical_tmp
                                !write(*,*) tmp
                                integer_tmp = j+1
                                coord(rank_data, dim_coord) = numerical_tmp
                            end if
                        end do
                        integer_tmp = 0
                        exit
                    end if
                end if
            end do
        end do
        close(1)
    end subroutine load_input_position

    subroutine load_input_position_last(allocated_value)
        use position, only: identity_Label, coord
        use mod_function, only : StripSpaces
        implicit none
        !
        ! Here the input of the position will be made.
        ! We need to follow this format :
        ! LABEL    x     y     z
        ! We take the identity label with respect to the number of particles.
        ! x, y and z will be double precision
        ! This function will only read the LAST position
        !
        logical, intent(in)     :: allocated_value ! Do we need to allocate the Label and coord?
        character(len=30)       :: input_fort       = './input_output/io_pos_opti.txt'
        character(len=1)        :: target_comment = "#"
        character(len=132)      :: line, tmp
        logical                 :: done, file_exists
        integer                 :: nlines, nSTOP, integer_tmp, integer_tmp2, stat
        integer                 :: i,j,k,k_stop, rank_data, dim_coord
        double precision        :: numerical_tmp
        !
        ! ***********************
        ! Find number of species
        ! **********************
        !
        nlines = 0
        tmp = ''
        done = .false.
        k = 0
        inquire(file=input_fort, exist=file_exists)
        if ( .NOT. file_exists ) then
            write(*,*) "Undetected input file."
            call create_file(input_fort)
        end if
        !
        open(1,file=input_fort,position="rewind")
        !
        nlines = 0
        ! We suppose that we do not loose particules/atoms/molecules in our system.
        ! So we can take the first step to know the size of our matrix.
        do
            read(1,*,end=10) line
            nlines = nlines + 1
            do i=1, len(line)                
                if(line(i:1) == ' ') cycle ! Remove reading blanks
                if(line(i:1) == target_comment) then  ! Comment line
                    nlines = nlines - 1
                    exit
                elseif (line(i:6) == "-STOP-") then
                    nlines = nlines - 1
                    go to 10
                end if
            end do
        end do
        10  rewind(1)
        ! We count the number of loop done.
        nSTOP = 0
        do
            read(1,*,end=12) line
            do i=1, len(line)                
                if(line(i:1) == ' ') cycle ! Remove reading blanks
                if(line(i:1) == target_comment) then  ! Comment line
                    exit
                elseif (line(i:6) == "-STOP-") then
                    nSTOP = nSTOP + 1
                end if
            end do
        end do
        12  rewind(1)
        !
        ! We allocate the space for the position of each species
        !
        if ( allocated_value ) then
            allocate(identity_Label(nlines),coord(nlines,3))
        end if
        !
        ! We put the data of the position of each species.
        !
        ! ************
        ! Extract data
        ! ************
        !
        done = .false.
        k = 0
        k_stop = 0
        rank_data = 0
        do while(.NOT. done)
            read(1, fmt="(a)", iostat=stat) line
            if(stat<0 .or. k == nlines)then
                done = .true.
                exit
            end if
            ! Test for comments or blank lines
            do i=1, len(line)
                if ( line(i:7) == " -STOP-" .or. line(i:6) == "-STOP-" ) then
                    ! Here we take into account the space!
                    k_stop = k_stop + 1
                end if
                ! Do not read any data until we are at the last one.
                if ( k_stop /= nSTOP-1 ) then
                    exit
                end if
                if(line(i:i) == ' ') cycle ! Remove reading blanks
                if(line(i:i) == target_comment) then  ! Comment line
                    exit
                else
                    tmp = ''
                    if(line(i:i) == ";") then 
                        k = k+1
                        rank_data = rank_data + 1
                        tmp = TRIM(line(1:i-1))
                        call StripSpaces(tmp)
                        !write(*,*) tmp, nSTOP
                        read(tmp,*) integer_tmp2
                        identity_Label(rank_data) = integer_tmp2
                        ! Here we will extract the x,y,z positions
                        tmp = ""
                        integer_tmp = i+1
                        dim_coord = 0
                        do j = i+1, len(line)
                            if ( line(j:j) == ";" ) then
                                dim_coord = dim_coord + 1
                                tmp = TRIM(line(integer_tmp:j-1))
                                call StripSpaces(tmp)
                                read(tmp,*) numerical_tmp
                                !write(*,*) tmp
                                integer_tmp = j+1
                                coord(rank_data, dim_coord) = numerical_tmp
                            end if
                        end do
                        integer_tmp = 0
                        exit
                    end if
                end if
            end do
        end do
        close(1)
    end subroutine load_input_position_last

    subroutine write_input_position()
        ! This subroutine wil only take care of writing, watever the data. It will append it at the end of the input/output file.
        use position, only: identity_Label, coord
        implicit none
        character(len=30)       :: input_fort       = './input_output/io_pos_opti.txt'
        logical                 :: file_exists
        integer                 :: i
        !
        ! Verification that the file exist
        inquire(file=input_fort, exist=file_exists)
        if (file_exists) then
            open(12, file=input_fort, status="old", position="append", action="write")
        else
            open(12, file=input_fort, status="new", action="write")
            write(12,*) "# Label    x    y   z"
        end if
        do i = 1, size(identity_Label)
            if ( .not. identity_Label(i) == 0 ) then
                write(12,*) identity_Label(i),";",coord(i,1),";",coord(i,2),";",coord(i,3),";"
            end if
        end do
        write(12,*) "-STOP-"
        close(12)
    end subroutine write_input_position

    subroutine create_file(filename)
        implicit none
        
        character(*), intent(in) :: filename
        integer                  :: unit=20
        integer                  :: ierr
    
        open(unit,file=filename,status='unknown', action="write",iostat=ierr)
        
        if ( ierr /=0 ) then
            write(*,*) "Error: Unable to create the file."
            stop
        end if
        write(unit,*) "# Label    x    y   z"
        write(unit,*) "1;0;0;0;" ! This line will be removed when we will be able to generate coordinates.
        write(unit,*) "-STOP-"
        write(unit,*)
        close(unit)
    end subroutine create_file

    ! /!\ Verify if coord and identity label are allocated /!\
    ! If we need to create them, they will surely aren't
    subroutine DNB(searchB)
        !
        ! ***********
        ! * Modules *
        ! ***********
        use constant, only : N_part, density, Box_dimension
        !
        implicit none
        logical, intent(in) :: searchB
        double precision    :: Volume=1.0d0
        integer             :: i
        !
        if ( .not. searchB ) then
            do i = 1, 3
                Volume = Box_dimension(i)*Volume
            end do
        end if
        !
        if (searchB) then
            do i = 1, 3
                Box_dimension(i) = (density/N_part)**(1/3)
            end do
        elseif (density == - 1) then
            density = N_part/Volume
        else
            N_part = Volume*density
        end if
        !
    end subroutine DNB

    subroutine Box_good(searchB)
        !
        ! ***********
        ! * Modules *
        ! ***********
        use constant, only : N_part, density, Box_dimension
        !
        implicit none
        logical, intent(out) :: searchB
        integer :: i
        double precision :: Do_we_have_box
        !
        ! Initialisation
        !
        Do_we_have_box = 0.d0
        searchB = .FALSE.
        !
        do i = 1, 3
            if ( Box_dimension(i) == -1 ) then
                Do_we_have_box = Do_we_have_box - 1
            end if
        end do
        if ( Do_we_have_box == - 3 ) then
            searchB = .TRUE.
        elseif ( Do_we_have_box == -2 ) then
            write(*,*) "We will assume a cubic box."
            Box_dimension(2) = Box_dimension(1)
            Box_dimension(3) = Box_dimension(1)
        elseif (Do_we_have_box == -1) then
            if ( Box_dimension(1) == Box_dimension(2) ) then
                write(*,*) "We will assume a cubic box."
                Box_dimension(3) = Box_dimension(1)
            else
                write(*,*) "Please input three dimensions for the box."
                stop
            end if
        end if

    end subroutine Box_good

    subroutine coord_gen()
        ! This subroutine will generate a grid depending on the values of the box dimension.
        ! If the value of the box is inferior to the maximum sigma, the program will stop.
        ! ***********
        ! * Modules *
        ! ***********
        use constant, only : Name, sigma, epsilon_, Proportion
        use constant, only : N_part, density, Box_dimension
        use constant, only : dr, Restart, simulation_time, Freq_write
        use constant, only : Number_of_species
        use position, only : identity_Label, coord
        use mod_function, only : sort_increasing, IS_ODD
        implicit none
        
        double precision, dimension(3):: Organise_Box
        integer :: n,m,l
        integer :: N_inside,maximum_sigma,P
        integer :: i,j,k
        logical :: IS_ODD_loop
        ! We sort the values of the box
        call sort_increasing(Box_dimension,Organise_Box)
        ! We call the variables
        n = 1
        N_inside = 0
        do while (N_inside < N_part)
            n = n+1
            m = n*ceiling(Organise_Box(2)/Organise_Box(1))
            l = n*ceiling(Organise_Box(3)/Organise_Box(1))
            N_inside = n*m*l
        end do
        maximum_sigma = maxloc(sigma,dim=1)
        ! Verification to avoid generating too small boxes.
        if ( Organise_Box(1)/n < sigma(maximum_sigma) .or. &
        Organise_Box(2)/m < sigma(maximum_sigma) .or. Organise_Box(3)/l < sigma(maximum_sigma) ) then
            write(*,*) "Error. The value of sigma cannot be inferior to the minimum value of the box."
            write(*,*) "To solve this issue, you can decrease the density," 
            write(*,*) "the number of total atoms or, increase the value of the box."
            write(*,*) "Box dimensions:"
            write(*,*) Organise_Box(1)/n,Organise_Box(2)/m,Organise_Box(3)/l
            write(*,*) "Maximum value of sigma:"
            write(*,*) sigma(maximum_sigma)
            stop
        end if
        !
        ! (n,m,l) is the number of "cubes" per side length.
        allocate(coord(N_inside,3))
        P = 0
        do while (P < N_inside)
            do i = 1, 2*n-1
                do j = 1, 2*m-1
                    do k = 1, 2*l-1
                        call IS_ODD(i,j,k,IS_ODD_loop)
                        if ( IS_ODD_loop ) then
                            P = P + 1
                            coord(P,1) = i * Organise_Box(1)/(2*n)
                            coord(P,2) = j * Organise_Box(2)/(2*m)
                            coord(P,3) = k * Organise_Box(3)/(2*l)
                        end if
                    end do
                end do
            end do
        end do
        !write(*,*) "n,l,m"
        !write(*,*) n,l,m
        !write(*,*) "Number of sites"
        !write(*,*) N_inside
        !write(*,*) "Small box dimensions"
        !write(*,*) Organise_Box(1)/n, Organise_Box(2)/m,Organise_Box(3)/l
        !write(*,*) "Total dimension"
        !do j = 1, size(coord,1)
        !    write(*,*) (coord(j,i), i=1, size(coord,2))
        !end do
        !stop
    end subroutine coord_gen

    subroutine random_atom()
        ! This subroutine will put randomly one atom inside one position.
        ! It will create the identity_Label vector
        ! ***********
        ! * Modules *
        ! ***********
        use constant, only : Name, sigma, epsilon_, Proportion
        use constant, only : N_part, density, Box_dimension
        use constant, only : dr, Restart, simulation_time, Freq_write
        use constant, only : Number_of_species
        use position, only: identity_Label, coord, Label
        implicit none
    
        integer :: i,j
        integer :: N_cell
        integer :: Atom_Proportion=0
        double precision :: Rand
        double precision, dimension(:), allocatable :: N_particule_in_species
        integer, dimension(:), allocatable :: counter_N_particule_in_species
        double precision, dimension(:), allocatable :: weights, Proport
        ! To do, change the names : Proportion to Numerator_Proportion

        N_cell = size(coord,1)

        allocate(weights(Number_of_species+1))
        allocate(N_particule_in_species(Number_of_species))
        allocate(counter_N_particule_in_species(Number_of_species+1))
        allocate(identity_Label(size(coord,1)))
        ! Computation of the weights
        do i = 1, Number_of_species
            Atom_Proportion = Atom_Proportion + Proportion(i)
        end do
        do i = 1, Number_of_species
            Proport = Proportion/Atom_Proportion
        end do
        do i = 1, Number_of_species
            N_particule_in_species(i) = (Proport(i) * N_part)
            counter_N_particule_in_species(i) = floor(N_particule_in_species(i)) ! For the counter
        end do
        do i = 1, Number_of_species
            weights(i) = (N_particule_in_species(i))/N_cell
        end do
        ! Initialisation
        weights(Number_of_species+1) = 0
        counter_N_particule_in_species(Number_of_species+1) = 0
        do i = 1, Number_of_species
            weights(Number_of_species+1) = weights(i) + weights(Number_of_species+1)
            ! We now add the void into the counter
            counter_N_particule_in_species(Number_of_species+1) = counter_N_particule_in_species(Number_of_species+1) &
            + counter_N_particule_in_species(i)
        end do
        weights(Number_of_species+1) = 1 - weights(Number_of_species+1)
        ! Addition of void into the counter
        counter_N_particule_in_species(Number_of_species+1) = N_cell - counter_N_particule_in_species(Number_of_species+1)
        ! Initialisation of the label
        identity_Label = 0
        ! *****************
        ! Random filling of the species inside the lattice
        do i = 1, size(coord,1)
            1 do j = 1, Number_of_species + 1
                call random_number(Rand)
                if ( weights(j) > Rand .and. .not. counter_N_particule_in_species(j) == 0) then
                    if ( j == (Number_of_species + 1) ) then
                        identity_Label(i) = 0
                        counter_N_particule_in_species(j) = counter_N_particule_in_species(j) - 1
                        exit
                    else
                        identity_Label(i) = Label(j)
                        counter_N_particule_in_species(j) = counter_N_particule_in_species(j) - 1
                        exit
                    end if
                elseif ( j == (Number_of_species + 1)) then
                    go to 1
                end if
            end do
        end do
        ! Deallocations to do
    end subroutine random_atom

    subroutine Debug_print()
        use constant, only : Temperature, Name, sigma, epsilon_, density, Box_dimension, N_part, Proportion
        use constant, only : dr,Restart, simulation_time, Number_of_species, Freq_write
        use position, only : Label, coord, identity_Label
        use mod_function, only : arithmetic_mean, geometric_mean,sort_increasing
        !
        implicit none
        integer     :: i,j
        
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
    end subroutine Debug_print
end module sub