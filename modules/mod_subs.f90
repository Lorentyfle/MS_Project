module sub
    implicit none
    
contains
    ! 27/09/2023 : Addition of load_input(), load_input_position()
    ! load_input_position_last(), write_input_position(), create_file()
    ! All this subroutines were coded by T.Jamin.
    subroutine load_input()
        
        ! ***********
        ! * Modules *
        ! ***********
        use constant, only : Temperature, Name, sigma, epsilon_
        use constant, only : N_part, density, Box_dimension
        use constant, only : dr, Restart, simulation_time, Freq_write
        use constant, only : Number_of_species
        use mod_function, only : StripSpaces
        implicit none
        ! ***************
        ! * Declaration *
        ! ***************
        character(len=24)       :: input_fort       = './input_output/input.txt'
        character(len=1)        :: target_comment = "#"
        character(len=12)       :: targetList(11)
        character(len=132)      :: line, tmp
        double precision        :: numerical_tmp
        logical                 :: good_line, done, multi_data_verif
        integer                 :: i, j, stat,k, integer_tmp
        integer                 :: nlines, input_lines, dimension_lattice
        !
        ! Initialisation of the target list
        !
        targetList(1) = "T ="
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
        allocate(sigma(Number_of_species),Name(Number_of_species), epsilon_(Number_of_species))
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
                        if ( density /= -1 .and. N_part /= -1 .and. &
                        Box_dimension(1) /= -1 .and. Box_dimension(2) /= -1 .and. Box_dimension(3) /= -1 ) then
                            write(*,*) "Data overflow."
                            write(*,*) "Please do not enter the dimension of the box, the density and &
                            the number of particules at the same time."
                            stop
                        end if
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
                            do j = 1, len(line)
                                if ( line(j:j) == ";" ) then
                                    dimension_lattice = dimension_lattice + 1
                                    tmp = TRIM(line(j-1:j-1))
                                    read(tmp,*) integer_tmp
                                    Box_dimension(dimension_lattice) = integer_tmp
                                end if
                            end do
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

    subroutine load_input_position()
        use position, only: Label, coord
        use mod_function, only : StripSpaces
        implicit none
        !
        ! Here the input of the position will be made.
        ! We need to follow this format :
        ! LABEL    x     y     z
        ! Label max size should be 10
        ! x, y and z will be double precision
        ! This function will only read the FIRST position
        !
        character(len=30)       :: input_fort       = './input_output/io_pos_opti.txt'
        character(len=1)        :: target_comment = "#"
        character(len=132)      :: line, tmp
        logical                 :: done, file_exists
        integer                 :: nlines, integer_tmp, stat
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
        allocate(Label(nlines),coord(nlines,3))
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
                        Label(rank_data) = tmp(1:10) ! Here we will have a truncation until 10 characters
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
        use position, only: Label, coord
        use mod_function, only : StripSpaces
        implicit none
        !
        ! Here the input of the position will be made.
        ! We need to follow this format :
        ! LABEL    x     y     z
        ! Label max size should be 10
        ! x, y and z will be double precision
        ! This function will only read the LAST position
        !
        logical, intent(in)     :: allocated_value ! Do we need to allocate the Label and coord?
        character(len=30)       :: input_fort       = './input_output/io_pos_opti.txt'
        character(len=1)        :: target_comment = "#"
        character(len=132)      :: line, tmp
        logical                 :: done, file_exists
        integer                 :: nlines, nSTOP, integer_tmp, stat
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
            allocate(Label(nlines),coord(nlines,3))
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
                if ( line(1:6) == "-STOP-" ) then
                    k_stop = k_stop + 1
                    if ( k_stop /= nSTOP ) then
                        exit
                    end if
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
                        !write(*,*) tmp
                        Label(rank_data) = tmp(1:10) ! Here we will have a truncation until 10 characters
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
        use position, only: Label, coord
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
        do i = 1, size(Label)
            write(12,*) Label(i),";",coord(i,1),";",coord(i,2),";",coord(i,3),";"
        end do
        write(12,*) "-STOP-"
        close(12)
    end subroutine write_input_position

    subroutine create_file(filename)
        character(*), intent(in) :: filename
        integer                  :: unit=20
        integer                  :: ierr
    
        open(unit,file=filename,status='unknown', action="write",iostat=ierr)
        
        if ( ierr /=0 ) then
            write(*,*) "Error: Unable to create the file."
            stop
        end if
        write(unit,*) "# Label    x    y   z"
        write(unit,*) "Dummy;0;0;0"
        write(unit,*) "-STOP-"
        write(unit,*)
        close(unit)
    end subroutine create_file

end module sub