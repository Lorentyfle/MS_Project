module mod_function
    implicit none
    
contains
    ! 11/10/2023 : Addition of the arithmetic_mean() and geometric_mean() subroutines.
    ! 13/11/2023 : Addition of sort_increasing()
    ! 14/11/2023 : Addition of dij()
    ! 17/11/2023 : Correction LJ function
    ! By T.Jamin
    subroutine StripSpaces(string)
        implicit none
        character(len=*) :: string
        integer :: stringLen 
        integer :: last, actual

        stringLen = len (string)
        last = 1
        actual = 1

        do while (actual < stringLen)
            if (string(last:last) == ' ') then
                actual = actual + 1
                string(last:last) = string(actual:actual)
                string(actual:actual) = ' '
            else
                last = last + 1
                if (actual < last) &
                    actual = last
            endif
        end do

    end subroutine

    function stop_at_space(input_text,option) result(output)
        implicit none
        !
        character(len=*), intent(in)  :: input_text
        integer,intent(in)            :: option
        ! /!\ option MUST be in between 1 and 3.
        ! If the input contain spaces in between words, some characters will not be taken into account.
        ! This bug is known and will NOT be fixed... 
        !character(len=3)                     :: output_text
        integer                       :: i, size_line, max_size, min_size
        integer                       :: output
        !
        size_line = 0
        max_size  = 0
        min_size  = 0
        !
        do i=1,len(input_text)
            if ( input_text(i:i) /= " " ) then
                size_line = size_line + 1
            else
                if(size_line == 0) then
                    min_size = min_size + 1
                end if
            end if
        end do
        !output_text = input_text(1:max_size+min_size+2)
        if ( option == 1 ) then
            output = min_size+1
        elseif ( option == 2) then
            max_size = min_size + size_line
            output = max_size
        elseif (option == 3) then
            output = size_line
        end if
    end function stop_at_space

    subroutine arithmetic_mean(datas,  mean_value)
        ! This subroutine will compute the arithmetic mean difference of a vector of n data.
        implicit none
        double precision,dimension(:), intent(in) :: datas
        double precision, intent(out) ::  mean_value
        integer             :: size_data
        double precision    :: Sum_data
        integer             :: i

        ! Initialisation
        size_data = size(datas)
        Sum_data = 0.0d0
        ! Program
        do i = 1, size_data
            Sum_data = Sum_data + datas(i)
        end do
        mean_value = (Sum_data)/(dble(size_data))
    end subroutine arithmetic_mean

    ! If we want to transfert the means from sub to functions
    function fn_arithmetic_mean(datas) result(mean_value)
        ! This function will compute the arithmetic mean difference of a vector of n data.
        implicit none
        double precision,dimension(:), intent(in) :: datas
        double precision    ::  mean_value
        integer             :: size_data
        double precision    :: Sum_data
        integer             :: i
        !
        ! Initialisation
        !
        size_data = size(datas)
        Sum_data = 0
        ! Program
        do i = 1, size_data
            Sum_data = Sum_data + datas(i)
        end do
        mean_value = (Sum_data)/(size_data)
    end function fn_arithmetic_mean

    subroutine geometric_mean(datas,  mean_value)
        ! This subroutine will compute the geometric mean difference of a vector of n data.
        implicit none
        double precision,dimension(:), intent(in) :: datas
        double precision, intent(out) ::  mean_value
        integer             :: size_data
        double precision    :: Prod_data
        integer             :: i
        ! Initialisation
        size_data = size(datas)
        Prod_data = 1.0d0
        ! Program
        do i = 1, size_data
            Prod_data = Prod_data * datas(i)
        end do
        
        mean_value = (Prod_data)**(1.0d0/dble(size_data))
    end subroutine geometric_mean

    subroutine sort_increasing(Input_vector,  Output_vector)
        implicit none
        double precision,dimension(:), intent(in) :: Input_vector
        double precision,dimension(:), intent(out) ::  Output_vector
        !
        !Setup
        !
        double precision, dimension(:),allocatable  :: u                         ! Set the vector
        integer                         :: n,i,j,k,s,boucle          ! Rank.
        integer                         :: x,y                       ! Set variables
        !
        !Init
        !
        allocate(u(size(Input_vector)))
        u = Input_vector
        s = size(u)
        n = 1
        k = 0
        boucle = 0
        !
        !Program
        !

        do while (n < s)
            x = u(n)
            y = u(n+1)
            if ( x > y ) then
                u(n)    = y
                u(n+1)  = x
            end if
            n = n+1
            if ( n == s ) then
                do j = 1, s
                    if ( u(j)>u(j+1) ) then
                        k = k+1
                    end if
                end do
                if ( k > 1 ) then
                    n = 0
                    k = 0
                    boucle = boucle + 1
                end if
            end if
        end do
        !
        ! Show the result
        !
        Output_vector = u
        deallocate(u)
    end subroutine sort_increasing

    subroutine sort_decreasing(Input_vector,  Output_vector)
        implicit none
        double precision,dimension(:), intent(in) :: Input_vector
        double precision,dimension(:), intent(out) ::  Output_vector
        !
        !Setup
        !
        double precision, dimension(:),allocatable  :: u                         ! Set the vector
        integer                         :: n,i,j,k,s,boucle          ! Rank.
        integer                         :: x,y                       ! Set variables
        !
        !Init
        !
        allocate(u(size(Input_vector)))
        u = Input_vector
        s = size(u)
        n = 1
        k = 0
        boucle = 0
        !
        !Program
        !

        do while (n < s)
            x = u(n)
            y = u(n+1)
            if ( x < y ) then
                u(n)    = y
                u(n+1)  = x
            end if
            n = n+1
            if ( n == s ) then
                do j = 1, s
                    if ( u(j)>u(j+1) ) then
                        k = k+1
                    end if
                end do
                if ( k > 1 ) then
                    n = 0
                    k = 0
                    boucle = boucle + 1
                end if
            end if
        end do
        !
        ! Show the result
        !
        Output_vector = u
        deallocate(u)
    end subroutine sort_decreasing

    subroutine IS_ODD(i,j,k,IS_ODD_r)
        integer, intent(in) :: i,j,k
        logical, intent(out) ::  IS_ODD_r
    
        if(.not. MOD(i,2) == 0 .AND. .not. MOD(j,2) == 0 .AND. .not. MOD(k,2) == 0) then
            IS_ODD_r = .TRUE.
        else
            IS_ODD_r = .FALSE.
        end if
        
    end subroutine IS_ODD

    function dij(ri,rj,box) result(dij_r)
        use constant, only : dr
        implicit none
        
        double precision, dimension(3), intent(in) :: ri,rj
        double precision, dimension(3), intent(in) :: box
        double precision :: dij_r
        
        double precision, dimension(3) :: ij
        integer     :: k

        do k = 1, 3
            ij(k) = ri(k) - rj(k)
            ij(k) = ij(k) - box(k)* dble(idint(ij(k)/(box(k)/2.0d0)))
        end do

        dij_r = dsqrt(ij(1)**2 + ij(2)**2 + ij(3)**2)
    end function dij

    function Lennard_Jones(r, LJ_params) result(Edimer)

        implicit none 
        double precision, dimension(2), intent(in) :: LJ_params
        double precision, intent(in) :: r
        double precision :: Edimer

        double precision :: e, o

        e = LJ_params(1)
        o = LJ_params(2)
        ! e is in kJ/mol
        ! o is in A
        ! Edimer is in kJ/mol
        Edimer = 4*e*((o/r)**12 - (o/r)**6)

    end function Lennard_Jones

    function sum_KQ(E_species,KQ) result(sum_data)
        !
        implicit none
        !
        integer, intent(in) :: KQ
        double precision,dimension(:,:),intent(in):: E_species
        double precision :: sum_data
        integer         :: i
        !
        sum_data = 0.0d0
        do i = 1, size(E_species,1)
            sum_data = sum_data + E_species(i,KQ)
        end do
        !
    end function sum_KQ

end module mod_function