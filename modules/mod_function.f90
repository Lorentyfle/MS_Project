module mod_function
    implicit none
    
contains
    ! 11/10/2023 : Addition of the arithmetic_mean() and geometric_mean() subroutines.
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
        Sum_data = 0
        ! Program
        do i = 1, size_data
            Sum_data = Sum_data + datas(i)
        end do
        mean_value = (Sum_data)/(size_data)
    end subroutine arithmetic_mean

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
        Prod_data = 1
        ! Program
        do i = 1, size_data
            Prod_data = Prod_data * datas(i)
        end do
        
        mean_value = (Prod_data) ** (1/size_data)
    end subroutine geometric_mean
end module mod_function