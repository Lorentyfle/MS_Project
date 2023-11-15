module mod_function
    implicit none
    
contains
    ! 11/10/2023 : Addition of the arithmetic_mean() and geometric_mean() subroutines.
    ! 13/11/2023 : Addition of sort_increasing()
    ! 14/11/2023 : Addition of dij()
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

    subroutine sort_increasing(Input_vector,  Output_vector)
        implicit none
        double precision,dimension(3), intent(in) :: Input_vector
        double precision,dimension(3), intent(out) ::  Output_vector
        !
        !Setup
        !
        double precision, dimension(3)  :: u                         ! Set the vector
        integer                         :: n,i,j,k,s,boucle          ! Rank.
        integer                         :: x,y                       ! Set variables
        !
        !Init
        !
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
    end subroutine sort_increasing

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
        use constant, only: dr
        implicit none
        
        double precision, dimension(3), intent(in) :: ri,rj
        double precision, dimension(3), intent(in) :: box
        double precision :: dij_r
        
        double precision, dimension(3) :: ij
        integer     :: k

        do k = 1, 3
            ij = ri(k) - rj(k)
            ij = ij(k) - box(k)* dfloat(idint(ij(k)/(dr)))
        end do

        dij_r = dsqrt( ij(1)**2 + ij(2)**2 + ij(3)**2)
        
    end function dij

    function Lorentz_Berthelot(atom_identity, neighbor_identity, epsilon_, sigma, Number_of_species) result(dimer_LJ_params)

        ! LJ_params = (epsilon_1, epsilon_2, sigma_1, sigma_2)

        implicit none
        integer, intent(in) :: atom_identity, neighbor_identity, Number_of_species
        double precision, dimension(Number_of_species), intent(in) :: epsilon_, sigma

        double precision, dimension(2) :: epsilons, sigmas
        double precision, dimension(2) :: dimer_LJ_params

        epsilons(1) = epsilon_(atom_identity)
        epsilons(2) = epsilon_(neighbor_identity)
        sigmas(2) = sigma(atom_identity)
        sigmas(2) = sigma(neighbor_identity)

        call geometric_mean(epsilons, dimer_LJ_params(1))
        call arithmetic_mean(sigmas, dimer_LJ_params(2))
            
    end function Lorentz_Berthelot

    function Lennard_Jones(r, LJ_params) result(Edimer)

        implicit none 
        double precision, dimension(2), intent(in) :: LJ_params
        double precision, intent(in) :: r
        double precision :: Edimer

        double precision :: e, o

        e = LJ_params(1)
        o = LJ_params(2)

        Edimer = 4*e*((o/r)**12 - (o/r)**6)

    end function Lennard_Jones

    ! function energy(cutoff, coord, atom, coord) result(Epot)

    !     implicit none


    !     call sort_increasing(distances, distances_sorted)
        
    !     do i = 1, N_part - 1

    !     end do

    ! end function energy

end module mod_function