module rdf
    implicit none

contains
    subroutine partial_rdf(A, B, precision, output_rdf)     ! calculates g_AB(r) using {precision} bins for the histogram

        use constant, only : dr, N_part, Box_dimension, pi
        use position, only : identity_Label, coord
        use sub, only : minimum_image

        implicit none

        integer, intent(in) :: A, B, precision
        double precision, allocatable, dimension(:,:), intent(out) :: output_rdf

        double precision, dimension(3) :: atom_origin
        double precision, dimension(N_part) :: rdf_distances
        integer :: i, j, k, total_number_B,total_number_A
        integer, dimension(precision) :: number_inside_bin
        double precision, dimension(precision) :: volume_bin, rmin, rmax,Verif
        double precision :: density_B, bin_width

        allocate(output_rdf(precision,2))
        number_inside_bin = 0.0d0
        bin_width = dr/dble(precision)
        do i = 1, precision
            rmin(i) = dble(i - 1) * bin_width
            rmax(i) = dble(i) * bin_width
            !volume_bin(i) = (4.0d0/3.0d0)*pi*(rmax(i)**3 - rmin(i)**3)
            !volume_bin(i) = 4.0d0*pi*rmax(i)**(2) * (rmax(i) - rmin(i))
            volume_bin(i) = 4.0d0*pi*rmin(i)**(2) * (rmax(i) - rmin(i))
        end do

        do i = 1, N_part
            !
            if ( identity_Label(i) == A ) then
                do j = 1, 3
                    atom_origin(j) = coord(i,j)
                end do
                !
                call minimum_image(atom_origin, i, rdf_distances)
                !
                do j = 1, N_part
                    !
                    if ( identity_Label(j) == B ) then
                        do k = 1, precision
                            if ( rdf_distances(j) > rmin(k) .AND. rdf_distances(j) < rmax(k) ) then
                                number_inside_bin(k) = number_inside_bin(k) + 1
                            end if
                        end do
                    end if
                end do
            end if
        end do

        total_number_B = 0
        total_number_A = 0
        do i = 1, N_part
            if ( identity_Label(i) == B) then
                total_number_B = total_number_B + 1
            end if
            ! Normalisation
            if ( identity_Label(i) == A) then
                total_number_A = total_number_A + 1
            end if
            
        end do

        density_B = dble(total_number_B)/(Box_dimension(1)*Box_dimension(2)*Box_dimension(3))
        Verif = 0.0d0
        write(*,*) "Verif :"
        output_rdf(1,1) = (rmin(1)+rmax(1))/2.0d0 ; output_rdf(1,2) = 0.0d0
        do i = 2, precision
            output_rdf(i,1) = (rmin(i)+rmax(i))/2.0d0
            output_rdf(i,2) = number_inside_bin(i) / (volume_bin(i)*density_B*dble(total_number_A))
            
            Verif(i) = output_rdf(i,2)*total_number_B*volume_bin(i)/(Box_dimension(1)*Box_dimension(2)*Box_dimension(3))
            !write(*,*) Verif(i), number_inside_bin(i)/dble(total_number_A)
        end do
    
    end subroutine partial_rdf

    subroutine write_g_of_r(A,B,g_of_r)
        use position, only : Label
        use constant, only : Name
        implicit none
        double precision,dimension(:,:), intent(in) :: g_of_r
        integer,intent(in)                          :: A,B
        !
        character(len=len(Name(Label(A))))    :: Name_A
        character(len=len(Name(Label(B))))    :: Name_B
        character(len=34+len(Name_A)+len(Name_B))  :: out_rdf, out_rdf2
        logical             :: file_exists,file_exists2
        integer             :: i,j
        !
        ! A and B are the integers identity for each RDFs
        !
        ! dimensions:       r           g(r)
        ! The file where the save will be done will have the name of the input used.
        !
        ! Initialisation
        Name_A = Name(Label(A))
        Name_B = Name(Label(B))
        !
        out_rdf = "./input_output/RDF_output/rdf"//Name_A//"-"//Name_B//".dat"
        out_rdf2 = "./input_output/RDF_output/rdf"//Name_B//"-"//Name_A//".dat"
        !
        inquire(file=out_rdf, exist=file_exists)
        inquire(file=out_rdf2, exist=file_exists2)
        !
        if (file_exists) then
            open(1, file=out_rdf, status="old", position="append", action="write")
            if ( Name_A /= Name_B .and. file_exists2 ) then
                go to 2 ! We go to the end of the program
            end if
        else
            open(1, file=out_rdf, status="new", action="write")
        end if
        write(1,*) "---New_Simulation---"
        write(1,*) "    r         g(r)_"//Name_A//"-"//Name_B
        do i = 1, size(g_of_r,1)
            write(1,*) (g_of_r(i,j), j=1, size(g_of_r,2))
        end do
        2 close(1)
    end subroutine write_g_of_r
end module rdf