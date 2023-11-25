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
        integer :: i, j, k, total_number_B
        integer, dimension(precision) :: number_inside_bin
        double precision, dimension(precision) :: volume_bin, rmin, rmax
        double precision :: density_B, bin_width

        allocate(output_rdf(precision,2))

        bin_width = dr/dble(precision)
        do i = 1, precision
            rmin(i) = dble(i - 1) * bin_width
            rmax(i) = dble(i) * bin_width
            volume_bin(i) = (4/3)*pi*(rmax(i)**3 - rmin(i)**3)
        end do

        do i = 1, N_part
            if ( identity_Label(i) == A ) then
                do j = 1, 3
                    atom_origin(j) = coord(i,j)
                end do

                call minimum_image(atom_origin, i, rdf_distances)

                do j = 1, N_part
                    if ( identity_Label(j) == B ) then
                        do k = 1, precision
                            if ( rdf_distances(j) >= rmin(k) .AND. rdf_distances(j) < rmax(k) ) then
                                number_inside_bin(k) = number_inside_bin(k) + 1
                            end if
                        end do
                    end if
                end do
            end if
        end do

        total_number_B = 0
        do i = 1, N_part
            if ( identity_Label(i) == B) then
                total_number_B = total_number_B + 1
            end if
        end do

        density_B = dble(total_number_B)/(Box_dimension(1)*Box_dimension(2)*Box_dimension(3))

        do i = 1, precision
            output_rdf(i,1) = rmin(i)
            output_rdf(i,2) = number_inside_bin(i) / (volume_bin(i)*density_B)
        end do

    end subroutine partial_rdf
end module rdf