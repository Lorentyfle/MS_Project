module position
    implicit none
    
    save
    ! 11/10/2023: Addition of identity_Label
    ! Variables
    integer,            allocatable,dimension(:)  :: Label          ! vector of each type of sphere.
    integer,            allocatable,dimension(:)  :: identity_Label ! vector of each INDIVIDUAL sphere.
    double precision,   allocatable,dimension(:,:):: coord          ! Matrix of coordinates (x,y,z).

end module position