program main
  implicit none
  call example1()

contains

  subroutine example1()
    ! use tecplot module
    use tecplot

    ! define a tecplot object
    type(tecplot_time_file) :: plt_file
    integer,allocatable :: locations(:)
    integer,allocatable :: type_list(:)
    integer,allocatable :: shared_list(:)
    integer,parameter :: num_of_variables = 5
    integer :: nx,ny,nz,i,j,k,d,ii
    character(len=50) :: filename='test.plt'
    real(kind=4) rando
    real(kind=8),allocatable :: your_datas(:,:,:)
    real(kind=4) :: physics_time
    real :: xyz(2), ijk(2)
    real start, finish

    call cpu_time(start)
    
    ! set dimensions
    nx = 125
    ny = 100
    !nz = 1 remove this dimension

    allocate(your_datas(nx,ny,num_of_variables))
    allocate(locations(num_of_variables))
    allocate(type_list(num_of_variables))
    allocate(shared_list(num_of_variables))

    ! locations = 0 means data in node, 1 means data in cell(not supported yet)
    locations = 0
    ! shared_list(i)=-1 means the i-th data is not shared in this zone. If shared_list(i)=m,
    ! it means the i-th data is shared with zone m in this file
    shared_list = -1
    ! type_list(i) = 1 means the i-th data is of type float. (Other data type not supported yet.)
    type_list = 1 !1=single, 2=double

    ! call init subroutine first
    ! nx, ny, nz means the dimension of the data
    ! 'x,y,z,u,v,w' is a string contains names of variables, must be divided by ','
    call plt_file%init(filename,nx,ny,'Tecplot File Title','x,y,u,v,w')

    do ii = 1, 14
       physics_time = ii*0.1
    ! for each zone, call the two subroutines
    ! physics_time can be any value, it will only be used when there are more than 1 zone in a file.
    call plt_file%write_zone_header('zone name', physics_time, 0, locations)

    ! your_datas(:,:,:,1:3) =  x,y,z coordinates(Variable assignment is omitted in this example)
    ! your_datas(:,:,:,4:6) =  u,v,w datas (Variable assignment is omitted in this example)
    ! ALL datas are stored in sequence like (((x(ix,iy,iz),ix=1,nx),iy=1,ny),iz=1,nz)
    ! set coordinate
    do d = 1, 2
       do concurrent(i=1:nx, j=1:ny)
          xyz = [i-1., j-1.]
          your_datas(i,j,d) = xyz(d)
       end do
    end do
    ! set value
    do d = 3, num_of_variables
       do i = 1, nx
          do j = 1, ny
             call RANDOM_NUMBER(your_datas(i,j,d))
          enddo
       enddo
       
    end do
    call plt_file%write_zone_data(type_list, shared_list, &
         real(your_datas(:,:,1:2),kind=4), &
         real(your_datas(:,:,3:num_of_variables),kind=4))
 enddo
 
    ! before exit, you must call complete subroutine
    call plt_file%complete


    call cpu_time(finish)
    WRITE(*,*) 'Execution Time = ', finish-start, 's'
     
  end subroutine example1

end program main
