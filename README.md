# tecplot-binary-files (written in Fortran)
Generate binary-format input files for Tecplot-post-processing (suitable for 2D simulation results).

# A simple example

```Fortran
! use tecplot module
use tecplot

! define a tecplot object
type(tecplot_time_file) :: plt_file

! Other important parameters
integer,allocatable :: locations(:) ！ variable location set to 0 (node)
integer,allocatable :: type_list(:) ! input type, could be single or double
integer,allocatable :: shared_list(:) !no sharing
integer,parameter :: num_of_variables = 6 
character(len=50) :: filename='test.plt'

! skip all data generation code

! construct a tecplot object first
call plt_file%init(filename,nx,ny,nz,'Tecplot File Title','x,y,z,u,v,w')

! for each zone, call the two subroutines
! split data into 2D grid and data
call plt_file%write_zone_header('zone name', physics_time, 0, locations) 
call plt_file%write_zone_data(type_list, shared_list, your_grid, your_data)

! before exit, you must call complete subroutine
call plt_file%complete
```

To test the code with the above example, just type the following two lines of command in terminal and check out the result `test.plt` with tecplot or paraview:
```
$ make
$ ./a.out
```
