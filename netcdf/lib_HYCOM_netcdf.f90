!! ***
!! Library to retrieve oceanographic data from results running the HYbrid Coordinate System Model (HYCOM). The traditional HYCOM
!!  output binary files ([.a], [.b]) should have been converted to the standard NetCDF format using CF-1.0 conventions by using,
!!  e.g., the built-in HYCOM routines archv2netcdf**.
!!
!! USAGE : 1) compile calling library netcdff , e.g.: gfortran -c lib_HYCOM_netcdf.f90 -I/usr/include -L/usr/lib -lnetcdff
!!         2) in the main code, declare variables as derived types for each input NetCDF file as type(HYCOM_netCDF_2D) or
!!             type(HYCOM_netCDF_3D) as appropriate.
!!         3) in the main code, declare variables as derived types for each Field of data desired as type(HYCOM_Field_2D) or
!!             type(HYCOM_Field_3D) as appropriate.
!!         4) initialize NetCDF reading of every file as, e.g. :
!!             declared_variable_for_file = open_HYCOM_netCDF("/path/to/files/file_name_XXX.nc", "Longitude_as_in_netcdf_file", &
!!                                                              "Latitude", "Depth", "MT", "Date")
!!         5) retrieve data for the desired field(s) in each NetCDF file as, e.g. :
!!                       declared_variable_for_Field01 = declared_variable_for_file % read_data("Variable_name_as_in_netcdf_file")
!!             or, e.g.: declared_variable_temperature = declared_variable_for_file % read_data("temperature")
!!         6) HYCOM model's grid can be accessed for every file. As, e.g. :
!!             longitude_at_the_205th_point_of_Longitude_mesh = declared_variable_for_file % longitude % values(205)
!!             number_of_points_in_latitude_mesh = declared_variable_for_file % latitude % grid_size
!!             axis_origin_of_time_coordinate = declared_variable_for_file % time % units
!!         8) Field values at grid points and the "fill_value" and "units" attributes can be accessed as, e.g. :
!!             temperature_at_the_12th_longitude_14th_latitude_1st_zlevel_1st_time = &
!!                                                               declared_variable_temperature % values(12,14,1,1)
!!             value_empty_grid_point_for_temperature = declared_variable_temperature % fill_value
!!             temperature_unit = declared_variable_temperature % units
!!
!! @author: Bruno Castaldi
!! @email : castaldi@usp.br
!! last updated: Sep 17, 2019
!! ***
module lib_HYCOM_netcdf

use netcdf

implicit none

!! PRIVATE DERIVED TYPES TO REPRESENT COORDINATES AND FIELDS IN NETCDF ========================================================== !!
type, private :: Dimension_1D
     character(:), allocatable :: name_in_file         ! as stored in netcdf file
     integer                   :: dim_id, var_id       ! the netcdf ID
     integer                   :: grid_size            ! the length of the 1D array that holds the grid points for the Dimension
     real(4), allocatable      :: values(:)            ! the actual values for the coordinate dimension along mesh
     character(30)             :: units                ! string with unit of measurement for data
end type Dimension_1D

type, private :: date_time_1D
     character(:), allocatable :: name_in_file         ! as stored in netcdf file
     integer, allocatable      :: dim_id, var_id       ! the netcdf ID
     integer                   :: grid_size            ! the length of the 1D array that holds the grid points for the Dimension
     real(8), allocatable      :: values(:)            ! the actual values for the coordinate dimension along mesh
     character(30)             :: units                ! string with unit of measurement for data
end type date_time_1D
!! ============================================================================================================================== !!



type, public :: HYCOM_netCDF_3D
     type(Dimension_1d)        :: longitude, latitude, z_level
     type(date_time_1D)        :: time, Date
     integer                   :: file_id
     character(:), allocatable :: file_name
     logical                   :: initiated = .false.

     contains

          procedure, public :: close_file => close_file_3D
          procedure, public :: read_data => read_data_HYCOM_3D
end type HYCOM_netCDF_3D


type, public :: HYCOM_Field_3D
     character(:), allocatable :: name_in_file         ! as stored in netcdf file
     integer                   :: variable_id          ! the variable ID in netcdf file
     real(4), allocatable      :: values(:,:,:,:)      ! the actual values for the 3D Field at every grid point and time record
     character(30)             :: units                ! string with unit of measurement for data
     real(4)                   :: fill_value           ! value to fill nonexistent points
end type HYCOM_Field_3D




type, public :: HYCOM_netCDF_2D
     type(Dimension_1d)        :: longitude, latitude
     type(date_time_1D)        :: time, Date
     integer                   :: file_id
     character(:), allocatable :: file_name
     logical                   :: initiated = .false.

     contains

          procedure, public :: close_file => close_file_2D
          procedure, public :: read_data => read_data_HYCOM_2D
end type HYCOM_netCDF_2D


type, public :: HYCOM_Field_2D
     character(:), allocatable :: name_in_file         ! as stored in netcdf file
     integer                   :: variable_id          ! the variable ID in netcdf file
     real(4), allocatable      :: values(:,:,:)        ! the actual values for the 3D Field at every grid point and time record
     character(30)             :: units                ! string with unit of measurement for data
     real(4)                   :: fill_value           ! value to fill nonexistent points
end type HYCOM_Field_2D



interface open_HYCOM_netCDF
     module procedure init_netCDF_3D_ , init_netCDF_2D_
end interface


contains

     function init_netCDF_3D_ (path_to_netCDF_file, lon_name, lat_name, lvl_name, rec_name, date_name) result (self)
     implicit none
     type(HYCOM_netCDF_3D)    :: self
     character(*), intent(in) :: path_to_netCDF_file, lon_name, lat_name, lvl_name, rec_name, date_name

          self % file_name = path_to_netCDF_file

          self % longitude % name_in_file = lon_name
          self %  latitude % name_in_file = lat_name
          self %   z_level % name_in_file = lvl_name
          self %      time % name_in_file = rec_name
          self %      Date % name_in_file = date_name

          allocate(self%time%dim_id, self%time%var_id, self%Date%var_id)

          ! Open netCDF file :
          call check( nf90_open (path=path_to_netCDF_file, mode=nf90_nowrite, ncid=self%file_id) )

          ! Retrieve Dimension IDs :
          call check( nf90_inq_dimid (self%file_id , name=self%longitude%name_in_file , dimid=self%longitude%dim_id) )
          call check( nf90_inq_dimid (self%file_id , name=self% latitude%name_in_file , dimid=self% latitude%dim_id) )
          call check( nf90_inq_dimid (self%file_id , name=self%  z_level%name_in_file , dimid=self%  z_level%dim_id) )
          call check( nf90_inq_dimid (self%file_id , name=self%     time%name_in_file , dimid=self%     time%dim_id) )

          ! Get size of dimensions :
          call check( nf90_inquire_dimension (self%file_id , self%longitude%dim_id , len=self%longitude%grid_size) )
          call check( nf90_inquire_dimension (self%file_id , self% latitude%dim_id , len=self% latitude%grid_size) )
          call check( nf90_inquire_dimension (self%file_id , self%  z_level%dim_id , len=self%  z_level%grid_size) )
          call check( nf90_inquire_dimension (self%file_id , self%     time%dim_id , len=self%     time%grid_size) )

          ! Allocate memory :
          allocate ( self % longitude % values(self%longitude%grid_size) )
          allocate ( self %  latitude % values(self% latitude%grid_size) )
          allocate ( self %   z_level % values(self%  z_level%grid_size) )
          allocate ( self %      time % values(self%     time%grid_size) )
          allocate ( self %      Date % values(self%     time%grid_size) )

          ! Get the varids of Coordinate variables :
          call check( nf90_inq_varid (self%file_id , self%longitude%name_in_file , self%longitude%var_id) )
          call check( nf90_inq_varid (self%file_id , self% latitude%name_in_file , self% latitude%var_id) )
          call check( nf90_inq_varid (self%file_id , self%  z_level%name_in_file , self%  z_level%var_id) )
          call check( nf90_inq_varid (self%file_id , self%     time%name_in_file , self%     time%var_id) )
          call check( nf90_inq_varid (self%file_id , self%     Date%name_in_file , self%     Date%var_id) )

          ! Retrieve the actual grid points for the Coordinate Variables :
          call check( nf90_get_var (self%file_id , self%longitude%var_id , self%longitude%values) )
          call check( nf90_get_var (self%file_id , self% latitude%var_id , self% latitude%values) )
          call check( nf90_get_var (self%file_id , self%  z_level%var_id , self%  z_level%values) )
          call check( nf90_get_var (self%file_id , self%     time%var_id , self%     time%values) )
          call check( nf90_get_var (self%file_id , self%     Date%var_id , self%     Date%values) )

          ! Get the attribute "units" of the Coordinate Variables :
          call check( nf90_get_att (self%file_id , self%longitude%var_id , "units" , self%longitude%units) )
          call check( nf90_get_att (self%file_id , self% latitude%var_id , "units" , self% latitude%units) )
          call check( nf90_get_att (self%file_id , self%  z_level%var_id , "units" , self%  z_level%units) )
          call check( nf90_get_att (self%file_id , self%     time%var_id , "units" , self%     time%units) )
          call check( nf90_get_att (self%file_id , self%     Date%var_id , "units" , self%     Date%units) )

          self % initiated = .true.

     end function init_netCDF_3D_


     function read_data_HYCOM_3D (self, variable_name) result (data_3d)
     implicit none
     class(HYCOM_netCDF_3D), intent(in) :: self
     character(*),           intent(in) :: variable_name
     type(HYCOM_Field_3D)               :: data_3d

          if (.not. self%initiated) stop 'NetCDF file is not open. ==> Aborting...'

          data_3d % name_in_file = variable_name

          allocate ( data_3d % values(self%longitude%grid_size, &
                                      self% latitude%grid_size, &
                                      self%  z_level%grid_size, &
                                      self%     time%grid_size) )

          ! Get the ID of the requested variable in the netCDF :
          call check( nf90_inq_varid (self%file_id , data_3d%name_in_file , data_3d%variable_id) )

          ! Get the actual variable values :
          call check( nf90_get_var (self%file_id , data_3d%variable_id , data_3d%values) )

          ! Get the attributes of variable :
          call check( nf90_get_att (self%file_id , data_3d%variable_id , "_FillValue" , data_3d%fill_value) )
          call check( nf90_get_att (self%file_id , data_3d%variable_id , "units"      , data_3d%units     ) )

     end function read_data_HYCOM_3D


     subroutine close_file_3D (self)
     implicit none
     class(HYCOM_netCDF_3D) :: self

          call check( nf90_close(self%file_id) )

     end subroutine close_file_3D



     function init_netCDF_2D_ (path_to_netCDF_file, lon_name, lat_name, rec_name, date_name) result (self)
     implicit none
     type(HYCOM_netCDF_2D)    :: self
     character(*), intent(in) :: path_to_netCDF_file, lon_name, lat_name, rec_name, date_name

          self % file_name = path_to_netCDF_file

          self % longitude % name_in_file = lon_name
          self %  latitude % name_in_file = lat_name
          self %      time % name_in_file = rec_name
          self %      Date % name_in_file = date_name

          allocate(self%time%dim_id, self%time%var_id, self%Date%var_id)

          ! Open netCDF file :
          call check( nf90_open (path=path_to_netCDF_file, mode=nf90_nowrite, ncid=self%file_id) )

          ! Retrieve Dimension IDs :
          call check( nf90_inq_dimid (self%file_id , name=self%longitude%name_in_file , dimid=self%longitude%dim_id) )
          call check( nf90_inq_dimid (self%file_id , name=self% latitude%name_in_file , dimid=self% latitude%dim_id) )
          call check( nf90_inq_dimid (self%file_id , name=self%     time%name_in_file , dimid=self%     time%dim_id) )

          ! Get size of dimensions :
          call check( nf90_inquire_dimension (self%file_id , self%longitude%dim_id , len=self%longitude%grid_size) )
          call check( nf90_inquire_dimension (self%file_id , self% latitude%dim_id , len=self% latitude%grid_size) )
          call check( nf90_inquire_dimension (self%file_id , self%     time%dim_id , len=self%     time%grid_size) )

          ! Allocate memory :
          allocate ( self % longitude % values(self%longitude%grid_size) )
          allocate ( self %  latitude % values(self% latitude%grid_size) )
          allocate ( self %      time % values(self%     time%grid_size) )
          allocate ( self %      Date % values(self%     time%grid_size) )

          ! Get the varids of Coordinate Variables :
          call check( nf90_inq_varid (self%file_id , self%longitude%name_in_file , self%longitude%var_id) )
          call check( nf90_inq_varid (self%file_id , self% latitude%name_in_file , self% latitude%var_id) )
          call check( nf90_inq_varid (self%file_id , self%     time%name_in_file , self%     time%var_id) )
          call check( nf90_inq_varid (self%file_id , self%     Date%name_in_file , self%     Date%var_id) )

          ! Retrieve the actual grid points for the Coordinate Variables :
          call check( nf90_get_var (self%file_id , self%longitude%var_id , self%longitude%values) )
          call check( nf90_get_var (self%file_id , self% latitude%var_id , self% latitude%values) )
          call check( nf90_get_var (self%file_id , self%     time%var_id , self%     time%values) )
          call check( nf90_get_var (self%file_id , self%     Date%var_id , self%     Date%values) )

          ! Get the attribute "units" of the Coordinate Variables :
          call check( nf90_get_att (self%file_id , self%longitude%var_id , "units" , self%longitude%units) )
          call check( nf90_get_att (self%file_id , self% latitude%var_id , "units" , self% latitude%units) )
          call check( nf90_get_att (self%file_id , self%     time%var_id , "units" , self%     time%units) )
          call check( nf90_get_att (self%file_id , self%     Date%var_id , "units" , self%     Date%units) )

          self % initiated = .true.

     end function init_netCDF_2D_


     function read_data_HYCOM_2D (self, variable_name) result (data_2d)
     implicit none
     class(HYCOM_netCDF_2D), intent(in) :: self
     character(*),          intent(in)  :: variable_name
     type(HYCOM_Field_2D)               :: data_2d

          if (.not. self%initiated) stop 'NetCDF file is not open. ==> Aborting...'

          data_2d % name_in_file = variable_name

          allocate ( data_2d % values(self%longitude%grid_size, &
                                      self% latitude%grid_size, &
                                      self%     time%grid_size) )

          ! Get the ID of the requested variable in the netCDF :
          call check( nf90_inq_varid (self%file_id , data_2d%name_in_file , data_2d%variable_id) )

          ! Retrieve the actual values of variable :
          call check( nf90_get_var (self%file_id , data_2d%variable_id , data_2d%values) )

          ! Get the variable attributes :
          call check( nf90_get_att (self%file_id , data_2d%variable_id , "_FillValue" , data_2d%fill_value) )
          call check( nf90_get_att (self%file_id , data_2d%variable_id , "units"      , data_2d%units     ) )

     end function read_data_HYCOM_2D


     subroutine close_file_2D (self)
     implicit none
     class(HYCOM_netCDF_2D) :: self

          call check( nf90_close(self%file_id) )

     end subroutine close_file_2D


     subroutine check(status)
     integer, intent (in) :: status

          if (status /= nf90_noerr) then
               print *, trim(nf90_strerror(status))
               stop 2
          end if

     end subroutine check


end module lib_HYCOM_netcdf
