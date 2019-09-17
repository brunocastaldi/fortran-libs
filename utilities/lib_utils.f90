!! ***
!! This module is a set of generic utility procedures to perform different tasks, as described in each one of them.
!!
!! @author: Bruno Castaldi
!! @email: castaldi@usp.br
!! last updated: Sep 17, 2019
!! ***
module lib_utils

use lib_mathematical_constants

implicit none

interface deg_2rad
     procedure deg_2rad_sp, deg_2rad_dp
end interface

contains

     !•••••••••••••••••••••••••••••••••••••••••••••••••••••• LINEAR SPACING •••••••••••••••••••••••••••••••••••••••••••••••••••••••!
     function linspace(min, max, n_elements_output)
     !! ***
     !! Computes a 1-d array whose (n_elements_output) values are evenly spaced and range from a given minimum and maximum
     !! ***

     implicit none
     integer, intent(in)  :: n_elements_output
     real(8), intent(in)  :: min, max
     real(8)              :: h, linspace(n_elements_output)
     integer              :: i

          if (n_elements_output == 1) then
               h = (max - min) / 2
               linspace = min + h * [1]

          else
               h = (max - min) / (n_elements_output - 1)
               linspace = min + h * [ (i,  i = 0,  n_elements_output - 1) ]

          end if

     end function linspace
     !–&–&–&–#–#–#–&–&–&– ••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••• –&–&–&–#–#–#–&–&–&–!

     !••••••••••••••••••••••••••••••••••••••••••••••••••• LOGARITHMIC SPACING •••••••••••••••••••••••••••••••••••••••••••••••••••••!
     function logspace(min, max, n_elements_output)
     !! ***
     !! Computes a 1-d array whose real64 values are evenly spaced in a log scale, ranging from a given minimum and maximum.
     !! ***

     implicit none
     integer, intent(in)  :: n_elements_output
     real(8), intent(in)  :: min, max
     real(8)              :: h
     real(8), allocatable :: logspace(:)
     integer              :: i

          allocate(logspace(n_elements_output))

          if (n_elements_output == 1) then
               h = (log10(max) - log10(min)) / 2d0
               logspace = log10(min) + h * [1]

          else
               h = (log10(max) - log10(min)) / (n_elements_output - 1)
               logspace = log10(min) + h * [(i, i=0, n_elements_output-1)]

          end if

          logspace = 10d0**logspace

     end function logspace
     !–&–&–&–#–#–#–&–&–&– ••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••• –&–&–&–#–#–#–&–&–&–!

     !••••••••••••••••••••••••••••••••••••••••••••••• LINEAR SPACING INTEGER VALUES •••••••••••••••••••••••••••••••••••••••••••••••!
     function logspace_int(min, max, n_elements_output)
     !! ***
     !! Computes a 1-d array whose integer values are evenly spaced in a log scale, ranging from a given minimum and maximum
     !! ***

     implicit none
     integer, intent(in)  :: min, max, n_elements_output
     integer              :: i
     real(8)              :: h, rmin, rmax

     real(8), allocatable :: logspace_aux(:)
     integer, allocatable :: logspace_aux_int(:), logspace_int(:)

          rmin = dble(min)
          rmax = dble(max)

          allocate(logspace_int(n_elements_output), logspace_aux(n_elements_output))

          if (n_elements_output == 1) then
               h = (log10(rmax) - log10(rmin)) / 2d0
               logspace_aux = log10(rmin) + h * [1]

          else
               h = (log10(rmax) - log10(rmin)) / (n_elements_output - 1)
               logspace_aux = log10(rmin) + h * [(i, i=0, n_elements_output-1)]

          end if

          logspace_aux_int = nint(10d0**logspace_aux)

          logspace_int = remove_dups_int(logspace_aux_int)


     end function logspace_int
     !–&–&–&–#–#–#–&–&–&– ••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••• –&–&–&–#–#–#–&–&–&–!

     !••••••••••••••••••••••••••••••••••••••••••••••••••••• REMOVE DUPLICATES •••••••••••••••••••••••••••••••••••••••••••••••••••••!
     function remove_dups_int(array)
     !! *** Remove duplicate values of a 1-D Array of integers *** !!
     implicit none
     integer, allocatable, intent(in) :: array(:)             ! The input
     integer, allocatable             :: remove_dups_int(:)   ! The output
     integer, allocatable             :: aux(:)               ! An auxiliary array
     integer                          :: k                    ! The number of unique elements
     integer                          :: i                    ! Iterator

          allocate ( aux(size(array)) )

          k = 1

          aux(1) = array(1)

          do i = 2, size(array)
               !! if the number already exist in res check next ::
               if (any( aux == array(i) )) cycle

               !! If no match found ==> add it to the auxiliary array ::
               k = k + 1
               aux(k) = array(i)

          end do

          allocate ( remove_dups_int, source=aux(1:k) )

     end function remove_dups_int
     !–&–&–&–#–#–#–&–&–&– ••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••• –&–&–&–#–#–#–&–&–&–!

     !••••••••••••••••••••••••••••••••••••••••••••• CONVERT DEGREE TO RADIAN [float32] ••••••••••••••••••••••••••••••••••••••••••••!
     elemental function deg_2rad_sp(degree_angle) result(radian_angle)
     !! *** Covert real type elements of an array of any size or shape evaluated in degrees to radian *** !!
     implicit none
     real(4), intent(in)  :: degree_angle   ! The input angle
     real(4)              :: radian_angle   ! The output angle

          radian_angle = degree_angle * pi/180.

     end function deg_2rad_sp
     !–&–&–&–#–#–#–&–&–&– ••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••• –&–&–&–#–#–#–&–&–&–!

     !••••••••••••••••••••••••••••••••••••••••••••• CONVERT DEGREE TO RADIAN [float32] ••••••••••••••••••••••••••••••••••••••••••••!
     elemental function deg_2rad_dp(degree_angle) result(radian_angle)
     !! *** Covert real type elements of an array of any size or shape evaluated in degrees to radian *** !!
     implicit none
     real(8), intent(in)  :: degree_angle   ! The input angle
     real(8)              :: radian_angle   ! The output angle

          radian_angle = degree_angle * pi/180d0

     end function deg_2rad_dp
     !–&–&–&–#–#–#–&–&–&– ••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••• –&–&–&–#–#–#–&–&–&–!

     !••••••••••••••••••••••••••••••••••••••• ROUTINE FOR OPENING AUTOMATICALLY NAMED FILES •••••••••••••••••••••••••••••••••••••••!
     SUBROUTINE openfl_3prms(unit, p1nm, prm1, p2nm, prm2, p3nm, prm3, dtnm, flext)
     !! ***
     !! This routine is intended to open a generic output file based on the name of three double precision real parameters (p1nm,
     !!   p2nm, p3nm), their respective non-negative values (prm1, prm2, prm3), the label of data to be written in the file (dtnm),
     !!   and the desired file extension (flext).
     !! It returns a file unit automatically created (support for Fortran 2008 and up needed) and a meaningful output file name.
     !!   CAUTION: old files with same name and extension will be overwritten with no warning! Prior user's data might be lost.
     !!
     !! @author: Bruno Castaldi
     !! @email: castaldi@usp.br
     !!
     !! last updated: Sep 5, 2019
     !! ***

     implicit none
     integer, intent(out)          :: unit
     real(8), intent(in)           :: prm1, prm2, prm3
     character(len=*), intent(in)  :: p1nm, p2nm, p3nm, dtnm, flext
     character(len=:), allocatable :: p1fl, p2fl, p3fl
     character(len=:), allocatable :: flnm
     character(len=20)             :: p1fl_aux, p2fl_aux, p3fl_aux

          write(p1fl_aux, '(F0.3)') prm1
          write(p2fl_aux, '(F0.3)') prm2
          write(p3fl_aux, '(F0.3)') prm3

          p1fl = trim(p1fl_aux)   ;   p2fl = trim(p2fl_aux)   ;   p3fl = trim(p3fl_aux)

          !! ==================================================================================================================== !!
          !! *** Some compilers/OS can print real numbers ranging ]-1,1[ as ".xxxxx" or "-.xxxxx" using fmt=F0.n to write
          !!      The following statements correct this behavior for non-negative numbers ***
          !!
          if ( (p1fl(1:1)) == '.' )  p1fl = '0' // p1fl
          if ( (p2fl(1:1)) == '.' )  p2fl = '0' // p2fl
          if ( (p3fl(1:1)) == '.' )  p3fl = '0' // p3fl
          !!
          !! ==================================================================================================================== !!

          !! ==================================================================================================================== !!
          !! *** Some compilers/OS can print real numbers ranging ]-1,1[ as ".xxxxx" or "-.xxxxx" using fmt=F0.n to write
          !!      The following statements correct this behavior for negative numbers ***
          !!
          if ( (p1fl(1:2)) == '-.' )  p1fl = '-0.' // p1fl(3:)
          if ( (p2fl(1:2)) == '-.' )  p2fl = '-0.' // p2fl(3:)
          if ( (p3fl(1:2)) == '-.' )  p3fl = '-0.' // p3fl(3:)
          !!
          !! ==================================================================================================================== !!

          flnm = p1nm // '_' // p1fl // '_' // p2nm // '_' // p2fl // '_' // p3nm // '_' // p3fl // '_' // dtnm // '.' // flext

          print *, ' output file >>> ' // flnm // new_line('a')

          open(newunit=unit, file=flnm, status='unknown')

     END SUBROUTINE openfl_3prms
     !–&–&–&–#–#–#–&–&–&– ••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••• –&–&–&–#–#–#–&–&–&–!

     !•••••••••••••••••••••••••••• LIST FILES IN A DIRECTORY OR PATH OF FILES (similar to python glob) ••••••••••••••••••••••••••••!
     function list_path_content(path_to_list)
     !! ***
     !! List the content of a path as elements of a 1D array of strings. The length of each and every string element is the maximum
     !!  length of any string element in the array. Maximum character(len=length) is set to length = 200.
     !!
     !! USAGE: 1) declare an allocatable character array of deferred shape and length ;
     !!        2) allocate the variable sourced by function list_path_content
     !!
     !! EXAMPLE: character(:), allocatable :: array_of_strings_name(:)
     !!             ...
     !!             ...
     !!          allocate( array_of_strings_name  ,  source = list_path_content("/path/to/files/*.ext")
     !!             ...
     !! ***
     implicit none
     character(*)  , intent(in)  :: path_to_list
     character(200), allocatable :: list_to_be_trimmed(:)
     character(:)  , allocatable :: list_path_content(:)
     integer                     :: scratch_list_id, ierr, count_elements, max_length

          call execute_command_line ("ls -1 " // path_to_list // " > scratch_list.lis", wait=.true.)

          open(newunit=scratch_list_id, file='scratch_list.lis', status='old', iostat=ierr)

          ! Count the number of lines in scratch file list
          count_elements = 0
          do
               read(scratch_list_id , * , iostat=ierr) ; if (ierr /= 0) exit
               count_elements = count_elements + 1
          end do
          rewind(scratch_list_id)

          allocate ( list_to_be_trimmed(count_elements) )

          max_length = 1
          do count_elements = 1, size(list_to_be_trimmed)
               read(scratch_list_id , '(a)') list_to_be_trimmed(count_elements)
               if (len_trim( list_to_be_trimmed(count_elements) ) > max_length) &
                                                                    max_length = len_trim( list_to_be_trimmed(count_elements) )
          end do

          close(scratch_list_id)

          call execute_command_line ("rm scratch_list.lis", wait=.true.)

          allocate( character(max_length) :: list_path_content(count_elements) )

          list_path_content(1:) = [ ( trim(list_to_be_trimmed(count_elements)) , count_elements=1,size(list_to_be_trimmed) ) ]

     end function list_path_content
     !–&–&–&–#–#–#–&–&–&– ••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••• –&–&–&–#–#–#–&–&–&–!

end module lib_utils
