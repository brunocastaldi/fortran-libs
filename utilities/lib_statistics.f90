!! ***
!! Module to perform a set of basic statistical analysis on a defined derived type variable that stores a 1-D Array with scalar values, 
!!  an auxiliary "label", and procedures to compute arithmetic mean, median, standard deviation, variance and quartiles.
!!
!! @author: Bruno Castaldi
!! @email : castaldi@usp.br
!! last updated: Sep 17, 2019
!! ***

module lib_statistics

implicit none

type, private :: quartile_type
     !! Variable type to compute quantiles: first quartile (lower), second quartile (median), third quartile (upper) !!
     real(8), allocatable  :: lower, first, median, second, upper, third
end type quartile_type

!! •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••• !!
type stat_array
!! ***
!! Variable type to store the 1-D Array elements, its statistical variables that will be estimated and the computing procedures.
!! ***
     real(8), allocatable             :: values(:)                                     ! 1-D Array with elements to be evaluated

     integer, allocatable             :: label                                         ! Optional number to identify Array in case
                                                                                       !  of variables of type (stat_array) have
                                                                                       !  multiple dimensions in the main code.

     real(8), allocatable             :: mean, median, variance, standard_deviation, & ! Basic statistical variables
                                         maximum, minimum

     type(quartile_type), allocatable :: quartile                                      ! Quartiles can be accessed as:
                                                                                       !  quartile%lower  or quartile%first  ;
                                                                                       !  quartile%median or quartile%second ;
                                                                                       !  quartile%upper  or quartile%third

     logical, private                 :: sorted = .false.                              ! Tells if the 1-D Array have already been
                                                                                       !  sorted or not. Default is not.

     contains

          procedure :: get_mean
          procedure :: get_median
          procedure :: get_var
          procedure :: get_stdv
          procedure :: get_maximum
          procedure :: get_minimum
          procedure :: sort => sort_array
          procedure :: get_quartiles
          procedure :: get_stats
          procedure :: stat => get_stats
          procedure :: stats => get_stats

end type stat_array
!! •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••• !!

contains

     !••••••••••••••••••••••••••••••••••••••••••••••••••••• MAKE STATISTICS •••••••••••••••••••••••••••••••••••••••••••••••••••••••!
     subroutine get_stats(self, sort)
     !! ***
     !! Routine to compute statistics, assigning the most common statistical parameters to the variable that holds the 1-D Array.
     !! ***

     implicit none
     class(stat_array), intent(inout) :: self
     class(stat_array), allocatable   :: alias
     logical, optional                :: sort
     logical                          :: sort_default = .true.

     if ( present(sort) ) sort_default = sort

     if (sort_default) then
          !! CAUTION: THE ORIGINAL ARRAY WILL BE REORDERED AND ITS ELEMENTS SORTED FROM SMALLEST TO LARGEST BY DEFAULT.
          self % quartile      = self % get_quartiles()

     else
          !! Otherwise, an alias is assumed to sort elements instead. ==> this approach can consume more memory than necessary.
          !! Avoid if the order of the elements doesn't matter.
          allocate(alias, source=self)
          self % quartile      = alias % get_quartiles()
          deallocate(alias)

     end if

     self % mean               = self % get_mean()
     self % variance           = self % get_var()
     self % median             = self % get_median()
     self % standard_deviation = self % get_stdv()
     self % maximum            = self % get_maximum()
     self % minimum            = self % get_minimum()

     end subroutine get_stats
     !•••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••!


     !••••••••••••••••••••••••••••••••••••••••••••••••••••••••••• MEAN ••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••!
     real(8) function get_mean(self)
     !! ***
     !! Function to compute the arithmetic mean of a 1-D Array elements. Mean is a measure of CENTRAL TENDENCY of the data.
     !! ***

     implicit none
     class(stat_array), intent(inout) :: self

          get_mean = sum(self%values) / size(self%values)

     end function get_mean
     !•••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••!


     !••••••••••••••••••••••••••••••••••••••••••••••••••••••••• VARIANCE ••••••••••••••••••••••••••••••••••••••••••••••••••••••••••!
     real(8) function get_var(self)
     !! ***
     !! Function to compute the variance of a 1-D Array elements. Variance is a measure of DISPERSION.
     !! ***

     implicit none
     class(stat_array), intent(in) :: self

          get_var = ( sum( (self%values)**2 ) / size(self%values) )  -  ( sum(self%values) / size(self%values) )**2

     end function get_var
     !•••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••!


     !••••••••••••••••••••••••••••••••••••••••••••••••••••• STANDARD DEVIATION ••••••••••••••••••••••••••••••••••••••••••••••••••••!
     real(8) function get_stdv(self)
     !! ***
     !! Function to compute the standard deviation of a 1-D Array elements. Std. Dvt. is a measure of DISPERSION.
     !! ***

     implicit none
     class(stat_array), intent(in) :: self

          get_stdv = sqrt( abs(get_var(self)) )             !! the module is taken from variance before extracting root to avoid
                                                            !!  the program to stop during runtime if, e.g., variance is too small
                                                            !!  (actually 0.00) but holds a minus sign due to numerical roundings

     end function get_stdv
     !•••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••!


     !•••••••••••••••••••••••••••••••••••••••••••••••••••••••••• MAXIMUM ••••••••••••••••••••••••••••••••••••••••••••••••••••••••••!
     real(8) function get_maximum(self)
     !! ***
     !! Function to compute the maximum value of a 1-D Array elements.
     !! ***

     implicit none
     class(stat_array), intent(inout) :: self

          get_maximum = maxval(self%values)

     end function get_maximum
     !•••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••!


     !•••••••••••••••••••••••••••••••••••••••••••••••••••••••••• MINIMUM ••••••••••••••••••••••••••••••••••••••••••••••••••••••••••!
     real(8) function get_minimum(self)
     !! ***
     !! Function to compute the maximum value of a 1-D Array elements.
     !! ***

     implicit none
     class(stat_array), intent(inout) :: self

          get_minimum = minval(self%values)

     end function get_minimum
     !•••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••!


     !•••••••••••••••••••••••••••••••••••••••••••••••••••••••••• MEDIAN •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••!
     !! ######################################################################################################################### !!
     real(8) function get_median(self)
     !! ***
     !! Function to compute the median value of a 1-D Array with unsorted elements.
     !! This procedure uses subroutines sort_heapsort and sifdown.
     !! The median is also a measure of CENTRAL TENDENCY, but at the center of the sequence of scalar data, sorted from smallest to
     !!  largest.
     !! ***

     implicit none
     class(stat_array), intent(in)  :: self
     real(8), allocatable           :: Array_temp(:)
     integer                        :: n

          if ( allocated(self%quartile) ) then
               !! if quartiles have already been computed, then median is also already known ::
               get_median = self % quartile % median

          else
               !! otherwise, sort array scalar elements from smallest to largest and compute ::
               n = size(self%values)
               allocate(Array_temp, source=self%values)                                !! copy array

               if (.not. self%sorted) call sort_heapsort(Array_temp)                   !! sort the copy

               !! compute the median ::
               if (mod(n, 2) == 0) then
                    get_median = ( Array_temp(n/2) + Array_temp(1 + n/2) ) /2d0        !! if Array has an even number of elements

               else
                    get_median = Array_temp(1 + n/2)                                   !! if Array has an odd number of elements

               end if

          end if

     end function get_median
     !! ######################################################################################################################### !!

     !! ######################################################################################################################### !!
     subroutine sort_heapsort(a)
     !! ***
     !! Sort a 1-D Array elements from smallest to largest, using the HEAPSORT METHOD.
     !! This procedure is called by functions get_median(), and sort_array().
     !! ***

     implicit none
     real(8), intent(inout) :: a(0:)
     integer                :: start, n, bottom
     real(8)                :: temp

     n = size(a)

     do start = (n - 2) / 2, 0, -1
          call siftdown(a, start, n)
     end do


     do bottom = n - 1, 1, -1

          temp = a(0)
          a(0) = a(bottom)
          a(bottom) = temp

          call siftdown(a, 0, bottom)

     end do

     end subroutine sort_heapsort
     !! ######################################################################################################################### !!

     !! ######################################################################################################################### !!
     subroutine siftdown(a, start, bottom)
     !! ***
     !! Shifts array elements according to their values. This procedure is called by subroutine sort_heapsort().
     !! ***

     implicit none
     real(8), intent(inout) :: a(0:)
     integer, intent(in)    :: start, bottom
     integer                :: child, root
     real(8)                :: temp


     root = start

     do while(root*2 + 1 < bottom)

          child = root * 2 + 1

          if (child + 1 < bottom) then
               if (a(child) < a(child+1)) child = child + 1
          end if


          if (a(root) < a(child)) then

               temp = a(child)
               a(child) = a (root)
               a(root) = temp
               root = child

          else
               return

          end if

     end do

     END SUBROUTINE siftdown
     !! ######################################################################################################################### !!
     !•••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••!


     !••••••••••••••••••••••••••••••••••••••••••••••••••••••••• QUARTILES •••••••••••••••••••••••••••••••••••••••••••••••••••••••••!
     function get_quartiles(self) result(quartiles)
     !! ***
     !! Subroutine to compute the first, second and third quartiles.
     !! ***

     implicit none
     class(stat_array), intent(inout) :: self
     type(quartile_type)              :: quartiles
     integer                          :: n, i25, i75

          n = size(self%values)

          i25 =      (n + 1)  / 4
          i75 = (3 * (n + 1)) / 4

          !! WATCH OUT: this procedure resorts the order of elements of the original array.
          call self % sort()

          !! COMPUTE FIRST AND THIRD QUARTILES BASED ON THE ARRAY LENGTH ::
          if      (mod(n + 1 , 4) == 0) then
               ! ••• (number of elements + 1) is a multiple of 4 ••• !
               quartiles % lower  =           self % values(i25)
               quartiles % upper  =           self % values(i75)

          else if (mod(n - 1 , 4) == 0) then
               ! ••• (number of elements - 1) is a multiple of 4 ••• !
               quartiles % lower  =  0.25d0 * self % values(i25)  +  0.75d0 * self % values(i25 + 1)
               quartiles % upper  =  0.75d0 * self % values(i75)  +  0.25d0 * self % values(i75 + 1)
          else
               ! ••• (number of elements - 3) is a multiple of 4 ••• !
               quartiles % lower  =  0.75d0 * self % values(i25)  +  0.25d0 * self % values(i25 + 1)
               quartiles % upper  =  0.25d0 * self % values(i75)  +  0.75d0 * self % values(i75 + 1)

          end if

          !! COMPUTE SECOND QUARTILE BASED ON THE PARITY OF ARRAY LENGTH ::
          if (mod(n , 2) == 0) then
               ! ••• Array has an even number of elements ==> median is the average of the 2 elements in the midlle ••• !
               quartiles % median =  0.50d0 * self % values(n/2)  +  0.50d0 * self % values(1 + n/2)
          else
               ! ••• Array has an odd number of elements ==> median is the element in the middle ••• !
               quartiles % median =                                           self % values(1 + n/2)
          end if

          quartiles%first  = quartiles%lower
          quartiles%second = quartiles%median
          quartiles%third  = quartiles%upper


     end function get_quartiles
     !•••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••!


     !•••••••••••••••••••••••••••••••••••••••••••••••••••••••• SORT ARRAY •••••••••••••••••••••••••••••••••••••••••••••••••••••••••!
     subroutine sort_array(self)
     !! ***
     !! Routine to sort a 1-D Array elements from smallest to largest. This porocedure is called by function get_quartiles()
     !! ***

     implicit none
     class(stat_array), intent(inout) :: self

     if (.not. self%sorted) call sort_heapsort(self%values)                   ! WATCH OUT: original array will be sorted!

     self%sorted = .true.

     end subroutine sort_array
     !•••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••!


end module lib_statistics
