!! ***
!! Set of mathematical constants using 64-bit float point variables.
!!
!! @author: Bruno Castaldi
!! @email: castaldi@usp.br
!! last updated: Sep 17, 2019
!! ***
module lib_mathematical_constants

implicit none

!! MATHEMATICAL CONSTANTS ======================================================================================================= !!
real(8), parameter ::      pi = 4d0 * atan(1d0)                                           ! pi
real(8), parameter ::   twopi = 8d0 * atan(1d0)                                           ! 2•pi
real(8), parameter ::  halfpi = 2d0 * atan(1d0)                                           ! pi/2
real(8), parameter ::    pi_2 = 9.86960440108935861883449099987615113531369940724079d0    ! pi^2
real(8), parameter :: GoldenR = 1.61803398874989484820458683436563811772030917980576d0    ! Golden Ratio = (1d0 + sqrt(5))/2d0
real(8), parameter ::  EulerN = 2.71828182845904523536028747135266249775724709369996d0    ! Euler's Number
real(8), parameter :: FeignbD = 4.66920160910299067185320382046620161725818557747577d0    ! Feigenbaum bifurcation velocity (delta)
real(8), parameter :: FeignbA = 2.50290787509589282228390287321821578638127137672715d0    ! Feigenbaum reduction parameter (alpha)
!! ============================================================================================================================== !!

private :: show_math_cts

interface show_mathematical_constants
     module procedure show_math_cts
end interface

contains

     subroutine show_math_cts()
     implicit none

          print *
          print '(*(g0))' , '•••••••••••••••••••••••' , &
                            '•••••••••••••••••••••••'
          print '(*(g0))' , '••• :: AVAILABLE MATHEMATICAL CONSTANTS :: •••' , ' ==> parameter_name'
          print '(*(g0))' , '•••••••••••••••••••••••' , &
                            '•••••••••••••••••••••••'

          print '(*(g0))' , '•                                            •'
          print '(*(g0))' , "•                  Pi = "  ,  pi      ,  "   •" , ' ==> pi'
          print '(*(g0))' , "•                2•Pi = "  ,  twopi   ,  "   •" , ' ==> twopi'
          print '(*(g0))' , "•          Pi squared = "  ,  pi_2    ,  "   •" , ' ==> pi_2'
          print '(*(g0))' , "•        Golden Ratio = "  ,  GoldenR ,  "   •" , ' ==> GoldenR'
          print '(*(g0))' , "•      Euler's Number = "  ,  EulerN  ,  "   •" , ' ==> EulerN'
          print '(*(g0))' , "•  Feigenbaum's delta = "  ,  FeignbD ,  "   •" , ' ==> FeignbD'
          print '(*(g0))' , "•  Feigenbaum's alpha = "  ,  FeignbD ,  "   •" , ' ==> FeignbA'
          print '(*(g0))' , '•                                            •'

          print '(*(g0))' , '•••••••••••••••••••••••' , &
                            '•••••••••••••••••••••••'
          print *

     end subroutine show_math_cts

end module lib_mathematical_constants
