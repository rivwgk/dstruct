module DSContainer
   implicit none

   type :: ds_container
      class(*), allocatable :: obj
   end type

   type, abstract, extends(ds_container) :: ds_ordered
   contains
      procedure(ordered_lt_interface), deferred :: ordered_lt
      procedure(ordered_eq_interface), deferred :: ordered_eq
      procedure, private :: ordered_le => ds_ordered_le
      procedure, private :: ordered_ne => ds_ordered_ne
      procedure, private :: ordered_gt => ds_ordered_gt
      procedure, private :: ordered_ge => ds_ordered_ge
      generic :: operator(.lt.) => ordered_lt
      generic :: operator(.eq.) => ordered_eq
   end type
   abstract interface
      elemental logical function ordered_lt_interface(lhs, rhs)
         import ds_ordered
         class(ds_ordered), intent(in) :: lhs, rhs
      end function
      elemental logical function ordered_eq_interface(lhs, rhs)
         import ds_ordered
         class(ds_ordered), intent(in) :: lhs, rhs
      end function
   end interface
contains
   elemental logical function ds_ordered_le(lhs, rhs)
      class(ds_ordered), intent(in) :: lhs, rhs
      ds_ordered_le = (lhs .lt. rhs).and.(lhs .eq. rhs)
   end function

   elemental logical function ds_ordered_ne(lhs, rhs)
      class(ds_ordered), intent(in) :: lhs, rhs
      ds_ordered_ne = .not.(lhs .eq. rhs)
   end function

   elemental logical function ds_ordered_gt(lhs, rhs)
      class(ds_ordered), intent(in) :: lhs, rhs
      ds_ordered_gt = .not.(lhs .lt. rhs).or..not.(lhs .eq. rhs)
   end function

   elemental logical function ds_ordered_ge(lhs, rhs)
      class(ds_ordered), intent(in) :: lhs, rhs
      ds_ordered_ge = .not.(lhs .lt. rhs)
   end function
end module
