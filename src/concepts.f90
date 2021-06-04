module dstruct_concepts
   implicit none

   abstract interface
      !> lhs .lt. rhs -> result < 0
      !> lhs .eq. rhs -> result = 0
      !> lhs .gt. rhs -> result > 0
      pure function c_ordering(lhs, rhs)
         integer :: c_ordering
         class(*), intent(in) :: lhs, rhs
      end function
   end interface

   type :: container_type
      class(*), allocatable :: obj
   end type
end module
