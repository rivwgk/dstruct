module DynamicArray
   use Concepts, only: container_type
   implicit none
   private
   public :: dynamic_array
   public :: destroy_dynamic_array

   type :: dynamic_array
      integer, private :: used, reserved
      type(container_type), private, pointer :: field(:)
   contains
      procedure :: used_size => dynamic_array_used
      procedure :: reserved_size => dynamic_array_reserved
      procedure :: push_back => dynamic_array_push_back
      procedure :: pop_back => dynamic_array_pop_back
      procedure :: at => dynamic_array_at
      procedure :: first => dynamic_array_first
      procedure :: last => dynamic_array_last
      procedure :: is_empty => dynamic_array_is_empty
      procedure :: shrink_to_fit => dynamic_array_shrink_to_fit
      procedure, private :: resize => dynamic_array_resize
   end type
   interface dynamic_array
      module procedure :: new_dynamic_array
   end interface

contains

   function new_dynamic_array(size) result(self)
      type(dynamic_array) :: self
      integer, optional, intent(in) :: size
      self%used = 0
      if (present(size)) then
         self%reserved = size
      else
         self%reserved = 8
      end if
      allocate(self%field(size))
   end function

   subroutine destroy_dynamic_array(self)
      type(dynamic_array), allocatable, intent(inout) :: self
      self%used = 0
      self%reserved = 0
      deallocate(self%field)
   end subroutine

   function dynamic_array_used(self)
      integer :: dynamic_array_used
      class(dynamic_array), intent(in) :: self
      dynamic_array_used = self%used
   end function

   function dynamic_array_reserved(self)
      integer :: dynamic_array_reserved
      class(dynamic_array), intent(in) :: self
      dynamic_array_reserved = self%reserved
   end function

   subroutine dynamic_array_push_back(self, obj)
      class(dynamic_array), intent(inout) :: self
      class(*), intent(in) :: obj

      if (self%used .eq. self%reserved) then
         call self%resize(3*self%reserved/2)
      end if
      self%used = self%used + 1
      self%field(self%used)%obj = obj
   end subroutine

   subroutine dynamic_array_pop_back(self)
      class(dynamic_array), intent(inout) :: self

      self%used = self%used - 1
   end subroutine

   function dynamic_array_at(self, i)
      type(container_type), pointer :: dynamic_array_at
      class(dynamic_array), intent(inout) :: self
      integer, intent(in) :: i

      dynamic_array_at => self%field(i)
   end function

   function dynamic_array_first(self)
      type(container_type), pointer :: dynamic_array_first
      class(dynamic_array), intent(in) :: self
      dynamic_array_first => self%field(1)
   end function

   function dynamic_array_last(self)
      type(container_type), pointer :: dynamic_array_last
      class(dynamic_array), intent(in) :: self
      dynamic_array_last => self%field(self%used)
   end function

   function dynamic_array_is_empty(self)
      logical :: dynamic_array_is_empty
      class(dynamic_array), intent(in) :: self
      dynamic_array_is_empty = self%used .eq. 0
   end function

   subroutine dynamic_array_shrink_to_fit(self)
      class(dynamic_array), intent(inout) :: self
      if (self%used .ne. self%reserved) then
         call self%resize(self%used)
      end if
   end subroutine

   subroutine dynamic_array_resize(self, size)
      class(dynamic_array), intent(inout) :: self
      integer, intent(in) :: size
      type(container_type), pointer :: temp(:)

      allocate(temp(size))
      temp(:min(size,self%reserved)) = self%field(:min(size,self%reserved))
      deallocate(self%field)
      self%field => temp
      self%reserved = size
   end subroutine
end module
