module dstruct_dynamicarray
   use dstruct_concepts, only: container_type
   implicit none
   private
   public :: dynamicarray_type

   type :: dynamicarray_type
      integer, private :: used, reserved
      type(container_type), private, pointer :: field(:)
   contains
      procedure :: used_size => dynamicarray_used
      procedure :: reserved_size => dynamicarray_reserved
      procedure :: push_back => dynamicarray_push_back
      procedure :: pop_back => dynamicarray_pop_back
      procedure :: insert => dynamicarray_insert
      procedure :: remove => dynamicarray_remove
      procedure :: at => dynamicarray_at
      procedure :: first => dynamicarray_first
      procedure :: last => dynamicarray_last
      procedure :: swap => dynamicarray_swap
      procedure :: is_empty => dynamicarray_is_empty
      procedure :: shrink_to_fit => dynamicarray_shrink_to_fit
      procedure, private :: resize => dynamicarray_resize
      final :: destroy_dynamicarray
   end type
   interface dynamicarray_type
      module procedure :: new_dynamicarray
      module procedure :: new_dynamicarray_from_array
   end interface

contains

   !>
   function new_dynamicarray(size) result(self)
      type(dynamicarray_type) :: self
      integer, optional, intent(in) :: size
      self%used = 0
      if (present(size)) then
         self%reserved = size
      else
         self%reserved = 8
      end if
      allocate(self%field(size))
   end function

   !>
   function new_dynamicarray_from_array(values) result(self)
      type(dynamicarray_type) :: self
      class(*), intent(in) :: values(:)
      integer :: i
      self%used = size(values)
      self%reserved = size(values)
      allocate(self%field(size(values)))
      do i=1,size(values)
         self%field(i)%obj = values(i)
      end do
   end function

   subroutine destroy_dynamicarray(self)
      type(dynamicarray_type), intent(inout) :: self
      integer :: i
      self%used = 0
      self%reserved = 0
      do i=1,self%used
         deallocate(self%field(i)%obj)
      end do
      deallocate(self%field)
   end subroutine

   function dynamicarray_used(self)
      integer :: dynamicarray_used
      class(dynamicarray_type), intent(in) :: self
      dynamicarray_used = self%used
   end function

   function dynamicarray_reserved(self)
      integer :: dynamicarray_reserved
      class(dynamicarray_type), intent(in) :: self
      dynamicarray_reserved = self%reserved
   end function

   subroutine dynamicarray_push_back(self, obj)
      class(dynamicarray_type), intent(inout) :: self
      class(*), intent(in) :: obj

      if (self%used .eq. self%reserved) &
    &    call self%resize(3*self%reserved/2)
      self%used = self%used + 1
      self%field(self%used)%obj = obj
   end subroutine

   subroutine dynamicarray_pop_back(self, removed)
      class(dynamicarray_type), intent(inout) :: self
      type(container_type), intent(out), optional :: removed

      if (present(removed)) then
         call move_alloc(self%field(self%used)%obj, removed%obj)
      else
         deallocate(self%field(self%used)%obj)
      end if
      self%used = self%used - 1
   end subroutine

   subroutine dynamicarray_insert(self, i, obj)
      class(dynamicarray_type), intent(inout) :: self
      integer, intent(in) :: i
      class(*), intent(in) :: obj

      if (self%used .eq. self%reserved) &
    &    call self%resize(3*self%reserved/2)
      self%field(i+1:self%used+1) = self%field(i:self%used)
      self%used = self%used + 1
      self%field(i)%obj = obj
   end subroutine

   subroutine dynamicarray_remove(self, i, removed)
      class(dynamicarray_type), intent(inout) :: self
      integer, intent(in) :: i
      type(container_type), intent(out), optional :: removed

      if (present(removed)) then
         call move_alloc(self%field(i)%obj, removed%obj)
      else
         deallocate(self%field(i)%obj)
      end if
      self%field(i:self%used-1) = self%field(i+1:self%used)
      deallocate(self%field(self%used)%obj)
      self%used = self%used - 1
   end subroutine

   function dynamicarray_at(self, i)
      type(container_type), pointer :: dynamicarray_at
      class(dynamicarray_type), intent(inout) :: self
      integer, intent(in) :: i

      dynamicarray_at => self%field(i)
   end function

   function dynamicarray_first(self)
      type(container_type), pointer :: dynamicarray_first
      class(dynamicarray_type), intent(in) :: self
      dynamicarray_first => self%field(1)
   end function

   function dynamicarray_last(self)
      type(container_type), pointer :: dynamicarray_last
      class(dynamicarray_type), intent(in) :: self
      dynamicarray_last => self%field(self%used)
   end function

   subroutine dynamicarray_swap(self, i, j)
      class(dynamicarray_type), intent(inout) :: self
      integer, intent(in) :: i, j
      class(*), allocatable :: tmp

      call move_alloc(self%field(i)%obj, tmp)
      call move_alloc(self%field(j)%obj, self%field(i)%obj)
      call move_alloc(tmp, self%field(j)%obj)
   end subroutine

   function dynamicarray_is_empty(self)
      logical :: dynamicarray_is_empty
      class(dynamicarray_type), intent(in) :: self
      dynamicarray_is_empty = self%used .eq. 0
   end function

   subroutine dynamicarray_shrink_to_fit(self)
      class(dynamicarray_type), intent(inout) :: self
      if (self%used .ne. self%reserved) then
         call self%resize(self%used)
      end if
   end subroutine

   subroutine dynamicarray_resize(self, size)
      class(dynamicarray_type), intent(inout) :: self
      integer, intent(in) :: size
      integer :: s
      type(container_type), pointer :: temp(:)

      allocate(temp(size))
      temp(:min(size,self%reserved)) = self%field(:min(size,self%reserved))
      do s=1,self%used
         deallocate(self%field(s)%obj)
      end do
      deallocate(self%field)
      self%field => temp
      self%reserved = size
   end subroutine
end module
