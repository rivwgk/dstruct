program Test_DynamicArray
   use DynamicArray
   use Concepts, only: container_type
   implicit none
   type(dynamic_array), allocatable :: da
   type(container_type), pointer :: c
   integer :: i
   allocate(da, source=dynamic_array(7))

   do i=1,40
      call da%push_back(i)
   end do

   print*,da%used_size(), '/', da%reserved_size()
   call da%shrink_to_fit()
   print*,da%used_size(), '/', da%reserved_size()

   c => da%at(10)
   c%obj = -3

   do i=1,da%used_size()
      c => da%at(i)
      select type(j => c%obj)
      type is (integer)
         print*, j 
      end select
   end do

   call destroy_dynamic_array(da)
   deallocate(da)
end program
