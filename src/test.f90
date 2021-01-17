program avltest
   use AVLTree, only: avl_tree, avl_node, destroy_avl_tree
   implicit none
   type(avl_tree), target :: tree
   logical :: stat

   call test_add(tree, 'f', 6, stat)
   call test_add(tree, 'd', 1, stat)
   call test_remove(tree, 'f', stat)
   call test_remove(tree, 'd', stat)

   call test_add(tree, 'f', 6, stat)
   call test_add(tree, 'd', 1, stat)
   call test_add(tree, 'c', 4, stat) ! rebalance left-left
   call test_add(tree, 'a', 3, stat)
   call test_add(tree, 'b', 2, stat) ! rebalance left-right
   call test_add(tree, 'h', 5, stat)
   call test_add(tree, 'k', 8, stat) ! rebalance right-right
   call test_add(tree, 'n', 9, stat)
   call test_add(tree, 'm', 7, stat) ! rebalance right-left

   call test_remove(tree, 'n', stat)
   call test_remove(tree, 'm', stat)
   call test_remove(tree, 'h', stat)

   call destroy_avl_tree(tree)
contains
   recursive subroutine output_tree(node, depth)
      class(avl_node), allocatable, intent(in) :: node
      integer, intent(in) :: depth
      integer :: i

      if (.not.allocated(node)) then
         print*, '(empty tree)'
         return
      end if

      print*, repeat('  ', depth), node%key, ', ', node%val
      if (allocated(node%left)) then
         call output_tree(node%left, depth+1)
      else
         print*, repeat('  ', depth+1), '-'
      end if
      if (allocated(node%right)) then
         call output_tree(node%right, depth+1)
      else
         print*, repeat('  ', depth+1), '-'
      end if
   end subroutine

   subroutine test_add(tree, key, val, stat)
      class(avl_tree), intent(inout) :: tree
      character(len=*), intent(in) :: key
      integer, intent(in) :: val
      logical, intent(out) :: stat
      call tree%add(key, val, stat)
      print'(a, a, a, i)', 'added key `', key, '` height: ', tree%height()
      call output_tree(tree%head, 0)
   end subroutine

   subroutine test_remove(tree, key, stat)
      class(avl_tree), intent(inout) :: tree
      character(len=*), intent(in) :: key
      logical, intent(out) :: stat
      call tree%remove(key, stat)
      print'(a, a, a, i)', 'removed key `', key, '` height: ', tree%height()
      call output_tree(tree%head, 0)
  end subroutine
end program
