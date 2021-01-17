program avltest
   use AVLTree, only: avl_tree, avl_node, destroy_avl_tree
   implicit none
   type(avl_tree), target :: tree
   logical :: stat

   call tree%add('f', 6, stat)
   print*, 'adding key `f`, height: ', tree%height()
   call output_tree(tree%head, 0)
   call tree%add('d', 1, stat)
   print*, 'adding key `d`, height: ', tree%height()
   call output_tree(tree%head, 0)
   call tree%add('c', 4, stat) ! rebalance left-left
   print*, 'adding key `c`, height: ', tree%height()
   call output_tree(tree%head, 0)
   call tree%add('a', 3, stat)
   print*, 'adding key `a`, height: ', tree%height()
   call output_tree(tree%head, 0)
   call tree%add('b', 2, stat) ! rebalance left-right
   print*, 'adding key `b`, height: ', tree%height()
   call output_tree(tree%head, 0)
   call tree%add('h', 5, stat)
   print*, 'adding key `h`, height: ', tree%height()
   call output_tree(tree%head, 0)
   call tree%add('k', 8, stat) ! rebalance right-right
   print*, 'adding key `k`, height: ', tree%height()
   call output_tree(tree%head, 0)
   call tree%add('n', 9, stat)
   print*, 'adding key `n`, height: ', tree%height()
   call output_tree(tree%head, 0)
   call tree%add('m', 7, stat) ! rebalance right-left
   print*, 'adding key `m`, height: ', tree%height()
   call output_tree(tree%head, 0)

   call destroy_avl_tree(tree)
contains
   recursive subroutine output_tree(node, depth)
      class(avl_node), intent(in) :: node
      integer, intent(in) :: depth
      integer :: i
      print*, repeat('  ', depth), node%key, ', ', node%val
      if (allocated(node%left)) then
         call output_tree(node%left, depth+1)
      end if
      if (allocated(node%right)) then
         call output_tree(node%right, depth+1)
      end if
   end subroutine

end program
