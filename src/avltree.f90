module AVLTree
   implicit none
   private
   public :: avl_tree, avl_node, destroy_avl_tree, iterator, &
      & avl_tree_preorder, avl_tree_inorder, avl_tree_postorder, &
      & avl_tree_levelorder

   type :: avl_node
      character(len=:), allocatable :: key
      integer :: val
      class(avl_node), allocatable :: left, right
   contains
      procedure, private :: find => avl_node_find
      procedure, private :: add => avl_node_add
      procedure, private :: balance => avl_node_balance
      procedure, private :: height => avl_node_height
      procedure, private :: min => avl_node_min
      procedure, private :: max => avl_node_max
      procedure, private :: is_leaf => avl_node_isleaf
      procedure, private :: is_inner => avl_node_isinner
   end type
   interface avl_node
      module procedure :: new_avl_node
   end interface

   type :: avl_tree
      class(avl_node), allocatable :: head
   contains
      procedure :: empty => avl_tree_empty
      procedure :: find => avl_tree_find
      procedure :: add => avl_tree_add
      procedure :: remove => avl_tree_remove
      procedure :: height => avl_tree_height
      procedure :: min => avl_tree_min
      procedure :: max => avl_tree_max
   end type
   interface avl_tree
      module procedure :: new_avl_tree
   end interface

   type, abstract :: iterator
   contains
      procedure(next_interface), deferred :: next
      procedure(has_next_interface), deferred :: has_next
   end type
   abstract interface
      subroutine next_interface(self, node)
         import :: iterator, avl_node
         class(iterator), intent(inout) :: self
         class(avl_node), pointer, intent(out) :: node
      end subroutine
      pure function has_next_interface(self)
         import :: iterator
         logical :: has_next_interface
         class(iterator), intent(in) :: self
      end function
   end interface

   type, extends(iterator) :: avl_tree_preorder
   contains
      procedure :: next => avl_tree_preorder_next
      procedure :: has_next => avl_tree_preorder_has_next
   end type
   interface avl_tree_preorder
      module procedure :: new_avl_tree_preorder
   end interface

   type, extends(iterator) :: avl_tree_inorder
   contains
      procedure :: next => avl_tree_inorder_next
      procedure :: has_next => avl_tree_inorder_has_next
   end type
   interface avl_tree_inorder
      module procedure :: new_avl_tree_inorder
   end interface

   type, extends(iterator) :: avl_tree_postorder
   contains
      procedure :: next => avl_tree_postorder_next
      procedure :: has_next => avl_tree_postorder_has_next
   end type
   interface avl_tree_postorder
      module procedure :: new_avl_tree_postorder
   end interface

   type, extends(iterator) :: avl_tree_levelorder
   contains
      procedure :: next => avl_tree_levelorder_next
      procedure :: has_next => avl_tree_levelorder_has_next
   end type
   interface avl_tree_levelorder
      module procedure :: new_avl_tree_inorder
   end interface

contains

   function new_avl_node(key, val)
      type(avl_node) :: new_avl_node
      character(len=*), intent(in) :: key
      integer, intent(in) :: val

      new_avl_node%key = key
      new_avl_node%val = val
   end function

   recursive subroutine destroy_avl_node(node)
      class(avl_node), allocatable, intent(inout) :: node
      integer :: stat
      character(len=512) :: err_msg
      if (allocated(node%left)) then
         call destroy_avl_node(node%left)
      end if
      if (allocated(node%right)) then
         call destroy_avl_node(node%right)
      end if
      deallocate(node, stat=stat, errmsg=err_msg)
      if (stat /= 0) then
         print*,err_msg
      end if 
   end subroutine

   recursive subroutine avl_node_find(self, key, stat, node)
      class(avl_node), target, intent(in) :: self
      character(len=*), intent(in) :: key
      logical, intent(out) :: stat
      class(avl_node), pointer, intent(inout) :: node

      stat = .false.
      nullify(node)
      if (self%key .eq. key) then
         stat = .true.
         node => self
      else if (key .lt. self%key) then
         if (allocated(self%left)) then
            call self%left%find(key, stat, node)
         end if
      else
         if (allocated(self%right)) then
            call self%right%find(key, stat, node)
         end if
      end if
   end subroutine

   recursive subroutine avl_node_add(self, key, val, stat)
      class(avl_node), intent(inout) :: self
      character(len=*), intent(in) :: key
      integer, intent(in) :: val
      logical, intent(out) :: stat
      class(avl_node), allocatable :: mid

      if (self%key .eq. key) then
         stat = .false.
      else if (key .lt. self%key) then
         if (allocated(self%left)) then
            call self%left%add(key, val, stat)
            if (stat) then
               call avl_node_rebalance(self%left, mid)
               if (allocated(mid)) then
                  call move_alloc(mid, self%left)
               end if
            end if
         else
            allocate(self%left, source=avl_node(key, val))
            stat = .true.
         end if
      else
         if (allocated(self%right)) then
            call self%right%add(key, val, stat)
            if (stat) then
               call avl_node_rebalance(self%right, mid)
               if (allocated(mid)) then
                  call move_alloc(mid, self%right)
               end if
            end if
         else
            allocate(self%right, source=avl_node(key, val))
            stat = .true.
         end if
      end if
   end subroutine

   recursive subroutine avl_node_remove(node, key, stat)
      class(avl_node), intent(inout) :: node
      character(len=*), intent(in) :: key
      logical, intent(out) :: stat
      class(avl_node), allocatable :: left, right, dum

      if (key .lt. node%key) then
         if (allocated(node%left)) then
            if (key .eq. node%left%key) then
               stat = .true.
               call avl_node_unlink(node%left, dum)
               call destroy_avl_node(node%left)
               call move_alloc(dum, node%left)
            else
               call avl_node_remove(node%left, key, stat)
               if (stat) then
                  call avl_node_rebalance(node%left, dum)
                  if (allocated(dum)) then
                     call move_alloc(dum, node%left)
                  end if
               end if
            end if
         else
            stat = .false.
         end if
      else if (key .gt. node%key) then
         if (allocated(node%right)) then
            if (key .eq. node%right%key) then
               stat = .true.
               call avl_node_unlink(node%right, dum)
               call destroy_avl_node(node%right)
               call move_alloc(dum, node%right)
            else
               call avl_node_remove(node%right, key, stat)
               if (stat) then
                  call avl_node_rebalance(node%right, dum)
                  if (allocated(dum)) then
                     call move_alloc(dum, node%right)
                  end if
               end if
            end if
         else
            stat = .false.
         end if
      end if
   end subroutine

   subroutine avl_node_unlink(node, new_node)
      class(avl_node), intent(inout) :: node
      class(avl_node), allocatable, intent(out) :: new_node
      class(avl_node), allocatable :: dum

      if (node%is_leaf()) then
         ! no children to unlink
      else if (node%is_inner()) then
         if (.not.allocated(node%right%left)) then
            call move_alloc(node%left, node%right%left)
            call move_alloc(node%right, new_node)
         else
            call unlink_min_node(node%right, dum)
            call move_alloc(node%left, dum%left)
            call move_alloc(node%right, dum%right)
            call move_alloc(dum, new_node)
         end if
      else if (allocated(node%left)) then
         call move_alloc(node%left, new_node)
      else ! (allocated(node%right)) then
         call move_alloc(node%right, new_node)
      end if
   contains
      recursive subroutine unlink_min_node(node, min)
         class(avl_node), intent(inout) :: node
         class(avl_node), allocatable, intent(out) :: min
         class(avl_node), allocatable :: dum

         if (allocated(node%left)) then
            if (.not.allocated(node%left%left)) then
               call move_alloc(node%left, min)
            else
               call unlink_min_node(node%left, min)
               call avl_node_rebalance(node%left, dum)
               if (allocated(dum)) then
                  call move_alloc(dum, node%left)
               end if
            end if 
         end if
      end subroutine
   end subroutine

   subroutine avl_node_rebalance(node, mid)
      class(avl_node), allocatable, intent(inout) :: node
      class(avl_node), allocatable, intent(out) :: mid

      class(avl_node), allocatable :: child,grandchild,t1,t2,t3,t4,min,max
      integer :: balance
      logical :: left

      if (.not.allocated(node)) then
         return
      end if

      balance = node%balance()
      if (abs(balance) .le. 1) then
         return
      end if

      if (balance .lt. 0) then
         call move_alloc(node%right, child)
      else
         call move_alloc(node%left, child)
      end if

      if (child%balance() .gt. 0) then
         call move_alloc(child%left, grandchild)
      else
         call move_alloc(child%right, grandchild)
      end if

      ! right - right
      if (node%key.lt.child%key .and. child%key.lt.grandchild%key) then
         call move_alloc(node%left, t1)
         call move_alloc(child%left, t2)
         call move_alloc(grandchild%left, t3)
         call move_alloc(grandchild%right, t4)

         call move_alloc(node, min)
         call move_alloc(child, mid)
         call move_alloc(grandchild, max)
      ! right - left
      else if (node%key.lt.child%key .and. child%key.gt.grandchild%key) then
         call move_alloc(node%left, t1)
         call move_alloc(grandchild%left, t2)
         call move_alloc(grandchild%right, t3)
         call move_alloc(child%right, t4)

         call move_alloc(node, min)
         call move_alloc(grandchild, mid)
         call move_alloc(child, max)
      ! left - right
      else if (node%key.gt.child%key .and. child%key.lt.grandchild%key) then
         call move_alloc(child%left, t1)
         call move_alloc(grandchild%left, t2)
         call move_alloc(grandchild%right, t3)
         call move_alloc(node%right, t4)

         call move_alloc(child, min)
         call move_alloc(grandchild, mid)
         call move_alloc(node, max)
      ! left - left
      else ! (self%key.gt.parent%key.and.parent%key.gt.greatparent%key) then
         call move_alloc(grandchild%left, t1)
         call move_alloc(grandchild%right, t2)
         call move_alloc(child%right, t3)
         call move_alloc(node%right, t4)

         call move_alloc(grandchild, min)
         call move_alloc(child, mid)
         call move_alloc(node, max)
      end if

      call move_alloc(t1, min%left)
      call move_alloc(t2, min%right)
      call move_alloc(t3, max%left)
      call move_alloc(t4, max%right)

      call move_alloc(min, mid%left)
      call move_alloc(max, mid%right)
   end subroutine

   pure function avl_node_balance(self)
      integer :: avl_node_balance
      class(avl_node), intent(in) :: self
      integer :: left_height, right_height
      left_height = 0
      if (allocated(self%left)) then
         left_height = self%left%height()
      end if 
      right_height = 0
      if (allocated(self%right)) then
         right_height = self%right%height()
      end if
      avl_node_balance = left_height - right_height
   end function

   pure function avl_node_isleaf(self)
      logical :: avl_node_isleaf
      class(avl_node), intent(in) :: self
      avl_node_isleaf = .not.(allocated(self%left).or.allocated(self%right))
   end function

   pure function avl_node_isinner(self)
      logical :: avl_node_isinner
      class(avl_node), intent(in) :: self
      avl_node_isinner = allocated(self%left).and.allocated(self%right)
   end function

   pure recursive function avl_node_height(self)
      integer :: avl_node_height
      class(avl_node), intent(in) :: self
      integer :: left_height, right_height

      left_height = 0
      if (allocated(self%left)) then
         left_height = self%left%height()
      end if
      right_height = 0
      if (allocated(self%right)) then
         right_height = self%right%height()
      end if
      avl_node_height = max(left_height, right_height) + 1
   end function

   pure recursive function avl_node_min(self)
      integer :: avl_node_min
      class(avl_node), intent(in) :: self
      if (allocated(self%left)) then
         avl_node_min = self%left%min()
      else
         avl_node_min = self%val
      end if
   end function

   pure recursive function avl_node_max(self)
      integer :: avl_node_max
      class(avl_node), intent(in) :: self
      if (allocated(self%right)) then
         avl_node_max = self%right%max()
      else
         avl_node_max = self%val
      end if
   end function


   pure function avl_tree_empty(self)
      class(avl_tree), intent(in) :: self
      logical :: avl_tree_empty
      avl_tree_empty = .not.allocated(self%head)
   end function

   subroutine avl_tree_find(self, key, stat, node)
      class(avl_tree), intent(in) :: self
      character(len=*), intent(in) :: key
      logical, intent(inout) :: stat
      class(avl_node), pointer, intent(out) :: node
      
      if (self%empty()) then
         stat = .false.
         nullify(node)
      end if

      call self%head%find(key, stat, node)
   end subroutine

   subroutine avl_tree_add(self, key, val, stat)
      class(avl_tree), intent(inout) :: self
      character(len=*), intent(in) :: key
      integer, intent(in) :: val
      logical, intent(inout) :: stat
      class(avl_node), allocatable :: mid

      if (self%empty()) then
         stat = .true.
         allocate(self%head, source=avl_node(key, val))
      else
         call avl_node_add(self%head, key, val, stat)
         if (stat) then
            call avl_node_rebalance(self%head, mid)
            if (allocated(mid)) then
               call move_alloc(mid, self%head)
            end if
         end if
      end if
   end subroutine

   subroutine avl_tree_remove(self, key, stat)
      class(avl_tree), intent(inout) :: self
      character(len=*), intent(in) :: key
      logical, intent(out) :: stat
      class(avl_node), allocatable :: left, dum

      if (self%empty()) then
         stat = .false.
      else if (self%head%key .eq. key) then
         stat = .true.
         call avl_node_unlink(self%head, dum)
         call destroy_avl_node(self%head)
         call move_alloc(dum, self%head)

         call avl_node_rebalance(self%head, dum)
         if (allocated(dum)) then
            call move_alloc(dum, self%head)
         end if
      else
         call avl_node_remove(self%head, key, stat)
         if (stat) then
            call avl_node_rebalance(self%head, dum)
            if (allocated(dum)) then
               call move_alloc(dum, self%head)
            end if
         end if
      end if
   end subroutine

   pure function avl_tree_height(self)
      integer :: avl_tree_height
      class(avl_tree), intent(in) :: self
      if (self%empty()) then
         avl_tree_height = 0
      else
         avl_tree_height = self%head%height()
      end if
   end function

   pure subroutine avl_tree_min(self, min, stat)
      class(avl_tree), intent(in) :: self
      integer, intent(out) :: min
      logical, intent(out) :: stat
      stat = .false.
      if (allocated(self%head)) then
         min = self%head%min()
         stat = .true.
      end if
   end subroutine

   pure subroutine avl_tree_max(self, max, stat)
      class(avl_tree), intent(in) :: self
      integer, intent(out) :: max
      logical, intent(out) :: stat
      stat = .false.
      if (allocated(self%head)) then
         max = self%head%max()
         stat = .true.
      end if
   end subroutine

   pure function new_avl_tree()
      type(avl_tree) :: new_avl_tree
   end function

   subroutine destroy_avl_tree(self)
      type(avl_tree) :: self
      if (allocated(self%head)) then
         call destroy_avl_node(self%head)
      end if
   end subroutine


   pure function new_avl_tree_preorder(tree)
      type(avl_tree_preorder) :: new_avl_tree_preorder
      class(avl_tree), intent(in) :: tree
      ! TODO
   end function
   
   subroutine avl_tree_preorder_next(self, node)
      class(avl_tree_preorder), intent(inout) :: self
      class(avl_node), pointer, intent(out) :: node
      nullify(node)
      ! TODO
   end subroutine
   
   pure function avl_tree_preorder_has_next(self)
      logical :: avl_tree_preorder_has_next
      class(avl_tree_preorder), intent(in) :: self
      ! TODO
   end function


   
   pure function new_avl_tree_inorder(tree)
      type(avl_tree_inorder) :: new_avl_tree_inorder
      class(avl_tree), intent(in) :: tree
      ! TODO
   end function
   
   subroutine avl_tree_inorder_next(self, node)
      class(avl_tree_inorder), intent(inout) :: self
      class(avl_node), pointer, intent(out) :: node
      nullify(node)
      ! TODO
   end subroutine
   
   pure function avl_tree_inorder_has_next(self)
      logical :: avl_tree_inorder_has_next
      class(avl_tree_inorder), intent(in) :: self
      ! TODO
   end function

   
   
   pure function new_avl_tree_postorder(tree)
      type(avl_tree_preorder) :: new_avl_tree_postorder
      class(avl_tree), intent(in) :: tree
      ! TODO
   end function

   subroutine avl_tree_postorder_next(self, node)
      class(avl_tree_postorder), intent(inout) :: self
      class(avl_node), pointer, intent(out) :: node
      ! TODO
   end subroutine

   pure function avl_tree_postorder_has_next(self)
      logical :: avl_tree_postorder_has_next
      class(avl_tree_postorder), intent(in) :: self
      ! TODO
   end function

   
   
   pure function new_avl_tree_levelorder(tree)
      type(avl_tree_levelorder) :: new_avl_tree_levelorder
      class(avl_tree), intent(in) :: tree
      ! TODO
   end function

   subroutine avl_tree_levelorder_next(self, node)
      class(avl_tree_levelorder), intent(inout) :: self
      class(avl_node), pointer, intent(out) :: node
      ! TODO
   end subroutine

   pure function avl_tree_levelorder_has_next(self)
      logical :: avl_tree_levelorder_has_next
      class(avl_tree_levelorder), intent(in) :: self
      ! TODO
   end function
end module
