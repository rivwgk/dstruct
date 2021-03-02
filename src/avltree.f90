module AVLTree
   use Concepts, only: c_ordering
   implicit none
   private
   public :: avl_tree
   public :: avl_node
   public :: destroy_avl_tree
   public :: iterator
   public :: avl_tree_preorder
   public :: avl_tree_inorder
   public :: avl_tree_postorder
   public :: avl_tree_levelorder

   type :: avl_node
      class(*), allocatable :: key
      class(*), allocatable :: val
      class(avl_node), pointer :: left, right
   end type
   interface avl_node
      module procedure :: new_avl_node
   end interface

   type :: avl_tree
      class(avl_node), pointer :: head
      procedure(c_ordering), nopass, pointer :: ordering => null()
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
      class(*), intent(in) :: key
      class(*), intent(in) :: val

      new_avl_node%key = key
      new_avl_node%val = val
      nullify(new_avl_node%left)
      nullify(new_avl_node%right)
   end function

   recursive subroutine destroy_avl_node(node)
      class(avl_node), pointer, intent(inout) :: node
      integer :: stat
      character(len=512) :: err_msg
      if (associated(node%left)) then
         call destroy_avl_node(node%left)
      end if
      if (associated(node%right)) then
         call destroy_avl_node(node%right)
      end if
      deallocate(node, stat=stat, errmsg=err_msg)
      if (stat /= 0) then
         print*,err_msg
      end if 
   end subroutine

   recursive subroutine avl_node_find(self, key, stat, node, ordering)
      class(avl_node), target, intent(in) :: self
      class(*), intent(in) :: key
      logical, intent(out) :: stat
      class(avl_node), pointer, intent(inout) :: node
      procedure(c_ordering) :: ordering

      integer :: order

      order = ordering(key, self%key)
      stat = .false.
      nullify(node)
      if (order .eq. 0) then
         stat = .true.
         node => self
      else if (order .lt. 0) then
         if (associated(self%left)) then
            call avl_node_find(self%left, key, stat, node, ordering)
         end if
      else
         if (associated(self%right)) then
            call avl_node_find(self%right, key, stat, node, ordering)
         end if
      end if
   end subroutine

   recursive subroutine avl_node_add(self, key, val, stat, ordering)
      class(avl_node), intent(inout) :: self
      class(*), intent(in) :: key, val
      logical, intent(out) :: stat
      procedure(c_ordering) :: ordering

      class(avl_node), pointer :: mid
      integer :: order

      order = ordering(key, self%key)
      if (order .eq. 0) then
         stat = .false.
      else if (order .lt. 0) then
         if (associated(self%left)) then
            call avl_node_add(self%left, key, val, stat, ordering)
            if (stat) then
               call avl_node_rebalance(self%left, mid, ordering)
               if (associated(mid)) then
                  self%left => mid
               end if
            end if
         else
            allocate(self%left, source=avl_node(key, val))
            stat = .true.
         end if
      else ! (order .gt. 0)
         if (associated(self%right)) then
            call avl_node_add(self%right, key, val, stat, ordering)
            if (stat) then
               call avl_node_rebalance(self%right, mid, ordering)
               if (associated(mid)) then
                  self%right => mid
               end if
            end if
         else
            allocate(self%right, source=avl_node(key, val))
            stat = .true.
         end if
      end if
   end subroutine

   recursive subroutine avl_node_remove(node, key, stat, ordering, removed)
      class(avl_node), intent(inout) :: node
      class(*), intent(in) :: key
      logical, intent(out) :: stat
      procedure(c_ordering) :: ordering
      class(avl_node), pointer, optional, intent(out) :: removed

      class(avl_node), pointer :: left, right, dum

      if (ordering(key, node%key) .lt. 0) then
         if (associated(node%left)) then
            if (ordering(key, node%left%key) .eq. 0) then
               stat = .true.
               call avl_node_unlink(node%left, dum, ordering)
               if (.not.present(removed)) then
                  call destroy_avl_node(node%left)
               else
                  removed => node%left
               end if
               node%left => dum
            else
               call avl_node_remove(node%left, key, stat, ordering)
               if (stat) then
                  call avl_node_rebalance(node%left, dum, ordering)
                  if (associated(dum)) then
                     node%left => dum
                  end if
               end if
            end if
         else
            stat = .false.
         end if
      else if (ordering(key, node%key) .gt. 0) then
         if (associated(node%right)) then
            if (ordering(key, node%right%key) .eq. 0) then
               stat = .true.
               call avl_node_unlink(node%right, dum, ordering)
               if (.not.present(removed)) then
                  call destroy_avl_node(node%right)
               else
                  removed => node%right
               end if
               node%right => dum
            else
               call avl_node_remove(node%right, key, stat, ordering)
               if (stat) then
                  call avl_node_rebalance(node%right, dum, ordering)
                  if (associated(dum)) then
                     node%right => dum
                  end if
               end if
            end if
         else
            stat = .false.
         end if
      end if
   end subroutine

   subroutine avl_node_unlink(node, new_node, ordering)
      class(avl_node), intent(inout) :: node
      class(avl_node), pointer, intent(out) :: new_node
      procedure(c_ordering) :: ordering

      nullify(new_node)
      if (avl_node_isleaf(node)) then
         ! no children to unlink
      else if (avl_node_isinner(node)) then
         if (.not.associated(node%right%left)) then
            node%right%left => node%left
            new_node => node%right
            nullify(node%left)
            nullify(node%right)
         else
            call unlink_min_node(node%right, new_node, ordering)
            new_node%left => node%left
            new_node%right => node%right
            nullify(node%left)
            nullify(node%right)
         end if
      else if (associated(node%left)) then
         new_node => node%left
         nullify(node%left)
      else ! (associated(node%right))
         new_node => node%right
         nullify(node%right)
      end if
   contains
      recursive subroutine unlink_min_node(node, min, ordering)
         class(avl_node), intent(inout) :: node
         class(avl_node), pointer, intent(out) :: min
         class(avl_node), pointer :: dum
         procedure(c_ordering) :: ordering

         if (associated(node%left)) then
            if (.not.associated(node%left%left)) then
               min => node%left
               node%left => node%left%right
            else
               call unlink_min_node(node%left, min, ordering)
               call avl_node_rebalance(node%left, dum, ordering)
               if (associated(dum)) then
                  node%left => dum
                  nullify(dum)
               end if
            end if 
         end if
      end subroutine
   end subroutine

   subroutine avl_node_rebalance(node, mid, ordering)
      class(avl_node), pointer, intent(inout) :: node
      class(avl_node), pointer, intent(out) :: mid
      procedure(c_ordering) :: ordering

      class(avl_node), pointer :: child,grandchild,t1,t2,t3,t4,min,max
      integer :: balance, order_nc, order_cgc

      nullify(mid)
      if (.not.associated(node)) then
         return
      end if

      balance = avl_node_balance(node)
      if (abs(balance) .le. 1) then
         return
      end if

      if (balance .lt. 0) then
         child => node%right
         nullify(node%right)
      else
         child => node%left
         nullify(node%left)
      end if

      if (avl_node_balance(child) .gt. 0) then
         grandchild => child%left
         nullify(child%left)
      else
         grandchild => child%right
         nullify(child%right)
      end if

      order_nc = ordering(node%key, child%key)
      order_cgc = ordering(child%key, grandchild%key)
      ! right - right
      if (order_nc .lt. 0 .and. order_cgc .lt. 0) then
         t1 => node%left
         t2 => child%left
         t3 => grandchild%left
         t4 => grandchild%right

         min => node
         mid => child
         max => grandchild
      ! right - left
      else if (order_nc .lt. 0 .and. order_cgc .gt. 0) then
         t1 => node%left
         t2 => grandchild%left
         t3 => grandchild%right
         t4 => child%right

         min => node
         mid => grandchild
         max => child
      ! left - right
      else if (order_nc .gt. 0 .and. order_cgc .lt. 0) then
         t1 => child%left
         t2 => grandchild%left
         t3 => grandchild%right
         t4 => node%right

         min => child
         mid => grandchild
         max => node
      ! left - left
      else ! (order_nc .gt. 0 .and. order_cgc .gt. 0)
         t1 => grandchild%left
         t2 => grandchild%right
         t3 => child%right
         t4 => node%right

         min => grandchild
         mid => child
         max => node
      end if

      min%left => t1
      min%right => t2
      max%left => t3
      max%right => t4

      mid%left => min
      mid%right => max
   end subroutine

   pure function avl_node_balance(self)
      integer :: avl_node_balance
      class(avl_node), intent(in) :: self
      integer :: left_height, right_height
      left_height = 0
      if (associated(self%left)) then
         left_height = avl_node_height(self%left)
      end if 
      right_height = 0
      if (associated(self%right)) right_height = avl_node_height(self%right)
      avl_node_balance = left_height - right_height
   end function

   pure function avl_node_isleaf(self)
      logical :: avl_node_isleaf
      class(avl_node), intent(in) :: self
      avl_node_isleaf = .not.(associated(self%left).or.associated(self%right))
   end function

   pure function avl_node_isinner(self)
      logical :: avl_node_isinner
      class(avl_node), intent(in) :: self
      avl_node_isinner = associated(self%left).and.associated(self%right)
   end function

   pure recursive function avl_node_height(self) result(height)
      integer :: height
      class(avl_node), intent(in) :: self
      integer :: left_height, right_height

      left_height = 0
      if (associated(self%left)) then
         left_height = avl_node_height(self%left)
      end if
      right_height = 0
      if (associated(self%right)) then
         right_height = avl_node_height(self%right)
      end if
      height = max(left_height, right_height) + 1
   end function

   recursive function avl_node_min(self) result(min)
      class(avl_node), pointer :: min
      class(avl_node), target, intent(in) :: self
      if (associated(self%left)) then
         min => avl_node_min(self%left)
      else
         min => self
      end if
   end function

   recursive function avl_node_max(self) result(max)
      class(avl_node), pointer :: max
      class(avl_node), target, intent(in) :: self
      if (associated(self%right)) then
         max => avl_node_max(self%right)
      else
         max => self
      end if
   end function


   pure function avl_tree_empty(self)
      class(avl_tree), intent(in) :: self
      logical :: avl_tree_empty
      avl_tree_empty = .not.associated(self%head)
   end function

   subroutine avl_tree_find(self, key, stat, node)
      class(avl_tree), intent(in) :: self
      class(*), intent(in) :: key
      logical, intent(inout) :: stat
      class(avl_node), pointer, intent(out) :: node
      
      if (self%empty()) then
         stat = .false.
         nullify(node)
      end if

      call avl_node_find(self%head, key, stat, node, self%ordering)
   end subroutine

   subroutine avl_tree_add(self, key, val, stat)
      class(avl_tree), intent(inout) :: self
      class(*), intent(in) :: key, val
      logical, intent(inout) :: stat
      class(avl_node), pointer :: mid

      if (self%empty()) then
         stat = .true.
         allocate(self%head, source=avl_node(key, val))
      else
         call avl_node_add(self%head, key, val, stat, self%ordering)
         if (stat) then
            call avl_node_rebalance(self%head, mid, self%ordering)
            if (associated(mid)) then
               self%head => mid
            end if
         end if
      end if
   end subroutine

   !> Removes node with `key` in the tree `self`, reporting success in `stat`.
   !> If supplied, `removed` contains afterwards the removed node.
   subroutine avl_tree_remove(self, key, stat, removed)
      class(avl_tree), intent(inout) :: self
      class(*), intent(in) :: key
      logical, intent(out) :: stat
      class(avl_node), optional, pointer :: removed
      class(avl_node), pointer :: left, dum
      nullify(dum)

      if (self%empty()) then
         stat = .false.
      else if (self%ordering(self%head%key, key) .eq. 0) then
         stat = .true.
         call avl_node_unlink(self%head, dum, self%ordering)
         call destroy_avl_node(self%head)
         self%head => dum
         nullify(dum)

         call avl_node_rebalance(self%head, dum, self%ordering)
         if (associated(dum)) then
            self%head => dum
         end if
      else
         if (present(removed)) then
            call avl_node_remove(self%head, key, stat, self%ordering, removed)
         else
            call avl_node_remove(self%head, key, stat, self%ordering)
         end if
         if (stat) then
            call avl_node_rebalance(self%head, dum, self%ordering)
            if (associated(dum)) then
               self%head => dum
            end if
         end if
      end if
   end subroutine

   !> Reports the height of tree `self`
   pure function avl_tree_height(self)
      integer :: avl_tree_height
      class(avl_tree), intent(in) :: self
      if (self%empty()) then
         avl_tree_height = 0
      else
         avl_tree_height = avl_node_height(self%head)
      end if
   end function

   subroutine avl_tree_min(self, min, stat)
      class(avl_tree), intent(in) :: self
      class(avl_node), pointer, intent(out) :: min
      logical, intent(out) :: stat
      stat = .false.
      nullify(min)
      if (associated(self%head)) then
         min => avl_node_min(self%head)
         stat = .true.
      end if
   end subroutine

   subroutine avl_tree_max(self, max, stat)
      class(avl_tree), intent(in) :: self
      class(avl_node), pointer, intent(out) :: max
      logical, intent(out) :: stat
      stat = .false.
      nullify(max)
      if (associated(self%head)) then
         max => avl_node_max(self%head)
         stat = .true.
      end if
   end subroutine

   pure function new_avl_tree(ordering)
      type(avl_tree) :: new_avl_tree
      procedure(c_ordering) :: ordering
      nullify(new_avl_tree%head)
      new_avl_tree%ordering => ordering
   end function

   subroutine destroy_avl_tree(self)
      type(avl_tree) :: self
      if (associated(self%head)) then
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
