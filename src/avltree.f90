module dstruct_avltree
   use dstruct_concepts, only: c_ordering
   implicit none
   private
   public :: avltree_type
   public :: avlnode_type
   public :: iterator
   public :: avltree_preorder
   public :: avltree_inorder
   public :: avltree_postorder
   public :: avltree_levelorder

   type :: avlnode_type
      class(*), allocatable :: key
      class(*), allocatable :: val
      class(avlnode_type), pointer :: left, right
   end type
   interface avlnode_type
      module procedure :: new_avlnode
   end interface

   type :: avltree_type
      class(avlnode_type), pointer :: head
      procedure(c_ordering), nopass, pointer :: ordering => null()
   contains
      procedure :: empty => avltree_empty
      procedure :: find => avltree_find
      procedure :: add => avltree_add
      procedure :: remove => avltree_remove
      procedure :: height => avltree_height
      procedure :: min => avltree_min
      procedure :: max => avltree_max
      final :: destroy_avltree
   end type
   interface avltree_type
      module procedure :: new_avltree
   end interface

   type, abstract :: iterator
   contains
      procedure(next_interface), deferred :: next
      procedure(has_next_interface), deferred :: has_next
   end type
   abstract interface
      subroutine next_interface(self, node)
         import :: iterator, avlnode_type
         class(iterator), intent(inout) :: self
         class(avlnode_type), pointer, intent(out) :: node
      end subroutine
      pure function has_next_interface(self)
         import :: iterator
         logical :: has_next_interface
         class(iterator), intent(in) :: self
      end function
   end interface

   type, extends(iterator) :: avltree_preorder
   contains
      procedure :: next => avltree_preorder_next
      procedure :: has_next => avltree_preorder_has_next
   end type
   interface avltree_preorder
      module procedure :: new_avltree_preorder
   end interface

   type, extends(iterator) :: avltree_inorder
   contains
      procedure :: next => avltree_inorder_next
      procedure :: has_next => avltree_inorder_has_next
   end type
   interface avltree_inorder
      module procedure :: new_avltree_inorder
   end interface

   type, extends(iterator) :: avltree_postorder
   contains
      procedure :: next => avltree_postorder_next
      procedure :: has_next => avltree_postorder_has_next
   end type
   interface avltree_postorder
      module procedure :: new_avltree_postorder
   end interface

   type, extends(iterator) :: avltree_levelorder
   contains
      procedure :: next => avltree_levelorder_next
      procedure :: has_next => avltree_levelorder_has_next
   end type
   interface avltree_levelorder
      module procedure :: new_avltree_inorder
   end interface

contains

   function new_avlnode(key, val, left, right)
      type(avlnode_type) :: new_avlnode
      class(*), intent(in) :: key
      class(*), intent(in) :: val
      type(avlnode_type), pointer, intent(in), optional :: left, right

      new_avlnode%key = key
      new_avlnode%val = val
      if (present(left)) then
         new_avlnode%left => left
      else
         nullify(new_avlnode%left)
      end if
      if (present(right)) then
         new_avlnode%right => right
      else
         nullify(new_avlnode%right)
      end if
   end function

   recursive subroutine destroy_avlnode(node)
      class(avlnode_type), pointer, intent(inout) :: node
      integer :: stat
      character(len=512) :: err_msg
      if (associated(node%left)) then
         call destroy_avlnode(node%left)
      end if
      if (associated(node%right)) then
         call destroy_avlnode(node%right)
      end if
      deallocate(node, stat=stat, errmsg=err_msg)
      if (stat /= 0) then
         print*,err_msg
      end if 
   end subroutine

   recursive subroutine avlnode_find(self, key, stat, node, ordering)
      class(avlnode_type), target, intent(in) :: self
      class(*), intent(in) :: key
      logical, intent(out) :: stat
      class(avlnode_type), pointer, intent(inout) :: node
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
            call avlnode_find(self%left, key, stat, node, ordering)
         end if
      else
         if (associated(self%right)) then
            call avlnode_find(self%right, key, stat, node, ordering)
         end if
      end if
   end subroutine

   recursive subroutine avlnode_add(self, key, val, stat, ordering)
      class(avlnode_type), intent(inout) :: self
      class(*), intent(in) :: key, val
      logical, intent(out) :: stat
      procedure(c_ordering) :: ordering

      class(avlnode_type), pointer :: mid
      integer :: order

      order = ordering(key, self%key)
      if (order .eq. 0) then
         stat = .false.
      else if (order .lt. 0) then
         if (associated(self%left)) then
            call avlnode_add(self%left, key, val, stat, ordering)
            if (stat) then
               call avlnode_rebalance(self%left, mid, ordering)
               if (associated(mid)) then
                  self%left => mid
               end if
            end if
         else
            allocate(self%left, source=avlnode_type(key, val))
            stat = .true.
         end if
      else ! (order .gt. 0)
         if (associated(self%right)) then
            call avlnode_add(self%right, key, val, stat, ordering)
            if (stat) then
               call avlnode_rebalance(self%right, mid, ordering)
               if (associated(mid)) then
                  self%right => mid
               end if
            end if
         else
            allocate(self%right, source=avlnode_type(key, val))
            stat = .true.
         end if
      end if
   end subroutine

   recursive subroutine avlnode_remove(node, key, stat, ordering, removed)
      class(avlnode_type), intent(inout) :: node
      class(*), intent(in) :: key
      logical, intent(out) :: stat
      procedure(c_ordering) :: ordering
      class(avlnode_type), pointer, optional, intent(out) :: removed

      class(avlnode_type), pointer :: left, right, dum

      if (ordering(key, node%key) .lt. 0) then
         if (associated(node%left)) then
            if (ordering(key, node%left%key) .eq. 0) then
               stat = .true.
               call avlnode_unlink(node%left, dum, ordering)
               if (.not.present(removed)) then
                  deallocate(node%left)
               else
                  removed => node%left
               end if
               node%left => dum
            else
               call avlnode_remove(node%left, key, stat, ordering)
               if (stat) then
                  call avlnode_rebalance(node%left, dum, ordering)
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
               call avlnode_unlink(node%right, dum, ordering)
               if (.not.present(removed)) then
                  deallocate(node%right)
               else
                  removed => node%right
               end if
               node%right => dum
            else
               call avlnode_remove(node%right, key, stat, ordering)
               if (stat) then
                  call avlnode_rebalance(node%right, dum, ordering)
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

   subroutine avlnode_unlink(node, new_node, ordering)
      class(avlnode_type), intent(inout) :: node
      class(avlnode_type), pointer, intent(out) :: new_node
      procedure(c_ordering) :: ordering

      nullify(new_node)
      if (avlnode_isleaf(node)) then
         ! no children to unlink
      else if (avlnode_isinner(node)) then
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
         class(avlnode_type), intent(inout) :: node
         class(avlnode_type), pointer, intent(out) :: min
         class(avlnode_type), pointer :: dum
         procedure(c_ordering) :: ordering

         if (associated(node%left)) then
            if (.not.associated(node%left%left)) then
               min => node%left
               node%left => node%left%right
            else
               call unlink_min_node(node%left, min, ordering)
               call avlnode_rebalance(node%left, dum, ordering)
               if (associated(dum)) then
                  node%left => dum
                  nullify(dum)
               end if
            end if 
         end if
      end subroutine
   end subroutine

   subroutine avlnode_rebalance(node, mid, ordering)
      class(avlnode_type), pointer, intent(inout) :: node
      class(avlnode_type), pointer, intent(out) :: mid
      procedure(c_ordering) :: ordering

      class(avlnode_type), pointer :: child,grandchild,t1,t2,t3,t4,min,max
      integer :: balance, order_nc, order_cgc

      nullify(mid)
      if (.not.associated(node)) then
         return
      end if

      balance = avlnode_balance(node)
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

      if (avlnode_balance(child) .gt. 0) then
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

   pure function avlnode_balance(self)
      integer :: avlnode_balance
      class(avlnode_type), intent(in) :: self
      integer :: left_height, right_height
      left_height = 0
      if (associated(self%left)) then
         left_height = avlnode_height(self%left)
      end if 
      right_height = 0
      if (associated(self%right)) right_height = avlnode_height(self%right)
      avlnode_balance = left_height - right_height
   end function

   pure function avlnode_isleaf(self)
      logical :: avlnode_isleaf
      class(avlnode_type), intent(in) :: self
      avlnode_isleaf = .not.(associated(self%left).or.associated(self%right))
   end function

   pure function avlnode_isinner(self)
      logical :: avlnode_isinner
      class(avlnode_type), intent(in) :: self
      avlnode_isinner = associated(self%left).and.associated(self%right)
   end function

   pure recursive function avlnode_height(self) result(height)
      integer :: height
      class(avlnode_type), intent(in) :: self
      integer :: left_height, right_height

      left_height = 0
      if (associated(self%left)) then
         left_height = avlnode_height(self%left)
      end if
      right_height = 0
      if (associated(self%right)) then
         right_height = avlnode_height(self%right)
      end if
      height = max(left_height, right_height) + 1
   end function

   recursive function avlnode_min(self) result(min)
      class(avlnode_type), pointer :: min
      class(avlnode_type), target, intent(in) :: self
      if (associated(self%left)) then
         min => avlnode_min(self%left)
      else
         min => self
      end if
   end function

   recursive function avlnode_max(self) result(max)
      class(avlnode_type), pointer :: max
      class(avlnode_type), target, intent(in) :: self
      if (associated(self%right)) then
         max => avlnode_max(self%right)
      else
         max => self
      end if
   end function


   pure function avltree_empty(self)
      class(avltree_type), intent(in) :: self
      logical :: avltree_empty
      avltree_empty = .not.associated(self%head)
   end function

   subroutine avltree_find(self, key, stat, node)
      class(avltree_type), intent(in) :: self
      class(*), intent(in) :: key
      logical, intent(inout) :: stat
      class(avlnode_type), pointer, intent(out) :: node
      
      if (self%empty()) then
         stat = .false.
         nullify(node)
      end if

      call avlnode_find(self%head, key, stat, node, self%ordering)
   end subroutine

   subroutine avltree_add(self, key, val, stat)
      class(avltree_type), intent(inout) :: self
      class(*), intent(in) :: key, val
      logical, intent(inout) :: stat
      class(avlnode_type), pointer :: mid

      if (self%empty()) then
         stat = .true.
         allocate(self%head, source=avlnode_type(key, val))
      else
         call avlnode_add(self%head, key, val, stat, self%ordering)
         if (stat) then
            call avlnode_rebalance(self%head, mid, self%ordering)
            if (associated(mid)) then
               self%head => mid
            end if
         end if
      end if
   end subroutine

   !> Removes node with `key` in the tree `self`, reporting success in `stat`.
   !> If supplied, `removed` contains afterwards the removed node.
   subroutine avltree_remove(self, key, stat, removed)
      class(avltree_type), intent(inout) :: self
      class(*), intent(in) :: key
      logical, intent(out) :: stat
      class(avlnode_type), optional, pointer :: removed
      class(avlnode_type), pointer :: left, dum
      nullify(dum)

      if (self%empty()) then
         stat = .false.
      else if (self%ordering(self%head%key, key) .eq. 0) then
         stat = .true.
         call avlnode_unlink(self%head, dum, self%ordering)
         deallocate(self%head)
         self%head => dum
         nullify(dum)

         call avlnode_rebalance(self%head, dum, self%ordering)
         if (associated(dum)) then
            self%head => dum
         end if
      else
         if (present(removed)) then
            call avlnode_remove(self%head, key, stat, self%ordering, removed)
         else
            call avlnode_remove(self%head, key, stat, self%ordering)
         end if
         if (stat) then
            call avlnode_rebalance(self%head, dum, self%ordering)
            if (associated(dum)) then
               self%head => dum
            end if
         end if
      end if
   end subroutine

   !> Reports the height of tree `self`
   pure function avltree_height(self)
      integer :: avltree_height
      class(avltree_type), intent(in) :: self
      if (self%empty()) then
         avltree_height = 0
      else
         avltree_height = avlnode_height(self%head)
      end if
   end function

   subroutine avltree_min(self, min, stat)
      class(avltree_type), intent(in) :: self
      class(avlnode_type), pointer, intent(out) :: min
      logical, intent(out) :: stat
      stat = .false.
      nullify(min)
      if (associated(self%head)) then
         min => avlnode_min(self%head)
         stat = .true.
      end if
   end subroutine

   subroutine avltree_max(self, max, stat)
      class(avltree_type), intent(in) :: self
      class(avlnode_type), pointer, intent(out) :: max
      logical, intent(out) :: stat
      stat = .false.
      nullify(max)
      if (associated(self%head)) then
         max => avlnode_max(self%head)
         stat = .true.
      end if
   end subroutine

   pure function new_avltree(ordering)
      type(avltree_type) :: new_avltree
      procedure(c_ordering) :: ordering
      nullify(new_avltree%head)
      new_avltree%ordering => ordering
   end function

   subroutine destroy_avltree(self)
      type(avltree_type) :: self
      if (associated(self%head)) then
         deallocate(self%head)
      end if
   end subroutine


   pure function new_avltree_preorder(tree)
      type(avltree_preorder) :: new_avltree_preorder
      class(avltree_type), intent(in) :: tree
      ! TODO
   end function
   
   subroutine avltree_preorder_next(self, node)
      class(avltree_preorder), intent(inout) :: self
      class(avlnode_type), pointer, intent(out) :: node
      nullify(node)
      ! TODO
   end subroutine
   
   pure function avltree_preorder_has_next(self)
      logical :: avltree_preorder_has_next
      class(avltree_preorder), intent(in) :: self
      ! TODO
   end function

   pure function new_avltree_inorder(tree)
      type(avltree_inorder) :: new_avltree_inorder
      class(avltree_type), intent(in) :: tree
      ! TODO
   end function
   
   subroutine avltree_inorder_next(self, node)
      class(avltree_inorder), intent(inout) :: self
      class(avlnode_type), pointer, intent(out) :: node
      nullify(node)
      ! TODO
   end subroutine
   
   pure function avltree_inorder_has_next(self)
      logical :: avltree_inorder_has_next
      class(avltree_inorder), intent(in) :: self
      ! TODO
   end function

   
   
   pure function new_avltree_postorder(tree)
      type(avltree_preorder) :: new_avltree_postorder
      class(avltree_type), intent(in) :: tree
      ! TODO
   end function

   subroutine avltree_postorder_next(self, node)
      class(avltree_postorder), intent(inout) :: self
      class(avlnode_type), pointer, intent(out) :: node
      ! TODO
   end subroutine

   pure function avltree_postorder_has_next(self)
      logical :: avltree_postorder_has_next
      class(avltree_postorder), intent(in) :: self
      ! TODO
   end function

   
   
   pure function new_avltree_levelorder(tree)
      type(avltree_levelorder) :: new_avltree_levelorder
      class(avltree_type), intent(in) :: tree
      ! TODO
   end function

   subroutine avltree_levelorder_next(self, node)
      class(avltree_levelorder), intent(inout) :: self
      class(avlnode_type), pointer, intent(out) :: node
      ! TODO
   end subroutine

   pure function avltree_levelorder_has_next(self)
      logical :: avltree_levelorder_has_next
      class(avltree_levelorder), intent(in) :: self
      ! TODO
   end function
end module
