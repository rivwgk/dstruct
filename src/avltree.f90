module AVLTree
   implicit none
   private
   public :: avl_tree, avl_node, destroy_avl_tree, iterator, &
      & avl_tree_preorder, avl_tree_inorder, avl_tree_postorder, &
      & avl_tree_levelorder

   type :: avl_node
      character(len=:), allocatable :: key
      integer :: val
      class(avl_node), pointer :: left, right, parent
   contains
      !> Θ(log n)
      procedure, private :: find => avl_node_find
      !> O(log n)
      procedure, private :: add => avl_node_add
      !> O(log n)
      !procedure, private :: remove => avl_node_remove
      !> Θ(log n)
      !procedure, private :: rebalance => avl_node_rebalance
      !> Θ(log n)
      procedure, private :: balance => avl_node_balance
      !> Θ(log n)
      procedure, private :: height => avl_node_height
   end type
   interface avl_node
      module procedure :: new_avl_node
   end interface

   type :: avl_tree
      class(avl_node), pointer :: head
   contains
      !> Θ(1)
      procedure :: empty => avl_tree_empty
      !> Θ(log n)
      procedure :: find => avl_tree_find
      !> O(log n)
      procedure :: add => avl_tree_add
      !> O(log n)
      procedure :: remove => avl_tree_remove
      !> Θ(log n)
      procedure :: height => avl_tree_height
   end type
   interface avl_tree
      module procedure :: new_avl_tree
   end interface

   type, abstract :: iterator
   contains
      procedure(next_interface),deferred :: next
      procedure(has_next_interface),deferred :: has_next
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

   function new_avl_node(key, val, parent)
      type(avl_node) :: new_avl_node
      character(len=*), intent(in) :: key
      integer, intent(in) :: val
      type(avl_node), target, optional, intent(in) :: parent

      new_avl_node%key = key
      new_avl_node%val = val
      nullify(new_avl_node%left)
      nullify(new_avl_node%right)
      if (present(parent)) then
         new_avl_node%parent => parent
      else
         nullify(new_avl_node%parent)
      end if
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
         if (associated(self%left)) then
            call self%left%find(key, stat, node)
         end if
      else
         if (associated(self%right)) then
            call self%right%find(key, stat, node)
         end if
      end if
   end subroutine

   recursive subroutine avl_node_add(self, key, val, stat)
      class(avl_node), intent(inout) :: self
      character(len=*), intent(in) :: key
      integer, intent(in) :: val
      logical, intent(out) :: stat

      if (self%key .eq. key) then
         stat = .false.
      else if (key .lt. self%key) then
         if (associated(self%left)) then
            call self%left%add(key, val, stat)
         else
            allocate(self%left, source=avl_node(key, val, self))
            stat = .true.
            call avl_node_rebalance(self%left)
         end if
      else
         if (associated(self%right)) then
            call self%right%add(key, val, stat)
         else
            allocate(self%right, source=avl_node(key, val, self))
            stat = .true.
            call avl_node_rebalance(self%right)
         end if
      end if
   end subroutine

   recursive subroutine avl_node_remove(self, key, stat)
      class(avl_node), pointer, intent(inout) :: self
      character(len=*), intent(in) :: key
      logical, intent(out) :: stat
      class(avl_node), pointer :: left, right, dum

      if (self%key .eq. key) then

         stat = .true.
         if (associated(self%left) .and. associated(self%right)) then
            ! both children, replace self with left child and add right child
            ! left of the left-most child of the parent's right child

            ! unlink children
            left => self%left
            right => self%right
            nullify(self%left)
            nullify(self%right)

            ! replace self in the parent with the left child
            if (self%key .eq. self%parent%left%key) then
               self%parent%left => left
            else ! (self%key .eq. self%parent%right%key) then
               self%parent%right => left
            end if
            left%parent => self%parent

            ! add right child
            if (associated(self%parent%right)) then
               ! find left-most node in parent's right child
               dum => self%parent
               do 
                  if (.not.associated(dum%left)) then
                     exit
                  end if
                  dum => dum%left
               end do
               dum%left => right
               right%parent => dum
            else
               self%parent%right => right
               right%parent => self%parent
            end if
            call avl_node_rebalance(dum)

         else if (associated(self%left)) then
            ! has only left child, replace self with it

            left => self%left
            nullify(self%left)
            if (self%key .eq. self%parent%left%key) then
               self%parent%left => left
            else ! (self%key .eq. self%parent%right%key) then
               self%parent%right => left
            end if
            left%parent => self%parent
            call avl_node_rebalance(self%parent)

         else if (associated(self%right)) then
            ! has only right child, replace self with it

            right => self%right
            nullify(self%right)
            if (self%key .eq. self%parent%left%key) then
               self%parent%left => right
            else ! (self%key .eq. self%parent%right%key) then
               self%parent%right => right
            end if
            right%parent => self%parent
            call avl_node_rebalance(self%parent)

         else
            ! no children

            if (self%key .eq. self%parent%left%key) then
               nullify(self%parent%left)
            else ! (self%key .eq. self%parent%right%key)
               nullify(self%parent%right)
            end if
            call avl_node_rebalance(self%parent)

         end if
         nullify(self%parent)
         call destroy_avl_node(self)
         
      else if (key .lt. self%key) then
         if (associated(self%left)) then
            call avl_node_remove(self%left, key, stat)
         else
            stat = .false.
         end if
      else ! (key .gt. self%key)
         if (associated(self%right)) then
            call avl_node_remove(self%right, key, stat)
         else
            stat = .false.
         end if
      end if
   end subroutine

   recursive subroutine avl_node_rebalance(self)
      class(avl_node), pointer :: self
      class(avl_node), pointer :: parent, greatparent
      class(avl_node), pointer :: t1, t2, t3, t4, min, max, mid

      nullify(parent)
      nullify(greatparent)
      if (associated(self%parent)) then
         parent => self%parent
         if (associated(parent%parent)) then
            greatparent => parent%parent
         end if
      end if

      if (.not.associated(parent).or..not.associated(greatparent)) then
         return
      end if
      if (abs(greatparent%balance()).le.1) then
         return
      end if

      if (self%key.lt.parent%key.and.parent%key.lt.greatparent%key) then
         min => self
         mid => parent
         max => greatparent
         t1 => self%left
         t2 => self%right
         t3 => parent%right ! self is left child
         t4 => greatparent%right ! parent is left child
      else if (self%key.gt.parent%key.and.parent%key.lt.greatparent%key) then
         min => parent
         mid => self
         max => greatparent
         t1 => parent%left ! self is right child
         t2 => self%left
         t3 => self%right
         t4 => greatparent%right ! parent is left child
      else if (self%key.lt.parent%key.and.parent%key.gt.greatparent%key) then
         min => greatparent
         mid => self
         max => parent
         t1 => greatparent%left ! parent is right child
         t2 => self%left
         t3 => self%right
         t4 => parent%right ! self is left child
      else ! (self%key.gt.parent%key.and.parent%key.gt.greatparent%key) then
         min => greatparent
         mid => parent
         max => self
         t1 => greatparent%left ! parent is right child
         t2 => parent%left ! self is right child
         t3 => self%left
         t4 => self%right
      end if

      if (associated(greatparent%parent)) then
         if (greatparent%parent%left%key .eq. greatparent%key) then
            greatparent%parent%left => mid
         else ! (greatparent%parent%right%key .eq. greatparent%key) then
            greatparent%parent%right => mid
         end if
      end if
      mid%parent => greatparent%parent
      mid%left => min
      mid%right => max

      min%parent => mid
      nullify(min%left)
      if (associated(t1)) then
         min%left => t1
      end if
      nullify(min%right)
      if (associated(t2)) then
         min%right => t2
      end if

      max%parent => mid
      nullify(max%left)
      if (associated(t3)) then
         max%left => t3
      end if
      nullify(max%right)
      if (associated(t4)) then
         max%right => t4
      end if

      call avl_node_rebalance(mid)
   end subroutine

   pure function avl_node_balance(self)
      integer :: avl_node_balance
      class(avl_node), intent(in) :: self
      integer :: left_height, right_height
      left_height = 0
      if (associated(self%left)) then
         left_height = self%left%height()
      end if 
      right_height = 0
      if (associated(self%right)) then
         right_height = self%right%height()
      end if
      avl_node_balance = left_height - right_height
   end function

   pure recursive function avl_node_height(self)
      integer :: avl_node_height
      class(avl_node), intent(in) :: self
      integer :: left_height, right_height

      left_height = 0
      if (associated(self%left)) then
         left_height = self%left%height()
      end if
      right_height = 0
      if (associated(self%right)) then
         right_height = self%right%height()
      end if
      avl_node_height = max(left_height, right_height) + 1
   end function



   pure function avl_tree_empty(self)
      class(avl_tree), intent(in) :: self
      logical :: avl_tree_empty
      avl_tree_empty = .not.associated(self%head)
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

      if (self%empty()) then
         stat = .true.
         allocate(self%head, source=avl_node(key, val))
      else
         call self%head%add(key, val, stat)
         do
            if (.not.associated(self%head%parent)) then
               exit
            else
               self%head => self%head%parent
            end if
         end do
      end if
   end subroutine

   subroutine avl_tree_remove(self, key, stat)
      class(avl_tree), intent(inout) :: self
      character(len=*), intent(in) :: key
      logical, intent(inout) :: stat
      class(avl_node), pointer :: dum, left, right

      if (self%empty()) then
         stat = .false.
      else if (self%head%key .eq. key) then

         left => self%head%left
         right => self%head%right
         if (associated(left) .and. associated(right)) then
            nullify(left%parent)
            nullify(self%head%left)
            nullify(self%head%right)
            self%head => left

            if (associated(self%head%right)) then
               dum => self%head%right
               do 
                  if (.not.associated(dum%right)) then
                     exit
                  end if
                  dum => dum%left
               end do
               dum%right => right
               right%parent => dum
            else
               self%head%right => right
            end if

         else if (associated(left)) then
            nullify(left%parent)
            nullify(self%head%left)
            deallocate(self%head)
            self%head => left
         else if (associated(right)) then
            nullify(right%parent)
            nullify(self%head%right)
            deallocate(self%head)
            self%head => right
         else
            deallocate(self%head)
            nullify(self%head)
         end if
      else
         call avl_node_remove(self%head, key, stat)
         do
            if (.not.associated(self%head%parent)) then
               exit
            else
               self%head => self%head%parent
            end if
         end do
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

   pure function new_avl_tree()
      type(avl_tree) :: new_avl_tree
      nullify(new_avl_tree%head)
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
      nullify(node)
   end subroutine

   pure function avl_tree_postorder_has_next(self)
      logical :: avl_tree_postorder_has_next
      class(avl_tree_postorder), intent(in) :: self
   end function

   
   
   pure function new_avl_tree_levelorder(tree)
      type(avl_tree_levelorder) :: new_avl_tree_levelorder
      class(avl_tree), intent(in) :: tree

   end function

   subroutine avl_tree_levelorder_next(self, node)
      class(avl_tree_levelorder), intent(inout) :: self
      class(avl_node), pointer, intent(out) :: node
   end subroutine

   pure function avl_tree_levelorder_has_next(self)
      logical :: avl_tree_levelorder_has_next
      class(avl_tree_levelorder), intent(in) :: self
   end function
end module
