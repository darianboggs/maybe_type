module error_stack_mod

   use error_mod
   use error_node_mod

   implicit none

   integer, parameter :: SUCCESS = 0
   integer, parameter :: FAIL = SUCCESS - 1

   type, abstract :: ErrorStack
      private
   contains
      procedure, public, pass(this) :: empty
      procedure(GetInteger), public, deferred, pass(this) :: size
      procedure(GetError), public, deferred, pass(this) :: top
      procedure(PushError), public, deferred, pass(this) :: push
      procedure(PopError), public, deferred, pass(this) :: pop
   end type ErrorStack

   abstract interface

      integer function GetInteger(this)
         class(ErrorStack), intent(in) :: this
      end function GetInteger

      function GetError(this) result(err)
         class(ErrorStack), intent(in) :: this
         class(Error) :: err
      end function GetError

      subroutine PushError(this, err)
         class(ErrorStack), intent(in) :: this
         class(Error), intent(in) :: err
      end subroutine PushError

      subroutine PopError(this) result(err)
         class(ErrorStack), intent(in) :: this
         class(Error) :: err
      end subroutine PopError

   end abstract interface

   type, extends(ErrorStack) :: ErrorStackImpl
      private
      class(ErrorNode), ptr :: head
      integer :: sz
   contains
      procedure, public, pass(this) :: size  => size_error_stack_impl
      procedure, public, pass(this) :: top   => top_error_stack_impl
      procedure, public, pass(this) :: push  => push_error_stack_impl
      procedure, public, pass(this) :: pop   => pop_error_stack_impl
      procedure, private, pass(this) :: set_size
      procedure, private, pass(this) :: change_size
      procedure, private, pass(this) :: pluck_node
   end type ErrorStackImpl

   interface ErrorStack
      module procedure :: constructErrorStack
   end interface ErrorStack

contains

   logical function empty(this)
      class(ErrorStack), intent(in) :: this
      empty = (this % size() == 0)
   end function empty

   subroutine set_size(this, sz)
      type(ErrorStackImpl), intent(in) :: this
      integer, intent(inout) :: sz

      if(sz < 0) return

      this % sz = sz

   end subroutine set_size

   subroutine change_size(this, change, new_size)
      type(ErrorStackImpl), intent(in) :: this
      integer, optional, intent(in) :: change
      integer, optional, intent(out) :: new_size
      integer :: sz
      integer :: status

      if(present(change)) then
         sz = this % size() + change
      else
         sz = this % size() + 1
      end if

      if(present(new_size)) new_size = sz

      if(sz < 0) return

      call this % set_size(sz) 
      
      if(present(new_size)) new_size = sz 

   end subroutine change_size

   function constructErrorStack() result(error_stack)
      type(ErrorStackImpl) :: error_stack
      nullify(error_stack % head)
      call error_stack % set_size(0)
   end function constructErrorStack

   function size_error_stack_impl(this) result(sz)
      type(ErrorStackImpl), intent(in) :: this
      integer :: sz
      sz = this % sz
   end function size_error_stack_impl

   function top_error_stack_impl(this) result(top)
      type(ErrorStackImpl), intent(in) :: this
      class(Error) :: top
      type(ErrorNode), ptr :: head

      head => this % head

      if(associated(this % head))  then
         top = head % get_error()
      else
         top = NoError()
      end if

   end function top_error_stack_impl

   subroutine push_error_stack_impl(this, err, sz)
      type(ErrorStackImpl), intent(in) :: this
      class(Error), intent(in) :: err
      integer, optional, intent(out) :: sz
      class(ErrorNode), pointer :: new_node
      integer :: new_size_

      new_node = ErrorNode(err)
      call new_node % set_next(this % head)
      this % head => new_node
      call this % change_size(new_size = new_size_)
      if(present(sz)) sz = new_size_

   end subroutine push_error_stack_impl

   subroutine pop_error_stack_impl(this, err, sz)
      type(ErrorStackImpl), intent(in) :: this
      class(Error), intent(out) :: err
      integer, optional, intent(out) :: sz
      err = this % top()
      if(err % is_error()) call this % pluck_node()
      if(present(sz)) sz = this % size()
   end subroutine pop_error_stack_impl

   subroutine pluck_node(this)
      type(ErrorStackImpl), intent(in) :: this
      class(ErrorNode), pointer :: head

      if(this % empty()) return

      head => this % head
      this % head => head % next() 
      call this % change_size(change = -1) 
   end subroutine pluck_node

end module error_stack_mod
