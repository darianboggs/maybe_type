module error_node_impl_mod

   use error_mod, only: Error
   use error_node_def_mod, only: ErrorNode

   implicit none

   public :: ErrorImpl

   type, extends(ErrorNode) :: ErrorNodeImpl
      private
      class(Error) :: err
      class(ErrorNode), pointer :: next
   contains
      procedure, public, pass(this) :: error => get_error_impl
      procedure, public, pass(this) :: set_next => set_next_impl
      procedure, public, pass(this) :: next => get_next_impl
   end type ErrorNodeImpl

   interface ErrorNodeImpl
      module procedure :: constructErrorNodeImpl
   end interface ErrorNodeImpl

contains

   function constructErrorNodeImpl(err) result(node)
      class(Error), intent(in) :: error
      type(ErrorNodeImpl) :: node 
      node % err = err
   end function constructErrorNodeImpl

   function get_error_impl(this) result(err)
      class(ErrorNodeImpl), intent(in) :: this
      class(Error) :: err
      err = this % err
   end function get_error_impl

   subroutine set_next_impl(this, next_ptr)
      class(ErrorNodeImpl), intent(in) :: this
      class(ErrorNode), pointer, intent(in) :: next_ptr
      this % next => next_ptr
   end subroutine set_next_impl

   function get_next_impl(this) result(ptr)
      class(ErrorNodeImpl), intent(in) :: this
      class(ErrorNode), pointer :: ptr
      ptr => this % next
   end function get_next_impl

end module error_node_impl_mod
