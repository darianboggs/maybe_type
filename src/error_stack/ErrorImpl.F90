module error_impl_mod

   use error_def_mod, only: Error

   implicit none
   
   public :: ErrorImpl

   private
  
   type, extends(Error) :: ErrorImpl
      private
      integer :: rc
      character(len=:), allocatable :: message
   contains
      procedure, public, pass(this) :: get_rc => get_rc_impl
      procedure, public, pass(this) :: get_message => get_message_impl
   end type ErrorImpl

   interface ErrorImpl
      module procedure :: construct_error_impl
   end interface ErrorImpl

contains

   function construct_error_impl(rc, message) result(err)
      integer, intent(in) :: rc
      character(len=*), intent(in) :: message
      type(ErrorImpl) :: err
      err % rc = rc
      err % message = message
      err % is_error_ = .TRUE.
   end function construct_error_impl

   integer function get_rc_impl(this)
      class(ErrorImpl), intent(in) :: this
      get_rc_impl = this % rc
   end function get_rc_impl

   function get_message_impl(this) result(message)
      class(ErrorImpl), intent(in) :: this
      character(len=:), allocatable :: message
   end function get_message_impl

end module error_impl_mod
