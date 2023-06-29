module no_error_mod

   use error_def_mod, only: Error

   implicit none

   type, extends(Error) :: NoError
   contains
      procedure, public, pass(this) :: get_rc
      procedure, public, pass(this) :: get_message
   end type NoError

contains

   function construct_no_error() result(err)
      type(NoError) :: err
      err % is_error_ = .FALSE.
   end function construct_no_error

   integer function get_rc(this)
      class(NoError), intent(in) :: this
      get_rc = 0
   end function get_rc

   function get_message(this) result(message)
      class(NoError), intent(in) :: this
      character(len=:), allocatable :: message
      message = 'NO ERROR'
   end function get_message

end module no_error_mod
