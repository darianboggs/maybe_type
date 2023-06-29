module error_def_mod

   implicit none

   public :: Error
   private

   type, abstract :: Error
      private
      logical :: is_error_
   contains
      procedure, deferred, public, pass(this) :: get_rc
      procedure, deferred, public, pass(this) :: get_message
      procedure, public, pass(this) :: is_error
   end type Error

contains
   
   logical function is_error(this)
      class(Error), intent(in) :: this
         is_error = this % is_error_
   end function is_error

end module error_def_mod
