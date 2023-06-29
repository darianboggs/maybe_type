module _TYPE
   
   use error_stack_module

   implicit none

   type :: _TYPE 
      private
      type(T) :: value_
      type(ErrorStack) :: errors_
      logical :: valid_
   contains
      procedure, public, pass(this) :: get_value
      procedure, public, pass(this) :: is_valid
      procedure, public, pass(this) :: set_invalid
      procedure, public, pass(this) :: get_errors
      procedure, public, pass(this) :: add_error
   end type _TYPE

   interface _TYPE 
      module procedure :: construct_TYPE 
   end interface _TYPE 

contains

   function construct_TYPE(val) result(this)
      type(T), intent(in) :: val
      type(_TYPE) :: this
      this % value_ = val
      this % valid_ = .TRUE.
   end function construct_TYPE

   function get_value(this) result(val)
      class(_TYPE), intent(in) :: this
      type(T) :: val
      val = this % value_
   end function get_value

   function get_errors(this) result(errors)
      class(_TYPE), intent(in) :: this
      type(ErrorStack) :: err
      errors = this % errors_
   end function get_errors

   function is_valid(this) result(lval)
      class(_TYPE), intent(in) :: this
      logical :: lval
      lval = this % valid_
   end function is_valid

   subroutine set_value(this, val)
      class(_TYPE), intent(in) :: this
      type(T), intent(in) :: val
      this % value_ = val
   end subroutine get_value

   subroutine add_error(this, err)
      class(_TYPE), intent(inout) :: this
      type(Error) :: err
      type(ErrorStack) :: this_stack

      this_stack = this % get_errors()
      call this_stack % push(err)
   end subroutine add_error

   subroutine set_invalid(this, rc, message, err)
      class(_TYPE), intent(inout) :: this
      this % valid_ = .FALSE.
      call this % add_error(Error(rc, message))
   end subroutine set_invalid

end module _TYPE

#ifdef T
#  undef T
#endif

#ifdef _TYPE
#  undef _TYPE
#endif
