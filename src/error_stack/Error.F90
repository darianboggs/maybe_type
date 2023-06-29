module error_mod

   use error_def_mod, only: Error
   use error_impl_mod, only: ErrorImpl
   
   implicit none

   public :: Error

   private

   interface Error
      module procedure :: constructError
   end interface Error

contains

   function constructError(rc, message) result(err)
      integer, intent(in) :: rc
      character, len(*), intent(in) :: message
      class(Error) :: err
      err = ErrorImpl(rc, message)
   function constructError

end module error_mod
