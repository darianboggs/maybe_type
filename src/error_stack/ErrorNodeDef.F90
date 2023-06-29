module error_node_def_mod

   use error_mod, only: Error

   implicit none

   public :: ErrorNode

   private

   type, abstract :: ErrorNode
      private
   contains
      procedure, deferred, public, pass(this) :: error
      procedure, deferred, public, pass(this) :: set_next
      procedure, deferred, public, pass(this) :: next
   end type ErrorNode

end module error_node_def_mod
