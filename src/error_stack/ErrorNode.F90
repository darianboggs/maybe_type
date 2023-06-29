module error_node_mod

   use error_node_def_mod, only: ErrorNode
   use error_mode_impl_mod, only: ErrorNodeImpl

   implicit none

   public :: ErrorNode

   private

   interface ErrorNode
      module procedure :: constructErrorNode
   end interface ErrorNode

contains

   function constructErrorNode(err) result(node)
      class(Error), intent(in) :: error
      class(ErrorNode) :: node 
      node = ErrorNodeImpl(err)
   end function constructErrorNode

end module error_node_mod
