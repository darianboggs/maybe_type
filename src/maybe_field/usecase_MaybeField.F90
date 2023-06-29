! use case MaybeField
! MaybeField is a Maybe derived type wrapping ESMF_Field.
! Given an operation on ESMF_Field (addition in this case),
! use MaybeField for a functional approach.

! ========
! | From |
! ========
! integer :: status
! type(ESMF_Field) :: x, y, z, u, w
!
! call add_esmf_field(x, y, u, rc = status)
! if(status /=) then
!   !handle status error
! end if
! call add_esmf_field(u, z, w, rc = status)
! if(status /=) then
!   !handle status error
! end if

! ========
! |  To  |
! ========
! type(Maybe_Field) :: mx, my, mz, mw
! class(ErrorStack) :: error_stack
!
! mw = mx + my + mz
! if(.not. mw % is_valid()) then
!   error_stack = mw % get_errors()
!   !handle error_stack
! endif

module maybe_field_usecase_mod

    use MaybeField
    use ESMF

    implicit none

    interface operator(+)
       module procedure :: add_maybe_field
    end interface operator

contains

    function compatible(x, y) result(lval)
        type(ESMF_Field), intent(in) :: x
        type(ESMF_Field), intent(in) :: y
        logical :: lval
        !! lval = check x, y compatible 
    end function compatible

    subroutine add_esmf_field(x,y,z,rc) 
        type(ESMF_Field), intent(in) :: x
        type(ESMF_Field), intent(in) :: y
        type(ESMF_Field), intent(out) :: z
        integer, optional, intent(out) :: rc 

        if(compatible(x,y)) then
           !! z = x + y
           if(present(rc)) rc = 0
        else
           if(present(rc)) rc = -1
        end if

    end subroutine add_esmf_field

    function add_maybe_field(x,y) result(z)
        type(MaybeField), intent(in) :: x, y
        type(MaybeField) :: z
        type(ESMF_Field) :: zf

        if(.not. (x % is_valid() .and. y % is_valid())) then
           call z % set_invalid()
           call z % add_error('ESMF_Field values are invalid.')
        else
           call add_esmf_field(x % get_value(), y % get_value(), zf, rc)
           if(rc /= 0) then
              call z % set_invalid()
              call z % add_error('Result is invalid.')
           else
              call z % set_value(z)
           end if
        endif

    end function add_maybe_field

end module maybe_field_usecase_mod

program maybe_field_usecase

    use maybe_field_usecase_mod

    implicit none
    
    integer :: status
    type(ESMF_Field) :: x, y, z, u, w
    type(Maybe_Field) :: mx, my, mz, mw
    class(ErrorStack) :: error_stack

    call add_esmf_field(x, y, u, rc = status)
    if(status /=) !handle status error

    call add_esmf_field(u, z, w, rc = status)
    if(status /=) !handle status error

    mw = mx + my + mz
    if(.not. mw % is_valid()) !handle error_stack = mw % get_errors()

end program maybe_field_usecase
