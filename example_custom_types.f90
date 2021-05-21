module m_example
  use iso_fortran_env
  implicit none

  type, public :: my_type
     integer :: n
  end type my_type

#define FFH_KEY_TYPE type(my_type)
#define FFH_CUSTOM_KEYS_EQUAL
#define FFH_VAL_TYPE integer(int64)
#include "ffhash_inc.f90"

  pure logical function keys_equal(a, b)
    type(my_type), intent(in) :: a, b
    keys_equal = (a%n == b%n)
  end function keys_equal
end module m_example

program test
  use m_example
  use iso_fortran_env

  implicit none

  type(ffh_t) :: h
  type(my_type) :: x, y

  x%n = 123
  y%n = 345

  call h%ustore_value(x, 1024_int64)
  call h%ustore_value(y, 2048_int64)

  if (h%fget_value(x) == 1024_int64) then
     print *, "PASSED"
  else
     error stop "FAILED"
  end if

  call h%reset()
end program test
