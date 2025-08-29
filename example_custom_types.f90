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

module m_example_2
  use iso_fortran_env
  implicit none

  type, public :: my_type_2
     integer, allocatable :: x(:)
  end type my_type_2

#define FFH_KEY_TYPE type(my_type_2)
#define FFH_CUSTOM_KEYS_EQUAL
#define FFH_CUSTOM_CONVERT_KEY
#define FFH_VAL_TYPE integer(int64)
#include "ffhash_inc.f90"

  pure logical function keys_equal(a, b)
    type(my_type_2), intent(in) :: a, b
    keys_equal = size(a%x) == size(b%x)
    if (keys_equal) keys_equal = all(a%x == b%x)
  end function keys_equal

  pure function convert_key(a) result(bstring)
    type(my_type_2), intent(in) :: a
    character(len=size(a%x)*4)  :: bstring
    bstring = transfer(a%x, bstring)
  end function convert_key
end module m_example_2

program test
  use m_example
  use m_example_2, ffh2_t => ffh_t
  use iso_fortran_env

  implicit none

  call test_1()
  call test_2()

contains

  subroutine test_1()
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
  end subroutine test_1

  subroutine test_2()
    type(ffh2_t) :: h
    type(my_type_2) :: a, b, c

    allocate(a%x(5), b%x(3), c%x(3))
    a%x = [1, 2, 3, 4, 5]
    b%x = [3, 2, 1]
    c%x = [1, 2, 3]

    call h%ustore_value(a, 1024_int64)
    call h%ustore_value(b, 2048_int64)
    call h%ustore_value(c, 4096_int64)

    if (h%fget_value(a) == 1024_int64) then
       print *, "PASSED"
    else
       error stop "FAILED"
    end if

    call h%reset()
  end subroutine test_2

end program test
