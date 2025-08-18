module m_hash_a
  implicit none
#define FFH_KEY_TYPE integer
#define FFH_VAL_TYPE character(len=15)
#define FFH_VAL_IS_STRING
#include "ffhash_inc.f90"
end module m_hash_a

module m_hash_b
  implicit none
#define FFH_KEY_TYPE character(len=20)
#define FFH_KEY_IS_STRING
#define FFH_VAL_TYPE integer
#include "ffhash_inc.f90"
end module m_hash_b

program test
  use m_hash_a, ffha_t => ffh_t
  use m_hash_b, ffhb_t => ffh_t
  implicit none

  type(ffha_t) :: ha
  type(ffhb_t) :: hb
  integer      :: i

  call ha%ustore_value(1, "hello world")
  print *, ha%fget_value(1)
  if (ha%fget_value(1) /= "hello world") error stop "FAILED"

  call hb%ustore_value("first", 12345)
  print *, hb%fget_value("first")
  if (hb%fget_value("first") /= 12345) error stop "FAILED"

  ! Try to store the same key twice
  call hb%store_key("first", i)
  if (i /= -2) error stop "Expected -2 for duplicate key"

  print *, "PASSED"
  call ha%reset()
  call hb%reset()
end program test
