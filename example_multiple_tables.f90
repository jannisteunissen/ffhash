module m_hash_a 
  use iso_fortran_env
  implicit none
#define FFH_KEY_TYPE integer
#define FFH_STRING_VAL_TYPE character(len=15)
#include "ffhash_inc.f90"
end module m_hash_a

module m_hash_b
  use iso_fortran_env
  implicit none
#define FFH_STRING_KEY_TYPE character(len=20)
#define FFH_VAL_TYPE integer
#include "ffhash_inc.f90"
end module m_hash_b

module m_hash_c
  use iso_fortran_env
  implicit none
#define FFH_ENABLE_INT64
#define FFH_KEY_TYPE integer(int64)
#define FFH_VAL_TYPE integer(int64)
#include "ffhash_inc.f90"
end module m_hash_c

module m_hash_d
  use iso_fortran_env
  implicit none
#define FFH_ENABLE_INT64
#define FFH_KEY_TYPE integer(int64)
#define FFH_STRING_VAL_TYPE character(len=31)
#include "ffhash_inc.f90"
end module m_hash_d

program test
  use iso_fortran_env, only: int64
  use m_hash_a, ffha_t => ffh_t
  use m_hash_b, ffhb_t => ffh_t
  use m_hash_c, ffhc_t => ffh_t
  use m_hash_d, ffhd_t => ffh_t
  implicit none

  type(ffha_t) :: ha
  type(ffhb_t) :: hb
  type(ffhc_t) :: hc
  type(ffhd_t) :: hd
  integer      :: i
  integer(int64) :: k64, v64

  ! int -> string
  call ha%ustore_value(1, "hello world")
  print *, ha%fget_value(1)
  if (ha%fget_value(1) /= "hello world") error stop "FAILED ha"

  ! string -> int
  call hb%ustore_value("first", 12345)
  print *, hb%fget_value("first")
  if (hb%fget_value("first") /= 12345) error stop "FAILED hb"

  call hb%store_key("first", i)
  call hb%store_key("first", i, existing_key_is_error = .true.)
  if (i /= -2) error stop "Expected -2 for duplicate key"

  ! int64 -> int64
  k64 = 9876543210123456_int64
  v64 = 1234567890123456_int64
  call hc%ustore_value(k64, v64)
  print *, hc%fget_value(k64)
  if (hc%fget_value(k64) /= v64) error stop "FAILED hc"

  ! int64 -> string
  call hd%ustore_value(k64, "64-bit key works")
  print *, hd%fget_value(k64)
  if (hd%fget_value(k64) /= "64-bit key works") error stop "FAILED hd"

  print *, "PASSED"

  call ha%reset()
  call hb%reset()
  call hc%reset()
  call hd%reset()
end program test
