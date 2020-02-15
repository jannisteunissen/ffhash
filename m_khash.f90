!    Fortran implementation of khash by Jannis Teunissen
!
!    Original khash license copied below:
!
!    The MIT License
!
!    Copyright (c) 2008, 2009, 2011 by Attractive Chaos <attractor@live.co.uk>

!    Permission is hereby granted, free of charge, to any person obtaining
!    a copy of this software and associated documentation files (the
!    "Software"), to deal in the Software without restriction, including
!    without limitation the rights to use, copy, modify, merge, publish,
!    distribute, sublicense, and/or sell copies of the Software, and to
!    permit persons to whom the Software is furnished to do so, subject to
!    the following conditions:

!    The above copyright notice and this permission notice shall be
!    included in all copies or substantial portions of the Software.

!    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
!    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
!    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
!    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
!    BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
!    ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!    CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
!    SOFTWARE.

! If defined, use a custom name for the module
#ifndef MODULE_NAME
#define MODULE_NAME m_khash
#endif

module MODULE_NAME
  use iso_fortran_env
  ! If defined, import extra module with this statement
#ifdef USE_MODULE
  USE_MODULE
#endif

  implicit none
  private

  double precision, parameter :: HASH_UPPER = 0.77

  type khash_t
     integer                :: n_buckets   = 0
     integer                :: size        = 0
     integer                :: n_occupied  = 0
     integer                :: upper_bound = 0
     integer                :: mask        = 0
     character, allocatable :: flags(:)
     KEY_TYPE, allocatable  :: keys(:)
     VAL_TYPE, allocatable  :: vals(:)
  end type khash_t

  public :: khash_t
  public :: khash_get
  public :: khash_put
  public :: khash_del
  public :: khash_exists

contains

  function khash_get(h, key) result(ix)
    type(khash_t), intent(in) :: h
    KEY_TYPE, intent(in)      :: key
    integer                   :: ix, i, step

    i = hash_index(h, key)

    do step = 1, h%n_buckets
       ! Exit when an empty bucket or the key is found
       if (isempty(h, i) .or. &
            (.not. isdel(h, i) .and. h%keys(i) == key)) exit
       i = next_index(h, i, step)
    end do

    ix = -1
    if (step == h%n_buckets + 1) return ! Not found in loop
    if (khash_not_exists(h, i)) return          ! Exited, but key not found
    ix = i
  end function khash_get

  function khash_put(h, key) result(i)
    type(khash_t), intent(inout) :: h
    KEY_TYPE, intent(in)         :: key
    integer                      :: i, i_deleted, step, status

    i = -1

    if (h%n_occupied >= h%upper_bound) then
       if (h%n_buckets > shiftl(h%size, 1)) then
          ! Enough free space, but need to clean up the table
          call khash_resize(h, h%n_buckets, status)
          if (status /= 0) return
       else
          ! Increase table size
          call khash_resize(h, 2*h%n_buckets, status)
          if (status /= 0) return
       end if
    end if

    i = hash_index(h, key)
    i_deleted = -1

    if (.not. isempty(h, i)) then
       ! Skip over filled slots if they are deleted or have the wrong key. Skipping
       ! over deleted slots ensures that a key is not added twice, in case it is
       ! not at its 'first' hash index, and some keys in between have been deleted.
       do step = 1, h%n_buckets
          if (isempty(h, i) .or. (.not. isdel(h, i) .and. h%keys(i) == key)) exit
          if (isdel(h, i)) i_deleted = i
          i = next_index(h, i, step)
       end do

       if (isempty(h, i) .and. i_deleted /= -1) then
          ! Use deleted location
          i = i_deleted
       end if
    end if

    h%keys(i) = key
    h%size    = h%size + 1
    if (isempty(h, i)) h%n_occupied = h%n_occupied + 1
    call set_isboth_false(h, i)

  end function khash_put

  subroutine khash_resize(h, new_n_buckets, status)
    type(khash_t), intent(inout) :: h
    integer, intent(in)          :: new_n_buckets
    integer, intent(out)         :: status
    integer                      :: n_new, i, j, step
    type(khash_t)                :: hnew

    ! Make sure n_new is a power of two, and at least 4
    n_new = 4
    do while (n_new < new_n_buckets)
       n_new = 2 * n_new
    end do

    if (h%size >= nint(n_new * HASH_UPPER)) then
       ! Requested size is too small
       status = -1
       return
    end if

    ! Expand or shrink table
    allocate(hnew%flags(0:n_new-1), hnew%keys(0:n_new-1), &
         hnew%vals(0:n_new-1), stat=status)
    if (status /= 0) return

    hnew%flags(:)    = achar(0)
    hnew%size        = h%size
    hnew%n_occupied  = h%size
    hnew%n_buckets   = n_new
    hnew%upper_bound = nint(n_new * HASH_UPPER)
    hnew%mask        = n_new - 1

    do j = 0, h%n_buckets-1
       if (khash_exists(h, j)) then
          ! Find a new index
          i = hash_index(hnew, h%keys(j))

          do step = 1, hnew%n_buckets
             if (isempty(hnew, i)) exit
             i = next_index(hnew, i, step)
          end do

          hnew%vals(i) = h%vals(j)
          hnew%keys(i) = h%keys(j)
          call set_isempty_false(hnew, i)
       end if
    end do

    h      = hnew
    status = 0
  end subroutine khash_resize

  subroutine khash_del(h, ix, status)
    type(khash_t), intent(inout) :: h
    integer, intent(in)          :: ix
    integer, intent(out)         :: status


    if (ix < lbound(h%keys, 1) .or. ix > ubound(h%keys, 1)) then
       status = -1
    else if (khash_not_exists(h, ix)) then
       status = -1
    else
       call set_isdel_true(h, ix)
       h%size = h%size - 1
       status = 0
    end if
  end subroutine khash_del

  pure logical function isempty(h, i)
    type(khash_t), intent(in) :: h
    integer, intent(in)       :: i
    isempty = (iand(iachar(h%flags(i)), 1) == 0)
  end function isempty

  pure logical function isdel(h, i)
    type(khash_t), intent(in) :: h
    integer, intent(in)       :: i
    isdel = (iand(iachar(h%flags(i)), 2) /= 0)
  end function isdel

  pure logical function khash_exists(h, i)
    type(khash_t), intent(in) :: h
    integer, intent(in)       :: i
    khash_exists = (iand(iachar(h%flags(i)), 3) == 1)
  end function khash_exists

  pure logical function khash_not_exists(h, i)
    type(khash_t), intent(in) :: h
    integer, intent(in)       :: i
    khash_not_exists = (iand(iachar(h%flags(i)), 3) /= 1)
  end function khash_not_exists

  pure subroutine set_isboth_false(h, i)
    type(khash_t), intent(inout) :: h
    integer, intent(in)          :: i
    h%flags(i) = achar(1)
  end subroutine set_isboth_false

  pure subroutine set_isempty_false(h, i)
    type(khash_t), intent(inout) :: h
    integer, intent(in)          :: i
    h%flags(i) = achar(ior(iachar(h%flags(i)), 1))
  end subroutine set_isempty_false

  pure subroutine set_isdel_true(h, i)
    type(khash_t), intent(inout) :: h
    integer, intent(in)          :: i
    h%flags(i) = achar(ior(iachar(h%flags(i)), 2))
  end subroutine set_isdel_true

  pure integer function hash_index(h, key) result(i)
    type(khash_t), intent(in) :: h
    KEY_TYPE, intent(in)      :: key
    i = iand(hash_function(key), h%mask)
  end function hash_index

  pure integer function next_index(h, i_prev, step) result(i_new)
    type(khash_t), intent(in) :: h
    integer, intent(in)       :: i_prev
    integer, intent(in)       :: step
    i_new = iand(i_prev + step, h%mask)
  end function next_index

#ifndef hash_function
  pure function hash_function(key) result(hash)
    KEY_TYPE, intent(in)   :: key
    integer(int32)         :: hash, n
    integer, parameter     :: n_bytes = ceiling(storage_size(key)*0.125d0)
    character(len=n_bytes) :: buf

    buf = transfer(key, buf)
    hash = 5381

    do n = 1, n_bytes
       hash = (shiftl(hash, 5) + hash) + iachar(buf(n:n)) ! hash * 33 + char
    end do
  end function hash_function
#endif
end module MODULE_NAME
