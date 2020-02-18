  ! This file should be included in a module ... end module block
  ! For example:
  ! module m_ffhash
  ! #define KEY_TYPE integer
  ! #define VAL_TYPE integer (optional)
  ! #include "ffhash_inc.f90"
  ! end module m_ffhash

  double precision, parameter :: HASH_UPPER = 0.77

  type khash_t
     integer                :: n_buckets   = 0
     integer                :: size        = 0
     integer                :: n_occupied  = 0
     integer                :: upper_bound = 0
     integer                :: mask        = 0
     character, allocatable :: flags(:)
     KEY_TYPE, allocatable  :: keys(:)
#ifdef VAL_TYPE
     VAL_TYPE, allocatable  :: vals(:)
#endif
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
    if (.not. khash_exists(h, i)) return  ! Exited, but key not found
    ix = i
  end function khash_get

  function khash_put(h, key) result(i)
    type(khash_t), intent(inout) :: h
    KEY_TYPE, intent(in)         :: key
    integer                      :: i, i_deleted, step, status

    i = -1

    if (h%n_occupied >= h%upper_bound) then
       if (h%size <= h%upper_bound/2) then
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

    if (isempty(h, i)) then
       h%n_occupied = h%n_occupied + 1
       h%size    = h%size + 1
       h%keys(i) = key
       call set_isboth_false(h, i)
    else if (isdel(h, i)) then
       h%n_occupied = h%n_occupied + 1
       h%size    = h%size + 1
       h%keys(i) = key
       call set_isboth_false(h, i)
    end if
    ! If key is already present, do nothing

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
#ifdef VAL_TYPE
    allocate(hnew%flags(0:n_new-1), hnew%keys(0:n_new-1), &
         hnew%vals(0:n_new-1), stat=status)
#else
    allocate(hnew%flags(0:n_new-1), hnew%keys(0:n_new-1), stat=status)
#endif
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
#ifdef VAL_TYPE
          hnew%vals(i) = h%vals(j)
#endif
          hnew%keys(i) = h%keys(j)
          call set_isboth_false(hnew, i)
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
    else if (.not. khash_exists(h, ix)) then
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
    khash_exists = (iachar(h%flags(i)) == 1)
  end function khash_exists

  pure subroutine set_isboth_false(h, i)
    type(khash_t), intent(inout) :: h
    integer, intent(in)          :: i
    h%flags(i) = achar(1)
  end subroutine set_isboth_false

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

  pure integer function next_index(h, i_prev, step)
    type(khash_t), intent(in) :: h
    integer, intent(in)       :: i_prev
    integer, intent(in)       :: step
    next_index = iand(i_prev + step, h%mask)
  end function next_index

#ifndef CUSTOM_HASH_FUNCTION
  pure function hash_function(key) result(hash)
    KEY_TYPE, intent(in)   :: key
    integer                :: hash
    integer, parameter     :: n_bytes = ceiling(storage_size(key)*0.125d0)
    integer, parameter     :: seed    = 42
    character(len=n_bytes) :: buf

    call MurmurHash3_x86_32(transfer(key, buf), n_bytes, seed, hash)
  end function hash_function

  pure integer(int32) function rotl32(x, r)
    use iso_fortran_env
    integer(int32), intent(in) :: x
    integer(int8), intent(in)  :: r
    rotl32 = ior(shiftl(x, r), shiftr(x, (32 - r)))
  end function rotl32

  pure integer(int64) function rotl64(x, r)
    use iso_fortran_env
    integer(int64), intent(in) :: x
    integer(int8), intent(in)  :: r
    rotl64 = ior(shiftl(x, r), shiftr(x, (64 - r)))
  end function rotl64

  ! Finalization mix - force all bits of a hash block to avalanche
  pure integer(int32) function fmix32(h_in) result(h)
    use iso_fortran_env
    integer(int32), intent(in) :: h_in
    h = h_in
    h = ieor(h, shiftr(h, 16))
    h = h * (-2048144789) !0x85ebca6b
    h = ieor(h, shiftr(h, 13))
    h = h * (-1028477387) !0xc2b2ae35
    h = ieor(h, shiftr(h, 16))
  end function fmix32

  pure subroutine MurmurHash3_x86_32(key, klen, seed, hash)
    use iso_fortran_env
    integer, intent(in)             :: klen
    character(len=klen), intent(in) :: key
    integer(int32), intent(in)      :: seed
    integer(int32), intent(out)     :: hash
    integer                         :: i, i0, n, nblocks
    integer(int32)                  :: h1, k1
    integer(int32), parameter       :: c1        = -862048943 ! 0xcc9e2d51
    integer(int32), parameter       :: c2        = 461845907  !0x1b873593
    integer, parameter              :: shifts(3) = [0, 8, 16]

    h1      = seed
    nblocks = shiftr(klen, 2)    ! nblocks/4

    ! body
    do i = 1, nblocks
       k1 = transfer(key(i*4-3:i*4), k1)

       k1 = k1 * c1
       k1 = rotl32(k1,15_int8)
       k1 = k1 * c2

       h1 = ieor(h1, k1)
       h1 = rotl32(h1,13_int8)
       h1 = h1 * 5 - 430675100  ! 0xe6546b64
    end do

    ! tail
    k1 = 0
    i  = iand(klen, 3)
    i0 = 4 * nblocks

    do n = i, 1, -1
       k1 = ieor(k1, shiftl(iachar(key(i0+n:i0+n)), shifts(n)))
    end do

    ! Check if the above loop was executed
    if (i >= 1) then
       k1 = k1 * c1
       k1 = rotl32(k1,15_int8)
       k1 = k1 * c2
       h1 = ieor(h1, k1)
    end if

    ! finalization
    h1 = ieor(h1, klen)
    h1 = fmix32(h1)
    hash = h1
  end subroutine MurmurHash3_x86_32
#endif
