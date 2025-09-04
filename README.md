ffhash: Fast Fortran Hash Table
==

The implementation is heavily inspired by [khash](https://github.com/attractivechaos/klib/blob/master/khash.h).

Properties of the hash table:
* [Open addressing](https://en.wikipedia.org/wiki/Open_addressing)
* [Quadratic probing](https://en.wikipedia.org/wiki/Quadratic_probing) (but can easily be adjusted to linear probing)
* [Murmur3_x86_32](http://code.google.com/p/smhasher/wiki/MurmurHash3) as default hash function, see also [murmur3-fortran](https://github.com/jannisteunissen/murmur3-fortran)
* Generic: the keys and values can be of any type.

Requirements
==

* Fortran 2008 compatible compiler, such as `gfortran` or `ifort`

Preprocessing should be enabled (typically with `-cpp`). With older `gfortran` versions (version 7 or older) there may be warnings.

List of examples
==

* [Using custom types](example_custom_types.f90)
* [Using a custom hash function](example_custom_hash_function.f90) (also shows how to use strings as keys)
* [Using multiple hash tables](example_multiple_tables.f90) (also shows how to use strings as keys/values)
* [Simple benchmark](example_benchmark.f90)

Usage
==

The file `ffhash_inc.f90` can for example be included like this:

```Fortran
module m_example
#define FFH_KEY_TYPE integer
#include "ffhash_inc.f90"
end module m_example
```

The following preprocessor commands can be used (on the input side):

* `#define FFH_KEY_TYPE` (required) followed by a type name (e.g., `integer`, `real`, `type(my_type)`).  
   This is the type of the keys stored in the hash table.  
* `#define FFH_STRING_KEY_TYPE` followed by a character type declaration (e.g., `character(len=15)`).  
  This is an alternative for `FFH_KEY_TYPE` used for string keys.  
  (If defined, `FFH_KEY_TYPE = FFH_STRING_KEY_TYPE` will be set automatically in `ffhash_inc.f90`.)
* `#define FFH_VAL_TYPE` (optional) followed by a type name, so that keys can be associated with values.  
* `#define FFH_STRING_VAL_TYPE` (optional) followed by a character type declaration (e.g., `character(len=30)`).  
  This is an alternative for `FFH_VAL_TYPE` used for string values.  
  (`FFH_VAL_TYPE = FFH_STRING_VAL_TYPE` will be set automatically in `ffhash_inc.f90`.)  
* `#define FFH_KEY_TYPE` (required) followed by a type name (e.g., `integer`, `real`, `type(my_type)`  
* `#define FFH_VAL_TYPE` (optional) followed by a type name, so that keys can be associated with values.
* `#define FFH_ENABLE_INT64` (optional) to use 64-bit integers for bucket indices, counters, and hash values.  
  This allows handling very large tables (more than 2 billion buckets or keys).  
  If undefined, 32-bit integers are used by default, which is usually sufficient and slightly faster.  
* `#define FFH_CUSTOM_KEYS_EQUAL` (optional) to define a custom function for comparing keys  
   (after the `#include "ffhash_inc.f90"` line).
* `#define FFH_CUSTOM_CONVERT_KEY` (optional) to convert the key into a `character` buffer suitable for hashing.  
   This gives full control over how keys are serialized (e.g., for derived types).  
   If undefined, the library supplies default conversions for string and non-string keys.  
* `#define FFH_CUSTOM_HASH_FUNCTION` (optional) to define a custom hash function  
   (after the `#include "ffhash_inc.f90"` line).

A type `ffh_t` can be used after importing the created module, for example like this:

```Fortran
type(ffh_t) :: h

! Store and retrieve a key/value pair
call h%store_value(key, value, status)
call h%get_value(key, value, status)

! Store and retrieve a key/value pair (will abort on failures)
call h%ustore_value(key, value)
call h%uget_value(key, value)

! The values can also be indexed directly
i = h%get_index(key)
if (i /= -1) h%vals(i) = value

! This is also possible
value = h%fget_value(key)

! The index range is from 0 to n_buckets-1
do i = 0, h%n_buckets-1
  if (h%valid_index(i)) ...
end do
```

Below is the full list of (public) methods included. The variants starting with a `u` call `error stop` in case of errors. The variants without a `u` have an additional `status` argument, which is `-1` in case of errors.

| name | description |
|---|---|
| `get_index` | Get index of a key |
| `valid_index` | Check whether a valid key is present at index |
| `store_key` | Store a new key |
| `delete_key`, `udelete_key` | Delete a key |
| `delete_index`, `udelete_index` | Delete key at an index |
| `store_value`, `ustore_value` | Store a key-value pair |
| `get_value`, `uget_value` | Get value for a key |
| `fget_value` | Function to get value for a key (can perform error stop) |
| `fget_value_or` | Function to get value for a key or a dummy if not found |
| `resize` | Manually resize the hash table (happens automatically) |
| `reset` | Reset the hash table to initial empty state |
| `hash_function` | Hash function |
| `convert_key`      | Function to convert a key to a string buffer before hashing (optional, user-supplied) |

Links
==

* [murmur3-fortran](https://github.com/jannisteunissen/murmur3-fortran) Murmur3
  hash function implemented in Fortran
* [Hash tables on Fortran Wiki](http://fortranwiki.org/fortran/show/Hash+tables)
