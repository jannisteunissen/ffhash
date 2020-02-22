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

Usage
==

The file `ffhash_inc.f90` can for example be included like this:

```Fortran
module m_example
#define FFH_KEY_TYPE integer
#include "ffhash_inc.f90"
end module m_example
```

The following preprocessor commands can be used:

* `#define FFH_KEY_TYPE` (required) followed by a type name (e.g., `integer`, `real`, `type(my_type)`
* `#define FFH_VAL_TYPE` (optional) followed by a type name, so that keys can be associated with values.
* `#define FFH_KEY_IS_STRING` (optional) for string key types. This allows to use keys shorter than the maximum length. Note that in Fortran "a" == "a " (trailing spaces are ignored).
* `#define FFH_VAL_IS_STRING` (optional), similar to `FFH_KEY_IS_STRING`.
* `#define FFH_CUSTOM_HASH_FUNCTION` (optional) to define a custom hash function (after the `#include "ffhash_inc.f90"` line).
* `#define FFH_CUSTOM_KEYS_EQUAL` (optional) to define a custom function for comparing keys (after the `#include "ffhash_inc.f90"` line).

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
```

Below is the full list of included methods. The variants starting with a `u` call `error stop` in case of errors. The variants without a `u` have an additional `status` argument, which is `-1` in case of errors.

| name | description |
|---|---|
| get_index | Get index of a key |
| valid_index | Check whether a valid key is present at index |
| store_key | Store a new key |
| delete_key | Delete a key |
| udelete_key | Delete a key (can perform error stop) |
| delete_index | Delete key at an index |
| udelete_index | Delete key at an index (can perform error stop) |
| resize | Resize the hash table |
| store_value | Store a key-value pair |
| ustore_value | Store a key-value pair (can perform error stop) |
| get_value | Get value for a key |
| uget_value | Get value for a key (can perform error stop) |
| fget_value | Function to get value for a key (can perform error stop) |
| hash_function | Hash function |

List of examples
==

* [Using custom types](example_custom_types.f90)
* [Using a custom hash function](example_custom_hash_function.f90) (also shows how to use strings as keys)
* [Using multiple hash tables](example_multiple_tables.f90) (also shows how to use strings as keys/values)
* [Simple benchmark](example_benchmark.f90)
