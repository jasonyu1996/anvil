### Values

#### Logic

A logic represents a bit taking values `1'b0` or `1'b1`.

#### Array

An array is a fixed-length sequence of values of the same data type.

A logic array written `n'x...`, where `x` is `b`, `o`, `d`, or `h`, is a logic
array of length `n`. In general, an array can also be represented with `[v1, v2, ..., vn]`.

#### Tuple

A tuple packs `n` values potentially of different data types, written `(v1, v2, ..., vn)`.
A special tuple value `()` is used to represent the absence of data.

#### Struct

A struct is similar to a tuple except that its elements are named. We refer to elements
in a struct _fields._ A struct is written `struct_type_ident::{ field1 = v1; field2 = v2; ...; fieldn = v2 }`, where `struct_type_ident` is the identifier of a struct type.

#### Enum

An enum represents a named value out of a set of constants defined in an enum type, written
`enum_type_ident::const_ident`, where `enum_type_ident` is the identifier of an enum type and
`const_ident` is an identifier of a constant defined in it.
