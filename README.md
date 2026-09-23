# byte-sequence

`byte-sequence` aims to provide a SRFI-1-inspired API for manipulating
byte sequences encoded as bytevectors. It is inspired by the Haskell
bytestring library (https://hackage.haskell.org/package/bytestring/).

This library is the successor of the `byte-blob` egg, renamed to
reflect the CHICKEN 6 datatype for the underlying storage object: the
CHICKEN 6 `blob` datatype is the same object as the `bytevector`, and
is now referred to as `bytevector` in this library's interface.

## Library Procedures

### Predicates

#### `(byte-sequence? X) => BOOL`

Returns `#t` if the given object is a byte-sequence, `#f` otherwise.

#### `(byte-sequence-empty? BYTE-SEQUENCE) => BOOL`

Returns `#t` if the given byte-sequence is empty, `#f` otherwise.

#### `(byte-sequence-valid-utf8? BYTE-SEQUENCE) => BOOL`

Returns `#t` if the bytes of the given byte-sequence form a valid UTF-8
sequence, `#f` otherwise. The validation rejects malformed lead bytes,
truncated sequences, UTF-16 surrogate code points (U+D800..U+DFFF), and
encodings of values above U+10FFFF.

#### `(byte-sequence-is-prefix-of? PREFIX BYTE-SEQUENCE) => BOOL`

Returns `#t` if `PREFIX` is a prefix of `BYTE-SEQUENCE`, `#f` otherwise.

#### `(byte-sequence-is-suffix-of? SUFFIX BYTE-SEQUENCE) => BOOL`

Returns `#t` if `SUFFIX` is a suffix of `BYTE-SEQUENCE`, `#f` otherwise.

### Constructors

#### `(byte-sequence-empty) => BYTE-SEQUENCE`

Returns an empty byte-sequence.

#### `(byte-sequence-replicate N V) => BYTE-SEQUENCE`

Returns a byte-sequence of length `N`, where each element is `V`.

#### `(byte-sequence-cons X BYTE-SEQUENCE) => BYTE-SEQUENCE`

Analogous to list cons, but of complexity O(N), as it requires copying
the elements of the byte-sequence argument.

#### `(byte-sequence-snoc BYTE-SEQUENCE X) => BYTE-SEQUENCE`

Analogous to `byte-sequence-cons`, but appends `X` at the end of the given
byte-sequence. Of complexity O(N), as it requires copying the elements of
the byte-sequence argument.

#### `(byte-sequence-singleton X) => BYTE-SEQUENCE`

Returns a byte-sequence of length 1 containing the byte `X`.

#### `(bytevector->byte-sequence BYTEVECTOR) => BYTE-SEQUENCE`

Returns a byte-sequence containing the elements of the given
bytevector.

#### `(list->byte-sequence LIST) => BYTE-SEQUENCE`

Returns a byte-sequence containing the elements of `LIST`.

#### `(string->byte-sequence STRING) => BYTE-SEQUENCE`

Returns a byte-sequence containing the elements of `STRING`.

### Accessors

#### `(byte-sequence-length BYTE-SEQUENCE) => INTEGER`

Returns the number of elements contained in the given byte-sequence.

#### `(byte-sequence-car BYTE-SEQUENCE) => X`

Returns the first element of a byte-sequence. The argument byte-sequence must
be non-empty, or an exception will be thrown.

#### `(byte-sequence-cdr BYTE-SEQUENCE) => BYTE-SEQUENCE`

Returns a byte-sequence that contains the elements after the first element
of the given byte-sequence. The argument byte-sequence must be non-empty, or
an exception will be thrown.

#### `(byte-sequence-last BYTE-SEQUENCE) => X`

Returns the last element of a byte-sequence. The argument byte-sequence must
be non-empty, or an exception will be thrown.

#### `(byte-sequence-init BYTE-SEQUENCE) => BYTE-SEQUENCE`

Returns a byte-sequence that contains all elements except the last element
of the given byte-sequence. The argument byte-sequence must be non-empty, or
an exception will be thrown.

#### `(byte-sequence-uncons BYTE-SEQUENCE) => X BYTE-SEQUENCE` or `#f`

Returns two values, the first element of the given byte-sequence and a
byte-sequence containing the remaining elements. Returns `#f` if the
byte-sequence is empty.

#### `(byte-sequence-unsnoc BYTE-SEQUENCE) => BYTE-SEQUENCE X` or `#f`

Returns two values, a byte-sequence containing all elements except the
last, and the last element of the given byte-sequence. Returns `#f` if the
byte-sequence is empty.

#### `(byte-sequence-ref BYTE-SEQUENCE I) => BYTE`

Returns the i-th element of the given byte-sequence as a signed byte.

#### `(byte-sequence-uref BYTE-SEQUENCE I) => BYTE`

Returns the i-th element of the given byte-sequence as an unsigned byte.

#### `(byte-sequence-index-maybe BYTE-SEQUENCE I) => BYTE` or `#f`

Returns the i-th element of the given byte-sequence as an unsigned byte,
or `#f` if `I` is out of range.

### Transformers

#### `(byte-sequence-set! BYTE-SEQUENCE I V) => VOID`

Sets the i-th element of the given byte-sequence to the signed byte `V`.

#### `(byte-sequence-uset! BYTE-SEQUENCE I V) => VOID`

Sets the i-th element of the given byte-sequence to the unsigned byte `V`.

#### `(byte-sequence-append BYTE-SEQUENCE BYTE-SEQUENCE) => BYTE-SEQUENCE`

Appends two byte-sequences together.

#### `(byte-sequence-reverse BYTE-SEQUENCE) => BYTE-SEQUENCE`

Returns a byte-sequence that contains the elements of the given byte-sequence
in reverse order.

#### `(byte-sequence-intersperse BYTE-SEQUENCE BYTE) => BYTE-SEQUENCE`

Returns a byte-sequence with the given byte placed between the elements of
the given byte-sequence.

#### `(byte-sequence-map F BYTE-SEQUENCE) => BYTE-SEQUENCE`

Returns a byte-sequence obtained by applying `F` to each element of the
given byte-sequence.

#### `(byte-sequence->bytevector BYTE-SEQUENCE) => BYTEVECTOR`

Returns the underlying Scheme bytevector object.

#### `(byte-sequence->list BYTE-SEQUENCE [F]) => LIST`

Returns a list containing the elements of the given byte-sequence. If
procedure `F` is provided as a second argument, it is applied to
every element of the returned list.

#### `(byte-sequence->string BYTE-SEQUENCE) => STRING`

Returns a string containing the elements of the given byte-sequence.

### Comparison

#### `(byte-sequence=? BYTE-SEQUENCE BYTE-SEQUENCE) => BOOL`

Returns `#t` if the two byte-sequences contain the same sequence of bytes,
`#f` otherwise.

#### `(byte-sequence-compare BYTE-SEQUENCE BYTE-SEQUENCE) => INTEGER`

Lexicographic comparison of two byte-sequences. Returns `-1`, `0`, or `1`
as the first argument is less than, equal to, or greater than the
second.

### Searching

#### `(byte-sequence-elem-index BYTE BYTE-SEQUENCE) => INTEGER` or `#f`

Returns the index of the first occurrence of `BYTE` in the given
byte-sequence, or `#f` if the byte-sequence does not contain it.

#### `(byte-sequence-elem-index-end BYTE BYTE-SEQUENCE) => INTEGER` or `#f`

Returns the index of the last occurrence of `BYTE` in the given
byte-sequence, or `#f` if the byte-sequence does not contain it.

#### `(byte-sequence-elem-indices BYTE BYTE-SEQUENCE) => LIST`

Returns a list of the indices of all occurrences of `BYTE` in the
given byte-sequence, in increasing order.

#### `(byte-sequence-find-index F BYTE-SEQUENCE) => INTEGER` or `#f`

Returns the index of the first byte in the given byte-sequence that
satisfies the predicate `F`, or `#f` if no byte does.

#### `(byte-sequence-find-index-end F BYTE-SEQUENCE) => INTEGER` or `#f`

Returns the index of the last byte in the given byte-sequence that
satisfies the predicate `F`, or `#f` if no byte does.

#### `(byte-sequence-find-indices F BYTE-SEQUENCE) => LIST`

Returns a list of the indices of all bytes in the given byte-sequence that
satisfy the predicate `F`, in increasing order.

#### `(byte-sequence-count BYTE BYTE-SEQUENCE) => INTEGER`

Returns the number of occurrences of `BYTE` in the given byte-sequence.

### Subsequences

#### `(byte-sequence-take BYTE-SEQUENCE N) => BYTE-SEQUENCE`

Returns the prefix of the given byte-sequence of length `N`, or the entire
byte-sequence if `N` is greater than its length.

#### `(byte-sequence-drop BYTE-SEQUENCE N) => BYTE-SEQUENCE`

Returns the suffix of the given byte-sequence after the first `N` elements.

#### `(byte-sequence-span BYTE-SEQUENCE START END) => BYTE-SEQUENCE`

Returns the subsequence of the give byte-sequence from position `START`
to position `END`.

#### `(byte-sequence-take-end BYTE-SEQUENCE N) => BYTE-SEQUENCE`

Returns the suffix of the given byte-sequence of length `N`, or the entire
byte-sequence if `N` is greater than its length.

#### `(byte-sequence-drop-end BYTE-SEQUENCE N) => BYTE-SEQUENCE`

Returns the given byte-sequence with its last `N` elements removed, or the
empty byte-sequence if `N` is greater than its length.

#### `(byte-sequence-split-at BYTE-SEQUENCE N) => BYTE-SEQUENCE BYTE-SEQUENCE`

Returns two values, the prefix of the given byte-sequence of length `N`
and the remainder, as byte-sequences. If `N` is greater than the length of
the byte-sequence, the prefix is the entire byte-sequence and the remainder is
empty.

#### `(byte-sequence-take-while F BYTE-SEQUENCE) => BYTE-SEQUENCE`

Returns the longest prefix of the given byte-sequence whose bytes all
satisfy the predicate `F`.

#### `(byte-sequence-drop-while F BYTE-SEQUENCE) => BYTE-SEQUENCE`

Returns the given byte-sequence with the longest prefix whose bytes all
satisfy the predicate `F` removed.

#### `(byte-sequence-take-while-end F BYTE-SEQUENCE) => BYTE-SEQUENCE`

Returns the longest suffix of the given byte-sequence whose bytes all
satisfy the predicate `F`.

#### `(byte-sequence-drop-while-end F BYTE-SEQUENCE) => BYTE-SEQUENCE`

Returns the given byte-sequence with its longest suffix whose bytes all
satisfy the predicate `F` removed.

#### `(byte-sequence-span-while F BYTE-SEQUENCE) => BYTE-SEQUENCE BYTE-SEQUENCE`

Returns two values, the longest prefix of the given byte-sequence whose
bytes all satisfy the predicate `F`, and the remainder.

#### `(byte-sequence-break-while F BYTE-SEQUENCE) => BYTE-SEQUENCE BYTE-SEQUENCE`

Returns two values, the longest prefix of the given byte-sequence whose
bytes all fail the predicate `F`, and the remainder.

#### `(byte-sequence-span-while-end F BYTE-SEQUENCE) => BYTE-SEQUENCE BYTE-SEQUENCE`

Returns two values, the longest suffix of the given byte-sequence whose
bytes all satisfy the predicate `F`, and the rest.

#### `(byte-sequence-break-while-end F BYTE-SEQUENCE) => BYTE-SEQUENCE BYTE-SEQUENCE`

Returns two values, the longest suffix of the given byte-sequence whose
bytes all fail the predicate `F`, and the rest.

#### `(byte-sequence-strip-prefix PREFIX BYTE-SEQUENCE) => BYTE-SEQUENCE` or `#f`

Returns the remainder of `BYTE-SEQUENCE` without its first `N` bytes, if
the first `N` bytes match `PREFIX`, `#f` otherwise.

#### `(byte-sequence-strip-suffix SUFFIX BYTE-SEQUENCE) => BYTE-SEQUENCE` or `#f`

Returns the prefix of `BYTE-SEQUENCE` without its last `N` bytes, if the
last `N` bytes match `SUFFIX`, `#f` otherwise.

### Fold

#### `(byte-sequence-fold-left F INIT BYTE-SEQUENCE) => VALUE`

#### `(byte-sequence-fold-right F INIT BYTE-SEQUENCE) => VALUE`

Given a procedure of two arguments, a starting value, and a byte-sequence,
reduces the byte-sequence using the supplied procedure, from left to
right, or right to left, respectively.

### Find

#### `(byte-sequence-find NEEDLE HAYSTACK) => LIST`

Finds all non-overlapping instances of the byte-sequence `NEEDLE` in the
byte-sequence `HAYSTACK`. The first element of the returned list is the
prefix of `HAYSTACK` prior to any matches of `NEEDLE`.  The second
is a list of lists.

The first element of each pair in the list is a span from the
beginning of a match to the beginning of the next match, while the
second is a span from the beginning of the match to the end of the
input.

### I/O

#### `(file->byte-sequence FILENAME [MODE]) => BYTE-SEQUENCE`

Returns a byte-sequence with the contents of the given file.

`MODE` is an optional argument that can be one of `#:text` or
`#:binary` to specify text or binary mode on Windows.

#### `(byte-sequence->file FILENAME BYTE-SEQUENCE [MODE]) => UNDEFINED`

Writes the given byte-sequence to the file named by `FILENAME`.

`MODE` is an optional argument that is passed to
`call-with-output-file`, so `#:append` can be used to append to an
existing file.

#### `(byte-sequence-read PORT N) => BYTE-SEQUENCE`

Reads a byte-sequence of length `N` from the given port.  Currently, the
port must support the `port->fileno` procedure, which means that
string ports are not supported (in that particular case, procedure
`string->byte-sequence` can be used for converting strings to byte sequences).

#### `(byte-sequence-write PORT BYTE-SEQUENCE) => UNDEFINED`

Writes the given byte-sequence to the given port. Currently, the port must
support the `port->fileno` procedure, which means that string ports
are not supported (in that particular case, procedure
`string->byte-sequence` can be used for converting strings to byte
sequences).

### SRFI-4 transformers

#### `(u8vector->byte-sequence U8VECTOR) => BYTE-SEQUENCE`
#### `(s8vector->byte-sequence S8VECTOR) => BYTE-SEQUENCE`
#### `(u16vector->byte-sequence U16VECTOR) => BYTE-SEQUENCE`
#### `(s16vector->byte-sequence S16VECTOR) => BYTE-SEQUENCE`
#### `(u32vector->byte-sequence U32VECTOR) => BYTE-SEQUENCE`
#### `(s32vector->byte-sequence S32VECTOR) => BYTE-SEQUENCE`
#### `(f32vector->byte-sequence F32VECTOR) => BYTE-SEQUENCE`
#### `(f64vector->byte-sequence F64VECTOR) => BYTE-SEQUENCE`
#### `(byte-sequence->u8vector BYTE-SEQUENCE) => U8VECTOR`
#### `(byte-sequence->s8vector BYTE-SEQUENCE) => S8VECTOR`
#### `(byte-sequence->u16vector BYTE-SEQUENCE) => U16VECTOR`
#### `(byte-sequence->s16vector BYTE-SEQUENCE) => S16VECTOR`
#### `(byte-sequence->u32vector BYTE-SEQUENCE) => U32VECTOR`
#### `(byte-sequence->s32vector BYTE-SEQUENCE) => S32VECTOR`
#### `(byte-sequence->f32vector BYTE-SEQUENCE) => F32VECTOR`
#### `(byte-sequence->f64vector BYTE-SEQUENCE) => F64VECTOR`

## License

> Based on ideas from the Haskell
> [bytestring](https://hackage.haskell.org/package/bytestring) library.
>
> The code for `byte-sequence-find` is based on code from the Haskell Text
> library by Tom Harper and Bryan O'Sullivan.
>
> Copyright 2009-2026 Ivan Raikov, Dan Thedens.
>
>
> This program is free software: you can redistribute it and/or modify
> it under the terms of the GNU Lesser General Public License as
> published by the Free Software Foundation, either version 3 of the
> License, or (at your option) any later version.
>
> This program is distributed in the hope that it will be useful, but
> WITHOUT ANY WARRANTY; without even the implied warranty of
> MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
> General Public License for more details.
>
> A full copy of the GPL license can be found at
> <http://www.gnu.org/licenses/>.