# byte-blob

`byte-blob` aims to provide a SRFI-1-inspired API for manipulating
byte vectors encoded as blobs. It is inspired by the Haskell bytestring library (https://hackage.haskell.org/package/bytestring/).

## Library Procedures

### Predicates

#### `(byte-blob? X) => BOOL`

Returns `#t` if the given object is a byte-blob, `#f` otherwise.

#### `(byte-blob-empty? BYTE-BLOB) => BOOL`

Returns `#t` if the given byte-blob is empty, `#f` otherwise.

#### `(byte-blob-valid-utf8? BYTE-BLOB) => BOOL`

Returns `#t` if the bytes of the given byte-blob form a valid UTF-8
sequence, `#f` otherwise. The validation rejects malformed lead bytes,
truncated sequences, UTF-16 surrogate code points (U+D800..U+DFFF), and
encodings of values above U+10FFFF.

#### `(byte-blob-is-prefix-of? PREFIX BYTE-BLOB) => BOOL`

Returns `#t` if `PREFIX` is a prefix of `BYTE-BLOB`, `#f` otherwise.

#### `(byte-blob-is-suffix-of? SUFFIX BYTE-BLOB) => BOOL`

Returns `#t` if `SUFFIX` is a suffix of `BYTE-BLOB`, `#f` otherwise.

### Constructors

#### `(byte-blob-empty) => BYTE-BLOB`

Returns an empty byte-blob.

#### `(byte-blob-replicate N V) => BYTE-BLOB`

Returns a byte-blob of length `N`, where each element is `V`.

#### `(byte-blob-cons X BYTE-BLOB) => BYTE-BLOB`

Analogous to list cons, but of complexity O(N), as it requires copying
the elements of the byte-blob argument.

#### `(byte-blob-snoc BYTE-BLOB X) => BYTE-BLOB`

Analogous to `byte-blob-cons`, but appends `X` at the end of the given
byte-blob. Of complexity O(N), as it requires copying the elements of
the byte-blob argument.

#### `(byte-blob-singleton X) => BYTE-BLOB`

Returns a byte-blob of length 1 containing the byte `X`.

#### `(blob->byte-blob BLOB) => BYTE-BLOB`

Returns a byte-blob containing the elements of the given blob.

#### `(list->byte-blob LIST) => BYTE-BLOB`

Returns a byte-blob containing the elements of `LIST`.

#### `(string->byte-blob STRING) => BYTE-BLOB`

Returns a byte-blob containing the elements of `STRING`.

### Accessors

#### `(byte-blob-length BYTE-BLOB) => INTEGER`

Returns the number of elements contained in the given byte-blob.

#### `(byte-blob-car BYTE-BLOB) => X`

Returns the first element of a byte-blob. The argument byte-blob must
be non-empty, or an exception will be thrown.

#### `(byte-blob-cdr BYTE-BLOB) => BYTE-BLOB`

Returns a byte-blob that contains the elements after the first element
of the given byte-blob. The argument byte-blob must be non-empty, or
an exception will be thrown.

#### `(byte-blob-last BYTE-BLOB) => X`

Returns the last element of a byte-blob. The argument byte-blob must
be non-empty, or an exception will be thrown.

#### `(byte-blob-init BYTE-BLOB) => BYTE-BLOB`

Returns a byte-blob that contains all elements except the last element
of the given byte-blob. The argument byte-blob must be non-empty, or
an exception will be thrown.

#### `(byte-blob-uncons BYTE-BLOB) => X BYTE-BLOB` or `#f`

Returns two values, the first element of the given byte-blob and a
byte-blob containing the remaining elements. Returns `#f` if the
byte-blob is empty.

#### `(byte-blob-unsnoc BYTE-BLOB) => BYTE-BLOB X` or `#f`

Returns two values, a byte-blob containing all elements except the
last, and the last element of the given byte-blob. Returns `#f` if the
byte-blob is empty.

#### `(byte-blob-ref BYTE-BLOB I) => BYTE`

Returns the i-th element of the given byte-blob as a signed byte.

#### `(byte-blob-uref BYTE-BLOB I) => BYTE`

Returns the i-th element of the given byte-blob as an unsigned byte.

#### `(byte-blob-index-maybe BYTE-BLOB I) => BYTE` or `#f`

Returns the i-th element of the given byte-blob as an unsigned byte,
or `#f` if `I` is out of range.

### Transformers

#### `(byte-blob-set! BYTE-BLOB I V) => VOID`

Sets the i-th element of the given byte-blob to the signed byte `V`.

#### `(byte-blob-uset! BYTE-BLOB I V) => VOID`

Sets the i-th element of the given byte-blob to the unsigned byte `V`.

#### `(byte-blob-append BYTE-BLOB BYTE-BLOB) => BYTE-BLOB`

Appends two byte-blobs together.

#### `(byte-blob-reverse BYTE-BLOB) => BYTE-BLOB`

Returns a byte-blob that contains the elements of the given byte-blob
in reverse order.

#### `(byte-blob-intersperse BYTE-BLOB BYTE) => BYTE-BLOB`

Returns a byte-blob with the given byte placed between the elements of
the given byte-blob.

#### `(byte-blob-map F BYTE-BLOB) => BYTE-BLOB`

Returns a byte-blob obtained by applying `F` to each element of the
given byte-blob.

#### `(byte-blob->blob BYTE-BLOB) => BLOB`

Returns the underlying Scheme blob object.

#### `(byte-blob->list BYTE-BLOB [F]) => LIST`

Returns a list containing the elements of the given byte-blob. If
procedure `F` is provided as a second argument, it is applied to
every element of the returned list.

#### `(byte-blob->string BYTE-BLOB) => STRING`

Returns a string containing the elements of the given byte-blob.

### Comparison

#### `(byte-blob=? BYTE-BLOB BYTE-BLOB) => BOOL`

Returns `#t` if the two byte-blobs contain the same sequence of bytes,
`#f` otherwise.

#### `(byte-blob-compare BYTE-BLOB BYTE-BLOB) => INTEGER`

Lexicographic comparison of two byte-blobs. Returns `-1`, `0`, or `1`
as the first argument is less than, equal to, or greater than the
second.

### Searching

#### `(byte-blob-elem-index BYTE BYTE-BLOB) => INTEGER` or `#f`

Returns the index of the first occurrence of `BYTE` in the given
byte-blob, or `#f` if the byte-blob does not contain it.

#### `(byte-blob-elem-index-end BYTE BYTE-BLOB) => INTEGER` or `#f`

Returns the index of the last occurrence of `BYTE` in the given
byte-blob, or `#f` if the byte-blob does not contain it.

#### `(byte-blob-elem-indices BYTE BYTE-BLOB) => LIST`

Returns a list of the indices of all occurrences of `BYTE` in the
given byte-blob, in increasing order.

#### `(byte-blob-find-index F BYTE-BLOB) => INTEGER` or `#f`

Returns the index of the first byte in the given byte-blob that
satisfies the predicate `F`, or `#f` if no byte does.

#### `(byte-blob-find-index-end F BYTE-BLOB) => INTEGER` or `#f`

Returns the index of the last byte in the given byte-blob that
satisfies the predicate `F`, or `#f` if no byte does.

#### `(byte-blob-find-indices F BYTE-BLOB) => LIST`

Returns a list of the indices of all bytes in the given byte-blob that
satisfy the predicate `F`, in increasing order.

#### `(byte-blob-count BYTE BYTE-BLOB) => INTEGER`

Returns the number of occurrences of `BYTE` in the given byte-blob.

### Subsequences

#### `(byte-blob-take BYTE-BLOB N) => BYTE-BLOB`

Returns the prefix of the given byte-blob of length `N`, or the entire
byte-blob if `N` is greater than its length.

#### `(byte-blob-drop BYTE-BLOB N) => BYTE-BLOB`

Returns the suffix of the given byte-blob after the first `N` elements.

#### `(byte-blob-span BYTE-BLOB START END) => BYTE-BLOB`

Returns the subsequence of the give byte-blob from position `START`
to position `END`.

#### `(byte-blob-take-end BYTE-BLOB N) => BYTE-BLOB`

Returns the suffix of the given byte-blob of length `N`, or the entire
byte-blob if `N` is greater than its length.

#### `(byte-blob-drop-end BYTE-BLOB N) => BYTE-BLOB`

Returns the given byte-blob with its last `N` elements removed, or the
empty byte-blob if `N` is greater than its length.

#### `(byte-blob-split-at BYTE-BLOB N) => BYTE-BLOB BYTE-BLOB`

Returns two values, the prefix of the given byte-blob of length `N`
and the remainder, as byte-blobs. If `N` is greater than the length of
the byte-blob, the prefix is the entire byte-blob and the remainder is
empty.

#### `(byte-blob-take-while F BYTE-BLOB) => BYTE-BLOB`

Returns the longest prefix of the given byte-blob whose bytes all
satisfy the predicate `F`.

#### `(byte-blob-drop-while F BYTE-BLOB) => BYTE-BLOB`

Returns the given byte-blob with the longest prefix whose bytes all
satisfy the predicate `F` removed.

#### `(byte-blob-take-while-end F BYTE-BLOB) => BYTE-BLOB`

Returns the longest suffix of the given byte-blob whose bytes all
satisfy the predicate `F`.

#### `(byte-blob-drop-while-end F BYTE-BLOB) => BYTE-BLOB`

Returns the given byte-blob with its longest suffix whose bytes all
satisfy the predicate `F` removed.

#### `(byte-blob-span-while F BYTE-BLOB) => BYTE-BLOB BYTE-BLOB`

Returns two values, the longest prefix of the given byte-blob whose
bytes all satisfy the predicate `F`, and the remainder.

#### `(byte-blob-break-while F BYTE-BLOB) => BYTE-BLOB BYTE-BLOB`

Returns two values, the longest prefix of the given byte-blob whose
bytes all fail the predicate `F`, and the remainder.

#### `(byte-blob-span-while-end F BYTE-BLOB) => BYTE-BLOB BYTE-BLOB`

Returns two values, the longest suffix of the given byte-blob whose
bytes all satisfy the predicate `F`, and the rest.

#### `(byte-blob-break-while-end F BYTE-BLOB) => BYTE-BLOB BYTE-BLOB`

Returns two values, the longest suffix of the given byte-blob whose
bytes all fail the predicate `F`, and the rest.

#### `(byte-blob-strip-prefix PREFIX BYTE-BLOB) => BYTE-BLOB` or `#f`

Returns the remainder of `BYTE-BLOB` without its first `N` bytes, if
the first `N` bytes match `PREFIX`, `#f` otherwise.

#### `(byte-blob-strip-suffix SUFFIX BYTE-BLOB) => BYTE-BLOB` or `#f`

Returns the prefix of `BYTE-BLOB` without its last `N` bytes, if the
last `N` bytes match `SUFFIX`, `#f` otherwise.

### Fold

#### `(byte-blob-fold-left F INIT BYTE-BLOB) => VALUE`

#### `(byte-blob-fold-right F INIT BYTE-BLOB) => VALUE`

Given a procedure of two arguments, a starting value, and a byte-blob,
reduces the byte-blob using the supplied procedure, from left to
right, or right to left, respectively.

### Find

#### `(byte-blob-find NEEDLE HAYSTACK) => LIST`

Finds all non-overlapping instances of the byte-blob `NEEDLE` in the
byte-blob `HAYSTACK`. The first element of the returned list is the
prefix of `HAYSTACK` prior to any matches of `NEEDLE`.  The second
is a list of lists.

The first element of each pair in the list is a span from the
beginning of a match to the beginning of the next match, while the
second is a span from the beginning of the match to the end of the
input.

### I/O

#### `(file->byte-blob FILENAME [MODE]) => BYTE-BLOB`

Returns a byte-blob with the contents of the given file.

`MODE` is an optional argument that can be one of `#:text` or
`#:binary` to specify text or binary mode on Windows.

#### `(byte-blob->file FILENAME BYTE-BLOB [MODE]) => UNDEFINED`

Writes the given byte-blob to the file named by `FILENAME`.

`MODE` is an optional argument that is passed to
`call-with-output-file`, so `#:append` can be used to append to an
existing file.

#### `(byte-blob-read PORT N) => BYTE-BLOB`

Reads a byte-blob of length `N` from the given port.  Currently, the
port must support the `port->fileno` procedure, which means that
string ports are not supported (in that particular case, procedure
`string->byte-blob` can be used for converting strings to byte blobs).

#### `(byte-blob-write PORT BYTE-BLOB) => UNDEFINED`

Writes the given byte-blob to the given port. Currently, the port must
support the `port->fileno` procedure, which means that string ports
are not supported (in that particular case, procedure
`string->byte-blob` can be used for converting strings to byte
blobs).

### SRFI-4 transformers

#### `(u8vector->byte-blob U8VECTOR) => BYTE-BLOB`
#### `(s8vector->byte-blob S8VECTOR) => BYTE-BLOB`
#### `(u16vector->byte-blob U16VECTOR) => BYTE-BLOB`
#### `(s16vector->byte-blob S16VECTOR) => BYTE-BLOB`
#### `(u32vector->byte-blob U32VECTOR) => BYTE-BLOB`
#### `(s32vector->byte-blob S32VECTOR) => BYTE-BLOB`
#### `(f32vector->byte-blob F32VECTOR) => BYTE-BLOB`
#### `(f64vector->byte-blob F64VECTOR) => BYTE-BLOB`
#### `(byte-blob->u8vector BYTE-BLOB) => U8VECTOR`
#### `(byte-blob->s8vector BYTE-BLOB) => S8VECTOR`
#### `(byte-blob->u16vector BYTE-BLOB) => U16VECTOR`
#### `(byte-blob->s16vector BYTE-BLOB) => S16VECTOR`
#### `(byte-blob->u32vector BYTE-BLOB) => U32VECTOR`
#### `(byte-blob->s32vector BYTE-BLOB) => S32VECTOR`
#### `(byte-blob->f32vector BYTE-BLOB) => F32VECTOR`
#### `(byte-blob->f64vector BYTE-BLOB) => F64VECTOR`

## License

> Based on ideas from the Haskell
> [bytestring](https://hackage.haskell.org/package/bytestring) library.
>
> The code for `byte-blob-find` is based on code from the Haskell Text
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