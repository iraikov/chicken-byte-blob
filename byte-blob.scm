;;
;;  Utility procedures for manipulating blobs as byte sequences.
;;
;;   Copyright 2009-2026 Ivan Raikov, Dan Thedens.
;;
;;   This program is free software: you can redistribute it and/or
;;   modify it under the terms of the GNU General Public License as
;;   published by the Free Software Foundation, either version 3 of
;;   the License, or (at your option) any later version.
;;
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;   General Public License for more details.
;;
;;   A full copy of the GPL license can be found at
;;   <http://www.gnu.org/licenses/>.

(module byte-blob

	(byte-blob?
	 byte-blob-empty?
	 byte-blob-length
	 byte-blob-empty
	 blob->byte-blob
	 list->byte-blob
	 string->byte-blob
	 file->byte-blob
         byte-blob-replicate
	 byte-blob->blob
	 byte-blob-offset
	 byte-blob-cons 
	 byte-blob-car
	 byte-blob-cdr
	 byte-blob-ref
	 byte-blob-uref
	 byte-blob-set!
	 byte-blob-uset!
	 byte-blob-append
	 byte-blob-reverse
	 byte-blob-intersperse
	 byte-blob-take
	 byte-blob-drop
         byte-blob-span
	 byte-blob-map
	 byte-blob-fold-left
	 byte-blob-fold-right
	 byte-blob-find
	 byte-blob->list
	 byte-blob->string
	 byte-blob-read
	 byte-blob-write

	 u8vector->byte-blob  
	 s8vector->byte-blob  
	 u16vector->byte-blob 
	 s16vector->byte-blob 
	 u32vector->byte-blob 
	 s32vector->byte-blob 
	 f32vector->byte-blob 
	 f64vector->byte-blob 
	 
	 byte-blob->u8vector  
	 byte-blob->s8vector  
	 byte-blob->u16vector 
	 byte-blob->s16vector 
	 byte-blob->u32vector 
	 byte-blob->s32vector 
	 byte-blob->f32vector 
	 byte-blob->f64vector 
	 )

	(import scheme (chicken base) (chicken foreign) (chicken blob)
                (chicken file posix) (chicken memory) chicken.internal srfi-1)



(define-record-type byte-blob
  (make-byte-blob object offset length )
  byte-blob?
  (object       byte-blob-object )
  (offset       byte-blob-offset )
  (length       byte-blob-length )
  )


(define (byte-blob->blob b)
  (if (zero? (byte-blob-offset b)) (byte-blob-object b)
      (let* ((origblob (byte-blob-object b))
	     (origlen  (byte-blob-length b))
	     (newblob  (make-blob origlen)))
	(move-memory! origblob newblob origlen (byte-blob-offset b) 0)
	newblob)))

(define (blob->byte-blob b)
  (and (blob? b) (make-byte-blob b 0 (blob-size b))))

(define (byte-blob-empty)
  (make-byte-blob (make-blob 0) 0 0))

(define (byte-blob-empty? b)
  (zero? (byte-blob-length b)))

(define (byte-blob-copy b  
			#!optional
			(offset (byte-blob-offset b))
			(length (byte-blob-length b)))
  (assert (and (or (positive? offset) (zero? offset))
	       (or (positive? length) (zero? length))
	       (>= (- (blob-size (byte-blob-object b)) offset) length)))
  (make-byte-blob (byte-blob-object b) offset length ))

(define (byte-blob-set! b i v)
  (let ((ob (byte-blob-object b))
	(offset (byte-blob-offset b))
	(length (byte-blob-length b)))
    (assert (and (or (zero? i) (positive? i))  (< i length)))
    (blob-set! ob (+ offset i) v)))

(define blob-uset! 
    (foreign-lambda* void ((nonnull-blob b) (integer offset) (unsigned-byte value))
#<<END
   b[offset] = value;
END
))

(define (byte-blob-uset! b i v)
  (let ((ob (byte-blob-object b))
	(offset (byte-blob-offset b))
	(length (byte-blob-length b)))
    (assert (and (or (zero? i) (positive? i))  (< i length)))
    (blob-uset! ob (+ offset i) v)))

(define blob-ref 
    (foreign-lambda* byte ((nonnull-blob b) (integer offset))
#<<END
   C_word result;
   result = b[offset];
   C_return (result);
END
))

(define (byte-blob-ref b i)
  (let ((ob (byte-blob-object b))
	(offset (byte-blob-offset b))
	(length (byte-blob-length b)))
    (assert (and (or (zero? i) (positive? i))  (< i length)))
    (blob-ref ob (+ offset i))))

(define blob-uref 
    (foreign-lambda* unsigned-byte ((nonnull-blob b) (integer offset))
#<<END
   C_word result;
   result = b[offset];
   C_return (result);
END
))

(define (byte-blob-uref b i)
  (let ((ob (byte-blob-object b))
	(offset (byte-blob-offset b))
	(length (byte-blob-length b)))
    (assert (and (or (zero? i) (positive? i))  (< i length)))
    (blob-uref ob (+ offset i))))

(define blob-set! 
    (foreign-lambda* void ((nonnull-blob b) (integer offset) (byte v))
#<<END
   b[offset] = v;
END
))

(define (list->byte-blob lst)
  (let* ((len (length lst))
	 (ob  (make-blob len)))
    (let loop ((lst lst) (i 0))
      (if (null? lst) (make-byte-blob ob 0 len)
	  (begin (blob-set! ob i (car lst))
		 (loop (cdr lst) (+ i 1)))))))
    
(define (string->byte-blob str)
  (make-byte-blob (string->blob str) 0 (string-length str)))

(define blob-fill 
    (foreign-lambda* void ((nonnull-blob b) (unsigned-int n) (integer offset) (byte value))
#<<END
   memset((void *)(b+offset),value,n);
END
))

(define (byte-blob-replicate n v)
  (assert (positive? n))
  (let* ((ob (make-blob n))
	 (bb (make-byte-blob ob 0 n)))
    (blob-fill ob n 0 v)
    bb))

;; 'blob-cons' is analogous to list cons, but of different complexity,
;; as it requires a memcpy.

(define (byte-blob-cons x b)
  (let* ((blen  (byte-blob-length b))
	 (b1len (+ 1 blen))
	 (b1    (make-blob b1len)))
    (blob-set! b1 0 x)
    (if (positive? blen) 
	(move-memory! (byte-blob-object b) b1 blen (byte-blob-offset b) 1))
    (make-byte-blob b1 0 b1len)))

(define (byte-blob-car b)
  (assert (positive? (byte-blob-length b)))
  (blob-car (byte-blob-object b) (byte-blob-offset b)))

(define blob-car 
    (foreign-primitive byte ((nonnull-blob b) (integer offset))
#<<END
   C_word result;
   result = b[offset];
   C_return (result);
END
))

(define (byte-blob-cdr b)
  (let ((n (byte-blob-length b)))
    (assert (positive? n))
    (byte-blob-copy b (+ 1 (byte-blob-offset b)) (- n 1))))

(define (byte-blob-append a . rst)
  (if (null? rst) a
      (let* ((rlen  (map byte-blob-length (cons a rst)))
	     (clen  (fold + 0 rlen))
	     (c     (make-blob clen)))
	(let loop ((pos 0) (lst (cons a rst)) (len rlen))
	  (if (null? lst) (make-byte-blob c 0 clen)
	      (let ((x (car lst))
		    (xlen (car len)))
		(move-memory! (byte-blob-object x) c xlen (byte-blob-offset x) pos)
		(loop (+ pos xlen) (cdr lst) (cdr len)))))
	)))

    

(define blob-reverse 
    (foreign-lambda* void ((nonnull-blob b) (nonnull-blob b1) (integer offset) (integer size))
#<<END
   int i,p;
   for (i=offset,p=size-1; p>=0; i++,p--)
   {
      b1[p] = b[i];
   }

   C_return (C_SCHEME_UNDEFINED);
END
))

(define (byte-blob-reverse b)
  (let* ((blen   (byte-blob-length b))
	 (ob     (byte-blob-object b))
	 (ob1    (make-blob blen)))
    (blob-reverse ob ob1 (byte-blob-offset b) blen)
    (make-byte-blob ob1 0 blen)))


(define blob-intersperse 
    (foreign-lambda* void ((nonnull-blob b) (nonnull-blob b1) (byte sep) (integer offset) (integer size))
#<<END
   int i,p,n;
   b1[0]=b[offset];
   for (i=offset+1,p=1,n=size-1; n>0; i++,p+=2,n--)
   {
      b1[p] = sep;
      b1[p+1] = b[i];
   }

   C_return (C_SCHEME_UNDEFINED);
END
))

(define (byte-blob-intersperse b x)
  (let ((blen   (byte-blob-length b)))
    (if (<= blen 1) b
	(let* ((ob     (byte-blob-object b))
	       (b1len  (- (* 2 blen) 1))
	       (ob1    (make-blob b1len)))
	  (blob-intersperse ob ob1 x (byte-blob-offset b) blen )
	  (make-byte-blob ob1 0 b1len)))))


(define (byte-blob-take b n)
  (assert (positive? n))
  (let ((blen   (byte-blob-length b)))
    (if (< blen n) b
	(let* ((ob     (byte-blob-object b))
	       (ob1    (make-blob n)))
	  (move-memory! ob ob1 n (byte-blob-offset b) 0)
	  (make-byte-blob ob1 0 n)))))
  

(define (byte-blob-drop b n)
  (if (zero? n) b
      (let ((blen   (byte-blob-length b)))
	(assert (and (positive? n) (<= n blen)))
	(byte-blob-copy b (+ n (byte-blob-offset b)) (- blen n)))))


(define (byte-blob-span b start end)
  (assert (and (or (zero? start) (positive? start)) (positive? end) (< start end)))
  (byte-blob-take (byte-blob-drop b start) (- end start)))


(define (byte-blob-map f b)
  (let* ((blen  (byte-blob-length b))
	 (ob    (byte-blob-object b))
	 (ob1   (make-blob blen)))
    (let loop ((i blen) (p (+ blen (byte-blob-offset b))))
      (if (positive? i) 
	  (let ((p (- p 1)))
	    (blob-set! ob1 p (f (blob-ref ob p)))
	    (loop (- i 1) p))
	  (make-byte-blob ob1 0 blen)))))
    

(define (byte-blob-fold-right f init b)
  (let* ((blen  (byte-blob-length b))
	 (ob    (byte-blob-object b)))
    (let loop ((i blen) (p (+ blen (byte-blob-offset b))) (ax init))
      (if (positive? i) 
	  (let ((p (- p 1)))
	    (loop (- i 1) p (f (blob-ref ob p) ax)))
	  ax))))

    
(define (byte-blob-fold-left f init b)
  (let* ((blen  (byte-blob-length b))
	 (ob    (byte-blob-object b)))
    (let loop ((i blen) (p (byte-blob-offset b))
	       (ax init))
      (if (positive? i) 
	  (loop (- i 1) (+ 1 p) (f (blob-ref ob p) ax))
	  ax))))
    
	
(define (byte-blob->list b . rest)
  (let-optionals rest ((fmap identity))
   (let loop ((b b) (ax '()))
     (cond ((byte-blob-empty? b) (reverse ax))
	   (else  (loop (byte-blob-cdr b) (cons (fmap (byte-blob-car b)) ax)))))))
	 
(define (byte-blob->string b)
  (assert (byte-blob? b))
  (let* ([n (byte-blob-length b)]
	 [s (make-string n)] )
    (move-memory! (byte-blob-object b) s n (byte-blob-offset b) 0)
    s))


;; Error handling: the C routines below never raise Scheme exceptions
;; themselves. Instead, they record an error condition in static
;; variables, and the Scheme wrappers check the flag after each foreign
;; call and raise a proper Scheme exception if an error is pending.

#>

#include <unistd.h>

static int blob_error_pending = 0;
static char blob_error_msg[256];

static void blob_set_error (const char *msg)
{
  blob_error_pending = 1;
  strncpy(blob_error_msg, msg, 255);
  blob_error_msg[255] = 0;
}

<#

(define blob-error-pending?
  (foreign-lambda* bool () "C_return(blob_error_pending);"))

(define blob-error-message
  (foreign-lambda* c-string () "C_return(blob_error_msg);"))

(define blob-clear-error!
  (foreign-lambda* void () "blob_error_pending = 0;"))

(define (blob-check-error! loc)
  (when (blob-error-pending?)
    (let ((msg (blob-error-message)))
      (blob-clear-error!)
      (error loc msg))))



(define blob-read
    (foreign-lambda* int ((integer fd) (nonnull-blob b) (integer n) )
#<<END
     ssize_t s;

     if ( (s = read(fd,b,n)) == -1 )
     {
          blob_set_error("read I/O error in byte-blob-read");
          C_return(-1);
     }
     C_return(s);
END
))


(define (byte-blob-read port n)
  (let ((ob (make-blob n)))
    (let ((s (blob-read (port->fileno port) ob n)))
      (blob-check-error! 'byte-blob-read)
      (if (positive? s)
	  (make-byte-blob ob 0 s)
	  #!eof))))

	
(define (file->byte-blob filename #!optional mode)
  (let ((filesize (file-size filename)))
    (if mode
        (call-with-input-file filename
          (lambda (port) (byte-blob-read port filesize))
          mode)
        (call-with-input-file filename
          (lambda (port) (byte-blob-read port filesize)))
        ))
  )


(define blob-write
    (foreign-lambda* void ((integer fd) (nonnull-blob b) (integer size) (integer offset))
#<<END
     ssize_t s,n;

     n = s = 0; 
     while (n < size)
     {
	  if ( (s = write(fd,(const void *)(b+n+offset),size-n)) == -1 )
	  {
	       blob_set_error("write I/O error in byte-blob-write");
	       C_return(C_SCHEME_UNDEFINED);
	  }
	  n += s;
     }
     C_return(C_SCHEME_UNDEFINED);
END
))

(define (byte-blob-write port b)
  (let ((ob (byte-blob-object b))
	(n  (byte-blob-length b))
	(offset (byte-blob-offset b)))
    (blob-write (port->fileno port) ob n offset)
    (blob-check-error! 'byte-blob-write)))


;; code adapted from srfi-4.scm:
;;
;; In CHICKEN 6, u8vectors are represented directly as bytevectors,
;; whereas the other typed vectors are structures that wrap a
;; bytevector in their second slot.

(define (pack-copy tag loc)
  (lambda (v)
    (cond ((eq? tag 'u8vector)
	   (##sys#check-blob v loc)
	   (make-byte-blob v 0 (##sys#size v)))
	  (else
	   (##sys#check-structure v tag loc)
	   (let* ((old (##sys#slot v 1))
		  (n   (##sys#size old))
		  (new (##sys#make-bytevector n)))
	     (move-memory! old new)
	     (make-byte-blob new 0 n))))))

(define u8vector->byte-blob (pack-copy 'u8vector 'u8vector->byte-blob))
(define s8vector->byte-blob (pack-copy 's8vector 's8vector->byte-blob))
(define u16vector->byte-blob (pack-copy 'u16vector 'u16vector->byte-blob))
(define s16vector->byte-blob (pack-copy 's16vector 's16vector->byte-blob))
(define u32vector->byte-blob (pack-copy 'u32vector 'u32vector->byte-blob))
(define s32vector->byte-blob (pack-copy 's32vector 's32vector->byte-blob))
(define f32vector->byte-blob (pack-copy 'f32vector 'f32vector->byte-blob))
(define f64vector->byte-blob (pack-copy 'f64vector 'f64vector->byte-blob))


(define (unpack-copy tag sz loc)
  (lambda (bb)
    (let ((str (byte-blob-object bb))
	  (offset (byte-blob-offset bb)))
      (cond ((eq? tag 'u8vector)
	     (##sys#check-blob str loc)
	     (let ((len (byte-blob-length bb)))
	       (if (or (zero? offset)
		       (= len (##sys#size str)))
		   str
		   (let ((new (##sys#make-bytevector len)))
		     (move-memory! str new len offset 0)
		     new))))
	    (else
	     (##sys#check-blob str loc)
	     (let* ((len (byte-blob-length bb))
		    (new (##sys#make-bytevector len)))
	       (if (or (eq? #t sz)
		       (eq? 0 (##core#inline "C_fixnum_modulo" len sz)))
		   (begin
		     (move-memory! str new len offset) 
		     (##sys#make-structure tag new))
		   (##sys#error loc "blob does not have correct size for packing" tag len sz) )))))))


(define byte-blob->u8vector (unpack-copy 'u8vector #t 'byte-blob->u8vector))
(define byte-blob->s8vector (unpack-copy 's8vector #t 'byte-blob->s8vector))
(define byte-blob->u16vector (unpack-copy 'u16vector 2 'byte-blob->u16vector))
(define byte-blob->s16vector (unpack-copy 's16vector 2 'byte-blob->s16vector))
(define byte-blob->u32vector (unpack-copy 'u32vector 4 'byte-blob->u32vector))
(define byte-blob->s32vector (unpack-copy 's32vector 4 'byte-blob->s32vector))
(define byte-blob->f32vector (unpack-copy 'f32vector 4 'byte-blob->f32vector))
(define byte-blob->f64vector (unpack-copy 'f64vector 8 'byte-blob->f64vector))

;;
;;
;; Fast sub-sequence search, based on work by Boyer, Moore, Horspool,
;; Sunday, and Lundh.
;;
;; Based on code from the Haskell text library by Tom Harper and Bryan
;; O'Sullivan. http://hackage.haskell.org/package/text
;;
;;
;; References:
;; 
;; * R. S. Boyer, J. S. Moore: A Fast String Searching Algorithm.
;;   Communications of the ACM, 20, 10, 762-772 (1977)
;;
;; * R. N. Horspool: Practical Fast Searching in Strings.  Software -
;;   Practice and Experience 10, 501-506 (1980)
;;
;; * D. M. Sunday: A Very Fast Substring Search Algorithm.
;;   Communications of the ACM, 33, 8, 132-142 (1990)
;;
;; * F. Lundh: The Fast Search Algorithm.
;;   <http://effbot.org/zone/stringlib.htm> (2006)
;;
;; From http://effbot.org/zone/stringlib.htm:
;;
;; When designing the new algorithm, I used the following constraints:
;;
;;     * should be faster than the current brute-force algorithm for
;;       all test cases (based on real-life code), including Jim
;;       Hugunin’s worst-case test
;;
;;     * small setup overhead; no dynamic allocation in the fast path
;;       (O(m) for speed, O(1) for storage)
;;
;;     * sublinear search behaviour in good cases (O(n/m))
;;
;;     * no worse than the current algorithm in worst case (O(nm))
;;
;;     * should work well for both 8-bit strings and 16-bit or 32-bit
;;       Unicode strings (no O(σ) dependencies)
;;
;;     * many real-life searches should be good, very few should be
;;       worst case
;;
;;     * reasonably simple implementation 
;;
;;  This rules out most standard algorithms (Knuth-Morris-Pratt is not
;;  sublinear, Boyer-Moore needs tables that depend on both the
;;  alphabet size and the pattern size, most Boyer-Moore variants need
;;  tables that depend on the pattern size, etc.).
;;
;;  After some tweaking, I came up with a simplication of Boyer-Moore,
;;  incorporating ideas from Horspool and Sunday. Here’s an outline:
;;
;; def find(s, p):
;;     # find first occurrence of p in s
;;     n = len(s)
;;     m = len(p)
;;     skip = delta1(p)[p[m-1]]
;;     i = 0
;;     while i <= n-m:
;;         if s[i+m-1] == p[m-1]: # (boyer-moore)
;;             # potential match
;;             if s[i:i+m-1] == p[:m-1]:
;;                 return i
;;             if s[i+m] not in p:
;;                 i = i + m + 1 # (sunday)
;;             else:
;;                 i = i + skip # (horspool)
;;         else:
;;             # skip
;;             if s[i+m] not in p:
;;                 i = i + m + 1 # (sunday)
;;             else:
;;                 i = i + 1
;;     return -1 # not found
;;
;; The delta1(p)[p[m-1]] value is simply the Boyer-Moore delta1 (or
;; bad-character skip) value for the last character in the pattern.
;;
;; For the s[i+m] not in p test, I use a 32-bit bitmask, using the 5
;; least significant bits of the character as the key. This could be
;; described as a simple Bloom filter.
;;
;; Note that the above Python code may access s[n], which would result in
;; an IndexError exception. For the CPython implementation, this is not
;; really a problem, since CPython adds trailing NULL entries to both
;; 8-bit and Unicode strings.  

;;
;; /O(n+m)/ Find the offsets of all non-overlapping indices of
;; needle within haystack.
;;
;; In (unlikely) bad cases, this algorithm's complexity degrades
;; towards /O(n*m)/.
;;



(define swizzle
    (foreign-lambda* unsigned-int ((unsigned-int k))
#<<END
     unsigned int result;

     result = (k & 0x1F);

     C_return(result);
END
))


(define initmask
    (foreign-lambda* void ((blob m))
#<<END
    memset (m, 0, 4);
END
))

(define setbit!
    (foreign-lambda* void ((blob m) (unsigned-int i))
#<<END
     unsigned int w;
     w = i / 8;

     m[w] = m[w] | ((1 << i) >> (8*w));
END
))

(define bitset? 
    (foreign-lambda* bool ((blob m) (unsigned-int i))
#<<END
     unsigned int w, result;
     w = i / 8;

     result = m[w] & ((1 << i) >> (8*w));
END
))
  



(define (make-table nlast nindex nlen z)
  (lambda (i msk skp)
    (let loop ((i i) (msk msk) (skp skp))
      (cond ((>= i nlast)  
             (begin (setbit! msk (swizzle z))
                    (values msk skp)))
	    (else         
	     (let* ((c    (nindex i))
		    (skp1 (cond ((= c z)  (- nlen i 2))
				(else     skp))))
               (setbit! msk (swizzle c))
	       (loop (+ 1 i) msk skp1)))
            ))
    ))

	      
(define (scan1 hindex hlen c)
  (let loop ((i 0) (ax '()))
    (cond ((>= i hlen)        (reverse ax))
	  ((= (hindex i) c)   (loop (+ 1 i) (cons i ax)))
	  (else               (loop (+ 1 i) ax)))))


(define (scan nindex hindex nlast nlen ldiff z mask skip i)

  (define (candidate-match i j)
    (cond ((>= j nlast)  #t)
	  ((not (= (hindex (+ i j)) (nindex j)))  #f)
	  (else (candidate-match i (+ 1 j)))))

  (let loop ((i i) (ax '()))

    (if (> i ldiff)   (reverse ax)

    (let ((c (hindex (+ i nlast))))
      (cond 
	    ;;
	    ((and (= c z) (candidate-match i 0))
	     (loop (+ i nlen) (cons i ax)))
	    ;;
	    (else
	     (let* ((next-in-pattern?
		     (not (bitset? mask (swizzle (hindex (+ i nlen))))))
		    (delta (cond (next-in-pattern? (+ 1 nlen))
				 ((= c z)  (+ 1 skip))
				 (else     1))))
	       (loop (+ i delta) ax))))))))


(define (subsequence-search needle haystack)
  (let ((nobj  (byte-blob-object needle))
	(noff  (byte-blob-offset needle))
	(nlen  (byte-blob-length needle))
	(hobj  (byte-blob-object haystack))
	(hoff  (byte-blob-offset haystack))
	(hlen  (byte-blob-length haystack)))
    (let* ((nindex   (lambda (k) (blob-ref nobj (+ noff k))))
	   (hindex   (lambda (k) (blob-ref hobj (+ hoff k))))
	   (ldiff    (- hlen nlen))
	   (nlast    (- nlen 1))
	   (z        (nindex nlast))
	   (tbl      (make-table nlast nindex nlen z))
           (m        (make-blob 4))
           )
      (initmask m)
      (let-values 
       (((mask skip)  (tbl 0 m (- nlen 2))))
       (cond ((= 1 nlen)  
	      (scan1 hindex hlen (nindex 0)))
	     ((or (<= nlen 0) (negative? ldiff))  
	      '())
	     (else
	      (scan nindex hindex nlast nlen ldiff z mask skip 0)))))))

;;
;; Based on code from the Haskell text library by Tom Harper and Bryan
;; O'Sullivan. http://hackage.haskell.org/package/text
;;
;;    /O(n+m)/ Find all non-overlapping instances of needle in
;;  haystack.  The first element of the returned pair is the prefix
;;  of haystack prior to any matches of needle.  The second is a
;;  list of pairs.
;;
;;  The first element of each pair in the list is a span from the
;;  beginning of a match to the beginning of the next match, while the
;;  second is a span from the beginning of the match to the end of the
;;  input.
;;
;;  Examples:
;;
;;  > find "::" ""
;;  > ==> ("", [])
;;  > find "/" "a/b/c/d"
;;  > ==> ("a", [("/b","/b/c/d"), ("/c","/c/d"), ("/d","/d")])
;;
;;  In (unlikely) bad cases, this function's time complexity degrades
;;  towards /O(n*m)/.

;; find :: Text * Text -> (Text, [(Text, Text)])

(define (byte-blob-find needle haystack)
  (cond ((byte-blob-empty? needle)  
	 (error 'find "empty pattern" needle))
	(else 
	 (let ((r (subsequence-search needle haystack)))
	   (cond ((null? r)  
		  (list haystack '()))
		 (else       
		  (let* ((hoff  (byte-blob-offset haystack))
			 (hlen  (byte-blob-length haystack))
			 (chunk (lambda (n l) (byte-blob-copy haystack (+ hoff n) l)))
			 (go    (lambda (s xs)
				  (let loop ((s s) (xs xs) (ax '()))
				    (if (null? xs)
					(let ((c (chunk s (- hlen s))))
					  (reverse (cons (list c c) ax)))
					(let ((x (car xs)) (xs (cdr xs)))
					  (loop x xs
						(cons (list (chunk s (- x s)) 
							    (chunk s (- hlen s))) 
						      ax)))))))
			 )
		    (list (chunk 0 (car r))
			  (go (car r) (cdr r)))))))
	 )))



)
