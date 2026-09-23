;;
;;  Utility procedures for manipulating byte sequences.
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

(module byte-sequence

	(byte-sequence?
	 byte-sequence-empty?
	 byte-sequence-length
	 byte-sequence-empty
	 bytevector->byte-sequence
	 list->byte-sequence
	 string->byte-sequence
	 file->byte-sequence
         byte-sequence-replicate
	 byte-sequence->bytevector
	 byte-sequence-offset
	 byte-sequence-cons 
	 byte-sequence-car
	 byte-sequence-cdr
	 byte-sequence-ref
	 byte-sequence-uref
	 byte-sequence-set!
	 byte-sequence-uset!
	 byte-sequence-append
	 byte-sequence-reverse
	 byte-sequence-intersperse
	 byte-sequence-take
	 byte-sequence-drop
         byte-sequence-span
	 byte-sequence-map
	 byte-sequence-fold-left
	 byte-sequence-fold-right
	 byte-sequence-find
	 byte-sequence->list
	 byte-sequence->string
	 byte-sequence-read
	 byte-sequence-write

	 byte-sequence=?
	 byte-sequence-compare
	 byte-sequence-singleton
	 byte-sequence-snoc
	 byte-sequence-last
	 byte-sequence-init
	 byte-sequence-uncons
	 byte-sequence-unsnoc

	 byte-sequence-index-maybe
	 byte-sequence-elem-index
	 byte-sequence-elem-index-end
	 byte-sequence-elem-indices
	 byte-sequence-find-index
	 byte-sequence-find-index-end
	 byte-sequence-find-indices
	 byte-sequence-count

	 byte-sequence-split-at
	 byte-sequence-take-end
	 byte-sequence-drop-end
	 byte-sequence-take-while
	 byte-sequence-drop-while
	 byte-sequence-take-while-end
	 byte-sequence-drop-while-end
	 byte-sequence-span-while
	 byte-sequence-break-while
	 byte-sequence-span-while-end
	 byte-sequence-break-while-end
	 byte-sequence-strip-prefix
	 byte-sequence-strip-suffix
	 byte-sequence-is-prefix-of?
	 byte-sequence-is-suffix-of?

	 byte-sequence->file
	 byte-sequence-valid-utf8?

	 u8vector->byte-sequence  
	 s8vector->byte-sequence  
	 u16vector->byte-sequence 
	 s16vector->byte-sequence 
	 u32vector->byte-sequence 
	 s32vector->byte-sequence 
	 f32vector->byte-sequence 
	 f64vector->byte-sequence 
	 
	 byte-sequence->u8vector  
	 byte-sequence->s8vector  
	 byte-sequence->u16vector 
	 byte-sequence->s16vector 
	 byte-sequence->u32vector 
	 byte-sequence->s32vector 
	 byte-sequence->f32vector 
	 byte-sequence->f64vector 
	 )

	(import scheme (chicken base) (chicken foreign) (chicken bytevector)
                (chicken file posix) (chicken memory) (chicken fixnum)
                chicken.internal srfi-1)



(define-record-type byte-sequence
  (make-byte-sequence object offset length )
  byte-sequence?
  (object       byte-sequence-object )
  (offset       byte-sequence-offset )
  (length       byte-sequence-length )
  )


(define (byte-sequence->bytevector b)
  (if (zero? (byte-sequence-offset b)) (byte-sequence-object b)
      (let* ((origbv (byte-sequence-object b))
	     (origlen  (byte-sequence-length b))
	     (newbv  (make-bytevector origlen)))
	(move-memory! origbv newbv origlen (byte-sequence-offset b) 0)
	newbv)))

(define (bytevector->byte-sequence b)
  (and (bytevector? b) (make-byte-sequence b 0 (bytevector-length b))))

(define (byte-sequence-empty)
  (make-byte-sequence (make-bytevector 0) 0 0))

(define (byte-sequence-empty? b)
  (zero? (byte-sequence-length b)))

(define (byte-sequence-copy b  
			#!optional
			(offset (byte-sequence-offset b))
			(length (byte-sequence-length b)))
  (assert (and (or (positive? offset) (zero? offset))
	       (or (positive? length) (zero? length))
	       (>= (- (bytevector-length (byte-sequence-object b)) offset) length)))
  (make-byte-sequence (byte-sequence-object b) offset length ))

(define (byte-sequence-set! b i v)
  (let ((ob (byte-sequence-object b))
	(offset (byte-sequence-offset b))
	(length (byte-sequence-length b)))
    (assert (and (or (zero? i) (positive? i))  (< i length)))
    (bv-set! ob (+ offset i) v)))

(define bv-uset! 
    (foreign-lambda* void ((nonnull-bytevector b) (integer offset) (unsigned-byte value))
#<<END
   b[offset] = value;
END
))

(define (byte-sequence-uset! b i v)
  (let ((ob (byte-sequence-object b))
	(offset (byte-sequence-offset b))
	(length (byte-sequence-length b)))
    (assert (and (or (zero? i) (positive? i))  (< i length)))
    (bv-uset! ob (+ offset i) v)))

(define bv-ref 
    (foreign-lambda* byte ((nonnull-bytevector b) (integer offset))
#<<END
   C_word result;
   result = b[offset];
   C_return (result);
END
))

(define (byte-sequence-ref b i)
  (let ((ob (byte-sequence-object b))
	(offset (byte-sequence-offset b))
	(length (byte-sequence-length b)))
    (assert (and (or (zero? i) (positive? i))  (< i length)))
    (bv-ref ob (+ offset i))))

(define bv-uref 
    (foreign-lambda* unsigned-byte ((nonnull-bytevector b) (integer offset))
#<<END
   C_word result;
   result = b[offset];
   C_return (result);
END
))

(define (byte-sequence-uref b i)
  (let ((ob (byte-sequence-object b))
	(offset (byte-sequence-offset b))
	(length (byte-sequence-length b)))
    (assert (and (or (zero? i) (positive? i))  (< i length)))
    (bv-uref ob (+ offset i))))

(define bv-set! 
    (foreign-lambda* void ((nonnull-bytevector b) (integer offset) (byte v))
#<<END
   b[offset] = v;
END
))

(define (list->byte-sequence lst)
  (let* ((len (length lst))
	 (ob  (make-bytevector len)))
    (let loop ((lst lst) (i 0))
      (if (null? lst) (make-byte-sequence ob 0 len)
	  (begin (bv-set! ob i (car lst))
		 (loop (cdr lst) (+ i 1)))))))
    
(define (string->byte-sequence str)
  (make-byte-sequence (string->latin1 str) 0 (string-length str)))

(define bv-fill 
    (foreign-lambda* void ((nonnull-bytevector b) (unsigned-int n) (integer offset) (byte value))
#<<END
   memset((void *)(b+offset),value,n);
END
))

(define (byte-sequence-replicate n v)
  (assert (positive? n))
  (let* ((ob (make-bytevector n))
	 (bb (make-byte-sequence ob 0 n)))
    (bv-fill ob n 0 v)
    bb))

;; 'byte-sequence-cons' is analogous to list cons, but of different complexity,
;; as it requires a memcpy.

(define (byte-sequence-cons x b)
  (let* ((blen  (byte-sequence-length b))
	 (b1len (+ 1 blen))
	 (b1    (make-bytevector b1len)))
    (bv-set! b1 0 x)
    (if (positive? blen) 
	(move-memory! (byte-sequence-object b) b1 blen (byte-sequence-offset b) 1))
    (make-byte-sequence b1 0 b1len)))

(define (byte-sequence-car b)
  (assert (positive? (byte-sequence-length b)))
  (bv-car (byte-sequence-object b) (byte-sequence-offset b)))

(define bv-car 
    (foreign-primitive byte ((nonnull-bytevector b) (integer offset))
#<<END
   C_word result;
   result = b[offset];
   C_return (result);
END
))

(define (byte-sequence-cdr b)
  (let ((n (byte-sequence-length b)))
    (assert (positive? n))
    (byte-sequence-copy b (+ 1 (byte-sequence-offset b)) (- n 1))))


;; Content-based comparison of the byte ranges of two byte-sequences,
;; using memcmp over the overlapping prefix of the slices.

(define bv-compare-bytes
    (foreign-lambda* int ((nonnull-bytevector b1) (integer off1) (nonnull-bytevector b2) (integer off2) (integer n))
#<<END
   C_return (memcmp ((const void *)(b1+off1), (const void *)(b2+off2), n));
END
))

;; /O(n)/ Lexicographic comparison of two byte-sequences. Returns -1, 0, or
;; +1 as the first argument is less than, equal to, or greater than the
;; second.

(define (byte-sequence-compare a b)
  (let* ((alen (byte-sequence-length a))
	 (blen (byte-sequence-length b))
	 (n    (min alen blen)))
    (cond ((zero? n)  (cond ((< alen blen) -1)
			    ((> alen blen) 1)
			    (else 0)))
	  (else
	     (let ((r (bv-compare-bytes (byte-sequence-object a) (byte-sequence-offset a)
					  (byte-sequence-object b) (byte-sequence-offset b) n)))
	       (cond ((not (zero? r)) (if (negative? r) -1 1))
		     ((< alen blen)   -1)
		     ((> alen blen)   1)
		     (else            0)))))))

;; /O(n)/ Returns #t if the two byte-sequences contain the same sequence of
;; bytes, and #f otherwise.

(define (byte-sequence=? a b)
  (zero? (byte-sequence-compare a b)))

(define (byte-sequence-singleton v)
  (let ((ob (make-bytevector 1)))
    (bv-set! ob 0 v)
    (make-byte-sequence ob 0 1)))

(define (byte-sequence-append a . rst)
  (if (null? rst) a
      (let* ((rlen  (map byte-sequence-length (cons a rst)))
	     (clen  (fold + 0 rlen))
	     (c     (make-bytevector clen)))
	(let loop ((pos 0) (lst (cons a rst)) (len rlen))
	  (if (null? lst) (make-byte-sequence c 0 clen)
	      (let ((x (car lst))
		    (xlen (car len)))
		(move-memory! (byte-sequence-object x) c xlen (byte-sequence-offset x) pos)
		(loop (+ pos xlen) (cdr lst) (cdr len)))))
	)))


;; 'byte-sequence-snoc' appends a byte to the end of a byte-sequence; like
;; byte-sequence-cons, it requires a memcpy.

(define (byte-sequence-snoc b x)
  (let* ((blen  (byte-sequence-length b))
	 (b1len (+ 1 blen))
	 (b1    (make-bytevector b1len)))
    (if (positive? blen) 
	(move-memory! (byte-sequence-object b) b1 blen (byte-sequence-offset b) 0))
    (bv-set! b1 blen x)
    (make-byte-sequence b1 0 b1len)))

;; Returns the last byte of a non-empty byte-sequence; raises an error on
;; an empty byte-sequence.

(define (byte-sequence-last b)
  (assert (positive? (byte-sequence-length b)))
  (bv-car (byte-sequence-object b) (+ (byte-sequence-offset b) (- (byte-sequence-length b) 1))))

;; Returns a byte-sequence containing all but the last byte of b; the
;; result is undefined if b is empty.

(define (byte-sequence-init b)
  (let ((n (byte-sequence-length b)))
    (assert (positive? n))
    (byte-sequence-copy b (byte-sequence-offset b) (- n 1))))

;; Returns two values, the first byte and the remainder of a non-empty
;; byte-sequence, or #f if the byte-sequence is empty.

(define (byte-sequence-uncons b)
  (if (byte-sequence-empty? b) #f
      (values (byte-sequence-car b) (byte-sequence-cdr b))))

;; Returns two values, all but the last byte and the last byte of a
;; non-empty byte-sequence, or #f if the byte-sequence is empty.

(define (byte-sequence-unsnoc b)
  (if (byte-sequence-empty? b) #f
      (values (byte-sequence-init b) (byte-sequence-last b))))

    
(define bv-reverse 
    (foreign-lambda* void ((nonnull-bytevector b) (nonnull-bytevector b1) (integer offset) (integer size))
#<<END
   int i,p;
   for (i=offset,p=size-1; p>=0; i++,p--)
   {
      b1[p] = b[i];
   }

   C_return (C_SCHEME_UNDEFINED);
END
))

(define (byte-sequence-reverse b)
  (let* ((blen   (byte-sequence-length b))
	 (ob     (byte-sequence-object b))
	 (ob1    (make-bytevector blen)))
    (bv-reverse ob ob1 (byte-sequence-offset b) blen)
    (make-byte-sequence ob1 0 blen)))


(define bv-intersperse 
    (foreign-lambda* void ((nonnull-bytevector b) (nonnull-bytevector b1) (byte sep) (integer offset) (integer size))
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

(define (byte-sequence-intersperse b x)
  (let ((blen   (byte-sequence-length b)))
    (if (<= blen 1) b
	(let* ((ob     (byte-sequence-object b))
	       (b1len  (- (* 2 blen) 1))
	       (ob1    (make-bytevector b1len)))
	  (bv-intersperse ob ob1 x (byte-sequence-offset b) blen )
	  (make-byte-sequence ob1 0 b1len)))))


(define (byte-sequence-take b n)
  (assert (positive? n))
  (let ((blen   (byte-sequence-length b)))
    (if (< blen n) b
	(let* ((ob     (byte-sequence-object b))
	       (ob1    (make-bytevector n)))
	  (move-memory! ob ob1 n (byte-sequence-offset b) 0)
	  (make-byte-sequence ob1 0 n)))))
  

(define (byte-sequence-drop b n)
  (if (zero? n) b
      (let ((blen   (byte-sequence-length b)))
	(assert (and (positive? n) (<= n blen)))
	(byte-sequence-copy b (+ n (byte-sequence-offset b)) (- blen n)))))


(define (byte-sequence-span b start end)
  (assert (and (or (zero? start) (positive? start)) (positive? end) (< start end)))
  (byte-sequence-take (byte-sequence-drop b start) (- end start)))


;; /O(n)/ Returns two values, the prefix of b of length n (or all of b
;; if n is greater than the length of b) and the remainder, as
;; byte-sequences.

(define (byte-sequence-split-at b n)
  (values (byte-sequence-take b n)
	  (if (< (byte-sequence-length b) n) (byte-sequence-empty) (byte-sequence-drop b n))))

;; /O(n)/ Returns the suffix of b of length n, or all of b if n is
;; greater than the length of b.

(define (byte-sequence-take-end b n)
  (let ((blen (byte-sequence-length b)))
    (if (< blen n) b
	(byte-sequence-copy b (+ (- blen n) (byte-sequence-offset b)) n))))

;; /O(n)/ Returns b with its last n bytes removed, or the empty
;; byte-sequence if n is greater than the length of b.

(define (byte-sequence-drop-end b n)
  (let ((blen (byte-sequence-length b)))
    (if (< blen n) (byte-sequence-empty)
	(byte-sequence-copy b (byte-sequence-offset b) (- blen n)))))


;; /O(n)/ Returns the longest prefix of b whose bytes all satisfy the
;; predicate f.

(define (byte-sequence-take-while f b)
  (let* ((blen  (byte-sequence-length b))
	 (ob    (byte-sequence-object b)))
    (let loop ((i 0) (p (byte-sequence-offset b)))
      (cond ((fx>= i blen) (byte-sequence-copy b (byte-sequence-offset b) blen))
	    ((f (bv-uref ob p)) (loop (+ 1 i) (+ 1 p)))
	    (else (byte-sequence-copy b (byte-sequence-offset b) i))))))

;; /O(n)/ Returns b with the longest prefix whose bytes all satisfy the
;; predicate f removed.

(define (byte-sequence-drop-while f b)
  (let* ((blen  (byte-sequence-length b))
	 (ob    (byte-sequence-object b)))
    (let loop ((i 0) (p (byte-sequence-offset b)))
      (cond ((fx>= i blen) (byte-sequence-empty))
	    ((f (bv-uref ob p)) (loop (+ 1 i) (+ 1 p)))
	    (else (byte-sequence-copy b (+ i (byte-sequence-offset b)) (- blen i)))))))

;; /O(n)/ Returns the longest suffix of b whose bytes all satisfy the
;; predicate f.

(define (byte-sequence-take-while-end f b)
  (byte-sequence-reverse (byte-sequence-take-while f (byte-sequence-reverse b))))

;; /O(n)/ Returns b with its longest suffix whose bytes all satisfy the
;; predicate f removed.

(define (byte-sequence-drop-while-end f b)
  (byte-sequence-reverse (byte-sequence-drop-while f (byte-sequence-reverse b))))

;; /O(n)/ Returns two values, the longest prefix of b whose bytes all
;; satisfy the predicate f, and the remainder.

(define (byte-sequence-span-while f b)
  (let ((pre (byte-sequence-take-while f b)))
    (values pre (byte-sequence-drop b (byte-sequence-length pre)))))

;; /O(n)/ Returns two values, the longest prefix of b whose bytes all
;; fail the predicate f, and the remainder.

(define (byte-sequence-break-while f b)
  (byte-sequence-span-while (lambda (x) (not (f x))) b))

;; /O(n)/ Returns two values, the longest suffix of b whose bytes all
;; satisfy the predicate f, and the rest.

(define (byte-sequence-span-while-end f b)
  (let ((pre (byte-sequence-take-while-end f b)))
    (values (byte-sequence-drop-end b (byte-sequence-length pre)) pre)))

;; /O(n)/ Returns two values, the longest suffix of b whose bytes all
;; fail the predicate f, and the rest.

(define (byte-sequence-break-while-end f b)
  (byte-sequence-span-while-end (lambda (x) (not (f x))) b))


;; /O(n)/ Returns the remainder of b without its first n bytes, if the
;; first n bytes of b match prefix, or #f otherwise.

(define (byte-sequence-strip-prefix prefix b)
  (let ((n (byte-sequence-length prefix)))
    (if (and (<= n (byte-sequence-length b))
	     (zero? (bv-compare-bytes
		     (byte-sequence-object prefix) (byte-sequence-offset prefix)
		     (byte-sequence-object b) (byte-sequence-offset b) n)))
	(byte-sequence-copy b (+ n (byte-sequence-offset b)) (- (byte-sequence-length b) n))
	#f)))

;; /O(n)/ Returns the prefix of b without its last n bytes, if its last
;; n bytes match suffix, or #f otherwise.

(define (byte-sequence-strip-suffix suffix b)
  (let ((n (byte-sequence-length suffix)))
    (if (and (<= n (byte-sequence-length b))
	     (zero? (bv-compare-bytes
		     (byte-sequence-object suffix) (byte-sequence-offset suffix)
		     (byte-sequence-object b)
		     (+ (- (byte-sequence-length b) n) (byte-sequence-offset b)) n)))
	(byte-sequence-copy b (byte-sequence-offset b) (- (byte-sequence-length b) n))
	#f)))

;; /O(n)/ Returns #t if prefix is a prefix of b, and #f otherwise.

(define (byte-sequence-is-prefix-of? prefix b)
  (and (byte-sequence-strip-prefix prefix b) #t))

;; /O(n)/ Returns #t if suffix is a suffix of b, and #f otherwise.

(define (byte-sequence-is-suffix-of? suffix b)
  (and (byte-sequence-strip-suffix suffix b) #t))


(define (byte-sequence-map f b)
  (let* ((blen  (byte-sequence-length b))
	 (ob    (byte-sequence-object b))
	 (ob1   (make-bytevector blen)))
    (let loop ((i blen) (p (+ blen (byte-sequence-offset b))))
      (if (positive? i) 
	  (let ((p (- p 1)))
	    (bv-set! ob1 p (f (bv-ref ob p)))
	    (loop (- i 1) p))
	  (make-byte-sequence ob1 0 blen)))))
    

(define (byte-sequence-fold-right f init b)
  (let* ((blen  (byte-sequence-length b))
	 (ob    (byte-sequence-object b)))
    (let loop ((i blen) (p (+ blen (byte-sequence-offset b))) (ax init))
      (if (positive? i) 
	  (let ((p (- p 1)))
	    (loop (- i 1) p (f (bv-ref ob p) ax)))
	  ax))))

    
(define (byte-sequence-fold-left f init b)
  (let* ((blen  (byte-sequence-length b))
	 (ob    (byte-sequence-object b)))
    (let loop ((i blen) (p (byte-sequence-offset b))
	       (ax init))
      (if (positive? i) 
	  (loop (- i 1) (+ 1 p) (f (bv-ref ob p) ax))
	  ax))))
    
;;
;; Searching by equality and by predicate. All procedures in this
;; section index into the underlying bytevector with bv-uref, so the byte
;; values are interpreted as unsigned octets.
;;

;; Returns the value of the byte at index i, or #f if i is out of
;; range.

(define (byte-sequence-index-maybe b i)
  (let ((blen (byte-sequence-length b)))
    (and (or (zero? i) (positive? i)) (< i blen)
	 (bv-uref (byte-sequence-object b) (+ i (byte-sequence-offset b))))))

;; /O(n)/ Returns the index of the first occurrence of byte c in b, or
;; #f if b does not contain c.

(define (byte-sequence-elem-index c b)
  (let ((blen (byte-sequence-length b))
	(ob   (byte-sequence-object b)))
    (let loop ((i 0) (p (byte-sequence-offset b)))
      (cond ((fx>= i blen) #f)
	    ((fx= (bv-uref ob p) c) i)
	    (else (loop (+ 1 i) (+ 1 p)))))))

;; /O(n)/ Returns the index of the last occurrence of byte c in b, or
;; #f if b does not contain c.

(define (byte-sequence-elem-index-end c b)
  (let ((blen (byte-sequence-length b))
	(ob   (byte-sequence-object b)))
    (let loop ((i (- blen 1)) (p (+ (- blen 1) (byte-sequence-offset b))))
      (cond ((negative? i) #f)
	    ((fx= (bv-uref ob p) c) i)
	    (else (loop (- i 1) (- p 1)))))))

;; /O(n)/ Returns a list of the indices of all occurrences of byte c
;; in b, in increasing order.

(define (byte-sequence-elem-indices c b)
  (let ((blen (byte-sequence-length b))
	(ob   (byte-sequence-object b)))
    (let loop ((i 0) (p (byte-sequence-offset b)) (ax '()))
      (cond ((fx>= i blen) (reverse ax))
	    ((fx= (bv-uref ob p) c) (loop (+ 1 i) (+ 1 p) (cons i ax)))
	    (else (loop (+ 1 i) (+ 1 p) ax))))))

;; /O(n)/ Returns the index of the first byte in b that satisfies the
;; predicate f, or #f if no byte does.

(define (byte-sequence-find-index f b)
  (let ((blen (byte-sequence-length b))
	(ob   (byte-sequence-object b)))
    (let loop ((i 0) (p (byte-sequence-offset b)))
      (cond ((fx>= i blen) #f)
	    ((f (bv-uref ob p)) i)
	    (else (loop (+ 1 i) (+ 1 p)))))))

;; /O(n)/ Returns the index of the last byte in b that satisfies the
;; predicate f, or #f if no byte does.

(define (byte-sequence-find-index-end f b)
  (let ((blen (byte-sequence-length b))
	(ob   (byte-sequence-object b)))
    (let loop ((i (- blen 1)) (p (+ (- blen 1) (byte-sequence-offset b))))
      (cond ((negative? i) #f)
	    ((f (bv-uref ob p)) i)
	    (else (loop (- i 1) (- p 1)))))))

;; /O(n)/ Returns a list of the indices of all bytes in b that satisfy
;; the predicate f, in increasing order.

(define (byte-sequence-find-indices f b)
  (let ((blen (byte-sequence-length b))
	(ob   (byte-sequence-object b)))
    (let loop ((i 0) (p (byte-sequence-offset b)) (ax '()))
      (cond ((fx>= i blen) (reverse ax))
	    ((f (bv-uref ob p)) (loop (+ 1 i) (+ 1 p) (cons i ax)))
	    (else (loop (+ 1 i) (+ 1 p) ax))))))

;; /O(n)/ Returns the number of occurrences of byte c in b.

(define (byte-sequence-count c b)
  (length (byte-sequence-elem-indices c b)))
    
	
(define (byte-sequence->list b . rest)
  (let-optionals rest ((fmap identity))
   (let loop ((b b) (ax '()))
     (cond ((byte-sequence-empty? b) (reverse ax))
	   (else  (loop (byte-sequence-cdr b) (cons (fmap (byte-sequence-car b)) ax)))))))
	 
(define (byte-sequence->string b)
  (assert (byte-sequence? b))
  (let* ([n (byte-sequence-length b)]
	 [s (make-string n)] )
    (move-memory! (byte-sequence-object b) s n (byte-sequence-offset b) 0)
    s))


;; Error handling: the C routines below never raise Scheme exceptions
;; themselves. Instead, they record an error condition in static
;; variables, and the Scheme wrappers check the flag after each foreign
;; call and raise a proper Scheme exception if an error is pending.

#>

#include <unistd.h>

static int bv_error_pending = 0;
static char bv_error_msg[256];

static void bv_set_error (const char *msg)
{
  bv_error_pending = 1;
  strncpy(bv_error_msg, msg, 255);
  bv_error_msg[255] = 0;
}

<#

(define bv-error-pending?
  (foreign-lambda* bool () "C_return(bv_error_pending);"))

(define bv-error-message
  (foreign-lambda* c-string () "C_return(bv_error_msg);"))

(define bv-clear-error!
  (foreign-lambda* void () "bv_error_pending = 0;"))

(define (bv-check-error! loc)
  (when (bv-error-pending?)
    (let ((msg (bv-error-message)))
      (bv-clear-error!)
      (error loc msg))))



(define bv-read
    (foreign-lambda* int ((integer fd) (nonnull-bytevector b) (integer n) )
#<<END
     ssize_t s;

     if ( (s = read(fd,b,n)) == -1 )
     {
          bv_set_error("read I/O error in byte-sequence-read");
          C_return(-1);
     }
     C_return(s);
END
))


(define (byte-sequence-read port n)
  (let ((ob (make-bytevector n)))
    (let ((s (bv-read (port->fileno port) ob n)))
      (bv-check-error! 'byte-sequence-read)
      (if (positive? s)
	  (make-byte-sequence ob 0 s)
	  #!eof))))

	
(define (file->byte-sequence filename #!optional mode)
  (let ((filesize (file-size filename)))
    (if mode
        (call-with-input-file filename
          (lambda (port) (byte-sequence-read port filesize))
          mode)
        (call-with-input-file filename
          (lambda (port) (byte-sequence-read port filesize)))
        ))
  )


(define bv-write
    (foreign-lambda* void ((integer fd) (nonnull-bytevector b) (integer size) (integer offset))
#<<END
     ssize_t s,n;

     n = s = 0; 
     while (n < size)
     {
	  if ( (s = write(fd,(const void *)(b+n+offset),size-n)) == -1 )
	  {
	       bv_set_error("write I/O error in byte-sequence-write");
	       C_return(C_SCHEME_UNDEFINED);
	  }
	  n += s;
     }
     C_return(C_SCHEME_UNDEFINED);
END
))

(define (byte-sequence-write port b)
  (let ((ob (byte-sequence-object b))
	(n  (byte-sequence-length b))
	(offset (byte-sequence-offset b)))
    (bv-write (port->fileno port) ob n offset)
    (bv-check-error! 'byte-sequence-write)))


;; Writes the byte-sequence b to the file named by filename. The optional
;; mode argument is passed to call-with-output-file, so #:append can
;; be used to append to an existing file. Raises an error if any write
;; fails.

(define (byte-sequence->file filename b #!optional mode)
  (if mode
      (call-with-output-file filename
	(lambda (port) (byte-sequence-write port b)) mode)
      (call-with-output-file filename
	(lambda (port) (byte-sequence-write port b)))))


;;
;; /O(n)/ UTF-8 validation, using a direct range-check state machine:
;; lead bytes are classified by the number of continuation bytes they
;; require; the first continuation byte of the sequences for lead byte
;; 0xED (surrogates) and 0xF4 (values above U+10FFFF) is range-limited.
;;

(define (byte-sequence-continuation-byte? c)
  (fx= 128 (fxand c 192)))

;; Returns the number of continuation bytes required by the lead byte
;; code, or -1 if code is not a valid lead byte.

(define (byte-sequence-utf8-remaining code)
  (cond ((fx< code 128)  0)
	((fx< code 194)  -1)
	((fx< code 224)  1)
	((fx< code 240)  2)
	((fx< code 245)  3)
	(else -1)))

;; Returns the lower and upper bounds (both inclusive) for the first
;; continuation byte of the multi-byte sequence whose lead byte is
;; code and which has rem continuation bytes remaining.

(define (byte-sequence-utf8-first-continuation-bounds code rem)
  (cond ((fx= code 237) (values 128 159)) ; exclude surrogates U+D800..U+DFFF
	((fx= code 244) (values 128 143)) ; exclude values above U+10FFFF
	(else (values 128 191))))

(define (byte-sequence-valid-utf8? b)
  (let ((blen (byte-sequence-length b))
	(ob   (byte-sequence-object b)))
    (let loop ((i 0) (rem 0) (bad #f) (lo 0) (hi 191))
      (cond ((fx>= i blen) (not (or bad (positive? rem))))
	    ((fx>= rem 1)
	     (let ((c (bv-uref ob (+ i (byte-sequence-offset b)))))
	       (cond ((not (byte-sequence-continuation-byte? c)) (loop blen 0 #t 0 191))
		     ((or (fx< c lo) (fx> c hi)) (loop blen 0 #t 0 191))
		     (else (loop (+ 1 i) (- rem 1) bad 0 191)))))
	    (else
	     (let* ((c (bv-uref ob (+ i (byte-sequence-offset b))))
		    (r (byte-sequence-utf8-remaining c)))
	       (cond ((negative? r) (loop blen 0 #t 0 191))
		     ((zero? r) (loop (+ 1 i) 0 bad 0 191))
		     (else
		      (let-values (((lo2 hi2) (byte-sequence-utf8-first-continuation-bounds c r)))
			(loop (+ 1 i) r bad lo2 hi2))))))))))


;; code adapted from srfi-4.scm:
;;
;; In CHICKEN 6, u8vectors are represented directly as bytevectors,
;; whereas the other typed vectors are structures that wrap a
;; bytevector in their second slot.

(define (pack-copy tag loc)
  (lambda (v)
    (cond ((eq? tag 'u8vector)
	   (##sys#check-bytevector v loc)
	   (make-byte-sequence v 0 (##sys#size v)))
	  (else
	   (##sys#check-structure v tag loc)
	   (let* ((old (##sys#slot v 1))
		  (n   (##sys#size old))
		  (new (##sys#make-bytevector n)))
	     (move-memory! old new)
	     (make-byte-sequence new 0 n))))))

(define u8vector->byte-sequence (pack-copy 'u8vector 'u8vector->byte-sequence))
(define s8vector->byte-sequence (pack-copy 's8vector 's8vector->byte-sequence))
(define u16vector->byte-sequence (pack-copy 'u16vector 'u16vector->byte-sequence))
(define s16vector->byte-sequence (pack-copy 's16vector 's16vector->byte-sequence))
(define u32vector->byte-sequence (pack-copy 'u32vector 'u32vector->byte-sequence))
(define s32vector->byte-sequence (pack-copy 's32vector 's32vector->byte-sequence))
(define f32vector->byte-sequence (pack-copy 'f32vector 'f32vector->byte-sequence))
(define f64vector->byte-sequence (pack-copy 'f64vector 'f64vector->byte-sequence))


(define (unpack-copy tag sz loc)
  (lambda (bb)
    (let ((str (byte-sequence-object bb))
	  (offset (byte-sequence-offset bb)))
      (cond ((eq? tag 'u8vector)
	     (##sys#check-bytevector str loc)
	     (let ((len (byte-sequence-length bb)))
	       (if (or (zero? offset)
		       (= len (##sys#size str)))
		   str
		   (let ((new (##sys#make-bytevector len)))
		     (move-memory! str new len offset 0)
		     new))))
	    (else
	     (##sys#check-bytevector str loc)
	     (let* ((len (byte-sequence-length bb))
		    (new (##sys#make-bytevector len)))
	       (if (or (eq? #t sz)
		       (eq? 0 (##core#inline "C_fixnum_modulo" len sz)))
		   (begin
		     (move-memory! str new len offset) 
		     (##sys#make-structure tag new))
		   (##sys#error loc "bytevector does not have correct size for packing" tag len sz) )))))))


(define byte-sequence->u8vector (unpack-copy 'u8vector #t 'byte-sequence->u8vector))
(define byte-sequence->s8vector (unpack-copy 's8vector #t 'byte-sequence->s8vector))
(define byte-sequence->u16vector (unpack-copy 'u16vector 2 'byte-sequence->u16vector))
(define byte-sequence->s16vector (unpack-copy 's16vector 2 'byte-sequence->s16vector))
(define byte-sequence->u32vector (unpack-copy 'u32vector 4 'byte-sequence->u32vector))
(define byte-sequence->s32vector (unpack-copy 's32vector 4 'byte-sequence->s32vector))
(define byte-sequence->f32vector (unpack-copy 'f32vector 4 'byte-sequence->f32vector))
(define byte-sequence->f64vector (unpack-copy 'f64vector 8 'byte-sequence->f64vector))

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
    (foreign-lambda* void ((bytevector m))
#<<END
    memset (m, 0, 4);
END
))

(define setbit!
    (foreign-lambda* void ((bytevector m) (unsigned-int i))
#<<END
     unsigned int w;
     w = i / 8;

     m[w] = m[w] | ((1 << i) >> (8*w));
END
))

(define bitset? 
    (foreign-lambda* bool ((bytevector m) (unsigned-int i))
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
  (let ((nobj  (byte-sequence-object needle))
	(noff  (byte-sequence-offset needle))
	(nlen  (byte-sequence-length needle))
	(hobj  (byte-sequence-object haystack))
	(hoff  (byte-sequence-offset haystack))
	(hlen  (byte-sequence-length haystack)))
    (let* ((nindex   (lambda (k) (bv-ref nobj (+ noff k))))
	   (hindex   (lambda (k) (bv-ref hobj (+ hoff k))))
	   (ldiff    (- hlen nlen))
	   (nlast    (- nlen 1))
	   (z        (nindex nlast))
	   (tbl      (make-table nlast nindex nlen z))
           (m        (make-bytevector 4))
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

(define (byte-sequence-find needle haystack)
  (cond ((byte-sequence-empty? needle)  
	 (error 'byte-sequence-find "empty pattern" needle))
	(else 
	 (let ((r (subsequence-search needle haystack)))
	   (cond ((null? r)  
		  (list haystack '()))
		 (else       
		  (let* ((hoff  (byte-sequence-offset haystack))
			 (hlen  (byte-sequence-length haystack))
			 (chunk (lambda (n l) (byte-sequence-copy haystack (+ hoff n) l)))
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
