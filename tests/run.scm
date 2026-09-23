
(import (chicken file posix) (chicken format) (chicken fixnum) (chicken file) byte-sequence test srfi-4)


(define a (byte-sequence-cons 1 (byte-sequence-cons 2 (byte-sequence-empty))))
(define b (byte-sequence-cons 3 (byte-sequence-cons 4 (byte-sequence-empty))))
(define c (list->byte-sequence (list 7 8 9)))
(define d (byte-sequence-replicate 10 6))
   


(test-begin "byte-sequence test")

            (test (sprintf "string <-> byte-sequence")
		  "est"
		  (byte-sequence->string (byte-sequence-cdr (string->byte-sequence "test"))))

            (test (sprintf "s8vector <-> byte-sequence")
                  (s8vector -42)
		  (byte-sequence->s8vector (s8vector->byte-sequence (s8vector -42))))

            (test (sprintf "f32vector <-> byte-sequence")
                  (f32vector 1.02 3.04 5.06)
		  (byte-sequence->f32vector (f32vector->byte-sequence (f32vector 1.02 3.04 5.06))))

            (test (sprintf "byte-sequence-replicate")
                  '(6 6 6 6 6 6 6 6 6 6)
		  (byte-sequence->list d))

	    (test (sprintf "byte-sequence-cons" ) 
		   '(1 2) (byte-sequence->list a))

	    (test (sprintf "byte-sequence-car" ) 
		   1 (byte-sequence-car a))

	    (test (sprintf "byte-sequence-cdr" ) 
		   '(2) (byte-sequence->list (byte-sequence-cdr a)))

	    (test (sprintf "byte-sequence-ref" ) 
		   1 (byte-sequence-ref (byte-sequence-cons 5 a) 1))

	    (test (sprintf "byte-sequence-uref" ) 
		   (fxand 255 -5)
		   (byte-sequence-uref (byte-sequence-cons -5 a) 0))

	    (test (sprintf "byte-sequence-append" ) 
		   '(1 2 3 4 7 8 9) (byte-sequence->list (byte-sequence-append a b c)))

	    (test (sprintf "byte-sequence-take" ) 
		   '(1 2 ) (byte-sequence->list (byte-sequence-take (byte-sequence-append a b c) 2)))

	    (test (sprintf "byte-sequence-drop" ) 
		   '(3 4 7 8 9) (byte-sequence->list (byte-sequence-drop (byte-sequence-append a b c) 2)))

	    (test (sprintf "byte-sequence-drop" ) 
		   '() (byte-sequence->list (byte-sequence-drop (list->byte-sequence '(1)) 1)))

	    (test (sprintf "byte-sequence-span" ) 
		   '(3 4 7) (byte-sequence->list (byte-sequence-span (byte-sequence-append a b c) 2 5)))

	    (test (sprintf "byte-sequence-map" ) 
		   '(10 2 4 6 8) 
		   (byte-sequence->list 
		    (byte-sequence-map (lambda (x) (* 2 x)) 
				   (byte-sequence-cons 5  (byte-sequence-append a b)))))

	    (test (sprintf "byte-sequence-reverse" ) 
		   '(10 8 6 4 2) 
		   (byte-sequence->list
		    (byte-sequence-map
		     (lambda (x) (* 2 x)) 
		     (byte-sequence-cons 
		      5 (byte-sequence-reverse (byte-sequence-append a b))))))

	    (test (sprintf "byte-sequence-intersperse" ) 
		   '(10 9 8 9 6 9 4 9 2) 
		   (byte-sequence->list
		    (byte-sequence-intersperse 
		     (byte-sequence-map
		      (lambda (x) (* 2 x)) 
		      (byte-sequence-cons 
		       5 (byte-sequence-reverse (byte-sequence-append a b)))) 9)))

	    (test (sprintf "byte-sequence-find" ) 
		   `(,(list->byte-sequence '(1)) ())
		   (byte-sequence-find 
		    (list->byte-sequence (list 9 10))
		    (list->byte-sequence (list 1))))

	    (test (sprintf "byte-sequence-find" ) 
		   `(,(list->byte-sequence '(9 10)) ())
		   (byte-sequence-find 
		    (list->byte-sequence (list 1))
		    (list->byte-sequence (list 9 10))
		    ))

	    (test (sprintf "byte-sequence-find" ) 
		   '((10) (((9 8) (9 8 9 6 9 4 9 2)) 
			   ((9 6) (9 6 9 4 9 2)) 
			   ((9 4) (9 4 9 2)) 
			   ((9 2) (9 2))))
		   (let ((r (byte-sequence-find 
			     (list->byte-sequence (list 9))
			     (byte-sequence-intersperse 
			      (byte-sequence-map
			       (lambda (x) (* 2 x)) 
			       (byte-sequence-cons 
				5 (byte-sequence-reverse (byte-sequence-append a b)))) 9))))
		      (list (byte-sequence->list (car r))
			    (map (lambda (x) (map byte-sequence->list x)) (cadr r)))))

	    (test (sprintf "byte-sequence-find" ) 
		   '((10 9 8) (((9 6 9 4 9 2) (9 6 9 4 9 2))))
		   (let ((r (byte-sequence-find 
			     (list->byte-sequence (list 9 6))
			     (byte-sequence-intersperse 
			      (byte-sequence-map
			       (lambda (x) (* 2 x)) 
			       (byte-sequence-cons 
				5 (byte-sequence-reverse (byte-sequence-append a b)))) 9))))
		      (list (byte-sequence->list (car r))
			    (map (lambda (x) (map byte-sequence->list x)) (cadr r)))))

            ;; test case contributed by dthedens (Trac issue #1037)
	    (test (sprintf "byte-sequence-find" ) 
		   `((0) (((1 31 3) (1 31 3))))
                   (let ((r (byte-sequence-find 
                             (list->byte-sequence (list 1 31))
                             (list->byte-sequence (list 0 1 31 3)))))
                     (list (byte-sequence->list (car r))
                           (map (lambda (x) (map byte-sequence->list x)) (cadr r)))))

           ;; test case contributed by dthedens (Trac issue #1037)
            (test (sprintf "byte-sequence-find" )
                   `((0) (((1 63 3) (1 63 3))))
                   (let ((r (byte-sequence-find
                             (list->byte-sequence (list 1 63))
                             (list->byte-sequence (list 0 1 63 3)))))
                     (list (byte-sequence->list (car r))
                           (map (lambda (x) (map byte-sequence->list x)) (cadr r)))))

            ;; test case contributed by dthedens (Trac issue #1038)
	    (test (sprintf "byte-sequence-find" ) 
		   `((0 1) (((2 3) (2 3))))
                   (let ((r (byte-sequence-find 
                             (list->byte-sequence (list 2))
                             (list->byte-sequence (list 0 1 2 3)))))
                     (list (byte-sequence->list (car r))
                           (map (lambda (x) (map byte-sequence->list x)) (cadr r)))))

            ;; test case contributed by dthedens (Trac issue #1038)
	    (test (sprintf "byte-sequence-find" ) 
		   `((0 1) (((2 3) (2 3))))
                   (let ((r (byte-sequence-find 
                             (list->byte-sequence (list 2 3))
                             (list->byte-sequence (list 0 1 2 3)))))
                     (list (byte-sequence->list (car r))
                           (map (lambda (x) (map byte-sequence->list x)) (cadr r)))))


	    (test (sprintf "byte-sequence-fold-left" ) 
		   66
		   (byte-sequence-fold-left
		    + 0
		    (byte-sequence-intersperse 
		     (byte-sequence-map
		      (lambda (x) (* 2 x)) 
		      (byte-sequence-cons 
		       5 (byte-sequence-reverse (byte-sequence-append a b)))) 9)))

	    (test (sprintf "byte-sequence-fold-right" ) 
		   -6
		   (byte-sequence-fold-left
		    - 0
		    (byte-sequence-intersperse 
		     (byte-sequence-map
		      (lambda (x) (* 2 x)) 
		      (byte-sequence-cons 
		       5 (byte-sequence-reverse (byte-sequence-append a b)))) 9)))

	    (let* ((temp-path (create-temporary-file "byte-sequence-test"))
		   (out-port (open-output-file temp-path #:binary)))

	      (test-assert
	       (sprintf "byte-sequence-write" ) 
	       (byte-sequence-write 
                out-port 
                (byte-sequence-intersperse 
                 (byte-sequence-map
                  (lambda (x) (* 2 x)) 
                  (byte-sequence-cons 
                   5 (byte-sequence-reverse (byte-sequence-append a b)))) 9)))

	      (close-output-port out-port)

	      (let ((in-port (open-input-file temp-path #:binary)))

		(test
		 (sprintf "byte-sequence-read" ) 
		 '(10 9 8 9 6 9 4 9 2) 
		 (byte-sequence->list
		  (byte-sequence-read in-port 9 )))
	      
		(close-input-port in-port))

	      (test 
	       (sprintf "file->byte-sequence")
	       '(10 9 8 9 6 9 4 9 2) 
	       (byte-sequence->list
		(file->byte-sequence temp-path)))

	      (test 
	       (sprintf "file->byte-sequence in binary mode")
	       '(10 9 8 9 6 9 4 9 2) 
	       (byte-sequence->list
		(file->byte-sequence temp-path #:binary)))

              (delete-file temp-path)
	       
	      )
            ;; test case contributed by dthedens 
            ;; Test for byte-sequence->bytevector for non-zero offset
            (test
             (sprintf "byte-sequence->bytevector for non-zero offset")
             (u8vector 9)
             (byte-sequence->bytevector (byte-sequence-drop c 2)))

            ;; comparisons and basic interface

	    (test (sprintf "byte-sequence=?") 
		  #t (byte-sequence=? a a))

	    (test (sprintf "byte-sequence=?") 
		  #f (byte-sequence=? a b))

	    (test (sprintf "byte-sequence=? on slices") 
		  #t (byte-sequence=? (byte-sequence-drop (byte-sequence-append a b c) 2) (byte-sequence-append b c)))

	    (test (sprintf "byte-sequence-compare") 
		  0 (byte-sequence-compare a a))

	    (test (sprintf "byte-sequence-compare") 
		  -1 (byte-sequence-compare a (byte-sequence-append a (byte-sequence-singleton 9))))

	    (test (sprintf "byte-sequence-compare") 
		  1 (byte-sequence-compare (byte-sequence-singleton 2) (byte-sequence-singleton 1)))

	    (test (sprintf "byte-sequence-singleton") 
		  '(1) (byte-sequence->list (byte-sequence-singleton 1)))

	    (test (sprintf "byte-sequence-snoc") 
		  '(1 2 5) (byte-sequence->list (byte-sequence-snoc a 5)))

	    (test (sprintf "byte-sequence-last") 
		  2 (byte-sequence-last a))

	    (test (sprintf "byte-sequence-init") 
		  '(1) (byte-sequence->list (byte-sequence-init a)))

	    (test (sprintf "byte-sequence-uncons") 
		  '(1 (2)) (let-values (((x r) (byte-sequence-uncons a)))
			     (list x (byte-sequence->list r))))

	    (test (sprintf "byte-sequence-uncons on empty") 
		  #f (byte-sequence-uncons (byte-sequence-empty)))

	    (test (sprintf "byte-sequence-unsnoc") 
		  '((1) 2) (let-values (((r x) (byte-sequence-unsnoc a)))
			     (list (byte-sequence->list r) x)))

	    (test (sprintf "byte-sequence-unsnoc on empty") 
		  #f (byte-sequence-unsnoc (byte-sequence-empty)))

            ;; searching

	    (test (sprintf "byte-sequence-index-maybe") 
		  2 (byte-sequence-index-maybe a 1))

	    (test (sprintf "byte-sequence-index-maybe out of range") 
		  #f (byte-sequence-index-maybe a 5))

	    (test (sprintf "byte-sequence-elem-index") 
		  1 (byte-sequence-elem-index 2 (byte-sequence-append a b)))

	    (test (sprintf "byte-sequence-elem-index missing") 
		  #f (byte-sequence-elem-index 9 (byte-sequence-append a b)))

	    (test (sprintf "byte-sequence-elem-index-end") 
		  3 (byte-sequence-elem-index-end 4 (byte-sequence-append a b)))

	    (test (sprintf "byte-sequence-elem-indices") 
		  '(0 1) (byte-sequence-elem-indices 1 (list->byte-sequence '(1 1 2))))

	    (test (sprintf "byte-sequence-elem-indices none") 
		  '() (byte-sequence-elem-indices 9 (byte-sequence-append a b)))

	    (test (sprintf "byte-sequence-find-index") 
		  1 (byte-sequence-find-index (lambda (x) (> x 1)) (byte-sequence-append a b)))

	    (test (sprintf "byte-sequence-find-index none") 
		  #f (byte-sequence-find-index (lambda (x) (> x 100)) (byte-sequence-append a b)))

	    (test (sprintf "byte-sequence-find-index-end") 
		  3 (byte-sequence-find-index-end (lambda (x) (> x 1)) (byte-sequence-append a b)))

	    (test (sprintf "byte-sequence-find-indices") 
		  '(1 3) (byte-sequence-find-indices (lambda (x) (even? x)) (byte-sequence-append a b)))

	    (test (sprintf "byte-sequence-count") 
		  2 (byte-sequence-count 1 (list->byte-sequence '(1 2 1))))

            ;; slicing

	    (test (sprintf "byte-sequence-split-at") 
		  '((1 2) (3 4)) (let-values (((x r) (byte-sequence-split-at (byte-sequence-append a b) 2)))
				   (list (byte-sequence->list x) (byte-sequence->list r))))

	    (test (sprintf "byte-sequence-split-at clamping") 
		  '((1 2) ()) (let-values (((x r) (byte-sequence-split-at a 10)))
				(list (byte-sequence->list x) (byte-sequence->list r))))

	    (test (sprintf "byte-sequence-take-end") 
		  '(3 4) (byte-sequence->list (byte-sequence-take-end (byte-sequence-append a b) 2)))

	    (test (sprintf "byte-sequence-drop-end") 
		  '(1 2) (byte-sequence->list (byte-sequence-drop-end (byte-sequence-append a b) 2)))

	    (test (sprintf "byte-sequence-take-while") 
		  '(1 2) (byte-sequence->list 
			  (byte-sequence-take-while (lambda (x) (< x 3)) (byte-sequence-append a b))))

	    (test (sprintf "byte-sequence-drop-while") 
		  '(3 4) (byte-sequence->list 
			  (byte-sequence-drop-while (lambda (x) (< x 3)) (byte-sequence-append a b))))

	    (test (sprintf "byte-sequence-take-while-end") 
		  '(3 4) (byte-sequence->list 
			  (byte-sequence-take-while-end (lambda (x) (> x 2)) (byte-sequence-append a b))))

	    (test (sprintf "byte-sequence-drop-while-end") 
		  '(1 2) (byte-sequence->list 
			  (byte-sequence-drop-while-end (lambda (x) (> x 2)) (byte-sequence-append a b))))

	    (test (sprintf "byte-sequence-span-while") 
		  '((1 2) (3 4)) (let-values (((x r) (byte-sequence-span-while 
							(lambda (x) (< x 3)) (byte-sequence-append a b))))
				   (list (byte-sequence->list x) (byte-sequence->list r))))

	    (test (sprintf "byte-sequence-break-while") 
		  '((1 2) (3 4)) (let-values (((x r) (byte-sequence-break-while 
							(lambda (x) (> x 2)) (byte-sequence-append a b))))
				   (list (byte-sequence->list x) (byte-sequence->list r))))

	    (test (sprintf "byte-sequence-span-while-end") 
		  '((1) (2)) (let-values (((x r) (byte-sequence-span-while-end 
						  (lambda (x) (> x 1)) a)))
			       (list (byte-sequence->list x) (byte-sequence->list r))))

	    (test (sprintf "byte-sequence-break-while-end") 
		  '((1) (2)) (let-values (((x r) (byte-sequence-break-while-end 
						  (lambda (x) (< x 2)) a)))
			       (list (byte-sequence->list x) (byte-sequence->list r))))

	    (test (sprintf "byte-sequence-strip-prefix") 
		  '(2) (byte-sequence->list (byte-sequence-strip-prefix (byte-sequence-singleton 1) a)))

	    (test (sprintf "byte-sequence-strip-prefix no match") 
		  #f (byte-sequence-strip-prefix (byte-sequence-singleton 9) a))

	    (test (sprintf "byte-sequence-strip-suffix") 
		  '(1) (byte-sequence->list (byte-sequence-strip-suffix (byte-sequence-singleton 2) a)))

	    (test (sprintf "byte-sequence-strip-suffix no match") 
		  #f (byte-sequence-strip-suffix (byte-sequence-singleton 9) a))

	    (test (sprintf "byte-sequence-is-prefix-of?") 
		  #t (byte-sequence-is-prefix-of? a (byte-sequence-append a b)))

	    (test (sprintf "byte-sequence-is-prefix-of?") 
		  #f (byte-sequence-is-prefix-of? b a))

	    (test (sprintf "byte-sequence-is-suffix-of?") 
		  #t (byte-sequence-is-suffix-of? b (byte-sequence-append a b)))

	    (test (sprintf "byte-sequence-is-suffix-of?") 
		  #f (byte-sequence-is-suffix-of? a b))

            ;; utf8 validation

	    (test (sprintf "byte-sequence-valid-utf8? ascii") 
		  #t (byte-sequence-valid-utf8? (string->byte-sequence "abc")))

	    (test (sprintf "byte-sequence-valid-utf8? empty") 
		  #t (byte-sequence-valid-utf8? (byte-sequence-empty)))

	    (test (sprintf "byte-sequence-valid-utf8? 2-byte") 
		  #t (byte-sequence-valid-utf8? (list->byte-sequence '(194 160))))

	    (test (sprintf "byte-sequence-valid-utf8? 3-byte") 
		  #t (byte-sequence-valid-utf8? (list->byte-sequence '(224 160 128))))

	    (test (sprintf "byte-sequence-valid-utf8? 4-byte max") 
		  #t (byte-sequence-valid-utf8? (list->byte-sequence '(244 143 191 191))))

	    (test (sprintf "byte-sequence-valid-utf8? bad lead") 
		  #f (byte-sequence-valid-utf8? (list->byte-sequence '(128))))

	    (test (sprintf "byte-sequence-valid-utf8? truncated") 
		  #f (byte-sequence-valid-utf8? (list->byte-sequence '(194))))

	    (test (sprintf "byte-sequence-valid-utf8? bad continuation") 
		  #f (byte-sequence-valid-utf8? (list->byte-sequence '(224 32 128))))

	    (test (sprintf "byte-sequence-valid-utf8? above U+10FFFF") 
		  #f (byte-sequence-valid-utf8? (list->byte-sequence '(244 165 165 165))))

	    (test (sprintf "byte-sequence-valid-utf8? invalid lead 245") 
		  #f (byte-sequence-valid-utf8? (list->byte-sequence '(245 160 160 160))))

	    (test (sprintf "byte-sequence-valid-utf8? surrogate") 
		  #f (byte-sequence-valid-utf8? (list->byte-sequence '(237 160 128))))

	    (test (sprintf "byte-sequence-valid-utf8? just below surrogate") 
		  #t (byte-sequence-valid-utf8? (list->byte-sequence '(236 160 128))))

            ;; file output

            (let* ((temp-path (create-temporary-file "byte-sequence-test-write")))
              (byte-sequence->file temp-path (byte-sequence-append a b))
              (test 
               (sprintf "byte-sequence->file")
               '(1 2 3 4) 
               (byte-sequence->list
                (file->byte-sequence temp-path)))

              (byte-sequence->file temp-path (byte-sequence-append a b) #:append)
              (test 
               (sprintf "byte-sequence->file append mode")
               '(1 2 3 4 1 2 3 4) 
               (byte-sequence->list
                (file->byte-sequence temp-path)))

              (delete-file temp-path))

(test-end)
(test-exit)
