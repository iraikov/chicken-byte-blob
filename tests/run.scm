
(import (chicken file posix) (chicken format) (chicken fixnum) (chicken file) byte-blob test srfi-4)


(define a (byte-blob-cons 1 (byte-blob-cons 2 (byte-blob-empty))))
(define b (byte-blob-cons 3 (byte-blob-cons 4 (byte-blob-empty))))
(define c (list->byte-blob (list 7 8 9)))
(define d (byte-blob-replicate 10 6))
   


(test-begin "byte-blob test")

            (test (sprintf "string <-> byte-blob")
		  "est"
		  (byte-blob->string (byte-blob-cdr (string->byte-blob "test"))))

            (test (sprintf "s8vector <-> byte-blob")
                  (s8vector -42)
		  (byte-blob->s8vector (s8vector->byte-blob (s8vector -42))))

            (test (sprintf "f32vector <-> byte-blob")
                  (f32vector 1.02 3.04 5.06)
		  (byte-blob->f32vector (f32vector->byte-blob (f32vector 1.02 3.04 5.06))))

            (test (sprintf "byte-blob-replicate")
                  '(6 6 6 6 6 6 6 6 6 6)
		  (byte-blob->list d))

	    (test (sprintf "byte-blob-cons" ) 
		   '(1 2) (byte-blob->list a))

	    (test (sprintf "byte-blob-car" ) 
		   1 (byte-blob-car a))

	    (test (sprintf "byte-blob-cdr" ) 
		   '(2) (byte-blob->list (byte-blob-cdr a)))

	    (test (sprintf "byte-blob-ref" ) 
		   1 (byte-blob-ref (byte-blob-cons 5 a) 1))

	    (test (sprintf "byte-blob-uref" ) 
		   (fxand 255 -5)
		   (byte-blob-uref (byte-blob-cons -5 a) 0))

	    (test (sprintf "byte-blob-append" ) 
		   '(1 2 3 4 7 8 9) (byte-blob->list (byte-blob-append a b c)))

	    (test (sprintf "byte-blob-take" ) 
		   '(1 2 ) (byte-blob->list (byte-blob-take (byte-blob-append a b c) 2)))

	    (test (sprintf "byte-blob-drop" ) 
		   '(3 4 7 8 9) (byte-blob->list (byte-blob-drop (byte-blob-append a b c) 2)))

	    (test (sprintf "byte-blob-drop" ) 
		   '() (byte-blob->list (byte-blob-drop (list->byte-blob '(1)) 1)))

	    (test (sprintf "byte-blob-span" ) 
		   '(3 4 7) (byte-blob->list (byte-blob-span (byte-blob-append a b c) 2 5)))

	    (test (sprintf "byte-blob-map" ) 
		   '(10 2 4 6 8) 
		   (byte-blob->list 
		    (byte-blob-map (lambda (x) (* 2 x)) 
				   (byte-blob-cons 5  (byte-blob-append a b)))))

	    (test (sprintf "byte-blob-reverse" ) 
		   '(10 8 6 4 2) 
		   (byte-blob->list
		    (byte-blob-map
		     (lambda (x) (* 2 x)) 
		     (byte-blob-cons 
		      5 (byte-blob-reverse (byte-blob-append a b))))))

	    (test (sprintf "byte-blob-intersperse" ) 
		   '(10 9 8 9 6 9 4 9 2) 
		   (byte-blob->list
		    (byte-blob-intersperse 
		     (byte-blob-map
		      (lambda (x) (* 2 x)) 
		      (byte-blob-cons 
		       5 (byte-blob-reverse (byte-blob-append a b)))) 9)))

	    (test (sprintf "byte-blob-find" ) 
		   `(,(list->byte-blob '(1)) ())
		   (byte-blob-find 
		    (list->byte-blob (list 9 10))
		    (list->byte-blob (list 1))))

	    (test (sprintf "byte-blob-find" ) 
		   `(,(list->byte-blob '(9 10)) ())
		   (byte-blob-find 
		    (list->byte-blob (list 1))
		    (list->byte-blob (list 9 10))
		    ))

	    (test (sprintf "byte-blob-find" ) 
		   '((10) (((9 8) (9 8 9 6 9 4 9 2)) 
			   ((9 6) (9 6 9 4 9 2)) 
			   ((9 4) (9 4 9 2)) 
			   ((9 2) (9 2))))
		   (let ((r (byte-blob-find 
			     (list->byte-blob (list 9))
			     (byte-blob-intersperse 
			      (byte-blob-map
			       (lambda (x) (* 2 x)) 
			       (byte-blob-cons 
				5 (byte-blob-reverse (byte-blob-append a b)))) 9))))
		      (list (byte-blob->list (car r))
			    (map (lambda (x) (map byte-blob->list x)) (cadr r)))))

	    (test (sprintf "byte-blob-find" ) 
		   '((10 9 8) (((9 6 9 4 9 2) (9 6 9 4 9 2))))
		   (let ((r (byte-blob-find 
			     (list->byte-blob (list 9 6))
			     (byte-blob-intersperse 
			      (byte-blob-map
			       (lambda (x) (* 2 x)) 
			       (byte-blob-cons 
				5 (byte-blob-reverse (byte-blob-append a b)))) 9))))
		      (list (byte-blob->list (car r))
			    (map (lambda (x) (map byte-blob->list x)) (cadr r)))))

            ;; test case contributed by dthedens (Trac issue #1037)
	    (test (sprintf "byte-blob-find" ) 
		   `((0) (((1 31 3) (1 31 3))))
                   (let ((r (byte-blob-find 
                             (list->byte-blob (list 1 31))
                             (list->byte-blob (list 0 1 31 3)))))
                     (list (byte-blob->list (car r))
                           (map (lambda (x) (map byte-blob->list x)) (cadr r)))))

           ;; test case contributed by dthedens (Trac issue #1037)
            (test (sprintf "byte-blob-find" )
                   `((0) (((1 63 3) (1 63 3))))
                   (let ((r (byte-blob-find
                             (list->byte-blob (list 1 63))
                             (list->byte-blob (list 0 1 63 3)))))
                     (list (byte-blob->list (car r))
                           (map (lambda (x) (map byte-blob->list x)) (cadr r)))))

            ;; test case contributed by dthedens (Trac issue #1038)
	    (test (sprintf "byte-blob-find" ) 
		   `((0 1) (((2 3) (2 3))))
                   (let ((r (byte-blob-find 
                             (list->byte-blob (list 2))
                             (list->byte-blob (list 0 1 2 3)))))
                     (list (byte-blob->list (car r))
                           (map (lambda (x) (map byte-blob->list x)) (cadr r)))))

            ;; test case contributed by dthedens (Trac issue #1038)
	    (test (sprintf "byte-blob-find" ) 
		   `((0 1) (((2 3) (2 3))))
                   (let ((r (byte-blob-find 
                             (list->byte-blob (list 2 3))
                             (list->byte-blob (list 0 1 2 3)))))
                     (list (byte-blob->list (car r))
                           (map (lambda (x) (map byte-blob->list x)) (cadr r)))))


	    (test (sprintf "byte-blob-fold-left" ) 
		   66
		   (byte-blob-fold-left
		    + 0
		    (byte-blob-intersperse 
		     (byte-blob-map
		      (lambda (x) (* 2 x)) 
		      (byte-blob-cons 
		       5 (byte-blob-reverse (byte-blob-append a b)))) 9)))

	    (test (sprintf "byte-blob-fold-right" ) 
		   -6
		   (byte-blob-fold-left
		    - 0
		    (byte-blob-intersperse 
		     (byte-blob-map
		      (lambda (x) (* 2 x)) 
		      (byte-blob-cons 
		       5 (byte-blob-reverse (byte-blob-append a b)))) 9)))

	    (let* ((temp-path (create-temporary-file "byte-blob-test"))
		   (out-port (open-output-file temp-path #:binary)))

	      (test-assert
	       (sprintf "byte-blob-write" ) 
	       (byte-blob-write 
                out-port 
                (byte-blob-intersperse 
                 (byte-blob-map
                  (lambda (x) (* 2 x)) 
                  (byte-blob-cons 
                   5 (byte-blob-reverse (byte-blob-append a b)))) 9)))

	      (close-output-port out-port)

	      (let ((in-port (open-input-file temp-path #:binary)))

		(test
		 (sprintf "byte-blob-read" ) 
		 '(10 9 8 9 6 9 4 9 2) 
		 (byte-blob->list
		  (byte-blob-read in-port 9 )))
	      
		(close-input-port in-port))

	      (test 
	       (sprintf "file->byte-blob")
	       '(10 9 8 9 6 9 4 9 2) 
	       (byte-blob->list
		(file->byte-blob temp-path)))

	      (test 
	       (sprintf "file->byte-blob in binary mode")
	       '(10 9 8 9 6 9 4 9 2) 
	       (byte-blob->list
		(file->byte-blob temp-path #:binary)))

              (delete-file temp-path)
	       
	      )
            ;; test case contributed by dthedens 
            ;; Test for byte-blob->blob for non-zero offset
            (test
             (sprintf "byte-blob->blob for non-zero offset")
             (u8vector 9)
             (byte-blob->blob (byte-blob-drop c 2)))

            ;; comparisons and basic interface

	    (test (sprintf "byte-blob=?") 
		  #t (byte-blob=? a a))

	    (test (sprintf "byte-blob=?") 
		  #f (byte-blob=? a b))

	    (test (sprintf "byte-blob=? on slices") 
		  #t (byte-blob=? (byte-blob-drop (byte-blob-append a b c) 2) (byte-blob-append b c)))

	    (test (sprintf "byte-blob-compare") 
		  0 (byte-blob-compare a a))

	    (test (sprintf "byte-blob-compare") 
		  -1 (byte-blob-compare a (byte-blob-append a (byte-blob-singleton 9))))

	    (test (sprintf "byte-blob-compare") 
		  1 (byte-blob-compare (byte-blob-singleton 2) (byte-blob-singleton 1)))

	    (test (sprintf "byte-blob-singleton") 
		  '(1) (byte-blob->list (byte-blob-singleton 1)))

	    (test (sprintf "byte-blob-snoc") 
		  '(1 2 5) (byte-blob->list (byte-blob-snoc a 5)))

	    (test (sprintf "byte-blob-last") 
		  2 (byte-blob-last a))

	    (test (sprintf "byte-blob-init") 
		  '(1) (byte-blob->list (byte-blob-init a)))

	    (test (sprintf "byte-blob-uncons") 
		  '(1 (2)) (let-values (((x r) (byte-blob-uncons a)))
			     (list x (byte-blob->list r))))

	    (test (sprintf "byte-blob-uncons on empty") 
		  #f (byte-blob-uncons (byte-blob-empty)))

	    (test (sprintf "byte-blob-unsnoc") 
		  '((1) 2) (let-values (((r x) (byte-blob-unsnoc a)))
			     (list (byte-blob->list r) x)))

	    (test (sprintf "byte-blob-unsnoc on empty") 
		  #f (byte-blob-unsnoc (byte-blob-empty)))

            ;; searching

	    (test (sprintf "byte-blob-index-maybe") 
		  2 (byte-blob-index-maybe a 1))

	    (test (sprintf "byte-blob-index-maybe out of range") 
		  #f (byte-blob-index-maybe a 5))

	    (test (sprintf "byte-blob-elem-index") 
		  1 (byte-blob-elem-index 2 (byte-blob-append a b)))

	    (test (sprintf "byte-blob-elem-index missing") 
		  #f (byte-blob-elem-index 9 (byte-blob-append a b)))

	    (test (sprintf "byte-blob-elem-index-end") 
		  3 (byte-blob-elem-index-end 4 (byte-blob-append a b)))

	    (test (sprintf "byte-blob-elem-indices") 
		  '(0 1) (byte-blob-elem-indices 1 (list->byte-blob '(1 1 2))))

	    (test (sprintf "byte-blob-elem-indices none") 
		  '() (byte-blob-elem-indices 9 (byte-blob-append a b)))

	    (test (sprintf "byte-blob-find-index") 
		  1 (byte-blob-find-index (lambda (x) (> x 1)) (byte-blob-append a b)))

	    (test (sprintf "byte-blob-find-index none") 
		  #f (byte-blob-find-index (lambda (x) (> x 100)) (byte-blob-append a b)))

	    (test (sprintf "byte-blob-find-index-end") 
		  3 (byte-blob-find-index-end (lambda (x) (> x 1)) (byte-blob-append a b)))

	    (test (sprintf "byte-blob-find-indices") 
		  '(1 3) (byte-blob-find-indices (lambda (x) (even? x)) (byte-blob-append a b)))

	    (test (sprintf "byte-blob-count") 
		  2 (byte-blob-count 1 (list->byte-blob '(1 2 1))))

            ;; slicing

	    (test (sprintf "byte-blob-split-at") 
		  '((1 2) (3 4)) (let-values (((x r) (byte-blob-split-at (byte-blob-append a b) 2)))
				   (list (byte-blob->list x) (byte-blob->list r))))

	    (test (sprintf "byte-blob-split-at clamping") 
		  '((1 2) ()) (let-values (((x r) (byte-blob-split-at a 10)))
				(list (byte-blob->list x) (byte-blob->list r))))

	    (test (sprintf "byte-blob-take-end") 
		  '(3 4) (byte-blob->list (byte-blob-take-end (byte-blob-append a b) 2)))

	    (test (sprintf "byte-blob-drop-end") 
		  '(1 2) (byte-blob->list (byte-blob-drop-end (byte-blob-append a b) 2)))

	    (test (sprintf "byte-blob-take-while") 
		  '(1 2) (byte-blob->list 
			  (byte-blob-take-while (lambda (x) (< x 3)) (byte-blob-append a b))))

	    (test (sprintf "byte-blob-drop-while") 
		  '(3 4) (byte-blob->list 
			  (byte-blob-drop-while (lambda (x) (< x 3)) (byte-blob-append a b))))

	    (test (sprintf "byte-blob-take-while-end") 
		  '(3 4) (byte-blob->list 
			  (byte-blob-take-while-end (lambda (x) (> x 2)) (byte-blob-append a b))))

	    (test (sprintf "byte-blob-drop-while-end") 
		  '(1 2) (byte-blob->list 
			  (byte-blob-drop-while-end (lambda (x) (> x 2)) (byte-blob-append a b))))

	    (test (sprintf "byte-blob-span-while") 
		  '((1 2) (3 4)) (let-values (((x r) (byte-blob-span-while 
							(lambda (x) (< x 3)) (byte-blob-append a b))))
				   (list (byte-blob->list x) (byte-blob->list r))))

	    (test (sprintf "byte-blob-break-while") 
		  '((1 2) (3 4)) (let-values (((x r) (byte-blob-break-while 
							(lambda (x) (> x 2)) (byte-blob-append a b))))
				   (list (byte-blob->list x) (byte-blob->list r))))

	    (test (sprintf "byte-blob-span-while-end") 
		  '((1) (2)) (let-values (((x r) (byte-blob-span-while-end 
						  (lambda (x) (> x 1)) a)))
			       (list (byte-blob->list x) (byte-blob->list r))))

	    (test (sprintf "byte-blob-break-while-end") 
		  '((1) (2)) (let-values (((x r) (byte-blob-break-while-end 
						  (lambda (x) (< x 2)) a)))
			       (list (byte-blob->list x) (byte-blob->list r))))

	    (test (sprintf "byte-blob-strip-prefix") 
		  '(2) (byte-blob->list (byte-blob-strip-prefix (byte-blob-singleton 1) a)))

	    (test (sprintf "byte-blob-strip-prefix no match") 
		  #f (byte-blob-strip-prefix (byte-blob-singleton 9) a))

	    (test (sprintf "byte-blob-strip-suffix") 
		  '(1) (byte-blob->list (byte-blob-strip-suffix (byte-blob-singleton 2) a)))

	    (test (sprintf "byte-blob-strip-suffix no match") 
		  #f (byte-blob-strip-suffix (byte-blob-singleton 9) a))

	    (test (sprintf "byte-blob-is-prefix-of?") 
		  #t (byte-blob-is-prefix-of? a (byte-blob-append a b)))

	    (test (sprintf "byte-blob-is-prefix-of?") 
		  #f (byte-blob-is-prefix-of? b a))

	    (test (sprintf "byte-blob-is-suffix-of?") 
		  #t (byte-blob-is-suffix-of? b (byte-blob-append a b)))

	    (test (sprintf "byte-blob-is-suffix-of?") 
		  #f (byte-blob-is-suffix-of? a b))

            ;; utf8 validation

	    (test (sprintf "byte-blob-valid-utf8? ascii") 
		  #t (byte-blob-valid-utf8? (string->byte-blob "abc")))

	    (test (sprintf "byte-blob-valid-utf8? empty") 
		  #t (byte-blob-valid-utf8? (byte-blob-empty)))

	    (test (sprintf "byte-blob-valid-utf8? 2-byte") 
		  #t (byte-blob-valid-utf8? (list->byte-blob '(194 160))))

	    (test (sprintf "byte-blob-valid-utf8? 3-byte") 
		  #t (byte-blob-valid-utf8? (list->byte-blob '(224 160 128))))

	    (test (sprintf "byte-blob-valid-utf8? 4-byte max") 
		  #t (byte-blob-valid-utf8? (list->byte-blob '(244 143 191 191))))

	    (test (sprintf "byte-blob-valid-utf8? bad lead") 
		  #f (byte-blob-valid-utf8? (list->byte-blob '(128))))

	    (test (sprintf "byte-blob-valid-utf8? truncated") 
		  #f (byte-blob-valid-utf8? (list->byte-blob '(194))))

	    (test (sprintf "byte-blob-valid-utf8? bad continuation") 
		  #f (byte-blob-valid-utf8? (list->byte-blob '(224 32 128))))

	    (test (sprintf "byte-blob-valid-utf8? above U+10FFFF") 
		  #f (byte-blob-valid-utf8? (list->byte-blob '(244 165 165 165))))

	    (test (sprintf "byte-blob-valid-utf8? invalid lead 245") 
		  #f (byte-blob-valid-utf8? (list->byte-blob '(245 160 160 160))))

	    (test (sprintf "byte-blob-valid-utf8? surrogate") 
		  #f (byte-blob-valid-utf8? (list->byte-blob '(237 160 128))))

	    (test (sprintf "byte-blob-valid-utf8? just below surrogate") 
		  #t (byte-blob-valid-utf8? (list->byte-blob '(236 160 128))))

            ;; file output

            (let* ((temp-path (create-temporary-file "byte-blob-test-write")))
              (byte-blob->file temp-path (byte-blob-append a b))
              (test 
               (sprintf "byte-blob->file")
               '(1 2 3 4) 
               (byte-blob->list
                (file->byte-blob temp-path)))

              (byte-blob->file temp-path (byte-blob-append a b) #:append)
              (test 
               (sprintf "byte-blob->file append mode")
               '(1 2 3 4 1 2 3 4) 
               (byte-blob->list
                (file->byte-blob temp-path)))

              (delete-file temp-path))

(test-end)
(test-exit)
