
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

(test-end)
(test-exit)
