;;; **********************************************************************
;;; Copyright (C) 2002 Heinrich Taube (taube@uiuc.edu) 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; **********************************************************************

;;; $Name$
;;; $Revision$
;;; $Date$

(define (sco-par-print par objv filv timv)
  ;; return a form that prints par to a .sco file. passed the
  ;; object, port and scoretime variables. the only real oddity
  ;; is that the scoretime variable currently substitutes for
  ;; the object-time accessor.
  (let* ((raw (if (parameter-time? par)
		timv
		(slot-getter-form objv (parameter-slot par))))
	 (acc (if (parameter-decimals par)
		`(decimals ,raw ,(parameter-decimals par))
		raw))
	 (delim #\space))
    (case (parameter-type par)
      ((required optional key)
       `(begin
         (write-char ,delim ,filv)
         (write ,acc ,filv)))
      ((rest )
       (let ((v (gensym)))
	 `(if (slot-bound? ,objv ',(parameter-slot par))
	    (do ((,v ,(slot-getter-form objv (parameter-slot par))
                     (cdr ,v)))
	        ((null? ,v) #f)
	      (write-char ,delim ,filv)
	      (write (car ,v) ,filv))))))))      

(define (make-sco-writer objclassname objclassvar pars supers sdecl)
  ;; define-output-method in level1.
  sdecl supers   ; gag 'unused var' message from cltl compilers
  (define-output-method objclassname objclassvar 'obj
    'sco-file '<sco-file> 'io 'scoretime
    (list `(let ((fp (io-open io)))
             (display (sco-name obj) fp)
             ,@ (map (lambda (p) (sco-par-print p 'obj 'fp 'scoretime))
                     pars) 
                (newline fp)
                (values)))))

(define-class* <sco-file> (<event-file>)
  ((header :init-value #f :init-keyword :header
           :accessor sco-file-header))
  :name 'sco-file
  :metaclass <io-class>
  :file-types '("*.sco")
  :definer (function make-sco-writer))

(define (set-sco-output-hook! fn)
  (unless (or (not fn) (procedure? fn))
    (err "Not a sco hook: ~s" fn))
  (set! (io-class-output-hook <sco-file>) fn)
  (values))

(define *csound* "csound -d -m 0")

(define (play-sco-file file . args)
  (let* ((cmd *csound*)
         (dir (or (filename-directory file) ""))
         (orc (list-prop args ':orchestra))
         (snd (list-prop args ':output))
         (tyo (list-prop args ':verbose))
         (wai (list-prop args ':wait (if snd #t #f)))
         (pla (list-prop args ':play (if snd #t #f))))
    (cond ((not orc) #f)
          (else
           (unless (filename-directory orc)
             (set! orc (string-append dir orc)))
           (when snd
             (unless (filename-directory snd)
               (set! snd (string-append dir snd)))
             (set! wai #t)
             (set! cmd (string-append cmd " -o " snd)))
           (set! cmd (string-append cmd " " orc " " file))
           (if tyo (format #t "~%; ~a" cmd))
           (shell cmd :wait wai)
           (if pla (play snd :wait #f))
           file))))

(set-sco-output-hook! (function play-sco-file))

;(defobject i1 ()
;  ((time :accessor object-time)
;    dur
;    frq
;    amp)
;  (:parameters time dur frq amp))

(define-method* (initialize-io (io <sco-file>))
  (format (io-open io)
	  "; ~a output on ~a~%"
	  (cm-version)
	  (date-and-time))
  (let ((header (sco-file-header io)))
    (cond ((pair? header)
	   (dolist (h header) (format (io-open io) "~a~%" h)))
	  ((string? header)
	   (format (io-open io) "~a~%" header))
	  ((not header)
	   #f)
	  (else
	   (err "Bad .sco file header: ~s" header)))))

;;;
;;; added back i and f for easy subclassing
;;;

(defobject i (event) 
           (ins dur)
  ;(:parameters time dur)            ; dont geneate output methods
  (:event-streams sco-file ) ; generate event methods for subclasses
  )

(defobject f (event) 
           (num size gen)
;  (:parameters time size gen)            ; dont generat output methods
  (:event-streams sco-file)
  )

(define-method* (sco-name (obj <i>))
  ;; if ins slot is bound use it, otherwise use class name
  (if (slot-bound? obj 'ins)
    (format #f "i~a" (slot-ref obj 'ins))
    (let ((n (symbol->string (class-name (class-of obj)))))
      (if (char-lower-case? (string-ref n 0))
        n
        (string-downcase n)))))

(define-method* (sco-name (obj <f>))
  ;; if num slot is bound use it, otherwise use class name
  (if (slot-bound? obj 'num)
    (format #f "f~a" (slot-ref obj 'num))
    (let ((n (symbol->string (class-name (class-of obj)))))
      (if (char-lower-case? (string-ref n 0))
        n
        (string-downcase n)))))

; (defobject i1 (i) (time dur freq amp) (:parameters time dur freq amp))
; (defobject f1 (f) ((env :initform '())) (:parameters time size gen &rest env))

;;;dont think anyone uses this
;; (define-method* (write-event (obj <midi>) (fil <sco-file>) scoretime)
;;   (let ((ins (midi-channel->name (midi-channel obj))))
;;     (if ins
;;       (format (io-open fil) "~a ~s ~s ~s ~s~%"
;; 	      ins
;; 	      scoretime
;; 	      (midi-duration obj)
;; 	      (hertz (midi-keynum obj))
;; 	      (midi-amplitude obj)))))

;;;
;;; .sco file importing
;;;

; (carry-pars (list "." "." "3" "4") (list 1 2 3 4 5 6))
; (carry-pars (list "." "+" "3" "4") (list 1 2 3 4 5 6))
; (carry-pars '() (list 1 2 3 4 5 6))

(define (carry-pars pars last)
  (let ((data pars)
        (head last)
        (pnum 1))
    (if (null? last)
      (loop for p in data 
         for i from 1
         collect
         (if (or (string=? p ".")
                 (string=? p "+")
                 (char=? (string-ref p 0) #\^))
           (err "No p~a value to carry in ~s" i pars)
           (string-read p)))
      (do ()
          ((or (null? data) (null? last))
           (if (and (null? data) (not (null? last)))
             (append pars last)
             pars))
        (cond ((string=? (car data) ".")
               (set-car! data (car last)))
              ((string=? (car data) "+") 
               (unless (= pnum 2)
                 (err "Found p2 carry value ~s in p~d." 
                      (car pars) pnum))
               (set-car! data (+ (cadr head) (caddr head))))
              ((char=? (string-ref (car data) 0) #\^)
               (unless (= pnum 2)
                 (err "Found p2 carry value ~S in p~d." 
                      (car pars) pnum))
               (let ((n (string-read (substring (car data) 1))))
                 (set-car! data (+ (cadr head) n))))
              (else
               ;; update last
               (set-car! data (string-read (car data)))
               (unless (number? (car data))
                 (format #t "; warning: Importing non-numerical p~s value: ~s."
                         pnum (car data))
                 ;;(SETF (CAR data) `(QUOTE ,(CAR DATA)))
                 )
               (set-car! last (car data))))
        (set! data (cdr data))
        (set! last (cdr last))
        (set! pnum (+ pnum 1))))))

(define (parse-i-statement line last)
  (let ((pars (list)))
    (set! pars (string-substrings line :start 1 ))
    (if (null? pars)
      (set! pars (list-copy
                  (if (null? last)
                    (err "Dangling i_statement: ~a" line)
                    last)))
      (if (not (null? last))
        (let ((p (car pars)))
          ;; see if we have new whole number i_statement 
          ;; and zap last if so.
          (when (or (char-numeric? (string-ref p 0))      
                    (and (char=? (string-ref p 0) #\-)
                         (char-numeric? (string-ref p 1))))
            (let ((n (string-read p)))
              (unless (= (inexact->exact (floor n))
                         (inexact->exact (floor (first last))))
                (set! last (list)))))
          (if (not (null? last))
            (set! pars (carry-pars pars last))
            (begin (set! pars (carry-pars pars (list)) )
                   (set! last (list-copy pars)))
            ))
        ;; dont have last 
        (begin (set! pars (carry-pars pars (list)))
               (set! last (list-copy pars)))))
    (values pars last)))

(define-macro (checkdefs pars d  )
  (let ((i (gensym)))
    `(let ((,i (inexact->exact 
                (floor (car ,pars)))))
       (unless (assoc ,i ,d)
         (push (list ,i 
                     (length ,pars)
                     (string->symbol (format #f "i~a" ,i))) 
               ,d)))))

(define-method* (import-events (io <sco-file>) . args)
  (with-args (args &key output)
    (let ((secs 0)
          (rate 60)
          (beat 0)
          (head (list))
          (list (list))
          (defs (list))
          (line #f)
          (next #f)
          (last (list))
          (pars (list))
          (sort #f)
          (keys (list))
          (statement #f)
          (in #f)
          (path #f)
          (name #f)
          (stop #f)
          )
      rate keys path name
      (with-open-io (input io :input)
        (set! in (io-open input))
        (do ()
            (stop #f)
          (if next
            (set! line next)
            (let ((raw (file-line in )))
              (if (file-eof? raw )
                (set! line raw)
                (let ((pos ;;(position #\; raw)
                       (do ((i 0 (+ i 1))
                            (e (string-length raw))
                            (f #f))
                           ((or f (= i e)) f)
                         (if (char=? (string-ref raw i) #\;)
                           (set! f i)))))
                  (if pos (set! raw (substring raw 0 pos)))
                  (set! line (strip-chars raw))))))
          (cond ((file-eof? line)
                 (set! stop #t))
                ((string=? line "")
                 (set! next #f))
                ((member (string-ref line 0)
                         '(#\i #\f #\a #\t #\s #\e))
                 ;; start of score file statement
                 (if statement
                   (begin
                    ;; begin process_statement
                    (case (string-ref statement 0)
                      (( #\i )
                       (multiple-value-setq 
                           (pars last)
                         (parse-i-statement statement last))
                       (push pars list)
                       ;(CHECKSORT pars sort beat)
                       (if (< (cadr pars) beat)
                         (set! sort #t)
                         (set! beat (cadr pars)))
                       ;; register i number if new
                       (CHECKDEFS pars defs))
                      (( #\f  )
                       (push statement head)
                       (set! last #f))
                      (( #\a )
                       (format 
			#t "; Warning: a_statement not implemented: ~s~%" 
			line)
                       (set! last #f))
                      (( #\t )
                       ;; WAS STRING-FORMS
                       (set! rate (string-read (substring statement 1)))
                       (set! last #f))
                      (( #\s )
                       (set! secs (+ secs 1))
                       (when (> secs 1)
                         (format #t
                                 "; Warning: Multiple 's' not implemented.~%"))
                       (set! last #f))
                      (( #\e )
                       (set! stop #t)
                       (set! last #f)))
                    ;; end process_statement
                    (begin (set! statement #f) (set! next line)
                           ))
                   (begin (set! statement line) (set! next #f)
                          ))
                 )
		(else
		 ;; continuation of statement or unknown line
		 (if statement
		     (set! statement 
			   (string-append statement " " line))
		     (begin
		       (format #t
			       "Skipping unimplemented statement: ~S~%" line)
		       (set! statement #f)))
		 (set! next #f))
		)
          )
        ;;finally
        (if statement 
          (if (char=? (string-ref statement 0) #\i)
            (let ((pars (parse-i-statement statement last)))
              (push pars list)
              ;;(CHECKSORT pars sort beat)
              (if (< (cadr pars) beat)
                (set! sort #t)
                (set! beat (cadr pars)))
              (CHECKDEFS pars defs))))
        )
      ;; write output file if objects and no error
      (if (and (file-eof? line) list)
        (begin
         (if sort
           (begin (format #t "~%Sorting i statements...")
                  (set! list (sort list
                                   (lambda (a b)
                                     (< (cadr a) (cadr b)))))
                  (format #t "done!"))
           (set! list (reverse! list)))
         (sco-write-and-load io output defs list)
         )
        #f))))

; (import-events "/Users/hkt/a1.sco")

(define (sco-write-and-load . args) args 
  (car (last args))
  )

;; (define (write-sco-and-load io file defs head list)
;;   ;; save f_statements in file header
;;   (let* ((maxpars 0)
;;          (infile (io-filename io))
;;          (seqname (string-append "from-" (pathname-name infile)))
;;          (outfile (or file
;;                       (string-append (filename-directory infile)
;;                                      (filename-file infile)
;;                                      ".cm")))
;;          (*print-case* ':downcase))
;;     ;; get max number of pars
;;     (dolist (d defs) (set! maxpars (max maxpars (cadr d))))
;;     (set! (sco-file-header io) head)
;;     ;; get data in proper order.
;;     (set! defs (reverse! defs))
;;     ;; collect :initargs for def with most pars
;;     (set! keys (loop for i from 1 to maxpars
;;                   for s = (string->symbol
;;                            (string-append (symbol->string 'p)
;;                                           (number->string i)))
;;                   collect (list s (symbol->keyword s))))
;;     (set! out (open-file outfile :output))
;;     (format out ";;; Imported from ~s on ~a~%"
;;             infile (date-and-time))
;;     ;; add defobjects for each i found.
;;     (dolist (d defs)
;;       (pprint
;;        (let* ((n (second d))
;;               (c (third d))
;;               (s (loop for p from 4 to n
;;                     for k in (cdddr keys)
;;                     collect (first k))))
;;          `(defobject ,c (i)
;;             ,(loop for p in s for i from 4
;;                 collect
;;                 `(,p :initarg ,p 
;;                      :initarg ,(second (elt keys (1- i)))))
;;             (:parameters time dur ,@s)))
;;        :stream out)
;;       (terpri out))
;;     (format out "~&(MAKE 'SEQ :NAME ~S~&  ~
;;                        :SUBOBJECTS~&  ~
;;                        (LIST" name)
;;                                         ;(PRINT LIST)
;;     (dolist (l list)
;;       (format out "~&    ")
;;       (write `(make 
;;                ',(third (find (floor (first l))
;;                               defs :key #'first))
;;                ., (loop for v in l
;;                      for i from 1 
;;                      collect
;;                      (second (elt keys (1- i))) 
;;                      collect (quote-if-necessary v)))
;;              :stream out))
;;     (format out "~&    ))~&(FIND-OBJECT ~S)~&" name))
;;   (if path
;;     (progn (load path ) (find-object name))      
;;     #f)
;;   )
;;)

;(defobject i (event) 
;           (ins dur)
;  (:parameters time dur)
;  (:event-streams )            ; dont define output methods
;  )