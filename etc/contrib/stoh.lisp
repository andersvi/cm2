;;; **********************************************************************
;;; Copyright (C) 2006 Rick Taube
;;; This program is free software; you can redistribute it and/or   
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************
;;; $Revision$
;;; $Date$

(in-package cm)

; (sal "load \"~/404A/sal/test.sal\"")

(defmacro getchar (x s)
 `(progn (setq ,x (read-char ,s nil ':eof))
	 (if (eq ,x ':eof) (return-from :read ':eof))))

(defun begspan (out type)
  (format out "<span class=\"~(~A~)\">" type))

(defun endspan (out &optional eol )
  (format out "</span>")
  (if eol (terpri out)))

(defmacro cpwhite (ch in ou bo)
  `(loop while (member ,ch +whites+)
      do (write-char ,ch ,ou)
	(if (eq ,ch #\Newline) (setq ,bo t)
	    (setq ,bo nil))
	(getchar ,ch ,in)))

(defun salhtmlheader (out source-url)
  (format out "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">
<head>
<meta http-equiv=\"content-type\" content=\"text/html; charset=iso-8859-1\"/>
<style type=\"text/css\" media=\"all\">
span.string  {color: #bc8f8f;} 
span.output  {color: #bc8f8f;} 
span.comment {color: #b22222;}  
span.keyword {color: #da70d6;} 
span.reserved {color: #a020f0;}
span.classof {color: #228b22;}
span.command {color: #0000ff;}
span.constant {color: #5f9ea0;}
pre.salcode{ background: #f7f7f7; }
</style>
<title>~A.html</title>
</head>
<body>
" (pathname-name source-url))
  (format out "<p><span style=\"font-size:large;\">Download source: ")
  (format out "<a href=~S>
<img src=\"textfile.png\" style=\"border:none;\" alt=\"[fileicon]\" /> ~A.~A</a>"
	  (namestring source-url)
	  (pathname-name source-url)
	  (pathname-type source-url))
  (format out "</span></p>~%<hr/>"))

(defun salhtmlfooter (out file)
  out file
  (format out "<hr/>~%<div style=\"float:left;\"><p>Generated by <em>sal2html</em> ~A</p></div><div style=\"float:right;\"><a href=\"http://validator.w3.org/check?uri=referer\"><img src=\"http://www.w3.org/Icons/valid-xhtml10\" style=\"border:none;\" alt=\"Valid XHTML 1.0 Strict\" height=\"31\" width=\"88\"/></a></div>~%</body>~%</html>~%"
	  (date-and-time)))

(defparameter tokentypes 
  `((:classof "variable" "function" "process"
	      , (lambda (s l)
		  (and (char= (elt s 0) #\<)
		       (char= (elt s (- l 1)) #\>))))
   (:keyword , (lambda (s l)
		 (char= (elt s (- l 1)) #\:)))
   (:reserved
    "begin" "chdir" "define" "exec" "if" "load" "loop" "open" "play" "plot"
    "print" "rts" "set"  "sprout" "system"
    "run" "exec" "output" "return" "unless" "wait" "when" "with"
    "above" "below" "by" "collect" "downto" "else"
    "end"  "finally" "for" "from" "in" "over" "repeat" "return" 
    "then" "to" "unless" "until" "wait" "when" "while" "with")
    ))

(defun toktype (str len bol)
  bol
  (loop for l in tokentypes
       do (loop for x in (cdr l)
	     do (if (stringp x)
		    (if (string-equal str x) 
			(return-from toktype (car l)))
		    (if (funcall x str len)
			(return-from toktype (car l)))))))

(defun htmlwritechar (c out)
  (cond ((eq c #\<) (write-string "&lt;" out))
	((eq c #\>) (write-string "&gt;" out))
	((eq c #\&) (write-string "&amp;" out))
	(t (write-char c out))))
	 
(defun htmlwritestring (s out &optional len)
  (loop for i below (or len (length s))
       do (htmlwritechar (elt s i) out)))

(defun colorizetok (out str len bol)
  (let ((type (toktype str len bol)))
    (if (eql type :reserved)
	(if bol (setq type :command)))
    (if type (begspan out type))
    (htmlwritestring str out len)
    (if type (endspan out))))
		   
(defun sal2html (file &optional html source-url)
  (unless html
    (setq html (make-pathname :type "html" :defaults file)))
  (unless source-url
    (setq source-url file))
  (let ()
    (with-open-file (sal file)
      (with-open-file (out html :direction :output
			   :if-exists :supersede 
			   :if-does-not-exist ':create)
	(salhtmlheader out (namestring (make-pathname :name (pathname-name file)
						      :type (pathname-type file)
						      :defaults source-url)))
	(format out "<pre class=\"salcode\">~%")
	(block :read
	  (loop with bol = t and chr and len
	       and str = (make-string 256)
	     do
	       (getchar chr sal)
	     ;; copy all white chars to output
	       (cpwhite chr sal out bol)
	     ;; chr now non-white char, possibly at bol
	       (cond ((char= chr #\;)
		      (begspan out ':comment)
		      (loop do (htmlwritechar chr out)
			   (getchar chr sal)
			 until (eql chr #\Newline))
		      (endspan out t)
		      (setq bol t))
		     ((char= chr #\")
		      (begspan out ':string)
		      (loop do (htmlwritechar chr out)
			   (getchar chr sal)
			   until (eql chr #\"))
		      (htmlwritechar chr out)
		      (endspan out)
		      (setq bol nil))
		     ((char= chr #\#)
		      (begspan out :constant)
		      (htmlwritechar chr out)
		      (getchar chr sal)
		      (htmlwritechar chr out)
		      (endspan out)
		      (setq bol nil))
		      (t
		       ;; gobble token to next delim, which may be chr
		       (setq len 0)
		       
		       (loop until (member chr +delims+ :test #'char=)
			    do
			    (setf (elt str len) chr)
			    (getchar chr sal)
			    (incf len))
		       
		       (if (> len 0)
			   (colorizetok out (subseq str 0 len)
					len bol))
		       ;; write delimiter
		       (htmlwritechar chr out)
		       (if (member chr '(#\Return #\Newline))
			 (setq bol t)
			 (setq bol nil))))))
	(format out "</pre>~%")
	(salhtmlfooter out file)
	))
    html))

; (directory "/Users/hkt/404a/sal/*.sal")
; (stoh "/Users/hkt/404a/sal/*.sal")

(defun stoh (file &key html-dir source-url)
  (unless html-dir
    (setq html-dir (namestring (make-pathname :type nil :name nil
					      :defaults file))))
  (loop for f in (directory file) 
     unless (member (elt (pathname-name f) 0) '( #\# #\.))
     collect
       (let ((h (make-pathname :name (pathname-name f)
			       :type "html" :defaults html-dir))
	     (u source-url))
	 (sal2html f h u)) into l
     finally (return (if (null (cdr l)) (car l) l))))

; (stoh "/Volumes/Web Classes/404a/sal/plot.sal" 	:html-dir "/Volumes/Web Classes/404a/html/" 	:source-url "../sal/")
(defun 404a ()
  (stoh "/Volumes/Web Classes/404a/sal/*.sal"
	:html-dir "/Volumes/Web Classes/404a/html/"
	:source-url "../sal/"))

