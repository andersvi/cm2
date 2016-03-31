;;; 
;;; ASDF 3.1 compliant system definition file for CM-2
;;;
;;; Anders Vinjar, March 2016
;;;
;;; Time-stamp: <2016-03-31 13:55:42 andersvi>
;;; 

(require :asdf)

;; Based heavily on code found in .asd file inside Robert Brown
;; <robert.brown@gmail.com> CL protobuf code.

(in-package :asdf-user)

(defclass cm-scm-source-file (cl-source-file)
  ((special-scm-pathname :initarg :scm-pathname
			  :initform nil
			  :reader scm-pathname
			  :documentation
			  "Name of .scm file if different from component"))
  (:documentation "A CM .scm source file."))

(defclass scm-to-lisp (operation)
  ()
  (:documentation
   "An ASDF operation that translates a .scm file into a Lisp source
   file, compiling and loading this"))

(defmethod component-depends-on ((operation compile-op) (component cm-scm-source-file))
  "Compiling a scm file depends on generating Lisp source code for
the .scm, but also on loading package definitions and in-line function
definitions that the machine-generated scm Lisp code uses."
  `((scm-to-lisp ,(component-name component))
    ,@(call-next-method)))

(defmethod component-depends-on ((operation load-op) (component cm-scm-source-file))
  "Loading a scm file depends on generating Lisp source code for the
.scm, but also on loading package definitions and in-line function
definitions that the machine-generated scm Lisp code uses."
  `((scm-to-lisp ,(component-name component))
    ,@(call-next-method)))

(defun scm-input (cm-scm-source-file)
  "Returns the pathname of the scm file that must be
translated into Lisp source code for this SCM-FILE component."
  (if (scm-pathname cm-scm-source-file)
      ;; Path of the scm file was specified with :SCM-PATHNAME.
      (merge-pathnames
       (make-pathname :type "scm")
       (merge-pathnames (pathname (scm-pathname cm-scm-source-file))
                        (component-pathname (component-parent cm-scm-source-file))))
    ;; No :SCM-PATHNAME was specified, so the path of the scm file
    ;; defaults to that of the Lisp file, but with a ".scm" suffix.
    (let ((lisp-pathname (component-pathname cm-scm-source-file)))
      (merge-pathnames (make-pathname :type "scm") lisp-pathname))))

(defmethod input-files ((operation scm-to-lisp) (component cm-scm-source-file))
  (list (scm-input component)))

(defmethod output-files ((operation scm-to-lisp) (component cm-scm-source-file))
  "Arranges for the Lisp output file of SCM-TO-LISP operations to be stored
where fasl files are located."
  (values (list (component-pathname component))
          nil))			   ; allow around methods to translate

(defmethod perform :before ((operation scm-to-lisp) (component cm-scm-source-file))
  (map nil #'ensure-directories-exist (output-files operation component)))

;;; calls #'stocl (in cm2/src/stocl.lisp) on .scm file, generating .lisp:

(defmethod perform ((operation scm-to-lisp) (component cm-scm-source-file))
  (let* ((source-file (scm-input component))
         (source-file-argument (namestring source-file))
         ;; Around methods on output-file may globally redirect output products, so we must call
         ;; that method instead of executing (component-pathname component).
         (output-file (first (output-files operation component))))
    (cl-user::stocl source-file-argument :file output-file :directory (directory-namestring output-file))))

(defmethod asdf::component-self-dependencies :around ((op load-op) (c cm-scm-source-file))
  "Removes SCM-TO-LISP operations from self dependencies.  Otherwise, the Lisp
output files of SCM-TO-LISP are considered to be input files for LOAD-OP,
which means ASDF loads both the .lisp file and the .fasl file."
  (remove-if (lambda (x)
               (eq (car x) 'scm-to-lisp))
             (call-next-method)))

(defmethod input-files ((operation compile-op) (c cm-scm-source-file))
  (output-files 'scm-to-lisp c))

(defsystem "cm2"
  :description "CM v.2 code for CL"
  :long-description "CL implementation of Common Music version 2"
  :version "2.12.0"
  :author "Rick Taube <taube (at) uiuc.edu>"
  :licence "LLGPL"
  :components
  ((:module "src/lisp"
	    :pathname "src"
	    :components ((:file "pkg")
			 #+sbcl (:file "sbcl")
			 #+lispworks (:file "lispworks")
			 (:file "stocl")
			 (:file "level1")
			 (:file "clos")))
   (:module "src/scm"
	    :pathname "src"
	    :components ((:cm-scm-source-file "iter" :scm-pathname "loop")
			 (:cm-scm-source-file "utils")
			 (:cm-scm-source-file "mop")
			 (:cm-scm-source-file "objects")
			 (:cm-scm-source-file "data")
			 (:cm-scm-source-file "scales")
			 (:cm-scm-source-file "spectral")
			 (:cm-scm-source-file "patterns" )
			 (:cm-scm-source-file "io")
			 (:cm-scm-source-file "scheduler")
			 (:cm-scm-source-file "gnuplot")
			 (:cm-scm-source-file "plt")
			 (:cm-scm-source-file "sco")
			 (:cm-scm-source-file "clm")
			 (:cm-scm-source-file "midi1")
			 (:cm-scm-source-file "midi2")
			 (:cm-scm-source-file "midi3")
			 (:cm-scm-source-file "cmn")
			 (:cm-scm-source-file "fomus")
			 (:cm-scm-source-file "midishare")
			 (:cm-scm-source-file "player")
			 (:cm-scm-source-file "sc")
			 (:cm-scm-source-file "pm")
			 (:cm-scm-source-file "rt")
			 ))
   (:module "src/after"
	    :pathname "src"
	    :components ((:file "parse")
			 (:file "sal")))))



