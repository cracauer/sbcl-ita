;;;; implementation of CL:DOCUMENTATION

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is in the public domain and is provided with absolutely no
;;;; warranty. See the COPYING and CREDITS files for more information.

(in-package "SB-C") ; FIXME: not the best package for FDOCUMENTATION

;;; FDOCUMENTATION refers to STRUCTURE-CLASS which has only a skeletal
;;; representation during cross-compilation. Better to define this late.
(defun fdocumentation (x doc-type)
  (case doc-type
    (variable
     (typecase x
       (symbol (values (info :variable :documentation x)))))
    (function
     ;; Unused
     (error "FUNCTION doc-type is not supported."))
    (structure
     (typecase x
       (symbol (cond
                 ((eq (info :type :kind x) :instance)
                  (values (info :type :documentation x)))
                 ((info :typed-structure :info x)
                  (values (info :typed-structure :documentation x)))))))
    (type
     (typecase x
       (structure-class (values (info :type :documentation (class-name x))))
       (t (and (typep x 'symbol) (values (info :type :documentation x))))))
    (setf (values (info :setf :documentation x)))
    ((t)
     (typecase x
       (function (%fun-doc x))
       (package (package-doc-string x))
       (structure-class (values (info :type :documentation (class-name x))))
       ((or symbol cons)
        (random-documentation x doc-type))))
    (t
     (when (typep x '(or symbol cons))
       (random-documentation x doc-type)))))

(in-package "SB-PCL")

(defun fun-doc (x)
  (if (typep x 'generic-function)
      (slot-value x '%documentation)
      (%fun-doc x)))

(defun (setf fun-doc) (new-value x)
  (if (typep x 'generic-function)
      (setf (slot-value x '%documentation) new-value)
      (setf (%fun-doc x) new-value)))

(defun set-function-name-documentation (name documentation)
  (aver name)
  (cond ((not (legal-fun-name-p name))
         nil)
        ((not (equal (sb-c::real-function-name name) name))
         (setf (random-documentation name 'function) documentation))
        (t
         (setf (fun-doc (or (and (symbolp name)
                                 (macro-function name))
                            (fdefinition name)))
               documentation)))
  documentation)

;;; Generic behavior

(defmethod (setf documentation) :around (new-value (x (eql nil)) doc-type)
  (style-warn "Ignoring doc-type ~a for ~a." doc-type nil)
  new-value)

;;; default if DOC-TYPE doesn't match one of the specified types
(defmethod documentation (object doc-type)
  (warn "unsupported DOCUMENTATION: doc-type ~S for object of type ~S"
        doc-type (type-of object))
  nil)

;;; default if DOC-TYPE doesn't match one of the specified types
(defmethod (setf documentation) (new-value object doc-type)
  ;; CMU CL made this an error, but since ANSI says that even for supported
  ;; doc types an implementation is permitted to discard docs at any time
  ;; for any reason, this feels to me more like a warning. -- WHN 19991214
  (warn "discarding unsupported DOCUMENTATION: doc-type ~S for object of type ~S"
        doc-type (type-of object))
  new-value)

;;; Deprecation note

(defun maybe-add-deprecation-note (namespace name documentation)
  (unless (member namespace '(function variable type))
    (return-from maybe-add-deprecation-note documentation))
  (binding* (((state since replacements)
              (deprecated-thing-p namespace name))
             (note (when state
                     (with-simple-output-to-string (stream)
                       (sb-impl::print-deprecation-message
                        namespace name (first since) (second since)
                        replacements stream)))))
    (cond
      ((and documentation note)
       (concatenate
        'string note #.(format nil "~2%") documentation))
      (documentation)
      (note))))

(defmethod documentation :around ((x t) (doc-type t))
  (let ((namespace (cond
                     ((typep x 'function)
                      'function)
                     ((eq doc-type 'compiler-macro)
                      'function)
                     ((typep x 'class)
                      'type)
                     ((eq doc-type 'structure)
                      'type)
                     (t
                      doc-type)))
        (name (cond
                ((typep x 'function)
                 (%fun-name x))
                ((typep x 'class)
                 (class-name x))
                (t
                 x)))
        (documentation (call-next-method)))
    (maybe-add-deprecation-note namespace name documentation)))

;;; functions, macros, and special forms

(flet ((maybe-function-documentation (name)
         (cond
           ((not (legal-fun-name-p name)))
           ((random-documentation name 'function))
           ;; Nothing under the name, check the function object.
           ((fboundp name)
            (fun-doc (cond
                       ((and (symbolp name) (special-operator-p name))
                        (fdefinition name))
                       ((and (symbolp name) (macro-function name)))
                       ((fdefinition name))))))))

  (defmethod documentation ((x function) (doc-type (eql 't)))
    (fun-doc x))

  (defmethod documentation ((x function) (doc-type (eql 'function)))
    (fun-doc x))

  (defmethod documentation ((x list) (doc-type (eql 'compiler-macro)))
    (awhen (compiler-macro-function x)
      (documentation it t)))

  (defmethod documentation ((x list) (doc-type (eql 'function)))
    (maybe-function-documentation x))

  (defmethod documentation ((x symbol) (doc-type (eql 'function)))
    (maybe-function-documentation x))

  (defmethod documentation ((x symbol) (doc-type (eql 'compiler-macro)))
    (awhen (compiler-macro-function x)
      (documentation it t)))

  (defmethod documentation ((x symbol) (doc-type (eql 'setf)))
    (fdocumentation x 'setf)))

(defmethod documentation ((x symbol) (doc-type (eql 'optimize)))
  (random-documentation x 'optimize))

(defmethod (setf documentation) (new-value (x function) (doc-type (eql 't)))
  (setf (fun-doc x) new-value))

(defmethod (setf documentation) (new-value (x function) (doc-type (eql 'function)))
  (setf (fun-doc x) new-value))

(defmethod (setf documentation) (new-value (x list) (doc-type (eql 'function)))
  (set-function-name-documentation x new-value))

(defmethod (setf documentation) (new-value (x list) (doc-type (eql 'compiler-macro)))
  (awhen (compiler-macro-function x)
    (setf (documentation it t) new-value)))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'function)))
  (set-function-name-documentation x new-value))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'compiler-macro)))
  (awhen (compiler-macro-function x)
    (setf (documentation it t) new-value)))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'setf)))
  (setf (fdocumentation x 'setf) new-value))

;;; method combinations
(defmethod documentation ((x method-combination) (doc-type (eql 't)))
  (slot-value x '%documentation))

(defmethod documentation
    ((x method-combination) (doc-type (eql 'method-combination)))
  (slot-value x '%documentation))

(defmethod documentation ((x symbol) (doc-type (eql 'method-combination)))
  (random-documentation x 'method-combination))

(defmethod (setf documentation)
    (new-value (x method-combination) (doc-type (eql 't)))
  (setf (slot-value x '%documentation) new-value))

(defmethod (setf documentation)
    (new-value (x method-combination) (doc-type (eql 'method-combination)))
  (setf (slot-value x '%documentation) new-value))

(defmethod (setf documentation)
    (new-value (x symbol) (doc-type (eql 'method-combination)))
  (setf (random-documentation x 'method-combination) new-value))

;;; methods
(defmethod documentation ((x standard-method) (doc-type (eql 't)))
  (slot-value x '%documentation))

(defmethod (setf documentation)
    (new-value (x standard-method) (doc-type (eql 't)))
  (setf (slot-value x '%documentation) new-value))

;;; packages

;;; KLUDGE: It's nasty having things like this accessor
;;; (PACKAGE-DOC-STRING) floating around out in this mostly-unrelated
;;; source file. Perhaps it would be better to support WARM-INIT-FORMS
;;; by analogy with the existing !COLD-INIT-FORMS and have them be
;;; EVAL'ed after basic warm load is done? That way things like this
;;; could be defined alongside the other code which does low-level
;;; hacking of packages.. -- WHN 19991203

(defmethod documentation ((x package) (doc-type (eql 't)))
  (package-doc-string x))

(defmethod (setf documentation) (new-value (x package) (doc-type (eql 't)))
  (setf (package-doc-string x) new-value))

;;; types, classes, and structure names

(macrolet
    ((define-type-documentation-methods (specializer get-form set-form)
       `(progn
          (defmethod documentation ((x ,specializer) (doc-type (eql 't)))
            ,get-form)

          (defmethod documentation ((x ,specializer) (doc-type (eql 'type)))
            (documentation x t))

          (defmethod (setf documentation) (new-value
                                           (x ,specializer)
                                           (doc-type (eql 't)))
            ,set-form)

          (defmethod (setf documentation) (new-value
                                           (x ,specializer)
                                           (doc-type (eql 'type)))
            (setf (documentation x 't) new-value))))
     (define-type-documentation-lookup-methods (doc-type)
       `(progn
          (defmethod documentation ((x symbol) (doc-type (eql ',doc-type)))
            (acond
             ((find-class x nil)
              (documentation it t))
             (t
              (fdocumentation x ',doc-type))))

          (defmethod (setf documentation) (new-value
                                           (x symbol)
                                           (doc-type (eql ',doc-type)))
            (acond
             ((find-class x nil)
              (setf (documentation it t) new-value))
             (t
              (setf (fdocumentation x ',doc-type) new-value)))))))

  (define-type-documentation-methods structure-class
      (fdocumentation (class-name x) 'type)
      (setf (fdocumentation (class-name x) 'type) new-value))

  (define-type-documentation-methods class
      (slot-value x '%documentation)
      (setf (slot-value x '%documentation) new-value))

  ;; although the CLHS doesn't mention this, it is reasonable to
  ;; assume that parallel treatment of condition-class was intended
  ;; (if condition-class is in fact not implemented as a
  ;; standard-class or structure-class).
  (define-type-documentation-methods condition-class
      (fdocumentation (class-name x) 'type)
      (setf (fdocumentation (class-name x) 'type) new-value))

  (define-type-documentation-lookup-methods type)
  (define-type-documentation-lookup-methods structure))


;;; variables
(defmethod documentation ((x symbol) (doc-type (eql 'variable)))
  (fdocumentation x 'variable))

(defmethod (setf documentation) (new-value
                                 (x symbol)
                                 (doc-type (eql 'variable)))
  (setf (fdocumentation x 'variable) new-value))

;;; extra-standard methods, for getting at slot documentation
(defmethod documentation ((slotd standard-slot-definition) (doc-type (eql 't)))
  (declare (ignore doc-type))
  (slot-value slotd '%documentation))

(defmethod (setf documentation)
    (new-value (slotd standard-slot-definition) (doc-type (eql 't)))
  (declare (ignore doc-type))
  (setf (slot-value slotd '%documentation) new-value))

;;; Now that we have created the machinery for setting documentation, we can
;;; set the documentation for the machinery for setting documentation.
#+sb-doc
(setf (documentation 'documentation 'function)
      "Return the documentation string of Doc-Type for X, or NIL if none
exists. System doc-types are VARIABLE, FUNCTION, STRUCTURE, TYPE, SETF, and T.

Function documentation is stored separately for function names and objects:
DEFUN, LAMBDA, &co create function objects with the specified documentation
strings.

 \(SETF (DOCUMENTATION NAME 'FUNCTION) STRING)

sets the documentation string stored under the specified name, and

 \(SETF (DOCUMENTATION FUNC T) STRING)

sets the documentation string stored in the function object.

 \(DOCUMENTATION NAME 'FUNCTION)

returns the documentation stored under the function name if any, and
falls back on the documentation in the function object if necessary.")
