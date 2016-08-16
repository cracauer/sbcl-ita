;;;; This file contains code which knows about both the type
;;;; representation and the compiler IR1 representation. This stuff is
;;;; used for doing type checking.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;;; FIXME: This is a poor name for this file, since CTYPE is the name
;;;; of the type used internally to represent Lisp types. It'd
;;;; probably be good to rename this file to "call-type.lisp" or
;;;; "ir1-type.lisp" or something.

(in-package "SB!C")

(declaim (type (or function null) *lossage-fun* *unwinnage-fun* *ctype-test-fun*))

;;; These are the functions that are to be called when a problem is
;;; detected. They are passed format arguments. If null, we don't do
;;; anything. The LOSSAGE function is called when something is
;;; definitely incorrect. The UNWINNAGE function is called when it is
;;; somehow impossible to tell whether the call is correct. (Thus,
;;; they should correspond fairly closely to the FAILURE-P and WARNINGS-P
;;; return values of CL:COMPILE and CL:COMPILE-FILE. However, see the
;;; KLUDGE note below for *LOSSAGE-DETECTED*.)
(defvar *lossage-fun*)
(defvar *unwinnage-fun*)

;;; the function that we use for type checking. The derived type is
;;; its first argument and the type we are testing against is its
;;; second argument. The function should return values like CSUBTYPEP.
(defvar *ctype-test-fun*)
;;; FIXME: Why is this a variable? Explain.

;;; *LOSSAGE-DETECTED* is set when a "definite incompatibility" is
;;; detected. *UNWINNAGE-DETECTED* is set when we can't tell whether the
;;; call is compatible or not. Thus, they should correspond very closely
;;; to the FAILURE-P and WARNINGS-P return values of CL:COMPILE and
;;; CL:COMPILE-FILE.) However...
;;;
;;; KLUDGE: Common Lisp is a dynamic language, even if CMU CL was not.
;;; As far as I can see, none of the "definite incompatibilities"
;;; detected in this file are actually definite under the ANSI spec.
;;; They would be incompatibilites if the use were within the same
;;; compilation unit as the contradictory definition (as per the spec
;;; section "3.2.2.3 Semantic Constraints") but the old Python code
;;; doesn't keep track of whether that's the case. So until/unless we
;;; upgrade the code to keep track of that, we have to handle all
;;; these as STYLE-WARNINGs. -- WHN 2001-02-10
(defvar *lossage-detected*)
(defvar *unwinnage-detected*)
(defvar *valid-fun-use-name*)

;;; Signal a warning if appropriate and set *FOO-DETECTED*.
(declaim (ftype (function (string &rest t) (values)) note-lossage note-unwinnage))
(defun note-lossage (format-string &rest format-args)
  (setq *lossage-detected* t)
  (when *lossage-fun*
    (apply *lossage-fun* format-string format-args))
  (values))
(defun note-unwinnage (format-string &rest format-args)
  (setq *unwinnage-detected* t)
  (when *unwinnage-fun*
    (apply *unwinnage-fun* format-string format-args))
  (values))


;;;; stuff for checking a call against a function type
;;;;
;;;; FIXME: This is stuff to look at when I get around to fixing
;;;; function type inference and declarations.

;;; A dummy version of SUBTYPEP useful when we want a functional like
;;; SUBTYPEP that always returns true.
(defun always-subtypep (type1 type2)
  (declare (ignore type1 type2))
  (values t t))

;;; Determine whether a use of a function is consistent with its type.
;;; These values are returned:
;;;    T, T: the call is definitely valid.
;;;    NIL, T: the call is definitely invalid.
;;;    NIL, NIL: unable to determine whether the call is valid.
;;;
;;; The ARGUMENT-TEST function is used to determine whether an
;;; argument type matches the type we are checking against. Similarly,
;;; the RESULT-TEST is used to determine whether the result type
;;; matches the specified result.
;;;
;;; Unlike the argument test, the result test may be called on values
;;; or function types. NODE-DERIVED-TYPE is intersected with the
;;; trusted asserted type.
;;;
;;; The error and warning functions are functions that are called to
;;; explain the result. We bind *COMPILER-ERROR-CONTEXT* to the
;;; combination node so that COMPILER-WARNING and related functions
;;; will do the right thing if they are supplied.
(defun valid-fun-use (call type &key
                      ((:argument-test *ctype-test-fun*) #'csubtypep)
                      (result-test #'values-subtypep)
                      ((:lossage-fun *lossage-fun*))
                      ((:unwinnage-fun *unwinnage-fun*)))
  (declare (type (or function null) result-test) (type combination call)
           ;; FIXME: Could TYPE here actually be something like
           ;; (AND GENERIC-FUNCTION (FUNCTION (T) T))?  How
           ;; horrible...  -- CSR, 2003-05-03
           (type ctype type))
  (let* ((*lossage-detected* nil)
         (*unwinnage-detected* nil)
         (*compiler-error-context* call)
         (args (combination-args call)))
    (if (fun-type-p type)
        (let* ((nargs (length args))
               (required (fun-type-required type))
               (min-args (length required))
               (optional (fun-type-optional type))
               (max-args (+ min-args (length optional)))
               (rest (fun-type-rest type))
               (keyp (fun-type-keyp type)))
          (cond
            ((fun-type-wild-args type)
             (loop for arg in args
                   and i from 1
                   do (check-arg-type arg *universal-type* i)))
            ((not (or optional keyp rest))
             (if (/= nargs min-args)
                 (note-lossage
                  "The function was called with ~R argument~:P, but wants exactly ~R."
                  nargs min-args)
                 (check-fixed-and-rest args required nil)))
            ((< nargs min-args)
             (note-lossage
              "The function was called with ~R argument~:P, but wants at least ~R."
              nargs min-args))
            ((<= nargs max-args)
             (check-fixed-and-rest args (append required optional) rest))
            ((not (or keyp rest))
             (note-lossage
              "The function was called with ~R argument~:P, but wants at most ~R."
              nargs max-args))
            ((and keyp (oddp (- nargs max-args)))
             (note-lossage
              "The function has an odd number of arguments in the keyword portion."))
            (t
             (check-fixed-and-rest args (append required optional) rest)
             (when keyp
               (check-key-args args max-args type))))

          (when result-test
            (let* ((dtype (node-derived-type call))
                   (out-type (or
                              (binding* ((lvar (node-lvar call) :exit-if-null)
                                         (dest (lvar-dest lvar)))
                                (when (and (cast-p dest)
                                           (eq (cast-type-to-check dest) *wild-type*)
                                           (immediately-used-p lvar call))
                                  (values-type-intersection
                                   dtype (cast-asserted-type dest))))
                              dtype))
                   (return-type (fun-type-returns type)))
              (multiple-value-bind (int win) (funcall result-test out-type return-type)
                (cond ((not win)
                       (note-unwinnage "can't tell whether the result is a ~S"
                                       (type-specifier return-type)))
                      ((not int)
                       (note-lossage "The result is a ~S, not a ~S."
                                     (type-specifier out-type)
                                     (type-specifier return-type))))))))
        (loop for arg in args
              and i from 1
              do (check-arg-type arg *wild-type* i)))
    (awhen (lvar-fun-name (combination-fun call) t)
      (let ((type (info :function :type it))
            (info (info :function :info it)))
        (when (and (not *lossage-detected*)
                   info
                   (fun-info-callable-check info))
          (let ((*valid-fun-use-name* it))
            (apply (fun-info-callable-check info)
                   (resolve-key-args args type))))
        ;; One more check for structure constructors:
        (when (typep type 'defstruct-description)
          (awhen (assq it (dd-constructors type))
            (check-structure-constructor-call call type (cdr it))))))
    (cond (*lossage-detected* (values nil t))
          (*unwinnage-detected* (values nil nil))
          (t (values t t)))))

;;; Turn constant LVARs in keyword arg positions to constants so that
;;; they can be passed to FUN-INFO-CALLABLE-CHECK.
(defun resolve-key-args (args type)
  (if (fun-type-keyp type)
      (let ((non-key (+ (length (fun-type-required type))
                        (length (fun-type-optional type))))
            key-arguments)
        (do ((key (nthcdr non-key args) (cddr key)))
            ((null key))
          (let ((k (first key))
                (v (second key)))
            (when (constant-lvar-p k)
              (let* ((name (lvar-value k))
                     (info (find name (fun-type-keywords type)
                                 :key #'key-info-name)))
                (when info
                  (push name key-arguments)
                  (push v key-arguments))))))

        (nconc (subseq args 0 non-key)
               (nreverse key-arguments)))
      args))

;;; Return MIN, MAX, whether it contaions &optional/&key/&rest
(defun fun-arg-limits (function)
  (cond ((fun-type-p function)
         (if (fun-type-wild-args function)
             (values nil nil)
             (let* ((min (length (fun-type-required function)))
                    (max (and (not (or (fun-type-rest function)
                                       (fun-type-keyp function)))
                              (+ min
                                 (length (fun-type-optional function))))))
               (values min max (or (fun-type-rest function)
                                   (fun-type-keyp function)
                                   (fun-type-optional function))))))
        ((lambda-p function)
         (let ((args (length (lambda-vars function))))
           (values args args)))
        ((not (optional-dispatch-p function))
         (values nil nil nil))
        ((optional-dispatch-more-entry function)
         (values (optional-dispatch-min-args function)
                 nil
                 t))
        (t
         (values (optional-dispatch-min-args function)
                 (optional-dispatch-max-args function)
                 t))))

(defun valid-callable-argument (lvar arg-count)
  (when lvar
    ;; Handle #'function,  'function and (lambda (x y))
    (let* ((use (principal-lvar-use lvar))
           (leaf (if (ref-p use)
                     (ref-leaf use)
                     (return-from valid-callable-argument nil)))
           (defined-type (and (global-var-p leaf)
                              (global-var-defined-type leaf)))
           (lvar-type (or defined-type
                          (lvar-type lvar)))
           (fun-name (cond ((or (fun-type-p lvar-type)
                                (functional-p leaf))
                            (if (constant-lvar-p lvar)
                                #+sb-xc-host (bug "Can't call %FUN-NAME")
                                #-sb-xc-host (%fun-name (lvar-value lvar))
                                (lvar-fun-debug-name lvar)))
                           ((constant-lvar-p lvar)
                            (lvar-value lvar))
                           (t
                            (return-from valid-callable-argument nil))))
           (type (cond ((fun-type-p lvar-type)
                        lvar-type)
                       ((symbolp fun-name)
                        (proclaimed-ftype fun-name))
                       (t
                        leaf)))
           (*lossage-fun* (if (and (not (eq (leaf-where-from leaf)
                                            :defined-here))
                                   (not (lambda-p leaf))
                                   (or (not fun-name)
                                       (not (info :function :info fun-name))))
                              #'compiler-style-warn
                              *lossage-fun*)))
      (multiple-value-bind (min max optional)
          (fun-arg-limits type)
        (cond
          ((and (not min) (not max)))
          ((not optional)
           (when (/= arg-count min)
             (note-lossage
              "The function ~S is called by ~S with ~R argument~:P, but wants exactly ~R."
              fun-name
              *valid-fun-use-name*
              arg-count min)))
          ((< arg-count min)
           (note-lossage
            "The function ~S is called by ~S with ~R argument~:P, but wants at least ~R."
            fun-name
            *valid-fun-use-name*
            arg-count min))
          ((not max))
          ((> arg-count max)
           (note-lossage
            "The function ~S called by ~S with ~R argument~:P, but wants at most ~R."
            fun-name
            *valid-fun-use-name*
            arg-count max)))))))

(defun check-structure-constructor-call (call dd ctor-ll-parts)
  (destructuring-bind (&optional req opt rest keys aux)
      (and (listp ctor-ll-parts) (cdr ctor-ll-parts))
    (declare (ignore rest))
    (let* ((call-args (combination-args call))
           (n-req (length req))
           (keyword-lvars (nthcdr (+ n-req (length opt)) call-args))
           (const-keysp (check-key-args-constant keyword-lvars))
           (n-call-args (length call-args)))
      (dolist (slot (dd-slots dd))
        (let ((name (dsd-name slot))
              (suppliedp :maybe)
              (lambda-list-element nil))
          ;; Ignore &AUX vars - it's not the caller's fault if wrong.
          (unless (find name aux :key (lambda (x) (if (listp x) (car x) x))
                        ;; is this right, or should it be EQ
                        ;; like in DETERMINE-UNSAFE-SLOTS ?
                        :test #'string=)
            (multiple-value-bind (arg position)
                (%find-position name opt nil 0 nil #'parse-optional-arg-spec
                                #'string=)
              (when arg
                (setq suppliedp (< (+ n-req position) n-call-args)
                      lambda-list-element arg)))
            (when (and (eq suppliedp :maybe) const-keysp)
              ;; Deduce the keyword (if any) that initializes this slot.
              (multiple-value-bind (keyword arg)
                  (if (listp ctor-ll-parts)
                      (dolist (arg keys)
                        (multiple-value-bind (key var) (parse-key-arg-spec arg)
                          (when (string= name var) (return (values key arg)))))
                      (values (keywordicate name) t))
                (when arg
                  (setq suppliedp (find-keyword-lvar keyword-lvars keyword)
                        lambda-list-element arg))))
            (when (eq suppliedp nil)
              (let ((initform (if (typep lambda-list-element '(cons t cons))
                                  (second lambda-list-element)
                                  (dsd-default slot))))
                ;; Return T if value-form definitely does not satisfy
                ;; the type-check for DSD. Return NIL if we can't decide.
                (when (if (sb!xc:constantp initform)
                          (not (sb!xc:typep (constant-form-value initform)
                                            (dsd-type slot)))
                          ;; Find uses of nil-returning functions as defaults,
                          ;; like ERROR and MISSING-ARG.
                          (and (sb!kernel::dd-null-lexenv-p dd)
                               (listp initform)
                               (let ((f (car initform)))
                                 ;; Don't examine :function :type of macros!
                                 (and (eq (info :function :kind f) :function)
                                      (let ((info (info :function :type f)))
                                        (and (fun-type-p info)
                                             (type= (fun-type-returns info)
                                                    *empty-type*)))))))
                  (note-lossage "The slot ~S does not have a suitable default, ~
and no value was provided for it." name))))))))))

;;; Check that the derived type of the LVAR is compatible with TYPE. N
;;; is the arg number, for error message purposes. We return true if
;;; arg is definitely o.k. If the type is a magic CONSTANT-TYPE, then
;;; we check for the argument being a constant value of the specified
;;; type. If there is a manifest type error (DERIVED-TYPE = NIL), then
;;; we flame about the asserted type even when our type is satisfied
;;; under the test.
(defun check-arg-type (lvar type n)
  (declare (type lvar lvar) (type ctype type) (type index n))
  (cond
   ((not (constant-type-p type))
    (let ((ctype (lvar-type lvar)))
      (multiple-value-bind (int win) (funcall *ctype-test-fun* ctype type)
        (cond ((not win)
               (note-unwinnage "can't tell whether the ~:R argument is a ~S"
                               n (type-specifier type))
               nil)
              ((not int)
               (note-lossage "The ~:R argument is a ~S, not a ~S."
                             n (type-specifier ctype) (type-specifier type))
               nil)
              ((eq ctype *empty-type*)
               (note-unwinnage "The ~:R argument never returns a value." n)
               nil)
              (t t)))))
    ((not (constant-lvar-p lvar))
     (note-unwinnage "The ~:R argument is not a constant." n)
     nil)
    (t
     (let ((val (lvar-value lvar))
           (type (constant-type-type type)))
       (multiple-value-bind (res win) (ctypep val type)
         (cond ((not win)
                (note-unwinnage "can't tell whether the ~:R argument is a ~
                                 constant ~S:~%  ~S"
                                n (type-specifier type) val)
                nil)
               ((not res)
                (note-lossage "The ~:R argument is not a constant ~S:~%  ~S"
                              n (type-specifier type) val)
                nil)
               (t t)))))))

;;; Check that each of the type of each supplied argument intersects
;;; with the type specified for that argument. If we can't tell, then
;;; we can complain about the absence of manifest winnage.
(declaim (ftype (function (list list (or ctype null)) (values)) check-fixed-and-rest))
(defun check-fixed-and-rest (args types rest)
  (do ((arg args (cdr arg))
       (type types (cdr type))
       (n 1 (1+ n)))
      ((or (null type) (null arg))
       (when rest
         (dolist (arg arg)
           (check-arg-type arg rest n)
           (incf n))))
    (declare (fixnum n))
    (check-arg-type (car arg) (car type) n))
  (values))

;;; Check that the &KEY args are of the correct type. Each key should
;;; be known and the corresponding argument should be of the correct
;;; type. If the key isn't a constant, then we can't tell, so we can
;;; complain about absence of manifest winnage.
(declaim (ftype (function (list fixnum fun-type) (values)) check-key-args))
(defun check-key-args (args pre-key type)
  (let (lossages allow-other-keys)
    (do ((key (nthcdr pre-key args) (cddr key))
         (n (1+ pre-key) (+ n 2)))
        ((null key))
      (declare (fixnum n))
      (let ((k (first key))
            (v (second key)))
        (cond
          ((not (check-arg-type k (specifier-type 'symbol) n)))
          ((not (constant-lvar-p k))
           (note-unwinnage "~@<The ~:R argument (in keyword position) is not ~
                            a constant, weakening keyword argument ~
                            checking.~:@>" n)
           ;; An unknown key may turn out to be :ALLOW-OTHER-KEYS at runtime,
           ;; so we cannot signal full warnings for keys that look bad.
           (unless allow-other-keys
             (setf allow-other-keys :maybe)))
          (t
           (let* ((name (lvar-value k))
                  (info (find name (fun-type-keywords type)
                              :key #'key-info-name)))
             (cond ((eq name :allow-other-keys)
                    (unless allow-other-keys
                      (if (constant-lvar-p v)
                          (setf allow-other-keys (if (lvar-value v)
                                                     :yes
                                                     :no))
                          (setf allow-other-keys :maybe))))
                   ((not info)
                    (unless (fun-type-allowp type)
                      (pushnew name lossages :test #'eq)))
                   (t
                    (check-arg-type (second key) (key-info-type info)
                                    (1+ n)))))))))
    (when (and lossages (member allow-other-keys '(nil :no)))
      (setf lossages (nreverse lossages))
      (if (cdr lossages)
          (note-lossage "~@<~{~S~^, ~} and ~S are not a known argument keywords.~:@>"
                        (butlast lossages)
                        (car (last lossages)))
          (note-lossage "~S is not a known argument keyword."
                        (car lossages)))))
  (values))

;;; Construct a function type from a definition.
;;;
;;; Due to the lack of a (LIST X) type specifier, we can't reconstruct
;;; the &REST type.
(declaim (ftype (sfunction (functional) fun-type) definition-type))
(defun definition-type (functional)
  (if (lambda-p functional)
      (make-fun-type
       :required (mapcar #'leaf-type (lambda-vars functional))
       :returns (tail-set-type (lambda-tail-set functional)))
      (let ((rest nil))
        (collect ((req)
                  (opt)
                  (keys))
          (dolist (arg (optional-dispatch-arglist functional))
            (let ((info (lambda-var-arg-info arg))
                  (type (leaf-type arg)))
              (if info
                  (ecase (arg-info-kind info)
                    (:required (req type))
                    (:optional (opt type))
                    (:keyword
                     (keys (make-key-info :name (arg-info-key info)
                                          :type type)))
                    ((:rest :more-context)
                     (setq rest *universal-type*))
                    (:more-count))
                  (req type))))

          (make-fun-type
           :required (req)
           :optional (opt)
           :rest rest
           :keywords (keys)
           :keyp (optional-dispatch-keyp functional)
           :allowp (optional-dispatch-allowp functional)
           :returns (tail-set-type
                     (lambda-tail-set
                      (optional-dispatch-main-entry functional))))))))

;;;; approximate function types
;;;;
;;;; FIXME: This is stuff to look at when I get around to fixing function
;;;; type inference and declarations.
;;;;
;;;; Approximate function types provide a condensed representation of all the
;;;; different ways that a function has been used. If we have no declared or
;;;; defined type for a function, then we build an approximate function type by
;;;; examining each use of the function. When we encounter a definition or
;;;; proclamation, we can check the actual type for compatibity with the
;;;; previous uses.

(defstruct (approximate-fun-type (:copier nil))
  ;; the smallest and largest numbers of arguments that this function
  ;; has been called with.
  (min-args sb!xc:call-arguments-limit
            :type (integer 0 #.sb!xc:call-arguments-limit))
  (max-args 0
            :type (integer 0 #.sb!xc:call-arguments-limit))
  ;; a list of lists of the all the types that have been used in each
  ;; argument position
  (types () :type list)
  ;; A list of APPROXIMATE-KEY-INFO structures describing all the
  ;; things that looked like &KEY arguments. There are distinct
  ;; structures describing each argument position in which the keyword
  ;; appeared.
  (keys () :type list))

(defstruct (approximate-key-info (:copier nil))
  ;; The keyword name of this argument. Although keyword names don't
  ;; have to be keywords, we only match on keywords when figuring an
  ;; approximate type.
  (name (missing-arg) :type keyword)
  ;; The position at which this keyword appeared. 0 if it appeared as the
  ;; first argument, etc.
  (position (missing-arg)
            :type (integer 0 #.sb!xc:call-arguments-limit))
  ;; a list of all the argument types that have been used with this keyword
  (types nil :type list)
  ;; true if this keyword has appeared only in calls with an obvious
  ;; :ALLOW-OTHER-KEYS
  (allowp nil :type (member t nil)))

;;; Return an APPROXIMATE-FUN-TYPE representing the context of
;;; CALL. If TYPE is supplied and not null, then we merge the
;;; information into the information already accumulated in TYPE.
(declaim (ftype (function (combination
                           &optional (or approximate-fun-type null))
                          approximate-fun-type)
                note-fun-use))
(defun note-fun-use (call &optional type)
  (let* ((type (or type (make-approximate-fun-type)))
         (types (approximate-fun-type-types type))
         (args (combination-args call))
         (nargs (length args))
         (allowp (some (lambda (x)
                         (and (constant-lvar-p x)
                              (eq (lvar-value x) :allow-other-keys)))
                       args)))

    (setf (approximate-fun-type-min-args type)
          (min (approximate-fun-type-min-args type) nargs))
    (setf (approximate-fun-type-max-args type)
          (max (approximate-fun-type-max-args type) nargs))

    (do ((old types (cdr old))
         (arg args (cdr arg)))
        ((null old)
         (setf (approximate-fun-type-types type)
               (nconc types
                      (mapcar (lambda (x)
                                (list (lvar-type x)))
                              arg))))
      (when (null arg) (return))
      (pushnew (lvar-type (car arg))
               (car old)
               :test #'type=))

    (collect ((keys (approximate-fun-type-keys type) cons))
      (do ((arg args (cdr arg))
           (pos 0 (1+ pos)))
          ((or (null arg) (null (cdr arg)))
           (setf (approximate-fun-type-keys type) (keys)))
        (let ((key (first arg))
              (val (second arg)))
          (when (constant-lvar-p key)
            (let ((name (lvar-value key)))
              (when (keywordp name)
                (let ((old (find-if
                            (lambda (x)
                              (and (eq (approximate-key-info-name x) name)
                                   (= (approximate-key-info-position x)
                                      pos)))
                            (keys)))
                      (val-type (lvar-type val)))
                  (cond (old
                         (pushnew val-type
                                  (approximate-key-info-types old)
                                  :test #'type=)
                         (unless allowp
                           (setf (approximate-key-info-allowp old) nil)))
                        (t
                         (keys (make-approximate-key-info
                                :name name
                                :position pos
                                :allowp allowp
                                :types (list val-type))))))))))))
    type))

;;; This is similar to VALID-FUN-USE, but checks an
;;; APPROXIMATE-FUN-TYPE against a real function type.
(declaim (ftype (function (approximate-fun-type fun-type
                           &optional function function function)
                          (values boolean boolean))
                valid-approximate-type))
(defun valid-approximate-type (call-type type &optional
                                         (*ctype-test-fun*
                                          #'types-equal-or-intersect)
                                         (*lossage-fun*
                                          #'compiler-style-warn)
                                         (*unwinnage-fun* #'compiler-notify))
  (let* ((*lossage-detected* nil)
         (*unwinnage-detected* nil)
         (required (fun-type-required type))
         (min-args (length required))
         (optional (fun-type-optional type))
         (max-args (+ min-args (length optional)))
         (rest (fun-type-rest type))
         (keyp (fun-type-keyp type)))

    (when (fun-type-wild-args type)
      (return-from valid-approximate-type (values t t)))

    (let ((call-min (approximate-fun-type-min-args call-type)))
      (when (< call-min min-args)
        (note-lossage
         "~:@<The function was previously called with ~R argument~:P, ~
          but wants at least ~R.~:>"
         call-min min-args)))

    (let ((call-max (approximate-fun-type-max-args call-type)))
      (cond ((<= call-max max-args))
            ((not (or keyp rest))
             (note-lossage
              "~:@<The function was previously called with ~R argument~:P, ~
                but wants at most ~R.~:>"
              call-max max-args))
            ((and keyp (oddp (- call-max max-args)))
             (note-lossage
              "~:@<The function was previously called with an odd number of ~
               arguments in the keyword portion.~:>")))

      (when (and keyp (> call-max max-args))
        (check-approximate-keywords call-type max-args type)))

    (check-approximate-fixed-and-rest call-type (append required optional)
                                      rest)

    (cond (*lossage-detected* (values nil t))
          (*unwinnage-detected* (values nil nil))
          (t (values t t)))))

;;; Check that each of the types used at each arg position is
;;; compatible with the actual type.
(declaim (ftype (function (approximate-fun-type list (or ctype null))
                          (values))
                check-approximate-fixed-and-rest))
(defun check-approximate-fixed-and-rest (call-type fixed rest)
  (do ((types (approximate-fun-type-types call-type) (cdr types))
       (n 1 (1+ n))
       (arg fixed (cdr arg)))
      ((null types))
    (let ((decl-type (or (car arg) rest)))
      (unless decl-type (return))
      (check-approximate-arg-type (car types) decl-type "~:R" n)))
  (values))

;;; Check that each of the call-types is compatible with DECL-TYPE,
;;; complaining if not or if we can't tell.
(declaim (ftype (function (list ctype string &rest t) (values))
                check-approximate-arg-type))
(defun check-approximate-arg-type (call-types decl-type context &rest args)
  (let ((losers *empty-type*))
    (dolist (ctype call-types)
      (multiple-value-bind (int win) (funcall *ctype-test-fun* ctype decl-type)
        (cond
         ((not win)
          (note-unwinnage "can't tell whether previous ~? ~
                           argument type ~S is a ~S"
                          context
                          args
                          (type-specifier ctype)
                          (type-specifier decl-type)))
         ((not int)
          (setq losers (type-union ctype losers))))))

    (unless (eq losers *empty-type*)
      (note-lossage "~:(~?~) argument should be a ~S but was a ~S in a previous call."
                    context args (type-specifier decl-type) (type-specifier losers))))
  (values))

;;; Check the types of each manifest keyword that appears in a keyword
;;; argument position. Check the validity of all keys that appeared in
;;; valid keyword positions.
;;;
;;; ### We could check the APPROXIMATE-FUN-TYPE-TYPES to make
;;; sure that all arguments in keyword positions were manifest
;;; keywords.
(defun check-approximate-keywords (call-type max-args type)
  (let ((call-keys (approximate-fun-type-keys call-type))
        (keys (fun-type-keywords type)))
    (dolist (key keys)
      (let ((name (key-info-name key)))
        (collect ((types nil append))
          (dolist (call-key call-keys)
            (let ((pos (approximate-key-info-position call-key)))
              (when (and (eq (approximate-key-info-name call-key) name)
                         (> pos max-args) (evenp (- pos max-args)))
                (types (approximate-key-info-types call-key)))))
          (check-approximate-arg-type (types) (key-info-type key) "~S" name))))

    (unless (fun-type-allowp type)
      (collect ((names () adjoin))
        (dolist (call-key call-keys)
          (let ((pos (approximate-key-info-position call-key)))
            (when (and (> pos max-args) (evenp (- pos max-args))
                       (not (approximate-key-info-allowp call-key)))
              (names (approximate-key-info-name call-key)))))

        (dolist (name (names))
          (unless (find name keys :key #'key-info-name)
            (note-lossage "Function previously called with unknown argument keyword ~S."
                  name)))))))

;;;; ASSERT-DEFINITION-TYPE

;;; Intersect LAMBDA's var types with TYPES, giving a warning if there
;;; is a mismatch. If all intersections are non-null, we return lists
;;; of the variables and intersections, otherwise we return NIL, NIL.
(defun try-type-intersections (vars types where)
  (declare (list vars types) (string where))
  (collect ((res))
    (mapc (lambda (var type)
            (let* ((vtype (leaf-type var))
                   (int (type-approx-intersection2 vtype type)))
              (cond
               ((eq int *empty-type*)
                (note-lossage
                 "Definition's declared type for variable ~A:~%  ~S~@
                  conflicts with this type from ~A:~%  ~S"
                 (leaf-debug-name var) (type-specifier vtype)
                 where (type-specifier type))
                (return-from try-type-intersections (values nil nil)))
               (t
                (res int)))))
          vars types)
    (values vars (res))))

;;; Check that the optional-dispatch OD conforms to TYPE. We return
;;; the values of TRY-TYPE-INTERSECTIONS if there are no syntax
;;; problems, otherwise NIL, NIL.
;;;
;;; Note that the variables in the returned list are the actual
;;; original variables (extracted from the optional dispatch arglist),
;;; rather than the variables that are arguments to the main entry.
;;; This difference is significant only for &KEY args with hairy
;;; defaults. Returning the actual vars allows us to use the right
;;; variable name in warnings.
;;;
;;; A slightly subtle point: with keywords and optionals, the type in
;;; the function type is only an assertion on calls --- it doesn't
;;; constrain the type of default values. So we have to union in the
;;; type of the default. With optionals, we can't do any assertion
;;; unless the default is constant.
;;;
;;; With keywords, we exploit our knowledge about how hairy keyword
;;; defaulting is done when computing the type assertion to put on the
;;; main-entry argument. In the case of hairy keywords, the default
;;; has been clobbered with NIL, which is the value of the main-entry
;;; arg in the unsupplied case, whatever the actual default value is.
;;; So we can just assume the default is constant, effectively
;;; unioning in NULL, and not totally blow off doing any type
;;; assertion.
(defun find-optional-dispatch-types (od type where)
  (declare (type optional-dispatch od)
           (type fun-type type)
           (string where))
  (let ((od-min (optional-dispatch-min-args od))
        (od-max (optional-dispatch-max-args od))
        (od-more (optional-dispatch-more-entry od))
        (od-keyp (optional-dispatch-keyp od))
        (od-allowp (optional-dispatch-allowp od))
        (type-required (fun-type-required type))
        (type-optional (fun-type-optional type))
        (type-rest (fun-type-rest type))
        (type-keyp (fun-type-keyp type))
        (type-allowp (fun-type-allowp type)))
    (flet ((check-num (num-definition num-type arg-kind)
             (unless (= num-definition num-type)
               (note-lossage
                "The definition has ~R ~A arg~P, but ~A has ~R."
                num-definition arg-kind num-definition where num-type)))
           (check-section (in-od-p in-type-p section)
             (unless (eq in-od-p in-type-p)
               (note-lossage
                "The definition ~:[doesn't have~;has~] ~A, but ~
                 ~A ~:[doesn't~;does~]."
                in-od-p section where in-type-p))))
      (check-num od-min (length type-required) 'required)
      ;; When TYPE does not have &OPTIONAL parameters and the type of
      ;; the &REST parameter is T, it may have been simplified from
      ;;
      ;;   (function (... &optional t &rest t ...) ...)
      ;;
      ;; We cannot check the exact number of optional parameters then.
      (unless (and (not type-optional)
                   type-rest (type= type-rest *universal-type*))
        (check-num (- od-max od-min) (length type-optional) '&optional))
      (check-section od-keyp type-keyp "&KEY arguments")
      (unless od-keyp
        (check-section (not (null od-more)) (not (null type-rest))
                       "&REST argument"))
      (check-section od-allowp type-allowp '&allow-other-keys))

    (when *lossage-detected*
      (return-from find-optional-dispatch-types (values nil nil)))

    (collect ((res)
              (vars))
      (let ((keys (fun-type-keywords type))
            (arglist (optional-dispatch-arglist od)))
        (dolist (arg arglist)
          (cond
           ((lambda-var-arg-info arg)
            (let* ((info (lambda-var-arg-info arg))
                   (default (arg-info-default info))
                   (def-type (when (sb!xc:constantp default)
                               (ctype-of (constant-form-value default)))))
              (ecase (arg-info-kind info)
                (:keyword
                 (let* ((key (arg-info-key info))
                        (kinfo (find key keys :key #'key-info-name)))
                   (cond
                    (kinfo
                     (res (type-union (key-info-type kinfo)
                                      (or def-type (specifier-type 'null)))))
                    (t
                     (note-lossage
                      "Defining a ~S keyword not present in ~A."
                      key where)
                     (res *universal-type*)))))
                (:required (res (pop type-required)))
                (:optional
                 ;; We can exhaust TYPE-OPTIONAL when the type was
                 ;; simplified as described above.
                 (res (type-union (or (pop type-optional)
                                      *universal-type*)
                                  (or def-type *universal-type*))))
                (:rest
                 (when (fun-type-rest type)
                   (res (specifier-type 'list))))
                (:more-context
                 (when (fun-type-rest type)
                   (res *universal-type*)))
                (:more-count
                 (when (fun-type-rest type)
                   (res (specifier-type 'fixnum)))))
              (vars arg)
              (when (arg-info-supplied-p info)
                (res *universal-type*)
                (vars (arg-info-supplied-p info)))))
           (t
            (res (pop type-required))
            (vars arg))))

        (dolist (key keys)
          (unless (find (key-info-name key) arglist
                        :key (lambda (x)
                               (let ((info (lambda-var-arg-info x)))
                                 (when info
                                   (arg-info-key info)))))
            (note-lossage
             "The definition lacks the ~S key present in ~A."
             (key-info-name key) where))))

      (try-type-intersections (vars) (res) where))))

;;; Check that TYPE doesn't specify any funny args, and do the
;;; intersection.
(defun find-lambda-types (lambda type where)
  (declare (type clambda lambda) (type fun-type type) (string where))
  (flet ((frob (x what)
           (when x
             (note-lossage
              "The definition has no ~A, but the ~A did."
              what where))))
    (frob (fun-type-optional type) "&OPTIONAL arguments")
    (frob (fun-type-keyp type) "&KEY arguments")
    (frob (fun-type-rest type) "&REST argument"))
  (let* ((vars (lambda-vars lambda))
         (nvars (length vars))
         (req (fun-type-required type))
         (nreq (length req)))
    (unless (= nvars nreq)
      (note-lossage "The definition has ~R arg~:P, but the ~A has ~R."
                    nvars where nreq))
    (if *lossage-detected*
        (values nil nil)
        (try-type-intersections vars req where))))

;;; Check for syntactic and type conformance between the definition
;;; FUNCTIONAL and the specified FUN-TYPE. If they are compatible
;;; and REALLY-ASSERT is T, then add type assertions to the definition
;;; from the FUN-TYPE.
;;;
;;; If there is a syntactic or type problem, then we call
;;; LOSSAGE-FUN with an error message using WHERE as context
;;; describing where FUN-TYPE came from.
;;;
;;; If there is no problem, we return T (even if REALLY-ASSERT was
;;; false). If there was a problem, we return NIL.
(defun assert-definition-type
    (functional type &key (really-assert t)
                          ((:lossage-fun *lossage-fun*) #'compiler-style-warn)
                          unwinnage-fun
                          (where "previous declaration"))
  (declare (type functional functional)
           (type function *lossage-fun*)
           (string where))
  (unless (fun-type-p type)
    (return-from assert-definition-type t))
  (let ((*lossage-detected* nil))
    (multiple-value-bind (vars types)
        (if (fun-type-wild-args type)
            (values nil nil)
            (etypecase functional
              (optional-dispatch
               (find-optional-dispatch-types functional type where))
              (clambda
               (find-lambda-types functional type where))))
      (let* ((type-returns (fun-type-returns type))
             (return (lambda-return (main-entry functional)))
             (dtype (when return
                      (lvar-derived-type (return-result return)))))
        (cond
          ((and dtype (not (values-types-equal-or-intersect dtype
                                                            type-returns)))
           (note-lossage
             "The result type from ~A:~%  ~
              ~/sb!impl:print-type/~@
              conflicts with the definition's result type:~%  ~
              ~/sb!impl:print-type/"
            where type-returns dtype)
           nil)
          (*lossage-detected* nil)
          ((not really-assert) t)
          (t
           ;; REALLY-ASSERT can be T or `(:NOT . ,vars) where the latter is
           ;; a list of vars for which compiling will *not* generate
           ;; an automatic check.
           (let ((policy (lexenv-policy (functional-lexenv functional))))
             (when (and (policy policy (> type-check 0))
                        (or (eq really-assert t)
                            (not (member :result (cdr really-assert)))))
               (assert-lvar-type (return-result return) type-returns
                                 policy)))
           (loop for var in vars and type in types do
                 (cond ((basic-var-sets var)
                        (when (and unwinnage-fun
                                   (not (csubtypep (leaf-type var) type)))
                          (funcall unwinnage-fun
                                   #.(#+sb-xc sb!impl::!xc-preprocess-format-control
                                      #-sb-xc identity
                                    "Assignment to argument: ~S~%  ~
                                     prevents use of assertion from function ~
                                     type ~A:~% ~/sb!impl:print-type/~%")
                                   (leaf-debug-name var) where type)))
                       ((and (listp really-assert) ; (:NOT . ,vars)
                             (member (lambda-var-%source-name var)
                                     (cdr really-assert)))) ; do nothing
                       (t
                        (setf (leaf-type var) type)
                        (let ((s-type (make-single-value-type type)))
                          (dolist (ref (leaf-refs var))
                            (derive-node-type ref s-type))))))
           t))))))

;;; Manipulate the poorly-named :REALLY-ASSERT value.
;;; It would make sense to pass the opposite sense of the arg
;;; (as ":SKIP-CHECKS") corresponding to the declaration.
(defun explicit-check->really-assert (explicit-check)
  (case explicit-check
    ((nil) t)
    ((t) nil)
    (t `(:not . ,explicit-check))))

;;; FIXME: This is quite similar to ASSERT-NEW-DEFINITION.
(defun assert-global-function-definition-type (name fun)
  (declare (type functional fun))
  (let ((where (info :function :where-from name))
        (explicit-check (getf (functional-plist fun) 'explicit-check)))
    (if (eq where :declared)
        (let ((type
               (massage-global-definition-type (proclaimed-ftype name) fun)))
          (setf (leaf-type fun) type)
          (assert-definition-type
           fun type
           :unwinnage-fun #'compiler-notify
           :where "proclamation"
           :really-assert (explicit-check->really-assert explicit-check)))
        ;; Can't actually test this. DEFSTRUCTs declare this, but non-toplevel
        ;; ones won't have an FTYPE at compile-time.
        #+nil
        (when explicit-check
          (warn "Explicit-check without known FTYPE is meaningless")))))

;;; If the function has both &REST and &KEY, FIND-OPTIONAL-DISPATCH-TYPES
;;; doesn't complain about the type missing &REST -- which is good, because in
;;; that case &REST is really an implementation detail and not part of the
;;; interface. However since we set the leaf type missing &REST from there
;;; would be a bad thing -- to make up a new type if necessary.
(defun massage-global-definition-type (type fun)
  (if (and (fun-type-p type)
           (optional-dispatch-p fun)
           (optional-dispatch-keyp fun)
           (optional-dispatch-more-entry fun)
           (not (or (fun-type-rest type)
                    (fun-type-wild-args type))))
      (make-fun-type :required (fun-type-required type)
                     :optional (fun-type-optional type)
                     :rest *universal-type*
                     :keyp (fun-type-keyp type)
                     :keywords (fun-type-keywords type)
                     :allowp (fun-type-allowp type)
                     :returns (fun-type-returns type))
      type))

;;; Call FUN with (arg-lvar arg-type)
(defun map-combination-args-and-types (fun call)
  (declare (type function fun) (type combination call))
  (binding* ((type (lvar-type (combination-fun call)))
             (nil (fun-type-p type) :exit-if-null)
             (args (combination-args call)))
    (dolist (req (fun-type-required type))
      (when (null args) (return-from map-combination-args-and-types))
      (let ((arg (pop args)))
        (funcall fun arg req)))
    (dolist (opt (fun-type-optional type))
      (when (null args) (return-from map-combination-args-and-types))
      (let ((arg (pop args)))
        (funcall fun arg opt)))

    (let ((rest (fun-type-rest type)))
      (when rest
        (dolist (arg args)
          (funcall fun arg rest))))

    (dolist (key (fun-type-keywords type))
      (let ((name (key-info-name key)))
        (do ((arg args (cddr arg)))
            ((null arg))
          (let ((keyname (first arg)))
            (when (and (constant-lvar-p keyname)
                       (eq (lvar-value keyname) name))
              (funcall fun (second arg) (key-info-type key)))))))))

;;; Assert that CALL is to a function of the specified TYPE. It is
;;; assumed that the call is legal and has only constants in the
;;; keyword positions.
(defun assert-call-type (call type &optional (trusted t))
  (declare (type combination call) (type fun-type type))
  (let ((policy (lexenv-policy (node-lexenv call)))
        (returns (fun-type-returns type)))
    (if trusted
        (derive-node-type call returns)
        (let ((lvar (node-lvar call)))
          ;; If the value is used in a non-tail position, and the lvar
          ;; is a single-use, assert the type. Multiple use sites need
          ;; to be elided because the assertion has to apply to all
          ;; uses. Tail positions are elided because the assertion
          ;; would cause us not the be in a tail-position anymore. MV
          ;; calls are elided because not only are the assertions of
          ;; less use there, but they can cause the MV call conversion
          ;; to cause astray.
          (when (and lvar
                     (not (return-p (lvar-dest lvar)))
                     (not (mv-combination-p (lvar-dest lvar)))
                     (lvar-has-single-use-p lvar))
            (when (assert-lvar-type lvar returns policy)
              (reoptimize-lvar lvar)))))
    (let* ((name (lvar-fun-name (combination-fun call) t))
           (info (and name
                      (info :function :info name))))
      (if (and info
               (fun-info-call-type-deriver info))
          (funcall (fun-info-call-type-deriver info) call trusted)
          (map-combination-args-and-types
           (lambda (arg type)
             (when (and (assert-lvar-type arg type policy)
                        (not trusted))
               (reoptimize-lvar arg)))
           call))))
  (values))

;;;; FIXME: Move to some other file.
(defun check-catch-tag-type (tag)
  (declare (type lvar tag))
  (let ((ctype (lvar-type tag)))
    (when (csubtypep ctype (specifier-type '(or number character)))
      (let ((sources (lvar-all-sources tag)))
        (if (singleton-p sources)
            (compiler-style-warn
              "~@<Using ~S of type ~/sb!impl:print-type/ as ~
               a catch tag (which tends to be unportable because THROW ~
               and CATCH use EQ comparison)~@:>"
             (first sources) (lvar-type tag))
            (compiler-style-warn
              "~@<Using ~{~S~^~#[~; or ~:;, ~]~} in ~S of type ~
               ~/sb!impl:print-type/ as a catch tag (which tends to be ~
               unportable because THROW and CATCH use EQ comparison)~@:>"
             (rest sources) (first sources) (lvar-type tag)))))))

(defun %compile-time-type-error (values atype dtype detail context)
  (declare (ignore dtype))
  (if (and (consp atype) (eq (car atype) 'values))
      (if (singleton-p detail)
          (error 'simple-type-error
                 :datum (car values)
                 :expected-type atype
                 :format-control
                  "~@<Value set ~2I~_[~{~S~^ ~}] ~I~_from ~S in~_~A ~
                   ~I~_is not of type ~
                   ~2I~_~/sb!impl:print-type-specifier/.~:>"
                 :format-arguments (list values
                                         (first detail) context
                                         atype))
          (error 'simple-type-error
                 :datum (car values)
                 :expected-type atype
                 :format-control
                  "~@<Value set ~2I~_[~{~S~^ ~}] ~
                   ~I~_from ~2I~_~{~S~^~#[~; or ~:;, ~]~} ~
                   ~I~_of ~2I~_~S ~I~_in~_~A ~I~_is not of type ~
                   ~2I~_~/sb!impl:print-type-specifier/.~:>"
                 :format-arguments (list values
                                         (rest detail) (first detail)
                                         context
                                         atype)))
      (if (singleton-p detail)
          (error 'simple-type-error
                 :datum (car values)
                 :expected-type atype
                 :format-control
                  "~@<Value of ~S in ~_~A ~I~_is ~2I~_~S, ~
                   ~I~_not a ~2I~_~/sb!impl:print-type-specifier/.~:@>"
                 :format-arguments (list (car detail) context
                                         (car values)
                                         atype))
          (error 'simple-type-error
                 :datum (car values)
                 :expected-type atype
                 :format-control
                  "~@<Value from ~2I~_~{~S~^~#[~; or ~:;, ~]~} ~
                   ~I~_of ~2I~_~S ~I~_in~_~A ~I~_is ~2I~_~S, ~
                   ~I~_not a ~2I~_~/sb!impl:print-type-specifier/.~:@>"
                 :format-arguments (list (rest detail) (first detail) context
                                         (car values)
                                         atype)))))

(defoptimizer (%compile-time-type-error ir2-convert)
    ((objects atype dtype detail context) node block)
  (declare (ignore objects context))
  (let ((*compiler-error-context* node))
    (setf (node-source-path node)
          (cdr (node-source-path node)))
    (let ((atype (lvar-value atype))
          (dtype (lvar-value dtype))
          (detail (lvar-value detail)))
      (unless (eq atype nil)
          (if (singleton-p detail)
              (let ((detail (first detail)))
                (if (constantp detail)
                    (warn 'type-warning
                          :format-control
                           "~@<Constant ~2I~_~S ~Iconflicts with its ~
                            asserted type ~
                            ~2I~_~/sb!impl::print-type-specifier/.~@:>"
                          :format-arguments (list (eval detail) atype))
                    (warn 'type-warning
                          :format-control
                           "~@<Derived type of ~S is ~2I~_~S, ~
                            ~I~_conflicting with its asserted type ~
                            ~2I~_~/sb!impl:print-type-specifier/.~@:>"
                          :format-arguments (list detail dtype atype))))
              (warn 'type-warning
                    :format-control
                     "~@<Derived type of ~2I~_~{~S~^~#[~; and ~:;, ~
                      ~]~} ~I~_in ~2I~_~S ~I~_is ~
                      ~2I~_~/sb!impl:print-type-specifier/, ~
                      ~I~_conflicting with their asserted type ~
                      ~2I~_~/sb!impl:print-type-specifier/.~@:>"
                    :format-arguments (list (rest detail) (first detail)
                                            dtype atype)))))
    (ir2-convert-full-call node block)))
