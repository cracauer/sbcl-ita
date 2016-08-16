;;;; array-specific optimizers and transforms

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; utilities for optimizing array operations

;;; Return UPGRADED-ARRAY-ELEMENT-TYPE for LVAR, or do
;;; GIVE-UP-IR1-TRANSFORM if the upgraded element type can't be
;;; determined.
(defun upgraded-element-type-specifier-or-give-up (lvar)
  (let ((element-type-specifier (upgraded-element-type-specifier lvar)))
    (if (eq element-type-specifier '*)
        (give-up-ir1-transform
         "upgraded array element type not known at compile time")
        element-type-specifier)))

(defun upgraded-element-type-specifier (lvar)
  (type-specifier (array-type-upgraded-element-type (lvar-type lvar))))

;;; Array access functions return an object from the array, hence its type is
;;; going to be the array upgraded element type. Secondary return value is the
;;; known supertype of the upgraded-array-element-type, if if the exact
;;; U-A-E-T is not known. (If it is NIL, the primary return value is as good
;;; as it gets.)
(defun array-type-upgraded-element-type (type)
  (typecase type
    ;; Note that this IF mightn't be satisfied even if the runtime
    ;; value is known to be a subtype of some specialized ARRAY, because
    ;; we can have values declared e.g. (AND SIMPLE-VECTOR UNKNOWN-TYPE),
    ;; which are represented in the compiler as INTERSECTION-TYPE, not
    ;; array type.
    (array-type
     (values (array-type-specialized-element-type type) nil))
    ;; Deal with intersection types (bug #316078)
    (intersection-type
     (let ((intersection-types (intersection-type-types type))
           (element-type *wild-type*)
           (element-supertypes nil))
       (dolist (intersection-type intersection-types)
         (multiple-value-bind (cur-type cur-supertype)
             (array-type-upgraded-element-type intersection-type)
           ;; According to ANSI, an array may have only one specialized
           ;; element type - e.g. '(and (array foo) (array bar))
           ;; is not a valid type unless foo and bar upgrade to the
           ;; same element type.
           (cond
             ((eq cur-type *wild-type*)
              nil)
             ((eq element-type *wild-type*)
              (setf element-type cur-type))
             ((or (not (csubtypep cur-type element-type))
                  (not (csubtypep element-type cur-type)))
              ;; At least two different element types where given, the array
              ;; is valid iff they represent the same type.
              ;;
              ;; FIXME: TYPE-INTERSECTION already takes care of disjoint array
              ;; types, so I believe this code should be unreachable. Maybe
              ;; signal a warning / error instead?
              (setf element-type *empty-type*)))
           (push (or cur-supertype (type-*-to-t cur-type))
                 element-supertypes)))
       (values element-type
               (when (and (eq *wild-type* element-type) element-supertypes)
                 (apply #'type-intersection element-supertypes)))))
    (union-type
     (let ((union-types (union-type-types type))
           (element-type nil)
           (element-supertypes nil))
       (dolist (union-type union-types)
         (multiple-value-bind (cur-type cur-supertype)
             (array-type-upgraded-element-type union-type)
           (cond
             ((eq element-type *wild-type*)
              nil)
             ((eq element-type nil)
              (setf element-type cur-type))
             ((or (eq cur-type *wild-type*)
                  ;; If each of the two following tests fail, it is not
                  ;; possible to determine the element-type of the array
                  ;; because more than one kind of element-type was provided
                  ;; like in '(or (array foo) (array bar)) although a
                  ;; supertype (or foo bar) may be provided as the second
                  ;; returned value returned. See also the KLUDGE below.
                  (not (csubtypep cur-type element-type))
                  (not (csubtypep element-type cur-type)))
              (setf element-type *wild-type*)))
           (push (or cur-supertype (type-*-to-t cur-type))
                 element-supertypes)))
       (values element-type
               (when (eq *wild-type* element-type)
                 (apply #'type-union element-supertypes)))))
    (member-type
     ;; Convert member-type to an union-type.
     (array-type-upgraded-element-type
      (apply #'type-union (mapcar #'ctype-of (member-type-members type)))))
    (t
     ;; KLUDGE: there is no good answer here, but at least
     ;; *wild-type* won't cause HAIRY-DATA-VECTOR-{REF,SET} to be
     ;; erroneously optimized (see generic/vm-tran.lisp) -- CSR,
     ;; 2002-08-21
     (values *wild-type* nil))))

(defun array-type-declared-element-type (type)
  (if (array-type-p type)
      (array-type-element-type type)
      *wild-type*))

;;; The ``new-value'' for array setters must fit in the array, and the
;;; return type is going to be the same as the new-value for SETF
;;; functions.
(defun assert-new-value-type (new-value array)
  (let ((type (lvar-type array)))
    (when (array-type-p type)
      (assert-lvar-type
       new-value
       (array-type-specialized-element-type type)
       (lexenv-policy (node-lexenv (lvar-dest new-value))))))
  (lvar-type new-value))

;;; Return true if ARG is NIL, or is a constant-lvar whose
;;; value is NIL, false otherwise.
(defun unsupplied-or-nil (arg)
  (declare (type (or lvar null) arg))
  (or (not arg)
      (and (constant-lvar-p arg)
           (not (lvar-value arg)))))

(defun supplied-and-true (arg)
  (and arg
       (constant-lvar-p arg)
       (lvar-value arg)
       t))

;;;; DERIVE-TYPE optimizers

(defun derive-aref-type (array)
  (multiple-value-bind (uaet other)
      (array-type-upgraded-element-type (lvar-type array))
    (or other uaet)))

(deftransform array-in-bounds-p ((array &rest subscripts))
  (block nil
    (flet ((give-up (&optional reason)
             (cond ((= (length subscripts) 1)
                    (let ((arg (sb!xc:gensym)))
                      `(lambda (array ,arg)
                         (and (typep ,arg '(and fixnum unsigned-byte))
                              (< ,arg (array-dimension array 0))))))
                   (t
                    (give-up-ir1-transform
                     (or reason
                         "~@<lower array bounds unknown or negative and upper bounds not ~
                         negative~:@>")))))
           (bound-known-p (x)
             (integerp x)))             ; might be NIL or *
      (let ((dimensions (catch-give-up-ir1-transform
                            ((array-type-dimensions-or-give-up
                              (lvar-conservative-type array))
                             args)
                          (give-up (car args)))))
        ;; Might be *. (Note: currently this is never true, because the type
        ;; derivation infers the rank from the call to ARRAY-IN-BOUNDS-P, but
        ;; let's keep this future proof.)
        (when (eq '* dimensions)
          (give-up "array bounds unknown"))
        ;; shortcut for zero dimensions
        (when (some (lambda (dim)
                      (and (bound-known-p dim) (zerop dim)))
                    dimensions)
          (return nil))
        ;; we first collect the subscripts LVARs' bounds and see whether
        ;; we can already decide on the result of the optimization without
        ;; even taking a look at the dimensions.
        (flet ((subscript-bounds (subscript)
                 (let* ((type1 (lvar-type subscript))
                        (type2 (if (csubtypep type1 (specifier-type 'integer))
                                   (weaken-integer-type type1 :range-only t)
                                   (give-up)))
                        (low (if (integer-type-p type2)
                                 (numeric-type-low type2)
                                 (give-up)))
                        (high (numeric-type-high type2)))
                   (cond
                     ((and (or (not (bound-known-p low)) (minusp low))
                           (or (not (bound-known-p high)) (not (minusp high))))
                      ;; can't be sure about the lower bound and the upper bound
                      ;; does not give us a definite clue either.
                      (give-up))
                     ((and (bound-known-p high) (minusp high))
                      (return nil)) ; definitely below lower bound (zero).
                     (t
                      (cons low high))))))
          (let* ((subscripts-bounds (mapcar #'subscript-bounds subscripts))
                 (subscripts-lower-bound (mapcar #'car subscripts-bounds))
                 (subscripts-upper-bound (mapcar #'cdr subscripts-bounds))
                 (in-bounds 0))
            (mapcar (lambda (low high dim)
                      (cond
                        ;; first deal with infinite bounds
                        ((some (complement #'bound-known-p) (list low high dim))
                         (when (and (bound-known-p dim) (bound-known-p low) (<= dim low))
                           (return nil)))
                        ;; now we know all bounds
                        ((>= low dim)
                         (return nil))
                        ((< high dim)
                         (aver (not (minusp low)))
                         (incf in-bounds))
                        (t
                         (give-up))))
                    subscripts-lower-bound
                    subscripts-upper-bound
                    dimensions)
            (if (eql in-bounds (length dimensions))
                t
                (give-up))))))))

(defoptimizer (aref derive-type) ((array &rest subscripts))
  (declare (ignore subscripts))
  (derive-aref-type array))

(defoptimizer ((setf aref) derive-type) ((new-value array &rest subscripts))
  (declare (ignore subscripts))
  (assert-new-value-type new-value array))

(macrolet ((define (name)
             `(defoptimizer (,name derive-type) ((array index))
                (declare (ignore index))
                (derive-aref-type array))))
  (define hairy-data-vector-ref)
  (define hairy-data-vector-ref/check-bounds)
  (define data-vector-ref))

#!+(or x86 x86-64)
(defoptimizer (data-vector-ref-with-offset derive-type) ((array index offset))
  (declare (ignore index offset))
  (derive-aref-type array))

(defoptimizer (vector-pop derive-type) ((array))
  (derive-aref-type array))

(macrolet ((define (name)
             `(defoptimizer (,name derive-type) ((array index new-value))
                (declare (ignore index))
                (assert-new-value-type new-value array))))
  (define hairy-data-vector-set)
  (define hairy-data-vector-set/check-bounds)
  (define data-vector-set))

#!+(or x86 x86-64)
(defoptimizer (data-vector-set-with-offset derive-type) ((array index offset new-value))
  (declare (ignore index offset))
  (assert-new-value-type new-value array))

;;; Figure out the type of the data vector if we know the argument
;;; element type.
(defun derive-%with-array-data/mumble-type (array)
  (let ((atype (lvar-type array)))
    (when (array-type-p atype)
      (specifier-type
       `(simple-array ,(type-specifier
                        (array-type-specialized-element-type atype))
                      (*))))))
(defoptimizer (%with-array-data derive-type) ((array start end))
  (declare (ignore start end))
  (derive-%with-array-data/mumble-type array))
(defoptimizer (%with-array-data/fp derive-type) ((array start end))
  (declare (ignore start end))
  (derive-%with-array-data/mumble-type array))

(defoptimizer (row-major-aref derive-type) ((array index))
  (declare (ignore index))
  (derive-aref-type array))

(defoptimizer (%set-row-major-aref derive-type) ((array index new-value))
  (declare (ignore index))
  (assert-new-value-type new-value array))

(defun derive-make-array-type (dims element-type adjustable
                               fill-pointer displaced-to)
  (let* ((simple (and (unsupplied-or-nil adjustable)
                      (unsupplied-or-nil displaced-to)
                      (unsupplied-or-nil fill-pointer)))
         (spec
           (or `(,(if simple 'simple-array 'array)
                 ,(cond ((not element-type) t)
                        ((ctype-p element-type)
                         (type-specifier element-type))
                        ((constant-lvar-p element-type)
                         (let ((ctype (careful-specifier-type
                                       (lvar-value element-type))))
                           (cond
                             ((or (null ctype) (contains-unknown-type-p ctype)) '*)
                             (t (sb!xc:upgraded-array-element-type
                                 (lvar-value element-type))))))
                        (t
                         '*))
                 ,(cond ((constant-lvar-p dims)
                         (let* ((val (lvar-value dims))
                                (cdims (ensure-list val)))
                           (if simple
                               cdims
                               (length cdims))))
                        ((csubtypep (lvar-type dims)
                                    (specifier-type 'integer))
                         '(*))
                        (t
                         '*)))
               'array)))
    (if (and (not simple)
             (or (supplied-and-true adjustable)
                 (supplied-and-true displaced-to)
                 (supplied-and-true fill-pointer)))
        (careful-specifier-type `(and ,spec (not simple-array)))
        (careful-specifier-type spec))))

(defoptimizer (make-array derive-type)
    ((dims &key element-type adjustable fill-pointer displaced-to
           &allow-other-keys))
  (derive-make-array-type dims element-type adjustable
                          fill-pointer displaced-to))

(defoptimizer (%make-array derive-type)
    ((dims widetag n-bits &key adjustable fill-pointer displaced-to
                          &allow-other-keys))
  (declare (ignore n-bits))
  (let ((saetp (and (constant-lvar-p widetag)
                    (find (lvar-value widetag)
                          sb!vm:*specialized-array-element-type-properties*
                          :key #'sb!vm:saetp-typecode))))
    (derive-make-array-type dims (if saetp
                                     (sb!vm:saetp-ctype saetp)
                                     *wild-type*)
                            adjustable fill-pointer displaced-to)))


;;;; constructors

;;; Convert VECTOR into a MAKE-ARRAY.
(define-source-transform vector (&rest elements)
  `(make-array ,(length elements) :initial-contents (list ,@elements)))

;;; Just convert it into a MAKE-ARRAY.
(deftransform make-string ((length &key
                                   (element-type 'character)
                                   (initial-element
                                    #.*default-init-char-form*)))
  `(the simple-string (make-array (the index length)
                       :element-type element-type
                       ,@(when initial-element
                           '(:initial-element initial-element)))))

;; Traverse the :INTIAL-CONTENTS argument to an array constructor call,
;; changing the skeleton of the data to be constructed by calls to LIST
;; and wrapping some declarations around each array cell's constructor.
;; In general, if we fail to optimize out the materialization
;; of initial-contents as distinct from the array itself, we prefer VECTOR
;; over LIST due to the smaller overhead (except for <= 1 item).
;; If a macro is involved, expand it before traversing.
;; Known limitations:
;; - inline functions whose behavior is merely to call LIST don't work
;;   e.g. :INITIAL-CONTENTS (MY-LIST a b) ; where MY-LIST is inline
;;                                        ; and effectively just (LIST ...)
(defun rewrite-initial-contents (rank initial-contents env)
  ;; If FORM is constant to begin with, we don't want to pessimize it
  ;; by turning it into a non-literal. That would happen because when
  ;; optimizing `#(#(foo bar) #(,x ,y)) we convert the whole expression
  ;; into (VECTOR 'FOO 'BAR X Y), whereas in the unidimensional case
  ;; it never makes sense to turn #(FOO BAR) into (VECTOR 'FOO 'BAR).
  (when (or (and (= rank 1) (sb!xc:constantp initial-contents env))
            ;; If you inhibit inlining these - game over.
            (fun-lexically-notinline-p 'vector env)
            (fun-lexically-notinline-p 'list env)
            (fun-lexically-notinline-p 'list* env))
    (return-from rewrite-initial-contents (values nil nil)))
  (let ((dimensions (make-array rank :initial-element nil))
        (output))
    (named-let recurse ((form (sb!xc:macroexpand initial-contents env))
                        (axis 0))
      (flet ((make-list-ctor (tail &optional (prefix nil prefixp) &aux val)
               (when (and (sb!xc:constantp tail)
                          (or (proper-list-p (setq val (constant-form-value tail env)))
                              (and (vectorp val) (not prefixp))))
                 (setq form
                       (cons 'list
                             (append (butlast prefix)
                                     (map 'list (lambda (x) (list 'quote x)) val)))))))
        ;; Express quasiquotation using only LIST, not LIST*.
        ;; e.g. `(,A ,B X Y) -> (LIST* A B '(X Y)) -> (LIST A B 'X 'Y)
        (if (typep form '(cons (eql list*) list))
            (let* ((cdr (cdr form)) (last (last cdr)))
              (when (null (cdr last))
                (make-list-ctor (car last) cdr)))
            (make-list-ctor form)))
      (unless (and (typep form '(cons (member list vector)))
                   (do ((items (cdr form))
                        (length 0 (1+ length))
                        (fun (let ((axis (the (mod #.array-rank-limit) (1+ axis))))
                               (if (= axis rank)
                                   (lambda (item) (push item output))
                                   (lambda (item) (recurse item axis))))))
                       ;; FIXME: warn if the nesting is indisputably wrong
                       ;; such as `((,x ,x) (,x ,x ,x)).
                       ((atom items)
                        (and (null items)
                             (if (aref dimensions axis)
                                 (eql length (aref dimensions axis))
                                 (setf (aref dimensions axis) length))))
                     (declare (type index length))
                     (funcall fun (pop items))))
        (return-from rewrite-initial-contents (values nil nil))))
    (when (some #'null dimensions)
      ;; Unless it is the rightmost axis, a 0-length subsequence
      ;; causes a NIL dimension. Give up if that happens.
      (return-from rewrite-initial-contents (values nil nil)))
    (setq output (nreverse output))
    (values
     ;; If the unaltered INITIAL-CONTENTS were constant, then the flattened
     ;; form must be too. Turning it back to a self-evaluating object
     ;; is essential to avoid compile-time blow-up on huge vectors.
     (if (sb!xc:constantp initial-contents env)
         (map 'vector (lambda (x) (constant-form-value x env)) output)
         (let ((f (if (singleton-p output) 'list 'vector)))
           `(locally (declare (notinline ,f))
             (,f ,@(mapcar (lambda (x)
                             (cond ((and (symbolp x)
                                         (not (nth-value
                                               1 (sb!xc:macroexpand-1 x env))))
                                    x)
                                   ((sb!xc:constantp x env)
                                    `',(constant-form-value x env))
                                   (t
                                    `(locally (declare (inline ,f)) ,x))))
                           output)))))
     (coerce dimensions 'list))))

;;; Prevent open coding :INITIAL-CONTENTS arguments, so that we
;;; can pick them apart in the DEFTRANSFORMS.
;;; (MAKE-ARRAY (LIST dim ...)) for rank != 1 is transformed now.
;;; Waiting around to see if IR1 can deduce that the dims are of type LIST
;;; is ineffective, because by then it's too late to flatten the initial
;;; contents using the correct array rank.
;;; We explicitly avoid handling non-simple arrays (uni- or multi-dimensional)
;;; in this path, mainly due to complications in picking the right widetag.
(define-source-transform make-array (dims-form &rest rest &environment env
                                               &aux dims dims-constp)
  (cond ((and (sb!xc:constantp dims-form env)
              (listp (setq dims (constant-form-value dims-form env)))
              (not (singleton-p dims))
              (every (lambda (x) (typep x 'index)) dims))
         (setq dims-constp t))
        ((and (cond ((typep (setq dims (sb!xc:macroexpand dims-form env))
                            '(cons (eql list)))
                     (setq dims (cdr dims))
                     t)
                    ;; `(,X 2 1) -> (LIST* X '(2 1)) for example
                    ((typep dims '(cons (eql list*) cons))
                     (let ((last (car (last dims))))
                       (when (sb!xc:constantp last env)
                         (let ((lastval (constant-form-value last env)))
                           (when (listp lastval)
                             (setq dims (append (butlast (cdr dims)) lastval))
                             t))))))
              (proper-list-p dims)
              (not (singleton-p dims)))
         ;; If you spell '(2 2) as (LIST 2 2), it is constant for purposes of MAKE-ARRAY.
         (when (every (lambda (x) (sb!xc:constantp x env)) dims)
           (let ((values (mapcar (lambda (x) (constant-form-value x env)) dims)))
             (when (every (lambda (x) (typep x 'index)) values)
               (setq dims values dims-constp t)))))
        (t
         ;; Regardless of dimension, it is always good to flatten :INITIAL-CONTENTS
         ;; if we can, ensuring that we convert `(,X :A :B) = (LIST* X '(:A :B))
         ;; into (VECTOR X :A :B) which makes it cons less if not optimized,
         ;; or cons not at all (not counting the destination array) if optimized.
         ;; There is no need to transform dimensions of '(<N>) to the integer N.
         ;; The IR1 transform for list-shaped dims will figure it out.
         (binding* ((contents (and (evenp (length rest)) (getf rest :initial-contents))
                              :exit-if-null)
                    ;; N-DIMS = 1 can be "technically" wrong, but it doesn't matter.
                    (data (rewrite-initial-contents 1 contents env) :exit-if-null))
           (setf rest (copy-list rest) (getf rest :initial-contents) data)
           (return-from make-array `(make-array ,dims-form ,@rest)))
         (return-from make-array (values nil t))))
  ;; So now we know that this is a multi-dimensional (or 0-dimensional) array.
  ;; Parse keywords conservatively, rejecting anything that makes it non-simple,
  ;; and accepting only a pattern that is likely to occur in practice.
  ;; e.g we give up on a duplicate keywords rather than bind ignored temps.
  (let* ((unsupplied '#:unsupplied) (et unsupplied) et-constp et-binding
         contents element adjustable keys data-dims)
    (unless (loop (if (null rest) (return t))
                  (if (or (atom rest) (atom (cdr rest))) (return nil))
                  (let ((k (pop rest))
                        (v rest))
                    (pop rest)
                    (case k
                      (:element-type
                       (unless (eq et unsupplied) (return nil))
                       (setq et (car v) et-constp (sb!xc:constantp et env)))
                      (:initial-element
                       (when (or contents element) (return nil))
                       (setq element v))
                      (:initial-contents
                       (when (or contents element) (return nil))
                       (if (not dims) ; If 0-dimensional, use :INITIAL-ELEMENT instead
                           (setq k :initial-element element v)
                           (setq contents v)))
                      (:adjustable ; reject if anything other than literal NIL
                       (when (or adjustable (car v)) (return nil))
                       (setq adjustable v))
                      (t
                       ;; Reject :FILL-POINTER, :DISPLACED-{TO,INDEX-OFFSET},
                       ;; and non-literal keywords.
                       (return nil)))
                    (unless (member k '(:adjustable))
                      (setq keys (nconc keys (list k (car v)))))))
      (return-from make-array (values nil t)))
    (when contents
      (multiple-value-bind (data shape)
          (rewrite-initial-contents (length dims) (car contents) env)
        (cond (shape ; initial-contents will be part of the vector allocation
               ;; and we aren't messing up keyword arg order.
               (when (and dims-constp (not (equal shape dims)))
                 ;; This will become a runtime error if the code is executed.
                 (warn "array dimensions are ~A but :INITIAL-CONTENTS dimensions are ~A"
                       dims shape))
               (setf data-dims shape (getf keys :initial-contents) data))
              (t ; contents could not be flattened
               ;; Preserve eval order. The only keyword arg to worry about
               ;; is :ELEMENT-TYPE. See also the remark at DEFKNOWN FILL-ARRAY.
               (when (and (eq (car keys) :element-type) (not et-constp))
                 (let ((et-temp (make-symbol "ET")))
                   (setf et-binding `((,et-temp ,et)) (cadr keys) et-temp)))
               (remf keys :initial-contents)))))
    (let* ((axis-bindings
            (unless dims-constp
              (loop for d in dims for i from 0
                    collect (list (make-symbol (format nil "D~D" i))
                                  `(the index ,d)))))
           (dims (if axis-bindings (mapcar #'car axis-bindings) dims))
           (size (make-symbol "SIZE"))
           (alloc-form
            `(truly-the (simple-array
                         ,(cond ((eq et unsupplied) t)
                                (et-constp (constant-form-value et env))
                                (t '*))
                         ,(if dims-constp dims (length dims)))
              (make-array-header*
               ,@(sb!vm::make-array-header-inits
                  `(make-array ,size ,@keys) size dims)))))
      `(let* (,@axis-bindings ,@et-binding (,size (the index (* ,@dims))))
         ,(cond ((or (not contents) (and dims-constp (equal dims data-dims)))
                 ;; If no :initial-contents, or definitely correct shape,
                 ;; then just call the constructor.
                 alloc-form)
                (data-dims ; data are flattened
                 ;; original shape must be asserted to be correct
                 ;; Arguably if the contents have a constant shape,
                 ;; we could cast each individual dimension in its binding form,
                 ;; i.e. (LET* ((#:D0 (THE (EQL <n>) dimension0)) ...)
                 ;; but it seems preferable to imply that the initial contents
                 ;; are wrongly shaped rather than that the array is.
                 `(sb!kernel::check-array-shape ,alloc-form ',data-dims))
                (t ; could not parse the data
                 `(fill-array ,(car contents) ,alloc-form)))))))

(define-source-transform coerce (x type &environment env)
  (if (and (sb!xc:constantp type env)
           (proper-list-p x)
           (memq (car x) '(sb!impl::|List| list
                           sb!impl::|Vector| vector)))
      (let* ((type (constant-form-value type env))
             (length (1- (length x)))
             (ctype (careful-values-specifier-type type)))
        (if (csubtypep ctype (specifier-type '(array * (*))))
            (multiple-value-bind (type element-type upgraded had-dimensions)
                (simplify-vector-type ctype)
              (declare (ignore type upgraded))
              (if had-dimensions
                  (values nil t)
                  `(make-array ,length
                               :initial-contents ,x
                               ,@(and (not (eq element-type *universal-type*))
                                      (not (eq element-type *wild-type*))
                                      `(:element-type ',(type-specifier element-type))))))
            (values nil t)))
      (values nil t)))

;;; This baby is a bit of a monster, but it takes care of any MAKE-ARRAY
;;; call which creates a vector with a known element type -- and tries
;;; to do a good job with all the different ways it can happen.
(defun transform-make-array-vector (length element-type initial-element
                                    initial-contents call)
  (let* ((c-length (if (lvar-p length)
                       (if (constant-lvar-p length) (lvar-value length))
                       length))
         (elt-spec (if element-type
                       (lvar-value element-type) ; enforces const-ness.
                       t))
         (elt-ctype (ir1-transform-specifier-type elt-spec))
         (saetp (if (unknown-type-p elt-ctype)
                    (give-up-ir1-transform "~S is an unknown type: ~S"
                                           :element-type elt-spec)
                    (find-saetp-by-ctype elt-ctype)))
         (default-initial-element (sb!vm:saetp-initial-element-default saetp))
         (n-bits (sb!vm:saetp-n-bits saetp))
         (typecode (sb!vm:saetp-typecode saetp))
         (n-pad-elements (sb!vm:saetp-n-pad-elements saetp))
         (n-words-form
          (if c-length
              (ceiling (* (+ c-length n-pad-elements) n-bits)
                       sb!vm:n-word-bits)
              (let ((padded-length-form (if (zerop n-pad-elements)
                                            'length
                                            `(+ length ,n-pad-elements))))
                (cond
                  ((= n-bits 0) 0)
                  ((>= n-bits sb!vm:n-word-bits)
                   `(* ,padded-length-form
                       ;; i.e., not RATIO
                       ,(the fixnum (/ n-bits sb!vm:n-word-bits))))
                  (t
                   (let ((n-elements-per-word (/ sb!vm:n-word-bits n-bits)))
                     (declare (type index n-elements-per-word)) ; i.e., not RATIO
                     `(ceiling (truly-the index ,padded-length-form)
                               ,n-elements-per-word)))))))
         (result-spec
          `(simple-array ,(sb!vm:saetp-specifier saetp) (,(or c-length '*))))
         (alloc-form
           `(truly-the ,result-spec
             (allocate-vector ,typecode
                              ;; If LENGTH is a singleton list,
                              ;; we want to avoid reading it.
                              (the index ,(or c-length 'length))
                              ,n-words-form))))
   (flet ((eliminate-keywords ()
            (eliminate-keyword-args
             call 1
             '((:element-type element-type)
               (:initial-contents initial-contents)
               (:initial-element initial-element)))))
    (cond ((and initial-element initial-contents)
           (abort-ir1-transform "Both ~S and ~S specified."
                                :initial-contents :initial-element))
          ;; Case (1)
          ;; :INITIAL-CONTENTS (LIST ...), (VECTOR ...) and `(1 1 ,x) with a
          ;; constant LENGTH.
          ((and initial-contents c-length
                (lvar-matches initial-contents
                              ;; FIXME: probably don't need all 4 of these now?
                              :fun-names '(list vector
                                           sb!impl::|List| sb!impl::|Vector|)
                              :arg-count c-length))
           (let ((parameters (eliminate-keywords))
                 (elt-vars (make-gensym-list c-length))
                 (lambda-list '(length)))
             (splice-fun-args initial-contents :any c-length)
             (dolist (p parameters)
               (setf lambda-list
                     (append lambda-list
                             (if (eq p 'initial-contents)
                                 elt-vars
                                 (list p)))))
             `(lambda ,lambda-list
                (declare (type ,elt-spec ,@elt-vars)
                         (ignorable ,@lambda-list))
                (truly-the ,result-spec
                 (initialize-vector ,alloc-form ,@elt-vars)))))
          ;; Case (2)
          ;; constant :INITIAL-CONTENTS and LENGTH
          ((and initial-contents c-length
                (constant-lvar-p initial-contents)
                ;; As a practical matter, the initial-contents should not be
                ;; too long, otherwise the compiler seems to spend forever
                ;; compiling the lambda with one parameter per item.
                ;; To make matters worse, the time grows superlinearly,
                ;; and it's not entirely obvious that passing a constant array
                ;; of 100x100 things is responsible for such an explosion.
                (<= (length (lvar-value initial-contents)) 1000))
           (let ((contents (lvar-value initial-contents)))
             (unless (= c-length (length contents))
               (abort-ir1-transform "~S has ~S elements, vector length is ~S."
                                    :initial-contents (length contents) c-length))
             (let ((lambda-list `(length ,@(eliminate-keywords))))
               `(lambda ,lambda-list
                  (declare (ignorable ,@lambda-list))
                  (truly-the ,result-spec
                   (initialize-vector ,alloc-form
                                      ,@(map 'list (lambda (elt)
                                                     `(the ,elt-spec ',elt))
                                             contents)))))))
          ;; Case (3)
          ;; any other :INITIAL-CONTENTS
          (initial-contents
           (let ((lambda-list `(length ,@(eliminate-keywords))))
             `(lambda ,lambda-list
                (declare (ignorable ,@lambda-list))
                (unless (= (length initial-contents) ,(or c-length 'length))
                  (error "~S has ~D elements, vector length is ~D."
                         :initial-contents (length initial-contents)
                         ,(or c-length 'length)))
                (truly-the ,result-spec
                           (replace ,alloc-form initial-contents)))))
          ;; Case (4)
          ;; :INITIAL-ELEMENT, not EQL to the default
          ((and initial-element
                (or (not (constant-lvar-p initial-element))
                    (not (eql default-initial-element (lvar-value initial-element)))))
           (let ((lambda-list `(length ,@(eliminate-keywords)))
                 (init (if (constant-lvar-p initial-element)
                           (list 'quote (lvar-value initial-element))
                           'initial-element)))
             `(lambda ,lambda-list
                (declare (ignorable ,@lambda-list))
                (truly-the ,result-spec
                           (fill ,alloc-form (the ,elt-spec ,init))))))
          ;; Case (5)
          ;; just :ELEMENT-TYPE, or maybe with :INITIAL-ELEMENT EQL to the
          ;; default
          (t
           #-sb-xc-host
           (and (and (testable-type-p elt-ctype)
                     (neq elt-ctype *empty-type*)
                     (not (ctypep default-initial-element elt-ctype)))
             ;; This situation arises e.g. in (MAKE-ARRAY 4 :ELEMENT-TYPE
             ;; '(INTEGER 1 5)) ANSI's definition of MAKE-ARRAY says "If
             ;; INITIAL-ELEMENT is not supplied, the consequences of later
             ;; reading an uninitialized element of new-array are undefined,"
             ;; so this could be legal code as long as the user plans to
             ;; write before he reads, and if he doesn't we're free to do
             ;; anything we like. But in case the user doesn't know to write
             ;; elements before he reads elements (or to read manuals before
             ;; he writes code:-), we'll signal a STYLE-WARNING in case he
             ;; didn't realize this.
             (if initial-element
                 (compiler-warn "~S ~S is not a ~S"
                                :initial-element default-initial-element
                                elt-spec)
                 (compiler-style-warn "The default initial element ~S is not a ~S."
                                      default-initial-element
                                      elt-spec)))
           (let ((lambda-list `(length ,@(eliminate-keywords))))
             `(lambda ,lambda-list
                (declare (ignorable ,@lambda-list))
                ,alloc-form)))))))

;;; IMPORTANT: The order of these three MAKE-ARRAY forms matters: the least
;;; specific must come first, otherwise suboptimal transforms will result for
;;; some forms.

(deftransform make-array ((dims &key initial-element initial-contents
                                     element-type
                                     adjustable fill-pointer
                                     displaced-to
                                     displaced-index-offset)
                          (t &rest *) *
                          :node node)
  (delay-ir1-transform node :constraint)
  (when (and initial-contents initial-element)
    (compiler-warn "Can't specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS")
    (give-up-ir1-transform))
  (when (and displaced-index-offset
             (not displaced-to))
    (compiler-warn "Can't specify :DISPLACED-INDEX-OFFSET without :DISPLACED-TO")
    (give-up-ir1-transform))
  (let ((fp-type (and fill-pointer
                      (lvar-type fill-pointer)) ))
    (when (and fp-type
               (csubtypep fp-type (specifier-type '(or index (eql t)))))
      (let* ((dims (and (constant-lvar-p dims)
                        (lvar-value dims)))
             (length (cond ((integerp dims)
                            dims)
                           ((singleton-p dims)
                            (car dims)))))
        (cond ((not dims))
              ((not length)
               (compiler-warn "Only vectors can have fill pointers."))
              ((and (csubtypep fp-type (specifier-type 'index))
                    (not (types-equal-or-intersect fp-type
                                                   (specifier-type `(integer 0 ,length)))))
               (compiler-warn "Invalid fill-pointer ~s for a vector of length ~s."
                              (type-specifier fp-type)
                              length))))))
  (macrolet ((maybe-arg (arg)
               `(and ,arg `(,,(keywordicate arg) ,',arg))))
    (let* ((eltype (cond ((not element-type) t)
                         ((not (constant-lvar-p element-type))
                          (give-up-ir1-transform
                           "ELEMENT-TYPE is not constant."))
                         (t
                          (lvar-value element-type))))
           (eltype-type (ir1-transform-specifier-type eltype))
           (saetp (if (unknown-type-p eltype-type)
                      (give-up-ir1-transform
                       "ELEMENT-TYPE ~s is not a known type"
                       eltype-type)
                      (find eltype-type
                            sb!vm:*specialized-array-element-type-properties*
                            :key #'sb!vm:saetp-ctype
                            :test #'csubtypep)))
           (creation-form `(%make-array
                            dims
                            ,(if saetp
                                 (sb!vm:saetp-typecode saetp)
                                 (give-up-ir1-transform))
                            ,(sb!vm:saetp-n-bits saetp)
                            ,@(maybe-arg initial-contents)
                            ,@(maybe-arg adjustable)
                            ,@(maybe-arg fill-pointer)
                            ,@(maybe-arg displaced-to)
                            ,@(maybe-arg displaced-index-offset))))
      (cond ((or (not initial-element)
                 (and (constant-lvar-p initial-element)
                      (eql (lvar-value initial-element)
                           (sb!vm:saetp-initial-element-default saetp))))
             creation-form)
            (t
             ;; error checking for target, disabled on the host because
             ;; (CTYPE-OF #\Null) is not possible.
             #-sb-xc-host
             (when (constant-lvar-p initial-element)
               (let ((value (lvar-value initial-element)))
                 (cond
                   ((not (ctypep value (sb!vm:saetp-ctype saetp)))
                    ;; this case will cause an error at runtime, so we'd
                    ;; better WARN about it now.
                    (warn 'array-initial-element-mismatch
                          :format-control "~@<~S is not a ~S (which is the ~
                                         ~S of ~S).~@:>"
                          :format-arguments
                          (list
                           value
                           (type-specifier (sb!vm:saetp-ctype saetp))
                           'upgraded-array-element-type
                           eltype)))
                   ((not (ctypep value eltype-type))
                    ;; this case will not cause an error at runtime, but
                    ;; it's still worth STYLE-WARNing about.
                    (compiler-style-warn "~S is not a ~S."
                                         value eltype)))))
             `(let ((array ,creation-form))
                (multiple-value-bind (vector)
                    (%data-vector-and-index array 0)
                  (fill vector (the ,(sb!vm:saetp-specifier saetp) initial-element)))
                array))))))

;;; The list type restriction does not ensure that the result will be a
;;; multi-dimensional array. But the lack of adjustable, fill-pointer,
;;; and displaced-to keywords ensures that it will be simple.
;;;
;;; FIXME: should we generalize this transform to non-simple (though
;;; non-displaced-to) arrays, given that we have %WITH-ARRAY-DATA to
;;; deal with those? Maybe when the DEFTRANSFORM
;;; %DATA-VECTOR-AND-INDEX in the VECTOR case problem is solved? --
;;; CSR, 2002-07-01
(deftransform make-array ((dims &key
                                element-type initial-element initial-contents)
                          (list &key
                                (:element-type (constant-arg *))
                                (:initial-element *)
                                (:initial-contents *))
                          *
                          :node call)
  (block make-array
    ;; If lvar-use of DIMS is a call to LIST, then it must mean that LIST
    ;; was declared notinline - because if it weren't, then it would have been
    ;; source-transformed into CONS - which gives us reason NOT to optimize
    ;; this call to MAKE-ARRAY. So look for CONS instead of LIST,
    ;; which means that LIST was *not* declared notinline.
    (when (and (lvar-matches dims :fun-names '(cons) :arg-count 2)
               (let ((cdr (second (combination-args (lvar-uses dims)))))
                 (and (constant-lvar-p cdr) (null (lvar-value cdr)))))
      (let* ((args (splice-fun-args dims :any 2)) ; the args to CONS
             (dummy (cadr args)))
        (flush-dest dummy)
        (setf (combination-args call) (delete dummy (combination-args call)))
        (return-from make-array
          (transform-make-array-vector (car args)
                                       element-type
                                       initial-element
                                       initial-contents
                                       call))))
    (unless (constant-lvar-p dims)
      (give-up-ir1-transform
       "The dimension list is not constant; cannot open code array creation."))
    (let ((dims (lvar-value dims))
          (element-type-ctype (and (constant-lvar-p element-type)
                                   (ir1-transform-specifier-type
                                    (lvar-value element-type)))))
      (when (contains-unknown-type-p element-type-ctype)
        (give-up-ir1-transform))
      (unless (every (lambda (x) (typep x '(integer 0))) dims)
        (give-up-ir1-transform
         "The dimension list contains something other than an integer: ~S"
         dims))
      (if (singleton-p dims)
          (transform-make-array-vector (car dims) element-type
                                       initial-element initial-contents call)
          (let* ((total-size (reduce #'* dims))
                 (rank (length dims))
                 (spec `(simple-array
                         ,(cond ((null element-type) t)
                                (element-type-ctype
                                 (sb!xc:upgraded-array-element-type
                                  (lvar-value element-type)))
                                (t '*))
                         ,(make-list rank :initial-element '*))))
            `(let ((header (make-array-header sb!vm:simple-array-widetag ,rank))
                   (data (make-array ,total-size
                                     ,@(when element-type
                                             '(:element-type element-type))
                                     ,@(when initial-element
                                             '(:initial-element initial-element)))))
               ,@(when initial-contents
                       ;; FIXME: This is could be open coded at least a bit too
                       `((fill-data-vector data ',dims initial-contents)))
               (setf (%array-fill-pointer header) ,total-size)
               (setf (%array-fill-pointer-p header) nil)
               (setf (%array-available-elements header) ,total-size)
               (setf (%array-data-vector header) data)
               (setf (%array-displaced-p header) nil)
               (setf (%array-displaced-from header) nil)
               ,@(let ((axis -1))
                      (mapcar (lambda (dim)
                                `(setf (%array-dimension header ,(incf axis))
                                       ,dim))
                              dims))
               (truly-the ,spec header)))))))

(deftransform make-array ((dims &key element-type initial-element initial-contents)
                          (integer &key
                                   (:element-type (constant-arg *))
                                   (:initial-element *)
                                   (:initial-contents *))
                          *
                          :node call)
  (transform-make-array-vector dims
                               element-type
                               initial-element
                               initial-contents
                               call))

;;;; ADJUST-ARRAY
(deftransform adjust-array ((array dims &key displaced-to displaced-index-offset)
                            (array integer &key
                                   (:displaced-to array)
                                   (:displaced-index-offset *)))
  (unless displaced-to
    (give-up-ir1-transform))
  `(progn
     (when (invalid-array-p array)
       (invalid-array-error array))
     (unless (= 1 (array-rank array))
       (error "The number of dimensions is not equal to the rank of the array"))
     (unless (eql (array-element-type array) (array-element-type displaced-to))
       (error "Can't displace an array of type ~S to another of type ~S"
              (array-element-type array) (array-element-type displaced-to)))
     (let ((displacement (or displaced-index-offset 0)))
       (when (< (array-total-size displaced-to) (+ displacement dims))
         (error "The :DISPLACED-TO array is too small"))
       (if (adjustable-array-p array)
           (let ((nfp (when (array-has-fill-pointer-p array)
                        (when (> (%array-fill-pointer array) dims)
                          (error "Cannot ADJUST-ARRAY an array to a size smaller than its fill pointer"))
                        (%array-fill-pointer array))))
             (set-array-header array displaced-to dims nfp
                               displacement dims t nil))
           (make-array dims :element-type (array-element-type array)
                            :displaced-to displaced-to
                            ,@(and displaced-index-offset
                                   '(:displaced-index-offset displacement)))))))

;;;; miscellaneous properties of arrays

;;; Transforms for various array properties. If the property is know
;;; at compile time because of a type spec, use that constant value.

;;; Most of this logic may end up belonging in code/late-type.lisp;
;;; however, here we also need the -OR-GIVE-UP for the transforms, and
;;; maybe this is just too sloppy for actual type logic.  -- CSR,
;;; 2004-02-18
(defun array-type-dimensions-or-give-up (type)
  (labels ((maybe-array-type-dimensions (type)
             (typecase type
               (array-type
                (array-type-dimensions type))
               (union-type
                (let* ((types (loop for type in (union-type-types type)
                                    for dimensions = (maybe-array-type-dimensions type)
                                    when (eq dimensions '*)
                                    do
                                    (return-from maybe-array-type-dimensions '*)
                                    when dimensions
                                    collect it))
                       (result (car types))
                       (length (length result))
                       (complete-match t))
                  (dolist (other (cdr types))
                    (when (/= length (length other))
                      (give-up-ir1-transform
                       "~@<dimensions of arrays in union type ~S do not match~:@>"
                       (type-specifier type)))
                    (unless (equal result other)
                      (setf complete-match nil)))
                  (if complete-match
                      result
                      (make-list length :initial-element '*))))
               (intersection-type
                (let* ((types (remove nil (mapcar #'maybe-array-type-dimensions
                                                  (intersection-type-types type))))
                       (result (car types)))
                  (dolist (other (cdr types) result)
                    (unless (equal result other)
                      (abort-ir1-transform
                       "~@<dimensions of arrays in intersection type ~S do not match~:@>"
                       (type-specifier type)))))))))
    (or (maybe-array-type-dimensions type)
        (give-up-ir1-transform
         "~@<don't know how to extract array dimensions from type ~S~:@>"
         (type-specifier type)))))

(defun conservative-array-type-complexp (type)
  (typecase type
    (array-type (array-type-complexp type))
    (union-type
     (let ((types (union-type-types type)))
       (aver (> (length types) 1))
       (let ((result (conservative-array-type-complexp (car types))))
         (dolist (type (cdr types) result)
           (unless (eq (conservative-array-type-complexp type) result)
             (return-from conservative-array-type-complexp :maybe))))))
    ;; FIXME: intersection type
    (t :maybe)))

;; Let type derivation handle constant cases. We only do easy strength
;; reduction.
(deftransform array-rank ((array) (array) * :node node)
  (let ((array-type (lvar-type array)))
    (cond ((eq t (and (array-type-p array-type)
                      (array-type-complexp array-type)))
           '(%array-rank array))
          (t
           (delay-ir1-transform node :constraint)
           `(if (array-header-p array)
                (%array-rank array)
                1)))))

(defun derive-array-rank (ctype)
  (let ((array (specifier-type 'array)))
    (flet ((over (x)
             (cond ((not (types-equal-or-intersect x array))
                    '()) ; Definitely not an array!
                   ((array-type-p x)
                    (let ((dims (array-type-dimensions x)))
                      (if (eql dims '*)
                          '*
                          (list (length dims)))))
                   (t '*)))
           (under (x)
             ;; Might as well catch some easy negation cases.
             (typecase x
               (array-type
                (let ((dims (array-type-dimensions x)))
                  (cond ((eql dims '*)
                         '*)
                        ((every (lambda (dim)
                                  (eql dim '*))
                                dims)
                         (list (length dims)))
                        (t
                         '()))))
               (t '()))))
      (declare (dynamic-extent #'over #'under))
      (multiple-value-bind (not-p ranks)
          (list-abstract-type-function ctype #'over :under #'under)
        (cond ((eql ranks '*)
               (aver (not not-p))
               nil)
              (not-p
               (specifier-type `(not (member ,@ranks))))
              (t
               (specifier-type `(member ,@ranks))))))))

(defoptimizer (array-rank derive-type) ((array))
  (derive-array-rank (lvar-type array)))

(defoptimizer (%array-rank derive-type) ((array))
  (derive-array-rank (lvar-type array)))

;;; If we know the dimensions at compile time, just use it. Otherwise,
;;; if we can tell that the axis is in bounds, convert to
;;; %ARRAY-DIMENSION (which just indirects the array header) or length
;;; (if it's simple and a vector).
(deftransform array-dimension ((array axis)
                               (array index))
  (unless (constant-lvar-p axis)
    (give-up-ir1-transform "The axis is not constant."))
  ;; Dimensions may change thanks to ADJUST-ARRAY, so we need the
  ;; conservative type.
  (let ((array-type (lvar-conservative-type array))
        (axis (lvar-value axis)))
    (let ((dims (array-type-dimensions-or-give-up array-type)))
      (unless (listp dims)
        (give-up-ir1-transform
         "The array dimensions are unknown; must call ARRAY-DIMENSION at runtime."))
      (unless (> (length dims) axis)
        (abort-ir1-transform "The array has dimensions ~S, ~W is too large."
                             dims
                             axis))
      (let ((dim (nth axis dims)))
        (cond ((integerp dim)
               dim)
              ((= (length dims) 1)
               (ecase (conservative-array-type-complexp array-type)
                 ((t)
                  '(%array-dimension array 0))
                 ((nil)
                  '(vector-length array))
                 ((:maybe)
                  `(if (array-header-p array)
                       (%array-dimension array axis)
                       (vector-length array)))))
              (t
               '(%array-dimension array axis)))))))

;;; If the length has been declared and it's simple, just return it.
(deftransform length ((vector)
                      ((simple-array * (*))))
  (let ((type (lvar-type vector)))
    (let ((dims (array-type-dimensions-or-give-up type)))
      (unless (and (listp dims) (integerp (car dims)))
        (give-up-ir1-transform
         "Vector length is unknown, must call LENGTH at runtime."))
      (car dims))))

;;; All vectors can get their length by using VECTOR-LENGTH. If it's
;;; simple, it will extract the length slot from the vector. It it's
;;; complex, it will extract the fill pointer slot from the array
;;; header.
(deftransform length ((vector) (vector))
  '(vector-length vector))

;;; If a simple array with known dimensions, then VECTOR-LENGTH is a
;;; compile-time constant.
(deftransform vector-length ((vector))
  (let ((vtype (lvar-type vector)))
    (let ((dim (first (array-type-dimensions-or-give-up vtype))))
      (when (eq dim '*)
        (give-up-ir1-transform))
      (when (conservative-array-type-complexp vtype)
        (give-up-ir1-transform))
      dim)))

;;; Again, if we can tell the results from the type, just use it.
;;; Otherwise, if we know the rank, convert into a computation based
;;; on array-dimension or %array-available-elements
(deftransform array-total-size ((array) (array))
  (let* ((array-type (lvar-type array))
         (dims (array-type-dimensions-or-give-up array-type)))
    (unless (listp dims)
      (give-up-ir1-transform "can't tell the rank at compile time"))
    (cond ((not (memq '* dims))
           (reduce #'* dims))
          ((not (cdr dims))
           ;; A vector, can't use LENGTH since this ignores the fill-pointer
           `(truly-the index (array-dimension array 0)))
          (t
           `(%array-available-elements array)))))

;;; Only complex vectors have fill pointers.
(deftransform array-has-fill-pointer-p ((array))
  (let ((array-type (lvar-type array)))
    (let ((dims (array-type-dimensions-or-give-up array-type)))
      (if (and (listp dims) (not (= (length dims) 1)))
          nil
          (ecase (conservative-array-type-complexp array-type)
            ((t)
             t)
            ((nil)
             nil)
            ((:maybe)
             (give-up-ir1-transform
              "The array type is ambiguous; must call ~
               ARRAY-HAS-FILL-POINTER-P at runtime.")))))))

(deftransform check-bound ((array dimension index) * * :node node)
  ;; This is simply to avoid multiple evaluation of INDEX by the
  ;; translator, it's easier to wrap it in a lambda from DEFTRANSFORM
  `(bound-cast array ,(if (constant-lvar-p dimension)
                          (lvar-value dimension)
                          'dimension)
               index))

;;;; WITH-ARRAY-DATA

;;; This checks to see whether the array is simple and the start and
;;; end are in bounds. If so, it proceeds with those values.
;;; Otherwise, it calls %WITH-ARRAY-DATA. Note that %WITH-ARRAY-DATA
;;; may be further optimized.
;;;
;;; Given any ARRAY, bind DATA-VAR to the array's data vector and
;;; START-VAR and END-VAR to the start and end of the designated
;;; portion of the data vector. SVALUE and EVALUE are any start and
;;; end specified to the original operation, and are factored into the
;;; bindings of START-VAR and END-VAR. OFFSET-VAR is the cumulative
;;; offset of all displacements encountered, and does not include
;;; SVALUE.
;;;
;;; When FORCE-INLINE is set, the underlying %WITH-ARRAY-DATA form is
;;; forced to be inline, overriding the ordinary judgment of the
;;; %WITH-ARRAY-DATA DEFTRANSFORMs. Ordinarily the DEFTRANSFORMs are
;;; fairly picky about their arguments, figuring that if you haven't
;;; bothered to get all your ducks in a row, you probably don't care
;;; that much about speed anyway! But in some cases it makes sense to
;;; do type testing inside %WITH-ARRAY-DATA instead of outside, and
;;; the DEFTRANSFORM can't tell that that's going on, so it can make
;;; sense to use FORCE-INLINE option in that case.
(sb!xc:defmacro with-array-data (((data-var array &key offset-var)
                                  (start-var &optional (svalue 0))
                                  (end-var &optional (evalue nil))
                                  &key force-inline check-fill-pointer
                                       array-header-p)
                                 &body forms
                                 &environment env)
  (once-only ((n-array array)
              (n-svalue `(the index ,svalue))
              (n-evalue `(the (or index null) ,evalue)))
    (let ((check-bounds (policy env (plusp insert-array-bounds-checks))))
      `(multiple-value-bind (,data-var
                             ,start-var
                             ,end-var
                             ,@ (when offset-var `(,offset-var)))
           (cond ,@(and (not array-header-p)
                        `(((not (array-header-p ,n-array))
                           (let ((,n-array ,n-array))
                             (declare (type vector ,n-array))
                             ,(once-only ((n-len `(length ,n-array))
                                          (n-end `(or ,n-evalue ,n-len)))
                                (if check-bounds
                                    `(if (<= 0 ,n-svalue ,n-end ,n-len)
                                         (values (truly-the simple-array ,n-array)
                                                 ,n-svalue ,n-end 0)
                                         ,(if check-fill-pointer
                                              `(sequence-bounding-indices-bad-error ,n-array ,n-svalue ,n-evalue)
                                              `(array-bounding-indices-bad-error ,n-array ,n-svalue ,n-evalue)))
                                    `(values (truly-the simple-array ,n-array)
                                             ,n-svalue ,n-end 0)))))))
                 (t
                  ,(cond (force-inline
                          `(%with-array-data-macro ,n-array ,n-svalue ,n-evalue
                                                   :check-bounds ,check-bounds
                                                   :check-fill-pointer ,check-fill-pointer
                                                   :array-header-p t))
                         (check-fill-pointer
                          `(%with-array-data/fp ,n-array ,n-svalue ,n-evalue))
                         (t
                          `(%with-array-data ,n-array ,n-svalue ,n-evalue)))))
         ,@forms))))

;;; This is the fundamental definition of %WITH-ARRAY-DATA, for use in
;;; DEFTRANSFORMs and DEFUNs.
(sb!xc:defmacro %with-array-data-macro
    (array start end &key (element-type '*) check-bounds check-fill-pointer
                          array-header-p)
  (with-unique-names (size defaulted-end data cumulative-offset)
    `(let* ((,size ,(cond (check-fill-pointer
                           `(length (the vector ,array)))
                          (array-header-p
                           `(%array-available-elements ,array))
                          (t
                           `(array-total-size ,array))))
            (,defaulted-end (or ,end ,size)))
       ,@ (when check-bounds
            `((unless (<= ,start ,defaulted-end ,size)
                ,(if check-fill-pointer
                     `(sequence-bounding-indices-bad-error ,array ,start ,end)
                     `(array-bounding-indices-bad-error ,array ,start ,end)))))
       (do ((,data ,(if array-header-p
                        `(%array-data-vector ,array)
                        array)
                   (%array-data-vector ,data))
            (,cumulative-offset ,(if array-header-p
                                     `(%array-displacement ,array)
                                     0)
                                (truly-the index
                                           (+ ,cumulative-offset
                                              (%array-displacement ,data)))))
           ((not (array-header-p ,data))
            (values (truly-the (simple-array ,element-type 1) ,data)
                    (truly-the index (+ ,cumulative-offset ,start))
                    (truly-the index (+ ,cumulative-offset ,defaulted-end))
                    ,cumulative-offset))))))

(defun transform-%with-array-data/mumble (array node check-fill-pointer)
  (let ((element-type (upgraded-element-type-specifier-or-give-up array))
        (type (lvar-type array))
        (check-bounds (policy node (plusp insert-array-bounds-checks))))
    (if (and (array-type-p type)
             (not (array-type-complexp type))
             (listp (array-type-dimensions type))
             (not (null (cdr (array-type-dimensions type)))))
        ;; If it's a simple multidimensional array, then just return
        ;; its data vector directly rather than going through
        ;; %WITH-ARRAY-DATA-MACRO. SBCL doesn't generally generate
        ;; code that would use this currently, but we have encouraged
        ;; users to use WITH-ARRAY-DATA and we may use it ourselves at
        ;; some point in the future for optimized libraries or
        ;; similar.
        (if check-bounds
            `(let* ((data (truly-the (simple-array ,element-type (*))
                                     (%array-data-vector array)))
                    (len (length data))
                    (real-end (or end len)))
               (unless (<= 0 start data-end lend)
                 (sequence-bounding-indices-bad-error array start end))
               (values data 0 real-end 0))
            `(let ((data (truly-the (simple-array ,element-type (*))
                                    (%array-data-vector array))))
               (values data 0 (or end (length data)) 0)))
        `(%with-array-data-macro array start end
                                 :check-fill-pointer ,check-fill-pointer
                                 :check-bounds ,check-bounds
                                 :element-type ,element-type))))

;; It might very well be reasonable to allow general ARRAY here, I
;; just haven't tried to understand the performance issues involved.
;; -- WHN, and also CSR 2002-05-26
(deftransform %with-array-data ((array start end)
                                ((or vector simple-array) index (or index null) t)
                                *
                                :node node
                                :policy (> speed space))
  "inline non-SIMPLE-vector-handling logic"
  (transform-%with-array-data/mumble array node nil))
(deftransform %with-array-data/fp ((array start end)
                                ((or vector simple-array) index (or index null) t)
                                *
                                :node node
                                :policy (> speed space))
  "inline non-SIMPLE-vector-handling logic"
  (transform-%with-array-data/mumble array node t))

;;;; array accessors

;;; We convert all typed array accessors into AREF and (SETF AREF) with type
;;; assertions on the array.
(macrolet ((define-bit-frob (reffer simplep)
             `(progn
                (define-source-transform ,reffer (a &rest i)
                  `(aref (the (,',(if simplep 'simple-array 'array)
                                  bit
                                  ,(mapcar (constantly '*) i))
                           ,a) ,@i))
                (define-source-transform (setf ,reffer) (value a &rest i)
                  `(setf (aref (the (,',(if simplep 'simple-array 'array)
                                     bit
                                     ,(mapcar (constantly '*) i))
                                    ,a) ,@i)
                         ,value)))))
  (define-bit-frob sbit t)
  (define-bit-frob bit nil))

(macrolet ((define-frob (reffer setter type)
             `(progn
                (define-source-transform ,reffer (a i)
                  `(aref (the ,',type ,a) ,i))
                (define-source-transform ,setter (a i v)
                  `(setf (aref (the ,',type ,a) ,i) ,v)))))
  (define-frob schar %scharset simple-string)
  (define-frob char %charset string))

;;; We transform SVREF and %SVSET directly into DATA-VECTOR-REF/SET: this is
;;; around 100 times faster than going through the general-purpose AREF
;;; transform which ends up doing a lot of work -- and introducing many
;;; intermediate lambdas, each meaning a new trip through the compiler -- to
;;; get the same result.
;;;
;;; FIXME: [S]CHAR, and [S]BIT above would almost certainly benefit from a similar
;;; treatment.
(define-source-transform svref (vector index)
  (let ((elt-type (or (when (symbolp vector)
                        (let ((var (lexenv-find vector vars)))
                          (when (lambda-var-p var)
                            (type-specifier
                             (array-type-declared-element-type (lambda-var-type var))))))
                      t)))
    (with-unique-names (n-vector)
      `(let ((,n-vector ,vector))
         (the ,elt-type (data-vector-ref
                         (the simple-vector ,n-vector)
                         (check-bound ,n-vector (length ,n-vector) ,index)))))))

(define-source-transform %svset (vector index value)
  (let ((elt-type (or (when (symbolp vector)
                        (let ((var (lexenv-find vector vars)))
                          (when (lambda-var-p var)
                            (type-specifier
                             (array-type-declared-element-type (lambda-var-type var))))))
                      t)))
    (with-unique-names (n-vector)
      `(let ((,n-vector ,vector))
         (truly-the ,elt-type (data-vector-set
                               (the simple-vector ,n-vector)
                               (check-bound ,n-vector (length ,n-vector) ,index)
                               (the ,elt-type ,value)))))))

(macrolet (;; This is a handy macro for computing the row-major index
           ;; given a set of indices. We wrap each index with a call
           ;; to CHECK-BOUND to ensure that everything works out
           ;; correctly. We can wrap all the interior arithmetic with
           ;; TRULY-THE INDEX because we know the resultant
           ;; row-major index must be an index.
           (with-row-major-index ((array indices index &optional new-value)
                                  &rest body)
             `(let (n-indices dims)
                (dotimes (i (length ,indices))
                  (push (make-symbol (format nil "INDEX-~D" i)) n-indices)
                  (push (make-symbol (format nil "DIM-~D" i)) dims))
                (setf n-indices (nreverse n-indices))
                (setf dims (nreverse dims))
                `(lambda (,@',(when new-value (list new-value))
                          ,',array ,@n-indices)
                   (declare (ignorable ,',array))
                   (let* (,@(let ((,index -1))
                              (mapcar (lambda (name)
                                        `(,name (array-dimension
                                                 ,',array
                                                 ,(incf ,index))))
                                      dims))
                            (,',index
                             ,(if (null dims)
                                  0
                                (do* ((dims dims (cdr dims))
                                      (indices n-indices (cdr indices))
                                      (last-dim nil (car dims))
                                      (form `(check-bound ,',array
                                                          ,(car dims)
                                                          ,(car indices))
                                            `(truly-the
                                              index
                                              (+ (truly-the index
                                                            (* ,form
                                                               ,last-dim))
                                                 (check-bound
                                                  ,',array
                                                  ,(car dims)
                                                  ,(car indices))))))
                                    ((null (cdr dims)) form)))))
                     ,',@body)))))

  ;; Just return the index after computing it.
  (deftransform array-row-major-index ((array &rest indices))
    (with-row-major-index (array indices index)
      index))

  ;; Convert AREF and (SETF AREF) into a HAIRY-DATA-VECTOR-REF (or
  ;; HAIRY-DATA-VECTOR-SET) with the set of indices replaced with the an
  ;; expression for the row major index.
  (deftransform aref ((array &rest indices))
    (with-row-major-index (array indices index)
      (hairy-data-vector-ref array index)))

  (deftransform (setf aref) ((new-value array &rest subscripts))
    (with-row-major-index (array subscripts index new-value)
                          (hairy-data-vector-set array index new-value))))

;; For AREF of vectors we do the bounds checking in the callee. This
;; lets us do a significantly more efficient check for simple-arrays
;; without bloating the code. If we already know the type of the array
;; with sufficient precision, skip directly to DATA-VECTOR-REF.
(deftransform aref ((array index) (t t) * :node node)
  (let* ((type (lvar-type array))
         (element-ctype (array-type-upgraded-element-type type)))
    (cond
      ((eq element-ctype *empty-type*)
       `(data-nil-vector-ref array index))
      ((and (array-type-p type)
            (null (array-type-complexp type))
            (neq element-ctype *wild-type*)
            (eql (length (array-type-dimensions type)) 1))
       (let* ((declared-element-ctype (array-type-declared-element-type type))
              (bare-form
                `(data-vector-ref array
                                  (check-bound array (array-dimension array 0) index))))
         (if (type= declared-element-ctype element-ctype)
             bare-form
             `(the ,(type-specifier declared-element-ctype) ,bare-form))))
      ((policy node (zerop insert-array-bounds-checks))
       `(hairy-data-vector-ref array index))
      (t `(hairy-data-vector-ref/check-bounds array index)))))

(deftransform (setf aref) ((new-value array index) (t t t) * :node node)
  (if (policy node (zerop insert-array-bounds-checks))
      `(hairy-data-vector-set array index new-value)
      `(hairy-data-vector-set/check-bounds array index new-value)))

;;; But if we find out later that there's some useful type information
;;; available, switch back to the normal one to give other transforms
;;; a stab at it.
(macrolet ((define (name transform-to extra extra-type)
             (declare (ignore extra-type))
             `(deftransform ,name ((array index ,@extra))
                (let* ((type (lvar-type array))
                       (element-type (array-type-upgraded-element-type type))
                       (declared-type (type-specifier
                                       (array-type-declared-element-type type))))
                  ;; If an element type has been declared, we want to
                  ;; use that information it for type checking (even
                  ;; if the access can't be optimized due to the array
                  ;; not being simple).
                  (when (and (eq element-type *wild-type*)
                             ;; This type logic corresponds to the special
                             ;; case for strings in HAIRY-DATA-VECTOR-REF
                             ;; (generic/vm-tran.lisp)
                             (not (csubtypep type (specifier-type 'simple-string))))
                    (when (or (not (array-type-p type))
                              ;; If it's a simple array, we might be able
                              ;; to inline the access completely.
                              (not (null (array-type-complexp type))))
                      (give-up-ir1-transform
                       "Upgraded element type of array is not known at compile time.")))
                  ,(if extra
                       ``(truly-the ,declared-type
                                    (,',transform-to array
                                                     (check-bound array
                                                                  (array-dimension array 0)
                                                                  index)
                                                     (the ,declared-type ,@',extra)))
                       ``(the ,declared-type
                           (,',transform-to array
                                            (check-bound array
                                                         (array-dimension array 0)
                                                         index))))))))
  (define hairy-data-vector-ref/check-bounds
      hairy-data-vector-ref nil nil)
  (define hairy-data-vector-set/check-bounds
      hairy-data-vector-set (new-value) (*)))

;;; Just convert into a HAIRY-DATA-VECTOR-REF (or
;;; HAIRY-DATA-VECTOR-SET) after checking that the index is inside the
;;; array total size.
(deftransform row-major-aref ((array index))
  `(hairy-data-vector-ref array
                          (check-bound array (array-total-size array) index)))
(deftransform %set-row-major-aref ((array index new-value))
  `(hairy-data-vector-set array
                          (check-bound array (array-total-size array) index)
                          new-value))

;;;; bit-vector array operation canonicalization
;;;;
;;;; We convert all bit-vector operations to have the result array
;;;; specified. This allows any result allocation to be open-coded,
;;;; and eliminates the need for any VM-dependent transforms to handle
;;;; these cases.

(macrolet ((def (fun)
             `(progn
               (deftransform ,fun ((bit-array-1 bit-array-2
                                                &optional result-bit-array)
                                   (bit-vector bit-vector &optional null) *
                                   :policy (>= speed space))
                 `(,',fun bit-array-1 bit-array-2
                   (make-array (array-dimension bit-array-1 0) :element-type 'bit)))
               ;; If result is T, make it the first arg.
               (deftransform ,fun ((bit-array-1 bit-array-2 result-bit-array)
                                   (bit-vector bit-vector (eql t)) *)
                 `(,',fun bit-array-1 bit-array-2 bit-array-1)))))
  (def bit-and)
  (def bit-ior)
  (def bit-xor)
  (def bit-eqv)
  (def bit-nand)
  (def bit-nor)
  (def bit-andc1)
  (def bit-andc2)
  (def bit-orc1)
  (def bit-orc2))

;;; Similar for BIT-NOT, but there is only one arg...
(deftransform bit-not ((bit-array-1 &optional result-bit-array)
                       (bit-vector &optional null) *
                       :policy (>= speed space))
  '(bit-not bit-array-1
            (make-array (array-dimension bit-array-1 0) :element-type 'bit)))
(deftransform bit-not ((bit-array-1 result-bit-array)
                       (bit-vector (eql t)))
  '(bit-not bit-array-1 bit-array-1))

;;; Pick off some constant cases.
(defoptimizer (array-header-p derive-type) ((array))
  (let ((type (lvar-type array)))
    (cond ((not (array-type-p type))
           ;; FIXME: use analogue of ARRAY-TYPE-DIMENSIONS-OR-GIVE-UP
           nil)
          (t
           (let ((dims (array-type-dimensions type)))
             (cond ((csubtypep type (specifier-type '(simple-array * (*))))
                    ;; no array header
                    (specifier-type 'null))
                   ((and (listp dims) (/= (length dims) 1))
                    ;; multi-dimensional array, will have a header
                    (specifier-type '(eql t)))
                   ((eql (array-type-complexp type) t)
                    (specifier-type '(eql t)))
                   (t
                    nil)))))))
