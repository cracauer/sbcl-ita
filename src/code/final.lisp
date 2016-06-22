;;;; finalization based on weak pointers

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(declaim (type list **finalizer-store**))
(defglobal **finalizer-store** nil)
(declaim (type hash-table **finalizer-store-ht**))
(defglobal **finalizer-store-ht** (make-hash-table :test 'eq :weakness :key))

(defglobal **finalizer-store-lock**
  (sb!thread:make-mutex :name "Finalizer store lock."))

(defmacro with-finalizer-store-lock (&body body)
  `(sb!thread::with-system-mutex (**finalizer-store-lock** :without-gcing t)
     ,@body))

(defun finalize (object function &key dont-save)
  #!+sb-doc
  "Arrange for the designated FUNCTION to be called when there
are no more references to OBJECT, including references in
FUNCTION itself.

If DONT-SAVE is true, the finalizer will be cancelled when
SAVE-LISP-AND-DIE is called: this is useful for finalizers
deallocating system memory, which might otherwise be called
with addresses from the old image.

In a multithreaded environment FUNCTION may be called in any
thread. In both single and multithreaded environments FUNCTION
may be called in any dynamic scope: consequences are unspecified
if FUNCTION is not fully re-entrant.

Errors from FUNCTION are handled and cause a WARNING to be
signalled in whichever thread the FUNCTION was called in.

Examples:

  ;;; GOOD, assuming RELEASE-HANDLE is re-entrant.
  (let* ((handle (get-handle))
         (object (make-object handle)))
   (finalize object (lambda () (release-handle handle)))
   object)

  ;;; BAD, finalizer refers to object being finalized, causing
  ;;; it to be retained indefinitely!
  (let* ((handle (get-handle))
         (object (make-object handle)))
    (finalize object
              (lambda ()
                (release-handle (object-handle object)))))

  ;;; BAD, not re-entrant!
  (defvar *rec* nil)

  (defun oops ()
   (when *rec*
     (error \"recursive OOPS\"))
   (let ((*rec* t))
     (gc))) ; or just cons enough to cause one

  (progn
    (finalize \"oops\" #'oops)
    (oops)) ; GC causes re-entry to #'oops due to the finalizer
            ; -> ERROR, caught, WARNING signalled"
  (unless object
    (error "Cannot finalize NIL."))
  (with-finalizer-store-lock
    (let ((entry (gethash object **finalizer-store-ht**)))
      (unless entry
        (setf entry (list (make-weak-pointer object) nil nil)
              (gethash object **finalizer-store-ht**) entry)
        (push entry **finalizer-store**))
      (if dont-save
          (push function (third entry))
          (push function (second entry)))))
  object)

(defun deinit-finalizers ()
  ;; remove :dont-save finalizers.
  (with-finalizer-store-lock
    (maphash (lambda (k entry)
               ;; Check if there are any finalizers that need to be saved.
               (if (second entry)
                   ;; In this case only remove the dont-save ones.
                   (setf (third entry) nil)
                   (remhash k **finalizer-store-ht**)))
             **finalizer-store-ht**)
    (setf **finalizer-store** (delete-if-not #'second **finalizer-store**)))
  nil)

(defun cancel-finalization (object)
  #!+sb-doc
  "Cancel all finalization for OBJECT."
  (when object
    (with-finalizer-store-lock
      (let ((entry (gethash object **finalizer-store-ht**)))
        (when entry
          (setf (car entry) nil
                (cdr entry) nil)
          (remhash object **finalizer-store-ht**))))
    object))

(defun run-pending-finalizers ()
  (let (pending)
    ;; We want to run the finalizer bodies outside the lock in case
    ;; finalization of X causes finalization to be added for Y.
    ;; And to avoid consing we can reuse the deleted conses from the
    ;; store to build the list of functions.
    (with-finalizer-store-lock
      ;; Split the **finalizer-store** by the weak-pointer-value.
      (let ((store (shiftf **finalizer-store** nil)))
        (loop while store do
          (cond ((null (caar store))
                 ;; Skip empty cell.
                 (shiftf store (cdr store) nil))
                ((weak-pointer-value (caar store))
                 ;; Push the cell back onto the **finalizer-store**.
                 (rotatef store (cdr store) **finalizer-store**))
                (t
                 ;; Push the cell onto the pending list.
                 (rotatef store (cdr store) pending))))))
    (loop for (ptr funs1 funs2) in pending do
      (flet ((call (fun)
               (handler-case (funcall fun)
                 (error (c)
                   (warn "Error calling finalizer ~S:~%  ~S" fun c)))))
        (mapc #'call funs1)
        (mapc #'call funs2))))
  nil)
