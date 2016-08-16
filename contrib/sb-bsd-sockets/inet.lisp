(in-package :sb-bsd-sockets)

;;;

(define-condition unknown-protocol ()
  ((name :initarg :name
         :reader unknown-protocol-name))
  (:report (lambda (c s)
             (format s "Protocol not found: ~a" (prin1-to-string
                                                 (unknown-protocol-name c))))))
(defvar *protocols*
  `((:tcp ,sockint::ipproto_tcp "tcp" "TCP")
    (:udp ,sockint::ipproto_udp "udp" "UDP")
    (:ip ,sockint::ipproto_ip "ip" "IP")
    (:ipv6 ,sockint::ipproto_ipv6 "ipv6" "IPV6")
    (:icmp ,sockint::ipproto_icmp "icmp" "ICMP")
    (:igmp ,sockint::ipproto_igmp "igmp" "IGMP")
    (:raw ,sockint::ipproto_raw "raw" "RAW")))

;;; Try to get to a protocol quickly, falling back to calling
;;; getprotobyname if it's available.
(defun get-protocol-by-name (name)
  "Given a protocol name, return the protocol number, the protocol name, and
a list of protocol aliases"
  (let ((result (cdr (if (keywordp name)
                         (assoc name *protocols*)
                         (assoc name *protocols* :test #'string-equal)))))
    (if result
        (values (first result) (second result) (third result))
        #-android
        (getprotobyname (string-downcase name))
        #+android (error 'unknown-protocol :name name))))

#+(and sb-thread (not os-provides-getprotoby-r) (not android) (not netbsd))
;; Since getprotobyname is not thread-safe, we need a lock.
(sb-ext:defglobal **getprotoby-lock** (sb-thread:make-mutex :name "getprotoby lock"))

;;; getprotobyname only works in the internet domain, which is why this
;;; is here
#-android
(defun getprotobyname (name)
  ;; Brownie Points.  Hopefully there's one person out there using
  ;; RSPF sockets and SBCL who will appreciate the extra info
  (labels ((protoent-to-values (protoent)
             (values
              (sockint::protoent-proto protoent)
              (sockint::protoent-name protoent)
              (let ((index 0))
                (loop
                  for alias = (sb-alien:deref
                               (sockint::protoent-aliases protoent) index)
                  while (not (sb-alien:null-alien alias))
                  do (incf index)
                  collect (sb-alien::c-string-to-string
                           (sb-alien:alien-sap alias)
                           (sb-impl::default-external-format)
                           'character))))))
    #+(and sb-thread os-provides-getprotoby-r (not netbsd))
    (let ((buffer-length 1024)
          (max-buffer 10000)
          (result-buf nil)
          (buffer nil)
          #-solaris
          (result nil))
      (declare (type fixnum buffer-length)
               (type fixnum max-buffer))
      (loop
        (unwind-protect
             (progn
               (setf result-buf (sb-alien:make-alien sockint::protoent)
                     buffer (sb-alien:make-alien sb-alien:char buffer-length))
               #-solaris
               (setf result (sb-alien:make-alien (* sockint::protoent)))
               (when (or (sb-alien:null-alien result-buf)
                         (sb-alien:null-alien buffer)
                         (sb-alien:null-alien result))
                 (error "Could not allocate foreign memory."))
               (let ((res (sockint::getprotobyname-r
                           name result-buf buffer buffer-length #-solaris result)))
                 (cond ((eql res 0)
                        #-solaris
                        (when (sb-alien:null-alien (sb-alien:deref result 0))
                          (error 'unknown-protocol :name name))
                        (return-from getprotobyname
                          (protoent-to-values result-buf)))
                       (t
                        (let ((errno (sb-alien:get-errno)))
                          (cond ((eql errno sb-unix:enoent)
                                 ;; Usually caused by missing /etc/protocols
                                 (error 'unknown-protocol :name name))
                                ((eql errno sockint::erange)
                                 (incf buffer-length 1024)
                                 (when (> buffer-length max-buffer)
                                   (error "Exceeded max-buffer of ~d" max-buffer)))
                                (t
                                 (error "Unexpected errno ~d" errno))))))))
          (when result-buf
            (sb-alien:free-alien result-buf))
          (when buffer
            (sb-alien:free-alien buffer))
          #-solaris
          (when result
            (sb-alien:free-alien result)))))
    #+(or (not sb-thread) (not os-provides-getprotoby-r) netbsd)
    (tagbody
       (flet ((get-it ()
                (let ((ent (sockint::getprotobyname name)))
                  (if (sb-alien::null-alien ent)
                      (go :error)
                      (return-from getprotobyname (protoent-to-values ent))))))
         #+(and sb-thread (not netbsd))
         (sb-thread::with-system-mutex (**getprotoby-lock**)
           (get-it))
         #+(or (not sb-thread) netbsd)
         (get-it))
     :error
       (error 'unknown-protocol :name name))))
