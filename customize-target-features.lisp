(lambda (features)
  (flet ((enable (x) (pushnew x features))
         (disable (x) (setf features (remove x features))))
    (enable :ita)
    (enable :sb-fasteval)
    (disable :sb-eval)
    (enable :sb-thread)
    (enable :sb-core-compression)
    ;; Make core file not depend on exact runtime addresses -- allows
    ;; relinking runtime.
    (enable :sb-dynamic-core)
    features))
