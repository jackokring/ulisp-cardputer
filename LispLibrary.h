const char LispLibrary[] PROGMEM = R"lisplibrary(
(defun object (&optional parent slots)
  (let ((obj (when parent (list (cons 'parent parent)))))
    (loop
     (when (null slots) (return obj))
     (push (cons (first slots) (second slots)) obj)
     (setq slots (cddr slots)))))
(defun value (obj slot)
  (when (symbolp obj) (setq obj (eval obj)))
  (let ((pair (assoc slot obj)))
    (if pair (cdr pair)
           (let ((p (cdr (assoc 'parent obj))))
             (and p (value p slot))))))
(defun update (obj slot value)
  (when (symbolp obj) (setq obj (eval obj)))
  (let ((pair (assoc slot obj)))
    (when pair (setf (cdr pair) value))))

)lisplibrary";
