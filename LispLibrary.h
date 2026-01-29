const char LispLibrary[] PROGMEM = R"lisplibrary(
; These functions use lisp cons cells from the free pool.
; Spaces here just use a little extra PROGMEM.
; comments also just use PROGMEM and not uLisp RAM from the free pool.

; ULOS Objects
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

; Bignum
(defun $expt (x y)
  (let (($e ($bignum 1))
        ($f ($bignum x)))
    (loop
     (when (zerop y) (return $e))
     (when (oddp y) (setq $e ($* $e $f)))
     (setq $f ($* $f $f) y (ash y -1)))))
(defun $sqrt ($a)
  (let* (($1 ($bignum 1))
         ($high $a)
         ($low ($bignum 0))
         ($mid ($+ ($ash $high -1) $1)))
    (loop
     (unless ($> $high $low) (return))
     (if ($> ($* $mid $mid) $a)
         (setq $high ($- $mid $1))
       (setq $low $mid))
     (setq $mid ($+ ($+ $low ($ash ($- $high $low) -1)) $1)))
    $low))
(defun $gcd (a b)
  (let (temp)
    (loop
     (when ($zerop b) (return a))
     (setq temp b b ($mod a b) a temp))))
(defun $pollard-rho (n)
  (let* (($1 ($bignum 1))
         (x ($bignum 2))
         (y ($bignum 2))
         (d ($bignum 1))
         ($g (lambda (x) ($mod ($+ ($* x x) $1) n))))
    (loop
     (unless ($= d $1) (return))
     (setq x ($g x))
     (setq y ($g ($g y)))
     (setq d ($gcd (if ($> x y) ($- x y) ($- y x)) n)))
    (if ($= d n) nil d)))

; Load Save
(defun load-file (filename)
  (with-sd-card (s filename)
    (loop (let ((r (read s)))
      (unless r (return))  
      (eval r)))))
(defun save-file (filename)
  (with-sd-card (s filename 2)
    (pprintall s)))

; String
(defun concat (&rest args)
  (apply concatenate (cons 'string args)))
(defun rjust (w &optional (s "") (c " "))
  (loop
    (if (>= (length s) w) (return s) (setq s (concat c s)))))
; Float
(defun ^ (n &rest e)
  (loop
    (if e (setq n (expt n (car e)) e (cdr e))
      (return n))))
(defun nanp (x) (/= x x))
(defun rational (x)
  (let* ((s (if (minusp x) -1 1))
         (x (abs x))
         (i (truncate x))
         (f (- x i)))
   (let* ((r f) (k 1) (l 1) (m 0) (j 0) d n)
       (loop
        (when (< (abs (- f (/ j l))) 1e-6) (return))
        (setq r (/ r))
        (setq d (truncate r))
        (setq r (- r d))
        (setq n j) (setq j (+ (* j d) k)) (setq k n)
        (setq n l) (setq l (+ (* l d) m)) (setq m n))
       (cond
        ((zerop j) (* i s))
        ((zerop i) (list '/ (* j s) l))
        ((plusp s) (list '+ i (list '/ j l)))
        (t (list '- (list '+ i (list '/ j l))))))))

; List
(defun reduce (op &rest arg)
  (cond
    ((null arg) (funcall op))
    ((null (cdr arg)) (car arg))
    (t (do (
      (a (car arg) (funcall op (car l) a))
      (l (cdr (reverse arg)) (cdr l)))
    ((null l) a)))))

; Platform
(defun gfx () (write-byte #\SO))
(defun cli () (write-byte #\SI) (write-byte #\Page))
(defun rgb (r g b)
  (logior (ash (logand r #xf8) 8) (ash (logand g #xfc) 3) (ash b -3)))
)lisplibrary";
