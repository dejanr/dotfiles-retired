(defmacro replace-hook (hook fn)
  `(remove-hook ,hook ,fn)
  `(add-hook ,hook ,fn))

(replace-hook *key-press-hook* 'show-key-seq)

(defun show-key-seq (key seq val)
  (message (print-key-seq (reverse seq))))
