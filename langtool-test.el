(require 'ert-async)
(require 'langtool)

(defun langtooltest-grammar-check (callback)
  (with-current-buffer (get-buffer-create "langtool-test")
  (should (eq (get-char-property (point) 'face) 'langtool-errline))
  (funcall callback)
))

(ert-deftest-async langtooltest-grammar-check (done-1)
    "Ensure double works are highlighted."
    (with-current-buffer (get-buffer-create "langtool-test")
        (erase-buffer)
	(insert "This is a a test")
	(langtool-check)
	(search-backward "a")
	(message "%s" (get-char-property (point) 'face))
	(run-at-time "1 sec" nil (lambda () (langtooltest-grammar-check done-1)))
    )
)

   

  
