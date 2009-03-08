(setq gms-db-nodename erl-nodename-cache)

(defun gms-trim (str)
     "Trim whitespace from both sides of string. Taken from emacswiki."
     (let ((s (if (symbolp str)(symbol-name str) str)))
        (save-excursion
          (while (and
	          (not (null (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
	          (> (length s) (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
	    (setq s (replace-match "" t nil s)))
          (while (and
	          (not (null (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
	          (> (length s) (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
	    (setq s (replace-match "" t nil s))))
        s))

(defun gms-get-paragraph (para-start)
  (save-excursion
    (let (para-end)
      (forward-paragraph)
      (setq para-end (point))
      (gms-trim (buffer-substring-no-properties para-start para-end)))))


(defun gms-rpc (mod fun args)
  (erl-spawn
    (erl-send-rpc gms-db-nodename mod fun args)
    (erl-receive ()
	((['rex value]
	  (print value))
	 (['rex ['badrpc reason]]
	  (message "RPC failed: %S"
		   reason))))))

(defun gms-delete-post (permalink)
  (gms-rpc 'blog_db 'delete_blogpost (list permalink)))

(defun gms-publish-post ()
  (interactive)
  (let ((blogpost (assemble-post)))
    (let ((title (car blogpost))
	  (body (cdr blogpost)))
      (gms-rpc 'blog_db 'add_blogpost (list title body)))))

(defun gms-test-html ()
  (interactive)
  (let ((blogpost (assemble-post)))
    (let ((body (cdr blogpost)))
      (gms-rpc 'blog_view 'test_html (list body)))))


;;; HTML-generating functions. HTML being mochiweb-html, in Distel
;;; form. i.e. Elisp vector <-> Erlang tuple, and Elisp list <->
;;; Erlang list.

(defun deftitle (title)
  title)

(defun deflink (dest &optional body)
  "Generate link tag"
  `[a ([href ,(intern (concat "http://" dest))]) ,body])

(defun defpara (&rest body)
  `[p nil ,body])

(defun defcode (code)
  "All code is in a PRE tag with class CODE."
  `[pre ([class code]) ,code])

(defun br ()
  `[br nil nil])

(defun assemble-post ()
  (eval (read (concat "(list " (buffer-substring-no-properties (point-min)
							       (point-max))
		      ")"))))

(defun get-title (blogpost)
  (car blogpost))

(defun get-body (blogpost)
  (cdr blogpost))



