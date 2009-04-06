(defun eb-get-paragraph (para-start)
  (save-excursion
    (let (para-end)
      (forward-paragraph)
      (setq para-end (point))
      (eb-trim (buffer-substring-no-properties para-start para-end)))))


(defun eb-rpc (mod fun args)
  (erl-spawn
    (erl-send-rpc erl-nodename-cache mod fun args)
    (erl-receive ()
	((['rex value]
	  (print value))
	 (['rex ['badrpc reason]]
	  (message "RPC failed: %S"
		   reason))))))

(defun eb-delete-post (permalink)
  (eb-rpc 'blog_db 'delete_blogpost (list permalink)))

(defun eb-publish-post ()
  (interactive)
  (let ((blogpost (assemble-post)))
    (let ((title (car blogpost))
	  (body (cdr blogpost)))
      (eb-rpc 'blog_db 'add_blogpost (list title body)))))

(defun eb-test-html ()
  (interactive)
  (let ((blogpost (assemble-post)))
    (let ((body (cdr blogpost)))
      (eb-rpc 'blog_view 'test_html (list body)))))


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

(defun defimg (imgsrc &optional class)
  `[img ([src ,imgsrc] [class ,class]) ()])

(defun center (elem)
  `[center () (,elem)])

(defun defeq (imgsrc)
  (defpara (center (defimg imgsrc 'equation))))

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
