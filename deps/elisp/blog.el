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

(defun eb-publish-post (&optional permalink)
  (interactive)
  (let ((blogpost (assemble-post)))
    (let ((title (car blogpost))
	  (body (cdr blogpost)))
      (if permalink
	  (eb-rpc 'blog_db 'add_blogpost (list title body permalink))
	(eb-rpc 'blog_db 'add_blogpost (list title body))))))

(defun eb-delete-post (permalink)
  "Deletes post from server, given said post's permalink."
  (interactive "MEnter permalink to delete: ")
  (eb-rpc 'blog_db 'delete_blogpost (list permalink)))

(defun eb-most-recent-post ()
  (interactive)
  (eb-rpc 'blog_db 'most_recent_blogpost '()))

(defun eb-test-html ()
  "Useful for checking if the Elisp parses into HTML correctly.
Sends the Elisp post to server, and if parses correct, returns
the post in raw HTML text.  Otherwise returns error."
  (interactive)
  (let ((blogpost (assemble-post)))
    (let ((body (cdr blogpost)))
      (eb-rpc 'blog_view 'test_html (list body)))))

;;; HTML-generating functions. HTML being mochiweb-html, in Distel
;;; form. i.e. Elisp vector <-> Erlang tuple, and Elisp list <->
;;; Erlang list.

(defun deftitle (title)
  "Has to appear at the very top of the post.  Otherwise things
  cock up."
  title)

(defun defclas (class &rest body)
  `[div ([class ,class]) ,body])

(defun deflink (dest &rest body)
  "Generate link tag"
  `[a ([href ,dest]) ,body])

(defun defpara (&rest body)
  `[p nil ,body])

(defun ul (&rest items)
  `[ul nil ,items])

(defun li (&rest body)
  `[li nil ,body])

(defun header (type &rest body)
  `[,type nil ,body])

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

(defun defnumlist (&rest items)
  `[ol ([type 1]) ,(mapcar (lambda (item)
				 `[li () ,item]) items)])

(defun inlinecode (code)
  `[code () (,code)])

(defun assemble-post ()
  (eval (read (concat "(list " (buffer-substring-no-properties (point-min)
							       (point-max))
		      ")"))))

(defun get-title (blogpost)
  (car blogpost))

(defun get-body (blogpost)
  (cdr blogpost))
