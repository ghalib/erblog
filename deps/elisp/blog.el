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


(defun test-html (body)
  (erl-spawn
    (erl-send-rpc gms-db-nodename 'blog_view 'test_html (list body))
    (erl-receive ()
	((['rex value]
	  (print value))
	 (['rex ['badrpc reason]]
	  (message "RPC failed: %S"
		   reason))))))

(defun gms-rpc (mod fun args)
  (erl-spawn
    (erl-send-rpc gms-db-nodename mod fun args)
    (erl-receive ()
	((['rex value]
	  (print value))
	 (['rex ['badrpc reason]]
	  (message "RPC failed: %S"
		   reason))))))

(defun delete-blogpost (permalink)
  (gms-rpc 'blog_db 'delete_blogpost (list permalink)))

(defun gms-publish-post (title body)
  (gms-rpc 'blog_db 'add_blogpost (list title body)))


;;; testing

(delete-blogpost "title2383")

(read "(hello
	what
	are
	you)")

(test-html '("r u emacs" [a ([href http://www.google.com]) nil]
	     [pre nil
		  ("here is some code")]))

(gms-publish-post "hello from emacs"
		  '("r u emacs"
		    [a ([href http://www.google.com]) nil]
		    ""
		    [pre nil
			 ("here is some code
followed by
more
code")]))