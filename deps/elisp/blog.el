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

(defun gms-publish-post ()
  (interactive)
  (erl-spawn
    (erl-send-rpc gms-db-nodename 'erlang 'now '())
    (erl-receive ()
	((['rex value]
	  (print value))
	 (['rex ['badrpc reason]]
	  (message "RPC failed: %S"
		   reason))))))
