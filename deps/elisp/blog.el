(defun get-paragraph (para-start)
  (save-excursion
    (let (para-end)
      (forward-paragraph)
      (setq para-end (point))
      (buffer-substring-no-properties para-start para-end))))