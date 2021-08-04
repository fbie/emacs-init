(defun fb/smart-back-to-indentation ()
  "Jump between the beginning of line and the line's first
character."
  (interactive)
  (let ((old-pos (point)))
    (move-beginning-of-line nil)
    (when (= old-pos (point))
      (back-to-indentation))))

(defun fb/smart-make-frame ()
  "Open current buffer in a new frame if there is more than one window open."
  (interactive)
  (let ((frame (selected-frame))
        (buffer (current-buffer)))
    (when (not (one-window-p frame))
      (make-frame)
      (delete-windows-on buffer frame))))

(defun fb/duplicate-line-at-point ()
  "Duplicate the line at point."
  (interactive)
  (save-excursion
    (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
      (move-end-of-line nil)
      (newline)
      (insert line))))

(defun fb/find-alternate-file-keep-point ()
  "Like 'find-alternate-file' but restore point.

I would like to implement this using 'save-excursion', but
apparently, that does not work."
  (interactive)
  ;;  Hence let-binding.
  (let ((current (point)))
    (call-interactively 'find-alternate-file)
    (goto-char current)))

(defun fb/update-package (package)
  "Update PACKAGE via a sequence of straight commands."
  (interactive "sPackage: ")
  (straight-fetch-package package)
  (straight-pull-package-and-deps package)
  (straight-rebuild-package package))

(provide 'fb)
