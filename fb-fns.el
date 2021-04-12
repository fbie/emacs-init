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

(defun fb/dynamic-split-window-sensibly (&optional window)
  "Figure out splitting configuration and then call 'split-window-sensibly' with WINDOW."
  (if (and (one-window-p t) (> (frame-width) 110))
      ;; If only one window is present, split it vertically when using
      ;; an external screen, otherwise split horizontally.
      (split-window-right)
    (split-window-sensibly window)))

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

(provide 'fb-fns)
