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

(defun fb/ends-with (a b)
  "Return t if A ends with B, nil otherwise."
  (let ((la (length a))
        (lb (length b)))
    (and (>= la lb) (string-equal b (substring a (- la lb) la)))))

;;; Now for some fantastic compatibility hacks that are needed thanks
;;; to how straight.el works.  When using straight.el, package
;;; dependencies will always be resolved to the latest version.  That
;;; often messes things up.  Hence, we hack some things to make
;;; problems go away.
(unless (boundp 'show-paren-context-when-offscreen)
  (defconst show-paren-context-when-offscreen nil "Emacs 27 compatibility hack."))

(unless (functionp 'string-replace)
  (defalias 'string-replace 'replace-regexp-in-string))

(defun fb/org-insert-src-block (mode)
  "Open a new source block for MODE."
  (interactive "sMode: ")
  (insert "#+begin_src")
  (when mode
      (insert " ")
      (insert mode))
  (newline)
  (newline)
  (insert "#+end_src")
  (line-move-visual -1))

(provide 'fb)
