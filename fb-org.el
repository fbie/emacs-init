(use-package org
  :straight (:type built-in)
  :config
  (setq org-log-done t ;; Log completion of tasks.
        org-use-sub-superscripts "{}"
        org-pretty-entities t
        org-support-shift-select t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-agenda-sticky t
        org-startup-folded t
        org-inhibit-startup nil

        ;; From Sacha Chua's config.
        org-agenda-use-tag-inheritance t
        org-agenda-skip-scheduled-if-done nil
        org-agenda-skip-deadline-if-done nil
        org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled

        org-agenda-skip-scheduled-if-deadline-is-shown t

        org-startup-with-inline-images t

        org-agenda-window-setup 'other-window

        org-todo-keywords '((sequence "TODO(t!)"
                                      "INPROGRESS(i!)"
                                      "INREVIEW(r!)"
                                      "|"
                                      "DONE(d!)"
                                      "DELEGATED(l!)"
                                      "ABANDONED(a!)"))
        org-todo-keyword-faces '(("TODO"       . (:foreground "red"         :weight bold))
                                 ("INPROGRESS" . (:foreground "orange"      :weight bold))
                                 ("INREVIEW"   . (:foreground "royal blue"  :weight bold))
                                 ("DONE"       . (:foreground "forestgreen" :weight bold))
                                 ("DELEGATED"  . (:foreground "forestgreen" :weight bold))
                                 ("ABANDONED"  . (:foreground "dim gray"    :weight bold))))

  ;; I like indented headers very much.
  (require 'org-indent)
  (add-hook 'org-mode-hook 'org-indent-mode)

  (defun org-agenda-and-todos (&optional arg)
    "Display org agenda and todo list.  Equal to <M-x> org-agenda <RET> n."
    (interactive)
    (org-agenda arg "n"))
  (global-set-key (kbd "C-c a") 'org-agenda-and-todos)
  (setq org-directory "~/org/")
  (add-to-list 'org-agenda-files org-directory))

(use-package org-journal
  :after org
  :bind
  (:map org-journal-mode-map
        ("C-c s" . org-journal-search)
        ("C-c C-s" . org-schedule))
  :config
  (setq org-journal-carryover-items nil)
  :init
  (setq org-journal-file-format "%Y%m%d.org")
  (setq org-journal-dir (concat org-directory "journal/"))
  (add-to-list 'org-agenda-files org-journal-dir))


(use-package org-present
  :disabled
  :after org
  :bind
  (:map org-present-mode-keymap
        ("q" . org-present-quit)
        ("<prior>" . org-present-prev)
        ("<next>" . org-present-next))
  :config
  (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)
                 (setq mode-line-format nil)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write)
              (setq mode-line-format (default-value 'mode-line-format)))))

(provide 'fb-org)
