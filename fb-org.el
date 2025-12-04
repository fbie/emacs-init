(use-package org
  :straight (:type built-in)
  :bind
  (("C-c a" . org-agenda-and-todos)
   ("C-c c" . org-capture))
  (:map org-mode-map ("M-<return>" . org-insert-heading-respect-content))
  :demand
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
        org-agenda-start-on-weekday nil
        org-agenda-start-day "-3d"
        org-agenda-span 17

        org-todo-keywords '((sequence "TODO(t!)"
                                      "INPROGRESS(p!)"
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
  (add-hook 'org-mode-hook 'auto-fill-mode)

  (defun org-agenda-and-todos (&optional arg)
    "Display org agenda and todo list.  Equal to <M-x> org-agenda <RET> n."
    (interactive)
    (org-agenda arg "n"))
  (global-set-key (kbd "C-c a") 'org-agenda-and-todos)
  (setq org-directory (if (eq system-type 'windows-nt)  "~/../../Documents/org/" "~/org/"))
  (setq org-default-notes-file (concat org-directory "todo.org")
        org-refile-targets '((nil . (:maxlevel . 2))))
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
  (setq org-journal-file-format "%Y%m%d.org"
        org-journal-dir (concat org-directory "journal/")
        org-journal-file-type 'monthly)
  (add-to-list 'org-agenda-files org-journal-dir))


(use-package org-present
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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)))

(setq org-babel-default-header-args:sql
      '((:engine . "mssql") (:dbinstance "(localdb)\\MSSQLLocalDB")))

(provide 'fb-org)
