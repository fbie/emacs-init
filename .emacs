;; .emacs --- .emacs file by Florian Biermann <fbie@itu.dk> -*- lexical-binding: t; -*-

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; What a great .emacs file!

;; Partially stolen from
;; - Harry Schwartz (https://github.com/hrs/dotfiles) and
;; - Michael Walker (https://github.com/barrucadu/dotfiles)
;; - Sacha Chua (http://pages.sachachua.com/.emacs.d)

;;; Code:
(setq user-full-name "Florian Biermann"
      user-mail-address "florian.biermann@protonmail.com")

(setq backup-directory-alist `(("." . "~/.emacs.d/backups"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      load-prefer-newer t
      w32-get-true-file-attributes nil)

;; Remove useless visual stuff.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen 1)

(setq-default require-final-newline t
              indent-tabs-mode nil
              tab-width 4)

;; Remove useless, annoying key-bindings.
(let ((bindings
       (list (kbd "C-z")        ;; Don't minimize.
             (kbd "C-x C-b")    ;; Don't show buffer overview.
             (kbd "C-x C-l")    ;; Don't use downcase-region.
             (kbd "C-x C-u")))) ;; Don't use upcase-region.
  (dolist (binding bindings)
    (global-unset-key binding)))

;; Revert automatically; this saves me a few key strokes on revert.
(global-auto-revert-mode t)

;; This is easier for my fingers:
(global-set-key (kbd "C-=") 'set-mark-command)

;; Why would you ever want them?
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Standard usability stuff.
(global-hl-line-mode)
(show-paren-mode)
(global-eldoc-mode)
(global-subword-mode 1)
(column-number-mode 1)
(electric-pair-mode)

;; Never quit Emacs!
(setq confirm-kill-emacs 'yes-or-no-p)

;; But increase laziness.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Kill, kill, kill!
(setq kill-whole-line t)


(defun smart-back-to-indentation ()
  "Jump between the beginning of line and the line's first
character."
  (interactive)
  (let ((old-pos (point)))
    (move-beginning-of-line nil)
    (when (= old-pos (point))
      (back-to-indentation))))

(global-set-key (kbd "C-a") 'smart-back-to-indentation)

(defun smart-make-frame ()
  "Open current buffer in a new frame if there is more than one window open."
  (interactive)
  (let ((frame (selected-frame))
        (buffer (current-buffer)))
    (when (not (one-window-p frame))
      (make-frame)
      (delete-windows-on buffer frame))))

(global-set-key (kbd "C-x w") 'smart-make-frame)

;; Too annoying to move the mouse to check time when in full screen
(display-time-mode 0)
(setq display-time-24hr-format 't
      display-time-day-and-date 't)

;; Otherwise, configure splitting and split sensibly.
(setq split-width-threshold  nil)

(defun dynamic-split-window-sensibly (&optional window)
  "Figure out splitting configuration and then call 'split-window-sensibly' with WINDOW."
  (if (and (one-window-p t) (> (frame-width) 110))
      ;; If only one window is present, split it vertically when using
      ;; an external screen, otherwise split horizontally.
      (split-window-right)
    (split-window-sensibly window)))


(setq split-window-preferred-function 'dynamic-split-window-sensibly)
(global-set-key (kbd "C-x C-o") 'window-swap-states)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<up>") 'enlarge-window)


;; Set-up melpa stable and gnu repositories.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Install use-package if needed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Why wouldn't you?
(setq use-package-always-ensure t)
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))


;; Install auctex. use-package does not handle this well.
(unless (package-installed-p 'auctex)
  (package-install 'auctex))


;; Allow imenu bindings also in LaTeX mode.
(add-hook 'LaTeX-mode-hook (lambda () (local-unset-key (kbd "C-c TAB"))))
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'prettify-symbols-mode)
(setq prettify-symbols-unprettify-at-point t)


(use-package diminish)


(use-package multiple-cursors
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this))

(use-package org
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
  (add-hook 'org-mode-hook 'org-indent-mode)

  (defun org-agenda-and-todos (&optional arg)
    "Display org agenda and todo list.  Equal to <M-x> org-agenda <RET> n."
    (interactive)
    (org-agenda arg "n"))
  (global-set-key (kbd "C-c a") 'org-agenda-and-todos)

  ;; TODO: My ITU ownCloud account will be closed soon; alternatives?
  (setq org-directory "~/ownCloud/org/")
  (add-to-list 'org-agenda-files org-directory))


(use-package flycheck
  :disabled
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (add-hook 'LaTeX-mode-hook 'flycheck-mode)
  (add-hook 'latex-mode-hook 'flycheck-mode)
  :config
  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message (one-or-more not-newline)
                       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
              line-end))
    :modes (latex-mode LaTeX-mode text-mode markdown-mode gfm-mode))
  (add-to-list 'flycheck-checkers 'proselint))


(use-package flyspell
  :diminish flyspell-mode
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))


(use-package writegood-mode
  :diminish writegood-mode
  :config
  (add-hook 'LaTeX-mode-hook 'writegood-mode))

;; This package shows undo operations as a tree and allows for
;; easy-peasy navigation in the undo history. Toggle with C-x u.
;; (use-package undo-tree
;;   :diminish undo-tree-mode
;;   :config
;;   (global-undo-tree-mode)
;;   (setq undo-tree-visualizer-diff t))


(use-package helm
  :diminish helm-mode
  :bind
  ("M-x"     . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("C-x C-g" . helm-recentf)
  ("C-c k"   . helm-show-kill-ring)
  ("C-c TAB" . helm-imenu)
  ("C-x b"   . helm-buffers-list)
  ("C-s"     . helm-occur)
  ("C-c s"   . isearch-forward)
  :init
  (setq helm-ff-skip-boring-files t
        helm-occur-show-buffer-name t
        helm-moccur-show-buffer-fontification t
        helm-buffer-max-length nil)
  (helm-mode 'true)
  (helm-autoresize-mode 'true)
  ; I cannot recall why I needed the line below?
  (add-hook 'LaTeX-mode-hook 'helm-mode))


(use-package magit
  :bind
  ("C-c i" . magit-status)
  :config
  (setq vc-handled-backends nil
        magit-git-executable "/usr/bin/git"))

(use-package gitconfig)

(use-package company
  :diminish company-mode
  :config
  (setq company-idle-delay 1.5)
  (global-company-mode))


(use-package helm-projectile
  :diminish projectile-mode
  :bind ("C-c p" . projectile-command-map)
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm
        projectile-enable-caching t
        projectile-indexing-method 'turbo-alien)
  (helm-projectile-on))

(use-package helm-git-grep
  :after helm
  :bind ("C-c g" . helm-git-grep-at-point))

(use-package paredit
  :diminish paredit-mode
  :bind
  (:map paredit-mode-map
	("M-<left>" . paredit-forward-barf-sexp)
	("M-<right>" . paredit-forward-slurp-sexp)
	("C-<left>" . left-word)
	("C-<right>" . right-word)
	("{" . paredit-open-curly)
	("}" . paredit-close-curly))
  :init
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook 'enable-paredit-mode))


(use-package omnisharp
  :after helm
  :bind
  (:map omnisharp-mode-map
	("C-SPC" . company-search-candidates)
	("M-." . omnisharp-go-to-definition)
	("M-," . pop-tag-mark)
	("C-u" . omnisharp-helm-find-usages)
	("S-s-<up>" . omnisharp-navigate-up)
	("S-s-<down>" . omnisharp-navigate-down)
	("s-i" . omnisharp-helm-find-implementations)
	("C-." . omnisharp-run-code-action-refactoring)
	("<f2>" . omnisharp-rename-interactively)
	("<f5>" . omnisharp-build-in-emacs))
  :config
  (setq omnisharp-company-template-use-yasnippet nil)
  (setq omnisharp-server-executable-path "~/.emacs.d/.cache/omnisharp/server/v1.26.3/run")
  (add-to-list 'company-backends 'company-omnisharp)
  (add-to-list 'auto-mode-alist '("\\.csproj$" . xml-mode))
  (add-to-list 'auto-mode-alist '("\\.cake?$" . csharp-mode))
  :init
  (add-hook 'csharp-mode-hook 'omnisharp-mode))


(use-package paren-face
  :config (global-paren-face-mode))


(use-package racket-mode
  :after paredit
  :bind
  (:map racket-mode-map ("C-h f" . racket-describe))
  (:map racket-repl-mode-map ("C-h f" . racket-describe))
  :init
  (add-hook 'racket-mode-hook      'enable-paredit-mode)
  (add-hook 'racket-mode-hook      'racket-unicode-input-method-enable)
  (add-hook 'racket-repl-mode-hook 'enable-paredit-mode)
  (add-hook 'racket-repl-mode-hook 'racket-unicode-input-method-enable)
  (add-hook 'racket-mode-hook 'visual-fill-column-mode)
  :config
  (setq racket-paren-face '(t (:inherit "shadow"))))


(use-package rainbow-delimiters
  :after racket-mode
  :init
  (add-hook 'racket-mode-hook      'rainbow-delimiters-mode)
  (add-hook 'racket-repl-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook  'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook        'rainbow-delimiters-mode))


(use-package fsharp-mode
  :config
  (setq fsharp-doc-idle-delay 1.0)
  (setq inferior-fsharp-program
	(string-join (list inferior-fsharp-program " --mlcompatibility -g -d:TRACE -d:DEBUG")))
  (add-to-list 'auto-mode-alist '("\\.fsproj$" . xml-mode)))


(use-package visual-fill-column
  :init
  (setq visual-fill-column-center-text t
        visual-fill-column-width       110)
  (add-hook 'text-mode-hook 'turn-on-visual-line-mode)
  (add-hook 'text-mode-hook 'visual-fill-column-mode)
  (add-hook 'emacs-lisp-mode-hook 'visual-fill-column-mode))


;; Downloading bibliography from CiteULike
(defcustom citeulike-user
  "fbie"
  "The CiteULike user to download bibliography from.")


(defconst citeulike-url
  "http://www.citeulike.org/bibtex/user/%s?key_type=4&clean_urls=0&incl_amazon=0&smart_wrap=1"
  "The URL to fetch from to get a .bib file with generated citation keys.")


(defun citeulike-download-bibliography ()
  "Retrieve the CiteULike bibliography of a user and store it as bibtex-file."
  (interactive)
  (let*
      ((current-path
	(if buffer-file-name
	    (file-name-directory buffer-file-name)
	  "~"))
       (citeulike-url-usr (format citeulike-url citeulike-user))
       (default-path (concat current-path citeulike-user ".bib"))
       (file-path (read-file-name "Write to: " nil nil nil default-path nil)))
    (url-copy-file citeulike-url-usr file-path 'true)))


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


(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t)
  (smart-mode-line-enable))

(use-package color-theme-sanityinc-tomorrow
  :after smart-mode-line
  :config
  (setq sml/theme 'light)
  (sml/setup)
  :init
  (color-theme-sanityinc-tomorrow-day)
  (set-face-font 'default "Fira Code Retina-11"))


(use-package gnuplot-mode
  :mode "\\.gnuplot\\'")


(use-package markdown-mode
  :mode "\\.md\\'"
  :bind
  (:map markdown-mode-map
        ("M-<right>" . markdown-demote)
        ("M-<left>"  . markdown-promote)))


(use-package ace-jump-mode
  :bind
  ("C-S-s" . ace-jump-mode))


(use-package ace-window
  :bind
  ("C-x o" . ace-window)
  ("C-x C-o" . ace-swap-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


(defun duplicate-line-at-point ()
  "Duplicate the line at point."
  (interactive)
  (save-excursion
    (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
      (move-end-of-line nil)
      (newline)
      (insert line))))

(global-unset-key (kbd  "C-d"))
(global-set-key (kbd "C-c d") 'duplicate-line-at-point)


(defun find-alternate-file-keep-point ()
  "Like 'find-alternate-file' but restore point.

I would like to implement this using 'save-excursion', but
apparently, that does not work."
  (interactive)
  ;;  Hence let-binding.
  (let ((current (point)))
    (call-interactively 'find-alternate-file)
    (goto-char current)))

(global-set-key (kbd "C-x C-v") 'find-alternate-file-keep-point)


(use-package merlin
  :diminish
  :preface (defconst merlin-path "~/.opam/system/bin/ocamlmerlin")
  :if (file-exists-p merlin-path)
  :bind
  (:map merlin-mode-map
        ("M-," . merlin-pop-stack)
        ("M-." . merlin-locate))
  :config
  (setq merlin-command merlin-path
        merlin-completion-with-doc nil
        merlin-completion-dwim nil)
  (defun helm-merlin-occurrences ()
    (interactive)
    (helm :sources '(merlin-occurrences))))

(use-package tuareg
  :demand
  :bind
  (:map tuareg-mode-map
        ("C-c TAB" . helm-imenu))
  :after merlin
  :config
  (setq tuareg-indent-align-with-first-arg nil
        tuareg-electric-close-vector 't)
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (add-to-list 'helm-boring-file-regexp-list "\\.cmi$")
  (add-to-list 'helm-boring-file-regexp-list "\\.cmt$")
  (add-to-list 'helm-boring-file-regexp-list "\\.cmx$")
  (add-to-list 'helm-boring-file-regexp-list "\\.obj$")
  (add-to-list 'helm-boring-file-regexp-list "\\.annot$"))

(use-package dune)

(use-package merlin-eldoc
  :disabled
  :config
  (setq merlin-eldoc-type-verbosity 'min)
  (add-hook 'tuareg-mode-hook 'merlin-eldoc-setup))

;; For testing my great Analog Emacs mode before putting it on MELPA.
;; (use-package delight)
;; (require 'analog "/home/fbie/src/analog-indicator/analog.el")
;; (analog-indicator-mode)

(use-package transpose-frame
  :bind ("C-x t o" . transpose-frame))

(use-package json-mode
  :mode "\\.\\(json\\|new\\)\\'"
  :config
  (add-hook 'json-mode-hook 'paredit-mode))

(use-package yascroll
  :diminish yascroll-mode
  :config
  (global-yascroll-bar-mode 1))

(provide 'emacs)
;;; .emacs ends here

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
