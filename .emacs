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

;; Require this on top thanks to safe themes.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(package-selected-packages
   (quote
    (erlang railscasts-reloaded-theme visual-fill-column auto-package-update merlin tuareg writegood-mode use-package undo-tree spotify smart-mode-line rainbow-delimiters racket-mode professional-theme paren-face paredit org-present org-journal omnisharp multiple-cursors markdown-mode magit helm-swoop helm-projectile haskell-mode gnuplot-mode fsharp-mode exec-path-from-shell ess delight centered-window-mode auctex ace-jump-mode ace-isearch))))

;;; Code:

(setq user-full-name "Florian Biermann"
      user-mail-address "fbie@itu.dk")

(setq backup-directory-alist `(("." . "~/.emacs.d/backups"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      load-prefer-newer t)

;; Remove useless visual stuff.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen 1)

(setq-default require-final-newline t
              indent-tabs-mode nil
              tab-width 4)

;; Remove useless, annoying key-bindings.
(global-unset-key (kbd "C-z")) ;; Don't minimize.
(global-unset-key (kbd "C-x C-b")) ;; Don't show buffer overview.
(global-unset-key (kbd "C-x C-l")) ;; Don't use downcase-region.
(global-unset-key (kbd "C-x C-u")) ;; Don't use upcase-region.

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


(defun system-win? ()
  "True if current system is Windows."
  (or (eq system-type 'windows-nt)
      (eq system-type 'ms-dos)))


(defun system-osx? ()
  "True if current system is Mac OSX."
  (eq system-type 'darwin))

(defun system-linux? ()
  "True if current system is Linux."
  (not (or (system-win?) (system-osx?))))


;; Too annoying to move the mouse to check time when in full screen
(display-time-mode 1)
(setq display-time-24hr-format 't
      display-time-day-and-date 't)


(defun external-screen? ()
  "Non-nil if Emacs is running on an external screen, I think."
  (not (string-equal "eDP1" (cdr (assoc 'name (car (display-monitor-attributes-list)))))))


(defun dynamic-split-window-sensibly (&optional window)
  "Figure out splitting configuration and then call 'split-window-sensibly' with WINDOW."
  (if (one-window-p t) ;; Behave ina special way if only one window.
      (if (external-screen?)
          (split-window-right)
        (split-window-below))
    ;; Otherwise, configure splitting and split sensibly away!
    (setq split-height-threshold (if (external-screen?) 0 nil)
          split-width-threshold  (if (external-screen?) nil 0))
    (split-window-sensibly window)))


(setq split-window-preferred-function 'dynamic-split-window-sensibly)
(global-set-key (kbd "C-x C-o") 'window-swap-states)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<up>") 'enlarge-window)


;; Set-up melpa stable and gnu repositories.
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Install use-package if needed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Only now add melpa, to avoid use-package updating itself to some
;; bleeding-edge version that doesn't know about the :pin keyword.
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

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
  :pin "melpa-stable"
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
        org-agenda-show-log t
        org-agenda-skip-scheduled-if-done nil
        org-agenda-skip-deadline-if-done nil
        org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled

        org-agenda-skip-scheduled-if-deadline-is-shown t

        org-startup-with-inline-images t

        org-agenda-window-setup 'other-window)

  ;; I like indented headers very much.
  (add-hook 'org-mode-hook 'org-indent-mode)

  (defun org-agenda-and-todos (&optional arg)
    "Display org agenda and todo list.  Equal to <M-x> org-agenda <RET> n."
    (interactive)
    (org-agenda arg "n"))
  (global-set-key (kbd "C-c a") 'org-agenda-and-todos)

  (setq org-directory "~/ownCloud/org/")
  (add-to-list 'org-agenda-files org-directory))


(use-package flycheck
  :pin "melpa-stable"
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
  :pin "melpa-stable"
  :diminish flyspell-mode
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))


(use-package writegood-mode
  :pin "melpa-stable"
  :diminish writegood-mode
  :config
  (add-hook 'LaTeX-mode-hook 'writegood-mode))


;; This package shows undo operations as a tree and allows for
;; easy-peasy navigation in the undo history. Toggle with C-x u.
(use-package undo-tree
  :pin "melpa"
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff t))


(use-package helm
  :pin melpa-stable
  :diminish helm-mode
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("C-x C-g" . helm-recentf)
  ("C-c k" . helm-show-kill-ring)
  ("C-c TAB" . helm-imenu)
  ("C-x b" . helm-buffers-list)
  ("C-s" . helm-occur)
  :init
  (helm-mode 'true)
  (helm-autoresize-mode 'true)
  (add-hook 'LaTeX-mode-hook 'helm-mode))


(use-package magit
  :pin "melpa-stable"
  :bind
  ("C-c i" . magit-status))


(use-package company
  :pin "melpa-stable"
  :diminish company-mode
  :config
  (global-company-mode))


(use-package helm-projectile
  :pin "melpa-stable"
  :diminish projectile-mode
  :config
  (projectile-mode 1)
  (helm-projectile-on))


(use-package paredit
  :pin "melpa-stable"
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
  :pin "melpa-stable"
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
  (setq omnisharp-server-executable-path "~/.emacs.d/omnisharp-roslyn-mono/omnisharp.sh")
  (add-to-list 'company-backends 'company-omnisharp)
  (add-to-list 'auto-mode-alist '("\\.sln$" . xml-mode))
  (add-to-list 'auto-mode-alist '("\\.csproj$" . xml-mode))
  :init
  (add-hook 'csharp-mode-hook 'omnisharp-mode))


(use-package paren-face
  :pin "melpa-stable"
  :config (global-paren-face-mode))


(use-package racket-mode
  :pin "melpa"
  :after paredit
  :bind
  (:map racket-mode-map ("C-h f" . racket-describe))
  (:map racket-repl-mode-map ("C-h f" . racket-describe))
  :init
  (add-hook 'racket-mode-hook #'enable-paredit-mode)
  (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable)
  (add-hook 'racket-repl-mode-hook #'enable-paredit-mode)
  (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
  :config
  (setq racket-paren-face '(t (:inherit "shadow"))))


(use-package rainbow-delimiters
  :pin "melpa-stable"
  :after racket-mode
  :init
  (add-hook 'racket-mode-hook      'rainbow-delimiters-mode)
  (add-hook 'racket-repl-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook  'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook        'rainbow-delimiters-mode))


(use-package fsharp-mode
  :pin "melpa-stable"
  :config
  (setq fsharp-doc-idle-delay 1.0)
  (setq inferior-fsharp-program
	(string-join (list inferior-fsharp-program " --mlcompatibility -g -d:TRACE -d:DEBUG")))
  (add-to-list 'auto-mode-alist '("\\.fsproj$" . xml-mode)))


(use-package visual-fill-column
  :pin "melpa-stable"
  :init
  (setq visual-fill-column-center-text t
        visual-fill-column-width       110)
  (add-hook 'text-mode-hook 'turn-on-visual-line-mode)
  (add-hook 'text-mode-hook 'visual-fill-column-mode)
  (add-hook 'prog-mode-hook 'visual-fill-column-mode))


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


(use-package spotify
  :pin "melpa-stable"
  :if (not (system-win?))
  :bind
  ("s-<pause>" . spotify-playpause)) ;; Works on Kinesis Advantage.


(use-package org-journal
  :pin "melpa-stable"
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
  :pin "melpa"
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
  :pin "melpa-stable"
  :init
  (setq sml/no-confirm-load-theme t)
  (smart-mode-line-enable))


(use-package professional-theme
  :pin "melpa"
  :after smart-mode-line
  :config
  (setq sml/theme 'light)
  (sml/setup)
  :init
  (load-theme 'professional t))


(use-package railscasts-reloaded-theme
  :disabled
  :pin "melpa-stable"
  :after smart-mode-line centered-window-mode
  :config
  ;; A bit of grim reverse engineering to get rid of large header
  ;; lines in org-mode while retaining scaling.
  (custom-theme-set-faces 'railscasts-reloaded
                          `(org-level-1 ((t (:foreground "#CC7733"))) t)
                          `(org-level-2 ((t (:foreground "#FFC66D"))) t))
  (setq sml/theme 'dark)
  (sml/setup)
  :init
  (load-theme 'railscasts-reloaded t))


(use-package gnuplot-mode
  :pin "melpa"
  :mode "\\.gnuplot\\'")


(use-package markdown-mode
  :pin "melpa-stable"
  :mode "\\.md\\'"
  :bind
  (:map markdown-mode-map
        ("M-<right>" . markdown-demote)
        ("M-<left>"  . markdown-promote)))


(use-package ace-jump-mode
  :pin "melpa-stable"
  :bind
  ("C-S-s" . ace-jump-mode))

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


(use-package ess
  :pin "melpa-stable"
  :bind
  (:map org-mode-map
        ("C-c r" . insert-R-code-block))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (cons '(R . t) org-babel-load-languages))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (setq org-confirm-babel-evaluate nil)

  (defun insert-R-code-block ()
    "Insert a new R code block at point."
    (interactive)
    (insert "#+begin_src R :session :exports both :results value")
    (newline)
    (insert "#+end_src")
    (newline)))


(use-package merlin
  :pin "melpa-stable"
  :diminish
  :preface (defconst merlin-path "~/.opam/system/bin/ocamlmerlin")
  :if (file-exists-p merlin-path)
  :config
  (setq merlin-command merlin-path))


(use-package tuareg
  :pin "melpa-stable"
  :demand
  :after merlin
  :config
  (setq tuareg-indent-align-with-first-arg 't)
  (when (package-installed-p 'merlin)
    (add-hook 'tuareg-mode-hook 'merlin-mode)))

;; For testing my great Analog Emacs mode before putting it on MELPA.
(use-package delight)
(require 'analog "/home/fbie/src/analog-indicator/analog.el")
(analog-indicator-mode)

;; Since auto-update does not work, I use this homegrown package
;; update function.
(defun update-all-packages ()
  "Update all packages."
  (interactive)
  (save-excursion
    (save-window-excursion
      (list-packages)
      (message (package-menu-mark-upgrades))
      (package-menu-execute 'no-query))))

(provide 'emacs)
;;; .emacs ends here
