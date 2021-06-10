;; .emacs --- .emacs file by Florian Biermann <florian.biermann@protonmail.com> -*- lexical-binding: t; -*-

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
;; - Harry Schwartz (https://github.com/hrs/dotfiles)
;; - Michael Walker (https://github.com/barrucadu/dotfiles)
;; - Sacha Chua (http://pages.sachachua.com/.emacs.d)
;; - Ryan Davis (https://github.com/zenspider/elisp)
;;; Code:

(defvar user-init-dir (file-name-directory
                       (concat "~/" (or (file-symlink-p user-init-file)
                                        user-init-file)))
  "Root directory of emacs.el, after following symlinks, etc.")

(setq custom-file (concat user-init-dir "custom.el"))
(add-to-list 'load-path user-init-dir)
(load-file custom-file)

(setq user-full-name "Florian Biermann"
      user-mail-address "flbm@simcorp.com")

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
       (list (kbd "C-z")              ;; Don't minimize.
             (kbd "C-x C-b")          ;; Don't show buffer overview.
             (kbd "C-x C-l")          ;; Don't use downcase-region.
             (kbd "C-x C-u")          ;; Don't use upcase-region.
             (kbd "<Scroll_Lock>")))) ;; Nope!
  (dolist (binding bindings)
    (global-unset-key binding)))

(global-set-key (kbd "M-C-u") 'upcase-char)

;; Revert automatically; this saves me a few key strokes on revert.
(global-auto-revert-mode t)

;; This is easier for my fingers:
(global-set-key (kbd "C-=") 'set-mark-command)

;; OSX, why?
(when (eq system-type 'darwin)
  (setq ns-right-option-modifier 'meta
        ns-control-modifier 'control
        ns-command-modifier 'none
        ns-left-command-modifier 'super
        ns-right-command-modifier 'control)
  (global-set-key (kbd "<home>") 'beginning-of-line)
  (global-set-key (kbd "<end>") 'end-of-line))

;; Why would you ever want them?
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Standard usability stuff.
(global-hl-line-mode)
(show-paren-mode)
(global-eldoc-mode)
(global-subword-mode 1)
(column-number-mode 1)
(electric-pair-mode)
(setq compilation-scroll-output 'first-error)

;; Never quit Emacs!
(setq confirm-kill-emacs 'yes-or-no-p)

;; But increase laziness.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Kill, kill, kill!
(setq kill-whole-line t)

(require 'fb)
(global-set-key (kbd "C-a") 'fb/smart-back-to-indentation)
(global-set-key (kbd "C-x w") 'fb/smart-make-frame)
(global-set-key (kbd "C-x C-o") 'window-swap-states)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<up>") 'enlarge-window)
(global-unset-key (kbd  "C-d"))
(global-set-key (kbd "C-c d") 'fb/duplicate-line-at-point)
(global-set-key (kbd "C-x C-v") 'fb/find-alternate-file-keep-point)

;; Too annoying to move the mouse to check time when in full screen
(display-time-mode 0)
(setq display-time-24hr-format 't
      display-time-day-and-date 't)

;; Otherwise, configure splitting and split sensibly.
(setq split-width-threshold nil)
(setq split-window-preferred-function 'fb/dynamic-split-window-sensibly)

(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(straight-use-package 'use-package)
(setq straight-use-package-by-default 't)


(use-package tex-site
  :straight auctex)

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
  ;; TODO: Maybe write some Windows-specific code, too?
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))


(use-package writegood-mode
  :diminish writegood-mode
  :config
  (add-hook 'LaTeX-mode-hook 'writegood-mode))

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
        helm-buffer-max-length nil
        helm-grep-default-command "grep -I --color=always -a -d skip %e -n%cH -e %p %f")
  (helm-mode 'true)
  (helm-autoresize-mode 'true))

(use-package which-key
  :diminish
  :init
  (which-key-mode))

(use-package magit
  :bind
  ("C-c i" . magit-status)
  :config
  (setq vc-handled-backends nil
        magit-bind-magit-project-status nil))

(use-package gitconfig)

(use-package company
  :diminish company-mode
  :config
  ;; (setq company-idle-delay 1.5) ;; Re-enable if displaying company stalls Emacs.
  (global-company-mode))

(use-package treemacs
  :bind (:map global-map
              ("C-z"       . treemacs-select-window)
              ;; ("C-x t 1"   . treemacs-delete-other-windows)
              ("C-x t t"   . treemacs)
              ;; ("C-x t B"   . treemacs-bookmark)
              ;; ("C-x t C-t" . treemacs-find-file)
              ;; ("C-x t M-t" . treemacs-find-tag)
              ))

(use-package treemacs-magit
  :after (treemacs magit))

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
  :after (helm company flycheck)
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
  (add-to-list 'company-backends 'company-omnisharp)
  (add-to-list 'auto-mode-alist '("\\.csproj$" . xml-mode))
  (add-to-list 'auto-mode-alist '("\\.cake?$" . csharp-mode))
  :init
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-hook 'omnisharp-mode-hook 'eldoc-mode)
  (add-hook 'omnisharp-mode-hook 'flycheck-mode))


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
  ;; (add-hook 'racket-mode-hook 'visual-fill-column-mode)
  :config
  (setq racket-paren-face '(t (:inherit "shadow"))))


(use-package rainbow-delimiters
  :after racket-mode
  :init
  (add-hook 'racket-mode-hook      'rainbow-delimiters-mode)
  (add-hook 'racket-repl-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook  'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook        'rainbow-delimiters-mode))

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
  (color-theme-sanityinc-tomorrow-day))


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


(use-package transpose-frame
  :bind ("C-x t o" . transpose-frame))

(use-package json-mode
  :mode "\\.json"
  :config
  (add-hook 'json-mode-hook 'paredit-mode))

(use-package yascroll
  :diminish yascroll-mode
  :config
  (global-yascroll-bar-mode 1))

(use-package lsp-java
  :config
  (add-hook 'java-mode-hook 'lsp))

(require 'fb-fsharp)
(require 'fb-org)
(require 'fb-ocaml)
(require 'fb-simcorp)
(provide 'emacs)
;;; .emacs ends here
