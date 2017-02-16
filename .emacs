;; .emacs --- .emacs file by Florian Biermann <fbie@itu.dk>

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

;;; Code:

;; I don't want this, but Emacs keeps setting it.  I'll put it at the
;; top to allow centered-window-mode to overwrite its value.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "White")))))

;; General settings.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq-default require-final-newline t)
(setq-default indent-tabs-mode nil)
(global-unset-key (kbd "C-z")) ;; Don't minimize.
(global-unset-key (kbd "C-x C-b")) ;; Don't show buffer overview.
(global-unset-key (kbd "C-x C-l")) ;; Don't use downcase-region.
(global-unset-key (kbd "C-x C-u")) ;; Don't use upcase-region.
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-hl-line-mode)
(show-paren-mode)
(global-eldoc-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq confirm-kill-emacs #'yes-or-no-p)

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
(setq display-time-24hr-format 't)
(setq display-time-day-and-date 't)

(defun external-screen? ()
  "Non-nil if Emacs is running on an external screen."
  (not (string-equal "eDP1" (cdr (assoc 'name (car (display-monitor-attributes-list)))))))

(defun configure-splitting ()
  "Configure frame splitting preferences based on screen setup."
  (interactive)
  (setq split-height-threshold nil)
  (if (external-screen?)
      (setq split-width-threshold 110)
    (setq split-width-threshold nil)))

(defun dynamic-split-window-sensibly (&optional window)
  "Figure out splitting configuration and then call 'split-window-sensibly' with WINDOW."
  (configure-splitting)
  (split-window-sensibly window))

(setq split-window-preferred-function 'dynamic-split-window-sensibly)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<up>") 'enlarge-window)

;; If Emacs is not started from shell, e.g. on Mac OSX, this fixes the
;; PATH environment variable. Important for running external programs,
;; such as latex.
(when (system-osx?)
  (let ((path-from-shell
	 (replace-regexp-in-string "[\t\n]*$" ""
				   (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; Set-up Okular to view PDFs from Latex mode.
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (when (system-linux?)
	      (push '("%(masterdir)" (lambda nil (file-truename (TeX-master-directory))))
		    TeX-expand-list)
	      (push '("Okular" "okular --unique %o#src:%n%(masterdir)./%b")
		    TeX-view-program-list)
	      (push '(output-pdf "Okular") TeX-view-program-selection))))

;; Set-up MELPA.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Install use-package if needed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

;; Install auctex. use-package does not handle this well.
(unless (package-installed-p 'auctex)
  (package-install 'auctex))

(use-package diminish)

(use-package flycheck
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
  :init
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package writegood-mode
  :diminish writegood-mode
  :init
  (add-hook 'LaTeX-mode-hook 'writegood-mode))

;; This package shows undo operations as a tree and allows for
;; easy-peasy navigation in the undo history. Toggle with C-x u.
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff t))

(use-package helm
  :diminish helm-mode
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("C-x C-g" . helm-recentf)
  ("C-c k" . helm-show-kill-ring)
  :init
  (helm-mode 'true)
  (helm-autoresize-mode 'true)
  (add-hook 'LaTeX-mode-hook 'helm-mode))

(use-package imenu-anywhere
  :after helm
  :bind
  ("C-c C-i" . helm-imenu-anywhere))

(use-package magit
  :bind
  ("C-c i" . magit-status))

(use-package company
  :diminish company-mode
  :init
  (global-company-mode))

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
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode))

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
  :init
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  :config
  (setq omnisharp-company-template-use-yasnippet nil)
  (setq omnisharp-server-executable-path "~/src/omnisharp-server/OmniSharp/bin/Release/OmniSharp.exe")
  (add-to-list 'company-backends 'company-omnisharp))

(use-package paren-face
  :init (global-paren-face-mode))

(use-package racket-mode
  :after paredit
  :bind
  (:map racket-mode-map ("C-h f" . racket-describe))
  (:map racket-repl-mode-map ("C-h f" . racket-describe))
  :init
  (add-hook 'racket-mode-hook #'enable-paredit-mode)
  (add-hook 'racket-repl-mode-hook #'enable-paredit-mode)
  :config
  (setq racket-paren-face '(t (:inherit "shadow"))))

(use-package fsharp-mode
  :config
  (setq inferior-fsharp-program
	(string-join (list inferior-fsharp-program " --mlcompatibility -g -d:TRACE -d:DEBUG"))))

(use-package centered-window-mode
  :if window-system
  :init
  (centered-window-mode t)
  (add-hook 'text-mode-hook 'turn-on-visual-line-mode))

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
  :if (not (system-win?))
  :bind
  ("s-<pause>" . spotify-playpause)) ;; Works on Kinesis Advantage.

(require 'org)
(setq org-log-done t) ;; Log completion of tasks.
(setq org-pretty-entities t)
(setq org-support-shift-select t)

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;; I like indented headers very much.
(add-hook 'org-mode-hook 'org-indent-mode)

(defun org-agenda-and-todos ()
  "Display org agenda and todo list.  Equal to <M-x> org-agenda <RET> n."
  (interactive)
  (org-agenda nil "n"))

(setq org-directory "~/ownCloud/org/")

;; Start emacs in agenda view.
(when (and (directory-files org-directory) (window-system))
  (add-to-list 'org-agenda-files org-directory)
  (setq initial-buffer-choice (lambda ()
				(save-window-excursion
				  (org-agenda-and-todos)
				  (get-buffer "*Org Agenda*")))))

(global-set-key (kbd "C-c a") 'org-agenda-and-todos)

(use-package org-journal
  :config
  (setq org-journal-file-format "%Y%m%d.org")
  (setq org-journal-dir (concat org-directory "journal"))
  (add-to-list 'org-agenda-files org-journal-dir))

;; (use-package professional-theme
;;   :init
;;   (load-theme 'professional t))

(use-package railscasts-reloaded-theme
  :init
  (load-theme 'railscasts-reloaded t)
  ;; We don't like the large headings.
  (require 'org)
  (dolist (level org-level-faces)
    (set-face-attribute level nil :height (face-attribute 'default :height))))

(use-package gnuplot-mode
  :mode "\\.gnuplot\\'")

(use-package markdown-mode
  :mode "\\.md\\'")

;; Includes ace-jump-mode and helm-swoop. Both are awesome!
(use-package ace-isearch
  :diminish ace-isearch-mode
  :init
  (global-ace-isearch-mode 1)
  (setq ace-isearch-input-length 2))

(use-package ssh-config-mode)

;; TODO: Re-enable ligatures.
(when (window-system)
  (set-frame-font "Fira Code Retina 11"))

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

(add-hook 'after-init-hook
	  (lambda () (when (window-system)
		       ;; Always run graphical Emacs in fullscreen.
		       (toggle-frame-fullscreen)
		       ;; Graphical Emacs instances may run as server.
		       (unless (and (fboundp 'server-running-p) (server-running-p))
			 (server-start)))))

