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
(scroll-bar-mode -1)
(setq require-final-newline t)
(global-unset-key (kbd "C-z")) ;; Don't minimize.
(global-unset-key (kbd "C-x C-b")) ;; Don't show buffer overview.
(global-unset-key (kbd "C-x C-l")) ;; Don't use downcase-region.
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-hl-line-mode)
(show-paren-mode)
(global-eldoc-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq confirm-kill-emacs #'yes-or-no-p)

;; Too annoying to move the mouse to check time when in full screen.
(display-time-mode 1)
(setq display-time-24hr-format 't)
(setq display-time-day-and-date 't)

(defun system-win? ()
  "True if current system is Windows."
  (or (eq system-type 'windows-nt)
      (eq system-type 'ms-dos)))

(defun system-osx? ()
  "True if current system is Mac OSX."
  (eq system-type 'darwin))

;; If Emacs is not started from shell, e.g. on Mac OSX, this fixes the
;; PATH environment variable. Important for running external programs,
;; such as latex.
(when (system-osx?)
  (let ((path-from-shell
	 (replace-regexp-in-string "[\t\n]*$" ""
				   (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; Minor mode keymap idea from http://stackoverflow.com/a/683575/804397
(defvar fbie-minor-mode-map (make-sparse-keymap) "Florian's keymap.")
(define-minor-mode fbie-minor-mode
  "Florians's global override keybindings."
  :init-value t
  :lighter " Florian")

(define-key fbie-minor-mode-map (kbd "s-<down>") 'shrink-window)
(define-key fbie-minor-mode-map (kbd "s-<up>") 'enlarge-window)

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

(use-package flycheck
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode))

(use-package flyspell
  :init
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package writegood-mode
  :init
  (add-hook 'LaTeX-mode-hook 'writegood-mode))

(use-package helm
  :init
  (helm-mode 'true)
  (helm-autoresize-mode 'true)
  (add-hook 'LaTeX-mode-hook 'helm-mode)
  ;; No need to put this in :bind
  (define-key fbie-minor-mode-map (kbd "M-x") 'helm-M-x)
  (define-key fbie-minor-mode-map (kbd "C-x C-f") 'helm-find-files)
  (define-key fbie-minor-mode-map (kbd "C-x C-g") 'helm-recentf))

(use-package magit
  :init
  (define-key fbie-minor-mode-map (kbd "C-c i") 'magit-status))

(use-package company
  :init
  (global-company-mode))

(use-package omnisharp
  :ensure company
  :bind (:map omnisharp-mode-map
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
  (add-to-list 'company-backends 'company-omnisharp)

  (defconst omnisharp-server "~/src/omnisharp-server/OmniSharp/bin/Release/OmniSharp.exe")
  (defconst omnisharp-roslyn "~/src/omnisharp-roslyn/artifacts/publish/OmniSharp/default/net451/OmniSharp.exe")

  (defcustom omnisharp-server-path
    omnisharp-server
    "User defined path to Omnisharp server executable.")
  (setq omnisharp-server-executable-path omnisharp-server-path)

  (defun omnisharp-set-server (path)
    "Use PATH as the path to the Omnisharp server."
    (setq omnisharp-server-path path)
    (setq omnisharp-server-executable-path path))

  (defun omnisharp-use-roslyn ()
    "Use Omnisharp with Roslyn compiler."
    (interactive)
    (omnisharp-set-server omnisharp-roslyn))

  (defun omnisharp-use-classic ()
    "Use the classic Omnisharp implementation."
    (interactive)
    (omnisharp-set-server omnisharp-server)))

(use-package paredit
  :init
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'ielm-mode-hook 'paredit-mode))

(use-package racket-mode
  :ensure paredit
  :bind (:map racket-mode-map
	 ("C-h f" . racket-describe)
	 :map racket-repl-mode-map
	 ("C-h f" . racket-describe))
  :init
  (add-hook 'racket-mode-hook 'paredit-mode)
  (add-hook 'racket-repl-mode-hook 'paredit-mode)
  :config
  (setq racket-paren-face '(t (:foreground "dark gray"))))

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
  :init
  (define-key fbie-minor-mode-map (kbd "s-<f15>") 'spotify-playpause)) ;; Works on Kinesis Advantage.

(require 'org)
(setq org-log-done t) ;; Log completion of tasks.
(setq org-pretty-entities t)

(defun org-agenda-and-todos ()
  "Display org agenda and todo list.  Equal to <M-x> org-agenda <RET> n."
  (interactive)
  (org-agenda nil "n"))

(when (file-directory-p "~/org")
  ;; Load org files only if directory exists.
  (setq org-agenda-files '("~/org/personal.org"
			   "~/org/work.org"
			   "~/org/funcalc.org"))
  ;; Start emacs in agenda view.
  (when (window-system)
    (setq initial-buffer-choice (lambda ()
				  (save-window-excursion
				    (org-agenda-and-todos)
				    (get-buffer "*Org Agenda*"))))))

(define-key fbie-minor-mode-map (kbd "C-c a") 'org-agenda-and-todos)

(use-package professional-theme
  :init
  (load-theme 'professional t))

(use-package gnuplot-mode
  :mode "\\.gnuplot\\'")

(use-package markdown-mode
  :mode "\\.md\\'")

;; Includes ace-jump-mode and helm-swoop. Both are awesome!
(use-package ace-isearch
  :init
  (global-ace-isearch-mode 1)
  (setq ace-isearch-input-length 1))

(use-package ssh-config-mode)

;; Fix FiraCode font ligatures. This is taken from
;; https://github.com/tonsky/FiraCode/wiki/Setting-up-Emacs and works
;; just as well for other fonts, apparently.
(when (window-system)
  (set-default-font "Fira Code Retina 15")
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
		 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
		 (36 . ".\\(?:>\\)")
		 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
		 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
		 ;; (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
		 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
		 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
		 (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
		 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
		 (48 . ".\\(?:x[a-zA-Z]\\)")
		 (58 . ".\\(?:::\\|[:=]\\)")
		 (59 . ".\\(?:;;\\|;\\)")
		 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
		 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
		 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
		 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
		 (91 . ".\\(?:]\\)")
		 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
		 (94 . ".\\(?:=\\)")
		 (119 . ".\\(?:ww\\)")
		 (123 . ".\\(?:-\\)")
		 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
		 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)"))))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
			    `([,(cdr char-regexp) 0 font-shape-gstring])))))
(put 'downcase-region 'disabled nil)

(add-hook 'after-init-hook
	  (lambda () (when (window-system)
		       ;; Always run graphical Emacs in fullscreen.
		       (toggle-frame-fullscreen)
		       ;; Graphical Emacs instances may run as server.
		       (unless (server-running-p)
			 (server-start)))))

(fbie-minor-mode 1)
