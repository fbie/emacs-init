;; .emacs file by Florian Biermann <fbie@itu.dk>

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex")
 '(default-input-method "TeX")
 '(global-prettify-symbols-mode nil)
 '(global-pretty-mode nil)
 '(mac-command-modifier (quote super))
 '(mac-mouse-wheel-smooth-scroll nil)
 '(mac-option-modifier (quote meta))
 '(org-support-shift-select t)
 '(package-selected-packages
   (quote
    (ssh ssh-config-mode gnuplot-mode markdown-mode auctex professional-theme spotify centered-window-mode fsharp-mode racket-mode paredit company omnisharp magit helm writegood-mode flyspell-lazy flycheck)))
 '(safe-local-variable-values (quote ((TeX-engine . latex))))
 '(save-place-mode t nil (saveplace))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "White"))))
 '(racket-paren-face ((t (:foreground "dark gray")))))

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

(global-unset-key (kbd "C-z")) ;; Don't minimize!
(setq require-final-newline t) ;; OMG final newlines!

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(defmacro require-install (package)
  "Require PACKAGE, install it if missing."
  `(unless (require ,package nil t)
     (package-install ,package)
     (require ,package)))

;; Always remove trailing whitespaces.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Highlight current line.
(add-hook 'after-init-hook 'global-hl-line-mode)

;; Make sure reftex is turned on.
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; Flycheck
(require-install 'flycheck)

;; Add flyspell mode hooks.
(require-install 'flyspell)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(require-install 'writegood-mode)
(add-hook 'LaTeX-mode-hook 'writegood-mode)

;; Helm
(require-install 'helm)
(helm-mode 'true)
(helm-autoresize-mode 'true)
(add-hook 'LaTeX-mode-hook 'helm-mode)
(define-key fbie-minor-mode-map (kbd "M-x") 'helm-M-x)
(define-key fbie-minor-mode-map (kbd "C-x C-f") 'helm-find-files)
(define-key fbie-minor-mode-map (kbd "C-x C-g") 'helm-recentf)

;; Magit
(require-install 'magit)
(define-key fbie-minor-mode-map (kbd "C-c i") 'magit-status)

;; C# and OmniSharp
(require-install 'omnisharp)
(add-hook 'csharp-mode-hook 'omnisharp-mode)
(add-hook 'omnisharp-mode-hook 'eldoc-mode)

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
  (omnisharp-set-server omnisharp-server))

;; Load company for omnisharp
(require-install 'company)
(setq omnisharp-company-template-use-yasnippet nil)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))
(add-hook 'omnisharp-mode-hook 'company-mode)

(define-key omnisharp-mode-map (kbd "C-SPC") 'company-search-candidates)
(define-key omnisharp-mode-map (kbd "M-.") 'omnisharp-go-to-definition)
(define-key omnisharp-mode-map (kbd "M-,") 'pop-tag-mark)

(define-key omnisharp-mode-map (kbd "C-u") 'omnisharp-helm-find-usages)

(define-key omnisharp-mode-map (kbd "S-s-<up>") 'omnisharp-navigate-up)
(define-key omnisharp-mode-map (kbd "S-s-<down>") 'omnisharp-navigate-down)

(define-key omnisharp-mode-map (kbd "s-i") 'omnisharp-helm-find-implementations)
(define-key omnisharp-mode-map (kbd "C-.") 'omnisharp-run-code-action-refactoring)

(define-key omnisharp-mode-map (kbd "<f2>") 'omnisharp-rename-interactively)
(define-key omnisharp-mode-map (kbd "<f5>") 'omnisharp-build-in-emacs)

;; Always run paredit and eldoc.
(require-install 'paredit)
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'paredit-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

;; Racket mode
(require-install 'racket-mode)
(add-hook 'racket-mode-hook 'paredit-mode)
(add-hook 'racket-repl-mode-hook 'paredit-mode)
;; I prefer elisp-mode style key bindings.
(define-key racket-mode-map (kbd "C-h f") 'racket-describe)
(define-key racket-repl-mode-map (kbd "C-h f") 'racket-describe)

;; F# mode
(require-install 'fsharp-mode)
(setq inferior-fsharp-program (string-join (list inferior-fsharp-program " --mlcompatibility -g -d:TRACE -d:DEBUG")))

;; Centered window mode.
(require-install 'centered-window-mode)
(centered-window-mode t)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Downloading bibliography from CiteULike
(defcustom citeulike-user "fbie" "The CiteULike user to download bibliography from.")
(defconst citeulike-url "http://www.citeulike.org/bibtex/user/%s?key_type=4&clean_urls=0&incl_amazon=0&smart_wrap=1")

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

(when (not (system-win?))
  (require-install 'spotify)
  (define-key fbie-minor-mode-map (kbd "s-<f15>") 'spotify-playpause)) ;; Works on Kinesis Advantage.

;; Org-mode
(require-install 'org)
(setq org-log-done t) ;; Log completion of tasks.
(setq org-pretty-entities t)
(defun org-agenda-and-todos ()
  "Display org agenda and todo list.  Equal to <M-x> org-agenda <RET> n."
  (interactive)
  (org-agenda nil "n"))

(when (file-directory-p "~/org") ;; Load org files only if directory exists.
  (setq org-agenda-files '("~/org/personal.org"
			   "~/org/work.org"
			   "~/org/funcalc.org")))

(setq initial-buffer-choice (lambda () ;; Start emacs in agenda view.
			      (save-window-excursion
				(org-agenda-and-todos)
				(get-buffer "*Org Agenda*"))))

(define-key fbie-minor-mode-map (kbd "C-c a") 'org-agenda-and-todos)

(scroll-bar-mode -1) ;; Hide scroll bars.
(require-install 'professional-theme)
(load-theme 'professional t) ;; More easy on the eyes!

(fbie-minor-mode 1)

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
