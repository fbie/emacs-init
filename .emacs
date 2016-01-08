;;; package -- Summary:
;;; Commentary:
;;; Code:

(defmacro require-install (package)
  "Require PACKAGE, install it if missing."
  `(unless (require ,package nil t)
     (package-install ,package)
     (require ,package)))

;; If Emacs is not started from shell, e.g. on Mac OSX, this fixes the
;; PATH environment variable. Important for running external programs,
;; such as latex.
(let ((path-from-shell
       (replace-regexp-in-string "[\t\n]*$" ""
				(shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
(setenv "PATH" path-from-shell)
(setq exec-path (split-string path-from-shell path-separator)))

;; Don't minimize!
(global-unset-key (kbd "C-z"))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Always remove trailing whitespaces.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Highlight current line and custom-set-faces seems broken im my
;; version of Emacs.
(add-hook 'after-init-hook 'global-hl-line-mode)
(add-to-list 'default-frame-alist
             '(font . "Monaco-14"))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; Flycheck
(require-install 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'LaTeX-mode-hook 'flycheck-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; Helm
(require-install 'helm-config)
(helm-mode 'true)
(helm-autoresize-mode 'true)
(add-hook 'LaTeX-mode-hook 'helm-mode)

;; Magit
(require-install 'magit)
(global-set-key (kbd "C-c i") 'magit-status)

;; C# and OmniSharp
(require-install 'omnisharp)
(add-hook 'csharp-mode-hook 'omnisharp-mode)
(add-hook 'omnisharp-mode-hook 'eldoc-mode)
(setq omnisharp-server-executable-path "/usr/local/bin/omnisharp")

;; Load company for omnisharp
(require-install 'company)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))
(add-hook 'omnisharp-mode-hook 'company-mode)

(define-key omnisharp-mode-map (kbd "C-SPC") 'company-search-candidates)
(define-key omnisharp-mode-map (kbd "<f12>") 'omnisharp-go-to-definition)
(define-key omnisharp-mode-map (kbd "C-x p") 'pop-tag-mark)

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
(add-hook 'ielm-mode-hook-hook 'paredit-mode)
(add-hook 'ielm-mode-hook-hook 'eldoc-mode)

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
       (file-path (read-string (format "Write to (default %s): " default-path) nil nil default-path)))
    (url-copy-file citeulike-url-usr file-path 'true)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex")
 '(highlight-current-line nil)
 '(org-support-shift-select t)
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
