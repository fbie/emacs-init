(use-package eglot)

(use-package fsharp-mode
  :after eglot
  :init
  (add-to-list 'auto-mode-alist '("\\.fsproj$" . xml-mode))
  :config
  (setq fsharp-doc-idle-delay 1.0
        inferior-fsharp-program "dotnet fsi")
  (add-to-list 'helm-boring-file-regexp-list "^obj/")
  (defun fsharp-eval-buffer ()
    "Evaluate current buffer in FSI."
    (interactive)
    (save-mark-and-excursion
      (fsharp-eval-region (point-min) (point-max))))
  :bind
  (:map fsharp-mode-map
        ("C-c x" . flycheck-next-error)
        ("C-c C-b" . fsharp-eval-buffer)))

(use-package eglot-fsharp
  :after
  fsharp-mode
  eglot
  project
  :init
  (unless (functionp 'project-root)
    (defun project-root (project)
      "A small alias for calling PROJECT-ROOT in Emacs 27."
      (car (project-roots project))))
  (add-hook 'fsharp-mode-hook (lambda ()
                                (require 'eglot-fsharp)
                                (eglot-ensure))))

(provide 'fb-fsharp)
