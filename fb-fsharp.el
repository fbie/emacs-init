(use-package eglot)

(use-package fsharp-mode
  :after eglot
  :init
  (add-to-list 'auto-mode-alist '("\\.fsproj$" . xml-mode))
  :config
  (setq fsharp-doc-idle-delay 1.0
        inferior-fsharp-program "dotnet fsi")
  (add-to-list 'helm-boring-file-regexp-list "^obj/")
  :bind
  (:map fsharp-mode-map
        ("C-c x" . flycheck-next-error)))

(use-package eglot-fsharp
  :after
  fsharp-mode
  eglot
  project
  :init
  (setq eglot-fsharp-server-runtime 'net-core)
  (unless (functionp 'project-root)
    (defun project-root (project)
      "A small alias for calling PROJECT-ROOT in Emacs 27."
      (car (project-roots project))))
  (add-hook 'fsharp-mode-hook (lambda ()
                                (require 'eglot-fsharp)
                                (eglot-ensure))))

(provide 'fb-fsharp)
