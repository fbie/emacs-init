(use-package eglot)

(use-package fsharp-mode
  :after eglot
  :bind (:map fsharp-mode-map ("C-c x" . flycheck-next-error))
  :init
  (add-to-list 'auto-mode-alist '("\\.fsproj$" . xml-mode))
  :config
  (setq fsharp-doc-idle-delay 1.0
        inferior-fsharp-program "dotnet fsi")
  (add-to-list 'helm-boring-file-regexp-list "^obj/"))

(use-package eglot-fsharp
  :after fsharp-mode
  :init
  (add-hook 'fsharp-mode-hook
            (lambda ()
              (require 'eglot)
              (require 'eglot-fsharp)
              ;; (setq eglot-fsharp-server-version "0.41.1")
              (eglot-ensure))))

(provide 'fb-fsharp)
