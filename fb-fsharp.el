(use-package eglot)

(use-package highlight-indentation
  :init )

(use-package fsharp-mode
  :after lsp-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.fsproj$" . xml-mode))
  (add-hook 'fsharp-mode-hook 'highlight-indentation-current-column-mode)
  (add-hook 'fsharp-mode-hook 'lsp-mode)
  :config
  (setq fsharp-doc-idle-delay 1.0
        warning-minimum-level :error
        inferior-fsharp-program "dotnet fsi --readline-")
  (add-to-list 'helm-boring-file-regexp-list "^obj/")
  :bind
  (:map fsharp-mode-map
        ("C-c x" . flycheck-next-error)))

(provide 'fb-fsharp)
