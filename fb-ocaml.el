(use-package tuareg
  :mode ("\\.ml[i]?" . tuareg-mode)
  :bind
  (:map tuareg-mode-map
        ("C-c TAB" . helm-imenu))
  :config
  (setq tuareg-indent-align-with-first-arg nil
        tuareg-electric-close-vector 't)
  (add-to-list 'helm-boring-file-regexp-list "\\.cmi$")
  (add-to-list 'helm-boring-file-regexp-list "\\.cmt$")
  (add-to-list 'helm-boring-file-regexp-list "\\.cmx$")
  (add-to-list 'helm-boring-file-regexp-list "\\.obj$")
  (add-to-list 'helm-boring-file-regexp-list "\\.annot$"))

(use-package merlin
  :diminish
  :demand
  :after
  tuareg
  helm
  :bind
  (:map merlin-mode-map
        ("M-," . merlin-pop-stack)
        ("M-." . merlin-locate)
        ("C-c x" . merlin-error-next))
  :config
  (setq merlin-completion-with-doc nil
        merlin-completion-dwim nil)
  (defun helm-merlin-occurrences ()
    (interactive)
    (helm :sources '(merlin-occurrences)))
  (add-hook 'tuareg-mode-hook 'merlin-mode))

(use-package dune)

(use-package merlin-eldoc
  :after
  tuareg
  merlin
  :config
  (setq merlin-eldoc-type-verbosity 'min)
  (add-hook 'tuareg-mode-hook 'merlin-eldoc-setup))

(provide 'fb-ocaml)
