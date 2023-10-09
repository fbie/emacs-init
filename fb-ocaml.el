(use-package tuareg
  :mode ("\\.ml[i]?" . tuareg-mode)
  :bind (:map tuareg-mode-map
              ("C-c TAB" . helm-imenu))
  :config
  (setq tuareg-indent-align-with-first-arg nil
        tuareg-electric-close-vector 't
        tuareg-default-indent 2)
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
        merlin-completion-dwim nil
        merlin-locate-in-new-window 'never)
  (defun helm-merlin-occurrences ()
    (interactive)
    (let ((occurrences (merlin-occurrences)))
      (helm :sources occurrences)))
  (add-hook 'tuareg-mode-hook 'merlin-mode))

(use-package dune)

(use-package merlin-eldoc
  :disabled
  :after
  tuareg
  merlin
  :config
  (setq merlin-eldoc-type-verbosity 'min)
  (add-hook 'tuareg-mode-hook 'merlin-eldoc-setup))

(provide 'fb-ocaml)
