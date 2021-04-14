(use-package sc-tools
  :straight (:type git :repo "https://simhub.simcorp.com/flbm/sc-tools-el"))

(when (eq system-type 'windows-nt)
  (require 'cygwin-mount)
  (require 'setup-cygwin))

(defconst fb/is-windows (eq system-type 'windows-nt))

(eval-after-load 'magit
  (when fb/is-windows ;; Never use Cygwin's outdated version.
      (setq magit-git-executable "c:/Program Files/Git/bin/git.exe")))

(eval-after-load 'merlin
  (let ((ml-mono-dir "C:/Repos/ml-mono/"))
    (when (and fb/is-windows (file-directory-p ml-mono-dir))
      (setq merlin-command (concat ml-mono-dir "tools/merlin/ocamlmerlin.exe")))))

(provide 'fb-simcorp)
