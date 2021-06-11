(use-package sc-tools
  :straight (:type git :repo "https://simhub.simcorp.com/flbm/sc-tools-el"))

(when (eq system-type 'windows-nt)
  (require 'cygwin-mount)
  (require 'setup-cygwin)

  (eval-after-load 'magit
    (setq magit-git-executable "C:/Program Files/Git/bin/git.exe"))

  (eval-after-load 'merlin
    (let ((ml-mono-dir "C:/Repos/ml-mono/"))
      (when (file-directory-p ml-mono-dir)
        (setq merlin-command (concat ml-mono-dir "tools/merlin/ocamlmerlin.exe")))))

  (eval-after-load 'eglot-fsharp
    (progn
      (require 'eglot-fsharp)
      (when (eq eglot-fsharp-server-runtime 'net-framework)
        (setq inferior-fsharp-program "C:/Program Files (x86)/Microsoft SDKs/F#/4.0/Framework/v4.0/FsiAnyCPU.exe"
              eglot-fsharp-server-version "0.41.1")))))

(use-package notgit
  :straight (:type git :repo "https://simhub.simcorp.com/NGWS/notgit")
  :demand
  :bind
  (:map notgit-status-mode-map
        ("q" . bury-buffer)))

(provide 'fb-simcorp)
