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
              eglot-fsharp-server-version "0.41.1"))))

  (eval-after-load 'omnisharp
    (setq omnisharp-server-executable-path "c:/Users/FLBM/Bin/omnisharp-win-x86/OmniSharp.exe")))

(use-package notgit
  :straight (:type git :repo "https://simhub.simcorp.com/NGWS/notgit")
  :demand
  :bind
  (:map notgit-status-mode-map
        ("q" . bury-buffer)
        ("f" . fb-notgit-project-open-from-workspace)
        ("e" . notgit-s))
  (:map notgit-shelvesets-mode-map
        ("q" . bury-buffer))
  :config
  (setq notgit-default-owner "FLBM"
        notgit-default-workspace "DEVPATCH-P_OTC")
  (defun fb-notgit-project-open-from-workspace (workspace)
    (interactive (list (read-string (notgit-make-prompt "Workspace" notgit-default-workspace)
                       nil
                       nil
                       notgit-default-workspace)))
    (helm-browse-project-find-files (concat "C:/Workspaces/" workspace "/DEVPATCH-P/IMS"))))

(provide 'fb-simcorp)
