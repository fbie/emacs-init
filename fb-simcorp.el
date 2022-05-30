(use-package sc-tools
  :straight (:type git :repo "https://simhub.simcorp.com/flbm/sc-tools-el"))

(when (eq system-type 'windows-nt)
  (require 'cygwin-mount)
  (require 'setup-cygwin)
  (require 'fb)

  (eval-after-load 'magit
    (setq magit-git-executable "C:/Program Files/Git/bin/git.exe"))

  (defconst fb/ml-mono "ml-mono"
    "The ml-mono name as a string.")

  (defconst fb/ml-mono-dir (concat "C:/Repos/" fb/ml-mono)
    "The directory pointing to a local ml-mono repository clone.")

  (defun fb/ml-mono-exists ()
    (file-directory-p fb/ml-mono-dir))

  ;; Some project.el integration.
  (when (fb/ml-mono-exists)
    (require 'project)
    (defun ocaml-find-ml-mono (dir)
      "Find ml-mono/src directory from DIR when visiting a buffer whose path contains ml-mono."
      (when (fb/ends-with dir (concat fb/ml-mono "/"))
        (let ((ml-mono-src (concat dir "src")))
          (cons 'ocaml ml-mono-src))))
    (cl-defmethod project-roots ((project (head ocaml)))
      (list (cdr project)))
    (add-hook 'project-find-functions #'ocaml-find-ml-mono))

  (eval-after-load 'merlin
    (when (fb/ml-mono-exists)
      (setq merlin-command (concat fb/ml-mono-dir "/tools/merlin/ocamlmerlin.exe")
            merlin-eldoc-doc nil)))

  (eval-after-load 'eglot-fsharp
    (progn
      (require 'eglot-fsharp)
      (setq inferior-fsharp-program "\"C:/Program Files (x86)/Microsoft SDKs/F#/4.0/Framework/v4.0/FsiAnyCPU.exe\"")))

  (eval-after-load 'omnisharp
    (setq omnisharp-server-executable-path "c:/Users/FLBM/Bin/omnisharp-win-x86/OmniSharp.exe")))

(when (eq system-type 'gnu/linux) ;; WSL
  (setq browse-url-browser-function (lambda (browse-url &rest args) (call-process "wslview" nil nil nil browse-url))))

(use-package notgit
  :straight (:type git :repo "https://simhub.simcorp.com/NGWS/notgit" :files ("*"))
  :demand
  :bind
  ("C-c n" . notgit-status)
  (:map notgit-status-mode-map
        ("q" . quit-window)
        ("f" . fb-notgit-project-open-from-workspace))
  (:map notgit-shelvesets-mode-map
        ("q" . bury-buffer))
  :config
  (setq notgit-default-owner "FLBM"
        notgit-default-workspace "DEVPATCH-P_OTC19")
  (defun fb-notgit-project-open-from-workspace ()
    (interactive)
    (helm-browse-project-find-files (concat "C:/Workspaces/" notgit-current-workspace "/DEVPATCH-P/IMS"))))

(use-package version2
  :straight (:type git :repo "https://simhub.simcorp.com/NGWS/version2" :files ("*"))
  :demand
  :config
  (setq version2-default-team "Oh No, More Lemmings"))

(provide 'fb-simcorp)
