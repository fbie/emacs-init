(add-hook 'c++-mode-hook 'lsp-mode)
(define-key c++-mode-map (kbd "C-c C-c") 'compile)

(provide 'fb-cpp)
