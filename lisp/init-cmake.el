(autoload 'cmake-mode "cmake-mode" t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

(defun imp/cmake-mode-hook ()
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (local-set-key [return] 'newline-and-indent)
  )
(add-hook 'cmake-mode-hook 'imp/cmake-mode-hook)
(provide 'init-cmake)
