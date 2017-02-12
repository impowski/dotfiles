;; helm
(use-package helm
  :defer 1
  :bind
  (("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)
   ("C-h SPC" . helm-all-mark-rings)
   ("C-c h o" . helm-occur)
   ("C-c h x" . helm-register)
   (:map minibuffer-local-map
         ("M-p" . helm-minibuffer-history)
         ("M-n" . helm-minibuffer-history))
   )
  :config
  (progn (setq-default helm-command-prefix-key "C-c h")
         (require 'helm-config)
         (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
         (bind-keys :map helm-map
                    ([tab] . helm-execute-persistent-action)
                    ("C-i" . helm-execute-persistent-action)
                    ("C-z" . helm-select-action))
         )
  (setq
   helm-scroll-amount 4
   helm-ff-search-library-in-sexp t
   helm-split-window-in-side-p t
   helm-candidate-number-limit 500
   helm-ff-file-name-history-use-recentf t
   helm-move-to-line-cycle-in-source t
   helm-buffers-fuzzy-matching t))

(use-package helm-flx
  :defer t
  :config
  (progn (setq helm-flx-for-helm-find-files nil)
         (helm-flx-mode)))

;; helm-make
(use-package helm-make
  :defer t)

;; helm-projectile
(use-package helm-projectile
  :commands (helm-projectile-switch-to-buffer
             helm-projectile-find-dir
             helm-projectile-dired-find-dir
             helm-projectile-recentf
             helm-projectile-find-file
             helm-projectile-grep
             helm-projectile
             helm-projectile-switch-project))

;; helm-swoop
(use-package helm-swoop
  :defer t
  :bind
  (
   ("C-c h o" . helm-swoop)
   ("C-c s" . helm-multi-swoop-all)
   (:map isearch-mode-map
         ("M-i" . helm-swoop-from-isearch))
   (:map helm-swoop-map
         ("M-i" . helm-multi-swoop-all-from-helm-swoop))
   )
  :init
  (progn
    (setq helm-multi-swoop-edit-save t
          helm-swoop-split-direction 'split-window-vertically
          helm-swoop-speed-or-color t
          helm-swoop-split-window-function 'helm-default-display-buffer
          helm-swoop-pre-input-function (lambda () "")))
  )


(global-unset-key (kbd "C-x c"))

(use-package helm-grep
  :ensure helm
  :bind
  ((:map helm-grep-mode-map
         ("<return>" . helm-grep-mode-jump-other-window)
         ("n" . helm-grep-mode-jump-other-window-forward)
         ("p" . helm-grep-mode-jump-other-window-backward))
   )
  )

(when (executable-find "curl")
  (setq helm-net-prefer-curl t))

(define-key 'help-command (kbd "C-f") 'helm-apropos)
(define-key 'help-command (kbd "r") 'helm-info-emacs)
(define-key 'help-command (kbd "C-l") 'helm-locate-library)

;; use helm to list eshell history
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

;;; Save current position to mark ring
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

(define-key global-map [remap find-tag] 'helm-etags-select)

(define-key global-map [remap list-buffers] 'helm-buffers-list)


(provide 'init-helm)
