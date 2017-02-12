;;; init.el --- Init file

;;; Commentary

;;; Code:
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'init-elpa)
(require 'init-editing)
(require 'init-environment)
(require 'init-cmake)
(require 'init-git)
(require 'init-helm)

(setq load-prefer-newer t)

;; auto-compile
(use-package auto-compile
  :ensure t
  :config
  (progn
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)))

;; cargo
(use-package cargo
  :init (add-hook 'rust-mode-hook 'cargo-minor-mode))

;; company
(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (progn
    (setq company-tooltip-limit 20)
    (setq company-tooltip-align-annotations 't)
    (setq company-idle-delay .3)
    (setq company-begin-commands '(self-insert-command))
    (global-set-key (kbd "C-c /") 'company-files)))

;; company-web
(use-package company-web-html
  :init (add-to-list 'company-backends 'company-web-html))

(use-package company-web-jade
  :init (add-to-list 'company-backends 'company-web-jade))

(use-package company-web-slim
  :init (add-to-list 'company-backends 'company-web-slim))

;; evil
; (use-package evil
  ; :config
  ; (evil-mode 1))

;; flycheck
(use-package flycheck)

;; flycheck-rust
(use-package flycheck-rust
  :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; flx
(use-package flx)

;; flx-ido
(use-package flx-ido
  :init
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  (progn
    (setq ido-enable-flex-matching t)
    (setq ido-use-faces nil)))

;; rust-mode
(use-package rust-mode)

;; racer
(use-package racer
  :init
  (progn
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode))
  :config
  (setq racer-rust-src-path "~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src/"))

(setq company-tooltip-align-annotations t)

;; tagedit
(use-package tagedit
  :init (add-hook 'html-mode-hook (lambda()
                                    (tagedit-mode 1)
                                    (tagedit-add-experimental-features))))

;; js-mode
(use-package js2-mode
  :init
  (progn
    (add-hook 'js-mode-hook 'js2-minor-mode)
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))))

;; vue-mode
(use-package vue-mode
  :init (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode)))

;; powerline
(use-package spaceline
  :config
  (use-package spaceline-config
    :config
    (spaceline-spacemacs-theme)
    (spaceline-helm-mode)
    (spaceline-toggle-battery-on)
    (spaceline-toggle-minor-modes-off)
    (spaceline-toggle-flycheck-info-on)
    (spaceline-toggle-buffer-size-off)
    (spaceline-toggle-auto-compile-on)
    (spaceline-info-mode)
    ))

;; projectile
(use-package projectile
  :init (projectile-mode)
  :config (setq projectile-enable-catching t))

;; smex
(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind ("M-x" . smex))

;; zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

(setq make-backup-files nil)

;; Font
(set-frame-font "Source Code Pro-10")

;; Theme
(load-theme 'base16-google-dark t)

;;; init.el ends here
