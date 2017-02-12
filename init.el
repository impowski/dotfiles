;;; init.el --- Init file

;;; Commentary

;;; Code:

(defvar personal-dir (file-name-directory load-file-name)
  "The root dir of personal Emacs config.")
(defvar personal-lisp-dir (expand-file-name "lisp" personal-dir))

(add-to-list 'load-path personal-lisp-dir)

(package-initialize)

(require 'init-elpa)
(require 'init-environment)
(require 'init-editing)
(require 'init-helm)
(require 'init-git)

(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; auto-compile
(use-package auto-compile
  :commands
  (auto-compile-on-load-mode
   auto-compile-on-save-mode))

(setq load-prefer-newer t)

;; cargo
(use-package cargo
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

;; company
(use-package company
  :ensure t
  :bind
  (("C-c /" . company-files))
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay .2)
  (setq company-tooltip-limit 20)
  (setq company-tooltip-align-annotations 't)
  (setq company-begin-commands '(self-insert-command)))

;; company-web
(use-package company-web-html
  :ensure company
  :config (add-to-list 'company-backends 'company-web-html))

(use-package company-web
  :ensure company
  :config (add-to-list 'company-backends 'company-web-jade))

(use-package company-web-slim
  :ensure company
  :config (add-to-list 'company-backends 'company-web-slim))

;; evil
; (use-package evil
  ; :config
  ; (evil-mode 1))

;; flycheck
(use-package flycheck
  :ensure t
  :commands flyspell-mode)

;; flycheck-rust
(use-package flycheck-rust
  :ensure flycheck
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
  :mode "\\.js\\'"
  :mode "\\.jsx\\'"
  :config
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode)))

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
