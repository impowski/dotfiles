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
  :defer t
  :init
  (progn
    (setq auto-compile-display-buffer nil
          auto-compile-use-mode-line nil
          auto-compile-mode-line-counter t)
    (add-hook 'emacs-lisp-mode-hook 'auto-compile-mode)))

(setq load-prefer-newer t)

;; cargo
(use-package cargo
  :defer t
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

;; company
(use-package company
  :defer t
  :bind
  (("C-c /" . company-files))
  :init
  (progn
    (setq
     company-idle-delay .2
     company-minimum-prefix-length 2
     company-require-match nil
     company-dabbrev-ignore-case nil
     company-dabbrev-downcase nil)
    (global-company-mode)
    ))

(use-package company-statistics
  :defer t
  :init
  (progn
    (add-hook 'company-mode-hook 'company-statistics-mode)))

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
  :defer t
  :init
  (progn
    (setq flycheck-standard-error-navigation nil
          flycheck-global-modes nil)
    (global-flycheck-mode 1)))

;; flycheck-rust
(use-package flycheck-rust
  :defer t
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
(use-package rust-mode
  :defer t)

;; toml-mode
(use-package toml-mode
  :mode "/\\(Cargo.lock\\|\\.cargo/config\\)\\'")

;; racer
(use-package racer
  :defer t
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
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode)
  (spaceline-toggle-battery-on)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-flycheck-info-on)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-auto-compile-on)
  (spaceline-info-mode)
  )

;; projectile
(use-package projectile
  :init (projectile-mode)
  :config (setq projectile-enable-catching t))

;; all-the-icons
(use-package all-the-icons)

;; neotree
(use-package neotree
  :defer t
  :commands neo-global--window-exists-p
  :bind ("<f8>" . neotree-projectile)
  :config
  (setq neo-smart-open t
        neo-theme (if (display-graphic-p) 'icons 'arrow)
        projectile-switch-project-action 'neotree-projectile-action))

(defun neotree-projectile ()
  "Open neotree with projectile as root and open node for current file.
If projectile unavailable or not in a project, open node at file path.
If file path is not available, open $HOME."
  (interactive)
  (if (neo-global--window-exists-p)
      (call-interactively 'neotree-hide)
    (let ((file-name (buffer-file-name)))
      (if (and (not file-name)
               (let ((buffer-name (buffer-name)))
                 (cond
                  ((equal buffer-name "*cider-repl server*") nil)
                  (t t))))
          (neotree-dir "~/")
        (let ((dir-name (if (and (fboundp 'projectile-project-p)
                                 (projectile-project-p))
                            (projectile-project-root)
                          (file-name-directory file-name))))
          (neotree-dir dir-name)
          (when file-name
            (neo-buffer--select-file-node file-name)))))))

;; smex
(use-package smex
  :defer t
  :bind ("M-x" . smex))

;; zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

(setq make-backup-files nil)

;; Font
(set-frame-font "Source Code Pro-10")

;; Theme
(use-package base16-theme
  :init (load-theme 'base16-google-dark t)
  :config
  (set-face-background 'linum "#1d1f21")
  )


;;; init.el ends here
