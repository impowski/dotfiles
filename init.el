;;; init.el --- Init file

;;; Commentary

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

(defconst imp-packages
  '(anzu
    auto-complete
    base16-theme
    cask
    cargo
    company
    company-irony
    company-irony-c-headers
    circe
    cmake-ide
    cmake-mode
    clean-aindent-mode
    comment-dwim-2
    duplicate-thing
    flycheck
    flycheck-irony
    flycheck-rust
    helm
    helm-swoop
    magit
    markdown-mode
    jabber
    jabber-otr
    dtrt-indent
    ws-butler
    iedit
    irony
    irony-eldoc
    jade-mode
    yasnippet
    smartparens
    powerline
    rtags
    rust-mode
    racer
    use-package
    projectile
    volatile-highlights
    undo-tree
    zygospore))

(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package imp-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)

(require 'init-editing)
(require 'setup-environment)
(require 'init-cmake)
(require 'init-git)
(require 'init-jabber)
(require 'init-helm)

;;(eval-after-load 'company
;;  '(add-to-list 'company-backends '(company-irony-c-headers company-irony)))
;;(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;;(add-hook 'c++-mode-hook 'irony-mode)
;;(add-hook 'c-mode-hook 'irony-mode)
;;(add-hook 'objc-mode-hook 'irony-mode)

;;(add-hook 'after-init-hook #'global-flycheck-mode)
;;(eval-after-load 'flycheck
;;  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;;(defun my-irony-mode-hook ()
;;  (define-key irony-mode-map [remap completion-at-point]
;;    'irony-completion-at-point-async)
;;  (define-key irony-mode-map [remap complete-symbol]
;;    'irony-completion-at-point-async))
;;(add-hook 'irony-mode-hook 'my-irony-mode-hook)
;;(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; autocomplete
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)

(require 'racer)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(require 'irony)
(add-to-list 'company-backends '(company-irony-c-headers company-irony))
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)

(require 'flycheck)
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(flycheck-irony-setup)

;; rtags
(require 'rtags)

;; jade-mode
(require 'jade-mode)

;; cmake-ide
(cmake-ide-setup)

(global-set-key (kbd "<f5>") 'cmake-ide-compile)

(require 'dtrt-indent)
(dtrt-indent-mode 1)

(require 'yasnippet)
(yas-global-mode 1)

;; Package: clean-aindent-mode
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; Package: powerline
(require 'powerline)
(powerline-center-theme)

;; Package: projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-catching t)

;; Package: zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

;; Style
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c w") 'whitespace-mode)

(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq make-backup-files nil)

;; Font
(set-frame-font "Source Code Pro-10")

;; Theme
(load-theme 'base16-google-dark t)

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))
;; Setup GDB
(setq gdb-many-windows t
      gdb-show-main t)

;;; init.el ends here
