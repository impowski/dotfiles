;;; init.el --- Init file

;;; Commentary

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

(defconst imp-packages
  '(anzu
    company
    company-irony
    company-irony-c-headers
    cmake-ide
    cmake-mode
    clean-aindent-mode
    comment-dwim-2
    duplicate-thing
    darcula-theme
    eldoc
    flycheck
    flycheck-irony
    magit
    jabber
    jabber-otr
    dtrt-indent
    ws-butler
    iedit
    irony
    yasnippet
    smartparens
    powerline
    rtags
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
(provide 'init)

(setq socks-override-functions 1)
(setq socks-noproxy '("localhost"))
(require 'socks)
(setq socks-server (list "Tor" "localhost" 9100 5))

(dired-async-mode 1)

;; Package: irony
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; rtags, cmake-ide
(require 'rtags)
(require 'cmake-ide)
(cmake-ide-setup)

;; Package: company-irony
(require 'company)
(require 'company-irony-c-headers)
(eval-after-load 'company
  '(add-to-list 'company-backends '(company-irony-c-headers company-irony)))

;; Package: flycheck
(require 'flycheck)
(require 'flycheck-irony)
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; Package: clean-aindent-mode
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; Package: powerline
(require 'powerline)
(setq powerline-arrow-shape 'curve)
(setq powerline-default-separator-dir '(left . right))


;; Package: projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-catching t)

;; Package: zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

;; Style
(setq c-default-style "linux"
      c-basic-offset 4)

;; Theme
(require 'darcula-theme)

;; Font
(set-default-font "Source Code Pro-14")

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c w") 'whitespace-mode)

(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

(setq-default indent-tabs-mode nil)

(setq-default tab-width 4)

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; Setup GDB
(setq gdb-many-windows t
          gdb-show-main t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (darcula)))
 '(magit-save-some-buffers (quote dontask))
 '(quote
   (custom-safe-themes
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(sml/mode-width
   (if
       (eq powerline-default-separator
           (quote arrow))
       (quote right)
     (quote full)))
 '(sml/pos-id-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s" powerline-default-separator
                            (car powerline-default-separator-dir)))
                   (quote powerline-active1)
                   (quote powerline-active2))))
     (:propertize " " face powerline-active2))))
 '(sml/pos-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s" powerline-default-separator
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active1)
                   nil)))
     (:propertize " " face sml/global))))
 '(sml/pre-id-separator
   (quote
    (""
     (:propertize " " face sml/global)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s" powerline-default-separator
                            (car powerline-default-separator-dir)))
                   nil
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active2)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s" powerline-default-separator
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active2)
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(jabber-chat-prompt-system ((t (:foreground "darkgreen" :weight bold))))
 '(magit-item-highlight ((t (:background "CadetBlue1")))))

;;; init.el ends here
