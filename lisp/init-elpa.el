(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(defadvice use-package-ensure-elpa (around use-package-ensure-safe activate)
  "Capture errors from installing packages."
  (condition-case-unless-debug err
      ad-do-it
    (error
     (ignore
      (display-warning 'use-package
                       (format "Failed to install %s: %s"
                               package (error-message-string err))
                       :warning)))))

;; (defconst imp-packages
;;   '(anzu
;;     ac-html-bootstrap
;;     auto-compile
;;     base16-theme
;;     cask
;;     cargo
;;     company
;;     company-web
;;     clean-aindent-mode
;;     comment-dwim-2
;;     diminish
;;     dtrt-indent
;;     duplicate-thing
;;     evil
;;     flycheck
;;     flycheck-rust
;;     flx
;;     flx-ido
;;     helm
;;     helm-ag
;;     helm-flx
;;     helm-make
;;     helm-mode-manager
;;     helm-projectile
;;     helm-swoop
;;     helm-themes
;;     imenu
;;     popwin
;;     projectile
;;     indent-guide
;;     iedit
;;     ido
;;     jade-mode
;;     js2-mode
;;     js2-refactor
;;     magit
;;     markdown-mode
;;     tagedit
;;     ws-butler
;;     yasnippet
;;     smartparens
;;     smex
;;     spaceline
;;     rtags
;;     rust-mode
;;     racer
;;     use-package    
;;     vue-mode
;;     volatile-highlights
;;     undo-tree
;;     zygospore))

(provide 'init-elpa)
