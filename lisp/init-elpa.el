(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
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
    eldoc
    duplicate-thing
    flycheck
    flycheck-irony
    clean-aindent-mode
    comment-dwim-2
    dtrt-indent
    ws-butler
    iedit
    irony
    yasnippet
    smartparens
    powerline
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

(provide 'init-elpa)
