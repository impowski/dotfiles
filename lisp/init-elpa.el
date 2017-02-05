(use-package package
  :init
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize))

(defconst imp-packages
  '(anzu
    ac-html-bootstrap
    auto-compile
    base16-theme
    cask
    cargo
    company
    company-irony
    company-irony-c-headers
    company-web
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
    dtrt-indent
    tagedit
    ws-butler
    indent-guide
    iedit
    irony
    irony-eldoc
    jade-mode
    js2-mode
    js2-refactor
    yasnippet
    smartparens
    powerline
    rtags
    rust-mode
    racer
    use-package
    projectile
    vue-mode
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
