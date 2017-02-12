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
    company-web
    cmake-ide
    cmake-mode
    clean-aindent-mode
    comment-dwim-2
    diminish
    dtrt-indent
    duplicate-thing
    evil
    flycheck
    flycheck-rust
    flx
    flx-ido
    helm
    helm-ag
    helm-flx
    helm-make
    helm-mode-manager
    helm-projectile
    helm-swoop
    helm-themes
    imenu
    popwin
    projectile
    indent-guide
    iedit
    ido
    jade-mode
    js2-mode
    js2-refactor
    magit
    markdown-mode
    tagedit
    ws-butler
    yasnippet
    smartparens
    smex
    spaceline
    rtags
    rust-mode
    racer
    use-package    
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
