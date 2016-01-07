;;; magit --- Emacs magit
;;; Commentary:
;;; Package: magit

(require 'magit)

;;; Code:

(global-set-key "\C-cm" 'magit-status)
(custom-set-variables
 '(magit-save-some-buffers (quote dontask)))

(setq magit-process-conection-type nil)
(setq magit-emacsclient-executable nil)
(setq magit-stage-all-confirm nil)
(setq magit-unstage-all-confirm nil)
(setq magit-restore-window-configuration t)

(define-key magit-mode-map "q" 'bury-buffer)

(custom-set-faces
 '(magit-item-highlight ((t (:background "CadetBlue1"))))
 )

(provide 'init-git)

;;; init-git.el ends here
