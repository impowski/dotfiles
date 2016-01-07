;;; init-jabber --- Jabber

;;; Commentary:

;;; Code:

(require 'jabber)
(require 'jabber-bookmarks)

(setq jabber-account-list '(("z3r0c0nf1d3nc3@dukgo.com"
                             (:password . nil)
                             (:port . 5222)
                             (:connection-type . starttls)
                             (:jabber-connection-ssl-program . openssl)))
      )

(setq jabber-socks5-proxies-data '(("streamhost"
                                    (:jid . z3r0c0nf1d3nc3@dukgo.com)
                                    (:host . 127.0.0.1)
                                    (:port . 9100)))
      )

(defun imp/jabber-connect-hook (jc)
  (jabber-connect-all)
  (jabber-send-presence "" "I'm online" 10)
  (let* ((state-data (fsm-get-state-data jc))
         (server (plist-get state-data :server)))
    (message "%s" server)
    ))
(add-hook 'jabber-post-connect-hooks 'imp/jabber-connect-hook)

(defun imp/jabber-chat-hook ()
  (auto-fill-mode -1)
  (flyspell-mode -1))
(add-hook 'jabber-chat-mode-hook 'imp/jabber-chat-hook)

(require 'jabber-chatbuffer)
(eval-after-load "jabber-chatbuffer"
  (progn
    (define-key jabber-chat-mode-map "\S-r" 'newline)
    (define-key jabber-chat-mode-map [S-return] 'newline)
    t))

(setq jabber-history-enabled t)
(setq jabber-use-global-history nil)
(setq jabber-roster-show-bindings nil)
(setq jabber-vcard-avatars-retrieve nil)

(require 'jabber-autoaway)
(add-hook 'jabber-post-connect-hook 'jabber-autoaway-start)

(setq jabber-alert-info-message-hooks (quote (jabber-info-echo)))
(setq jabber-alert-message-hooks (quote (jabber-message-beep jabber-message-scroll)))
(setq jabber-alert-presence-hooks (quote (jabber-presence-update-roster)))
(setq jabber-nickname "z3r0c0nf1d3nc3")

(setq jabber-chat-buffer-show-avatar nil)

(custom-set-variables
 '(jabber-auto-reconnect t)
 '(jabber-groupchat-buffer-format "*-jg-%n-*")
 '(jabber-roster-buffer "*-jroster-*")
 '(jabber-roster-line-format " %c %-25n %u %-8s  %S")
 '(jabber-chat-buffer-format "*-jc-%n-*")
 '(jabber-muc-private-buffer-format "*-jmuc-priv-%g-%n-*")
 '(jabber-rare-time-format "%e %b %Y %H:00")
 )

(custom-set-faces
 '(jabber-chat-prompt-system ((t (:foreground "darkgreen" :weight bold))))
 )

(setq fsm-debug nil)

(provide 'init-jabber)

;;; init-jabber.el ends here
