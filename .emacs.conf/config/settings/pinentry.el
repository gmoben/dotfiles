;;; pinentry.el --- GPG pinentry integration -*- lexical-binding: t; -*-

;; Use pinentry-emacs for minibuffer prompts
;; Works with multiple emacsclient instances - prompt appears in active frame
;; Requires allow-emacs-pinentry in ~/.gnupg/gpg-agent.conf

(use-package pinentry
  :config
  (setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
  (pinentry-start))
