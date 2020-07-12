(use-package ggtags
	     :config
	     (add-hook 'projectile-mode-hook 'ggtags-mode)
	     (define-key ggtags-mode-map (kbd "M-.") nil)
	     (define-key ggtags-mode-map (kbd "M-<") nil)
	     (define-key ggtags-mode-map (kbd "M->") nil)
	     (define-key ggtags-mode-map (kbd "M-n") nil)
	     (define-key ggtags-mode-map (kbd "M-p") nil)
	     (define-key ggtags-mode-map (kbd "M-,") nil)
	     (define-key ggtags-mode-map (kbd "M-]") nil)
	     (define-key ggtags-mode-map (kbd "M--") 'ggtags-find-reference))
