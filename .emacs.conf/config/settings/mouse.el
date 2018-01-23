(require 'mouse)

(xterm-mouse-mode 1)
(global-set-key [mouse-4] (lambda()
			    (interactive)
			    (scroll-down 3)))
(global-set-key [mouse-5] (lambda()
			    (interactive)
			    (scroll-up 3)))
